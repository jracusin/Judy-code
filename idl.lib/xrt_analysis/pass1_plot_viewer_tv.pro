;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_plot_viewer_tv.pro	1.23
;;;
;;; DESCRIPTION:  Plot Viewer TV Widget
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;;
;;; INTERFACE TO CLIENT

;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreatePlotViewerTv, PARENT_WIDGET=parent, PLOT_VIEWER=pv_id, $
			     DRAW_ID=draw_id
widget_control, /HOURGLASS

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base(parent, EVENT_FUNC='PlotViewerTvEventFn', $
			KILL_NOTIFY='PlotViewerTvCleanup', $
			/BASE_ALIGN_CENTER, /ROW, /SPACE, /XPAD, /YPAD ) 

  list = ['Box', 'Gaussian']
  kernel_style_drop = widget_droplist( top_base, VALUE=list, TITLE='Kernel:' )
  
; bw_base = widget_base(top_base, /ROW, /SPACE, /XPAD, /YPAD, /FRAME)

    label = widget_label(top_base, VALUE='Min BW:')

    bandwidth_x = cw_field(top_base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE='X', XSIZE=5)

    bandwidth_y = cw_field(top_base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE='Y', XSIZE=5)
    
    bw_x = 0.5  &  bw_y = 0.5
    widget_control, bandwidth_x, SET_VALUE=bw_x
    widget_control, bandwidth_y, SET_VALUE=bw_y

  scaling_button = widget_button(top_base, VALUE='Scaling')
  
  cut_button = widget_button(top_base, VALUE='Cut')

  surface_button = widget_button(top_base, VALUE='Insight')
  
  color_button = widget_button(top_base, VALUE='Colors')


color_manager, UTILITY_DIR=utility_dir

; Setup state structure.
state = { $ 
	;IDs of widgets that generate events or need to be updated. 
	top_base:top_base, pv_id:pv_id, draw_id:draw_id, msg_id:-1L, $
	scaling_button:scaling_button, cut_button:cut_button, $
	kernel_style_drop:kernel_style_drop, kernel_style:'Box', $
	bandwidth_x:bandwidth_x, bandwidth_y:bandwidth_y, $
	support_x:0.0, support_y:0.0, $
	surface_button:surface_button, color_button:color_button, $
	
	;Other info
        im_ll_dat:[0.,0.], scale_array_to_data:[0.,0.], im_size_array:[0,0],$
	im_ll_dev:[0.,0.], im_size_dev:[0.,0.], $

	image:ptr_new(/ALLOC), img_min:0.0, img_range:0.0, $
	pixel_x:ptr_new(/ALLOC), pixel_y:ptr_new(/ALLOC), $
	scale_midpt:0.25, scale_width:0.5, starting_column:0, $

	dataset_name:'', dataset_xptr:ptr_new(), dataset_yptr:ptr_new(), $
	data_x:ptr_new(/ALLOC), data_y:ptr_new(/ALLOC), $
	data_weight:ptr_new(/ALLOC), $

	indexed_add_lib:utility_dir + 'indexed_add.so', $
	data_stale_flag:1B, image_stale_flag:0B, $
	zero_weight_count:0L, num_points_unique:0L, search_frontier:0L }

;; Save state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
END


;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO PlotViewerTvCleanup, top_base

widget_control, top_base, GET_UVALUE=st, /NO_COPY

ptr_free, st.image, st.pixel_x, st.pixel_y, st.data_x, st.data_y, st.data_weight

return
end

;==========================================================================
;;; Routine to scale & TV the image
;==========================================================================
PRO ShowPlotViewerTv, st

color_manager, NCOLORS=ncolors, TABLED_USED=table_used

;; Compute the absolute scaling limits.
if (st.img_range EQ 0) then begin
  st.img_min   = min( *st.image, MAX=max_val )
  st.img_range = max_val - st.img_min
endif

tvlow  = st.img_min + st.img_range * (st.scale_midpt - st.scale_width/2.0)
tvhigh = st.img_min + st.img_range * (st.scale_midpt + st.scale_width/2.0)

im = bytscl( *st.image, TOP=ncolors-1, MIN=tvlow, MAX=tvhigh )

; Show B&W PostScript as a negative image.
if (table_used EQ 0) then im = 255 - temporary(im)

tv, im,     st.im_ll_dev(0),         st.im_ll_dev(1), /DEVICE, $
    XSIZE=st.im_size_dev(0), YSIZE=st.im_size_dev(1)
return
end

;==========================================================================
;;; Function to apply the Kernel Estimator to (datapoint, pixel) pairs.
;;; The 2-D Kernel Estimator is defined as:
;;;	f(x,y) = 1 / (n*hx*hy) SUM [ K( (x-Xi)/hx, (y-Yi)/hy ) ]

;;; This routine evaluates a weighted version of this kernel function K().
;;; See "Density Estimation for Statistics and Data Analysis" by 
;;; B.W. Silverman, Chapman and Hall 1986.

;--------------------------------------------------------------------------
; THE RETURNED VECTOR MUST BE FLOAT because the caller expects that!!!!
;--------------------------------------------------------------------------

;;; The parameters data_x & data_y are vectors containing a set of datapoints 
;;; (Xi, Yi), i=0..N-1.  For efficiency, each datapoint has a weight, 
;;; data_weight, so that duplicate calls to this function can be avoided when 
;;; there are multiple datapoints at the same location.

;;; The parameters pixel_x & pixel_y are scalars or vectors defining a set of
;;; pixel positions (x,y) at which K should be evaluated.  If they are scalars, 
;;; then the kernel at the single specified position (pixel_x, pixel_y) is 
;;; evaluated for each of the specified datapoints, i.e. we compute
;;;
;;; w[i] * K( (pixel_x - data_x[i])/bw_x, (pixel_y - data_y[i])/bw_y ).

;;; Alternatively, if pixel_x & pixel_y are vectors of the same length as 
;;; data_x, containing a bunch of pixel positions Pi, i=0..N-1, then the kernel
;;; functions computed are:
;;;
;;; w[i] * K( (pixel_x[i] - data_x[i])/bw_x, (pixel_y[i] - data_y[i])/bw_y ).

;;; NOTE NOTE NOTE NOTE NOTE NOTE 
;;; Only N evaluations of the kernel are performed.  We do NOT compute 
;;; "cross terms", e.g.
;;;
;;; w[i] * K( (pixel_x[j] - data_x[i])/bw_x, (pixel_y[j] - data_y[i])/bw_y ),
;;; i != j.
;==========================================================================

FUNCTION ApplyKernel, pixel_x, pixel_y, data_x, data_y, data_weight, $
		      bandwidth_x, bandwidth_y, kernel_style
case kernel_style of
  ;-------------------------------------------------------------------------   
  ;; Box kernel K(a,b) = 0.25 if -1<a<=1 and -1<b<=1
  ;; The conditions -1<a<=1 and -1<b<=1 correspond to
  ;; -bw_x < (pixel_x - data_x) <= bw_x and 
  ;; -bw_y < (pixel_y - data_y) <= bw_y
  
  ;; So, the box kernel equals (0.25 * data_weight), except for 
  ;; (datapoint, pixel) pairs that are too far apart, where the kernel equals 0.
  ;; However, since we don't care about scaling factors, we'll omit the 0.25
  ;; term.
  ;--------------------------------------------------------------------------
  'Box': $
   begin
   
   kernel = float(data_weight)
   
   x_dist = pixel_x - data_x
   y_dist = pixel_y - data_y
   
   index = where( (-bandwidth_x GE x_dist) OR (x_dist GT bandwidth_x) OR $
   	          (-bandwidth_y GE y_dist) OR (y_dist GT bandwidth_y), count )
   
   if (count GT 0) then begin
     kernel(index) = 0
   endif 
   end

   
  ;-------------------------------------------------------------------------   
  ;; Gaussian kernel K(a,b) = 1 / (2*pi) exp( -0.5 * (a^2 + b^2) )
  ;; Since we don't care about scaling factors, we'll omit the 1/(2pi) term.
  ;-------------------------------------------------------------------------   
  'Gaussian': $
   begin
   aa = (pixel_x - data_x) / bandwidth_x
   bb = (pixel_y - data_y) / bandwidth_y
   
   kernel = data_weight * exp( -0.5 * (aa^2 + bb^2) )
   end
endcase

return, kernel
end

;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawPlotViewerTv, st

widget_control, /HOURGLASS
last_tv_time = systime(1)
;;---------------------------------------------------------------------------
;; Create the STOP button.
location = get_screen_size()/2 - [150,0]
stop_base = widget_base( TITLE='Computing image ...', $
	    XOFF=location(0), YOFF=location(1), GROUP=st.pv_id )
stop_button = widget_button( stop_base, VALUE='STOP', XSIZ=300 )
widget_control, stop_base, /REALIZE


;;---------------------------------------------------------------------------
;; Determine the region (LL and UR corners) of the DEVICE coordinate system
;; that will be covered by the image.  These are integers!

im_corners_dev = convert_coord( !X.WINDOW + [0.01, -0.01], $
				!Y.WINDOW + [0.01, -0.01], /NORM,/TO_DEV)

im_ll_dev = [round( im_corners_dev[0,0] ), round( im_corners_dev[1,0] )]
im_ur_dev = [round( im_corners_dev[0,1] ), round( im_corners_dev[1,1] )] 
im_size_dev = im_ur_dev - im_ll_dev + [1,1]

;;---------------------------------------------------------------------------
;; Determine the DATA coordinates of those same corners.
;; These are real numbers!

im_ll_dat = (convert_coord( im_ll_dev, /DEV, /TO_DATA ))(0:1)
im_ur_dat = (convert_coord( im_ur_dev, /DEV, /TO_DATA ))(0:1)
im_size_dat = im_ur_dat - im_ll_dat

;;---------------------------------------------------------------------------
;; We need to position an image array over the region defined above
;; and define a transformation from the ARRAY coordinate system to the DATA 
;; coordinate system:
;;	[data_x,data_y] = im_ll_dat + scale_array_to_data * [column,row]

if (0 EQ (!d.flags and 1)) then begin
  ; If the device has fixed-size pixels (X window), then array elements will
  ; coincide exactly with device pixels, so the array will have dimensions
  ; im_ur_dev - im_ll_dev + [1,1].
  im_size_array = im_size_dev

endif else begin
  ; If the device has scalable pixels (PostScript), then the array size will
  ; be arbitrarily set to 512x512.  
  im_size_array = [512,512]

endelse

scale_array_to_data = im_size_dat / im_size_array 

;;---------------------------------------------------------------------------
;; Set various flags to determine what needs to be computed.
flags = [st.im_ll_dat, st.scale_array_to_data, st.im_size_array] NE $
	[im_ll_dat, scale_array_to_data, im_size_array]
if (total(flags) NE 0) then begin
  st.image_stale_flag    = 1
  st.im_ll_dev           = im_ll_dev
  st.im_ll_dat           = im_ll_dat
  st.im_size_dev         = im_size_dev
  st.scale_array_to_data = scale_array_to_data
  st.im_size_array       = im_size_array
endif

;;---------------------------------------------------------------------------
;; If necessary, create a blank image.
n_columns = st.im_size_array(0)
n_rows    = st.im_size_array(1)

if (st.image_stale_flag) then begin
  st.image_stale_flag = 0
  *st.image           = fltarr( n_columns, n_rows )
  st.img_range        = 0.
  st.starting_column  = 0

  ; Compute the (X,Y) location of each image pixel in array coordinates.
  indexes = lindgen( n_columns, n_rows )
  row     = indexes / n_columns
  column  = indexes - (row * n_columns)

  ; Now, convert the array coordinates to data coordinates.
  ;	[data_x,data_y] = im_ll_dat + scale_array_to_data * [column,row]
  *st.pixel_x = st.im_ll_dat(0) + st.scale_array_to_data(0) * column
  *st.pixel_y = st.im_ll_dat(1) + st.scale_array_to_data(1) * row
endif 

;;---------------------------------------------------------------------------
;; If necessary, retrieve the data.  We MUST make a copy of the X & Y data
;; arrays held by plot_viewer because this widget is going to modify them
;; as it identifies duplicate datapoints.
if (st.data_stale_flag) then begin
  st.data_stale_flag = 0

  *st.data_x = *st.dataset_xptr
  *st.data_y = *st.dataset_yptr
  *st.data_weight = replicate(1L, n_elements(*st.data_x))
  st.num_points_unique = 0
  st.search_frontier   = 0
  st.zero_weight_count = 0
  print, 'retrieved data from plot_viewer
  widget_control, st.top_base, TIMER=2
endif


;; The bandwidth used needs to be at least half of the size of the screen
;; pixels, otherwise some data points will not contribute to the display
;; at all.
widget_control, st.bandwidth_x, GET_VALUE=bandwidth_x
widget_control, st.bandwidth_y, GET_VALUE=bandwidth_y
bandwidth_x = bandwidth_x > (scale_array_to_data(0) / 2.)
bandwidth_y = bandwidth_y > (scale_array_to_data(1) / 2.)
print, bandwidth_x, bandwidth_y, f='("Bandwidths used: (",G9.3,", ",G9.3,")")'

;;---------------------------------------------------------------------------
;; Figure out the largest possible support (in column/row units) that the
;; kernel could have.
;; [column,row] = ([data_x,data_y] - im_ll_dat) / scale_array_to_data
case st.kernel_style of
  'Box': $
   ; The box kernel extends exactly one bandwidth in each direction.
   begin
   st.support_x = 2 * bandwidth_x
   st.support_y = 2 * bandwidth_y
   end
   
  'Gaussian': $
   ; The Gaussian kernel has infinite support, but we'll truncate it at 3  
   ; bandwidths (in each direction) to keep the computation manageable.  
   ; To give you some sense of the error we're making, consider the ratio
   ; K(3,0)/K(0,0) = exp( -0.5 * (3^2+0) ) = 0.01.  This would seem to imply
   ; that we're making at most a 1% error in the value of K.  
   begin
   st.support_x = 6 * bandwidth_x
   st.support_y = 6 * bandwidth_y
   end
endcase

support_column = 1 > ceil( st.support_x / scale_array_to_data(0) )
support_row    = 1 > ceil( st.support_y / scale_array_to_data(1) )
;help, support_column, support_row

;;---------------------------------------------------------------------------
;; Determine the origin (ll corner) of the sub-arrays that might be affected
;; by each data point.
;;
;; A sub array is defined as image( cstrt:cstrt+support_column-1, 
;;				    rstrt:rstrt+support_row-1 ).
;;
;; For example, to get cstrt we compute the location of the point 
;; (data_x - 0.5*support_x) in the array coordinate system. Then cstrt is
;; the CEILING of this value (NOT the floor).  Say that (data_x - 0.5*support_x)
;; falls at location 7.5 in the array coordinate system.  Then column 7
;; of the full image would NOT be affected by the datapoint, but column 8 would.
;;
left_support_edge =  (*st.data_x - 0.5*st.support_x - im_ll_dat(0)) / $
		     scale_array_to_data(0)

lower_support_edge = (*st.data_y - 0.5*st.support_y - im_ll_dat(1)) / $
		     scale_array_to_data(1)
		     
cstrt = 0 > ceil( left_support_edge )

rstrt = 0 > ceil( lower_support_edge )

;;---------------------------------------------------------------------------
;; Raster through these sub-arrays, applying the kernel to the sub-array 
;; elements for all the datapoints.

;; If we wanted the image array to be samples of a probability density
;; then we should scale it by 1 / (N * bandwidth_x * bandwidth_y).
;; See "Density Estimation for Statistics and Data Analysis" by 
;; B.W. Silverman, Chapman and Hall 1986.
;; The N term is the number of data points in the entire dataset.

;; In our case however, we don't care about scaling effects on the array
;; image since we're going to scale it before displaying anyway.
;; Thus, for efficiency we will here omit these terms.
  

for col_offset = st.starting_column, support_column-1 do begin
  for row_offset = 0, support_row-1 do begin
  
    cols = cstrt + col_offset
    rows = rstrt + row_offset
    
    ; Figure out which image pixels (cols, rows) are outside the array bounds.
    invisible = where( cols LT 0 OR cols GE n_columns OR $
    		       rows LT 0 OR rows GE n_rows, invisible_count   )
    		     
    ; Set the pixel coordinates of the invisible datapoints to (0,0) to avoid
    ; violating the bounds of the image array.  We'll make sure the kernel
    ; evaluates to zero for these datapoints later.
    if (invisible_count GT 0) then begin
      cols(invisible) = 0
      rows(invisible) = 0
    endif 
    
    ; Look up the positions of these image pixels in the data coordinate system.
    pixel_x = (*st.pixel_x)(cols,rows)
    pixel_y = (*st.pixel_y)(cols,rows)
      
    ; Evaluate the kernel at all these datapoint/pixel pairs.
    kernel = ApplyKernel( pixel_x, pixel_y, $
    			  *st.data_x,  *st.data_y,  *st.data_weight, $
      			  bandwidth_x, bandwidth_y, st.kernel_style )
    
    ; Set the kernel to zero for invisible datapoints.
    if (invisible_count GT 0) then begin
      kernel(invisible) = 0
    endif
      			    
    ; Increment the image pixels by the kernel value.
    index1d =  (rows * LONG(n_columns)) + cols
      
    ; Don't forget: image and kernel must be FLOAT, index1d must be LONG.
;   help, *st.image, index1d, kernel, n_elements(kernel)
    dummy = CALL_EXTERNAL( st.indexed_add_lib, 'indexed_add', $
      			   *st.image, index1d, kernel, n_elements(kernel) )
      			     
    ; TV the image periodically to entertain the user
    if (4 LT (systime(1) - last_tv_time)) then begin
        st.img_range = 0.
        if (!D.NAME EQ 'X') then ShowPlotViewerTv, st
        last_tv_time = systime(1)
        
        footprint_done = (support_row * col_offset) + row_offset
        label = string(100.*footprint_done / (support_column*support_row), $
        		f='("STOP  (",I0,"% completed)  STOP")')
        widget_control, stop_button, SET_VALUE=label
    endif
  endfor

  st.starting_column = col_offset + 1
  
  ; Poll the STOP button.  We do this in the OUTER loop because a policy of
  ; always computing full columns of the sub-arrays makes it easier to write
  ; the code so that it can be restarted after a user abort.
  event = widget_event( stop_button, /NOWAIT, BAD_ID=bad )
  widget_control, /HOURGLASS
    
  if (event.id EQ stop_button) then GOTO, ABORT

endfor


;;---------------------------------------------------------------------------
ABORT:

;; Destroy the STOP button.
widget_control, stop_base, /DESTROY, BAD_ID=bad

;; TV the complete image.
ShowPlotViewerTv, st

return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION PlotViewerTvEventFn, Event

new_event   = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st, /NO_COPY

;; Process the event.
case Event.ID of
;--------------------------------------------------------------------------
; TOP BASE
  top_base: $
  begin  
  ;; At the beginning, st.num_points_unique datapoints have been proven unique.
  ;; They are represented by non-zero weights in the head of *st.data_weight,
  ;; i.e. (*st.data_weight)[0:st.search_frontier-1] possibly mixed in with 
  ;; zero-weight points.
  
  ;; The datapoints that have not been checked yet lie in the tails of
  ;; *st.data_x & *st.data_y in the index range [st.search_frontier:*].
   
  ;; Look through the tail of the weight array for the next unique datapoint.
  num_pts = n_elements(*st.data_weight)

  new_point_found = 0
  while ((NOT new_point_found) AND (st.search_frontier LT num_pts)) do begin
  
    if ((*st.data_weight)(st.search_frontier) NE 0) then begin
      new_point_found = 1
      st.num_points_unique = st.num_points_unique + 1
      data_x = (*st.data_x)(st.search_frontier)
      data_y = (*st.data_y)(st.search_frontier)
      
      
      ; We've encountered a new data point, not yet seen.  Look for duplicates.
      ; We really only need to look for duplicates in the tail of the array,
      ; since the head is known to contain unique data, however it might
      ; take longer to extract the tail than to search the whole array.

      index = where( (*st.data_x EQ data_x) AND (*st.data_y EQ data_y), count )
  
      if (count GT 1) then begin
        total_weight             = total( (*st.data_weight)(index) )
        (*st.data_weight)(index) = 0
        (*st.data_weight)(st.search_frontier) = total_weight
        st.zero_weight_count = st.zero_weight_count + count - 1
      endif

    endif
    st.search_frontier = st.search_frontier + 1
    
  endwhile
        

  ;; If helpful or if the search frontier has passed the end of the array, 
  ;; throw away the zero-weight points.
  if ((st.zero_weight_count GT num_pts/10) OR $
      (st.search_frontier GE num_pts)) then begin
          widget_control, /HOURGLASS
	  st.zero_weight_count = 0
	  st.search_frontier   = st.num_points_unique
	  index = where( *st.data_weight, count )
	  *st.data_x      = (*st.data_x)(index)
	  *st.data_y      = (*st.data_y)(index)
	  *st.data_weight = (*st.data_weight)(index)
	  print, st.num_points_unique, $
	  	 100. * st.search_frontier / n_elements(*st.data_weight), $
	  	 f='(I0," unique data points out of ",I0,"% searched")'
  endif
  
  ;; Setup another timer event.
  if (st.search_frontier LT n_elements(*st.data_weight)) then $
    widget_control, top_base, TIMER=2

  end


;--------------------------------------------------------------------------
; SCALING BUTTON
  st.scaling_button: $
   begin
   ;; Make sure the correct X window is active,
   coordsystem_manager, st.draw_id, /RESTORE

   ;; Give some instructions
   prompt = ['Move the mouse to rescale.',$
   	'Horizontal position <=> location of "grey" within data range.', $
   	'Vertical position   <=> % of data range between "white" & "black".',$
   	'Click when finished']
   msg = NonmodalMessage( prompt, POS=st.scaling_button )

   ;; Position the pointer.
   tvcrs, st.scale_midpt, st.scale_width, /NORM

   ;; Enable motion events in the draw widget.
   widget_control, st.draw_id, /DRAW_MOTION_EVENTS

   ;; Handle the motion events.
   repeat begin
     event = widget_event( st.draw_id )

     pt = convert_coord( event.x, event.y, /DEVICE, /TO_NORM )
     st.scale_midpt = 0.0   > (pt(0) - 0.05)/.9 < 1.0  
     st.scale_width = 0.001 > (pt(1) - 0.05)/.9 < 1.0

     ShowPlotViewerTv, st
   endrep until (event.type EQ 1)

   ;; Disable motion events in the draw widget.
   widget_control, st.draw_id, DRAW_MOTION_EVENTS=0
   widget_control, st.draw_id, /CLEAR_EVENTS
   widget_control, msg, /DESTROY, BAD_ID=bad_id

   ;; Pass the event up to plot_viewer so that a re-draw is initiated.
   new_event = event
   new_event.ID = top_base
   end

;--------------------------------------------------------------------------
; CUT BUTTON
  st.cut_button: $
   begin
   ;; Select endpoints with mouse.
   plot_viewer, st.pv_id, /CLICK, MOUSE_POSN=first_pt, $
		 PROMPT='Select a cut endpoint with the mouse.'
   plot_viewer, st.pv_id, /CLICK, MOUSE_POSN=last_pt, $
		 PROMPT='Select the other endpoint with the mouse.'
		
   ;; Confirm endpoint coordinates with D-box. 
    b1 = '1, BASE,, ROW, FRAME'

    f2 = '0, FLOAT,' + string(first_pt(0)) + $
	 ', TAG=x0, LABEL_LEFT=Endpoint (X Y), WIDTH=12'

    f3 = '2, FLOAT,' + string(first_pt(1)) + ', TAG=y0, WIDTH=12'

    b2 = '1, BASE,, ROW, FRAME'

    f4 = '0, FLOAT,' + string(last_pt(0)) + $
	 ', TAG=x1, LABEL_LEFT=Endpoint (X Y), WIDTH=12'

    f5 = '2, FLOAT,' + string(last_pt(1)) + ', TAG=y1, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [b1,f2,f3,b2,f4,f5,f6], /COLUMN, $
			 TITLE='Cut Endpoints' )

    first_pt = [result.x0, result.y0]
    last_pt  = [result.x1, result.y1]
    
    
   ;; Let's represent this line in the form Ax + By = C.
   ;; A * (last_pt(0) - first_pt(0)) + B * (last_pt(1) - first_pt(1)) = 0
   ;; The constants A, B, & C are not uniquely determined -- we'll set
   ;; either A or B to 1 depending on the slope of the line.
   widget_control, /HOURGLASS
   delta_x = (last_pt(0) - first_pt(0))
   delta_y = (last_pt(1) - first_pt(1))   
   
   if (abs(delta_x) GT abs(delta_y)) then begin
     B = 1.0
     A = -B * delta_y / delta_x
     C = A * first_pt(0) + B * first_pt(1)
   endif else begin
     A = 1.0
     B = -A * delta_x / delta_y
     C = A * first_pt(0) + B * first_pt(1)
   endelse
  
   
   ;; Evaluate kernel along the cut.
   num_points = 500.0
   t = findgen(num_points)/num_points
   pixel_x = first_pt(0) + t * delta_x
   pixel_y = first_pt(1) + t * delta_y
   kernel = fltarr(num_points)
   
   ;; We only need to consider datapoints that lie close enough to the
   ;; line to affect the kernel evaluations along the line.
   ;; The distance from a point to the line is:
   ;;   abs( Ax + By - C ) / sqrt( A^2 + B^2 )
   
   threshold = 0.5 * (st.support_x > st.support_y)
   distance  = abs( A*(*st.data_x) + B*(*st.data_y) - C ) / sqrt( A^2 + B^2 )
   
   index = where( distance LE threshold, count )
   if (count GT 0) then begin
     widget_control, st.bandwidth_x, GET_VALUE=bandwidth_x
     widget_control, st.bandwidth_y, GET_VALUE=bandwidth_y
     data_x      = (*st.data_x)(index)
     data_y      = (*st.data_y)(index)
     data_weight = (*st.data_weight)(index)
     
     for ii = 0,num_points-1 do $
       kernel(ii) = total( ApplyKernel( pixel_x(ii), pixel_y(ii), $
     				        data_x, data_y, data_weight, $
     				        bandwidth_x, bandwidth_y, $
     				        st.kernel_style ) )
   endif
   
   norm = max(*st.image)
   if (norm NE 0) then kernel = temporary(kernel)/norm
     				
   tit = string( first_pt, last_pt, $
   	  f='("Cut between (",2G11.4,") and (",2G11.4,")")' )
   subtit = string( bandwidth_x, bandwidth_y, $
   	  f='("Bandwidths: (",2G11.4,")")' )
   	  
   plot_viewer, ID1, pixel_x, kernel, GROUP=top_base, $
   		TIT=strcompress(tit), SUBT=strcompress(subtit), $
   		XTIT='Horizontal Image Axis', YTIT='Normalized Density Value', $
   		WIDGET_TITLE=st.dataset_name, DATASET_NAME=st.dataset_name
   	
   plot_viewer, ID2, pixel_y, kernel, GROUP=top_base, $
   		TIT=strcompress(tit), SUBT=strcompress(subtit), $
   		XTIT='Vertical Image Axis', YTIT='Normalized Density Value', $
   		WIDGET_TITLE=st.dataset_name, DATASET_NAME=st.dataset_name
   	
   ;; Make sure the correct X window is active & draw cut line.
   coordsystem_manager, st.draw_id, /RESTORE
   color_manager, RED=red
   plots, [first_pt(0), last_pt(0)], [first_pt(1), last_pt(1)], /DATA, $
   	  THICK=2, COLOR=red
   end

;--------------------------------------------------------------------------
; SURFACE BUTTON
  st.surface_button: $
   begin
   widget_control, /HOURGLASS
   pixel_x=*st.pixel_x  &  pixel_y=*st.pixel_y  &  density=*st.image
   insight, pixel_x, pixel_y, density, /INDEXED_COLOR
   end

;--------------------------------------------------------------------------
; COLOR BUTTON
  st.color_button: $
   begin
   color_manager, NCOLORS=ncolors
   xloadct, GROUP=top_base, NCOLORS=ncolors
   end
   
;--------------------------------------------------------------------------
  st.kernel_style_drop: $
   begin
   case Event.index of
    0: st.kernel_style = 'Box'
    1: st.kernel_style = 'Gaussian'
   endcase
   st.image_stale_flag = 1

   ;; Pass the event up to plot_viewer so that a re-draw is initiated.
   new_event = event
   new_event.ID = top_base
   end
   
;--------------------------------------------------------------------------
; BANDWIDTH BUTTONS
  st.bandwidth_x: $
   begin
   st.image_stale_flag = 1

   ;; Pass the event up to plot_viewer so that a re-draw is initiated.
   new_event = event
   new_event.ID = top_base
   end

  st.bandwidth_y: $
   begin
   st.image_stale_flag = 1

   ;; Pass the event up to plot_viewer so that a re-draw is initiated.
   new_event = event
   new_event.ID = top_base
   end

 ;--------------------------------------------------------------------------
  else: message, 'unknown event in plot_viewer_roi'
endcase

widget_control, top_base, SET_UVALUE=st, /NO_COPY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO PlotViewerTvEvent, Event
 
event = PlotViewerTvEventFn( Event )
return
end


;==========================================================================
;;; MAIN "pass1_plot_viewer_tv" ROUTINE

;;; The keywords IMAGE, CRVAL, CRPIX, CRDELT are used by plot_viewer to
;;; retrieve the image array to save it to a file, along with the FITS
;;; keywords to carry along the coordinate system transformations.
;==========================================================================

PRO pass1_plot_viewer_tv, top_base, DATASET=dataset, MODIFIED=modified, DRAW=draw,$
		    IMAGE=image, CRVAL=crval, CRPIX=crpix, CDELT=cdelt, $
		    _EXTRA=extra


;; If the widget ID of an existing plot_viewer_roi was not passed, the create
;; the widget.
if (NOT keyword_set(top_base)) then $
  top_base = CreatePlotViewerTv( _EXTRA=extra )


;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st, /NO_COPY

;; If necessary, mark the data and image as obsolete.
if keyword_set(dataset) then begin
  if ((st.dataset_name EQ dataset.name) AND keyword_set(modified)) then begin
    st.dataset_xptr = dataset.x_data
    st.dataset_yptr = dataset.y_data
    st.data_stale_flag  = 1
    st.image_stale_flag = 1
  endif
  
  if keyword_set(draw) then begin
    if (st.dataset_name NE dataset.name) then begin
      st.dataset_name = dataset.name
      st.dataset_xptr = dataset.x_data
      st.dataset_yptr = dataset.y_data
      st.data_stale_flag  = 1
      st.image_stale_flag = 1
    endif
    
    RedrawPlotViewerTv, st
  endif
endif


;;---------------------------------------------------------------------------
;; Return the image and coordinate transformations if requested.
if arg_present( image ) then begin
  ; The FITS keywords CRVALn, CRPIXn, & CDELTn define a data coordinate
  ; system in terms of the 1-based "pixel index" coordinates of the array.
  ;
  ; (data - CRVALn) = CDELTn * (pixel - CRPIXn)
  ;
  ; The transformation we work with in this widget is from the zero-based
  ; IDL array index system to the data system:
  ;
  ; [data_x,data_y] = im_ll_dat + scale_array_to_data * [column,row]
  ;
  ; Substituting  column/row = pixel - 1, we get
  ;
  ; data = im_ll_dat + scale_array_to_data * (pixel - 1)
  ;
  ; So, CDELT = scale_array_to_data, CRPIX = 1, CRVAL = im_ll_dat.
  
  image = *st.image
  crval  = st.im_ll_dat
  crpix  = [1.0, 1.0]
  cdelt = st.scale_array_to_data 
endif

;;---------------------------------------------------------------------------
;; Save the state structure.
widget_control, top_base, SET_UVALUE=st, /NO_COPY

return
END
