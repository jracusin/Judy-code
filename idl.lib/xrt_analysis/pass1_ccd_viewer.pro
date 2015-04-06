; $Id: pass1_ccd_viewer.pro 1.0 2002/01/18 17:25:30 burrows $
;
;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_ccd_viewer.pro	9.5
;;;
;;; DESCRIPTION:  CCD Viewer Widget
;;;
;;;               Used to display a CCD image.  Includes an pass1_image_viewer
;;;               widget and two pass1_image_viewer_roi widgets.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;; INTERFACE TO CLIENT

;;;FUNCTION pass1_ccd_viewer, image, ID=id, PARENT=parent, TITLE=title, $
;;;		        UVALUE=uvalue, GROUP=group, BLOCK=block,$ 
;;;                     INDEX_TO_DATA_GAIN=index_to_data_gain,$
;;;                     INDEX_TO_DATA_OFFSET=index_to_data_offset,$
;;;                     XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle, $
;;;                     HANDLE=p_handle
;;;
;;; The parameters PARENT, UVALUE, & GROUP are the usual ones associated with
;;; IDL widgets.  If pass1_ccd_viewer is a top-level widget, then TITLE
;;; controls the title in the X-window frame.
;;;
;;; The HANDLE keyword is used to specify the parent of the pass1_ccd_viewer_handle,
;;; which is always created.  Specifying a parent handle will cause all ccd
;;; viewer handles to be freed whenever the parent handle is freed.  If
;;; pass1_ccd_viewer is called as a top level base widget, then all handles will
;;; be freed whenever the DISMISS button is pressed.
;;;
;;; The parameters INDEX_TO_DATA_GAIN, INDEX_TO_DATA_OFFSET, XTITLE, & YTITLE 
;;; are  used to define a "DATA coordinate system" -- see the comments in
;;; pass1_image_viewer.pro.
;;;
;;; TYPICAL USAGE
;;;
;;; This widget can be created in three distinct ways.
;;;
;;; First, it can be created as a child of an unrealized base widget by
;;; supplying the keyword parameter PARENT, just like one of the compound
;;; widgets in the user's library.
;;; The pass1_ccd_viewer widget will be realized when the parent is realized.
;;; The parent widget is responsible for asking the pass1_ccd_viewer widget
;;; to display the data for the first time by calling pass1_ccd_viewer with
;;; the ID keyword.  THIS IS NOT YET IMPLEMENTED!!
;;;
;;; For example:
;;;
;;;     pass1_ccd_viewer = pass1_ccd_viewer(image, PARENT=parent_id)
;;;     widget_control, parent_id, /REALIZE
;;;     dummy = pass1_ccd_viewer( ID=ccd_viewer )
;;;
;;; Second, the event handler of a realized widget can create a top level
;;; pass1_ccd_viewer that will be managed independently by the XMANAGER by
;;; omitting the parameter PARENT.
;;; The parameter TITLE may be supplied to put in the window frame.
;;; The pass1_ccd_viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  
;;;
;;; For example:
;;;
;;;     pass1_ccd_viewer = pass1_ccd_viewer(image, GROUP=creator_id)
;;;
;;; Third, a pass1_ccd_viewer may be created at the command line or by a regular
;;; IDL routine (not an event handler) by omitting the parameter PARENT.
;;; The pass1_ccd_viewer will realize itself, draw itself, and register with
;;; the XMANAGER. An explicit call to XMANAGER must then be made to start
;;; processing events.
;;; For example:
;;;
;;;     pass1_ccd_viewer = pass1_ccd_viewer(image, TITLE='My Data')
;;;     xmanager
;;;
;;; COMMUNICATION AMONG WIDGETS
;;;
;;; The pass1_ccd_viewer, pass1_image_viewer, and two pass1_image_viewer_roi widgets 
;;; interact in a complex way to display an image with overlay'd graphics
;;; depicting the ROI's.  Below are some example communication sequences.
;;;
;;; A. The pass1_image_viewer re-displays the image, destroying all overlays.
;;;
;;; 1. pass1_image_viewer generates an event to pass1_ccd_viewer
;;; 2. pass1_ccd_viewer gets overlay vectors of roi1 through its VALUE structure
;;; 3. pass1_ccd_viewer sends overlay vectors to pass1_image_viewer, e.g.
;;;	dummy = pass1_image_viewer( ID=iv, X_OVERLAY=x, Y_OVERLAY=y )
;;; 4. pass1_image_viewer draws the graphics
;;; 5. pass1_ccd_viewer gets overlay vectors of roi2 through its VALUE structure
;;; 6. pass1_ccd_viewer sends overlay vectors to image_viewer, e.g.
;;;	dummy = pass1_image_viewer( ID=iv, X_OVERLAY=x, Y_OVERLAY=y )
;;; 7. pass1_image_viewer draws the graphics
;;;
;;; B. A roi definition is changed by the user.
;;;
;;; 1. the roi widget generates an event to pass1_ccd_viewer
;;; 2. pass1_ccd_viewer obtains the revised roi statistics through the VALUE
;;;    structure of the roi widget
;;; 3. pass1_ccd_viewer forces image_viewer to re-display the image (erasing the
;;;    exsiting annoations) by the call:
;;;      dummy = pass1_image_viewer( ID=id )
;;; 4. pass1_ccd_viewer obtains ROI overlays and forwards them to pass1_image_viewer
;;;    as in sequence A.
;;;
;;; C. The user selects the DEFAULT roi.
;;;
;;; 1. the roi widget obtains the VALUE structure of the pass1_image_viewer
;;; 2. using a handle in the VALUE structure, the roi widget determines the
;;;    dimensions of the image and establishes a default roi
;;; 3. goto sequence B.
;;;
;;; D. The user indicates that a roi parameter should be set to the position
;;;    of the marker in the pass1_image_viewer.
;;;
;;; 1. the roi widget obtains the VALUE structure of the pass1_image_viewer
;;; 2. using the marker_x and marker_y fields in the VALUE structure, the roi 
;;;    widget redefines the ROI.
;;; 3. goto sequence B.
;;;
;;; E. New image data is passed to pass1_ccd_viewer.
;;;
;;; 1. pass1_ccd_viewer passes the new data to pass1_image_viewer, e.g.
;;;	dummy = pass1_image_viewer( ID=iv, image )
;;; 2. pass1_image_viewer re-displays the data, but does NOT generate an event
;;; 3. pass1_ccd_viewer informs both roi widgets that their ROI's are invalid;
;;;    the roi widgets do NOT generate events
;;;	dummy = pass1_image_viewer_roi( ID=roi1 )
;;;	dummy = pass1_image_viewer_roi( ID=roi2 )
;;; 4. pass1_ccd_viewer simulates an event arriving from image_viewer, starting
;;;    sequence A.
;;;
;;; F. Startup of pass1_ccd_viewer.
;;;
;;; 1. Same as E.
;;;
;
; $Log:  pass1_ccd_viewer.pro $
;-
;==========================================================================

FORWARD_FUNCTION CcdViewerEventFn

;==========================================================================
;;; This routine is called to change the image that is displayed.
;==========================================================================
PRO ModifyCcdViewer, top_base, image, TITLE=title, _EXTRA=extra

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

if keyword_set(title) then state.title=title

if (0 NE n_elements(image)) then begin

  ;; Floating point errors are a very sticky point.  We've decided to handle
  ;; NaN's (which can be generated by saturating a pixel) by replacing them
  ;; with zeroes.  The reasoning is:
  ;;    1) There should never be more than a few NaN's in your image.  CCDs
  ;;       are not normally operated in a mode where large portions of the
  ;;       detector are swamped.
  ;;    2) Zero is a value which is probably in the middle of the noise curve
  ;;       after bias subtraction.  Since we have only a few NaN's, setting
  ;;       them to zero will probably not throw off our statistical calculations
  ;;       much, whereas replacing a NaN with a large positive or negative
  ;;       number can mess up our statistics.  Adding a new zeros, then, should
  ;;       be much like adding a few points of noise.

  s=size(image)
  junk = where( finite(image) NE 1 )
  if (junk(0) NE -1) then begin
    x_bin = junk MOD s(1)
    y_bin = junk/s(1)
    str=strarr(n_elements(junk) + 2)
    str(0)="Invalid floating point values were found in the following " + $
	   "image bins:"
    for ii = 0, n_elements(junk)-1 do begin
      str(ii+1) = "[" + string(x_bin(ii),FORMAT='(I4)') + $
		  "," + string(y_bin(ii),FORMAT='(I4)') + "]"
    endfor
    str(n_elements(junk)+1)="These values have been replaced by the value 0.0."
    ret=NonModalMessage(str, POSITIONING_PARENT=top_base, GROUP=top_base)
    image( x_bin, y_bin ) =  0.0
  endif

  state.image_area = n_elements(image)
  state.image_sum  = total(image)

  ;; Display the new image by passing it to pass1_image_viewer.
  dummy = pass1_image_viewer( image, ID=state.pass1_image_viewer, $
			TITLE=state.title, _EXTRA=extra )

  ;; Inform the ROI's that the data has changed.
  dummy = pass1_image_viewer_roi( ID=state.roi1 )
  dummy = pass1_image_viewer_roi( ID=state.roi2 )

  ;; Display statistics.
  ccd_viewer_show_stats, state, top_base

endif ;image parameter supplied



;; Save the state structure.
pass1_image_viewer = state.pass1_image_viewer
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

;; If these widgets are realized, then simulate an event from the pass1_image_viewer
;; to get the ROI overlay's drawn.
if (widget_info(pass1_image_viewer, /REALIZED)) then begin
  event = {ID:pass1_image_viewer, TOP:top_base, HANDLER:top_base} 
  dummy = CcdViewerEventFn( event )
endif

return
end


;==========================================================================
;;; CCD Viewer statistics display routine
;==========================================================================
PRO ccd_viewer_show_stats, State, top_base

widget_control, State.pass1_image_viewer, GET_VALUE=iv
widget_control, State.backgrnd_value, $
	SET_VALUE='Sky Lvl' +string(iv.sky_level,f='(G9.3)')

widget_control, State.backgrnd_stddev, $
	SET_VALUE='Sky Sig' +string(iv.sky_stddev,f='(G9.3)')

case State.StatGroupId of
 0: $
  begin
  Min    = iv.image_min
  Max    = iv.image_max
  area   = state.image_area
  sum   = state.image_sum
  end

 1: $
  begin
  widget_control, State.roi1, SET_VALUE={evaluate_roi:1}
  widget_control, State.roi1, GET_VALUE=struct
  Min    = struct.roi_min
  Max    = struct.roi_max
  area   = struct.roi_area
  sum    = struct.roi_sum
  end

 2: $
  begin
  widget_control, State.roi2, SET_VALUE={evaluate_roi:1}
  widget_control, State.roi2, GET_VALUE=struct
  Min    = struct.roi_min
  Max    = struct.roi_max
  area   = struct.roi_area
  sum    = struct.roi_sum
  end
endcase

widget_control, top_base, UPDATE=0
widget_control, State.MinLabel,   SET_VALUE='Min ' +string(Min,f='(G12.6)')
widget_control, State.MaxLabel,   SET_VALUE='Max ' +string(Max,f='(G12.6)')


widget_control, State.area_label, SET_VALUE='Area' +string(area,f='(G12.6)')
widget_control, State.sum_label,  SET_VALUE='Sum ' +string(sum,f='(G12.6)')
widget_control, State.mean_label, SET_VALUE='Mean' +string(sum/area,f='(G12.6)')
widget_control, top_base, UPDATE=1

return
end

;==========================================================================
;;; Widget Event Handler Procedure
;;; Called when widget on top level.
;==========================================================================
FUNCTION CcdViewerEventFn, Event

widget_control, /HOURGLASS

;; Get the pass1_ccd_viewer state structure.
top_base = Event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY


;; Process the event.

DestroyFlag      = 0
display_roi_flag = 0

case Event.ID of

;--------------------------------------------------------------------------
; Image viewer widget events (ROI annotations need to be redrawn).
  state.pass1_image_viewer: display_roi_flag = 1

;--------------------------------------------------------------------------
; ROI definition changed.
  state.roi1: $
   begin
   ;; Display the statistics in case this ROI is chosen.
   ccd_viewer_show_stats, state, top_base

   ;; Ask image_viewer to redraw the image, then redraw ROI's. 
   dummy =pass1_image_viewer( ID=state.pass1_image_viewer )
   display_roi_flag = 1
   end

;--------------------------------------------------------------------------
; ROI definition changed.
  state.roi2: $
   begin
   ;; Display the statistics in case this ROI is chosen.
   ccd_viewer_show_stats, state, top_base

   ;; Ask image_viewer to redraw the image, then redraw ROI's. 
   dummy =pass1_image_viewer( ID=state.pass1_image_viewer )
   display_roi_flag = 1
   end


;--------------------------------------------------------------------------
; Centroid button
  state.centroid_button: $
   begin
   if (state.centroid_msg_id EQ 0) then begin
     txt=[$
'A centroid and three different sample standard deviations will be computed',$
'for the pixels in Region-of-interest #1.', '',$
'Press the "Centroid" button again when you have defined ROI #1, i.e. you',$
'have specified spatial boundaries and a pixel threshold value.', '', $
'HINT: set both scaling limits equal to the Threshold Pixel Value used',$
'in ROI #1 to more clearly see which pixels are in the region-of-interest.']

     state.centroid_msg_id =  NonmodalMessage( txt, GROUP=top_base, $
					       POSITIONING_PARENT=top_base, $
			                       TITLE='Centroid Instructions' )
   endif else begin
     ;Compute the centroid

     widget_control, State.pass1_image_viewer, GET_VALUE=iv
     handle_value, iv.image_handle, image

     widget_control, State.roi1, SET_VALUE={evaluate_roi:1}
     widget_control, State.roi1, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes

     if (n_elements(roi_indexes) EQ 0) then begin
       dummy = dialog_message( 'ROI #1 is empty!', $
       			       DIALOG_PARENT=state.centroid_button, /ERROR )
     endif else begin

       roi_data = image( roi_indexes )

       ; Compute the (X,Y) coordinates of the pixels
       x_dim   = iv.image_dims(0)
       x_coord = roi_indexes mod x_dim
       y_coord = roi_indexes  /  x_dim

       ; Compute the centroid
       x_cen = total( roi_data * x_coord ) / total(roi_data)
       y_cen = total( roi_data * y_coord ) / total(roi_data)

       ; Compute the second moments
       sqr_distance = (x_coord-x_cen)^2 + (y_coord-y_cen)^2
       variance     = total( roi_data * sqr_distance ) / $
		      total(roi_data)
       std_dev      = sqrt( variance )

       sqr_distance = (x_coord-x_cen)^2
       variance     = total( roi_data * sqr_distance ) / $
		      total(roi_data)
       std_dev_x    = sqrt( variance )

       sqr_distance = (y_coord-y_cen)^2
       variance     = total( roi_data * sqr_distance ) / $
		      total(roi_data)
       std_dev_y    = sqrt( variance )

       ; Convert the index position (x_cen,y_cen) to DATA coordinates.

       ; Display the results
       widget_control, state.pass1_image_viewer, GET_VALUE=iv

       handle_value, iv.x_handle, [x_cen], /SET
       handle_value, iv.y_handle, [y_cen], /SET
       widget_control, state.pass1_image_viewer, SET_VALUE= {index_to_data_flag:1}
       handle_value, iv.x_handle, x_cen_data
       handle_value, iv.y_handle, y_cen_data

       message = [$
string( x_cen_data, y_cen_data, f='("CENTROID: (",G12.6,", ",G12.6,")")' ),$
string( std_dev,   f='("2-D SAMPLE STANDARD DEVIATION:         ",G10.4)' ),$
string( std_dev_x, f='("1-D SAMPLE STANDARD DEVIATION, X-axis: ",G10.4)' ),$
string( std_dev_y, f='("1-D SAMPLE STANDARD DEVIATION, Y-axis: ",G10.4)' ),$
'',$
'The three sample standard deviations are the square-root of second',$
'moments (about the centroid) of the distribution of light.',$
'The 2-D version uses radial distances from the centroid.',$
'The 1-D versions use distances along the X and Y axes.']

       if (widget_info( state.centroid_msg_id, /VALID_ID )) then $
         widget_control, state.centroid_msg_id, /DESTROY

       state.centroid_msg_id = NonmodalMessage( message, GROUP=top_base, $
					       POSITIONING_PARENT=top_base, $
					TITLE='Centroid Results' )

       print
       for ii=0,n_elements(message)-1 do print, message(ii)

       ;; Setup ROI #2 for encircled energy.
       roi2_val = { center:[x_cen,y_cen], threshold:struct.threshold, $
		    inner_radius:0, outer_radius:(5 > struct.outer_radius) } 
       widget_control, State.roi2, SET_VALUE=roi2_val

       ; Move the marker if requested.
       if (event.value EQ 1) then begin
	 ;; Change the marker position and tv scaling of the pass1_image_viewer.
         widget_control, State.pass1_image_viewer, $
		 SET_VALUE= { marker_index:[x_cen, y_cen] }
       endif ;marker to be moved

       ;; Ask pass1_image_viewer to redraw the image, then redraw ROI's. 
       dummy =pass1_image_viewer( ID=state.pass1_image_viewer )
       display_roi_flag = 1

     endelse ;ROI was not empty
   endelse ;compute centroid
   end

;--------------------------------------------------------------------------
; Encircled Energy button
  state.encircled_button: $
   begin
   if (state.encirc_msg_id EQ 0) then begin
     txt=[$
'Encircled energy and radial surface brightness will be plotted for the',$
'pixels in Region-of-interest #2 which must be an annulus.',$
'In most situations, you should FIRST perform the centroid computation,',$
'which will automatically setup ROI #2 as an annulus centered on the centroid',$
'using the same pixel threshold value.','',$
'Press the Radial Distribution button again when you have configured ROI #2.',$
'', $
'HINT: set both scaling limits equal to the Threshold Pixel Value used',$
'in ROI #2 to more clearly see which pixels are in the region-of-interest.']

     state.encirc_msg_id = NonmodalMessage( txt, GROUP=top_base, $
					       POSITIONING_PARENT=top_base, $
				        TITLE='Encircled Energy Instructions' )
   endif else begin
     ;Compute the encircled energy

     widget_control, State.roi2, GET_VALUE=struct
     x_cen = struct.center(0)
     y_cen = struct.center(1)

     start_radius = 2.0
     stepsize     = 1.0
     num_points   = 1 + round((struct.outer_radius - start_radius) / stepsize)
     num_points   = 2 > num_points

     radii    = fltarr(num_points)
     energies = fltarr(num_points)
     areas    = fltarr(num_points)

     for ii = 0, num_points-1 do begin
       outer_radius = start_radius + stepsize*ii

       struct = { inner_radius:0, outer_radius:outer_radius, evaluate_roi:1 } 
       widget_control, State.roi2, SET_VALUE=struct
       widget_control, State.roi2, GET_VALUE=struct

       if (outer_radius NE struct.outer_radius) then message,'BUG!'

       radii(ii)    = struct.outer_radius
       energies(ii) = struct.roi_sum
       areas(ii)    = struct.roi_area
     endfor

     ; Display the results
     total_energy = energies(num_points-1)

     tit = string( state.Title, x_cen, y_cen, $
		   f='(A0, ": Radial distribution about (",F7.1,F8.1,")")' )

     subt = string( total_energy, f='("Pixel sum at largest radius: ",G10.4)' )

     plot_viewer, ID1, radii, 100*energies/total_energy, $
		       GROUP=top_base, WIDGET_TITLE=state.Title,$
		       TITLE=tit, SUBTITLE=subt,$
		       YTITLE='Sum of Encircled Pixel Values (%)',$
		       XTITLE='Radius (pixels)'
		       
     annular_energies = energies(1:num_points-1) - energies(0:num_points-2)
     annular_areas    =    areas(1:num_points-1) -    areas(0:num_points-2)
     annular_centers  =   (radii(1:num_points-1) +    radii(0:num_points-2))/2.

     plot_viewer, ID2, annular_centers, annular_energies/annular_areas,$
		       GROUP=top_base, WIDGET_TITLE=state.Title, TITLE=tit,$
		       YTITLE='Radial Surface Brightness in Annulus',$
		       XTITLE='Mid-radius of Annulus (pixels)', PSYM=10
   endelse
   end


;--------------------------------------------------------------------------
; Marginal Sums button
  state.marginal_sum_button: $
   begin
   if (state.sum_msg_id EQ 0) then begin
     txt=[$
'The "X marginal sum" (add up pixels in each column) and',$
'the "Y marginal sum" (add up pixels in each row) will be computed for', $
'the pixels in Region-of-interest #1.', '',$
'Press the "Marginal Sums" button again when you have defined ROI #1,',$
'i.e. you have specified spatial boundaries and a pixel threshold value.','', $
'HINT: set both scaling limits equal to the Threshold Pixel Value used',$
'in ROI #1 to more clearly see which pixels are in the region-of-interest.']

     state.sum_msg_id =  NonmodalMessage( txt, GROUP=top_base, $
					       POSITIONING_PARENT=top_base, $
		                       TITLE='Marginal Sum Instructions' )
   endif else begin
     ;Compute the marginal sums

     widget_control, State.pass1_image_viewer, GET_VALUE=iv
     handle_value, iv.image_handle, image

     widget_control, State.roi1, SET_VALUE={evaluate_roi:1}
     widget_control, State.roi1, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes

     if (n_elements(roi_indexes) EQ 0) then begin
       dummy = dialog_message( 'ROI #1 is empty!', $
       			       DIALOG_PARENT=state.marginal_sum_button, /ERROR )
     endif else begin

       ; Construct an image that is zero except in the ROI.
       roi_data = fltarr( iv.image_dims(0), iv.image_dims(1) )
       roi_data( roi_indexes ) = image( roi_indexes )


       ; Compute the X marginal sums, convert the column & row indexes into 
       ; DATA coordinates, and display.
       x_sum = total( roi_data, 2 )
       y_sum = total( roi_data, 1 )

       handle_value, iv.x_handle, indgen( iv.image_dims(0) ), /SET
       handle_value, iv.y_handle, indgen( iv.image_dims(1) ), /SET
       widget_control, state.pass1_image_viewer, SET_VALUE= {index_to_data_flag:1}
       handle_value, iv.x_handle, x_index
       handle_value, iv.y_handle, y_index

       plot_viewer, ID1, x_index, x_sum, $
		         GROUP=top_base, WIDGET_TITLE=state.Title,$
			 SUBTITLE=state.title, $
		         TITLE='X Marginal Sum in Region-of-interest', $
		         YTITLE='Sum of ROI Pixels Along Columns',$
		         XTITLE=iv.xtitle, PSYM=10

       plot_viewer, ID2, y_index, y_sum, $
		         GROUP=top_base, WIDGET_TITLE=state.Title,$
			 SUBTITLE=state.title, $
		         TITLE='Y Marginal Sum in Region-of-interest', $
		         YTITLE='Sum of ROI Pixels Along Rows',$
		         XTITLE=iv.ytitle, PSYM=10

     endelse ;ROI was not empty
   endelse ;compute marginal sums
   end

;--------------------------------------------------------------------------
; Cuts menu.
  state.CutsMenu: $
   begin
   ; Retrieve the image array and marker coordinates.
   widget_control, state.pass1_image_viewer, GET_VALUE=iv
   handle_value, iv.image_handle, image


   ; Plot a cut through the full image.
   case Event.value of
    1: begin
       ; Extract the cut.
       marker_y      = iv.marker_index(1)
       vertical_axis = image(*, marker_y)

       ; Convert marker_y and the column indexes into DATA coordinates to 
       ; make the title and horizontal axis vector for the plot.
       handle_value, iv.x_handle, indgen( iv.image_dims(0) ), /SET
       handle_value, iv.y_handle, marker_y, /SET
       widget_control, state.pass1_image_viewer, SET_VALUE= {index_to_data_flag:1}
       handle_value, iv.x_handle, horizontal_axis
       handle_value, iv.y_handle, marker_y

       pv_title  = string( state.title, iv.ytitle, marker_y,$
			   f='(A0,": Horizontal cut through {",A0,"} = ",I0)' )
       pv_ytitle = iv.ztitle
       pv_xtitle = iv.xtitle
       end

    2: begin
       ; Extract the cut.
       marker_x      = iv.marker_index(0)
       vertical_axis = image(marker_x, *)

       ; Convert marker_x and the row indexes into DATA coordinates to 
       ; make the title and the horizontal axis vector for the plot.
       handle_value, iv.x_handle, marker_x, /SET
       handle_value, iv.y_handle, indgen( iv.image_dims(1) ), /SET
       widget_control, state.pass1_image_viewer, SET_VALUE= {index_to_data_flag:1}
       handle_value, iv.x_handle, marker_x
       handle_value, iv.y_handle, horizontal_axis

       pv_title  = string( state.title, iv.xtitle, marker_x,$
			   f='(A0,": Vertical cut through {",A0,"} = ",I0)' )
       pv_ytitle = iv.ztitle
       pv_xtitle = iv.ytitle
       end
   endcase

   if (datatype(vertical_axis,1) EQ 'Double') then $
     vertical_axis = float( temporary(vertical_axis) )

   plot_viewer, ID,horizontal_axis, vertical_axis, GR=top_base, TIT=pv_title, $
		    XTIT=pv_xtitle, YTIT=pv_ytitle, WIDGET_TITLE=state.Title
   end

;--------------------------------------------------------------------------
; Stats droplist.
  state.stats_droplist: $
   begin
   ; Redraw stats label widgets.
   state.StatGroupId = event.index 
   ccd_viewer_show_stats, state, top_base
   end

;--------------------------------------------------------------------------
; Histogram button.
  state.HistButton: $
   ; Display a histogram of full CCD or ROI pixels.
   begin
   widget_control, State.pass1_image_viewer, GET_VALUE=iv
   handle_value, iv.image_handle, image

   case State.StatGroupId of
    0: $
     begin
     pixel_data = temporary( image )
     title = 'Histogram of Full Image'
     end

    1: $
     begin
     widget_control, State.roi1, SET_VALUE={evaluate_roi:1}
     widget_control, State.roi1, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes
     if (n_elements(roi_indexes) GT 0) then pixel_data = image( roi_indexes )
     title = 'Histogram of ROI #1'
     end

    2: $
     begin
     widget_control, State.roi2, SET_VALUE={evaluate_roi:1}
     widget_control, State.roi2, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes
     if (n_elements(roi_indexes) GT 0) then pixel_data = image( roi_indexes )
     title = 'Histogram of ROI #2'
     end
   endcase

   if (n_elements(pixel_data) GT 0) then begin

     if (datatype(pixel_data,1) EQ 'Double') then $
       pixel_data = float( temporary(pixel_data) )

     histogram_viewer, ID, pixel_data, GROUP=top_base, TITLE=title, $
			   WIDGET_TITLE=state.Title, /AUTOMIN, /AUTOBIN, $
                           XTIT=iv.ztitle,  YTIT='Number of Pixels'
   endif
   end

;--------------------------------------------------------------------------
 state.DoneButton: DestroyFlag = 1

;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in pass1_ccd_viewer'
       help,/ST,Event
       end
endcase


if (display_roi_flag) then begin
   color_manager, GREEN=green, BLUE=blue

   widget_control, state.roi1, GET_VALUE=struct
   handle_value, struct.x_overlay1, x
   handle_value, struct.y_overlay1, y
   if (n_elements(x) GT 0) then $
     dummy = pass1_image_viewer( ID=state.pass1_image_viewer, $
			   X_OVERLAY=x, Y_OVERLAY=y, COLOR_OVERLAY=blue )

   handle_value, struct.x_overlay2, x
   handle_value, struct.y_overlay2, y
   if (n_elements(x) GT 0) then $
     dummy = pass1_image_viewer( ID=state.pass1_image_viewer, $
			   X_OVERLAY=x, Y_OVERLAY=y, COLOR_OVERLAY=blue )

   widget_control, state.roi2, GET_VALUE=struct
   handle_value, struct.x_overlay1, x
   handle_value, struct.y_overlay1, y
   if (n_elements(x) GT 0) then $
     dummy = pass1_image_viewer( ID=state.pass1_image_viewer, $
			   X_OVERLAY=x, Y_OVERLAY=y, COLOR_OVERLAY=green )

   handle_value, struct.x_overlay2, x
   handle_value, struct.y_overlay2, y
   if (n_elements(x) GT 0) then $
     dummy = pass1_image_viewer( ID=state.pass1_image_viewer, $
			   X_OVERLAY=x, Y_OVERLAY=y, COLOR_OVERLAY=green )
endif

; DONE button
if (DestroyFlag) then handle_free,state.ccd_viewer_handle

;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

; DONE button
if (DestroyFlag) then widget_control, top_base, /DESTROY

return, 0
end


;==========================================================================
;;; Widget Event Handler Procedure
;==========================================================================
PRO CcdViewerEvent, Event

event = CcdViewerEventFn( Event )
return
end


;==========================================================================
;;; Widget Creation 
;==========================================================================
FUNCTION pass1_ccd_viewer, image, ID=id, PARENT=parent, TITLE=title, $
		      UVALUE=uvalue, GROUP=group, BLOCK=block, _EXTRA=extra, HANDLE=p_handle

;; Preliminary stuff

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(group)  then group = 0
if NOT keyword_set(title)  then title=''

;; If the user supplies a widget ID, then he wants to modify an existing
;; pass1_ccd_viewer widget, not create a new one.
if (0 NE n_elements(id)) then begin
  ModifyCcdViewer, id, image, _EXTRA=extra, TITLE=title
  return, id
endif

if (NOT keyword_set(p_handle)) then begin
  ccd_viewer_handle = handle_create()
endif else ccd_viewer_handle = handle_create(p_handle)


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

if keyword_set(Parent) then $
 top_base = widget_base(parent, UVALUE=uvalue, /COLUMN, $
                        SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_RIGHT, $
                        EVENT_FUNC    ='CcdViewerEventFn') $
else begin
 center = ScreenCenter()
 xoffset = center(0) - 250
 yoffset = center(1) - 375
 top_base = widget_base(TITLE=title, UVALUE=uvalue, /COLUMN, $
                        SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_RIGHT, $
		        XOFFSET=xoffset, YOFFSET=yoffset, GROUP_LEADER = group)
endelse

 pass1_image_viewer = pass1_image_viewer( PARENT=top_base, HANDLE=ccd_viewer_handle )

 bottom_base = widget_base(top_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

  roi1 = pass1_image_viewer_roi( bottom_base, pass1_image_viewer, $
			  TITLE='ROI1' )

  roi2 = pass1_image_viewer_roi( bottom_base, pass1_image_viewer, $
			  TITLE='ROI2' )


  StatsBase = widget_base( bottom_base, /COLUMN, FRAME=2, $
			   /BASE_ALIGN_CENTER, SPACE=1, XPAD=1, YPAD=1 )

;   label           = widget_label(StatsBase, VALUE='STATISTICS')

    stats_droplist = widget_droplist(StatsBase, $
				     VALUE=['Full Image','ROI #1','ROI #2'])
    widget_control, stats_droplist, SET_DROPLIST_SELECT=0


    sum_label = widget_label(StatsBase, VALUE='Sum', /DYNAMIC_RESIZE)

    area_label = widget_label(StatsBase, VALUE='Area', /DYNAMIC_RESIZE)

    mean_label = widget_label(StatsBase, VALUE='Mean', /DYNAMIC_RESIZE)

    MaxLabel = widget_label(StatsBase, VALUE='Max', /DYNAMIC_RESIZE)

    MinLabel = widget_label(StatsBase, VALUE='Min', /DYNAMIC_RESIZE)

    HistButton = widget_button(StatsBase, VALUE='Pixel Histogram')


  PullDownBase = widget_base( bottom_base, /COLUMN, $
			   /BASE_ALIGN_CENTER, SPACE=1, XPAD=1, YPAD=1 )

   backgrnd_value  = widget_label(PullDownBase, /DYNAMIC_RESIZE, /ALIGN_LEFT )

   backgrnd_stddev = widget_label(PullDownBase, /DYNAMIC_RESIZE, /ALIGN_LEFT )

   Menu = [{ CW_PDMENU_S, flags:3, name:'Centroid' }, $
	   { CW_PDMENU_S,       0, 'Move marker to centroid' }, $
	   { CW_PDMENU_S,       2, 'Do not move marker' }  ]

   centroid_button = cw_pdmenu(PullDownBase, Menu, /RETURN_INDEX)

   encircled_button = widget_button(PullDownBase, VALUE='Radial Profile' )

   marginal_sum_button = widget_button(PullDownBase, VALUE='Marginal Sums')

   Menu = [{ CW_PDMENU_S, flags:3, name:'Cuts' }, $
	   { CW_PDMENU_S,       0,      'Horizontal' }, $
	   { CW_PDMENU_S,       2,      'Vertical' }  ]

   CutsMenu = cw_pdmenu(PullDownBase, Menu, /RETURN_INDEX)


   if (0 NE n_elements(parent)) then begin
     DoneButton = 0L
   endif else begin
     DoneButton = widget_button(PullDownBase, VALUE='Dismiss' )
   endelse



;; Initialize widgets and state structure.

state = { $
	;IDs of widgets that generate events.
	pass1_image_viewer:pass1_image_viewer, roi1:roi1, roi2:roi2, $

	centroid_button:centroid_button, encircled_button:encircled_button,$
	marginal_sum_button:marginal_sum_button, $
	CutsMenu:CutsMenu, $
	stats_droplist:stats_droplist, HistButton:HistButton, $
	DoneButton:DoneButton, $

	;IDs of labels that will change.
	sum_label:sum_label, $
	MaxLabel:MaxLabel, backgrnd_value:backgrnd_value, MinLabel:MinLabel, $
	backgrnd_stddev:backgrnd_stddev, area_label:area_label, $
	mean_label:mean_label, $

        ;ID to indicate which sets of statistics to display.
	StatGroupId: 0, $
	
	;Other stuff
	ccd_viewer_handle:ccd_viewer_handle, $
	centroid_msg_id:0L, encirc_msg_id:0L, sum_msg_id:0L, $
	Title:Title, $

	;Full image statistics
	image_area:0.0, image_sum:0.0 }

; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state

;; If this is top-level widget, realize it, draw it, and register.
if NOT keyword_set(Parent) then begin
  widget_control, top_base, /REALIZE

  ; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
  ; widget to behave when called by either widget applications or normal
  ; programs (which block on tty reads).
  xmanager, 'pass1_ccd_viewer', top_base, GROUP_LEADER=Group, $
	    JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0), $
	    EVENT_HANDLER='CcdViewerEvent' 
endif

;; Pass the image to ModifyCcdViewer so it can be stored and drawn.
ModifyCcdViewer, top_base, image, _EXTRA=extra

return, top_base
end

