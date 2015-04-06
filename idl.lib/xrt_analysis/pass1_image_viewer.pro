;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_image_viewer.pro	9.6
;;;
;;; DESCRIPTION:  Image Viewer Compound Widget
;;;
;;;               Used to display an image and zoomed window with control
;;;               over display scaling.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;;
;;; INTERFACE TO CLIENT
;;;
;;;FUNCTION pass1_image_viewer, image, ID=id, PARENT=parent, TITLE=title, $
;;;			UVALUE=uvalue, GROUP=group, $
;;;			COLORTABLEFILE=colortable_file, $
;;;			INDEX_TO_DATA_GAIN=index_to_data_gain,$
;;;			INDEX_TO_DATA_OFFSET=index_to_data_offset,$
;;;		        XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle,$
;;;			X_OVERLAY=x_overlay, Y_OVERLAY=y_overlay, $
;;;			COLOR_OVERLAY=color_overlay

;;; The parameters PARENT, UVALUE, & GROUP are the usual ones associated with
;;; IDL widgets.  If plot_viewer is a top-level widget, then TITLE
;;; controls the title in the X-window frame.

;;; The HANDLE keyword is used to specify the parent of the pass1_image_viewer_handle,
;;; which is always created.  Specifying a parent handle will cause all image
;;; viewer handles to be freed whenever the parent handle is freed.  Image
;;; Viewer is not meant to be run as a standalone tool (a top base widget),
;;; so there is no call to handle_free() in this routine.

;;; COLORTABLEFILE is the path name of the user's personal color table.
;;; THIS DOESN'T WORK YET!!

;;; When "image" is supplied, the caller can define a DATA coordinate system 
;;; in which the image will be drawn using the keywords INDEX_TO_DATA_GAIN
;;; and INDEX_TO_DATA_OFFSET.
;;; The transformation from "index coordinates" of the image array to the 
;;; user's DATA coordinate system is:
;;;    data_x = index_x * INDEX_TO_DATA_GAIN(0) + INDEX_TO_DATA_OFFSET(0)
;;;    data_y = index_y * INDEX_TO_DATA_GAIN(1) + INDEX_TO_DATA_OFFSET(1)
;;; In other words the DATA coordinates of the lower-left pixel are
;;; [INDEX_TO_DATA_OFFSET(0), INDEX_TO_DATA_OFFSET(1)].

;;; Vectors passed in the OVERLAY keywords describe lines that should
;;; be drawn over the images using the IDL routine PLOTS.
;;; The COLOR_OVERLAY keyword controls the color of the overlay graphics.
;;; These vectors are in the "index coordinate system" of the image array.
;;; A parent can draw several sets of lines using the ID keyword, e.g.
;;;	
;;;     pass1_image_viewer = pass1_image_viewer(image, PARENT=parent_id)
;;;	dummy = pass1_image_viewer( ID=pass1_image_viewer, X_OVERLAY=x1, Y_OVERLAY=y1 )
;;;	dummy = pass1_image_viewer( ID=pass1_image_viewer, X_OVERLAY=x2, Y_OVERLAY=y2 )

;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).
;;; The image array that is to be displayed is stored using an IDL handle
;;; which is found in the state structure.


;==========================================================================
;;; VALUE PROPERTY
;;; The VALUE property of this widget, when READ (GET_VALUE), is a structure 
;;; containing the following elements:

;;;   image_handle:  handle on which the image array is stored
;;;   image_dims:    (X,Y) dimensions of the image array
;;;   marker_index:  (X,Y) coordinates of the marker in index coord system
;;;   image_min:     minimum value in the image
;;;   image_max:     maximum value in the image
;;;   sky_level:     estimated background level in the image
;;;   sky_stddev:    estimated standard deviation of the sky level
;;;   tvlow:         lower cutoff of display scaling
;;;   tvhigh:        upper cutoff of display scaling
;;;   xtitle:        title of X-axis
;;;   ytitle:        title of Y-axis
;;;   ztitle:        title of "pixel axis"

;;;   x_handle:	     handle to a vector of X-axis values (see note below)
;;;   y_handle:	     handle to a vector of Y-axis values (see note below)


;;; The VALUE property of this widget, when WRITTEN (SET_VALUE), is a
;;; structure containing the following elements, all of which are optional.  

;;; The tag "index_to_data_flag" is  
;;; used when the caller wishes pass1_image_viewer to provide coordinate 
;;; transformation services between the DATA and INDEX coordinate systems 
;;; (described above).  
;;; If the tag "index_to_data_flag" is supplied, then the vectors of 
;;; coordinates currently stored on the handles x_handle & y_handle (above)
;;; are transformed from one coordinate system to another.  The boolean
;;; value of the tag index_to_data_flag determines the direction of the
;;; transformation (index_to_data or data_to_index).
;;; The procedure the caller follows is: first do a GET_VALUE call to get
;;; the x_handle and y_handle, then write vectors of coordinates to those
;;; handles, then do a SET_VALUE call that includes index_to_data_flag,
;;; then retrieve the transformed vectors from then handles.

;;;  [marker_index]:  (X,Y) coordinates of the marker in index coord system
;;;  [tvlow]:         lower cutoff of display scaling
;;;  [tvhigh]:        upper cutoff of display scaling
;;;  [index_to_data_flag]: SET if the desired coordinate transformation is 
;;;			  index-to-data, CLEAR if it is data-to-index


;==========================================================================
;;; EVENTS GENERATED
;;; This widget generates an event for its parent whenever the full-size or
;;; zoomed images are redisplayed.  This allows the parent to redraw any
;;; annotations required using the X_OVERLAY and Y_OVERLAY keywords.


;==========================================================================
;;; TYPICAL USAGE

;;; This widget can be used in three distint ways.

;;; First, it can be created as a child of an unrealized base widget by
;;; supplying the keyword parameter PARENT, just like one of the compound
;;; widgets in the user's library.
;;; The pass1_image_viewer widget will be realized when the parent is realized.
;;; The parent widget is responsible for asking the pass1_image_viewer widget
;;; to display the image for the first time by SENDing an event to the
;;; pass1_image_viewer or by passing in the image after everything is realized.
;;; The pass1_ccd_viewer uses pass1_image_viewer in this manner.

;;; For example:
;;;
;;;     pass1_image_viewer = pass1_image_viewer( PARENT=parent_id)
;;;     widget_control, parent_id, /REALIZE
;;;	dummy = pass1_image_viewer( image, ID=pass1_image_viewer )

;;; Second, the event handler of a realized widget can create a top level
;;; image viewer that will be managed independently by the XMANAGER by
;;; omitting the parameter PARENT.
;;; The image viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  

;;; For example:
;;;
;;;     pass1_image_viewer = pass1_image_viewer(image, GROUP=creator_id)

;;; Third, a pass1_image_viewer may be created at the command line or by a regular
;;; IDL routine (not an event handler) by omitting the parameter PARENT.
;;; The pass1_image_viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  An explicit call to XMANAGER must then be made to start
;;; processing events.
;;; For example:
;;;
;;;     pass1_image_viewer = pass1_image_viewer(image)
;;;     xmanager
;-
;==========================================================================

FORWARD_FUNCTION ImageViewEventFn

;==========================================================================
;;; This routine is called to change the image that is displayed and to
;;; draw overlay graphics.
;==========================================================================
PRO ModifyImageViewer, top_base, image, TITLE=title, $
			INDEX_TO_DATA_GAIN=index_to_data_gain,$
			INDEX_TO_DATA_OFFSET=index_to_data_offset,$
		        XTITLE=xtitle, YTITLE=ytitle, ZTITLE=ztitle,$
			X_OVERLAY=x_overlay, Y_OVERLAY=y_overlay, $
 			COLOR_OVERLAY=color_overlay

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

if (n_elements(title) NE 0) then state.title = title

;; Initialize widgets and state structure based on new image array.
if (0 NE n_elements(image)) then begin

  ;; Handle the DEVICE-to-DATA coordinate system transformation keywords.
  if (2 EQ n_elements(index_to_data_gain))   then $
    state.index_to_data_gain   = index_to_data_gain

  if (2 EQ n_elements(index_to_data_offset)) then $
    state.index_to_data_offset = index_to_data_offset

  if (0 NE n_elements(xtitle)) then state.xtitle = xtitle
  if (0 NE n_elements(ytitle)) then state.ytitle = ytitle
  if (0 NE n_elements(ztitle)) then state.ztitle = ztitle


  ;; Save image to a handle.
  handle_value, state.image_handle, image, /SET

  S = size(image)
  x_dim = S(1)  &  y_dim = S(2)
  type_code = S( S(0) + 1 )

  state.dims = [x_dim, y_dim]
  state.integer_image_flag = $
		      (type_code EQ 1 OR type_code EQ 2 OR type_code EQ 3)


  ;------------------------------------------------------------------------
  ;; Resize the normal-view draw widget and its pixmap, trying to match 
  ;; the aspect ratio.
  ;------------------------------------------------------------------------
  geometry = widget_info( state.draw_ids(0), /GEOMETRY )
  new_ysize = 300 < ((geometry.draw_xsize * y_dim) / x_dim) > 150

  if (new_ysize NE geometry.draw_ysize) then begin
    widget_control, state.draw_ids(0),$
		    XSIZE=geometry.draw_xsize, YSIZE=new_ysize

    if (state.pixmap_nums(0) NE -1) then begin
       wdelete, state.pixmap_nums(0)
       state.pixmap_nums(0) = -1
    endif
  endif


  ;------------------------------------------------------------------------
  ;; Calculate and store image statistics.
  ;------------------------------------------------------------------------
  image_min          = min( image, MAX=image_max, /NaN )
  state.image_min    = image_min
  state.image_max    = image_max

  ; Compute the image value corresponding to each percentile from 0 to 100.
  ; Sample large images so that SORT() won't run forever!
  N = n_elements(image)
  if (N GT 2000) then begin
    sampled_image = congrid( reform(image,N), 1000 )
    s = sort( sampled_image )

    indexes = long( (n_elements(s)-1) * (indgen(101) / 100.0) )
    state.percentiles = sampled_image( s( indexes ) )
  endif else begin
    s = sort( image )

    indexes = long( (n_elements(s)-1) * (indgen(101) / 100.0) )
    state.percentiles = image( s( indexes ) )
  endelse


  ; We have to catch calls to MESSAGE and look for math errors in the call 
  ; to SKY.PRO since oddly valued images or really small images may mess up 
  ; the SKY or MMM routines.
  catch, error_code

  if (error_code EQ 0) then begin
    trash = check_math( 0, 1 )  ;clear previous errors and inhibit messages

    sky, image, skymode, skysig, /SILENT

    if (0 NE check_math( 0, 0 )) then begin  
      error_code = 1
      err_string = 'Math error detected.'
    endif
  endif else begin
    err_string = !ERR_STRING
  endelse

  catch, /CANCEL

  if (error_code EQ 0) then begin
    state.sky_level  = skymode
    state.sky_stddev = skysig
  endif else begin
    message = [$
        'The level and standard deviation could not be computed for the',$
	'background of this image.',$
	'{ ' + err_string + ' }']
    id = dialog_message( message, DIALOG_PARENT=top_base )

    state.sky_level  = state.percentiles(50)
    state.sky_stddev = 1.0
  endelse


  ;------------------------------------------------------------------------
  ;; Configure the widgets and update their appearance.
  ;------------------------------------------------------------------------
  if (state.use_defaults_flag) then begin
    state.use_defaults_flag = 0

    ; Set initial marker position, zoom factor, and display thresholds.
    state.zoom_center = state.dims/2
    widget_control, state.marker_index_x, SET_VALUE=state.zoom_center(0)
    widget_control, state.marker_index_y, SET_VALUE=state.zoom_center(1)

    state.mag  = 4
    widget_control, state.mag_mode, SET_DROPLIST_SELECT=2

    widget_control, state.low_slider,   SET_VALUE = 50
    widget_control, state.high_slider,  SET_VALUE = 95
    im_view_pcent_to_range, state, /LOW, /HIGH

  endif else begin
    ; Try to keep the existing marker position.
    widget_control, state.marker_index_x, GET_VALUE=marker_index_x
    widget_control, state.marker_index_y, GET_VALUE=marker_index_y
    marker_index = 0 > [marker_index_x,marker_index_y] < (state.dims - 1)

    widget_control, state.marker_index_x, SET_VALUE=marker_index(0)
    widget_control, state.marker_index_y, SET_VALUE=marker_index(1)

    ; Center the zoom on the marker.
    state.zoom_center = marker_index

    ; Keep the existing display thresholds, but re-compute the 
    ; corresponding percentiles.
    trash = where( image LE state.tvhigh, count )
    widget_control, state.high_slider, SET_VALUE=100*count/n_elements(image)

    trash = where( image LE state.tvlow, count )
    widget_control, state.low_slider,  SET_VALUE=100*count/n_elements(image)

  endelse

endif ;image parameter supplied


;; Annotate if requested, otherwise redraw the image.
if (widget_info(top_base, /REALIZED)) then begin

  if (keyword_set(x_overlay) and keyword_set(y_overlay)) then begin
    color_manager, WHITE=white
    if (n_elements(color_overlay) EQ 0) then color_overlay = white
    RedrawImageViewer, state, 0, $
	X_OVERLAY=x_overlay, Y_OVERLAY=y_overlay, COLOR_OVERLAY=color_overlay
    RedrawImageViewer, state, 1, $
	X_OVERLAY=x_overlay, Y_OVERLAY=y_overlay, COLOR_OVERLAY=color_overlay
  endif else begin
    RedrawImageViewer, state, 0, [0,0], state.dims-1
    RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag
  endelse
endif

;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return
end


;==========================================================================
;;; Widget SET_VALUE Routine
;==========================================================================
PRO ImageViewerSet, top_base, struct

redraw = 0

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

tag_names = tag_names( struct )

;; Assign the marker position
if ((where( tag_names EQ 'MARKER_INDEX' ))(0) NE -1) then begin
  marker_index_x = 0 > struct.marker_index(0) < (state.dims(0)-1)
  marker_index_y = 0 > struct.marker_index(1) < (state.dims(1)-1)

  ; Change marker state, but not zoom center.
  widget_control, state.marker_index_x, SET_VALUE=marker_index_x
  widget_control, state.marker_index_y, SET_VALUE=marker_index_y
  redraw = 1
endif


;; Assign the TV scaling fields 
if ((where( tag_names EQ 'TVHIGH' ))(0) NE -1) then begin
  state.tvhigh = struct.tvhigh
  state.tvlow  = struct.tvlow 
  redraw = 1
endif


;; Redraw if necessary
if (redraw) then begin
  RedrawImageViewer, state, 0, [0,0], state.dims-1
  RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag
endif


;; Provide coordinate tranformation services if requested.
if ((where( tag_names EQ 'INDEX_TO_DATA_FLAG' ))(0) NE -1) then begin
  handle_value, state.x_handle, x, /NO_COPY
  handle_value, state.y_handle, y, /NO_COPY

  if (struct.index_to_data_flag) then begin
    ; Convert from "index coordinates" to DATA coordinates.
    x = x * state.index_to_data_gain(0) + state.index_to_data_offset(0)
    y = y * state.index_to_data_gain(1) + state.index_to_data_offset(1)
  endif else begin
    ; Convert from DATA coordinates to "index coordinates".
    x = (x - state.index_to_data_offset(0)) / state.index_to_data_gain(0) 
    y = (y - state.index_to_data_offset(1)) / state.index_to_data_gain(1) 
  endelse

  handle_value, state.x_handle, x, /SET, /NO_COPY
  handle_value, state.y_handle, y, /SET, /NO_COPY
endif


;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY
return
end



;==========================================================================
;;; Widget GET_VALUE Routine
;==========================================================================
FUNCTION ImageViewerGet, top_base

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

;; Get the marker coordinates.
widget_control, state.marker_index_x, GET_VALUE=marker_index_x
widget_control, state.marker_index_y, GET_VALUE=marker_index_y

;; Create the return structure.
struct = { image_handle:state.image_handle, image_dims:state.dims, $
	   marker_index:[marker_index_x, marker_index_y], $ 
	   image_min:state.image_min, image_max:state.image_max, $
	   sky_level:state.sky_level, sky_stddev:state.sky_stddev, $
	   tvlow:state.tvlow, tvhigh:state.tvhigh, $
	   xtitle:state.xtitle, ytitle:state.ytitle, ztitle:state.ztitle, $
	   x_handle:state.x_handle, y_handle:state.y_handle }

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, struct
end


;==========================================================================
;;; Routine to convert histogram percentile limits to a data range.
;==========================================================================
PRO im_view_pcent_to_range, state, LOW=low, HIGH=high

widget_control, state.low_slider,  GET_VALUE=low_percent
widget_control, state.high_slider, GET_VALUE=high_percent

if (keyword_set(low))  then state.TvLow  = state.percentiles( low_percent )

if (keyword_set(high)) then state.TvHigh = state.percentiles( high_percent )

widget_control, state.low_limit,  SET_VALUE=state.TvLow
widget_control, state.high_limit, SET_VALUE=state.TvHigh

return
end

;==========================================================================
;;; Image Display Routine: RedrawImageViewer
;;; Displays a part of the image to the draw widget "draw_num", draws the 
;;; marker, &  updates the Z value display.

;;; If min_roi_index & max_roi_index are supplied, then the region-of-interest
;;;
;;; (min_roi_index(0):max_roi_index(0), min_roi_index(1):max_roi_index(1))
;;;
;;; is scaled and displayed.  If MAG is zero, then the ROI is magnified or
;;; reduced so that the ROI fills the plot data window.  If MAG is a positive
;;; integer, then the ROI is magnified by that factor.

;;; If min_roi_index & max_roi_index are NOT supplied, then the previously
;;; drawn plot data window is simply re-drawn with the marker in the new
;;; position.

;;; The X_OVERLAY & Y_OVERLAY keywords overplot line segments on the existing
;;; display -- they should be in the "index coordinate system" of the image 
;;; array.  

;;; If POSTSCRIPT is set, then the current graphics device is Postscript
;;; with COLOR disabled.  If COLOR_POSTSCRIPT is set, then the current
;;; graphics device is Postscript with COLOR enabled.
;==========================================================================
PRO RedrawImageViewer, state, draw_num, min_roi_index, max_roi_index, MAG=mag,$
     X_OVERLAY=x_overlay, Y_OVERLAY=y_overlay, COLOR_OVERLAY=color_overlay
     
postscript = (!D.NAME EQ 'PS')
color_manager, RED=red, BLACK=black, NEGATIVE_IMAGES=negative_images, NCOLORS=ncolors

widget_control, /HOURGLASS

;;; Even if we're trying to do Postscript output, we need to temporarily set 
;;; the device back to X so we can access the device parameters of the 
;;; draw widgets.
set_plot,'X'

coordsystem_manager, state.draw_ids(draw_num), /RESTORE


;--------------------------------------------------------------------------
;; If requested, draw overlay annotations and return.
if (keyword_set(x_overlay) AND keyword_set(y_overlay)) then begin

  ; Convert the positions (x_overlay, y_overlay) from "index coordinates"
  ; to DATA coordinates.
  x_data = x_overlay * state.index_to_data_gain(0) $
	             + state.index_to_data_offset(0)
  y_data = y_overlay * state.index_to_data_gain(1) $
	             + state.index_to_data_offset(1)

  ; Draw in the selected plot data window.
  plots, x_data, y_data, /DATA, THICK=2, LINESTYLE=0, COLOR=color_overlay
  return
endif


;--------------------------------------------------------------------------
;; Borrow the image array off the handle.
;; The handle_value routine has a quirk that if the array stored was Nx1
;; then the array returned in a VECTOR of length N, so we must correct
;; for that.
handle_value, state.image_handle, image, /NO_COPY
if ((size(image))(0) EQ 1) then image = reform(image, n_elements(image), 1)


;; Figure out if we're going to re-create the plot data window and rescale
;; the subimage, or just redisplay the existing one from cache.
if (2 EQ n_elements(min_roi_index) AND $
    2 EQ n_elements(max_roi_index)) then begin

;print, 'rescaling window', draw_num
  ;--------------------------------------------------------------------------
  ; Make sure the TvLow and TvHigh fields have sane values, based on the
  ; type of the image data.
  ; If the image array is byte/integer/long, then the TvLow and TvHigh vars
  ; need to have integer values because the bytscl() routine will convert
  ; MIN and MAX keywords to the type of the data parameter.
  if (state.integer_image_flag) then begin
    state.TvLow  = round(state.TvLow)
    state.TvHigh = round(state.TvHigh) 
  endif

  if (state.TvLow GE state.TvHigh) then begin
    if (state.integer_image_flag OR (state.TvHigh EQ 0)) then begin
      state.TvHigh = state.TvLow + 1
    endif else begin
      state.TvLow = state.TvHigh * 0.9999
    endelse
  endif

  widget_control, state.low_limit,  SET_VALUE=state.TvLow
  widget_control, state.high_limit, SET_VALUE=state.TvHigh


  ;--------------------------------------------------------------------------
  ; Calculate the size of the Plot Data Window in device coordinates.
  ; Determine a MAG value if none supplied.

  roi_size_index = 1 + max_roi_index - min_roi_index 

  if (draw_num EQ 0) then begin
    !X.MARGIN = [9,1]  &  !Y.MARGIN=[4,2]
    xtit = state.xtitle  &  ytit = state.ytitle
  endif else begin
    !X.MARGIN = [5,1]  &  !Y.MARGIN=[3,1]
    xtit = ''  &  ytit = ''
  endelse

  left_margin   = round( !D.X_CH_SIZE * !X.MARGIN(0) )
  right_margin  = round( !D.X_CH_SIZE * !X.MARGIN(1) )
  bottom_margin = round( !D.Y_CH_SIZE * !Y.MARGIN(0) )
  top_margin    = round( !D.Y_CH_SIZE * !Y.MARGIN(1) )

  plot_size_device = [!D.X_SIZE - left_margin - right_margin,$
		      !D.Y_SIZE - bottom_margin - top_margin]

  if (keyword_set(mag)) then begin
    ; Make the device dimensions of the Plot Data Window be an integral
    ; multiple of it's dimensions in index units.
    ; Mag is a positive integer here!
    mag              = 1 > fix(mag)

    plot_size_index  = (plot_size_device / mag)
    plot_size_device = mag * plot_size_index

  endif else begin
    ; Figure out how the ROI has to be scaled to fill the available space,
    ; keeping the aspect ratio unchanged.
    mag = min( float(plot_size_device) / roi_size_index )

    plot_size_index  = round(plot_size_device / mag)
    plot_size_device = round(mag * plot_size_index)

  endelse

  ;--------------------------------------------------------------------------
  ; Calculate the index coordinates of the lower-left and upper-right corners
  ;the Plot Data Window  so that the PDW is centered over the ROI.
  ; Don't forget about 1/2 index unit offset (draw a picture!).
  ll_plot_index = (min_roi_index - 0.5) - (plot_size_index - roi_size_index)/2

  ur_plot_index = ll_plot_index + plot_size_index

  ; Calculate the corners of the Plot Data Window in DATA coordinates.
  ll_plot_data = ll_plot_index * state.index_to_data_gain $
	         + state.index_to_data_offset
  ur_plot_data = ur_plot_index * state.index_to_data_gain $
	         + state.index_to_data_offset

  ;--------------------------------------------------------------------------
  ; Define the DATA coordinate system.
  XRANGE = [ll_plot_data(0), ur_plot_data(0)]
  YRANGE = [ll_plot_data(1), ur_plot_data(1)]

  if (postscript EQ 0) then begin
    ; We want to draw to the screen.
    POSITION = [left_margin, bottom_margin, $
		left_margin   + plot_size_device(0) - 1, $
		bottom_margin + plot_size_device(1) - 1]

    plot,[0,1],/NODATA, XTITLE=xtit, YTITLE=ytit, TITLE=state.title, $
      POSITION=POSITION, /DEVICE, COLOR=red, BACKGROUND=black,$
      XSTYLE=1, XRANGE=XRANGE, YSTYLE=1, YRANGE=YRANGE, TICKLEN=-0.02

    coordsystem_manager, state.draw_ids(draw_num), /SAVE

  endif else begin
    ; We want to draw to the Postscript device, keeping square pixels.
    plot_aspect = float(plot_size_device(0)) / plot_size_device(1)

    set_plot,'PS'

    left_margin   = round( !D.X_CH_SIZE * !X.MARGIN(0) )
    right_margin  = round( !D.X_CH_SIZE * !X.MARGIN(1) )
    bottom_margin = round( !D.Y_CH_SIZE * !Y.MARGIN(0) )
    top_margin    = round( !D.Y_CH_SIZE * !Y.MARGIN(1) )

    plot_size_device = [!D.X_SIZE - left_margin - right_margin,$
		        !D.Y_SIZE - bottom_margin - top_margin]

    temp = round( [plot_size_device(1) * plot_aspect, $
	           plot_size_device(0) / plot_aspect]  )

    plot_size_device = plot_size_device < temp 

    POSITION = [left_margin, bottom_margin, $
		left_margin   + plot_size_device(0) - 1, $
		bottom_margin + plot_size_device(1) - 1]

    plot,[0,1],/NODATA, XTITLE=xtit, YTITLE=ytit, TITLE=state.title, $
      POSITION=POSITION, /DEVICE, FONT=0, COLOR=red,$
      XSTYLE=1, XRANGE=XRANGE, YSTYLE=1, YRANGE=YRANGE, TICKLEN=-0.02

  endelse


  ;--------------------------------------------------------------------------
  ; Calculate the index and DATA coordinates of the region inside the PDW 
  ; that will actually display parts of the image array.  
  ll_region_index =          (0 - 0.5) > ll_plot_index 
  ur_region_index = (state.dims - 0.5) < ur_plot_index 

  ll_region_data = ll_region_index * state.index_to_data_gain $
		   + state.index_to_data_offset
  ur_region_data = ur_region_index * state.index_to_data_gain $
		   + state.index_to_data_offset

  ; Extract the sub-image that needs to be displayed.
  ; IDL's array indexing semantics has a quirk that if the array requested is
  ; Nx1 then the array returned in a VECTOR of length N, so we must correct
  ; for that.
  sub_array = image( ceil(ll_region_index(0)):floor(ur_region_index(0)),$
		     ceil(ll_region_index(1)):floor(ur_region_index(1)) )

  if ((size(sub_array))(0) EQ 1) then begin
    sub_array = reform(sub_array, n_elements(sub_array), 1)
  endif

  ;--------------------------------------------------------------------------
  ; Magnify or reduce the sub-image to fit the Plot Data Window.
  sub_x_dim = (size(sub_array))(1)  &  sub_y_dim = (size(sub_array))(2)

  mag_x_dim = round(sub_x_dim * mag)
  mag_y_dim = round(sub_y_dim * mag)

  if (mag EQ 1) then begin
    ; Do nothing

  endif else if ((mag_x_dim MOD sub_x_dim) EQ 0 AND $
		 (mag_y_dim MOD sub_y_dim) EQ 0    ) then begin
    ; We have an integer magnification to do.
    sub_array = rebin( sub_array, sub_x_dim * mag, sub_y_dim * mag, /SAMPLE )

  endif else if ((sub_x_dim MOD mag_x_dim) EQ 0 AND $
		 (sub_y_dim MOD mag_y_dim) EQ 0    ) then begin
    ; We have an integer reduction to do.
    sub_array = rebin( sub_array, sub_x_dim * mag, sub_y_dim * mag, /SAMPLE )

  endif else begin
    sub_array = congrid( sub_array, round(sub_x_dim * mag), $
				    round(sub_y_dim * mag) )
  endelse

  ;--------------------------------------------------------------------------
  ; Display the image at the correct location within the PDW. 
  im = bytscl( sub_array, /NAN, MIN=state.TvLow, $
			  MAX=state.TvHigh, TOP=ncolors-1 )

  ; Show B&W PostScript as a negative image.
  if (negative_images) then im = 255 - temporary(im)
  			  
  tv, im, ll_region_data(0), ll_region_data(1), /DATA, $
      XSIZE=ur_region_data(0)-ll_region_data(0), $
      YSIZE=ur_region_data(1)-ll_region_data(1) 

  ;--------------------------------------------------------------------------
  ; Create the caching pixmap, if necessary.
  if (state.pixmap_nums(draw_num) EQ -1) then begin

    window, /FREE, /PIX, XSIZE=!D.X_SIZE, YSIZE=!D.Y_SIZE

    state.pixmap_nums(draw_num) = !D.WINDOW
  endif

  ;--------------------------------------------------------------------------
  if (!D.NAME EQ 'X') then begin
    ; Copy the screen graphics we just did to a "pixmap" as cache.
    widget_control, state.draw_ids(draw_num), GET_VALUE=window_num

    wset, state.pixmap_nums(draw_num)
    device, COPY=[0, 0, !D.X_SIZE, !D.Y_SIZE, 0, 0, window_num]
    
  endif 

endif else begin
  ;--------------------------------------------------------------------------
  ;; Just redisplay from cache.
  widget_control, state.draw_ids(draw_num), GET_VALUE=window_num

;print, 'pixmap copy to window', draw_num
  wset, window_num
  device, COPY=[0, 0, !D.X_SIZE, !D.Y_SIZE, 0, 0, state.pixmap_nums(draw_num)]

endelse


;--------------------------------------------------------------------------
if (!D.NAME EQ 'X') then begin
  ;; Draw the marker.
  ; Convert the marker position from index coordinates to data coordinates.
  widget_control, state.marker_index_x, GET_VALUE=marker_index_x
  widget_control, state.marker_index_y, GET_VALUE=marker_index_y

  state.marker_data=[marker_index_x,marker_index_y] * state.index_to_data_gain $
	            + state.index_to_data_offset

  fmt = '("(",G10.4,")")'
  widget_control, state.marker_data_x, $
		SET_VALUE=strcompress(/REM,string( state.marker_data(0),f=fmt ))
  widget_control, state.marker_data_y, $
		SET_VALUE=strcompress(/REM,string( state.marker_data(1),f=fmt ))

  ; Convert the marker position from data coordinates to device coordinates
  ; and draw it.
  coordsystem_manager, state.draw_ids(draw_num), /RESTORE

  marker_device = convert_coord( state.marker_data(0), state.marker_data(1),$
			       /DATA, /TO_DEVICE )
  mark_window, marker_device(0), marker_device(1), 20, COLOR=red


  ;--------------------------------------------------------------------------
  ;; Update the Z value display.

  label= string(image( round(marker_index_x),round(marker_index_y)),f='(G10.4)')
  label= state.ztitle + '=' + strcompress(/REMOVE,label)
  widget_control, state.marker_z, SET_VALUE=label
endif

;; Return the image array to the handle.
handle_value, state.image_handle, image, /SET, /NO_COPY

return
end


;==========================================================================
;;; Widget Event Handler Function
;==========================================================================
FUNCTION ImageViewEventFn, Event

new_event = 0

;; Get the state structure.
top_base = Event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Normal image mouse press events
     state.draw_ids(0) : $
         if (Event.type EQ 0) then begin
            ; Convert from device to data coordinates.

            coordsystem_manager, state.draw_ids(0), /RESTORE

            marker_data = (convert_coord( event.x, event.y, /DEVICE, /TO_DATA ))(0:1)

            ; Convert from data to index coordinates.
            marker_index = (marker_data - state.index_to_data_offset) $
		  / state.index_to_data_gain

            ; Error check index coordinates.
            marker_index = 0 > round(marker_index) < (state.dims - 1)

            ; Change marker and zoom center states.
            widget_control, state.marker_index_x, SET_VALUE=marker_index(0)
            widget_control, state.marker_index_y, SET_VALUE=marker_index(1)
            state.zoom_center = marker_index

            ; Erase annotations, redraw zoom window, and reannotate.
            RedrawImageViewer, state, 0
            RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

            ; Rewrite the event.
            new_event = {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'Cursor moved'}
         endif

;--------------------------------------------------------------------------
; Zoomed image mouse press events
     state.draw_ids(1) : $
        if (Event.type EQ 0) then begin
           ; Convert from device to data coordinates.
           coordsystem_manager, state.draw_ids(1), /RESTORE

           marker_data = (convert_coord( event.x, event.y, /DEVICE, /TO_DATA ))(0:1)

           ; Convert from data to index coordinates.
           marker_index = (marker_data - state.index_to_data_offset) $
		  / state.index_to_data_gain

           ; Error check index coordinates.
           marker_index = 0 > round(marker_index) < (state.dims - 1)

           ; Change marker state, but not zoom center.
           widget_control, state.marker_index_x, SET_VALUE=marker_index(0)
           widget_control, state.marker_index_y, SET_VALUE=marker_index(1)

           ; Erase and redraw annotations.
           RedrawImageViewer, state, 0
           RedrawImageViewer, state, 1

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'Cursor moved'}
        endif

;--------------------------------------------------------------------------
; Marker coordinate fields
     state.marker_index_x : $
        begin
           ; Error check value entered and redisplay value.
           widget_control, state.marker_index_x, GET_VALUE=marker_index_x
           marker_index_x = 0 > marker_index_x < (state.dims(0)-1)
           widget_control, state.marker_index_x, SET_VALUE=marker_index_x

           ; Change zoom center state.
           state.zoom_center(0) = marker_index_x

           ; Move zoom window and redisplay the marker.
           RedrawImageViewer, state, 0
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'Cursor moved'}
        end

     state.marker_index_y : $
        begin
           ; Error check value entered and redisplay value.
           widget_control, state.marker_index_y, GET_VALUE=marker_index_y
           marker_index_y = 0 > marker_index_y < (state.dims(1)-1)
           widget_control, state.marker_index_y, SET_VALUE=marker_index_y

           ; Change zoom center state.
           state.zoom_center(1) = marker_index_y

           ; Move zoom window and redisplay the marker.
           RedrawImageViewer, state, 0
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'Cursor moved'}
        end

;--------------------------------------------------------------------------
; Magnification pulldown 
     state.mag_mode : $
        begin
           ; Change zoom center state.
           widget_control, state.marker_index_x, GET_VALUE=marker_index_x
           widget_control, state.marker_index_y, GET_VALUE=marker_index_y
           state.zoom_center = [marker_index_x, marker_index_y]

           ; Update zoom factor and label.
           case Event.index of
              0: state.mag = 1
              1: state.mag = 2
              2: state.mag = 4
              3: state.mag = 8
              4: state.mag = 16
             else: print, 'ERROR in mag_mode event'
          endcase

          ; Erase annotations, redraw zoom window, and reannotate.
          RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

          ; Rewrite the event.
          new_event = {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'Mag changed'}
       end

;--------------------------------------------------------------------------
; Display scaling controls
     state.low_slider  : $
        begin
           im_view_pcent_to_range, state, /LOW

           ; Redraw normal & zoom windows, and reannotate.
           RedrawImageViewer, state, 0, [0,0], state.dims-1
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:Event.top, HANDLER:0L, VALUE:'Scaling changed'}
        end

;--------------------------------------------------------------------------
     state.high_slider  : $
        begin
           im_view_pcent_to_range, state, /HIGH

           ; Redraw normal & zoom windows, and reannotate.
           RedrawImageViewer, state, 0, [0,0], state.dims-1
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:Event.top, HANDLER:0L, VALUE:'Scaling changed'}
        end

;--------------------------------------------------------------------------
     state.low_limit  : $
        begin
           widget_control, state.low_limit, GET_VALUE=tvlow
           state.tvlow = tvlow

           handle_value, state.image_handle, image, /NO_COPY   
           trash = where( image LE tvlow, count )
           widget_control, state.low_slider, SET_VALUE=100*count/n_elements(image) 
           handle_value, state.image_handle, image, /NO_COPY, /SET   

           ; Redraw normal & zoom windows, and reannotate.
           RedrawImageViewer, state, 0, [0,0], state.dims-1
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:Event.top, HANDLER:0L, VALUE:'Scaling changed'}
        end

;--------------------------------------------------------------------------
     state.high_limit  : $
        begin
           widget_control, state.high_limit, GET_VALUE=tvhigh
           state.tvhigh = tvhigh

           handle_value, state.image_handle, image, /NO_COPY   
           trash = where( image LE tvhigh, count )
           widget_control, state.high_slider, SET_VALUE=100*count/n_elements(image)
           handle_value, state.image_handle, image, /NO_COPY, /SET   

           ; Redraw normal & zoom windows, and reannotate.
           RedrawImageViewer, state, 0, [0,0], state.dims-1
           RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			MAG=state.mag

           ; Rewrite the event.
           new_event = {ID:top_base, TOP:Event.top, HANDLER:0L, VALUE:'Scaling changed'}
        end


;--------------------------------------------------------------------------
     state.ColorButton : $
        begin
           color_manager, NCOLORS=ncolors
           xloadct, GROUP=top_base, FILE=colortable_file, NCOLORS=ncolors
        end

;--------------------------------------------------------------------------
; Print menu.  Save the image to a file or print it.
     state.PrintMenu: $
        begin
           PsDevice, top_base, filename, success

           if (success EQ 1 ) then begin
              if (event.value EQ 1) then begin
                 RedrawImageViewer, state, 0, [0,0], state.dims-1
              endif else begin
                 RedrawImageViewer, state, 1, state.zoom_center, state.zoom_center,$
			  MAG=state.mag
              endelse

              device, /CLOSE
              color_manager, /X_PSEUDO

              if (filename EQ '') then begin
                 spawn, '/usr/bin/lp -c idl.ps; /bin/rm idl.ps >& /dev/null'
                 print, 'Printed image'
              endif else begin
                 print, 'Wrote Postscript file ' + filename
              endelse
           endif else result=dialog_message( "No PostScript output was produced")
        end

;--------------------------------------------------------------------------
     else: message, 'unknown event in pass1_image_viewer'
endcase

;--------------------------------------------------------------------------
;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, new_event
end

;==========================================================================
;;; Widget Event Handler Procedure
;;; Called when widget on top level.
;==========================================================================
PRO ImageViewEvent, Event

new_event = ImageViewEventFn(Event)
return
end

;==========================================================================
;;; Widget Creation Routine
;==========================================================================
FUNCTION pass1_image_viewer, image, ID=id, PARENT=parent, $
		       UVALUE=uvalue, GROUP=group, $
		       COLORTABLEFILE=colortable_file,  _EXTRA=extra, $
		       HANDLE=p_handle

;; If the user supplies a widget ID, then he wants to modify an existing
;; pass1_image_viewer widget, not create a new one.

if (0 NE n_elements(id)) then begin
  ModifyImageViewer, id, image, _EXTRA=extra
  return, id
endif


;; Preliminary stuff

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(group) then group = 0

if (NOT keyword_set(p_handle)) then begin
  image_viewer_handle = handle_create()
endif else image_viewer_handle = handle_create(p_handle)

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

if keyword_set(parent) then $
 top_base = widget_base(parent, UVALUE=uvalue, /COLUMN, $
			SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_LEFT, $
		        EVENT_FUNC    ='ImageViewEventFn', $
		        FUNC_GET_VALUE='ImageViewerGet', $
		        PRO_SET_VALUE='ImageViewerSet' ) $
else  begin
 center = ScreenCenter()
 xoffset = center(0) - 250
 yoffset = center(1) - 250
 top_base = widget_base(TITLE='Image Viewer', UVALUE=uvalue, /COLUMN, $
                        SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_LEFT, $
                        FUNC_GET_VALUE='ImageViewerGet', $
                        PRO_SET_VALUE='ImageViewerSet', $
                        XOFFSET=xoffset, YOFFSET=yoffset, GROUP_LEADER = group)
endelse


;;; !!!! THE FIRST CHILD OF THE TOP BASE CAN NOT BE A WIDGET_DRAW BECAUSE
;;; THE UVALUE SLOTS OF DRAW WIDGETS ARE USED BY coordsystem_manager!!!!!

 empty_base = widget_base(top_base)

 normal_draw = widget_draw(top_base, /BUTTON_EVENTS, RETAIN=1,$
			    XSIZE=560, YSIZE=300) 

 bottom_base = widget_base(top_base, /ROW, SPACE=1, XPAD=1, YPAD=1)


  zoom_draw = widget_draw(bottom_base, /BUTTON_EVENTS, RETAIN=1, $
			  XSIZE=290, YSIZE=270)

  right_base  = widget_base(bottom_base, /COLUMN, SPACE=1, XPAD=1, YPAD=1)

   label = 'Array indexes (coordinates) at marker'
   label = widget_label(right_base, VALUE=label)

   MarkerBase = widget_base(right_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

;   base = widget_base(MarkerBase, /ROW, SPACE=1, XPAD=1, YPAD=1, /FRAME)
    base = widget_base(MarkerBase, /ROW, SPACE=1, XPAD=1, YPAD=1 )

     marker_index_x = cw_field(base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE='X', XSIZE=5)

     marker_data_x = widget_label(base, /DYNAMIC_RESIZE)

;   base = widget_base(MarkerBase, /ROW, SPACE=1, XPAD=1, YPAD=1, /FRAME)
    base = widget_base(MarkerBase, /ROW, SPACE=1, XPAD=1, YPAD=1)

     marker_index_y = cw_field(base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE='Y', XSIZE=5)

     marker_data_y = widget_label(base, /DYNAMIC_RESIZE)

   marker_z = widget_label(right_base, /DYNAMIC_RESIZE, /ALIGN_CENTER)



  scaling_base = widget_base(right_base, /COLUMN, SPACE=1, XPAD=1, YPAD=1)
; scaling_base = widget_base(right_base, /COLUMN, SPACE=1, XPAD=1, YPAD=1, $
;			    FRAME=2)

   label     = widget_label(scaling_base, VALUE='Scaling Limits of Display')

   high_base = widget_base(scaling_base, /ROW, SPACE=1, XPAD=1, YPAD=1)

    high_slider = widget_slider(high_base, XSIZE=115, TITLE='Upper Percentile')

    high_limit  = cw_field(high_base, /FLOAT, TITLE='White',/RETURN_EVENTS,XSIZE=9)

   low_base = widget_base(scaling_base, /ROW, SPACE=1, XPAD=1, YPAD=1)

    low_slider = widget_slider(low_base, XSIZE=115, TITLE='Lower Percentile')

    low_limit  = cw_field(low_base, /FLOAT, TITLE='Black', /RETURN_EVENTS, XSIZE=9)


   base = widget_base(right_base, /ROW, SPACE=1, XPAD=1, YPAD=1,/BASE_ALIGN_CEN)

    list = ['magnify X1', 'magnify X2', 'magnify X4', $
	    'magnify X8', 'magnify X16'] 

    mag_mode = widget_droplist( base, VALUE=list, /ALIGN_LEFT )

    ColorButton = widget_button(base, VALUE='Colors')

    Menu = [ $
           { CW_PDMENU_S, flags:3,name:'Print' }, $
	   { CW_PDMENU_S,       0,     'Full image' }, $ 
	   { CW_PDMENU_S,       2,     'Magnified image' }  ]

    PrintMenu = cw_pdmenu(base, Menu, /RETURN_INDEX)

;; Save the state.

state = { $ 
	;IDs of widgets that generate events or have to be updated. 
	;Many of these widgets have "value" slots that hold state information.

	; The draw widget ID's and the pixmaps that go with them.
        draw_ids:[normal_draw, zoom_draw],  pixmap_nums:[-1,-1],$

	; The widget fields showing the marker coordinates in both index
	; and DATA coordinate systems.
	marker_index_x:marker_index_x, marker_index_y:marker_index_y, $
	marker_data_x: marker_data_x,  marker_data_y: marker_data_y, $
	marker_data:[0.0,0.0], marker_z:marker_z, $

        index_to_data_gain:[1.0,1.0], index_to_data_offset:[0.0,0.0],$
        xtitle:'X-index (pixels)', ytitle:'Y-index (pixels)', $
	ztitle:'Pixel Value (DN)', title:'', $

	mag_mode: mag_mode, $

	low_slider:low_slider, high_slider:high_slider, $
	low_limit:low_limit,   high_limit:high_limit, $
	ColorButton:ColorButton, PrintMenu:PrintMenu, $


	;SCALING information.
	TvLow:0.0, TvHigh:0.0, percentiles:fltarr(101), $


	;ZOOMING Information
        ;Requested center of zoom window in index coordinates.
	;Zoom scale factor
	zoom_center:[0,0], mag:1, $


	;OTHER state Information
	use_defaults_flag:1B, $

	;Handle IDs used to store the image array.
        image_viewer_handle:image_viewer_handle, $
	image_handle:handle_create(image_viewer_handle), $
	x_handle:    handle_create(image_viewer_handle, VALUE=[0]), $
	y_handle:    handle_create(image_viewer_handle, VALUE=[0]), $

	;image dimensions and statistics.
	dims:[0,0], integer_image_flag:0, $
	image_max:0.0, image_min:0.0, sky_level:0.0, sky_stddev:0.0 }

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

;; If this is top-level widget, realize it.
if NOT keyword_set(parent) then begin
  widget_control, top_base, /REALIZE

  xmanager, 'pass1_image_viewer', top_base, GROUP_LEADER=group, $
	    EVENT_HANDLER='ImageViewEvent', /NO_BLOCK
endif

;; Pass the image to ModifyImageViewer so it can be stored and drawn.
ModifyImageViewer, top_base, image, _EXTRA=extra

return, top_base
END

