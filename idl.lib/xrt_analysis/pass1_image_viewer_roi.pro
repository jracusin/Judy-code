; $Id: pass1_image_viewer_roi.pro 1.0 2002/01/18 17:25:30 burrows $
;
;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_image_viewer_roi.pro	9.1
;;;
;;; DESCRIPTION:  PASS1 Image Viewer Region-of-interest Compound Widget
;;;
;;;               Used to define a region-of-interest in a pass1_image_viewer_roi
;;;               widget.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:
;;;
;;; INTERFACE TO CLIENT
;;;
;;; FUNCTION pass1_image_viewer_roi, parent, pass1_image_viewer, ID=id, $
;;;			    UVALUE=uvalue, GROUP=group, TITLE=title

;;; The parameters PARENT, UVALUE, & GROUP are the usual ones associated with
;;; IDL widgets.  The parameter pass1_image_viewer should be the widget ID of
;;; a pass1_image_viewer widget.

;;; A sibling of this widget should always be an image_viewer widget -- its
;;; widget ID should be passed in the parameter "image_viewer".
;;; The parent must coordinate communication between the image_viewer and
;;; all the image_viewer_roi widgets to achieve complete redraws of the
;;; image and ROI annotation when needed.  See ccd_viewer for an example.

;;; If the parent needs to signal that the data in the associated plot_viewer
;;; widget has been changed (thus invalidating the region-of-interest), the
;;; parent should call plot_viewer_roi again, supplying the ID of the
;;; plot_viewer_roi widget in the keyword ID, e.g.
;;;	dummy = pass1_image_viewer_roi( ID=roi1 )

;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).


;==========================================================================
;;; VALUE PROPERTY
;;; The VALUE property of this widget, when READ (GET_VALUE),
;;; is a structure containing the following elements:

;;;   x_overlay1:   
;;;   y_overlay1:   
;;;   x_overlay2:   
;;;   y_overlay2:  handles to vectors that describe line drawings that 
;;;		   demarcate the ROI
;;;   outer_radius: value of the outer_radius field, if Annulus Mode
;;;   center:       (X,Y) value of the center field, if Annulus Mode
;;;   threshold:    value of the threshold field

;;; {These tags will contain the specified data ONLY if the VALUE property
;;;  has just been WRITTEN (SET_VALUE) with the tag "evaluate_roi" set!.}
;;;   roi_indexes: handle containing indexes of pixels included in ROI
;;;   roi_min:     minimum value in the ROI
;;;   roi_max:     maximum value in the ROI
;;;   roi_area:    area of the ROI in units of square pixels
;;;   roi_sum:     sum of the pixel values in the ROI

;;; Note that regions of interest can be empty.


;;; The VALUE property of this widget, when WRITTEN (SET_VALUE),
;;; is a structure containing the following elements.  
;;; All tags are optional.  If center, threshold, inner_radius, or
;;; outer_radius is included, the widget is placed into Annulus Mode.

;;; [center]:       (X,Y) value to assign to the center position
;;; [threshold]:    value to assign to the threshold field
;;; [inner_radius]: value to assign to the inner_radius field
;;; [outer_radius]: value to assign to the outer_radius field
;;; [evaluate_roi]: if present, the ROI computations are performed so that
;;;                 a subsequent GET_VALUE will return valid data



;==========================================================================
;;; EVENTS GENERATED
;;; This widget generates an event for its parent whenever the user changes
;;; the ROI.  The parent should then GET this widget's VALUE to obtain the 
;;; revised ROI.

;;; Events should not be SENT to this widget.
;-
;==========================================================================

FORWARD_FUNCTION ImageViewRoiEventFn

;==========================================================================
;;; This function computes ROI overlay graphics, 
;==========================================================================
PRO ComputeIvRoiOverlays, state

  case state.style of
    ;-----------------------------------------------------------------------
    'None': $
     begin
     handle_value, state.x_overlay1, undefined_var, /SET
     handle_value, state.y_overlay1, undefined_var, /SET
     handle_value, state.x_overlay2, undefined_var, /SET
     handle_value, state.y_overlay2, undefined_var, /SET
     end

    ;-----------------------------------------------------------------------
    'Rectangle': $
     begin
     widget_control, state.x_point1, GET_VALUE=x1
     widget_control, state.y_point1, GET_VALUE=y1
     widget_control, state.x_point2, GET_VALUE=x2
     widget_control, state.y_point2, GET_VALUE=y2

     ; Draw a rectangle
     handle_value, state.x_overlay1, [x1,x2,x2,x1,x1], /SET
     handle_value, state.y_overlay1, [y2,y2,y1,y1,y2], /SET
     handle_value, state.x_overlay2, undefined_var, /SET
     handle_value, state.y_overlay2, undefined_var, /SET
     end

    ;-----------------------------------------------------------------------
    'Annulus'   : $
     begin
     widget_control, state.x_point1,     GET_VALUE=x_center
     widget_control, state.y_point1,     GET_VALUE=y_center
     widget_control, state.inner_radius, GET_VALUE=inner_radius
     widget_control, state.outer_radius, GET_VALUE=outer_radius

     ; Draw two circles
     handle_value, state.x_overlay1, undefined_var, /SET
     handle_value, state.y_overlay1, undefined_var, /SET

     if (inner_radius GT 0) then begin
       tvcircle_vectors, inner_radius, x_center, y_center, x_overlay, y_overlay
       handle_value, state.x_overlay1, x_overlay, /SET, /NO_COPY
       handle_value, state.y_overlay1, y_overlay, /SET, /NO_COPY
     endif

     tvcircle_vectors, outer_radius, x_center, y_center, x_overlay, y_overlay
     handle_value, state.x_overlay2, x_overlay, /SET, /NO_COPY
     handle_value, state.y_overlay2, y_overlay, /SET, /NO_COPY
     end

  endcase
state.roi_overlays_valid = 1
return
end


;==========================================================================
;;; This function computes ROI statistics, obtaining the image data from
;;; a handle in the VALUE structure of the associated pass1_image_viewer widget.
;==========================================================================
PRO ComputeIvRoiStats, state

;; Borrow the image data.
widget_control, State.image_viewer, GET_VALUE=struct
handle_value, struct.image_handle, image, /NO_COPY
x_dim = struct.image_dims(0)
y_dim = struct.image_dims(1)

empty_roi = 1
if (n_elements(image) NE 0) then begin

  case state.style of
    ;-----------------------------------------------------------------------
    'None': $
     begin
     end

    ;-----------------------------------------------------------------------
    'Rectangle': $
     begin
     widget_control, state.x_point1, GET_VALUE=x1
     widget_control, state.y_point1, GET_VALUE=y1
     widget_control, state.x_point2, GET_VALUE=x2
     widget_control, state.y_point2, GET_VALUE=y2
     widget_control, state.threshold, GET_VALUE=threshold
     print, threshold, f='(/"Evaluating rectangular ROI; threshold is ",' + $
			 'G10.4/"Boundary pixels are INCLUDED.")'

     min_x = 0 > (x1 < x2) < (x_dim - 1)
     max_x = 0 > (x1 > x2) < (x_dim - 1)
     min_y = 0 > (y1 < y2) < (y_dim - 1)
     max_y = 0 > (y1 > y2) < (y_dim - 1)

     ; Extract pixels in rectangle
     sub_image = extract_sub_image( image, [min_x, max_x], $
					   [min_y, max_y], sub_indexes )

     ; Find the ones above the threshold
     index = where( sub_image GE threshold, count )

     if (count EQ 0) then begin
       print, '  ROI IS EMPTY!'
     endif else begin
       roi_data    = sub_image(  index )
       roi_indexes = sub_indexes(index )
       print, count, f='("  ROI consists of ",I0," pixels.")'

       state.roi_area = n_elements(sub_image)
       state.roi_sum  = total( roi_data )
       empty_roi = 0
     endelse
     end

    ;-----------------------------------------------------------------------
    'Annulus'   : $
     begin
     widget_control, state.x_point1,     GET_VALUE=x_center
     widget_control, state.y_point1,     GET_VALUE=y_center
     widget_control, state.inner_radius, GET_VALUE=inner_radius
     widget_control, state.outer_radius, GET_VALUE=outer_radius
     widget_control, state.threshold,    GET_VALUE=threshold

     ;; We do not allow radii smaller than 2 pixels because the crude algorithm
     ;; we have for dealing with partial pixels really breaks down for
     ;; small radii.
     if (inner_radius NE 0) then inner_radius = inner_radius > 2.0
     if (outer_radius NE 0) then outer_radius = outer_radius > 2.0
     widget_control, state.inner_radius, SET_VALUE=inner_radius
     widget_control, state.outer_radius, SET_VALUE=outer_radius

     print, threshold, f='(/"Evaluating annular ROI with a threshold of: ",' + $
			 'G10.4/"  Boundary pixels are fractionally included.")'

     ; Extract a sub-array from the image to work with so that dist_circle
     ; won't take so long to execute.  
     min_x = 0 > round(x_center - outer_radius - 1) < (x_dim - 1)
     max_x = 0 > round(x_center + outer_radius + 1) < (x_dim - 1)
     min_y = 0 > round(y_center - outer_radius - 1) < (y_dim - 1)
     max_y = 0 > round(y_center + outer_radius + 1) < (y_dim - 1)

     sub_image = extract_sub_image( image, [min_x,max_x], $
					  [min_y, max_y], sub_indexes )

     ; Compute the distance from (x_center, y_center) to the CENTER of 
     ; each pixel in the sub_image.  
     S = size(sub_image)
     x_dim = S(1)  &  y_dim = S(2)
     dist_circle, distances, [x_dim, y_dim], x_center - min_x, y_center - min_y


     ; At this point, the 2-D arrays 'sub_image' and 'distances' should have
     ; the same dimensions and the vector 'sub_indexes' should contain the
     ; indexes (in the full image) of the pixels in 'sub_image'.

     ; Figure out which sub_image pixels, if any, are partially inside 
     ; the annulus and above the threshold.

     index = where( distances LT (outer_radius + 0.5) AND $
		    distances GT (inner_radius - 0.5) AND $
		    sub_image GE threshold, count )

     if (count GT 0) then begin
       empty_roi = 0
       ; Store those pixels in 'roi_data' and their indexes in the full image
       ; in 'roi_indexes'.
       roi_data    = sub_image(  index )
       roi_indexes = sub_indexes(index )
       print, count, f='("  ROI touches ",I0," pixels.")'


       ;; ------------------------------------------------------------------
       ;; Add up the light inside the outer radius.
       ;; ------------------------------------------------------------------

       ; Figure out which ROI pixels are partially inside the outer radius
       ; and above the threshold..
       index = where( distances LT (outer_radius + 0.5) AND $
		      sub_image GE threshold, count )

       if (count EQ 0) then message, 'Bug in ComputeIvRoiStats!'

       outer_data      = sub_image(index)
       outer_distances = distances(index)

       ; Compute the fraction of each pixel that should be included in the
       ; outer sum, allowing for partial pixels in the manner of DAOPHOT.
       ; Then add up the light.
       pixel_fraction = 0.0 > ((outer_radius + 0.5) - outer_distances) < 1.0

       sum_in_outer_radius  = total( outer_data * pixel_fraction )

       ;; ------------------------------------------------------------------
       ;; IF the inner radius is not zero, then add up the light inside 
       ;; the inner radius.
       ;; ------------------------------------------------------------------

       ; Figure out which pixels, if any, are partially inside the inner 
       ; radius and above the threshold.
       index = where( distances LT (inner_radius + 0.5) AND $
		      sub_image GE threshold, count )

       if (inner_radius NE 0  AND  count GT 0) then begin
         inner_data      = sub_image(index)
         inner_distances = distances(index)

         ; Compute the fraction of each pixel that should be included in the
         ; sum, allowing for partial pixels in the manner of DAOPHOT.
         ; Then add up the light.
         pixel_fraction = 0.0 > ((inner_radius + 0.5) - inner_distances) < 1.0

         sum_in_inner_radius  = total( inner_data * pixel_fraction )

       endif else begin
	 sum_in_inner_radius  = 0.0
	 print, '  No flux inside inner radius.'
       endelse

       ;; ------------------------------------------------------------------
       ;; Subtract the two sums.
       ;; ------------------------------------------------------------------
       state.roi_sum  = sum_in_outer_radius  - sum_in_inner_radius
       state.roi_area = (!PI * outer_radius^2) - (!PI * inner_radius^2)

     endif else begin 
       print, '  ROI IS EMPTY!'
     endelse
     end

  endcase
endif ;(n_elements(image) NE 0)


if (empty_roi EQ 1) then begin
     roi_indexes = 0
     dummy = temporary( roi_indexes )
     roi_data    = [0]
     state.roi_area = 0
     state.roi_sum  = 0
endif

; Save the roi data on a handle in the state structure.
handle_value, state.roi_indexes, roi_indexes, /SET

; We compute min and max values here and save in the state structure
; because the min and max values may be later recomputed on a sampled 
; dataset below.
roi_min = min(roi_data, MAX=roi_max)
state.roi_min  = roi_min
state.roi_max  = roi_max


; Put the image data back.
handle_value, struct.image_handle, image, /SET, /NO_COPY

state.roi_stats_valid = 1
return
end


;==========================================================================
;;; Widget SET_VALUE Routine
;==========================================================================
PRO ImageViewerRoiSet, top_base, struct

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

tag_names    = tag_names( struct )
center       = ((where( tag_names EQ 'CENTER' ))(0) NE -1)
threshold    = ((where( tag_names EQ 'THRESHOLD' ))(0) NE -1)
inner_radius = ((where( tag_names EQ 'INNER_RADIUS' ))(0) NE -1)
outer_radius = ((where( tag_names EQ 'OUTER_RADIUS' ))(0) NE -1)
evaluate_roi = ((where( tag_names EQ 'EVALUATE_ROI' ))(0) NE -1)


;; Deal with annulus mode tags.
if (center OR threshold OR inner_radius OR outer_radius) then begin
  state.roi_stats_valid    = 0
  state.roi_overlays_valid = 0

  ;; Put the widget into Annulus Mode if necessary.
  if (state.style NE 'Annulus') then begin
    style_droplist = state.style_droplist

    ;; Save state structure.
    widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

    ;; Get into Annulus Mode
    widget_control, style_droplist, SET_DROPLIST_SELECT=2
    dummy = ImageViewRoiEventFn( {ID:style_droplist, TOP:0L, $
				HANDLER:top_base, index:2} )

    ;; Get the state structure.
    widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY
  endif

  if (center) then begin
    widget_control, state.x_point1,     SET_VALUE=struct.center(0)
    widget_control, state.y_point1,     SET_VALUE=struct.center(1)
  endif

  if (threshold) then $
    widget_control, state.threshold,    SET_VALUE=struct.threshold
  if (inner_radius) then $
    widget_control, state.inner_radius, SET_VALUE=struct.inner_radius
  if (outer_radius) then $
    widget_control, state.outer_radius, SET_VALUE=struct.outer_radius

endif

;; Compute the ROI if necessary.
if (evaluate_roi AND (state.roi_stats_valid EQ 0)) then ComputeIvRoiStats, state


;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return
end


;==========================================================================
;;; Widget GET_VALUE Routine
;==========================================================================
FUNCTION ImageViewerRoiGet, top_base

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY


if (state.style NE 'None') then begin
  widget_control, state.threshold, GET_VALUE=threshold
endif else begin
  threshold = 0
endelse

if (state.style EQ 'Annulus') then begin
  widget_control, state.outer_radius, GET_VALUE=outer_radius
  widget_control, state.x_point1,     GET_VALUE=x_center
  widget_control, state.y_point1,     GET_VALUE=y_center
endif else begin
  outer_radius = 0
  x_center     = 0
  y_center     = 0
endelse


if (state.roi_overlays_valid EQ 0) then ComputeIvRoiOverlays, state


;; Create the return structure.
struct = { roi_min:state.roi_min, roi_max:state.roi_max, $
	   roi_area:state.roi_area, roi_sum:state.roi_sum, $
	   roi_indexes:state.roi_indexes, $
	   x_overlay1:state.x_overlay1, y_overlay1:state.y_overlay1, $ 
	   x_overlay2:state.x_overlay2, y_overlay2:state.y_overlay2, $
	   threshold:threshold, outer_radius:outer_radius, $
	   center:[x_center,y_center] }

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, struct
end



;==========================================================================
;;; Widget Event Handler Function
;==========================================================================
FUNCTION ImageViewRoiEventFn, Event


;; Get the state structure.
top_base = Event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY
widget_control, /HOURGLASS

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; style droplist events
  state.style_droplist: $
      begin
         widget_control, top_base, UPDATE=0

         ;; Determine the dimensions of the image.
         widget_control, State.image_viewer, GET_VALUE=struct
         x_dim = struct.image_dims(0)
         y_dim = struct.image_dims(1)


         ; Recreate an empty controls base.
         widget_control, state.controls_base, /DESTROY
         state.controls_base = widget_base( top_base, /COL, SPACE=1, XPAD=1, YPAD=1 )


         ; Fill it in with the appropriate widgets.
         case Event.index of
             ;----------------------------------------------------------------------
             0: $ ;;'None'
                 begin
                    state.style = 'None'
                 end

             ;----------------------------------------------------------------------
             1: $ ;;'Rectangle'
                 begin
                    state.style = 'Rectangle'

                    state.threshold = cw_field( state.controls_base, /ROW, /FLOAT, $
				 /RETURN_EVENTS, TITLE='Pixel Threshold', $
				 XSIZE=5, VALUE=0 )

                    point1_base = widget_base( state.controls_base, /COLUMN, $
			    SPACE=1, XPAD=1, YPAD=1 )
;			    /FRAME, SPACE=1, XPAD=1, YPAD=1 )

                    label = widget_label( point1_base, VALUE='Corner #1' )

                    base = widget_base( point1_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

                    state.x_point1 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='X', XSIZE=5, VALUE=0 )

                    state.y_point1 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='Y', XSIZE=5, VALUE=0 )

                    state.point1_button = widget_button( point1_base, $
						VALUE='Use Marker Position' )

                    point2_base = widget_base( state.controls_base, /COLUMN, $
			    SPACE=1, XPAD=1, YPAD=1 )
;			    /FRAME, SPACE=1, XPAD=1, YPAD=1 )

                    label = widget_label( point2_base, VALUE='Corner #2' )

                    base = widget_base( point2_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

                    state.x_point2 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='X', XSIZE=5, VALUE=x_dim-1 )

                    state.y_point2 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='Y', XSIZE=5, VALUE=y_dim-1 )

                    state.point2_button = widget_button( point2_base, $
						VALUE='Use Marker Position' )
                 end

             ;----------------------------------------------------------------------
             2: $ ;;'Annulus'
                 begin
                    state.style = 'Annulus'

                    state.threshold = cw_field( state.controls_base, /ROW, /FLOAT, $
				 /RETURN_EVENTS, TITLE='Pixel Threshold', $
				 XSIZE=5, VALUE=0 )

                    point1_base = widget_base( state.controls_base, /COLUMN, $
			    SPACE=1, XPAD=1, YPAD=1 )
;			    /FRAME, SPACE=1, XPAD=1, YPAD=1 )

                    label = widget_label( point1_base, VALUE='Center' )

                    base = widget_base( point1_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

                    state.x_point1 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='X', XSIZE=5, VALUE=struct.marker_index(0))

                    state.y_point1 = cw_field( base, /ROW, /FLOAT, /RETURN_EVENTS, $
			      TITLE='Y', XSIZE=5, VALUE=struct.marker_index(1))

                    state.point1_button = widget_button( point1_base, $
						VALUE='Use Marker Position' )

                    state.inner_radius = cw_field( state.controls_base, /ROW, /FLOAT, $
		       /RETURN_EVENTS, TITLE='Inner Radius', XSIZE=5, VALUE=0)

                    state.outer_radius = cw_field( state.controls_base, /ROW, /FLOAT, $
		       /RETURN_EVENTS, TITLE='Outer Radius', XSIZE=5, VALUE=20)
                 end

         endcase

         widget_control, top_base, UPDATE=1
      end
  state.threshold: $
      begin
      end


;--------------------------------------------------------------------------
; rectangular mode events
  state.x_point1: $
      begin
      end

  state.y_point1: $
      begin
      end

  state.point1_button: $
      begin
         widget_control, State.image_viewer, GET_VALUE=struct
         widget_control, State.x_point1, SET_VALUE=round( struct.marker_index(0) )
         widget_control, State.y_point1, SET_VALUE=round( struct.marker_index(1) )
      end

  state.x_point2: $
      begin
      end

  state.y_point2: $
      begin
      end

  state.point2_button: $
      begin
         widget_control, State.image_viewer, GET_VALUE=struct
         widget_control, State.x_point2,    SET_VALUE=round( struct.marker_index(0) )
         widget_control, State.y_point2,    SET_VALUE=round( struct.marker_index(1) )
      end

;--------------------------------------------------------------------------
; circular mode events
 state.inner_radius: $
     begin
     end

 state.outer_radius: $
     begin
     end


;--------------------------------------------------------------------------
   else: message, 'unknown event in pass1_image_viewer_roi'
endcase

state.roi_stats_valid    = 0
state.roi_overlays_valid = 0

;--------------------------------------------------------------------------
;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, {ID:top_base, TOP:event.top, HANDLER:0L, VALUE:'ROI changed'}
end


;==========================================================================
;;; Widget Creation Routine
;==========================================================================
FUNCTION pass1_image_viewer_roi, parent, pass1_image_viewer, ID=id, $
			   UVALUE=uvalue, GROUP=group, TITLE=title

;; If the parent supplies a widget ID, then it wants this widget to know that
;; the dataset itself has changed and the ROI is now invalid.

if (0 NE n_elements(id)) then begin
  ;; Get the state structure.
  widget_control, widget_info(id, /CHILD), GET_UVALUE=state, /NO_COPY

  state.roi_stats_valid = 0

  ;; Save the state structure.
  widget_control, widget_info(id, /CHILD), SET_UVALUE=state, /NO_COPY

  return, id
endif


;; Preliminary stuff

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(group)  then group = 0
if NOT keyword_set(title)  then title='Region of Interest'

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

;top_base = widget_base(parent, UVALUE=uvalue, /COLUMN, /FRAME, $
top_base = widget_base(parent, UVALUE=uvalue, /COLUMN, $
			SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_CENTER, $
		        EVENT_FUNC    ='ImageViewRoiEventFn', $
		        FUNC_GET_VALUE='ImageViewerRoiGet',$  
		        PRO_SET_VALUE='ImageViewerRoiSet' ) 

 base = widget_base( top_base, SPACE=1, XPAD=1, YPAD=1, /ROW )

  label = widget_label( base, VALUE=title )

  styles = ['None','Rectangle','Annulus']
  style_droplist = widget_droplist( base, VALUE=styles )
  widget_control, style_droplist, SET_DROPLIST_SELECT=0

 controls_base = widget_base( top_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )



;; Save the state.

state = { $ 
	;IDs of widgets that generate events or have to be updated. 
	;Many of these widgets have "value" slots that hold state information.

	style_droplist:style_droplist, $
	controls_base:controls_base, style:'None', $
	roi_stats_valid:0, roi_overlays_valid:0, $

	threshold:0L, $
	x_point1: 0L, y_point1: 0L, point1_button: 0L, $
	x_point2: 0L, y_point2: 0L, point2_button: 0L, $
	inner_radius: 0L, outer_radius: 0L, $


	;Image Viewer widget id
	pass1_image_viewer:pass1_image_viewer, $

	;image dimensions and statistics.
	roi_max:0.0, roi_min:0.0, $
	roi_area:0.0, roi_sum:0.0, roi_indexes:handle_create(), $
	x_overlay1:handle_create(), y_overlay1:handle_create(), $ 
	x_overlay2:handle_create(), y_overlay2:handle_create() }

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, top_base
END

