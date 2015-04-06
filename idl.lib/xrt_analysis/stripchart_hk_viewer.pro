; $Id: stripchart_hk_viewer.pro 1.0 2002/01/18 17:25:30 burrows $
;
;==========================================================================
;;; hk Viewer Widget
;;; Used to display an XRT HK "image".  Includes a p1_image_viewer widget 
;;; and two p1_image_viewer_roi widgets.
;;;
;;;  XRT PASS1 stores HK as a 2-D FITS image file, with columns representing
;;;	HK channels and rows representing time samples.  This program
;;;	displays those files in physical units.

;;; Patrick Broos, August 1994
;;; @(#)stripchart_hk_viewer.pro	1.6
;;;     This routine is based on Pat Broos' ccd_viewer routine, modified
;;;	by Dave Burrows for use in PASS1 for XRT data processing.
;
;   Rev:
;	05/30/04 by DNB: fixed a bunch of bugs
;	04/07/02 by DNB: changed to new HK format (no time words at end).	
;

;==========================================================================

;==========================================================================
;;; INTERFACE TO CLIENT

;FUNCTION stripchart_hk_viewer, image, ID=id, PARENT=parent, TITLE=title, $
;		      UVALUE=uvalue, GROUP=group, UNITS=units


;;; The parameters PARENT, UVALUE, & GROUP are the usual ones associated with
;;; IDL widgets.  If ccd_viewer is a top-level widget, then TITLE
;;; controls the title in the X-window frame.

;;; The parameter UNITS is not yet implemented.



;==========================================================================
;;; TYPICAL USAGE

;;; This widget can be created in three distinct ways.

;;; First, it can be created as a child of an unrealized base widget by
;;; supplying the keyword parameter PARENT, just like one of the compound
;;; widgets in the user's library.
;;; The stripchart_hk_viewer widget will be realized when the parent is realized.
;;; The parent widget is responsible for asking the stripchart_hk_viewer widget
;;; to display the data for the first time by calling stripchart_hk_viewer with
;;; the ID keyword.  THIS IS NOT YET IMPLEMENTED!!

;;; For example:
;;;
;;;     stripchart_hk_viewer = stripchart_hk_viewer(image, PARENT=parent_id)
;;;     widget_control, parent_id, /REALIZE
;;;     dummy = stripchart_hk_viewer( ID=stripchart_hk_viewer )

;;; Second, the event handler of a realized widget can create a top level
;;; stripchart_hk_viewer that will be managed independently by the XMANAGER by
;;; omitting the parameter PARENT.
;;; The parameter TITLE may be supplied to put in the window frame.
;;; The stripchart_hk_viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  

;;; For example:
;;;
;;;     stripchart_hk_viewer = stripchart_hk_viewer(image, GROUP=creator_id)

;;; Third, a stripchart_hk_viewer may be created at the command line or by a regular
;;; IDL routine (not an event handler) by omitting the parameter PARENT.
;;; The stripchart_hk_viewer will realize itself, draw itself, and register with
;;; the XMANAGER. An explicit call to XMANAGER must then be made to start
;;; processing events.
;;; For example:
;;;
;;;     stripchart_hk_viewer = stripchart_hk_viewer(image, TITLE='My Data')
;;;     xmanager
;==========================================================================

;==========================================================================
;;; COMMUNICATION AMONG WIDGETS

;;; The stripchart_hk_viewer, p1_image_viewer, and two p1_image_viewer_roi widgets 
;;; interact in a complex way to display an image with overlay'd graphics
;;; depicting the ROI's.  Below are some example communication sequences.

;;; A. The p1_image_viewer re-displays the image, destroying all overlays.

;;; 1. p1_image_viewer generates an event to hk_viewer
;;; 2. hk_viewer gets overlay vectors of roi1 through its VALUE structure
;;; 3. hk_viewer sends overlay vectors to p1_image_viewer, e.g.
;;;	dummy = p1_image_viewer( ID=iv, X_OVERLAY=x, Y_OVERLAY=y )
;;; 4. p1_image_viewer draws the graphics
;;; 5. hk_viewer gets overlay vectors of roi2 through its VALUE structure
;;; 6. hk_viewer sends overlay vectors to p1_image_viewer, e.g.
;;;	dummy = p1_image_viewer( ID=iv, X_OVERLAY=x, Y_OVERLAY=y )
;;; 7. p1_image_viewer draws the graphics


;;; B. A roi definition is changed by the user.

;;; 1. the roi widget generates an event to hk_viewer
;;; 2. hk_viewer obtains the revised roi statistics through the VALUE
;;;    structure of the roi widget
;;; 3. hk_viewer forces p1_image_viewer to re-display the image (erasing the
;;;    exsiting annoations) by SENDing an event to p1_image_viewer
;;; 4. goto sequence A.


;;; C. The user selects the DEFAULT roi.

;;; 1. the roi widget obtains the VALUE structure of the p1_image_viewer
;;; 2. using a handle in the VALUE structure, the roi widget determines the
;;;    dimensions of the image and establishes a default roi
;;; 3. goto sequence B.


;;; D. The user indicates that a roi parameter should be set to the position
;;;    of the marker in the p1_image_viewer.

;;; 1. the roi widget obtains the VALUE structure of the p1_image_viewer
;;; 2. using the marker_x and marker_y fields in the VALUE structure, the roi 
;;;    widget redefines the ROI.
;;; 3. goto sequence B.


;;; E. New image data is passed to hk_viewer.

;;; 1. hk_viewer passes the new data to p1_image_viewer, e.g.
;;;	dummy = p1_image_viewer( ID=iv, image )
;;; 2. p1_image_viewer re-displays the data, but does NOT generate an event
;;; 3. hk_viewer forces both roi widgets to recompute the roi statistics;
;;;    the roi widgets do NOT generate events
;;;	dummy = p1_image_viewer_roi( ID=roi1 )
;;;	dummy = p1_image_viewer_roi( ID=roi2 )
;;; 4. hk_viewer forces p1_image_viewer to re-display the image again (sorry
;;;    about the extra re-display) by SENDing an event to p1_image_viewer
;;; 5. goto sequence A.


;;; F. Startup of hk_viewer.

;;; 1. Same as E.
;
; $Log: stripchart_hk_viewer.pro $
;

;==========================================================================
;;; This routine is called to change the image that is displayed.
;==========================================================================
PRO ModifyStripViewer, top_base, image, _EXTRA=extra

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

state.image_area = n_elements(image)
state.image_sum  = total(image)

;; Store the image.
dummy = p1_image_viewer( image, ID=state.p1_image_viewer, $
		      INDEX_TO_DATA_OFFSET=[1,0], ZTITLE='HK Measurement',$
		      XTITLE='HK Channel', YTITLE='Sample #') 

;; Force the ROI's to be recomputed.
dummy = p1_image_viewer_roi( ID=state.roi1 )
dummy = p1_image_viewer_roi( ID=state.roi2 )

;; Send an event to p1_image_viewer to redraw image and start the process of
;; redrawing the annotations.
if (widget_info(state.p1_image_viewer, /REALIZED)) then begin
  widget_control, state.p1_image_viewer, SEND_EVENT={ID:0L, TOP:0L, HANDLER:0L}
endif

;; Display statistics.
strip_viewer_show_stats, state, top_base

;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return
end


;==========================================================================
;;; hk Viewer statistics display routine
;==========================================================================
PRO strip_viewer_show_stats, State, top_base

widget_control, State.p1_image_viewer, GET_VALUE=iv
;widget_control, State.backgrnd_value, $
;	SET_VALUE='Sky Level' +string(iv.sky_level,f='(G10.4)')
;
;widget_control, State.backgrnd_stddev, $
;	SET_VALUE='Sky Sigma' +string(iv.sky_stddev,f='(G10.4)')

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
  widget_control, State.roi1, GET_VALUE=struct
  Min    = struct.roi_min
  Max    = struct.roi_max
  area   = struct.roi_area
  sum   = struct.roi_sum
  end

 2: $
  begin
  widget_control, State.roi2, GET_VALUE=struct
  Min    = struct.roi_min
  Max    = struct.roi_max
  area   = struct.roi_area
  sum   = struct.roi_sum
  end
endcase

widget_control, top_base, UPDATE=0
widget_control, State.MinLabel,   SET_VALUE='Min ' +string(Min,f='(G10.4)')
widget_control, State.MaxLabel,   SET_VALUE='Max ' +string(Max,f='(G10.4)')


widget_control, State.area_label, SET_VALUE='Area' +string(area,f='(G10.4)')
widget_control, State.sum_label,  SET_VALUE='Sum ' +string(sum,f='(G10.4)')
widget_control, top_base, UPDATE=1

return
end

;==========================================================================
;;; Widget Event Handler Procedure
;;; Called when widget on top level.
;==========================================================================
FUNCTION stripViewerEventFn, Event

@pass1_common

widget_control, /HOURGLASS

;; Get the stripchart_hk_viewer state structure.
top_base = Event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY


;; Process the event.

DestroyFlag = 0
case Event.ID of

;--------------------------------------------------------------------------
; Image viewer widget events (ROI annotations need to be redrawn).
  state.p1_image_viewer: $
   begin
   widget_control, state.roi1, GET_VALUE=struct
   handle_value, struct.x_overlay1, x
   handle_value, struct.y_overlay1, y
   if (n_elements(x) GT 0) then $
     dummy = p1_image_viewer( ID=state.p1_image_viewer, X_OVERLAY=x, Y_OVERLAY=y )

   handle_value, struct.x_overlay2, x
   handle_value, struct.y_overlay2, y
   if (n_elements(x) GT 0) then $
     dummy = p1_image_viewer( ID=state.p1_image_viewer, X_OVERLAY=x, Y_OVERLAY=y )

   widget_control, state.roi2, GET_VALUE=struct
   handle_value, struct.x_overlay1, x
   handle_value, struct.y_overlay1, y
   if (n_elements(x) GT 0) then $
     dummy = p1_image_viewer( ID=state.p1_image_viewer, X_OVERLAY=x, Y_OVERLAY=y )

   handle_value, struct.x_overlay2, x
   handle_value, struct.y_overlay2, y
   if (n_elements(x) GT 0) then $
     dummy = p1_image_viewer( ID=state.p1_image_viewer, X_OVERLAY=x, Y_OVERLAY=y )


   ;; Display the HK value at the marker.
   widget_control, State.p1_image_viewer, GET_VALUE=iv
   handle_value, iv.image_handle, image
   marker_x = iv.marker_index(0)
   marker_y = iv.marker_index(1)

   if ((marker_x ge 1) and (marker_x le 113)) then begin
        widget_control, State.XValLabel, SET_VALUE='Channel:   ' $
                        + hk_name(stripchart_hk_chan[marker_x-1])
;                       + strmid(hk_name(stripchart_hk_chan[marker_x-1]),0,26)
   	widget_control, State.ZValLabel, SET_VALUE='HK Value: ' $
                        + string(image(marker_x,marker_y),f='(g10.6)') $
                        + ' ' + hk_units(stripchart_hk_chan[marker_x-1])

   endif else begin
        if (marker_x eq 0) then begin
		widget_control, State.XValLabel, $
                        SET_VALUE='Channel:   S/C Time (s)'
   	widget_control, State.ZValLabel, SET_VALUE='HK Value: '
	endif else begin
	    case marker_x of
		114: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   XRT Count Rate (cps)'    	
		    end
		115: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   XRT State Flag'
		    end

		116: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   XRT Mode Flag'    	
		    end
		117: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   ACS Is_Settled Flag'    	
		    end
		118: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   ACS Is_In_10_Arcmin Flag'
		    end
		119: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   ACS In_SAA Flag'
		    end
		120: begin
        		widget_control, State.XValLabel, SET_VALUE='Channel:   ACS Safe Mode Flag'
		    end
		endcase
	endelse

 	widget_control, State.ZValLabel, SET_VALUE='HK Value: ' $
                        	+ string(image(marker_x,marker_y),f='(g10.6)')

   endelse


   widget_control, State.YValLabel, SET_VALUE='S/C Time: ' $
                        + string(image(0,marker_y),f='(f15.1)') $
                        + ' (s)'
  
   end


;--------------------------------------------------------------------------
; ROI definition changed.
  state.roi1: $
   begin
   ;; Redisplay the statistics in case this ROI is currently displayed.
   strip_viewer_show_stats, state, top_base

   ;; Send an event to p1_image_viewer to redraw image and start the process of
   ;; redrawing the annotations.
   widget_control, state.p1_image_viewer, SEND_EVENT={ID:0L, TOP:0L, HANDLER:0L}
   end

;--------------------------------------------------------------------------
; ROI definition changed.
  state.roi2: $
   begin
   ;; Redisplay the statistics in case this ROI is currently displayed.
   strip_viewer_show_stats, state, top_base

   ;; Send an event to p1_image_viewer to redraw image and start the process of
   ;; redrawing the annotations.
   widget_control, state.p1_image_viewer, SEND_EVENT={ID:0L, TOP:0L, HANDLER:0L}
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

     state.centroid_msg_id =  NonmodalMessage( txt, GROUP=top_base, XSIZE=500, $
			                       TITLE='Centroid Instructions' )
   endif else begin

     ;Compute the centroid
     widget_control, State.p1_image_viewer, GET_VALUE=iv
     handle_value, iv.image_handle, image

     widget_control, State.roi1, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes

     if (n_elements(roi_indexes) EQ 0) then begin
       dummy = widget_message( 'ROI #1 is empty!', $
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
       variance     = total( roi_data * sqr_distance ) / total(roi_data)
       std_dev      = sqrt( variance )

       sqr_distance = (x_coord-x_cen)^2
       variance     = total( roi_data * sqr_distance ) / total(roi_data)
       std_dev_x    = sqrt( variance )

       sqr_distance = (y_coord-y_cen)^2
       variance     = total( roi_data * sqr_distance ) / total(roi_data)
       std_dev_y    = sqrt( variance )

       ; Display the results
       message = [$
string( x_cen, y_cen, f='("CENTROID: (",G10.4,", ",G10.4,")")' ),$
string( std_dev,   f='("2-D SAMPLE STANDARD DEVIATION:         ",G10.4)' ),$
string( std_dev_x, f='("1-D SAMPLE STANDARD DEVIATION, X-axis: ",G10.4)' ),$
string( std_dev_y, f='("1-D SAMPLE STANDARD DEVIATION, Y-axis: ",G10.4)' ),$
	 '',$
         'The three sample standard deviations are the square-root of second',$
	 'moments (about the centroid) of the distribution of light.',$
	 'The 2-D version uses radial distances from the centroid.',$
	 'The 1-D versions use distances along the X and Y axes.']

     widget_control, state.centroid_msg_id, /DESTROY
     state.centroid_msg_id = $
       NonmodalMessage( message, GROUP=top_base, XSIZE=450, $
				       TITLE='Centroid Results' )

       print
       for ii=0,n_elements(message)-1 do print, message(ii)

       ; Move the marker if requested.
       if (event.value EQ 1) then begin
	 ;; Change the marker position and tv scaling of the p1_image_viewer.
         widget_control, State.p1_image_viewer, $
		 SET_VALUE= { marker_index:[x_cen, y_cen], $
		              tvhigh:struct.threshold, tvlow:struct.threshold }

         ;; Send an event to p1_image_viewer to redraw image and start the process
         ;; of redrawing the annotations.
;        widget_control, state.p1_image_viewer, $
;			 SEND_EVENT={ID:0L, TOP:0L, HANDLER:0L}
       endif

       ;; Setup ROI #2 for encircled energy.
       roi2_val = { center:[x_cen,y_cen], threshold:struct.threshold, $
		    inner_radius:0, outer_radius:struct.outer_radius } 
       widget_control, State.roi2, SET_VALUE=roi2_val
     endelse
   endelse
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

     state.encirc_msg_id = NonmodalMessage( txt, GROUP=top_base, XSIZE=500, $
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

       struct = { inner_radius:0, outer_radius:outer_radius } 
       widget_control, State.roi2, SET_VALUE=struct
       widget_control, State.roi2, GET_VALUE=struct

       if (outer_radius NE struct.outer_radius) then message,'BUG!'

       radii(ii)    = struct.outer_radius
       energies(ii) = struct.roi_sum
       areas(ii)    = struct.roi_area
     endfor

     ; Display the results
     total_energy = energies(num_points-1)

     tit = string( x_cen, y_cen, $
		   f='("Radial distribution about (",F7.1,F8.1,")")' )

     subt = string( total_energy, f='("Pixel sum at largest radius: ",G10.4)' )

     ID = p1_plot_viewer( radii, 100*energies/total_energy, $
		       GROUP=top_base, WIDGET_TITLE=state.Title,$
		       TITLE=tit, SUBTITLE=subt,$
		       YTITLE='Sum of Encircled Pixel Values (%)',$
		       XTITLE='Radius (pixels)' ) 

     annular_energies = energies(1:num_points-1) - energies(0:num_points-2)
     annular_areas    =    areas(1:num_points-1) -    areas(0:num_points-2)
     annular_centers  =   (radii(1:num_points-1) +    radii(0:num_points-2))/2.

     ID = p1_plot_viewer( annular_centers, annular_energies/annular_areas,$
		       GROUP=top_base, WIDGET_TITLE=state.Title, TITLE=tit,$
		       YTITLE='Radial Surface Brightness in Annulus',$
		       XTITLE='Mid-radius of Annulus (pixels)' ) 
   endelse
   end


;--------------------------------------------------------------------------
; Cuts menu.
  state.CutsMenu: $
   begin
   ; Retrieve the image array and marker coordinates.
   widget_control, state.p1_image_viewer, GET_VALUE=struct
   handle_value, struct.image_handle, image

   marker_x = struct.marker_index(0)
   marker_y = struct.marker_index(1)

   ; Plot a cut through the full image.
   case Event.value of
    1: begin
       Cut = float(image(*, marker_y))
	Cut[0,*] = 0.0	; because S/C time is so large that rest of plot is useless
       pv_title = string(marker_y,f='("HK data for sample # ",I0)') $
		+ string(image[0,marker_y],f='(" at S/C time = ",F15.1)')
       Xtit = 'HK Channel Number'
       Ytit = 'HK Value'
       ID = p1_plot_viewer(Cut, GROUP=top_base, TITLE=pv_title, YTIT=Ytit, $
			 WIDGET_TITLE=state.Title, XTIT=Xtit)
       end

    2: begin
       Cut = image[marker_x, *]
       time = image[0,*]

       if ((marker_x ge 1) and (marker_x le 113)) then begin
           pv_title = 'HK CH' + strtrim(string(stripchart_hk_chan[marker_x-1]),2) $
                + ': ' + hk_name(stripchart_hk_chan[marker_x-1])
	   if (time[0] gt 1.0e8) then begin
		jkl = fix(time[0]/1.0e8)
		zeropoint = jkl*1.0e8
		modtime = float(time - zeropoint)
		Xtit = 'S/C Time - ' + string(zeropoint,f='(E7.1)') + ' (s)'
	   endif else begin
		modtime = float(time)
           	Xtit = 'S/C Time (s)'
	   endelse
           Ytit = hk_units(stripchart_hk_chan[marker_x-1])
           ID = p1_plot_viewer(ModTime, Cut, GROUP=top_base, $
                 TITLE=pv_title, $
                 WIDGET_TITLE=state.Title, XTIT=Xtit, YTIT=Ytit)
        endif else begin
	   if (marker_x eq 0) then begin
               pv_title = 'S/C Time (s)'
               Xtit = 'Sample number'
  	   	if (cut[0] gt 1.0e8) then begin
			jkl = fix(cut[0]/1.0e8)
			zeropoint = jkl*1.0e8
			modcut = float(cut - zeropoint)
			Ytit = 'S/C Time - ' + string(zeropoint,f='(E7.1)') + ' (s)'
	   	endif else begin
			modcut = float(cut)
           		Ytit = 'S/C Time (s)'
	   	endelse
               ID = p1_plot_viewer(ModCut, GROUP=top_base, $
                         TITLE=pv_title, $
                         WIDGET_TITLE=state.Title, XTIT=Xtit, YTIT=Ytit)
           endif else begin
		case marker_x of 
		    114: begin
 		          pv_title = 'Column 114: XRT Count Rate'
 		          Ytit = '(cps)'
			end
		    115: begin
 		          pv_title = 'Column 115: XRT State Flag'
 		          Ytit = 'XRT State'
			end
		    116: begin
 		          pv_title = 'Column 116: XRT Mode Flag'
 		          Ytit = 'XRT Mode'
			end
		    117: begin
 		          pv_title = 'Column 117: ACS Is_Settled Flag'
 		          Ytit = 'Is_Settled Flag'
			end
		    118: begin
 		          pv_title = 'Column 118: ACS Is_In_10_Arcmin Flag'
 		          Ytit = 'Is_In_10_Arcmin Flag'
			end
		    119: begin
 		          pv_title = 'Column 119: ACS Is_In_SAA Flag'
 		          Ytit = 'Is_In_SAA Flag'
			end
		    120: begin
 		          pv_title = 'Column 120: ACS Safe Mode Flag'
 		          Ytit = 'Safe Mode Flag'
			end
		endcase
	   	if (time[0] gt 1.0e8) then begin
			jkl = fix(time[0]/1.0e8)
			zeropoint = jkl*1.0e8
			modtime = float(time - zeropoint)
			Xtit = 'S/C Time - ' + string(zeropoint,f='(E7.1)') + ' (s)'
	   	endif else begin
			modtime = float(time)
        	   	Xtit = 'S/C Time (s)'
	   	endelse
           	ID = p1_plot_viewer(ModTime, Cut, GROUP=top_base, $
       		          TITLE=pv_title, $
       		          WIDGET_TITLE=state.Title, XTIT=Xtit, YTIT=Ytit)
	   endelse
        endelse
	end
   endcase

   end

;--------------------------------------------------------------------------
; Stats droplist.
  state.stats_droplist: $
   begin
   ; Redraw stats label widgets.
   state.StatGroupId = event.index 
   strip_viewer_show_stats, state, top_base
   end

;--------------------------------------------------------------------------
; Histogram button.
  state.HistButton: $
   ; Display a histogram of full hk or ROI pixels.
   begin
   widget_control, State.p1_image_viewer, GET_VALUE=struct
   handle_value, struct.image_handle, image

   case State.StatGroupId of
    0: $
     begin
     pixel_data = temporary( image )
     title = 'Histogram of Full Image'
     end

    1: $
     begin
     widget_control, State.roi1, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes
     if (n_elements(roi_indexes) GT 0) then pixel_data = image( roi_indexes )
     title = 'Histogram of ROI #1'
     end

    2: $
     begin
     widget_control, State.roi2, GET_VALUE=struct
     handle_value, struct.roi_indexes, roi_indexes
     if (n_elements(roi_indexes) GT 0) then pixel_data = image( roi_indexes )
     title = 'Histogram of ROI #2'
     end
   endcase

   if (n_elements(pixel_data) GT 0) then begin

     if (datatype(pixel_data,1) EQ 'Double') then $
       pixel_data = float( temporary(pixel_data) )

     ID = p1_histogram_viewer(pixel_data, GROUP=top_base, TITLE=title, $
			   WIDGET_TITLE=state.Title, /AUTOMIN, /AUTOBIN, $
                           XTIT='Pixel Value',  YTIT='Number of Pixels' )
   endif
   end

;--------------------------------------------------------------------------
 state.DoneButton: DestroyFlag = 1

;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in stripchart_hk_viewer'
       help,/ST,Event
       end
endcase


;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

; DONE button
if (DestroyFlag) then widget_control, top_base, /DESTROY

return, 0
end


;==========================================================================
;;; Widget Event Handler Procedure
;==========================================================================
PRO stripViewerEvent, Event

event = stripViewerEventFn( Event )
return
end


;==========================================================================
;;; Widget Creation 
;==========================================================================
FUNCTION stripchart_hk_viewer, image, ID=id, PARENT=parent, $
	TITLE=title, UVALUE=uvalue, GROUP=group, UNITS=units, _EXTRA=extra

;===========================================================================
;===========================================================================

;; If the user supplies a widget ID, then he wants to modify an existing
;; stripchart_hk_viewer widget, not create a new one.
if (0 NE n_elements(id)) then begin
  ModifystripViewer, id, image, _EXTRA=extra
  return, id
endif

;; Preliminary stuff

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(group)  then group = 0
if NOT keyword_set(title)  then title=''
if NOT keyword_set(units)  then units="DN"

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

if keyword_set(Parent) then $
 top_base = widget_base(parent, UVALUE=uvalue, /COLUMN, $
                        SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_RIGHT, $
                        EVENT_FUNC    ='stripViewerEventFn') $
else $
 top_base = widget_base(TITLE=title, UVALUE=uvalue, /COLUMN, $
                        SPACE=1, XPAD=1, YPAD=1, /BASE_ALIGN_RIGHT, $
		        XOFFSET=100, YOFFSET=0, GROUP_LEADER  = group)

 p1_image_viewer = p1_image_viewer( PARENT=top_base )

 bottom_base = widget_base(top_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

  roi1 = p1_image_viewer_roi( bottom_base, p1_image_viewer, $
			  TITLE='ROI #1' )

  roi2 = p1_image_viewer_roi( bottom_base, p1_image_viewer, $
			  TITLE='ROI #2' )


  ControlsBase = widget_base(bottom_base, /COL, SPACE=1, XPAD=1, YPAD=1 )

   XValLabel = widget_label(ControlsBase, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
                	    VALUE='Channel:' )

   ZValLabel = widget_label(ControlsBase, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
                	    VALUE='HK Value:' )

   YValLabel = widget_label(ControlsBase, /ALIGN_LEFT, /DYNAMIC_RESIZE, $
			    VALUE='S/C Time (s)' )


   StatsBase    = widget_base(ControlsBase, /COLUMN, FRAME=2, $
				/BASE_ALIGN_CENTER, SPACE=1, XPAD=1, YPAD=1 )

;   label           = widget_label(StatsBase, VALUE='STATISTICS')

;    backgrnd_value       = widget_label(StatsBase, /DYNAMIC_RESIZE, /ALIGN_LEFT )

;    backgrnd_stddev = widget_label(StatsBase, /DYNAMIC_RESIZE, /ALIGN_LEFT )

   base= widget_base(StatsBase, /COLUMN, FRAME=2, $
				/BASE_ALIGN_CENTER, SPACE=1, XPAD=1, YPAD=1 )

    stats_droplist = widget_droplist(base, VALUE=['Full Image','ROI #1','ROI #2'])
    widget_control, stats_droplist, SET_DROPLIST_SELECT=0


    sum_label = widget_label(base, VALUE='Sum', /DYNAMIC_RESIZE)

    area_label = widget_label(base, VALUE='Area', /DYNAMIC_RESIZE)

    MaxLabel = widget_label(base, VALUE='Max', /DYNAMIC_RESIZE)

    MinLabel = widget_label(base, VALUE='Min', /DYNAMIC_RESIZE)

    HistButton = widget_button(base, VALUE='Pixel Histogram')


  PullDownBase = widget_base( bottom_base, /COLUMN, $
			      /BASE_ALIGN_CENTER, /ALIGN_BOTTOM )

   Menu = [ $
           { CW_PDMENU_S, flags:3, name:'Cuts' }, $
	   { CW_PDMENU_S,       0,      'Horizontal' }, $
	   { CW_PDMENU_S,       2,      'Vertical' }  ]

   CutsMenu = cw_pdmenu(PullDownBase, Menu, /RETURN_INDEX)

   Menu = [ $
           { CW_PDMENU_S, flags:3, name:'Centroid' }, $
	   { CW_PDMENU_S,       0,      $
	    'Move marker to centroid & set scaling to show ROI.' }, $
	   { CW_PDMENU_S,       2, 'Do not change marker or scaling.' }  ]

   centroid_button = cw_pdmenu(PullDownBase, Menu, /RETURN_INDEX)

   encircled_button = widget_button(PullDownBase, VALUE='Radial Dist.' )


   if (0 NE n_elements(parent)) then begin
     DoneButton = 0L
   endif else begin
     DoneButton = widget_button(PullDownBase, VALUE='Dismiss' )
   endelse



;; Initialize widgets and state structure.

state = { $
	;IDs of widgets that generate events.
	p1_image_viewer:p1_image_viewer, roi1:roi1, roi2:roi2, $

	centroid_button:centroid_button, encircled_button:encircled_button,$
	CutsMenu:CutsMenu, $
	stats_droplist:stats_droplist, HistButton:HistButton, $
	DoneButton:DoneButton, $

	;IDs of labels that will change.
        XValLabel:XValLabel, YValLabel:YValLabel, ZValLabel:ZValLabel, $

	sum_label:sum_label, $
	MaxLabel:MaxLabel, $
;	backgrnd_value:backgrnd_value, $
	MinLabel:MinLabel, $
;	backgrnd_stddev:backgrnd_stddev, $
	area_label:area_label, $

        ;ID to indicate which sets of statistics to display.
	StatGroupId: 0, $
	
	;Other stuff
	centroid_msg_id:0L, encirc_msg_id:0L, $
	Title:Title, $

	;Full image statistics
	image_area:0.0, image_sum:0.0 }


; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state

;; If this is top-level widget, realize it, draw it, and register.
if NOT keyword_set(Parent) then begin
  widget_control, top_base, /REALIZE

  xmanager, 'stripchart_hk_viewer', top_base, GROUP_LEADER=Group, $
	    EVENT_HANDLER='stripViewerEvent', /JUST_REG
;  xdisplayfile, '', text=header, group=group, $
;		title='XRT RT Housekeeping Data Header'
endif

;; Pass the image to ModifyStripViewer so it can be stored and drawn.
ModifyStripViewer, top_base, image, _EXTRA=extra

return, top_base
end

