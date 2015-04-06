;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_plot_viewer_fit.pro	9.11
;;;
;;; DESCRIPTION:  Plot Viewer Fitting Widget
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
FUNCTION CreatePlotViewerFit, parent, pv_id, pv_roi_id, SCATTER=scatter 

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base(parent, EVENT_FUNC='PlotViewerFitEventFn', $
			KILL_NOTIFY='PlotViewerFitCleanup', $
			/BASE_ALIGN_CENTER, /ROW, /SPACE, /XPAD, /YPAD ) 

if keyword_set(scatter) then begin
  intgauss = 0L
  gauss    = 0L
endif else begin
  menu = [{ CW_PDMENU_S, flags:1, name:'Integrated Gaussian' }, $
	  { CW_PDMENU_S,       0,      'no background' }, $
	  { CW_PDMENU_S,       0,      'constant background' }, $
	  { CW_PDMENU_S,       0,      'linear background' }, $
	  { CW_PDMENU_S,       2,      'quadratic background' }] 

  intgauss = cw_pdmenu(top_base, menu, /RETURN_INDEX)

  menu = [{ CW_PDMENU_S, flags:1, name:'Gaussian' }, $
	  { CW_PDMENU_S,       0,      'no background' }, $
	  { CW_PDMENU_S,       0,      'constant background' }, $
	  { CW_PDMENU_S,       0,      'linear background' }, $
	  { CW_PDMENU_S,       2,      'quadratic background' }] 

  gauss = cw_pdmenu(top_base, menu, /RETURN_INDEX)
endelse

  menu = [{ CW_PDMENU_S, flags:1, name:'Polynomial' }, $
	  { CW_PDMENU_S,       0,      'linear' }, $
	  { CW_PDMENU_S,       2,      'quadratic' }] 

  polynom = cw_pdmenu(top_base, menu, /RETURN_INDEX)

  stats = widget_button(top_base, VALUE='Stats')

if keyword_set(scatter) then begin
  profile = widget_button(top_base, VALUE='Profile')
endif else begin
  profile = 0L
endelse

; Setup state structure.
state = { $ 
	;IDs of widgets that generate events or need to be updated. 
	pv_id:pv_id, pv_roi_id:pv_roi_id, msg_id:-1L, $
	fitnum:0, fit_parameters:fltarr(6), $
	intgauss:intgauss, gauss:gauss, polynom:polynom, stats:stats,$
	profile:profile }

;; Save state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
END


;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO PlotViewerFitCleanup, top_base

widget_control, top_base, GET_UVALUE=st, /NO_COPY

return
end

;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawPlotViewerFit, st

widget_control, /HOURGLASS

return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION PlotViewerFitEventFn, Event

fit_done = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st, /NO_COPY
widget_control, /HOURGLASS

plot_viewer, st.pv_id, SELECTED_DATASET_NAME=selected_dataset_name

plot_viewer, st.pv_id, DATASET_NAME=selected_dataset_name, $
			GET_DATASET=dataset, $
			/APPLY_ROI, ROI_NAME='fitting & stats', ROI_MASK=mask

fit_indexes = where( *mask, fit_count )

if (fit_count GT 0) then begin
  x_data = (*dataset.x_data)(fit_indexes)
  y_data = (*dataset.y_data)(fit_indexes)

  ; If possible, use the standard deviations to compute weights for the fit,
  ; otherwise do an un-weighted fit.
  weights = 0  &  weighted_fit = 0

  if (n_elements(*dataset.standard_deviation) GT 0) then begin
    standard_deviation = (*dataset.standard_deviation)(fit_indexes)

    ; Look for std_dev values of zero!
    dummy = where( standard_deviation EQ 0, count )

    if (count EQ 0) then begin
      weights = 1.0 / (standard_deviation^2) 
      weighted_fit = 1
    endif 
  endif
endif else begin
  dummy=dialog_message('ROI is empty!', DIALOG_PARENT=top_base)
  widget_control, top_base, SET_UVALUE=st, /NO_COPY
  return, 0
endelse


;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; INTEGRATED GAUSSIAN FIT
  st.intgauss: $
   begin
   ; Try to determine a binsize, assuming that the data points are
   ; sorted in X.
   steps = x_data(1:fit_count-1) - x_data(0:fit_count-2) 
   step_max = max( steps, MIN=step_min )

   if (step_max - step_min GT 0.01 * abs(step_max)) then begin
	 dummy=dialog_message('NO FIT PERFORMED; could not compute binsize.',$
			      DIALOG_PARENT=st.intgauss)
   endif else begin
     binsize = median(steps)


     ; Establish an error handler for exceptions thrown in fitting routines.
     catch, error_code

     if (error_code NE 0) then begin
       dummy=dialog_message(['ERROR!', !ERR_STRING],DIALOG_PAR=st.intgauss,/ERR)
       fit_done = 0
     endif else begin 
       fit_done = 1

     nterms = 2 + event.value
     ndata  = n_elements(y_data)

     if (ndata LE nterms) then message, /NONAME, 'Too few data points'

       case Event.value of
	    ;----------------------------------------------------------------
            1: begin 
	     y_fit = intgaussfit( x_data, y_data, BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=-1, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma )
	    
             fit_name = "INT[gaussian]: "
	    end

	    ;----------------------------------------------------------------
            2: begin 
	     y_fit = intgaussfit( x_data, y_data, BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=0, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant )

             fit_name = "INT[gaussian + constant]: "
	    end

	    ;----------------------------------------------------------------
            3: $
	    begin 
	     y_fit = intgaussfit( x_data, y_data, BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=1, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant, LINEAR=linear )

             fit_name = "INT[gaussian + linear]: "
	    end

	    ;----------------------------------------------------------------
            4: $
	    begin 
	     y_fit = intgaussfit( x_data, y_data, BINSIZE=binsize, $
				  WEIGHTS=weights, ORDER=2, $
				  CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
				  GAIN=area, MEAN=mean, SIGMA=sigma, $
				  CONSTANT=constant, LINEAR=linear, $
				  QUADRATIC=quadratic )

             fit_name = "INT[gaussian + quadratic]: "
	    end
       endcase
     endelse ;error_code EQ 0

     catch, /CANCEL
   endelse ;binsize was computed
   end

;--------------------------------------------------------------------------
; REGULAR GAUSSIAN FIT
  st.gauss: $
   begin
   ; Establish an error handler for exceptions thrown in fitting routines.
   catch, error_code

   if (error_code NE 0) then begin
     dummy=dialog_message(['ERROR!', !ERR_STRING],DIALOG_PAR=st.gauss,/ERR)
     fit_done = 0
   endif else begin 
     fit_done = 1

     nterms = 2 + event.value
     ndata  = n_elements(y_data)

     if (ndata LE nterms) then message, /NONAME, 'Too few data points'

     y_fit = gaussfit(x_data, y_data, A, NTERMS=nterms )

     reduced_chi2 = 0.0
     chi2         = 0.0

     case Event.value of
	  ;----------------------------------------------------------------
          1: $
	  begin 
	   mean     = A(1)
	   sigma    = A(2)
	   area     = A(0) * sigma * SQRT(2*!PI)
	   fit_name = "gaussian: "
	  end

	  ;----------------------------------------------------------------
          2: $
	  begin 
	   mean     = A(1)
	   sigma    = A(2)
	   area     = A(0) * sigma * SQRT(2*!PI)
	   constant = A(3)
	   fit_name = "gaussian + constant: "
	  end

	  ;----------------------------------------------------------------
          3: $
	  begin 
	   mean     = A(1)
	   sigma    = A(2)
	   area     = A(0) * sigma * SQRT(2*!PI)
	   constant = A(3)
	   linear   = A(4)
	   fit_name = "gaussian + linear: "
	  end

	  ;----------------------------------------------------------------
          4: $
	  begin 
	   mean     = A(1)
	   sigma    = A(2)
	   area     = A(0) * sigma * SQRT(2*!PI)
	   constant = A(3)
	   linear   = A(4)
	   quadratic= A(5)
	   fit_name = "gaussian + quadratic: "
	  end

     endcase

   endelse ;error_code EQ 0

   catch, /CANCEL
   end

;--------------------------------------------------------------------------
; POLYNOMIAL FIT
  st.polynom: $
   begin
   ; Establish an error handler for exceptions thrown in fitting routines.
   catch, error_code

   if (error_code NE 0) then begin
     dummy=dialog_message(['ERROR!', !ERR_STRING],DIALOG_PAR=st.polynom,/ERR)
     fit_done = 0
   endif else begin 
     fit_done = 1
     y_fit = 1
     chi2  = 0

     nterms = 1 + event.value
     ndata  = n_elements(y_data)

     if (ndata LE nterms) then message, /NONAME, 'Too few data points'

     if (weighted_fit) then begin
       A = svdfit(x_data, y_data, nterms, WEIGHT=weight, $
		  CHISQ=chi2, YFIT=y_fit)
       reduced_chi2 = chi2 / (ndata - nterms)
     endif else begin
       A = svdfit(x_data, y_data, nterms, CHISQ=chi2, YFIT=y_fit)
     endelse

     case Event.value of
      ;----------------------------------------------------------------
      1: $
      begin 
      constant = A(0)
      linear   = A(1)
      fit_name = "linear: "
      end

      ;----------------------------------------------------------------
      2: $
      begin 
      constant = A(0)
      linear   = A(1)
      quadratic= A(2)
      fit_name = "quadratic: "
      end
     endcase
   endelse ;error_code EQ 0

   catch, /CANCEL
   end

;--------------------------------------------------------------------------
; STATS BUTTON
  st.stats: $
   begin
   widget_control, st.msg_id, /DESTROY, BAD_ID=bad

   s2 = string("Number of data points:",n_elements(y_data), $
		f='(A32,I10)')
   s0 = string("Range: [",min(x_data),max(x_data), $
		f='(A23,G11.5,2x,G11.5,"]")')
   s1 = string("Range: [",min(y_data),max(y_data), $
		f='(A23,G11.5,2x,G11.5,"]")')

   catch, error_code
   if (error_code NE 0) then begin
     dummy=dialog_message(['ERROR!', !ERR_STRING],DIALOG_PAR=st.stats,/ERR)
     y_mom = 0  &  y_sdev = 0
   endif else begin
     y_mom =  moment( y_data, SDEV=y_sdev )
   endelse ;error_code EQ 0
   catch, /CANCEL

   s7 = "Sigma is the `sample standard deviation', defined"
   s8 = "as SQRT( TOTAL( (data-mean)^2 ) / (N-1) )"

   if (dataset.scatter_flag) then begin
     catch, error_code
     if (error_code NE 0) then begin
       dummy=dialog_message(['ERROR!', !ERR_STRING],DIALOG_PAR=st.stats,/ERR)
       x_mom = 0  &  x_sdev = 0
     endif else begin 
       x_mom = moment( x_data, SDEV=x_sdev )
     endelse ;error_code EQ 0
     catch, /CANCEL

     s3 = string( "Mean:  ",  x_mom(0), f='(A23,G11.5)')
     s4 = string( "Mean:  ",  y_mom(0), f='(A23,G11.5)')
     s5 = string( "Sigma:  ", x_sdev,   f='(A23,G11.5)')
     s6 = string( "Sigma:  ", y_sdev,   f='(A23,G11.5)')

     msg = [s2,'','X AXIS',s0,s3,s5,'','Y AXIS',s1,s4,s6,'',s7,s8]
   endif else begin
     s3 = string( "Sum of values:  ", total(y_data), f='(A23,G11.5)')
     s4 = string( "Mean:  ",          y_mom(0), f='(A23,G11.5)')
     s5 = string( "Sigma:  ",         y_sdev,   f='(A23,G11.5)')

     centrd = total( float(x_data)*y_data ) / total(y_data)
     s6 = string( "Y-weighted Mean:  ",centrd, f='(A23,G11.5)')

     msg = [s2,'','X AXIS',s0,s6,'','Y AXIS',s1,s4,s5,s3,'',s7,s8]
   endelse

   st.msg_id = NonmodalMessage( msg, GROUP=top_base, $ 
			    TITLE='Statistics Inside Fitting & Stats ROI')
   end


;--------------------------------------------------------------------------
; PROFILE BUTTON
  st.profile: $
   begin
   widget_control, st.msg_id, /DESTROY, BAD_ID=bad
         
   plot_viewer_roi, st.pv_roi_id, ROI_NAME='fitting & stats', GET_ROI=roi
   save_roi = roi

   ;; Compute the centroid.
   roi.point_a(0) = total(x_data) / n_elements(x_data)
   roi.point_a(1) = total(y_data) / n_elements(y_data)

   case roi.style of
    'annulus': $
     begin
     b1 = '1, BASE,, ROW, FRAME'

     f2 = '0, FLOAT,' + string(roi.point_a(0)) + $
	 ', TAG=x0, LABEL_LEFT=Center, WIDTH=12'

     f3 = '2, FLOAT,' + string(roi.point_a(1)) + ', TAG=y0, WIDTH=12'

     f4 = '0, FLOAT,' + string(roi.length_a) + $
	 ', TAG=length_a, LABEL_LEFT=Inner Radius (fixed), WIDTH=12'

     f5 = '0, FLOAT,' + string(roi.length_b) + $
	 ', TAG=length_b, LABEL_LEFT=Outer Radius (variable), WIDTH=12'

     f6 = '0, FLOAT, 1, TAG=length_incr, LABEL_LEFT=Outer Radius Increment' +$
	 ', WIDTH=12'

     f7 = '2,BUTTON,OK,QUIT,TAG=ok'

     result = cw_form( [b1,f2,f3,f4,f5,f6,f7], /COLUMN, $
			 TITLE='Radial Profile Parameters' )

     ;; First, let's apply the full annular ROI to the dataset and compute
     ;; the 'sample standard deviation' of the radii.
     moi = TOTAL((x_data-result.x0)^2 + (y_data-result.y0)^2) / n_elements(x_data)
     
     s0 = "The `normalized moment of intertia' for the N datapoints in"
     s1 = 'the ROI about the specified location, defined as'
     s2 = ' TOTAL( (x-x0)^2 + (y-y0)^2 ) / N, is:'
     s3 = string( moi, f='("  ",G11.5)')
     s4 = "This is a measure of the radial spread of the data."
     msg = [s0,s1,s2,'',s3,'',s4]
          
     
     ;; Count number of annuli we must consider.
     num_points = round((result.length_b-result.length_a) / result.length_incr)
     num_points = 2 > num_points

     ; An annulus with outer radius "radii(i)" has area "areas(i)" and
     ; contains "counts(i)" data points.
     radii    = fltarr(num_points)
     counts   = lonarr(num_points)
     areas    = fltarr(num_points)

     roi.point_a  = [result.x0, result.y0]
     roi.length_a = result.length_a

     for ii = 0, num_points-1 do begin
       roi.length_b = roi.length_a + ii * result.length_incr
       plot_viewer_roi, st.pv_roi_id, ROI_NAME='fitting & stats', SET_ROI=roi

       plot_viewer, st.pv_id, DATASET_NAME=selected_dataset_name, $
			/APPLY_ROI, ROI_NAME='fitting & stats', ROI_MASK=mask

       dummy = where( *mask, count )
       counts(ii) = count
       areas(ii)  = !PI * (roi.length_b^2 - roi.length_a^2)
       radii(ii)  = roi.length_b
     endfor

     ;; Display the results
     total_counts = counts(num_points-1)
     tit  = string( roi.point_a, $
		    f='("Radial distribution about (",F7.1,F8.1,")")' )
     subt = string( total_counts, $
		    f='("Number of datapoints at largest radius: ",G11.5)' )

     encirc_fraction = counts * 100.0 / total_counts
     plot_viewer, ID1, radii, encirc_fraction, $
		       STANDARD_DEVIATION=sqrt(encirc_fraction),$
		       GROUP=top_base, TITLE=tit, SUBTITLE=subt,$
		       YTITLE='Number of Encircled Datapoints (%)',$
		       XTITLE='Radius (pixels)'

     annular_counts  = counts(1:num_points-1) - counts(0:num_points-2)
     annular_areas   =  areas(1:num_points-1) -  areas(0:num_points-2)
     annular_centers = (radii(1:num_points-1) +  radii(0:num_points-2))/2.

     brightness = annular_counts/annular_areas
     plot_viewer, ID2, annular_centers, brightness,$
		       STANDARD_DEVIATION=sqrt(brightness),$
		       GROUP=top_base, TITLE=tit,$
		       YTITLE='Radial Surface Brightness in Annulus',$
		       XTITLE='Mid-radius of Annulus', PSYM=10

     st.msg_id = NonmodalMessage( msg, GROUP=top_base, TITLE=tit )
     end ; annulus case


    else: dummy=dialog_message('Not implemented for this ROI style.',$
			       DIALOG_PARENT=top_base)
   endcase
   plot_viewer_roi, st.pv_roi_id, ROI_NAME='fitting & stats', SET_ROI=save_roi

   end

  else: print, 'unknown event in pass1_plot_viewer_fit'
endcase


;--------------------------------------------------------------------------
if (fit_done) then begin
  if (dataset.utype EQ 'fit') then begin
    dummy=dialog_message('The dataset you have fit is itself a fit.',$
			 DIALOG_PARENT=top_base)
  endif

  if (NOT weighted_fit) then $
    dummy=dialog_message(['A NON-weighted fit was done because', $
		       'data points with standard deviation of ZERO', $
		       'were found.'], DIALOG_PARENT=top_base)

   if (event.id EQ st.polynom) then begin
     st.fit_parameters = [0, 0, 0, 0, 0, 0]
     msg = 'Polynomial Coefficients:'
   endif else begin
     st.fit_parameters = [area, mean, sigma, 0, 0, 0]

     fmt = $
'("area under gaussian =",G11.5/15x,"mean =",G11.5/14x,"sigma =",G11.5)'
     msg = string( area, mean, sigma, f=fmt )

     fmt = '(2(G11.5,","), G11.5)'
     fit_name = fit_name + string( area, mean, sigma, f=fmt )
   endelse

   if (n_elements(constant) NE 0) then begin
         msg = [msg, string( constant, f='(6x,"constant term =",G11.5)' )]
         fit_name = fit_name + string( constant, f='(",",G11.5)' )
         st.fit_parameters(3) = constant
   endif

   if (n_elements(linear) NE 0) then begin
         msg = [msg, string( linear, f='(8x,"linear term =",G11.5)' )]
         fit_name = fit_name + string( linear, f='(",",G11.5)' )
         st.fit_parameters(4) = linear
   endif

   if (n_elements(quadratic) NE 0) then begin
         msg = [msg, string( quadratic, f='(4x,"quadratic term  =",G11.5)' )]
         fit_name = fit_name + string( quadratic, f='(",",G11.5)' )
         st.fit_parameters(5) = quadratic
   endif

   if (weighted_fit) then begin
         msg = [msg, string(reduced_chi2,f='(6x,"reduced chi^2 =",G11.5)')]
   endif else begin
         msg = [msg, string(chi2,f='(10x,"chi^2=",G11.5)')]
   endelse

   if (event.id EQ st.gauss AND dataset.utype EQ 'histogram') then begin
     msg = [msg,'WARNING! The dataset you fit is a histogram.',$
		'The reported area under the gaussian curve does NOT',$
		'reflect the total number of binned data points.',$
		'Try the Integrated Gaussian Model.']
   endif

   if (widget_info( st.msg_id, /VALID_ID )) then $
         widget_control, st.msg_id, /DESTROY

   st.msg_id = NonmodalMessage( msg, TITLE='Fit Results',GROUP=top_base )

   fit_name = strcompress( fit_name )

  name = string(st.fitnum, f='("fit",I0,": ")') + dataset.description
  st.fitnum = st.fitnum + 1
  plot_viewer, st.pv_id, x_data, y_fit, DATASET_NAME=name, COLOR='green', $
		DESCRIPTION=fit_name, SET_UTYPE='fit', LINE=1, /ENABLE_DELETE

endif

widget_control, top_base, SET_UVALUE=st, /NO_COPY

event.id=top_base
return, event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO PlotViewerFitEvent, Event
 
event = PlotViewerFitEventFn( Event )
return
end


;==========================================================================
;;; MAIN "pass1_plot_viewer_fit" ROUTINE
;==========================================================================

PRO pass1_plot_viewer_fit, top_base, parent, pv_id, pv_roi_id, _EXTRA=extra

top_base = CreatePlotViewerFit( parent, pv_id, pv_roi_id, _EXTRA=extra )


;;---------------------------------------------------------------------------
;; Get the state structure.
widget_control, top_base, GET_UVALUE=st, /NO_COPY


;;---------------------------------------------------------------------------
;; Save the state structure.
widget_control, top_base, SET_UVALUE=st, /NO_COPY

return
END

