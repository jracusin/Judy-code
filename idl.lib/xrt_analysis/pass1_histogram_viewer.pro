; $Id: pass1_histogram_viewer.pro 1.0 2002/01/18 17:25:30 burrows$
;
;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_histogram_viewer.pro	9.8
;;;
;;; DESCRIPTION:  Histogram Viewer Compound Widget
;;;
;;;               Used to compute and display a histogram.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1995, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;; INTERFACE TO CLIENT
;;;
;;;FUNCTION pass1_histogram_viewer, data, ID=id, PARENT=parent, $
;;;			   LABEL_MIN=label_min, LABEL_BIN=label_bin, $
;;;			   GROUP=group, 
;;;			   WIDGET_TITLE=widget_title, $
;;;                    	   DATASET_NAME=dataset_name, $
;;;		           AUTOMIN=automin, AUTOBIN=autobin, $
;;;			   DEFAULT_AXES=default_axes, $
;;;			   MIN=min, BINSIZE=binsize
;;;
;;; The parameters PARENT & GROUP are the usual ones associated with
;;; IDL widgets.  If pass1_histogram_viewer is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; The data to be histogrammed is passed in "data" and the keywords MIN
;;; and BINSIZE are identical to IDL's histogram function.
;;; THE DATA, AS WELL AS THE MIN AND BINSIZE PARAMETERS, MAY NOT BE DOUBLE!!
;;;
;;; Reasonable default values for MIN and BINSIZE can be computed
;;; automatically by setting AUTOMIN & AUTOBIN.
;;; The axis ranges in the included plot_viewer can be set to cover the full
;;; histogram by setting DEFAULT_AXES.
;;; The labels on the min and binsize fields can be set with LABEL_MIN &
;;; LABEL_BIN.
;;;
;;; Any extra keywords supplied are passed on to the plot_viewer widget.
;;;
;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).
;;;
;;; This widget does not generate any events for its parent.
;;;
;==========================================================================
;;; TYPICAL USAGE
;;;
;;; This widget can be used in three distinct ways.
;;;
;;; First, it can be created as a child of an unrealized base widget by 
;;; supplying the keyword parameter PARENT, just like one of the compound 
;;; widgets in the user's library.  
;;; The pass1_histogram_viewer widget will be realized when the parent is realized.
;;; The parent widget is responsible for asking the pass1_histogram_viewer widget
;;; to histogram the data for the first time by calling pass1_histogram_viewer() 
;;; with the ID keyword.
;;;
;;; For example:
;;;
;;;     pass1_histogram_viewer = pass1_histogram_viewer(y_data, PARENT=parent_id)
;;;     widget_control, parent_id, /REALIZE
;;;     dummy = pass1_histogram_viewer( ID=histogram_viewer )
;;;
;;; Second, the event handler of a realized widget can create a top level
;;; histogram viewer that will be managed independently by the XMANAGER by 
;;; omitting the parameter PARENT.
;;; The parameter WIDGET_TITLE may be supplied to put in the window frame.
;;; The histogram viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  The pass1_ccd_viewer widget uses pass1_histogram_viewer in this manner.
;;;
;;; For example:
;;;
;;;     pass1_histogram_viewer = pass1_histogram_viewer(y_data, GROUP=creator_id)
;;;
;;; Third, a pass1_histogram_viewer may be created at the command line or by a regular
;;; IDL routine (not an event handler) by omitting the parameter PARENT.
;;; The pass1_histogram_viewer will realize itself, draw itself, and register with
;;; the XMANAGER.  An explicit call to XMANAGER must then be make to start
;;; processing events.
;;;
;;; For example:
;;;
;;;     pass1_histogram_viewer = pass1_histogram_viewer(y_data, WIDGET_TITLE='My Data')
;;;     xmanager
;
; $Log: pass1_histogram_viewer.pro $
;
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreateHistViewer, parent, group, widget_title, $
			   label_min, label_bin


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print, 'THIS TOOL IS OBSOLETE.  Use dataset_1d.pro
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

if (0 NE n_elements(parent)) then $
 top_base = widget_base(parent, $
			EVENT_FUNC='HistogramViewerEventFn', $
			KILL_NOTIFY='HistogramViewerCleanup', $
			/COLUMN, /SPACE, /XPAD, /YPAD ) $
else begin
 center = ScreenCenter()
 xoffset = center(0) - 300
 yoffset = center(1) - 250
 top_base = widget_base(TITLE=widget_title,GROUP_LEADER=group, $
			XOFFSET=xoffset, YOFFSET=yoffset, $
			/COLUMN, /SPACE, /XPAD, /YPAD )
endelse

 hist_base = widget_base(top_base, /ROW, SPACE=1, XPAD=1, YPAD=1, $
				/ALIGN_RIGHT )

   button_base = widget_base(hist_base, /ROW, SPACE=1, XPAD=1, YPAD=1, FRAME=2)

    min_button = cw_field(button_base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE=label_min, VALUE=0.0, XSIZE=13)

    default_button = widget_button(button_base, VALUE='Default') 

    binsize_button = cw_field(button_base, /ROW, /FLOAT, /RETURN_EVENTS, $
			 TITLE=label_bin, VALUE=1.0, XSIZE=13)


if (0 NE n_elements(parent)) then $
   done_button = 0L $
else $
   done_button = widget_button(hist_base, VALUE='Dismiss') 

 plot_viewer, plot_viewer, PARENT_WIDGET=top_base


; Setup state structure.
state = { $ 
	;IDs of widgets that generate events or need to be updated. 
	;Many of these widgets have "value" slots that hold state information.

	plot_viewer:plot_viewer, $
	min_button:min_button, binsize_button:binsize_button, $
	default_button:default_button, done_button:done_button }


;; Save state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

;; If this is top-level widget, realize it, draw the histogram, and register.
if (0 EQ n_elements(parent)) then begin
  widget_control, top_base, /REALIZE

  xmanager, 'pass1_histogram_viewer', top_base, GROUP_LEADER=group, /NO_BLOCK, $
	CLEANUP='HistogramViewerCleanup', EVENT_HANDLER='HistogramViewerEvent'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO HistogramViewerCleanup, top_base

widget_control, top_base, GET_UVALUE=st, /NO_COPY

return
end

;==========================================================================
;;; Histogram Display Routine: RedrawHistogramViewer
;==========================================================================
PRO RedrawHistogramViewer, st, dataset_name, _EXTRA=extra, $
			   AUTOMIN=automin, AUTOBIN=autobin 

widget_control, /HOURGLASS

;; Retrieve the histogram data and parameters.
plot_viewer, st.plot_viewer, DATASET_NAME=dataset_name, GET_UPTR=get_uptr
hist_st = *get_uptr


;; Check for non-positive binsize or an excessive number of bins.
;; Check for silly min value.
if (hist_st.binsize LE 0.0) then hist_st.binsize = 1.0

if ( (hist_st.data_max - hist_st.min) / hist_st.binsize GT 1E6 ) then $
  autobin = 1
  
hist_st.min = hist_st.min < hist_st.data_max


;; Handle AUTOMIN & AUTOBIN keywords
if keyword_set(autobin) then begin
  ; If we're also going to set hist_st.min, then we want the full range of
  ; the data to span ~100 bins, otherwise we want the data ABOVE the
  ; chosen hist_st.min to span ~100 bins.
  
  if keyword_set(automin) then begin
    hist_st.binsize = (hist_st.data_max - hist_st.data_min)/100.0
  endif else begin
    hist_st.binsize = (hist_st.data_max - hist_st.min)/100.0
  endelse
  
  ; Reduce the precision of the binsize number.
  hist_st.binsize = float( limit_precision(hist_st.binsize, 2) )
  
  ; Make sure we don't end up with a binsize of zero!  
  if (hist_st.binsize LE 0.0) then hist_st.binsize = 1.0
endif

if keyword_set(automin) then begin
  hist_st.min = hist_st.data_min - hist_st.binsize/2.0
  
  ; Reduce the precision of the min number.
  hist_st.min = float( limit_precision(hist_st.min, 3) )
endif


;; Store and show the revised histogram parameters.
*get_uptr = hist_st
widget_control, st.min_button,     SET_VALUE=hist_st.min
widget_control, st.binsize_button, SET_VALUE=hist_st.binsize


;; Compute histogram and pass to plot_viewer.
;print, 'computing histogram of ' + dataset_name

hist   = histogram( *hist_st.data, MIN=hist_st.min, BINSIZE=hist_st.binsize, $
		    REVERSE_INDICES=revind )
n_bins = n_elements(hist)

x_data = lindgen(n_bins) * hist_st.binsize + hist_st.min + hist_st.binsize/2.0

hist   = [0, hist, 0]
x_data = [x_data(0)-hist_st.binsize, x_data, x_data(n_bins-1)+hist_st.binsize]


;--------------------------------------------------------------------------
;; Compute standard deviations for each bin using the usual SQRT(N) counting 
;; statistics with the fudge that bins with ZERO counts have std_dev = 1.0.
;--------------------------------------------------------------------------

stddev = sqrt( hist )
index  = where( stddev EQ 0, count )
if (count GT 0) then stddev(index) = 1


plot_viewer, st.plot_viewer, x_data, hist, DATASET_NAME=dataset_name, $
		     STANDARD_DEVIATION=stddev, PSYM=10, _EXTRA=extra


;; Store the reverse indexes.
*hist_st.revind = revind

return
end

;==========================================================================
;;; Widget Event Handler Function
;==========================================================================
FUNCTION HistogramViewerEventFn, Event

new_event   = 0
destroy_flag = 0
default_flag = 0

;; Get the state structure and histogram data.
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st, /NO_COPY


;; Get the currently selected dataset's name, type, and uptr.
plot_viewer, st.plot_viewer, SELECTED_DATASET_NAME=dataset_name
plot_viewer, st.plot_viewer, DATASET_NAME=dataset_name, $
		  GET_UPTR=get_uptr, GET_UTYPE=utype

;; Process the event.
case Event.ID of

; plot_viewer
  st.plot_viewer : $
   begin
   if (event.value EQ 'dataset changed') then begin
     ;; A new dataset has been selected by the user.  If it is a histogram
     ;; dataset, then update the MIN and BIN fields, otherwise desensitize
     ;; them.
     if (utype EQ 'histogram') then begin
       hist_st = *get_uptr

       widget_control, st.min_button,     SENS=1, SET_VALUE=hist_st.min
       widget_control, st.binsize_button, SENS=1, SET_VALUE=hist_st.binsize
       widget_control, st.default_button, SENS=1
     endif else begin
       widget_control, st.min_button,     SENS=0
       widget_control, st.binsize_button, SENS=0
       widget_control, st.default_button, SENS=0
     endelse
   endif
   new_event = event
   new_event.ID = top_base
   end

; Min field
  st.min_button : $
   begin
   ;; Store the new value in the hist_st structure.
   widget_control, st.min_button,     GET_VALUE=min
   hist_st = *get_uptr
   hist_st.min = min
   *get_uptr = hist_st

   RedrawHistogramViewer, st, dataset_name
   end

; Binsize field
  st.binsize_button : $ 
   begin
   ;; Store the new value in the hist_st structure.
   widget_control, st.binsize_button, GET_VALUE=binsize
   hist_st = *get_uptr
   hist_st.binsize = binsize
   *get_uptr = hist_st

   RedrawHistogramViewer, st, dataset_name
   end

; Default button
  st.default_button : $
   begin
   ;; Change the min and binsize values and redraw with default axes.
   RedrawHistogramViewer, st, dataset_name, /AUTOMIN, /AUTOBIN, /DEFAULT_AXES
   end

; DONE button
  st.done_button : destroy_flag = 1

  else: print, 'unknown event in pass1_histogram_viewer'
endcase

;; Save the state structure.
widget_control, top_base, SET_UVALUE=st, /NO_COPY


if destroy_flag then begin
  widget_control, top_base, /DESTROY
endif

return, new_event
end


;==========================================================================
;;; Widget Event Handler Procedure
;==========================================================================
PRO HistogramViewerEvent, Event

event = HistogramViewerEventFn( Event )
return
end

;==========================================================================
;;; MAIN "pass1_histogram_viewer" ROUTINE
;==========================================================================
PRO pass1_histogram_viewer, top_base, data, DATASET_NAME=dataset_name, $
		      MIN=min, BINSIZE=binsize, $
		      AUTOMIN=automin, AUTOBIN=autobin, $ 
		      LABEL_MIN=label_min, LABEL_BIN=label_bin, $

		     ;ROI_NAME=roi_name, 
		      APPLY_ROI=apply_roi, $
		      ROI_MASK=roi_mask, _EXTRA=extra, $

		      PARENT_WIDGET=parent, GROUP=group, $
		      WIDGET_TITLE=widget_title

;; If the widget ID of an existing plot_viewer was not passed, the create
;; the widget.
create_flag = 1

if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if create_flag then begin

  ;; Default values
  if (0 EQ n_elements(group))        then group  = 0
  if (0 EQ n_elements(widget_title)) then widget_title ='Histogram Viewer'
  if (0 EQ n_elements(label_min))    then label_min = 'First Bin'
  if (0 EQ n_elements(label_bin))    then label_bin = 'Binsize'

  top_base = CreateHistViewer( parent, group, widget_title, $
				label_min, label_bin)
endif


;;---------------------------------------------------------------------------
;; Get the state structure.
if (NOT keyword_set(st)) then $
widget_control, top_base, GET_UVALUE=st, /NO_COPY
 

if (NOT keyword_set(dataset_name)) then dataset_name = 'histogram'

;;---------------------------------------------------------------------------
;; Handle parameters which involve modifying a dataset.
 
dataset_modified = (0 NE n_elements(data)) OR $
		   (0 NE n_elements(min))  OR $ 
		   keyword_set(binsize)    OR $
		   keyword_set(automin)    OR $
		   keyword_set(autobin)

		   
if (dataset_modified) then begin
  
  ;; Create or retrieve a plot_viewer dataset.
  plot_viewer, st.plot_viewer, DATASET_NAME=dataset_name, $
		GET_UPTR=get_uptr, GET_UTYPE=utype
  
  ;; Retrieve or create a structure to hold the histogram data and parameters.
  if (utype EQ 'histogram') then begin
    hist_st = *get_uptr
  endif else begin
    plot_viewer, st.plot_viewer, DATASET_NAM=dataset_name, SET_UTYPE='histogram'

    widget_control, st.min_button,     GET_VALUE=min_button
    widget_control, st.binsize_button, GET_VALUE=binsize_button

    ; These heap vars are going to have to be freed in PlotViewerCleanup().
    hist_st = { data:   ptr_new(/ALLOC), $
		revind: ptr_new(/ALLOC), $
		min:min_button, binsize:binsize_button, $
		data_min:0.0, data_max:0.0 }
    *hist_st.data = [0]
    *hist_st.revind = [0]
  endelse

  if (N_ELEMENTS(data) NE 0) then begin
    if ('Double' EQ datatype( data, 1 )) then $
      message, 'ERROR: parameter DATA cannot be of type DOUBLE'

    ;; Store the data on the handle in the structure.
    *hist_st.data = float(data)
    hist_st.data_min = min( data, MAX=data_max )
    hist_st.data_max = data_max
  endif

  ;; If necessary, change the min and binsize parameters.
  if (0 NE n_elements(min)) then  hist_st.min     = min
  if keyword_set(binsize)   then  hist_st.binsize = binsize

  ;; Store the structure back on its pointer.
  *get_uptr = hist_st

  ;; Compute and display the histogram.
  RedrawHistogramViewer, st, dataset_name, _EXTRA=extra, $
  		AUTOMIN=keyword_set(automin), AUTOBIN=keyword_set(autobin)
endif else begin 
  ;; If we're not going to recompute histogram, then we need some way
  ;; to pass on any _EXTRA keywords to plot_viewer.
  plot_viewer, st.plot_viewer, DATASET_NAME=dataset_name, _EXTRA=extra
endelse



;;---------------------------------------------------------------------------
;; Handle the APPLY_ROI keyword.
if keyword_set(apply_roi) then begin

  ;; Apply the specified ROI_NAME (which is expected to be in _EXTRA) to
  ;; the specified dataset, returning a handle that holds a mask vector.
  plot_viewer, st.plot_viewer, DATASET_NAME=dataset_name, _EXTRA=extra, $
	       /APPLY_ROI, ROI_MASK=mask_ptr, GET_UPTR=get_uptr

  hist_st = *get_uptr

  ;; The TRICKY part here is that the histogram vector to which revind
  ;; corresponds, was augmented by a zero at each end before it was passed
  ;; to plot_viewer (to make it look pretty).  Thus, the vector bin_mask
  ;; is two elements longer than the histogram vector we're interested in.
  bin_mask = (*mask_ptr)(1 : n_elements(*mask_ptr)-2)


  ;; Convert the bin_mask vector returned from plot_viewer, which indicates
  ;; which bins are accepted, into a much larger roi_mask vector, which
  ;; indicates which datapoints are accepted.

  roi_mask = bytarr( n_elements(*hist_st.data) )

  bins_in_roi = where( bin_mask, count )
  for ii = 0L, count-1 do begin
    first = (*hist_st.revind)(bins_in_roi(ii))
    last  = (*hist_st.revind)(bins_in_roi(ii)+1) - 1

    if (last GE first) then begin
      indexes_of_data_in_roi = (*hist_st.revind)( first : last )
      roi_mask( indexes_of_data_in_roi ) = 1
    endif
  endfor
endif

;; Save the state structure.
widget_control, top_base, SET_UVALUE=st, /NO_COPY

return
end

