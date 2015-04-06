; $Id
;
;+
;========================================================================
;;;
;;; FILE NAME:    @(#)pass1_plot_viewer.pro	9.30
;;;
;;; DESCRIPTION:  Plot Viewer Compound Widget
;;;               Used to display a 2-D plot.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;;
;;; INTERFACE TO CLIENT
;;;
;PRO pass1_plot_viewer, top_base, param1, param2, DATASET_NAME=dataset_name, $
;		 STANDARD_DEVIATION=standard_deviation, $
;
;		 TITLE=title, SUBTITLE=subtitle, XTITLE=xtitle, YTITLE=ytitle, $
;		 COLOR=color, LINESTYLE=linestyle, PSYM=psym, $
;		 DESCRIPTION=description, SCATTER=scatter, $
;		 HIDE_DATASET=hide_dataset, ENABLE_DELETE=enable_delete, $
;
;		 GET_UTYPE=get_utype, SET_UTYPE=set_utype, $
;		 GET_UPTR=get_uptr, $
;
;		 ROI_NAME=roi_name, ROI_STYLE=roi_style, HIDE_ROI=hide_roi, $
;		 APPLY_ROI=apply_roi, ROI_MASK=roi_mask, $
;
;		 DELETE=delete, DEFAULT_AXES=default_axes, $
;		 LIST_OF_DATASETS=list_of_datasets, $
;		 SELECTED_DATASET_NAME=selected_dataset_name, $
;
;		 CLICK=click, MOUSE_POSN=mouse_posn, PROMPT=prompt, $
;		 _EXTRA=extra 
;		 
;		 PARENT_WIDGET=parent, GROUP=group, $
;		 WIDGET_TITLE=widget_title

;;;
;;; The parameters PARENT_WIDGET & GROUP are the usual ones associated 
;;; with widgets.  If pass1_plot_viewer is a top-level widget, then WIDGET_TITLE
;;; controls the title in the X-window frame.
;;;
;;; Parameters and keywords are similar to the IDL "plot" function.
;;; If only Y data are available, pass them in "param1" and omit "param2".
;;; If both X & Y data are available, pass the X data in "param1" and pass 
;;; the Y data in "param2".  Both vectors should have the same number of
;;; elements.  Neither vector may be of type DOUBLE!!

;;; If a dataset is supplied (with param1 and/or param2), then standard 
;;; deviation values may also be passed 
;;; in the keyword STANDARD_DEVIATION, which should be a vector with the same 
;;; number of elements as param1.

;;; COLOR can have the values 'red', 'blue', 'green', or 'white'.

;;; LINESTYLE can have the values 6 (none), 0 (solid), 1 (dotted), 
;;; 2 (dashed), 3 (dash dot), 4 (dash dot dot dot), 5 (long dashes).

;;; PSYM can have the values 0 (none), 1 (+), 2 (*), 3 (.), 
;;; 4 (diamond), 5 (triangle), 6 (box), 7 (X), 10(histogram).

;;; A title, subtitle, and axis titles may be placed on the plot with the 
;;; keywords TITLE, SUBTITLE, XTITLE, & YTITLE. 


;;; The pass1_plot_viewer can work with multiple named datasets.
;;; Multiple datasets are passed with multiple calls to pass1_plot_viewer -- see
;;; example below.
;;; A dataset may be deleted by the caller using /DELETE.


;;; The state of this widget is stored as an anonymous structure in the 
;;; user value slot of the first child (the normal convention).

;;; The "generic format" selection under the "Save" menu writes a simple ASCII
;;; file containing the number of data points followed by all the (x,y) pairs 
;;; of data points.
;;; The "plotxy" format is defined by Dave Burrows.


;==========================================================================
;;; EVENTS GENERATED

;;; Whenever a ROI is changed by the user, an event is generated with the
;;; following structure elements:
;;;   VALUE:    'ROI changed' 
;;;   ROI_NAME: the name (a string) of the ROI that changed
;==========================================================================


;==========================================================================
;;;                     CALLING OPTIONS

;;; From a non-widget program or the command line, plot vectors X & Y 
;;; and error vector sigma in a top-level pass1_plot_viewer. 
;;; ** pass1_plot_viewer, id, X, Y, STANDARD_DEVIATION=sigma
;;; ** xmanager

;;; From a non-widget program or the command line, plot two datasets 
;;; in a top-level plot_viewer.
;;; ** pass1_plot_viewer, id, WIDGET_TITLE='My Data'
;;; ** pass1_plot_viewer, id, X1, Y1, DATASET_NAME='First Dataset'
;;; ** pass1_plot_viewer, id, X2, Y2, DATASET_NAME='Second Dataset'
;;; ** xmanager

;;; From a widget program, create a top-level pass1_plot_viewer.
;;; ** pass1_plot_viewer, id, GROUP=creator_widget_id

;;; Create a pass1_plot_viewer as a child of another widget.
;;; ** pass1_plot_viewer, id, PARENT_WIDGET=base

;;; Delete a dataset from an existing pass1_plot_viewer.
;;; ** pass1_plot_viewer, id, /DELETE, DATASET_NAME=name

;;; Force an existing pass1_plot_viewer to redraw using the default axis ranges.
;;; ** pass1_plot_viewer, id, /DEFAULT_AXES

;;; Add a named region-of-interest in a specified style to the pass1_plot_viewer.
;;; ** pass1_plot_viewer, id, ROI_NAME=roi_name, ROI_STYLE=roi_style

;;; Obtain a handle to a byte array that shows which data points in the 
;;; specified dataset are inside the specified ROI.
;;; ** pass1_plot_viewer, id, DATASET_NAME=dn, ROI_NAME=rn, /APPLY_ROI, ROI_MASK=mask

;;; Modally obtain the DATA coordinates of a mouse click on the plot.
;;; The caller is responsible for prompting the user.
;;; ** pass1_plot_viewer, id, /CLICK, MOUSE_POSN=point, PROMPT='Select a point'

;;; Call pass1_plot_viewer from inside it's own event handler, in which case
;;; we already have the state structure.  You must omit "id".
;;; ** pass1_plot_viewer, STATE=st, ...


;==========================================================================
;-

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreatePlotViewer, PARENT_WIDGET=parent, GROUP=group, $
			   WIDGET_TITLE=widget_title

;; Default values
if (0 EQ n_elements(group))          then group = 0
if (0 EQ n_elements(widget_title))   then widget_title = 'Plot Viewer'
  

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

if (0 NE n_elements(parent)) then $
 top_base = widget_base( parent, /BASE_ALIGN_CENTER, $
			 EVENT_FUNC='PlotViewerEventFn', $
			 KILL_NOTIFY='PlotViewerCleanup', $
			 /COLUMN, /SPACE, /XPAD, /YPAD ) $
else begin
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print, 'THIS TOOL IS OBSOLETE.  Use function_1d.pro
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
 center = ScreenCenter()
 xoffset = center(0) - 300
 yoffset = center(1) - 250
 top_base = widget_base(TITLE=widget_title,GROUP_LEADER=group, $
                        XOFFSET=xoffset, YOFFSET=yoffset, $
                        /BASE_ALIGN_CENTER, /COLUMN, /SPACE, /XPAD, /YPAD )
endelse

 upper_base = widget_base( top_base, /BASE_ALIGN_TOP, /ROW, $
			   /SPACE, /XPAD, /YPAD )


  draw_widget = widget_draw(upper_base, /BUTTON_EVENTS, RETAIN=1, $
			 XSIZE=580, YSIZE=410 )


 lower_base = widget_base(top_base, /ROW, /SPACE, /XPAD, /YPAD)

; left_base = widget_base( lower_base, /COLUMN, /SPACE, /XPAD, /YPAD, /FRAME )
  left_base = widget_base( lower_base, /COLUMN, /SPACE, /XPAD, /YPAD)

   ; List of the datasets
   label = widget_label( left_base, VALUE='Dataset List', /ALIGN_CENTER )

   dataset_list = widget_list( left_base, XSIZE=32, YSIZE=4 )


   ;; Dataset management controls
   base = widget_base( left_base, /BASE_ALIGN_CENTER, /ROW, $
		       /SPACE, /XPAD, /YPAD )

     menu = [{ CW_PDMENU_S, flags:1, name:'Load' }, $ 
	     { CW_PDMENU_S,       2,      'ASCII format' }] 
     load_menu = cw_pdmenu(base, menu, /RETURN_INDEX)

     menu = [{ CW_PDMENU_S, flags:1, name:'Save' }, $ 
	     { CW_PDMENU_S,     0,      'selected dataset' }, $ 
	     { CW_PDMENU_S,     2,      'all datasets (PlotXY format)' }]
     save_menu = cw_pdmenu(base, menu, /RETURN_INDEX)

     style_button = widget_button( base, VALUE='Style' )

     delete_button = widget_button( base, VALUE='Delete' )

     copy_button = widget_button( base, VALUE='Copy' )



  middle_base = widget_base( lower_base, /BASE_ALIGN_CENTER, /COLUMN, $
			 SPACE=2, /XPAD, /YPAD )

   format_button = widget_button( middle_base, VALUE='Options' )

   print_button = widget_button( middle_base, VALUE='Print')

   base = widget_base( middle_base, /COLUMN, /SPACE, /XPAD, /YPAD )

    x_point = widget_label( base, /DYNAMIC_RESIZE, VALUE='', /ALIGN_LEFT )
    y_point = widget_label( base, /DYNAMIC_RESIZE, VALUE='', /ALIGN_LEFT ) 

   if (0 EQ n_elements(parent)) then begin
      DoneButton = widget_button(middle_base, VALUE='Dismiss') 
   endif else begin
      DoneButton = 0L 
   endelse


; right_base = widget_base( lower_base, /COLUMN, /SPACE, /XPAD, /YPAD, /FRAME )
  right_base = widget_base( lower_base, /COLUMN, /SPACE, /XPAD, /YPAD)

   plot_viewer_roi, pv_roi, PARENT_WID=right_base, $
		    PLOT_VIEWER=top_base, ROI_NAME='AXIS RANGES', $
		    /HIDE_ROI, STYLE='box_interior'

   plot_viewer_roi, pv_roi, ROI_NAME='fitting & stats', STYLE='none'


; Setup state structure.
dataset = { name:'', description:'', utype:'', uptr:ptr_new(/ALLOC), $
	    hidden_flag:0, delete_flag:0, error_bar_flag:0, $
	    scatter_flag:0, image_flag:0, $
	    num_data_points:0L, $
	    x_data:ptr_new(/ALLOC), y_data:ptr_new(/ALLOC), $
	    standard_deviation:ptr_new(/ALLOC), $
	    linestyle:0, psym:0, color:'white', $
	    y_max:0.0, y_min:0.0, x_max:0.0, x_min:0.0 }
	
state = { $ 
	;IDs of widgets that generate events or need to be updated. 
	;Many of these widgets have "value" slots that hold state information.

	pv_roi:pv_roi, $
	format_button:format_button, dataset_list:dataset_list, $
	delete_button:delete_button, copy_button:copy_button, $
	style_button:style_button, $
	save_menu:save_menu, load_menu:load_menu, $
	fit_widget:0L, tv_widget:0L, print_button:print_button, $
	x_point:x_point, y_point:y_point, $

	msg_id:0L, DoneButton:DoneButton, draw_widget:draw_widget, $

	;Dataset structures
	selected_name:'', datasets: replicate( dataset, 20 ), $
	copy_number: 0, $

	;Other state information.
	invalid_axis_ranges: 0, use_default_axis_ranges:1, $
	default_xrange:[0.,1.], default_yrange:[0.,1.], $
	title:'Y vs X', subtitle:'', xtitle:'X', ytitle:'Y', $

	xlog:0, ylog:0, xlog_handled:0, ylog_handled:0, $

	show_date:1, show_legend:0 }
	
ptr_free, dataset.x_data, dataset.y_data, $
	  dataset.standard_deviation, dataset.uptr

;; Allocate heap variables in dataset structures.
for ii = 0, n_elements(state.datasets)-1 do begin
  state.datasets(ii).x_data  = ptr_new(/ALLOC)
  state.datasets(ii).y_data  = ptr_new(/ALLOC)
  state.datasets(ii).standard_deviation  = ptr_new(/ALLOC)
  state.datasets(ii).uptr    = ptr_new(/ALLOC)
endfor

;; Save state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; If this is top-level widget, realize it and register.
if (0 EQ n_elements(parent)) then begin
  widget_control, top_base, /REALIZE

  xmanager, 'pass1_plot_viewer', top_base, GROUP_LEADER=group, /NO_BLOCK, $
	    EVENT_HANDLER='PlotViewerEvent', CLEANUP='PlotViewerCleanup'
endif

return, top_base
END

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO PlotViewerCleanup, top_base

widget_control, top_base, GET_UVALUE=st, /NO_COPY

;; Free all the heap vars allocated by histogram_viewer.
uptrs  = st.datasets.uptr
utypes = st.datasets.utype
for ii = 0, n_elements(uptrs)-1 do begin
  if ('histogram' EQ utypes(ii)) then $
    ptr_free, (*uptrs(ii)).data, (*uptrs(ii)).revind
endfor

;; Free the heap vars allocated locally.
ptr_free, st.datasets.x_data, st.datasets.y_data, $
          st.datasets.standard_deviation, st.datasets.uptr
return
end

;==========================================================================
;;; Handle Log Scaling
;;
;; This routine is used to handle log scaling.  If log scaling is set,
;; then we need to do something with zero and negative points.  
;; It turns out that simply making sure the plot axis ranges are positive
;; will work, even if negative data is plotted.
;; So, if there are non-positive data points, then we'll set the axis
;; minimum value to 100 times smaller than the smallest positive data point.
;; Be sure to warn the user that this is happening.
;==========================================================================
PRO HandlePlotViewerLogScaling, st, Xrange, Yrange
if ((st.xlog EQ 0) AND (st.ylog EQ 0)) then return

;; Check the axis ranges.  If they include negative values, then change them.

if(Xrange(0) LE 0.0) then st.xlog_handled = 0
if(Yrange(0) LE 0.0) then st.ylog_handled = 0

if ((st.xlog EQ 1) AND (st.xlog_handled EQ 0))then begin
  ;; Find the smallest value in the dataset
  st.xlog_handled = 1
  save_min = 1.0e32
  indexes = where((st.datasets.name NE '') AND $
		  (st.datasets.hidden_flag EQ 0), count)
  if(count NE 0) then begin
    plotsets = st.datasets(indexes)
    for ii = 0, count-1 do begin
      x_data = plotsets(ii).x_data
      this_min = min( (*x_data)(where(*x_data > 0.0)))
      if (this_min < save_min) then save_min = this_min
    endfor
  endif else begin
	save_min = 1.0
  endelse
  save_min = save_min / 100.0

  if(Xrange(0) LE 0.0) then Xrange(0) = save_min
endif

if ((st.ylog EQ 1) AND (st.ylog_handled EQ 0))then begin
  ;; Find the smallest value in the dataset
  st.ylog_handled = 1
  save_min = 1.0e32
  indexes = where((st.datasets.name NE '') AND $
		  (st.datasets.hidden_flag EQ 0), count)
  if(count NE 0) then begin
    plotsets = st.datasets(indexes)
    for ii = 0, count-1 do begin
      y_data = plotsets(ii).y_data
      this_min = min( (*y_data)(where(*y_data > 0.0)))
      if (this_min < save_min) then save_min = this_min
    endfor
  endif else begin
	save_min = 1.0
  endelse
  save_min = save_min / 100.0

  if(Yrange(0) LE 0.0) then Yrange(0) = save_min
endif

return
END


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawPlotViewer, st, BUTTONS_ONLY=buttons_only

widget_control, /HOURGLASS

;--------------------------------------------------------------------------
;; Update various widget properties.
;--------------------------------------------------------------------------
widget_control, st.x_point, SET_VALUE=' '
widget_control, st.y_point, SET_VALUE=' '

;; Redraw  the dataset list widget to show all the non-null datasets.
;; Verify that st.selected_name is a real dataset.
ds_indexes = where( st.datasets.name NE '' )
names      = st.datasets(ds_indexes).name
hide_flags = st.datasets(ds_indexes).hidden_flag

selected_index = (where( names EQ st.selected_name, count ))(0)

if (count EQ 0) then begin
  st.selected_name = names(0)
  selected_index   = 0
endif

selected_dataset = st.datasets( selected_index )

indexes = where( hide_flags EQ 1, count )
if (count NE 0) then names(indexes) = '{' + names(indexes) + '}'

widget_control, st.dataset_list, SET_LIST_SEL=selected_index, SET_VAL=names


;; Set button sensitivities.
if (selected_dataset.delete_flag) then begin
  widget_control, st.delete_button,    SENSITIVE=1
endif else begin
  widget_control, st.delete_button,    SENSITIVE=0
endelse



;; If a dataset has been changed, recompute the default axis ranges.
if (st.invalid_axis_ranges) then begin
  st.invalid_axis_ranges = 0

  ;; Modify the default axis ranges.
  datasets = st.datasets(ds_indexes)
  x_max    = max( datasets.x_max )
  y_max    = max( datasets.y_max )
  x_min    = min( datasets.x_min )
  y_min    = min( datasets.y_min )

  plot_viewer_roi, st.pv_roi, DEFAULT_XRANGE=[x_min, x_max], $
                              DEFAULT_YRANGE=[y_min, y_max]
  st.default_xrange = [x_min,x_max]
  st.default_yrange = [y_min,y_max]
endif


;; If desired, configure the axis range widgets to use the default ranges.
if (st.use_default_axis_ranges) then begin
  st.use_default_axis_ranges = 0
  plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', STYLE='box_interior'
endif

if keyword_set(buttons_only) then return


;--------------------------------------------------------------------------
;; Setup the device and system variables correctly.
;--------------------------------------------------------------------------
color_manager, RED=red, BLACK=black

if (!D.NAME EQ 'X') then begin
  Font = -1   ; "vector drawn" font
  coordsystem_manager, st.draw_widget, /RESTORE
endif else begin
  Font =  0   ; "hardware" font 
endelse
 

!X.MARGIN = [10,3]  &  !Y.MARGIN=[4,2]

;--------------------------------------------------------------------------
;; Figure out the axis ranges.
;--------------------------------------------------------------------------
;; Save a copy of the ROI 'AXIS RANGES' so we can muck with it.
restore_roi_flag = 0
plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', GET_ROI=save_roi
roi = save_roi

case roi.style of
 'x_range': $
  begin
  x_min = roi.point_a(0)
  x_max = roi.point_b(0)
  y_min = st.default_yrange(0)
  y_max = st.default_yrange(1)
  end
 'y_range': $
  begin
  x_min = st.default_xrange(0)
  x_max = st.default_xrange(1)
  y_min = roi.point_a(1)
  y_max = roi.point_b(1)
  end
 'box_interior': $
  begin
  x_min = roi.point_a(0)
  x_max = roi.point_b(0)
  y_min = roi.point_a(1)
  y_max = roi.point_b(1)
  end

 'annulus': $
  begin
  ; We want to choose axis ranges so the plot has a 1:1 aspect ratio
  ; with respect to the device coordinates.
  ; We can NOT use !X.WINDOW because for the PostScript device, we have
  ; not yet set up a plot coordinate system.  If we did that with a plot
  ; command, we'd get two pages of PostScript output.
  x_size_dev = !D.X_SIZE - !D.X_CH_SIZE * total( !X.MARGIN )
  y_size_dev = !D.Y_SIZE - !D.Y_CH_SIZE * total( !Y.MARGIN )

  data_per_dev = roi.length_b / min([x_size_dev,y_size_dev])

  x_min = roi.point_a(0) - x_size_dev * data_per_dev
  x_max = roi.point_a(0) + x_size_dev * data_per_dev
  y_min = roi.point_a(1) - y_size_dev * data_per_dev
  y_max = roi.point_a(1) + y_size_dev * data_per_dev

  restore_roi_flag = 1
  roi.style = 'box_interior'
  roi.point_a = [x_min, y_min]
  roi.point_b = [x_max, y_max]
  plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', SET_ROI=roi
  end

 else: $
  begin
  x_min = st.default_xrange(0)
  x_max = st.default_xrange(1)
  y_min = st.default_yrange(0)
  y_max = st.default_yrange(1)

  ; The user has chosen a forbidden style, so let's change it.
  plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', STYLE='box_interior'
  plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', GET_ROI=save_roi
  roi = save_roi
  end
endcase

Xrange = [x_min-(x_min EQ x_max),x_max+(x_min EQ x_max)]

Yrange = [y_min-(y_min EQ y_max),y_max+(y_min EQ y_max)]

HandlePlotViewerLogScaling, st, Xrange, Yrange

;--------------------------------------------------------------------------
;; Draw the plot axes, title, etc.
;; The BACKGROUND keyword is ignored by the PostScript device.
;--------------------------------------------------------------------------
plot, [0], /NODATA, FONT=Font, TITLE=st.title, SUBTITLE=st.subtitle, $
        YTITLE=st.ytitle, YRANGE=Yrange, YSTYLE=3, $
        XTITLE=st.xtitle, XRANGE=Xrange, XSTYLE=3, $
        XLOG=st.xlog, YLOG=st.ylog, COLOR=red, BACKGROUND=black


;--------------------------------------------------------------------------
;; Plot all the non-null and not-hidden datasets.
;--------------------------------------------------------------------------
indexes = where((st.datasets.name NE '') AND $
		(st.datasets.hidden_flag EQ 0), dataset_count)

if (dataset_count NE 0) then begin

  plotsets = st.datasets(indexes)

  ;; See if we're supposed to TV an image instead of plotting datasets.
  indexes = where( plotsets.image_flag, count )
  if (count GT 0) then begin
    dataset_count = 0
    plot_viewer_tv, st.tv_widget, DATASET=plotsets(indexes(0)), /DRAW
  endif

  ;; Plot all the datasets
  for ii = 0, dataset_count-1 do begin

    ;----------------------------------------------------------------------
    ;; Figure out which data points we want to plot.
    ;; When we get here the ROI 'roi' is either 'x_range', 'y_range', or
    ;; 'box_interior'.
    ;----------------------------------------------------------------------

    if ((plotsets(ii).psym EQ 10) OR (plotsets(ii).linestyle NE 6)) then begin 
      ; If this dataset involves drawing lines, then we must use an x-range
      ; style of ROI, including data that lie above and below the plot, so 
      ; that we get the appropriate lines connecting the data points.
      ; We may NOT sample since that would mess up the lines too.
      temp_roi  = roi
      roi.style = 'x_range'
      roi.point_a = [x_min, 0]
      roi.point_b = [x_max, 0]
      plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', SET_ROI=roi
      plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', GET_ROI=roi, $
				  DATASET=plotsets(ii), /APPLY_ROI

      mask = roi.roi_mask
      pts_to_plot = where( *mask, point_count )
      roi = temp_roi
      plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', SET_ROI=roi

    endif else begin
      ; If the plot will NOT have lines connecting the data points, then we 
      ; can simply plot the data points that fall inside the axis ranges, and
      ; we are free to sample the points we need to plot to avoid saturation
      ; of the plotting area.
      plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', GET_ROI=roi, $
				  DATASET=plotsets(ii), /APPLY_ROI

      mask = roi.roi_mask
      pts_to_plot = where( *mask, point_count )

      if (point_count GT 20000) then begin
        print, 'Sampling dataset in pass1_plot_viewer'
        point_count = 10000
        pts_to_plot = congrid( temporary(pts_to_plot), point_count )
      endif
    endelse

    if (point_count EQ 0) then pts_to_plot = [0]


    ;; Retrieve the dataset pointers.
    x_data = plotsets(ii).x_data
    y_data = plotsets(ii).y_data
    sigma  = plotsets(ii).standard_deviation

    ;; If the points we want to plot are a subset of the whole dataset,
    ;; then extract the points we want to plot.
    sampled_data_flag = (n_elements(pts_to_plot) LT n_elements(*x_data))

    if (sampled_data_flag) then begin
      x_data = ptr_new( (*x_data)(pts_to_plot) )
      y_data = ptr_new( (*y_data)(pts_to_plot) )

      if (n_elements(*sigma) NE 0) then begin
	sigma = ptr_new( (*sigma)(pts_to_plot) )
      endif else begin
        sigma  = ptr_new( /ALLOC )
      endelse
    endif


    ;----------------------------------------------------------------------
    ;; Plot the data with the correct style attributes.
    ;----------------------------------------------------------------------
    color_manager, plotsets(ii).color, plot_color 

    if (plotsets(ii).psym EQ 8) then plotsets(ii).psym = 10

    if (plotsets(ii).linestyle EQ 6) then begin 
      ;We want symbols only.
      plotsets(ii).linestyle = 0

      oplot, *x_data, *y_data, PSYM=plotsets(ii).psym, COLOR=plot_color

      if (plotsets(ii).psym EQ 10) then plotsets(ii).psym = 0

    endif else begin                           
      ;We want both lines and symbols.
      if (plotsets(ii).psym EQ 10) then begin
        oplot, *x_data, *y_data, LINESTYLE=plotsets(ii).linestyle, $
			       PSYM=plotsets(ii).psym, COLOR=plot_color
	plotsets(ii).psym = 0
      endif else begin
        plotsets(ii).psym = -plotsets(ii).psym
        oplot, *x_data, *y_data, LINESTYLE=plotsets(ii).linestyle, $
			       PSYM=plotsets(ii).psym, COLOR=plot_color
      endelse
    endelse


    ;----------------------------------------------------------------------
    ;; Plot error bars
    ;----------------------------------------------------------------------
    if ((n_elements(*sigma) NE 0)          AND $
        (plotsets(ii).error_bar_flag EQ 1) AND $
	(point_count LT 100)                     ) then begin

      errplot, *x_data, *y_data - *sigma, *y_data + *sigma
    endif 


    ;----------------------------------------------------------------------
    ;; If we were sampling, then we had to allocate new heap variables
    ;; which we now need to destroy.
    ;----------------------------------------------------------------------
    if (sampled_data_flag) then ptr_free, x_data, y_data, sigma
  endfor ;; ii loop


  ;------------------------------------------------------------------------
  ;; Draw a legend
  ;------------------------------------------------------------------------
  ;; Annotate with a legend if desired.  Be careful of color handling.  When
  ;; in grayscale Postscript mode, there are no colors in the IDL colormap,
  ;; so just use black.
  if (st.show_legend) then begin
    ; Convert a vector of color names to a vector of color indexes.
    colors = intarr( n_elements(plotsets) )
    for ii = 0, n_elements(plotsets)-1 do begin
      color_manager, plotsets[ii].color, color_index
      colors[ii] = color_index
    endfor
    
    legend, plotsets.description, PSYM=plotsets.psym, LINE=plotsets.linestyle, $
            COLORS=colors, TEXTCOLORS=colors, /TOP, CHARSIZE=0.75
  endif
endif ;(dataset_count NE 0)


;------------------------------------------------------------------------
;; Draw other annotations
;------------------------------------------------------------------------
;; Annotate with the date if desired.  
if (st.show_date) then $
  xyouts, 0.03, 0.01, /NORMAL, getenv('USER') + ' ' + systime(), $
	  ALIGN=0, FONT=font, COLOR=red


;; If necessary, restore the ROI 'AXIS RANGES'. 
if (restore_roi_flag) then $
  plot_viewer_roi, st.pv_roi, ROI_NAME='AXIS RANGES', SET_ROI=save_roi


;; Draw all the ROI's.
plot_viewer_roi, st.pv_roi, LIST_OF_ROIS=roi_names
for ii=0, n_elements(roi_names)-1 do $
  plot_viewer_roi, st.pv_roi, ROI_NAME=roi_names(ii), /DRAW

if (!D.NAME EQ 'X') then begin
  coordsystem_manager, st.draw_widget, /SAVE
endif

return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION PlotViewerEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st, /NO_COPY

selected_index   = (where( st.datasets.name EQ st.selected_name))(0)
selected_dataset = st.datasets( selected_index )

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Plot window mouse events.
  st.draw_widget: $
   begin
   if ((Event.type EQ 0) AND $
       (selected_dataset.hidden_flag EQ  0)) then begin
     coordsystem_manager, st.draw_widget, /RESTORE

     ; Find distance from click to each data point.
     data_locations = convert_coord( *selected_dataset.x_data, /DATA, $
     				     *selected_dataset.y_data, /TO_DEVICE )
     
     distances = (data_locations(0,*) - event.X)^2 + $
     		 (data_locations(1,*) - event.Y)^2
     		 
     ; Display closest data point.
     minDist = min(distances,Imin)
     x_pt    = (*selected_dataset.x_data)(Imin)
     y_pt    = (*selected_dataset.y_data)(Imin)

     fmt = '(G11.5)'
     widget_control, st.x_point, SET_VALUE='X: '+strtrim(string(x_pt,f=fmt),2)
     widget_control, st.y_point, SET_VALUE='Y: '+strtrim(string(y_pt,f=fmt),2)
     tvcrs, x_pt, y_pt, /DATA
   endif
   end


;--------------------------------------------------------------------------
; Format button
  st.format_button : $
   begin
   ; We have to escape any commas that appear in the titles so we
   ; don't confuse cw_form().
   comma = (byte(','))(0)
   slash = (byte('\'))(0)

   title = byte( ' ' + st.title )
   indexes = where( title EQ comma, count )
   for ii=count-1, 0, -1 do $
     title = [title(0:indexes(ii)-1), [slash], title(indexes(ii):*)] 

   subtitle = byte( ' ' + st.subtitle )
   indexes = where( subtitle EQ comma, count )
   for ii=count-1, 0, -1 do $
     subtitle = [subtitle(0:indexes(ii)-1), [slash], subtitle(indexes(ii):*)] 

   xtitle = byte( ' ' + st.xtitle )
   indexes = where( xtitle EQ comma, count )
   for ii=count-1, 0, -1 do $
     xtitle = [xtitle(0:indexes(ii)-1), [slash], xtitle(indexes(ii):*)] 

   ytitle = byte( ' ' + st.ytitle )
   indexes = where( ytitle EQ comma, count )
   for ii=count-1, 0, -1 do $
     ytitle = [ytitle(0:indexes(ii)-1), [slash], ytitle(indexes(ii):*)] 

   f1 = '0, TEXT,' + strtrim( string(title), 2 ) + $
	', TAG=title, LABEL_LEFT=Title, WIDTH=38'

   f2 = '0, TEXT,' + strtrim( string(subtitle), 2 ) + $
	', TAG=subtitle, LABEL_LEFT=Sub-title, WIDTH=34'

   f3 = '0, TEXT,' + strtrim( string(xtitle), 2 ) + $
	', TAG=xtitle, LABEL_LEFT=X-title, WIDTH=34'

   f4 = '0, TEXT,' + strtrim( string(ytitle), 2 ) + $
	', TAG=ytitle, LABEL_LEFT=Y-title, WIDTH=34'

   b1 = '1, BASE,, ROW, FRAME'

   f5 = '0, BUTTON, Linear|Log,' + $
	'EXCLUSIVE, SET_VALUE=' + string(st.xlog) + $
	', TAG=xlog, LABEL_TOP=X-axis Style'

   f6 = '0, BUTTON, Linear|Log,' + $
	'EXCLUSIVE, SET_VALUE=' + string(st.ylog) + $
	', TAG=ylog, LABEL_TOP=Y-axis Style'

   f7 = '0, BUTTON, Hide|Show,' + $
	'EXCLUSIVE, SET_VALUE=' + string(st.show_legend) + $
	', TAG=show_legend, LABEL_TOP=Legend'

   f8 = '2, BUTTON, Hide|Show,' + $
	'EXCLUSIVE, SET_VALUE=' + string(st.show_date) + $
	', TAG=show_date, LABEL_TOP=Date'

   f9 = '2,BUTTON,OK,QUIT,TAG=ok'

   result = cw_form( [f1,f2,f3,f4,b1,f5,f6,f7,f8,f9], /COLUMN, $
		     TITLE='Annotations and Axis Style' )

   st.title       = result.title
   st.subtitle    = result.subtitle
   st.xtitle      = result.xtitle
   st.ytitle      = result.ytitle
   st.xlog        = result.xlog
   st.ylog        = result.ylog
   st.show_legend = result.show_legend
   st.show_date   = result.show_date
   RedrawPlotViewer, st
   end

;--------------------------------------------------------------------------
; ROI widget
  st.pv_roi: $
   begin
   if (event.roi_name EQ 'fitting & stats') then begin
     plot_viewer_roi, st.pv_roi, ROI_NAME=event.roi_name, GET_ROI=roi

     fit_widg_exists = widget_info(st.fit_widget, /VALID_ID)

     ;; If the fit widget exists, and the user has changed style to 'none',
     ;; then destroy the fit widget.
     if (fit_widg_exists AND roi.style EQ 'none') then $
       widget_control, st.fit_widget, /DESTROY

     ;; If the fit widget does not exist, and the user has changed style to 
     ;; something other than 'none', then create the fit widget.
     if ((NOT fit_widg_exists) AND roi.style NE 'none') then begin
       scatter = total( st.datasets.scatter_flag )
       pass1_plot_viewer_fit, id, top_base, top_base, st.pv_roi, SCATTER=scatter
       st.fit_widget = id
     endif
   endif
   
   RedrawPlotViewer, st

   ;; Pass on the event.
   new_event = event
   new_event.ID = top_base
   end


;--------------------------------------------------------------------------
; Fit widget
  st.fit_widget: $
   begin
   new_event = { ID:top_base, TOP:event.top, HANDLER:0L, $
		 VALUE:'dataset changed' }
   end


;--------------------------------------------------------------------------
; TV widget
  st.tv_widget: RedrawPlotViewer, st


;--------------------------------------------------------------------------
; Dataset scrolling list
  st.dataset_list: $
   begin
   indexes          = where( st.datasets.name NE '' )
   names            = st.datasets(indexes).name
   st.selected_name = names( event.index )

   RedrawPlotViewer, st, /BUTTONS_ONLY
   new_event = { ID:top_base, TOP:event.top, HANDLER:0L, $
		 VALUE:'dataset changed' }
   end


;--------------------------------------------------------------------------
; delete button: $
  st.delete_button: $
   begin
   ;; We do NOT let the user delete the last dataset.
   dummy = where( st.datasets.name NE '', count ) 

   if ((count NE 1) AND selected_dataset.delete_flag) then begin
     st.datasets(selected_index).name        = ''
     st.invalid_axis_ranges = 1

     ; Select the dataset just prior to the deleted one.
     st.selected_name = st.datasets(0 > selected_index-1).name

     RedrawPlotViewer, st
     new_event = { ID:top_base, TOP:event.top, HANDLER:0L, $
		 VALUE:'dataset changed' }
   endif
   end

;--------------------------------------------------------------------------
; Copy button: $
  st.copy_button: $
   begin
   ;Store the data in a new dataset.
   str = strcompress(string(st.copy_number), /REMOVE_ALL)
   if (n_elements(*selected_dataset.standard_deviation) EQ 0) then begin
     pass1_plot_viewer, top_base, STATE=st, $
	 *selected_dataset.x_data, *selected_dataset.y_data, $
	 DATASET_NAME='copy' + str + ' of ' + selected_dataset.name, $
	 DESCRIPTION='copy' + str + ' of ' + $
		     selected_dataset.description, /ENABLE_DELETE
     st.copy_number = st.copy_number + 1
   endif else begin
     pass1_plot_viewer, top_base, STATE=st, $
	 *selected_dataset.x_data, *selected_dataset.y_data, $
	 STANDARD_DEVIATION=*selected_dataset.standard_deviation,$
	 DATASET_NAME='copy'+ str + ' of ' + selected_dataset.name, $
	 DESCRIPTION='copy'+ str + ' of ' + $
		     selected_dataset.description, /ENABLE_DELETE
     st.copy_number = st.copy_number + 1
   endelse
   end

;--------------------------------------------------------------------------
; Style button: $
  st.style_button: $
   begin
   ; We have to escape any commas that appear in the dataset description so we
   ; don't confuse cw_form().
   name = byte( ' ' + selected_dataset.description )
   comma = (byte(','))(0)
   slash = (byte('\'))(0)

   indexes = where( name EQ comma, count )
   for ii=count-1, 0, -1 do begin
     index = indexes(ii)
     name = [name(0:index-1), [slash], name(index:*)] 
   endfor

   f1 = '0, TEXT,' + strtrim( string(name), 2 ) + $
	', TAG=description, LABEL_LEFT=Description, WIDTH=30'

   o1 = '0, BUTTON, Lines & Symbols|Density Image,' + $
	'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.image_flag) + $
	', TAG=image_flag, ROW'

   b1 = '1, BASE,, ROW, FRAME'

   f2 = '0, BUTTON, ' + $
        'Solid|Dotted|Dashed|Dash Dot|Dash Dot Dot|Long Dashes|No Line,' + $
	'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.linestyle) + $
	', TAG=linestyle, LABEL_TOP=Line Style'

   f3 = '0, BUTTON, ' + $
        'None|+ Symbol|Asterisk|Dot|Diamond|Triangle|Box|X Symbol|Histogram,'+$
	'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.psym) + $
	', TAG=psym, LABEL_TOP=Symbol Style'

   ; We have to translate the field "color", which is a string, into
   ; an index into the list "Red|Green|Blue|White".
   case (selected_dataset.color) of
    'red'  : color_index = '0'
    'green': color_index = '1'
    'blue' : color_index = '2'
    'white': color_index = '3'
    else :   color_index = '3'
   endcase

   f4 = '2, BUTTON, Red|Green|Blue|White,'+$
	'EXCLUSIVE, SET_VALUE=' + color_index + ', TAG=color, LABEL_TOP=Color'

   f5 = '0, BUTTON, Show Plot|Hide Plot,' + $
	'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.hidden_flag) + $
	', TAG=hidden_flag, ROW'

   f6 = '0, BUTTON, Hide Error Bars|Show Error Bars,' + $
	'EXCLUSIVE, SET_VALUE=' + string(selected_dataset.error_bar_flag) + $
	', TAG=error_bar_flag, ROW'

   f7 = '2,BUTTON,OK,QUIT,TAG=ok'

   if (selected_dataset.scatter_flag) then begin
     result = cw_form( [f1,o1,b1,f2,f3,f4,f5,f6,f7], /COLUMN,$
		       TITLE='Dataset Properties' )

     ; Enforce only one image_flag set.
     if (1 EQ result.image_flag) then st.datasets.image_flag = 0
     selected_dataset.image_flag = result.image_flag

     ; Create the TV widget if necessary.
     if (result.image_flag AND $
	 (NOT widget_info(st.tv_widget, /VALID_ID))) then begin
       plot_viewer_tv, temp_id, PARENT_WIDGET=top_base, PLOT_VIEWER=top_base,$
				DRAW_ID=st.draw_widget
       st.tv_widget = temp_id
     endif

   endif else begin
     result = cw_form( [f1,b1,f2,f3,f4,f5,f6,f7], /COLUMN,$
		       TITLE='Dataset Properties' )
   endelse

   ; We have to translate "result.color", which is an index into
   ; "Red|Green|Blue|White", into a color name (string).
   case (result.color) of
     0: selected_dataset.color = 'red'
     1: selected_dataset.color = 'green'
     2: selected_dataset.color = 'blue'
     3: selected_dataset.color = 'white'
   endcase

   ; Handle the case where the user selects both "no line" and "no symbol".
   if (result.linestyle EQ 6 AND result.psym EQ 0) then result.psym = 2

   selected_dataset.description    = result.description
   selected_dataset.linestyle      = result.linestyle
   selected_dataset.psym           = result.psym
   selected_dataset.hidden_flag    = result.hidden_flag
   selected_dataset.error_bar_flag = result.error_bar_flag

   st.datasets(selected_index) = selected_dataset  
   RedrawPlotViewer, st
   end

;--------------------------------------------------------------------------
; Save menu: $
  st.save_menu: $
   begin
   mode_names = ['X/Y Points (ASCII)', 'X/Y Points (FITS table)', $
   		 'Density Image (FITS)', $
   		 'All Datasets (PlotXY format)']
   		 
   case Event.value of
    ; save selected dataset
    1: begin 
       ; Determine the format
       if (selected_dataset.image_flag) then begin
         f1 = '0, BUTTON, ' + string( mode_names[0:2], f='(A,"|",A,"|",A)') + $
              ',EXCLUSIVE, SET_VALUE=0, TAG=format'
       endif else begin
         f1 = '0, BUTTON, ' + string( mode_names[0:1], f='(A,"|",A)') + $
              ',EXCLUSIVE, SET_VALUE=0, TAG=format'
       endelse
       
       f2 = '2,BUTTON,OK,QUIT,TAG=ok'
       result = cw_form( [f1,f2], /COLUMN, TITLE='Dataset File Format Desired' )
       
       mode = result.format
       
       case (mode) of
        0: name = strcompress( selected_dataset.name + '.txt', /REMOVE_ALL )
        1: name = strcompress( selected_dataset.name + '.fits', /REMOVE_ALL )
        2: name = strcompress( selected_dataset.name + '.density', /REMOVE_ALL )
       endcase
       end
       
    ; save all datasets to a plotxy file
    2: begin 
       name = strcompress( st.title + '.plotxy', /REMOVE_ALL )
       mode = 3
       end
   endcase

   pathname = dialog_pickfile( GROUP=top_base, FILE=name(0), $
				TITLE='Save ' + mode_names[mode] )
   widget_control, /HOURGLASS
   
   if (pathname NE '') then begin
     ; Make sure we can write to this file.
     openw, Unit, pathname, /GET_LUN, ERROR=Error
     if (Error EQ 0) then begin
       free_lun, Unit
       
       message = 'Wrote dataset <' + selected_dataset.name + $
 	           '> to file ' + pathname
       
       case mode of
        0: $
         ; ASCII
         begin
         openw, Unit, pathname, /GET_LUN
	 num_points = selected_dataset.num_data_points
	 
	 if (n_elements(*selected_dataset.standard_deviation) EQ 0) then begin
	   printf, unit, '        X          Y'
	   for ii = 0L, num_points-1 do $
	   printf, Unit, (*selected_dataset.x_data)(ii), $
			 (*selected_dataset.y_data)(ii)
	 endif else begin
	   printf, unit, '        X          Y         sigma_Y'
	   for ii = 0L, num_points-1 do $
	   printf, Unit, (*selected_dataset.x_data)(ii), $
			 (*selected_dataset.y_data)(ii), $
			 (*selected_dataset.standard_deviation)(ii)
	 endelse
	 
          

	 free_lun, Unit
         end

        1: $
         ; FITS table
         begin
         get_date, date_today
         mkhdr, pheader, '', /EXTEND
         sxaddpar, pheader, "CREATOR", "Plot Viewer, Version 9.30"
	 sxaddpar, pheader, 'ORIGIN',   'Penn State University'
	 sxaddpar, pheader, "FITSVERS", "IDL Astronomy Users Library"
	 sxaddpar, pheader, "FNFITS",   pathname
	 sxaddpar, pheader, 'DATE',     date_today
	 writefits, pathname, no_data, pheader
	 
         sxaddpar, fits_header, "CREATOR", "Plot Viewer, Version 9.30"
	 sxaddpar, fits_header, 'ORIGIN',   'Penn State University'
	 sxaddpar, fits_header, 'DATE',     date_today

         bin_table = replicate( { $
         		X: (*selected_dataset.x_data)(0), $
         		Y: (*selected_dataset.y_data)(0) }, $
         		 selected_dataset.num_data_points )
         bin_table.X = *selected_dataset.x_data
         bin_table.Y = *selected_dataset.y_data
         mwrfits, bin_table, pathname, fits_header
         end

        2: $
         ; Density Image
         begin
         plot_viewer_tv, st.tv_widget, IMAGE=image, $
         		 CRVAL=crval, CRPIX=crpix, CDELT=cdelt
		                
         mkhdr,    fits_header, image, /EXTEND
         sxaddpar, fits_header, "CREATOR", "Plot Viewer, Version 9.30"
         sxaddpar, fits_header, 'ORIGIN',  'Penn State University'
     
         sxaddpar, fits_header, 'CTYPE1', st.xtitle
         sxaddpar, fits_header, 'CTYPE2', st.ytitle
         sxaddpar, fits_header, 'CRPIX1', crpix(0)
         sxaddpar, fits_header, 'CRPIX2', crpix(1)
         sxaddpar, fits_header, 'CRVAL1', crval(0)
         sxaddpar, fits_header, 'CRVAL2', crval(1)
         sxaddpar, fits_header, 'CDELT1', cdelt(0)
         sxaddpar, fits_header, 'CDELT2', cdelt(1)

         writefits, pathname, image, fits_header
         end

        3: $
         ; All datasets in PlotXY
         begin
         openw, Unit, pathname, /GET_LUN
	 printf, unit, st.title
	 printf, unit, st.xtitle
	 printf, unit, st.ytitle

	 printf, unit, 0, 0, 0, 0, 0, 0, 0, 0
	 printf, unit, 'IDL', '01/01/96', '00:00:00', f='(3A10)'

         nskip = 1
         for ii = 0, n_elements(st.datasets)-1 do begin
	   dataset = st.datasets(ii)
           if (dataset.name NE '') then begin
	     printf, unit, dataset.num_data_points, 0, 1, nskip, f='(4(I0,X))'
	     nskip = -1
	     for jj=0L, dataset.num_data_points-1 do $
	       printf, unit, (*dataset.x_data)(jj), 0, (*dataset.y_data)(jj), 0
	   endif
	 endfor
	 free_lun, Unit
	 message = 'Wrote all datasets to file ' + pathname
         end

       endcase

     endif else begin
       message = 'ERROR opening file '+ pathname
     endelse
     dummy=dialog_message( message, DIALOG_PARENT=st.save_menu, /INFO )
   endif

   end

;--------------------------------------------------------------------------
; Load menu: $
  st.load_menu: $
   begin
   pathname = dialog_pickfile( GROUP=top_base, /MUST_EXIST, $
			       TITLE='Choose file to load dataset' )
   if (pathname NE '') then begin
     widget_control, /HOURGLASS
   
     txt=['When you dismiss this message, you will be presented with',$
   	'the ASCII_TEMPLATE tool built into IDL.  Fill out the',$
   	'three forms in this tool to define two fields (columns) in',$
   	'your ascii file which should be extracted.  Name these',$
   	'fields "X" and "Y".']
   	
     dum = dialog_message( txt, DIALOG_PARENT=st.load_menu, /INFO )
   
     table = read_ascii( pathname, TEMPLATE=ascii_template(pathname) )
   
     ;Store the data in a new dataset.
     pass1_plot_viewer, top_base, STATE=st, table.X, table.Y, $
			    DATASET_NAME=pathname, /ENABLE_DELETE
   endif
   end

;--------------------------------------------------------------------------
; Print menu: $
  st.print_button: $
   begin
   PsDevice, top_base, filename, success

   if (success EQ 1 ) then begin
     RedrawPlotViewer, st
     device, /CLOSE
     color_manager, /X_PSEUDO

     if (Filename EQ '') then begin
       spawn, '/usr/bin/lp -c idl.ps; /bin/rm idl.ps >& /dev/null'
       message='Printed plot'
     endif else begin
       message='Wrote Postscript file ' + filename
     endelse
;    dummy=dialog_message(message, DIALOG_PARENT=st.print_button, /INFO )
   endif 
   end


;--------------------------------------------------------------------------
; DONE button
  st.DoneButton : DestroyFlag = 1

  else: print, 'unknown event in pass1_plot_viewer'
endcase


widget_control, top_base, SET_UVALUE=st, /NO_COPY

if DestroyFlag then widget_control, top_base, /DESTROY

return, new_event
end



;==========================================================================
;;; Event Handler Procedure
;==========================================================================
PRO PlotViewerEvent, Event
 
event = PlotViewerEventFn( Event )
return
end


;==========================================================================
;;; MAIN "pass1_plot_viewer" ROUTINE
;==========================================================================

PRO pass1_plot_viewer, top_base, param1, param2, DATASET_NAME=dataset_name, $
		 STANDARD_DEVIATION=standard_deviation, $

		 TITLE=title, SUBTITLE=subtitle, XTITLE=xtitle, YTITLE=ytitle, $
		 COLOR=color, LINESTYLE=linestyle, PSYM=psym, $
		 DESCRIPTION=description, SCATTER=scatter, $
		 HIDE_DATASET=hide_dataset, ENABLE_DELETE=enable_delete, $

		 GET_UTYPE=get_utype, SET_UTYPE=set_utype, $
		 GET_UPTR=get_uptr, GET_DATASET=dataset, $

		 ROI_NAME=roi_name, ROI_STYLE=roi_style, HIDE_ROI=hide_roi, $
		 APPLY_ROI=apply_roi, ROI_MASK=roi_mask, $

		 DELETE=delete, DEFAULT_AXES=default_axes, $
		 LIST_OF_DATASETS=list_of_datasets, $
		 SELECTED_DATASET_NAME=selected_dataset_name, $

		 CLICK=click, MOUSE_POSN=mouse_posn, PROMPT=prompt, $
		 STATE=st, _EXTRA=extra

;; If the widget ID of an existing plot_viewer was not passed, then create 
;; the widget.
state_passed = keyword_set(st)
create_flag = 1

if state_passed then create_flag = 0

if (n_elements(top_base) EQ 1) then begin
  if (widget_info( top_base, /VALID_ID )) then create_flag = 0
endif

if create_flag then top_base = CreatePlotViewer( _EXTRA=extra )


;;---------------------------------------------------------------------------
;; Get the state structure.
if (NOT state_passed) then $
  widget_control, top_base, GET_UVALUE=st, /NO_COPY


;;---------------------------------------------------------------------------
;; Handle the keywords that modify the plot as a whole.
if (keyword_set(default_axes)) then st.use_default_axis_ranges = 1
if (0 NE n_elements(title))    then st.title    = title
if (0 NE n_elements(subtitle)) then st.subtitle = subtitle
if (0 NE n_elements(xtitle))   then st.xtitle   = xtitle
if (0 NE n_elements(ytitle))   then st.ytitle   = ytitle


;;---------------------------------------------------------------------------
;; Handle parameters which involve modifying a dataset.

dataset_accessed = keyword_set(dataset_name) OR (n_elements(set_utype) NE 0)

dataset_modified = (n_elements(param1)      NE 0)  OR $
		   (n_elements(color)       NE 0)  OR $
		   (n_elements(linestyle)   NE 0)  OR $
		   (n_elements(psym)        NE 0)  OR $
		   (n_elements(description) NE 0)  OR $
		   keyword_set(delete)          

if (dataset_accessed OR dataset_modified) then begin

  ;; Find the specified dataset or create one.
  if (NOT keyword_set(dataset_name)) then dataset_name = 'x_y'
  
  dataset_index = (where( st.datasets.name EQ dataset_name, count ))(0)
  
  new_dataset_flag = 0
  if (count EQ 0) then begin
    new_dataset_flag = 1
    dataset_index = (where( st.datasets.name EQ '', count ))(0)
  
    if (count EQ 0) then dataset_index = 0

    if (n_elements(st.datasets) GT count) then st.show_legend = 1
  endif
  
  dataset = st.datasets(dataset_index) 
  
  
  ;;---------------------------------------------------------------------------
  ;; If necessary, initialize a new dataset.
  if new_dataset_flag then begin
    dataset.name            = dataset_name
    dataset.description     = dataset_name
    dataset.utype           = ''
    dataset.hidden_flag     = 0
    dataset.delete_flag     = 0
    dataset.scatter_flag    = 0
    dataset.image_flag      = 0
    dataset.error_bar_flag  = 0
    dataset.num_data_points = 1

    *dataset.x_data = [0]
    *dataset.y_data = [0]
    *dataset.standard_deviation = 0
    *dataset.uptr               = 0
    dummy = temporary( *dataset.standard_deviation )
    dummy = temporary( *dataset.uptr )

    ;; Notify the ROI and TV widgets that this dataset has changed.
    plot_viewer_roi, st.pv_roi, MODIFIED_DATASET=dataset_name

    if (widget_info(st.tv_widget, /VALID_ID)) then $
      plot_viewer_tv, st.tv_widget, DATASET=dataset, /MODIFIED
  endif
  
  
  ;;---------------------------------------------------------------------------
  ;; Handle the keywords that modify a dataset.
  if (0 NE n_elements(description))  then dataset.description = description
  if (0 NE n_elements(set_utype))    then dataset.utype       = set_utype
  if (0 NE n_elements(color))        then dataset.color       = color
  if (0 NE n_elements(linestyle))    then dataset.linestyle   = linestyle
  if (0 NE n_elements(hide_dataset)) then dataset.hidden_flag = hide_dataset 
  if (0 NE n_elements(enable_delete)) then dataset.delete_flag = enable_delete 
  if (0 NE n_elements(scatter))      then begin
    dataset.scatter_flag = scatter
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print, 'THIS TOOL IS OBSOLETE.  Use dataset_2d.pro
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  endif
  
  if (0 NE n_elements(psym)) then begin
    if (psym EQ 10) then dataset.psym = 8 $
                    else dataset.psym = psym
  endif
  
  if keyword_set(delete) then dataset.name = ''

  ;;---------------------------------------------------------------------------
  ;; Handle the keywords that retrieve info from a dataset.
  get_utype = dataset.utype
  get_uptr  = dataset.uptr
  
  
  ;;---------------------------------------------------------------------------
  ;; Store new X-Y data values if supplied.
  
  if (N_ELEMENTS(param1) NE 0) then begin
  
    if ('Double' EQ datatype( param1, 1 )) then $
      message, 'ERROR: parameter PARAM1 cannot be of type DOUBLE'
  
    if (N_ELEMENTS(param2) EQ 0) then begin
  
      if ('Double' EQ datatype( param2, 1 )) then $
        message, 'ERROR: parameter PARAM2 cannot be of type DOUBLE'
  
      num_data_points = n_elements(param1) 
      y_data = reform(param1)  &  x_data = lindgen(num_data_points)
  
    endif else begin
  
      num_data_points = n_elements(param1) < n_elements(param2) 
      x_data = reform(param1)  &  y_data = reform(param2) 
  
    endelse
  
  
    x_max = max(x_data, MIN=x_min)
  
    if (n_elements(standard_deviation) NE 0) then begin
      y_max = max(y_data + standard_deviation)
      y_min = min(y_data - standard_deviation)
  
      *dataset.standard_deviation = standard_deviation
  
    endif else begin
      y_max = max(y_data, MIN=y_min)
  
      *dataset.standard_deviation = 0
      dummy = temporary( *dataset.standard_deviation )
    endelse
  
    dataset.x_max           = x_max
    dataset.x_min           = x_min
    dataset.y_max           = y_max
    dataset.y_min           = y_min
  
    dataset.num_data_points = num_data_points
    *dataset.x_data = temporary( x_data )
    *dataset.y_data = temporary( y_data )

    ;; Notify the ROI and TV widgets that this dataset has changed.
    plot_viewer_roi, st.pv_roi, MODIFIED_DATASET=dataset_name

    if (widget_info(st.tv_widget, /VALID_ID)) then $
      plot_viewer_tv, st.tv_widget, DATASET=dataset, /MODIFIED
  
    ;;Set log scaling flags
    st.xlog_handled = 0
    st.ylog_handled = 0
    st.invalid_axis_ranges = 1
  
  endif ;(N_PARAMS() GT 1)
  
  
  ;;---------------------------------------------------------------------------
  ;; Store the dataset structure we've been modifying.
  st.datasets(dataset_index) = dataset
; st.selected_name           = dataset.name

endif ; dataset_modified


;;---------------------------------------------------------------------------
;; Handle keywords related to ROI's.
if keyword_set(roi_name) then begin
  plot_viewer_roi, st.pv_roi, ROI_NAME=roi_name

  if keyword_set(roi_style) then begin
    plot_viewer_roi, st.pv_roi, ROI_NAME=roi_name, STYLE=roi_style
  endif

  if keyword_set(apply_roi) then begin
    plot_viewer_roi, st.pv_roi, /APPLY_ROI, GET_ROI=roi, $
		     ROI_NAME=roi_name, DATASET=dataset
    roi_mask = roi.roi_mask
  endif 

  if (n_elements(hide_roi) NE 0) then $
    plot_viewer_roi, st.pv_roi, ROI_NAME=roi_name, HIDE_ROI=hide_roi
endif


;;---------------------------------------------------------------------------
if widget_info(top_base, /REALIZED) then begin

  ;; If desired, get a mouse click. 
  if keyword_set(click) then begin
    coordsystem_manager, st.draw_widget, /RESTORE

    if NOT keyword_set(prompt) then prompt = 'Select a point with the mouse.'

    ;; We expect two events -- button down and button up.
    text = ''
    msg  = -1
    repeat begin
      if (strlen(text) LE 1) then begin
	text = prompt
	widget_control, msg, /DESTROY, BAD_ID=bad_id
	msg = NonmodalMessage( prompt, POS=st.dataset_list, TEXT_WID=text_wid )
	pause_cnt = 60
      endif else begin
	if (pause_cnt LT 0) then text = strmid( text, 1, 1000 )
	widget_control, text_wid, SET_VALUE=text, BAD_ID=bad_id
	wait, 0.05
	pause_cnt = pause_cnt - 1
      endelse

      event = widget_event( st.draw_widget, /NOWAIT )
    endrep until (event.id EQ st.draw_widget)

    event = widget_event( st.draw_widget )
    widget_control, msg, /DESTROY, BAD_ID=bad_id

    mark_window, event.x, event.y, 10

    mouse_posn = (convert_coord( event.x, event.y, /DEVICE, /TO_DATA ))(0:1)
  endif 

  ;; If necessary, redraw widget.
  if dataset_modified then RedrawPlotViewer, st
endif


;;---------------------------------------------------------------------------
;; Return a list of all the ROI names, and the selected dataset name.
indexes          = where( st.datasets.name NE '', count)
if (count NE 0) then list_of_datasets = st.datasets(indexes).name
 
selected_dataset_name = st.selected_name

;;---------------------------------------------------------------------------
;; Save the state structure.
if (NOT state_passed) then $
  widget_control, top_base, SET_UVALUE=st, /NO_COPY

return
END
