; $Id
;
; Add a flag that indicates if the user has ever modified the ROI.  
; Do not initialize the ROI params when the style is changed unless this 
; flag is clear.
;
; $Log pass_plot_viewer_roi.pro $
;

;==========================================================================
;;; EVENTS GENERATED

;;; Whenever a ROI is changed by the user, an event is generated with the
;;; following structure elements:
;;;   VALUE:    'ROI changed' 
;;;   ROI_NAME: the name (a string) of the ROI that changed
;==========================================================================


;==========================================================================
;;; For ROI's of type Box, point_a represents the lower-left corner
;;; and point_b represents the upper-right corner.

;;; For ROI's of type Annulus, point_a represents the center, length_a
;;; represents the inner radius, and length_b represents the outer radius.

;;; The pointer roi.roi_mask points to a byte array that is 1
;;; wherever the dataset named roi.dataset_name is inside the ROI.
;==========================================================================

;==========================================================================
;;; Create the widget.
;==========================================================================
FUNCTION CreatePlotViewerRoi, PARENT_WIDGET=parent, PLOT_VIEWER=pass1_plot_viewer


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
 
top_base = widget_base( parent, /COLUMN, FRAME=1, $
			/SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER, $
			KILL_NOTIFY='PlotViewerRoiCleanup', $
			EVENT_FUNC='PlotViewRoiEventFn' )

 up_base = widget_base( top_base, /SPACE, /XPAD, /YPAD, /ROW )

  left_base = widget_base( up_base, /SPACE, /XPAD, /YPAD, /COL )

   label = widget_label( left_base, VALUE='ROI List', /ALIGN_CENTER )
 
   roi_list = widget_list( left_base, XSIZE=20, YSIZE=3 )


  right_base = widget_base( up_base, /SPACE, /XPAD, /YPAD, /COLUMN )

   default_button = widget_button( right_base, VALUE='Default' )
   define_button  = widget_button( right_base, VALUE='Define' )
   edit_button    = widget_button( right_base, VALUE='Edit' )

 down_base = widget_base( top_base, /SPACE, /XPAD, /YPAD, /ROW )

  styles = ['None','X-range','Y-range','Box interior','Box exterior','Annulus']
  style_droplist = widget_droplist( down_base, VALUE=styles )
  widget_control, style_droplist, SET_DROPLIST_SELECT=0

  extra_button1 = widget_button( down_base, VALUE=' ', /DYNAMIC_RESIZE )



;; Create the state structure.
roi = { name:'', style:'none', hide_flag:0, color:'blue', x_axis_only_flag:0, $
	point_a:[0.,0.], point_b:[1.,1.], length_a:0., length_b:1., $
        dataset_name:'', roi_mask:ptr_new(/ALLOC) }

state = { $
	  ;ROI structures
	  rois: replicate( roi, 5 ), roi_list:roi_list, selected_name:'', $

	  style_droplist:style_droplist, edit_button:edit_button, $
	  default_button:default_button, $
	  define_button:define_button, extra_button1:extra_button1, $

	  default_xrange:[0.,1.], default_yrange:[0.,1.], $
	  plot_viewer:plot_viewer }

ptr_free, roi.roi_mask

;; Allocate heap vars for roi structures.
for ii = 0, n_elements(state.rois)-1 do begin
  state.rois(ii).roi_mask = ptr_new(/ALLOC)
endfor

;; Save state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY
 
return, top_base
END


;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO PlotViewerRoiCleanup, top_base

widget_control, top_base, GET_UVALUE=st, /NO_COPY

ptr_free, st.rois.roi_mask
return
end
;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawPlotViewRoi, st

;; Redraw the roi list widget to show all the non-null roi's.
;; Verify that st.selected_name is a real roi.
indexes    = where( st.rois.name NE '' )
names      = st.rois(indexes).name
hide_flags = st.rois(indexes).hide_flag
styles     = st.rois(indexes).style   

selected_index = (where( names EQ st.selected_name, count ))(0)

if (count EQ 0) then begin
  st.selected_name = names(0)
  selected_index   = 0
endif

indexes = where( (styles EQ 'none') OR (hide_flags EQ 1), count )
if (count NE 0) then names(indexes) = '{' + names(indexes) + '}'

widget_control, st.roi_list, SET_LIST_SEL=selected_index, SET_VAL=names


;; Set the style_droplist and buttons to match the selected ROI.
widget_control, st.default_button, SENSITIVE=1
widget_control, st.define_button,  SENSITIVE=1
widget_control, st.edit_button,    SENSITIVE=1

selected_roi = st.rois( selected_index )
case selected_roi.style of
   'none': $
    begin
    index=0
    widget_control, st.extra_button1,  SENSITIVE=0, SET_VALUE=' '
    widget_control, st.default_button, SENSITIVE=0
    widget_control, st.define_button,  SENSITIVE=0
    widget_control, st.edit_button,    SENSITIVE=0
    end

   'x_range': $
    begin
    index=1
    widget_control, st.extra_button1,  SENSITIVE=0, SET_VALUE=' '
    end

   'y_range': $
    begin
    index=2
    widget_control, st.extra_button1,  SENSITIVE=0, SET_VALUE=' '
    end

   'box_interior': $
    begin
    index=3
    widget_control, st.extra_button1,  SENSITIVE=0, SET_VALUE=' '
    end

   'box_exterior': $
    begin
    index=4
    widget_control, st.extra_button1,  SENSITIVE=0, SET_VALUE=' '
    end

   'annulus': $
    begin
    index=5
    widget_control, st.extra_button1, SET_VALUE='Inner Radius', SENSITIVE=1
    end

    else: message, 'Invalid style: ' + string(style)
endcase
widget_control, st.style_droplist, SET_DROPLIST_SELECT=index


return
END


;==========================================================================
;;; Event Handler
;==========================================================================
FUNCTION PlotViewRoiEventFn, Event

;; Get the state structure.
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st, /NO_COPY
widget_control, /HOURGLASS

new_event = 0
roi_changed_flag = 1

selected_index = (where( st.rois.name EQ st.selected_name))(0)
selected_roi   = st.rois( selected_index )
roi_name       = selected_roi.name

;; Process the event.
case Event.ID of
 
 ;--------------------------------------------------------------------------
 ; widget_list events
 st.roi_list: $
  begin
  indexes          = where( st.rois.name NE '' )
  names            = st.rois(indexes).name
  st.selected_name = names( event.index )
  roi_changed_flag  = 0
  end

 ;--------------------------------------------------------------------------
 ; style droplist events
 st.style_droplist: $
  begin
  case Event.index of
   0: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='none'
   1: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='x_range'
   2: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='y_range'
   3: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='box_interior'
   4: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='box_exterior'
   5: pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='annulus'
  endcase
  end

 ;--------------------------------------------------------------------------
 ; default button events
 st.default_button: $
  begin
  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, GET_ROI=roi 

  case roi.style of
   'none': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='none'
   'x_range': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='x_range'
   'y_range': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='y_range'
   'box_interior': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='box_interior'
   'box_exterior': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='box_exterior'
   'annulus': $
     pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, STYLE='annulus'
  endcase
  end

 ;--------------------------------------------------------------------------
 ; edit button events
 st.edit_button: $
  begin
  ;run a modal form to edit roi parameters
  ;DON't forget to error check the values entered!

  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, GET_ROI=roi 

  f1 = '0, BUTTON, Show|Hide,' + $
	 'EXCLUSIVE, SET_VALUE=' + string(roi.hide_flag) + $
	 ', TAG=hide_flag, LABEL_LEFT=Boundary, ROW'

  case roi.style of
   'x_range': $
    begin
    b1 = '1, BASE,, ROW, FRAME'

    f2 = '2, FLOAT,' + string(roi.point_a(0)) + $
	 ', TAG=xlow, LABEL_LEFT=Min X, WIDTH=12'

    b2 = '1, BASE,, ROW, FRAME'

    f4 = '2, FLOAT,' + string(roi.point_b(0)) + $
	 ', TAG=xhigh, LABEL_LEFT=Max X, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [f1,b1,f2,b2,f4,f6], /COLUMN, $
			 TITLE='ROI Parameters' )

    corner0 = [result.xlow, 0]
    corner1 = [result.xhigh, 0]
    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    roi.hide_flag = result.hide_flag
    end

   'y_range': $
    begin
    b1 = '1, BASE,, ROW, FRAME'

    f2 = '2, FLOAT,' + string(roi.point_a(1)) + $
	 ', TAG=ylow, LABEL_LEFT=Min Y, WIDTH=12'

    b2 = '1, BASE,, ROW, FRAME'

    f4 = '2, FLOAT,' + string(roi.point_b(1)) + $
	 ', TAG=yhigh, LABEL_LEFT=Max Y, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [f1,b1,f2,b2,f4,f6], /COLUMN, $
			 TITLE='ROI Parameters' )

    corner0 = [0, result.ylow]
    corner1 = [0, result.yhigh]
    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    roi.hide_flag = result.hide_flag
    end

   'box_interior': $
    begin
    b1 = '1, BASE,, ROW, FRAME'

    f2 = '0, FLOAT,' + string(roi.point_a(0)) + $
	 ', TAG=x0, LABEL_LEFT=Corner1 (X Y), WIDTH=12'

    f3 = '2, FLOAT,' + string(roi.point_a(1)) + ', TAG=y0, WIDTH=12'

    b2 = '1, BASE,, ROW, FRAME'

    f4 = '0, FLOAT,' + string(roi.point_b(0)) + $
	 ', TAG=x1, LABEL_LEFT=Corner2 (X Y), WIDTH=12'

    f5 = '2, FLOAT,' + string(roi.point_b(1)) + ', TAG=y1, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [f1,b1,f2,f3,b2,f4,f5,f6], /COLUMN, $
			 TITLE='ROI Parameters' )

    corner0 = [result.x0, result.y0]
    corner1 = [result.x1, result.y1]
    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    roi.hide_flag = result.hide_flag
    end

   'box_exterior': $
    begin

    b1 = '1, BASE,, ROW, FRAME'

    f2 = '0, FLOAT,' + string(roi.point_a(0)) + $
	 ', TAG=x0, LABEL_LEFT=Corner1 (X Y), WIDTH=12'

    f3 = '2, FLOAT,' + string(roi.point_a(1)) + ', TAG=y0, WIDTH=12'

    b2 = '1, BASE,, ROW, FRAME'

    f4 = '0, FLOAT,' + string(roi.point_b(0)) + $
	 ', TAG=x1, LABEL_LEFT=Corner2 (X Y), WIDTH=12'

    f5 = '2, FLOAT,' + string(roi.point_b(1)) + ', TAG=y1, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [f1,b1,f2,f3,b2,f4,f5,f6], /COLUMN, $
			 TITLE='ROI Parameters' )

    corner0 = [result.x0, result.y0]
    corner1 = [result.x1, result.y1]
    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    roi.hide_flag = result.hide_flag
    end

   'annulus': $
    begin
    b1 = '1, BASE,, ROW, FRAME'

    f2 = '0, FLOAT,' + string(roi.point_a(0)) + $
	 ', TAG=x0, LABEL_LEFT=Center, WIDTH=12'

    f3 = '2, FLOAT,' + string(roi.point_a(1)) + ', TAG=y0, WIDTH=12'

    f4 = '0, FLOAT,' + string(roi.length_a) + $
	 ', TAG=length_a, LABEL_LEFT=Inner Radius, WIDTH=12'

    f5 = '0, FLOAT,' + string(roi.length_b) + $
	 ', TAG=length_b, LABEL_LEFT=Outer Radius, WIDTH=12'

    f6 = '2,BUTTON,OK,QUIT,TAG=ok'

    result = cw_form( [f1,b1,f2,f3,f4,f5,f6], /COLUMN, $
			 TITLE='ROI Parameters' )

    roi.point_a  = [result.x0, result.y0]
    roi.length_a = result.length_a
    roi.length_b = result.length_b
    end
  endcase

  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, SET_ROI=roi 
  end

 ;--------------------------------------------------------------------------
 ; button 0 events: SELECT MULTIPLE POINTS TO DEFINE ROI
 st.define_button: $
  begin
  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, GET_ROI=roi 

  case roi.style of
   'x_range': $
    begin
    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner0, $
		 PROMPT='Select an X limit with the mouse.'

    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner1, $
		 PROMPT='Select another X limit with the mouse.'

    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    end

   'y_range': $
    begin
    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner0, $
		 PROMPT='Select a Y limit with the mouse.'

    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner1, $
		 PROMPT='Select another Y limit with the mouse.'

    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    end

   'box_interior': $
    begin
    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner0, $
		 PROMPT='Select a corner with the mouse.'

    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner1, $
		 PROMPT='Select another corner with the mouse.'

    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    end

   'box_exterior': $
    begin
    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner0, $
		 PROMPT='Select a corner with the mouse.'

    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=corner1, $
		 PROMPT='Select another corner with the mouse.'

    roi.point_a = corner0 < corner1
    roi.point_b = corner0 > corner1
    end

   'annulus': $
    begin
    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=center, $
		 PROMPT='Select center with the mouse.'

    plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=point, $
		 PROMPT='Select outer radius with the mouse.'

    roi.point_a  = center
    roi.length_a = 0
    roi.length_b = sqrt( total( (center - point)^2 ) )
    end
  endcase

  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, SET_ROI=roi 
  end

 ;--------------------------------------------------------------------------
 ; extra_button1 events: DEFINE THE INNER RADIUS
 st.extra_button1: $
  begin
  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, GET_ROI=roi 

  plot_viewer, st.plot_viewer, /CLICK, MOUSE_POSN=point, $
		 PROMPT='Select inner radius with the mouse.'

  roi.length_a = sqrt( total( (roi.point_a - point)^2 ) )

  pass1_plot_viewer_roi, STATE=st, ROI_NAME=roi_name, SET_ROI=roi 
  end


 ;--------------------------------------------------------------------------
 else: message, 'unknown event in pass1_plot_viewer_roi'
endcase

RedrawPlotViewRoi, st

if roi_changed_flag then $
  new_event = {ID:top_base, TOP:event.top, HANDLER:0L, $
	       VALUE:'ROI changed', ROI_NAME:roi_name}

;--------------------------------------------------------------------------
;; Save the state structure.
widget_control, top_base, SET_UVALUE=st, /NO_COPY
 
return, new_event
END


;==========================================================================
;;; MAIN "pass1_plot_viewer_roi" ROUTINE


;;;                     CALLING OPTIONS

;;; Create a new plot_viewer_roi, returning the widget ID.
;;; ** plot_viewer_roi, id, PARENT_WIDGET=base
;;;		        PLOT_VIEWER=plot_viewer

;;; Create a new ROI in a specified style, or change the style of an
;;; existing ROI.
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, STYLE='box_interior'

;;; Hide and un-hide a ROI.
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, HIDE_ROI=1
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, HIDE_ROI=0

;;; Get a specific ROI structure. 
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, GET_ROI=roi 

;;; Set a specific ROI structure. 
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, SET_ROI=roi 

;;; Draw a specific ROI. 
;;; ** plot_viewer_roi, id, ROI_NAME=roi_name, /DRAW

;;; Return a list of the names of all the ROI's.
;;; ** plot_viewer_roi, id, LIST_OF_ROIS=roi_names

;;; Apply a specific ROI to a plot_viewer dataset structure.
;;; ** plot_viewer_roi, id, /APPLY_ROI, ROI_NAME=roi_name, DATASET=dataset

;;; Notify the roi widget that a specific dataset has changed so that
;;; any cached information about the application of ROI's to that dataset
;;; can be thrown away.
;;; ** plot_viewer_roi, id, MODIFIED_DATASET=dataset_name

;;; Notify the roi widget that the ranges of the datasets in plot_viewer
;;; have changed.
;;; ** plot_viewer_roi, id, DEFAULT_XRANGE=default_xrange,
;;;			 DEFAULT_YRANGE=default_yrange

;;; Call plot_viewer_roi from inside it's own event handler, in which case
;;; we already have the state structure.  You must omit "id".
;;; ** plot_viewer_roi, state=st, ...



;==========================================================================
PRO pass1_plot_viewer_roi, top_base, STATE=st, $
		  ROI_NAME=roi_name, STYLE=style, HIDE_ROI=hide_roi, $
		  GET_ROI=get_roi, SET_ROI=set_roi, DRAW=draw, $
		  LIST_OF_ROIS=list_of_rois, $
		  APPLY_ROI=apply_roi, DATASET=dataset, $
		  MODIFIED_DATASET=modified_dataset, $
		  DEFAULT_XRANGE=default_xrange, $
		  DEFAULT_YRANGE=default_yrange, $
		  _EXTRA=extra

;; If the widget ID of an existing plot_viewer_roi was not passed, the create
;; the widget.
if ((NOT keyword_set(top_base)) AND (NOT keyword_set(st))) then $
  top_base = CreatePlotViewerRoi( _EXTRA=extra)
 

;;---------------------------------------------------------------------------
;; Get the state structure.
if (NOT keyword_set(st)) then $
  widget_control, top_base, GET_UVALUE=st, /NO_COPY


;;---------------------------------------------------------------------------
;; If MODIFIED_DATASET was passed, then invalidate all ROI's that have been 
;; applied to that dataset. 
if keyword_set(modified_dataset) then begin
  indexes = where( st.rois.dataset_name EQ modified_dataset, count )
  if (count NE 0) then st.rois(indexes).dataset_name = ''
endif


;;---------------------------------------------------------------------------
;; Handle DEFAULT_XRANGE & DEFAULT_YRANGE keywords.
if keyword_set(default_xrange) then st.default_xrange=default_xrange
if keyword_set(default_yrange) then st.default_yrange=default_yrange


;;---------------------------------------------------------------------------
;; If a ROI was specified, find it or create a new one.
if keyword_set(roi_name) then begin
  roi_index = (where( st.rois.name EQ roi_name, count ))(0)

  if (count EQ 0) then begin
    roi_index = (where( st.rois.name EQ '', count ))(0)

    if (count EQ 0) then roi_index = 0

    if (NOT keyword_set(style)) then style = 'none'
  endif

  roi = st.rois(roi_index)
  roi.name = roi_name
endif


;;---------------------------------------------------------------------------
;; If a ROI was passed in with SET_ROI, then use it.
if keyword_set(set_roi) then begin
  roi = set_roi
  roi.dataset_name = ''
endif


;;---------------------------------------------------------------------------
;; If desired, set the ROI to a specified style with default parameters.
if keyword_set(style) then begin
  mid_pt = [total(st.default_xrange)/2.0, total(st.default_yrange)/2.0]
  xsize  = st.default_xrange(1) - st.default_xrange(0)
  ysize  = st.default_yrange(1) - st.default_yrange(0)

  roi.style = style
  case roi.style of
   'none': 

   'x_range': $
    begin
    roi.point_a = [st.default_xrange(0), 0]
    roi.point_b = [st.default_xrange(1), 0]
    st.selected_name = roi.name
    end

   'y_range': $
    begin
    roi.point_a = [0, st.default_yrange(0)]
    roi.point_b = [0, st.default_yrange(1)]
    st.selected_name = roi.name
    end

   'box_interior': $
    begin
    roi.point_a = [st.default_xrange(0), st.default_yrange(0)]
    roi.point_b = [st.default_xrange(1), st.default_yrange(1)]
    st.selected_name = roi.name
    end

   'box_exterior': $
    begin
    roi.point_a = mid_pt - [xsize/4, ysize/4]
    roi.point_b = mid_pt + [xsize/4, ysize/4]
    st.selected_name = roi.name
    end

   'annulus': $
    begin
    roi.point_a  = mid_pt
    roi.length_a = 0
    roi.length_b = (xsize/2.0) > (ysize/2.0)
    st.selected_name = roi.name
    end

    else: message, 'Invalid style: ' + string(style)
  endcase

  roi.dataset_name = ''
endif


;;---------------------------------------------------------------------------
;; If supplied, handle the HIDE_ROI keyword.
if (n_elements(hide_roi) NE 0) then roi.hide_flag = hide_roi


;;---------------------------------------------------------------------------
;; If necessary, apply the ROI to the specified dataset.
if keyword_set(apply_roi) then $
  if (roi.dataset_name NE dataset.name) then begin
    empty_roi = 0

;   if (roi.name NE 'AXIS RANGES') then $
;     print, roi.name, dataset.name, $
;	   f='("Applying ROI [",A0,"] to dataset [",A0,"].")'

    ; Save the dataset name so we can skip this calculation next time.
    roi.dataset_name = dataset.name

    ; Get the data pointers.
    x_data = dataset.x_data
    y_data = dataset.y_data
  
    ; Intersect the data with the ROI.
    case roi.style of
     'none': mask = replicate( 1B, dataset.num_data_points > 1 )

     'x_range': $
      begin
      xl=roi.point_a(0)  &  xh=roi.point_b(0)

      mask = (xl LE *x_data AND xh GE *x_data)
      end

     'y_range': $
      begin
      yl=roi.point_a(1)  &  yh=roi.point_b(1)

      mask = (yl LE *y_data AND yh GE *y_data)
      end

     'box_interior': $
      begin
      xl=roi.point_a(0)  &  xh=roi.point_b(0)
      yl=roi.point_a(1)  &  yh=roi.point_b(1)

      mask = (xl LE *x_data AND xh GE *x_data AND $
	      yl LE *y_data AND yh GE *y_data)
      end

     'box_exterior': $
      begin
      xl=roi.point_a(0)  &  xh=roi.point_b(0)
      yl=roi.point_a(1)  &  yh=roi.point_b(1)

      mask = (xl GE *x_data OR xh LE *x_data OR $
	      yl GE *y_data OR yh LE *y_data)
      end

     'annulus': $
      begin
      x_center    =roi.point_a(0) &  y_center    =roi.point_a(1)
      inner_radius=roi.length_a   &  outer_radius=roi.length_b

      x = *x_data
      y = *y_data

      ; We want to dither integer-valued data.
      if ('INT' EQ datatype(*x_data, 0)) then begin
	x = randomu(seed, n_elements(x)) - 0.5 + temporary(x)
	print, 'Dithering X values'
      endif

      if ('INT' EQ datatype(*y_data, 0)) then begin
	y = randomu(seed, n_elements(y)) - 0.5 + temporary(y) 
	print, 'Dithering Y values'
      endif

      sqr_dist = (x - x_center)^2 + (y - y_center)^2
      mask = (inner_radius^2 LE sqr_dist AND outer_radius^2 GE sqr_dist)
      end
    endcase

    *roi.roi_mask = mask

endif


;;---------------------------------------------------------------------------
;; If desired, draw the ROI. 
if keyword_set(draw) then $
 if (roi.hide_flag EQ 0) then begin
  color_manager, roi.color, color_index
  case roi.style of
   'none': 

   'x_range': $
    begin
    xl=roi.point_a(0)  &  xh=roi.point_b(0)
    if (!Y.TYPE EQ 0) then begin
      yl=!Y.CRANGE(0)    &  yh=!Y.CRANGE(1)
    endif else begin
      yl=10^!Y.CRANGE(0) &  yh=10^!Y.CRANGE(1)
    endelse
    plots, [xl,xl], [yl,yh], /DATA, THICK=2, COLOR=color_index 
    plots, [xh,xh], [yl,yh], /DATA, THICK=2, COLOR=color_index 
    end

   'y_range': $
    begin
    if (!X.TYPE EQ 0) then begin
      xl=!X.CRANGE(0)    &  xh=!X.CRANGE(1)
    endif else begin
      xl=10^!X.CRANGE(0) &  xh=10^!X.CRANGE(1)
    endelse
    yl=roi.point_a(1)       &  yh=roi.point_b(1)
    plots, [xl,xh], [yh,yh], /DATA, THICK=2, COLOR=color_index 
    plots, [xl,xh], [yl,yl], /DATA, THICK=2, COLOR=color_index 
    end

   'box_interior': $
    begin
    xl=roi.point_a(0)  &  xh=roi.point_b(0)
    yl=roi.point_a(1)  &  yh=roi.point_b(1)
    plots, [xl,xh,xh,xl,xl], [yl,yl,yh,yh,yl], /DATA, THICK=2, COLOR=color_index 
    end

   'box_exterior': $
    begin
    xl=roi.point_a(0)  &  xh=roi.point_b(0)
    yl=roi.point_a(1)  &  yh=roi.point_b(1)
    plots, [xl,xh,xh,xl,xl], [yl,yl,yh,yh,yl], /DATA, THICK=2, COLOR=color_index 
    end

   'annulus': $
    begin
    tvcircle_vectors, roi.length_a, roi.point_a(0), roi.point_a(1), x, y
    plots, x, y, /DATA, THICK=2, COLOR=color_index 
    tvcircle_vectors, roi.length_b, roi.point_a(0), roi.point_a(1), x, y
    plots, x, y, /DATA, THICK=2, COLOR=color_index 
    end
  endcase
endif


;;---------------------------------------------------------------------------
;; Save any changes we've made to the ROI and return it to caller.
if (n_elements(roi) NE 0) then begin
  get_roi            = roi
  st.rois(roi_index) = roi
  RedrawPlotViewRoi, st
endif


;;---------------------------------------------------------------------------
;; Return a list of all the ROI names.
indexes      = where( st.rois.name NE '')
list_of_rois = st.rois(indexes).name


;;---------------------------------------------------------------------------
;; Save the state structure.
if keyword_set(top_base) then $
  widget_control, top_base, SET_UVALUE=st, /NO_COPY

return
END

