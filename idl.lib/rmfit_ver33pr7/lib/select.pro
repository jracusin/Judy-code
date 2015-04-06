; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     select (FUNCTION)
;
; PURPOSE:
;     Interactively select intervals (bins) from a plot
;     with the mouse cursor.
;
; CALLING SEQUENCE:
;     span = SELECT (times, drawID [, oldSpan])
;
; INPUTS:
;     xData   : (2, n) array of data bins
;     drawID  : A valid WIDGET_DRAW ID in which the selections are made
;     oldSpan : Optional input containing the current span of the data
;               (e.g., a span selected with a previous call to SELECT)
;
; OUTPUTS:
;     span : A SPAN array containing the selected intervals.
;
;         A SPAN array is a (m, 2) array, where the (0, *) interval contains
;         the convex hull of all the selections, the (1, *) interval contains
;         the first selection interval, etc.  
;
;         Overlapping intervals are combined into one interval.
;         A selection is always returned.  If user makes no selection, 
;         a (1, 2) SPAN array is returned that contains the full range of 
;         the input data array.
;
; KEYWORDS:
;     HAVESELECTION : Set the keyword HAVESELECTION to return an integer 
;         denoting if a selection was made.  Returns 0 for no selection, 
;         otherwise 1.
;
;     SINGLEBIN : Set this keyword to restrict the selection to a single 
;         data bin (single mouse click).
;
;     SINGLEINTERVAL : Set this keyword to restrict the selection to a single 
;         data interval (two mouse clicks).
;
;     COLOR :       Color index of the plotted selections.
;
;     BACKGROUND :  Color index of the plot background.
;
; RESTRICTIONS:
;     Assumes the input xData array is monotonically increasing in value.
;
; DEPENDENCIES
;     track_mouse.pro
;
; MODIFICATION HISTORY:
;
;     RDP, 2005 May, added BACKGROUND keyword.
;     Written, 1999 September, Robert.Mallozzi@msfc.nasa.gov, based on
;              interval merging code by Robert.Preece@msfc.nasa.gov
;
;     June 19, 2003; RDP: added IMMEDIATE keyword to return without user
;              having to click in the LEFT margin area. This is useful to 
;              support multiple actions.
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 


; ----------------------------------------------------------------------------
FUNCTION select, xData, drawWidgetID, inSpan, $
    HAVESELECTION = haveSelection, $
    SINGLEBIN = singleBin, SINGLEINTERVAL = singleInterval, $
    IMMEDIATE = immediate, $
    COLOR = COLOR, $
    BACKGROUND = BACKGROUND, $
    _EXTRA = extra 

    haveSelection = 0

    WIDGET_CONTROL, drawWidgetID, SHOW = 1

    ;== If we have a default, then we automatically have a selection
    IF (N_PARAMS () GT 2) THEN BEGIN
       span = inSpan 
       haveSelection = 1
    ENDIF

	IF (N_ELEMENTS (color) EQ 0) THEN $
	   color = !P.COLOR
	
	IF (N_ELEMENTS (background) EQ 0) THEN $
	   background = !P.BACKGROUND
    
    xRange = AXISRANGE (/XAXIS)
    yRange = AXISRANGE (/YAXIS)
    
    fullSpan = TRANSPOSE ([MIN (xData), MAX (xData)])
        
    doExit = 0
    REPEAT BEGIN

        IF (KEYWORD_SET (singleBin)) THEN BEGIN
        
           ok = TRACK_MOUSE (drawWidgetID, region, coords, $
                    RIGHT_TEXT = 'Clear Selections',       $
                    /GET_SINGLE, /NOWAIT, TOP_TEXT = '',   $
                    COLOR = color, BACKGROUND = background, $
                    BOTTOM_TEXT = '', _EXTRA = extra)
           IF (REGION EQ 1) THEN ok = 0
	   
	   IF (KEYWORD_SET (immediate)) THEN doExit = 1
              
        ENDIF ELSE BEGIN
        
           ok = TRACK_MOUSE (drawWidgetID, region, coords, $
                    RIGHT_TEXT = 'Clear Selections',       $
                    COLOR = color, BACKGROUND = background, $
                    /GET_XRANGE, /NOWAIT, _EXTRA = extra)

        ENDELSE

        IF (ok) THEN BEGIN
                   
           haveSelection = (region NE 2)

           IF (N_ELEMENTS (span) NE 0) THEN $
              prevSpan = span
                         
           IF (KEYWORD_SET (singleBin)) THEN BEGIN

              s = REFORM ([coords[0], coords[0]], 1, 2)  
              newSpan = [s, s]

           ENDIF ELSE BEGIN
	      
              IF (KEYWORD_SET (singleInterval)) THEN BEGIN
                 newSpan = [coords[0, *], coords[0, *]]
              ENDIF ELSE BEGIN   
                 newSpan = RESOLVE_SELECTIONS (xData, coords, span)
              ENDELSE
           
           ENDELSE
           
           span = newSpan

           nPrev = (SIZE (prevSpan))[1] - 1     
           nSpan = (SIZE (span))[1] - 1     
           
           FOR i = 1, nPrev DO BEGIN

               edges = NEAREST_EDGE (xData, prevSpan[i, *])

               OPLOT, [1, 1] * edges[0], yRange, COLOR = background
               OPLOT, [1, 1] * edges[1], yRange, COLOR = background

           ENDFOR

           FOR i = 1, nSpan DO BEGIN

               edges = NEAREST_EDGE (xData, span[i, *])

               OPLOT, [1, 1] * edges[0], yRange, _EXTRA = extra
               OPLOT, [1, 1] * edges[1], yRange, _EXTRA = extra

           ENDFOR
                            
        ENDIF ELSE BEGIN

           CASE (region) OF
                
                2: BEGIN ; reset

                   IF (N_ELEMENTS (span) NE 0) THEN BEGIN
                   
                      prevSpan = span 

                      nSpan = (SIZE (span))[1] - 1     
                      FOR i = 1, nSpan DO BEGIN

                          edges = NEAREST_EDGE (xData, span[i, *])

                          OPLOT, [1, 1] * edges[0], yRange, _EXTRA = extra
                          OPLOT, [1, 1] * edges[1], yRange, _EXTRA = extra

                      ENDFOR
                    
                   ENDIF
                   span = fullSpan
                   haveSelection = 0
                   END
                
                3: BEGIN ; numerical entry

                   obj = OBJ_NEW ('selectBox', drawWidgetID, $
                       xRange, yRange, /OPLOT, $
                       /XSELECT, /NOEXACT, LINESTYLE = 2)
                   
                   canceled = obj->get ('canceled') 
                   coords   = obj->get ('selection')
                   exact    = obj->get ('exact')
                   OBJ_DESTROY, obj

                   IF (NOT canceled) THEN BEGIN
                   
                      haveSelection = 1
                      
                      xr = coords[0, *]
                      yr = coords[1, *]

                      xr = xr[SORT (xr)]
                      yr = yr[SORT (yr)]

                      s = TRANSPOSE (xr)
                      IF (N_ELEMENTS (span) NE 0) THEN BEGIN
                         span = [span, s]
                      ENDIF ELSE BEGIN
                         span = [[xr], [xr]]
                      ENDELSE

                      span[0, 0] = MIN (span)
                      span[0, 1] = MAX (span)

                      edges = NEAREST_EDGE (xData, s)
                      OPLOT, [1, 1] * edges[0], yRange, _EXTRA = extra
                      OPLOT, [1, 1] * edges[1], yRange, _EXTRA = extra

                      prevSpan = span 
	              newSpan = RESOLVE_SELECTIONS (xData, coords, span)
                      span = newSpan

                   ENDIF 
                   END

                4: ; do nothing

                ELSE: doExit = 1
           
           ENDCASE

           ; Erase old span, plot new span
           ;            
           IF ((region EQ 2) OR (region EQ 3)) THEN BEGIN

              nPrev = (SIZE (prevSpan))[1] - 1     
              nSpan = (SIZE (span))[1] - 1     

              FOR i = 1, nPrev DO BEGIN

                  edges = NEAREST_EDGE (xData, prevSpan[i, *])

                  OPLOT, [1, 1] * edges[0], yRange, COLOR = background
                  OPLOT, [1, 1] * edges[1], yRange, COLOR = background

              ENDFOR

              FOR i = 1, nSpan DO BEGIN

                  edges = NEAREST_EDGE (xData, span[i, *])

                  OPLOT, [1, 1] * edges[0], yRange, _EXTRA = extra
                  OPLOT, [1, 1] * edges[1], yRange, _EXTRA = extra

              ENDFOR

           ENDIF           
                     
	ENDELSE

    ENDREP UNTIL (doExit EQ 1) 

    ; Clear the plot
    ;
    nSpan = (SIZE (span))[1] - 1     
    FOR i = 1, nSpan DO BEGIN

        edges = NEAREST_EDGE (xData, span[i, *])

        OPLOT, [1, 1] * edges[0], yRange, COLOR = background
        OPLOT, [1, 1] * edges[1], yRange, COLOR = background

    ENDFOR

     
    RETURN, (NOT haveSelection) ? fullSpan : span
    
END
