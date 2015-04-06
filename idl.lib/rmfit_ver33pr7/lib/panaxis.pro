; ----------------------------------------------------------------------------
;+
;
; NAME:
;    panAxis (FUNCTION)
;
; PURPOSE:
;    Pan the range of a plot axis.
;    Handles both linear and log scales transparently.
;
; CALLING SEQUENCE:
;    RANGE = panAxis (PAN [, /XAXIS, /YAXIS, /ZAXIS])
;
; INPUTS:
;     PAN: FLOAT value used to pan the axis range.  To slide an axis 
;          range to the right by 10%, set PAN to 0.10.  To slide the
;          range to the left set PAN to a negative value.  
;
;          x-axis: PAN >= 0 to pan right
;                  PAN <  0 to pan left
;
;          y-axis: PAN >= 0 to pan up
;                  PAN <  0 to pan down
;
;          z-axis: PAN >= 0 to pan forward
;                  PAN <  0 to pan back
;
;          Note that this function pans the scale of the AXIS, so to pan
;          a plot, pan the axis in the opposite direction that you want to
;          pan the plot.  For example, to pan a plot to the left, use
;          a positive value for PAN:   range = panAxis (/XAXIS, 0.10) 
;
; KEYWORDS:
;    XAXIS : Set this keyword to apply the scaling to the x-axis
;    YAXIS : Set this keyword to apply the scaling to the y-axis
;    ZAXIS : Set this keyword to apply the scaling to the z-axis
;
; OUTPUTS:
;    RANGE: two element FLTARR of the range of the line
;
; EXAMPLE:
;
;    ; Slide the axis scale up by 10%
;    ;
;    PLOT, xRange, yRange
;    newYrange = panAxis (/XAXIS, -0.10)
;    PLOT, xRange, newYrange
;
; MODIFICATION HISTORY:
;       Written: 1999 April, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------

FUNCTION panAxis, pan, XAXIS = xaxis, YAXIS = yaxis, ZAXIS = zaxis

    ; Default to x-axis
    ;
    crange = !X.CRANGE
    type   = !X.TYPE

    IF (KEYWORD_SET (xaxis)) THEN BEGIN
     
       crange = !X.CRANGE
       type   = !X.TYPE
    
    ENDIF

    IF (KEYWORD_SET (yaxis)) THEN BEGIN
     
       crange = !Y.CRANGE
       type   = !Y.TYPE
    
    ENDIF

    IF (KEYWORD_SET (zaxis)) THEN BEGIN
     
       crange = !Z.CRANGE
       type   = !Z.TYPE
    
    ENDIF

    delta = (crange[1] - crange[0]) * ABS (pan)

    IF (pan GE 0) THEN $
       newRange = crange + delta
    
    IF (pan LT 0) THEN $       
       newRange = crange - delta
  

    RETURN, (newRange * (type EQ 0)) + $
            (10.0^(newRange * (type EQ 1)) * (type EQ 1))
    
END
