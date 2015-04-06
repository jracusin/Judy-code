; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;+
; NAME:
;    scaleAxis
;
; PURPOSE:
;    Scale the range of a plot axis.
;    Handles both linear and log scales transparently.
;
; CATEGORY:
;    Plotting
;
; CALLING SEQUENCE:
;    RANGE = scaleAxis (SCALE [, /XAXIS, /YAXIS, /ZAXIS])
;
; INPUTS:
;    SCALE: FLOAT value used to scale the axis range.  To
;           increase the scale of the axis by 10%, set SCALE
;           to 0.10.  To decrease the scale, set SCALE to
;           a negative value.
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
;    ; Increase the scale of the y-axis by 10%
;    ;
;    PLOT, xRange, yRange
;    newYrange = scaleAxis (/YAXIS, 0.10)
;    PLOT, xRange, newYrange
;
; MODIFICATION HISTORY:
;       Written: 1999 April, Robert.Mallozzi@msfc.nasa.gov
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

FUNCTION scaleAxis, scale, XAXIS = xaxis, YAXIS = yaxis, ZAXIS = zaxis

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
                
    delta = (crange[1] - crange[0]) * ABS (scale)

    IF (scale GE 0) THEN $
       newRange = [crange[0] - (delta / 2.0), crange[1] + (delta / 2.0)]
    
    IF (scale LT 0) THEN $       
       newRange = [crange[0] + (delta / 2.0), crange[1] - (delta / 2.0)]
  

    RETURN, (newRange * (type EQ 0)) + $
            (10.0^(newRange * (type EQ 1)) * (type EQ 1))
    
END
