;+
; NAME:
;	DRAW_HISTOGRAM
;
; PURPOSE:
;	Creates a histogram
;
; CATEGORY:
;	Plotting
;
; CALLING SEQUENCE:
;	DRAW_HISTOGRAM, bin_center, bin_width, bin_value
;
; INPUTS:
;	BIN_CENTER = bin centers for independent data
;       BIN_WIDTH  = bin widths for independent data
;	BIN_VALUE  = value in each bin
; 
; KEYWORD PARAMETERS:
;       OPLOT:     Overplot on the current device
;       _EXTRA:    Any valid IDL PLOT (or OPLOT) keywords
;
; OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;	Written, 1999 November, Robert.Mallozzi@msfc.nasa.gov
;-

PRO DRAW_HISTOGRAM, bin_center, bin_width, bin, $
    OPLOT = oplot, _EXTRA = extra
 
    binSave = bin
    numBins = N_ELEMENTS (bin)
     
    ; Check for bins of zero events in case of log scale
    ;
    IF (!Y.TYPE EQ 1) THEN BEGIN
    
       YR = 10.0^!Y.CRANGE
       idx = WHERE (BIN EQ 0, cnt)
       IF (cnt NE 0) THEN bin[idx] = YR[0]
    
    ENDIF ELSE BEGIN
    
       YR = !Y.CRANGE
    
    ENDELSE

    bin_edge = bin_center - (bin_width / 2.0)
    bin_edge = [bin_edge, bin_center[numBins-1] + (bin_width[numBins-1] /2.0)]
    
    IF (NOT KEYWORD_SET (oplot)) THEN BEGIN
    
       xMin = MIN (bin_edge, MAX = xMax)
       yMin = MIN (bin, MAX = yMax)

       PLOT, /NODATA, /XLOG, /YLOG, [xMin, xMax], [yMin, yMax], _EXTRA = extra
    
    ENDIF
    
    FOR i = 0L, numBins - 1 DO BEGIN

        OPLOT, [bin_edge[i], bin_edge[i+1]], [bin[i], bin[i]], _EXTRA = extra

        IF (i GT 0) THEN BEGIN
           OPLOT, [bin_edge[i], bin_edge[i]], [bin[i-1], bin[i]], _EXTRA = extra
        ENDIF

    ENDFOR

    bin = binSave

END
