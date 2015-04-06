;+
; NAME:
;	MAKE_HISTOGRAM
;
; PURPOSE:
;	Creates a histogram
;
; CATEGORY:
;	Plotting
;
; CALLING SEQUENCE:
;	MAKE_HISTOGRAM, bin_edge, bin
;
; INPUTS:
;	BIN_EDGE = bin edges for the data.
;	BIN      = value in each bin, length is N_ELEMENTS(BIN_EDGE) - 1.
; 
; KEYWORD PARAMETERS:
;       PLOT: Set to create a new plot, else overplots
;       HELP: Prints calling sequence
;       EXTRA: Pass EXTRA keywords on to PLOT, OPLOT. Note: some PLOT keywords
;              are not accepted by OPLOT, such as /XLOG, so take care.
;
; OUTPUTS:
;       NONE.
;
; COMMON BLOCKS:
;	NONE.
;
; SIDE EFFECTS:
;       Plots to the current window.
;
; RESTRICTIONS:
;	NONE.
;
; PROCEDURE:
;       Straight-forward
;
; MODIFICATION HISTORY:
;	Written by R. Mallozzi, UAH / NASA, January, 1995.
;
;    Mar. 22, 2000: (RDP) Added _EXTRA keyword to pass additional parameters to
;    PLOT and OPLOT commands. Also fixed PLOT keyword so that the plot can be 
;    generated in one pass.
;-

PRO MAKE_HISTOGRAM, BIN_EDGE, BIN, PLOT = PLOT, HELP = HELP, _EXTRA = EXTRA


IF (KEYWORD_SET(HELP)) THEN BEGIN
   PRINT, 'MAKE_HISTOGRAM, BIN_EDGE, BIN, PLOT = PLOT, HELP = HELP, _EXTRA = EXTRA'
   RETURN
ENDIF

NUM_BINS = N_ELEMENTS(BIN)
BIN_SAVE = BIN
BIN = FLOAT(BIN)

; Check for bins of zero events in case of log scale
IF (!Y.TYPE EQ 1) THEN BEGIN
   YR = 10.0^!Y.CRANGE
   CHECK = WHERE(BIN EQ 0, ZCOUNT)
   IF (ZCOUNT NE 0) THEN BIN(WHERE(BIN EQ 0)) = YR(0)
ENDIF ELSE BEGIN
   YR = !Y.CRANGE
ENDELSE

IF KEYWORD_SET(PLOT) THEN BEGIN
   XRRANGE = [MIN (BIN_EDGE), MAX (BIN_EDGE)]
   PLOT, BIN_EDGE[1:*], BIN, XRANGE = XRRANGE, /NODATA, _EXTRA = EXTRA
ENDIF ;ELSE BEGIN
  ; OPLOT, [BIN_EDGE(0), BIN_EDGE(0)], [YR(0), BIN(0)], _EXTRA = EXTRA
;ENDELSE

FOR I=0, NUM_BINS-1 DO BEGIN

    OPLOT, [BIN_EDGE(I), BIN_EDGE(I+1)], [BIN(I), BIN(I)], _EXTRA = EXTRA

    IF (I GT 0) THEN BEGIN
       OPLOT, [BIN_EDGE(I), BIN_EDGE(I)], [BIN(I-1), BIN(I)], _EXTRA = EXTRA
    ENDIF
ENDFOR
OPLOT, [BIN_EDGE(NUM_BINS), BIN_EDGE(NUM_BINS)], [YR(0), BIN(NUM_BINS-1)], _EXTRA = EXTRA

BIN = BIN_SAVE



END
