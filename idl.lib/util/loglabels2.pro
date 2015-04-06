;+
; NAME:
;  LOGLABELS
;
;
; PURPOSE:
;  Generate nice labels for a log plot. 
;
;
; CATEGORY:
;  Plotting
;
;
; CALLING SEQUENCE:
;  This is used with the [xyz]tickformat keyword to plotting routines. e.g.
;  plot, x, y, /xlog, xtickf='loglabels'
;
; MODIFICATION HISTORY:
;  Written by Erin Sheldon UofChicago
;
;-


FUNCTION loglabels2, axis, index, value,everyother=everyother

  on_error,2

  ;; we won't use axis or index

  ;; make sure its log binning
;  IF axis EQ 0 THEN IF !x.type NE 1 THEN return,ntostr(value)
;  IF axis EQ 1 THEN IF !y.type NE 1 THEN return,ntostr(value)
;  IF axis EQ 2 THEN IF !z.type NE 1 THEN return,ntostr(value)

  tickval = round(alog10(value))
  IF tickval EQ 0 THEN BEGIN 
      tickn = '1'
  ENDIF ELSE IF tickval EQ 1 THEN BEGIN 
      tickn = '10'
  ENDIF ELSE IF tickval EQ -1 THEN BEGIN 
      tickn = '0.1'
  ENDIF ELSE BEGIN 
      tickn = '10!U'+ntostr(tickval)+'!N'
  ENDELSE 
  
  if tickval mod 2 eq 1 then tickn=''
     
  return,tickn

END 


