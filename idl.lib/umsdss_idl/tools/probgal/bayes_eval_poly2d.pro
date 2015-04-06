;+
; NAME:
;    BAYES_EVAL_POLY2D
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;    Creation:  ??-??-?? Dave Johnston, UofChicago
;
;     Current version: 1.5
;-

PRO bayes_eval_poly2d,x,y,k,val

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax bayes_eval_poly2d,x,y,k,val'
      RETURN
  ENDIF

  ss=size(k)
  nx=ss(1)
  ny=ss(1)

  val = 0.0d

  FOR i=0,nx-1 DO BEGIN
      FOR j=0,ny-1 DO BEGIN
          val = val + k(j,i)*x^i*y^j
      ENDFOR
  ENDFOR


  RETURN

END










