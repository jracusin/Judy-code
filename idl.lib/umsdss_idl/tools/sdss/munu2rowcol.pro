
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;   ROWCOL2MUNU 
;       
; PURPOSE:
;    convert from great circle coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so
;
; CALLING SEQUENCE:
;    munu2rowcol, trans, field, mu, nu, row, col
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    mu,nu: great circle coordinates
;
; OUTPUTS: 
;    row,col: row/col in the bandpass of the trans structure
;
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    15-AUG-2002 Creation.  Erin Scott Sheldon UofMich
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO munu2rowcol, trans, field, mu, nu, row, col, ri=ri

  IF n_params() LT 4 THEN BEGIN 
      print,'-Syntax: munu2rowcol, trans, field, mu, nu, row, col, ri=ri'
      return
  ENDIF 

  nmu = n_elements(mu)
  nnu = n_elements(nu)

  IF nmu NE nnu THEN BEGIN 
      print,'mu/nu must be same size'
      return
  ENDIF 

  w=where(trans.field EQ field, nw)
  IF nw EQ 0 THEN BEGIN
      print,'Field ',field,' is not in trans file'
      return
  ENDIF 

  a = trans[w].a
  b = trans[w].b
  c = trans[w].c
  d = trans[w].d
  e = trans[w].e
  f = trans[w].f

  ;; ignore higher order stuff. Invert this relationship
  ;; mu = a + b * rowm + c * colm
  ;; nu = d + e * rowm + f * colm

  det = b*f - c*e

  mudiff = mu - a
  nudiff = nu - d

  row = ( mudiff*f - c*nudiff )/det
  col = ( b*nudiff - mudiff*e )/det

  return


  return
END 

