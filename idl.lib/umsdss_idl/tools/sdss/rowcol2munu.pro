
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;   ROWCOL2MUNU 
;       
; PURPOSE:
;    convert from row-column to great circle coordinates (mu,nu)
;
; CALLING SEQUENCE:
;    rowcol2munu, trans, field, row, col, mu, nu, ri=ri
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    row,col: the row,column to be converted to mu,nu
;
; OPTIONAL INPUTS:
;    ri: the r-i color of the objects. Only necessary for objects
;         with r-i > trans.ricut
;
; OUTPUTS: 
;    mu,nu: SDSS great circle coords.
;
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    14-NOV-2000 Creation.  Erin Scott Sheldon UofMich
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO rowcol2munu, trans, field, row, col, mu, nu, ri=ri

  IF n_params() LT 4 THEN BEGIN 
      print,'-Syntax: rowcol2munu, trans, field, row, col, mu, nu, ri=ri'
      return
  ENDIF 

  nrow = n_elements(row)
  ncol = n_elements(col)

  IF nrow NE ncol THEN BEGIN 
      print,'row and col must be same size'
      return
  ENDIF 

  IF nrow GT 1 THEN BEGIN 
      rowm = dblarr(nrow)
      colm = dblarr(ncol)
  ENDIF ELSE BEGIN 
      rowm = 0d
      colm = 0d
  ENDELSE 

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

  drow0 = trans[w].drow0
  drow1 = trans[w].drow1
  drow2 = trans[w].drow2
  drow3 = trans[w].drow3

  dcol0 = trans[w].dcol0
  dcol1 = trans[w].dcol1
  dcol2 = trans[w].dcol2
  dcol3 = trans[w].dcol3

  csrow = trans[w].csrow
  cscol = trans[w].cscol
  ccrow = trans[w].ccrow
  cccol = trans[w].cccol
  ricut = trans[w].ricut
  
  ;; usually ricut is not meaningful, like ricut=100, so its optional

  IF n_elements(ri) EQ 0 THEN BEGIN
      nless = nrow
      nmore = 0
      wl = lindgen(nrow) 
  ENDIF ELSE BEGIN 
      wl = where(ri LT ricut,nless)
      wm = where(ri GE ricut,nmore)
  ENDELSE 

  IF nless NE 0 THEN BEGIN
      rowm[wl] = row[wl]+dRow0+dRow1*col[wl]+dRow2*(col[wl]^2)+dRow3*(col[wl]^3)+csRow*c
      colm[wl] = col[wl]+dCol0+dCol1*col[wl]+dCol2*(col[wl]^2)+dCol3*(col[wl]^3)+csCol*c
  ENDIF 
  IF nmore NE 0 THEN BEGIN  
      rowm[wm] = row[wm]+dRow0+dRow1*col[wm]+dRow2*(col[wm]^2)+dRow3*(col[wm]^3)+ccRow
      colm[wm] = col[wm]+dCol0+dCol1*col[wm]+dCol2*(col[wm]^2)+dCol3*(col[wm]^3)+ccCol
  ENDIF 

  mu = a + b * rowm + c * colm
  nu = d + e * rowm + f * colm

  return
END 

