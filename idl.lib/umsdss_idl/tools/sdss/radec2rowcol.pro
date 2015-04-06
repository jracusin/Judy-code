PRO radec2rowcol, trans, node, inc, field, ra, dec, row, col

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;   ROWCOL2MUNU 
;       
; PURPOSE:
;    convert from equatorial coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so
;
; CALLING SEQUENCE:
;    radec2rowcol, trans, node, inc, field, ra, dec, row, col
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    ra,dec: equatorial coordinates
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

  IF n_params() LT 6 THEN BEGIN 
      print,'-Syntax: radec2rowcol, trans, node, inc, field, ra, dec, row, col'
      return
  ENDIF 

  eq2gc, ra, dec, node, inc, mu, nu
  munu2rowcol, trans, field, mu, nu, row, col

END 
