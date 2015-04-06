;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GC2SURVEY
;       
; PURPOSE:
;    convert from SDSS great circle to SDSS survey coordinates
;
; CALLING SEQUENCE:
;    gc2survey, mu, nu, node, inc, lambda, eta
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   lambda, eta: survye coords. 
;
;
; CALLED ROUTINES:
;   GC2EQ
;   GC2SURVEY
;
; PROCEDURE: 
;    Call gc2eq, then eq2survey
;	
;
; REVISION HISTORY:
;    26-Sep-2002  Erin Scott Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO gc2survey, mu_in, nu_in, node_in, inc_in, lambda, eta

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: gc2survey, mu, nu, node, inc, lambda, eta'
      print,' mu, nu, node, inc in degrees'
      return
  ENDIF 

  ;; just convert to ra,dec and then convert
  ;; to survey

  gc2eq, mu_in, nu_in, node_in, inc_in, ra, dec
  eq2survey, ra, dec, lambda, eta

  return
END 
