;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GC2CSURVEY
;       
; PURPOSE:
;    convert from SDSS great circle to corrected SDSS survey coordinates
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
;   EQ2CSURVEY
; PROCEDURE: 
;    Call gc2eq, then eq2csurvey
;	
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO gc2csurvey, mu_in, nu_in, node_in, inc_in, clambda, ceta

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: gc2csurvey, mu, nu, node, inc, clambda, ceta'
      print,' mu, nu, node, inc in degrees'
      return
  ENDIF 

  ;; just convert to ra,dec and then convert
  ;; to survey

  gc2eq, mu_in, nu_in, node_in, inc_in, ra, dec
  eq2csurvey, ra, dec, clambda, ceta

  return
END 
