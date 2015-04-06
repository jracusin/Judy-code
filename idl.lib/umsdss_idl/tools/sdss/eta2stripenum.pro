FUNCTION eta2stripenum, eta, southern=southern, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    ETA2STRIPENUM
;       
; PURPOSE:
;    Find which stripe goes with this eta.
;
; CALLING SEQUENCE:
;    stripe = eta2stripenum(eta, southern=southern)
;
; INPUTS: 
;    eta: SDSS survey longitude.
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    /southern: This is a southern stripe
;       
; OUTPUTS: 
;    stripe
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    NTOSTR
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    14-NOV-2000 Taken from astrotools
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: stripe = eta2stripenum(eta, southern=southern)'
      return,-1
  ENDIF 

  stripesep = 2.5
  stripe = fix( (eta+58.75)/stripesep )
  
  IF keyword_set(southern) THEN BEGIN
      CASE stripe OF
          9:  stripe = 82
          10: stripe = 82
          11: stripe = 82
          3:  stripe = 76
          4:  stripe = 76        ;just in case
          5:  stripe = 76
          13: stripe = 86
          14: stripe = 86
          15: stripe = 86
          ELSE: BEGIN
              IF NOT keyword_set(silent) THEN print,'No southern stripe at eta = ',ntostr(eta)
              stripe = -1
          END 
      ENDCASE 

  ENDIF 
  return, stripe

END 
