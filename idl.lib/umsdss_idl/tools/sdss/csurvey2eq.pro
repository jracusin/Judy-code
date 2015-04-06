;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    CSURVEY2EQ
;       
; PURPOSE:
;    Convert from corrected lambda, eta (SDSS survey coordinates) to ra, dec
;
; CALLING SEQUENCE:
;    csurvey2eq, clambda, ceta, ra, dec
;
; INPUTS: 
;    clambda: Survey longitude in degrees
;    ceta: Survey latitude in degrees
;
; OPTIONAL INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;       
; OUTPUTS: 
;    ra: Equatorial latitude in degrees
;    dec: Equatorial longitude in degrees
;
; OPTIONAL OUTPUTS:
;    None
;
; CALLED ROUTINES:
;    SURVEY2EQ
; 
; PROCEDURE: 
;    Just calls survey2eq, which works on the corrected coordinates.
;	
;
; REVISION HISTORY:
;    Written: 26-Sep-2002  Erin Scott Sheldon
;                        Taken from astrotools.
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO csurvey2eq, clambda_in, ceta_in, ra, dec

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: csurvey2eq, clambda, ceta, ra, dec'
      print,' lambda, eta in degrees'
      return
  ENDIF 

  survey2eq, clambda_in, ceta_in, ra, dec
      
  return
END
