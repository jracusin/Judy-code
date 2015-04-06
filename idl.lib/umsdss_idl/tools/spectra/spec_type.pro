;+
; NAME:
;  SPEC_TYPE
;
;
; PURPOSE:
;  Convert the spec class number to a string representing the type of the
;  object.  For example, spec_type(2) is "GALAXY"
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  st = spec_type(spec_cln)
;
; INPUTS:
;  spec_cln: the spec classification number from spectro 1d. This can be an
;            array. 
;
; OUTPUTS:
;  The string version of the classification is returned
;
;
; MODIFICATION HISTORY:
;   Dave Johnston ??/??/??  
;   Added to archive, documented.  Erin Sheldon 15-Aug-2003
;
;-


function spec_type,spec_cln

  types=['UNKNOWN','STAR','GALAXY','QSO','HIZ_QSO','SKY',$
         'STAR_LATE','GAL_EM']

  return,types(spec_cln)
end
