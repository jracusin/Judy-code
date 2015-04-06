pro mom2seeing,mom,seeing

;+
; NAME:
;    MOM2SEEING
;
;
; PURPOSE:
;    Take the adaptive moments of the psf and convert this a "seeing" value 
;    or FWHM assumes the psf is Gaussian
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
;-


if n_params() eq 0 then begin
   print,'-syntax  mom2seeing,mom,seeing'
   return
endif

seeing=2.35*0.4*sqrt ( mom/2.0 > 0.0)

return
end

