;+
; Name:
;	FUNCTION_COM
; CATEGORY: SPECTRA
;
;FUNCTION_COM - Common and common source for information on fitting functions
; Restrictions:
;	This is just an include block, not a procedure
; History:
;	ras, circa 1990
;-
common function_com, f_model, Epivot, a_cutoff
checkvar, Epivot, 50.0 ;Normal spectral pivot point for Hard X-ray spectra
checkvar, f_model, 'F_VTH_BPOW' ;DEFAULT FITTING FUNCTION, THERMAL AND BPOW
checkvar, a_cutoff, [10.,1.5]  ;parameters of low energy pl cutoff, keV and index
