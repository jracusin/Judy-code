;+
; NAME: 
;	F_BPOW
;	
; PURPOSE: broken power-law function with/without discontinuities in the
;	derivatives
;	
; CALLING SEQUENCE: result = f_bpow(x, a)
;		
; INPUTS:
;	x - independent variable, nominally energy in keV under SPEX
;	a - parameters describing the broken power-law, where:
;		a(0) - normalization at epivot
;		a(1) - negative power law index below break
;		a(2) - break energy
;		a(3) - negative power law index above break
;		a(4) - low energy cutoff
;		a(5) - negative power law index of low energy cutoff, 1<Eco<2, default 1.5
; OPTIONAL INPUTS:
;	Fname- the name of the function to really use, either 'bpow' or 'bpow_taper'
;		defaults to 'bpow'
; OUTPUTS:
;	result of function, a broken power law
; OPTIONAL OUTPUTS:
;
; PROCEDURE:	uses a spline interpolation to give a smooth
;	transitions at the break energies at bpow_taper otherwise
;	normal power-law with hard breaks
;
; CALLS:   bpow (hard breaks) or bpow_taper (smoothed breaks)
;
; COMMON BLOCKS:
;	f_bpow_com 
;	
; RESTRICTIONS:
;	break energy should be higher than the low energy cutoff by at least dx
; MODIFICATION HISTORY:
;	ras, 15 March 1995	
;-
;

function f_bpow,x,a, fname


common f_bpow_com, function_name, tension, dx
fname = fcheck( fname, fcheck(function_name, 'bpow') )
function_name = fname
if fname eq 'bpow_taper' then return, a(0)*bpow_taper(x, a(1:*), $
	fcheck(dx, 0.20), fcheck(tension, 1.0) ) else $
	return, a(0)* bpow( x, a(1:*) )
end
