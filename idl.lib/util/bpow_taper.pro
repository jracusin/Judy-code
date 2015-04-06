;+
; NAME: bpow_taper
;	
; PURPOSE: broken power-law function without discontinuities in the
;	derivatives
;	
; CALLING SEQUENCE: result = bpow_taper(x, a)
;	
; INPUTS:
;	x - independent variable, nominally energy in keV under SPEX
;	a - parameters defined as
;	normalization at Epivot is 1
;		a(0) - negative power law index below break
;		a(1) - break energy
;		a(2) - negative power law index above break
;		a(3) - low energy cutoff
;		a(4) - negative power law index of low energy cutoff, 1<Eco<2, default 1.5
;	
; OPTIONAL INPUTS:
;	dx - scale length for spline interpolation around breaks 
;	     in fractional units of x	
;	tension - tension in spline fit
; OUTPUTS:
;	result of function, a broken power law
; OPTIONAL OUTPUTS:
;
; PROCEDURE:	uses a spline interpolation to give a smooth
;	transitions at the break energies
;
; CALLS:
;
; COMMON BLOCKS:
;	function_com
;	common function_com, f_model, Epivot, a_cutoff
;	checkvar, Epivot, 50.0 ;Normal spectral pivot point for Hard X-ray spectra
;	checkvar, f_model, 'F_VTH_BPOW' ;DEFAULT FITTING FUNCTION, THERMAL AND BPOW
;	checkvar, a_cutoff, [10.,1.5]  ;parameters of low energy pl cutoff, keV and index
;	
; RESTRICTIONS:
;	break energy should be higher than the low energy cutoff by at least dx
; MODIFICATION HISTORY:
;	ras, 15 March 1995	
;-
;
function bpow_taper,x,a, dx, tension
;broken power law function
;
;If the break energy is set to zero, the break is ignored and the power
;law is calculated over the full range

;normalization at Epivot is 1
;a(0) - negative power law index below break
;a(1) - break energy
;a(2) - negative power law index above break
;a(3) - low energy cutoff
;a(4) - negative power law index of low energy cutoff, 1<Eco<2, default 1.5

;If the break energy is set to zero, the break is ignored and the power
;law is calculated over the full range
;

;default parameters

@function_com

apar=[fltarr(3),10.0,1.5]

npar = n_elements(a)
apar(0:npar-1) = a
apar(4) =  (apar(4)>1.0) < 2.0 ;spectral slope of low energy cutoff spectrum

ans = x*0.0

delta = apar(2)-apar(0) ;difference in power-laws
checkvar,dx, 0.20
checkvar,tension, 1.0

wl = where( x le (1.-dx)*apar(1), nl)
wh = where( x ge (1.+dx)*apar(1), nh)
wb = where( x ge (1.-dx)*apar(1) and x le (1.+dx)*apar(1), nb)

ans = x * 0.0

if nl ge 1 then ans(wl) = (epivot / x(wl) ) ^apar(0)
if nh ge 1 then ans(wh) = (epivot / x(wh) ) ^apar(2) * ( apar(1)/epivot )^delta
if nb ge 1 then begin
	xindx = x([wl,wh])
	ord   = sort(xindx)
        ans(wb) = exp(spline( alog(xindx(ord)),alog( ans(([wl,wh])(ord))), alog(x(wb)), tension ))
endif
   
;test for low-energy cutoff
wless = where( x lt apar(3), nless)

;EXTEND THE SPECTRUM TO LOW ENERGIES BELOW THE CUTOFF WITH A POWER-LAW OF <2
if nless ge 1 then begin

   delta = apar(4)-apar(0) ;difference in power-laws
   dx = 0.10
   wl = where( x le (1.-dx)*apar(3), nl)
   wh = where( x ge (1.+dx)*apar(3) and x lt (1.- dx)*apar(1), nh)
   wb = where( x ge (1.-dx)*apar(3) and x le (1.-dx)*apar(1), nb)
   if nl ge 1 then ans(wl) = (epivot / x(wl) ) ^apar(4) * ( apar(3)/epivot )^delta

   if nb ge 1 then begin
	xindx = x([wl,wh])
	ord   = sort(xindx)
        ans(wb) = exp(spline( alog(xindx(ord)),alog( ans(([wl,wh])(ord))),alog( x(wb)), tension ))
   endif

endif

a=apar[0:2]

return,ans
end
