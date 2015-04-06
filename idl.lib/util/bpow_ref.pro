;+
; NAME:
;	BPOW
;
; PURPOSE:
;	This function returns a broken(double) power-law for an input vector.
;
; CATEGORY:
;	SPEX(spectroscopic analysis), fitting
;
; CALLING SEQUENCE:
; 	Spectrum = BPOW( X, A)
;
; INPUTS:
;	E: Input vector, for X-ray spectroscopy generally an Energy in keV.
;	   E must be greater than 0. For a discrete spectrum, these should be 
;          the channel energy means (arithmetic or geometric) or an array of
;          2xN low and high energy edges.  Then, arithmetic mean is used.
;	A: A set of parameters describing the broken power-law.
;		A(0): negative power-law index, always greater than 0.0 for E
;		      less than A(1)
;		A(1): Break point in E. If this value is 0.0 then the whole
;		      spectrum is a single power-law. 
;		A(2): negative power-law index, always greater than 0.0 for E
;		      greater than A(1)
;		A(3): Optional low-energy (X) break point.  If it isn't set
;		      on input it defaults to the value of "a_cutoff(0)"
;		      in the common "function_com"    
;		A(4): Optional negative power-law index below A(3).  If it isn't set
;		      on input it defaults to the value of "a_cutoff(1)"
;		      in the common "function_com"    
;
; OUTPUTS:
;	This function returns a broken(double) power-law for an input vector.
;
; COMMON BLOCKS:
;	FUNCTION_COM: see FUNCTION_COM.PRO
;
; PROCEDURE:
;	Divides the input vector into groups of points based on the break values,
;	then calculates the power-law output for each section where the normalization
;	is taken as 1.0 and the X values are scaled by "Epivot" given in
;	FUNCTION_COM.PRO
;
; EXAMPLE:
;	Spectrum = BPOW( findgen(100)+15.,[2.,40.,3.])
;
; MODIFICATION HISTORY:
; 	Written by:	RAS, documented 12-dec-1995
;	Version 2, richard.schwartz@gsfc.nasa.gov, use energy means or 2xn array of energies.
;-
;function bpow, e,a
pro bpow,e,a,ans

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

  apar=[fltarr(3),a_cutoff]

  npar = n_elements(a)
  apar(0:npar-1) = a
  apar(4) =  (apar(4)>1.0) < 2.0 ;spectral slope of low energy cutoff spectrum
;normal_eco=a[0]

  if (size(e))(0) eq 2 then edge_products, e, mean=em else em=e

  ans = em * 0.0

  if apar(1) gt 0.0 then begin
     w1=where(em le apar(1), nw1)
     if nw1 ge 1 then ans(w1) = (Epivot/em(w1))^apar(0)
     w2=where(em gt apar(1), nw2)
     if nw2 ge 1 then ans(w2) = $
        (Epivot/apar(1))^apar(0) * (apar(1)/em(w2))^apar(2)

     wless = where( em lt apar(3), nless)

;EXTEND THE SPECTRUM TO LOW ENERGIES BELOW THE CUTOFF WITH A POWER-LAW OF <2
     if nless ge 1 then begin
                                ;
                                ;what is the normalization at the cutoff energy?
        
        normal_eco = bpow( apar(3), apar)
        ans(wless) = normal_eco * (apar(3)/em(wless))^apar(4)
     endif 

  endif else ans = (Epivot/em)^apar(0)
 
  a=apar[0:2]
  
  return
;return,ans
end
