FUNCTION angdist, zmax, zmin, h=h, q0=q0, omegamat=omegamat, verbose=verbose, $
                  plot=plot, oplot=oplot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    ANGDIST
;       
; PURPOSE: 
;    calculate angular diameter distance between zmin and zmax
;    Currently only works for lambda = 0 universe.
;	
;
; CALLING SEQUENCE: 
;    result = angdist(z1, z2, h=h, q0=q0, omegamat=omegamat, $
;                     silent=silent, plot=plot, oplot=oplot )
;      
; INPUTS:  
;    zmax: max redshift 
;
; OPTIONAL INPUTS:
;    zmin:  Zmin is optional, the default is 0.0
;
; OPTIONAL KEYWORDS: 
;    h: hubble parameter in units of H100 default is 1.0
;    q0: dacceleration parameter. default is q0=0.5
;    omegamat:  If input, uses q0=omegamat/2.
;
; OUTPUTS: 
;    dist in MPC
;
; REVISION HISTORY: 
;    Author: Erin Scott Sheldon 2/24/99
;	   
;                                      
;-                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() eq 0 then begin
	print,'-Syntax: result = angdist(zmax [, zmin, h=h, q0=q0, omegamat=omegamat, silent=silent] )'
	print,'   Returns Angular diameter Distance in Mpc from zmin to zmax'
        print,'   for a matter only universe.'
	return,0.
  endif

IF n_elements(zmin) EQ 0 THEN zmin = 0.
IF n_elements(h) EQ 0 THEN h=1.0

;;;;;;;; some parameters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c = 2.9979e5                  ;;  speed of light in km/s
H0 = 100.0*h                  ;;  hubbles constant in km/s/Mpc

IF n_elements(q0) EQ 0 THEN q0 = 0.5
IF n_elements(omegamat) NE 0 THEN q0 = omegamat/2.

fac = 2.*c/H0
dang = sqrt(1.+2.0*q0*zmin)*( 2.-2.*q0*(1.-zmax) ) - $
                               sqrt(1.+2.*q0*zmax)*( 2.-2.*q0*(1.-zmin) )
dang = dang*fac/( 4.*q0^2*(1.+zmax)^2*(1.+zmin) )

IF keyword_set(verbose) THEN BEGIN
  print,'-----------------------------------'
  print,'Using h = ',ntostr(h),' q0 = ',ntostr(q0)
  print,'Ang Diameter Dist: '+ntostr(dang)+' Mpc ('+ntostr(zmin,4)+ ' to '+ntostr(zmax,4)+')'
  print,'-----------------------------------'
ENDIF

return,dang
END

