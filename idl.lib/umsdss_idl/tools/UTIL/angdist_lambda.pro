FUNCTION aeta_lambda, z, omegamat

  s3 = (1.-float(omegamat))/omegamat
  s = s3^(1./3.)
  s2 = s^2
  s4 = s^4
  a = 1./(1.+float(z))

  c0 = 1.
  c1 = -.1540
  c2 = .4304
  c3 = .19097
  c4 = .066941
  
  ex = -1./8.

  ;; returns conformal time
  return, 2.*sqrt(s3+1.)*( c0/a^4 + c1*s/a^3 + $
                           c2*s2/a^2 + c3*s3/a +c4*s4)^ex
END 

FUNCTION angdist_lambda, zmax, zmin, h=h, omegamat=omegamat, dlum=dlum, verbose=verbose

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    ANGDIST_LAMBDA
;       
; PURPOSE: 
;    calculate angular diameter from zmin to zmax in flat lambda universe
;    Uses 4th order polynomial approximation 
;    see Pen, U., 1999, ApJ Supplement, 20, 49.
;    For z between 0.2 and 1.0 the error is about .4% 
;    (Exact for omega_matter = 1.0)
;	
;
; CALLING SEQUENCE: 
;    result = angdist(zmax, [zmin, h=h, omegamat=omegamat, dlum=dlum, 
;                     verbose=verbose])
;      
; INPUTS:  
;    z: redshift
;
; OPTIONAL INPUTS: 
;    h: hubble parameter       default is 1.0
;    omegamat:  omega matter   default is .3
;
; OPTIONAL OUTPUTS:
;    dlum: the luminosity distance.
;
; OPTIONAL KEYWORDS: 
;    /verbose:  print distances.
;       
; OUTPUTS: distance in megaparsecs.
;
; REVISION HISTORY: 
;    Author: Erin Scott Sheldon 2/24/99
;	
;       
;                                      
;-                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() eq 0 then begin
	print,'-Syntax: result = angdist_lambda(zmax [zmin , h=h, omegamat=omegamat, dlum=dlum, verbose=verbose] )'
	print,'   Returns Angular diameter Distance in Mpc from zmin to zmax (zmin optional, default is zmin=0.'
        print,'   in a flat lambda cosmology.'
        print,'   (assumes omega = 1 = omegamat + omegalambda)'
        print,'   default is omegamat=.3   h=1.0'
	return,0.
  endif

IF n_elements(h) EQ 0 THEN h=1.0
IF n_elements(omegamat) EQ 0 THEN omegamat=.3
IF n_elements(zmin) EQ 0 THEN zmin = 0.

;;;;;;;; some parameters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c = 2.9979e5                  ;;  speed of light in km/s
H0 = 100.0*h                  ;;  hubbles constant in km/s/Mpc

fac = c/H0*(1.+zmax)

dlum = fac*( aeta_lambda(zmin, omegamat) - aeta_lambda(zmax, omegamat) )
dang = dlum/(1.+zmax)^2

IF keyword_set(verbose) THEN BEGIN
  print,'-----------------------------------'
  print,'Using h = ',ntostr(h),'  omega matter = ',ntostr(omegamat)
  print,'Ang Diameter Dist: '+ntostr(dang)+' Mpc ('+ntostr(zmin,4)+ ' to '+ntostr(zmax,4)+')'
  print,'-----------------------------------'
ENDIF

return,dang
END

