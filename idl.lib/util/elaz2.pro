pro elaz2,v,el,az
;+
; NAME: ELAZ2
; PURPOSE:
;     ELAZ2 returns the elevation and azimuth of a given vector.  It is used
;     to convert from a right-handed Cartesian vector to celestial, galactic
;     or ecliptic coords.  The inverse function is provided by VECT.
;
; CALLING SEQUENCE:  
;       ELAZ2,V,EL,AZ
;
; INPUTS:  
;      V = vector (dim 3) pointing in direction of interest.
;      EL = elevation angle of vector V in decimal degrees.
;      AZ = azimuthal angle of vector V in decimal degrees.

; MODIFIED:
;      11/??/72 Originally based on Fortran version but works slightly differently
;      08/09/04 by JLR - translated into IDL
;      09/17/04 by JLR - changed to ELAZ2 to not conflict with ELAZ (astrolib)
;- 
  
  n=n_elements(v[0,*])
  el=dblarr(n) & az=el
  
  for i=0,n-1 do begin 
     el[i]=asin(v[2,i]/sqrt(v[0,i]^2.+v[1,i]^2.+v[2,i]^2.))*!radeg
     ccc=v[0,i]^2.+v[1,i]^2.
     
     if v[1,i] ge 0. then s=1. else s=-1.
     
     if ccc ne 0. then $
        az[i]=(abs(acos(v[0,i]/sqrt(v[0,i]^2.+v[1,i]^2.))*!radeg)*s+720.) mod 360. else az=0.
  endfor 
   
  return
end 
