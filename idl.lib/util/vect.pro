pro vect,el,az,v
  
;+
; NAME: VECT
; PURPOSE:
;       VECT returns the components of a unit vector in the direction
;       with given elevation and azimuth.  It is used to convert celestial,
;       galactic, and ecliptic coordinates into a right-handed Cartesian
;       vector.  The inverse function is provided by ELAZ.
;
; CALLING SEQUENCE:  
;       VECT,EL,AZ,V
;
; INPUTS:  
;      EL = elevation angle in degrees ("latitude" or "dec" coord)
;      AZ = azimuth angle in degrees ("longitude" or "RA" coord)
;      V = unit vector in direction of EL, AZ (dimension 3).
;  
; MODIFIED:
;      11/??/72 Originally written by Rick Borken in Fortran
;      Fall 81 by DNB - This subroutine used to exist
;          under the names pfvect, vector, and xector.  these have been
;          removed from 1789*util. and the name vect has been substituted.
;          this may result in some old programs bombing because of calls to
;          one of the old names.
;      08/09/04 by JLR - translated into IDL
;-  
  
  
  if n_params() eq 0 then begin
     print,'syntax - VECT,EL,AZ,V'
     print,'      EL = elevation angle in degrees ("latitude" or "dec" coord)'
     print,'      AZ = azimuth angle in degrees ("longitude" or "RA" coord)'
     print,'      V = unit vector in direction of EL, AZ (dimension 3)'
     return
  endif 
  
  v=dblarr(3)
  a=el*!dtor
  b=az*!dtor
  
  v[0]=cos(a)*cos(b)
  v[1]=cos(a)*sin(b)
  v[2]=sin(a)
  
  return
end 
