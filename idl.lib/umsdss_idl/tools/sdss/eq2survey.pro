;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    EQ2SURVEY
;       
; PURPOSE:
;    Convert from ra, dec to lambda, eta (SDSS survey coordinates)
;
; CALLING SEQUENCE:
;    eq2survey, ra, dec, lambda, eta
;
; INPUTS: 
;    ra: Equatorial latitude in degrees 
;    dec: Equatorial longitude in degrees
;
; OPTIONAL INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;       
; OUTPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
;
; OPTIONAL OUTPUTS:
;    None
;
; CALLED ROUTINES:
;    ATBOUND
;    ATBOUND2
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO atbound, angle, min, max

  w=where(angle LT min,nw)
  WHILE nw NE 0 DO BEGIN 
      angle[w] = angle[w] + 360.0
      w=where(angle LT min,nw)
  ENDWHILE 

  w=where(angle GE max,nw)
  WHILE nw NE 0 DO BEGIN 
      angle[w] = angle[w] - 360.0
      w=where(angle GT max,nw)
  ENDWHILE 

  return
END 

PRO atbound2, theta, phi

  atbound, theta, -180.0, 180.0
  w = where( abs(theta) GT 90.,nw)
  IF nw NE 0 THEN BEGIN
      theta[w] = 180. - theta[w]
      phi[w] = phi[w] + 180.
  ENDIF 
  atbound, theta, -180.0, 180.0
  atbound, phi, 0.0, 360.0

  w=where( abs(theta) EQ 90., nw)
  IF nw NE 0 THEN phi[w] = 0.0

  return
END 

PRO eq2survey, ra_in, dec_in, lambda, eta

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: eq2survey, ra, dec, lambda, eta'
      print,' ra, dec in degrees'
      return
  ENDIF 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  surveyCenterRa  =  185.0
  surveyCenterDec =   32.5

  deg2Rad = !dpi/180.
  rad2Deg = 180./!dpi
 
  node = surveyCenterRa - 90.
  node = node * deg2Rad

  etaPole = surveyCenterDec * deg2Rad

  nra = n_elements(ra_in)
  ndec = n_elements(dec_in)
  IF nra NE ndec THEN BEGIN 
      print,'ra and dec must be same size'
      return
  ENDIF 

  w=where((ra_in GT 360d) OR (ra_in LT 0d), nw)
  IF nw NE 0 THEN message,'RA must be within [0,360]'
  w=where((dec_in GT 90d) OR (dec_in LT -90d),nw)
  IF nw NE 0 THEN message,'DEC must be within [-90,90]'

  ;; Convert to radians
  ra = double(ra_in*deg2Rad)
  dec = double(dec_in*deg2Rad)

  x1 = cos(ra-node)*cos(dec)
  y1 = sin(ra-node)*cos(dec)
  z1 = sin(dec)

  ;; free memory
  ra=0
  dec=0

  lambda = -asin( temporary(x1) )
  eta = atan( temporary(z1), temporary(y1) ) - etaPole

  ;; convert to degrees
  lambda = lambda * rad2Deg
  eta = eta * rad2Deg

  atbound2, lambda, eta
  atbound, eta, -180.0, 180.0
  
  w=where(eta GT (90. - surveyCenterDec) , nw)
  IF nw NE 0 THEN BEGIN 
      eta[w] = eta[w] - 180.
      lambda[w] = 180. - lambda[w]
  ENDIF 
  atbound, lambda, -180.0, 180.0
  
  return
END
