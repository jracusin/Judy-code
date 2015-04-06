;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    SURVEY2EQ
;       
; PURPOSE:
;    Convert from lambda, eta (SDSS survey coordinates) to ra, dec
;
; CALLING SEQUENCE:
;    survey2eq, lambda, eta, ra, dec
;
; INPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
;
; OPTIONAL INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;       
; OUTPUTS: 
;    ra: Equatorial latitude in degrees
;    dec: Equatorial longitude in degrees
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

PRO survey2eq, lambda_in, eta_in, ra, dec

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: survey2eq, lambda, eta, ra, dec'
      print,' lambda, eta in degrees'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  at_surveyCenterRa  =  185.0
  at_surveyCenterDec =   32.5

  at_deg2Rad = !dpi/180.
  at_rad2Deg = 180./!dpi
 
  node = at_surveyCenterRa - 90.
  node = node * at_deg2Rad

  etaPole = at_surveyCenterDec * at_deg2Rad

  nlam = n_elements(lambda_in)
  neta = n_elements(eta_in)
  IF nlam NE neta THEN BEGIN 
      print,'lambda and eta must be same size'
      return
  ENDIF 

  ;; Convert to radians
  lambda = double(lambda_in*at_deg2Rad)
  eta = double(eta_in*at_deg2Rad)

  x1 = -sin(lambda)
  y1 = cos(eta + etaPole)*cos(lambda)
  z1 = sin(eta + etaPole)*cos(lambda)

  ra = atan( temporary(y1), temporary(x1) ) + node
  dec = asin( temporary(z1) )

  ;; convert ot degrees
  ra = ra*at_rad2Deg
  dec = dec*at_rad2Deg
  atbound2, dec, ra
      
  return
END
