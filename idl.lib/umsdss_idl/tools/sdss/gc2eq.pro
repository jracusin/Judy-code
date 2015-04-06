
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GC2EQ
;       
; PURPOSE:
;    convert from great circle to equatorial coordinates
;
; CALLING SEQUENCE:
;    gc2eq, mu, nu, node, inc, ra, dec
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   ra,dec: equatorial coords. 
;
;
; CALLED ROUTINES:
;    ATBOUND
;    ATBOND2 
;
; PROCEDURE: 
;    Taken from astrotools
;	
;
; REVISION HISTORY:
;    14-NOV-2000  Erin Scott Sheldon UofMich Taken from astrotools
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

PRO gc2eq, mu_in, nu_in, node_in, inc_in, ra, dec

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: gc2eq, mu, nu, node, inc, ra, dec'
      print,' mu, nu, node, inc in degrees'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  at_deg2Rad = !dpi/180.
  at_rad2Deg = 180./!dpi

  ;; convert to radians
  mu = double( mu_in*at_deg2Rad )
  nu = double( nu_in*at_deg2Rad )

  node = double( node_in*at_deg2Rad )
  inc  = double( inc_in*at_deg2Rad )

  x2 = cos(mu-node)*cos(nu)
  y2 = sin(mu-node)*cos(nu)
  z2 = sin(nu)
  y1 = y2*cos(inc) - z2*sin(inc)
  z1 = temporary(y2)*sin(inc) + temporary(z2)*cos(inc)

  ra = atan(temporary(y1), temporary(x2)) + node
  dec = asin(temporary(z1))

  ;; convert back to degrees
  ra = ra*at_rad2Deg
  dec = dec*at_rad2Deg

  atbound2, dec, ra

  return
END 
