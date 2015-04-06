
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    EQ2GC
;       
; PURPOSE:
;    convert from equatorial to great circle coordinates
;
; CALLING SEQUENCE:
;    eq2gc, ra, dec, node, inc, mu, nu
;
; INPUTS: 
;    ra, dec: equatorial
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   mu, nu: great circle coords
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
      w=where(angle GE max,nw)
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

PRO eq2gc, ra_in, dec_in, node_in, inc_in, mu, nu

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: eq2gc, ra, dec, node, inc, mu, nu'
      print,' ra, dec, node, inc in degrees'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  at_deg2Rad = !dpi/180.
  at_rad2Deg = 180./!dpi

  ;; convert to radians
  ra = double( ra_in*at_deg2Rad )
  dec = double( dec_in*at_deg2Rad )

  node = double( node_in*at_deg2Rad )
  inc  = double( inc_in*at_deg2Rad )

  x1 = cos(ra - node)*cos(dec)
  y1 = sin(ra - node)*cos(dec)
  z1 = sin(dec)
  x2 = x1
  y2 = y1*cos(inc) + z1*sin(inc)
  z2 =-y1*sin(inc) + z1*cos(inc)

  mu = atan(y2, x2) + node
  nu = asin(z2)

  mu = mu*at_rad2Deg
  nu = nu*at_rad2Deg

  atbound2, nu, mu

  return
END 
