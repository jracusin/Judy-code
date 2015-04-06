function cel2xyz, RA, Dec
;+
; NAME:
;	Cel2XYZ
;
; PURPOSE:
;	Determine the x,y,z coordinates in a universal reference frame
;	of a direction to a given RA and Declination.
;
; CATEGORY:
;	Astronomy
;
; CALLING SEQUENCE:
;	result = Cel2XYZ (RA, Dec)
;
; INPUTS:
;       RA:  right ascension in arcus (0 * !dtor .. 360 * !dtor)
;       Dec: declination in arcus (-90 * !dtor .. +90 * !dtor)
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;       Cel2XYZ returns a vector with three elements, giving the
;       direction of the celestial coordinates entered. The length of
;       the vector is 1. x-direction is to vernal equinox, z-direction
;       is due North.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Hopefully none.
;
; RESTRICTIONS:
;	Don't know.
;       If Dec > 90 deg, a warning is printed on the screen.
;
; MODIFICATION HISTORY:
;	1997 Sep 14, first version. (c) Detlef Koschny
;-

CelCoords = DBLARR (3)

r = 1.0D

if double (!radeg) * Dec GT 90. then print, 'Cel2XYZ: Warning - Dec > 90 deg!'
CelCoords (0) = r * cos (Dec) * cos (RA)
CelCoords (1) = r * cos (Dec) * sin (RA)
CelCoords (2) = r * sin (Dec)

return, CelCoords
end  ;function Cel2XYZ
