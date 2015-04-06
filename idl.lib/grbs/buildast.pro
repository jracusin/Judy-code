function buildast, cd, crpix, crval, ctype, flag, cam, equinox,$
RADIANS=radians, SCALE=scale,OFFSET=offset
;+
;  NAME:
;	BUILDAST
;  PURPOSE:
;	Build an ASTROMETRY structure from given data.  If data are not
;	supplied, prompt for all necessary.  A CD matrix must be supplied and
;	will not be prompted.
;
;  CALLING SEQUENCE:
;	ASTR = BUILDAST( CD, [CRPIX, CRVAL, CType, Flag, Cam, Equinox, $
;			/RADIANS,SCALE=scl,OFFSET=offst])
;
;  INPUTS:
;   	CD   - 2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;   	      	in DEGREES/PIXEL                                CD2_1 CD2_2
;	CRPIX - 2 element vector giving X and Y coord of reference pixel
;	CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;       	in DEGREES
;	CTYPE - 2 element vector giving dimension types (Nominally "RA--TAN",
;		"RA--UIT", "DEC--TAN","DEC--UIT")  (Defaults to "*--TAN")
;	FLAG - String containing "T" if plate solution requires a distortion
;		to match the plate, "F" if not. (Defaults to "F")
;	CAM -  String containing the UIT camera of the image ("A", "B" or "").
;		(Defaults to "")
;       EQUINOX- Scalar containing the equinox of astrometry coordinates 
;               (Defaults to 2000).
;
;  KEYWORD INPUT:
;	RADIANS - If present and non-zero, assumes that the input CD and CRVAL
;		are in RADIANS, not DEGREES.
;       SCALE - Scalar containing the multiplicative scale factor of the image
;               (e.g. 4.0 for a 512x512 compressed UIT image).  If not 
;		supplied, default value of 1 assumed (SCALE=0 ignored).
;       OFFSET - 2 element vector containing the X and Y offsets needed to put
;               the CRPIXes at (1024.5,1024.5) AFTER scaling.  If not supplied,
;		default value of [0,0] assumeed.
;
;  OUTPUT:
;     Function value = ASTROMETRY structure, containing:
;   	.CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;   	      	 in DEGREES/PIXEL                                 CD2_1 CD2_2
;	.CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;	.CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;       	 in DEGREES
;	.CTYPE - 2 element vector giving dimension types (Nominally "RA---TAN"
;		 or "RA---UIT", "DEC--TAN","DEC--UIT")
;	.FLAG -  String containing "T" if plate solution requires a distortion
;		 to match the plate, "F" if not.
;	.CAM -   String containing the UIT camera of the image ("A" or "B","N"
;		 or blank if not a UIT image).
;       .EQUINOX- Scalar containing equinox of CRVAL coordinates (def=2000)
;	.SCALE - Scalar containing the multiplicative scale factor of the image
;		 (e.g. 4.0 for a 512x512 compressed UIT image).  Will be 1.0
;		 if not a UIT image.
;	.OFFSET - 2 element vector containing the X and Y offsets needed to put
;		 the CRPIXes at (1024.5,1024.5) AFTER scaling.  Will be [0,0] 
;		 if not a UIT image.
;  PROCEDURE:
;	Routine prompts for any missing parameters (CD matrix must be 
;	supplied).
;	Once values for all fields are complete, the routine will build an
;	ASTROMETRY structure and return it.
;  COMMON BLOCKS:
;	None
;
;  EXAMPLES:
;	IDL> astr = buildast(cd, crpix, crval)	;Prompts for other values
;	IDL> putast,hdr, buildast(cd, cp, cv, ct, f, cam, equ) ;Puts astrometry
;								into header hdr
;  MODIFICATION HISTORY:
;	Written.  Joel D. Offenberg, Hughes STX, January, 1993.
;       All numerics floated.  RSH, HSTX, 22-Apr-1993.
;       Degrees converted to radians for entered CRVALs.  RSH, HSTX,
;          23-Apr-1993.
;	Now able to handle IRAS headers (3 CTYPES, 3 CRPIX---just ignore the 
;	third.)		JDO, HSTX, May 26, 1993
;-
	
npar = N_params()
if npar eq 0 then begin
	message,/inf,"Syntax:  res = BUILDAST(CD [,crpix,crval,ctype,flag,cam,equinox]) "
	return,0
endIF
 r2d = 180.0D/!DPI
IF (not(keyword_set(SCALE))) THEN SCALE = 1.0 ELSE SCALE = float(SCALE)
sze = size(OFFSET) 
IF (sze(sze(0)+2) ne 2) THEN OFFSET = [0.0,0.0] ELSE OFFSET = float(OFFSET)

;		Check CD and other parameters.
;
siz = size(cd)
IF (siz(0) ne 2) or (siz(1) ne 2) or (siz(2) ne 2) then begin 	;CD is a CD 
	message,/inf,"ERROR - CD must be a 2x2 matrix."		;matrix?
	return,0
endIF else $				;If CD exists and RADIAN is set,
IF keyword_set(RADIANS) then cd = cd * r2d 	;convert CD to Degrees
							

siz = size(crpix)
WHILE (siz(0) ne 1) or (siz(1) lt 2) do BEGIN	       ;Need at least 2 CRPIXes
	inp = ""
	print,'Reference Pixel: Fortran standard, first pixel is (1,1), not (0,0)'
	def = [1024.5,1024.5]
	read,string(f='("Enter Reference pixel locations [",f7.1,", ",f7.1,"] :")',def), inp
	if (inp eq "") then crpix = def $
	else crpix = getopt(inp,'F')
	siz = size(crpix)
endWHILE
crpix = crpix(0:1)

siz = size(crval)
;If CRVAL exists and RADIANS keyword is set, convert CRVAL to DEGREES
IF (keyword_set(RADIANS) AND (siz(0) eq 1) and (siz(1) ge 2)) then $
		crval = crval * r2d 	

WHILE (siz(0) ne 1) or (siz(1) lt 2) do BEGIN		;Need at least 2 CRVALs
	inp = ""
	WHILE n_elements(ra) lt 3 do BEGIN
		read,'Enter RA of reference pixel [HRS, MIN, SEC]: ',inp
		ra = getopt(inp,'F')
		if n_elements(ra) lt 3 then message,/inf,'INVALID INPUT: 3 scalars, please'
	endWHILE
	WHILE n_elements(dec) lt 3 do BEGIN
		read,'Enter DEC of reference pixel [DEG, MIN, SEC]: ',inp
		dec = getopt(inp,'F')
		if n_elements(dec) lt 3 then message,/inf,'INVALID INPUT: 3 scalars, please'
	endWHILE
	crval = [ten(ra * 15.), ten(dec)]/!RADEG
	siz = size(crval)
endWHILE

IF (n_elements(flag) lt 1) THEN flag = "" else flag = string(flag)
WHILE flag ne "T" and flag ne "F" do BEGIN
	inp = ""
	read,"Is the plate solution (not the image) undistorted? [Y/<CR>=N]: ",inp
	inp = strupcase(strmid(inp,0,1))
	IF ((inp eq "Y") or (inp eq "T")) then flag = "T" $
	ELSE IF ((inp eq "N") or (inp eq "F") or (inp eq "")) then flag = "F" $
	ELSE BEGIN
		flag = ""
		print,string(7b),"PLEASE Enter Y, N, T, F or <CR>."
	endELSE
endWHILE
	
sze = size(ctype)
WHILE (sze(0) ne 1) or (sze(1) lt 2) or (sze(sze(0)+1) ne 7) do BEGIN
	IF (Flag eq "F") then def = "TAN" else def = "UIT"
	inp = ''
	read,string(def,f='("Enter coordinate type 1 [RA---",A3,"]: ")'), inp
	IF inp eq "" THEN ctype0 = string(def,f='("RA---",A3)') $
		ELSE ctype0 = inp
	
	inp = ''
	read,string(def,f='("Enter coordinate type 2 [DEC--",A3,"]: ")'), inp
	IF inp eq "" THEN ctype1 = string(def,f='("DEC--",A3)') $
		ELSE ctype1 = inp

	ctype = [ctype0, ctype1]
	sze = size(ctype)
endWHILE

IF n_elements(cam) lt 1 THEN cam = "" ELSE cam = string(cam)
IF ((CAM NE "A") AND (CAM NE "B")) THEN CAM = "N"

sze= size(equinox)
IF sze(sze(0)+1) eq 0 THEN equinox = 0.
IF ((equinox lt 1850.) or (equinox gt 2100.)) THEN equinox = 2000.0
cdelt = [1.0D,1.0D]         ;Flag that CDELT not used with UIT images

  ASTR = {UIT_ASTROMETRY, CD: float(cd),CDELT: float(cdelt(0:1)), $
		CRPIX: float(crpix(0:1)), $
                CRVAL:float(crval(0:1)), CTYPE: ctype(0:1), $
                FLAG: flag, CAM:cam, EQUINOX: float(equinox), $
		SCALE: float(scale), OFFSET: float(offset)}
  return,ASTR
end
