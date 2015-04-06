PRO astrom, raref, decref, idgs, ximg, yimg, astr, err, fl, $
	NOQUESTIONS=NoQues, DUMPTYPE=DumpType
;+
; NAME:
;	ASTROM
; PURPOSE:
;	Computes parameters describing a linear fit to a plate    
;	solution using the algorithm supplied by R. S. HARRINGTON, USNO
;	Use the procedure ASTROMIT for interactive use of this program
;
; CALLING SEQUENCE:
;	astrom, raref, decref, idgs, ximg, yimg, astr, err, [ fl ,
;				/NOQUESTIONS, DUMPTYPE= ]
;
; INPUTS:
;	raref - Array of RA'S of astrometric reference stars (DEGREES)
;	decref- Array of DEC'S of astrometric reference stars (DEGREES)
;	idgs  - Array of indices of selected reference stars.  IDGS must
;		contain the same or fewer elements than RAREF and DECREF.
;		If the total # of reference stars is N, then to select
;		all the reference stars, set IDGS = INDGEN(N)
;	ximg  - Array of image source X COORDS.                  
;	yimg  - Array of image source Y COORDS.
;	astr  - A structure, of type ASTROMETRY, containing the initial guess
;		at the plate solution.
;
; OPTIONAL INPUTS:
;	fl    - Camera Focal Length (in pixel units).  If omitted, a focal
;		length of 1.7E5 (UIT camera) will be assumed
; KEYWORDS:
;	NOQUESTIONS - If set, user will not be asked 'Ren/Del/Cont'
;	DUMPTYPE    - If 0, printed information will NOT be dumped.  If 1,
;		printed information will be dumped to ASTROM.DMP in the
;		same format as the screen.  If 2, a higher precision and
;		sexigesimal format will be dumped to file ASTROM.DMP.
;		DumpType=2 is noticeably slower...
; OUTPUTS:
;	astr  - The input ASTROMETRY structure is filled with the just-computed
;		plate solution.
;	err   - (output) error in star location (measured - calc) in arcsec   
;		Same number of elements as IDGS
;
; REVISION HISTORY:
;	Written  by J. K. HILL, STX CORP.                        7/11/86  
;	Converted to IDL, made interactive by R. H. CORNETT, STX 5/29/87
;	03-SEP-90 Fixed Error calculation and made it dump in to file. (Deutsch)
;	Added option to delete or rename output file, J. Isensee, ST Systems Corp.
;	Added Calling sequence message, N. Collins, STX, 11/20/90
;	20-AUG-91 Fixed Error calculation by removing extra SQRT inserted in
;	line 161 probably when the ERR variable was changed to ERROUT by someone.
;	Therefore, all error values were wrong, probably since 11/20/90.  Also
;	changed variable ERROUT back to ERR because only SOME of the ERRs had
;	been changed, which gave more errors.       (E. Deutsch)
;	20-AUG-91 Added /NoQues parameter to avoid Del/Ren Prompt.  (E. Deutsch)
;	24-AUG-91 Added DumpType= keyword and corresponding code.   (E. Deutsch)
;	24-AUG-91 Changed Dumpfile Unit=8 to /get_lun               (E. Deutsch)
; 	Fixed the returned ERR array.  The square of the errors was being
;	returned; a square root is now taken prior to returning.  The displayed
;	error analysis is not affected.  M. R. Greason, Hughes STX, 
;	10 December 1992.
;	Converted to the new, image-distortion-related, astrometry scheme.
;	Reformatted the procedure and beefed up the internal documentation.
;	M. R. Greason, Hughes STX, 6 January 1993.
;	Fixed variable-name error.  RSH, HSTX, 25-Mar-93
;	New astrometry software WBL HSTX  Feb 94
;- 
on_error,2
;
;			Check parameters.
;
IF (n_params() LT 7) THEN begin
 	print, 'Syntax - astrom, raref, decref, idgs, ximg, yimg, astr, err, 
	print,'					[ fl, DUMPTYPE =, NOQUES = ]
	return
endif
;
;				Camera focal length is 2.0D4 for the
;				schwarzschild camera, 1.7D5 for UIT.
;
IF (n_params(0) LT 10) THEN fl = 1.7d5
;
NoQues = keyword_set(NoQues)
IF (n_elements(DumpType) EQ 0) THEN DumpType = 1
;
;			Define numerical constants.
;
one = 1.0d0
r90 = !Dpi / 2.d
r270 = 3.d * r90
r360 = !Dpi * 2.d
r2d = 180.0d0/!Dpi
;
;			Other initialization.
;
xycen = astr.crpix - 1.
crvalr = astr.crval/r2d
sdo = sin(crvalr(1))
cdo = cos(crvalr(1))
nmatch = n_elements(idgs)       ; Number of selected reference stars.
;
;			Extract specified target ID's .
;
ra_ref  = double(raref(idgs)) / r2d
dec_ref = double(decref(idgs)) / r2d
x_img   = double(ximg(idgs))
y_img   = double(yimg(idgs))
;
;			Adjust reference star measures & print out all data on
;			star.
;
x = x_img / fl
y = y_img / fl
;
xymean = [total(x),total(y)] / nmatch	; Mean position of reference stars.
;
;			Store coordinate differences from plate center.
;
IF (crvalr(0) GT r270) THEN BEGIN
	select = where (ra_ref LT r90, nselect)
	IF (nselect GT 0) THEN ra_ref(select) = ra_ref(select) + r360
ENDIF ELSE IF (crvalr(0) LT r90) THEN BEGIN
	select = where (ra_ref GT r270, nselect)
        IF (nselect GT 0) THEN ra_ref(select) = ra_ref(select) - r360
ENDIF
;
a = ra_ref - double(crvalr(0))
d = dec_ref
sd = sin(d) & cdx = cos(d)
sa = sin(a) &  ca = cos(a)
denom = (sd * sdo) + (cdx * cdo * ca)
;
;			New A, D are in fact XSI, ETA (radians).
;
a = cdx * sa / denom
d = (sd * cdo - cdx * sdo * ca) / denom  
admean = [total(a),total(d)] / nmatch
;
;			Compute linear plate constants.
;			All measures and standard coordinates are first
;			referred to their respective centroids, eliminating the
;			constant plate constant.  no assumptions are made
;			regarding rigid rotations or perpendicular axes.
;    
a = a - admean(0)
d = d - admean(1)
x = (xymean(0) - x) + a
y = (xymean(1) - y) + d 
;               
sxx    = total( a * a )
syy    = total( d * d )
sxy    = total( a * d )
sxmx   = total( x * a )
sxmy   = total( x * d )
symx   = total( y * a )
symy   = total( y * d )
denom1 = (sxx * syy) - (sxy * sxy)
;
;			Scale constants.
;
ax = (sxmx * syy - sxmy * sxy) / denom1
by = (symy * sxx - symx * sxy) / denom1
;
;			Rotation constants.
;
bx = (sxmy * sxx - sxmx * sxy) / denom1
ay = (symx * syy - symy * sxy) / denom1
denom2 = one - ax - by + (ax * by) - (ay * bx)
;
;			Compute fit sigmas (radians).
;			We're not happy with the values this part gets!
;
IF (nmatch EQ 3) THEN BEGIN
	rmsa = 0.0d0
	rmsd = 0.0d0
ENDIF ELSE BEGIN
	rmsa =total ((x - ax * a - bx * d) ^ 2 )
	rmsd =total ((y - by * d - ay * a) ^ 2 )
	rmsa = sqrt(rmsa / (nmatch-3.d0))
	rmsd = sqrt(rmsd / (nmatch-3.d0)) 
ENDELSE
;
;			Finish computing the plate solution.
;
;				The CD matrix.
;
cd = replicate(0.d, 2, 2) + [one-by, ay, bx, one-ax] / (denom2 * fl)
astr.cd = r2d * cd
;
;				The CRPIX array.
;
xymean = xymean * fl
v = -admean + cd # xymean
det = cd(0,0) * cd(1,1) - cd(1,0) * cd(0,1)	; Determinant of CD.
astr.crpix(0) = (v(0) * cd(1,1) - v(1) * cd(0,1)) / det + 1.
astr.crpix(1) = (v(1) * cd(0,0) - v(0) * cd(1,0)) / det + 1.
;
;				The CRVAL array.  Use XY2AD to convert
;				the plate center to (RA,DEC).
;
if strmid(astr.ctype(0),5,3) EQ 'UIT' then $
UIT_xy2ad, xycen(0), xycen(1), astr, ac, dc else $
    xy2ad, xycen(0), xycen(1), astr, ac, dc
astr.crval = [ac, dc]
astr.crpix = xycen + 1.		; Restore original center position
;
;			Compute and display the errors of the fit.
;
;				Start with the display titles.
;
IF (DumpType GT 0) THEN openw, dmp, 'astrom.dmp', /GET_LUN
IF (DumpType EQ 1) THEN BEGIN
	printf, dmp, '                         STAR POSITIONS and ERRORS'
	printf, dmp, '  I    X      Y       RA (calc) Dec      RA (cat)  Dec     d(RA) d(Dec) d(tot)'
ENDIF
IF (DumpType EQ 2) THEN BEGIN
	printf, dmp, '[',!stime,']                    Star Positions and Errors'
	printf, dmp, 'Idx       X       Y            RA   (calc)   Dec             RA   (cat)    Dec         d(RA)   d(Dec)    d(tot)'
	printf, dmp, '---    ------- -------    --------------------------    --------------------------    ------- -------   --------'
ENDIF
;
print, ' '
print, '                         STAR POSITIONS and ERRORS'
print, '  I    X      Y       RA (calc) Dec      RA (cat)  Dec     d(RA) d(Dec) d(tot)'
;
;				Use XY2AD to compute star positions.
;
if strmid(astr.ctype(0),5,3) EQ 'UIT' then $
 UIT_xy2ad, x_img, y_img, astr, asdeg, dsdeg else $
     xy2ad, x_img, y_img, astr, asdeg, dsdeg 
 as = asdeg / r2d
 ds = dsdeg / r2d
;
;				Compute the residuals, in arcseconds.
;
era = (as - ra_ref) * cos(dec_ref) * (r2d * 3600.)
erd = (ds - dec_ref) * (r2d * 3600.)
err = (era ^ 2) + (erd ^ 2)
;
;				Display the info relating to each star.
;
ra_ref = ra_ref*r2d    &   dec_ref = dec_ref*r2d
fmt = '(I3,1X,2F7.1,1X,2F9.4,1X,2F9.4,1X,3F7.2)'
fmth = '(I3,1X,2F7.1,1X,2F9.4,1X,2F9.4,1X,3F7.2)'
FOR k = 0, nmatch-1 DO BEGIN
	print, FORM=fmt, idgs(k), x_img(k), y_img(k), asdeg(k), dsdeg(k), $
		ra_ref(k), dec_ref(k), era(k), erd(k), sqrt(err(k))
	IF (DumpType EQ 1) THEN printf, dmp, FORM=fmt, idgs(k), x_img(k), $
		y_img(k), asdeg(k), dsdeg(k), ra_ref(k), dec_ref(k), $
		era(k), erd(k), sqrt(err(k))
	IF (DumpType EQ 2) THEN printf, dmp, form=fmth, idgs(k), x_img(k), $
		y_img(k), adstring(asdeg(k), dsdeg(k), 2), $
		adstring(ra_ref(k), dec_ref(k), 2), era(k), erd(k), $
		sqrt(err(k))
ENDFOR
;
;				Display the summary info.
;
print, 'Using n=' + strn(nmatch) + ' stars:'
print, FORM='(A29,T58,3F7.2)', $
	'RMS positional error(arcsec):', sqrt(total(era ^ 2) / nmatch), $
	sqrt(total(erd ^ 2) / nmatch), sqrt(total(err) / nmatch)
print, FORM='(A11,F6.3,3X,A19,F6.3,3X,A21,F6.3)', 'RMS Error: ', $
	sqrt(total(err)/nmatch), 'RMS Error/sqrt(n): ', $
	sqrt(total(err)/nmatch) / sqrt(nmatch), 'RMS Error/sqrt(n-3): ', $
	sqrt(total(err)/nmatch) / sqrt((nmatch - 3) > 1)
;
IF (DumpType EQ 1) THEN BEGIN
	printf, dmp, FORM='(A29,T58,3F7.2)', $
		'RMS positional error(arcsec):', sqrt(total(era^2)/nmatch), $
		sqrt(total(erd^2)/nmatch), sqrt(total(err)/nmatch)
	printf, dmp, FORM='(A11,F6.3,3X,A19,F6.3,3X,A21,F6.3)','RMS Error: ', $
		sqrt(total(err)/nmatch),'RMS Error/sqrt(n): ', $
		sqrt(total(err)/nmatch)/sqrt(nmatch),'RMS Error/sqrt(n-3): ', $
		sqrt(total(err)/nmatch)/sqrt((nmatch-3)>1)
ENDIF
;
IF (DumpType EQ 2) THEN BEGIN
	printf, dmp, FORM='(A20,T53,A29,T86,2F8.3,1X,F10.4)', $
		'Using n='+strn(nmatch)+' Stars       ', $
		'RMS positional error(arcsec):',sqrt(total(era^2)/nmatch), $
		sqrt(total(erd^2)/nmatch),sqrt(total(err)/nmatch)
	printf, dmp, FORM='(A11,F8.5,3X,A19,F8.5,3X,A21,F8.5)','RMS Error: ', $
		sqrt(total(err)/nmatch),'RMS Error/sqrt(n): ', $
		sqrt(total(err)/nmatch)/sqrt(nmatch),'RMS Error/sqrt(n-3): ', $
		sqrt(total(err)/nmatch)/sqrt((nmatch-3)>1)
ENDIF
IF (DumpType GT 0) THEN free_lun, dmp	; Close the file.
;
;			The error should be returned in arcseconds, 
;			not arcseconds^2.
;
err = sqrt(err)
;
;			Give the user a chance to manipulate the output
;			summary file.
;
IF ((NoQues EQ 0) AND (DumpType GT 0)) THEN BEGIN
;
;				What does the user want to do with the file?
;
	print, ' '
	print, '____________________________________________________________'
	print, ' '
	print, 'Above results have been printed to file ASTROM.DMP'
	print, ' '
	answ = ' '
	read, 'Type D to DELETE, R to RENAME, Return to KEEP AS IS:  ', answ
	answ = strupcase(strmid(strtrim(answ, 2), 0, 1))
	IF (answ EQ 'D') THEN BEGIN
;
;				Delete it?
;
		IF (!version.os EQ "vms") THEN spawn, 'delete astrom.dmp;*' $
		                          ELSE spawn, 'rm astrom.dmp'
;
	ENDIF ELSE IF (answ EQ 'R') THEN BEGIN
;
;				Rename it?
;
		print, ' '
		newname = ''
		read, 'Enter new file name: ', newname
		print,' '
		IF !version.os EQ "vms" THEN cmnd = 'rename ' $
		                        ELSE cmnd = 'mv '
		spawn, cmnd + 'astrom.dmp ' + newname
;
	ENDIF
ENDIF
;
RETURN
END
