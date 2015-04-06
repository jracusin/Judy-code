Function FPU_fix, x, no_abs = nab

;+
; NAME:
;	FPU_FIX
; VERSION:
;	3.0
; PURPOSE:
;	Clears Floating Point Underflow errors, setting the offending values to 
;	zero.
; CATEGORY:
;	Programming.
; CALLING SEQUENCE:
;	Result = FPU_FIX( X)
; INPUTS:
;    X
;	Arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /NO_ABS
;	Switch.  If set, uses value instead of absolute value for comparison 
;	with machine minimum.  For internal use only.
; OUTPUTS:
;	If the input is of any numeric type, returns the input, with the 
;	possible substitution of 0 for all occurences of Floating Point 
;	Underflow.  A non-numeric input is returned as is.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses the system routines CHECK_MATH and MACHAR.  Also 
;	calls ISNUM and M_ABS from MIDL.
; MODIFICATION HISTORY:
;	Created 30-AUG-1998 by Mati Meron.
;-

    on_error, 1
    fpucod = 32
    matherrs = ['Integer divided by zero','Integer overflow','','',$
		'Floating-point divide by zero','Floating-point underflow',$
		'Floating-point overflow','Floating-point operand error']

    chem = check_math()
    if Isnum(x,type = typ) and chem gt 0 then begin
	if chem eq fpucod then begin
	    sinf = machar(double = Isnum(x,/double))
	    if keyword_set(nab) then dum = where(x lt sinf.xmin, nuf) $
	    else dum = where(M_abs(x) lt sinf.xmin, nuf)
	    if nuf gt 0 then x(dum) = 0
	endif else message, matherrs(round(alog(chem)/alog(2)))
    endif

    return, x
end
