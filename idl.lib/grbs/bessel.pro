Function bessel, x, ni, eps, integral = int

;+
; NAME:
;	BESELK
; VERSION:
;	3.0
; PURPOSE:
;	Calculates an approximation to Bessel K functions or their integrals.
; CATEGORY:
;	Mathemetical Function (General).
; CALLING SEQUENCE:
;	Result = BESELK (X, NI [, /INTEGRAL ])
; INPUTS:
;    X
;	Numerical, otherwise arbitrary.
;    NI
;	Scalar, the order of the function.
; OPTIONAL INPUT PARAMETERS:
;    EPS
;	Allowed relative error.  Default is machine precision.
; KEYWORD PARAMETERS:
;    /INTEGRAL
;	Switch, if set the integral of the K function from X to infinity is
;	calculated.
; OUTPUTS:
;	Returns the value(s) of K_ni(X) or, if INTEGRAL is set, of the integral
;	of K_ni(t) from X to infinity.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses the Kostroun approximation, see NIM 172, 371-374 (1980).
;	Calls DEFAULT, FPU_FIX, ISNUM and TOLER from MIDL.
; MODIFICATION HISTORY:
;	Created 1-MARCH-1993 by Mati Meron.
;	Modified 15-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    teps = Toler(x)
    wips = 1./(Default(eps,teps,/dtype) > teps)
    intf = keyword_set(int)
    if Isnum(x,/double) then wpi = !dpi else wpi = !pi

    h = 2*wpi/sqrt(x^2 + (ni + 2/wpi*alog(wips))^2)
    res = FPU_fix(0.5*exp(-x))
    nozer = where(res gt 0, noc)
    if noc gt 0 then res(nozer) = res(nozer)*exp(x(nozer))
    r = 0
    while noc gt 0 do begin
	r = r + 1
	rh = r*h(nozer)
	term = cosh(ni*rh)*exp((1.-cosh(rh))*x(nozer))
	if intf then term = term/cosh(rh)
	res(nozer) = res(nozer) + term
	pozer = where(wips*term ge res(nozer), noc)
	if noc gt 0 then nozer = nozer(pozer)
    endwhile
    
    return, FPU_fix(h*exp(-x)*res)
end
