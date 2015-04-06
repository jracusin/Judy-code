; for use in exponential tick marks
;
; Taken off the web.  Correction made to pick up negative number, and
;   non-integral mantissa. trim trailing zeroes in mantissa
;
FUNCTION ExponentStr, axis, index, number

IF number EQ 0 THEN RETURN, '0' ;; Special case

ex = String(number, '(e9.0)') 
pt = StrPos(ex, '.')

first = STRTRIM( StrMid(ex, 0, pt), 2 )
sign = StrMid(ex, pt+2, 1)
exponent = StrMid(ex, pt+3, 100)

   ; make sure resulting number is same as input
for ndec = 1, 8 do begin
   tickStr = FLOAT( first ) * 10.^exponent
   IF sign EQ '-' THEN tickStr = FLOAT( first ) * 10.^(-1.0*exponent)

   if ABS((FLOAT( tickStr ) - number)) GT ABS(number/1.e6) then begin
;;;	       print,'using g-format, tickstr=', tickstr
;;;               tickStr = STRING( number, FORMAT='(G9.'+ STRTRIM(ndec,2) +')' )
      ex = String(number, '(e16.'+ STRTRIM(ndec,2) +')') 
      first = STRTRIM( StrMid(ex, 0, pt+ndec+1), 2 )

   endif else goto, valOK
endfor
valOK:

   ; trim trailing zeroes in mantissa
while strmid( first, strlen(first)-1, 1 ) eq '0' do $
     first = strmid( first, 0, strlen(first)-1 )


    ; Shave off leading zero in exponent

WHILE StrMid(exponent, 0, 1) EQ '0' DO exponent = StrMid(exponent, 1, 100)
if exponent eq '' then exponent = '0'

;;;print,'EXP: number,first,sign,exponent=', number,' ',first,' ',sign,' ',$
;;;                                        exponent
;;;stop  
IF sign EQ '-' THEN   RETURN, first + 'x10!A' + sign + exponent $
   ELSE               RETURN, first + 'x10!A' + exponent
END
