FUNCTION DECIMALS, inval, dp

; inval is the input number
; dp is the decimals places to be rounded to

inval=STRUPCASE(STRCOMPRESS(STRING(inval), /REMOVE_ALL))

; If input is in scientific notation, strip off the
; exponential part and save it for later
IF STRPOS(inval, 'E') NE -1 THEN BEGIN
  sciterm=STRMID(inval, STRPOS(inval, 'E'))
  inval=STRMID(inval, 0, STRPOS(inval, 'E')-1)
ENDIF

; Define the integer portion of the number
intval=STRMID(inval, 0, STRPOS(inval, '.')+1)

; Define the values after the decimal to be rounded to
rdp=STRMID(inval, STRPOS(inval, '.')+1, dp)

; Define the next value after the number of decimal places you are rounding to
ndp=STRCOMPRESS(STRMID(inval, STRPOS(inval, '.')+dp+1, 1), /REMOVE_ALL)
ndp=FIX(ndp)


IF (ndp GE 5) THEN BEGIN

    updec=STRMID(rdp, 0, 1, /REVERSE_OFFSET)
    
    ; If the last decimal place value is less than 9, increase by one 
    IF LONG(updec) NE 9 THEN BEGIN
      rdp=STRMID(rdp, 0, STRLEN(rdp)-1)
      updec=LONG(updec)+1
      rdp=rdp+STRCOMPRESS(STRING(updec), /REMOVE_ALL)
      
      ; If last decimal place =9 then set digit to 0 and increase
      ; next digit forward
    ENDIF ELSE BEGIN
      rdp=STRMID(rdp, 0, STRLEN(rdp)-1)
      rdp=STRCOMPRESS(STRING(LONG(rdp)+1), /REMOVE_ALL)
      rdp=rdp+'0'
      
    ENDELSE
    
ENDIF

; Used n the case that rounding will increase the value of the 
; digit on the integer side of the number
IF STRLEN(rdp) GT dp THEN BEGIN
  intval=STRCOMPRESS(STRING(LONG(intval)+1), /REMOVE_ALL)
  intval=intval+'.'
  rdp=STRMID(rdp, 1)
ENDIF

; Format and return rounded number

IF N_ELEMENTS(sciterm) NE 0 THEN outval=STRCOMPRESS(intval+rdp+sciterm, /REMOVE_ALL) ELSE $
outval=STRCOMPRESS(intval+rdp, /REMOVE_ALL)

RETURN, outval

END