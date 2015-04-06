FUNCTION FITSTime, theTime
; Converts GLAST MET to the FITS timestamp format

	caldat,theTime/86400.0d + 2451910.5d,pmo,pday,pyr,phr,pmin,psec
	year = STRTRIM(STRING(FIX(pyr)),2)
	month = (pmo LT 10) ? '0' + STRTRIM(STRING(FIX(pmo)),2) : STRTRIM(STRING(FIX(pmo)),2)
	day = (pday LT 10) ? '0' + STRTRIM(STRING(FIX(pday)),2) : STRTRIM(STRING(FIX(pday)),2)
	hour = (phr LT 10) ? '0' + STRTRIM(STRING(FIX(phr)),2) : STRTRIM(STRING(FIX(phr)),2)
	minute = (pmin LT 10) ? '0' + STRTRIM(STRING(FIX(pmin)),2) : STRTRIM(STRING(FIX(pmin)),2)
	second = (psec LT 10) ? '0' + STRTRIM(STRING(FIX(psec)),2) : STRTRIM(STRING(FIX(psec)),2)
	date_str = year + '-' + month + '-' + day + 'T' + hour + ':' + minute + ':' + second 
	RETURN, date_str

END
