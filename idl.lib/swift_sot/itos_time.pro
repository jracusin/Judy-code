function itos_time, sc_time, utc_factor 

; This function converts Swift spacecraft time (double precision seconds) 
;	into ITOS time (string)
;
;  Rev:
;	12/10/04 by DNB: finally added utc_factor correction
;

if (n_params() lt 2) then utc_factor = 0.0D

;
; Convert time to GMT
;	First, get the Julian date for Jan 1, 2001, which is when the system clock starts
;
	
jd_of_epoch = julday(1,1,2001,0,0,0)

;
; Now add the spacecraft time to the starting epoch
;
	
jd_of_data = jd_of_epoch + (sc_time + utc_factor)/(3600.*24)

;
; Convert the Julian date of the sc_time to a calendar date
;

caldat, jd_of_data, month, day, year, hour, minute, seconds 
w=where(abs(seconds-round(seconds)) lt 0.01,nw)
if nw gt 0 then seconds[w]=round(seconds[w])

doy = ymd2dn(year, month, day)

itos_time = strtrim(string(year),2) + '-' + string(doy,format='(i3.3)') $
		+ '-' + string(hour,format='(i2.2)') + ':' $
		+ string(minute,format='(i2.2)') + ':' $
		+ strtrim(string(seconds),2)	

return, itos_time
end
