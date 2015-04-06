function met2date, met, utc_factor,fermi=fermi

; This function converts Swift spacecraft time (double precision seconds) 
;	into a date (string) in the format yyyy-mm-dd:hh:mm:ss.sss

if (n_params() lt 2) then utc_factor = 0.0D

if keyword_set(fermi) then begin 
   if met gt 457401600d then met=met-1.
   if met gt 362793600d then met=met-1.
   if met gt 252460801d then met=met-1.
   if met gt 157766400d then met=met-1.
endif 

met2date = itos_time(met, utc_factor)

return, met2date
end
