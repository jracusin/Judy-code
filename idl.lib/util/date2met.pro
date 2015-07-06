function date2met,date,fermi=fermi
;
; Rev:
;   07/28/06 by JLR: created ability to handle month-day or doy as input
;	12/10/04 by DNB: made more general by parsing date with any 
;		of the following separators: "-", ":", "/".  This version
;		will work with several input formats, such as
;			2004-347-23:22:00	(PPST format)
;			2004/347:23:22:00	(ATF format)
;	01/21/04 by DNB: Fixed error message if no parameters are given so
;		it includes the required quotes.
; 
  
  if n_params() eq 0 then begin
     print,"met = date2met('yyyy-ddd-hh:mm:ss.sss')"
     print,"           or ('yyyy-mm-dd-hh:mm:ss.sss')"
     print,"           or ('yyyy-mm-ddThh:mm:ss.sss')"
     return,0
  endif
  
  chunks=strsplit(date,'-:/T ',/extract)
  if n_elements(chunks) eq 5 then begin 
     year=chunks[0]
     day=chunks[1]
     hour=chunks[2]
     minute=chunks[3]
     second=chunks[4]
  
     dayfrac=hour/(24d)+minute/60./(24d)+second/60./60./(24d)
  
     ydn2md,year,day+dayfrac,month,dday
  endif else begin 

     year=chunks[0]
     month=chunks[1]
     day=chunks[2]
     hour=chunks[3]
     minute=chunks[4]
     second=chunks[5]
     dayfrac=hour/(24d)+minute/60./(24d)+second/60./60./(24d)
     dday=day+dayfrac
  endelse 
     
  juldate,[year,month,dday],jd
  juldate,[2001,1,1],jd0
  
  met=(jd-jd0)*24.*60.*60D
  ;;; leap seconds!
  if keyword_set(fermi) then begin 
     if year gt 2005 then met=met+1.
     if year gt 2008 then met=met+1.
     if year gt 2012.5 then met=met+1.
     if year gt 2015.5 then met=met+1.
  endif 
  return,met
end 
