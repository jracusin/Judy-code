function readdate2filedate,readdate
  
  if n_params() eq 0 then begin
     print,"syntax - date=readdate2filedate('Jul-18-2004')"
     print,"         resulting date='2004001'"
     return,0
  endif
  
  year=strmid(readdate,7,4)
  day=strmid(readdate,4,2)
  mn=strmid(readdate,0,3)
  m=month_cnv(mn)
  
  dy = ymd2dn(year,m,day)
  
  if strlen(strtrim(dy,2)) eq 2 then dy='0'+ntostr(dy) else begin
     if strlen(strtrim(dy,2)) eq 1 then dy='00'+ntostr(dy) else d=ntostr(dy)
  endelse 
  
  date=ntostr(year)+ntostr(dy)
  
  return,date
end 
