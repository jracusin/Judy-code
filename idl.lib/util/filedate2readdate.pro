function filedate2readdate,filedate
  
  if n_params() eq 0 then begin
     print,"syntax - date=filedate2readdate('20042001030')"
     print,"         resulting date='Jul-18-2004'"
     return,0
  endif
  
  year=strmid(filedate,0,4)
  day=strmid(filedate,4,3)
  time=strmid(filedate,7,4)
  
  ydn2md,year,day,m,d
  
  if strlen(strtrim(d,2)) eq 1 then d='0'+ntostr(d) else d=ntostr(d)
  
  month=month_cnv(m,/short)
  
  date=month+'-'+d+'-'+year
  
  return,date
end 
