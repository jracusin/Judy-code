pro mjd2doy,mjd,yr,doy,hr,mn,sec,silent=silent
  
  if n_params() eq 0 then begin
     print,'syntax - mjd2doy,mjd,year,doy,[/silent]'
     return
  endif 
  
  
  jd=mjd+2400000.5D 
  
  daycnv,jd,yr,m,day,hr
  
  doy = ymd2dn(yr,m,day)
  
  mn=(hr mod 1.)*60.
  sec=(mn mod 1.)*60.
  hr=round(hr-mn/60.)
  mn=round(mn-sec/60.)
  
  if not keyword_set(silent) then print,yr,doy,hr,mn,sec
  
  return
end 
