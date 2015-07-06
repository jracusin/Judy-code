function met2date_judy,met,fermi=fermi
  
  if n_params() eq 0 then begin
     print,'syntax - calendar time = met2date(met)'
     print,'            MET MUST BE PASSED AS A DOUBLE (i.e. 123884485.3726D)'
     return,0
  endif 
  
;  if met gt 252460801d then met=met-1
;  if met gt 157766400d then met=met-1
  if keyword_set(fermi) then begin 
     if met gt 457315203d then met=met-1.
     if met gt 362707202d then met=met-1.
     if met gt 252460801d then met=met-1.
     if met gt 157766400d then met=met-1.
  endif 

  tmp=met/60D                     ;minutes
  tmp2=tmp/60D                  ;hours
  tmp3=tmp2/24D                 ;days
  jdcnv,2001,1,1,0,jd1
  xjd=jd1+tmp3
  daycnv,xjd,yr,mn,day,hr
  hour=fix(hr)
  mint=(hr - fix(hr))*60D
  sec=(mint-fix(mint))*60D
  minute=fix(mint)
  dy = ymd2dn(yr,mn,day)
  
  return,[yr,dy,hour,minute,sec]
end 
