pro chile_single_slew,date,ra1,dec1,ra2,dec2

  if n_params() lt 5 then begin
     print,'syntax - chile_single_slew,date,ra1,dec1,ra2,dec2'
     print,"         note: date='YYYY-DDD-HH:MM:SS'"
     return
  endif 

  s=' | '
  met=date2met(date)
  date2=date;met2date(met);+120d)
  date3=met2date(met+300d)
  dd=strmid(date,0,4)+strmid(date,5,3)+strmid(date,9,2)+strmid(date,12,2)
  dd1=strmid(date,0,4)+strmid(date,5,3)+strmid(date,9,2)+ntostr((strmid(date,12,2)*1L+5))
  
  file='PPST_'+dd+'_'+dd1+'.txt'
  print,file

  openw,lun,file,/get_lun
  printf,lun,date+s+'PPT'+s+'End'+s+'Target1'+s+'1'+s+'0'+s+'0'+s+ntostr(ra1)+s+ntostr(dec1)+s+'0'+s+'0'+s+'0'+s+'99'+s+'0'+'Single Slew PPST target 1'+s+'targ.tcl'
  printf,lun,date2+s+'PPT'+s+'Begin'+s+'Target2'+s+'1'+s+'0'+s+'0'+s+ntostr(ra2)+s+ntostr(dec2)+s+'0'+s+'0'+s+'0'+s+'99'+s+'0'+'Single Slew PPST target 2'+s+'targ.tcl'
  printf,lun,date3+s+'PPT'+s+'End'+s+'Target2'+s+'1'+s+'0'+s+'0'+s+ntostr(ra2)+s+ntostr(dec2)+s+'0'+s+'0'+s+'0'+s+'99'+s+'0'+'Single Slew PPST target 2'+s+'targ.tcl'
  close,lun
  free_lun,lun

  chile,file

  return
end 

