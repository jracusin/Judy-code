pro write_flareout_file,str,file,latespec=latespec;,fnum=fnum
  
;  file=''
;  read,prompt='Flare Out output file name (hit return for default)? ',file
;  if file eq '' then file='flare'+ntostr(fnum)+'/flare.out'
  print,'Writing out '+file
  
  openw,lun,file,/get_lun
  s=' '
  printf,lun,'TSTART '+ntostr(str.tstart)
  printf,lun,'TSTOP '+ntostr(str.tstop)
  printf,lun,'EXP_FACT '+ntostr(str.exp_fact)
  printf,lun,'PUFACT '+ntostr(str.pufact)
  printf,lun,'UL_TSTARTS '+ntostrarr(str.ul_tstarts)
  printf,lun,'UL_TSTOPS '+ntostrarr(str.ul_tstops)
  printf,lun,'UL_NORM '+ntostrarr(str.ul_norm)
  printf,lun,'UL_POW '+ntostrarr(str.ul_pow)
  printf,lun,'UL_CHISQ/DOF '+ntostr(str.ul_chisqdof)
  printf,lun,'UL_DOF '+ntostr(str.ul_dof)
  printf,lun,'RISE_NORM '+ntostrarr(str.rise_norm)
  printf,lun,'RISE_POW '+ntostrarr(str.rise_pow)
  printf,lun,'RISE_CHISQ/DOF '+ntostr(str.rise_chisqdof)
  printf,lun,'RISE_DOF '+ntostr(str.rise_dof)
  printf,lun,'DECAY_NORM '+ntostrarr(str.decay_norm)
  printf,lun,'DECAY_POW '+ntostrarr(str.decay_pow)
  printf,lun,'DECAY_CHISQ/DOF '+ntostr(str.decay_chisqdof)
  printf,lun,'DECAY_DOF '+ntostr(str.decay_dof)
  printf,lun,'RESET_FLAG '+ntostrarr(str.reset_flag)
  printf,lun,'INTERCEPT_TPEAK '+ntostr(str.intercept_tpeak)
  printf,lun,'INTERCEPT_PEAK '+ntostr(str.intercept_peak)
  printf,lun,'INTERCEPT_INCREASE '+ntostr(str.intercept_increase)
  printf,lun,'MAX_TPEAK '+ntostr(str.max_tpeak)
  printf,lun,'MAX_PEAK '+ntostr(str.max_peak)
  printf,lun,'MAX_INCREASE '+ntostr(str.max_increase)
  printf,lun,'SIGNIF '+ntostr(str.signif)
  printf,lun,'ULRATIO '+ntostr(str.ulratio)  
  if keyword_set(latespec) then begin
     printf,lun,'UL_MEAN_TIME '+ntostr(str.ul_mean_time)
     printf,lun,'UL_NORM2 '+ntostrarr(str.ul_norm2)
     printf,lun,'UL_POW2 '+ntostrarr(str.ul_pow2)
     printf,lun,'UL_CHISQ2/DOF2 '+ntostr(str.ul_chisqdof2)
     printf,lun,'UL_DOF2 '+ntostr(str.ul_dof2)
  endif 
  close,lun,/file
  free_lun,lun
  
  return
end 

pro read_flareout_file,file,flstr,latespec=latespec


  openr,lun,file,/get_lun
  par=''
  value=0d
  err=''
  tstarts='' & tstops=''
  while not eof(lun) do begin
     line=readline(lun,delim=' ')
     w=where(line eq '',nw)
     if nw gt 0 then begin
        nnw=where(line ne '')
        line=line[nnw]
     endif 
     par=[par,line[0]]
     value=[value,line[1]]
     if n_elements(line) eq 3 and line[0] ne 'UL_TSTARTS' and line[0] ne 'UL_TSTOPS' then err=[err,line[2]] else err=[err,'']
     if line[0] eq 'UL_TSTARTS' then tstarts=line[1:*]
     if line[0] eq 'UL_TSTOPS' then tstops=line[1:*]
     
  endwhile
  
  close,lun,/file
  free_lun,lun
  n=n_elements(tstarts)
  make_flareout_struct,flstr,n
  
  par=par[1:*]
  value=value[1:*]
  err=err[1:*]
  
  flstr.tstart=value[0]
  flstr.tstop=value[1]
  flstr.exp_fact=value[2]
  flstr.pufact=value[3]
  flstr.ul_tstarts=tstarts
  flstr.ul_tstops=tstops
  flstr.ul_norm=[value[6],err[6]]
  flstr.ul_pow=[value[7],err[7]]
  flstr.ul_chisqdof=value[8]
  flstr.ul_dof=value[9]
  flstr.rise_norm=[value[10],err[10]]
  flstr.rise_pow=[value[11],err[11]]
  flstr.rise_chisqdof=value[12]
  flstr.rise_dof=value[13]
  flstr.decay_norm=[value[14],err[14]]
  flstr.decay_pow=[value[15],err[15]]
  flstr.decay_chisqdof=value[16]
  flstr.decay_dof=value[17]
  flstr.reset_flag=[value[18],err[18]]
  flstr.intercept_tpeak=value[19]
  flstr.intercept_peak=value[20]
  flstr.intercept_increase=value[21]
  flstr.max_tpeak=value[22]
  flstr.max_peak=value[23]
  flstr.max_increase=value[24]
  flstr.signif=value[25]
  flstr.ulratio=value[26]
  if n_elements(value) gt 27 then begin 
     flstr.ul_mean_time=value[27]
     flstr.ul_norm2=[value[28],err[28]]
     flstr.ul_pow2=[value[29],err[29]]
     flstr.ul_chisqdof2=value[30]
     flstr.ul_dof2=value[31]
     latespec=1
  endif
  
  
  return
end 
pro make_flareout_struct,struct,n
  
  ;deal with errors & many gtis
  derr=dblarr(2)
  if n eq 1 then dn=0d else dn=dblarr(n)
  struct=create_struct('TSTART',0d,$
                       'TSTOP',0d,$
                       'EXP_FACT',0d,$
                       'PUFACT',0d,$
                       'UL_TSTARTS',dn,$
                       'UL_TSTOPS',dn,$
                       'UL_NORM',derr,$
                       'UL_POW',derr,$
                       'UL_CHISQDOF',0d,$
                       'UL_DOF',0L,$
                       'RISE_NORM',derr,$
                       'RISE_POW',derr,$
                       'RISE_CHISQDOF',0d,$
                       'RISE_DOF',0L,$
                       'DECAY_NORM',derr,$
                       'DECAY_POW',derr,$
                       'DECAY_CHISQDOF',0d,$
                       'DECAY_DOF',0L,$
                       'RESET_FLAG',intarr(2),$
                       'INTERCEPT_TPEAK',0d,$
                       'INTERCEPT_PEAK',0d,$
                       'INTERCEPT_INCREASE',0d,$
                       'MAX_TPEAK',0d,$
                       'MAX_PEAK',0d,$
                       'MAX_INCREASE',0d,$
                       'SIGNIF',0d,$
                       'ULRATIO',0d,$
                       'UL_MEAN_TIME',0d,$
                       'UL_NORM2',derr,$
                       'UL_POW2',derr,$
                       'UL_CHISQDOF2',0d,$
                       'UL_DOF2',0L)
  return
end 


pro fix_fit_flares_bug,file
  
  if numlines(file) eq 32 then latespec=1 else latespec=0
  
  print,'reading in '+file
  read_flareout_file,file,str,latespec=latespec
  
  print,'Swapping wrong errors'
  rise_powerr=str.decay_norm[1]
  decay_normerr=str.rise_pow[1]
  
  str.decay_norm[1]=decay_normerr
  str.rise_pow[1]=rise_powerr
  
  print,'Re-writing '+file
  
  write_flareout_file,str,file,latespec=latespec
  print
  
  return
end 
