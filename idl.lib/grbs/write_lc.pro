pro write_lc,lc,file
  
  openw,lun,file,/get_lun
  printf,lun,'time    t_start    t_stop   src_rate   src_rate_err   tot_hard   tot_hard_err   exptime    src_counts  back_ctrate   det_sig   psf_corr   junk   junk   rate1   rate2   rate3  rate1_err   rate2_err    rate3_err   hard1   hard2   hard1_err   hard2_err src_rate   back_rate   back_area_corr   pu_corr    psf_corr  somejunk  e1_cts  e2_cts  e3_cts  somejunk somejunk somejunk tot_back_cts  hard_ratio_1  hard_ratio_2  hard_ratio_err1  hard_ratio_err2 mean_time sigma_time'
  printf,lun,'junk2'
  
  time=lc.time
;  s=sort(time)
  s=lindgen(n_elements(time))
  tstart=lc.tstart
  tstop=lc.tstop
  counts=lc.src_rate
  err=lc.src_rate_err
  srccounts=lc.src_counts
  backcts=lc.tot_back_cts
  exptime=lc.exptime
  corr=lc.pu_corr
  type=lc.type
  
  sp='    '
  for i=0,n_elements(time)-1 do begin 
     printf,lun,ntostr(time[s[i]])+sp+ntostr(tstart[s[i]])+sp+ntostr(tstop[s[i]])+sp+ntostr(counts[s[i]])+sp+ntostr(err[s[i]])+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(exptime[s[i]])+sp+ntostr(srccounts[s[i]])+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(type[s[i]])+sp+ntostr(-999.)+sp+ntostr(1.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(corr[s[i]])+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(backcts[s[i]])+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)
  endfor 
  close,lun
  free_lun,lun
  
  return
end
