pro convert_phil2lcout,file,qdp=qdp,outfile=outfile,lc=lc
  
  if not keyword_set(qdp) then phil=1
  if n_elements(lc) eq 0 then lc=lcout2fits(file,phil=phil,qdp=qdp)
  s=sort(lc.time)
  
  if n_elements(outfile) eq 0 then outfile='lc_newout_phil.txt'
  openw,lun,outfile,/get_lun
  
  sp='          '
  printf,lun,'time    t_start    t_stop   src_rate   src_rate_err   tot_hard   tot_hard_err   exptime    src_counts  back_ctrate   det_sig   psf_corr   junk   junk   rate1   rate2   rate3  rate1_err   rate2_err    rate3_err   hard1   hard2   hard1_err   hard2_err src_rate   back_rate   back_area_corr   pu_corr    psf_corr  somejunk  e1_cts  e2_cts  e3_cts  somejunk somejunk somejunk tot_back_cts  hard_ratio_1  hard_ratio_2  hard_ratio_err1  hard_ratio_err2 mean_time sigma_time'
  printf,lun,'junk2'
  for i=0,n_elements(lc)-1 do begin 
     printf,lun,ntostr(lc[s[i]].time)+sp+ntostr(lc[s[i]].tstart)+sp+ntostr(lc[s[i]].tstop)+sp+ntostr(lc[s[i]].src_rate)+sp+ntostr(lc[s[i]].src_rate_err)+sp+ntostr(1.)+sp+ntostr(.1)+sp+ntostr(lc[s[i]].exptime)+sp+ntostr(lc[s[i]].src_counts)+sp+ntostr(0)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(-999.)+sp+ntostr(lc[s[i]].type)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(lc[s[i]].pu_corr)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(lc[s[i]].tot_back_cts)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)
     
  endfor 
  
  close,lun
  free_lun,lun
  
  return
end 
