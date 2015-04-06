pro lcout2qdp,file
  
if n_elements(file) eq 0 then file='lc_out.txt'
  openw,lun,'lc_out.qdp',/get_lun
;  printf,lun,'dev /xs'
;  printf,lun,'skip sing'
;  printf,lun,'read terr 1'
;  printf,lun,'READ SERR 2'
;  printf,lun,'log on'
;  printf,lun,'la fi'
;  printf,lun,'la x Time from BAT Trigger (s)'
;  printf,lun,'la y XRT Count Rate (c/s)'
  
  printf,lun,'cpd /xs'
  printf,lun,'READ SERR 1 2'
  
  readcol,file,time,tstart,tstop,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,format='(d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)'
  wt=where(curr_ftype eq 0,nwt)
  pc=where(curr_ftype eq 1 and sigma ge 3,npc)
  ul=where(sigma lt 3,nul)
  timerr=((time-tstart)+(tstop-time))/2.
  
  if nwt gt 0 then begin 
     printf,lun,'!WT data'
     for i=0,nwt-1 do printf,lun,time[wt[i]],timerr[wt[i]],cts[wt[i]],err[wt[i]]
     printf,lun,'no no'
  endif 
  if npc gt 0 then begin 
     printf,lun,'!PC data'
     for i=0,npc-1 do printf,lun,time[pc[i]],timerr[pc[i]],cts[pc[i]],err[pc[i]]
  endif 
  if nul gt 0 then begin 
     printf,lun,'!upper limit'
     printf,lun,'no no'
     for i=0,nul-1 do printf,lun,time[ul[i]],timerr[ul[i]],cts[ul[i]],0
  endif 
  
;  for i=0,n_elements(time)-1 do printf,lun,time[i],tstart[i]-time[i],tstop[i]-time[i],cts[i],err[i]
  
  close,lun
  free_lun,lun
  
  return
end 
