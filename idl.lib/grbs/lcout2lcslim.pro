pro lcout2lcslim,file=file,outfile=outfile
  
  if n_elements(file) eq 0 then file='lc_out.txt'
  if n_elements(outfile) eq 0 then outfile='lc_slim.txt'
  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  timerr=((time-tstarted)+(tstoped-time))/2.
  curr_ftype=fix(curr_ftype)
  writecol,'lc_slim.txt',time,timerr,cts,err,curr_ftype,header='time     timerr   cts     err  type'
  
  return
end 
