pro fix_lc_out,file,time,tstart,tstop,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct
  
  for i=0,n_elements(time)-1 do cts=calc_3sig_ul(src,bg,time)
  
  
  return
end 
