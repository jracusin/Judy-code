pro fit_grb051117a
  
  
  
  readcol,'lc_out.txt',time,tstart,tstop,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct
  timerr=((time-tstart)+(tstop-time))/2.
  
  w=where(time gt 220. and time lt 300. or time gt 740. and time lt 835. or time gt 1238. and time lt 1270. or time gt 5180 and time lt 7400.)
  
  fit_flares,time,timerr,cts,err,100,10000,norm1,pow1,norm2,pow2,sigma1,/fit,w=w
  k=get_kbrd(10)
  xrange=[100,10000]
  yrange=[1,1e3]
  print,'Flare 1 start'
  fit_flares,time,timerr,cts,err,100,140,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt1,xrange=xrange,w=w
  k=get_kbrd(10)
  print,'Flare 1 stop'
  fit_flares,time,timerr,cts,err,170,220,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt2,xrange=xrange,w=w ;[600,800]
  q=where(time gt icpt1 and time lt icpt2)
  print,max(cts[q],p),time[q[p]]
  oplot,[time[q[p]],time[q[p]]],yrange
  k=get_kbrd(10)
  
  print,'Flare 2 start'
  fit_flares,time,timerr,cts,err,300,330,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt1,xrange=xrange,w=w
  k=get_kbrd(10)
  print,'Flare 2 stop'
  fit_flares,time,timerr,cts,err,660,750,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt2,xrange=xrange,w=w;[600,800]
  q=where(time gt icpt1 and time lt icpt2)
  print,max(cts[q],p),time[q[p]]
  oplot,[time[q[p]],time[q[p]]],yrange
  k=get_kbrd(10)
  
  print,'Flare 3 start'
  fit_flares,time,timerr,cts,err,820,900,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt1,xrange=xrange,w=w;[800,1000]
  k=get_kbrd(10)
  print,'Flare 3 stop'
  fit_flares,time,timerr,cts,err,1110,1250,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt2,xrange=xrange,w=w ;[1000,1300]
  q=where(time gt icpt1 and time lt icpt2)
  print,max(cts[q],p),time[q[p]]
  oplot,[time[q[p]],time[q[p]]],yrange
  k=get_kbrd(10)
  
  print,'Flare 4 start'
  fit_flares,time,timerr,cts,err,1240,1340,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt1,xrange=xrange,w=w;[1100,1400]
  k=get_kbrd(10)
  print,'Flare 4 stop'
  fit_flares,time,timerr,cts,err,1540,1800,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt2,xrange=xrange,w=w;[1500,1900]
  q=where(time gt icpt1 and time lt icpt2)
  print,max(cts[q],p),time[q[p]]
  oplot,[time[q[p]],time[q[p]]],yrange
  return
end 
