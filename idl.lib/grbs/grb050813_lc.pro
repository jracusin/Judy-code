pro grb050813_lc
  
  goto,skipy
;  .com /home/dmorris/Idl/plot_lc_prelease11
  files=['sw00150139000xpcw4po_cl.evt','sw00150139001xpcw4po_cl.evt','sw00150139003xpcw4po_cl.evt','sw00150139004xpcw4po_cl.evt']
  plot_lc,[1,1,1,1],files,241.98837,11.248753,20,242.00641,11.357903,40,[10,10,10,10],145608309.d,4,2.08,1
  ;[55,45,30,30,30]
  skipy:
  readcol,'lc_out.txt',time,tstart,tstop,rate,err,hard,harderr,exptime,scts,bcts,skip=2,format='(d,d,d,d,d,d,d)'
  !p.multi=0
  erase
  tbin=tstop-tstart
;  skipd:
  
  if keyword_set(out) then begin
     begplot,name='grb050813_lc.ps',/land,/color
     color=!black
  endif else color=!white
  
  btime=date2met('2005-225-06:45:09.44')
;  ul=where(rate-err lt 1e-5,nul)
;  g=where(rate-err gt 1e-5)
  ul=where(scts-bcts lt 3.*sqrt(bcts+scts),nul)
  g=where(scts-bcts gt 3.*sqrt(bcts+scts),ng)
  
  ploterror,time[g],rate[g],err[g],psym=3,/nohat,/xlog,/ylog,xtitle='T-T0 (s)',ytitle='XRT count rate (cts s!U-1!N)',title='GRB050813',xrange=[1e-2,1e6],xstyle=1,yrange=[1e-4,1]
  for i=0,n_elements(time[g])-1 do $
     oplot,[tstart[g[i]],tstop[g[i]]],[rate[g[i]],rate[g[i]]]
;  t=[1,time,1e8]
;  a=[100,-1]
  
  if n_elements(a) eq 0 then a=[1,1,3e5,4]
  
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 4)
  parinfo.value=a
  parinfo[2].limits=[100,1e8]
  parinfo[1].limits=[0,10]
  parinfo[3].limits=[0,10]
;  parinfo[2].step=1000
  parinfo[3].step=0.01
  
  error=err
  if nul gt 0 then error[ul]=error[ul]*3.
  weights=1/error^2
  fit=mpcurvefit(time,rate,weights,a,sig,function_name='bpow2',/noder,chisq=chisq,itmax=500,nfree=4,xtol=1e-20,parinfo=parinfo)
;  fit=curvefit(time,rate,weights,a,sig,function_name='bpow2',/noder,chisq=chisq)
  
;  print,chisq
  dof=n_elements(time)-n_elements(a)
  print,sig
  sig=sig*sqrt(chisq/dof)
;  sig[[0,2]]=sig[[0,2]]*4.
  sig[[1,3]]=sig[[1,3]]*3.
  w0=where(sig eq 0,nw0) & if nw0 gt 0 then sig[w0]=a[w0]*0.1
  print,a,sig
  print,chisq/dof
;  oplot,t,a[0]*t^a[1]
  plotsym,1,5,thick=3,/fill
  w1=where(time lt a[2])
  w2=where(time gt a[2])

;  ploterror,time[g],rate[g],err[g],psym=3,/nohat,/xlog,/ylog,xtitle='T-T0 (s)',ytitle='XRT count rate (cts s!U-1!N)',title='GRB051022',xrange=[1e4,1e7],xstyle=1,yrange=[1e-4,10]

  for i=0,n_elements(time)-1 do $
     oplot,[tstart[i],tstop[i]],[rate[i],rate[i]]
  plotsym,1,5,thick=3,/fill
  if nul gt 0 then begin 
     plots,time[ul],rate[ul],psym=8
  endif
  t1=[1,time[w1],a[2]]
  t2=[a[2],time[w2],1e10]
  oplot,t1,a[0]*(a[2]/t1)^a[1],color=!blue
  oplot,t2,a[0]*(a[2]/t2)^a[3],color=!green  
  ind1=!tsym.alpha+'!L1!N' +' = '+ntostr(round(a[1]*100.)/100.,4)+!tsym.plusminus+ntostr(round(sig[1]*100.)/100.,4)
  ind2=!tsym.alpha+'!L2!N' +' = '+ntostr(round(a[3]*100.)/100.,4)+!tsym.plusminus+ntostr(round(sig[3]*100.)/100.,4)
  bt='Break Time = '+ntostr(round(a[2]/100.)*100)+!tsym.plusminus+ntostr(round(sig[2]/100.)*100)
  if keyword_set(out) then color=!black else color=!white
  legend,[ind1,ind2,bt],/top,/right,box=0,textcolor=[!blue,!green,color]
  print,nul
  
;  if nul gt 0 then begin 
;     plots,time[ul],rate[ul]+abs(err[ul]),psym=8
;     for i=0,n_elements(time[ul])-1 do $
;        oplot,[tstart[ul[i]],tstop[ul[i]]],[rate[ul[i]]+abs(err[ul[i]]),rate[ul[i]]+abs(err[ul[i]])]
;  endif 
;  legend,[!tsym.alpha +' = '+ntostr(a[1],5)+!tsym.plusminus+ntostr(sig[1],4)],/top,/right,box=0
  
  if keyword_set(out) then begin
     endplot
     spawn,'/usr/bin/convert -rotate 270 grb051022_lc.ps grb051022_lc.gif'
  endif 
  
  
;  stop
  return
end 
