pro grb051022_lc,a,time,rate,err,tstart,tbin,out=out
  
  goto,skipy
;  .com plot_lc_prelease10
  files=['seg1_v6/sw00020019001xpcw2po_cl.evt','seg2_v8/sw00020019002xpcw2po_cl.evt','seg3_v2/sw00020019003xpcw2po_cl.evt','seg4_v21/sw00020019004xpcw2po_cl.evt','seg5_v18/sw00020019005xpcw2po_cl.evt','seg6_v22/sw00020019006xpcw2po_cl.evt','seg7_v15/sw00020019007xpcw2po_cl.evt','seg8_v44/sw00020019008xpcw2po_cl.evt','seg9_v16/sw00020019009xpcw2po_cl.evt']
;  plot_lc,[1,1,1,1,1,1,1,1,1],files,359.01826,19.606416,20,359.00763,19.710275,40,[55,45,20,30,40,40,30,30,30],151679279.d,4,2.08,1,[1e5,4.8e5,5.6e5,8.15e5]
  
  plot_lc,[1,1,1,1,1,1,1,1,1],files,359.01826,19.606416,7,359.00763,19.710275,40,[100,50,20,30,40,40,30,50,30],151679279.d,4,2.08,1,[1e5,4.8e5,5.6e5,8.15e5]
  
  skipy:
  readcol,'lc_out.txt',time,tstart,tstop,rate,err,hard,harderr,exptime,scts,bcts,skip=2,format='(d,d,d,d,d,d,d)',/silent
  !p.multi=0
  erase
  tbin=tstop-tstart
;  skipd:
  
  if keyword_set(out) then begin
     begplot,name='grb051022_lc.ps',/land,/color
     color=!black
  endif else color=!white
  
  btime=date2met('2005-295-13:07:58')
;  ul=where(rate-err lt 1e-5,nul)
;  g=where(rate-err gt 1e-5)
  ul=where(scts-bcts lt 3.*sqrt(bcts+scts),nul)
  g=where(scts-bcts gt 3.*sqrt(bcts+scts),ng)
  
  ploterror,time[g],rate[g],err[g],psym=3,/nohat,/xlog,/ylog,xtitle='T-T0 (s)',ytitle='XRT count rate (cts s!U-1!N)',title='GRB051022',xrange=[1e4,1e7],xstyle=1,yrange=[1e-4,1]
  for i=0,n_elements(time[g])-1 do $
     oplot,[tstart[g[i]],tstop[g[i]]],[rate[g[i]],rate[g[i]]]
;  t=[1,time,1e8]
;  a=[100,-1]
  
  if n_elements(a) eq 0 then a=[1,1,3e5,4]
  
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 4)
  parinfo.value=a
  parinfo[2].limits=[1e4,1e6]
  parinfo[1].limits=[0.1,10]
  parinfo[3].limits=[0.1,10]
;  parinfo[2].step=1000
;  parinfo[3].step=0.01
  
  error=err
;  if nul gt 0 then error[ul]=error[ul]*3.
  weights=(1./error)^2
  fit=mpcurvefit(time,rate,weights,a,sig,function_name='bpow2',/noder,chisq=chisq,itmax=500,xtol=1e-20,parinfo=parinfo)
;  fit=mpfitfun('bpow3',time,rate,error,a,parinfo=parinfo,perror=sig,xtol=1e-12)
;  stop
;  stop
;  a=fit
;  print,chisq
  dof=n_elements(time)-n_elements(a)
  print,sig
;  CHISQ = TOTAL( (rate-bpow3(time,a))^2 * ABS(WEIGHTS) )
  sig=sig*sqrt(chisq/dof)
;  sig[[0,2]]=sig[[0,2]]*4.
;  sig[[1,3]]=sig[[1,3]]*3.
  sig=sig*3.
;  w0=where(sig eq 0,nw0) & if nw0 gt 0 then sig[w0]=a[w0]*0.1
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
  bpow2,t1,a,m1
  bpow2,t2,a,m2
  oplot,t1,m1,color=!blue
  oplot,t2,m2,color=!green
;  oplot,t1,a[0]*t1^(-a[1]),color=!blue
;  oplot,t2,a[0]*a[2]^(-a[1]+a[3])*t2^(-a[3]),color=!green
;  oplot,t1,a[0]*(a[2]/t1)^a[1],color=!blue
;  oplot,t2,a[0]*(a[2]/t2)^a[3],color=!green  
  ind1=!tsym.alpha+'!L1!N' +' = '+ntostr(round(a[1]*100.)/100.,4)+!tsym.plusminus+ntostr(round(sig[1]*100.)/100.,4)
  ind2=!tsym.alpha+'!L2!N' +' = '+ntostr(round(a[3]*100.)/100.,4)+!tsym.plusminus+ntostr(round(sig[3]*100.)/100.,4)
  bt='Break Time = '+ntostr(round(a[2]/100.)*100)+!tsym.plusminus+ntostr(round(sig[2]/100.)*100)
  cs=!tsym.chi+'!U2!N/dof = '+ntostr(chisq/dof,4)
  if keyword_set(out) then color=!black else color=!white
  legend,[ind1,ind2,bt,cs],/top,/right,box=0,textcolor=[!blue,!green,color,color]
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
