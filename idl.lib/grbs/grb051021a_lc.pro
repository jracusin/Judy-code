pro grb051021a_lc,a,out=out
  
;  readcol,'~/grbs/grb051021a_30.qdp',time,tbin,rate,err,ratio,skip=7
  
  goto,skipy
;  .com /home/dmorris/Idl/plot_lc_prelease11
  files=['seg1_v9/sw00020018001xpcw2po_cl.evt','seg3_v19/sw00020018003xpcw2po_cl.evt','seg4_v18/sw00020018004xpcw2po_cl.evt','seg5_v22/sw00020018005xpcw2po_cl.evt','seg6_v9/sw00020018006xpcw2po_cl.evt','seg7_v28/sw00020018007xpcw2po_cl.evt','seg8_v18/sw00020018008xpcw2po_cl.evt','seg9_v10/sw00020018009xpcw2po_cl.evt','seg10_v17/sw00020018010xpcw2po_cl.evt','seg11_v18/sw00020018011xpcw2po_cl.evt']
;  plot_lc,[1,1,1,1,1,1,1,1,1],files,29.151708,9.0693333,10,29.05318,9.1879536,40,[42,35,30,30,30,30,30,30,30],151593717.d,0,1,1
  plot_lc,[1,1,1,1,1,1,1,1,1,1],files,29.151708,9.0693333,8,29.09298,9.1460489,40,[42,35,30,30,30,30,30,30,30,30],151593717.d,0,1,1
  
  skipy:
  
  btime=151593717.d
  readcol,'lc_out_comb.txt',time,tstart,tstop,rate,err,hard,harderr,exptime,scts,bcts,expfact,totext,skip=2,format='(d,d,d,d,d,d,d,d,d)',/silent
  tbin=tstop-tstart
  
  if n_elements(a) eq 0 then a=[100,-1]
  fit=mpcurvefit(time,rate,1./err^2,a,sig,function_name='pwr',chisq=chisq,/noder,ftol=1e-12)
  dof=n_elements(time)-n_elements(a)
  sig=sig*sqrt(chisq/dof)
  print,a,sig
  sig=sig*3.
  w0=where(sig eq 0,nw0) & if nw0 gt 0 then sig[w0]=a[w0]*0.1
  
  defsymbols
;  a=[100,1,3e3,1]
  
;  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 4)
;  parinfo.value=a
;  parinfo[2].limits=[0,1e8]
;  parinfo[1].limits=[0,20]
;  parinfo[3].limits=[0,20]

;  weights=1/err^2
;  fit=mpcurvefit(time,rate,weights,a,sig,function_name='bpow2',/noder,parinfo=parinfo,chisq=chisq)
;  dof=n_elements(time)-n_elements(a)
;  sig=sig*sqrt(chisq/dof)*4.
;  w0=where(sig eq 0,nw0) & if nw0 gt 0 then sig[w0]=a[w0]*0.1
;  print,a,sig
;  w1=where(time lt a[2])
;  w2=where(time gt a[2])
  
  
  if keyword_set(out) then begplot,name='grb051021a_lc.ps',/land
  
  ul=where(scts-bcts lt 3.*sqrt(bcts+scts),nul)
  g=where(scts-bcts gt 3.*sqrt(bcts+scts),ng)
  
  ploterror,time[g],rate[g],err[g],psym=3,/nohat,/xlog,/ylog,xtitle='T-T0 (s)',ytitle='XRT count rate (cts s!U-1!N)',title='GRB051021A',xrange=[1e4,1e7],xstyle=1,yrange=[1e-4,1] ;,xtickname=['10!U4!N',' ',' ',' ','5x10!U4!N']
  plotsym,1,5,thick=3,/fill
  if nul gt 0 then $
     plots,time[ul],rate[ul],psym=8
  print,nul
  t=[1,time,1e8]
  oplot,t,a[0]*t^a[1]
  for i=0,n_elements(time)-1 do $
     oplot,[tstart[i],tstop[i]],[rate[i],rate[i]]
  legend,[!tsym.alpha +' = '+ntostr(a[1],5)+!tsym.plusminus+ntostr(sig[1],4)],/top,/right,box=0
  
  
  if keyword_set(out) then begin
     endplot
     spawn,'/usr/bin/convert -rotate 270 grb051021a_lc.ps grb051021a_lc.gif'
  endif 
  
  
  return
end 
