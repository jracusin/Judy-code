pro grb051021b_lc,a,out=out
  
  simpctable
  defsymbols
  goto,skipy
;  .com plot_lc_prelease10
  files=['seg0_v9/sw00160672000xwtw2po_cl_ff.evt','seg0_v9/sw00160672000xpcw2po_cl.evt','seg1_v14/sw00160672001xpcw2po_cl.evt','seg2_v19/sw00160672002xpcw2po_cl.evt','seg3_v19/sw00160672003xpcw2po_cl.evt','seg4_v20/sw00160672004xpcw2po_cl.evt']
  plot_lc,[0,1,1,1,1,1],files,126.04917,-45.541889,20,125.97078,-45.519514,40,[16,12,40,40,40,40],151630312.d,4,2.08,1
  skipy:
  
  readcol,'lc_out_comb.txt',time,tstart,tstop,rate,err,hard,harderr,exptime,scts,bcts,skip=2,format='(d,d,d,d,d,d,d)'
;  set_plot,'x'
  !p.multi=0
  erase
  tbin=tstop-tstart
  
  ul=where(scts-bcts lt 3.*sqrt(bcts+scts),nul)
  g=where(scts-bcts gt 3.*sqrt(bcts+scts),ng)
  
  if keyword_set(out) then begplot,name='grb051021b_lc.ps',/land,/color
;  fit_bpow,time,rate,err
;  goto,jump
  if n_elements(a) eq 0 then a=[1,1,4e3,1]
  
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 4)
  parinfo.value=a
  parinfo[2].limits=[0,1e8]
  parinfo[1].limits=[0,20]
  parinfo[3].limits=[0,20]
;  parinfo[2].step=10
  
  error=err
  if nul gt 0 then error[ul]=error[ul]*3.
  weights=1/error^2
  fit=mpcurvefit(time[3:*],rate[3:*],weights[3:*],a,sig,function_name='bpow2',/noder,parinfo=parinfo,chisq=chisq,itmax=500,nfree=4,ftol=1e-12)
  dof=n_elements(time)-n_elements(a)
  sig=sig*sqrt(chisq/dof)
  sig[[0,1,2]]=sig[[0,1,2]]*4.
  sig[3]=sig[3]*2.
  w0=where(sig eq 0,nw0) & if nw0 gt 0 then sig[w0]=a[w0]*0.1
  print,a,sig
  w1=where(time lt a[2])
  w2=where(time gt a[2])

  ploterror,time[g],rate[g],err[g],psym=3,/nohat,/xlog,/ylog,xtitle='T-T0 (s)',ytitle='XRT count rate (cts s!U-1!N)',title='GRB051021B',xrange=[1e1,1e7],xstyle=1,yrange=[1e-4,10]
;  t=[1,time,1e6]
  t1=[1,time[w1],a[2]]
  t2=[a[2],time[w2],1e10]
  oplot,t1,a[0]*(a[2]/t1)^a[1],color=!blue
  oplot,t2,a[0]*(a[2]/t2)^a[3],color=!green
  for i=0,n_elements(time)-1 do $
     oplot,[tstart[i],tstop[i]],[rate[i],rate[i]]

  oploterror,time[0:1],rate[0:1],err[0:1],psym=3,/nohat,color=!red,errcolor=!red
  for i=0,1 do begin
     oplot,[tstart[i],tstop[i]],[rate[i],rate[i]],color=!red
  endfor 
;  oploterror,time[0:2],rate[0:2],tbin[0:2]/2.,err[0:2],psym=3,/nohat,color=!red,errcolor=!red
  plotsym,1,5,thick=3,/fill
  if nul gt 0 then begin 
     plots,time[ul],rate[ul],psym=8
;     for i=0,nul-1 do $
;        oplot,[tstart[ul[i]],tstop[ul[i]]],[rate[ul[i]],rate[ul[i]]]
;        oplot,[time[ul[i]]-tbin[ul[i]]/2.,time[ul[i]]+tbin[ul[i]]/2.],[rate[ul[i]],rate[ul[i]]]
  
  endif
  ind1=!tsym.alpha+'!L1!N' +' = '+ntostr(a[1],4)+!tsym.plusminus+ntostr(sig[1],4)
  ind2=!tsym.alpha+'!L2!N' +' = '+ntostr(a[3],4)+!tsym.plusminus+ntostr(sig[3],4)
  bt='Break Time = '+ntostr(a[2],4)+!tsym.plusminus+ntostr(sig[2],3)
  if keyword_set(out) then color=!black else color=!white
  legend,[ind1,ind2,bt,'WT','PC'],/top,/right,box=0,textcolor=[!blue,!green,color,!red,color]
;  jump:
  
  if keyword_set(out) then begin
     endplot
     spawn,'/usr/bin/convert -rotate 270 grb051021b_lc.ps grb051021b_lc.gif'
  endif 
  
;  stop
  
  goto,jump1
  files=['seg0_v7/sw00160672000xwtw2po_cl.evt','seg0_v7/sw00160672000xpcw2po_cl.evt','seg1_v14/sw00160672001xpcw2po_cl.evt']
  btime=date2met('2005-294-23:31:53')
  ra=[126.05115d,126.04917d,126.04917d]                             
  dec=[-45.519522d,-45.541889d,-45.541889d]                         
  bra=[125.97078d,126.07901d,126.07901d]                            
  bdec=[-45.519514d,-45.46854d,-45.46854d]   
  plot_lc,[0,1,1],files,ra,dec,20,bra,bdec,40,[20,20,20],[0,2,0],1,btime
  jump1:
  
;  plot_lc,[ftype],['filename'],[src_x],[src_y],src_rad,[back_x],[back_y],back_rad,[binning],puc_rad,exp_corr_flag,bat_trig
  
  return
end 
