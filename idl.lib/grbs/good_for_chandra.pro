@fit_functions
@chandra_analysis
@fit_lc
pro plot_160625B

  cd,'~/Chandra/GRB160625B/'
  begplot,name='GRB160625B_chandra_predict.ps',/color,/land,font='helvetica'

  lc=lcout2fits()
  spec=mrdfits('UL_specfits.fits',1)

  fact=spec[0].unabs_cfratio
  xrange=[1e3,1e8]
  !x.margin=[4,3]
  yrange=[1e-15,1e-8]
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate*fact,lc.src_rate_err*fact,lc.type,xrange=xrange,/xlog,/ylog,ytitle='Flux (0.3-10 keV; erg cm!U-2!N s!U-1!N)',/xsty,yrange=yrange,xtitle='Time since trigger (s)',charsize=1.5,/ysty,yminor=0,ytickv=yrange,yticks=1
;  ploterror,lc.time,lc.src_rate*fact,lc.src_rate_err*fact,/nohat,/xlog,/ylog,xrange=xrange,yrange=yrange,ytitle='Flux (erg cm!U-2!N s!U-1!N)',xtitle='Time since trigger (s)',charsize=1.5,psym=3,/xsty,/ysty,yminor=0,ytickv=yrange,yticks=1
;  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]*fact
  axis,xrange[0],1e-15,yminor=9,yaxis=0,/ylog,yrange=yrange,/ysty

  read_lcfit,'lc_fit_out_idl_int9.dat',pnames,p
  mo=fit_models(pnames,p,basemo=basemo)
  t=[lc.time,p,1000,1200,1400,1600,1800,2000,3000,5000,7000,1e5,1.5e5,2e5,4e5,7e5,1e6,2e6,3e6]
  t=t[sort(t)]
  tmp=execute('yfit='+mo+'(t,p)')
  oplot,t,yfit*fact,color=!green,line=2
  
  trigtime=date2met('2016-06-25-22:40:16.28',/fermi)
  oct1=date2met('2016-10-01-00:00:00',/fermi)-trigtime
  oplot,[oct1,oct1],yrange,line=1,color=!orange
  xyouts,oct1*0.9,1e-12,'October 1 2016',orient=90,color=!orange,charsize=1.
  dec1=date2met('2016-12-01-00:00:00',/fermi)-trigtime
  oplot,[dec1,dec1],yrange,line=1,color=!orange
  xyouts,dec1*0.9,1e-12,'December 1 2016',orient=90,color=!orange,charsize=1.
  mar14=date2met('2017-03-14-00:00:00',/fermi)-trigtime
  oplot,[mar14,mar14],yrange,line=1,color=!orange
  xyouts,mar14*0.9,1e-12,'March 14 2017',orient=90,color=!orange,charsize=1.


  fjy=flux2jy(1.,spec[0].phind)*1e6
  ytickv=[1e-4,1e-3,1e-2,1e-1,1,10,100,1000,1e4]/fjy
  axis,xrange[1],1e-15,yminor=9,yaxis=1,/ylog,yrange=yrange,/ysty,yticks=8,ytickv=ytickv,ytickname=['10!U-4!N','10!U-3!N','10!U-2!N','10!U-1!N','1','10','10!U2!N','10!U3!N'],ytitle='Flux ('+!tsym.mu+'Jy)'

  readcol,'160625B.txt',time,flux,err,format='(f,f,f)'
  time=time*86400.
  plotsym,0,1,/fill
  oplot,time,flux/fjy,color=!purple,psym=8
;  oplot,xrange,[1e-14,1e-14],line=1,color=!red
  legend,['XRT','Optical'],box=0,/top,/right,textcolor=[!p.color,!purple]

;oplot,xrange,[1e-11,1e-11],color=!red
  endplot
  ps2pdf,'GRB160625B_chandra_predict.ps'

  print,good_for_chandra('GRB160625B',1.4,dir='~/Chandra/GRB160625B',/lat,/plotanyway,/ps)


stop
  return
end 

pro plot_160509a

  cd,'~/Chandra/GRB160509A'
  lc=lcout2fits(/chandra)
  spec=mrdfits('UL_specfits.fits',1)
  f=flux2jy(spec.cfratio,spec.phind)*1e6
  trigtime=date2met('2016-05-09-20:47:00.89')

  begplot,name='GRB160509A_Xray_radio_lc.ps',/land,/color
;  ploterror,lc.time,lc.src_rate*f,lc.src_rate_err*f
  plot_like_qdp,lc=lc,flux=f,ytitle='Flux ('+!tsym.mu+'Jy)',yrange=[1e-3,1e4],yformat='(loglabels)',/ysty,yminor=9
  read_lcfit,'lc_fit_comb.dat',pnames,p
  t=[lc.time,p[2],p[4]]
  t=t[sort(t)]
  oplot,t,bkn2pow(t,p)*f,color=!green

  may1=date2met('2016-05-01-00:00:00')-trigtime
  t6=may1+[9.72,11.42,14.5,20.5]*86400.
  r6=[80,530,520,130] ;; uJy

  t9=may1+[11.42,14.5,20.5]*86400.
  r9=[720,390,160] ;;uJy

  june1=date2met('2016-06-01-00:00:00')-trigtime
  tx=june1+[2,15]*86400.
  rx=[71,51]
  rxerr=[7,8]
  
  tc=june1+[2,15]*86400.
  rc=[80,40]
  rcerr=[4,4]

  plotsym,0,1,/fill
  oplot,t6,r6,psym=8,color=!blue
  oplot,t9,r9,psym=8,color=!cyan
  oplot,tx,rx,psym=8,color=!dodgerblue;,/nohat,errcolor=!dodgerblue
  oplot,tc,rc,psym=8,color=!royalblue;,/nohat,errcolor=!royalblue

  legend,['XRT','Chandra','6 GHz','9GHz','X-band','C-band'],box=0,/top,/right,textcolor=[!red,!purple,!blue,!cyan,!dodgerblue,!royalblue]
  endplot
  ps2pdf,'GRB160509A_Xray_radio_lc.ps'

  return
end 

pro sim_160509a,plot_results=plot_results,plot_sims=plot_sims,snum=snum

  ;; add fake data point at specific time (x2 of last data point)
  ;; randomize data point and error bar within poisson errors N times
  ;; refit the light curve and calc errors (?) for each realization
  ;; save output
  ;; look at distribution of fit params and errors on fits
  ;; try with different exposure times

  cd,'~/Chandra/GRB160509A'
  lc=lcout2fits(/chandra)
  newt=lc[208].time*2.
  read_lcfit,'lc_fit_comb.dat',pnames,p,perr
  read_psffits,'GRB160509A',psfspec,rad=3
  xspec=mrdfits('UL_specfits.fits',1)
  exposure=[20e3,40e3,60e3,80e3,100e3]
  simdir=['sim20','sim40','sim60','sim80','sim100']
  if not keyword_set(plot_results) and not keyword_set(plot_sims) then begin 
     if n_elements(snum) eq 0 then snum=0
     i=snum
     exposure=exposure[i]
     print,simdir[i]
     
     sim_chandra,lc,newt,pnames,p,xspec.cfratio,psfspec.fluxfact,exposure,sdir=simdir[i]+'/'
  endif 

  if keyword_set(plot_results) then begin 
     begplot,name='add_chandra_obs_sim.ps',/color
     !p.multi=[0,1,4]
     color=[!red,!blue,!green,!orange,!magenta]
     !p.charsize=2.

     xrange=[0,50]
     yrange=[0,20]
     bin=1
     plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle='Chandra Counts'
     med=fltarr(n_elements(simdir))
     for i=0,n_elements(simdir)-1 do begin
        sim=mrdfits(simdir[i]+'/lc_sim_fit.fits',1)
        plothist,sim.rxcts,bin=bin,xrange=xrange,yrange=yrange,color=color[i],/overplot
        med[i]=median(sim.rxcts)
     endfor 
     legend,simdir+' median='+ntostr(fix(med)),textcolor=color,box=0,/top,/right,charsize=1.

     xrange=[1.7,2.1]
     yrange=[0,20]
     bin=0.01
     plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle='PL'
     med=fltarr(n_elements(simdir))
     for i=0,n_elements(simdir)-1 do begin
        sim=mrdfits(simdir[i]+'/lc_sim_fit.fits',1)
        plothist,sim.p[5],bin=bin,xrange=xrange,yrange=yrange,color=color[i],/overplot
        med[i]=median(sim.p[5])
     endfor 
     legend,simdir+' median='+numdec(med,3),textcolor=color,box=0,/top,/left,charsize=1.

     pm=['-','+']
     for j=0,1 do begin 
        xrange=[0.1,0.3]
        yrange=[0,50]
        bin=0.002
        plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle='PL '+pm[j]+' err'
     
        med=fltarr(n_elements(simdir))
        for i=0,n_elements(simdir)-1 do begin
           sim=mrdfits(simdir[i]+'/lc_sim_fit.fits',1)
           plothist,sim.perr[j,5],bin=bin,xrange=xrange,yrange=yrange,color=color[i],/overplot
           med[i]=median(sim.perr[j,5])
        endfor 
        legend,simdir+' median='+numdec(med,3),textcolor=color,box=0,/top,/right,charsize=1.
     endfor 
     !p.multi=0

     endplot
     ps2pdf,'add_chandra_obs_sim.ps'
  endif 

  if keyword_set(plot_sims) then begin
     i=snum
     sim=mrdfits(simdir[i]+'/lc_sim_fit.fits',1)
     ex=exposure[i]
     for i=0,99 do begin 
        plot_like_qdp,lc=lc,xrange=[1e3,1e7],yrange=[1e-5,10]
        plotsym,0,1,/fill
        plots,newt,sim[i].crate,psym=8
        oplot,[newt,newt],sim[i].crate+sim[i].crateerr*[-1,1.]
;psfspec.fluxfact/xspec.cfratio+sqrt(sim[i].rxcts)/ex*[-1,1.]*psfspec.fluxfact/xspec.cfratio
        t=[lc.time,newt,sim[i].p[[2,4]]]
        t=t[sort(t)]
        oplot,t,bkn2pow(t,sim[i].p),color=!green
        oplot,[sim[i].p[2],sim[i].p[2]],[1e-5,1e2],color=!green,line=2
        oplot,[sim[i].p[4],sim[i].p[4]],[1e-5,1e2],color=!green,line=2
        oplot,t,bkn2pow(t,p),color=!cyan
        oplot,[p[2],p[2]],[1e-5,1e2],color=!cyan,line=2
        oplot,[p[4],p[4]],[1e-5,1e2],color=!cyan,line=2
        numd=[2,3,2,3,2,3]
        sci=[1,0,1,0,1,0]
        leg=''
        for j=0,n_elements(p)-1 do leg=[leg,pnames[j]+' = '+numdec(sim[i].p[j],numd[j],sci=sci[j])+' {-'+numdec(sim[i].perr[0,j],numd[j],sci=sci[j])+',+'+numdec(sim[i].perr[1,j],numd[j],sci=sci[j])+'}']
        legend,leg[1:*],box=0,/top,/right
        k=get_kbrd(10)
        if k eq 's' then stop
     endfor 
  endif 
  stop

  return
end 

pro sim_chandra,lc,newt,pnames,p,xfact,cfact,exposure,sdir=sdir

  if n_elements(sdir) eq 0 then sdir=''
  nsim=100.
  model=fit_models(pnames,p,breaks=breaks)
  nbreaks=n_elements(breaks)

  wdet=where(lc.src_rate_err ne 0,nwdet)
  time=[lc[wdet].time,newt]
  timeerr=dblarr(2,nwdet+1)
  timeerr[0,*]=[lc[wdet].time-lc[wdet].tstart,exposure/2.]
  timeerr[1,*]=[lc[wdet].tstop-lc[wdet].time,exposure/2.]

  xrate=call_function(model,newt,p)  ;;; extrapolated rate in xrt
  crate=xrate*xfact/cfact ;; converted to chandra
  rxcts=randomu(seed,nsim,poi=round(crate*exposure),/double) ;; randomized counts
  intmo='int'+model

  pfin=dblarr(nsim)
  outp=create_struct('sim',0,'rxcts',0,'crate',0d,'crateerr',0d,'pnames',strarr(30),'p',dblarr(30),'perr',dblarr(2,30))
  
  if exist(sdir+'lc_sim_fit.fits') then outp=mrdfits(sdir+'lc_sim_fit.fits',1) else $
     outp=replicate(outp,nsim)
  s=where(outp.p[0] eq 0)
  s=s[0]

  for i=s,nsim-1 do begin 

     crate2=rxcts[i]/exposure*cfact/xfact ;; randomized rate
     crate2err=sqrt(rxcts[i])/exposure*cfact/xfact
     frate=[lc[wdet].src_rate,crate2]
     fraterr=[lc[wdet].src_rate_err,crate2err]
     
     ;;; properly fill out sim chandra obs
     clc=lcout2fits(/empty)
     clc.time=newt
     clc.tstart=newt-exposure/2.
     clc.tstop=newt+exposure/2.
     clc.src_rate=crate2
     clc.src_rate_err=crate2err
     clc.src_counts=rxcts[i]
     clc.tot_back_cts=0.009*rxcts[i]
     clc.pu_corr=1.
     clc.exptime=exposure
     clc.type=2
     concat_structs,lc,clc,lc2

     fit_pow_model,time,frate,timeerr,fraterr,p,intmo,pnames,yfit,newp,perror,chisq2,dof2,weights,status=status,/silent,breaks=nbreaks,noint=noint,uvot=uvot
;     print,rxcts
     pfin[i]=newp[n_elements(newp)-1]
     
     lc_monte_pow,lc2,newp,pnames,chisq,dof,perror,ps=ps,/nowrite,nsim=1000,int=int,file=file,uvot=uvot,fflux=fflux,flux=flux,nsig=nsig,breaks=nbreaks,/noplot,add=ntostr(i),outdir=sdir
     outp[i].sim=i
     outp[i].rxcts=rxcts[i]
     outp[i].crate=crate2
     outp[i].crateerr=crate2err;sqrt(crate2*exposure)/exposure
     outp[i].pnames[0:n_elements(pnames)-1]=pnames
     outp[i].p[0:n_elements(newp)-1]=newp
     outp[i].perr[0,0:n_elements(newp)-1]=perror[0,*]
     outp[i].perr[1,0:n_elements(newp)-1]=perror[1,*]

     mwrfits,outp,sdir+'lc_sim_fit.fits',/create

  endfor 

stop

  return
end 

pro plot_131108a

  cd,'~/Desktop/GRB131108A'
  begplot,name='GRB131108A_chandra_predict.ps',/color,/land,font='helvetica'

  lc=lcout2fits(/phil)
  spec=mrdfits('UL_specfits.fits',1)

  fact=spec[0].cfratio
  xrange=[100,3e6]
  !x.margin=[1,2]
  yrange=[1e-15,1e-8]
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate*fact,lc.src_rate_err*fact,lc.type,xrange=xrange,/xlog,/ylog,ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,yrange=yrange,xtitle='Time since trigger (s)',charsize=1.5,/ysty

  read_lcfit,'lc_fit_out_idl_int7_pow.dat',pnames,p
  read_lcfit,'lc_fit_out_idl_int7_bknpow.dat',pnames2,p2
  mo=fit_models(pnames,p,basemo=basemo)
  t=[lc.time,p2[2],1000,1200,1400,1600,1800,2000,3000,5000,7000,1e5,1.5e5,2e5,4e5,7e5,1e6,2e6,3e6]
  t=t[sort(t)]
  tmp=execute('yfit='+mo+'(t,p)')
  oplot,t,yfit*fact,color=!green,line=2

  mo2=fit_models(pnames2,p2,basemo=basemo)
  yfit2=bknpow(t,p2)
  oplot,t,yfit2*fact,color=!magenta,line=2

;  p3=[p[0:5]]
;  tmp=execute('yfit3='+basemo+'(t,p3)')
;  oplot,t,yfit3*fact,color=!green

  day=86400.
;  oplot,[15,15]*day,yrange,line=2
;  oplot,[22,22]*day,yrange,line=2
  oplot,xrange,[1e-14,1e-14],line=1
  oplot,xrange,[5e-15,5e-15],line=1

  cfratio=spec.cfratio
  np=n_elements(p)
  pmax=p[np-1]

  tmp=execute('nmax='+mo+'(p[np-2],p)/pow(p[np-2],[1.,pmax])')
  nml=round(alog10(nmax*cfratio)-0.5)
  nm=nmax*cfratio/10.^nml
  if nml ne 0 then nmp=ntostr(nm,4)+'x10!U'+ntostr(fix(nml))+'!N' else nmp=ntostr(nm,4)
  f=[1e-14,8e-15,5e-15,3e-15,1e-15]
  x=round(alog10(f)-0.49)
  tlim=(f/(nmax[0]*cfratio))^(-1./pmax[0])
  legend,['Final Slope','Norm = '+nmp,!tsym.alpha+' = '+ntostr(pmax,4)],box=0,/top,/right,textcolor=!green
  legend,['T-T0 (F='+ntostr(f*1e15,1)+'x10!U'+ntostr(x)+'!N)='+numdec(tlim/1e6,2)+'x10!U6!N s = '+numdec(tlim/86400.,1)+' days'],/top,/left,box=0,textcolor=[!green,!green,!green,!green,!green]
  oplot,[tlim[0],tlim[0]],yrange,line=1,color=!green
  oplot,[tlim[2],tlim[2]],yrange,line=1,color=!green

  np=n_elements(p2)
  pmax=p2[np-1]
  p=p2
  mo=mo2
  tmp=execute('nmax='+mo+'(p[np-2],p)/pow(p[np-2],[1.,pmax])')
  nml=round(alog10(nmax*cfratio)-0.5)
  nm=nmax*cfratio/10.^nml
  if nml ne 0 then nmp=ntostr(nm,4)+'x10!U'+ntostr(fix(nml))+'!N' else nmp=ntostr(nm,4)
  f=[1e-14,8e-15,5e-15,3e-15,1e-15]
  x=round(alog10(f)-0.49)
  tlim=(f/(nmax[0]*cfratio))^(-1./pmax[0])
  legend,['Final Slope','Norm = '+nmp,!tsym.alpha+' = '+ntostr(pmax,4)],box=0,/center,/left,textcolor=!magenta
  legend,['T-T0 (F='+ntostr(f*1e15,1)+'x10!U'+ntostr(x)+'!N)='+numdec(tlim/1e6,2)+'x10!U6!N s = '+numdec(tlim/86400.,1)+' days'],/bottom,/left,box=0,textcolor=[!magenta,!magenta,!magenta,!magenta,!magenta]
  oplot,[tlim[0],tlim[0]],yrange,line=1,color=!magenta
  oplot,[tlim[2],tlim[2]],yrange,line=1,color=!magenta



  endplot
  spawn,'ps2pdf GRB131108A_chandra_predict.ps'

  print,good_for_chandra('GRB131108A',2.4,dir='~/Desktop/GRB131108A',/lat,/plotanyway,/ps)
  
  stop
return
end 
  
pro plot_130907a

  cd,'~/Chandra/GRB130907A'
  begplot,name='GRB130907A_chandra_predict.ps',/color,/land,font='helvetica'

  lc=lcout2fits(/phil)
  spec=mrdfits('UL_specfits.fits',1)

  fact=spec[0].cfratio
  xrange=[100,3e6]
  !x.margin=[1,2]
  yrange=[1e-15,1e-8]
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate*fact,lc.src_rate_err*fact,lc.type,xrange=xrange,/xlog,/ylog,ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,yrange=yrange,xtitle='Time since trigger (s)',charsize=1.5,/ysty

  read_lcfit,'lc_fit_out_idl_int8.dat',pnames,p
  mo=fit_models(pnames,p,basemo=basemo)
  t=[lc.time,p2[2],1000,1200,1400,1600,1800,2000,3000,5000,7000,1e5,1.5e5,2e5,4e5,7e5,1e6,2e6,3e6]
  t=t[sort(t)]
  tmp=execute('yfit='+mo+'(t,p)')
  oplot,t,yfit*fact,color=!green,line=2

  day=86400.
  oplot,xrange,[1e-14,1e-14],line=1
  oplot,xrange,[5e-15,5e-15],line=1

  cfratio=spec[n_elements(spec)-1].cfratio
  np=n_elements(p)
  pmax=p[np-1]

  tmp=execute('nmax='+mo+'(p[np-2],p)/pow(p[np-2],[1.,pmax])')
  nml=round(alog10(nmax*cfratio)-0.5)
  nm=nmax*cfratio/10.^nml
  if nml ne 0 then nmp=ntostr(nm,4)+'x10!U'+ntostr(fix(nml))+'!N' else nmp=ntostr(nm,4)
  f=[1e-14,8e-15,5e-15,3e-15,1e-15]
  x=round(alog10(f)-0.49)
  tlim=(f/(nmax[0]*cfratio))^(-1./pmax[0])
  legend,['Final Slope','Norm = '+nmp,!tsym.alpha+' = '+ntostr(pmax,4)],box=0,/top,/right
  legend,['T-T0 (F='+ntostr(f*1e15,1)+'x10!U'+ntostr(x)+'!N)='+numdec(tlim/1e6,2)+'x10!U6!N s = '+numdec(tlim/86400.,1)+' days'],/top,/left,box=0
  oplot,[tlim[0],tlim[0]],yrange,line=1,color=!green
  oplot,[tlim[2],tlim[2]],yrange,line=1,color=!green
  endplot
  spawn,'ps2pdf GRB130907A_chandra_predict.ps'

  print,good_for_chandra('GRB130907A',1.2,dir='~/Chandra/GRB130907A',/lat,/plotanyway,/ps)
  
  stop

return
end 

pro plot_130831a

  cd,'~/Desktop/GRB130831A'
  begplot,name='GRB130831A_chandra_predict.ps',/color,/land,font='helvetica'

  lc=lcout2fits()
  spec=mrdfits('UL_specfits.fits',1)

  fact=spec[1].unabs_cfratio
  xrange=[100,3e6]
  !x.margin=[1,2]
  yrange=[1e-15,1e-8]
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate*fact,lc.src_rate_err*fact,lc.type,xrange=xrange,/xlog,/ylog,ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,yrange=yrange,xtitle='Time since trigger (s)',charsize=1.5,/ysty

  read_lcfit,'lc_fit_out_idl_int8.dat',pnames,p
  mo=fit_models(pnames,p,basemo=basemo)
  t=[lc.time,p[[2,4]],1000,1200,1400,1600,1800,2000,3000,5000,7000,1e5,1.5e5,2e5,4e5,7e5,1e6,2e6,3e6]
  t=t[sort(t)]
  tmp=execute('yfit='+mo+'(t,p)')
  oplot,t,yfit*fact,color=!green,line=2

  p2=[p[0:5],1.58e5,1.3]
  yfit2=bkn3pow(t,p2)
  oplot,t,yfit2*fact,color=!magenta

  p3=[p[0:5]]
  tmp=execute('yfit3='+basemo+'(t,p3)')
  oplot,t,yfit3*fact,color=!green

  day=86400.
  oplot,[15,15]*day,yrange,line=2
  oplot,[22,22]*day,yrange,line=2
  oplot,xrange,[3e-14,3e-14],line=1
  oplot,xrange,[1.5e-14,1.5e-14],line=1

  endplot
  spawn,'ps2pdf GRB130831A_chandra_predict.ps'
  
  stop
return
end 

pro plot_130427a

  cd,'~/Desktop/GRB130427A'
  begplot,name='GRB130427A_chandra_predict.ps',/color,/land,font='helvetica'

;  lc=lcout2fits(/phil,pcfile='PCCURVE_mod.qdp')
  lc=lcout2fits(/chandra) ;; with first chandra point
  spec=mrdfits('UL_specfits.fits',1)
  xrange=[1e4,2e8]
  read_lcfit,'lc_fit_out_idl_int8_bkn2pow.dat',pname,p1,p1err
  read_lcfit,'lc_fit_out_idl_int8_bkn3pow.dat',pname,p2,p2err
  p3=[p2,lc[n_elements(lc)-1].time,2.2]

  tc=2000.*86400.

  yrange=[1d-15,1d-9]
  fact=spec[1].cfratio
  plot,xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xlog,/ylog,ytitle='Flux (erg cm!U-2!N s!U-1!N',/xsty,xtitle='Time since trigger (s)',charsize=1.5

  db=ymd2dn(2013,04,27) ;; 130427
  t1=(ymd2dn(2013,7,22)-db)*86400d
  t2=(ymd2dn(2013,10,18)-db)*86400d
  t3=(ymd2dn(2014,1,1)+365-db)*86400d
  t4=(ymd2dn(2014,2,15)+365-db)*86400d
  t5=(ymd2dn(2015,1,1)+2*365-db)*86400d
  t6=(ymd2dn(2015,10,1)+3*365-db)*86400d
  t7=(ymd2dn(2016,3,1)+4*365-db)*86400d

  ts=[t1,t2,t3,t4,t5,t6,t7]
  mon=['July 22','Oct 18','Jan 1 2014','Feb 15','Jan 1 2015','Oct 1','Mar 1 2016']
  polyfill,[t1,t2,t2,t1,t1],[yrange[0]*1.02,yrange[0]*1.02,yrange[1]*0.98,yrange[1]*0.98,yrange[0]*1.02],color=!yellow
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate*fact,lc.src_rate_err*fact,lc.type,/over

  xyouts,1.2e7,2e-12,'Swift Sun Constrained',/data,orient=90,charsize=1.0
  for i=0,n_elements(mon)-1 do begin 
     oplot,[ts[i],ts[i]],[1e-20,1e-8],line=1
     xyouts,ts[i]*0.97,1e-13,mon[i],/data,orient=90,charsize=0.9
  endfor

  t=[lc.time,p2[2],p2[4],p2[6],tc]
  t=t[sort(t)]
  oplot,t,bkn2pow(t,p1)*fact,color=!cyan,line=2
  oplot,t,bkn3pow(t,p2)*fact,color=!magenta,line=2
  w=where(t ge lc[n_elements(lc)-1].time)
  oplot,t[w],bkn4pow(t[w],p3)*fact,color=!orange,line=2

;  oplot,xrange,[4e-14,4e-14],line=2

  legend,['bknpow: '+!tsym.alpha+'!L2!N='+numdec(p1[3],2),$;+!tsym.plusminus+numdec(p1err[1,3],3),$
;          'bkn2pow:
;          '+!tsym.alpha+'!L2!N='+numdec(p2[5],2)+!tsym.plusminus+numdec(p2err[1,5],2)+', '+!tsym.alpha+'!L3!N='+numdec(p2[7],2)+!tsym.plusminus+numdec(p2err[1,7],2)],/left,/bottom,box=0,textcolor=[!cyan,!magenta],charsize=1.5
          'bkn2pow: '+!tsym.alpha+'!L2!N='+numdec(p2[5],2)+', '+!tsym.alpha+'!L3!N='+numdec(p2[7],2)],/left,/bottom,box=0,textcolor=[!cyan,!magenta],charsize=1.5

  endplot
  spawn,'ps2pdf GRB130427A_chandra_predict.ps'

;  t1=(ymd2dn(2014,1,1)+365-db)*86400d
;  t2=(ymd2dn(2014,2,15)+365-db)*86400d
;  t3=(ymd2dn(2014,7,1)+365-db)*86400d
;  t4=(ymd2dn(2014,10,1)+365-db)*86400d
;  t5=(ymd2dn(2015,1,1)+2*365-db)*86400d
;  t6=(ymd2dn(2015,6,1)+2*365-db)*86400d
;  t7=(ymd2dn(2016,1,1)+3*365-db)*86400d
;  ts=[t2,t3,t4,t5,t6,t7]

;  mon=['Feb 15 2014','Jul 1 2014','Oct 1 2014','Jan 1 2015','Jun 1 2015','Jan 1 2016']

  cfact=1.1e-11;1/1.06e11 ;; chandra flux->cts/s
  f=bkn2pow(ts,p1)
  print,'Date        Flux (erg/cm2/s)  Chandra (cts/s)  Cts (20 ks)  Cts (40 ks)    Cts (60 ks)  Cts (120 ks)'
  print,'BKN2POW'
  colprint,mon+'        '+numdec(f*fact,2,/sci)+'        '+numdec(f*fact/cfact,2,/sci)+'        '+numdec(f*fact/cfact*20e3,2)+'        '+numdec(f*fact/cfact*40e3,2)+'        '+numdec(f*fact/cfact*60e3,2)+'        '+numdec(f*fact/cfact*120e3,2)
  print
  f=bkn3pow(ts,p2)
;  print,'Date        Flux (erg/cm2/s)  Chandra (cts/s)  Cts (20 ks)  Cts (40 ks)  Cts (60 ks)'
  print,'BKN3POW'
  colprint,mon+'        '+numdec(f*fact,2,/sci)+'        '+numdec(f*fact/cfact,2,/sci)+'        '+numdec(f*fact/cfact*20e3,2)+'        '+numdec(f*fact/cfact*40e3,2)+'        '+numdec(f*fact/cfact*60e3,2)+'        '+numdec(f*fact/cfact*120e3,2)
  print
  print,'BKN4POW - Jet break ~last observation'
  f=bkn4pow(ts,p3)
  colprint,mon+'        '+numdec(f*fact,2,/sci)+'        '+numdec(f*fact/cfact,2,/sci)+'        '+numdec(f*fact/cfact*20e3,2)+'        '+numdec(f*fact/cfact*40e3,2)+'        '+numdec(f*fact/cfact*60e3,2)+'        '+numdec(f*fact/cfact*120e3,2)



  stop
  return
end 
pro plot_types,time,tstart,tstop,y,yerr,type,_extra=_extra,over=over

  wt=where(type eq 0 or type eq 5,nwt)
  pc=where(type eq 1,npc)
  c=where(type eq 2,nc)

  if not keyword_set(over) then plot,time,y,psym=3,_extra=_extra
  if nwt gt 0 then begin 
     oploterror,time[wt],y[wt],yerr[wt],errcolor=!blue,/nohat,psym=3
     for i=0,nwt-1 do oplot,[tstart[wt[i]],tstop[wt[i]]],[y[wt[i]],y[wt[i]]],color=!blue
  endif 
  if npc gt 0 then begin 
     oploterror,time[pc],y[pc],yerr[pc],errcolor=!red,/nohat,psym=3
     for i=0,npc-1 do oplot,[tstart[pc[i]],tstop[pc[i]]],[y[pc[i]],y[pc[i]]],color=!red
     ul=where(yerr eq 0,nul)
     if nul gt 0 then begin 
        plotsym,1,1,color=!red
        plots,time[ul],y[ul],yerr[ul],color=!red,psym=8
     endif 
  endif 
  if nc gt 0 then begin 
     oploterror,time[c],y[c],yerr[c],errcolor=!green,/nohat,psym=3
     for i=0,nc-1 do oplot,[tstart[c[i]],tstop[c[i]]],[y[c[i]],y[c[i]]],color=!green
     plotsym,0,1,/fill
     oplot,time[c],y[c],color=!green,psym=8
  endif 
return 
end
pro plot_110731

  cd,'~/Chandra/GRB110731A'
  begplot,name='GRB110731A_chandra_predict.ps',/color
;  tc=17452800d ;; Feb 18
  tc=dindgen(100)*10.*86400.
  !x.margin=[2,0]
  !y.margin=[0,0]
  db=ymd2dn(2011,07,31) ;; 110731
  t1=(ymd2dn(2012,3,1)+365-db)*86400.
  t2=(ymd2dn(2012,5,1)+365-db)*86400.
  t3=(ymd2dn(2012,7,1)+365-db)*86400.
  ts=[t1,t2,t3]
  mon=['March','May','July']

  fluxfact=9.14e-12
  xrange=[1e4,1e8]
erase 
  lc=lcout2fits('lc_newout_chandra_4p.txt')
  wt=where(lc.type eq 0)
  pc=where(lc.type eq 1)
  c=where(lc.type eq 2)
  spec=mrdfits('UL_specfits.fits',1)
  multiplot,[1,5],/init
;  plot_like_qdp,flux=1.000001,file='lc_newout_chandra_4p.txt',/nohard,yrange=[1e-16,1e-8],xrange=xrange,/xsty,/ysty
  ;; PLOT FLUX
  multiplot
  plot_types,lc.time,lc.tstart,lc.tstop,lc.src_rate,lc.src_rate_err,lc.type,xrange=xrange,/xlog,/ylog,ytitle='Flux',/xsty,charsize=1.,yrange=[1d-16,1d-11]
  
  read_lcfit,'lc_fit_out_idl_int7_xrt_spl.dat',pname,p1,p1err
  read_lcfit,'lc_fit_out_idl_int7_cxo_spl.dat',pname,p2,p2err
  read_lcfit,'lc_fit_out_idl_int7_xrt_bpl.dat',pname,p3,p3err

  t=[lc.time,p3[2],tc]
  t=t[sort(t)]
  oplot,t,pow(t,p1),color=!cyan,line=1
  oplot,t,pow(t,p2),color=!magenta,line=1
  oplot,t,bknpow(t,p3),color=!orange,line=1
;  oplot,[tc,tc],[1e-16,1e-8],line=2
  pm=!tsym.plusminus
  legend,!tsym.alpha+' = '+[numdec(p1[1],2)+pm+numdec(p1err[1,1],2),$
                            numdec(p2[1],2)+pm+numdec(p2err[0,1],2),$
                            numdec(p3[1],2)+pm+numdec(p3err[1,1],2)+' , '+$
                            numdec(p3[3],2)+pm+numdec(p3err[1,3],2)],$
         /bottom,/left,textcolor=[!cyan,!magenta,!orange],box=0,charsize=1.
  for i=0,2 do begin 
     oplot,[ts[i],ts[i]],[1e-20,1e-8],line=1
     xyouts,ts[i]*0.95,1e-13,mon[i],/data,orient=90,charsize=0.9
  endfor

  ;; PLOT COUNT RATE
  multiplot
  plot_types,lc[c].time,lc[c].tstart,lc[c].tstop,lc[c].src_rate/fluxfact,lc[c].src_rate_err/fluxfact,lc[c].type,xrange=xrange,/xlog,/ylog,ytitle='Count Rate',yrange=[1e-5,1e-2],/xsty,charsize=1.

  oplot,t,pow(t,p1)/fluxfact,color=!cyan,line=1
  oplot,t,pow(t,p2)/fluxfact,color=!magenta,line=1
  oplot,t,bknpow(t,p3)/fluxfact,color=!orange,line=1
  for i=0,2 do oplot,[ts[i],ts[i]],[1e-5,1],line=1

  ;; PLOT COUNTS
  exptime=120d3 
  multiplot
  fact=fluxfact/exptime*1.0568521
  plot,xrange,[0,80],/nodata,xrange=xrange,/xlog,ytitle='Scaled Counts',/ylog,yrange=[1,1000],/xsty,charsize=1.
  oploterror,lc[c].time,lc[c].src_counts*exptime/lc[c].exptime,sqrt(lc[c].src_counts)*exptime/lc[c].exptime,errcolor=!green,psym=8,/nohat,color=!green
  oplot,t,pow(t,p1)/fact,color=!cyan,line=1
  oplot,t,pow(t,p2)/fact,color=!magenta,line=1
  oplot,t,bknpow(t,p3)/fact,color=!orange,line=1
  legend,/bottom,/left,box=0,'exposure time = 120 ks',charsize=1
  for i=0,2 do oplot,[ts[i],ts[i]],[1,1e3],line=1
  
  ;; PLOT THETA_J
  eiso=5.98d53
  theta_j=jet_angle(t/86400.,z=2.83,eiso=eiso,/silent)
  multiplot
  plot,t,theta_j,xrange=xrange,/xlog,ytitle=!tsym.theta+'!Lj!N (deg)',/xsty,charsize=1.,yrange=[0,30]
  for i=0,2 do oplot,[ts[i],ts[i]],[0,40],line=1

  ;; PLOT E_GAMMA
  eiso=5.98d53
  egam=eiso*(1-cos(theta_j*!dtor))
  multiplot
  plot,t,egam,xrange=xrange,/xlog,/ylog,ytitle='E!L'+!tsym.gamma+'!N (erg)',yrange=[1d50,1d53],/ysty,/xsty,charsize=1.,xtitle='Time (s)'
  for i=0,2 do oplot,[ts[i],ts[i]],[1d48,1d54],line=1

  multiplot,/reset,/default
  endplot
  spawn,'convert GRB110731A_chandra_predict.ps GRB110731A_chandra_predict.pdf'
stop

  f=[pow(tc,p1),pow(tc,p2),bknpow(tc,p3)]
  ex=round(alog10(f)-0.5)
  flux=ntostr(f,4)+'x10!U'+ntostr(ex)+'!N'
  counts=numdec(60e3*f/9.14E-12,1)
  p=[p1[n_elements(p1)-1],p2[n_elements(p2)-1],p3[n_elements(p3)-1]]

  xyouts,2e6,1e-9,'Feb 18',/data
;  legend,['XRT SPL      ','CXO SPL      ','XRT BKNPL']+' f='+flux+', CTS(t=60 ks)='+counts+', a='+numdec(p,2),box=0,/bottom,/left,textcolor=[!cyan,!magenta,!orange],charsize=1
  legend,['WT','PC','Chandra'],box=0,/left,/center,textcolor=[!blue,!red,!green]

  endplot
stop
  return
end 
pro wrap_gfc,ps=ps

  cd,'~/GRBs/'
  grbs=file_search('GRB*/')
  ngrbs=n_elements(grbs)
  good=intarr(ngrbs)
  readcol,'~/Chandra/GRB_z.dat',sgrb,tid,t90,z,delim='|',format='(a,l,f,f)'
  sgrb=strcompress(sgrb,/remove)

  match,grbs,sgrb,m1,m2
  match,grbs+'A',sgrb,m1a,m2a
  m1=[m1,m1a]
  m2=[m2,m2a]
  s=sort(m1)
  m1=m1[s]
  m2=m2[s]
  
  wz=where(z[m2] ne 0 and t90[m2] gt 2.,ngrbs)
  grbs=grbs[m1[wz]]
  z=z[m2[wz]]
  t90=t90[m2[wz]]
  cf=dblarr(ngrbs)
  
  colprint,indgen(ngrbs),grbs
  print,'Start where?  g=?'
  g=0
  stop
  
  for i=g,ngrbs-1 do begin
     good[i]=good_for_chandra(grbs[i],z[i],cfratio=cfratio,ps=ps)
     cf[i]=cfratio
     if not keyword_set(ps) then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 
  wg=where(good eq 1)
  help,wg
  
  colprint,grbs[wg],z[wg],cf[wg]
stop
return
end 

function good_for_chandra,grb,z,cfratio=cfratio,ps=ps,dir=dir,lat=lat,lcfile=lcfile,plotanyway=plotanyway

  ;;; before running - need to mkdir, download LC & SPEC, run fit_lc,
  ;;;                  add to GRB_z.dat
  ;;;need to input grb name, z
  ;;; reads in lc_fit_out_idl_int7.dat, UL_specfits.fits, lc_newout_phil.txt

  if n_elements(dir) eq 0 then begin 
     cd,'~/GRBs/'
     cd,grb
  endif else cd,dir
  
  if keyword_set(ps) then begplot,name=grb+'_Good_for_chandra.ps',/land,/color
  good=0
  if n_elements(lcfile) eq 0 then lcfile='lc_fit_out_idl_int9.dat'
  if not exist(lcfile) or not exist('UL_specfits.fits') then begin
     print,'missing UL_specfits.fits'
     return,good
  endif 
  read_lcfit,lcfile,pnames,p
  np=n_elements(p)
  lc=lcout2fits(/phil)
  sfile='UL_specfits.fits'
  spec=mrdfits(sfile,1)
  gyr=strmid(grb,3,2)
  gmn=strmid(grb,5,2)
  gday=strmid(grb,7,2)
  gmet=date2met('20'+gyr+'-'+gmn+'-'+gday+'-00:00:00')
  days=round((today()-gmet)/86400.)
  t=lc.time
  t10=10.*86400d
  t21=21.*86400d
  t12=12.*86400d
  t30=30.*86400d
  ttd=days*86400d
;;;;;;SOMETHING IS WRONG WITH READ_PHIL IN LCOUT2FITS  
  lastobs=max(lc.tstop)

  case np of
     0: begin
        print,'missing LC fit'
        return,good
     end 
     2: mo='pow'
     4: mo='bknpow'
     6: mo='bkn2pow'
     8: mo='bkn3pow'
     10: mo='bkn4pow'
  endcase 
  if np eq 4 then t=[t,p[2]]
  if np eq 6 then t=[t,p[2],p[4]]  
  if np eq 8 then t=[t,p[2],p[4],p[6]]
  if np eq 10 then t=[t,p[2],p[4],p[6],p[8]]
  t=[t,ttd,1e8]
  t=t[sort(t)]
  nt=n_elements(t)
  pmax=p[np-1]
  if np ge 4 then $
     tmp=execute('nmax='+mo+'(p[np-2],p)/pow(p[np-2],[1.,pmax])') else nmax=p[0]

  cfratio=spec[n_elements(spec)-1].cfratio
  tmp=execute('f='+mo+'(t,p)*cfratio')
  tmp=execute('f10='+mo+'(t10,p)*cfratio')
  tmp=execute('f21='+mo+'(t21,p)*cfratio')
  tmp=execute('f12='+mo+'(t12,p)*cfratio')
  tmp=execute('f30='+mo+'(t30,p)*cfratio')
  tmp=execute('ftd='+mo+'(ttd,p)*cfratio')
  tmp=execute('ftd10='+mo+'(ttd+t10,p)*cfratio')
  tmp=execute('ftd30='+mo+'(ttd+t30,p)*cfratio')
  
  if keyword_set(plotanyway) then good=1 else good=0

  if (f10 ge 1e-14 and f21 ge 3e-15) or good eq 1 then begin 
     good=1
     
;     plot,t,f,/xlog,/ylog,xrange=[10,1e8],yrange=[1d-16,1e-8],/xsty,/ysty
     xrange=[10,1e8]
     yrange=[1e-15,1e-7]
     plot,xrange,yrange,/xlog,/ylog,/nodata,xtitle='Time Since BAT Trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',title=grb+' (z='+ntostr(z,4)+')',xrange=xrange,yrange=yrange,/xsty,/ysty
     oploterror,lc.time,lc.src_rate*cfratio,lc.src_rate_err*cfratio,psym=3,/nohat
     ul=where(lc.src_rate_err eq 0,nul)
     plotsym,1,3,thick=5
     if nul gt 0 then plots,lc[ul].time,lc[ul].src_rate*cfratio,psym=8
     for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]*cfratio
     oplot,t[0:nt-3],f[0:nt-3],color=!green
     oplot,t,f,color=!green,line=2
     if not keyword_set(lat) then begin 
        oplot,[t10,t10],yrange,line=1
        oplot,[10,1e8],[f10,f10],line=1
        oplot,[t21,t21],yrange,line=1,color=!blue
        oplot,[10,1e8],[f21,f21],line=1,color=!blue
     endif else begin
        oplot,[t12,t12],yrange,line=1
        oplot,[10,1e8],[f12,f12],line=1
        oplot,[t30,t30],yrange,line=1,color=!blue
        oplot,[10,1e8],[f30,f30],line=1,color=!blue
     endelse       
     if ttd lt 1e8 then begin 
        oplot,[ttd,ttd],yrange,line=1,color=!red
        oplot,[10,1e8],[ftd,ftd],line=1,color=!red
        oplot,[ttd+t10,ttd+t10],yrange,line=1,color=!red
        oplot,[10,1e8],[ftd10,ftd10],line=1,color=!red
        oplot,[ttd+t30,ttd+t30],yrange,line=1,color=!red
        oplot,[10,1e8],[ftd30,ftd30],line=1,color=!red
     endif 
     if not keyword_set(lat) then begin 
        oplot,[10,1e8],[1e-14,1e-14],line=2
        oplot,[10,1e8],[3e-15,3e-15],line=2
        xyouts,t10*0.2,1d-10,'10 days'
        xyouts,t21*1.2,1d-10,'21 days',color=!blue
        xyouts,20,1.2e-14,'XRT limit @ >10 days'
        xyouts,20,3.5e-15,'Chandra limit @ >21 days'
    
     endif else begin 
        oplot,[10,1e8],[1e-14,1e-14],line=2
        oplot,[10,1e8],[3e-15,3e-15],line=2
        oplot,[10,1e8],[5e-15,5e-15],line=2
        oplot,[10,1e8],[1e-15,1e-15],line=2
        xyouts,t12*0.3,1d-10,'12 days'
        xyouts,t30*1.2,1d-10,'30 days',color=!blue
        xyouts,20,1.5e-14,'Flux @ 12 days'
        xyouts,20,1.5e-15,'Flux @ 30 days'

     endelse 
     
    
     if ttd lt 1e8 then xyouts,ttd*1.2,1d-11,'Today',color=!red
;     oplot,t,pow(t,[nmax,pmax])*cfratio,color=!purple,line=2
     nml=round(alog10(nmax*cfratio)-0.5)
     nm=nmax*cfratio/10.^nml
     if nml ne 0 then nmp=ntostr(nm,4)+'x10!U'+ntostr(fix(nml))+'!N' else nmp=ntostr(nm,4)
     f=[1e-14,8e-15,5e-15,3e-15,1e-15]
     x=round(alog10(f)-0.4)
     tlim=(f/(nmax[0]*cfratio[0]))^(-1./pmax[0])
     ;f=n*t^-alpha
     ;t=f^(-1/alpha)/n
;     print,nmax*cfratio,nml,nm
     legend,['Final Slope','Norm = '+nmp,!tsym.alpha+' = '+ntostr(pmax,4)],box=0,/top,/right
     legend,['T-T0 (F='+ntostr(f*1e15,1)+'x10!U'+ntostr(x)+'!N)='+numdec(tlim/1e6,2)+'x10!U6!N s = '+numdec(tlim/86400.,1)+' days'],/top,/left,box=0

  endif 

  print,grb
  print,'Last Observation @ '+ntostr(lastobs/86400.)+' days'
  print,'Final Decay index = '+ntostr(p[np-1])
  if not keyword_set(lat) then begin 
     print,'Flux @ 10 days = '+ntostr(f10)+' erg cm-2 s-1'
     print,'Flux @ 21 days = '+ntostr(f21)+' erg cm-2 s-1'
  endif else begin 
     print,'Flux @ 12 days = '+ntostr(f12)+' erg cm-2 s-1'
     print,'Flux @ 30 days = '+ntostr(f30)+' erg cm-2 s-1'
  endelse 
  print,'Flux Today @ '+ntostr(days)+' days = '+ntostr(ftd)+' erg cm-2 s-1'
  print,'Today is '+ntostr(days)+' since the burst'
  print,'Flux Today + 10 days @ '+ntostr(days+10)+' days = '+ntostr(ftd10)+' erg cm-2 s-1'
  print,'Flux Today + 30 days @ '+ntostr(days+30)+' days = '+ntostr(ftd30)+' erg cm-2 s-1'

  if good then $
     print,'Good for Chandra @ 21 days!  z='+ntostr(z) $
  else print,'Not good for Chandra follow-up'
  if n_elements(dir) eq 0 then cd,'..'
  if keyword_set(ps) then begin
     endplot
     ps2pdf,grb+'_Good_for_chandra.ps'
  endif 

  return,good
end 
