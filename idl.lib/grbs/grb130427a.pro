@fit_functions
pro pass8,ps=ps

  trigtime=388741629.42d;388741677.51-50.d
  p8=mrdfits('~/GRBWorkdir/GRB130427324/pass8/gll_ft1_tr_bn130427324_v00_filt.fit',1)
  p7=mrdfits('~/GRBWorkdir/GRB130427324/gll_ft1_tr_bn130427324_v00_filt.fit',1)
  if keyword_set(ps) then begplot,name='grb130427a_p8.ps',/land,/color
  mra=median(p8.ra)
  mdec=median(p8.dec)
  dist=separation(p8.ra,p8.dec,mra,mdec)/3600.
  w=where(dist lt 8.)
;  p8=p8[w]
  dist=separation(p7.ra,p7.dec,mra,mdec)/3600.
  w=where(dist lt 8.)
;  p7=p7[w]

  plot,indgen(10),/nodata,xrange=[1e-2,2e5],yrange=[1,1e3],/xsty,/ysty,/xlog,/ylog,xtickname=replicate(' ',8),ytickname=replicate(' ',4)
  plotsym,0,1,/fill
  m1=0 & m2=0 & dm1=0
  for i=0,n_elements(p8)-1 do begin
     t=abs(p8[i].time-p7.time)
     mt=min(t,wt)
     if mt lt 0.01 and p8[i].energy/p7[wt].energy gt 0.9 and p8[i].energy/p7[wt].energy lt 1.1 then begin
        m1=[m1,i]
        m2=[m2,wt]
     endif else begin
        dm1=[dm1,i]
     endelse 
  endfor 
  m1=m1[1:*] & m2=m2[1:*] & dm1=dm1[1:*]

;  oplot,p7.time-trigtime,p7.energy/1e3,psym=8,color=!red
  oplot,p8[dm1].time-trigtime,p8[dm1].energy/1e3,psym=2,color=!cyan,symsize=1.5
  if keyword_set(ps) then begin
     endplot
     spawn,'ps2pdf grb130427a_p8.ps'
  endif 

stop
return
end

pro main_press_lc,indiv=indiv

  if not keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 
  !p.charsize=1
  plotsym,0,1,/fill
  ;;; GBM

  ;;; BAT
  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_BAT.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

  readcol,'~/Desktop/GRB130427A/NuSTAR/bat_flux_timedel1_with_gamma_BATBAND.qdp',time,timepos,timeneg,flux,fluxpos,fluxneg,format='(d,d,d,d,d,d)'
  blc=create_struct('time',0d,'tstart',0d,'tstop',0d,'flux',0d,'fluxerr',dblarr(2),'fdens',0d)
  blc=replicate(blc,n_elements(time))
  blc.time=time+51.1
  blc.tstart=time+timeneg
  blc.tstop=time+timepos
  blc.flux=flux
  blc.fluxerr[0]=-fluxneg
  blc.fluxerr[1]=fluxpos
  blc.fdens=flux2jy(blc.flux,2.1,low=15,high=150,eeff=50.)*1e3
  blc=blc[0:501]
  oplot,blc.time,blc.fdens,psym=8,color=!green
  xyouts,0.4,0.6,'Swift BAT 50 keV',color=!green
  if keyword_set(indiv) then endplot

  ;;; XRT
  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_XRT.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

;  xlc=mrdfits('~/Desktop/GRB130427A/NuSTAR/xlc.fits',1)
  xlc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE_binned2.qdp',wtfile='~/Desktop/GRB130427A/WTCURVE.qdp',/phil)
  xlc.time=xlc.time+51.1
  spec=mrdfits('~/Desktop/GRB130427A/UL_specfits.fits',1)
  fd=flux2jy(xlc.src_rate*spec[1].unabs_cfratio,1.8,low=0.3,high=10.0,eeff=2.)*1e3
  oplot,xlc.time,fd,psym=8,color=!cyan
  xyouts,3e3,2e-2,'Swift XRT 2 keV',color=!cyan
  if keyword_set(indiv) then endplot


  ;;; NuSTAR

  nlc1=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',1)  ;; 3-10
  nlc2=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',2)  ;; 10-30
  nlc3=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',3)  ;; 30-79
  nlc4=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',4)  ;; 3-79
  gamma=1.67
  fluxdens1=flux2jy(nlc1.flux,gamma,low=3.,high=10.,eeff=5.)*1e3
  fluxdens2=flux2jy(nlc2.flux,gamma,low=10.,high=30.,eeff=20)*1e3
  fluxdens3=flux2jy(nlc3.flux,gamma,low=30.,high=79.,eeff=50.)*1e3
  fluxdens4=flux2jy(nlc4.flux,gamma,low=3.,high=79.,eeff=15.)*1e3
  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_NuSTAR_5keV.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

  oplot,nlc1.time,fluxdens1,psym=8,color=!violet
  xyouts,8e3,6e-5,'NuSTAR 5 keV',color=!violet
  if keyword_set(indiv) then endplot

  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_NuSTAR_20keV.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 
  oplot,nlc2.time,fluxdens2,psym=8,color=!salmon
  xyouts,8e3,4e-5,'NuSTAR 20 keV',color=!salmon
  if keyword_set(indiv) then endplot

  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_NuSTAR_50keV.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

  oplot,nlc3.time,fluxdens3,psym=8,color=!deeppink
;  oplot,nlc4.time,fluxdens4,psym=8,color=!purple
  xyouts,8e3,2.5e-5,'NuSTAR 50 keV',color=!deeppink
;  xyouts,2e4,2e-5,'NuSTAR 30 keV',color=!purple
  if keyword_set(indiv) then endplot


  ;;; UVOT
;  readcol,'~/Desktop/GRB130427A/NuSTAR/GRB130427A_UVOT_lc.txt',time,exp,rate,ratecorr,rateerr,mag,magcorr,magerr0,magerr1,magul,flux,fluxcorr,fluxerr0,fluxerr1,fluxUL,filt,format='(d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,a)'
  readcol,'~/Desktop/GRB130427A/press/GRB130427A_UVOT_flux_lc.txt',time,exp,flux,fluxerr,bang,filt,format='(f,f,f,f,a,a)'
  filter=['v','b','u','uvw1','uvm2','uvw2','white']
  lam_eff=[5402,4329,3501,2634,2231,2030,3471]*1d-8 ;; cm
  c=3d10                                               ;; cm/s
  freq=c/lam_eff

  ulc=create_struct('time',0d,'tstart',0d,'tstop',0d,'exp',0.,'ctr',0.,'ctrerr',0.,'mag',0.,'magerr',fltarr(2),'flux',0d,'fluxerr',0d,'filter','','fdens',0d)
  ulc=replicate(ulc,n_elements(time))
  ulc.time=time+51.1
  ulc.tstart=time-exp
  ulc.tstop=time+exp
  ulc.exp=exp*2.
;  ulc.ctr=ratecorr
;  ulc.ctrerr=rateerr
  ulc.filter=filt
  ulc.fdens=flux

;  for i=0,n_elements(time)-1 do begin
;     w=where(strtrim(filt[i],2) eq filter or strtrim(filt[i],2) eq strupcase(filter),nwf)
;     ulc[i].flux=ratecorr[i]*fluxfact[w[0]]*lam_eff[w[0]];*1d31*lam_eff[w[0]]^2/c
;     ulc[i].fluxerr=rateerr[i]*fluxfact[w[0]]*lam_eff[w[0]];*1d31*lam_eff[w[0]]^2/c
;  endfor 
  
;  ulc.fdens=uvot2jy(ulc.flux/1e3,ulc.filter,/flux,freq=freq)
;  filt=ulc[rem_dup(ulc.filter)].filter
;  freq=freq[rem_dup(ulc.filter)]
;  ord=[6,5,0,1,3,2,4]
  ord=[0,1,2,6,3,4,5]
  filter=filter[ord]
  freq=freq[ord]
  h=4.135e-15 ;; ev*s
  eng=freq*h
  y=[35,25,18,13,9,6,4]
  for i=0,6 do begin 
     if keyword_set(indiv) then begin 
        begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_UVOT_'+strtrim(filter[i],2)+'.ps',/color,font='helvetica'
        plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
     endif 
     color=[!p.color,!firebrick,!darkred,!sienna,!red,!orange,!yellow]

     w=where(ulc.filter eq strupcase(filter[i]),nw)
     oplot,ulc[w].time,ulc[w].fdens,color=color[i],psym=8
     xyouts,5e3,y[i],'UVOT '+filter[i]+' '+numdec(eng[i],1)+' eV',color=color[i]
     if keyword_set(indiv) then endplot

  endfor 
;     oplot,ulc.time,ulc.fdens,color=!orange,psym=8

  ;;; LAT
  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_LAT.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

  readcol,'~/Desktop/GRB130427A/NuSTAR/LAT_lightcurve.txt',time,timerr0,timerr1,eflux,efluxerr,pflux,pfluxerr,pind,pinderr,format='(d,d,d,d,d,d,d,d,d)'
  llc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'ts',0d,'pind',0d,'pinderr',0d,'flux',0d,'fluxerr',0d,'fdens',0d)
  llc=replicate(llc,n_elements(time))
  llc.tstart=time-timerr0
  llc.tstop=time+timerr1
  llc.time=time;(llc.tstop-llc.tstart)/2.+llc.tstart
  llc.ctr=pflux
  llc.ctrerr=pfluxerr
  llc.pind=pind
  llc.pinderr=pinderr
  llc.flux=eflux
  llc.fluxerr=efluxerr
  eav=qpint1d('x*x^(-2.2)',100e3,300e6,/expr)/qpint1d('x^(-2.2)',100e3,300e6,/expr)
  eav=round(eav*1e-5)*1e5
  llc.fdens=flux2jy(llc.flux,2.2,low=100e3,high=300e6,eeff=eav)*1e3*1e2
  oplot,llc.time,llc.fdens,psym=8,color=!magenta
  xyouts,100,2e-4,'Fermi LAT 500 MeV (x 100)',color=!magenta
  if keyword_set(indiv) then endplot
  ;; Ground-based optical

  if keyword_set(indiv) then begin 
     begplot,name='~/Desktop/GRB130427A/press/press_LC_plot_ground.ps',/color,font='helvetica'
     plot,[0.1,1e7],[1e-5,1e4],/nodata,xtitle='Seconds since Fermi GBM trigger',ytitle='Brightness (mJy)',/xlog,/ylog,xrange=[0.1,1e7],yrange=[1e-5,1e4],/xsty,/ysty,yticks=9,ytickformat='loglabels',xminor=9,yminor=9,xticks=8,charsize=1
  endif 

  readcol,"GRB130427A_r\'-filter_data.txt",time,exp,flux
  oplot,time,flux,psym=8,color=!purple
  xyouts,1e5,1,"Ground based r'",color=!purple,charsize=1

  if keyword_set(indiv) then endplot else begin 
     endplot
     spawn,'ps2pdf ~/Desktop/GRB130427A/press/press_LC_plot.ps ~/Desktop/GRB130427A/press/press_LC_plot.pdf'
  endelse


stop
  return
end 

pro nustar_press_plot
  
  nlc1=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',1)  ;; 3-10
  nlc2=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',2)  ;; 10-30
  nlc3=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',3)  ;; 30-79
  nlc4=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',4)  ;; 3-79

  time=nlc4.time
  fluxdens=flux2jy(nlc4.flux,1.67,low=3.,high=79.,eeff=10.)*1e3

  plotsym,0,1,/fill
  plot,time,fluxdens,/xlog,/ylog,psym=8

  begplot,name='~/Desktop/GRB130427A/press/nustar_15_kev.ps',/color,/land,font='helvetica'
  time=nlc4.time
  fluxdens=flux2jy(nlc4.flux,1.67,low=3.,high=79.,eeff=15.)*1e3
  plotsym,0,1,/fill
  plot,time,fluxdens,/xlog,/ylog,psym=8,xtitle='Seconds since Fermi GBM trigger',ytitle='Flux Density (mJy)',xrange=[1e4,5e6],yrange=[1e-5,1e-3]
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/press/nustar_15_kev.ps ~/Desktop/GRB130427A/NuSTAR/press/nustar_15_kev.pdf'

  begplot,name='~/Desktop/GRB130427A/press/nustar_50_kev.ps',/color,/land,font='helvetica'
  time=nlc3.time
  fluxdens=flux2jy(nlc3.flux,1.67,low=30.,high=79.,eeff=50)*1e3
  plotsym,0,1,/fill
  plot,time,fluxdens,/xlog,/ylog,psym=8,xtitle='Seconds since Fermi GBM trigger',ytitle='Flux Density (mJy)',xrange=[1e4,5e6],yrange=[1e-5,1e-3]
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/press/nustar_50_kev.ps ~/Desktop/GRB130427A/NuSTAR/press/nustar_50_kev.pdf'


  return
end 

pro plot_new_lc

  ;; need to plot Xray, LAT, UVOT, LT, Nustar on single plot, with
  ;; inset of current figure

  readcol,'~/Desktop/GRB130427A/NuSTAR/lat/LAT_extrapolation_po.txt',ltime,lflux,lfluxerr0,lfluxerr1
  nlc1=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',1)
  nlc2=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',2)
  nlc3=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',3)
  nlc4=mrdfits('~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',4)
  xlc=mrdfits('~/Desktop/GRB130427A/NuSTAR/xlc.fits',1)
;  blc=mrdfits('~/Desktop/GRB130427A/00554620000-bat/lc/sw00554620000b_1chan_1s.lc',1)
  readcol,'~/Desktop/GRB130427A/NuSTAR/bat_flux_timedel1_with_gamma_BATBAND.qdp',time,timepos,timeneg,flux,fluxpos,fluxneg,format='(d,d,d,d,d,d)'
  blc=create_struct('time',0d,'tstart',0d,'tstop',0d,'flux',0d,'fluxerr',dblarr(2))
  blc=replicate(blc,n_elements(time))
  blc.time=time
  blc.tstart=time+timeneg
  blc.tstop=time+timepos
  blc.flux=flux
  blc.fluxerr[0]=-fluxneg
  blc.fluxerr[1]=fluxpos

  readcol,'~/Desktop/GRB130427A/NuSTAR/GRB130427A_UVOT_lc.txt',time,exp,rate,ratecorr,rateerr,mag,magcorr,magerr0,magerr1,magul,flux,fluxcorr,fluxerr0,fluxerr1,fluxUL,filt,format='(d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,a)'
  filter=['v','b','u','uvw1','uvm2','uvw2','white']
  lam_eff=[5402,4329,3501,2634,2231,2030,3471];*1d-8 ;; cm
  fluxfact=[2.614,1.472,1.63,4.00,8.50,6.2,0.37]*1d-16 ;; erg cm-2 s-1 ang-1
  c=3d10
  ulc=create_struct('time',0d,'tstart',0d,'tstop',0d,'exp',0.,'ctr',0.,'ctrerr',0.,'mag',0.,'magerr',fltarr(2),'flux',0d,'fluxerr',0d,'filter','')
  ulc=replicate(ulc,n_elements(time))
  ulc.time=time
  ulc.tstart=time-exp
  ulc.tstop=time+exp
  ulc.exp=exp*2.
  ulc.ctr=ratecorr
  ulc.ctrerr=rateerr
  ulc.filter=filt

  for i=0,n_elements(time)-1 do begin
     w=where(strtrim(filt[i],2) eq filter or strtrim(filt[i],2) eq strupcase(filter),nwf)
     ulc[i].flux=ratecorr[i]*fluxfact[w[0]]*lam_eff[w[0]];*1d31*lam_eff[w[0]]^2/c
     ulc[i].fluxerr=rateerr[i]*fluxfact[w[0]]*lam_eff[w[0]];*1d31*lam_eff[w[0]]^2/c
  endfor 

  llc=mrdfits('~/Desktop/GRB130427A/NuSTAR/lat/latlc.fits',1)
  w=where(llc.time ge 10)
  llc=llc[w]
;  readcol,'~/Desktop/GRB130427A/NuSTAR/fitted_curves_energy_flux.txt',lattime,latflux,latfluxerr,format='(d,d,d)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/lat_lcs.dat',starttime,stoptime,ts,pflux,pfluxerr,pind,pinderr,eflux,efluxerr,format='(d,d,d,d,d,d,d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/LAT_lightcurve.txt',time,timerr0,timerr1,eflux,efluxerr,pflux,pfluxerr,pind,pinderr,format='(d,d,d,d,d,d,d,d,d)'
  llc2=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'ts',0d,'pind',0d,'pinderr',0d,'flux',0d,'fluxerr',0d)
  llc2=replicate(llc2,n_elements(time))
  llc2.tstart=time-timerr0
  llc2.tstop=time+timerr1
  llc2.time=time;(llc2.tstop-llc2.tstart)/2.+llc2.tstart
  llc2.ctr=pflux
  llc2.ctrerr=pfluxerr
  llc2.pind=pind
  llc2.pinderr=pinderr
  llc2.flux=eflux
  llc2.fluxerr=efluxerr

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')
  nu3=date2met('2013:122:02:01:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=(388741688d)-51.
  tnu1=nu1-battrig
  tnu2=nu2-battrig+1000
  tnu3=nu3-battrig
  tnu6=nu6-battrig

  tnustart=[tnu1,tnu3]
  tnustop=[tnu2,tnu6]

  ;; xrt 0.3-10 keV
  rate0=[0.282686,9.04322E-02]
  flux0=[2.3293e-11,3.852e-12]
  flux0err=[1e-12,1.7e-13]
  xfact=[2.057,1.12] ;; pucorr fact

  begplot,name='~/Desktop/GRB130427A/NuSTAR/context_lc.eps',/encap,/color,/land,font='helvetica'
  !x.margin=[15,0]
  yrange=[1e-14,1e-2]
  xrange=[10,1e6]
  multiplot,[1,1],/init
  multiplot
  plot,xrange,yrange,/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',/nodata,xrange=xrange,/xsty,/ysty,yticks=11,yminor=9,charsize=1.5,yrange=yrange,ytickv=10^(dindgen(12)-13);,xticks=1,xtickname=[' ',' '],xminor=0

  xr2=[1.005e5,4.98e5]
  yr2=[1.03e-12,2.9e-10]

  oplot,[2.22e3,1e5],[4e-8,2.7e-10],line=2
  oplot,[5.6e5,5.e5],[4e-8,2.7e-10],line=2
  polyfill,[xr2,reverse(xr2),xr2[0]],[yr2[0],yr2[0],yr2[1],yr2[1],yr2[0]],color=!grey90
  oplot,[xr2,reverse(xr2),xr2[0]],[yr2[0],yr2[0],yr2[1],yr2[1],yr2[0]],thick=3
;  axis,xticks=4,xtickv=[1e5,2e5,3e5,4e5,5e5],xtickname=['10!U5!N','2'+!tsym.times+'10!U5!N','3'+!tsym.times+'10!U5!N','4'+!tsym.times+'10!U5!N','5'+!tsym.times+'10!U5!N'],charsize=1.5

  days=[2,3,4,5]*86400.
;  axis,xaxis=1,xtickv=days,charsize=1.5,xtickname=['2','3','4','5'],xticks=3,xminor=0
;  xyouts,1.65e5,6e-10,'Time since GBM Trigger (days)',/data,charsize=1.5
  x=[llc.time,reverse(llc.time),llc[0].time]
  y=[llc.flux+llc.fluxerr[1],reverse(llc.flux-llc.fluxerr[0]),llc[0].flux+llc[0].fluxerr]
  oplot,x,y,color=!hotpink
  polyfill,x,y,color=!hotpink,/line_fill,orient=45
  plotsym,0,1,/fill
  oploterror,llc2.time,llc2.flux,llc2.fluxerr,psym=8,/nohat,color=!magenta,symsize=0.7
  for i=0,n_elements(llc2)-1 do oplot,[llc2[i].tstart,llc2[i].tstop],[llc2[i].flux,llc2[i].flux],color=!magenta
  leflux=[1.47e-10,2.01e-10]
  tle=[mean([tnu1,tnu2]),mean([tnu3,tnu6])]
  plotsym,1,3,thick=5
  oplot,tle,leflux,psym=8,color=!magenta,thick=5
  oplot,[tnu1,tnu2],[leflux[0],leflux[0]],color=!magenta,thick=5
  oplot,[tnu3,tnu6],[leflux[1],leflux[1]],color=!magenta,thick=5

;  plotsym,0,1
;  plot,llc.time,llc.flux,color=!magenta,psym=8

;  oploterror,xlc.time,xlc.src_rate*spec[1].unabs_cfratio,xlc.src_rate_err*spec[1].unabs_cfratio,/nohat,psym=3,errcolor=!blue
;  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].src_rate,xlc[i].src_rate]*spec[1].unabs_cfratio,color=!blue

  for i=0,n_elements(blc)-1 do begin
     oplot,[blc[i].tstart,blc[i].tstop],[blc[i].flux,blc[i].flux],color=!dodgerblue
     oplot,[blc[i].time,blc[i].time],[blc[i].flux-blc[i].fluxerr[0],blc[i].flux+blc.fluxerr[1]],color=!dodgerblue
  endfor 
  oploterror,xlc.time,xlc.flux,xlc.fluxerr,/nohat,psym=3,errcolor=!blue
  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].flux,xlc[i].flux],color=!blue

  plotsym,1,1,thick=4
  ncolor=[!cyan,!green,!darkgreen,!purple]
  oploterror,nlc1.time,nlc1.flux,nlc1.fluxerr,/nohat,psym=3,errcolor=ncolor[0]
  for i=0,n_elements(nlc1)-1 do oplot,[nlc1[i].tstart,nlc1[i].tstop],[nlc1[i].flux,nlc1[i].flux],color=ncolor[0]
  oploterror,nlc2.time,nlc2.flux,nlc2.fluxerr,/nohat,psym=3,errcolor=ncolor[1]
  for i=0,n_elements(nlc2)-1 do oplot,[nlc2[i].tstart,nlc2[i].tstop],[nlc2[i].flux,nlc2[i].flux],color=ncolor[1]
  det=where(nlc3.flux-nlc3.fluxerr gt yrange[0])
  ul=where(nlc3.flux-nlc3.fluxerr lt yrange[0])
  nlc3[ul].flux=nlc3[ul].flux+nlc3[ul].fluxerr;*3
  oploterror,nlc3[det].time,nlc3[det].flux,nlc3[det].fluxerr,/nohat,psym=3,errcolor=ncolor[2]
  oplot,nlc3[ul].time,nlc3[ul].flux,psym=8,color=ncolor[2]
  for i=0,n_elements(nlc3)-1 do oplot,[nlc3[i].tstart,nlc3[i].tstop],[nlc3[i].flux,nlc3[i].flux],color=ncolor[2]
  oploterror,nlc4.time,nlc4.flux,nlc4.fluxerr,/nohat,psym=3,errcolor=ncolor[3]
  for i=0,n_elements(nlc4)-1 do oplot,[nlc4[i].tstart,nlc4[i].tstop],[nlc4[i].flux,nlc4[i].flux],color=ncolor[3]
  plotsym,0,1,/fill

  color=[!firebrick,!red,!red,!darkred,!orange,!salmon,!hotpink,!p.color]
;  color=!red
  model='pow'
  p=[1e-4,1.4]

  for i=2,2 do begin;n_elements(filter)-1 do begin 
     w=where(strtrim(ulc.filter,2) eq filter[i] or strtrim(ulc.filter,2) eq strupcase(filter[i]),nwf)
     wt=where(ulc[w].time gt tnustart[0] and ulc[w].time lt tnustop[1])
     if nwf gt 0 then begin 
        oploterror,ulc[w].time,ulc[w].flux,ulc[w].fluxerr,/nohat,psym=8,color=color[i],errcolor=color[i],symsize=0.7
        for j=0,nwf-1 do oplot,[ulc[w[j]].tstart,ulc[w[j]].tstop],[ulc[w[j]].flux,ulc[w[j]].flux],color=color[i]
        newpu=mpfitfun(model,ulc[w[wt]].time,ulc[w[wt]].flux,ulc[w[wt]].fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perroru)
        oplot,ulc[w[wt]].time,pow(ulc[w[wt]].time,newpu),color=!red,line=1
        pcerroru = perroru * sqrt(chisq / dof) ; scaled uncertainties
        print,filter[i],newpu,perroru,pcerroru,chisq/dof

     endif 
  endfor 


  a=!tsym.alpha
  pm=!tsym.plusminus
  xx=1.4
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4  

  legend,['!4Swift!X/UVOT !4u!X-band ('+a+'= 1.47 '+pm+' 0.17)',$
          '!4Swift!X/XRT 0.3-10 kev ('+a+'= 1.281 '+pm+' 0.004)',$
          '!4Swift!X/BAT 15-50 keV',$
          '!4NuSTAR!X 3-10 keV',$
          '!4NuSTAR!X 10-30 keV',$
          '!4NuSTAR!X 30-79 keV',$
          '!4NuSTAR!X 3-79 keV ('+a+'= 1.42 '+pm+' 0.02)',$
          '!4Fermi!X/LAT 100 MeV-100 GeV ('+a+'= 1.17 '+pm+' 0.06)'],$
         textcolor=[!red,!blue,!dodgerblue,ncolor,!magenta],box=0,charsize=1.2,/bottom,/left;position=[1.05e5,4e-12]


  yrange=[1e-12,3e-10]
  xrange=[1e5,5e5]
  x=!tsym.times
  plot,xrange,yrange,/nodata,/xlog,/ylog,position=[0.58,0.57,0.96,0.92],xtitle='Time since GBM Trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',xrange=xrange,/xsty,/ysty,yminor=9,charsize=1.,yrange=yrange,xticks=3,xtickv=[1e5,2e5,3e5,5e5],xtickname=['10!U5!N','2'+x+'10!U5!N','3'+x+'10!U5!N','5'+x+'10!U5!N'];,xtickname=['10!U5!N','2','3','4','5','6']
;  xyouts,6.3e5,7e-13,'x10!U5!N',charsize=1.

  xr2=[1.005e5,4.98e5]
  yr2=[1.03e-12,2.9e-10]
  polyfill,[xr2,reverse(xr2),xr2[0]],[yr2[0],yr2[0],yr2[1],yr2[1],yr2[0]],color=!grey90
  axis,yaxis=0,yrange=yrange,/ysty,/ylog,yminor=9,ytickname=[' ',' ',' ']
  axis,yaxis=1,yrange=yrange,/ysty,/ylog,yminor=9,ytickname=[' ',' ',' ']
  oplot,[4e5,4e5],[1e-12,1.1e-12]
  axis,xaxis=0,xrange=xrange,/xsty,/xlog,xtickname=[' ',' ',' ',' '],xtickv=[1e5,2e5,3e5,5e5],xticks=3
  plotsym,1,1,thick=4

  w=where(llc.time ge tnustart[0] and llc.time le 5e5)
  llc=llc[w]
  x=[tnustart[0],llc.time,reverse(llc.time),tnustart[0],tnustart[0]]
  y=[1.56e-10,llc.flux+llc.fluxerr[1],reverse(llc.flux-llc.fluxerr[0]),7.12e-11,1.56e-10]
  
  polyfill,x,y,color=!hotpink,/line_fill,orient=45
  oplot,x,y,color=!hotpink

  ncolor=[!cyan,!green,!darkgreen,!purple]
  oploterror,xlc.time,xlc.flux,xlc.fluxerr,/nohat,psym=3,errcolor=!blue
  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].flux,xlc[i].flux],color=!blue
  oploterror,nlc1.time,nlc1.flux,nlc1.fluxerr,/nohat,psym=3,errcolor=ncolor[0]
  for i=0,n_elements(nlc1)-1 do oplot,[nlc1[i].tstart,nlc1[i].tstop],[nlc1[i].flux,nlc1[i].flux],color=ncolor[0]
  oploterror,nlc2.time,nlc2.flux,nlc2.fluxerr,/nohat,psym=3,errcolor=ncolor[1]
  for i=0,n_elements(nlc2)-1 do oplot,[nlc2[i].tstart,nlc2[i].tstop],[nlc2[i].flux,nlc2[i].flux],color=ncolor[1]
  oploterror,nlc3[det].time,nlc3[det].flux,nlc3[det].fluxerr,/nohat,psym=3,errcolor=ncolor[2]
  oplot,nlc3[ul].time,nlc3[ul].flux,psym=8,color=ncolor[2]
  for i=0,n_elements(nlc3)-1 do oplot,[nlc3[i].tstart,nlc3[i].tstop],[nlc3[i].flux,nlc3[i].flux],color=ncolor[2]
  oploterror,nlc4.time,nlc4.flux,nlc4.fluxerr,/nohat,psym=3,errcolor=ncolor[3]
  for i=0,n_elements(nlc4)-1 do oplot,[nlc4[i].tstart,nlc4[i].tstop],[nlc4[i].flux,nlc4[i].flux],color=ncolor[3]
  plotsym,0,1,/fill

  ;;; XRT
  model='pow'
  p=[1e-4,1.4]
  newp0=mpfitfun(model,xlc.time,xlc.flux,xlc.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror0)
  oplot,xlc.time,pow(xlc.time,newp0),color=!blue,line=1
  pcerror0 = perror0 * sqrt(chisq / dof)             ; scaled uncertainties
  print,newp0,perror0,pcerror0,chisq/dof

  ;;; 3-10
  model='pow'
  p=[1e-4,1.4]
  newp1=mpfitfun(model,nlc1.time,nlc1.flux,nlc1.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror1)  
  oplot,nlc1.time,pow(nlc1.time,newp1),color=ncolor[0],line=1
  pcerror1 = perror1 * sqrt(chisq / dof)             ; scaled uncertainties
  print,newp1,perror1,pcerror1,chisq/dof

  ;; 10-30
  model='pow'
  p=[1e-4,1.4]
  newp2=mpfitfun(model,nlc2.time,nlc2.flux,nlc2.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror2)  
  oplot,nlc2.time,pow(nlc2.time,newp2),color=ncolor[1],line=1
  pcerror2 = perror2 * sqrt(chisq / dof)             ; scaled uncertainties
  print,newp2,perror2,pcerror2,chisq/dof

  ;; 30-79
  model='pow'
  p=[1e-4,1.4]
  newp3=mpfitfun(model,nlc3.time,nlc3.flux,nlc3.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror3)  
  oplot,nlc3.time,pow(nlc3.time,newp3),color=ncolor[2],line=1
  pcerror3 = perror3 * sqrt(chisq / dof)             ; scaled uncertainties
  print,newp3,perror3,pcerror3,chisq/dof

  ;; 3-79
  model='pow'
  p=[1e-4,1.4]
  newp4=mpfitfun(model,nlc4.time,nlc4.flux,nlc4.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror4)  
  oplot,nlc4.time,pow(nlc4.time,newp4),color=ncolor[3],line=1
  pcerror4 = perror4 * sqrt(chisq / dof)             ; scaled uncertainties
  print,newp4,perror4,pcerror4,chisq/dof

  ;;; LAT
  leflux=[1.47e-10,2.01e-10]
  tle=[mean([tnu1,tnu2]),mean([tnu3,tnu6])]
  plotsym,1,3,thick=5
  oplot,tle,leflux,psym=8,color=!magenta,thick=5
  oplot,[tnu1,tnu2],[leflux[0],leflux[0]],color=!magenta,thick=5
  oplot,[tnu3,tnu6],[leflux[1],leflux[1]],color=!magenta,thick=5


  xyouts,1.1e5,1.5e-12,'!4NuSTAR Intervals!X',charsize=1

  multiplot,/reset,/default
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/context_lc.eps ~/Desktop/GRB130427A/NuSTAR/context_lc.pdf'
  stop

return
end

pro onboard_plot,ra,dec,time,energy

  trigtime=(388741688d)-51.
;  RA = 11:32:32.82
;  Dec = +27:41:56.06
  bra=173.13675  ;;; CARMA
  bdec=27.698906

  gra=170.55
  gdec=47.716667
  gerr=3.35

;  Columns are photon #, RA, dec, time, energy

  if n_elements(ra) eq 0 then begin 
     readcol,'~/Desktop/GRB130427A/130427A_burstPhotons.dat',phonum,ra,dec,time,energy,format='(i,d,d,d,f)'
     time=time-trigtime
  endif 
  dist=separation(bra,bdec,ra,dec)/3600.
  w=where(time gt -10 and time lt 40 and dist lt 40)
  
  plotsym,8,0.5,/fill

;  begplot,name='~/Desktop/GRB130427A/LAT_onboard_plot.ps',/color,font='helvetica',/land
  plot,[0,1],[0,1],/nodata,/iso,xrange=[150,200],yrange=[10,60],charsize=2,xtitle='RA (deg)',ytitle='Dec (deg)'
  oplot,ra[w],dec[w],psym=8,color=!grey30
  plots,gra,gdec,color=!green,psym=2,symsize=2
  tvcircle,10.,gra,gdec,color=!green,thick=10
  plots,bra,bdec,psym=2,color=!magenta,symsize=2
  xyouts,202,50,'GBM Onboard Position',color=!green,charsize=1.5
  xyouts,202,40,'Real Burst Position',color=!magenta,charsize=1.5
;  endplot
;  spawn,'convert ~/Desktop/GRB130427A/LAT_onboard_plot.ps ~/Desktop/GRB130427A/LAT_onboard_plot.pdf'

  stop


return
end

pro errork

;  as=[1.36,1.23,1.22,1.14,1.28];,1.17]
;  aserr=[0.05,0.02,0.04,0.32,0.02];,0.06]
  as=[1.28,1.36]
  aserr=[0.02,0.05]
  aav=weighted_mean(as,aserr,err)
;  print,aav
;  print,err

  a=1.30
  aerr=0.05
  b=0.69
  berr=0.01
  dkda=8./((2*a-3*b)^2*(1./(2*a-3*b)+1.)^2)
  dkdb=12./((1/(2*a-3*b)+1)^2*(2*a-3*b)^2)
  kerr=sqrt(aerr^2*dkda^2+berr^2*dkdb^2)
  k=4./(1+1./(2*a-3*b))
  print,'a=',a
  print,'sig_a=',aerr
  print,'b=',b
  print,'sig_b',berr
  print,'k=',k
  print,'sig_k=',kerr
;  colprint,a,aerr,k,kerr
   
  stop

 return
end 

pro plot_seds_final

  h=4.1357d-15*(1e-3/1.)        ; keV s
  kev2erg=1.602e-12*1d3  ;; 1kev= # ergs
  lateng=[1e5,1e8]
  s=0.85
  p=[1.66767,1.75589]
  q=p+0.5
  q[1]=0
  b=[69.,0]
  norm=[3.98581E-06,5.83662E-04,7.67437E-07]

  begplot,name='~/Desktop/GRB130427A/NuSTAR/sed_plot_final.ps',/color,font='helvetica'
  multiplot,[1,2],/init
  lateng1=[[100e3,1e6],[1e6,10e6],[10e6,100e6]]

  for z=0,1 do begin
     if z eq 0 then begin 
        uf='~/Desktop/GRB130427A/NuSTAR/uf_ep1_sbpl_fixed.dat'
        ufa='~/Desktop/GRB130427A/NuSTAR/uf_ep1_sbpl_fixed_noabs.dat'
        readcol,'~/Desktop/GRB130427A/NuSTAR/covariance_sbpl_fixed_ep1.dat',be1c,pc,n1c,format='(f,f,f)'
;        readcol,'~/Desktop/GRB130427A/NuSTAR/contours.txt',ce1,ce2,ceav,cpf,c2sl,c2sh,c3sl,c3sh,format='(d,d,d,d,d,d,d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/2sigma_errM_sbpl.txt',ceav2m,cpf2m,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/2sigma_errP_sbpl.txt',ceav2p,cpf2p,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/3sigma_errM_sbpl.txt',ceav3m,cpf3m,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/3sigma_errP_sbpl.txt',ceav3p,cpf3p,format='(d,d)'

        c=[1.,0.9,0.9,1]
        yrange=[5e-13,1e-9]
        latgam=q       
        lateng=[100e3,100e6]
        leflux=[2.2e-11,6.2e-11,5.65e-10]
        mo='smbknpow'
        dp=6
     endif else begin         
        uf='~/Desktop/GRB130427A/NuSTAR/uf_ep2_pl.dat'
        ufa='~/Desktop/GRB130427A/NuSTAR/uf_ep2_pl_noabs.dat'
        uf2='uf_ep2_sbpl_borrowed.dat'
        uf2a='uf_ep2_sbpl_borrowed_noabs.dat'
        readcol,'~/Desktop/GRB130427A/NuSTAR/covariance_pl_ep2.dat',pc,n1c,format='(f,f)'
        c=[1,1.21,1.24,1]
        leflux=[2.27e-11,6.45e-11,3.72e-10]
        xtitle='Energy (keV)'
        yrange=[1e-13,1e-9]
        mo='pow'
        dp=7
        xtickformat='loglabels'
        readcol,uf2,eng2,engerr2,pe2,peerr2,mpe2,format='(a,a,a,a,a)',/silent
        readcol,uf2a,eng0,engerr0,pe0,peerr0,mpe02,format='(d,d,d,d,d)',/silent
        readcol,'~/Desktop/GRB130427A/NuSTAR/2sigma_errM.txt',plceav2m,plcpf2m,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/2sigma_errP.txt',plceav2p,plcpf2p,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/3sigma_errM.txt',plceav3m,plcpf3m,format='(d,d)'
        readcol,'~/Desktop/GRB130427A/NuSTAR/3sigma_errP.txt',plceav3p,plcpf3p,format='(d,d)'
     endelse 
     readcol,uf,eng,engerr,pe,peerr,mpe,format='(a,a,a,a,a)',/silent
     readcol,ufa,eng0,engerr0,pe0,peerr0,mpe0,format='(d,d,d,d,d)',/silent

     w=where(eng eq 'NO',nw) ;;opt1,xrt1,nu1a,nu1b,lat1,opt2,xrt2,nu2a,nu2b

     w0=indgen(w[dp])
     w1=indgen(w[dp+1]-w[dp]-1)+w[dp]+1
     w2=indgen(w[dp+2]-w[dp+1]-1)+w[dp+1]+1
     w3=indgen(n_elements(eng)-w[dp+2]-1)+w[dp+2]+1

     w0=w0[where(eng[w0] ne 'NO',nw0)]
     w1=w1[where(eng[w1] ne 'NO',nw1)]
     w2=w2[where(eng[w2] ne 'NO',nw2)]
     w3=w3[where(eng[w3] ne 'NO',nw3)]

     wg=[w0,w1,w2,w3]

     eng=eng[wg]*1.
     engerr=engerr[wg]*1.
     pe=pe[wg]*1.
     peerr=peerr[wg]*1.
     mpe=mpe[wg]*1.
;     mpe0=mpe0[wg]*1.
     if z eq 1 then begin 
        pe2=pe2[wg]*1.
        peerr2=peerr2[wg]*1.
        mpe2=mpe2[wg]*1.
;        mpe02=mpe02[wg]*1.
     endif 

     const=[replicate(c[0],nw0),replicate(c[1],nw1),replicate(c[2],nw2),replicate(c[3],nw3)]
     w0opt=where(eng[0:nw0-1] lt 0.1,nw0opt)
     w0x=where(eng[0:nw0-1] gt 0.1,nw0x)

     ;; define colors for different energies
     ccode=[replicate(0,nw0opt),replicate(1,nw0x),replicate(2,nw1+nw2),replicate(3,nw3)]

     ;; array of energies to compute model
     earr=[(dindgen(9)+1.)*1e-3,(dindgen(9)+1.)*1e-2,(dindgen(9)+1.)/10.,(dindgen(99)+1.),(dindgen(9)+1)*1e2,(dindgen(9)+1)*1e3,(dindgen(9)+1.)*1e4,(dindgen(9)+1.)*1e5,(dindgen(9)+1.)*1e6,(dindgen(10)+1.)*1e7]

     maxe=max(eng,m)
     n=n_elements(eng)
     w=indgen(m+1)
     nw=n_elements(w)
     colors=[!orange,!blue,!green,!yellow]
     color=lonarr(n)
     for c=0,3 do begin 
        c0=where(ccode eq c,nc0)
        color[c0]=colors[c]
     endfor 
     eav=dblarr(nw)
     eaverr=dblarr(2,nw)
     nufnu=dblarr(nw) & emin=dblarr(nw) & emax=dblarr(nw) & slope=fltarr(nw) & nufnuerr=dblarr(nw)
     nufnu2=nufnu & nufnuerr2=nufnu & eav2=eav & eaverr2=eaverr
     for i=0,nw-1 do begin
        emax[i]=eng[w[i]]+engerr[w[i]]
        emin[i]=eng[w[i]]-engerr[w[i]]
       
        if eng[w[i]] lt b[z] then a=-p[z] else a=-q[z]
        slope[i]=a
        if z eq 0 then begin 
           private=[norm[z],b[z],p[z],s,q[z]]
           eav[i]=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)
        endif else begin
           private=[norm[z],p[z]]
           eav[i]=qpint1d('x*p[0]*x^(-p[1])',emin[i],emax[i],/expr,private)/qpint1d('p[0]*x^(-p[1])',emin[i],emax[i],/expr,private)

           ;;; sbpl scaled down
           private=[norm[2],b[z],p[z],s,q[z]]
           eav2[i]=qpint1d('x*p[0]*x^(-p[1])',emin[i],emax[i],/expr,private)/qpint1d('p[0]*x^(-p[1])',emin[i],emax[i],/expr,private)
           nufnu2[i]=eav2[i]^2*pe2[w[i]]*kev2erg*mpe02[w[i]]/mpe2[w[i]]/const[w[i]]
           nufnuerr2[i]=eav2[i]^2*peerr2[w[i]]*kev2erg*mpe02[w[i]]/mpe2[w[i]]/const[w[i]]
           eaverr2[0,i]=eav2[i]-emin[i]
           eaverr2[1,i]=emax[i]-eav2[i]

        endelse 
        nufnu[i]=eav[i]^2*pe[w[i]]*kev2erg*mpe0[w[i]]/mpe[w[i]]/const[w[i]]
        nufnuerr[i]=eav[i]^2*peerr[w[i]]*kev2erg*mpe0[w[i]]/mpe[w[i]]/const[w[i]]
        eaverr[0,i]=eav[i]-emin[i]
        eaverr[1,i]=emax[i]-eav[i]

     endfor 
     print,max(eav)
     
     ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (erg cm!U-2!N s!U-1!N)'
     multiplot
     plot,[1e-3,1e10],[1e-13,1e-8],/nodata,/xlog,/ylog,xrange=[1e-3,1e8],xtitle=xtitle,ytitle=ytitle,/xsty,yrange=yrange,/ysty,xtickformat=xtickformat
     wy=where(color[w] eq !orange,ny)
     wb=where(color[w] eq !blue,nb)
     wg=where(color[w] eq !green,ng)
;     print,ny,nb,ng
     plotsym,0,0.8,/fill
;     if z eq 1 then begin
;        w50=where(eav2 lt 1e4,nw50)
;        oploterror,eav2[w50],nufnu2[w50],nufnuerr2[w50],color=!grey50,psym=3,/nohat,errcolor=!grey50
;        oplot,eav2[w50],nufnu2[w50],color=!grey50,psym=8,symsize=1
;        for j=0,nw50-1 do oplot,[eav2[w50[j]]-eaverr2[0,j],eav2[w50[j]]+eaverr2[1,j]],[nufnu2[w50[j]],nufnu2[w50[j]]],color=!grey50
;     endif 
    oploterror,eav[wy[0:1]],nufnu[wy[0:1]],nufnuerr[wy[0:1]],errcolor=!red,psym=3,/nohat
    oplot,eav[wy[0:1]],nufnu[wy[0:1]],color=!red,psym=8
    for j=0,1 do oplot,[eav[wy[j]]-eaverr[0,wy[j]],eav[wy[j]]+eaverr[1,wy[j]]],[nufnu[wy[j]],nufnu[wy[j]]],color=!red
    oploterror,eav[wy[2:*]],nufnu[wy[2:*]],nufnuerr[wy[2:*]],errcolor=!orange,psym=3,/nohat
    oplot,eav[wy[2:*]],nufnu[wy[2:*]],color=!orange,psym=8
    for j=2,ny-1 do oplot,[eav[wy[j]]-eaverr[0,wy[j]],eav[wy[j]]+eaverr[1,wy[j]]],[nufnu[wy[j]],nufnu[wy[j]]],color=!orange
    oploterror,eav[wb],nufnu[wb],nufnuerr[wb],psym=3,errcolor=!blue,/nohat
    for j=0,nb-1 do oplot,[eav[wb[j]]-eaverr[0,wb[j]],eav[wb[j]]+eaverr[1,wb[j]]],[nufnu[wb[j]],nufnu[wb[j]]],color=!blue
    oploterror,eav[wg],nufnu[wg],nufnuerr[wg],color=!green,psym=3,/nohat
    for j=0,ng-1 do oplot,[eav[wg[j]]-eaverr[0,wg[j]],eav[wg[j]]+eaverr[1,wg[j]]],[nufnu[wg[j]],nufnu[wg[j]]],color=!green


     if z eq 0 then begin 
        f=smbknpow(earr,[norm[z],p[z],b[z],q[z],s])
        private=[norm[z],b[z],p[z],s,q[z]]
        leav=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,private)
        lflux=5.65e-8 ;; ph/cm2/s from LAT input
        lfluxerr=3e-8

        private=[1.,b[z],p[z],s,q[z]]
        a=lflux/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng1[0,0],lateng1[1,0],/expr,private)
        par=[a,p[z],b[z],q[z],s]
        lnufnu=smbknpow(leav,par)*leav^2*kev2erg

        lnufnuerr=lfluxerr/lflux*lnufnu
        oplot,[leav,leav],[lnufnu-lnufnuerr,lnufnu+lnufnuerr],color=!magenta,line=2,thick=15
        oplot,[lateng[0],lateng[1]],[lnufnu,lnufnu],color=!magenta,line=2,thick=15
        f0=f
     endif else begin 
        f=pow(earr,[norm[z],p[z]])
        private=[norm[z],p[z]]
        leav=qpint1d('x*p[0]*x^(-p[1])',lateng[0],lateng[1],/expr,private)/qpint1d('p[0]*x^(-p[1])',lateng[0],lateng[1],/expr,private)
        sval=27 ;;1 keV
;        sval=95 ;;break energy
        f2=smbknpow(earr,[norm[2],p[0],b[0],q[0],s])
;        snorm=norm[2]/norm[0];f2[sval]/f0[sval]

     endelse 
     oplot,earr,earr^2*f*kev2erg
     
     ;; LAT UL
     av=dblarr(3)
     for g=0,2 do begin
        av[g]=sqrt(lateng1[0,g]*lateng1[1,g])
        oplot,[lateng1[0,g],lateng1[1,g]],[leflux[g],leflux[g]],color=!magenta,thick=5
     endfor 
     plotsym,1,4,thick=5

     plots,av,leflux,color=!magenta,psym=8

     if z eq 0 then begin 
     ;; SBPL error contour
        ebe=earr/b[z]
        ebe0=earr/b[0]
        ebe1=earr/b[1]
        dfdn0=(ebe0^(s*p[0])+ebe0^(s*q[0]))^(-1./s)
        dfdn1=(ebe1^(s*p[1])+ebe1^(s*q[1]))^(-1./s)
        dfdbe0=-norm[0]/s*(ebe0^(s*p[0])+ebe0^(s*q[0]))^(-1./s-1.)*(-q[0]*s*earr^(s*q[0])*b[0]^(-q[0]*s-1)-p[0]*s*earr^(s*p[0])*b[0]^(-p[0]*s-1.))
        dfdbe1=-norm[1]/s*(ebe1^(s*p[1])+ebe1^(s*q[1]))^(-1./s-1.)*(-q[1]*s*earr^(s*q[1])*b[1]^(-q[1]*s-1)-p[1]*s*earr^(s*p[1])*b[1]^(-p[1]*s-1.))
        dfdp=-norm[z]/s*(ebe^(s*(p[0]+0.5))+ebe^(p[0]*s))^(-1./s-1)*(s*ebe^(s*(p[0]+0.5))-alog(b[0])*s*ebe^(s*(p[0]+0.5))+s*ebe^(s*p[0])-alog(b[0])*s*ebe^(p[0]*s))
        
        mm=[[be1c],[pc],[n1c]]
        sigf2=dblarr(n_elements(earr))
        for ee=0,n_elements(earr)-1 do begin 
           dm=[[dfdbe0[ee]],[dfdp[ee]],[dfdn0[ee]]]
           sigf2[ee]=transpose(dm)##mm##dm
        endfor 
        sigf=sqrt(sigf2)
        sigf0=sigf
     endif else begin 

        dfdn1=earr^(-p[z])
        dfdp=-norm[0]*earr^(-p[z])
        mm=[[pc],[n1c]]
        sigf2=dblarr(n_elements(earr))

        for ee=0,n_elements(earr)-1 do begin 
           dm=[[dfdp[ee]],[dfdn1[ee]]]
           sigf2[ee]=transpose(dm)##mm##dm
        endfor         
        sigf=sqrt(sigf2)
     endelse 


;if z eq 1 then begin 
;     for u=2,3 do begin
;        y1=earr^2*(f+sigf*u)*kev2erg
;        y2=earr^2*(f-sigf*u)*kev2erg
;        oplot,earr,y1,line=4-u    ;,color=pcolor[z]
;        oplot,earr,y2,line=4-u    ;,color=pcolor[z]
;     endfor 
;  endif 
     if z eq 0 then begin 
        oplot,ceav2m,cpf2m,line=2
        oplot,ceav2p,cpf2p,line=2
        oplot,ceav3m,cpf3m,line=1
        oplot,ceav3p,cpf3p,line=1
;        oplot,ceav,cpf,color=!purple
     endif 

     if z eq 1 then begin ;; scale down fits from epoch 1
        
        oplot,earr,earr^2*f2*kev2erg,color=!grey50
     
        nn=norm[2]/norm[0]
        oplot,ceav2m,cpf2m*nn,line=2,color=!grey50
        oplot,ceav2p,cpf2p*nn,line=2,color=!grey50
        oplot,ceav3m,cpf3m*nn,line=1,color=!grey50
        oplot,ceav3p,cpf3p*nn,line=1,color=!grey50
        oplot,plceav2m,plcpf2m,line=2
        oplot,plceav2p,plcpf2p,line=2
        oplot,plceav3m,plcpf3m,line=1
        oplot,plceav3p,plcpf3p,line=1

;        for u=2,3 do begin
;           y1=earr^2*(f2+sigf0*u*norm[2]/norm[0])*kev2erg;*snorm
;           y2=earr^2*(f2-sigf0*u*norm[2]/norm[0])*kev2erg;*snorm
;           oplot,earr,y1,line=4-u,color=!grey50
;           oplot,earr,y2,line=4-u,color=!grey50
;        endfor 
     endif 

     legtime=['T0+ ~1.5 days','T0+ ~5 days']
     legend,legtime[z],box=0,/top,/center

     if z eq 0 then begin 
        plot,[5,100],[3e-12,3e-11],/nodata,/xlog,/ylog,position=[0.27,0.79,0.58,0.94],xticks=1,charsize=1,xtickv=[10,100],xtickname=['10','100'],xtitle='Energy (keV)',xrange=[3,100],yrange=[5e-12,2e-11],/xsty,/ysty,xminor=9
        oploterror,eav[wg],nufnu[wg],nufnuerr[wg],color=!green,psym=3,/nohat
        for j=0,ng-1 do oplot,[eav[wg[j]]-eaverr[0,wg[j]],eav[wg[j]]+eaverr[1,wg[j]]],[nufnu[wg[j]],nufnu[wg[j]]],color=!green
        oplot,earr,earr^2*f*kev2erg
;        for u=2,3 do begin
;           y1=earr^2*(f+sigf*u)*kev2erg
;           y2=earr^2*(f-sigf*u)*kev2erg
;           oplot,earr,y1,line=4-u ;,color=pcolor[z]
;           oplot,earr,y2,line=4-u ;,color=pcolor[z]
;        endfor 
        oplot,ceav2m,cpf2m,line=2
        oplot,ceav2p,cpf2p,line=2
        oplot,ceav3m,cpf3m,line=1
        oplot,ceav3p,cpf3p,line=1

        xyouts,22,5.5e-12,'!4NuSTAR!X',charsize=1
     endif

  endfor
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4  
  legend,['!4Fermi!X/LAT','!4NuSTAR!X','!4Swift!X/XRT','!4Swift!X/UVOT','Liverpool Telescope'],textcolor=[!magenta,!green,!blue,!orange,!red],box=0,/top,/left

  multiplot,/reset,/default
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/sed_plot_final.ps'
stop  

return
end

pro plot_seds
  readcol,'~/Desktop/GRB130427A/NuSTAR/uf_kev_2sed_wLAT_new4.dat',eng,engerr,pe,peerr,mpe,format='(a,a,a,a,a)',/silent
  readcol,'~/Desktop/GRB130427A/NuSTAR/uf_kev_2sed_wLAT_noabs_new4.dat',eng0,engerr0,pe0,peerr0,mpe0,format='(d,d,d,d,d)',/silent
;  readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance2.txt',p1ac,be1c,n1c,format='(f,f,f)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance.txt',be1c,pc,n1c,be2c,n2c,format='(f,f,f,f,f)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance4.txt',cf1c,be1c,pc,cf2c,be2c,format='(f,f,f,f,f)'


  w=where(eng eq 'NO',nw) ;;opt1,xrt1,nu1a,nu1b,lat1,opt2,xrt2,nu2a,nu2b

  w0=indgen(w[6])
  w1=indgen(w[7]-w[6]-1)+w[6]+1
  w2=indgen(w[8]-w[7]-1)+w[7]+1
  w3=indgen(w[9]-w[8]-1)+w[8]+1
  w4=indgen(w[17]-w[9]-1)+w[9]+1
  w5=indgen(w[18]-w[17]-1)+w[17]+1
  w6=indgen(w[19]-w[18]-1)+w[18]+1
  w7=indgen(n_elements(eng)-w[19]-1)+w[19]+1
  
  w0=w0[where(eng[w0] ne 'NO',nw0)]
  w1=w1[where(eng[w1] ne 'NO',nw1)]
  w2=w2[where(eng[w2] ne 'NO',nw2)]
  w3=w3[where(eng[w3] ne 'NO',nw3)]
  w4=w4[where(eng[w4] ne 'NO',nw4)]
  w5=w5[where(eng[w5] ne 'NO',nw5)]
  w6=w6[where(eng[w6] ne 'NO',nw6)]
  w7=w7[where(eng[w7] ne 'NO',nw7)]

  wg=[w0,w1,w2,w3,w4,w5,w6,w7]

  h=4.1357d-15*(1e-3/1.)        ; keV s

  eng=eng[wg]*1.
  engerr=engerr[wg]*1.
  pe=pe[wg]*1.
  peerr=peerr[wg]*1.
  mpe=mpe[wg]*1.

;  c=[1.,0.9,0.9,1,1.,1.246,1.277,1.]
  c=[1.,0.9,0.9,1,1,1.223,1.26,1]

  const=[replicate(c[0],nw0),replicate(c[1],nw1),replicate(c[2],nw2),replicate(c[3],nw3),replicate(c[4],nw4),replicate(c[5],nw5),replicate(c[6],nw6),replicate(c[7],nw7)]
  w0opt=where(eng[0:nw0-1] lt 0.1,nw0opt)
  w0x=where(eng[0:nw0-1] gt 0.1,nw0x)
  w4opt=where(eng[nw0+nw1+nw2+nw3:nw0+nw1+nw2+nw3+nw4-1] lt 0.1,nw4opt)
  w4x=where(eng[nw0+nw1+nw2+nw3:nw0+nw1+nw2+nw3+nw4-1] gt 0.1,nw4x)

  ccode=[replicate(0,nw0opt),replicate(1,nw0x),replicate(2,nw1+nw2),replicate(3,nw3),replicate(0,nw4opt),replicate(1,nw4x),replicate(2,nw5+nw6),replicate(3,nw7)]

  earr=[(dindgen(9)+1.)*1e-3,(dindgen(9)+1.)*1e-2,(dindgen(9)+1.)/10.,(dindgen(99)+1.),(dindgen(9)+1)*1e2,(dindgen(9)+1)*1e3,(dindgen(9)+1.)*1e4,(dindgen(9)+1.)*1e5,(dindgen(9)+1.)*1e6,(dindgen(10)+1.)*1e7]

  s=0.85
  p=[1.69,1.69]
  q=p+0.5
;  b=[152,22]
  b=[153,23.4]
;  cflux=10^[-10.4237,-11.5225]
  kev2erg=1.602e-12*1d3  ;; 1kev= # ergs
;  norm=[8.366e-7,3.983e-6] ;;; come back to this!
  ;;; norm=cflux/integral(e*N(E),100e3,100e6)/kev2erg where
  ;;; par=[1,be,p,s,q]
  lateng=[1e5,1e8]
;  norm0=cflux[0]/qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,[1.,b[0],p[0],s,q[0]])/kev2erg
;  norm1=cflux[1]/qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,[1.,b[1],p[1],s,q[1]])/kev2erg
;  norm=[norm0,norm1]
;  norm=[7.73459E-07,3.84497E-06]
;  norm=[9.17168E-07,4.88019E-06]
  norm=[9.12149e-7,4.39577E-06]
 
  maxe=max(eng,m)
  n=n_elements(eng)
  w1=indgen(m+1)
  w2=indgen(n-m-1)+m+1
  nw1=n_elements(w1)
  nw2=n_elements(w2)
  begplot,name='~/Desktop/GRB130427A/NuSTAR/sed_plot.ps',/color,font='helvetica'
  multiplot,[1,2],/init
  colors=[!orange,!blue,!green,!yellow];,!blue,!green,!p.color]
  color=lonarr(n)
  for c=0,3 do begin 
     c0=where(ccode eq c,nc0)
     color[c0]=colors[c]
  endfor 
  print,!orange,!blue,!green,!yellow
;  color=[!orange,!salmon]
;  pcolor=[!blue,!red]
  for z=0,1 do begin 
     multiplot
     if z eq 0 then begin 
        w=w1
        nw=nw1;-5
        yrange=[5e-13,1e-9]
        latgam=q                ;[1.85,1.92]
        lateng=[100e3,100e6]
;        leflux=[1.47e-10,2.01e-10];3.08e-10,3.43e-10] ;/lfact ;; for
;        converting to nuFnu
        lateng1=[[100e3,1e6],[1e6,10e6],[10e6,100e6]]
;        leflux=[2.2e-7,6.24e-8,5.69e-8]
        leflux=[2.2e-11,6.2e-11,5.65e-10]
        plotsym,1,4,thick=10
        l=where(earr ge 100e3 and earr le 1e8,nl)
     endif else begin
        lateng1=[[100e3,1e6],[1e6,10e6],[10e6,100e6]]
;        leflux=[2.28e-7,6.49e-8,3.75e-8]
        leflux=[2.27e-11,6.45e-11,3.72e-10]
        xtitle='Energy (keV)'
        yrange=[1e-13,1e-9]
        w=w2
        nw=nw2
     endelse 
     eav=dblarr(nw)
     eaverr=dblarr(2,nw)
     nufnu=dblarr(nw) & emin=dblarr(nw) & emax=dblarr(nw) & slope=fltarr(nw) & nufnuerr=dblarr(nw)
     for i=0,nw-1 do begin
;        ee=[,]
        emax[i]=eng[w[i]]+engerr[w[i]]
        emin[i]=eng[w[i]]-engerr[w[i]]
;        we=where(earr ge eng[w[i]]
;        eeerr=[engerr[w[i]],engerr[w[i]]]
        if eng[w[i]] lt b[z] then a=-p[z] else a=-q[z]
        slope[i]=a
;        dnde=norm[z]*(((ee/b[z])^(s*p[z])+(ee/b[z])^(s*q[z]))^(-1/s))
;        eav[i]=(alog(emax[i])-alog(emin[i]))/(((emax[i]^(a+1))-(emin[i]^(a+1)))/(a+1))
;        eav[i]=(1.-a)/(2.-a)*(emax[i]^(2-a)-emin[i]^(2-a))/(emax[i]^(1-a)-emin[i]^(1-a))
;        nee=norm[z]*(((ee/b[z])^(s*p[z])+(ee/b[z])^(s*q[z]))^(-1/s))
        private=[norm[z],b[z],p[z],s,q[z]]
        eav[i]=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)
;        eav[i]=sqrt(emin[i]*emax[i])
        nufnu[i]=eav[i]^2*pe[w[i]]*kev2erg*mpe0[w[i]]/mpe[w[i]]/const[w[i]];/(emax[i]-emin[i])
        nufnuerr[i]=eav[i]^2*peerr[w[i]]*kev2erg*mpe0[w[i]]/mpe[w[i]]/const[w[i]];/(emax[i]-emin[i])
;        nufnu[i]=norm[z]*(emax[i]^(a+1)-emin[i]^(a+1))/(a+1)*eav[i]^2
;        nuFnu_Data2 =  (K^2.0) * ((Emax^(a+1)) - Emin^(a+1)/(a+1)) * Ec^2.0 / (Emax-Emin)       
;        eav[i]=int_tabulated(ee,ee*dnde)/int_tabulated(ee,dnde)
;        eav[i]=10^mean(alog10(ee))
;        eav[i]=mean(ee)
        eaverr[0,i]=eav[i]-emin[i];eav[i]-ee[0]
        eaverr[1,i]=emax[i]-eav[i];ee[1]-eav[i]
     endfor 
print,max(eav)

;     nee=norm[z]*(((eav/b[z])^(s*p[z])+(eav/b[z])^(s*q[z]))^(-1/s))
;     nufnu=eav^2*pe[w]*kev2erg
;     ppe=pe[w]*(mpe0[w]/mpe[w])/const[w]*kev2erg ;; divide out extinction/absorp and constants
;     ppe=pe[w]/const[w]*kev2erg ;; divide out extinction/absorp and constants
 ;    ppeerr=peerr[w]*(mpe0[w]/mpe[w])/const[w]*kev2erg
     if z eq 0 then begin
        nufnu0=nufnu
        nufnuerr0=nufnuerr
        eav0=eav
        eaverr0=eaverr
     endif else begin
        nufnu1=nufnu
        nufnuerr1=nufnuerr
        eav1=eav
        eaverr1=eaverr
        xtickformat='loglabels'
     endelse         
     
     ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (erg cm!U-2!N s!U-1!N)'
     plot,[1e-3,1e10],[1e-13,1e-8],/nodata,/xlog,/ylog,xrange=[1e-3,1e8],xtitle=xtitle,ytitle=ytitle,/xsty,yrange=yrange,/ysty,xtickformat=xtickformat
;     oploterror,eng[w],eng[w]^2*ppe,eng[w]^2*ppeerr,psym=3,/nohat
;;     oploterror,eav,nufnu,nufnuerr,color=color[w],psym=3,/nohat
;     if z eq 0 then begin 
;        oploterror,eav[w0opt],nufnu[w0opt],nufnuerr[w0opt],color=!orange,psym=3,/nohat
;        for j=0,nw0opt-1 do oplot,[eav[j]-eaverr[0,j],eav[j]+eaverr[1,j]],[nufnu[j],nufnu[j]],color=!orange
;        oploterror,eav[w0x],nufnu[w0x],nufnuerr[w0x],color=!blue,psym=3,/nohat
;        oploterror,eav[[w1,w2]],nufnu[[w1,w2]],nufnuerr[[w1,w2]],color=!green,psym=3,/nohat
;     endif else begin
;        oploterror,eav[w4opt],nufnu[w4opt],nufnuerr[w4opt],color=!orange,psym=3,/nohat
;        oploterror,eav[w4x],nufnu[w4x],nufnuerr[w4x],color=!blue,psym=3,/nohat
;        oploterror,eav[[w5,w6]],nufnu[[w5,w6]],nufnuerr[[w5,w6]],color=!green,psym=3,/nohat
;     endelse 
;     oploterror,eav,eav^2*ppe,eav^2*ppeerr,psym=3,/nohat,color=color[z]
;     for j=0,nw-1 do begin
;        oplot,[eng[w[j]]-engerr[w[j]],eng[w[j]]+engerr[w[j]]],[eng[w[j]]^2*ppe[j],eng[w[j]]^2*ppe[j]]
;        oplot,[eav[j]-eaverr[0,j],eav[j]+eaverr[1,j]],[eav[j]^2*ppe[j],eav[j]^2*ppe[j]],color=color[z]
;;        oplot,[eav[j]-eaverr[0,j],eav[j]+eaverr[1,j]],[nufnu[j],nufnu[j]],color=color[j]
;     endfor 

     wy=where(color[w] eq !orange,ny)
     wb=where(color[w] eq !blue,nb)
     wg=where(color[w] eq !green,ng)
;     print,ny,nb,ng
;     plotsym,0,0.8,/fill
     oploterror,eav[wy[0:1]],nufnu[wy[0:1]],nufnuerr[wy[0:1]],errcolor=!red,psym=3,/nohat
     for j=0,1 do oplot,[eav[wy[j]]-eaverr[0,wy[j]],eav[wy[j]]+eaverr[1,wy[j]]],[nufnu[wy[j]],nufnu[wy[j]]],color=!red
     oploterror,eav[wy[2:*]],nufnu[wy[2:*]],nufnuerr[wy[2:*]],errcolor=!orange,psym=3,/nohat
     for j=2,ny-1 do oplot,[eav[wy[j]]-eaverr[0,wy[j]],eav[wy[j]]+eaverr[1,wy[j]]],[nufnu[wy[j]],nufnu[wy[j]]],color=!orange
     oploterror,eav[wb],nufnu[wb],nufnuerr[wb],color=!blue,psym=3,/nohat
     for j=0,nb-1 do oplot,[eav[wb[j]]-eaverr[0,wb[j]],eav[wb[j]]+eaverr[1,wb[j]]],[nufnu[wb[j]],nufnu[wb[j]]],color=!blue
     oploterror,eav[wg],nufnu[wg],nufnuerr[wg],color=!green,psym=3,/nohat
     for j=0,ng-1 do oplot,[eav[wg[j]]-eaverr[0,wg[j]],eav[wg[j]]+eaverr[1,wg[j]]],[nufnu[wg[j]],nufnu[wg[j]]],color=!green


     f=smbknpow(earr,[norm[z],p[z],b[z],q[z],s])
     oplot,earr,earr^2*f*kev2erg;,line=2;,color=pcolor[z]
;     oplot,lateng,lateng^2*leflux[0]*kev2erg,psym=8
;     mlateng=10^mean(alog10(lateng))
;;     lfact=[(lateng[1]/lateng[0])^(2-latgam[z]) - 1.]/(2-latgam[z])
;;     nfact0=(earr[l]/lateng[0])^(latgam[0]-2)*lfact
     
     private=[norm[z],b[z],p[z],s,q[z]]
     leav=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng[0],lateng[1],/expr,private)
;     lnufnu=leav*leflux[z]/(lateng[1]-lateng[0])
     we=where(earr ge lateng[0] and earr le lateng[1],nwe)
;     luleav=fltarr(nwe) & 
     lfact=fltarr(3,nwe)
     for g=0,2 do begin 
        we=where(earr ge lateng1[0,g] and earr le lateng1[1,g],nwe)

        for k=0,nwe-1 do begin
;        luleav[k]=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',earr[we[k]],earr[we[k+1]],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',earr[we[k]],earr[we[k+1]],/expr,private)
;        lulnufnu=luleav*leflux[z]/(earr[k+1]-earr[k])
;;           lfact[k]=[(lateng[1]/lateng[0])^(2.-latgam[z]) -
;;           1.]/(2-latgam[z])*(earr[we[k]]/lateng[0])^(latgam[0]-2.)
           lfact[g,k]=[(lateng1[1,g]/lateng1[0,g])^(2.-latgam[z]) - 1.]/(2-latgam[z])*(earr[we[k]]/lateng1[0,g])^(latgam[0]-2.)
        endfor 
     endfor 
     leng=leav
     lflux=5.65e-8 ;; ph/cm2/s from LAT input
     lfluxerr=3e-8
;     leng=mean(lateng)
;     lflux=7.5224e-11
;     lfluxerr=5.2e-12

     if z eq 0 then begin
;        lnufnu=leng^2*kev2erg*[lflux,lflux]/(lateng[1]-lateng[0])
;        lnufnuerr=leng^2*kev2erg*[lfluxerr,lfluxerr]/(lateng[1]-lateng[0])
;        a=lflux*(1-q[z])/(lateng[1]^(1-q[z])-lateng[0]^(1-q[z]))
;        lnufnu=a*leav^(-q[z])*leav^2*kev2erg
;        lnufnuerr=lfluxerr/lflux*lnufnu
;        leav=sqrt(lateng[0]*lateng[1])

;        private=[1.,2.0];p[z]+0.5]
;        private=[1.,2.0]
;        leav=qpint1d('x*p[0]*x^(-p[1])',lateng[0],lateng[1],/expr,private)/qpint1d('p[0]*x^(-p[1])',lateng[0],lateng[1],/expr,private)

;        a=lflux/qpint1d('p[0]*(x^(-p[1]))',lateng[0],lateng[1],/expr,private)
;        par=[a,2.0];p[z]+0.5]
;        lnufnu=pow(leav,par)*leav^2*kev2erg

        private=[1.,b[z],p[z],s,q[z]]
        a=lflux/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lateng1[0,0],lateng1[1,0],/expr,private)
        par=[a,p[z],b[z],q[z],s]
        lnufnu=smbknpow(leav,par)*leav^2*kev2erg

;        print,lnufnu
        lnufnuerr=lfluxerr/lflux*lnufnu
        oplot,[leav,leav],[lnufnu-lnufnuerr,lnufnu+lnufnuerr],color=!magenta,line=2,thick=15
        oplot,[lateng[0],lateng[1]],[lnufnu,lnufnu],color=!magenta,line=2,thick=15
;        for g=0,2 do oplot,[lateng1[0,g],lateng1[1,g]],[lnufnu,lnufnu],color=!magenta,line=2,thick=10
;        oplot,[lateng[0],lateng[1]],[lnufnu2,lnufnu2],color=!green
;        print,leav,a,lnufnu

     endif 

     ;; LAT UL
;     oplot,earr[we],leflux,color=!magenta,thick=10
;     oplot,earr[we],leflux/lfact,color=!magenta,thick=10
     av=dblarr(3) & avlfact=dblarr(3)
     for g=0,2 do begin
        av[g]=sqrt(lateng1[0,g]*lateng1[1,g])
;        oplot,[lateng1[0,g],lateng1[1,g]],[leflux[g],leflux[g]],color=!magenta
        avlfact[g]=1.;[(lateng1[1,g]/lateng1[0,g])^(2.-latgam[z]) - 1.]/(2-latgam[z])*(av/lateng1[0,g])^(latgam[0]-2.)       
        oplot,[lateng1[0,g],lateng1[1,g]],[leflux[g],leflux[g]]/avlfact[g],color=!magenta,thick=5
     endfor 

     plots,av,leflux/avlfact,color=!magenta,psym=8
;stop
;     plots,av,leflux,color=!magenta,psym=8

     ;; SBPL error contour
     ebe=earr/b[z]
;     dfdn=(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s)
;     dfdbe=-norm[z]/s*(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s-1.)*(-q[z]*s*earr^(s*q[z])*b[z]^(-q[z]*s-1)-p[z]*s*earr^(s*p[z])*b[z]^(-p[z]*s-1.))
;     dfdp=-norm[z]/s*(ebe^(s*(p[z]+0.5))+ebe^(p[z]*s))^(-1./s-1)*(s*ebe^(s*(p[z]+0.5))-alog(b[z])*s*ebe^(s*(p[z]+0.5))+s*ebe^(s*p[z])-alog(b[z])*s*ebe^(p[z]*s))
     ebe0=earr/b[0]
     ebe1=earr/b[1]
     ;;; need partial derivatives with respect to cflux not n
     ;; cflux=alog10(norm*integral_e1^e2(E*dn/de dE))
     dfdn0=(ebe0^(s*p[0])+ebe0^(s*q[0]))^(-1./s)
     dfdn1=(ebe1^(s*p[1])+ebe1^(s*q[1]))^(-1./s)
     dfdbe0=-norm[0]/s*(ebe0^(s*p[0])+ebe0^(s*q[0]))^(-1./s-1.)*(-q[0]*s*earr^(s*q[0])*b[0]^(-q[0]*s-1)-p[0]*s*earr^(s*p[0])*b[0]^(-p[0]*s-1.))
     dfdbe1=-norm[1]/s*(ebe1^(s*p[1])+ebe1^(s*q[1]))^(-1./s-1.)*(-q[1]*s*earr^(s*q[1])*b[1]^(-q[1]*s-1)-p[1]*s*earr^(s*p[1])*b[1]^(-p[1]*s-1.))
;     dfdp=-norm[z]/s*(ebe^(s*(p[z]+0.5))+ebe^(p[z]*s))^(-1./s-1)*(s*ebe^(s*(p[z]+0.5))-alog(b[z])*s*ebe^(s*(p[z]+0.5))+s*ebe^(s*p[z])-alog(b[z])*s*ebe^(p[z]*s))
     dfdp=-norm[z]/s*(ebe^(s*(p[0]+0.5))+ebe^(p[0]*s))^(-1./s-1)*(s*ebe^(s*(p[0]+0.5))-alog(b[0])*s*ebe^(s*(p[0]+0.5))+s*ebe^(s*p[0])-alog(b[0])*s*ebe^(p[0]*s))


;     par1=[1,0,2]
;     par2=[3,4,5]
;     par=[0,1,2]
;     m=transpose([[p1ac],[be1c],[n1c]])
;     m1=[[m[par1,par1[0]]],[m[par1,par1[1]]],[m[par1,par1[2]]]]
;     m2=[[m[par,par2[0]]],[m[par,par2[1]]],[m[par,par2[2]]]] ;;; if
;     cov2
;     if z eq 0 then mm=m1 else mm=m2
     mm=[[be1c],[pc],[n1c],[be2c],[n2c]]
;     mm=[[cf1c],[be1c],[pc],[cf2c],[be2c]]

     sigf2=dblarr(n_elements(earr))
     for ee=0,n_elements(earr)-1 do begin 
;        dm=[[dfdp[ee]],[dfdbe[ee]],[dfdn[ee]]]
;        dm=[[dfdbe0[ee]],[dfdp[ee]],[dfdn0[ee]],[dfdbe1[ee]],[dfdn1[ee]]]
        if z eq 0 then dm=[[dfdbe0[ee]],[dfdp[ee]],[dfdn0[ee]],[0],[0]]
;dm=[[dfdn0[ee]],[dfdbe0[ee]],[dfdp[ee]],[0],[0]]
;
        if z eq 1 then dm=[[0],[dfdp[ee]],[0],[dfdbe1[ee]],[dfdn1[ee]]]
 
;dm=[[0],[0],[dfdp[ee]],[dfdn1[ee]],[dfdbe1[ee]]]
;       
        sigf2[ee]=transpose(dm)##mm##dm
     endfor 
     sigf=sqrt(sigf2)

     for u=2,3 do begin
        y1=earr^2*(f+sigf*u)*kev2erg
        y2=earr^2*(f-sigf*u)*kev2erg
        oplot,earr,y1,line=4-u    ;,color=pcolor[z]
        oplot,earr,y2,line=4-u    ;,color=pcolor[z]
     endfor 
     ;; LAT UL
;     oplot,luleav,lulnufnu,color=color[z]
;     oplot,[lateng[0],lateng[1]],[lnufnu,lnufnu],color=color[z]
;     plots,leav,lnufnu,psym=8,color=color[z]


     

;     plots,leav,lnufnu,psym=8,color=color[z]
;     plots,mlateng,leflux[z]/lfact,psym=8,color=color[z]
;     oplot,earr[l],leflux[z]/nfact0,color=!green,thick=10     

;     oplot,[mlateng,mlateng],[1.15e-12-6e-13,1.15e-12+7e-13],color=!red
;     oplot,[lateng[0],lateng[1]],[1.15e-12,1.15e-12],color=!red
     legtime=['T0+ ~1.5 days','T0+ ~5 days']
     legend,legtime[z],box=0,/top,/center

     if z eq 0 then begin 
        plot,[5,100],[3e-12,3e-11],/nodata,/xlog,/ylog,position=[0.27,0.79,0.57,0.93],xticks=1,charsize=1,xtickv=[10,100],xtickname=['10','100'],xtitle='Energy (keV)',xrange=[3,100],yrange=[3e-12,3e-11],/xsty,/ysty,xminor=9
        oploterror,eav[wg],nufnu[wg],nufnuerr[wg],color=!green,psym=3,/nohat
        for j=0,ng-1 do oplot,[eav[wg[j]]-eaverr[0,wg[j]],eav[wg[j]]+eaverr[1,wg[j]]],[nufnu[wg[j]],nufnu[wg[j]]],color=!green
        oplot,earr,earr^2*f*kev2erg
        for u=2,3 do begin
           y1=earr^2*(f+sigf*u)*kev2erg
           y2=earr^2*(f-sigf*u)*kev2erg
           oplot,earr,y1,line=4-u ;,color=pcolor[z]
           oplot,earr,y2,line=4-u ;,color=pcolor[z]
        endfor 
        xyouts,20,4e-12,'!4NuSTAR!X',color=!green,charsize=1
     endif

  endfor
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4  
  legend,['!4Fermi!X/LAT','!4NuSTAR!X','!4Swift!X/XRT','!4Swift!X/UVOT','Liverpool Telescope'],textcolor=[!magenta,!green,!blue,!orange,!red],box=0,/top,/left

  multiplot,/reset,/default
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/sed_plot.ps'
stop
  return
end 
pro plot_seds_old

  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_2sed_wLAT_sbpl.dat',freq,freqerr,nfnu,nfnuerr,mnfnu,format='(a,a,a,a,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_noabs_Hz_2sed_wLAT_sbpl.dat',freq0,freqerr0,nfnu0,nfnuerr0,mnfnu0,format='(a,a,a,a,a)'
;  norms=[3.86e-3,3.21e-3,3.20e-3,3.86e-3,6.06e-4,3.65e-4,3.74e-4,6.06e-4]
;  norms=[3.83e-3,3.15e-3,3.15e-3,3.83e-3,6.0e-4,3.57e-4,3.66e-4,6.0e-4]
;  norms=[3.89e-3,3.32e-3,3.29e-3,3.89e-3,6.17e-4,3.79e-4,3.92e-4,6.17e-4]  ;; 100bin BPL
;  norms=[2.46e-6,2.29e-6,2.27e-6,2.46e-6,1.93e-11,1.18e-11,1.22e-11,1.94e-11] ;; 100bin SBPL
;  norms=[2.81e-6,2.59e-6,2.57e-6,2.81e-6,4.99e-13,2.98e-13,3.09e-13] ;; 100bin SBPL w/ LAT extrap
  norms=[6.9e-7,6.32e-7,6.27e-7,6.9e-7,3.11e-6,1.91e-6,1.97e-6] ;; 100bin SBPL w/ LAT extrap tied phind
  normratio=[norms[2]/norms[0],norms[2]/norms[1],norms[2]/norms[2],norms[2]/norms[3],$
             norms[6]/norms[4],norms[6]/norms[5],norms[6]/norms[6]];,norms[6]/norms[7]]
;; show parameters 16,32,48,64,80,96,112

;; normalize to second nustar epoch

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')
  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=(388741688d)-51.
  tnu1=nu1-battrig
  tnu2=nu2-battrig+1000
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

;  tnustart=[tnu1,tnu3,tnu5]
;  tnustop=[tnu2,tnu4,tnu6]
  tnustart=[tnu1,tnu3]
  tnustop=[tnu2,tnu6]

;  w=where(freq eq 'NO',nw) ;;xrt1,xrt2,nu1a,nu1b,nu2a,nu2b,nu3a,nu3b
;  w1=indgen(w[0]-1)
;  w2=indgen(w[1]-w[0]-1)+w[0]+1
;  w3=indgen(w[2]-w[1]-1)+w[1]+1
;  w4=indgen(w[3]-w[2]-2)+w[2]+1
;  w5=indgen(w[4]-w[3]-1)+w[3]+1
;  w6=indgen(w[5]-w[4]-1)+w[4]+1
;;  w7=indgen(w[6]-w[5]-1)+w[5]+1
;  w7=indgen(n_elements(freq)-nw-w[5])+w[5]+1

  w=where(freq eq 'NO',nw) ;;opt1,xrt1,nu1a,nu1b,lat1,opt2,xrt2,nu2a,nu2b,lat2
  w1=indgen(w[0]-1)
  w2=indgen(w[1]-w[0]-1)+w[0]+1
  w3=indgen(w[2]-w[1]-1)+w[1]+1
  w4=indgen(w[3]-w[2]-1)+w[2]+1
  w5=indgen(w[4]-w[3]-1)+w[3]+1
  w6=indgen(w[5]-w[4]-1)+w[4]+1
  w7=indgen(n_elements(freq)-nw-w[5])+w[5]+1
;  w7=indgen(w[6]-w[5]-1)+w[5]+1
;  w8=indgen(w[7]-w[6]-1)+w[6]+1
;  w9=indgen(w[8]-w[7]-1)+w[7]+1
;  w8=indgen(n_elements(freq)-nw-w[6])+w[6]+1

;  wi1=indgen(426)
;  wi2=indgen(531-426)+426
;  wi3=indgen(623-532)+532
;  nwi1=n_elements(wi1)
;  nwi2=n_elements(wi2)
;  nwi3=n_elements(wi3)

  wi1=[w1,w2,w3,w4]
  nwi1=[n_elements(w1),n_elements(w2),n_elements(w3),n_elements(w4)]
  wi2=[w5,w6,w7];,w8]
  nwi2=[n_elements(w5),n_elements(w6),n_elements(w7)];,n_elements(w8)]
;  wi3=[w7]
  freq1=double(freq[wi1])
  freqerr1=double(freqerr[wi1])
  nfnu1=double(nfnu[wi1])
  nfnuerr1=double(nfnuerr[wi1])
  wneg=where(nfnu1-nfnuerr1 lt 0)
;  nfnuerr1[wneg]=nfnu1[wneg]-1e-17
  mnfnu1=double(mnfnu[wi1])
  normratio1=[replicate(normratio[0],nwi1[0]),replicate(normratio[1],nwi1[1]),replicate(normratio[2],nwi1[2]),replicate(normratio[3],nwi1[3])]
  absratio1=double(mnfnu0[wi1])/mnfnu1*normratio1
  freq2=double(freq[wi2])
  freqerr2=double(freqerr[wi2])
  nfnu2=double(nfnu[wi2])
  nfnuerr2=double(nfnuerr[wi2])
  wneg=where(nfnu2-nfnuerr2 lt 0)
;  nfnuerr2[wneg]=nfnu2[wneg]-1e-14
  mnfnu2=double(mnfnu[wi2])
  normratio2=[replicate(normratio[4],nwi2[0]),replicate(normratio[5],nwi2[1]),replicate(normratio[6],nwi2[2])];,replicate(normratio[7],nwi2[3])]
  absratio2=double(mnfnu0[wi2])/mnfnu2*normratio2

;;   freq3=double(freq[wi3])
;;   freqerr3=double(freqerr[wi3])
;;   nfnu3=double(nfnu[wi3])
;;   nfnuerr3=double(nfnuerr[wi3])
;;   mnfnu3=double(mnfnu[wi3])
;;   normratio3=[replicate(normratio[6],nwi3[0])]
;;   absratio3=double(mnfnu0[wi3])/mnfnu3*normratio3
  

;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int1.dat',freq1,freqerr1,nfnu1,nfnuerr1,mnfnu1,format='(d,d,d,d,d)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int2.dat',freq2,freqerr2,nfnu2,nfnuerr2,mnfnu2,format='(d,d,d,d,d)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int3.dat',freq3,freqerr3,nfnu3,nfnuerr3,mnfnu3,format='(d,d,d,d,d)'

  h=4.1357d-15*(1e-3/1.)        ; keV s
  c=3e8                         ; cm/s
  eng1=h*freq1
  engerr1=h*freqerr1
  eng2=h*freq2
  engerr2=h*freqerr2
;  eng3=h*freq3
;  engerr3=h*freqerr3


;  nfnu1=mnfnu1
;  nfnu2=mnfnu2
;  nfnu3=mnfnu3

  begplot,name='~/Desktop/GRB130427A/NuSTAR/joint_sed.ps',/color,font='helvetica'
;  !p.multi=[0,1,2]
  multiplot,[1,2],/init
;  iz=[2,0,1]
  color=[!green,!blue,!red]
  xrange=[1e-3,100e6]
  yrange=[1e-14,1e-09]
  for z=0,1 do begin 
;     z=iz[zz]
     multiplot
     if z eq 1 then xtitle='Energy (keV)'
     plotsym,0,0.5,/fill

     plot,xrange,yrange,/nodata,/xlog,/ylog,/xsty,/ysty,ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (erg cm!U-2!Ns!U-1!N)',xtitle=xtitle
 
     case z of 
        0: begin 
           oplot,eng1,nfnu1*absratio1,psym=8,color=!lightgreen
;           plot,eng1,nfnu1*absratio1,/xlog,/ylog,psym=3,xrange=[1e-3,100e6],/xsty,/ysty,yrange=[1e-14,4e-10],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)' ;,xtitle='Energy (keV)'
;           a=where(eng1[1:*]-eng1[0:*] lt 0)
;           color=[replicate(!lightgreen,a[0]),replicate(!forestgreen,a[1]-a[0]),replicate(!forestgreen,n_elements(eng1)-a[1])]
           for i=0,n_elements(eng1)-1 do begin
              if nfnu1[i]-nfnuerr1[i] lt 0 then errlow=1e-14 else errlow=nfnu1[i]-nfnuerr1[i]
              oplot,[eng1[i]-engerr1[i],eng1[i]+engerr1[i]],[nfnu1[i],nfnu1[i]]*absratio1[i],color=!lightgreen
              oplot,[eng1[i],eng1[i]],[errlow,nfnu1[i]+nfnuerr1[i]]*absratio1[i],color=!lightgreen
           endfor 
        end 
        1: begin 
           oplot,eng2,nfnu2*absratio2,psym=8,color=!lightblue

;           plot,eng2,nfnu2*absratio2,/xlog,/ylog,psym=3,xtitle='Energy (keV)',xrange=[0.3,100e6],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)',yrange=[1e-13,4e-10],/xsty,/ysty
;           a=where(eng2[1:*]-eng2[0:*] lt 0)
;           color=[replicate(!dodgerblue,a[0]),replicate(!royalblue,a[1]-a[0]),replicate(!royalblue,a[2]-a[1])]
        
           oplot,eng2,nfnu2*absratio2,psym=3,color=!blue
           for i=0,n_elements(eng2)-1 do begin
              if nfnu2[i]-nfnuerr2[i] lt 0 then errlow=1e-14 else errlow=nfnu2[i]-nfnuerr2[i]              
              oplot,[eng2[i]-engerr2[i],eng2[i]+engerr2[i]],[nfnu2[i],nfnu2[i]]*absratio2[i],color=!dodgerblue
              oplot,[eng2[i],eng2[i]],[errlow,nfnu2[i]+nfnuerr2[i]]*absratio2[i],color=!dodgerblue;color[i]
           endfor 
        end 
        2: begin
           oplot,eng3,nfnu3*absratio3,psym=3

;           plot,eng3,nfnu3*absratio3,/xlog,/ylog,psym=3,xtitle='Energy (keV)',xrange=[0.3,100e6],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)',yrange=[1e-13,4e-10],/xsty,/ysty
;           a=where(eng3[1:*]-eng3[0:*] lt 0)
;           color=replicate(!salmon,a[0])
        
           oplot,eng3,nfnu3*absratio3,psym=3,color=!red
           for i=0,n_elements(eng3)-1 do begin
              oplot,[eng3[i]-engerr3[i],eng3[i]+engerr3[i]],[nfnu3[i],nfnu3[i]]*absratio3[i],color=!salmon;color[i]
              oplot,[eng3[i],eng3[i]],[nfnu3[i]-nfnuerr3[i],nfnu3[i]+nfnuerr3[i]]*absratio3[i],color=!salmon;color[i]
           endfor
        end 
     endcase            

;;   a=where(eng3[1:*]-eng3[0:*] lt 0)
;;   color=[replicate(!red,a[0]),replicate(!salmon,a[1]-a[0]),replicate(!orange,n_elements(eng3)-a[1])]

;;   c=1.;0.2
;;   oplot,eng3,c*nfnu3,psym=3,color=!red
;;   for i=0,n_elements(eng3)-1 do begin
;;      oplot,[eng3[i]-engerr3[i],eng3[i]+engerr3[i]],c*[nfnu3[i],nfnu3[i]],color=color[i]
;;      oplot,[eng3[i],eng3[i]],c*[nfnu3[i]-nfnuerr3[i],nfnu3[i]+nfnuerr3[i]],color=color[i]
;;   endfor 

     s1=sort(eng1)
     s2=sort(eng2)
     s3=sort(eng3)
;  s3=sort(eng3)
     eng=[(dindgen(9)+1.)*1e-3,(dindgen(9)+1.)*1e-2,(dindgen(9)+1.)/10.,(dindgen(99)+1.),(dindgen(9)+1)*1e2,(dindgen(9)+1)*1e3,(dindgen(9)+1.)*1e4,(dindgen(9)+1.)*1e5,(dindgen(9)+1.)*1e6,(dindgen(10)+1.)*1e7]
;     engerr=[replicate(1e-2,9),replicate(0.1,9),replicate(1.,99),replicate(1e3,9),replicate(1e4,9),replicate(1e5,9),replicate(1e6,9)]

;     p=[1.84910,1.92259]
;     perr=[0.032,0.078]
;     flux0=[-10.4655,-11.2168]
;     fluxerr0=[0.013,0.046]

     ;;SBPL
;     p=[1.69,1.77]
     p=[1.69,1.69]
     perr=[0.02,0.02]
;     be=[87.,1.7e4]
;     be=[82.7,1.42571E+05]
     be=[178,28.]
     beerr=[10,10.]
;     flux=[3.35e-11,3.77e-12]
;     flux=[3.36e-11,3.84e-12]
     flux=[3.42e-11,3.46e-12]
     fluxerr=[1e-12,1e-13]
;     readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance.txt',ebvc,nhc,p1ac,be1c,n1c,n2c,n3c,be2c,p1bc,n4c,n5c,n6c,format=('f,f,f,f,f,f,f,f,f,f,f'),skip=4
     readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance2.txt',p1ac,be1c,n1c,format='(f,f,f)'
     ;; BPL
;     p=[1.72,1.765]
;     perr=[0.011,0.018]
;     be=[17.64,32.6]
;     beerr=[1.7,12.]
     q=p+0.5
     s=0.85
;     flux=[3.08e-11,3.82e-12]
;     fluxerr=[4e-12,2e-13]

;; BPL
;     readcol,'~/Desktop/GRB130427A/NuSTAR/bpl_covariance2.txt',ebvc,nhc,p1ac,be1c,n1c,n2c,n3c,be2c,p1bc,n4c,n5c,n6c,format=('f,f,f,f,f,f,f,f,f,f,f')
;     readcol,'~/Desktop/GRB130427A/NuSTAR/bpl_covariance.txt',p1ac,be1c,n1c,format='(f,f,f)'
;     par1=[2,3,6]
;     par2=[7,8,11]
;     m=[[ebvc],[nhc],[p1ac],[be1c],[n1c],[n2c],[n3c],[be2c],[p1bc],[n4c],[n5c],[n6c]]
     par1=[0,1,2]
     par2=[3,4,5]
     m=transpose([[p1ac],[be1c],[n1c]])
     m1=[[m[par1,par1[0]]],[m[par1,par1[1]]],[m[par1,par1[2]]]]
     m2=[[m[par1,par2[0]]],[m[par1,par2[1]]],[m[par1,par2[2]]]] ;;; if cov2
;     m2=[[m[par2,par2[0]]],[m[par2,par2[1]]],[m[par2,par2[2]]]] ;;; if cov1
;     m1=[transpose([p1ac[0:2]]),transpose([be1c[0:2]]),transpose(n1c[0:2])]
;     m2=[transpose([p1ac[3:*]]),transpose([be1c[3:*]]),transpose(n1c[3:*])]
     if z eq 0 then mm=m1 else mm=m2
     kev2erg=1.602e-12*1d3  ;; 1kev= # ergs
;     fluxerr=10.^(flux0+fluxerr0)-10^flux0
;     flux=10^flux0
     emin=3.0
     emax=79.0
     w=where(eng ge emin and eng le emax)
     norm=[norms[2],norms[6]];,norms[6]]

     normerr=(fluxerr/flux)*norm

     f=smbknpow(eng,[norm[z],p[z],be[z],q[z],s])
;     f=bknpow(eng,[norm[z],p[z],be[z],q[z]])
     arb=1.6e-9                 ;0.09 ;;kev2erg
     ;; SBPL
     ebe=eng/be[z]
     dfdn=(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s)
     dfdbe=-norm[z]/s*(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s-1.)*(-q[z]*s*eng^(s*q[z])*be[z]^(-q[z]*s-1)-p[z]*s*eng^(s*p[z])*be[z]^(-p[z]*s-1.))
     dfdp=-norm[z]/s*(ebe^(s*(p[z]+0.5))+ebe^(p[z]*s))^(-1./s-1)*(s*ebe^(s*(p[z]+0.5))-alog(be[z])*s*ebe^(s*(p[z]+0.5))+s*ebe^(s*p[z])-alog(be[z])*s*ebe^(p[z]*s))

     sigf2=dblarr(n_elements(eng))
     for ee=0,n_elements(eng)-1 do begin 
        dm=[[dfdp[ee]],[dfdbe[ee]],[dfdn[ee]]]
        sigf2[ee]=transpose(dm)##mm##dm
     endfor 
     sigf=sqrt(sigf2)

     ;; BKNPOW
     goto,bpl
     wbb=where(eng lt be[z]) ;; where before break
     wab=where(eng gt be[z]) ;; where after break
     dfdn=[eng[wbb]^(-p[z]),be[z]^(q[z]-p[z])*eng[wab]^(-q[z])]
;     dfdn=be[z]^(q[z]-p[z])*eng^(-q[z])
     dfdp=[norm[z]*eng[wbb]^(-p[z])*alog(eng),norm[z]*be[z]^(q[z]-p[z])*eng[wab]^(-q[z])*alog(be[z])]
;     dfdp=norm[z]*be[z]^(q[z]-p[z])*eng^(-q[z])*alog(be[z])
;     dfdq=[eng[wbb]*0.,norm[z]*be[z]^(q[z]-p[z])*eng[wab]^(-q[z])*(alog(be[z])-alog(eng[wab]))]
     dfdbe=[eng[wbb]*0.,(q[z]-p[z])*norm[z]*be[z]^(q[z]-p[z]-1.)*eng[wab]^(-q[z])]
;     dfdbe=(q[z]-p[z])*norm[z]*be[z]^(q[z]-p[z]-1.)*eng^(-q[z])
     sigf2=dblarr(n_elements(eng))
     for ee=0,n_elements(eng)-1 do begin 
        dm=[[dfdp[ee]],[dfdbe[ee]],[dfdn[ee]]]
        sigf2[ee]=transpose(dm)##mm##dm
     endfor 
;     sigf=sqrt(sigf2)
     bpl:
     ;; OLD
;     dfdn=eng^(-p[0])
;     dfdp=norm[0]*eng^(-p[0])*alog(eng) ;p[0]*norm[0]*eng^(-p[0]-1.)
;  x=[eng,reverse(eng)]
;  y=[eng^2*(f+sqrt(dfdn^2*normerr[0]^2+dfdp^2*perr[0]^2)),reverse(eng^2*(f-sqrt(dfdn^2*normerr[0]^2+dfdp^2*perr[0]^2)))]
;  polyfill,x,y,color=!green,/transparent
;     if z eq 0 then begin 
        oplot,eng,eng^2*f*arb,color=color[z]
        x=eng
        ;; just wrong, doesn't use covariance
;        sigf=sqrt(dfdn^2*normerr[z]^2+dfdp^2*perr[z]^2+dfdq^2*perr[z]^2+dfdbe^2*beerr[z]^2)
        y1=eng^2*(f+sigf)*arb
        y2=eng^2*(f-sigf)*arb
;        polyfill,[x,reverse(x)],[y1,reverse(y2)],color=color[z]
;;        oplot,eng,y1,line=1,color=color[z]
;;        oplot,eng,y2,line=1,color=color[z]
;     endif 
;     f=pow(eng,[norm[1],p[1]])
;     dfdn=eng^(-p[1])
;     dfdp=norm[1]*eng^(-p[1])*alog(eng) ;p[1]*norm[1]*eng^(-p[1]-1.)
;     if z eq 1 then begin 
;        oplot,eng,eng^2*f*arb,color=!blue
;        oplot,eng,eng^2*(f+sqrt(dfdn^2*normerr[1]^2+dfdp^2*perr[1]^2+dfdpp^2*pp[1]^2+dfdbe^2*be[1]^2))*arb,line=1,color=!blue
;        oplot,eng,eng^2*(f-sqrt(dfdn^2*normerr[1]^2+dfdp^2*perr[1]^2+dfdpp^2*pp[1]^2+dfdbe^2*be[1]^2))*arb,line=1,color=!blue
;     endif 

;     f=pow(eng,[norm[2],p[2]])
;     dfdn=eng^(-p[2])
;     dfdp=norm[2]*eng^(-p[2])*alog(eng) ;p[1]*norm[1]*eng^(-p[1]-1.)
;     if z eq 2 then begin 
;        oplot,eng,eng^2*f*arb,color=!red
;        oplot,eng,eng^2*(f+sqrt(dfdn^2*normerr[2]^2+dfdp^2*perr[2]^2+dfdpp^2*pp[2]^2+dfdbe^2*be[2]^2))*arb,line=1,color=!red
;        oplot,eng,eng^2*(f-sqrt(dfdn^2*normerr[2]^2+dfdp^2*perr[2]^2+dfdpp^2*pp[2]^2+dfdbe^2*be[2]^2))*arb,line=1,color=!red
;     endif 
     

     ;; LAT UL
     latgam=q;[1.85,1.92]
     lateng=[100e3,100e6]
     leflux=[3.08e-10,3.43e-10] ;/lfact ;; for converting to nuFnu
     plotsym,1,5,thick=10
     w=where(eng ge 100e3 and eng le 1e8,nw)

     ff1_0=[(lateng[1]/lateng[0])^(2-latgam[0]) - 1.]/(2-latgam[0])
     ff1_1=[(lateng[1]/lateng[0])^(2-latgam[1]) - 1.]/(2-latgam[1])
     nfact0=[(eng[w]/lateng[0])^(latgam[0]-2)*ff1_0[0]]
     nfact1=[(eng[w]/lateng[0])^(latgam[1]-2)*ff1_1[0]]
     if z eq 0 then begin 
        plots,eng[w[14]],leflux[0]/nfact0[14],psym=8,color=!green
        oplot,eng[w],leflux[0]/nfact0,color=!green,thick=10
        oplot,[lateng[0],lateng[1]],[1.15e-12,1.15e-12],color=!red
        mlateng=10^mean(alog10(lateng))
        oplot,[mlateng,mlateng],[1.15e-12-6e-13,1.15e-12+7e-13],color=!red
     endif else begin 
        plots,eng[w[12]],leflux[1]/nfact1[12],psym=8,color=!blue
        oplot,eng[w],leflux[1]/nfact1,color=!blue,thick=10
     endelse 


     ;; MORE FUN FROM YONI - LAT EXTRAP

  ;;; need to make sure latgam is consistent between LC & SPEC & UL
     llc=mrdfits('~/Desktop/GRB130427A/NuSTAR/lat/latlc.fits',1)
;  w0=where(llc.time ge tnustart[0] and llc.time le tnustop[0],nw0)
;  w1=where(llc.time ge tnustart[1] and llc.time le tnustop[1],nw1)
     leflux=llc.flux
     lefluxerr0=llc.fluxerr[0]
     lefluxerr1=llc.fluxerr[1]
     ff1_0=[(lateng[1]/lateng[0])^(2-latgam[0]) - 1.]/(2-latgam[0])
     ff1_1=[(lateng[1]/lateng[0])^(2-latgam[1]) - 1.]/(2-latgam[1])
     nfact0=[(eng[w]/lateng[0])^(latgam[0]-2)*ff1_0[0]]
     nfact1=[(eng[w]/lateng[0])^(latgam[1]-2)*ff1_1[0]]

     
     slope=-(alog10(llc[11].flux)-alog10(llc[0].flux))/(alog10(llc[11].time)-alog10(llc[0].time)) ;; LAT temporal decay index???
     slopep=-(alog10(llc[11].flux+llc[11].fluxerr[1])-alog10(llc[0].flux+llc[0].fluxerr[1]))/(alog10(llc[11].time)-alog10(llc[0].time))
     slopem=-(alog10(llc[11].flux-llc[11].fluxerr[0])-alog10(llc[0].flux-llc[0].fluxerr[0]))/(alog10(llc[11].time)-alog10(llc[0].time))
     norm=median(llc.flux/pow(llc.time,[1.,slope]))
     normerr0=median((llc.flux-llc.fluxerr[0])/pow(llc.time,[1.,slopem]))
     normerr1=median((llc.flux+llc.fluxerr[1])/pow(llc.time,[1.,slopep]))
     iflux0=intpow([tnustart[0],tnustop[0]],[norm,slope])
     iflux0errm=intpow([tnustart[0],tnustop[0]],[normerr0,slopem])
     iflux0errp=intpow([tnustart[0],tnustop[0]],[normerr1,slopep])
     iflux1=reform(intpow([tnustart[1],tnustop[1]],[norm,slope]))
     iflux1errm=intpow([tnustart[1],tnustop[1]],[normerr0,slopem])
     iflux1errp=intpow([tnustart[1],tnustop[1]],[normerr1,slopep])

;  polyfill,[eng[w],reverse(eng[w])],[iflux0errp[0]/nfact0,reverse(iflux0errm[0]/nfact0)],/line_fill,orient=45,color=!green
;  polyfill,[eng[w],reverse(eng[w])],[iflux1errp[0]/nfact1,reverse(iflux1errm[0]/nfact1)],/line_fill,orient=45,color=!blue

;  oplot,[eng[w],reverse(eng[w]),eng[w[0]]],[iflux0errp[0]/nfact0,reverse(iflux0errm[0]/nfact0),iflux0errp[0]/nfact0[0]],color=!green
;  oplot,[eng[w],reverse(eng[w]),eng[w[0]]],[iflux1errp[0]/nfact1,reverse(iflux1errm[0]/nfact1),iflux1errp[0]/nfact1[0]],color=!blue

     g=!tsym.gamma_cap
     pm=!tsym.plusminus
     if z eq 0 then $
        legend,['T+T0=1.03-1.61 x 10!U5!N s ('+g+'='+numdec(p[0],2)+pm+numdec(perr[0],2)+')'],box=0,/top,/left,textcolor=!green
     if z eq 1 then $
        legend,['T+T0=4.11-4.96 x 10!U5!N s ('+g+'='+numdec(p[1],2)+pm+numdec(perr[1],2)+')'],box=0,/top,/left,textcolor=!blue
     if z eq 2 then $
        legend,['T+T0=2-50 x 10!U4!N s ('+g+'='+numdec(p[2],2)+pm+numdec(perr[2],2)+')'],box=0,/top,/left,textcolor=!red

  endfor 
  multiplot,/reset,/default
;  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/joint_sed.ps ~/Desktop/GRB130427A/NuSTAR/joint_sed.pdf'


  stop
  return
end 

pro rebin_lc,t,c,terr,cerr,mincts,lc=lc

  if mincts eq 0 then begin 
     lc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
     lc=replicate(lc,n_elements(t))
     lc.time=t
     lc.tstart=t-terr
     lc.tstop=t+terr
     lc.ctr=c/(terr*2.)
     lc.ctrerr=cerr/(terr*2.)
     lc.exp=terr*2.
     return
  endif

;  w=where(c gt 0)
;  t=t[w]
;  c=c[w]
;  terr=terr[w]
;  cerr=cerr[w]
  nt=n_elements(t)
  tnew=t[0]
  cnew=c[0]
  texp=0
  cerrnew=cerr[0]
  tstart=0d
  tstop=0d
  i=0

  n=mincts
  for i=0,nt-1,n do begin
     if i+n gt nt then n=nt-i-1
     print,i,i+n-1
     tnew=[tnew,mean(t[i:i+n-1])]
     texp=[texp,n]
     cnew=[cnew,total(c[i:i+n-1])]
     cerrnew=[cerrnew,sqrt(total((cerr[i:i+n-1])^2))];*mean(c[i:i+1])]
     tstart=[tstart,t[i]-terr[i]]
     tstop=[tstop,t[i+n-1]+terr[i+n-1]]
  endfor 

  goto,skip2
  for i=0,nt-1 do begin
     if c[i] ge mincts then begin;or ((t[i+1]-t[i])/t[i] lt 1.) then begin 
        ctmp=c[i]
        ttmp=t[i]
        cetmp=cerr[i]
        etmp=1
        tmin=t[i]-terr[i]
        tmax=t[i]+terr[i]
     endif else begin
        j=i
        ctmp=c[i]
        while (ctmp lt mincts and j lt nt-1) do begin 
           ctmp=ctmp+c[j]
           j=j+1
        endwhile 
        ttmp=(t[j-1]-t[i])/2.+t[i];mean(t[i:j-1])
        tmin=t[i]-terr[i]
        tmax=t[j-1]+terr[i]
        etmp=j-i+1
;        ctmp=ctmp/etmp
;        cetmp=sqrt(ctmp)
        cetmp=sqrt(total((cerr[i:j-1]/c[i:j-1])^2))*ctmp;/etmp
        i=j
;        colprint,ctmp,ttmp,etmp
     endelse 
     cnew=[cnew,ctmp]
     tnew=[tnew,ttmp]
;     terrnew=[terrnew,terr[0]*etmp]
     tstart=[tstart,tmin]
     tstop=[tstop,tmax]
     cerrnew=[cerrnew,cetmp]
     texp=[texp,etmp]
        
  endfor 

skip2:
  cnew=cnew[1:*]
  tnew=tnew[1:*]
  texp=texp[1:*]
  cerrnew=cerrnew[1:*]
  tstart=tstart[1:*]
  tstop=tstop[1:*]

  lc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
  lc=replicate(lc,n_elements(cnew))
  lc.time=tnew
  lc.tstart=tstart
  lc.tstop=tstop
  lc.exp=texp*5000.
  lc.ctr=cnew/lc.exp
  lc.ctrerr=cerrnew/lc.exp

;  ploterror,t,c,cerr,/xlog,/ylog,psym=3,/nohat,yrange=[1,1e3]
;  for i=0,nt-1 do oplot,[t[i]-terr[i],t[i]+terr[i]],[c[i],c[i]]
;  oploterror,tnew,cnew,cerrnew,psym=3,errcolor=!red,/nohat
;  for i=0,n_elements(tnew)-1 do oplot,[tstart[i],tstop[i]],[cnew[i],cnew[i]],color=!red;oplot,[tnew[i]-terrnew[i],tnew[i]+terrnew[i]],[cnew[i],cnew[i]],color=!red
;stop

return
end

pro plot_lcs

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')
  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=(388741688d)-51.
  tnu1=nu1-battrig
  tnu2=nu2-battrig+1000
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

;  tnustart=[tnu1,tnu3,tnu5]
;  tnustop=[tnu2,tnu4,tnu6]
  tnustart=[tnu1,tnu3]
  tnustop=[tnu2,tnu6]

  xlc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE_binned2.qdp',wtfile='~/Desktop/GRB130427A/WTCURVE.qdp',/phil)
;  xlc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE.qdp',/phil)
  spec=mrdfits('~/Desktop/GRB130427A/UL_specfits.fits',1)
  readcol,'~/Desktop/GRB130427A/NuSTAR/xrt.dat',xtime,xexp,xflux,xerr,format='(d,d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/uvot.dat',utime,uexp,uflux,uerr,ufilt,format='(d,d,d,d,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_3-10.txt',ntime1,nrate1,nrate1err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_10-30.txt',ntime2,nrate2,nrate2err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_30-79.txt',ntime3,nrate3,nrate3err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_3-79.txt',ntime4,nrate4,nrate4err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/lat/LAT_extrapolation_po.txt',ltime,lflux,lfluxerr0,lfluxerr1
  
;  w=where((xlc.tstart gt 103379.00 and xlc.tstop lt 160079.00) or
;  (xlc.tstart gt 411179.00 and xlc.tstop lt 495179.00),nw)
;  w=where(xlc.time gt tnustart[0] and xlc.time lt tnustop[1],nw)
  xlc2=xlc;[w]
  nw=n_elements(xlc)

  xlc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
  xlc=replicate(xlc,nw)
  xlc.time=xlc2.time
  xlc.tstart=xlc2.tstart
  xlc.tstop=xlc2.tstop
  xlc.ctr=xlc2.src_rate;/xlc2.pu_corr
  xlc.ctrerr=xlc2.src_rate_err;/xlc2.pu_corr
  xlc.exp=xlc2.exptime

  emin=[0.3,3,10,30,3]
  emax=[10,10,30.,79.,79.]
  eav=fltarr(5)
  s=0.85
  fnorm=[8.366e-7,3.983e-6]
  p=[1.69,1.69]
  p2=p+0.5
  b=[160.3,24.7]

  llc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',dblarr(2))
  llc=replicate(llc,n_elements(ltime))
  llc.time=ltime
  private=[fnorm[0],b[0],p[0],s,p2[0]]
  lemin=100e3
  lemax=100e6
  leav=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lemin,lemax,/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',lemin,lemax,/expr,private)


  llc.flux=lflux;/leav;/(100e6-100e3)
  llc.fluxerr[0]=lfluxerr0;/leav;/(100e6-100e3)
  llc.fluxerr[1]=lfluxerr1;/leav;/(100e6-100e3)
;  w=where(llc.time gt tnustart[0] and llc.time lt tnustop[2],nw)
  w=where(llc.time ge 1e5 and llc.time lt 5e5,nw)
  llc2=llc[w]
  mwrfits,llc,'~/Desktop/GRB130427A/NuSTAR/lat/latlc.fits',/create
  llc=llc2

  nexp=5000.
  cts1=nrate1*nexp
  cts2=nrate2*nexp
  cts3=nrate3*nexp
  cts4=nrate4*nexp

;  rb=[300,300,50,300]
  rb=[0.,0.,10.,0.]
  rebin_lc,ntime1,cts1,replicate(nexp/2.,n_elements(ntime1)),nrate1err*nexp,rb[0],lc=lc1
  rebin_lc,ntime2,cts2,replicate(nexp/2.,n_elements(ntime2)),nrate2err*nexp,rb[1],lc=lc2
  rebin_lc,ntime3,cts3,replicate(nexp/2.,n_elements(ntime3)),nrate3err*nexp,4,lc=lc3a
  rebin_lc,ntime3,cts3,replicate(nexp/2.,n_elements(ntime3)),nrate3err*nexp,4,lc=lc3b
  rebin_lc,ntime4,cts4,replicate(nexp/2.,n_elements(ntime4)),nrate4err*nexp,rb[3],lc=lc4

  w1=where(lc3a.time lt 2e5)
  w2=where(lc3b.time gt 2e5)
  concat_structs,lc3a[w1],lc3b[w2],lc3

  ;; use XSPEC fit with phind over intervals

  ;; xrt 3 intervals
  rate0=[0.282686,9.04322E-02]
  flux0=[2.3293e-11,3.852e-12]
  flux0err=[1e-12,1.7e-13]

;  flux0=[-10.5625,-11.3692]
;  flux0err=[0.021,0.044]
;  rate0=[0.293699,8.742e-2]
  xfact=[2.057,1.12] ;; pucorr fact

;  flux0=[-10.5642,-11.3207,-11.4358]
;  flux0err=[0.021,0.070,0.099]
;  rate0=[0.293703,9.98601e-2,7.60682e-2]
;  xfact=[2.057,1.0987,1.136]

  ;; nustar 3-10 all 3 intervals
  rate1=[0.222977+0.204306,3.70084E-02+3.51434E-02]
  flux1=[1.02e-11,2.21e-12]
  flux1err=[2e-13,1e-13]
;  flux1=[-10.97,-11.65]
;  flux1err=[0.0111,0.038]
;  rate1=[0.146+0.1347,(2.44e-2+2.3979e-2+2.1574e-2+2.034e-2)/2.]
;  flux1=[-10.97,-11.62,-11.67]
;  flux1err=[0.011,0.034,0.048]
;  rate1=[0.14,2.38e-2,2.16e-2]*2

  ;; nustar 10-30 all 3 intervals
  rate2=[7.72119E-02+6.95992E-02,1.11014E-02+1.11242E-02]
  flux2=[1.16e-11,2.27e-12]
  flux2err=[4e-13,5e-14]
;  rate2=[7.86e-2+7.054e-2,(1.228e-2+1.176e-2+1.086e-2+9.976e-3)/2.]
;  flux2=[-10.93,-11.65]
;  flux2err=[0.0125,0.042]

;  flux2=[-10.93,-11.62,-11.72]
;  flux2err=[0.013,0.0415,0.060]
;  rate2=[7.46e-2,1.195e-2,9.88e-3]*2
;  fluxfact2=[9.777E-11,9.621E-11,9.461E-11]

  ;; nustar 30-79 all 3 intervals
;  flux3=[-10.935,-11.68]
;  flux3err=[0.009,0.033]
;  rate3=[6.33e-3+5.845e-3,(9.084e-4+8.915e-4+8.04e-4+7.565e-4)/2.]
  rate3=[5.77014E-03+5.26895E-03,7.93494E-04+6.26531E-04]
  flux3=[1.19e-11,2.12e-12]
  flux3err=[3e-13,5e-14]

  ;; nustar 3-79 all 3 intervals
;  flux4=[-10.46,-11.18]
;  flux4err=[0.013,0.04]
;  rate4=[0.232+0.211,(3.77e-2+3.675e-2+3.335e-2+3.1177e-2)/2.]
  flux4=[3.36e-11,6.63e-12]
  flux4err=[1e-13,2e-13]
  rate4=[0.307998+0.280989,5.00626E-02+4.78850E-02]

;;; LOOK INTO CHANGE IN RATE IN FIRST INT
;  flux4=[-10.46,-11.15,-11.24]
;  flux4err=[0.013,0.039,0.054]
;  rate4=[3.0e-1,5.46e-2,4.6e-2]*2
;  fluxfact4=[7.469E-11,6.974E-11,6.515E-11]

  ;;;; need to get intervals right
  for i=0,4 do begin ;;; loop over light curves
     case i of
        0: lc=xlc   ;; 0.3-10 xrt
        1: lc=lc1   ;; 3-10 nustar
        2: lc=lc2   ;; 10-30 nustar
        3: lc=lc3   ;; 30-79 nustar
        4: lc=lc4   ;; 3-79 nustar
     endcase
     tmp=execute('rate=rate'+ntostr(i))
     tmp=execute('flux=flux'+ntostr(i))
     tmp=execute('fluxerr=flux'+ntostr(i)+'err')
     for j=0,1 do begin ;;; loop over times
;        w=where(lc.time ge tnustart[j] and lc.time lt tnustop[j],nw)
        nw=n_elements(lc)
        w=indgen(nw)
        
;        f=10.^flux[j]
;        ferr=10.^(flux[j]+fluxerr[j])-f
;        ferr=f-10.^(flux[j]-fluxerr[j])
        f=flux[j]
        ferr=fluxerr[j]
        private=[fnorm[j],b[j],p[j],s,p2[j]]

        eav[i]=qpint1d('x*p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)/qpint1d('p[0]*(((x/p[1])^(p[3]*p[2])+(x/p[1])^(p[3]*p[4]))^(-1/p[3]))',emin[i],emax[i],/expr,private)
        print,emin[i],emax[i],eav[i]
        if i eq 0 then begin
           lc[w].fluxfact=(f/rate[j])/xfact[j];/eav[i];/(engup[j]-englow[j])
           lc[w].fluxfacterr=(ferr/rate[j])/xfact[j];/eav[i];/(engup[j]-englow[j])
        endif else begin
;           tmp=execute('ff=fluxfact'+ntostr(i))
;           tmp=execute('fferr=fluxfacterr+'+ntostr(i))
           lc[w].fluxfact=f/rate[j];/eav[i];/(engup[j]-englow[j])
           lc[w].fluxfacterr=ferr/rate[j];/eav[i];/(engup[j]-englow[j])
        endelse 
     endfor 
     lc.flux=lc.ctr*lc.fluxfact
     lc.fluxerr=sqrt((lc.ctrerr/lc.ctr)^2+(lc.fluxfacterr/lc.fluxfact)^2)*lc.flux
     q=where(lc.flux eq 0,nq)
     if nq gt 0 then begin 
        help,q
        lc[q].fluxfact=lc[0].fluxfact
        lc[q].fluxfacterr=lc[0].fluxfacterr
        lc[q].flux=lc[q].ctr*lc[q].fluxfact
        lc[q].fluxerr=sqrt((lc[q].ctrerr/lc[q].ctr)^2+(lc[q].fluxfacterr/lc[q].fluxfact)^2)*lc[q].flux
     endif 
;     lc=lc[q]
     if i eq 0 then mwrfits,lc,'~/Desktop/GRB130427A/NuSTAR/xlc.fits',/create else begin
        if i eq 1 then create=1 else create=0
        mwrfits,lc,'~/Desktop/GRB130427A/NuSTAR/nustar_lcs.fits',create=create
     endelse 
     case i of 
        0: xlc=lc
        1: lc1=lc
        2: lc2=lc
        3: lc3=lc
        4: lc4=lc
     endcase
;  q=where(lc.flux eq 0)
;  stop  

  endfor 


  begplot,name='~/Desktop/GRB130427A/NuSTAR/lc_plot.ps',/color,font='helvetica',/land
  !p.charsize=1

  yrange=[1e-12,4e-10]
;  yrange=[1e-17,3e-11]
  plot,[1e5,6e5],yrange,/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',/nodata,xrange=[1e5,5e5],/xsty,/ysty,yminor=9,charsize=1.5,yrange=yrange,xticks=1,xtickname=[' ',' '],xminor=0

  axis,xticks=4,xtickv=[1e5,2e5,3e5,4e5,5e5],xtickname=['10!U5!N','2'+!tsym.times+'10!U5!N','3'+!tsym.times+'10!U5!N','4'+!tsym.times+'10!U5!N','5'+!tsym.times+'10!U5!N'],charsize=1.5

  days=[2,3,4,5]*86400.
  axis,xaxis=1,xtickv=days,charsize=1.5,xtickname=['2','3','4','5'],xticks=3,xminor=0
  xyouts,1.65e5,6e-10,'Time since GBM Trigger (days)',/data,charsize=1.5
  x=[tnustart[0],llc.time,reverse(llc.time),tnustart[0],tnustart[0]]
  y=[1.56e-10,llc.flux+llc.fluxerr[1],reverse(llc.flux-llc.fluxerr[0]),7.12e-11,1.59e-10]
  oplot,x,y,color=!magenta,thick=10
  polyfill,x,y,color=!magenta,/line_fill,orient=45
;  oploterror,llc.time,llc.flux,llc.fluxerr,/nohat,psym=3,errcolor=!magenta

  oploterror,xlc.time,xlc.flux,xlc.fluxerr,/nohat,psym=3,errcolor=!blue
  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].flux,xlc[i].flux],color=!blue
  oploterror,lc1.time,lc1.flux,lc1.fluxerr,/nohat,psym=3,errcolor=!red
  for i=0,n_elements(lc1)-1 do oplot,[lc1[i].tstart,lc1[i].tstop],[lc1[i].flux,lc1[i].flux],color=!red
  oploterror,lc2.time,lc2.flux,lc2.fluxerr,/nohat,psym=3,errcolor=!cyan
  for i=0,n_elements(lc2)-1 do oplot,[lc2[i].tstart,lc2[i].tstop],[lc2[i].flux,lc2[i].flux],color=!cyan
  oploterror,lc3.time,lc3.flux,lc3.fluxerr,/nohat,psym=3,errcolor=!orange
  for i=0,n_elements(lc3)-1 do oplot,[lc3[i].tstart,lc3[i].tstop],[lc3[i].flux,lc3[i].flux],color=!orange
  oploterror,lc4.time,lc4.flux,lc4.fluxerr,/nohat,psym=3,errcolor=!green
  for i=0,n_elements(lc4)-1 do oplot,[lc4[i].tstart,lc4[i].tstop],[lc4[i].flux,lc4[i].flux],color=!green

;  legend,['PL','BPL'],/right,/center,line=[2,1],box=0
  ;;;; lc fits
  ;; xrt
  model='pow'
  p=[1e-4,1.4]
  newp0=mpfitfun(model,xlc.time,xlc.flux,xlc.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror0)
;  lc_monte_pow,xlc,newp,pnames,chisq,dof,outperr,nsim=1000
  oplot,lc.time,pow(lc.time,newp0),color=!blue,line=2
  print,newp0,perror0,chisq/dof

  ;;; 3-10
  model='pow'
  p=[1e-4,1.4]
  newp1=mpfitfun(model,lc1.time,lc1.flux,lc1.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror1)  
  oplot,lc1.time,pow(lc1.time,newp1),color=!red,line=2
  print,newp1,perror1,chisq/dof
;;   model='bknpow'
;;   p=[1e-4,1.4,2e5,2.]
;;   newp=mpfitfun(model,lc1.time,lc1.ctr*c1,lc1.ctrerr*c1,p,bestnorm=chisq,dof=dof,/quiet)  
;;   t2=[lc1.time,newp[2]]
;;   t2=t2[sort(t2)]
;;   oplot,t2,bknpow(t2,newp),color=!red,line=1
;;   print,newp,chisq/dof


  ;; 10-30
  model='pow'
  p=[1e-4,1.2]
  newp2=mpfitfun(model,lc2.time,lc2.flux,lc2.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror2)  
  oplot,lc2.time,pow(lc2.time,newp2),color=!cyan,line=2
  print,newp2,perror2,chisq/dof

;;   model='bknpow'
;;   p=[1e-4,1.4,2e5,2.]
;;   newp=mpfitfun(model,lc2.time,lc2.ctr*c2,lc2.ctrerr*c2,p,bestnorm=chisq,dof=dof,/quiet)  
;;   t2=[lc2.time,newp[2]]
;;   t2=t2[sort(t2)]
;;   oplot,t2,bknpow(t2,newp),color=!cyan,line=1
;;   print,newp,chisq/dof

  ;; 30-79
  model='pow'
  p=[1e-4,1.4]
  newp3=mpfitfun(model,lc3.time,lc3.flux,lc3.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror3)  
  oplot,lc3.time,pow(lc3.time,newp3),color=!orange,line=2
  print,newp3,perror3,chisq/dof
;  model='bknpow'
;  p=[1e-4,1.4,2e5,2.]
;  newp=mpfitfun(model,lc3.time,lc3.ctr*c3,lc3.ctrerr*c3,p,bestnorm=chisq,dof=dof,/quiet)  
;  t2=[lc3.time,newp[2]]
;  t2=t2[sort(t2)]
;  oplot,t2,bknpow(t2,newp),color=!orange,line=1
;  print,newp,chisq/dof


  ;; 3-79
  model='pow'
  p=[1e-4,1.4]
  newp4=mpfitfun(model,lc4.time,lc4.flux,lc4.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror4)  
  oplot,lc4.time,pow(lc4.time,newp4),color=!green,line=2
  print,newp4,perror4,chisq/dof


  ;;; LAT
  
;  leflux=[3.08e-10,3.43e-10];/leav
  leflux=[1.47e-10,2.01e-10]
  tle=[mean([tnu1,tnu2]),mean([tnu3,tnu6])]
  plotsym,1,3,thick=5
  oplot,tle,leflux,psym=8,color=!magenta,thick=5
  oplot,[tnu1,tnu2],[leflux[0],leflux[0]],color=!magenta,thick=5
  oplot,[tnu3,tnu6],[leflux[1],leflux[1]],color=!magenta,thick=5


  a=!tsym.alpha
  pm=!tsym.plusminus
  xx=1.4
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4  

  legend,['!4Swift!X/XRT 0.3-10 keV ('+a+'='+ntostr(newp0[1],4)+pm+ntostr(perror0[1]*xx,4)+')',$
          '!4NuSTAR!X 3-10 keV ('+a+'='+ntostr(newp1[1],4)+pm+ntostr(perror1[1]*xx,4)+')',$
          '!4NuSTAR!X 10-30 keV ('+a+'='+ntostr(newp2[1],4)+pm+ntostr(perror2[1]*xx,4)+')',$
          '!4NuSTAR!X 30-79 keV ('+a+'='+ntostr(newp3[1],4)+pm+ntostr(perror3[1]*xx,4)+')',$
          '!4NuSTAR!X 3-79 keV ('+a+'='+ntostr(newp4[1],4)+pm+ntostr(perror4[1]*xx,4)+')',$
          '!4Fermi!X/LAT 100 MeV-100 GeV ('+a+'=1.17'+pm+'0.06)'],$
         textcolor=[!blue,!red,!cyan,!orange,!green,!magenta],box=0,charsize=1.2,position=[1.05e5,4e-12]

  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/lc_plot.ps ~/Desktop/GRB130427A/NuSTAR/lc_plot.pdf'

stop
;  plot,[1e5,6e5],[1e-6,1],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux Density (mJy)',/nodata,xrange=[1e5,6e5],/xsty

;  oploterror,xtime,xflux,xexp/2.,xerr,/nohat,psym=3,color=!blue,errcolor=!blue
;  oploterror,ntime1,nflux1,replicate(250.,n_elements(ntime1)),nflux1err,/nohat,psym=3,errcolor=!grey80
;  oploterror,ntime2,nflux2,replicate(250.,n_elements(ntime2)),nflux2err,/nohat,psym=3,errcolor=!grey50
;  oploterror,ntime3,nflux3,replicate(250.,n_elements(ntime3)),nflux3err,/nohat,psym=3,errcolor=!grey30



return
end 

pro lcs,xtime,xexp,xflux,xerr,utime,uexp,uflux,uerr,ufilt,ntime1,nfd1,nfd1err,ntime2,nfd2,nfd2err,ntime3,nfd3,nfd3err;,xtime2,xexp2,xflux2,xerr2

  plot,[1e5,6e5],[1e-6,1],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux Density (mJy)',/nodata,xrange=[1e5,6e5],/xsty
  oploterror,xtime,xflux,xexp/2.,xerr,/nohat,psym=3,color=!blue,errcolor=!blue
;  oploterror,xtime2,xflux2,xexp2/2.,xerr2,/nohat,psym=3,color=!blue,errcolor=!blue

  oploterror,ntime1,nfd1,replicate(250.,n_elements(ntime1)),nfd1err,/nohat,psym=3,errcolor=!grey80
  oploterror,ntime2,nfd2,replicate(250.,n_elements(ntime2)),nfd2err,/nohat,psym=3,errcolor=!grey50
;  oploterror,ntime3,nfd3,nfd3err,/nohat,psym=3,errcolor=!grey30

;  oploterror,utime,uflux,uexp/2.,uerr,/nohat,psym=3,color=!red,errcolor=!red

  color=[!cyan,!royalblue,!darkgreen,!green,!pink,!salmon,!purple,!violet,!p.color,!orange,!orangered,!sienna,!firebrick,!darkred,!yellow]
  filter=['W2','M2','W1','U','B','V','r','i','z','j','h','k']
  plotsym,0,1,/fill
  for i=0,n_elements(filter)-1 do begin
     w=where(filter[i] eq strtrim(ufilt,2),nw)
     plots,utime[w],uflux[w],color=color[i],psym=8
;     if nw gt 1 then oplot,utime[w],uflux[w],color=color[i]
     for j=0,nw-1 do begin
        oplot,[utime[w[j]]-uexp[w[j]]/2.,utime[w[j]]+uexp[w[j]]/2.],[uflux[w[j]],uflux[w[j]]],color=color[i]
        oplot,[utime[w[j]],utime[w[j]]],[uflux[w[j]]-uerr[w[j]],uflux[w[j]]+uerr[w[j]]],color=color[i]
     endfor 
  endfor 

  stop
;;; bin 500 s in nustar

  return
end 

pro seds

  lc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE_binned.qdp',/phil)
  spec=mrdfits('~/Desktop/GRB130427A/UL_specfits.fits',1)
  readcol,'~/Desktop/GRB130427A/NuSTAR/xrt.dat',xtime,xexp,xflux,xerr,format='(d,d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/uvot.dat',utime,uexp,uflux,uerr,ufilt,format='(d,d,d,d,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_3-10.txt',ntime1,nflux1,nflux1err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_10-30.txt',ntime2,nflux2,nflux2err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_30-79.txt',ntime3,nflux3,nflux3err,format='(d,d,d)'


  filter=['V','B','U','W1','M2','W2','white','r','i','z','j','h','k','Rc','Ic']
  lam_eff1=[5402,4329,3501,2634,2231,2030,3471]*1d-8 ;; cm
  lam_eff2=[0.64,0.79,0.91,1.26,1.6,2.22,0.64,0.79]*1d-4 ;; cm
  lam_eff=[lam_eff1,lam_eff2]
  ueng=dblarr(n_elements(utime))
  h=4.1357d-15; eV s
  c=3e8; cm/s
  for i=0,n_elements(utime)-1 do begin
     w=where(filter eq strtrim(ufilt[i],2))
     ueng[i]=h*c/lam_eff[w];*1d-3
  endfor 

;  uflux=uflux*1e-3
;  uerr=uerr*1e-3
  erg2jy = 1d23/2.42d17
  xflux=xflux*erg2jy*1e3
  xerr=xerr*erg2jy*1e3
  xeng=replicate(1e3,n_elements(xtime))

;  w=where(lc.tstart gt 1e5 and lc.tstop lt 1e6)
  w=where((lc.tstart gt 103379.00 and lc.tstop lt 160079.00) or (lc.tstart gt 411179.00 and lc.tstop lt 495179.00))
  lc=lc[w]
  xflux2=lc.src_rate*spec[1].unabs_cfratio ;;; count rate to flux
  xerr2=lc.src_rate_err*spec[1].unabs_cfratio
  xfd=flux2jy(xflux2,spec[1].phind,eeff=1.) ;; flux to flux density @ 1 keV
  xfderr=xerr2/xflux2*xfd
  xflux2=xfd*1e3 ;;; Jy to mJy
  xerr2=xfderr*1e3
  xexp2=(lc.tstop-lc.tstart)
  xtime2=lc.time

  nfd1=flux2jy(nflux1,1.8,eeff=6.5,low=3,high=10)*1e3
  nfd2=flux2jy(nflux2,1.8,eeff=15,low=10,high=30)*1e3
  nfd3=flux2jy(nflux3,1.8,eeff=54.5,low=30,high=79)*1e3
  nfd1err=nflux1err/nflux1*nfd1
  nfd2err=nflux2err/nflux2*nfd2
  nfd3err=nflux3err/nflux3*nfd3
stop
  lcs,xtime2,xexp2,xflux2,xerr2,utime,uexp,uflux,uerr,ufilt,ntime1,nfd1,nfd1err,ntime2,nfd2,nfd2err,ntime3,nfd3,nfd3err

stop

  !p.multi=[0,1,2]
  plot,[1e-2,1e5],[1e-8,10],/nodata,xtitle='Energy (eV)',ytitle='Flux Density (Jy)',/xlog,/ylog
  w1=where(utime lt 1.5e5)
  wx1=where(xtime lt 1.5e5)
  oploterror,ueng[w1],uflux[w1],uerr[w1],/nohat,psym=5,color=!red
  oploterror,xeng[wx1],xflux[wx1],xerr[wx1],/nohat,psym=5,color=!blue
  
  plot,[1e-2,1e5],[1e-8,10],/nodata,xtitle='Energy (eV)',ytitle='Flux Density (Jy)',/xlog,/ylog
  w2=where(utime gt 1.5e5)
  wx2=where(xtime gt 1.5e5)
  oploterror,ueng[w2],uflux[w2],uerr[w2],/nohat,psym=5,color=!red
  oploterror,xeng[wx2],xflux[wx2],xerr[wx2],/nohat,psym=5,color=!blue
  !p.multi=0

  stop
return
end 

pro grb130427a

  ;;; nustar predictions

  cd,'~/Desktop/GRB130427A/'
  begplot,name='XRT_LC.eps',/color,font='helvetica',/land
;  !p.multi=[0,1,3]
;  plot_like_qdp,yrange=[1e-1,1e5],/phil,/ysty,ytitle='XRT Count Rate (0.3-10 keV) (cts/s)'
;  plot_like_qdp,/phil,ytitle='XRT Flux (0.3-10 keV) (erg/cm2/s)',/useflux,flux=4e-11
;  plot_like_qdp,/phil,ytitle='Predicted NuSTAR (3-80 keV) (cts/s)',/useflux,flux=0.88,yrange=[1e-1,1e5],/ysty

  lc=lcout2fits(/phil)
  nu3_80=8.875e-01
  nu3_10=6.327E-01
  nu10_20=1.86e-1
  nu20_40=5.51e-2
  nu40_80=1.286e-2

;2013-04-28T12:31:07 - 2013-04-29T04:16:07 (1.048487634999924E+08 -
;1.049044494249942E+08)

;2013-05-02T02:01:07 - 2013-05-02T14:01:07 (1.051574488800000E+08 -
;1.051992000000000E+08)

;2013-05-02T18:06:07 - 2013-05-03T01:21:07 (1.052159018200000E+08 -
;1.052399999999999E+08)

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')

  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=388741688d
  tnu1=nu1-battrig
  tnu2=nu2-battrig
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

  ffact=4e-11

  plot,[100,1e6],[1e-3,1e5],xrange=[1e4,2e6],yrange=[1e-4,10],/xlog,/ylog,/nodata,xtitle='Time since BAT trigger',ytitle='XRT scaled to !4NuSTAR!X Count Rate (cts/s)',/xsty
  polyfill,[tnu1,tnu2,tnu2,tnu1,tnu1],[1e-4,1e-4,10,10,1e-4],color=!grey70
  polyfill,[tnu3,tnu4,tnu4,tnu3,tnu3],[1e-4,1e-4,10,10,1e-4],color=!grey70
  polyfill,[tnu5,tnu6,tnu6,tnu5,tnu5],[1e-4,1e-4,10,10,1e-4],color=!grey70
;  axis,xaxis=0
;  axis,xaxis=1,xrange=[1e4,1e6]
  
  nu=[nu3_10,nu10_20,nu20_40,nu40_80,nu3_80]
  color=[!red,!blue,!green,!orange,!cyan]
  for j=0,4 do begin

     oploterror,lc.time,lc.src_rate*nu[j],lc.src_rate_err*nu[j],psym=3,/nohat,color=color[j]
     for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate*nu[j],lc[i].src_rate*nu[j]],color=color[j]
  endfor 

  legend,['NuSTAR Obs','3-80 keV','3-10 keV','10-20 keV','20-40 keV','40-80 keV'],textcolor=[!grey70,color[4],color[0:3]],box=0,/top,/right

  endplot

stop

return
end 
