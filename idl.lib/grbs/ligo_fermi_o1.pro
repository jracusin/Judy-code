pro ligo_fermi_o1

  ;;; LAT sGRB plot

  cd,'~/papers/ligo_fermi_O1/ShortLighcturve'

;  begplot,name='LAT_sgrb_LC_limits.ps',/land,/color,font='helvetica'

  files=file_search('like*txt')
  
  MeV2erg=1.6027e-6
  xrange=[1e-2,1e4]
  yrange=[1e-11,1e-4]
  p=plot(xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',xtickformat='loglabels',/xsty,xminor=9,yminor=9,/ysty,font_size=12.)

;  plot,xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',xtickformat='loglabels',/xsty,xminor=9,yminor=9,/ysty,charsize=2.

;  plotsym,1,3,thick=5

;  color=[!navyblue,!blue,!dodgerblue,!cyan,!green,!lightgreen]
  color=['navy','blue','dodger blue','cyan','steel blue','sky blue']
  grbs=['GRB081024B','GRB090510','GRB110529A','GRB120619A','GRB120830A','GRB140402A']

  for i=0,n_elements(files)-1 do begin
     readcol,files[i],fitNum,tstart,tend,tmedian,Emin,Emax,Ts,NobsTot,Npred_100,Npred_1000,index,errorI,Flux,ErrorF,EnergyFlux,ErrorEF,Fluence,/silent
     wdet=where(erroref gt 0,nwdet)
     p2=errorplot(tmedian[wdet],energyflux[wdet]*mev2erg,erroref[wdet]*mev2erg,/overplot,symbol=3,errorbar_capsize=0,linestyle='none',color=color[i],errorbar_color=color[i])
     for j=0,nwdet-1 do p2=plot([tstart[wdet[j]],tend[wdet[j]]],[energyflux[wdet[j]],energyflux[wdet[j]]]*mev2erg,color=color[i],/overplot)
;     for k=0,nwdet-1 do begin
;        j=wdet[k]
;        p2=plot([tstart[j],tend[j]],[energyflux[j],energyflux[j]]*mev2erg,color=color[i],/overplot)
;        p3=plot([tmedian[j],tmedian[j]],[energyflux[j]-erroref[j],energyflux[j]+erroref[j]]*mev2erg,color=color[i],/overplot)
;        oplot,[tstart[j],tend[j]],[energyflux[j],energyflux[j]]*mev2erg,color=color[i]
;        oplot,[tmedian[j],tmedian[j]],[energyflux[j]-erroref[j],energyflux[j]+erroref[j]]*mev2erg,color=color[i]
;     endfor
     wul=where(errorf eq 0 and tmedian lt 1e4,nwul)
     if nwul gt 0 then begin
;        p3=errorplot(tmedian[wul],energyflux[wul]*mev2erg,tmedian[wul]-tstart[wul],erroref[wul]*mev2erg,/overplot,symbol=3,errorbar_capsize=0,linestyle='none',color=color[i])
        for j=0,nwul-1 do p3=arrow([tmedian[wul[j]],tmedian[wul[j]]],[energyflux[wul[j]],energyflux[wul[j]]*0.2]*mev2erg,color=color[i],head_size=0.5,/data,/current)
;        for k=0,nwul-1 do begin
;           j=wul[k]
;           oplot,[tstart[j],tend[j]],[energyflux[j],energyflux[j]]*mev2erg,color=color[i]
;           oplot,[tmedian[j],tmedian[j]],[energyflux[j],energyflux[j]]*mev2erg,color=color[i],psym=8
;        endfor 
     endif 

  endfor 

  gws=['GW150914','LVT151012','GW151226']
;  gwcolor=[!magenta,!salmon,!red]
;  gwcolor=['magenta','red','orange']
  gwcolor=['red','orange','lime green']

;  legend,[grbs,gws],textcolor=[color,0,gwcolor],box=0,/top,/right
  leg=[grbs,'',gws]
  c=[color,'white',gwcolor]
  for i=0,n_elements(leg)-1 do t=text(0.7,0.8-i*0.04,leg[i],font_color=c[i],/overplot)

  t=10
;  plotsym,1,4,thick=t

  ;;; GW150914
;  flim=[1e-9,3.7e-09]
  flim=[1e-9,6.2e-9]
  xr=[4442,4867]
  xmen=mean(xr)
  p2=plot(xr,[flim[1],flim[1]],/overplot,thick=2,color=gwcolor[0])
  a=arrow([xmen,xmen],[flim[1],flim[0]],/data,head_size=1,color=gwcolor[0],/current,thick=2)

;  xx=[xr[0],xr[1],xr[1],xr[0],xr[0]]
;  yy=[flim[0],flim[0],flim[1],flim[1],flim[0]]
;  p4=polygon(xx,yy,fill_color=gwcolor[0],transparency=50,/overplot,/data,color=gwcolor[0],thick=2)

;  polyfill,xx,yy,color=gwcolor[0]
;  plots,xmen,flim,psym=8,color=gwcolor[0]
;  oplot,xr,[flim,flim],color=gwcolor[0],thick=t

  ;; LVT151012
;  xmin=[xrange[0],1000,4600,6500]
;  xmax=[1200,4600,6300,9000]
;  n=n_elements(xmin)
;  xmen=fltarr(n)
;  flim=[2e-9,1e-9,1.2e-9,1.1e-9]
;  for i=0,n-1 do xmen[i]=mean([xmin[i],xmax[i]])
  flim0=2.0e-10
  flim1=6.1e-10
  flim=[1e-10,5e-10]
  xr=[xrange[0],8000]
  xmen=mean(xr)
  xx=[xr[0],xr[1],xr[1],xr[0],xr[0]]
  yy=[flim0,flim0,flim1,flim1,flim0]
;  p4=polygon(xx,yy,fill_color=gwcolor[1],transparency=30,/overplot,/data,color=gwcolor[1],thick=2)
  p2=plot(xr,[flim[1],flim[1]],/overplot,thick=2,color=gwcolor[1])
  a=arrow([xmen,xmen]*0.5,[flim[1],flim[0]],/data,head_size=1,color=gwcolor[1],/current,thick=2)
 

;  plots,xmen,flim,psym=8,color=gwcolor[1]
;  oplot,xr,[flim,flim],color=gwcolor[1],thick=t
;  for i=0,n-1 do oplot,[xmin[i],xmax[i]],[flim[i],flim[i]],color=gwcolor[1],thick=t

  ;; GW151226
  flim0=[5.0e-10,1.9e-10]
  flim1=[2.2e-9,4.9e-10]
  xmin=[xrange[0],xrange[0]]
  xmax=[1200,10000.]

  flim=[8e-11,3e-10]
  xr=[xrange[0],xmax[1]]
  xmen=mean(xr)
  p2=plot(xr,[flim[1],flim[1]],/overplot,thick=2,color=gwcolor[2])
  a=arrow([xmen,xmen],[flim[1],flim[0]],/data,head_size=1,color=gwcolor[2],/current,thick=2)

;  xr=[xrange[0],1200]
;  xmen=mean(xr)
;   xmin=[xrange[0],3000,8300]
;   xmax=[3300,4000,11000]
   n=n_elements(xmin)
   xmen=fltarr(n)
;   for i=0,1 do begin
;      xr=[xmin[i],xmax[i]]
;      xx=[xr[0],xr[1],xr[1],xr[0],xr[0]]
;      yy=[flim0[i],flim0[i],flim1[i],flim1[i],flim0[i]]
;      p4=polygon(xx,yy,fill_color=gwcolor[2],transparency=50,/overplot,/data,color=gwcolor[2],thick=2)
;   endfor 

  ;; flim=[1.5e-9,1.1e-8,9.3e-10]
;   for i=0,n-1 do xmen[i]=mean([xmin[i],xmax[i]])
;  plots,xmen,flim,psym=8,color=gwcolor[2]
;  oplot,xr,[flim,flim],color=gwcolor[1],thick=t
;  for i=0,n-1 do oplot,[xmin[i],xmax[i]],[flim[i],flim[i]],color=gwcolor[2],thick=t
;  endplot
;  ps2pdf,'LAT_sgrb_LC_limits.ps'
   p.save,'~/papers/ligo_fermi_O1/LAT_sgrb_LC_limits.png'
   p.save,'~/papers/ligo_fermi_O1/LAT_sgrb_LC_limits.pdf'
   p.close

stop

  return
end 

pro gbm_plot

  cd,'~/papers/ligo_fermi_O1'
  gbmcat=mrdfits('3gbm.fits',1)
  w=where(gbmcat.t90 le 2. and gbmcat.t90 gt 0)
  gbmcat=gbmcat[w]
  pflux=gbmcat.flux_1024
  fluence=gbmcat.fluence

  bins=[logarr(1e-8,1e-4)]
  nbins=n_elements(bins)
  y=fltarr(nbins)
  x=fltarr(nbins)
  for i=1,nbins-1 do begin
     w=where(fluence ge bins[i-1],nw); and pflux lt bins[i],nw)
     q=where(fluence le bins[i],nq)
     if nq gt 0 then y[i]=nw
     x[i]=bins[i]
     
  endfor 

;  gwcolor=['magenta','red','orange']
  gwcolor=['red','orange','lime green']

  p=plot(x,y/max(y),/xlog,/ylog,yrange=[0.01,1.5],xrange=[1e-8,1e-4],/xsty,/ysty,xtitle='Fluence (10-1000 keV; erg cm!U-2!N)',ytitle='Fraction of sGRB',/histogram)

  ;;gw150914
  flim=[1.4e-7,(2.4+1.7)*1e-7]
  xx=[flim[0],flim[1],flim[1],flim[0],flim[0]]
;  yy=[4,4,5.5,5.5,4]/10.
;  p2=polygon(xx,yy,fill_color=gwcolor[0],/overplot,/data,color=gwcolor[0],transparency=50,thick=2)
;  p2=arrow([flim[1]  
  i=interpol(y/max(y),x,flim[1])
  p2=plot([flim[0],flim[1]],[i,i],/overplot,color=gwcolor[0],thick=3)

  ;;lvt151012
  flim=[0.55e-6,1.2e-6] ;; erg/cm2
;  flim=[1e-8,1.2e-6]
  xx=[flim[0],flim[1],flim[1],flim[0],flim[0]]
  yy=[2.5,2.5,3.5,3.5,2.5]
;  p2=polygon(xx,yy,fill_color=gwcolor[1],/overplot,/data,color=gwcolor[1],transparency=50,thick=2)
  i=interpol(y/max(y),x,flim[1])
  p2=arrow([flim[1],flim[0]],[i,i],/data,head_size=1,color=gwcolor[1],/current,thick=2)

  ;;; Gw151226
  flim=[0.5e-6,1.1e-6] ;; erg/cm2
;  flim=[1e-8,1.1e-6]
  xx=[flim[0],flim[1],flim[1],flim[0],flim[0]]
  yy=[1.5,1.5,2,2,1.5]
;  p2=polygon(xx,yy,fill_color=gwcolor[2],/overplot,/data,color=gwcolor[2],transparency=50,thick=2)
  i=interpol(y/max(y),x,flim[1])
  p2=arrow([flim[1],flim[0]],[i,i],/data,head_size=1,color=gwcolor[2],/current,thick=2)

  gws=['GW150914','LVT151012','GW151226']
  leg=['GBM sGRBs','',gws]
  c=['black','white',gwcolor]
  for i=0,n_elements(leg)-1 do t=text(0.7,0.8-i*0.04,leg[i],font_color=c[i],/overplot)

  gws=['GW150914','LVT151012','GW151226']

  p.save,'~/papers/ligo_fermi_O1/gbm_sgrbs.png'
  p.save,'~/papers/ligo_fermi_O1/gbm_sgrbs.pdf'
  p.close
stop
  return
end 
