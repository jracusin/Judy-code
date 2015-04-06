pro calc_factor,srcra,srcdec,ra,dec,rx,expf,rad,fx,alttotpsf,altflux
  
  f2c=5e-11
  pixscale=0.1
  n=rad/pixscale*2.+1
  srcdist=dblarr(n,n) & srcdist1=srcdist
  altdist=dblarr(n,n)
  srcpsf=dblarr(n,n)
  altpsf=dblarr(n,n)
  
  pix=(findgen(n)-n/2.+0.5)*pixscale
  arctopix=2.353d
  xx=srcra+pix*arctopix/3600.
  yy=srcdec+pix*arctopix/3600.
  for y=0,n-1 do begin
     for x=0,n-1 do begin
        srcdist[x,y]=sqrt((srcra-xx[x])^2+(srcdec-yy[y])^2.)*3600./arctopix
        intup=interpol(expf,rx,srcdist[x,y]+pixscale/2.)
        intdown=interpol(expf,rx,srcdist[x,y]-pixscale/2.)
        diff=intup-intdown
        srcpsf[x,y]=diff/(2.*!pi*srcdist[x,y]/pixscale)
        
        altdist[x,y]=sqrt((ra-xx[x])^2+(dec-yy[y])^2.)*3600./arctopix
        intup=interpol(expf,rx,altdist[x,y]+pixscale/2.)
        intdown=interpol(expf,rx,altdist[x,y]-pixscale/2.)
        diff=intup-intdown
        altpsf[x,y]=diff/(2.*!pi*altdist[x,y]/pixscale)
     endfor 
  endfor 
  wsrc=where(srcdist le rad and srcdist ne 0.)
  srctotpsf=total(srcpsf[wsrc])

  alttotpsf=total(altpsf[wsrc])
  
  w=where(srcdist ne 0.)
;  print,total(srcpsf[w]),total(altpsf[w])
  altflux=alttotpsf*fx/f2c
  print,srctotpsf,alttotpsf,altflux
  return
end 

pro sn2008d
  
  f2c=5e-11
  psfquick,2,2,rx,expf,/silent ;;x in pixels
  begplot,name='source_extraction.ps',/color  
  srcra=137.377604167d
  srcdec=33.1389333333d
  srcerr=0.19/3600d
  
  ra1=137.3825d
  dec1=33.1347305556d
  err1=0.05/3600d
;  fx1=10.8d-14
  fx1=1.33e-13
  
  ra2=137.3800625d
  dec2=33.1393972222d
  err2=0.09/3600d
;  fx2=3.9d-14
  fx2=5.55e-14
  
  ra3=137.377858333d
  dec3=33.1349416667d
  err3=0.19/3600d
;  fx3=0.7d-14
  fx3=8.93e-15
  
  sep1=separation(srcra,srcdec,ra1,dec1)
  sep2=separation(srcra,srcdec,ra2,dec2)
  sep3=separation(srcra,srcdec,ra3,dec3)
  
  ras=[srcra,ra1,ra2,ra3]
  decs=[srcdec,dec1,dec2,dec3]
  plot,ras,decs,psym=1,/yno,/iso,xrange=[max(ras),min(ras)],xtitle='R.A. (deg)',ytitle='Dec (deg)'
  tvcircle,srcerr,srcra,srcdec,/data
  tvcircle,err1,ra1,dec1,/data
  tvcircle,err2,ra2,dec2,/data
  tvcircle,err3,ra3,dec3,/data
  xyouts,ra1,dec1+0.0002,'1',/data
  xyouts,ra2,dec2+0.0002,'2',/data
  xyouts,ra3,dec3+0.0002,'3',/data
  
;  tvcircle,4.*2.35/3600.,srcra,srcdec,/data,color=!red
  tvcircle,6.*2.35/3600.,srcra,srcdec,/data,color=!yellow
  
  rad=4.                       ;pixels
  tvcircle,rad*2.35/3600.,srcra,srcdec,/data,color=!green
  print
  print,'Using '+ntostr(rad)+' pixel extraction radius'
  print,'Source 1:'
  calc_factor,srcra,srcdec,ra1,dec1,rx,expf,rad,fx1,alttotpsf1,altflux1
  print,'Source 2:'
  calc_factor,srcra,srcdec,ra2,dec2,rx,expf,rad,fx2,alttotpsf2,altflux2
  print,'Source 3:'
  calc_factor,srcra,srcdec,ra3,dec3,rx,expf,rad,fx3,alttotpsf3,altflux3
  
  sub=total(altflux1+altflux2+altflux3)
  print,'Total: '+ntostr(sub)
  
  begplot,name='SN2008d_lc.ps',/color,/land
  plot_like_qdp
  lc=lcout2fits()
  n=n_elements(lc)
  j=indgen(6)+n-6
;  subrate=lc[j].src_rate-sub 
  xrtrateerr=lc[j].src_rate_err
  xrtcounts=lc[j].src_counts
  backcounts=lc[j].tot_back_cts;*lc[j].back_area_corr
  corr=lc[j].pu_corr*lc[j].psf_corr
  exptime=lc[j].exptime
  cxocounts=sub*exptime
  subrate=(xrtcounts-cxocounts)*lc[j].pu_corr*lc[j].psf_corr/exptime
;  subrateerr=(xrtcounts-cxocounts)/sqrt(xrtcounts+cxocounts+backcounts*2.)/exptime*2.
  subrateerr=sqrt(xrtcounts+cxocounts*2.+backcounts*2.)/exptime;*sqrt(3.)
;  subrateerr=sqrt(subrateerr^2+((xrtcounts/corr-cxocounts)/sqrt(xrtcounts/corr+cxocounts)/exptime)^2)
  print,subrateerr
  
  oploterror,lc[j[0:4]].time,subrate[0:4],subrateerr[0:4],/nohat,psym=3
  for i=0,4 do oplot,[lc[j[i]].tstart,lc[j[i]].tstop],[subrate[i],subrate[i]]
  
  ;;CXO flux *5e-11
  plotsym,0,1,/fill
  cxoflux=1.24d-14
  cxofluxerr=sqrt(10.)*cxoflux/10.
  plots,898810.00,cxoflux/f2c,psym=8,color=!green
  oplot,[889860.00,907760.00],[cxoflux,cxoflux]/f2c,color=!green
  oplot,[898810.00,898810.00],[cxoflux-cxofluxerr,cxoflux+cxofluxerr]/f2c,color=!green
  
  uplim=dblarr(n_elements(xrtcounts))
  for i=0,n_elements(xrtcounts)-1 do begin
     confidlev,backcounts[i]+cxocounts[i],xrtcounts[i]+backcounts[i],0.9973,smin,smax
     uplim[i]=smax
  endfor 
  
  confidlev,total(backcounts+cxocounts),total(xrtcounts+backcounts),0.9973,smin,smax
  
  if smin gt 0 then begin 
     combctr=(total(xrtcounts)-total(cxocounts))/total(exptime) 
     comberr=sqrt(total(xrtcounts)+total(cxocounts)+total(backcounts*2.))/total(exptime)
     plotsym,0,1,/fill
     usearr=0
  endif else begin 
;     combctr=(total(xrtcounts)-total(cxocounts))/total(exptime) 
     combctr=smax/total(exptime)
     comberr=0.
     plotsym,1,5,thick=4
     usearr=1
  endelse 
  combtime=(lc[j[5]].tstop-lc[j[0]].tstart)/2.+lc[j[0]].tstart
  plots,combtime,combctr,psym=8,color=!blue
  oplot,[lc[j[0]].tstart,lc[j[5]].tstop],[combctr,combctr],color=!blue
  oplot,[combtime,combtime],[combctr-comberr,combctr+comberr],color=!blue
  
  endplot
  plotsym,0,1,/fill
  begplot,name='SN2008D_lc_flux.ps',/color,/land
;  plot_like_qdp,flux=f2c,yrange=[1e-15,1e-9]
  wdet=where(lc.src_rate_err gt 0,nwdet)
  wnodet=where(lc.src_rate_err lt 0,nwnodet)

  ploterror,lc[wdet].time,lc[wdet].src_rate*f2c,lc[wdet].src_rate_err*f2c,psym=8,xtitle='Time since 2008-01-09-13:30 (s)',ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,/nohat,xrange=[10,1e7],yrange=[1e-15,1e-9]
  for i=0,nwdet-1 do oplot,[lc[wdet[i]].tstart,lc[wdet[i]].tstop],[lc[wdet[i]].src_rate,lc[wdet[i]].src_rate]*f2c
  plotsym,1,5,thick=4
  if nwnodet gt 0 then begin
     plots,lc[wnodet].time,lc[wnodet].src_rate*f2c,psym=8
     oplot,[lc[wnodet].tstart,lc[wnodet].tstop],[lc[wnodet].src_rate*f2c,lc[wnodet].src_rate*f2c]
  endif 
  plotsym,0,1,/fill
  
  if usearr then plotsym,1,5,thick=4
  plots,combtime,combctr*f2c,psym=8,color=!blue
  oplot,[lc[j[0]].tstart,lc[j[5]].tstop],[combctr,combctr]*f2c,color=!blue
  oplot,[combtime,combtime],[combctr-comberr,combctr+comberr]*f2c,color=!blue
  
  plotsym,1,5,thick=4
  oplot,lc[j].time,uplim/exptime*f2c,psym=8,color=!orange
  for i=0,5 do oplot,[lc[j[i]].tstart,lc[j[i]].tstop],[uplim[i]/exptime[i],uplim[i]/exptime[i]]*f2c,color=!orange
  
  plotsym,0,1,/fill
  oploterror,lc[j[0:4]].time,subrate[0:4]*f2c,subrateerr[0:4]*f2c,/nohat,psym=3,errcolor=!red
  for i=0,4 do oplot,[lc[j[i]].tstart,lc[j[i]].tstop],[subrate[i],subrate[i]]*f2c,color=!red
  
  cxoflux=1.24d-14
  cxofluxerr=sqrt(10.)*cxoflux/10.
  plots,898810.00,cxoflux,psym=8,color=!green
  oplot,[889860.00,907760.00],[cxoflux,cxoflux],color=!green
  oplot,[898810.00,898810.00],[cxoflux-cxofluxerr,cxoflux+cxofluxerr],color=!green
  
  
  legend,['XRT observed','Chandra','Individual Corrected XRT','Combined Corrected XRT','Corrected 3'+!tsym.sigma+' upper limits'],/top,/right,box=0,textcolor=[!black,!green,!red,!blue,!orange]
  endplot
  stop
  return
end 
