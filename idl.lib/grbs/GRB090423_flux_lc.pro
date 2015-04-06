pro GRB090423_flux_lc
  
  ;readcol,'../flux.qdp',time,tbin0,tbin1,flux,fluxerr
  lc=lcout2fits(/phil)
  w=where(lc.time lt 1000)
  flux=dblarr(n_elements(lc)) & fluxerr=flux
  
  fluxfact1=3.7e-11 ;;observed not unabs
  fluxfact2=4.8d-11 ;;observed not unabs
  
  flux[w]=lc[w].src_rate*fluxfact1
  fluxerr[w]=lc[w].src_rate_err*fluxfact1   
  w=where(lc.time gt 1000)
  flux[w]=lc[w].src_rate*fluxfact2
  fluxerr[w]=lc[w].src_rate_err*fluxfact2
  ct0=263587109d  
  ct1=263684303d    
  battime=262166119d  
  ctime=((ct1-ct0)/2d)+ct0-battime
  cflux=8.901d-16  
  cfluxerr=cflux/sqrt(5d) 
  
  begplot,name='GRB090423_lc_wcxo.ps',/landscape
  ploterror,lc.time,flux,fluxerr,psym=3,/nohat,xrange=[1e2,1e7],/xlog,/ylog,yrange=[1e-16,1e-9],xtitle='Time since BAT trigger (s)',ytitle='Flux (0.3-10.0 keV) (erg cm!U-1!N s!U-1!N)',ytickf='loglabels',/ysty,yticks=7,yminor=9
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[flux[i],flux[i]] 
  plotsym,1,3,thick=3
  plots,lc[i-1].time,flux[i-1],psym=8
  plotsym,0,/fill
  plots,ctime,cflux,psym=8                                            
  oplot,[ct0-battime,ct1-battime],[cflux,cflux]
  oplot,[ctime,ctime],[cflux-cfluxerr,cflux+cfluxerr]    
  endplot
  
  lc.src_rate=flux
  lc.src_rate_err=fluxerr
  lc2=lcout2fits(/empty)
  lc2.time=ctime
  lc2.src_rate=cflux;/fluxfact2
  lc2.src_rate_err=cfluxerr;/fluxfact2
  lc2.tstart=ct0-battime
  lc2.tstop=ct1-battime
  lc2.exptime=31796d
  lc2.pu_corr=1.
  lc2.src_counts=4.
  lc2.tot_back_cts=(12.566/1178.097)*(6.+10.)
  q=where(lc.src_rate_err eq 0,nq)
  if nq gt 0 then lc[q].src_rate_err=-1.
  concat_structs,lc,lc2,tmp
  lc=tmp
  write_lc,lc,'lc_newout_wchandra.txt'
  
  stop
  return
end 
