pro ligo_150914

;;; plot GBM light
  cd,'~/papers/ligo_lat_150914/'
  file='GBMTRIGCAT-v2-DATA-LAT2CATALOG-P8_TRANSIENT010E-090510016-ExtendedEmission-like_090510016.txt'
  
  readcol,file,fitnum,tstart,tend,tmed,emin,emax,ts,nobstot,npred_100,npred_1000,index,error1,flux,fluxerr,eflux,efluxerr,fluence

  xerr=fltarr(2,n_elements(tmed))
  xerr[0,*]=tmed-tstart
  xerr[1,*]=tend-tmed
  wul=where(efluxerr eq 0 and tend lt 3e4,nul)


;  p=plot([1e-2,1e6],[1e-10,1e-2],/xlog,/ylog,/nodata,xtitle='Time
;  Since Trigger (s)',ytitle='Flux (100 MeV - 10 GeV) [ergs cm!U-2!N
;                                             s!U-1!N]',font_size=14,xminor=9,yminor=9,xtickvalues=[1e-2,0.1,1,10,1e2,1e3,1e4,1e5,1e6],ytickvalues=10^(findgen(9)-10),xrange=[1e-2,1e6],yrange=[1e-10,1e-2],xtickformat='loglabels')
  p=plot([1e-2,1e6],[1e-10,1e-2],/xlog,/ylog,/nodata,ytitle='Flux!L100 MeV-10 GeV!N (ergs cm!U-2!N s!U-1!N)',font_size=14,xminor=9,yminor=9,xtickvalues=[1e-2,0.1,1,10,1e2,1e3,1e4],ytickvalues=10^(findgen(9)-10),xrange=[1e-2,2e4],yrange=[1e-10,1e-2],position=[0.18,0.4,0.95,0.95],xtickname=replicate(' ',7),ytickname=[' ','10!U'+ntostr(indgen(8)-9)+'!N'],dimensions=[500,700])

  zgrb=0.903
  zgw=0.09
;  z1=1.+z
  z1=(1.+zgrb)/(1.+zgw)
  kgrb=fltarr(n_elements(index))
  kgw=kgrb
  for i=0,n_elements(index)-1 do begin
     kgw[i]=kcorr(zgw,index[i],/pow,emin=emin[i]*1e3,emax=emax[i]*1e3)
     kgrb[i]=kcorr(zgrb,index[i],/pow,emin=emin[i]*1e3,emax=emax[i]*1e3)
  endfor 

  lum=eflux*4*!pi*lumdist(zgrb)^2*kgrb
  lumerr=efluxerr*4*!pi*lumdist(zgrb)^2*kgrb
  
  mev2erg=1.60218e-6
  f=lum/(4*!pi*lumdist(zgw)^2*kgw)*mev2erg
  ferr=lumerr/(4*!pi*lumdist(zgw)^2*kgw)*mev2erg

  p=errorplot(tmed/z1,f,xerr/z1,ferr,errorbar_capsize=0,linestyle='none',/overplot);,color='red')
  for i=0,nul-1 do a=arrow([tmed[wul[i]],tmed[wul[i]]]/z1,[f[wul[i]],0.5*f[wul[i]]],/data,/current,head_size=0.5);,color='red')

;  p=errorplot(tmed,eflux*mev2erg,xerr,efluxerr*mev2erg,errorbar_capsize=0,linestyle='none',/overplot,color='magenta')
;  for i=0,nul-1 do a=arrow([tmed[wul[i]],tmed[wul[i]]],[eflux[wul[i]],0.5*eflux[wul[i]]]*mev2erg,/data,/current,head_size=0.5,color='magenta')

  t=text(2,1e-4,'GRB 090510 scaled to z=0.09',font_size=12,/data);,color='red')
;  t=text(10,1e-6,'GRB 090510 as observed at z=0.90',font_size=14,/data,color='magenta')


;  geflux=[0.86e-9,3.64e-9]
  geflux=[1.46000001e-09  , 2.85000001e-09 ,  6.18950007e-09]
;  gpflux=geflux*691.514 ;;; from eflux to pflux scaling in 090510
  xr=[4442,4867]
  xmen=mean(xr)
;  o=polygon(xr[[0,1,1,0,0]],[geflux[[0,0,1,1,0]]],fill_color='red',/overplot,/data,color='red')
  t=text(0.1,2e-9,'LAT Upper Limit during T!L1!N',/data,color='red',font_size=14)
  pl=plot(xr,[geflux[2],geflux[2]],/overplot,color='red',thick=5)
  a=arrow([xmen,xmen],[geflux[2],geflux[2]*0.2],color='red',head_size=1,/data,/current,thick=3)


  readcol,'~/papers/ligo_lat_150914/sky_coverage.txt',time,frac

  p2=plot([1e-2,1e6],[0,100],/nodata,/xlog,xtitle='Time Since Trigger (s)',ytitle='Cumulative Sky Coverage (%)',font_size=14,xminor=9,yminor=9,xtickvalues=[1e-2,0.1,1,10,1e2,1e3,1e4],xrange=[1e-2,2e4],yrange=[0,100],position=[0.18,0.15,0.95,0.4],/current,xtickformat='loglabels')
  p3=plot([1e-2,time,1e6],[min(frac),frac,99],/xlog,/overplot,color='blue',thick=3)

  p.save,'GRB090510_lc_scaled_skycov.png'

;  p.save,'GRB090510_lc_scaled.png'
  p.close

stop
return
end 
