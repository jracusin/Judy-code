@fit_functions
@calc_eiso
pro predict_compair

  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog_parse.fits',1)
  w=where(gbm.model_bestfit eq 'FLNC_BAND' and gbm.alpha_band lt 0,nw)
  evtoerg=1.60217646d-12
  kevtoerg=evtoerg*1d3
  

  emin=5e3 ;; keV
  emax=30e3 ;; keV

  e0=logarr(10,emax,bin=0.1)
  e=logarr(emin,emax,bin=0.1)

  eflux=dblarr(nw) & efluence=eflux 
  norm=dblarr(nw) & beta=dblarr(nw)

  for i=0,nw-1 do begin 
     p=[gbm[w[i]].norm_band,gbm[w[i]].alpha_band,gbm[w[i]].e0_band,gbm[w[i]].beta_band,50.]
     f=band(e,p)

     fpow=pow(e,[1.,-gbm[w[i]].beta_band],tnorm=1e3)
     norm[i]=band(1e3,p)/pow(1e3,[1.,-gbm[w[i]].beta_band],tnorm=1e3)
     beta[i]=-gbm[w[i]].beta_band

     eflux[i]=int_tabulated(e,f*e,/double)*1e3;*kevtoerg
     efluence[i]=eflux[i]*gbm[w[i]].t90

;     plot,e0,band(e0,p),/xlog,/ylog
;     oplot,e,fpow*norm[i],color=!red
;     print,norm[i],pow(1e3,[norm[i],-gbm[w[i]].beta_band],tnorm=1e3)
;     k=get_kbrd(10)
  endfor 

  writecol,'~/proposals/COMPAIR/GRB_spectra.txt',norm*1e3,-beta,gbm[w].t90,header='Norm (ph/cm2/s/MeV)  Photon Index       T90 (s)',delim='          '

stop
  begplot,name='~/proposals/COMPAIR/GRB_fluence.ps',/land,font='helvetica'
  plotloghist,efluence,bin=0.2,xtitle='Fluence (5-100 MeV) [MeV cm!U-2!N]',ytitle='N'
  endplot
  spawn,'ps2pdf ~/proposals/COMPAIR/GRB_fluence.ps ~/proposals/COMPAIR/GRB_fluence.pdf'
stop
  return
end 
