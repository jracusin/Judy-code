@swift_lat_pop_studies
@fit_functions
@fit_functions_flares
@calc_eiso
pro read_integral
  readcol,'~/stuff_for_people/Sam/Integral.txt',grb,alpha,beta,e0,pow,cstatdof,fluence,format='(a,a,a,a,a,a,a)',delim=' ',skip=3
  in=create_struct('grb','','alpha',0.,'alpha_err',fltarr(2),'beta',0.,'beta_err',fltarr(2),$
                   'e0',0.,'e0_err',fltarr(2),'epeak',0.,'epeak_err',fltarr(2),'model','',$
                   'cstat',0.,'dof',0.,'fluence',0d,'fluence_err',fltarr(2))
  nin=n_elements(grb)
  in=replicate(in,nin)
  in.grb='GRB'+grb

  w=where(strlen(alpha) eq 15)
  in[w].alpha=1.*strmid(alpha[w],0,5)
  in[w].alpha_err[1]=1.*strmid(alpha[w],6,4)
  in[w].alpha_err[0]=1.*strmid(alpha[w],10,5)

  w=where(strlen(pow) eq 15)
  in[w].alpha=1.*strmid(pow[w],0,5)
  in[w].alpha_err[1]=1.*strmid(pow[w],6,4)
  in[w].alpha_err[0]=1.*strmid(pow[w],10,5)
  
  w=where(strlen(beta) eq 15)
  in[w].beta=1.*strmid(beta[w],0,5)
  in[w].beta_err[1]=1.*strmid(beta[w],6,4)
  in[w].beta_err[0]=1.*strmid(beta[w],10,5)

  w=where(strlen(e0) ge 4,nw)
  for i=0,nw-3 do begin 
     e0str=strsplit(e0[w[i]],'+->',/ex)
     in[w[i]].e0=1.*e0str[0]
     if strlen(e0[w[i]]) ge 8 then begin 
        in[w[i]].e0_err[1]=1.*e0str[1]
        in[w[i]].e0_err[0]=1.*e0str[2]
     endif 
  endfor 
  in[w].epeak=in[w].e0*(2+in[w].alpha)
  in[w].epeak_err[0]=sqrt(in[w].e0_err[0]^2*(2+in[w].alpha)^2+in[w].alpha_err[0]^2*in[w].e0^2)
  in[w].epeak_err[1]=sqrt(in[w].e0_err[1]^2*(2+in[w].alpha)^2+in[w].alpha_err[1]^2*in[w].e0^2)

  in=in[0:nin-3]
  nin=nin-2

  w=where(in.alpha ne 0 and in.beta ne 0 and in.e0 ne 0)
  in[w].model='band'
  w=where(in.alpha ne 0 and in.epeak ne 0 and in.beta eq 0)
  in[w].model='cpl'
  w=where(in.alpha ne 0 and in.epeak eq 0 and in.beta eq 0)
  in[w].model='pl'

  for i=0,nin-1 do begin 
     str=strsplit(cstatdof[i],'/',/ex)
     in[i].cstat=str[0,*]
     in[i].dof=str[1,*]
     str=strsplit(fluence[i],'+-<',/ex)
     in[i].fluence=str[0]*1e-7
     if n_elements(str) gt 1 then begin 
        in[i].fluence_err[0]=str[1]*1e-7
        in[i].fluence_err[1]=str[2]*1e-7
     endif 
  endfor 

  mwrfits,in,'~/stuff_for_people/Sam/Integral.fits',/create
  return
end 

pro swift_eisos

  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog_parse.fits',1)
  bat=mrdfits('~/Swift/batcat.fits',1)
  kw=mrdfits('~/Swift/decay_lum_corr/Konustable.fits',1)
  in=mrdfits('~/Swift/decay_lum_corr/Integral.fits',1)
  ngbm=n_elements(gbm)
  nbat=n_elements(bat)
  nkw=n_elements(kw)
  nin=n_elements(in)

  ;;; need to match grbs, but not by name, by trigger time

  m2=intarr(nbat)
  mdiff=fltarr(nbat)
  for i=300,nbat-1 do begin 
     diff=abs(bat[i].trigtime-gbm.trigtime_met)
     mdiff[i]=min(diff,m)
     m2[i]=m
  endfor 
  w=where(mdiff lt 100. and mdiff ne 0.)
  m2=m2[w]

  gbm[m2].grb=bat[w].grb
  mwrfits,gbm,'~/Fermi/GBM_GRB_Catalog_parse.fits',/create
  bat[w].model_bestfit=gbm[m2].model_bestfit

;  readcol,'~/Fermi/Swift_pop_study/Fermi_energetics.csv',grb3,who,zz,fluence,fluerr,obs_emin,obs_emax,a,norm_eng,duration,epeak,alpha,beta,phind,a2,norm_eng2,eiso0,stuff1,stuff2,format='(a,a,f,d,d,d,d,d,f,f,f,f,f,f,f,f,d,a,a)'
;  match,strtrim(bat.grb,2),'GRB'+grb3,m1,m2
;  bat[m1].epeak_band=epeak[m2]
;  wm=where(alpha[m2] ne 0 and beta[m2] ne 0)
;  bat[m1[wm]].alpha_band=alpha[m2[wm]]
;  bat[m1[wm]].beta_band=beta[m2[wm]]
;  bat[m1[wm]].nenergy_band=norm_eng
;  wc=where(alpha[m2] ne 0 and beta[m2] eq 0)
;  bat[m1[wc]].alpha_cpl=alpha[m2[wc]]
;  bat[m1[wc]].nenergy_cpl=norm_eng
;;  bat[m1[wc]].fluence=fluence
;;  bat[m1[wc]].fluence_err=rotate([[fluerr],[fluerr]],1)

;; KW
  kw.grb='GRB'+kw.grb
  match,strtrim(bat.grb,2),strtrim(kw.grb,2),m1,m2
  match,strtrim(bat.grb,2),strtrim(kw.grb+'A',2),m1a,m2a
  kw[m2a].grb=bat[m1a].grb
  kw=kw[[m2,m2a]]
  
  wband=where(strtrim(kw.model,2) eq '(Band)',nwband)
  match,strtrim(bat.grb,2),strtrim(kw[wband].grb,2),m1,m2
  if m2[0] ne -1 then begin 
     m2=wband[m2]
     print,'KW- band: '+ntostr(n_elements(m2))
     bat[m1].alpha_band=kw[m2].alpha
     bat[m1].alpha_band_err[0]=kw[m2].alpha_minus_err
     bat[m1].alpha_band_err[1]=kw[m2].alpha_plus_err
     bat[m1].beta_band=kw[m2].beta
     bat[m1].beta_band_err[0]=kw[m2].beta_minus_err
     bat[m1].beta_band_err[1]=kw[m2].beta_plus_err
     bat[m1].epeak_band=kw[m2].ep
     bat[m1].epeak_band_err[0]=kw[m2].ep_minus_err
     bat[m1].epeak_band_err[1]=kw[m2].ep_plus_err
     bat[m1].fluence=kw[m2].fluence
     bat[m1].fluence_err=(abs(kw[m2].fluence_minus_err)+abs(kw[m2].fluence_plus_err))/2.
     bat[m1].fluence_energy=[20,2e3]
     bat[m1].model_bestfit='kwBAND'
  endif 

  wcpl=where(strtrim(kw.model,2) eq 'exponentialcutoff' or strtrim(kw.model,2) eq 'cutoff(CPL)',nwcpl)
  match,bat.grb,kw[wcpl].grb,m1,m2
  if m2[0] ne -1 then begin 
     m2=wcpl[m2]
     print,'KW- cpl: '+ntostr(n_elements(m2))
     bat[m1].alpha_cpl=kw[m2].alpha
     bat[m1].alpha_cpl_err[0]=kw[m2].alpha_minus_err
     bat[m1].alpha_cpl_err[1]=kw[m2].alpha_plus_err
     bat[m1].epeak_cpl=kw[m2].ep
     bat[m1].epeak_cpl_err[0]=kw[m2].ep_minus_err
     bat[m1].epeak_cpl_err[1]=kw[m2].ep_plus_err
     bat[m1].fluence=kw[m2].fluence
     bat[m1].fluence_err=(abs(kw[m2].fluence_minus_err)+abs(kw[m2].fluence_plus_err))/2.
     bat[m1].fluence_energy=[20,2e3]
     bat[m1].model_bestfit='kwCOMP'
  endif 
  ;; what does a single PL in KW mean?
  ;; wpl=where(strtrim(kw.model,2) eq 'singlepower-law' or strtrim(kw.model,2) eq 'powerlaw')
  ;; match,bat.grb,kw[wpl].grb,m1,m2
  ;; m2=wpl[m2]
  ;; bat[m1].alpha_pl=kw[m2].alpha
  ;; bat[m1].alpha_pl_err[0]=kw[m2].alpha_minus_err
  ;; bat[m1].alpha_pl_err[1]=kw[m2].alpha_plus_err
  ;; bat[m1].epeak_pl=kw[m2].ep
  ;; bat[m1].epeak_pl_err[0]=kw[m2].ep_minus_err
  ;; bat[m1].epeak_pl_err[1]=kw[m2].ep_plus_err
  ;; bat[m1].fluence=kw[m2].fluence
  ;; bat[m1].fluence_err[0]=kw[m2].fluence_minus_err
  ;; bat[m1].fluence_err[1]=kw[m2].fluence_plus_err
  ;; bat[m1].gbmmodel_bestfit='flnc_comp'

;;; Integral
  match,strtrim(bat.grb,2),strtrim(in.grb,2),m1,m2
  match,strtrim(bat.grb,2),strtrim(in.grb+'A',2),m1a,m2a
  if m1a[0] ne -1 then begin 
     in[m2a].grb=bat[m1a].grb
     in=in[[m2,m2a]]
  endif else in=in[m2]

  wband=where(strtrim(in.model,2) eq 'band',nwband)
  if nwband gt 0 then begin 
     match,bat.grb,in[wband].grb,m1,m2

     if m2[0] ne -1 then begin 
        m2=wband[m2]
        print,'Integral- band: '+ntostr(n_elements(m2))
        bat[m1].alpha_band=in[m2].alpha
        bat[m1].alpha_band_err=in[m2].alpha_err
        bat[m1].beta_band=in[m2].beta
        bat[m1].beta_band_err=in[m2].beta_err
        bat[m1].epeak_band=in[m2].epeak
        bat[m1].epeak_band_err=in[m2].epeak_err
        bat[m1].fluence=in[m2].fluence
        bat[m1].fluence_err=mean(abs(in[m2].fluence_err))
        bat[m1].model_bestfit='intBAND'
        bat[m1].fluence_energy=[20,200]
     endif 
  endif 
  wcpl=where(strtrim(in.model,2) eq 'cpl',nwcpl)
  if nwcpl gt 0 then begin 
     match,bat.grb,in[wcpl].grb,m1,m2
     if m2[0] ne -1 then begin 
        m2=wcpl[m2]
        print,'Integral- cpl: '+ntostr(n_elements(m2))
        bat[m1].alpha_cpl=in[m2].alpha
        bat[m1].alpha_cpl_err=in[m2].alpha_err
        bat[m1].epeak_cpl=in[m2].epeak
        bat[m1].epeak_cpl_err=in[m2].epeak_err
        bat[m1].fluence=in[m2].fluence
        bat[m1].fluence_err=mean(abs(in[m2].fluence_err))
        bat[m1].model_bestfit='intCOMP'
        bat[m1].fluence_energy=[20,200]
     endif 
  endif 

;;; GBM
  wband=where(strtrim(gbm.model_bestfit,2) eq 'FLNC_BAND' and gbm.grb ne '')
  match,bat.grb,gbm[wband].grb,m1,m2
  m2=wband[m2]
  bat[m1].alpha_band=gbm[m2].alpha_band
  bat[m1].alpha_band_err=gbm[m2].alpha_band_err
  bat[m1].beta_band=gbm[m2].beta_band
  bat[m1].beta_band_err=gbm[m2].beta_band_err
  bat[m1].e0_band=gbm[m2].e0_band
  bat[m1].e0_band_err=gbm[m2].e0_band_err
  bat[m1].epeak_band=gbm[m2].epeak_band
  bat[m1].epeak_band_err=gbm[m2].epeak_band_err
  bat[m1].nenergy_band=100.
  bat[m1].fluence=gbm[m2].fluence_band
  bat[m1].fluence_err=gbm[m2].fluence_band
  bat[m1].fluence_energy=[10,1000]
  bat[m1].model_bestfit='gbmBAND';gbm[m2].model_bestfit

  wcpl=where(strtrim(gbm.model_bestfit,2) eq 'FLNC_COMP' and gbm.grb ne '')
  match,bat.grb,gbm[wcpl].grb,m1,m2
  m2=wcpl[m2]
  bat[m1].alpha_cpl=gbm[m2].alpha_cpl
  bat[m1].alpha_cpl_err=gbm[m2].alpha_cpl_err
  bat[m1].epeak_cpl=gbm[m2].epeak_cpl
  bat[m1].epeak_cpl_err=gbm[m2].epeak_cpl_err
  bat[m1].nenergy_cpl=100.
  bat[m1].model_bestfit='gbmCOMP';gbm[m2].model_bestfit

  wsbpl=where(strtrim(gbm.model_bestfit,2) eq 'FLNC_SBPL' and gbm.grb ne '')
  match,bat.grb,gbm[wsbpl].grb,m1,m2
  m2=wsbpl[m2]
  bat[m1].alpha_sbpl=gbm[m2].alpha_sbpl
  bat[m1].alpha_sbpl_err=gbm[m2].alpha_sbpl_err
  bat[m1].beta_sbpl=gbm[m2].beta_sbpl
  bat[m1].beta_sbpl_err=gbm[m2].beta_sbpl_err
  bat[m1].epeak_sbpl=gbm[m2].epeak_sbpl
  bat[m1].epeak_sbpl_err=gbm[m2].epeak_sbpl_err
  bat[m1].nenergy_sbpl=100.
  bat[m1].model_bestfit='gbmSBPL';gbm[m2].model_bestfit
  
  w=where(bat.epeak_band eq 0. and bat.alpha_pl ge -2.3 and bat.alpha_pl le -1.3 and bat.epeak_cpl eq 0)
  bat[w].epeak_band=10^(3.258+0.829*bat[w].alpha_pl) ;; Taka relation

  w=where(bat.epeak_band eq 0. and bat.epeak_cpl ne 0 and (bat.batmodel_bestfit eq 'CPL' or strpos(bat.model_bestfit,'COMP') ne -1))
  bat[w].epeak_band=bat[w].epeak_cpl

  w=where(bat.epeak_band eq 0. and strpos(bat.model_bestfit,'SBPL') ne -1)
  bat[w].epeak_band=bat[w].epeak_sbpl

  w2=where(bat.alpha_band eq 0.)
  bat[w2].alpha_band=-1
  bat[w2].beta_band=-2.5

  w=where(bat.e0_band eq 0 and bat.epeak_band ne 0)
  bat[w].e0_band=bat[w].epeak_band*(2.+bat[w].alpha_band)
  w=where(bat.e0_band ne 0 and bat.epeak_band eq 0,nw)
  if nw gt 0 then bat[w].epeak_band=bat[w].e0_band/(2.+bat[w].alpha_band)

  evtoerg=1.60217646d-12
  kevtoerg=evtoerg*1d3
  emin=15.
  emax=150.
  iemin=10.    ;;; 10 keV
  iemax=10000. ;;; 10 MeV
  if emax gt 1e3 then x=50d else x=1.
  if emax gt 1d6 then x=1000d
  e=dindgen((emax-emin)/x)*x+emin

  for i=0,nbat-1 do begin
     f=band(e,[1.,bat[i].alpha_band,bat[i].e0_band,bat[i].beta_band,bat[i].nenergy_pl])
     intf=int_tabulated(e,f*e,/double)*kevtoerg*(bat[i].tstop-bat[i].tstart)
     bat[i].norm_band=bat[i].sbat_15_150/intf
     
     p=[bat[i].norm_band,bat[i].alpha_band,bat[i].epeak_band,bat[i].beta_band]
     bat[i].eiso_band=calc_eiso(bat[i].z,p,bat[i].tstart,bat[i].tstop,model='band',emin=iemin,emax=iemax,enorm=bat[i].nenergy_pl,sbol=sbol)

     pcpl=[bat[i].norm_cpl,bat[i].alpha_cpl,bat[i].epeak_cpl]
     bat[i].eiso_cpl=calc_eiso(bat[i].z,pcpl,bat[i].tstart,bat[i].tstop,model='cpl',emin=iemin,emax=iemax,enorm=bat[i].nenergy_cpl,sbol=sbol)

     pl=[bat[i].norm_pl,bat[i].alpha_pl]
     bat[i].eiso_pl=calc_eiso(bat[i].z,pl,bat[i].tstart,bat[i].tstop,model='simple_pl',emin=iemin,emax=iemax,enorm=bat[i].nenergy_pl,sbol=sbol)

     bat[i].eiso2=calc_eiso2(bat[i].sbat_15_150,emin,emax,bat[i].z,bat[i].alpha_band,bat[i].beta_band,bat[i].epeak_band,emin=iemin,emax=iemax)

     bat[i].eiso1=bat[i].eiso_band
     
  endfor 

  cpl=where(strpos(bat.model_bestfit,'COMP') ne -1)
  bat[cpl].eiso_bestfit=bat[cpl].eiso_cpl

  band=where(strpos(bat.model_bestfit,'BAND') ne -1)
  bat[band].eiso_bestfit=bat[band].eiso_band

  pl=where(bat.eiso_bestfit eq 0 and strtrim(bat.batmodel_bestfit,2) eq 'PL' and bat.epeak_cpl eq 0.)
  bat[pl].eiso_bestfit=bat[pl].eiso_pl
  wpl=where(bat.z gt 0 and bat.e0_band eq 0)
  bat[wpl].eiso1=bat[wpl].eiso_bestfit
  
  ;;; best fit is PL, but have CPL, so use that for Eiso pBand
  pl=where(strtrim(bat.batmodel_bestfit,2) eq 'PL' and bat.epeak_cpl gt 0. and bat.eiso_band gt 0.)
  bat[pl].eiso_bestfit=bat[pl].eiso_pl
  bat[pl].eiso1=bat[pl].eiso_band
  
;  calc_eiso,z,p,start,stop,model=model,emin=emin,emax=emax,h0=h0,sbol=sbol,enorm=enorm,omega_m=omega_m,bandpl=bandpl,oenorm=oenorm
;  calc_eiso2,fluence,fmin,fmax,z,alpha,beta,epeak,phind,n,emin=emin,emax=emax,h0=h0,omega_m=omega_m,cutoff=cutoff,pl=pl,bandpl=bandpl

  !p.multi=[0,2,1]
  w=where(bat.z gt 0 and bat.epeak_band gt 0)
  plot,bat[w].eiso1,bat[w].eiso2,/xlog,/ylog,psym=1,/iso
  oplot,[1d48,1d58],[1d48,1d58],line=1,color=!green     
  stop
  plothist,alog10(bat[w].eiso1),bin=0.2
  plothist,alog10(bat[w].eiso2),bin=0.2,/over,color=!red

  !p.multi=0

  ;;; how to deal with fluence or normalization???
  ;;;  give it energy fluence and calc_eiso2
  ;;;  specify 15-150 keV vs 10-1000 keV for BAT vs GBM
  ;; need pivot energy for each model (diff for BAT & GBM) - done
  ;; probably need to run eiso2 and eiso (for old non-bat/gbm bursts)

  mwrfits,bat,'~/Swift/batcat.fits',/create
  stop
  return
end 
