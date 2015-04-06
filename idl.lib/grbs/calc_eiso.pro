function band_pl,e,k1,alpha,e0,beta,enorm1,k2,alpha2,enorm2

  f1=band(e,k1,alpha,e0,beta,enorm1)
  f2=simple_pl(e,k2,alpha2,enorm2)
  f=f1+f2

  return,f
end   

function band,e,p;k,alpha,e0,beta,enorm
  k=p[0]
  alpha=p[1]
  e0=p[2]
  beta=p[3]
  enorm=p[4]
  f=dblarr(n_elements(e))
  w1=where(e le (alpha-beta)*e0,n1)
  w2=where(e ge (alpha-beta)*e0,n2)
  if n1 gt 0 then f[w1]=k*(e[w1]/enorm)^alpha*exp(-e[w1]/e0)
  if n2 gt 0 then f[w2]=k*((alpha-beta)*e0/enorm)^(alpha-beta)*exp(beta-alpha)*(e[w2]/enorm)^beta
  return,f
end 

function eband,e,p;k,alpha,e0,beta,enorm
  k=p[0]
  alpha=p[1]
  e0=p[2]
  beta=p[3]
  enorm=p[4]
  f=dblarr(n_elements(e))
  w1=where(e le (alpha-beta)*e0,n1)
  w2=where(e ge (alpha-beta)*e0,n2)
  if n1 gt 0 then f[w1]=k*(e[w1]/enorm)^alpha*exp(-e[w1]/e0)*e[w1]
  if n2 gt 0 then f[w2]=k*((alpha-beta)*e0/enorm)^(alpha-beta)*exp(beta-alpha)*(e[w2]/enorm)^beta*e[w2]
  return,f
end 
  
function simple_pl,e,k,alpha,enorm,intf=intf
  
  f=k*(e/enorm)^(alpha)
  intf=k/enorm^alpha*(1./(alpha+2.))*(max(e)^(alpha+2.)-min(e)^(alpha+2.))
  
  return,f
end 

function cutoff_pl,e,k,alpha,epeak,enorm
  
  f=k*(e/enorm)^alpha*exp(-e*(2.+alpha)/epeak)
  
  return,f
end

pro test_sbol
  
  h0=71.
  omega_m=0.27
  grbstr1=create_struct('grb','','z',0.,$
                        'alpha_pl',0.,'alpha_pl_err',0.,$
                        'kpl',0.,'kpl_err',0.,'chi2pl',0.,$
                        'alpha_cpl',0.,'alpha_cpl_err',fltarr(2),$
                        'epeak',0.,'epeak_err',fltarr(2),$
                        'kcpl',0.,'kcpl_err',fltarr(2),$
                        'chi2cpl',0.,$
                        'kband',0.,'kband_err',fltarr(2),$
                        'alpha_band',0.,'alpha_band_err',fltarr(2),$
                        'beta_band',0.,'beta_band_err',fltarr(2),$
                        'tstart',0.,'tstop',0.,$
                        'model','',$
                        'sbat',0.,'sbat_err',0.,$
                        'sbat_50_100',0.,'sbat_25_50',0.,$
                        'hr',0.,'hr_err',0.,$
                        'sme',0.,'sme_err',fltarr(2),$
                        'sband',0.,'sband_err',fltarr(2),$
                        'eiso_bestfit',0d,'eiso_bestfit_err',dblarr(2),$
                        'eiso_band',0d,'eiso_band_err',dblarr(2),$
                        'eiso_cpl',0d,'eiso_cpl_err',dblarr(2),$
                        'eiso',0d,'eiso_err',dblarr(2),$
                        'shb',0)
                       
;  readcol,'~/jetbreaks/taka_tbl5.txt',grb,alphapl,alphaplerr,kpl,kplerr,chi2pl,format='(a,d,d,d,d,d)',/silent
;  readcol,'~/jetbreaks/taka_tbl5.txt',grb2a,alphapl2,alphaplerr2,kpl2,kplerr2,chi2pl2,alphacpl,alphacplerr1,alphacplerr2,eobspeak,eobspeakerr1,eobspeakerr2,kcpl,kcplerr1,kcplerr2,chi2cpl,format='(a,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent
  readcol,'~/jetbreaks/bat_grb_timeave_spec_para_PL_bat2.txt',grb,trig,inst,model,alphapl,alphaplerr1,alphaplerr,kpl,kpl_err1,kplerr,nee,chi2pl,dof,format='(a,l,a,a,a,a,a,a,a,a,a,f,i)'
  readcol,'~/jetbreaks/bat_grb_timeave_spec_para_CPL_bat2.txt',grb2a,trig,inst,model,alphacpl,alphacplerr1,alphacplerr2,eobspeak,eobspeakerr1,eobspeakerr2,kcpl,kcplerr1,kcplerr2,nee,chi2cpl,dof,format='(a,l,a,a,f,f,f,f,f,f,f,f,f,a,f,a)'
  n=n_elements(grb)

  grbstr=replicate(grbstr1,n)
  grbstr.grb=grb
  grbstr.alpha_pl=alphapl
  w=where(alphaplerr eq '-')
  alphaplerr[w]=0.
  grbstr.alpha_pl_err=alphaplerr
  w=where(kpl eq '-')
  kpl[w]=0.
  grbstr.kpl=kpl;*1.e-4
  w=where(kplerr eq '-')
  kplerr[w]=0.
  grbstr.kpl_err=kplerr;*1.e-4
  grbstr.chi2pl=chi2pl
  
  match,grb,grb2a,m1,m2
  grbstr[m1].alpha_cpl=alphacpl[m2]
  grbstr[m1].alpha_cpl_err[0]=alphacplerr1[m2]
  grbstr[m1].alpha_cpl_err[1]=alphacplerr2[m2]
  grbstr[m1].epeak=eobspeak[m2]
  grbstr[m1].epeak_err[0]=eobspeakerr1[m2]
  grbstr[m1].epeak_err[1]=eobspeakerr2[m2]
  grbstr[m1].kcpl=kcpl[m2];*1e-4
  grbstr[m1].kcpl_err[0]=kcplerr1[m2];;*1e-4
  grbstr[m1].kcpl_err[1]=kcplerr2[m2];*1e-4
  grbstr[m1].chi2cpl=chi2cpl[m2]
  
; readcol,'~/jetbreaks/taka_tbl2.txt',grb2,model,s1,s1err,s2,s2err,s3,s3err,s4,s4err,s5,s5err,start,stop,format='(a,a,d,d,d,d,d,d,d,d,d,d,d,d)',/silent
  readcol,'~/jetbreaks/bat_grb_fluence_pflux_bat2.txt',grb2,trig,model,s1,s1err,s2,s2err,s3,s3err,s4,s4err,s5,s5err,start,stop,format='(a,l,a,a,a,a,a,a,a,a,a,d,d,d,d)',/silent
  
  match,strtrim(grbstr.grb,2),strtrim(grb2,2),m1,m2
  grbstr[m1].tstart=start[m2]
  grbstr[m1].tstop=stop[m2]
  grbstr[m1].model=model[m2]
  grbstr[m1].sbat=s5[m2];*1.e-8
  grbstr[m1].sbat_err=s5err[m2];*1.e-8
  grbstr[m1].sbat_50_100=s3[m2]
  grbstr[m1].sbat_25_50=s2[m2]
  grbstr[m1].hr=s3[m2]/(s2[m2]*1.)
  grbstr[m1].hr_err=sqrt((s3err[m2]/(s3[m2]*1.))^2.+(s2err[m2]/(s2[m2]*1.))^2.)*grbstr[m1].hr

  readcol,'~/Fermi/Swift_pop_study/sample_stats.csv',grb4,who,z4,sl,t90,format='(a,a,f,a,f)'
  match,strtrim(grbstr.grb,2),grb4,m1,m2
  grbstr[m1].z=z4[m2]
  wshort=where(sl[m2] eq 'short')
  grbstr[m1[wshort]].shb=1
  

;;   forgot_grb='' & forgot_z=0.
;;   openr,lun,'~/jetbreaks/grb_z_epeak_band.csv',/get_lun ;;; need to update/use new z's
;;   line=readline(lun)
;;   while not eof(lun) do begin
;;      line=readline(lun,delim=',')
;;      wshort=where(line eq 'short',ns)
;;      if n_elements(line) gt 1 then line=line[0:n_elements(line)-4]
;;      w=where(line ne '',nline)
;;      print,nline,line
;;      if nline gt 0 then begin 
;;         line=line[w]
;;         w=where(strtrim(grbstr.grb,2) eq 'GRB'+line[0],nw)
;;         if ns gt 0 and nw gt 0 then grbstr[w].shb=1
;;         print,line[0]
;;         if nw gt 0 then begin
;;            grbstr[w].z=line[1]
;;            if nline gt 2 then begin
;;               grbstr[w].epeak=line[2]*1.
;;               if nline gt 3 then begin 
;;                  grbstr[w].epeak_err[0]=line[4]*1.
;;                  grbstr[w].epeak_err[1]=line[3]*1.
;;                  if nline gt 5 then begin
;;                     if nline gt 10 and ns eq 0 then begin 
;;                        grbstr[w].alpha_band=line[5]*1.
;;                        grbstr[w].alpha_band_err[0]=line[7]*1.
;;                        grbstr[w].alpha_band_err[1]=line[6]*1.
;;                        grbstr[w].beta_band=line[8]*1.
;;                        grbstr[w].beta_band_err[0]=line[10]*1.
;;                        grbstr[w].beta_band_err[1]=line[9]*1.
;;                     endif 
;;                     if ns gt 0. then begin 
;;                        grbstr[w].alpha_cpl=line[5]*1.
;;                        grbstr[w].alpha_cpl_err[0]=line[7]*1.
;;                        grbstr[w].alpha_cpl_err[1]=line[6]*1.
;;                     endif
;;                  endif 
;;               endif 
;;            endif 
;;         endif else begin
;;            forgot_grb=[forgot_grb,line[0]]
;;            forgot_z=[forgot_z,line[1]]
;;         endelse 
;;      endif; else print,'GRB '+line[0]+'does not exist'
;;   endwhile 
  
;;   close,lun
;;   free_lun,lun
  
;;   if n_elements(forgot_grb) gt 1 then begin 
;;      forgot_grb=forgot_grb[1:*]
;;      forgot_z=forgot_z[1:*]
;;      w=where(forgot_grb ne '',nw)
;;      if nw gt 0 then begin
;;         grbstr2=replicate(grbstr1,nw)
;;         grbstr2[w].grb=forgot_grb[w]
;;         grbstr2[w].z=forgot_z[w]
;;         concat_structs,grbstr,grbstr2,dumbass
;;         grbstr=dumbass[sort(dumbass.grb)]
;;      endif 
;;   endif
     
  
;  readcol,'~/jetbreaks/grb_z_epeak_band.csv',grb3,zz,format='(a,f)',/silent
;  match,strtrim(grbstr.grb,2),grb3,m1,m2
;  grbstr[m1].z=zz[m2]
;  readcol,'~/jetbreaks/grb_z_epeak_band.csv',grb3,zz,epeak,epeakerr1,epeakerr2,format='(a,f,f,f,f)',/silent
;  match,strtrim(grbstr.grb,2),grb3,m1,m2
;  grbstr[m1].z=zz[m2]
;  grbstr[m1].epeak=epeak[m2]
;  grbstr[m1].epeak_err[0]=epeakerr2[m2]
;  grbstr[m1].epeak_err[1]=epeakerr1[m2]

  readcol,'~/Fermi/Swift_pop_study/Fermi_energetics.csv',grb3,who,zz,fluence,fluerr,obs_emin,obs_emax,a,norm_eng,duration,epeak,alpha,beta,phind,a2,norm_eng2,eiso0,stuff1,stuff2,format='(a,a,f,d,d,d,d,d,f,f,f,f,f,f,f,f,d,a,a)'
  match,strtrim(grbstr.grb,2),'GRB'+grb3,m1,m2
  grbstr[m1].epeak=epeak[m2]
  wm=where(alpha[m2] ne 0 and beta[m2] ne 0)
  grbstr[m1[wm]].alpha_band=alpha[m2[wm]]
  grbstr[m1[wm]].beta_band=beta[m2[wm]]
  wc=where(alpha[m2] ne 0 and beta[m2] eq 0)
  grbstr[m1[wc]].alpha_cpl=alpha[m2[wc]]

  wz=where(grbstr.z gt 0.,nwz)
  grbstr=grbstr[wz]
  n=n_elements(grbstr)
  
  w=where(grbstr.alpha_band eq 0.)
  grbstr[w].alpha_band=-1
  grbstr[w].beta_band=-2.5
  ;;; RELATION FROM ZHANG ET AL. (2008?) - EFFICIENCIES PAPER
;  w=where(grbstr.epeak eq 0. and grbstr.alpha_pl gt -2.3 and grbstr.alpha_pl lt -1.2)
;  grbstr[w].epeak=10^(2.76-3.61*alog10(-grbstr[w].alpha_pl))

  ;;; RELATION FROM SAKAMOTO ET AL. (2009)
  w=where(grbstr.epeak eq 0. and grbstr.alpha_pl gt -2.3 and grbstr.alpha_pl lt -1.3)
  grbstr[w].epeak=10^(3.258+0.829*grbstr[w].alpha_pl)
  
  evtoerg=1.60217646d-12
  kevtoerg=evtoerg*1d3
  enorm=50.
  emin=15.
  emax=150.
;  iemin=1.
;  iemax=1d4
  iemin=10. ;; 10 keV
;  iemax=2e3
;  iemax=1d7 ;;; 10 GeV
  iemax=1d4 ;;; 10 MeV
;  if emax gt 1e3 then x=10d else x=1.
  if emax gt 1e3 then x=50d else x=1.
  if emax gt 1d6 then x=1000d

  e=dindgen((emax-emin)/x)*x+emin
;  e=findgen(emax-emin)+emin
  
  for i=0,n-1 do begin
;     f=simple_pl(e,kpl[m1[i]],alphapl[m1[i]],intf=intf)
;     sbol[m1[i]]=intf*kevtoerg/1e-8*(stop[m2[i]]-start[m2[i]])
;     eiso[m1[i]]=calc_eiso(z[m1[i]],k[m1[i]],alpha[m1[i]],start[m2[i]],stop[m2[i]],epeak[m1[i]],sbol=sb,emin=emin,emax=emax)
;     sbol[m1[i]]=sb/1.e-8
     ;;15-150 keV fluence
     if grbstr[i].kcpl eq 0. then $
        tmp=calc_eiso(0.,[grbstr[i].kpl,grbstr[i].alpha_pl],grbstr[i].tstart,grbstr[i].tstop,emin=emin,emax=emax,sbol=sb,model='simple_pl',h0=h0,omega_m=omega_m) else $
           tmp=calc_eiso(0.,[grbstr[i].kcpl,grbstr[i].alpha_cpl,grbstr[i].epeak],grbstr[i].tstart,grbstr[i].tstop,emin=emin,emax=emax,sbol=sb,model='cutoff_pl',h0=h0,omega_m=omega_m)
     grbstr[i].sme=sb;*1e4
     
     ;;10-10000 keV fluence
     if grbstr[i].kcpl eq 0. then $
        grbstr[i].eiso_bestfit=calc_eiso(grbstr[i].z,[grbstr[i].kpl,grbstr[i].alpha_pl],grbstr[i].tstart,grbstr[i].tstop,emin=iemin,emax=iemax,sbol=sb,model='simple_pl',h0=h0,omega_m=omega_m) else begin 
        grbstr[i].eiso_bestfit=calc_eiso(grbstr[i].z,[grbstr[i].kcpl,grbstr[i].alpha_cpl,grbstr[i].epeak],grbstr[i].tstart,grbstr[i].tstop,emin=iemin,emax=iemax,sbol=sb,model='cutoff_pl',h0=h0,omega_m=omega_m)
     endelse 
     
     if grbstr[i].epeak gt 0. and grbstr[i].shb eq 0 then begin 
        e0=grbstr[i].epeak/(2.+grbstr[i].alpha_band)
        f=band(e,1.,grbstr[i].alpha_band,e0,grbstr[i].beta_band,enorm)
        intf=int_tabulated(e,f*e,/double)*kevtoerg*(grbstr[i].tstop-grbstr[i].tstart)
        grbstr[i].kband=grbstr[i].sbat/intf;*1d-8/intf
        
        grbstr[i].eiso_band=calc_eiso(grbstr[i].z,[grbstr[i].kband,grbstr[i].alpha_band,grbstr[i].epeak,grbstr[i].beta_band],grbstr[i].tstart,grbstr[i].tstop,emin=emin,emax=emax,model='band',sbol=sband,h0=h0,omega_m=omega_m)
        grbstr[i].sband=sband;*1e8

        grbstr[i].eiso_band=calc_eiso(grbstr[i].z,[grbstr[i].kband,grbstr[i].alpha_band,grbstr[i].epeak,grbstr[i].beta_band],grbstr[i].tstart,grbstr[i].tstop,emin=iemin,emax=iemax,model='band',h0=h0,omega_m=omega_m)

     endif 
     if grbstr[i].epeak gt 0 and grbstr[i].shb eq 1 then begin 
        if grbstr[i].alpha_cpl eq 0. then grbstr[i].alpha_cpl=-0.8 ;;only for shb
        f=cutoff_pl(e,1.,grbstr[i].alpha_cpl,grbstr[i].epeak,enorm)
        intf=int_tabulated(e,f*e,/double)*kevtoerg*(grbstr[i].tstop-grbstr[i].tstart)
        grbstr[i].kcpl=grbstr[i].sbat/intf;*1d-8/intf
        grbstr[i].eiso_cpl=calc_eiso(grbstr[i].z,[grbstr[i].kcpl,grbstr[i].alpha_cpl,grbstr[i].epeak],grbstr[i].tstart,grbstr[i].tstop,emin=iemin,emax=iemax,model='cutoff_pl',h0=h0,omega_m=omega_m)
        print,grbstr[i].alpha_cpl,grbstr[i].epeak,grbstr[i].kcpl,grbstr[i].eiso_cpl,grbstr[i].alpha_pl
     endif 
     
  endfor 
  ws=where(grbstr.shb eq 1 and grbstr.eiso_cpl gt 0)
  wl=where(grbstr.shb eq 0 and grbstr.eiso_band gt 0)
  grbstr[ws].eiso=grbstr[ws].eiso_cpl
  grbstr[wl].eiso=grbstr[wl].eiso_band

  mwrfits,grbstr,!mdata+'grb_info_z_epeak_10keV_10MeV_2009.fits',/create  
  !p.multi=[0,2,2]
  plot,grbstr.sbat,grbstr.sme,psym=1,/xlog,/ylog,xtitle='S!LBAT!N',ytitle='S!Lme!N'
  oplot,[1e-10,1e4],[1e-10,1e4]
  
  read_butler_eiso,bgrb,beiso,beisoerr,bz
  match,'GRB'+bgrb,strtrim(grbstr.grb,2),m1,m2
  match,'GRB'+bgrb,strtrim(grbstr[ws].grb,2),sm1,sm2

;  plothist,alog10(grbstr.eiso_bestfit),bin=0.1,xtitle='log E!Liso!N'
  plothist,alog10(beiso[m1]*1d52),bin=0.2,xtitle='log E!Liso!N',yrange=[0,15]
  w=where(grbstr.eiso_band ne 0.)
  plothist,alog10(grbstr[w].eiso_band),bin=0.2,/over,color=!red
  plothist,alog10(grbstr[ws].eiso_cpl),bin=0.2,/over,color=!green
  
  plot,alog10(beiso[m1]*1d52),alog10(grbstr[m2].eiso_bestfit),psym=1,/yno,/iso,xtitle='Butler E!Liso!N',ytitle='My E!Liso!N'
  oplot,alog10(beiso[m1]*1d52),alog10(grbstr[m2].eiso_band),psym=1,color=!red
  oplot,alog10(beiso[sm1]*1d52),alog10(grbstr[ws[sm2]].eiso_cpl),psym=1,color=!blue
  oplot,[0,100],[0,100]
  
  plothist,grbstr.alpha_pl,bin=0.1,xtitle=!tsym.alpha+'!LPL!N'
  
  !p.multi=0
  

  stop
  return
end 

function calc_eiso,z,p,start,stop,model=model,emin=emin,emax=emax,h0=h0,sbol=sbol,enorm=enorm,omega_m=omega_m,bandpl=bandpl,oenorm=oenorm
  
  if n_elements(emin) eq 0 then emin=1. else if emin eq 0 then emin=1.
  if n_elements(emax) eq 0 then emax=1d4 else if emax eq 0 then emax=1d4
  if n_elements(h0) eq 0 then h0=71
  if n_elements(enorm) eq 0 then enorm=50.
  if n_elements(omega_m) eq 0 then omega_m=0.3
  if z le 0 then return,0.

;  print,model
  
  evtoerg=1.60217646d-12
  kevtoerg=evtoerg*1d3
  
  if emax gt 1e3 then x=10d else x=1.
  e=findgen((emax-emin)/x)*x+emin
  e=e/(1.+z)
  de=1.
  if model eq 'simple_pl' then begin
     k=p[0]
     alpha=p[1]
     f=simple_pl(e,k,alpha,enorm,intf=sbol)
;     plot,e,f,/xlog,/ylog,psym=1
;     sbol2=total(f*e*de)*kevtoerg*(stop-start)
     sbol=int_tabulated(e,f*e,/double)*kevtoerg*(stop-start)
;     sbol=sbol*kevtoerg*(stop-start)
;     print,[sbol,sbol2]/1e-8
  endif 
  
  if model eq 'cutoff_pl' then begin
     k=p[0]
     alpha=p[1]
     epeak=p[2];/(1.+z)
     f=cutoff_pl(e,k,alpha,epeak,enorm)
;     plot,e,f,/xlog,/ylog,psym=1
     sbol=int_tabulated(e,f*e,/double)*kevtoerg*(stop-start)
;     sbol=total(f*e*de)*kevtoerg*(stop-start)
;     print,sbol/1e-8
  endif 
  
  if model eq 'band' then begin 
     k=p[0]
     alpha=p[1]
     epeak=p[2];/(1.+z)
     e0=epeak/(2.+alpha)
     beta=p[3]
     p=[k,alpha,e0,beta,enorm]
     f=band(e,p);k,alpha,e0,beta,enorm)
     sbol=int_tabulated(e,f*e,/double)*kevtoerg*(stop-start)
  endif  

  if model eq 'band_pl' then begin 
     k=p[0]
     alpha=p[1]
     epeak=p[2];/(1.+z)
     e0=epeak/(2.+alpha)
     beta=p[3]
     f1=band(e,k,alpha,e0,beta,enorm)
     k2=p[4]
     alpha2=p[5]
     f2=simple_pl(e,k2,alpha2,oenorm)
     f=f1+f2
     sbol=int_tabulated(e,f*e,/double)*kevtoerg*(stop-start)
  endif 

  if z gt 0 then begin 
     pctocm=3.08568025d18
     d=lumdist(z,h=h0,omega_m=omega_m,lambda0=(1.-omega_m),/silent)
     dist=d*1e6*pctocm
     eiso=4.*!pi*dist^2.*sbol/(1.+z);^2.
     print,'Eiso = '+ntostr(eiso)+' '+ntostr(alog10(eiso))
  endif else eiso=0.

  return,eiso
end 

;;short grb alpha_cpl=-0.8, use cpl
