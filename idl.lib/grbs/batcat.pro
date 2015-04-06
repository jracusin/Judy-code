pro batcat

  readcol,'~/jetbreaks/bat_grb_timeave_spec_para_PL_bat2.txt',grb,trig1,inst,model,alphapl,alphaplerr1,alphaplerr,kpl,kpl_err1,kplerr,nee,chi2pl,dof,format='(a,l,a,a,a,a,a,a,a,a,a,f,i)',/silent
  readcol,'~/jetbreaks/bat_grb_timeave_spec_para_CPL_bat2.txt',grb2a,trig,inst,model,alphacpl,alphacplerr1,alphacplerr2,eobspeak,eobspeakerr1,eobspeakerr2,kcpl,kcplerr1,kcplerr2,nee,chi2cpl,dof,format='(a,l,a,a,f,f,f,f,f,f,f,f,f,a,f,a)',/silent
  readcol,'~/jetbreaks/bat_grb_fluence_pflux_bat2.txt',grb2,trig,model,s1,s1err,s2,s2err,s3,s3err,s4,s4err,s5,s5err,start,stop,format='(a,l,a,a,a,a,a,a,a,a,a,d,d,d,d)',/silent
  readcol,'~/jetbreaks/bat_grb_summary_bat2.txt',grb3,trig,trigtime,f_ra,f_dec,f_lc,f_im,g_ra,g_dec,theta,phi,gim,g_cir,t90,t90err,t50,t50err,tstart,tstop,pcode,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,f,f,f,f,a,a,a)',/silent
  readcol,'~/Swift/bat_grb_peak_eneflux_bat2.txt',grb4,trig4,model4,p_st,p_end,pe15_25,pe15_25e,pe25_50,pe25_50e,pe50_100,pe50_100e,pe100_150,pe100_150e,pe15_150,pe15_150e,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,a,a)',/silent


 grbstr1=create_struct('grb','','targid',0L,'trigtime',0d,'z',0.,$
                       'alpha_pl',0.,'alpha_pl_err',0.,$
                       'norm_pl',0.,'norm_pl_err',0.,'chi2pl',0.,$
                       'nenergy_pl',0.,$
                       'alpha_cpl',0.,'alpha_cpl_err',fltarr(2),$
                       'epeak_cpl',0.,'epeak_cpl_err',fltarr(2),$
                       'norm_cpl',0.,'norm_cpl_err',fltarr(2),$
                       'nenergy_cpl',0.,$
                       'chi2cpl',0.,$
                       'alpha_band',0.,'alpha_band_err',fltarr(2),$
                       'beta_band',0.,'beta_band_err',fltarr(2),$
                       'e0_band',0.,'e0_band_err',fltarr(2),$
                       'epeak_band',0.,'epeak_band_err',fltarr(2),$
                       'norm_band',0.,'norm_band_err',fltarr(2),$
                       'nenergy_band',0.,$
                       'alpha_sbpl',0.,'alpha_sbpl_err',fltarr(2),$
                       'beta_sbpl',0.,'beta_sbpl_err',fltarr(2),$
                       'epeak_sbpl',0.,'epeak_sbpl_err',fltarr(2),$
                       'norm_sbpl',0.,'norm_sbpl_err',fltarr(2),$
                       'nenergy_sbpl',0.,$
                       'tstart',0.,'tstop',0.,$
                       'batmodel_bestfit','',$
                       'model_bestfit','',$
                       'sbat_15_150',0.,'sbat_15_150_err',0.,$
                       'sbat_50_100',0.,'sbat_25_50',0.,$
                       'pflux_15_150',0.,'pflux_15_150_err',0.,$
;                        'hr',0.,'hr_err',0.,$
;                       'sme',0.,'sme_err',fltarr(2),$
                       'sband_10_1e4',0.,'sband_10_1e4_err',fltarr(2),$ ;; 10 keV - 10 MeV
                       'fluence',0.,'fluence_err',0.,'fluence_energy',fltarr(2),$
                       'eiso_bestfit',0d,'eiso_bestfit_err',dblarr(2),$
                       'eiso_band',0d,'eiso_band_err',dblarr(2),$
                       'eiso_cpl',0d,'eiso_cpl_err',dblarr(2),$
                       'eiso_pl',0d,'eiso_pl_err',dblarr(2),$
                       'eiso1',0d,'eiso1_err',dblarr(2),$
                       'eiso2',0d,'eiso2_err',dblarr(2),$
                       't90',0.,'t90_err',0d,$
                       'shb',0)

  n=n_elements(grb)
  grbstr2=grbstr1
  grbstr=replicate(grbstr1,n)
  grbstr.grb=grb
  grbstr.targid=trig1
  grbstr.alpha_pl=alphapl
  grbstr.nenergy_pl=50.
  w=where(alphaplerr eq '-')
  alphaplerr[w]=0.
  grbstr.alpha_pl_err=alphaplerr
  w=where(kpl eq '-')
  kpl[w]=0.
  grbstr.norm_pl=kpl;*1.e-4
  w=where(kplerr eq '-')
  kplerr[w]=0.
  grbstr.norm_pl_err=kplerr;*1.e-4
  grbstr.chi2pl=chi2pl
  
  match,grb,grb2a,m1,m2
  grbstr[m1].alpha_cpl=alphacpl[m2]
  grbstr[m1].alpha_cpl_err[0]=alphacplerr1[m2]
  grbstr[m1].alpha_cpl_err[1]=alphacplerr2[m2]
  grbstr[m1].epeak_cpl=eobspeak[m2]
  grbstr[m1].epeak_cpl_err[0]=eobspeakerr1[m2]
  grbstr[m1].epeak_cpl_err[1]=eobspeakerr2[m2]
  grbstr[m1].norm_cpl=kcpl[m2];*1e-4
  grbstr[m1].norm_cpl_err[0]=kcplerr1[m2];;*1e-4
  grbstr[m1].norm_cpl_err[1]=kcplerr2[m2];*1e-4
  grbstr[m1].chi2cpl=chi2cpl[m2]
  grbstr[m1].nenergy_cpl=50.

  match,strtrim(grbstr.grb,2),strtrim(grb2,2),m1,m2
  grbstr[m1].tstart=start[m2]
  grbstr[m1].tstop=stop[m2]
  grbstr[m1].batmodel_bestfit=model[m2]
  grbstr[m1].sbat_15_150=s5[m2];*1.e-8
  grbstr[m1].sbat_15_150_err=s5err[m2];*1.e-8
  grbstr[m1].sbat_50_100=s3[m2]
  grbstr[m1].sbat_25_50=s2[m2]
;  grbstr[m1].hr=s3[m2]/(s2[m2]*1.)
;  grbstr[m1].hr_err=sqrt((s3err[m2]/(s3[m2]*1.))^2.+(s2err[m2]/(s2[m2]*1.))^2.)*grbstr[m1].hr

  match,strtrim(grbstr.grb,2),strtrim(grb3,2),m1,m2
  grbstr[m1].t90=t90[m2]
  grbstr[m1].t90_err=t90err[m2]
  s=where(grbstr[m1].t90 lt 2.)
  grbstr[m1[s]].shb=1
  for i=0,n_elements(m1)-1 do grbstr[m1[i]].trigtime=date2met(trigtime[m2[i]])

  match,strtrim(grbstr.grb,2),strtrim(grb3,2),m1,m2
  grbstr[m1].pflux_15_150=pe15_150[m2]
  grbstr[m1].pflux_15_150_err=pe15_150e[m2]

  grbstr1=grbstr
  ;;; 2010-present

;  readcol,'~/jetbreaks/bat_grb_table.txt',grb,trig,trigtime,f_ra,f_dec,f_lc,f_im,g_ra,g_dec,theta,phi,gim,g_cir,t90,t90err,t50,t50err,tstart,tstop,pcode

  readcol,'~/Swift/bat_grb_spec_para_PL.txt',grb,trig,inst,model,alphapl,alphaplerr1,alphaplerr,ep,eplo,ephi,beta,be_low,be_high,kpl,kpl_err1,kplerr,nee,chi2pl,dof,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,a,f,i)',/silent
  readcol,'~/Swift/bat_grb_spec_para_CPL.txt',grb2a,trig,inst,model,alphacpl,alphacplerr1,alphacplerr2,eobspeak,eobspeakerr1,eobspeakerr2,beta,belo,behi,kcpl,kcplerr1,kcplerr2,nee,chi2cpl,dof,format='(a,l,a,a,f,f,f,f,f,f,f,f,f,f,f,f,a,f,a)',/silent
  readcol,'~/Swift/bat_grb_flux.txt',grb2,trig,model,s1,s1err,s2,s2err,s3,s3err,s4,s4err,s5,s5err,start,stop,format='(a,l,a,a,a,a,a,a,a,a,a,d,d,d,d)',/silent
  readcol,'~/Swift/bat_grb_table.txt',grb3,trig,trigtime,f_ra,f_dec,f_lc,f_im,g_ra,g_dec,theta,phi,gim,g_cir,t90,t90err,t50,t50err,tstart,tstop,pcode,format='(a,l,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',/silent
  readcol,'~/Swift/bat_grb_peakeneflux.txt',grb4,trig,mo,p_st,p_end, pe15_25,pe15_25e,pe25_50,pe25_50e,pe50_100,pe50_100e,pe100_150,pe100_150e,pe15_150, pe15_150e,format='(a,a,a,f,f,f,f,f,f,f,f,f,f,f,f)',/silent

  n=n_elements(grb)
  grbstr=replicate(grbstr2,n)
  grbstr.grb=grb
  grbstr.alpha_pl=alphapl
  w=where(alphaplerr eq '-',nw)
  if nw gt 0 then alphaplerr[w]=0.
  grbstr.alpha_pl_err=alphaplerr
  w=where(kpl eq '-',nw)
  if nw gt 0 then kpl[w]=0.
  grbstr.norm_pl=kpl;*1.e-4
  w=where(kplerr eq '-',nw)
  if nw gt 0 then kplerr[w]=0.
  grbstr.norm_pl_err=kplerr;*1.e-4
  grbstr.chi2pl=chi2pl
  
  match,grb,grb2a,m1,m2
  grbstr[m1].nenergy_pl=50.
  grbstr[m1].alpha_cpl=alphacpl[m2]
  grbstr[m1].alpha_cpl_err[0]=alphacplerr1[m2]
  grbstr[m1].alpha_cpl_err[1]=alphacplerr2[m2]
  grbstr[m1].epeak_cpl=eobspeak[m2]
  grbstr[m1].epeak_cpl_err[0]=eobspeakerr1[m2]
  grbstr[m1].epeak_cpl_err[1]=eobspeakerr2[m2]
  grbstr[m1].norm_cpl=kcpl[m2];*1e-4
  grbstr[m1].norm_cpl_err[0]=kcplerr1[m2];;*1e-4
  grbstr[m1].norm_cpl_err[1]=kcplerr2[m2];*1e-4
  grbstr[m1].chi2cpl=chi2cpl[m2]

  match,strtrim(grbstr.grb,2),strtrim(grb2,2),m1,m2
  grbstr[m1].tstart=start[m2]
  grbstr[m1].tstop=stop[m2]
  grbstr[m1].batmodel_bestfit=model[m2]
  grbstr[m1].sbat_15_150=s5[m2];*1.e-8
  grbstr[m1].sbat_15_150_err=s5err[m2];*1.e-8
  grbstr[m1].sbat_50_100=s3[m2]
  grbstr[m1].sbat_25_50=s2[m2]
;  grbstr[m1].hr=s3[m2]/(s2[m2]*1.)
;  grbstr[m1].hr_err=sqrt((s3err[m2]/(s3[m2]*1.))^2.+(s2err[m2]/(s2[m2]*1.))^2.)*grbstr[m1].hr

  match,strtrim(grbstr.grb,2),strtrim(grb3,2),m1,m2
  grbstr[m1].t90=t90[m2]
  grbstr[m1].t90_err=t90err[m2]
  s=where(grbstr[m1].t90 lt 2.)
  grbstr[m1[s]].shb=1
  for i=0,n_elements(m1)-1 do grbstr[m1[i]].trigtime=date2met(trigtime[m2[i]])

  match,strtrim(grbstr.grb,2),strtrim(grb3,2),m1,m2
  grbstr[m1].pflux_15_150=pe15_150[m2]
  grbstr[m1].pflux_15_150_err=pe15_150e[m2]

  grbstr2=grbstr

  concat_structs,grbstr1,grbstr2,grbstr

;  readcol,'~/jetbreaks/grb_tid_z_table.txt',grb,tid,z,delim='|',format='(a,l,f)',/silent
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  match,grbstr.grb,strtrim(g.grb,2),m1,m2
  grbstr[m1].z=g[m2].z

  mwrfits,grbstr,'~/Swift/batcat.fits',/create

stop
return
end 
