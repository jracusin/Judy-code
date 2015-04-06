@fit_functions
@fit_functions_flares
@fit_lc
function gauss1_bkn2pow_pow,t,p
  yfit=gauss(t,p[8:10])+bkn2pow(t,p[0:5])+pow(t,p[6:7])
return,yfit
end

function bknpow_pow,t,p
  yfit=bknpow(t,p[0:3])+pow(t,p[4:5])
return,yfit
end

pro grb130831a

  cd,'~/Desktop/GRB130831A/Boris'
  file='lightcurve_detailed.qdp'
  lc1=lcout2fits(pcfile=file,/phil)
  lc2=lcout2fits('../lc_newout_chandra.txt')
  concat_structs,lc1,lc2,lc

  begplot,name='grb130831a_fit.ps',/land,/color,font='helvetica'
  !x.margin=[1,2]
  read_lcfit,'../lc_fit_out_idl_int8.dat',pnames0,p0
  p=[p0[0:5],2e5,1.2,p0[6:8]]
  pnames=[pnames0[0:5],'break3','pow4',pnames0[6:*]]
  
;  model=fit_models(pnames,p)
  f=findgen(9)+1
  tt=[f*100.,f*1e3,f*1e4,f*1e5,f*1e6,f*1e7]
;  tt=dblarr(2,n_elements(lc))
;  tt[0,*]=lc.tstart
;  tt[1,*]=lc.tstop
  spec=mrdfits('../UL_specfits.fits',1)
  cfratio=spec[1].unabs_cfratio
  cts=lc.src_rate;/cfratio
  err=lc.src_rate_err;/cfratio

  ploterror,lc.time,lc.src_rate*cfratio,lc.src_rate_err*cfratio,psym=3,/nohat,/xlog,/ylog,yrange=[1e-15,1e-8],xrange=[100,1e7],/xsty,/ysty,color=!black,xtitle='Time since BAT Trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)'
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]*cfratio,color=!black
  ul=where(lc.src_rate_err eq 0)
  det=where(lc.src_rate_err ne 0)
  plotsym,1,3
  plots,lc[ul].time,lc[ul].src_rate*cfratio,psym=8,color=!black
  yfit=gauss1_bkn3pow(tt,p)
  yfit_base=bkn3pow(tt,p)
;  oplot,lc.time,yfit_base,line=2,color=!black
  
;  oplot,lc.time,yfit,color=!red

  model='gauss1_bkn3pow'
  newp=mpfitfun(model,lc[det].time,cts[det],err[det],p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=1000,errmsg=errmsg,$
                perror=perror,yfit=yfit2,status=status,nprint=nprint,$
                ftol=1e-25,xtol=1e-25,gtol=1e-25,quiet=quiet)
  chisq=total(((yfit2-cts[det])/err[det])^2)
  print,chisq,dof,chisq/dof
  yfit_new=gauss1_bkn3pow(tt,newp)
  oplot,tt,yfit_new*cfratio,color=!green,line=1
  oplot,tt,bkn3pow(tt,newp[0:7])*cfratio,color=!green,line=2
  oplot,tt,gauss(tt,newp[8:10])*cfratio,color=!green,line=2
;  lc_monte_pow,lc,newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int=int,file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit

  model='gauss1_bkn2pow_pow'
  p=[p0[0:5],1e-3,1.2,p0[6:8]]
  pnames=[pnames[0:5],'norm2','pow4',pnames[6:*]]
  newp=mpfitfun(model,lc[det].time,cts[det],err[det],p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=1000,errmsg=errmsg,$
                perror=perror,yfit=yfit3,status=status,nprint=nprint,$
                ftol=1e-25,xtol=1e-25,gtol=1e-25,quiet=quiet)
  chisq=total(((yfit3-cts[det])/err[det])^2)
  print,chisq,dof,chisq/dof

  yfit_pow=gauss1_bkn2pow_pow(tt,newp)
  oplot,tt,yfit_pow*cfratio,color=!red,line=2
  oplot,tt,pow(tt,newp[6:7])*cfratio,line=1,color=!red
  oplot,tt,bkn2pow(tt,newp[0:5])*cfratio,line=1,color=!red
  oplot,tt,gauss(tt,newp[8:10])*cfratio,line=1,color=!red
;  lc_monte_pow,lc,newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int='9',file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit,mo=model

  model='bknpow_pow'
  p=[1e-9,p0[3:5],1e-3,1.2]
  pnames=[pnames[0:3],'norm2','pow3']
  det=where(lc.src_rate_err ne 0 and lc.time gt 3e3)  
  parinfo = parinfo_struct(n_elements(p))
  parinfo.parname=pnames
  parinfo.value=p
  parinfo[0].limits=[0,0] ;;norm > 0
  parinfo[0].limited=[1,0]
;  parinfo[5].limits=[0,3]
;  parinfo[5].limited=[0,1]

  newp=mpfitfun(model,lc[det].time,cts[det],err[det],p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=1000,errmsg=errmsg,$
                perror=perror,yfit=yfit4,status=status,nprint=nprint,$
                ftol=1e-25,xtol=1e-25,gtol=1e-25,quiet=quiet)
  chisq=total(((yfit4-cts[det])/err[det])^2)
  print,chisq,dof,chisq/dof

  yfit_pow=bknpow_pow(tt,newp)
  oplot,tt,yfit_pow*cfratio,color=!purple,line=2
  oplot,tt,pow(tt,newp[4:5])*cfratio,line=1,color=!purple
  oplot,tt,bknpow(tt,newp[0:3])*cfratio,line=1,color=!purple
;  lc_monte_pow,lc,newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int='10',file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit,mo=model

  legend,['gauss+bkn3pow','gauss+bkn2pow+pow','bknpow+pow'],box=0,/top,/right,textcolor=[!green,!red,!purple]

  endplot


stop
return
end 
