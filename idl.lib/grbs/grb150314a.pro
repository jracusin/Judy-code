@fit_functions
@fit_functions_flares
function int_con_gauss2_bknpow,t,p

  yfit=p[0]+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intbknpow(t,p[1:4])
return,yfit
end

function con_gauss2_bknpow,t,p

  yfit=p[0]+gauss(t,p[5:7])+gauss(t,p[8:10])+bknpow(t,p[1:4])
return,yfit
end
  
pro grb150314a

  ;;; LAT GRB w/ chandra
  ;;; want to fit weird LC model

  cd,'~/Chandra/GRB150314A'
  lc=lcout2fits(/chandra)

  read_lcfit,'lc_fit_comb.dat',pnames,p0
  mo=fit_models(pnames,p0,basemo=basemo)
  intmo='int_con_gauss2_bknpow'
  mo='con_gauss2_bknpow'
  p=[2e-4,p0]
  tt=dblarr(2,n_elements(lc))
  tt[0,*]=lc.tstart
  tt[1,*]=lc.tstop
  print,p
  quiet=1
  newp=mpfitfun(intmo,tt,lc.src_rate,lc.src_rate_err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=1000,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=nprint,$
                ftol=1e-25,xtol=1e-25,gtol=1e-25,quiet=quiet)
  chisq=total(((yfit-lc.src_rate)/lc.src_rate_err[0])^2)

  print,newp

  xrt=mrdfits('~/Chandra/GRB150314A/UL_specfits.fits',1)

  r=xrt[0].unabs_cfratio
  begplot,name='~/Chandra/GRB150314A/GRB150314A_combined_lc_wcons.ps',/land,/color
  plot_like_qdp,lc=lc,/nohard,yrange=[1e-15,1e-7],/ysty,flux=r,/useflux

  t=logarr(lc[0].tstart,1e7,bin=0.1)
  oplot,t,call_function(mo,t,newp)*r
  oplot,t,call_function(basemo,t,newp[1:*])*r,line=2,color=!green
  oplot,t,gauss(t,newp[5:7])*r,color=!green,line=2
  oplot,t,gauss(t,newp[8:10])*r,color=!green,line=2
  oplot,minmax(t),[newp[0],newp[0]]*r,line=2,color=!magenta
  endplot
  spawn,'ps2pdf ~/Chandra/GRB150314A/GRB150314A_combined_lc_wcons.ps ~/Chandra/GRB150314A/GRB150314A_combined_lc_wcons.pdf'

  stop

return
end 
