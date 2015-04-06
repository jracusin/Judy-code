@fit_functions
pro sn87a_lc_fit

  readcol,'~/SNR87A/fluxes.dat',day,f1,f1err,f2,f2err
;  f1=f1[0:17]
;  f1err=f1err[0:17]
  
  begplot,name='~/SNR87A/lc_fits.ps',/land,/color
  ploterror,day,f1,f1err,psym=4,/nohat,/xlog,/ylog,xrange=[4000,10000],/xsty;,xtickname=['4000','10000']

  xyouts,3900,0.8,'4000'
  d=findgen(80)*100.+2000.
  p=[1.,-2]
  newp=mpfitfun('pow',day,f1,f1err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=10,$
                ftol=1e-15,xtol=1e-15,gtol=1e-25,quiet=1)
  chisq=total(((yfit-f1)/f1err)^2)/dof
;  print,chisq,newp
  yfit=pow(d,newp)
  oplot,d,yfit
  
  p=[1.,-5.,6e3,-8.]
  np=n_elements(p)
  parinfo = parinfo_struct(np)
  newp1=mpfitfun('bknpow',day,f1,f1err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=10,$
                ftol=1e-15,xtol=1e-15,gtol=1e-25,quiet=1)
  chisq1=total(((yfit-f1)/f1err)^2)/dof
;  print,chisq,newp1
  d=[d,newp1[2]]
  d=d[sort(d)]
  yfit=bknpow(d,newp1)
  oplot,d,yfit,line=1,color=!blue

  p=[1.,-7.,7e3,-3.,8e3,-1.]
  np=n_elements(p)
  parinfo = parinfo_struct(np)
;  parinfo[2]
  newp2=mpfitfun('bkn2pow',day,f1,f1err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=10,$
                ftol=1e-15,xtol=1e-15,gtol=1e-25,quiet=1)
  chisq2=total(((yfit-f1)/f1err)^2)/dof
;  print,chisq,newp2
  d=[d,newp2[2],newp2[4]]
  d=d[sort(d)]
  yfit=bkn2pow(d,newp2)
  oplot,d,yfit,line=2,color=!red
  legend,['pow','bknpow','bkn2pow'],box=0,/top,/left,line=[0,1,2],color=[!p.color,!blue,!red]
  
  print,chisq,newp
  print,chisq1,newp1
  print,chisq2,newp2

  endplot
  stop
return
end 
