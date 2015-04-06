pro fit_pow,x,y,yerr,a,doplot=doplot,out=out,yfit=yfit
  
  if n_params() eq 0 then begin 
     print,'syntax - fit_pow,x,y,yerr,a,doplot=doplot,out=out'
     return
  endif 
  if n_elements(a) eq 0 then a=[1d,1d]
  
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0], step:0.D ,parname:'',relstep:0.,mpside:0}, 2)
  
  parinfo.value=a
  parinfo.parname=['norm','ind']
  
  weights=(1/yerr)^2
;  weights=(y/yerr)^2
  yfit=mpcurvefit(x,y,weights,a,sigma,function_name='pwr',/noder,parinfo=parinfo,chisq=chisq,status=status,xtol=1e-15,ftol=1e-15)
;  pwr,x,a,yfit
;  chisq=total(((y-yfit)/yerr)^2)
  print,status
  
  norm=a[0]
  ind=a[1]
  DOF     = N_ELEMENTS(X) - N_ELEMENTS(a) ; deg of freedom
  CSIGMA  = SIGMA * SQRT(CHISQ / DOF)
  print,csigma
  inderr=csigma[1]
  
  if keyword_set(doplot) then begin
     defsymbols
     simpctable
     yrange=prange(y,yerr)
     xrange=prange(x,x*0.1)
     ploterror,x,y,yerr,psym=1,/xlog,/ylog,xtitle='Time (s)',ytitle='Count Rate (cnts sec!U-1!N)',xstyle=1,/nohat,yrange=yrange,xrange=xrange
     
     t=[0,x,max(x)*10]
     pwr,t,a,yfit
     
     oplot,t,yfit
     ind=!tsym.alpha+'!L1!N' +' = '+ntostr(round(ind*100.)/100.,4)+!tsym.plusminus+ntostr(round(inderr*100.)/100.,4)
     cs=!tsym.chi+'!U2!N/dof = '+ntostr(chisq/dof,4)
     legend,[ind,cs],/top,/right,box=0
  endif
  
  return
end 
