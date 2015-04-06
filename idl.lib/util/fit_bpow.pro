pro fit_bpow,x,y,yerr,a,doplot=doplot,out=out
  
  if n_elements(a) eq 0 then a=[1D,1D,mean(x),2D]
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0], step:0.D ,parname:'',$
                       relstep:0.,mpside:0}, 4)
  parinfo.value=a
  parinfo.parname=['norm','ind1','break','ind2']
  parinfo.mpside=0
  parinfo[1].limits=[0,10]
  parinfo[3].limits=[0,10]
  n=n_elements(x)
  parinfo[2].limits=[min(x),max(x[0:n-2])]
;  parinfo[0].limits=[0,1e8]
;  parinfo[0].limited=[1,0]
  parinfo[1:3].limited=[1,1]
;  parinfo[2].step=1e2
  parinfo.relstep=[0,0.01,0,0.01]
  
  weights=1/yerr^2
;  weights=(y/yerr)^2
  
  yfit=mpcurvefit(x,y,weights,a,sigma,function_name='bpow',/noder,parinfo=parinfo,chisq=chisq,status=status,xtol=1e-15,ftol=1e-15)
  print,status
  
  norm=a[0]
  ind1=a[1]
  bt=a[2]
  ind2=a[3]
  DOF     = N_ELEMENTS(X) - N_ELEMENTS(a) ; deg of freedom
  CSIGMA  = SIGMA * SQRT(CHISQ / DOF)
  ind1err=csigma[1]
  ind2err=csigma[3]
  bterr=csigma[2]
  
  if keyword_set(doplot) then begin 
     defsymbols
     simpctable
     yrange=prange(y,yerr)
     xrange=prange(x,x*0.1)
     ploterror,x,y,yerr,psym=1,/xlog,/ylog,xtitle='Time (s)',ytitle='Count Rate (cnts sec!U-1!N)',xstyle=1,/nohat,yrange=yrange,xrange=xrange
     
     w1=where(x lt bt,nw1)
     w2=where(x ge bt,nw2)
     t=[0,x[w1],bt,bt,x[w2]]
     w1=[0,w1+1,nw1+1]
     w2=[nw1+2,w2+3]
     bpow,t,a,yfit
     oplot,t[w1],yfit[w1],color=!blue
     oplot,t[w2],yfit[w2],color=!green
     
     ind1=!tsym.alpha+'!L1!N' +' = '+ntostr(round(ind1*100.)/100.,4)+!tsym.plusminus+ntostr(round(ind1err*100.)/100.,4)
     ind2=!tsym.alpha+'!L2!N' +' = '+ntostr(round(ind2*100.)/100.,4)+!tsym.plusminus+ntostr(round(ind2err*100.)/100.,4)
     bt='Break Time = '+ntostr(round(bt/100.)*100)+!tsym.plusminus+ntostr(round(bterr/100.)*100)
     cs=!tsym.chi+'!U2!N/dof = '+ntostr(chisq/dof,4)
     if keyword_set(out) then color=!black else color=!white
     legend,[ind1,ind2,bt,cs],/top,/right,box=0,textcolor=[!blue,!green,color,color]
     
  endif 
     
     
  return
end 
  
