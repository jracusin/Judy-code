pro fit_bpow3,x,y,yerr,a,tstart,tstop,w=w,doplot=doplot,out=out
  
  if n_params() eq 0 then begin
     print,'syntax - fit_bpow3,x,y,yerr,a,doplot=doplot,out=out'
     return
  endif 
  if n_elements(a) eq 0 then a=[1D,1D,mean(x)/5.,1D,mean(x),1D]
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0], step:0.D ,parname:'',$
                       relstep:0.,mpside:0}, 6)
  parinfo.value=a
  parinfo.parname=['norm','ind1','break1','ind2','break2','ind3']
  parinfo.mpside=0
  parinfo[[1,5]].limits=[0,10]
  parinfo[3].limits=[-5,5]
  n=n_elements(x)
  parinfo[[2,4]].limits=[min(x),max(x[0:n-2])]
;  parinfo[0].limits=[0,1e8]
;  parinfo[0].limited=[1,0]
  parinfo[1:5].limited=[1,1]
;  parinfo[2].step=1e2
  parinfo.relstep=[0,0.01,0,0.01,0,0.01]
  
  weights=1/yerr^2
;  weights=(y/yerr)^2
  
  yfit=mpcurvefit(x[w],y[w],weights[w],a,sigma,function_name='bpow3',/noder,parinfo=parinfo,chisq=chisq,status=status,xtol=1e-15,ftol=1e-15)
  print,status
  
  norm=a[0]
  ind1=a[1]
  bt1=a[2]
  ind2=a[3]
  bt2=a[4]
  ind3=a[5]
  
  DOF     = N_ELEMENTS(X) - N_ELEMENTS(a) ; deg of freedom
  CSIGMA  = SIGMA * SQRT(CHISQ / DOF)
  ind1err=csigma[1]
  ind2err=csigma[3]
  ind3err=csigma[5]
  bt1err=csigma[2]
  bt2err=csigma[4]
  
  print,csigma
  if keyword_set(doplot) then begin 
     defsymbols
     simpctable
     yrange=prange(y,yerr)
     xrange=prange(x,x*0.1)
     ploterror,x,y,yerr,psym=3,/xlog,/ylog,xtitle='Time (s)',ytitle='Count Rate (cnts sec!U-1!N)',xstyle=1,/nohat,yrange=yrange,xrange=xrange
     
     if n_elements(tstart) gt 0 and n_elements(tstop) gt 0 then begin
        for i=0,n_elements(tstart)-1 do $
           oplot,[tstart[i],tstop[i]],[cts[i],cts[i]]
     endif 
     
     w1=where(x le bt1,nw1)
     w2=where(x ge bt1 and x le bt2,nw2)
     w3=where(x ge bt2,nw3)
     
     t=[0,x[w1],bt1,x[w2],bt2,x[w3]]
     w1=[0,w1+1,nw1+1]
     w2=[nw1+1,w2+2,nw1+nw2+1]
     w3=[nw1+nw2+1,w3+3]
;     t=[0,x[w1],bt1,bt1,x[w2],bt2,bt2,x[w3]]
;     w1=[0,w1+1,nw1+1]
;     w2=[nw1+2,w2+3,nw1+nw2+2]
;     w3=[nw1+nw2+3,w3+5]
     bpow3,t,a,yfit
     oplot,t[w1],yfit[w1],color=!blue
     oplot,t[w2],yfit[w2],color=!green
     oplot,t[w3],yfit[w3],color=!yellow
     
     ind1=!tsym.alpha+'!L1!N' +' = '+ntostr(round(ind1*100.)/100.,4)+!tsym.plusminus+ntostr(round(ind1err*100.)/100.,4)
     ind2=!tsym.alpha+'!L2!N' +' = '+ntostr(round(ind2*100.)/100.,4)+!tsym.plusminus+ntostr(round(ind2err*100.)/100.,4)
     ind3=!tsym.alpha+'!L3!N' +' = '+ntostr(round(ind3*100.)/100.,4)+!tsym.plusminus+ntostr(round(ind3err*100.)/100.,4)
     bt1='Break Time = '+ntostr(round(bt1))+!tsym.plusminus+ntostr(round(bt1err))
     bt2='Break Time = '+ntostr(round(bt2))+!tsym.plusminus+ntostr(round(bt2err))
     cs=!tsym.chi+'!U2!N/dof = '+ntostr(chisq/dof,4)
     if keyword_set(out) then color=!black else color=!white
     legend,[ind1,ind2,ind3,bt1,bt2,cs],/top,/right,box=0,textcolor=[!blue,!green,!yellow,color,color,color]
     
  endif 
     
     
  return
end 
  
