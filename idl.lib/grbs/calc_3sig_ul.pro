pro calc_3sig_ul,src,bg,srcrad,bgrad,time,exp,rate,raterr,sigma
  
  if n_params() eq 0 then begin
     print,'syntax - calc_3sig_ul,src,bg,srcrad,bgrad,time,exp,rate,raterr,sigma'
     return
  endif 
  
  areabg=!pi*bgrad^2
  areasrc=!pi*srcrad^2
  aratio=areasrc/areabg
  cts=src-aratio*bg
  sigsrc=sqrt(src)
  sigbg=aratio*sqrt(bg)
  ctserr=sqrt(src+aratio^2*bg)
  sigma=(src-bg*aratio)/sqrt(src+bg*aratio)

  bgrate=bg*aratio;/time
  
  if sigma lt 3 then begin 
     stop
     confidlev,bgrate,src,0.9973,smin,smax
     cts=smax
  endif 
     
  rate=cts/time*exp
  raterr=ctserr/time
  
  
;  cts=src-bg
;  sigma=(src-bg)/sqrt(src+bg)
;  ctr=cts/time*exp
;  err=sqrt(src+bg)
;  ctrerr=err/time
;  print,sigma,ctr,ctrerr
;  if sigma lt 3 then ul=ctr+ctrerr*3 else ul=0
;  ul=ctr+ctrerr*1.
  
  return
  
end 
