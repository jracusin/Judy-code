function sync,norm,nu,p,num,nuc
  
  f=bkn2pow(nu,[norm,-1./3.,num,(p-1.)/2.,nuc,p/2.])
  return,f
end 
  
function band,e,k,alpha,e0,beta
  f=dblarr(n_elements(e))
  w1=where(e le (alpha-beta)*e0,n1)
  w2=where(e ge (alpha-beta)*e0,n2)
  if n1 gt 0 then f[w1]=k*(e[w1]/50.)^alpha*exp(-e[w1]/e0)
  if n2 gt 0 then f[w2]=k*((alpha-beta)*e0/50.)^(alpha-beta)*exp(beta-alpha)*(e[w2]/50.)^beta
  return,f
end 

function ssc,nu,nuc,num,p,norm,cool

  f=dblarr(n_elements(nu))

  if cool eq 1 then begin 
     w1=where(nu le nuc)
     w2=where(nu gt nuc and nu le num)
     w3=where(nu gt num); and nu lt nue)
;     w4=where(nu ge nue)
     f[w1]=(nuc/num)^0.5*(nu[w1]/nuc)^(4./3.)
     f[w2]=(nu[w2]/num)^0.5
     f[w3]=(nu[w3]/num)^((2.-p)/2.)
;     f[w4]=
  endif else begin
     w1=where(nu le num)
     w2=where(nu gt num and nu le nuc)
     w3=where(nu gt nuc); and nu lt nue)
;     w4=where(nu ge nue)
     f[w1]=(num/nuc)^((3.-p)/2.)*(nu[w1]/num)^(4./3.)
     f[w2]=(nu[w2]/nuc)^((3.-p)/2.)
     f[w3]=(nu[w3]/nuc)^((2.-p)/2.)
  endelse 
  f=norm*f
  
return,f
end 

function ic,nu,eic,p,norm
  
;  f=bknpow(nu,[norm,-0.5,eic,(p-2.)/2.])
;  f=band(nu,norm,0.5,eic,-(p-2.)/2.)
  f=bkn2pow(nu,[norm,-1.,1e6,-1./3.,1e7,1./2.,1e8,p/2.])
  
  return,f
end 

pro grb_spec
  
  p=2.2
  nu=10^((dindgen(100)-10.)/8.)*1e-3
  
  norm1=1.e1
  norm2=1e-1
  norm2b=1e-6
  norm3=1e-6
  f1=sync(norm1,nu,p,2.e-3,1e-2)
;  f1=band(nu,norm1,1.,2.,-2.5)
;  f2=ssc(nu,1e4,1e2,p,norm2,0)
  f2=ssc(nu,1e1,1e3,p,norm2,1)
;  f2b=band(nu,norm2b,0.8,651,-3.5)
  f3=ic(nu,1d7,p,norm3)
  
  begplot,name='~/Desktop/Jobs/grb_spec.eps',/encap,/land
  plot,nu,f1,line=0,/xlog,/ylog,yrange=[1e-6,1e2],xrange=[1e-4,1e8],/xsty,/ysty,xtitle='E (keV)',ytitle=!tsym.nu+'F!L'+!tsym.nu,/iso;,ytickf='loglabels2'
  oplot,nu,f2,line=1
;  oplot,nu,nu*f2b,line=3
  oplot,nu,f3,line=2
  xyouts,1e-3,4,'Synchrotron',charsize=2
  xyouts,5e1,1e-1,'SSC',charsize=2
  xyouts,1e7,6,'IC',charsize=2
  endplot

  return
end 
