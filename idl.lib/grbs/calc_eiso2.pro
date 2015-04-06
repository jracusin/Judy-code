function simple_pl2,e,alpha
  
  f=e^(alpha)
  
  return,f
end 

function cutoff_pl2,e,alpha,epeak
  
  f=e^alpha*exp(-e*(2.+alpha)/epeak)
  
  return,f
end

function band2,e,alpha,e0,beta,enorm
  f=dblarr(n_elements(e))
  w1=where(e le (alpha-beta)*e0,n1)
  w2=where(e ge (alpha-beta)*e0,n2)
  if n1 gt 0 then f[w1]=(e[w1]/enorm)^alpha*exp(-e[w1]/e0)
  if n2 gt 0 then f[w2]=((alpha-beta)*e0/enorm)^(alpha-beta)*exp(beta-alpha)*(e[w2]/enorm)^beta
  return,f
end 

function band_pl,e,alpha,e0,beta,alpha2,n,enorm,f1=f1,f2=f2

  f1=band2(e,alpha,e0,beta,enorm)
  f2=n*simple_pl2(e,alpha2)
  f=f1+f2

  return,f
end   

function calc_eiso2,fluence,fmin,fmax,z,alpha,beta,epeak,phind,n,emin=emin,emax=emax,h0=h0,omega_m=omega_m,cutoff=cutoff,pl=pl,bandpl=bandpl,enorm=enorm

  if n_params() eq 0 then begin 
     print,'syntax - eiso=calc_eiso2(fluence,fmin,fmax,z,alpha,beta,epeak,phind,n,emin=emin,emax=emax,h0=h0,omega_m=omega_m,cutoff=cutoff,pl=pl,bandpl=bandpl)'
     return,0
  end 

  if n_elements(emin) eq 0 then emin=10d ;; 10 keV
  if n_elements(emax) eq 0 then emax=1d4 ;; 10 MeV
  if n_elements(omega_m) eq 0 then omega_m=0.27
  if n_elements(enorm) eq 0 then enorm=1.
  if z le 0 then return,0.

  d=lumdist(z,h0=h0,omega_m=omega_m,lambda0=(1.-omega_m),/silent)
  pc2cm=3.08568025d18
  dist=d*1e6*pc2cm
;  e0=epeak;/(1.+z)
  e0=epeak/(2.+alpha);*(1.+z)
  ev2erg=1.60217646d-12
  kev2erg=ev2erg*1d3
  
  if emax gt 1e3 then x=50d else x=1.
  if emax gt 1d6 then x=1000d
  e=dindgen((emax-emin)/x)*x+emin
  e=e/(1.+z)
  ;;; add cutoff PL & PL
  if keyword_set(cutoff) then f1=cutoff_pl2(e,alpha,epeak)
  if keyword_set(pl) then f1=simple_pl2(e,alpha)
  if keyword_set(bandpl) then f1=band_pl(e,alpha,e0,beta,phind,n)
  if not keyword_set(cutoff) and not keyword_set(pl) and not keyword_set(bandpl) then $
     f1=band2(e,alpha,e0,beta,enorm)
  k1=int_tabulated(e,f1*e,/double)*kev2erg
  if fmax gt 1e3 then x=10d else x=1.
  e2=dindgen((fmax-fmin)/x)*x+fmin
  if keyword_set(cutoff) then f2=cutoff_pl2(e2,alpha,epeak)
  if keyword_set(pl) then f2=simple_pl2(e2,alpha)
  if keyword_set(bandpl) then f2=band_pl(e2,alpha,e0,beta,phind,n)
  if not keyword_set(cutoff) and not keyword_set(pl) and not keyword_set(bandpl) then $
     f2=band2(e2,alpha,e0,beta,enorm)
  k2=int_tabulated(e2,f2*e2,/double)*kev2erg

  k=k1/k2
  eiso=4.*!pi*k*dist^2*fluence/(1.+z)

  return,eiso
end 
  
