function band,k,e0,alpha,beta,emin=emin,emax=emax,en=en,intf=intf
  
;  A(E)=K(E/100.)^alpha_1 * exp(-E/E_c)     E < (alpha_1-alpha_2)*E_c
;  A(E)=K[(alpha_1-alpha_2)*E_c/100]^(alpha_1-alpha_2) *(E/100)^alpha_2* exp-(alpha_1-alpha_2)   E > (alpha_1 - alpha_2)*E_c
  
  if n_elements(emin) eq 0 then emin=1. ;kev
  if n_elements(emax) eq 0 then emax=1.e4 ;kev
  
  en=dindgen(emax-emin)+emin  ;;1 keV to 10 MeV
  a=dblarr(emax-emin)
  
  w1=where(en le (alpha-beta)*e0,n1)
  w2=where(en ge (alpha-beta)*e0,n2)
  
  if n1 gt 0 then a[w1]=k*(en[w1]/100.)^alpha*exp(-en[w1]/e0)
  if n2 gt 0 then a[w2]=k*((alpha-beta)*e0/100.)^(alpha-beta)*exp(beta-alpha)*(en[w2]/100.)^beta
  
  intf=int_tabulated(en,a*en,/double)
;  plot,en,a,/xlog,/ylog;,xrange=[emin,emax],xstyle=1,yrange=minmax(a),ystyle=1
  
  return,a
end 

function calc_eiso_band,z,alphapl,obsflu,obsband=obsband,alpha=alpha,beta=beta,epeak=epeak,h0=h0
  
  if n_params() eq 0 then begin
     print,'syntax - eiso=calc_eiso(z,alpha_PL,observed fluence[,obsband=[15,150.],'
     print,'                        alpha=-1.,beta=-2.5,epeak=epeak,h0=0.73])'
     return,0
  endif
  
  ;;FOLLOWING METHOD OF BLOOM ET AL. (2001)
  if n_elements(h0) eq 0 then h=0.7 else h=h0
  evtoerg=1.60217646d-12
  kevtoerg=evtoerg*1d3
  pctocm=3.08568025d18
  d=lumdist(z,h=h*100.)
  dist=d*1e6*pctocm       ;;distance in cm
  ;;;using Sakamoto et al. (2007)/Zhang et al. (2007) epeak vs. alpha PL correlation
  
  if n_elements(epeak) eq 0 then doepeak=1 else if epeak eq 0 then doepeak=1 else doepeak=0
  
  if doepeak then begin 
     log_epeak=2.76-3.61*alog10(-alphapl)
     epeak=10.^log_epeak
  endif 
  
  ;;assume average alpha & beta
  if n_elements(alpha) eq 0 then alpha=-1d else if alpha eq 0 then alpha=-1d
  if n_elements(beta) eq 0 then beta=-2.5d else if beta eq 0 then beta=-2.5d
  
  e0=epeak/(alpha+2.)  ;;from Sato et al. (2006)

  if n_elements(obsband) eq 0 then begin
     e1=15.                     ;kev
     e2=150.                    ;kev
  endif else begin
     e1=obsband[0]
     e2=obsband[1]
  endelse 
  ee1=1.                        ;kev
  ee2=1e4                       ;kev = 10 MeV
    
  de=1. ;keV
;  obs_band=band(1.,e0,alpha,beta,emin=15.,emax=150.,en=en)
;  obs_band_int=total(en*obs_band*de)
  s0=1.;obsflu/obs_band_int
  
  ee1z=ee1/(1.+z) ;;shifts from intrinsic 1 keV to observed
  ee2z=ee2/(1.+z) ;;shifts from intrinsic 10 MeV to observed
  sz_band=band(1.,e0,alpha,beta,emin=ee1z,emax=ee2z,en=en)
  sz=int_tabulated(en,en*sz_band)
  Iz=sz/s0
  so_band=band(1.,e0,alpha,beta,emin=e1,emax=e2,en=en)
  so=int_tabulated(en,en*so_band)
  Io=so/s0
  k=Iz/Io ;;k-correction
  
  eiso=obsflu*4.*!pi*dist^2/(1.+z)*k
  print,eiso,alog10(eiso)
  stop
;  stop
  return,eiso
end   

