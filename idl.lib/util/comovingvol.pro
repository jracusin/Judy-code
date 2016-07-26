function ezint,z,p

  omega_k=0.
  omega_m=p[0]
  omega_lambda=p[1]

  val=1./sqrt(omega_m*(1+z)^3+omega_k*(1+z)^2+omega_lambda)

  return,val
end

function comovingvol,z,h=h,omega_m=omega_m,omega_lambda=omega_lambda

  if n_elements(omega_m) eq 0 then omega_m=0.27
  if n_elements(omega_lambda) eq 0 then omega_lambda=0.73
  if n_elements(h) eq 0 then h=0.71
  if h gt 1 then h=h/100.

  c=3e5 ;; km/s
  H0=100*h ;; km s-1 Mpc-1
  Dh=c/H0

  nz=n_elements(z)
  
  dc=dblarr(nz)
  for i=0,nz-1 do Dc[i]=Dh*qpint1d('ezint',0,z[i],[omega_m,omega_lambda])
  Dm=Dc  ;;; for omega_k=0
  if nz eq 1 then dm=dm[0]

  vol=4*!pi/3.*Dm^3  ;; in Mpc-3
  return,vol
end 
