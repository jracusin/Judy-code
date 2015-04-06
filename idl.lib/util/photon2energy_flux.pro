function photon2energy_flux,eng,phind,flux

  if n_params() eq 0 then begin
     print,'syntax - f=photon2energy_flux(eng, phind, flux)'
     print,'                   assumes eng in keV, power law spectrum, and flux in photons cm-2 s-1'
     return,0
  end 

  kev2erg=1.602e-12*1d3
  enind=(eng[1]-eng[0])/99.
  en=dindgen(100)*enind+eng[0]
  enar=dblarr(2,99)
  enar[0,*]=en[0:98]
  enar[1,*]=en[1:99]
  miden=enar[0,*]+enind/2.
  
  int=intpow(eng,[1.,phind])*(eng[1]-eng[0])
  n=flux/int
  s=total(intpow(enar,[1.,phind])*enind*miden)*kev2erg
  f=s
  print,int,n,s;,f

  return,f
end 
