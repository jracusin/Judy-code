function whatlum,z,mag,tj
  dist=lumdist(z,h0=71,lambda=0.73)*1d6
  absmag=mag-5.*(alog10(dist)-1.)
  lum=10^((4.72-absmag)/2.5)
  print,lum
  lumgam=lum*(1-cos(tj*!dtor))
  print,lumgam
  
  return,absmag
end 

pro compare_bright_bursts
  
  ;;grb080319b
  
  z=0.937
  mag=5.3
  tj=0.4
  print,'080319b',whatlum(z,mag,tj)
  
  ;;grb990123
  z=1.6
  mag=9.
  tj=2.9
  print,'990123',whatlum(z,mag,tj)
  
  ;;grb050904
  z=6.295
  z=1.
  mag=7.6
  tj=4.
  print,'050904',whatlum(z,mag,tj)
  
  ;;grb061007
  z=1.26
  mag=10.3
  tj=0.8
  print,'061007',whatlum(z,mag,tj)
  
  ;;grb030329
  z=0.1685
  mag=13.
  tj=3.8
  print,'030329',whatlum(z,mag,tj)
  
  
  
  
  return
end 
