PRO emission_measure,norm,err1,err2,toroid=toroid

  
  da=50D3*3.0857D18 ;50kpc -> cm
  z=0

  r=5.7D17  ;cm from 0".76 from park et al.(2004)
  w=1D16

  IF NOT keyword_set(toroid) THEN Vt=4./3.*!pi*r^3 ELSE Vt=!pi*r^2*w

  em=1D14*4*!pi*norm*da^2

  emerr1=(err1/norm)*em
  emerr2=(err2/norm)*em

  n_e=sqrt(em/(Vt*1.5))

;  em1=1D-14/(4*!pi*(da*(1+z))^2)
;  em2=1.5*nh^2*Vt                   ;/norm
;  Em=em1*em2

;print,em1,em2,em
;stop

  print,em,emerr1,emerr2
  print,n_e

return
END 

  
