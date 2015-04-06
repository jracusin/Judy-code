pro calc_src_rate,src,bg,radsrc,radbg,exptime
  
  cts=(1.*src-bg*1.*(radsrc*1./radbg)^2)/exptime*1.
  err=sqrt(src*1.+bg*(radsrc*1./radbg)^2)/exptime*1.
  
  print,cts,'+/-',err
  
  
  ;;;;NOT SURE IF THIS IS RIGHT
  return
end 
