function moon_parallax,mra,mdec,mdist,x,y,z
  
  mth=mra*!dtor
  mphi=(90-mdec)*!dtor
  mx=mdist*sin(mphi)*cos(mth)
  my=mdist*sin(mphi)*sin(mth)
  mz=mdist*cos(mphi)
  mv=[mx,my,mz]
  
  ev=[x,y,z]
  
  diff=mv-ev
  
  elaz2,diff,dec,ra
  
  return,[ra,dec]
  
end 
