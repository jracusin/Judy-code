function separation,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12
  
  if n_params() eq 0 then begin
     print,'syntax - dist=separation(ra1,dec1,ra2,dec2)'
     print,'      or dist=separation(rah1,ram1,ras1,decd1,decm1,decs1,rah2,ram2,ras2,decd2,decm2,decs2)'
     return,-1
  endif 
  
  if n_params() eq 12 then begin
     hms2radec,c1,c2,c3,c4,c5,c6,ra1,dec1
     hms2radec,c7,c8,c9,c10,c11,c12,ra2,dec2
  endif else begin
     ra1=c1
     dec1=c2
     ra2=c3
     dec2=c4
  endelse 
     
  lon1=ra1*!dtor*1d
  lat1=dec1*!dtor*1d
  lon2=ra2*!dtor*1d
  lat2=dec2*!dtor*1d
  
  dellon=lon2-lon1
  
  dist=acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(dellon))*!radeg*3600d
  
  return,dist
  
end 
