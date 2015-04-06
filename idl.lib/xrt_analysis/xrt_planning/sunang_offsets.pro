pro sunang_offsets
  
  offra=0.;180.
  offset=50.5D
  tmp=calc_sunang(2004,362,0,0,sunpos)
  
  sunra=sunpos[0]
  sundec=sunpos[1]
  
  euler,sunra,sundec,sunlong,sunlat,3
  
  print,'Sun (ra,dec): ',sunra,sundec
  print,'Sun (long,lat): ',sunlong,sunlat          ;sun in ecliptic coords
  
  objlong=sunlong+offra
  objlat=sunlat+offset
  
  euler,objlong,objlat,objra,objdec,4
  
  print,'Sun + '+ntostr(offset)+' deg (ra,dec): ',objra,objdec
  
  objlong=sunlong
  objlat=sunlat-offset
  
  euler,objlong,objlat,objra,objdec,4
  
  print,'Sun - '+ntostr(offset)+' deg (ra,dec): ',objra,objdec
  
  return
end 
