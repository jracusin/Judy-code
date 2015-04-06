pro check_mag_plane,date,planera,planedec,nmagra,nmagdec,smagra,smagdec
  
  ;;;Geo-magnetic pole location as of 2005
  nmagpolelat=79.7
  nmagpolelon=360.-71.8
  smagpolelat=-79.8
  smagpolelon=108.2
  
  s=size(date)
  if s[1] eq 7 then met=date2met(date) else met=date
  
  d=met2date_judy(met)
  year=d[0]
  doy=d[1]
  hr=d[2]
  minute=d[3]
  sec=d[4]
  hour=hr+minute/60.+sec/3600.
  ydn2md,year,doy,month,day
  
  ct2lst,nsidetime,nmagpolelon,0,hour,day,month,year ;jd
  nmagra=nsidetime/24.*360.
  nmagdec=nmagpolelat
  ct2lst,ssidetime,smagpolelon,0,hour,day,month,year ;,jd
  smagra=ssidetime/24.*360.
  smagdec=smagpolelat
  
  planera=findgen(365)
  delra=nmagra-planera
  w=where(delra lt 0)
  delra[w]=delra[w]+360.
  tandelplane=cos(delra*!dtor)/(-tan(nmagdec*!dtor))
  planedec=atan(tandelplane,/phase)*!radeg
  
  return
end 
