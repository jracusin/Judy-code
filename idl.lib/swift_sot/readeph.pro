function readeph,ephfile
  
  readcol,ephfile,date,time,x,y,z,lat,lon,ra,dec,format='(a,a,d,d,d,d,d,d,d)'
  eph=create_struct('date','',$
                    'dy','',$
                    'time','',$
                    'met',0d,$
                    'x',0d,$
                    'y',0d,$
                    'z',0d,$
                    'lat',0d,$
                    'lon',0d,$
                    'ra',0d,$
                    'dec',0d,$
                    'era',0d,$
                    'edec',0d)
  
  n=n_elements(date)
  eph=replicate(eph,n)
  
  for i=0L,n-1 do begin 
     tmp=str_sep(date[i],'/')
     eph[i].date=tmp[1]+'-'+tmp[0]+'-'+time[i]
     eph[i].dy=tmp[0]+'/'+tmp[1]
     
     elaz2,-1.*[x[i],y[i],z[i]],edec,era
     eph[i].era=era
     eph[i].edec=edec
     eph[i].met=round(date2met(eph[i].date))
  
  endfor 
  eph.time=time
  
  eph.ra=ra                   
  eph.dec=dec          
  eph.x=x
  eph.y=y
  eph.z=z
  eph.lat=lat
  eph.lon=lon
  
  date=0 & time=0 & x=0 & y=0 & z=0 & lat=0 & lon=0 & ra=0 & dec=0 ;free memory
  
  return,eph
end 
