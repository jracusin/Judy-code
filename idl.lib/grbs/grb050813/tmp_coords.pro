pro tmp_coords,evt,ra,dec
  
  n=n_elements(evt)
  ra=dblarr(n)
  dec=dblarr(n)
  
  scra=241.98599
  scdec=11.261900
  roll=286.14099

  for i=0,n-1 do begin
     coordinates,evt[i].rawx,evt[i].rawy,scra,scdec,roll,0,0,0,0,rahms,decdms,ratmp,dectmp,/notam,raw=1
     ra[i]=ratmp
     dec[i]=dectmp
     
  endfor 
  
  return
end 
