function calc_sunang,year,day,ra,dec,sunpos
  
  sunang=dblarr(n_elements(ra))
  sunpos=dblarr(2,n_elements(ra))
  for i=0,n_elements(ra)-1 do begin 
     ydn2md,year[i],day[i],month,dy
     jdcnv,year[i],month,dy,0.,jd
     sunpos,jd,sunra,sundec
     print,jd
     gcirc,1,sunra/360.*24D,sundec,ra[i]/360D*24D,dec[i],dis
     sunang[i]=dis/3600.
     sunpos[0,i]=sunra
     sunpos[1,i]=sundec
  endfor 
  
  return,sunang
end 
