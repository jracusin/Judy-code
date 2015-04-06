function calc_coord_offset,ra1,dec1,ra2,dec2,hours=hours,dra=dra,ddec=ddec
  
  if n_params() eq 0 then begin 
     print,'syntax - offset=calc_coord_offset(ra1,dec1,ra2,dec2[,/hours])'
     print,'                 default ra in degrees, set hours if in hours'
     return,0
  endif 
  
  if keyword_set(hours) then begin
     ura1=ra1*15D
     ura2=ura2*15D
  endif else begin 
     ura1=double(ra1)
     ura2=double(ra2)
  endelse 
  udec1=double(dec1)
  udec2=double(dec2)
  
  dra=(ura1-ura2)*cos(udec2*!dtor)
  ddec=(udec1-udec2)
  
;  offset=sqrt(((ura1-ura2)*cos(udec2*!dtor))^2+(udec1-udec2)^2)*3600D
  offset=sqrt(dra^2+ddec^2)*3600D
  
  dra=dra*3600D
  ddec=ddec*3600D
  
  return,offset
end 
