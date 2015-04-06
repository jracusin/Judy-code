pro check_orbit_plane,date,eph,orbra,orbdec,norbnormra,norbnormdec,sorbnormra,sorbnormdec
  
  orbtime=96.5*60.
  met=date2met(date)
  w=where(eph.met gt met-orbtime/2. and eph.met lt met+orbtime/2.,nw)
  if nw gt 0 then begin 
     orbra=eph[w].era
     orbdec=eph[w].edec
  endif else begin
     orbra=0.
     orbdec=0.
  endelse 
  
  mindec=min(orbdec,m)
  norbnormdec=mindec+90.
  delalp=acos(-tan(norbnormdec*!dtor)*tan(mindec*!dtor))*!radeg
  norbnormra=orbra[m]+delalp
  
  maxdec=max(orbdec,m)
  sorbnormdec=maxdec-90.
  delalp=acos(-tan(sorbnormdec*!dtor)*tan(maxdec*!dtor))*!radeg
  sorbnormra=orbra[m]-delalp
  
;  sorbnormdec=-norbnormdec
;  sorbnormra=norbnormra+180.
;  if sorbnormra gt 360. then sorbnormra=sorbnormra-360.
  
  
  return
end 
