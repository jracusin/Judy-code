function dist_overpole,ang1,ang2,pole=pole
  
  if n_params() eq 0 then begin
     print,'syntax - d=dist_overpole(ang1,ang2)'
     return,-1
  endif 
  
  angs=[ang1,ang2]
  a1=min(angs)
  a2=max(angs)
  
  if n_elements(pole) eq 0 then pole=360.
  d1=abs(a1+pole-a2)
  d2=abs(a2-a1)
  
  d=min([d1,d2])
  return,d
end 
