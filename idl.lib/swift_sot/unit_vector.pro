function unit_vector,p
  
  if n_params() eq 0 then begin
     print,'pu = unit_vector(p)  i.e. pu=|p|'
     return
  endif 
  
  p1=p^2
  p2=total(p1)
  p3=sqrt(p2)
  
  return,p3
  
end 
