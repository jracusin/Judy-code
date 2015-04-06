function dot,v1,v2
  
  if n_params() eq 0 then begin
     print,'syntax - a=dot(v1,v2)    i.e. a=v1 dot v2'
     return,0
  endif 
  
  
  a=transpose(v1)#v2
;  a=a[0]
  
  return,a
end 
