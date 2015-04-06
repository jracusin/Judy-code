function prange,x,xerr
  
  if n_params() eq 0 then begin
     print,'syntax - range = prange(param,err)'
     return,0
  endif 
  
  m1=min(x-xerr)
  m2=max(x+xerr)
  range=[m1-0.1*m1,m2+0.1*m2]
  
  return,range
end 
  
  
