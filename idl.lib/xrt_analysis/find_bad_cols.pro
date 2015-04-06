function find_bad_cols,x,y,nbc
  
  nx=n_elements(x)
  
  badcols=0
  yes=0
  for i=0,nx-1 do begin
     
     w=where(x eq x[i],nw)
     if nw gt 20 then begin 
        badcols=[badcols,x[i]]
        yes=1
     endif 
  endfor 
  
  if yes then begin
     badcols=badcols[1:*]
     badcols=badcols[rem_dup(badcols)]
  endif 
  
  nbc=n_elements(bc)
  return,badcols
  
end 
