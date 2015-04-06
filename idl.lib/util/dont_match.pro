pro dont_match,arr1,arr2,dm1,dm2
  
  if n_params() eq 0 then begin 
     print,'syntax - dont_match,arr1,arr2,dm1,dm2'
     return
  endif 
  
  match,arr1,arr2,m1,m2
  
  p1=intarr(n_elements(arr1))
  p2=intarr(n_elements(arr2))
  
  if m1[0] ne -1 then p1[m1]=1
  if m2[0] ne -1 then p2[m2]=1
  
  dm1=where(p1 eq 0)
  dm2=where(p2 eq 0)
  
  return
end 
