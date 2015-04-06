function ntostrarr,nums,delim,ndig
  
  if n_params() eq 0 then begin
     print,"syntax - starr=ntostrarr(nums,delim {default=' '},ndig)"
     return,0
  endif 
  
  
  n=n_elements(nums)
  if n_elements(ndig) eq 0 then ndig=100
  
  if n_elements(delim) eq 0 then delim=' '
  
  for i=0,n-1 do begin
     if i eq 0 then st=ntostr(nums[i],ndig) else st=st+delim+ntostr(nums[i],ndig)
  endfor 

  return,st
end 
