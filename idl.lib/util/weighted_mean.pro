function weighted_mean, xs, us, unc
  
  if n_params() eq 0 then begin
     print,'syntax - wmean=weighted_mean(array,err on array,err on mean)'
     return,-1
  endif 
  
  wmn=total(xs/us^2)/total(1.0/us^2)
  unc=sqrt(1./total(1.0/us^2))
  
return,wmn
end
