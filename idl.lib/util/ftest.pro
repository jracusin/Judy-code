function ftest,chisq1,chisq2,m,n,p
  
  if n_params() eq 0 then begin
     print,'syntax - F=ftest(chisq1,chisq2,m,N,p)'
     print,'  m=# of old terms, N=# of points, p=# of additional terms (default=1)'
     return,0
  endif 
  
  if n_elements(p) eq 0 then p=1
  
  f=(chisq1-chisq2)/(chisq2/(n-m-p))
  
  return,f
end 
