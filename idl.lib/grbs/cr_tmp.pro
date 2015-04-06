pro cr_tmp
  
  dir=file_search('GRB*/')
  fit=file_search('GRB*/lc_fit_out_idl.dat')
  nfit=n_elements(fit)
  fdir=strarr(nfit)
  for i=0,nfit-1 do begin
     p=str_sep(fit[i],'/')
     fdir[i]=p[0]
     
  endfor
  dont_match,dir,fdir,m1,m2
  stop
  
  return
end 
