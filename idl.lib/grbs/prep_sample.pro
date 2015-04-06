pro prep_sample,file

  if n_params() eq 0 then begin 
     print,'syntax - prep_sample,file'
     return
  end 

  readcol,file,grb,tid,format='(a,a)'

  cd,'~/GRBs'
  n=n_elements(grb)
  g=0
  colprint,indgen(n),grb,tid
  stop
  
  for i=g,n-1 do begin 
     download_phil_lc,grb[i],tid[i]
     download_phil_spec,grb[i],tid[i]
  endfor 
  return
end 
