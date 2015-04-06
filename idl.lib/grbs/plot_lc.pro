pro plot_lc,grb

  ngrb=n_elements(grb)

  for i=0,ngrb-1 do begin
     cd,'~/GRBs'
     cd,strtrim(grb[i],2)
     print,grb[i]
     fit_lc,/phil,/nohard,/justplot
     k=get_kbrd(10)
     if k eq 's' then stop
     cd,'..'
  endfor 

return
end
