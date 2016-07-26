pro plot_lc,grb,lc=lc,ps=ps

  ngrb=n_elements(grb)
  if ngrb eq 0 and n_elements(lc) gt 0 then begin
     ngrb=1
     skip=0
  endif else skip=1

  for i=0,ngrb-1 do begin
     if skip then begin 
        cd,'~/GRBs'
        cd,strtrim(grb[i],2)
        print,grb[i]
     endif
     fit_lc,/phil,/nohard,/justplot,lc=lc,ps=ps
     if skip then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
        cd,'..'
     endif 
  endfor 

return
end
