pro which_lc,grb,lc=lc,phil=phil
  
  if n_params() eq 0 then begin
     print,'syntax - which_lc,grb'
     return
  endif 
  
  !p.multi=0
  ngrb=n_elements(grb)
  cd,!mdata
  for i=0,ngrb-1 do begin
     dir=strtrim(grb[i])
     if exist(dir) then begin 
        print,dir
        cd,dir
        fit_lc,/justplot,title=dir,lc=lc,phil=phil
        
        k=get_kbrd(10)
        if k eq 's' then stop
        if k eq 'q' then goto,cheat
        cd,'..'
     endif else print,dir+' does not exist'
  endfor 
  cheat:
  
  return
end 
