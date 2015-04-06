pro find_grb_agn,yes

  cd,'~/GRBs'
  
  dir=file_search('GRB*')
  n=n_elements(dir)
  
  g=0
;  if n_elements(yes) eq 0 then yes=intarr(n) else begin
     w=where(yes eq 1,n)
;     g=max(w)
;  endelse 

;  stop
     
  for j=g,n-1 do begin
     i=w[j]
     cd,dir[i]
     print,i,'   ',dir[i]
     if exist('PCCURVE.qdp') or exist('WTCURVE.qdp') then begin 
        fit_lc,/justplot,/phil,title=dir[i]
        print,'Is this flat? (y/n)'
        k=get_kbrd(10)
        if k eq 'y' then yes[i]=1 else yes[i]=0
        if k eq 's' then stop
        if k eq 'q' then goto,skip
     endif 
     cd,'..'
  endfor 

  skip:
  w=where(yes eq 1)
  colprint,dir[w],yes[w],w

  save,dir,yes,file='flat.dat'

  stop
  return
end 
