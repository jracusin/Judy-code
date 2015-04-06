function random_pdist,pdist,n,xrand=x,yrand=y,arr=arr,xarr=xarr,yarr=yarr

;  t=systime(1)
  w=where(pdist gt 0.005,nw)
  if n_elements(arr) eq 0 then begin 
     g=100.
     arr=0.
     for i=0,nw-1 do begin
        if fix(pdist[w[i]]*g) gt 0 then arr=[arr,replicate(i,fix(pdist[w[i]]*g))]
     endfor 
     arr=arr[1:*]
  endif 

  r=round(randomu(seed,n)*n_elements(arr))
  
  ri=arr[r]

  s=size(pdist)
  if s[0] eq 2 then begin
     xarr=intarr(s[1],s[1])
     yarr=xarr
     for i=0,s[1]-1 do begin
        xarr[i,*]=i
        yarr[*,i]=i
     endfor 

     x=xarr[w[ri]]
     y=yarr[w[ri]]
  endif 

;  ptime,systime(1)-t
  return,ri
end 
