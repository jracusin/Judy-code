pro clustering,x,y,xg,yg
  
  n=n_elements(x)
  
;  data=fltarr(n,n)
;  dist=fltarr(n)
  ncen=n_elements(xg)
  
  d=fltarr(ncen,n)

;     for i=0,n-1 do begin
;     ind=intarr(n)
;     ind[i]=1
;     w=where(ind eq 0)
;     d=sqrt((y[w]-y[i])^2+(x[w]-x[i])^2)
;     maxd=max(d,m)
;     mind=min(d,m)
;     dist[i]=maxd
;     endfor 
  
  c=intarr(n)
  for j=0,ncen-1 do begin 
      d[j,*]=sqrt((x-xg[j])^2+(y-yg[j])^2)   
  endfor 
  for i=0,n-1 do begin
     mind=min(d[*,i],m)
     c[i]=m
  endfor 
  
  
  
  stop
  return
end 
