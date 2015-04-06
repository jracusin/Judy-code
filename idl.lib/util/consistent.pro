function consistent,val0,x,err,wcon=wcon,nw=nw

  if n_params() lt 3 then begin
     print,'syntax -  [true/false] = consistent(value, measurement , error [can be 2d])'
     return,-1
  end 

  n=n_elements(x)

  s=size(err)
  if s[1] eq 2 then begin
     err0=err[0,*]
     err1=err[1,*]
  endif else begin
     err0=err
     err1=err
  endelse

  if n_elements(val0) eq 1 then val=replicate(val0,n) else val=val0

  wpos=where(x-val ge 0.,npos)
  wneg=where(x-val le 0.,nneg)

  if npos gt 0 then wconp=where(x[wpos]-val[wpos]-err0[wpos] le 0.,nconp) else nconp=0
  if nneg gt 0 then wconn=where(val[wneg]-x[wneg]-err1[wneg] le 0.,nconn) else nconn=0

  wcon=-1
  if nconp gt 0 then wcon=wpos[wconp]
  if nconn gt 0 then wcon=wneg[wconn]
  if nconp gt 0 and nconn gt 0 then wcon=[wpos[wconp],wneg[wconn]]

  con=intarr(n)

  if wcon[0] ne -1 then begin 
     s=sort(wcon)
     wcon=wcon[s]
     con[wcon]=1
  endif 
  w=where(con eq 1,nw)

return,con
end 
