pro calc_cdf,val,x,p;,bin=bin

  if n_params() eq 0 then begin 
     print,'syntax - calc_cdf, val, x, p'
     return
  end 

  nv=n_elements(val)
  if nv gt 1 then begin
;     plothist,val,x,y,bin=bin,/noplot 
     s=sort(val)
     x=val[s]
     y=nv
  endif else begin 
     x=val
     y=1.
  endelse
 
  ymax=y*1d
;  ymax=1.
;  ymax=total(y)
  n=n_elements(x)
  p=dblarr(n)
  for i=0,n-1 do begin
;     p[i]=total(y[0:i])/ymax
     p[i]=(i+1.)/ymax
  endfor 
  if nv eq 1 then p=1.

return
end 
