function approx,x,val,m,n

  if n_params() eq 0 then begin
     print,'syntax - a=approx(x,val,ind,num)'
     return,0
  end

  minx=min(abs(x-val),m)  
  a=x[m]

  if n_elements(n) gt 0 then begin 
     a=[a,fltarr(n)]
     ms=[m,lonarr(n)]
     xs=x
     mm=m
     ind=lindgen(n_elements(x))
     for i=0,n-1 do begin 
        xx=[xs[0:mm-1],xs[mm+1:*]]
        in=[ind[0:mm-1],ind[mm+1:*]]
        minx=min(abs(xx-val),mm)
        a[i+1]=xx[mm]
        ms[i+1]=in[mm]
        xs=xx
     endfor 
  endif else ms=m
  
  m=ms
  return,a
end 
