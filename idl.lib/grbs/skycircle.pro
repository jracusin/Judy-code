pro skycircle,ra,dec,rad,x,y,n=n
  if n_elements(n) eq 0 then n=1000.
  x=fltarr(n) & y=x
  for i=0,n-1 do begin
     temp=ll_arc_distance([ra,dec]*!dtor,rad*!dtor,360./n*i*!dtor)
     x[i]=temp[0]*!radeg
     y[i]=temp[1]*!radeg
  endfor
  x=[x,x[0]]
  y=[y,y[0]]
;  x=x/cos(y*!dtor)

  w=where(x lt 0.,nw)
  if nw gt 0 then x=x+360.

return
end
