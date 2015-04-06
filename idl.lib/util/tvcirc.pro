pro tvcirc, x, y, r, color=color, help=hlp
  
  if (n_params(0) lt 3) or keyword_set(hlp) then begin
     print,' Draw a circle on the display.'
     print,' tvcirc, x, y, r'
     print,'   x,y = center of circle in device units.    in'
     print,'   r = Radius of circle in device units.      in'
     print,'       May be an array of radii.'
     print,' Keywords:'
     print,'   COLOR=c  plot color (def=!p.color).'
     return
  endif
  
  if n_elements(color) eq 0 then color = !p.color
  
  a = makex(0, 360, 2)/!radeg
  
  for i = 0, n_elements(r)-1 do begin
     xx = x + r(i)*cos(a)
     yy = y + r(i)*sin(a)
     plots,xx,yy,/data,color=color
  endfor
  
  return
end 
