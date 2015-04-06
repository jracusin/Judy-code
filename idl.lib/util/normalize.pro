function normalize,x,off=off,scale=scale
  
  if n_params() eq 0 then begin
     print, 'syntax - f=normalize(x)'
     return,0
  end 

  y=x-min(x)
  off=min(x)
  maxy=max(y)
  miny=min(y)
  
  scale=(maxy-miny)
  y=y/(maxy-miny)

  return,y
end 
