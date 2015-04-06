pro model_det_area
  
  scen=[291,318];[500.09274,483.46201]
  bcen=[330,155];[473.08237,650.15287]
  
  sr=20.
  br=40.
  
  simg=fltarr(41,41)
  bimg=fltarr(81,81)
  
  sx=intarr(41,41)
  sy=intarr(41,41)
  for i=0,40 do begin
     sx[*,i]=indgen(41)+scen[0]-sr
     sy[i,*]=indgen(41)+scen[1]-sr
  endfor 
  
  bx=intarr(81,81)
  by=intarr(81,81)
  for i=0,80 do begin
     bx[*,i]=indgen(81)+bcen[0]-br
     by[i,*]=indgen(81)+bcen[1]-br
  endfor 
  
  simg[*,*]=sqrt((sx-scen[0])^2+(sy-scen[1])^2)
  bimg[*,*]=sqrt((bx-bcen[0])^2+(by-bcen[1])^2)
  
  help,where(simg le sr and (sx eq 292 or sx eq 293 or sx eq 319))
  help,where(bimg le br and (bx eq 292 or bx eq 293 or bx eq 319))
  stop
  return
end 
