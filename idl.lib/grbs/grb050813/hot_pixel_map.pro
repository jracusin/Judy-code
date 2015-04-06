pro hot_pixel_map,bp,rmap,map
  
  
  rmap=intarr(600,600) & map=intarr(601,601)
  
  badcol=[146,177,291,292,319]
  badpix=[[345,224],$
          [260,246],$
          [304,265],$
          [389,271],$
          [236,301],$
          [306,303],$
          [230,306],$
          [301,332],$
          [289,361],$
          [347,390]]
;  for i=0,n_elements(badcol)-1 do rmap[badcol[i],*]=1
  rmap[badcol,*]=1
  x=badpix[0,*]
  y=badpix[1,*]
  rmap[x,y]=1
  rmap[bp.rawx,bp.rawy]=1
  
  dbadcol=badcol+1
;  for i=0,n_elements(badcol)-1 do map[dbadcol[i],*]=1
  map[badcol+1,*]=1
  x=badpix[0,*]+1
  y=badpix[1,*]+1
  map[x,y]=1
  map[bp.rawx+1,bp.rawy+1]=1
  stop
  !p.multi=[0,1,2]
  rdis,rmap
  rdis,map
  !p.multi=0
  stop
  return
end 
