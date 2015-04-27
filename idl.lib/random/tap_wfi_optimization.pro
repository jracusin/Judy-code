pro tap_wfi_optimization

  r=28.5
  a=1.
  x=[-r,-r,-r,0,0,0,r,r-a,r-a]
  y=[-r,0,r,-r,0,r-a,-r,0,r-a]

  map_set,/aitoff,/grid
;  oplot,x,y,psym=2
  xyouts,x,y,ntostr(indgen(9)+1),/data
;  plot,x,y,psym=2,/iso,xrange=[-r*2,r*2],yrange=[-r*2,r*2]
  sx=[-r/2,r/2,r/2,-r/2,-r/2]
  sy=[-r/2,-r/2,r/2,r/2,-r/2]

  for i=0,8 do begin
     oplot,sx+x[i],sy+y[i],line=2
  endfor 


  stop
  return
end 
