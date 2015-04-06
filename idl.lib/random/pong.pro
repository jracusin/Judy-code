pro pong

  xrange=[0,10]
  yrange=[0,10]
  plot,xrange,yrange,/nodata,xticks=1,yticks=1

  plotsym,0,/fill
  die=0
  while not die do begin
     x=randomu(seed)*10.
     y=randomu(seed)*10.
     print,x,y
     plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xticks=1,yticks=1
     plots,x,y,psym=8,symsize=2
     oplot,xrange,[1,1],line=2
     wait,1

     if y lt 1 then die=1
  endwhile

return
end 
