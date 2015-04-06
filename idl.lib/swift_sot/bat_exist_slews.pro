pro bat_exist_slews

  erase
  ra=[120.117,104.455,63.944,47.84,96.173,94.266,85.562,84.116,92.001,111.165]
  dec=[15.776,24.79,16.739,13.656,-33.009,-21.775,-4.483,48.577,66.697,76.637]
  roll=[106,86,71,71,76,76,86,91,101,121]
  map_set,/aitoff,/grid
  plots,ra,dec,psym=1
  plots,ra,dec

  ras=fltarr(10,2)
  decs=ras
  for i=0,9 do begin 
     rotate_roll,ra[i],dec[i],ra[i]-5.,dec[i],roll[i],rap1,decp1    
     rotate_roll,ra[i],dec[i],ra[i]+5.,dec[i],roll[i],rap2,decp2     
     ras[i,0]=rap1
     decs[i,0]=decp1
     ras[i,1]=rap2
     decs[i,1]=decp2
     plots,[rap1,rap2],[decp1,decp2],thick=3
  endfor 

stop
return
end 
