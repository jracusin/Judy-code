pro lat_tmp

 ra=266.69
 dec=-28.982
 gra=266.416833                      
 gdec=-29.007806
 grad=20./60.
 grad2=grad/2.
 xpt=23.5/60.
 xpt2=xpt/2.

 ora=267.18
 odec=-29.22

 plot,[266,267],[-29.5,-28.5],/nodata,/iso

 plots,ra,dec,psym=2
 plots,gra,gdec,psym=4
 plots,ora,odec,psym=5

 oplot,[gra-grad2,gra+grad2,gra+grad2,gra-grad2,gra-grad2],[gdec-grad2,gdec-grad2,gdec+grad2,gdec+grad2,gdec-grad2]

 oplot,[gra-xpt2,gra+xpt2,gra+xpt2,gra-xpt2,gra-xpt2],[gdec-xpt2,gdec-xpt2,gdec+xpt2,gdec+xpt2,gdec-xpt2],color=!orange

stop
return
end 
 
