pro grb060904b_compare_plots,newra,newdec,merr

  mygcnra=58.2094166667
  mygcndec=-0.725694444444
  mygcnerr=3.6
  
  tarotra=58.2109166667
  tarotdec=-0.724833333333
  taroterr=2.0
  
  uvotra=58.2106
  uvotdec=-0.7253
  uvoterr=0.7
  
  bootesra=58.2105833333
  bootesdec=-0.725138888889
  booteserr=0.5
  
  crnira=58.2105
  crnidec=-0.72525
  crnierr=0.2
  
  ras=[mygcnra,tarotra,uvotra,bootesra,crnira]
  decs=[mygcndec,tarotdec,uvotdec,bootesdec,crnidec]
  err=[mygcnerr,taroterr,uvoterr,booteserr,crnierr]
  name=['XRT refined','TAROT','UVOT','Bootes','Crni vrh']
  
  dras=(ras-mygcnra)*3600./cos(decs*!dtor)
  ddecs=(decs-mygcndec)*3600.
  
  off=5.
  plot,[min(dras)-off,max(dras)+off],[min(ddecs)-off,max(ddecs)+off],psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA offset (")',ytitle='Dec offset (")'
  
  color=[!green,!blue,!red,!orange,!yellow,!cyan,!magenta]
  
  for i=0,n_elements(ras)-1 do begin
     tvcircle,err[i],dras[i],ddecs[i],color=color[i],/data
     xyouts,dras[i]-err[i]/2.,ddecs[i]+err[i]*1.1,name[i],color=color[i],/data;,charsize=0.5
  endfor 
     
;  plots,dras[2],ddecs[2],color=!blue,psym=2
;  xyouts,dras[2]-0.05,ddecs[2]+0.2,'p60',color=!blue,/data,charsize=0.5
    
  if n_params() gt 0 then begin
     mra=(newra-mygcnra)*3600./cos(newdec*!dtor)
     mdec=(newdec-mygcndec)*3600.
     
     tvcircle,merr,mra,mdec,color=!purple,/data
     xyouts,mra-merr/2.,mdec+merr+0.2,'New position',color=!purple,/data;,charsize=0.5
     print,'offset between New & UVOT: '+ntostr(separation(newra,newdec,uvotra,uvotdec))
     print,'offset between New and XRT refined: '+ntostr(separation(newra,newdec,mygcnra,mygcndec))
  endif 
  
  
end 
  
  
