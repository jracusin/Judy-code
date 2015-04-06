pro grb060906_compare_plots,newra,newdec,merr
  
  initgcnra=40.75375d
  initgcndec=30.3608194444d
  initgcnerr=8.
  
  kaitra=40.7535d
  kaitdec=30.3616388889d
  kaiterr=0.2
  
  p60ra=40.75375d
  p60dec=30.3617d
  
  mygcnra=40.752733901d
  mygcndec=30.3604988247d
  mygcnerr=3.64d
  
  ras=[mygcnra,kaitra,p60ra,initgcnra]
  decs=[mygcndec,kaitdec,p60dec,initgcndec]
  
  dras=(ras-mygcnra)*3600./cos(decs*!dtor)
  ddecs=(decs-mygcndec)*3600.
  
  plot,[min(dras)-10,max(dras)+10],[min(ddecs)-10,max(ddecs)+10],psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA offset (")',ytitle='Dec offset (")'
  
  tvcircle,mygcnerr,dras[0],ddecs[0],color=!green,/data
  xyouts,dras[0]-1,ddecs[0]+4,'XRT refined',color=!green,/data,charsize=0.5
  
  tvcircle,initgcnerr,dras[3],ddecs[3],color=!yellow,/data
  xyouts,dras[3]-1,ddecs[3]+4,'Initial XRT position',color=!yellow,/data,charsize=0.5
  
  tvcircle,kaiterr,dras[1],ddecs[1],color=!red,/data
  xyouts,dras[1]-1,ddecs[1]+4,'KAIT position',color=!red,/data,charsize=0.5
  
  plots,dras[2],ddecs[2],color=!blue,psym=2
  xyouts,dras[2]-0.05,ddecs[2]+0.2,'p60',color=!blue,/data,charsize=0.5
    
  if n_params() gt 0 then begin
     mra=(newra-mygcnra)*3600./cos(newdec*!dtor)
     mdec=(newdec-mygcndec)*3600.
     
     tvcircle,merr,mra,mdec,color=!purple,/data
     xyouts,mra-1,mdec+merr+0.2,'New position',color=!purple,/data,charsize=0.5
     print,'offset between New & KAIT: '+ntostr(separation(newra,newdec,kaitra,kaitdec))
     print,'offset between New and XRT refined: '+ntostr(separation(newra,newdec,mygcnra,mygcndec))
  endif 
  
  
end 
  
  
