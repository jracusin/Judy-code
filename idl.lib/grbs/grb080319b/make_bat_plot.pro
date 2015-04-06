pro make_bat_plot,ps=ps
  
  cd,'~/Desktop/GRB080319B/data_tables/'
  readcol,'306757_bat64ms_lc.txt',time,f1,f1err,f2,f2err,f3,f3err,f4,f4err,ftot,ftoterr
  
  
  erase
  if keyword_set(ps) then begplot,name='../BAT_lc.eps',font='helvetica',/land,/encaps,/cmyk
  multiplot2,[1,5],/init
  
  xrange=[-20,180]
  w1=where(time lt 120)
  w2=where(time gt 120)

  multiplot2
;  ploterror,time[w1],f1[w1],f1err[w1],xrange=xrange,yrange=[0,8],/nohat,/xsty
;  oploterror,time[w2],f1[w2],f1err[w2],/nohat
  plot,time[w1],f1[w1],xrange=xrange,yrange=[0,8],/xsty,thick=2,xmargin=[10,0]
  oplot,time[w2],f1[w2],thick=2
  legend,['15-25 keV'],/top,/right,box=0
  
  multiplot2
;  ploterror,time[w1],f2[w1],f2err[w1],xrange=xrange,yrange=[0,10],/nohat,/xsty
;  oploterror,time[w2],f2[w2],f2err[w2],/nohat
  plot,time[w1],f2[w1],xrange=xrange,yrange=[0,10],/xsty,thick=2
  oplot,time[w2],f2[w2],thick=2
  legend,['25-50 keV'],/top,/right,box=0
  
  multiplot2
;  ploterror,time[w1],f3[w1],f3err[w1],xrange=xrange,yrange=[0,10],/nohat,ytitle='Counts/64ms/det',/xsty
;  oploterror,time[w2],f3[w2],f3err[w2],/nohat
  plot,time[w1],f3[w1],xrange=xrange,yrange=[0,10],/xsty,ytitle='BAT Count Rate (Counts/64ms/det)',thick=2
  oplot,time[w2],f3[w2],thick=2
  legend,['50-100 keV'],/top,/right,box=0
  
  multiplot2
;  ploterror,time[w1],f4[w1],f4err[w1],xrange=xrange,yrange=[0,3],/nohat,/xsty
;  oploterror,time[w2],f4[w2],f4err[w2],/nohat
  plot,time[w1],f4[w1],xrange=xrange,yrange=[0,3],/xsty,thick=2
  oplot,time[w2],f4[w2],thick=2
  legend,['100-350 keV'],/top,/right,box=0
  
  multiplot2
;  ploterror,time[w1],ftot[w1],ftoterr[w1],xrange=xrange,yrange=[0,25],/nohat,xtitle='Time since BAT trigger (s)',/xsty
;  oploterror,time[w2],ftot[w2],ftoterr[w2],/nohat
  plot,time[w1],ftot[w1],xrange=xrange,yrange=[0,25],/xsty,xtitle='Time since BAT trigger (s)',thick=2
  oplot,time[w2],ftot[w2],thick=2
  legend,['15-350 keV'],/top,/right,box=0
  multiplot2,/reset,/default
  
  if keyword_set(ps) then endplot
  
  stop
  return
end 
