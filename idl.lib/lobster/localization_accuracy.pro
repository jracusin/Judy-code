pro localization_accuracy

  c=findgen(2000)+1.
  exp=[50,100,500,1000,5000]
  b=0.35e-4*16.6^2.*exp

  begplot,name='~/iLobster/simulations/localization_accuracy.ps',/land,/color,font='helvetica'
  xtitle='Source Counts in Spot'
  ytitle='Err!L90!N (arcsec)'
  plot,[1,1000],[1,1000],/nodata,/xlog,/ylog,xrange=[5,2000],yrange=[5,200],/xsty,/ysty,xtitle=xtitle,ytitle=ytitle
  color=[!blue,!red,!green,!orange,!purple]
  for i=0,4 do begin 
     wa=where(c/b[i] lt 1.43,nwa)
     wb=where(c/b[i] ge 1.43,nwb)
     r90a=268.7*c^(-0.5)+82.7*(c/b[i])^(-0.554d)
     r90b=268.7*c^(-0.5)+104.7*(c/b[i])^(-1.215d)
     r90=r90b
     if nwa gt 0 then r90[wa]=r90a[wa]
     oplot,c,r90,color=color[i],thick=5
  endfor 
  oplot,c,268.7*c^(-0.5),line=2

  readcol,'~/iLobster/simulations/Phil_r90.dat',x,y
  oplot,x,y,psym=1

  legend,['Exposure:',ntostr(exp)+' s','B=0'],textcolor=[!p.color,color,!p.color],box=0,/top,/right,line=[-1,0,0,0,0,0,2],color=[!p.color,color,!p.color]
  endplot
  spawn,'ps2pdf ~/iLobster/simulations/localization_accuracy.ps ~/iLobster/simulations/localization_accuracy.pdf'

  return
end 
