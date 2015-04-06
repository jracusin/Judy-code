pro make_fig3
  
  !x.margin=[4,2]
  !y.margin=[10,10]
  cs=2

  dir='~/Desktop/GRB080319B/Valentin/'
  begplot,name='racusin_fig3_band.eps',font='helvetica',/land,/encaps,/color,/cmyk
;  plot,[1e-3,1e4],[1e-2,1e5],/nodata,xtitle='Energy (keV)',ytitle='keV / cm!U2!N s keV',/xlog,/ylog,/xstyle,/ystyle,xtickf='loglabels',ytickf='loglabels',xminor=9,yminor=9
  plot,[1e-3,1e4],[1e-2,1e2],/nodata,xtitle='Energy (keV)',/xlog,/ylog,/xstyle,/ystyle,xticks=1,yticks=1,xticklen=0.001,xtickname=[' ',' '],yticklen=0.001,ytickname=[' ',' '],xmargin=[15,10],ymargin=[4,3],charsize=cs
  
  colors=[!green,!blue,!red]
;  colors=[!p.color,!p.color,!p.color];!grey,!grey50]
  plotsym,0,1,/fill
;  psym=[1,5,8]
  psym=[8,8,8]
  
  for i=0,1 do begin
     kwfile='GRB080319B_KW_sp_Int'+ntostr(i+1)+'.dat'
     mofile='GRB080319B_Model_sp_Int'+ntostr(i+1)+'.dat'
     pifile='GRB080319B_PI_Int'+ntostr(i+1)+'.dat'
  
     readcol,dir+kwfile,en,enerr,flux,fluxerr,format='(f,f,f,f)'
     oploterror,en,flux,enerr,fluxerr,psym=3,errcolor=colors[i],color=colors[i],/nohat
     readcol,dir+mofile,en,flux,format='(f,f)'
     oplot,en,flux,color=colors[i];,line=i
     readcol,dir+pifile,en,flux,fluxerr,format='(f,f,f)'
;     oploterror,en,flux,fluxerr,psym=psym[i],color=colors[i],errcolor=colors[i],/nohat
  
  
  endfor 
  
;  1mJy -> 1.509 keV/[cm^2 s keV]
;  1keV -> 1.418e17 Hz
  
  ;;;bottom x axis
  xt=indgen(8)-3
  xtv=10^(xt*1d)
  axis,1,xaxis=0,xtickname='10!U'+ntostr(xt),xticks=7,/data,xminor=9,xtickv=xtv,xtickf='loglabels',charsize=cs
  
  ;;;top x axis
  xt=indgen(7)+15
  xtv=10^(xt*1d)/1.418d17
  axis,xaxis=1,xtickname='10!U'+ntostr(xt),xticks=6,xtitle=!tsym.nu+',Hz',xminor=9,/data,xtickv=xtv,charsize=cs
  
  ;;;left y axis
  yt=indgen(8)-2
  ytv=10^(yt*1d)
  axis,1e-3,yaxis=0,ytickname='10!U'+ntostr(yt),yticks=7,yminor=9,/data,ytickv=ytv,ytitle='keV / cm!U2!N s keV',ytickf='loglabels',charsize=cs
  
  ;;;right y axis
  yt=indgen(7)-2
  ytv=10^(yt*1d)*1.509  ;;;SHOULD BE MULTIPLIED?
  axis,yaxis=1,ytickname='10!U'+ntostr(yt),yticks=6,yminor=9,/data,ytickv=ytv,ytitle='mJy',ytickf='loglabels',charsize=cs
  
  
;  legend,['Interval 1','Interval 2','Interval 3'],box=0,color=colors,/top,/right,line=[0,1,2]
;  legend,['Interval 1','Interval 2','Interval 3'],box=0,color=colors,line=[0,1,2],position=[0.57,0.875],/norm
;  legend,psym=[1,5,8],/top,/right,box=0
  
  endplot
;  spawn,'convert -rotate 270 racusin_fig3.ps racusin_fig3.jpg'
  
  stop
  return
end 
