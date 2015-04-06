@fit_functions
pro thesis_figs
  
  goto,crap
  ;;;; reproducing jet illustration from Ghisellini et al. 2001
  
  plot,[0,10],[0,10],/nodata,xticks=2,yticks=2,xtickname=[' ',' '],ytickname=[' ',' ']
  
  rad=0.3
  xc=1
  yc=8
  ang=20.*!dtor
  rad2=rad+1.
  tvcircle,rad,xc,yc,/data
  oplot,[xc,xc+rad2],[yc,yc+rad2*sin(ang)]
  oplot,[xc,xc+rad2+0.1],[yc,yc]
  oplot,[xc,xc+rad2],[yc,yc-rad2*sin(ang)]
  
  rad=1.
  xc=4.
  yc=8.
  ang=45*!dtor
  rad2=rad+1.
  tvcircle,rad,xc,yc,/data
  oplot,[xc,xc+rad2],[yc,yc+rad2*sin(ang)]
  oplot,[xc,xc+rad2+0.1],[yc,yc]
  oplot,[xc,xc+rad2],[yc,yc-rad2*sin(ang)]
  theta=(indgen(45))*!dtor
  oplot,xc+rad/cos(theta),yc+rad*sin(theta),color=!green
  
  ;;;NO
  
  begplot,name='~/thesis/chapters/intro/pre_swift_lc.eps',/encap,/land
  plot,[10,1000],[1e-2,100],/nodata,/xlog,/ylog,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '],xminor=1,yminor=1,/xsty,/ysty,color=!white
  
  t=(findgen(100)+1.)*3.
  t=t[5:*]
  w1=where(t lt 100.)
  w2=where(t gt 100.)
  tt=[t[w1],100.,t[w2]]
  oplot,tt,bknpow(tt,[100.,1.,100.,2.])
  xyouts,40,3,'~t!U-1!N',/data,charsize=2
  xyouts,200,0.3,'~t!U-2!N',/data,charsize=2
  oplot,[100,100],[1e-1,10],line=2
  xyouts,120,0.1,'t!Lj!N',/data,charsize=2
  endplot
  
  crap:

  ;;chapter 6 figure about probability of detecting narrow jets given range of narrow jet angles
  
  x=findgen(100)/50.
  x=x+x[1]
  begplot,name='~/thesis/chapters/twocomp/nj_theta_calc.eps',/encap,font='helvetica'
  y1=(x/5.4)^2*100.
  y2=(x/3.4)^2*100.
  y3=(x/6.6)^2*100.
  aplot,1,x,y1,xtitle='Narrow Jet Half-Opening Angle',ytitle='# detected per year',yrange=[0,20]
  w=where(y2 lt 100)
  w2=where(y2 lt 100 and x lt 1.53)
  polyfill,[x,2.,reverse(x[w2])],[y3,20.,reverse(y2[w2])],color=!grey80
;  oplot,x[w2],y2[w2],line=2
;  oplot,x,y3,line=2

  oplot,x,y1
  axis,xaxis=0,xtickname=replicate(' ',6)
  axis,yaxis=0,ytickname=replicate(' ',6)
  axis,xaxis=1,xtickname=replicate(' ',6)
  axis,yaxis=1,ytickname=replicate(' ',6)
  endplot
  
  stop
end
