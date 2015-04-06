@fit_functions
pro twocomp_schematic,ps=ps
  
  ;;;; SCHEMATIC PLOT OF POSSIBLE TWO-COMPONENT JET X-RAY MORPHOLOGIES
  ;;;; IF PARTS OF BOTH NARROW AND WIDE JET ARE SEEN
  
  
  if keyword_set(ps) then begplot,name='~/thesis/chapters/twocomp/twocomp_schematic.eps',/color,/encap,font='helvetica' else erase
  !p.background=!white
  !p.color=!black
  leg=['a','b','c','d','e']
;  !p.multi=[0,1,5]
  multiplot2,[1,5],/init
;  yrange=[1e-6,1e2]
  x=dindgen(10)+1
  t=[x*100,x*1000,x*1e4,x*1e5]*1d
  ;; 0-I-II-III
  p=[1e3,1.,1e3,2.,100.,1.,1e5,2.]
  y0=double_bknpow(t,p,f10,f20)
  multiplot2
  plot,t,y0,/xlog,/ylog,xrange=[1e1,1e7],/xsty,ytickname=replicate(' ',7),yrange=[1e-6,1e2],/ysty
  polyfill,[1e2,1e6,1e6,1e2],[1e-6,1e-6,1e2,1e2],transp=0.1,color=!grey90
  oplot,t,y0
  axis,xaxis=1,xticks=6,xminor=9,xtickname=replicate(' ',7)
  axis,xticks=6,xminor=9,charsize=1.
  oplot,t,f10,color=!red
  oplot,t,f20,color=!blue
  legend,[leg[0]],/top,/right,box=0
  
  ;; I-II-III a
  p=[1e6,2.,1e2,1.,1e5,2.]
  y=pow_bknpow(t,p,f1,f2)
  multiplot2
  w=where(t ge 1e3)
  w0=where(t le 1e3)
  plot,t[w],y[w],/xlog,/ylog,xrange=[1e1,1e7],/xsty,ytickname=replicate(' ',7),yminor=0,yrange=[1e-6,1e2],/ysty
  polyfill,[1e3,1e6,1e6,1e3],[1e-6,1e-6,1e2,1e2],transp=0.1,color=!grey90
  oplot,t[w],y[w]
  axis,xaxis=1,xticks=6,xminor=9,xtickname=replicate(' ',7)
  axis,xticks=6,xminor=9,charsize=1.
  oplot,t[w],f1[w],color=!red
  oplot,t[w],f2[w],color=!blue
  oplot,t[w0],f10[w0],line=1,color=!red
  legend,[leg[1]],/top,/right,box=0
  
  ;; I-II-III b
  p=[1e3,1.25,1e2,1.,1e5,2.]
  y=pow_bknpow(t,p,f1,f2)
  multiplot2
  w=where(t le 1e4)
  w0=where(t ge 1e4)
  plot,t,y,/xlog,/ylog,xrange=[1e1,1e7],/xsty,ytickname=replicate(' ',7),yminor=0,ytitle='X-ray Count Rate',yrange=[1e-6,1e2],/ysty
  polyfill,[1e2,1e6,1e6,1e2],[1e-6,1e-6,1e2,1e2],transp=0.1,color=!grey90
  oplot,t,y
  axis,xaxis=1,xticks=6,xminor=9,xtickname=replicate(' ',7)
  axis,xticks=6,xminor=9,charsize=1.
  oplot,t[w],f1[w],color=!red
  oplot,t,f2,color=!blue
  oplot,t[w0],f10[w0],line=1,color=!red
  legend,[leg[2]],/top,/right,box=0
  
  ;; 0-I-II
  p=[100.,1.,1e3,1.,1e3,2.]
  y=pow_bknpow(t,p,f1,f2)
  multiplot2
  w=where(t le 1e5)
  w0=where(t ge 1e5)
  plot,t[w],y[w],/xlog,/ylog,xrange=[1e1,1e7],/xsty,yrange=[1e-6,1e2],/ysty,ytickname=replicate(' ',8),yminor=0
  polyfill,[1e2,1e5,1e5,1e2],[1e-6,1e-6,1e2,1e2],transp=0.1,color=!grey90
  oplot,t[w],y[w]
  axis,xaxis=1,xticks=6,xminor=9,xtickname=replicate(' ',7)
  axis,xticks=6,xminor=9,charsize=1.
  oplot,t[w],f1[w],color=!blue
  oplot,t[w],f2[w],color=!red
  oplot,t[w0],f20[w0],line=1,color=!blue
  legend,[leg[3]],/top,/right,box=0
  
  ;; I-II
  p=[1e6,2.,1e2,1.]
  y=pow_pow(t,p,f1,f2)
  multiplot2
  w=where(t ge 1e3 and t le 1e5)
  w0=where(t le 1e3)
  w1=where(t ge 1e5)
  plot,t[w],y[w],/xlog,/ylog,xrange=[1e1,1e7],/xsty,yrange=[1e-6,1e2],/ysty,ytickname=replicate(' ',8),xtitle='Time (s)',yminor=0
  polyfill,[1e3,1e5,1e5,1e3],[1e-6,1e-6,1e2,1e2],transp=0.1,color=!grey90
  oplot,t[w],y[w]
  axis,xaxis=1,xticks=6,xminor=9,xtickname=replicate(' ',7)
  axis,xticks=6,xminor=9,charsize=1.,xtickname=replicate(' ',7)
  oplot,t[w],f1[w],color=!red
  oplot,t[w],f2[w],color=!blue
  oplot,t[w0],f10[w0],line=1,color=!red
  oplot,t[w1],f20[w1],line=1,color=!blue
  legend,[leg[4]],/top,/right,box=0
  
  multiplot2,/reset,/default
  
  !p.multi=0
  if keyword_set(ps) then endplot
  
  stop
  return
end 
