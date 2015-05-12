@fit_functions
pro make_avg_canon_plot

  begplot,name='~/papers/decay_lum_corr/avg_canon.eps',/land,/encap,/color
;  !x.margin=[5,3]
  plot,[1e2,1e6],[0,1],/nodata,/xlog,/ylog,yrange=[1e-4,100],xrange=[1e2,1e6],/xstyle,ytickname=replicate(' ',7),color=!white

  xyouts,1.1e2,5e-4,'log Flux',orientation=90,/data,charsize=2
  arrow,1e2,8e-3,1e2,1e-1,/data,thick=10,hsize=!d.x_size/48.,/solid ;; flux
  xyouts,2e2,1.8e-4,'log Time',/data,charsize=2
  arrow,8e2,2e-4,3e3,2e-4,/data,thick=10,hsize=!d.x_size/48.,/solid ;; time
  
  p=[1,3.,5.e2,0.5,1e4,1.2,1e5,2.]
 
  t=logarr(10,1e6,bin=0.01)
;  t=10.^((indgen(100)+30.)/20.)
  w=where(t gt 100. and t lt 1e6)
  t=t[w]
  t=[t,[1e3,1e4,1e5]]*1d
  t=t[sort(t)]
  w1=where(t le p[2]+30)
  w2=where(t ge p[2] and t le p[4])
  w3=where(t ge p[4] and t le p[6])
  w4=where(t ge p[6])
  wf=where(t ge p[2]+200 and t le p[4]-7000)
  wa=where(t ge p[2])

  y0=bkn3pow(t,p)
  y=y0/max(y0)*100.
  yf0=gaussian(t,[10.,1700,300.])
  yf=yf0+y
  oplot,t[w1],y[w1],thick=8
  oplot,t[w2],y[w2],line=1,thick=8
  oplot,t[w3],y[w3],thick=8
  oplot,t[w4],y[w4],line=1,thick=8
  oplot,t[wf],yf[wf],line=2,thick=8
  
  y2=pow(t,[5e3,1.2])
  oplot,t[wa],y2[wa],line=3,color=!red,thick=8

;  plotsym,2,4,thick=4
  alpha=!tsym.alpha
  cs1=2.5
  cs2=2.
  cs3=1.5
  xyouts,2.5e2,15,'I',/data,charsize=cs1
  xyouts,1e2,5,alpha+'!LI!N~3',/data,charsize=cs2
  xyouts,2e2,0.3,'t!Lb1!N~10!U2!N-10!U3!N s',/data,charsize=cs3
;  plots,5e2,0.3,psym=8
  
  xyouts,3e3,0.5,'II',/data,charsize=cs1
  xyouts,1e3,0.15,alpha+'!LII!N~0.5',/data,charsize=cs2
  xyouts,5e3,0.5,'t!Lb2!N~10!U3!N-10!U4!N s',/data,charsize=cs3
;  plots,1e4,0.07,psym=8
  
  xyouts,3e4,0.12,'III',/data,charsize=cs1
  xyouts,1.4e4,0.01,alpha+'!LIII!N~1.2',/data,charsize=cs2
  xyouts,7e4,0.04,'t!Lb3!N~10!U4!N-10!U5!N s',/data,charsize=cs3
;  plots,1e5,0.005,psym=8
  
  xyouts,3e5,0.003,'IV',/data,charsize=cs1
  xyouts,1.5e5,0.0003,alpha+'!LIV!N~2',/data,charsize=cs2
  
  xyouts,1.4e3,15,'V',/data,charsize=cs1

  xyouts,1.3e6,2e-4,alpha+'!Lavg!N',/data,charsize=cs2,color=!red

  endplot

  spawn,'ps2pdf ~/papers/decay_lum_corr/avg_canon.eps ~/papers/decay_lum_corr/avg_canon.pdf'

stop
  return
end 

pro make_opt_canon_plot
  
  begplot,name='~/jetbreaks/opt_canon.eps',/encap;,/land
  plot,[1e2,1e6],[0,10],/nodata,/xlog,/ylog,yrange=[1e-6,1e6],xrange=[1e2,1e6],/xstyle,ytickname=replicate(' ',2),xtickname=replicate(' ',2),/ysty,xticks=1,yticks=1,yminor=1,xminor=1,color=!white

  tb=[500.,1e5]
  p=[1.,1.75,500.,1.2,1e5,2.]
  t=10.^((indgen(100)+30.)/20.)
  w=where(t gt 100. and t lt 1e6)
  t=t[w]
  t=[t,[tb]]*1d
  t=t[sort(t)]
  w1=where(t le p[2]+30)
  w2=where(t ge p[2] and t le p[4])
  w3=where(t ge p[4])
  
  y=bkn2pow(t,p)
  y=y/max(y)*100.
  oplot,t[w1],y[w1],thick=8,line=2
  oplot,t[w2],y[w2],thick=8
  oplot,t[w3],y[w3],thick=8,line=2

  alpha=!tsym.alpha
  cs1=2.
  cs2=1.5
  cs3=1.5
;  xyouts,2.e2,40,'RS',/data,charsize=cs1
  xyouts,70,7,alpha+'!LRS!N~1.75',/data,charsize=cs2
  xyouts,500,10,'t!Lb!N~10!U2!N-10!U3!N s',/data,charsize=cs3

;  xyouts,4e3,1,'FS',/data,charsize=cs1
  xyouts,3e3,0.1,alpha+'!LFS!N~1',/data,charsize=cs2
  xyouts,5e4,0.04,'t!LJB!N~10!U5!N-10!U6!N s',/data,charsize=cs3

;  xyouts,2e5,0.005,'post-JB',/data,charsize=cs1
  xyouts,1.1e5,0.0005,alpha+'!LJB!N~2',/data,charsize=cs2

;;;;;;;;;;;;;

  tb=[500.,1e5]
  p=[1.,-2,500.,1.2,1e5,2.]
  t=10.^((indgen(100)+30.)/20.)
  w=where(t gt 100. and t lt 1e6)
  t=t[w]
  t=[t,[tb]]*1d
  t=t[sort(t)]
  w1=where(t le p[2]+30)
  w2=where(t ge p[2] and t le p[4])
  w3=where(t ge p[4])
  
  y=bkn2pow(t,p)
  y=y/max(y)*0.01
  oplot,t[w1],y[w1],thick=8,line=2
  oplot,t[w2],y[w2],thick=8
  oplot,t[w3],y[w3],thick=8,line=2

  alpha=!tsym.alpha
;  xyouts,1.5e2,4e-3,'FS',/data,charsize=cs1
  xyouts,200,1e-3,alpha+'!LRS!N~1',/data,charsize=cs2
  xyouts,600,1e-2,'t!Lb!N~10!U2!N-10!U3!N s',/data,charsize=cs3

;  xyouts,4e3,1e-3,'FS',/data,charsize=cs1
  xyouts,3e3,1.5e-4,alpha+'!LFS!N~1',/data,charsize=cs2
  xyouts,5e4,5e-5,'t!LJB!N~10!U5!N-10!U6!N s',/data,charsize=cs3

;  xyouts,2e5,7e-6,'post-JB',/data,charsize=cs1
  xyouts,8e4,2e-6,alpha+'!LJB!N~2',/data,charsize=cs2

  endplot
stop
return
end 

pro make_canon_plot
  
  begplot,name='~/papers/jetbreaks1/canon.eps',/land,/encap
;  !x.margin=[5,3]
  plot,[1e2,1e6],[0,1],/nodata,/xlog,/ylog,yrange=[1e-4,100],xrange=[1e2,1e6],/xstyle,ytickname=replicate(' ',7),color=!white
  
  p=[1,3.,5.e2,0.5,1e4,1.2,1e5,2.]
;  x=reverse(alog10(indgen(10)+2))
;  x2=max(x)-x
;  x=reverse(alog10(indgen(100)+2))
;  x3=max(x)-x
;  t=[x2*1e2,100.+x2[0:8]*1e3,1000.,1000.+x3[0:98]*0.6e5,1.e5,1e5.+x3*
  
  t=10.^((indgen(100)+30.)/20.)
  w=where(t gt 100. and t lt 1e6)
  t=t[w]
  t=[t,[1e3,1e4,1e5]]*1d
  t=t[sort(t)]
  w1=where(t le p[2]+30)
  w2=where(t ge p[2] and t le p[4])
  w3=where(t ge p[4] and t le p[6])
  w4=where(t ge p[6])
  
  y=bkn3pow(t,p)
  y=y/max(y)*100.
  oplot,t[w1],y[w1],thick=8
  oplot,t[w2],y[w2],line=1,thick=8
  oplot,t[w3],y[w3],thick=8
  oplot,t[w4],y[w4],line=1,thick=8
  
;  plotsym,2,4,thick=4
  alpha=!tsym.alpha
  cs1=2.5
  cs2=2.
  cs3=1.5
  xyouts,2.5e2,15,'I',/data,charsize=cs1
  xyouts,1e2,5,alpha+'!LI!N~3',/data,charsize=cs2
  xyouts,2e2,0.5,'t!Lb1!N~10!U2!N-10!U3!N s',/data,charsize=cs3
;  plots,5e2,0.3,psym=8
  
  xyouts,2e3,1,'II',/data,charsize=cs1
  xyouts,1e3,0.2,alpha+'!LII!N~0.5',/data,charsize=cs2
  xyouts,5e3,0.5,'t!Lb2!N~10!U3!N-10!U4!N s',/data,charsize=cs3
;  plots,1e4,0.07,psym=8
  
  xyouts,3e4,0.12,'III',/data,charsize=cs1
  xyouts,1.4e4,0.02,alpha+'!LIII!N~1.2',/data,charsize=cs2
  xyouts,7e4,0.04,'t!Lb3!N~10!U4!N-10!U5!N s',/data,charsize=cs3
;  plots,1e5,0.005,psym=8
  
  xyouts,3e5,0.003,'IV',/data,charsize=cs1
  xyouts,1.5e5,0.0005,alpha+'!LIV!N~2',/data,charsize=cs2
  
  endplot
  
  ;;;what would have been
  begplot,name='~/papers/jetbreaks1/pre_swift_canon.eps',/land,/encap

  plot,[1e2,1e6],[0,1],/nodata,/xlog,/ylog,yrange=[1e-4,100],xrange=[1e2,1e6],/xstyle,ytickname=replicate(' ',7),color=!white
  p=[1,1.,1e5,2]
  t=10.^((indgen(100)+30.)/20.)
  w=where(t gt 100. and t lt 1e6)
  t=t[w]
  t=[t,[1e3,1e4,1e5]]*1d
  t=t[sort(t)]
  w1=where(t le p[2]+30)
  w2=where(t ge p[2])  
  y=bknpow(t,p)
  y=y/max(y)*100.
  oplot,t[w1],y[w1],thick=8
  oplot,t[w2],y[w2],line=1,thick=8  
  
;  xyouts,3e4,0.12,'III',/data,charsize=cs1
  xyouts,1e3,1,alpha+'~1',/data,charsize=cs2
  xyouts,7e4,0.4,'t!Lb!N~10!U5!N s',/data,charsize=cs3
  
;  xyouts,3e5,0.003,'IV',/data,charsize=cs1
  xyouts,1.5e5,0.006,alpha+'~2',/data,charsize=cs2
  
  endplot  
  stop
  return
end 
