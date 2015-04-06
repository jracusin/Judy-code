@fit_functions
pro sari_plot

  ;;; remaking plot from Sari et al. 1998, but converting to energy
  ;;; and adding instrument ranges
  eng=[(dindgen(9)+1.)*1e-10,(dindgen(9)+1.)*1e-9,(dindgen(9)+1.)*1e-8,(dindgen(9)+1.)*1e-7,(dindgen(9)+1.)*1e-6,(dindgen(9)+1.)*1e-5,(dindgen(9)+1.)*1e-4,(dindgen(9)+1.)*1e-3,(dindgen(9)+1.)*1e-2,(dindgen(9)+1.)/10.,(dindgen(99)+1.),(dindgen(9)+1)*1e2,(dindgen(9)+1)*1e3,(dindgen(9)+1.)*1e4,(dindgen(9)+1.)*1e5,(dindgen(9)+1.)*1e6,(dindgen(10)+1.)*1e7,(dindgen(10)+1.)*1e8]

  h=4.135e-15/1e3 ;; kev*s
  freq=eng/h
  
  pp=2.2
  nua=5e9
  num=1e13
  nuc=3e16
  p=[1e-15,-2,nua,-1/3.,num,(pp-1.)/2.,nuc,pp/2.]
  f=bkn3pow(freq,p)

  y=[1e-8,1e6]
  begplot,name='~/Swift/afterglow_schematic.eps',/encap,/color,/land,font='helvetica'

  !p.charsize=2
  !y.margin=[4,0]
  !x.margin=[5,2]
  plot,eng,f,/xlog,/ylog,ytitle='Flux',xtitle='Energy (keV)',yrange=y,/ysty,xtickformat='loglabels',ytickformat='loglabels'

  x=[5.55e14,1.48e15]*h ;; UVOT
  polyfill,[x[0],x[1],x[1],x[0],x[0]],[y[0],y[0],y[1],y[1],y[0]],color=!red,/trans
  x=[0.3,10.] ;; XRT
  polyfill,[x[0],x[1],x[1],x[0],x[0]],[y[0],y[0],y[1],y[1],y[0]],color=!blue,/trans
  x=[100e3,300e6] ;; LAT
  polyfill,[x[0],x[1],x[1],x[0],x[0]],[y[0],y[0],y[1],y[1],y[0]],color=!magenta,/trans

  axis,xaxis=0,/xlog,/ylog,yrange=y,/ysty,xtickname=replicate(' ',5)
  axis,yaxis=0,/xlog,/ylog,yrange=y,/ysty,ytickname=replicate(' ',8)
  axis,xaxis=1,/xlog,/ylog,yrange=y,/ysty,xtickname=replicate(' ',5)
  axis,yaxis=1,/xlog,/ylog,yrange=y,/ysty,ytickname=replicate(' ',8)

  xyouts,1e-4,1,'UVOT',charsize=3
  xyouts,0.1,1e-2,'XRT',charsize=3
  xyouts,5e5,1e-2,'LAT',charsize=3

  oplot,[nua,nua]*h,[y[0],2e4],line=2
  xyouts,nua*h*0.1,1e-7,!tsym.nu+'!Lsa!N'
  oplot,[num,num]*h,[y[0],3.5e5],line=2
  xyouts,num*h*0.15,1e-7,!tsym.nu+'!Lm!N'
  oplot,[nuc,nuc]*h,[y[0],3e3],line=2
  xyouts,nuc*h*0.2,1e-7,!tsym.nu+'!Lc!N'

  xyouts,4e-10,5e2,!tsym.nu+'!U2!N'
  xyouts,1e-7,1e5,!tsym.nu+'!U1/3!N'
  xyouts,1e-3,1e5,!tsym.nu+'!U(1-p)/2!N'
  xyouts,1e2,5,!tsym.nu+'!U-p/2!N'

  oplot,eng,f,thick=10

  arrow,nua*h,1e-6,nua*h*10,1e-6,/data,/solid,thick=10,hthick=1
  arrow,nua*h,5e-6,nua*h*0.1,5e-6,/data,/solid,thick=10,hthick=1
  arrow,num*h,1e-6,num*h*10,1e-6,/data,/solid,thick=10,hthick=1
  arrow,num*h,5e-6,num*h*0.1,5e-6,/data,/solid,thick=10,hthick=1
  arrow,nuc*h,1e-6,nuc*h*10,1e-6,/data,/solid,thick=10,hthick=1
  arrow,nuc*h,5e-6,nuc*h*0.1,5e-6,/data,/solid,thick=10,hthick=1

  endplot
  spawn,'ps2pdf ~/Swift/afterglow_schematic.eps ~/Swift/afterglow_schematic.pdf'
  
  stop
return
end 
