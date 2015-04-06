pro square,xc,yc,rx,ry,x,y

  rxc=rx/cos(yc*!dtor)
  x=[xc-rxc,xc+rxc,xc+rxc,xc-rxc,xc-rxc]
  y=[yc-ry,yc-ry,yc+ry,yc+ry,yc-ry]

  return
end 
pro grb110721a

  l1ra=333.433
  l1dec=-38.423
  l1err68=0.5522
  l1err90=0.8558
  l1err95=0.9736
  l1err99=1.1252

  l2ra=333.369
  l2dec=-38.973
  l2err68=0.5115
  l2err90=0.7465
  l2err95=0.8670
  l2err99=1.1269

  gra=331.42
  gdec=-36.42 
  gerr=1.0+3.

  x1ra=333.433	
  x1dec=-38.423
  xerr=23.5/60./2.
  yerr=23.5/60./2.

  x2ra=333.369
  x2dec=-38.973

  x3ra=334.103
  x3dec=-38.973
  x4ra=333.736
  x4dec=-38.479
  x5ra=333.1546	
  x5dec=-38.6843

  x6ra=332.9402
  x6dec=-38.973

  x7ra=333.1546 
  x7dec=-39.2617
  x8ra=333.5834 
  x8dec=-39.2617

;  ipnra=[333.18561,333.56592,333.86798,331.52139,331.21906,330.72327,333.18576]
;  ipndec=[-41.50952,-39.22529,-36.23254,-36.00094,-38.83620,-41.08130,-41.15883]

  ipnra=[332.01,333.43,333.67,332.01,332.01]
  ipndec=[-40.49,-40.49,-38.01,-38.01,-40.49]

  ipnra2=[333.603,333.487,333.565]
  ipndec2=[-38.2813,-39.7266,-39.0625]
 
  ipnra2=[333.65,333.5]
  ipndec2=[-38.28,-39.75]

;20166	333.433	-38.423		Yes				0
;20167	333.369	-38.973		Yes				2000s
;20168	334.103	-38.973		Yes				unknown due to STNOK, probably 1-1.5 ks
;20169	333.736	-38.479		Scheduled for 19:57 pass
;20170	333.1546 -38.6843	Schedule for 23:22 pass
;20171	332.9402 -38.9730	Scheduled for 02:49 pass

;20170	333.002	-38.479		Scheduled for 23:22 pass
;20171	332.635	-38.973		Scheduled for 02:49 pass
;20172	333.002	-39.467		Scheduled for 09:44 pass
;20173	333.736	-39.467		Scheduled for 12:37 pass

;20172 6  333.1546 -39.2617  22 12 37.1 -39 15 42.0
;20173 7  333.5834 -39.2617  22 14 20.0 -39 15 42.0

;09:44, 12:37 and 13:08
;;29, 32, 33.5

  begplot,name='~/Desktop/GRB110721A/GRB110721A_pointings.ps',/land,/color
  plot,[332,336],[-40.5,-38],/nodata,xtitle='RA (deg)',ytitle='Dec (deg)',/xsty,/ysty

  polyfill,ipnra,ipndec,color=!orange
;  oplot,ipnra2,ipndec2,color=!magenta,psym=2

;  tvellipse,l1err68/cos(l1dec*!dtor),l1err68,l1ra,l1dec,0,/data
;  tvellipse,l1err90/cos(l1dec*!dtor),l1err90,l1ra,l1dec,0,/data,line=2
;  tvellipse,l1err95/cos(l1dec*!dtor),l1err95,l1ra,l1dec,0,/data,line=1

;  tvcircle,l2err68,l2ra,l2dec,/data,color=!green
;  tvcircle,l2err90,l2ra,l2dec,/data,color=!green,line=2
;  tvcircle,l2err95,l2ra,l2dec,/data,color=!green,line=1
  tvellipse,l2err68/cos(l2dec*!dtor),l2err68,l2ra,l2dec,0,/data
  tvellipse,l2err90/cos(l2dec*!dtor),l2err90,l2ra,l2dec,0,/data,line=2
  tvellipse,l2err95/cos(l2dec*!dtor),l2err95,l2ra,l2dec,0,/data,line=1
;  tvcircle,l2err99,l2ra,l2dec,/data,color=!green

;  tvcircle,gerr,gra,gdec,/data,color=!red
  tvellipse,gerr/cos(gdec*!dtor),gerr,gra,gdec,/data,color=!red

;  tvcircle,x1err,x1ra,x1dec,/data,color=!blue
;  tvcircle,x2err,x2ra,x2dec,/data,color=!blue
;  square,x1ra,x1dec,xerr,yerr,x1,y1
;  oplot,x1,y1,color=!blue
;  xyouts,x1ra-0.1,x1dec-0.1,'20166',color=!blue,charsize=1.

;  square,x2ra,x2dec,xerr,yerr,x2,y2
;  oplot,x2,y2,color=!blue
  tvellipse,xerr/cos(x2dec*!dtor),yerr,x2ra,x2dec,color=!blue,/data
  xyouts,x2ra-0.1,x2dec-0.1,'20167',color=!blue,charsize=1.

;  square,x3ra,x3dec,xerr,yerr,x3,y3
;  oplot,x3,y3,color=!blue
  tvellipse,xerr/cos(x3dec*!dtor),yerr,x3ra,x3dec,color=!blue,/data
  xyouts,x3ra-0.1,x3dec-0.1,'20168',color=!blue,charsize=1.

;  square,x4ra,x4dec,xerr,yerr,x4,y4
;  oplot,x4,y4,color=!blue
  tvellipse,xerr/cos(x4dec*!dtor),yerr,x4ra,x4dec,color=!blue,/data
  xyouts,x4ra-0.1,x4dec-0.1,'20169',color=!blue,charsize=1.

;  square,x5ra,x5dec,xerr,yerr,x5,y5
;  oplot,x5,y5,color=!blue
  tvellipse,xerr/cos(x5dec*!dtor),yerr,x5ra,x5dec,color=!blue,/data
  xyouts,x5ra-0.1,x5dec-0.1,'20170',color=!blue,charsize=1.

;  square,x6ra,x6dec,xerr,yerr,x6,y6
;  polyfill,x6,y6,color=!yellow
;  oplot,x6,y6,color=!blue
  tvellipse,xerr/cos(x6dec*!dtor),yerr,x6ra,x6dec,color=!blue,/data
  xyouts,x6ra-0.1,x6dec-0.1,'20171',color=!blue,charsize=1.

;  square,x7ra,x7dec,xerr,yerr,x7,y7
;  polyfill,x7,y7,color=!yellow
;  oplot,x7,y7,color=!blue
  tvellipse,xerr/cos(x7dec*!dtor),yerr,x7ra,x7dec,color=!blue,/data
  xyouts,x7ra-0.1,x7dec-0.1,'20172',color=!blue,charsize=1.

  ;;; not gonna do
;  square,x8ra,x8dec,xerr,yerr,x8,y8
;  polyfill,x8,y8,color=!yellow
;  oplot,x8,y8,color=!blue
;  xyouts,x8ra-0.1,x8dec-0.1,'20173',color=!blue,charsize=1.

  ;;;CRAPPY ROSAT SOURCE
;  bra=333.930698947
;  bdec=-39.0432328643

  ;;;AFTERGLOW?????
  bra=333.65946
  bdec=-38.593417
  plots,bra,bdec,psym=2,color=!red

  legend,['GBM (stat+sys)','LAT pos','XRT Pointings','IPN'],box=0,textcolor=[!red,!p.color,!blue,!orange],/top,/right
  legend,['68%','90%','95%'],box=0,/bottom,/right,line=[0,2,1]

  endplot
stop
return
end 
