pro grb120107a

  lra=246.267
  ldec=-69.822
  lerr=0.5 ;; deg
; Ra = 246.31, Dec = -69.93,
  xrange=[255,240]
  yrange=[-76,-68]
;  xrange=[360,180]
;  yrange=[-90,90]
  aplot,1,xrange,yrange,/nodata,xtitle='RA (deg)',ytitle='Dec (deg)',/xsty,/ysty,xrange=xrange,yrange=yrange
  
  ipnra=268.419 
  ipndec=-23.372
  ipnerr=48.611
  ipnerrerr=0.094 
  ipnra2=262.874
  ipndec2=-24.735
  ipnerr2=47.383 
  ipnerrerr2=1.667; deg (3 sigma). 
  
;  polyfill,ipnra,ipndec,color=!orange
  
  tvellipse,ipnerr/cos(ipndec*!dtor),ipnerr,ipnra,ipndec,0,/data,color=!orange
  tvellipse,(ipnerr+ipnerrerr)/cos(ipndec*!dtor),ipnerr+ipnerrerr,ipnra,ipndec,0,/data,color=!orange
  tvellipse,(ipnerr-ipnerrerr)/cos(ipndec*!dtor),ipnerr-ipnerrerr,ipnra,ipndec,0,/data,color=!orange
;  tvellipse,l2err68/cos(l2dec*!dtor),l2err68,l2ra,l2dec,0,/data

  tvellipse,ipnerr2/cos(ipndec2*!dtor),ipnerr2,ipnra2,ipndec2,0,/data,color=!blue,line=2
  tvellipse,(ipnerr2+ipnerrerr2)/cos(ipndec2*!dtor),ipnerr2+ipnerrerr2,ipnra2,ipndec2,0,/data,color=!blue
  tvellipse,(ipnerr2-ipnerrerr2)/cos(ipndec2*!dtor),ipnerr2-ipnerrerr2,ipnra2,ipndec2,0,/data,color=!blue


  tvellipse,lerr/cos(ldec*!dtor),lerr,lra,ldec,0,/data,color=!green

  return
end 
