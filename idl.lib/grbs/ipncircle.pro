pro grb141028a

  ira=16.52
  idec=5.123
  irad=54.469
  ierr=0.2

  lra=322.688
  ldec=-0.286
  lerr=0.4

  xra1=322.40419
  xdec1=0.08014
  xerr1=8.7/3600.

  xra2=322.60078
  xdec2=-0.23116
  xerr2=5.8/3600.

  hms2radec,21,30,24.51,-00,13,52.3,mra,mdec
  merr=0.02
  ipncircle,ira,idec,irad,ierr,lra,ldec,lerr,xrange=[323.5,322],yrange=[-0.7,0.3]
  bigcirc,mra,mdec,merr,x,y
  oplot,x,y,color=!orange
  bigcirc,xra1,xdec1,xerr1,x,y
  oplot,x,y,color=!yellow
  bigcirc,xra2,xdec2,xerr2,x,y
  oplot,x,y,color=!magenta

  print,separation(mra,mdec,xra2,xdec2)

  return
end 

pro grb130821a

  ira=297.840
  idec=-21.89
  irad=18.673
  ierr=0.049

  lra2=314.1
  ldec2=-12.0

  lra3=314.254
  ldec3=-11.6862

  lra=314.253
  ldec=-11.65
  lerr=0.09

  begplot,name='~/Desktop/GRB130821A/grb130821a_positions.ps',/land,/color,font='helvetica'
  ipncircle,ira,idec,irad,ierr,lra,ldec,lerr,xrange=[315.5,313],yrange=[-12.7,-11]
  bigcirc,lra2,ldec2,lerr,x,y
  oplot,x,y,color=!orange
  bigcirc,lra3,ldec3,lerr,x,y
  oplot,x,y,color=!cyan

  xyouts,315,-12.5,'IPN',color=!red
  xyouts,314.3,-12.2,'LAT circular',color=!orange
  xyouts,314.5,-11.5,"Nicola's new LAT position",color=!green
  xyouts,314.1,-11.7,"Judy's gtbursts LAT position",color=!cyan
  endplot
  spawn,'convert ~/Desktop/GRB130821A/grb130821a_positions.ps ~/Desktop/GRB130821A/grb130821a_positions.pdf'

end

pro grb120709a

  ira=268.324d
  idec=-21.05d
  irad=49.214d
  ierr=0.565d
  lra=318.41d
  ldec=-50.03d
  lerr=0.35d

  xra=318.17277
  xdec=-49.97297
  xerr=3.5/3600.

  begplot,name='~/Desktop/GRB120709A/sources.ps',font='helvetica',/land,/color
  ipncircle,ira,idec,irad,ierr,lra,ldec,lerr,xrange=[321,317],yrange=[-51,-49]
;  bigcirc,xra,xdec,xerr,x,y
;  oplot,x,y,color=!blue
  ;tvcircle,xerr,xra,xdec,color=!blue,/data
  plots,xra,xdec,psym=1,color=!blue

  bigcirc,lra,ldec,23.5/2./60.,x,y
;  xs=23.5/2./60.
;  y=[ldec-xs,ldec-xs,ldec+xs,ldec+xs,ldec-xs]
;  x=lra+[-xs,xs,xs,-xs,-xs]/cos(y*!dtor)

  oplot,x,y,color=!orange

;  tvellipse,xs/cos(ldec*!dtor),xs,lra,ldec,color=!orange,/data
  xyouts,319,-49.6,'IPN',color=!red
  xyouts,317.9,-49.8,'LAT',color=!green
  xyouts,318.,-50.2,'XRT Pointing',color=!orange
  xyouts,318,-50.0,'XRT Source',color=!blue
  endplot
  
end 

pro bigcirc,ra,dec,rad,x,y
  n=1000.
  x=fltarr(n) & y=x
  for i=0,n-1 do begin 
     temp=ll_arc_distance([ra,dec]*!dtor,rad*!dtor,360./n*i*!dtor)
     x[i]=temp[0]*!radeg
     y[i]=temp[1]*!radeg
  endfor 
  x=[x,x[0]]
  y=[y,y[0]]
  
  w=where(x lt 0.,nw)
  if nw gt 0 then x=x+360.

return
end 

pro ipncircle,ira,idec,irad,ierr,lra,ldec,lerr,xrange=xrange,yrange=yrange

  if n_elements(xrange) eq 0 then xrange=[min(lra-lerr)+4,max(lra+lerr)-3]
  if n_elements(yrange) eq 0 then yrange=[min(ldec-lerr)-1,max(ldec+lerr)+1]
;map_set,/aitoff,/grid
;  xrange=[0,360]
;  yrange=[-90,90]
;  xrange=reverse(xrange)

  plot,xrange,yrange,/nodata,xtitle='RA (deg)',ytitle='Dec (deg)',/xsty,/ysty,xrange=xrange,yrange=yrange,/iso
  plots,lra,ldec,psym=1,color=!green

  bigcirc,ira,idec,irad,x,y
  oplot,x,y,color=!red,line=1
  bigcirc,ira,idec,irad+ierr,x,y
  oplot,x,y,color=!red
  bigcirc,ira,idec,irad-ierr,x,y
  oplot,x,y,color=!red

;  tvellipse,irad/cos(idec*!dtor),irad,ira,idec,0,!red,line=1,/data
;  tvellipse,(irad+ierr)/cos(idec*!dtor),irad+ierr,ira,idec,0,!red,/data
;  tvellipse,(irad-ierr)/cos(idec*!dtor),irad-ierr,ira,idec,0,!red,/data

  bigcirc,lra,ldec,lerr,x,y
  oplot,x,y,color=!green
;  tvellipse,lerr/cos(ldec*!dtor),lerr,lra,ldec,color=!green,/data

return
end 
