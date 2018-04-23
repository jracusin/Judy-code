pro grb170522a

  center=[159.268,-34.716]
  c1=[158.845, -35.550]
  c2=[158.818,-35.346]
  c3=[159.689,-33.872]
  c4=[159.715,-34.080]

  lat1=[139.34,25.67]
  lat1err=0.19

  lat2=[139.451, 25.863]
  lat2err=0.19

  ipnbox,lat1,c1,c2,c3,c4,yrange=[24,27],xrange=[137,141],/xrt
  tvellipse,lat1err/cos(lat1[1]*!dtor),lat1err,lat1[0],lat1[1],0,/data,color=!magenta
  tvellipse,lat2err/cos(lat2[1]*!dtor),lat1err,lat2[0],lat2[1],0,/data,color=!pink
  plots,lat1[0],lat1[1],psym=2,color=!magenta
  plots,lat2[0],lat2[1],psym=2,color=!pink
  plots,center[0],center[1],psym=1

  return
end 

pro grb160509a

  center=[312.131,75.810]
  c1=[310.897 , 75.728]
  c2=[311.105 , 75.626]
  c3=[313.379 , 75.887]
  c4=[313.181 , 75.991]
;  lat=[310.1, 76.0]
;  lat=[310.635, 76.157]
;  lat=[312.4,75.99]
;  lat=[310.39,76.03]
  lat=[311.2,76.07]
  laterr=0.09
  ipnbox,lat,c1,c2,c3,c4,yrange=[74,78],xrange=[308,314],/xrt
  tvellipse,laterr/cos(lat[1]*!dtor),laterr,lat[0],lat[1],0,/data,color=!magenta
  plots,lat[0],lat[1],psym=2,color=!magenta
  plots,center[0],center[1],psym=1
  plots,311.74406,76.106341,psym=1,color=!blue

stop
return
end 

pro grb150127a

  center=[296.953,-9.416]

  c1=[297.190,-6.883]
  c2=[296.956,-7.457]
  c3=[296.868,-13.764]
  c4=[297.021,-12.130]

  ipnbox,center,c1,c2,c3,c4
  return
end 

pro grb140219a

  center=[156.44,7.46]
  
  c1=[157.06,10.06]
  c2=[156.56,7.30]
  c3=[155.51,4.70]
  c4=[156.31,7.66]

  sra=[center[0],156.358,156.505]
  sdec=[center[1],7.430,7.781]
  srad=23.5/2./60.

  ;; Judy LAT
  lat1=[156.654,7.654]
  laterr1=1.3

  ;; Giacomo LAT
  lat2=[158.24,7.179]
  laterr2=0.477

  begplot,name='~/GRBWorkdir/GRB140219824/grb140219a_ipn.ps',/color,font='helvetica'

  ipnbox,center,c1,c2,c3,c4,xrange=[159,155]
  plots,lat1[0],lat1[1],color=!salmon,psym=2
  plots,lat2[0],lat2[1],color=!red,psym=2
  tvellipse,laterr1/cos(lat1[1]*!dtor),laterr1,lat1[0],lat1[1],0,/data,color=238 ;; judy
  tvellipse,laterr2/cos(lat2[1]*!dtor),laterr2,lat2[0],lat2[1],0,/data,color=!red ;; giacomo
  for i=1,2 do tvellipse,srad/cos(sdec[i]*!dtor),srad,sra[i],sdec[i],0,/data,color=252
  legend,['IPN','Judy LAT','Giacomo LAT','XRT Tiles'],textcolor=[!p.color,!salmon,!red,!blue],box=0,/top,/left

;stop
  endplot
  spawn,'ps2pdf ~/GRBWorkdir/GRB140219824/grb140219a_ipn.ps ~/GRBWorkdir/GRB140219824/grb140219a_ipn.pdf'
stop
return
end 

pro grb131108a

  ;; lat onboard
  ;; xrt/uvot tile
  ;; ipn
  ;; lat ground
  ;; grb position

  ;;; give up doesn't help

  begplot,name='~/Desktop/GRB131108A/GRB131108a_pointings.ps',/land,/color

  c1=[156.363,9.411]
  c4=[156.702,10.345]
  c2=[156.477,9.400]
  c3=[156.797,10.283]
  center=[mean([c1[0],c2[0],c3[0],c4[0]]),mean([c1[1],c2[1],c3[1],c4[1]])]
  lat=[156.47,9.9]

  ipnbox,center,c1,c2,c3,c4,yrange=[9,11.2]
  laterr=0.5
  tvellipse,laterr/cos(lat[1]*!dtor),laterr,lat[0],lat[1],0,/data,color=!red

  r=700/60.
  g=r*sqrt(3.)
  g2=r*sqrt(2.)
  xrtrad=23.6/2.
  tra=[0,-g,g,g*0.5,-g*0.5,-g*0.5,g*0.5]/60.
  tdec=[0,0,0,g2,-g2,g2,-g2]/60.
  for i=0,6 do tvcircle,xrtrad/60.,lat[0]+tra[i],lat[1]+tdec[i],251
  for i=0,6 do tvcircle,17./2./60.,lat[0]+tra[i],lat[1]+tdec[i],245


  xrt=[156.50035, 9.6625441]
  xrterr=3.6/60./60.
  plots,xrt[0],xrt[1],psym=2,color=!blue
  tvellipse,xrterr/cos(xrt[1]*!dtor),xrterr,xrt[0],xrt[1],0,/data,color=!blue

  lra=[156.367,156.383,156.383,156.6,156.633,156.467,156.450,156.383]
  ldec=[11.1,11.067,11.067,9.683,9.9,9.9,9.867,9.967]
  oplot,lra,ldec,psym=1,color=!red
  for i=0,7 do xyouts,lra[i]+0.01,ldec[i]-0.02,ntostr(i),color=!red,charsize=1

  plots,156.662,9.77387,psym=2,color=!orange
  tvcircle,0.153,156.662,9.77387,!orange

  legend,['LAT Onboard','LAT Ground','IPN','XRT Tiles','UVOT Tiles','XRT GRB Position'],box=0,/top,/right,textcolor=[!red,!orange,!p.color,!green,!purple,!blue]
  endplot
  

  return
end

pro grb130310a

  begplot,name='GRB130310A_pointings.ps',/land,/color 
  center=[141.905,-17.431]
  c1=[141.722,-16.780]
  c2=[141.801,-17.435]
  c3=[142.093,-18.072]
  c4=[142.008,-17.428]
  ipnbox,center,c1,c2,c3,c4,/xrt
  lat=[142.34, -17.23]
  laterr=0.45
  tvellipse,laterr/cos(lat[1]*!dtor),laterr,lat[0],lat[1],0,/data,color=!red

  xrt=[141.9,-17.4]
  xrterr=23.6/60./2.
  tvellipse,xrterr/cos(xrt[1]*!dtor),xrterr,xrt[0],xrt[1],0,/data,color=!blue

  endplot

  spawn,'convert GRB130310A_pointings.ps GRB130310A_pointings.pdf '
return
end 

pro grb120603a

  center=[198.794,4.326]
  c1=[197.693,2.527]
  c2=[199.766,6.230]
  c3=[199.927,6.103]
  c4=[197.853,2.399]

  ipnbox,center,c1,c2,c3,c4
return
end 

pro grb120911B
  center=[172.146,-37.696]
  c1=[172.616,-38.705]
  c2=[172.297,-38.543]
  c3=[171.709,-36.675]
  c4=[172.013,-36.848]
  lat=[172.03,-37.51]
  laterr=0.3
  ipnbox,center,c1,c2,c3,c4

  plots,lat[0],lat[1],psym=1,color=!red
  tvellipse,laterr/cos(lat[1]*!dtor),laterr,lat[0],lat[1],0,/data,color=!red
  legend,['IPN Center','LAT 68%'],psym=[2,1],color=[!p.color,!red],textcolor=[!p.color,!red],box=0,/top,/right

  return
end 

pro ipnbox,center,c1,c2,c3,c4,xrt=xrt,xrange=xrange,yrange=yrange

  ra=[center[0],c1[0],c2[0],c3[0],c4[0],c1[0]]
  dec=[center[1],c1[1],c2[1],c3[1],c4[1],c1[1]]
  if n_elements(xrange) eq 0 then xrange=[round(min(ra)-0.5),round(max(ra)+0.5)]+[-0.3,0.3]
  if n_elements(yrange) eq 0 then yrange=[round(min(dec)-0.5),round(max(dec)+0.5)]

  plot,xrange,yrange,/nodata,xtitle='RA (deg)',ytitle='Dec (deg)',/xsty,/ysty,xrange=xrange,yrange=yrange,/iso
;  plots,ra[0],dec[0],psym=2
  
  oplot,ra[1:*],dec[1:*]
  xrtrad=23.5/2./60.

  if keyword_set(xrt) then tvellipse,xrtrad/cos(dec[0]*!dtor),xrtrad,ra[0],dec[0],0,/data,color=!orange

  return
end 
