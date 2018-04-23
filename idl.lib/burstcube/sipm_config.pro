pro sipm_config

  begplot,name='~/proposals/Burstcube/SiPM_config.ps',/color
  !p.charsize=1.
  r=94. ;; diameter/width in cm/10
  c=7.2
  c0=6.
  off0=2.5
  
  !p.multi=[0,1,2]
;;; cube

  n=round(r/c)
  nchips=n^2.
  print,ntostr(n)+' x '+ntostr(n)+' array of '+ntostr(fix(nchips))+' chips'
  plot,[0,r],[0,r],/nodata,/iso
  ax=[0,r,r,0,0] ;;; whole board
  ay=[0,0,r,r,0]
  oplot,ax+off0,ay+off0,color=!green

  off=off0+(r-n*c)/2. ;; put extra gap (from non-perfect dividing of chips) on outside
  x=[0,c,c,0,0] ;;; outside of each inactive area
  y=[0,0,c,c,0] 
  x0=[0,c0,c0,0,0]+(c-c0)/2. ;;; outside of each active area
  y0=[0,0,c0,c0,0]+(c-c0)/2.
  xc=fltarr(n,n) & yc=fltarr(n,n)
  for i=0,n-1 do begin
     xc[i,*]=findgen(n)*c
     yc[*,i]=findgen(n)*c
  endfor 
  for i=0,nchips-1 do begin
     oplot,xc[i]+x+off,yc[i]+y+off,color=!red,line=1
;     oplot,xc[i]+x0+off,yc[i]+y0+off,color=!blue,line=2
     polyfill,xc[i]+x0+off,yc[i]+y0+off,color=!blue
  endfor 
  print,'active filing factor = ',n^2*c0^2/r^2.



;;; cylinder

  rad=r/2.
  cen=rad+off0
  plot,[0,r],[0,r],/nodata,/iso  
  theta=findgen(360)
  oplot,cen+rad*cos(theta*!dtor),cen+rad*sin(theta*!dtor),color=!green
;  tvcircle,rad,cen,cen,color=!green
  dist=fltarr(nchips)
  for i=0,nchips-1 do begin
;     dist[i]=sqrt((cen-xc[i]-off-c0/2.)^2+(cen-yc[i]-off-c0/2.)^2)
     dist[i]=max(sqrt((cen-(xc[i]+off+x0))^2+(cen-(yc[i]+off+y0))^2))    
;     print,xc[i],yc[i],dist[i]
;     k=get_kbrd(10)
  endfor 
  w=where(dist le rad,nw)
  print,ntostr(nw)+' chips for '+ntostr(r/10.,3)+' cm cylinder'
  for i=0,nw-1 do begin
     oplot,xc[w[i]]+x+off,yc[w[i]]+y+off,color=!red,line=1
     polyfill,xc[w[i]]+x0+off,yc[w[i]]+y0+off,color=!blue
  endfor 
  print,'active filing factor = ',nw*c0^2/((r/2.)^2*!pi)
  
  !p.multi=0
  
  endplot

  begplot,name='~/proposals/Burstcube/SiPM_num_vs_diameter.ps',/color,/land

  rads=findgen(31)+70
  nrads=n_elements(rads)
  nsquare=fltarr(nrads)
  ncircle=fltarr(nrads)
  for j=0,nrads-1 do begin
     r=rads[j]
     n=round(r/c)
     nchips=n^2.
     nsquare[j]=nchips

     rad=r/2.
     cen=rad+off0
     dist=fltarr(nchips)
     ax=[0,r,r,0,0] ;;; whole board
     ay=[0,0,r,r,0]
     off=off0+(r-n*c)/2. ;; put extra gap (from non-perfect dividing of chips) on outside
     x=[0,c,c,0,0]       ;;; outside of each inactive area
     y=[0,0,c,c,0] 
     x0=[0,c0,c0,0,0]+(c-c0)/2. ;;; outside of each active area
     y0=[0,0,c0,c0,0]+(c-c0)/2.
     xc=fltarr(n,n) & yc=fltarr(n,n)
     for i=0,n-1 do begin
        xc[i,*]=findgen(n)*c
        yc[*,i]=findgen(n)*c
     endfor 
     for i=0,nchips-1 do begin
        dist[i]=max(sqrt((cen-(xc[i]+off+x0))^2+(cen-(yc[i]+off+y0))^2))    
     endfor 
     w=where(dist le rad,nw)
     ncircle[j]=nw

  endfor

  plot,rads/10.,nsquare,xtitle='Crystal Width/Diameter (cm)',ytitle='Number of SiPMs'
  oplot,rads/10.,ncircle,color=!green,line=2
  legend,['Square','Circular'],line=[0,2],color=[!p.color,!green]
  
  endplot

  ps2pdf,'~/proposals/Burstcube/SiPM_num_vs_diameter.ps'

  ps2pdf,'~/proposals/Burstcube/SiPM_config.ps'

stop  
  return
end 
