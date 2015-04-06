pro test,new

  use=20.
  map_set,/ait,/grid
  tvscl,new,6.,10.
  mu=max(use)
  key=fltarr(mu*4+4,10)
  for i=0,9 do begin
     for j=0,mu do begin 
        key[j*4:j*4+3,i]=j
     endfor 
  endfor 
  tvscl,key,20,40
  plots,[20,mu*4+20],[40,40],/device
  xyouts,20,100,'0',color=!white,/device
  xyouts,mu+20,100,ntostr(fix(mu)),/device,color=!white
  xyouts,mu,160,'# Pointings',/device,color=!white

  map_grid,latdel=30,londel=30
stop
end 

pro drawarc, p1, p2, color, theta,ssize,lstyle,nowset=nowset ; theta is angle of circle segment 
;						(in degrees)
;Draw a great circle between two input points

  n = fix(theta)                ;# of points (one per degree)

; Convert input directions to vectors
  p1r = p1 * !dtor              ;To radians
  p2r = p2 * !dtor
  lon = [p1r(0), p2r(0)]
  lat = [p1r(1), p2r(1)]
  x = cos(lat) * cos(lon)		;To xyz space
  y = cos(lat)* sin(lon)
  z = sin(lat)
  u = [x(0), y(0), z(0)]		; 3-vectors
  v = [x(1), y(1), z(1)]

  temp = crossp(u,v)
  w = temp/norm(temp)
  temp = crossp(w,u)
  vv = temp/norm(temp)

  lon = fltarr(n+1)
  lat = fltarr(n+1)
  for i = 0,n do begin
     ds = i * (theta * !DTOR/n)
     uu = [cos(ds), sin(ds), 0.0]
     matrix = [[u], [vv], [w]]
     uuu = matrix # uu
; Convert back to lat, long
     lat(i) = asin(uuu(2)/norm(uuu))*!RADEG
     ccc = uuu(0)^2 + uuu(1)^2
     lon(i) = 0.0
     if (ccc ne 0.0) then begin
        temp = acos(uuu(0)/sqrt(ccc))
        if (uuu(1) lt 0.0) then temp = -1.0*temp
        lon(i) = (temp*!RADEG + 720.0) mod 360.0
     endif
  endfor
  plotsym,0,/fill,1
  ltest=size(lstyle)

  if ltest[1] eq 2 then plots,lon,lat, color = color, thick=4,linestyle=lstyle $
  else plots,lon,lat, color = color, thick=2,psym=8,symsize=ssize
  return
end

pro draw_constraint,lon,lat,con,color=color

  if n_elements(color) eq 0 then color=!p.color

  ang = 2*!PI*findgen(49)/48.   ;Get position every 5 deg
  psize=con
  xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
  xarr=xarr+lon  &  yarr=yarr+lat
  xarr2=fltarr(49)
  yarr2=fltarr(49)
  for ct=0,48 do begin
     temp=ll_arc_distance([lon,lat]*!pi/180.,psize*!pi/180.,ct*7.5*!pi/180.)
     xarr2(ct)=temp(0)*!radeg
     yarr2(ct)=temp(1)*!radeg
  endfor
  oplot,xarr2,yarr2,color=color,thick=3

end

FUNCTION Inside, x, y, px, py

                                ;  x - The x coordinate of the point.
                                ;  y - The y coordinate of the point.
                                ; px - The x coordinates of the polygon.
                                ; py - The y coordinates of the polygon.
                                ;
                                ; The return value of the function is 1 if the point is inside the
                                ; polygon and 0 if it is outside the polygon.

  sx = Size(px)
  sy = Size(py)
  IF (sx[0] EQ 1) THEN NX=sx[1] ELSE RETURN, -1      ; Error if px not a vector
  IF (sy[0] EQ 1) THEN NY=sy[1] ELSE RETURN, -1      ; Error if py not a vector
  IF (NX EQ NY) THEN N = NX ELSE RETURN, -1          ; Incompatible dimensions
  
  tmp_px = [px, px[0]]          ; Close Polygon in x
  tmp_py = [py, py[0]]          ; Close Polygon in y
  
  i = indgen(N)                 ; Counter (0:NX-1)
  ip = indgen(N)+1              ; Counter (1:nx)
  
  X1 = tmp_px(i)  - x 
  Y1 = tmp_py(i)  - y
  X2 = tmp_px(ip) - x 
  Y2 = tmp_py(ip) - y
  
  dp = X1*X2 + Y1*Y2            ; Dot-product
  cp = X1*Y2 - Y1*X2            ; Cross-product
  theta = Atan(cp,dp)
  
  IF (Abs(Total(theta)) GT !PI) THEN RETURN, 1 ELSE RETURN, 0
END


pro dealwpoles,x,y
  
  w=where(x lt 0,nw)
  if nw gt 0 then x[w]=x[w]+360.
  w=where(x gt 360,nw)
  if nw gt 0 then x[w]=x[w]-360.

  w=where(x lt 0,nw)
  if nw gt 0 then x[w]=x[w]+360.
  w=where(x gt 360,nw)
  if nw gt 0 then x[w]=x[w]-360.

  w=where(y lt -90,nw)
  if nw gt 0 then begin
     y[w]=-180-y[w]
;     x[w]=x[w]+180.
;     q=where(x[w] gt 360,nq)
;     if nq gt 0 then x[w[q]]=x[w[q]]-360.
  endif 
;y[w]=y[w]+180.
  w=where(y gt 90,nw)
  if nw gt 0 then begin
     y[w]=180-y[w]
;     x[w]=x[w]+180.
;     q=where(x[w] gt 360,nq)
;     if nq gt 0 then x[w[q]]=x[w[q]]-360.
  endif 
;  if nw gt 0 then y[w]=y[w]-180.
  return
end 

pro rotate_onsky,x,y,roll,xp,yp,x0=x0,y0=y0
  
  if n_elements(x0) eq 0 then x0=0.
  if n_elements(y0) eq 0 then y0=0.
  theta=roll*!dtor
  ct=cos(theta)
  st=sin(theta)
  rx=[ct,st,st,ct,ct]
  ry=[st,st,ct,ct,st]

  m=fltarr(2,n_elements(x))
  m[0,*]=x-x0
  m[1,*]=y-y0
  rm=[[ct,-st],[st,ct]]

  nm=m
  for i=0,n_elements(x)-1 do nm[*,i]=rm##m[*,i]
  xp=nm[0,*]+x0
  yp=nm[1,*]+y0

  return
end 

pro plot_fov,x0,y0,roll,map=map,color=color


  ;;; total area
  y=55                          ; deg
  x=34                          ;deg
  xs=x/2.
  ys=y/2.
  
  ;;; overlap/underlap area
  xv=2.
  yv=11.

  ;; boresight offset
  xoff=-3.
  yoff=-2.5 ;;; get boresight in middle of overlap strip

  ;;; total coverage
  xx=[x0-xs,x0+xs,x0+xs,x0-xs,x0-xs]
  yy=[y0+ys,y0+ys,y0-ys,y0-ys,y0+ys]
  rotate_onsky,xx,yy-5.3,roll,xp,yp,x0=x0,y0=y0
;  yp=yp*(1.-cos(yp*!dtor))

;  plots,xx,(yy-5.3),thick=1,/data,color=!red
;  plots,xp,yp,color=!red

  ;;; each WFI
  xc=x0+xoff+[-8.59,2.86,14.32,-3.73,9.46,-5.73,11.46,-8.59,2.86,14.32]
  yc=y0+yoff+[17.19,17.19,17.19,2.86,2.86,-8.59,-8.59,-22.92,-22.92,-22.952]
  wx=11.5 ;;; WFI size
  wy=17.2
  rotate_onsky,xc,yc,roll,xp,yp,x0=x0,y0=y0
;  yp=yp*(1.-cos(yp*!dtor))
  xyouts,xp-1,yp-1,ntostr(indgen(10)+1),/data
;  dealwpoles,xc,yc

;  plots,xc,yc,psym=1
  for i=0,9 do begin
     if i le 2 or i ge 7 then begin 
        xs=[-wx/2.,wx/2.,wx/2.,-wx/2.,-wx/2.]+xc[i]
        ys=[+wy/2.,wy/2.,-wy/2.,-wy/2.,wy/2.]+yc[i]
;        dealwpoles,xs,ys
        rotate_onsky,xs,ys,roll,xp,yp,x0=x0,y0=y0
;        oplot,xs,ys
     endif else begin
        xs=[-wy/2.,wy/2.,wy/2.,-wy/2.,-wy/2.]+xc[i]
        ys=[wx/2.,wx/2.,-wx/2.,-wx/2.,wx/2.]+yc[i]
;        dealwpoles,xs,ys
        rotate_onsky,xs,ys,roll,xp,yp,x0=x0,y0=y0
;        oplot,xs,ys  
     endelse 

     huh=yp
     
     dealwpoles,xp,yp
;        yp=yp*(1.-cos(yp*!dtor))
     if yp[0] lt -90 then stop

     oplot,xp,yp,color=color
     mx=fix([min(xp),max(xp)])+[-2,2.]
     my=fix([min(yp),max(yp)])+[-2.,2.]
     
;        w=where(xp gt 360,nw)
;        if nw gt 0 then xp=xp mod 360.
     
     if (abs(xp[1]-xp[0]) gt 180) then begin
        w=where(xp lt 50,nw)
        xp[w]=xp[w]+360.
        mx=[mx[0],mx[1]+360]
     endif 
     for j=mx[0],mx[1] do begin
        for k=my[0],my[1] do begin 
           xa=j*1.
           ya=k*1.
           if j ge 360 then jj=360. else jj=0.
           in=inside(xa,ya,xp[0:3],yp[0:3])
           if in eq 1 then map[j-jj,k+90.]=map[j-jj,k+90.]+1.
;              print,j,k,in
        endfor 
     endfor 
  endfor 
;  y0=y0*(1.-cos(y0*!dtor))
  dealwpoles,x0,y0

  plots,x0,y0,psym=2
end

pro lobster_obs,x0,y0,roll,cart=cart,celestial=celestial,ecliptic=ecliptic,eph=eph,ps=ps

  if keyword_set(celestial) then s=0
  if keyword_set(ecliptic) then s=3
;  window,0
  if keyword_set(ps) then begplot,name='~/Lobster/lobster_obs.ps',/land,/color
;  multiplot,[1,2],/init
;  !p.multi=[0,1,2]
;  !p.multi=[0,2,2]
                                ; multiplot
;  fov=34 x 55 - 2 x 11 (x2)
;  map=mrdfits('~/idl.lib/swift_sot/blank_map.fits')
  map=fltarr(360.,180.)
;  xmap=map & ymap=map
;  for i=0,360-1 do xmap[i,*]=i
;  for i=0,180-1 do ymap[*,i]=i-90
  if not keyword_set(cart) then map_set,/ait,/grid else $
     plot,[-30,30.],[-50,50],/nodata,/iso

  year=2010 ;;; testing Oct 16 because have ephermeris
  dn=289.
  date=[2010,dn]
  partday=0.

  ydn2md,date[0],date[1],month,day
  jdcnv,date[0],month,day,partday*24.,jd
  
  ;;plotting sun/moon data for date
  sunpos,jd,sunra,sundec
  moonpos,jd,moonra,moondec

  ;; converting ra/dec to galactic lon/lat
  if not keyword_set(celestial) and not keyword_set(ecliptic) then s=1
  euler, sunra, sundec, sunlon, sunlat, s 
  euler, moonra, moondec, moonlon, moonlat, s

  ;; get earth position (random ephemeris from Swift)
  ephfile='~/Lobster/STK_EPH_2010285_2010301_00.txt'
  print,'Using Ephemeris file: ',ephfile
  if n_elements(eph) eq 0 then eph=readeph(ephfile)
  
  ;;constraints (from Swift)
  scon=45.
  mcon=21.
  econ=28.+67.

  plotsym,0,2,/fill
;  plots,sunlon,sunlat,color=!red,psym=8
;  draw_constraint,sunlon,sunlat,scon,color=!red
;  plots,moonlon,moonlat,color=!p.color,psym=8
;  draw_constraint,moonlon,moonlat,mcon,color=!p.color

  ;;; Obs begin times (every 10 minutes for simplicity) (8 obs + 2 slew)
  hr=ntostr(indgen(24),2)
  hr[0:9]=replicate('0',10)+hr
  mn=ntostr(indgen(6)*10)
  mn[0]='00'
  sec='00.00'
  times=''
  for i=0,23 do times=[times,hr[i]+':'+mn+':'+sec]
  times=times[1:*]
  ntimes=n_elements(times) ;; 24*6
  era=fltarr(ntimes)
  edec=fltarr(ntimes)
  for i=0,ntimes-1 do begin 
     time=times[i]
     ddate=ntostr(fix(date[1]))+'/'+ntostr(date[0],4)
     x=where(strtrim(eph.dy,2) eq ddate and strtrim(eph.time,2) eq time)
     elaz2,-1.*[eph[x].x,eph[x].y,eph[x].z],edec0,era0
     era[i]=era0
     edec[i]=edec0
  endfor 




  ;;; equal placement observing strategy - non-time dependent
;  n=20.
;  rx=findgen(n)/(n/2.)*360.
;  rx=[rx,rx]
;  ry=[replicate(65.,n/2.),replicate(-65.,n/2.)];,replicate(5,n/3.)]
;  rr=fltarr(n)
;  rr[n/2:*]=180.
;  rr[0:n/2-1]=0.
;  for i=0,n-1 do plot_fov,rx[i],ry[i],rr[i],map=map

  ;;; observing strategy - equal placement - time dependent
  n=20.
  rx=findgen(n/2.)/(n/2.)*360.
  rx=[rx,rx]
  ry=[replicate(35.,n/2.),replicate(-35.,n/2.)] ;,replicate(5,n/3.)]
  rr=fltarr(n)
  rr[n/2:*]=180.
  rr[0:n/2-1]=0.
  ;; add polar coverage
  nn=6
  n=n+nn*2
  rx=[rx,indgen(nn)*60.+45,indgen(nn)*60.+25.]
  ry=[ry,replicate(75.,nn),replicate(-75.,nn)]
  rr=[rr,replicate(90.,nn),replicate(90.,nn)]
  help,rx,ry
  map_set,/ait,/grid
  for j=0,n-1 do plot_fov,rx[j],ry[j],rr[j],map=map
  if not keyword_set(ps) then begin 
     k=get_kbrd(10)
     if k eq 's' then stop
  endif 
  erase
  ;;; every other
;  i=[indgen(10)*2,indgen(10)*2+1]
;  rx=rx[i]
;  ry=ry[i]
;  rr=rr[i]

  off=12.
  plotsym,0,2,/fill
  use=intarr(n)
  nslew=144
  rxx=fltarr(nslew) & ryy=rxx & rrr=rxx
  point=0
  slewdist=fltarr(nslew) & sundist=slewdist & sunha=slewdist
  for i=0,nslew-1 do begin
     euler, era[i], edec[i], elon, elat, s 
        ;;; choose pointing least in constraint and smallest slew
        ;;; distance
;              in=inside(xa,ya,xp[0:3],yp[0:3])

     ;; not max dist but min overlap
     mdist=separation(moonlon,moonlat,rx,ry)/3600.
     sdist=separation(sunlon,sunlat,rx,ry)/3600.
     edist=separation(elon,elat,rx,ry)/3600.
;
;     w=where(mdist gt mcon and sdist gt scon+10. and edist gt
;     econ+10. and use eq 0,nw)
     if i gt 0 then sep=separation(rxx[i-1],ryy[i-1],rx,ry)/3600 else sep=0.
     w=where(sdist gt scon and edist gt econ and use le median(use) and sep le 100.,nw)
     if nw eq 0 then begin
        w=where(edist gt econ,nww)
        if nww gt 0  then begin 
           m=max(sdist[w],mm)
           w=w[mm]
        endif
     endif 
;w=where(sdist gt scon+10. and edist gt econ+10.,nw)
;     if nw gt 0 then begin 
        erase
        map_set,/ait,/grid 
        xyouts,10.,15.,times[i],/device
        plots,sunlon,sunlat,color=!red,psym=8
        draw_constraint,sunlon,sunlat,scon,color=!red
        
        plots,moonlon,moonlat,color=!p.color,psym=8
        draw_constraint,moonlon,moonlat,mcon,color=!p.color
        if i gt 0 then begin
;           u=rem_dup(rxx+ryy)
;           nu=n_elements(u)
;           for j=0,nu-1 do
;           plot_fov,rxx[u[j]],ryy[u[j]],rrr[u[j]],map=map
           for j=0,i-1 do plot_fov,rxx[j],ryy[j],rrr[j],map=map
        endif 
 
       point=point+1
;     m=max(mdist[w]+sdist[w]+edist[w],mm)
;        m=max(edist[w],mm)
       mm=round(randomu(seed)*(nw-1))

;     if i gt 0 then m=min(separation(rx[w],ry[w],rxx[i],ryy[i])/3600.,mm)
;     if i gt 0 then m=min((rx[w]-rxx[i]),mm)
        
       if nw gt 1 then k=w[mm] else k=w
        if i gt 0 then slewdist[i]=sep[k]
        sundist[i]=sdist[k]

        colprint,mdist[k],sdist[k],edist[k],use[k]
;        if nw eq 0 then stop     
        use[k]=use[k]+1
        rxx[i]=rx[k]
        ryy[i]=ry[k]
        rrr[i]=rr[k]
        sunha[i]=abs(rxx[i]-sunlon)
        
;     if edist[k] lt econ+20. and ryy[i] gt 0 then ryy[i]=ryy[i]+10
;     if edist[k] lt econ+20. and ryy[i] lt 0 then ryy[i]=ryy[i]-10
        plot_fov,rxx[i],ryy[i],rrr[i],map=map,color=!green
;     colprint,times[i+off],rxx[i],ryy[i],rrr[i]
        plots,rxx[i],ryy[i],psym=2,color=!magenta
        plots,elon,elat,color=!blue,psym=8
        econt=[indgen(10)*econ/9.5,95.]
        for h=0,n_elements(econt)-1 do draw_constraint,elon,elat,econt[h],color=!blue
        if not keyword_set(ps) then begin 
           k=get_kbrd(10)
           if k eq 's' then stop
        endif 
;     endif 
  endfor 
  
  print,'N pointings:',point

  if not keyword_set(ps) then k=get_kbrd(10)
;;;;; EXPOSURE MAP
;  window,1
  new=map_image(map,0.,0.,latmin=-90,latmax=90.,lonmin=0,lonmax=360.) ;,/bilin,min_value=0.)
;  multiplot
  !p.multi=0
  map_set,/ait,/grid
  tvscl,new,6.,10.
  mwrfits,new,'exposure_map.fits',/create
  mu=max(use)
  key=fltarr(mu*4+4,1)
  for i=0,0 do begin
     for j=0,mu do begin 
        key[j,i]=j
;        key[j*4:j*4+3,i]=j
     endfor 
  endfor 
  tvscl,key,2,40
  plots,[20,mu*4+20],[40,40],/device
  xyouts,1000,1000,'0',color=!white,/device
  xyouts,5500,1000,ntostr(fix(mu)),/device,color=!white
  xyouts,1600,1600,'# Pointings',/device,color=!white
;  xyouts,20,30,'0',/device
;  xyouts,mu*4+20,30,ntostr(fix(mu)),/device
;  xyouts,mu*4/3,60,'# Pointings',/device

  map_grid,latdel=30,londel=30

  ;; Each WFI 1934 sq deg
  w=where(map ge 1.,nw)
  print,nw/(360.*180.)

;  map_grid,latdel=30,londel=30;,glinestyle=0
;  multiplot,/default,/reset
  !p.multi=[0,1,3]
  plothist,slewdist,bin=5.,xtitle='Slew Length (deg)'
  plothist,sundist,bin=5.,xtitle='Sun Distance (deg)'
  plothist,sunha/15.,bin=1.,xtitle='Sun Hour Angle (hr)'
  !p.multi=0

  if keyword_set(ps) then endplot
;  !p.multi=0

  ;; observing strategy
  ;;; full sky coverage
  ;;; no point IRT at Sun, Moon, Earth
  ;;; 90 min orbit, 28 deg inclination - Swift is okay approx
  ;;; slew time of 90 s - 10 fields per orbit, 450 s per field
  ;;; covers 85% of sky in 2 orbits

  ;; secondary goals
  ;;; bias towards anti-sun pointing
  ;;; would require changing exposure times, or doing fewer pointings
  ;;; perhaps getting more exposure coverage anti-sun, but still
  ;;; covering whole sky in a day - would even-out over a year (?)

  ;;;; to achieve goals
  ;;;; need to stagger fields a bit to cover plane and poles
  ;;;; stagger fields away from Sun constraint and (?) moon constraint
  ;;;; could also minimize earth constraint overlap
  ;;;; roll angle configuration would have strong impact


  stop
end
