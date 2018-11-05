pro exposure_hist

  ;add=['1day','1day2','1week','1month','1year']
  add='1week2'
  bin=[500,1000,1000,1e4,1e5]
  for i=0,n_elements(add)-1 do begin 
     
     l=mrdfits('wfi_swift_points_expomap_'+add[i]+'.fits')

     begplot,name='WFI_pointings_hist_'+add[i]+'.ps',/color,/land
     plothist,l,bin=bin[i],/fill,xtitle='Exposure Time (s)',fcolor=!blue,ytitle='N',title=add[i],xrange=[0,10000]
     endplot
     ps2pdf,'WFI_pointings_hist_'+add[i]+'.ps'

  endfor 
  stop
end

pro add_maps,xx,yy,map,add=add,nside=nside

  ;;; get set of healpix pixels with each pixel <1 deg
  ;;; select pixels in WFI foV centered at 0,0, or somewhere
  ;;; equatorial
  ;;; figure out rotation/translation to get 0,0 to ra,dec,roll
  ;;; inverse translate/rotate to get all of those pixels back to
  ;;; ra/dec/roll then find nearest pixel and increment exposure
  ;;; nevermind - I just did it all in healpix, though depends on
  ;;;             other rotation

  if n_elements(nside) eq 0 then nside=16
  npix=nside2npix(nside)
  if n_elements(map) eq 0 then map=lonarr(npix)
  if n_elements(add) eq 0 then add=1.

;  r=round(randomu(seed,1000)*npix)
  w1=where(xx lt 180,nw1)
  w2=where(xx ge 180,nw2)
  ang2vec,yy[w1],xx[w1],ipnest1,/astro
  ang2vec,yy[w2],xx[w2],ipnest2,/astro

  if nw1 gt 3 then begin 
     query_polygon,nside,ipnest1,r1
     map[r1]=map[r1]+add
  endif 
  if nw2 gt 3 then begin 
     query_polygon,nside,ipnest2,r2
     map[r2]=map[r2]+add
  endif 
;  mollview,map
;stop  


  return
end 

pro rotate_box,ang,xxp,yyp,xc=xc,yc=yc,xsize=xsizes,ysize=ysizes,bin=bin

  if n_elements(yc) eq 0 then yc=0.
  if n_elements(xc) eq 0 then xc=0.

;  if n_elements(xsizes) eq 0 then xsize=10
;  if n_elements(ysizes) eq 0 then ysize=45
  xsize=xsizes/2.
  ysize=ysizes/2.
  if n_elements(bin) eq 0 then bin=1.

  cx=indgen(xsize/bin)*bin
  cy=indgen(ysize/bin)*bin
  rcx=reverse(cx)+1
  rcy=reverse(cy)+1
  bx=replicate(xsize,ysize/bin)
  by=replicate(ysize,xsize/bin)

;  yy=[-b,-b,-rc,c,b,b,rc,-c]
;  xx=[-rc,c,b,b,rc,-c,-b,-b]/cos((yy+yc)*!dtor)

  yy=[-by,-by,-rcy,cy,by,by,rcy,-cy]
  xx=[-rcx,cx,bx,bx,rcx,-cx,-bx,-bx];/cos((yy+yc)*!dtor)
  theta=atan(yy*1./xx*1.)
  t=theta*!radeg
  w=where(xx le 0 and yy lt 0) ;; 3rd quadrant
  t[w]=t[w]+180
  w=where(xx ge 0 and yy lt 0) ;; 4th quadrant
  t[w]=t[w]+360
  w=where(xx le 0 and yy gt 0) ;; 2nd quadrant
  t[w]=t[w]+180
  w=where(xx gt 0 and yy eq 0)
  t[w]=0
  w=where(xx lt 0 and yy eq 0)
  t[w]=180
  w=where(xx eq 0 and yy gt 0)
  t[w]=90
  w=where(xx eq 0 and yy lt 0)
  t[w]=270

  theta=t*!dtor

  rad=sqrt(xx^2+yy^2)
;  rad=r*sqrt(2.)
  yyp=cos(theta+ang*!dtor)*rad+yc
  xxp=sin(theta+ang*!dtor)/cos(yyp*!dtor)*rad+xc
  if ang eq 0 then begin 
     yyp=yy+yc
     xxp=xx/cos(yyp*!dtor)+xc
  endif 


  yyp=yyp mod 180.
  xxp=xxp mod 360.

  w=where(yyp lt -90,nw)
;  if nw gt 0 then yyp[w]=-180-yyp[w]
  while nw gt 0 do begin
     yyp[w]=-180-yyp[w]
     w=where(yyp lt -90,nw)
  endwhile

  w=where(yyp gt 90,nw)
  while nw gt 0 do begin
     yyp[w]=180-yyp[w]
     w=where(yyp gt 90,nw)
  endwhile

;  if nw gt 0 then yyp[w]=180-yyp[w]
;  w=where(xxp lt 0 or xxp gt 360,nw)
;  if nw gt 0 then xxp[w]=xxp[w] mod 360
  w=where(xxp lt 0,nw)
  if nw gt 0 then xxp[w]=xxp[w]+360.
  w=where(xxp gt 360,nw)
  if nw gt 0 then xxp[w]=xxp[w]-360.
; skip:

;  map_set,/ait,/grid
;  oplot,xxp,yyp,color=!red

  return
end

pro read_swift_afst

  readcol,'~/Lobster/Swift_2015.txt',line,delim='$',skip=1,format='(a)'
  nlines=n_elements(line)

  afst=create_struct('date','','time',0d,'duration',0d,'target','','ra',0.,'dec',0.,'roll',0.,'sun_ra',0.,'sun_dec',0.,'sun_ha',0.)
  afst=replicate(afst,nlines)
  met0=date2met('2015-001-00:00:00')
  lasti=-1
;nlines=100
  for i=0L,nlines-1 do begin
     chunks=strsplit(line[i],' ',/ex)
     if chunks[1] eq 'PPT' and chunks[2] eq 'Begin' then begin 
        afst[i].date=chunks[0]
        afst[i].time=(date2met(chunks[0])-met0)/86400./365.+2015.
        if chunks[7] gt 400. and chunks[7] lt 1e5 then n=0
        if chunks[8] gt 400. and chunks[8] lt 1e5 then n=1
        if chunks[9] gt 400. and chunks[9] lt 1e5 then n=2
        if chunks[10] gt 400. and chunks[10] lt 1e5 then n=3
        if chunks[11] gt 400. and chunks[11] lt 1e5 then n=4
        
        for j=0,n do afst[i].target=afst[i].target+chunks[3+j]
        afst[i].ra=chunks[4+n]
        afst[i].dec=chunks[5+n]
        afst[i].roll=chunks[6+n]
        lasti=i
;print,i,n
;if n ge 1 then begin
;   help,afst[i],/str
;endif 
;colprint,i*1.,chunks[4+n]
     endif 
     if chunks[1] eq 'PPT' and chunks[2] eq 'End' then begin 
        if lasti gt -1 then begin
           afst[lasti].duration=(date2met(chunks[0])-date2met(afst[lasti].date))/86400.
        endif 
     endif 
;     k=get_kbrd(10)
;     if k eq 's' then stop
  endfor 
  w=where(afst.date ne '' and afst.ra ge 0 and afst.ra le 360. and afst.roll le 360.)
  afst=afst[w]

  for i=0,n_elements(afst)-1 do begin 
     chunks=strsplit(afst[i].date,'-:',/ex)
     year=chunks[0]
     doy=chunks[1]
;     time=(chunks[2]+chunks[3]/60+chunks[4]/3600.)/24.
     
     ydn2md,year,doy,month,dy
     jdcnv,year,month,dy,0.,jd

     sunpos,jd,sunra,sundec
     afst[i].sun_ra=sunra
     afst[i].sun_dec=sundec
     ha=abs(sunra-afst[i].ra)/15.
     if ha gt 12 then ha=24.-ha
     afst[i].sun_ha=ha
  endfor 

  mwrfits,afst,'~/Lobster/Swift_2015_afst.fits',/create

;  stop
  return
end 

pro swift_sim,wfi=wfi,irt=irt

  if not keyword_set(wfi) and not keyword_set(irt) then begin
     print,'Need to set either /wfi or /irt'
     return
  end 

;;; 2 simulations
  ;;;; take 1 year (2015?) of Swift pointing history, and change the
  ;;;; BAT into the WFI, and XRT/UVOT into IRT, and make exposure
  ;;;; maps, excluding SAA cold pointings
  
  ;;;; plan vaguely realistic daily schedule, with 10-15% SAA, some
  ;;;; slew rate, and tile entire visible sky outside of Sun/Moon
  ;;;; constraints, maybe multiple times depending on sky exposure.
  ;;;; For each pass move the tile slightly to fill in the IRT
  ;;;; exposure.

  ;;;; For both of these
  ;;;;   - make exposure maps, color coding WFI & IRT
  ;;;;   - make animated gif of map filling in
  
  ;;;; find out (from Jeff) sensitivity, and compare IRT to 2MASS, and
  ;;;; WFI to ROSAT

  ;;;; other huge benefit is our own catalogs for difference imaging

  afst=mrdfits('~/Lobster/Swift_2015_afst.fits',1)
  w=where(strpos(afst.target,'saa') eq -1)
  afst=afst[w]
;  w=where(afst.time ge 2015.+(94/365.) and afst.time le 2015+(95./365))
;  afst=afst[w]
;  add='_1day2'
;  addlabel='1 day '
;  add='_1month'
;  addlabel='1 month '
  add='_1week2'
  addlabel='1 week '
  w=where(afst.time ge 2015.5 and afst.time le 2015.5+7/365.)
  afst=afst[w]
;  add='_1year'
;  addlabel='1 year '
  n=n_elements(afst)

;  map_set,/aitoff,/grid
;  w=where(afst.time le 2015.00274,n)
;  n=round(n/365.)
  for i=0,n-1 do begin
;     plots,afst[i].ra,afst[i].dec,psym=1
     ;; wfi
     if keyword_set(wfi) then begin
        rotate_box,0,xx,yy,xc=afst[i].ra,yc=afst[i].dec,xsize=38,ysize=38,bin=0.5
;        rotate_box,afst[i].roll,xx,yy,xc=afst[i].ra,yc=afst[i].dec,xsize=38,ysize=38,bin=0.5
        nside=64
     endif 
     ;; irt
     if keyword_set(irt) then begin
        rotate_box,afst[i].roll,xx,yy,xc=afst[i].ra,yc=afst[i].dec,xsize=1,ysize=1,bin=0.5
        nside=64
     endif 

;     print,afst[i].roll
;     oplot,xx,yy,psym=1,color=!blue

;     rotate_box,0,xx,yy,xc=afst[i].ra,yc=afst[i].dec,xsize=10,ysize=45,bin=0.5
;     oplot,xx,yy,psym=1,color=!red

;     k=get_kbrd(10)
;     if k eq 's' then stop
     add_maps,xx,yy,map,add=afst[i].duration*86400.,nside=nside
;     if i mod 100 eq 0 then ;print,afst[i].date
  endfor 

  w=where(map eq 0)
  map[w]=1.
  if keyword_set(irt) then begin
     mollview,map,title=addlabel+'IRT-driven IRT pointings',/log,png='IRT_swift_pointings'+add+'.png'
     mwrfits,map,'irt_swift_points_expomap'+add+'.fits',/create
  endif 
  if keyword_set(wfi) then begin
     mollview,map,title=addlabel+'IRT-driven WFI pointings',png='WFI_swift_pointings'+add+'.png'
     mwrfits,map,'wfi_swift_points_expomap'+add+'.fits',/create
  endif 

stop
return
end 

pro sky_survey_sim,irt=irt,wfi=wfi

  ;;; need array of ra/dec 
  nside=64
  npix=nside2npix(nside)
  map=lonarr(npix)
  pix2ang_nest,nside,lindgen(npix),theta,phi
  ra=360.-phi*!radeg
  dec=(0.5*!pi-theta)*!radeg

  ;; loop over daily sky with sun/moon constraints removed
  ;; need to place WFI tiles not overlapping, by choosing subset of
  ;; IRT tiles

;  npoint=??
;  exposure=86400.*365.*0.85/npoint

  if keyword_set(irt) then begin 
     for i=0L,n_elements(ra)-1 do begin 
;     rotate_box,0,xx,yy,xc=ra[i],yc=dec[i],xsize=1,ysize=1,bin=0.5
;     add_maps,xx,yy,map,add=1.
        map[i]=map[i]+1
     endfor 

     mwrfits,map,'irt_irt_survey_points_expomap.fits',/create
     mollview,map,title='IRT Pointings in IRT Sky Survey',png='IRT_IRT_survey_pointings.png' ;,max=max(map[1:*])

  endif 

  if keyword_set(wfi) then begin 
     for i=0L,n_elements(ra)-1 do begin 
        rotate_box,0,xx,yy,xc=ra[i],yc=dec[i],xsize=38,ysize=38,bin=0.5
        add_maps,xx,yy,map,add=1.,nside=64
;        mollview,map
;        k=get_kbrd(10)
;        if k eq 's' then stop
     endfor 
     
     mwrfits,map,'wfi_irt_survey_points_expomap.fits',/create

     mollview,map,title='WFI Pointings in IRT Sky Survey',png='WFI_IRT_survey_pointings.png' ;,max=max(map[1:*])
  endif 


return
end 

pro sky_survey_plots

  cd,'~/Lobster/TAP/'
  
goto,skip
  irt=mrdfits('irt_swift_points_expomap.fits',0)
  nirt=n_elements(irt)*1.

  begplot,name='irt_swift_pointings_hist.ps',/land,/color
  plot,[-1,6],[0,nirt*0.8],/nodata,xrange=[-1,6],/xsty,xtickname=['10!U-1','1','10','100','10!U3','10!U4','10!U5','10!U6'],xtitle='Exposure (s)',ytitle='Sky Fraction',yrange=[0,0.03*nirt],ytickv=[0,0.01,0.02,0.03]*nirt,ytickname=ntostr([0,0.01,0.02,0.03],4),charsize=2,yticks=3
  plothist,alog10(irt),bin=0.1,x,y,/overplot,/fill,fcolor=!blue
  w=where(irt eq 1,nw)
  print,nw*1./nirt
  endplot
  ps2pdf,'irt_swift_pointings_hist.ps'
  


  wfi=mrdfits('wfi_swift_points_expomap.fits',0)
  nwfi=n_elements(wfi)*1.

  begplot,name='wfi_swift_pointings_hist.ps',/land,/color
  plot,[1e4,1e7],[0,nwfi*0.3],/nodata,xrange=[5,7],/xsty,yrange=[0,nwfi*0.3],xtickv=[4,5,6,7],xtickname=['10!U4','10!U5','10!U6','10!U7'],xticks=3,xtitle='Exposure (s)',ytitle='Sky Fraction',ytickv=[0,0.1,0.2,0.3]*nwfi,ytickname=ntostr([0,0.1,0.2,0.3],3),charsize=2,yticks=3,yminor=9
  plothist,alog10(wfi),bin=0.1,x,y,/overplot,/fill,fcolor=!blue
  for j=4.,7 do begin 
     for i=1.,10 do begin
        xt=alog10(i*10^(j))
        oplot,[xt,xt],[0,nwfi*0.006]
;        print,i,j,i*10^j,xt
;        print,[10^(i/30.),10^(i/30.)]+4+j
     endfor 
  endfor 
  oplot,[5,5],[0,0.015*nwfi]
  oplot,[6,6],[0,0.015*nwfi]
  w=where(wfi eq 1,nw)
  print,nw*1./nwfi
  endplot
  ps2pdf,'wfi_swift_pointings_hist.ps'

skip:
;stop

;  map_set,/aitoff,grid=30

;  ra=findgen(18)*20.+5.
;  dec=[replicate(67.5,18)]
  ra=0
  dec=0
;  d=[72,36,0,-36,-71]
  numdec=7.
  d=180/(numdec-1)*(numdec-1-findgen(numdec-1))-15-90
  for i=0,n_elements(d)-1 do begin 
;     ra=[ra,findgen(i+1)/(i+1.)*360.]
;     dec=[dec,replicate(84.99-i*10.,i+1)]
     npoint=round(360*cos(d[i]*!dtor)/30.+0.5)
     dec=[dec,replicate(d[i],npoint)]
     ra=[ra,360./npoint*findgen(npoint)]
;     print,findgen(i+1)/(i+1.),replicate(90-i*10.,i+1)
     print,npoint
  endfor 

;  ra=[0,findgen(2)/2.,findgen(3)/3.,findgen(4)/4.,findgen(5)/5.]*360.
;  dec=[90,80,80,70,70,70,60,60,60,60,50,50,50,50,50]
  ra=ra[1:*]
  dec=dec[1:*]
;  ra=[ra,reverse(ra)]
;  dec=[dec,reverse(-dec)]

;  for i=1,2 do begin 
;     ra=[ra,ra+i*45]
;     dec=[dec,dec]
;  endfor 
  w=where(ra gt 360.)
  ra[w]=ra[w]-360.

  n=n_elements(ra)

  print,ra,dec
stop
  
;goto,survey
  for i=0,n-1 do begin 
     rotate_box,0.,xx,yy,xc=ra[i],yc=dec[i],xsize=38,ysize=38,bin=0.5

     add_maps,xx,yy,map,add=1.,nside=64
;     for j=0,9 do begin 
;        add_maps,xx+36*j,yy,map,add=1.,nside=64
;        add_maps,xx+36*j,-yy,map,add=1.,nside=64
;     endfor 
  endfor 
  map=map+1
  mollview,map,title='WFI Sky Survey',png='WFI_survey_pointings.png'
  mwrfits,map,'wfi_survey_points_expomap.fits',/create

  for i=0,n-1 do begin 
     rotate_box,0.,xx,yy,xc=ra[i],yc=dec[i],xsize=1.,ysize=1.,bin=0.5

     add_maps,xx,yy,map,add=1.,nside=64

;     for j=0,9 do begin 
;        add_maps,xx+36*j,yy,map,add=1.,nside=64
;        add_maps,xx+36*j,-yy,map,add=1.,nside=64
;     endfor 

  endfor 
  mollview,map,title='IRT Pointings in WFI Sky Survey',png='IRT_WFI_survey_pointings.png',max=max(map[1:*])
  mwrfits,map,'IRT_WFI_survey_points_expomap.fits',/create
  survey:
  
;  goto,skip2
  wfi=mrdfits('wfi_survey_points_expomap_1day.fits',0)
  nwfi=n_elements(wfi)*1.

  begplot,name='wfi_survey_pointings_hist.ps',/land,/color
  plot,[0,30],[0,nwfi*0.3],/nodata,xrange=[0,30],/xsty,yrange=[0,nwfi*0.5],xtitle='Exposure (arbitrary units)',ytitle='Sky Fraction',ytickv=[0,0.1,0.2,0.3,0.4,0.5]*nwfi,ytickname=ntostr([0,0.1,0.2,0.3,0.4,0.5],3),charsize=2,yticks=5,yminor=9
  plothist,wfi,bin=1,x,y,/overplot,/fill,fcolor=!blue
  endplot
  ps2pdf,'wfi_survey_pointings_hist.ps'
  w=where(wfi eq 0,nw)
  print,nw*1./nwfi

goto,skip2
  irt=mrdfits('irt_wfi_survey_points_expomap.fits',0)
  nirt=n_elements(irt)*1.

  begplot,name='irt_wfi_survey_pointings_hist.ps',/land,/color
  plot,[0,30],[0,nirt*0.3],/nodata,xrange=[0,5],/xsty,yrange=[0,nirt*1.0],xtitle='Exposure (arbitrary units)',ytitle='Sky Fraction',ytickv=[0,0.2,0.4,0.6,0.8,1.0]*nirt,ytickname=ntostr([0,0.2,0.4,0.6,0.8,1.0],3),charsize=2,yticks=6,yminor=9
  plothist,irt,bin=1,x,y,/overplot,/fill,fcolor=!blue
  endplot
  ps2pdf,'irt_wfi_survey_pointings_hist.ps'
  w=where(irt eq 0,nw)
  print,nw*1./nirt
  skip2:

  irt=mrdfits('irt_irt_survey_points_expomap.fits',0)
  irt2=irt
  irt2[0]=0
  nirt=n_elements(irt)
  mollview,irt2,title='IRT Pointings in IRT Sky Survey',png='IRT_IRT_survey_pointings.png';,max=max(map[1:*])
  begplot,name='irt_irt_survey_pointings_hist.ps',/land,/color
  plothist,irt2,/fill,fcolor=!blue,yrange=[0,5e4],ytickv=[0,0.2,0.4,0.6,0.8,1.0]*nirt,ytickname=ntostr([0,0.2,0.4,0.6,0.8,1.0],3),yticks=5,xtitle='Exposure (arbitrary units)',ytitle='Sky Fraction',charsize=2
  endplot
  ps2pdf,'irt_irt_survey_pointings_hist.ps'

  wfi=mrdfits('wfi_irt_survey_points_expomap.fits')
  nwfi=n_elements(wfi)*1.
  begplot,name='wfi_irt_survey_pointings_hist.ps',/land,/color
  plot,[0,1],[0,nwfi*0.3],/nodata,xrange=[400,1400],/xsty,yrange=[0,nwfi*0.5],xtitle='Exposure (arbitrary units)',ytitle='Sky Fraction',ytickv=[0,0.1,0.2,0.3,0.4,0.5]*nwfi,ytickname=ntostr([0,0.1,0.2,0.3,0.4,0.5],3),charsize=2,yticks=5,yminor=9
  plothist,wfi,x,y,/overplot,/fill,fcolor=!blue,bin=30
  endplot
  ps2pdf,'wfi_irt_survey_pointings_hist.ps'
  w=where(wfi eq 0,nw)
  print,nw*1./nwfi


stop
  return
end 

pro roll

  n=18
  ra=findgen(n)*20.+1.
  dec=fltarr(n)
  sunra=0.
  sun_angle=ra-sunra
  roll_constraint=10.

  optimum_roll=45.
  roll_range=fltarr(2,n)
  roll_range[0,*] = optimum_roll + roll_constraint / sin(sun_angle*!dtor)
  roll_range[1,*] = optimum_roll - roll_constraint / sin(sun_angle*!dtor)
  w=where(roll_range gt 360)
  roll_range[w]=roll_range[w]-360.

  afst=mrdfits('~/Lobster/Swift_2015_afst.fits',1)
  

  begplot,name='roll_range.ps',/land,/color
  map_set,/aitoff,/grid

  plotsym,0,2,/fill
  plots,sunra,0.,psym=8,color=!yellow

  plotsym,0,2
  plots,sunra,0.,psym=8,thick=2

  for i=0,n-1 do begin
     rotate_box,roll_range[0,i],xx0,yy0,xc=ra[i],yc=dec[i],xsize=38.,ysize=38.,bin=0.5
     rotate_box,roll_range[1,i],xx1,yy1,xc=ra[i],yc=dec[i],xsize=38.,ysize=38.,bin=0.5

     oplot,xx0,yy0,color=!blue
     oplot,xx1,yy1,color=!red
;     k=get_kbrd(10)
;     if k eq 's' then stop
  endfor 
  endplot
  ps2pdf,'roll_range.ps'



stop
return
end 
