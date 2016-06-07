pro waveforms

  readcol,'~/Desktop/GW150914/waveform.dat',x,y
  y=y-mean(y)+1

  begplot,name='~/Desktop/GW150914/gbm_ligo_scenarios.ps',/color
;  plot,x,y
  x2=findgen(100)/100.
  y2=spline(x,y,x2)

  gx=findgen(100)/100.
  gy=replicate(1.,100)
  noise=(randomu(seed,100))*0.3+1
  gy[[50,51,52,53,54]]=[8.5,8.5,5,1.7,1.3]
  gy2=gy*noise

  gyrange=[0,10]
  yrange=[0,2]
  !p.multi=[0,2,4]
  plot,[0,1],gyrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,gx,gy2,color=!red,thick=3
  plot,[0,1],yrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,x2,(y2-1.)*3.+1,color=!blue,thick=3

  plot,[0,1],gyrange,/nodata,color=!white,xminor=0,yminor=0
  noise=(randomu(seed,100))*0.3+1
  oplot,gx,(((gy-1.)*0.1)+1.)*noise,color=!red,thick=3
  plot,[0,1],yrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,x2,(y2-1.)*3.+1,color=!blue,thick=3

  plot,[0,1],gyrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,gx,gy2,color=!red,thick=3
  noise=(randomu(seed,100))
  plot,[0,1],yrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,x2,y2*((noise-mean(noise))*0.15+1.),color=!blue,thick=3

  plot,[0,1],gyrange,/nodata,color=!white,xminor=0,yminor=0
  noise=(randomu(seed,100))*0.3+1
  oplot,gx,(((gy-1.)*0.1)+1.)*noise,color=!red,thick=3
  noise=(randomu(seed,100))
  plot,[0,1],yrange,/nodata,color=!white,xminor=0,yminor=0
  oplot,x2,y2*((noise-mean(noise))*0.15+1.),color=!blue,thick=3

  endplot

  spawn,'convert ~/Desktop/GW150914/gbm_ligo_scenarios.ps ~/Desktop/GW150914/gbm_ligo_scenarios.png'

stop


  return
end 

function transform_ra,ra,cra
  
  newra=ra-cra
  w=where(newra gt 180,nw)
  if nw gt 0 then newra[w]=newra[w]-360

  newra=ra
  return,newra
end

function transform_dec,dec,cdec

  newdec=dec-cdec
  w=where(newdec gt 90,nw)
  if nw gt 0 then newdec[w]=newdec[w]-180

  newdec=dec
  return,newdec
end 

pro readreg,file,ra,dec

  readcol,file,lines,format='(a)',delim='$'
  s1=strpos(lines[2],'(')
  s2=strpos(lines[2],')')
  t1=strmid(lines[2],s1+1,s2-s1-1)
  g=str_sep(t1,',')
  n=n_elements(g)
  ra=g[indgen(round(n/2.+0.5))*2]*1.
  dec=g[indgen(round(n/2.+0.5))*2+1]*1.

  return
end  

pro gw150914_press

  cd,'~/Desktop/GW150914'
  projection='Orthographic'
;  projection='Equirectangular'
;  projection='hammer'
;  projection='cartesian'

  hms2radec,08,22,04,-73,24,0,cra,cdec
  cra=123
  cdec=-40
;  cra=157.5
;  cdec=-30.
;  map_set,/ait,limit=[-cra-180,cdec-45,-cra+180,cdec+45],grid=30
;  map_set,/ait,grid=30
;  m=map(projection,label_show=0,color='grey',limit=[-90,-180,90,180],center_longitude=-cra);,limit=[-90,-90,90,90],center_latitude=cdec)
  if projection eq 'Orthographic' then m=map(projection,label_show=0,color='grey',limit=[-90,-180,90,180],center_longitude=-cra,center_latitude=cdec)
  if projection eq 'Equirectangular' then  m=map(projection,label_show=0,color='grey',center_longitude=-cra)
  if projection eq 'cartesian' then m=plot([0,0],[-90,90],xrange=[-180,180],yrange=[-90,90],/nodata,position=[0.05,0.25,0.95,0.75]) ;,xminor=0,yminor=0,xtickname=[' ',' '],ytickname=[' ',' '],/xstyle,/ystyle)

  gal_lon=indgen(37)*10.
  gal_lat=replicate(0,37)
  euler,gal_lon,gal_lat,galra,galdec,2
  galdec2=transform_dec(galdec,cdec)
  s=sort(-transform_ra(galra,cra))
  p0=plot(-transform_ra(galra[s],cra),galdec2[s],/overplot,color='red')

  cdec2=transform_dec(cdec,cdec)  
  p1=symbol(-transform_ra(cra,cra),cdec2,'+',sym_color='magenta',/overplot,/data,/current,/sym_filled,sym_size=3)

  readreg,'gbm_localization.reg',gra,gdec
  writecol,'gbm_localization.txt',gra,gdec,header='RA (deg),    Dec (deg)',delim=',  '
;  for i=0,n_elements(gra)-1 do oplot,[gra[i],gra[i+1]],[gdec[i],gdec[i+1]]
  nn=500
  gdec1=transform_dec(gdec,cdec)
  for j=0,1 do begin
     if j eq 0 then w=where(gdec1 lt 0)
     if j eq 1 then w=where(gdec1 gt 0)
     gra2=gra[w]
     gdec2=gdec1[w]
     if j eq 0 then begin 
        s=sort(-transform_ra(gra2,cra))
        gra2=gra2[s]
        gdec2=gdec2[s]
     endif else begin 
        gra2=gra2[0:n_elements(gra2)-2]
        gdec2=gdec2[0:n_elements(gdec2)-2]
     endelse 
     ind=rotate([[indgen(n_elements(gra2)/nn+1)*nn],[((indgen(n_elements(gra2)/nn+1))+1)*nn]],180)
     ind[1,n_elements(ind[0,*])-1]=n_elements(gra2)-1
     print,n_elements(gra2),ind
     for i=0,n_elements(ind[0,*])-1 do p2=plot(-transform_ra(gra2[ind[0,i]:ind[1,i]],cra),gdec2[ind[0,i]:ind[1,i]],/overplot)
  endfor 

;  readreg,'gbm_localization_noearth.reg',gera,gedec
;  for i=0,n_elements(gera)-nn,nn do p3=plot(-gera[i:i+nn],gedec[i:i+nn],symbol='.',color='red',/overplot,linestyle='none')

  readreg,'LALInference_skymap.reg',lra,ldec
  writecol,'LIGO_localization.txt',lra,ldec,header='RA (deg),    Dec (deg)',delim=',  '

  ldec1=transform_dec(ldec,cdec)
  for j=0,1 do begin 
     if j eq 0 then w=where(ldec1 lt -30)
     if j eq 1 then w=where(ldec1 gt -30)
     lra2=lra[w]
     ldec2=ldec1[w]
     lra2=lra2[0:n_elements(lra2)-2]
     ldec2=ldec2[0:n_elements(ldec2)-2]

     ind=rotate([[indgen(n_elements(lra2)/nn+1)*nn],[((indgen(n_elements(lra2)/nn+1))+1)*nn]],180)
     ind[1,n_elements(ind[0,*])-1]=n_elements(lra2)-1
     print,n_elements(lra2),ind
     for i=0,n_elements(ind[0,*])-1 do p2=plot(-transform_ra(lra2[ind[0,i]:ind[1,i]],cra),ldec2[ind[0,i]:ind[1,i]],/overplot,color='green')
  endfor 
;  for i=0,n_elements(lra)-nn,n_elements(lra)/nn do p4=plot(-lra[i:i+nn],ldec2[i:i+nn],symbol='.',color='green',/overplot,linestyle='none')

  hms2radec,14, 39, 36.204,-60, 50, 08.23,acen_ra,acen_dec
  hms2radec,05, 14, 32.27210,  -08, 12, 05.8981,rigel_ra,rigel_dec
  hms2radec,06, 45, 08.91728,  -16, 42, 58.0171,sir_ra,sir_dec
  hms2radec,06, 23, 57.10988, -52, 41, 44.3810,can_ra,can_dec
  hms2radec,12., 26., 35.871, -63., 05, 56.58,acrux_ra,acrux_dec

  ras=[acen_ra,rigel_ra,sir_ra,can_ra,acrux_ra]
  decs=[acen_dec,rigel_dec,sir_dec,can_dec,acrux_dec]
  name=['Alpha Cen','Rigel','Sirius','Canopus','Acrux']
  writecol,'stars.txt',ras,decs,name,header='RA (deg),    Dec (deg),    Name',delim=',  '

  decs2=transform_dec(decs,cdec)
  p5=symbol(-transform_ra(ras,cra),decs2,symbol='+',sym_color='blue',/overplot,/data,/sym_filled)

  m.save,'GW150914_'+projection+'.pdf',/landscape,height=9
  m.close

;Alpha Cen          14 39 36.204      -60 50 08.23 
;Rigel                   05 14 32.27210  -08 12 05.8981
;Sirius                  06 45 08.91728  -16 42 58.0171
;Canopus             06 23 57.10988 -52 41 44.3810
;Acrux                  12 26 35.871     -63 05 56.58

  stop
end 
