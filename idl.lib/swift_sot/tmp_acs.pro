pro tmp_acs,scdate,scra,scdec,quick=quick,ps=ps
  
;;   readcol,'ACS_Anomaly_07247_2.csv',time,dec,b1,b2,b3,b4,b5,ra,roll,format='(a,d,d,d,d,d,d,d)'
  
;;   openw,lun,'dtas_SACSRA_new.dat',/get_lun
;;   printf,lun,'********************'
;;   for i=0,n_elements(ra)-1 do printf,lun,time[i]+','+ntostr(ra[i])
;;   close,/all
  
;;   openw,lun,'dtas_SACSDEC_new.dat',/get_lun
;;   printf,lun,'********************'
;;   for i=0,n_elements(dec)-1 do printf,lun,time[i]+','+ntostr(dec[i])
;;   close,/all
  
;;   openw,lun,'dtas_SACSROLL_new.dat',/get_lun
;;   printf,lun,'********************'
;;   for i=0,n_elements(roll)-1 do printf,lun,time[i]+','+ntostr(roll[i])
;;   close,/all
  
  if keyword_set(ps) then begplot,name='crazy_slews.ps',/land,/color
  if n_elements(scdate) eq 0 then begin 
;     readcol,'dtas_SACSRA_new.dat',scdate,scra,format='(a,d)',skip=2,delim=','
;     readcol,'dtas_SACSDEC_new.dat',scdate,scdec,format='(a,d)',skip=2,delim=','
     readcol,'ACS_Anomaly_07247_2.csv',scdate,scdec,b1,b2,b3,b4,b5,scra,delim=',',format='(a,d,i,i,i,i,i,d)'
  endif 
  !p.background=!white
  !p.color=!black
  !p.thick=2
  plotsym,0,/fill,1
  nbuf=n_elements(scdate)
  sctime=dblarr(nbuf)
  for i=0,nbuf-1 do sctime[i]=date2met(scdate[i])
  tstart=date2met('2007-247-18:48:00')
  tstop=date2met('2007-247-18:56:00') 
  map_set,/aitoff,/grid
  daystart=date2met('2007-247-00:00:00')
  tot=round(tstop-tstart)
  ydn2md,2007,247,month,day
  partday=19
  jdcnv,2007,month,day,partday,jd
  sunpos,jd,sunra,sundec
  moonpos,jd,moonra,moondec
  plots,-1.*sunra,sundec,psym=8,color=!red,symsize=3
  plots,-1.*moonra,moondec,psym=8,color=!blue,symsize=3
  
  
  cir=create_struct('color',0L)
  ;;SUN
  color = !red                  ;(cir.color+32) mod 256
  ang = 2*!PI*findgen(49)/48.   ;Get position every 5 deg
  psize=45.
  xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
  xarr=xarr+sunra  &  yarr=yarr+sundec
  xarr2=fltarr(49)
  yarr2=fltarr(49)
  for ct=0,48 do begin
     temp=ll_arc_distance([-1.*sunra,sundec]*!pi/180.,psize*!pi/180.,ct*7.5*!pi/180.)
     xarr2(ct)=temp(0)*180./!pi
     yarr2(ct)=temp(1)*180./!pi
  endfor
  
  for i=1,48 do begin
     cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]],/nowset,color=color
  endfor 
  
  ;;MOON
  color = !blue                  ;(cir.color+32) mod 256
  ang = 2*!PI*findgen(49)/48.    ;Get position every 5 deg
  psize=21.
  xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
  xarr=xarr+moonra  &  yarr=yarr+moondec
  xarr2=fltarr(49)
  yarr2=fltarr(49)
  for ct=0,48 do begin
     temp=ll_arc_distance([-1.*moonra,moondec]*!pi/180.,psize*!pi/180.,ct*7.5*!pi/180.)
     xarr2(ct)=temp(0)*180./!pi
     yarr2(ct)=temp(1)*180./!pi
  endfor
  for i=1,48 do begin
     cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]],/nowset,color=color
  endfor 
  
  
  ;;SLEW PATH
  b=1
  for i=0,tot,b do begin
     t=tstart+i
     w=where(sctime ge t and sctime le t+b,nw)
     if nw gt 0 then begin 
        plots,-1.*scra[w],scdec[w],thick=1,color=!black
        if not keyword_set(quick) then wait,0.01
        if i mod 60 eq 0 then print,met2date(sctime[w[0]])
     endif 
  endfor 
  
  dist=0.
  mdist=0.
  btime=0.
  for i=0,tot,b do begin
     t=tstart+i
     w=where(sctime ge t and sctime le t+b,nw)
     partday=(sctime[w[0]]-daystart)/3600.
     jdcnv,2007,month,day,partday,jd
     moonpos,jd,moonra,moondec
;     plots,-1.*moonra,moondec,psym=8,color=!blue,symsize=3
;     stop
     if nw gt 0 then begin 
        for j=0,nw-1 do begin
           p1=[sunra,sundec]
           p2=[scra[w[j]],scdec[w[j]]]
           p3=[moonra,moondec]
           
           p1r = p1 * !dtor     ;To radians
           p2r = p2 * !dtor
           p3r = p3 * !dtor

           twopi = 2 * !pi
           dlon = twopi + p2r(0) - p1r(0)        ;delta longitude
           while dlon gt !pi do dlon = dlon - twopi ;to -pi to +pi
           cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
           d = acos(cosd)*180.0/!pi ;Angular separation in degrees
           dist=[dist,d]
           if d lt 45. then begin 
              color=!green 
              btime=btime+b*1./nw*1.
;              print,met2date(sctime[w[j]]),partday
           endif else color=!yellow
           
           ;;moon dist
           dlon = twopi + p2r(0) - p3r(0)        ;delta longitude
           while dlon gt !pi do dlon = dlon - twopi ;to -pi to +pi
           cosd = sin(p3r(1))*sin(p2r(1)) + cos(p3r(1))*cos(p2r(1))*cos(dlon)
           md = acos(cosd)*180.0/!pi ;Angular separation in degrees
           mdist=[mdist,md]
           
           plots,-1.*scra[w[j]],scdec[w[j]],thick=2,color=color,psym=1
;        k=get_kbrd(10)
;        print,sctime[w[j]]-date2met('2007-247-18:50:00')
           if not keyword_set(quick) then wait,0.02
        endfor 
;     plots,-1.*scra[w],scdec[w],thick=2,color=color
;     wait,0.05
        if i mod 60 eq 0 then print,met2date(sctime[w[0]])
     endif 
  endfor 
  dist=dist[1:*]
  mdist=mdist[1:*]
  print,min(dist),min(mdist)
  print,btime
  if keyword_set(ps) then endplot
stop  
end 
