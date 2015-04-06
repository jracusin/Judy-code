pro rotate_roll,ra,dec,ra1,dec1,ra2,dec2,theta,ra1p,dec1p,ra2p,dec2p
  
  cost=cos(theta)
  sint=sin(theta)
  vect,dec,ra,v
  vect,dec1,ra1,v1
  vect,dec2,ra2,v2
  
  M=[[cost+(1-cost)*v[0]^2,(1-cost)*v[0]*v[1]-sint*v[2],(1-cost)*v[0]*v[2]+sint*v[1]],$
     [(1-cost)*v[1]*v[0]+sint*v[2],cost+(1-cost)*v[1]^2,(1-cost)*v[1]*v[2]-sint*v[0]],$
     [(1-cost)*v[2]*v[0]-sint*v[1],(1-cost)*v[2]*v[1]+sint*v[0],cost+(1-cost)*v[2]^2]]
    
  v1p=M#v1
  v2p=M#v2
  
  elaz2,v1p,dec1p,ra1p
  elaz2,v2p,dec2p,ra2p
  
  return
end 

pro real_slew,ppst,sctime,scra,scdec,scroll
  
  syear=strmid(ppst[0].begdate,0,4)
  startday=strmid(ppst[0].begdate,5,3)
  eyear=strmid(ppst[n_elements(ppst)-1].enddate,0,4)
  endday=strmid(ppst[n_elements(ppst)-1].enddate,5,3)
  if startday eq endday then endday=endday+1
  ydn2md,syear,startday,smonth,sday
  ydn2md,eyear,endday,emonth,eday
  s=' '
  if not exist('dtas_SACSRA.dat') then $
     spawn,'get_dtas.py '+ntostr(syear)+s+ntostr(smonth)+s+ntostr(sday)+s+ntostr(eyear)+s+ntostr(emonth)+s+ntostr(eday)
  readcol,'dtas_SACSRA.dat',scdate,scra,format='(a,d)',skip=2,delim=','
  readcol,'dtas_SACSDEC.dat',scdate,scdec,format='(a,d)',skip=2,delim=','
  readcol,'dtas_SACSROLL.dat',scdate,scroll,format='(a,d)',skip=2,delim=','
  nbuf=n_elements(scdate)
  if nbuf eq 0 then begin
     print,'No real data yet.  CHILE is not a crystal ball!'
     return
  endif 
  sctime=lonarr(nbuf)
  for i=0L,nbuf-1 do sctime[i]=date2met(scdate[i])
;  spawn,'rm dtas*dat'
  
  return
end 

pro calc_traj,ra1,dec1,ra2,dec2,theta,_extra=_extra
  
  dely=dec2-dec1;dist_overpole(dec2,dec1,pole=90)
  delx=dist_overpole(ra1,ra2)
  
  theta=atan(dely,delx)*!radeg
  r=sqrt(dely^2+delx^2)
  
  return 
end 
  
pro draw_circ,ra,dec,rad,_extra=_extra
  
  ang = 2*!PI*findgen(25)/24.   ;Get position every 15 deg

  xarr = rad*cos(ang)  &  yarr = rad*sin(ang)
  xarr=xarr+ra  &  yarr=yarr+dec
  xarr2=fltarr(25)
  yarr2=fltarr(25)
  
  for ct=0,24 do begin 
     temp=ll_arc_distance([ra,dec]*!pi/180.,rad*!pi/180.,ct*30*!pi/180.) 
     if temp(0) ne temp(0) then temp(0)=0.  ;trick to check for NaNs and 0-fill
     if temp(1) ne temp(1) then temp(1)=0. 

     xarr2(ct)=temp(0)*180./!pi 
     yarr2(ct)=temp(1)*180./!pi 
     if ct gt 1 then begin  
        i=ct 
        dst=separation(xarr2[i-1],yarr2[i-1],xarr2[i],yarr2[i])/3600. 
        draw_arc,xarr2[i-1],yarr2[i-1],xarr2[i],yarr2[i],dst,_extra=_extra,/data 
     endif  
  endfor 
  
  
  return
end 
  
pro draw_arc,ra1,dec1,ra2,dec2,theta,_extra=_extra
  
  n = fix(theta)
  p1=[ra1,dec1] & p2=[ra2,dec2]
  
  p1r = p1 * !dtor	;To radians
  p2r = p2 * !dtor
  lon = [p1r(0), p2r(0)]
  lat = [p1r(1), p2r(1)]
  x = cos(lat) * cos(lon)		;To xyz space
  y = cos(lat)* sin(lon)
  z = sin(lat)
  u = [x[0],y[0],z[0]]		; 3-vectors
  v = [x[1],y[1],z[1]]
  
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
  
  plots,lon,lat,_extra=_extra
  
  return
end 

pro chile_light,ppstfile,ppst,antisun=antisun,onlyat=onlyat,everyslew=everyslew,$
                afst=afst,realslew=realslew,ephfile=ephfile,ephem=eph
  
  simpctable
  !p.background=!white
  !p.color=!black
  
  if n_elements(ephfile) eq 0 then find_eph,ppstfile,ephfile
  print,'Using Ephemeris file: ',ephfile
  if n_elements(eph) eq 0 then eph=readeph(ephfile)
  
  ;;constraint definitions (same as S/C)
;  econ=66.+28. ;or 67????  ; calculate yourself
  econ0=28.
  erad = 6378.14 ;km
  mcon=21.
  scon=45.
  ascon=20.
  
  print
  print,'Checking for constraint traps'
  if keyword_set(antisun) then print,'Checking for close Anti-Sun slews'
  print,'Using the following constraint values (same as S/C)'
  print,'      Earth    - 28 deg'
  print,'      Moon     - 21 deg'
  print,'      Sun      - 45 deg'
  print,'      Anti-Sun - 20 deg'
  print
  print,'Working in equatorial coordinates (not Ecliptic!)'
  print
  
  nslews=n_elements(ppst)
;  for i=0,nslews-1 do begin
  i=0
  k=''
  
  if keyword_set(realslew) then real_slew,ppst,sctime,scra,scdec,scroll
  openw,llun,'roll_crap.dat',/get_lun
  p=i
  while i lt nslews and k ne 'q' do begin 
  
     date1=str_sep(ppst[i].begdate,'-')
     date2=str_sep(date1[2],':')
     date=[date1[0],date1[1],date2[0],date2[1],date2[2]]
     partday=date[2]/24.+date[3]/24./60.+date[4]/24./3600.
     
  ;;get sun/moon/anti-sun positions at time of slew
     ydn2md,date[0],date[1],month,day
     jdcnv,date[0],month,day,partday*24.,jd

     sunpos,jd,sunra,sundec
     moonpos,jd,moonra,moondec,moondist
     antisunra=sunra+180.
     if antisunra gt 360. then antisunra=antisunra-360.
     antisundec=-1.*sundec
     
;     time=round(ppst[i].begtime/60.)*60. ;begtime or slewtime(slewbeg)????
     time=ppst[i].begtime
;     wt=where(round(time) eq eph.met)
     mint=min(abs(time-eph.met),wt)
     earthra=eph[wt].era
     earthdec=eph[wt].edec
     
     ;;calc moon parallax
     mooncoords=moon_parallax(moonra,moondec,moondist,eph[wt].x,eph[wt].y,eph[wt].z)
     moonra=mooncoords[0]
     moondec=mooncoords[1]
;     print,separation(moonra,moondec,moonra1,moondec1)/3600.
     
     slewfrom=i-1
     slewto=i
     if slewfrom eq -1 then begin ;if target is first in ppst, retreive first PPT End from ppst
        openr,lun,ppstfile,/get_lun
        ppstline=readline(lun,delim='|')
        free_lun,lun
        close,lun
        if not keyword_set(afst) then begin 
           ptargname=ppstline[3]
           pra=ppstline[7]
           pdec=ppstline[8]
           proll=ppstline[9]
        endif else begin
           ptargname=ppstline[1]
           pra=ppstline[10]
           pdec=ppstline[11]
           proll=ppstline[12]
        endelse 
     endif else begin
        ptargname=ppst[slewfrom].targname
        pra=ppst[slewfrom].ra
        pdec=ppst[slewfrom].dec
        proll=ppst[slewfrom].roll
     endelse 
;     if pra ne ppst[i].ra then begin 
        ;; predict slewpath from previous to current target
        theta_roll=dist_overpole(ppst[i].roll,proll)*!dtor
        predict_slew,pra,pdec,ppst[i].ra,ppst[i].dec,earthra,earthdec,sunra,sundec,moonra,moondec,ora,odec,tslew,/noplot,dostop=dostop,theta_roll=theta_roll
        ;; ora & odec are points along slew path
        ;; tslew is slew durration time (not very good prediction)
     
        ;;need to get position of earth at end of slew
;        slewendtime=round(time+round((tslew+30.)/60.)*60.) ;;or ppst[i].slewend?
        if ppst[i].slewend eq '' or ppst[i].slewend eq ppst[i].slewbeg then $
           slewendtime=round(time+tslew) else $
           slewendtime=date2met(ppst[i].slewend)
           ;slewendtime=round(time+round((tslew+30.)/60.)*60.) else $
        
        mint1=min(abs(slewendtime-eph.met),wt1)
;        wt1=where(slewendtime eq eph.met)
        earthra1=eph[wt1].era
        earthdec1=eph[wt1].edec
        
        ;;lets check for constraint traps
        dearth0=separation(ora,odec,earthra,earthdec)/3600.
        dearth1=separation(ora,odec,earthra1,earthdec1)/3600.
        srad=sqrt(eph[wt].x^2+eph[wt].y^2+eph[wt].z^2)
        ; doesn't work because it's not lat of eph but lat of pointing
;        a=6378.135
;        b=6356.750
;        lat=eph[wt].lat*!radeg
;        erad=a^2*b/(a^2*cos(lat)^2+b^2*sin(lat)^2)
        
        econ=asin(erad/srad)*!radeg+econ0 ;earth constraint at T0
        srad1=sqrt(eph[wt1].x^2+eph[wt1].y^2+eph[wt1].z^2)
        econ1=asin(erad/srad)*!radeg+econ0 ;earth constraint at T1
        
        wde=where(dearth1 lt econ1 or dearth0 lt econ,nwde) ;does slewpath go into constraint?
        
        ;;ecliptic plane
        elon=findgen(100)*3.65
        elat=fltarr(100)
        euler,elon,elat,era,edec,4
        
        ;;lets check for anti-sun slews
        asdist=separation(ora,odec,antisunra,antisundec)/3600.
        euler,antisunra,antisundec,aslon,aslat,4
        euler,ora,odec,olon,olat,4
        was=where(asdist lt ascon and abs(olat-aslat) lt 5,nwas)
        if not keyword_set(antisun) then nwas=0
        
        ;;only too/at slews
        at=0
        if keyword_set(onlyat) then begin
           if ptargname eq 'TOO/AT' or ppst[i].targname eq 'TOO/AT' or ptargname eq 'ToO' or ppst[i].targname eq 'ToO' or ptargname eq 'AT' or ppst[i].targname eq 'AT' then at=1 else at=0
           
        endif else at=1
        
        nto=n_elements(ora)-1
        if nto gt 10 then nto=10
        calc_traj,-1.*earthra,earthdec,-1.*earthra1,earthdec1,te
        calc_traj,-1.*ora[0],odec[0],-1.*ora[nto],odec[nto],to
        delt=abs(te-to)
        
        ;delt gt 60?
        if ((nwde gt 0 and delt gt 0) or nwas gt 0 and at) or keyword_set(everyslew) then begin ;defines bad slew
;        if ((nwde gt 0) or nwas gt 0 and at) or keyword_set(everyslew) then begin ;defines bad slew
           print,delt
           print,'Potential constraint trap '
           print,'Slewing from '+ptargname+' to '+ppst[i].targname
           print,'Slew begins: '+ppst[i].slewbeg
;           print,'Slew ends:   '+ppst[i].slewend+'  '+eph[wt1].date
           print,'Slew ends:   '+met2date(slewendtime);eph[wt1].date
           
           if keyword_set(antisun) then print,'Min Anti-Sun distance: '+ntostr(min(asdist))
           ;if nwde gt 0 then 
           print,min(dearth0),min(dearth1),econ,econ1
           
           map_set,/ait,/grid,glinethick=2,/noborder
           
           xyouts,10,35,ptargname+' to '+ppst[i].targname,/device
           xyouts,10,25,ppst[i].slewbeg,/device
           xyouts,10,15,eph[wt1].date,/device
           
           plotsym,0,/fill,1
           oplot,-1.*ora,odec,psym=8,symsize=0.8,color=!green ;plot slew path
           plots,-1.*moonra,moondec,psym=8,symsize=2 ;plot moon constraint
           draw_circ,-1.*moonra,moondec,mcon,thick=2
;           plots,-1.*moonra1,moondec1,psym=8,symsize=2,!grey ;plot moon constraint
;           draw_circ,-1.*moonra1,moondec1,mcon,thick=2,color=!grey
           
           plots,-1.*sunra,sundec,psym=8,color=!red,symsize=2 ;plot sun constraint
;           tvellipse2,scon,scon,-1.*sunra,sundec,color=!red,/data,thick=2
           draw_circ,-1.*sunra,sundec,scon,thick=2,color=!red
           plots,-1.*earthra,earthdec,psym=8,color=!blue,symsize=2 ;plot earth constraint
;           for j=1,econ,2 do tvellipse2,j,j,-1.*earthra,earthdec,color=!blue,/data
           for j=1,econ,3 do draw_circ,-1.*earthra,earthdec,j,color=!blue
           plots,-1.*earthra1,earthdec1,psym=8,color=!green,symsize=2 ;plot earth constraint at slew end
;           for j=1,econ,2 do tvellipse2,j,j,-1.*earthra1,earthdec1,color=!green,/data
           for j=1,econ,3 do draw_circ,-1.*earthra1,earthdec1,j,color=!green
           
           dist=separation(pra,pdec,ppst[i].ra,ppst[i].dec)/3600.
           draw_arc,-1.*pra,pdec,-1.*ppst[i].ra,ppst[i].dec,dist,color=!orange,thick=3
           
           vsym,5,/star,/fill
           plots,-1.*antisunra,antisundec,psym=8,color=!red,symsize=4 ;antisun
           
           xyouts,-pra-20,pdec-2,ptargname
           xyouts,-ppst[i].ra-20,ppst[i].dec-2,ppst[i].targname
           
           ;;calc ecliptic plane
           oplot,-1.*era,edec
           
           ;;dtas slews
           if keyword_set(realslew) then begin 
              
              w=where(sctime ge time and sctime le slewendtime,nw)
              plots,-1.*scra[w],scdec[w],thick=3,color=!magenta
              n=indgen(round(nw/2)+1)*2
              ww=w[n]
              nu=n_elements(ww)
              theta=dblarr(nu)
              rs=dblarr(4,nu)
              for u=0,nu-1 do begin
                 ra1=scra[ww[u]]-5.
                 ra2=scra[ww[u]]+5.
                 dec1=scdec[ww[u]]
                 dec2=dec1
                 theta[u]=scroll[ww[u]]*!dtor
                 rotate_roll,scra[ww[u]],scdec[ww[u]],ra1,dec1,ra2,dec2,theta[u],ra1p,dec1p,ra2p,dec2p
                 rs[0,u]=ra1p
                 rs[1,u]=dec1p
                 rs[2,u]=ra2p
                 rs[3,u]=dec2p
                 oplot,-1.*[ra1p,ra2p],[dec1p,dec2p],thick=2,color=!black
                 
              endfor 
              scdroll=dist_overpole(scroll[w[0]],scroll[w[nw-1]])
              scdroll2=0d
              for q=0,nw-2 do scdroll2=scdroll2+dist_overpole(scroll[w[q]],scroll[w[q+1]])
              droll=dist_overpole(proll,ppst[i].roll)
              pathdist=0d
              for q=0,n_elements(ora)-2 do begin
                 s=separation(ora[q],odec[q],ora[q+1],odec[q+1])/3600.
                 pathdist=pathdist+s
              endfor 
              print,'Delta roll (dtas,ppst) = '+ntostr(scdroll)+' '+ntostr(droll)
              print,'time,pathdist, roll rate = '+ntostr(tslew)+' '+ntostr(pathdist)+' '+ntostr(droll/tslew)+' '+ntostr((pathdist+droll)/tslew)
              euler,ppst[i].ra,ppst[i].dec,lat,lon,3
              printf,llun,ppst[i].begdate+' '+ntostr(scdroll)+' '+ntostr(droll)+' '+ntostr(tslew)+' '+ntostr(pathdist)+' '+ntostr(scdroll2)+' '+ntostr(lat)
           endif 

           print,'Type n for next or p for previous or i to image or q to quit: '
;           k='n'
           k=get_kbrd(10)
           print
           
           if k eq 's' then stop
           if k eq 'p' then i=p-1
           
           if k eq 'i' then begin  ;produce ps outputs of this plot
              begplot,name='slew_'+ppst[i].slewbeg+'.ps',/color,/land
              map_set,/ait,/grid,glinethick=2,/noborder
              
              xyouts,400,1600,ptargname+' to '+ppst[i].targname,/device,charsize=1
              xyouts,400,1200,ppst[i].slewbeg,/device,charsize=1
              xyouts,400,800,eph[wt1].date,/device,charsize=1
           
              plotsym,0,/fill,1
              oplot,-1.*ora,odec,psym=8,symsize=0.5,color=!green ;plot slew path
              plots,-1.*moonra,moondec,psym=8,symsize=2 ;plot moon constraint
              draw_circ,-1.*moonra,moondec,mcon,thick=4
           
              plots,-1.*sunra,sundec,psym=8,color=!red,symsize=2 ;plot sun constraint
              draw_circ,-1.*sunra,sundec,scon,color=!red,thick=4
              plots,-1.*earthra,earthdec,psym=8,color=!blue,symsize=2 ;plot earth constraint
              for j=1,econ,3 do draw_circ,-1.*earthra,earthdec,j,color=!blue,thick=1
              plots,-1.*earthra1,earthdec1,psym=8,color=!green,symsize=2 ;plot earth constraint at slew end
              for j=1,econ,3 do draw_circ,-1.*earthra1,earthdec1,j,color=!green,thick=1
              
              dist=separation(pra,pdec,ppst[i].ra,ppst[i].dec)/3600.
              draw_arc,-1.*pra,pdec,-1.*ppst[i].ra,ppst[i].dec,dist,color=!orange,thick=8
           
              vsym,5,/star,/fill
              plots,-1.*antisunra,antisundec,psym=8,color=!red,symsize=4 ;antisun
              
              xyouts,-pra-20,pdec-2,ptargname,charsize=1
              xyouts,-ppst[i].ra-20,ppst[i].dec-2,ppst[i].targname,charsize=1
           
              ;;calc ecliptic plane
              oplot,-1.*era,edec
              
              ;;dtas slews
              if keyword_set(realslew) then begin 
                 
                 w=where(sctime ge time and sctime le slewendtime)
                 plots,-1.*scra[w],scdec[w],thick=5,color=!magenta
                 
                 n=indgen(round(nw/2)+1)*2
                 ww=w[n]
                 nu=n_elements(ww)
                 theta=dblarr(nu)
                 rs=dblarr(4,nu)
                 for u=0,nu-1 do begin
                    ra1=scra[ww[u]]-5.
                    ra2=scra[ww[u]]+5.
                    dec1=scdec[ww[u]]
                    dec2=dec1
                    theta[u]=scroll[ww[u]]*!dtor
                    rotate_roll,scra[ww[u]],scdec[ww[u]],ra1,dec1,ra2,dec2,theta[u],ra1p,dec1p,ra2p,dec2p
                    rs[0,u]=ra1p
                    rs[1,u]=dec1p
                    rs[2,u]=ra2p
                    rs[3,u]=dec2p
                    oplot,-1.*[ra1p,ra2p],[dec1p,dec2p],thick=2,color=!black
                    
                 endfor 
                 scdroll=dist_overpole(scroll[w[0]],scroll[w[nw-1]])
                 scdroll2=0d
                 for q=0,nw-2 do scdroll2=scdroll2+dist_overpole(scroll[w[q]],scroll[w[q+1]])
                 droll=dist_overpole(proll,ppst[i].roll)
                 pathdist=0d
                 for q=0,n_elements(ora)-2 do begin
                    s=separation(ora[q],odec[q],ora[q+1],odec[q+1])/3600.
                    pathdist=pathdist+s
                 endfor 

              endif 
              endplot
              !p.background=!white
              !p.color=!black
           endif 
           
           p=i
        endif 

;     endif else k='n'           ; else print,'Slew-in-Place'
     i=i+1
;     if k eq 'p' then i=i-1 else i=i+1
  endwhile    
;  endfor  
  
  print,'No more potential bad slews'
  
  close,llun
  free_lun,llun
  ;;; read ephemeris
  ;;; progress through slews
  ;;;;; predict slewpath
  ;;;;; check constraint traps
  ;;; output potential bad slew info & plots
  ;;; (make webpage with them???)
  
  return
end 
