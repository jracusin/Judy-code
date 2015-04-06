function min_distance,planera,planedec,ra,dec,m=m
  
  n=n_elements(planera)
  d=dblarr(n)
  d=separation(ra,dec,planera,planedec)/3600.
;  for i=0,n-1 do begin 
;     gcirc,0,ra*!dtor,dec*!dtor,planera[i]*!dtor,planedec[i]*!dtor,d0
;     d[i]=d0*!radeg
;  endfor 
  mind=min(d,m)
  return,mind
end 

pro compare_dist2plane,file,eph=eph,title=title,sctime=sctime,scmom=scmom
  
  if n_elements(file) eq 0 then file=pickfile(filter='*ST_*txt')
  apos=strpos(file,'AFST')
  if apos[0] eq -1 then read_ppst,file,ppst else read_afst,file,ppst
  n=n_elements(ppst)
  if n_elements(eph) eq 0 then begin 
     ephfile=get_ephfile(file)
     print,'Reading ephemeris: '+ephfile
     eph=readeph(ephfile)
  endif
  
  startdate=met2date_judy(ppst[0].begtime)
  ydn2md,startdate[0],startdate[1],month0,day0
  year0=ntostr(startdate[0],4)
  if month0 lt 10 then month0='0'+ntostr(fix(month0)) else month0=ntostr(fix(month0))
  if day0 lt 10 then day0='0'+ntostr(fix(day0)) else day0=ntostr(fix(day0))
  stopdate=met2date_judy(ppst[n-1].endtime)
  ydn2md,stopdate[0],stopdate[1],month1,day1
  year1=ntostr(stopdate[0],4)
  if month1 lt 10 then month1='0'+ntostr(fix(month1)) else month1=ntostr(fix(month1))
  if day1 lt 10 then day1='0'+ntostr(fix(day1)) else day1=ntostr(fix(day1))
;  dfile='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_CFG_TOTALMOM.dat'
  dfile1='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_1.dat'
  dfile2='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_2.dat'
  dfile3='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_3.dat'
  s=' '
  print,dfile1;,dfile2,dfile3
  if not exist(dfile1) then begin
     com='get_dtas_par.py -s '+year0+s+month0+s+day0+s+year1+s+month1+s+day1
     print,com
     spawn,com
  endif 
  if n_elements(sctime) eq 0 and n_elements(scmom) eq 0 then begin 
     readcol,dfile1,scdate,scmom1,format='(a,d)',skip=2,delim=','
     readcol,dfile2,scdate,scmom2,format='(a,d)',skip=2,delim=','
     readcol,dfile3,scdate,scmom3,format='(a,d)',skip=2,delim=','
     scmom=sqrt(scmom1^2+scmom2^2+scmom3^2)
     sctime=dblarr(n_elements(scdate))
     for i=0L,n_elements(scdate)-1 do sctime[i]=date2met(scdate[i])
  endif 
  
  magdist=dblarr(n) & orbdist=dblarr(n)
;  time=dblarr(n) & tot=dblarr(n) & targtime=dblarr(n)
;  cumdist_mag=dblarr(n) & cumdist_orb=dblarr(n)
  nmin=(max(ppst.endtime)-min(ppst.begtime))/60.
  minutes=dindgen(nmin)
  magdist_min=dblarr(nmin) & orbdist_min=dblarr(nmin)
  starttime=ppst[0].begtime
  ndist=dblarr(n) & sdist=dblarr(n) & ndist_min=dblarr(nmin) & sdist_min=dblarr(nmin)
  dist=dblarr(n) & dist_min=dblarr(nmin)
  for i=0L,n-1 do begin 
     
     midtime=met2date((ppst[i].endtime-ppst[i].begtime)/2.+ppst[i].begtime)
     check_mag_plane,midtime,planera,planedec,nmagra,nmagdec,smagra,smagdec
     check_orbit_plane,midtime,eph,orbra,orbdec,norbnormra,norbnormdec,sorbnormra,sorbnormdec
     magdist[i]=min_distance(planera,planedec,ppst[i].ra,ppst[i].dec,m=m)
;     if planedec[m] lt ppst[i].dec then magdist[i]=-1.*magdist[i]
     if n_elements(orbra) gt 0 then orbdist[i]=min_distance(orbra,orbdec,ppst[i].ra,ppst[i].dec,m=m)
;     if orbdec[m] lt ppst[i].dec then orbdist[i]=-1.*orbdist[i]
     ndist[i]=separation(norbnormra,norbnormdec,nmagra,nmagdec)/3600.
     sdist[i]=separation(sorbnormra,sorbnormdec,smagra,smagdec)/3600.
;     gcirc,0,norbnormra*!dtor,norbnormdec*!dtor,nmagra*!dtor,nmagdec*!dtor,d0
;     ndist[i]=d0*!radeg
;     gcirc,0,sorbnormra*!dtor,sorbnormdec*!dtor,smagra*!dtor,smagdec*!dtor,d0
;     sdist[i]=d0*!radeg
     
     w=where(eph.met ge ppst[i].begtime and eph.met lt ppst[i].endtime)
     scmomra=mean(eph[w].ra)
     scmomdec=mean(eph[w].dec)
     
     scorbdist=min_distance(orbra,orbdec,ppst[i].ra,ppst[i].dec,m=worb)
     if worb+2 lt n_elements(orbra) then norb=worb+2 else norb=worb-2
     scmomang=atan(orbra[worb]-orbra[norb],orbdec[worb]-orbdec[norb])*!radeg
     
     magorbdist=min_distance(planera,planedec,ppst[i].ra,ppst[i].dec,m=m)
     if m+2 lt n_elements(planera) then nm=m+2 else nm=m-2
     magang=atan(planera[m]-planera[nm],planedec[m]-planedec[nm])*!radeg
     dist[i]=abs(abs(scmomang)-abs(magang))
;     stop
;     momdist=abs(90.-separation(nmagra,nmagdec,scmomra,scmomdec)/3600.)
;     planedist=separation(scmomra,scmomdec,planera,planedec)/3600.
;     minplanedist=min(planedist-momdist,m)
;     magmomra=planera[m]
;     magmomdec=planedec[m]    
;     nx=n_elements(orbra) & ny=n_elements(planera)
;     orbmagsep=dblarr(nx,ny)
;     indx=intarr(nx,ny) & indy=indx
;     for j=0,nx-1 do begin
;        orbmagsep[j,*]=separation(planera,planedec,orbra[j],orbdec[j])/3600.
;        indy[j,*]=indgen(ny)
;     endfor 
;     for j=0,ny-1 do indx[*,j]=indgen(nx)
;     minsep=min(orbmagsep,m)
;     x0=indx[m]
;     y0=indy[m]
;     d0=separation(orbra[x0],orbdec[x0],scmomra,scmomdec)/3600.
;     d1=separation(orbra[x0],orbdec[x0],magmomra,magmomdec)/3600.
     
;     theta=acos((cos(d1*!dtor)-cos(momdist*!dtor)*cos(d0*!dtor))/(sin(momdist*!dtor)*sin(d0*!dtor)))*!radeg
;     dist[i]=theta
;     deg90=acos((cos(d0*!dtor)-cos(momdist*!dtor)*cos(d1*!dtor))/(sin(momdist*!dtor)*sin(d1*!dtor)))*!radeg
;     print,deg90
;     print,theta
;     print,d0,d1
;     add=0.
;     if d0 gt 90. or d1 gt 90 then begin 
;        d0=180.-d0
;        d1=180.-d1
;        add=90.
;     endif 
;     if d0 lt d1 then d0=d1
          
;     dist[i]=asin(sin(d1*!dtor)*sin(90.*!dtor)/sin(d0*!dtor))*!radeg+add
;     if finite(dist[i]) eq 0 then stop
;     print,dist[i]
;     stop
;     print,theta
     
;     stop
     
 ;    dist[i]=mean(separation(nmagra,nmagdec,eph[w].ra,eph[w].dec)/3600.)
     
;     targtime[i]=ppst[i].endtime-ppst[i].begtime
;     time[i]=ppst[i].begtime
;     if i gt 0 then begin 
;        tot[i]=tot[i-1]+targtime[i]
;        cumdist_mag[i]=cumdist_mag[i-1]+(magdist[i]*targtime[i])
;        cumdist_orb[i]=cumdist_orb[i-1]+(orbdist[i]*targtime[i])
;     endif 
     w=where(minutes*60.+starttime ge ppst[i].begtime and minutes*60.+starttime le ppst[i].endtime,nw)
     if nw gt 0 then begin 
        magdist_min[w]=magdist[i]
        orbdist_min[w]=orbdist[i]
        ndist_min[w]=ndist[i]
        sdist_min[w]=sdist[i]
        dist_min[w]=dist[i]
     endif 
  endfor 
  
  orblen=97.
  norb=fix(nmin/orblen)
  magorb=dblarr(norb) & orborb=magorb & orbtime=magorb & nnormdist=magorb & snormdist=magorb
  magorb_max=dblarr(norb) & orborb_max=dblarr(norb) & orbmax=dblarr(norb) & timeatmax=dblarr(norb)
  magorbrange=dblarr(norb) & orborbrange=dblarr(norb) & orbmom=dblarr(norb) & simmom=dblarr(norb)
  simmom2=dblarr(norb)
  for i=0L,norb-1 do begin
     j=i*orblen
     j1=(i+1)*orblen
     jj=indgen(orblen)+j
     orbtime[i]=mean(minutes[j:j1])
     magorb[i]=mean(magdist_min[j:j1])
     orborb[i]=mean(orbdist_min[j:j1])
     magorb_max[i]=max(magdist_min[j:j1],mm)
     wm=where(magdist_min[j:j1] eq magorb_max[i],nwm)
     timeatmax[i]=nwm/97.
     orborb_max[i]=max(orbdist_min[j:j1],mo)
     nnormdist[i]=mean(ndist_min[j:j1])
     snormdist[i]=mean(sdist_min[j:j1])
     wn0=where(magdist_min[j:j1] ne 0.)
     magorbrange[i]=max(magdist_min[jj])-min(magdist_min[jj[wn0]])
     orborbrange[i]=max(orbdist_min[jj])-min(orbdist_min[jj[wn0]])
     
     worb=where(sctime-starttime ge minutes[j]*60. and sctime-starttime lt minutes[j1]*60.,nworb)
     if nworb gt 0 then orbmom[i]=mean(scmom[worb])
     
;     wno0=where(dist_min[jj] ne 0)
     simmom[i]=mean(dist_min[jj])
;    simmom[i]=mean(sin(dist_min[j:j1]*!dtor)^2.)
;     totdist=total(magdist_min[j:j1])
     if i gt 0 then ss=simmom2[i-1] else ss=0
;     if magorb_max[i] gt 50 and orborb_max[i] gt 50 then simmom2[i]=ss-1. else simmom2[i]=ss+1.
     if magorb_max[i] lt 50 and orborb_max[i] lt 50 then simmom2[i]=ss+1.2 else simmom2[i]=ss-1.0
;     if magorb_max[i] gt 50 and orborb_max[i] gt 50 then simmom2[i]=ss-(magorb_max[i]-50.)/2. else simmom2[i]=(50.-magorb_max[i])-ss
     if simmom2[i] lt 0 then simmom2[i]=0.
     
     
;     orbmax[i]=max([magorb_max[i],orborb_max[i]])
  endfor 
  
  ;;what is the force driving the momentum up?  Is it the max distance, delta distance, both?

  
  
;  plot,orbtime/60./24.,nnormdist
;  oplot,orbtime/60./24.,snormdist,color=!green
;  stop
  
  day0=startdate[1]
  days=orbtime/60./24.+day0
  
  plot,days,orborb,xtitle='DOY',ytitle='Distance (degrees)',title=title,yrange=[0,90],xrange=[min(days),max(days)],/xstyle,/nodata
;  oplot,days,magorb,color=!green
  oplot,days,magorb_max,color=!green,line=2
;  oplot,days,magorbrange,color=!orange
;  oplot,minutes/60./24.+day0,magdist_min,color=!green
  oplot,days,orborb_max,line=2
  legend,['Dist between Pointing & Orbit plane','Dist between Pointing & Mag Plane','Dist between Mag & Orb Norm','CFG_TOTALMOM','WARNING'],box=0,/top,/right,color=[!p.color,!green,!red,!blue,!magenta],line=[0,0,0,0,0]
  
  oplot,days,nnormdist,color=!red

  scale=10.;max(orbmom)*2.
;  warning=normalize(1./(magorb_max*nnormdist^2))*scale
;  oplot,days,warning,color=!magenta
  
  ;;;plot dtas data
  oplot,(sctime-starttime)/86400.+day0,scmom,color=!blue
  oplot,day0+[0,100],[7,7],line=2,color=!yellow
  
  howlong=intarr(norb) & whenmax=howlong
  w=where(magorb_max gt 50)
  whenmax[w]=minutes[w]
  mx=0
  for i=0,norb-1 do begin
     if whenmax[i] gt mx then mx=whenmax[i]
     howlong[i]=minutes[i]-mx+1
  endfor 
  
  oplot,[min(days),max(days)],[50,50],line=1
  
  oplot,days,orbmom,color=!cyan
;  oplot,days,normalize(1./simmom)*scale
;  oplot,days,normalize(1./simmom2)*scale,color=!green
  w=where(simmom2 eq 0.)
  simmom2[w]=2.
;  warning=smooth(normalize((simmom2+1.)^0.5/simmom/nnormdist/magorb_max),2)
  warning=normalize((simmom2^0.5)/nnormdist/simmom/magorb_max)
  oplot,days,warning*scale,color=!orange
  
  stop
  return
end 
;compare_dist2plane,file6,eph=eph6,sctime=sctime,scmom=scmom
;;simplify program for ppst checking purposes, which grabs dtas data if ppst in past or afst
