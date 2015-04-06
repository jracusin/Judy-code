function min_distance,planera,planedec,ra,dec,m=m
  
  n=n_elements(planera)
  d=dblarr(n)
  d=separation(ra,dec,planera,planedec)/3600.
  mind=min(d,m)
  return,mind
end 

pro check_momentum,file,eph=eph,sctime=sctime,scmom=scmom,rmind=rmind,ps=ps,back=back
  
  ps=1
  simpctable
  
  jdnow=systime(/julian)
  daycnv,jdnow,yrnow,mnnow,daynow,hrnow
  doynow = ymd2dn(yrnow,mnnow,daynow)  
  
  if n_elements(file) eq 0 then begin 

     dfile='datefile.dat'
     olddate='e.g. 2005353'
     if exist(dfile) then begin 
        readcol,dfile,olddate,format='(a)' 
        date=olddate
     endif else date=''
     read,'What date ('+olddate+')? ',date
     days=''
     read,'N days (1)? ',days
     if date eq '' then date=olddate
     if date*1L lt 300 then date='2008'+date
     if date ne olddate then writecol,dfile,date
     if days eq '' then days=1
     
     enddate=strtrim(ntostr(date*1L+days),2)
     
     print,date
     
     doynow=ntostr(fix(doynow))
     if doynow*1. lt 100 then doynow='0'+doynow
     if doynow*1. lt 10 then doynow='0'+doynow
     datenow=strtrim(ntostr(fix(yrnow)),2)+doynow
     
  ;;; if past then grab afst
     if date*1L lt datenow*1L then begin 
        ffiles=''
        ddate=date
        file='AFST_'+date+'0000_'+enddate+'0000_00.txt'
        print,file
        if not exist(file) then begin 
           for i=0,days-1 do begin 
              get_afst,ddate,file,/partial,/only
              ffiles=[ffiles,file]
              ddate=ntostr(ddate+1L)
           endfor 
           ffiles=ffiles[1:*]
           file=strmid(ffiles[0],0,17)+strmid(ffiles[days-1],17,18)
           print,file
           if days gt 1 then spawn,'cat '+ntostrarr(ffiles)+' > '+file
        endif else print,'Using existing AFST file'
        read_afst,file,ppst,/includeslew
        afst=1
     endif else begin 
        if date*1L eq datenow*1L then begin 
           print,'DATE IS TODAY'
           again:
           print,'CHOOSE AFST or PPST (a/p)?'
           k=get_kbrd(10)
           if k eq 'a' or k eq 'p' then begin 
              if k eq 'a' then begin 
                 get_afst,date,file,/partial,/only
                 read_afst,file,ppst,/includeslew
                 afst=1
              endif else begin 
                 print,'Grabbing Master PPST'
                 get_master_ppst,date,enddate,file,noppst=noppst
                 read_ppst,file,ppst
              endelse 
           endif else begin
              print,'Invalid entry'
              goto,again
           endelse 
        endif else begin 
           
  ;;; if future, look for master PPST
           get_master_ppst,date,enddate,file,noppst=noppst
           
           if noppst eq 0 then begin
              read_ppst,file,ppst
              print,'Grabbing Master PPST'
           endif else begin
  ;;; if not in master PPST, then prompt planner for PPST
              file=file_search('PPST*'+date+'*'+enddate+'*txt')
              file=file[n_elements(file)-1]
              print,file
              print,'Is this the correct PPST? (y/n)'
              k=get_kbrd(10)
              if k eq 'n' then begin 
                 print,'Pick PPST'
                 file=pickfile(filter='*ST*txt')
              endif 
              apos=strpos(file,'AFST')
              if apos[0] eq -1 then $
                 read_ppst,file,ppst else $
                    read_afst,file,ppst
           endelse 
           afst=0
        endelse 
     endelse 
  endif else begin 
     apos=strpos(file,'AFST')
     if apos[0] eq -1 then $
        read_ppst,file,ppst else $
           read_afst,file,ppst
  endelse 
;  apos=strpos(file,'AFST')
;  if apos[0] eq -1 then read_ppst,file,ppst else read_afst,file,ppst
  startdate=met2date_judy(ppst[0].begtime)
  n=n_elements(ppst)
  stopdate=met2date_judy(ppst[n-1].endtime)
  sdate=ntostr(long(startdate[0]))+ntostr(long(startdate[1]))
  pdate=ntostr(long(stopdate[0]))+ntostr(long(stopdate[1]))
  if n_elements(back) eq 0 then back=2 ;days

  nfile=file
  print,nfile  
  n=n_elements(ppst)
  if n_elements(eph) eq 0 then begin 
     ephfile=get_ephfile(nfile)
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
  day1=day1+1
  year1=ntostr(stopdate[0],4)
  dom=[0,31,28,31,30,31,30,31,31,30,31,30,31]
  if day1 gt dom[month1] then begin
     day1=day1-dom[month1]
     month1=month1+1
  endif 

  if month1 lt 10 then month1='0'+ntostr(fix(month1)) else month1=ntostr(fix(month1))
  if day1 lt 10 then day1='0'+ntostr(fix(day1)) else day1=ntostr(fix(day1))

  dtas=0
  doy0=ymd2dn(year0,month0,day0)
  if doy0 lt 100 then doythen='0'+ntostr(doy0) else doythen=ntostr(doy0)
  if doy0 lt 10 then doythen='0'+ntostr(doy0)
  
  datethen=year0+doythen
  datenow=ntostr(fix(yrnow))+ntostr(fix(doynow))

  if datethen*1. le datenow*1. then begin 
     dfile1='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_CFG_TOTALMOM.dat'
;     dfile1='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_1.dat'
;     dfile2='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_2.dat'
;     dfile3='dtas_'+ntostr(startdate[0],4)+'_'+month0+'_'+day0+'_SAC_HTOT_3.dat'
     s=' '
;     print,dfile1               ;,dfile2,dfile3
     if not exist(dfile1) then begin
        com='get_dtas_par.py -m '+year0+s+month0+s+day0+s+year1+s+month1+s+day1
        print,com
        spawn,com
     endif 
     if n_elements(sctime) eq 0 and n_elements(scmom) eq 0 then begin 
        readcol,dfile1,scdate,scmom1,format='(a,d)',skip=2,delim=','
;        readcol,dfile2,scdate,scmom2,format='(a,d)',skip=2,delim=','
;        readcol,dfile3,scdate,scmom3,format='(a,d)',skip=2,delim=','
;        scmom=sqrt(scmom1^2+scmom2^2+scmom3^2)
        scmom=scmom1
        sctime=dblarr(n_elements(scdate))
        for i=0L,n_elements(scdate)-1 do sctime[i]=date2met(scdate[i])
     endif
     dtas=1
  endif 
  magdist=dblarr(n) & orbdist=dblarr(n)
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
     if n_elements(orbra) gt 0 then orbdist[i]=min_distance(orbra,orbdec,ppst[i].ra,ppst[i].dec,m=m)
     ndist[i]=separation(norbnormra,norbnormdec,nmagra,nmagdec)/3600.
     sdist[i]=separation(sorbnormra,sorbnormdec,smagra,smagdec)/3600.
     
     w=where(eph.met ge ppst[i].begtime and eph.met lt ppst[i].endtime)
     if w[0] eq -1 then w=where(eph.met ge ppst[i].begtime-60. and eph.met lt ppst[i].endtime+60.)
     scmomra=mean(eph[w].ra)
     scmomdec=mean(eph[w].dec)
     
     scorbdist=min_distance(orbra,orbdec,ppst[i].ra,ppst[i].dec,m=worb)
     if worb+2 lt n_elements(orbra) then norb=worb+2 else norb=worb-2
     scmomang=atan(orbra[worb]-orbra[norb],orbdec[worb]-orbdec[norb])*!radeg
     
     magorbdist=min_distance(planera,planedec,ppst[i].ra,ppst[i].dec,m=m)
     if m+2 lt n_elements(planera) then nm=m+2 else nm=m-2
     magang=atan(planera[m]-planera[nm],planedec[m]-planedec[nm])*!radeg
     dist[i]=abs(abs(scmomang)-abs(magang))

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
     mdm=magdist_min[j:j1]
     wmdm=where(mdm ne 0.)
     magorb_max[i]=min(mdm[wmdm])
;     magorb_max[i]=min(magdist_min[j:j1]);,mm)
     wm=where(magdist_min[j:j1] eq magorb_max[i],nwm)
     timeatmax[i]=nwm/97.
     odm=orbdist_min[j:j1]
     wodm=where(odm ne 0.)
     orborb_max[i]=min(odm[wodm])
;     orborb_max[i]=min(orbdist_min[j:j1]);,mo)
     nnormdist[i]=mean(ndist_min[j:j1])
     snormdist[i]=mean(sdist_min[j:j1])
     wn0=where(magdist_min[j:j1] ne 0.)
     magorbrange[i]=max(magdist_min[jj])-min(magdist_min[jj[wn0]])
     orborbrange[i]=max(orbdist_min[jj])-min(orbdist_min[jj[wn0]])
     
     if dtas then begin 
        worb=where(sctime-starttime ge minutes[j]*60. and sctime-starttime lt minutes[j1]*60.,nworb)
        if nworb gt 0 then orbmom[i]=mean(scmom[worb])
     endif 
     
     simmom[i]=mean(dist_min[jj])
     if i lt norb-1 then begin 
        if magorb_max[i] gt 10 or orborb_max[i] gt 10 then begin 
           simmom2[i+1]=simmom[i]+10 ;magorb_max[i]+orborb_max[i]
;           if i+2 lt norb-1 then simmom2[i+2]=simmom[i]+10
;           if i+3 lt norb-1 then simmom2[i+3]=simmom[i]+10
;           if i+4 lt norb-1 then simmom2[i+4]=simmom[i]+10
        endif 
        if magorb_max[i] lt 10 and orborb_max[i] lt 10 then simmom2[i+1]=simmom[i]-4
     endif 
     if simmom2[i] eq 0 then simmom2[i]=simmom[i]
     
;     if orborb_max[i] gt 5 then simmom[i]=simmom[i]-2
     
;     if i gt 0 then ss=simmom2[i-1] else ss=0
;     if magorb_max[i] lt 10 and orborb_max[i] lt 10 then simmom2[i]=ss+1.1 else simmom2[i]=ss-1.0
;     if simmom2[i] lt 0 then simmom2[i]=0.
     
  endfor 
  
  day0=startdate[1]
  time0=date2met(year0+'-'+ntostr(fix(day0))+'-00:00:00')
  days=orbtime/60./24.+day0
  w=where(magorb_max lt 45,nw)
  halforb=97/2.*60.
  rind=0
  if nw gt 0 and n_elements(rmind) eq 0 then begin 
     wtime=orbtime[w]*60.+time0
;     print,'Need to change a target in each of these orbits:'
     for i=0,nw-1 do begin
;        print,met2date(wtime[i])
        worb=where(ppst.begtime gt wtime[i]-halforb and ppst.endtime lt wtime[i]+halforb,nworb)
        if nworb gt 0 then begin 
           minorb=min(magdist[worb],m)
;        minorb=min(abs(ppst.begtime-wtime[i]),m)
;        print,ppst[worb[m]].begdate,ppst[worb[m]].dec,magdist[worb[m]],worb[m]
           rind=[rind,worb[m]]
        endif 
     endfor 
  endif 
  if n_elements(rind) gt 1 then rmind=rind[1:*]
;  if n_elements(rmind) gt 0 then begin
;     colprint,ppst[rmind].begdate,ppst[rmind].dec,magdist[rmind],rmind,ppst[rmind].targname
;     tid=ppst[rmind].targetid
;     u=uniq(tid)
;     utid=tid[u]
;     nu=n_elements(utid)
;     dup=intarr(nu)
;     for j=0,nu-1 do begin
;        w=where(tid eq utid[j],nw)
;        dup[j]=nw
;     endfor 
;     w=where(dup gt 1,nw)
;     print,ppst[rmind[u[w]]].targname
;  endif 
  
  spos=strpos(file,'ST')
  dir='momentum_plots'
  if exist(dir) then dir=dir+'/' else dir=''
  plotfile=dir+strmid(file,spos-2,28)+'_momentum.ps'
  print,plotfile
  if keyword_set(ps) then begplot,name=plotfile,/color,/land else erase
  
  multiplot2,[1,2],/init
  multiplot2
  plot,days,orborb,ytitle='Distance (degrees)',title=title,yrange=[0,40],xrange=[min(days),max(days)],/xstyle,/nodata,/ysty
  oplot,days,magorb_max,color=!green,line=2
  oplot,days,orborb_max,line=2
  oplot,days,simmom2,color=!orange,line=2
  legend,['Min Dist btw Point & Orbit plane','Min Dist btw Point & Mag Plane','Dist between Mag & Orb Norm','Mean dist'],box=0,/top,/right,textcolor=[!p.color,!green,!red,!orange],charsize=1.
  
  oplot,days,nnormdist,color=!red
;  oplot,days,snormdist,color=!red,line=2

  scale=10.                     ;max(orbmom)*2.
  
  multiplot2
  plot,[min(days),max(days)],xrange=[min(days),max(days)],/nodata,yrange=[0,10],xtitle='DOY',/xsty,/ysty
  ;;;plot dtas data
  if dtas then begin 
     oplot,(sctime-starttime)/86400.+day0,scmom,color=!blue
     oplot,days,orbmom,color=!cyan
  endif 
  oplot,day0+[0,100],[8,8],color=!yellow
  oplot,day0+[0,100],[5,5],color=!red
  
  howlong=intarr(norb) & whenmax=howlong
  w=where(magorb_max gt 3)
  whenmax[w]=minutes[w]
  mx=0
  for i=0,norb-1 do begin
     if whenmax[i] gt mx then mx=whenmax[i]
     howlong[i]=minutes[i]-mx+1
  endfor 
  
  oplot,[min(days),max(days)],[30,30],line=1
  ndays=max(days)-min(days)
  w=where(simmom2 eq 0.)
;  simmom2=simmom2^0.5/max(simmom^0.5)*7.+1.

  ;;;ISSUES WITH NORMALIZATION, NEED TO NORMALIZE INDEPENDENT OF PPST BEING BAD
;  simmom2=simmom2/ndays*40.
;  simmom3=normalize(simmom2)*10.+2.
  wmm=where(magorb_max-orborb_max gt 0,nwmm)
  mmmax=magorb_max
  if nwmm gt 0 then mmmax[wmm]=orborb_max[wmm]+1.
  wmm=where(magorb_max-orborb_max lt 0,nwmm)
  if nwmm gt 0 then mmmax[wmm]=magorb_max[wmm]+0.05
  
;  mmmax=(magorb_max+orborb_max)/2.
;  mmmax=magorb_max
  
  w=where(magorb_max gt 12,nw)  ; and orborb_max gt 10)
  if nw gt 0 then begin 
     mmmax[w]=mmmax[w]+10.
     if max(w)+2 lt norb then $
        mmmax[w+2]=magorb_max[w+2]+magorb_max[w]
  endif 
  
;  warning=(simmom3^0.5)/nnormdist/simmom/mmmax*900. ;*4500.
;  warning=1./nnormdist/simmom/mmmax*600.
  w=where(simmom2 lt 0,nw)
  if nw gt 0 then simmom2[w]=10.
;  w=where(simmom2 lt 2,nw)
;  if nw gt 0 then simmom2[w]=2.
;  w=where(mmmax lt 1,nw)
;  if nw gt 0 then mmmax[w]=1.
  simmom2=smooth(simmom2,2)+0.3
  mmmax=mmmax+0.8
  
;  warning=1./simmom2^1.5/mmmax/nnormdist^3*30000.+0.3
  warning=1./simmom2/mmmax*33.+0.5
  
;  warning=1./simmom2/mmmax/nnormdist*300.+0.5 
;  warning=1./magorb/simmom*300.
  
  warning=smooth(warning,3)
;  warning[2:*]=smooth(warning+warning[2:*],3)
  oplot,days,warning,color=!magenta
  print,'Checking PPST covering days '+sdate+' to '+pdate
  sday=strmid(sdate,4,3)*1
  pday=strmid(pdate,4,3)*1
  wday=where(days ge sday and days lt pdate)
  print,'Max predicted total momentum in this time period: ',max(warning[wday])
  print,'Mean predicted total momentum in this time period: ',mean(warning[wday])
  
  legend,['CFG_TOTALMOM','Orb Avg Momentum','Prediction','Yellow limit','Warning'],box=0,/top,/right,textcolor=[!cyan,!blue,!magenta,!yellow,!red],charsize=1.
  
;  stop
;  if n_elements(rmind) gt 0 then begin 
;     stime=date2met(strmid(sdate,0,4)+'-'+strmid(sdate,4,3)+'-00:00:00')
;     rmtime=(ppst[rmind].begtime-stime)/60./60./24.
;     q=0
;     maxday=round(max(days)-day0)
;     sday=day0
;     maxday=pdate*1L-sdate*1L
;     sday=strmid(sdate,4,3)*1
;     for i=0,maxday-1 do begin
;        wday=where(days ge sday+i and days le sday+i+1)
;        ww=where(warning[wday] gt 5,nww)
;        if nww gt 0 then begin 
;           w=where(rmtime ge i and rmtime le i+1,nw)
                                ;          if nw gt 0 then q=[q,w]
                                ;       endif
;     endfor 
;     if n_elements(q) gt 1 then begin 
;        q=q[1:*]
;        print,'Perhaps change the following targets to ones with larger dec:'
;        print,'    Snapshot time          Dec             mag_dist  target name'  
;        colprint,ppst[rmind[q]].begdate,ppst[rmind[q]].dec,magdist[rmind[q]],ppst[rmind[q]].targname
;        rmind=rmind[q]
;                
;     endif else print,'Predicted total momentum looks fine'; for '+sdate+' to '+pdate
;  endif 
  
  multiplot2,/reset,/default
  if keyword_set(ps) then begin 
     endplot
     ppos=strpos(plotfile,'ps')
     plotfile2=strmid(plotfile,0,ppos)+'png'
     print,'Output plot in: ',plotfile2
;     plotfile2=file+'_momentum.png'
     spawn,'convert -rotate 270 '+plotfile+' '+plotfile2
     spawn,'display '+plotfile2+' &'
  endif 

  return
end 
;;;;need data from time period before to show momentum build-up
;;;;  as predictive tools, would need to grab afst for few days prior?
;;;;  but won't work as there will be a gap
