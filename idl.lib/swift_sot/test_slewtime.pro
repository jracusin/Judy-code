pro test_slewtime,eph=eph,sctime=sctime,scra=scra,scdec=scdec
  
  cd,'~/sciplan'
  file='AFST_20082200000_20082300000_00.txt'
  
  read_afst,file,afst
  add_tag,afst,'ptime',0.,new
  afst=new
  w=where(afst.slewend ne '')
  afst=afst[w]
  
  goto,skip1
  if n_elements(sctime) eq 0 then begin 
     syear=strmid(afst[0].begdate,0,4)
     startday=strmid(afst[0].begdate,5,3)
     eyear=strmid(afst[n_elements(afst)-1].enddate,0,4)
     endday=strmid(afst[n_elements(afst)-1].enddate,5,3)
     if startday eq endday then endday=endday+1
     ydn2md,syear,startday,smonth,sday
     ydn2md,eyear,endday,emonth,eday
     s=' '
     com='get_dtas.py '+ntostr(syear)+s+ntostr(smonth)+s+ntostr(sday)+s+ntostr(eyear)+s+ntostr(emonth)+s+ntostr(eday)
     print,com
     spawn,com
     readcol,'dtas_SACSRA.dat',scdate,scra,format='(a,d)',skip=2,delim=','
     readcol,'dtas_SACSDEC.dat',scdate,scdec,format='(a,d)',skip=2,delim=','
     readcol,'dtas_SACSROLL.dat',scdate,scroll,format='(a,d)',skip=2,delim=','
     nbuf=n_elements(scdate)
     sctime=lonarr(nbuf)
     for i=0L,nbuf-1 do sctime[i]=date2met(scdate[i])
  endif 
  skip1:
  
  nslew=n_elements(afst)-2
  find_eph,file,ephfile
  print,'Using Ephemeris file: ',ephfile
  if n_elements(eph) eq 0 then eph=readeph(ephfile)
  realtime=fltarr(nslew)
  ptime0=realtime & ptime1=realtime & ptime2=realtime & phis=realtime
  
  for i=0,nslew-1 do begin 
     ra1=afst[i].ra
     dec1=afst[i].dec
     ra2=afst[i+1].ra
     dec2=afst[i+1].dec
     
     date1=str_sep(afst[i].begdate,'-')
     date2=str_sep(date1[2],':')
     date=[date1[0],date1[1],date2[0],date2[1],date2[2]]
     partday=date[2]/24.+date[3]/24./60.+date[4]/24./3600.
     
     ydn2md,date[0],date[1],month,day
     jdcnv,date[0],month,day,partday*24.,jd
     
     sunpos,jd,sra,sdec
     moonpos,jd,mra,mdec
     
     time=afst[i].begtime
     mint=min(abs(time-eph.met),wt)
     era=eph[wt].era
     edec=eph[wt].edec
     
     theta_roll=dist_overpole(afst[i].roll,afst[i+1].roll)*!dtor
     predict_slew,ra1,dec1,ra2,dec2,era,edec,sra,sdec,mra,mdec,pra,pdec,tslew,theta_roll=theta_roll,tslew0=tslew0,tslew1=tslew1,phi=phi,/noplot
     rt=round((tslew+30.)/60.)*60.
     afst[i].ptime=rt
     ptime0[i]=tslew;tslew0
     ptime1[i]=tslew0
     ptime2[i]=tslew1
     phis[i]=phi
     
;     print,afst[i].slewbeg,afst[i].slewend
     realtime[i]=date2met(afst[i+1].slewend)-date2met(afst[i+1].slewbeg)
     
     goto,skip
     ndeg=n_elements(pra)
     gap=10.
     md=(ndeg-1) mod gap
     ngap=round(ndeg/gap)
     rap=fltarr(ngap)
     decp=fltarr(ngap)
     tslewp=fltarr(ngap)
     distp=fltarr(ngap)
     jj=0
     for j=md,ndeg,gap do begin
        predict_slew,ra1,dec1,pra[j],pdec[j],era,edec,sra,sdec,mra,mdec,ppra,ppdec,tslewpart;,theta_roll=theta_roll
        rap[jj]=pra[j]
        decp[jj]=pdec[j]
        tslewp[jj]=tslewpart
        gcirc,1,ra1,dec1,pra[j],pdec[j],dist
        distp[jj]=dist/3600.
        print,pra[j],pdec[j],tslewp[jj],j,jj
        jj=jj+1
     endfor 
     tmp1=min(abs(date2met(afst[i+1].slewbeg)-sctime),tstart)
     tmp2=min(abs(date2met(afst[i+1].slewend)-sctime),tstop)
     distact=fltarr(tstop-tstart) & sctimes=distact
     for j=0,tstop-tstart-1 do begin 
        gcirc,1,scra[tstart],scdec[tstart],scra[j+tstart],srdec[j+tstop],dist
        distact[j]=dist/3600.
        sctimes[j]=sctime[j+tstart]-sctime[tstart]
     endfor 
     plot,tslewp,disp
     plot,sctimes,distact,color=!red
     
     stop
     skip:
  endfor 
  
  begplot,name='slew_time_test.ps',/color
  plot,realtime,afst.ptime,psym=1,/iso,xtitle='Real time (s)',ytitle='Predicted time (s)'
;  oplot,realtime,ptime0,psym=1,color=!red
  oplot,realtime,ptime1,psym=1,color=!red
;  oplot,realtime,ptime2,psym=1,color=!green
  oplot,[0,500],[0,500]
  w=where(phis ne 0)
  oplot,realtime[w],ptime1[w],psym=1,color=!blue
  x=findgen(30)*10.
  oplot,x,x/2.
  legend,['Slew time prediction','Non-great circle slews','Rounded prediction'],box=0,/top,/left,textcolor=[!red,!blue,!black]
  
  endplot
  
  stop
  
  
  
  return
end 
