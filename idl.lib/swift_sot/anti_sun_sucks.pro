pro plot_slews,ra1,dec1,roll1,ra2,dec2,roll2,slewbeg
  
  n=n_elements(ra1)
  for i=0,n-1 do begin 
     met=date2met(slewbeg[i])
     date=met2date_judy(met)
     ydn2md,date[0],date[1],month,day
     partday=date[3]/24.+date[4]/60./24.
     jdcnv,date[0],month,day,partday*24.,jd
     
     sunpos,jd,sunra,sundec
     moonpos,jd,moonra,moondec
     antisunra=sunra+180.
     if antisunra gt 360. then antisunra=antisunra-360.
     antisundec=-1.*sundec
     theta_roll=dist_overpole(roll2[i],roll1[i])*!dtor
     predict_slew,ra1[i],dec1[i],ra2[i],dec2[i],sunra,sundec,sunra,sundec,moonra,moondec,ora,odec,tslew,/noplot,dostop=dostop,theta_roll=theta_roll,dist=dist0,noslew=noslew
     asdist=separation(ora,odec,antisunra,antisundec)/3600.

     map_set,/aitoff,grid=2
     euler,ora,odec,olo,ola,3
     plots,olo,ola,thick=3
     plotsym,3,2,/fill
     euler,antisunra,antisundec,aslon,aslat,3
     plots,aslon,aslat,psym=8,color=!red
     k=get_kbrd(10)
  endfor 
     
end 
pro anti_sun_sucks
  
  cd,'~/sciplan/'
  readcol,'AntiSun_Slews.csv',obsend,offset,droll,dantisun,slewbeg,ra1,dec1,roll1,ra2,dec2,roll2,format='(a,d,d,d,a,d,d,d,d,d,d)',delim=',',/silent,skip=1
  n=n_elements(offset)
  euler,ra1,dec1,lon1,lat1,3 
  euler,ra2,dec2,lon2,lat2,3 
  dist=separation(ra1,dec1,ra2,dec2)/3600.
  
  minasdist=fltarr(n) & minasecldist=minasdist
  cross=intarr(n)
  for i=0,n-1 do begin 
     met=date2met(slewbeg[i])
     date=met2date_judy(met)
     ydn2md,date[0],date[1],month,day
     partday=date[3]/24.+date[4]/60./24.
     jdcnv,date[0],month,day,partday*24.,jd
     
     sunpos,jd,sunra,sundec
     moonpos,jd,moonra,moondec
     antisunra=sunra+180.
     if antisunra gt 360. then antisunra=antisunra-360.
     antisundec=-1.*sundec
     theta_roll=dist_overpole(roll2[i],roll1[i])*!dtor
     predict_slew,ra1[i],dec1[i],ra2[i],dec2[i],sunra,sundec,sunra,sundec,moonra,moondec,olo,ola,tslew,/noplot,dostop=dostop,theta_roll=theta_roll,dist=dist0,noslew=noslew
     asdist=separation(olo,ola,antisunra,antisundec)/3600.
;     was=where(asdist lt 15. and abs(ola-antisundec) lt 3,nwas)
     mindist=min(abs(ola-antisundec),was)
     nwas=1
     if nwas gt 0 then minasecldist[i]=min(asdist[was]) else minasdist[i]=0
     minasdist[i]=min(asdist)
;     plot_slews,olo,ola,antisunra,antisundec
     if (lat1[i] gt 0 and lat2[i] lt 0) or (lat1[i] lt 0 and lat2[i] gt 0) then cross[i]=1 else cross[i]=0
     print,minasdist[i],min(asdist),droll[i],dist[i],offset[i],cross[i]
;     k=get_kbrd(10)
  endfor 
     
;  w=where((lat1 gt 0 and lat2 lt 0) or (lat1 lt 0 and lat2 gt 0)) 
  w=where(cross and minasdist lt 15)
  w2=where(minasdist lt 15 and droll gt 120)
  w3=where(cross and minasdist lt 15 and dist gt 70 and droll gt 120); and minasecldist lt 15)

;  w3=where(droll[w2]/dist[w2] gt 1 and droll[w2]/dist[w2] lt 3)
;  w3=w2[w3]
  bad=3.
  wbad=where(offset gt bad,nwbad)
  print,'# offset by > '+ntostr(fix(bad))+' deg',nwbad
  wbad1=where(offset[w] gt bad,nwbad1)
  wgood1=where(offset[w] lt bad,nwgood1)
  print
  print,'# caught by old criteria',nwbad1
  print,'# missed by old criteria',nwbad-nwbad1
  print,'# good but flagged by old criteria',nwgood1
  
  wbad2=where(offset[w2] gt bad,nwbad2)
  wgood2=where(offset[w2] lt bad,nwgood2)
  print
  print,'# caught by John criteria',nwbad2
  print,'# missed by John criteria',nwbad-nwbad2
  print,'# good but flagged by John criteria',nwgood2
  
  wbad3=where(offset[w3] gt bad,nwbad3)
  wgood3=where(offset[w3] lt bad,nwgood3)
  print
  print,'# caught by new criteria',nwbad3
  print,'# missed by new criteria',nwbad-nwbad3
  print,'# good but flagged by new criteria',nwgood3

  
  begplot,name='antisun_tests.ps',/color
  plotsym,0,1,/fill
  !p.charsize=1.
;  !p.multi=[0,1,3]
  multiplot2,[1,4],/init
  multiplot2
  plot,offset,droll,psym=1,ytitle='delta roll (deg)';,ytitle='target offset (arcmin)'
  oplot,offset[w],droll[w],psym=8,color=!red
  oplot,offset[w2],droll[w2],psym=1,color=!blue
  oplot,[3,3],[-1000,1000],line=1
  oplot,offset[w3],droll[w3],psym=8,symsize=0.4,color=!green
  
  multiplot2
  plot,offset,dist,psym=1,ytitle='slew length (deg)';,xtitle='target offset (arcmin)'
  oplot,offset[w],dist[w],psym=8,color=!red
  oplot,offset[w2],dist[w2],psym=1,color=!blue
  oplot,[3,3],[-1000,1000],line=1
  oplot,offset[w3],dist[w3],psym=8,symsize=0.4,color=!green
;  plot,dist,droll,psym=1,xtitle='slew length (deg)',ytitle='delta roll (deg)'
;  oplot,dist[w],droll[w],psym=1,color=!red
  multiplot2
  plot,offset,droll/dist,psym=1,ytitle='delta roll/slew length',yrange=[0,10]
  oplot,offset[w],droll[w]/dist[w],psym=8,color=!red
  oplot,offset[w2],droll[w2]/dist[w2],psym=1,color=!blue
  oplot,[3,3],[-1000,1000],line=1
  oplot,offset[w3],droll[w3]/dist[w3],psym=8,symsize=0.4,color=!green
  
  multiplot2
  plot,offset,minasdist,psym=1,ytitle='Min AS dist (deg)',xtitle='target offset (arcmin)',yrange=[0,15]
  oplot,offset[w],minasdist[w],psym=8,color=!red
  oplot,offset[w2],minasdist[w2],psym=1,color=!blue
  oplot,offset[w3],minasdist[w3],psym=8,symsize=0.4,color=!green
  oplot,[3,3],[-1000,1000],line=1
  
  multiplot2,/reset,/default
  endplot
  stop
  return
end 
  
