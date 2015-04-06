pro comet_sun
  
  readcol,'comet_b_eph.dat',date,time,rah,ram,ras,decd,decm,decs,tmag,nmag,ad,format='(a,a,a,a,a,a,a,a,a,a,a)'
  
  year=strmid(date,0,4)
  month=strmid(date,5,3)
  day=strmid(date,9,2)
  
  hms2radec,rah,ram,ras,decd,decm,decs,ra,dec
  
  mn=month_cnv(month)
  h=strmid(time,0,2)
  m=strmid(time,3,2)
  hr=h*1.+m/60.
  
  jdcnv,year,mn,day,hr,jd
  
  sunpos,jd,sra,sdec
  moonpos,jd,mra,mdec
  
  n=n_elements(ra)
;  dist=fltarr(n)
;  for i=0,n-1 do begin 
;     dist[i]=calc_coord_offset(ra[i],dec[i],sra[i],sdec[i])/3600.
;  endfor 
  dist=sphdist(ra,dec,sra,sdec,/degrees)
  mdist=sphdist(ra,dec,mra,mdec,/degrees)
;  w=where(dist gt 180)
;  dist[w]=360.-dist[w]
  
;  plot,jd,dist,psym=1
  doy=ymd2dn(year,mn,day)
  doyt=doy+hr/24.
  
  !p.multi=[0,1,3]
  
  plot,ra,dec,psym=1,xrange=[0,360],yrange=[-90,90],xstyle=1,ystyle=1,xtitle='R.A',ytitle='Dec',title='Comet 73P/Schwassmann-Wachmann 3-C'
  oplot,sra,sdec,psym=1,color=!yellow
  oplot,mra,mdec,psym=1,color=!blue
  tvellipse,45,45,sra[0],sdec[0],0,!yellow,/data
  tvellipse,45,45,sra[n-1],sdec[n-1],0,!yellow,/data
  tvellipse,23,23,mra[0],mdec[0],0,!blue,/data
  tvellipse,23,23,mra[n-1],mdec[n-1],0,!blue,/data
  
  plot,doyt,dist,xtitle='Day of Year',ytitle='Comet-Sun Distance'
  oplot,[100,200],[45,45],line=2
  
  plot,doyt,mdist,xtitle='Day of Year',ytitle='Comet-Moon Distance'
  oplot,[100,200],[23,23],line=2
  
  !p.multi=0
  stop
  return
end 
