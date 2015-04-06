pro tmp

  dn=findgen(365)
  ydn2md,2012,indgen(365),m,d
  jd=dindgen(365)
  for i=0,364 do begin 
     jdcnv,2012,m[i],d[i],0,j
     jd[i]=j
  endfor 
  sunpos,jd,sra,sdec

  hms2radec,20.,32.,25.78,40.,57.,27.9,ra,dec

  dist=separation(sra,sdec,ra,dec)/3600.
  plot,dn,dist,xrange=[0,365],/xsty
  oplot,[0,365],[45,45],line=2,color=!red

return
end 

pro tmp2

  b=mrdfits('~/Downloads/browse_results.fits',1)
  w=where(strtrim(b.trigger_type,2) eq 'GRB')
  b=b[w]

;  readcol,'~/Downloads/browse_results.txt',v,t,name,ra,dec,trig,type,rel,format='(a,a,a,a,a,a,a,f)',delim='|'
;  w=where(strtrim(type,2) eq 'GRB')
;  trig=trig[w]
;  name=name[w]
  
;  ntrig=n_elements(trig)
;  met=dblarr(ntrig)  
;  for i=0,ntrig-1 do met[i]=date2met(trig[i])  

;  daycnv,(2400000.5d)+b.trigger_time,yr,mon,day,hrs
;  hr=round(hrs-0.5)
;  mn=(hrs-hr)*60.
;  min=round(mn-0.5)
;  sec=(mn-min)*60.
;  w=where(mon lt 10)
;  mon=ntostr(mon)
;  mon[w]='0'+mon[w]
;  w=where(day lt 10)
;  day=ntostr(day)
;  day[w]='0'+day[w]

;  btime=ntostr(yr)+'-'+mon+'-'+day+' '+ntostr(hr)+':'+ntostr(min)+':'+ntostr(sec)
;  bmet=dblarr(ntrig)
;  for i=0,ntrig-1 do bmet[i]=date2met(btime[i]) 
;  diff0=met-bmet
  gmjd=b.trigger_time-51910d

  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  w=where(bat.trigtime ne 0)
  bat=bat[w]
  bmjd=bat.trigtime/86400d

  nbat=n_elements(bat)
  mdiff=fltarr(nbat)
  m2=intarr(nbat)
  for i=0,nbat-1 do begin 
;     diff=abs(bat[i].trigtime-bmet)
     diff=abs(bmjd[i]-gmjd)*86400d
     mindiff=min(diff,m)
;     mdiff[i]=bat[i].trigtime-bmet[m]
     mdiff[i]=(bmjd[i]-gmjd[m])*86400d
     m2[i]=m
  endfor 
  w=where(abs(mdiff) lt 200. and mdiff ne 0.)
  m2=m2[w]
  colprint,bat[w].grb,b[m2].name,mdiff[w];,bat[w].trigtime,bmet[m2],met[m2]

  begplot,name='~/jetbreaks/BAT_GBM_trigger_diff.ps',/land
  plothist,mdiff[w],xtitle='BAT Trigger - GBM Trigger (s)'
  print,median(mdiff[w])
  print,stddev(mdiff[w])
  endplot

stop
  return
end 
