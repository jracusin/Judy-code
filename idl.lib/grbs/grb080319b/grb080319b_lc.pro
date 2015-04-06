pro grb080319b_lc,_extra=_extra
  
  ;;;XRT
  cd,'~/Desktop/GRB080319B/'
  file='loredana.qdp'
  lc=lcout2fits(file,/qdp)
  wl=where(lc.time lt 1e4)
  lcl=lc[wl]
  
  lc=lcout2fits(/phil)
  wp=where(lc.time gt 1e4)
  lcp=lc[wp]
  
  concat_structs,lcl,lcp,lc
  
;  tmin=[68.189981,102.19785,132.20277,170.19413,220.18925,260.23553,314.19632,392.18947,538.25537,916.34337]
;  tmax=[102.18361,132.18320,170.18345,220.18747,260.17150,314.18386,392.18057, 538.10948,916.12988,1720.0655]
;  corr=[12.0633,12.0617,11.9731,7.07089,7.07130,7.06963,7.06928,1.85293,1.29116,1.29116]
;  oldcorr=[10.3052,10.3034,10.3053,6.02534,6.02538,6.02521,6.02355,1.52453,1.2822,1.28219]
;  ctr=lc.src_rate
;  for i=0,9 do begin
;     w=where(lc.tstart ge tmin[i] and lc.tstop le tmax[i])
;     ctr[w]=ctr[w]/oldcorr[i]*corr[i]
;  endfor 
;  lc.src_rate=ctr
  convert_phil2lcout,file,/qdp,outfile='combined_xrt_lc.txt',lc=lc
  
  !p.multi=[0,1,3]
  plot_like_qdp,file='combined_xrt_lc.txt',_extra=_extra
;  for i=0,9 do begin
;     oplot,[tmin[i],tmin[i]],[1e-3,1e4],line=1
;     oplot,[tmax[i],tmax[i]],[1e-3,1e4],line=1
;  endfor 
  
  
  ;;;READ OPTICAL TABLES
  dir='~/Desktop/GRB080319B/composite_lc/'
  print,'ft.dat'
  readcol,dir+'ft.dat',time_mn1,exp1,band1,flux1,pm,fluxerr1,format='(f,a,a,f,a,f)'
  readcol,dir+'ft.dat',time_mn1b,exp1b,band1b,flux1b,format='(f,a,a,a)'
  time_mn1=[time_mn1,time_mn1b]
  exp1=[exp1,exp1b]
  flux1=[flux1,flux1b]  
  fluxerr1=[fluxerr1,fltarr(n_elements(exp1b))]
  band1=[band1,band1b]  
  time1=time_mn1*60.
  tmin1=time1-exp1/2.*60.
  tmax1=time1+exp1/2.*60.
  n1=n_elements(time1)
  
  print,'lt.dat'
  readcol,dir+'lt.dat',time_mn2,exp2,band2,flux2,pm,fluxerr2,format='(f,a,a,f,a,f)'
  time2=time_mn2*60.
  tmin2=time2-exp2/2.*60.
  tmax2=time2+exp2/2.*60.
  n2=n_elements(time2)
  
  print,'iucaa.dat'
  readcol,dir+'iucaa.dat',time_mn3,exp3,band3,flux3,pm,fluxerr3,format='(f,a,a,f,a,f)'
  time3=time_mn3*60.
  tmin3=time3-exp3/2.
  tmax3=time3+exp3/2.
  n3=n_elements(time3)
  
  print,'rem.dat'
  readcol,dir+'rem.dat',time4,exp4,band4,flux4,pm,fluxerr4,format='(f,a,a,f,a,f)'
  exp4=exp4*2.
  tmin4=time4-exp4/2.
  tmax4=time4+exp4/2.
  n4=n_elements(time4)
  
  print,'tortura.dat'
  readcol,dir+'tortura.dat',time5,exp5,flux5,fluxerr5,format='(f,f,f,f)'
  exp5=exp5*2.
  tmin5=time5-exp5/2.
  tmax5=time5+exp5/2.
  n5=n_elements(time5)
  band5=replicate('V',n5)
  
  print,'vlt.dat'
  readcol,dir+'vlt.dat',time6,exp6,band6,flux6,pm,fluxerr6,format='(f,a,a,f,a,f)'
  exp6=exp6*2.
  tmin6=time6-exp6/2.
  tmax6=time6+exp6/2.
  n6=n_elements(time6)
  
  print,'wsrt.dat'
  readcol,dir+'wsrt.dat',month,eph1,tmp,eph2,band7,d1,tmp,d2,time_day7,integ,flux7,pm,fluxerr7,format='(a,f,a,f,a,f,a,f,f,f,a,a,f)'
  readcol,dir+'wsrt.dat',monthb,eph1b,tmpb,eph2b,band7b,d1b,tmpb,d2b,time_day7b,integb,flux7b,pm,format='(a,f,a,f,a,f,a,f,f,f,a,a)'
  w=[0,1,4]
  time_day7=[time_day7,time_day7b[w]]
  d1=[d1,d1b[w]]
  d2=[d2,d2b[w]]
  flux7=[flux7,flux7b[w]]
  fluxerr7=[fluxerr7,fltarr(n_elements(eph2b[w]))]
  band7=[band7,band7b[w]]
  time7=time_day7*86400.
  tmin7=d1*86400.
  tmax7=d2*86400.
  lpos=strpos(flux7,'<')
  w=where(lpos eq 0)
  flux7[w]=strmid(flux7[w],1,3)
  n7=n_elements(time7)
  
  lcstr=create_struct('instrument','','band','','time',0.,'tmin',0.,'tmax',0.,'flux',0.,'fluxerr',0.)
  n=n1+n2+n3+n4+n5+n6+n7
  lcstr=replicate(lcstr,n)
  lcstr.time=[time1,time2,time3,time4,time5,time6,time7]
  lcstr.tmin=[tmin1,tmin2,tmin3,tmin4,tmin5,tmin6,tmin7]
  lcstr.tmax=[tmax1,tmax2,tmax3,tmax4,tmax5,tmax6,tmax7]
  lcstr.flux=[flux1,flux2,flux3,flux4,flux5,flux6,flux7]
  lcstr.fluxerr=[fluxerr1,fluxerr2,fluxerr3,fluxerr4,fluxerr5,fluxerr6,fluxerr7]
  lcstr.instrument=[replicate('LT',n1),replicate('FT',n2),replicate('IUCAA',n3),replicate('REM',n4),replicate('TORTURA',n5),replicate('VLT',n6),replicate('WSRT',n7)]
  lcstr.band=[band1,band2,band3,band4,band5,band6,band7]
  
  plot,[10,1e6],[25,5],/nodata,/xlog,xtitle='T-T0 (s)',ytitle='mag',yrange=[25,5],/yst
  oploterror,lcstr.time,lcstr.flux,lcstr.fluxerr,psym=3,/nohat
  for i=0,n_elements(lcstr)-n7-1 do oplot,[lcstr[i].tmin,lcstr[i].tmax],[lcstr[i].flux,lcstr[i].flux]
    
  
  plot,[10,1e6],[0,200],/nodata,/xlog,xtitle='T-T0 (s)',ytitle=!tsym.mu+'Jy'
  oploterror,time7,flux7,fluxerr7,psym=3,/nohat
  for i=0,n_elements(time7)-1 do oplot,[tmin7[i],tmax7[i]],[flux7[i],flux7[i]]
  
  
  !p.multi=0
  stop
  return
end 
     
