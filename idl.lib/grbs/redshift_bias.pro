pro redshift_bias

;;cr_paper_results,nsig=2.
;;save,last,w0,w12,w23,w123,w3c,w3b,w2b,w2c,gold,silver,pewter,iron,bronze,nojb,cr,file='cr_files.sav'

  cd,!mdata
  restore,'cr_files.sav'
  zz=where(cr[last].z gt 0)
  z=last[zz]
  nozz=where(cr[last].z eq 0)
  noz=last[nozz]

  !p.multi=[0,1,2]
  bin=0.1
  xrange=[1,6]
  yrange=[0,40]

  !p.background=!white
  !p.color=!black
  plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/xsty,/ysty
  plothist,alog10(cr[noz].tstart),bin=bin,xrange=xrange,yrange=yrange,/xsty,/ysty,/fill,fcolor=!grey30,/over,color=!grey30
  plothist,alog10(cr[z].tstart),/over,bin=bin,xrange=xrange,yrange=yrange,/xsty,/ysty

  print,'Mean/Median'
  print,'No z',mean(cr[noz].tstart),median(cr[noz].tstart)
  print,'w/ z',mean(cr[z].tstart),median(cr[z].tstart)

  xrange=[2,9]
  yrange=[0,20]
  plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/xsty,/ysty
  plothist,alog10(cr[noz].tstop),bin=bin,xrange=xrange,yrange=yrange,/xsty,/ysty,/fill,fcolor=!grey30,/over,color=!grey30
  plothist,alog10(cr[z].tstop),/over,bin=bin,xrange=xrange,yrange=yrange,/xsty,/ysty

  print,'Mean/Median'
  print,'No z',mean(cr[noz].tstop),median(cr[noz].tstop)
  print,'w/ z',mean(cr[z].tstop),median(cr[z].tstop)

  !p.multi=0


  ;;; how long have we followed bursts over time w & w/o reshifts?
  n=n_elements(last)
  grb=cr[last].grb
  gdate=strmid(grb,3,6)
  year='20'+strmid(gdate,0,2)
  mn=strmid(gdate,2,2)
  dy=strmid(gdate,4,2)
  met=dblarr(n)
  for i=0,n-1 do met[i]=date2met(year[i]+'-'+mn[i]+'-'+dy[i]+'-12:00:00')

  met=met-date2met('2005-001-00:00:00')
  met=met/86.4e3
  plot,met[zz],cr[z].tstop,psym=1,/ylog,xtitle='GRB day',ytitle='T!Lstop!N'
  oplot,met[nozz],cr[noz].tstop,psym=1,color=!red

  stop
  return
end 
