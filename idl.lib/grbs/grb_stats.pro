pro xrt_time,frac,lastobs

  cd,'~/GRBs/'

  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  if n_elements(frac) eq 0 then begin 
     lastobs=dblarr(n)
     frac=fltarr(n)
     for i=0,n-1 do begin 
        grb=grbs[i]
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits('lc_newout_phil.txt')
           lastobs[i]=max(lc.tstop)
           year=strmid(grb,3,2)+2000.
           month=strmid(grb,5,2)*1.
           day=strmid(grb,7,2)*1.
           
           dn=ymd2dn(year,month,day)
           frac[i]=year+dn/365
           
        endif 
        cd,'..'
     endfor 
  endif 

  begplot,name='~/Swift/Munich_swift_status_2012/xrt_lastobs.eps',/encap,/land,/color,font='helvetica'
  w=where(frac ne 0)
  frac=frac[w]
  lastobs=lastobs[w]
  plot,frac[w],lastobs[w],/ylog,psym=1,xtitle='Year',ytitle='End of XRT Observations (s)',charsize=2,yrange=[1e3,1d8]
  med=fltarr(10)
  for i=0,10 do begin
     w=where(frac ge 2004+i and frac lt 2004+i+1,nw)
     if nw gt 0 then begin 
        med[i]=median(lastobs[w])
     endif
  endfor 

  oplot,indgen(10)+2004,med,color=!blue,thick=10
  legend,'Median value',textcolor=!blue,/top,/right,box=0,charsize=2
  endplot
  spawn,'convert ~/Swift/Munich_swift_status_2012/xrt_lastobs.eps ~/Swift/Munich_swift_status_2012/xrt_lastobs.pdf'

  stop
return
end 

pro grb_stats

  readcol,'~/Swift/Munich_swift_status_2012/grb_stats_120504.txt',grb,tid,bat_detection,t90,xrt_detection,xrt_onsource,uvot_detection,uvot_onsource,redshift,radio_detection,infra_detection,opt_detection,format='(a,l,a,f,a,d,a,d,f,a,a,a)',delim='|',skip=3

  year=strmid(grb,4,2)+2000.
  month=strmid(grb,6,2)*1.
  day=strmid(grb,8,2)*1.

  dn=ymd2dn(year,month,day)
  frac=year+dn/365.
  bin=6./12.
  xrange=[2004,2014]

  begplot,name='~/Swift/Munich_swift_status_2012/burst_detection.eps',/encap,/color,font='helvetica'
  !x.margin=[10,0]
  !y.margin=[4,0]
  multiplot,[1,2],/init
  multiplot
  b=where(strtrim(bat_detection,2) eq 'Y' and t90 gt 2.,nb)
  plothist,frac[b],x,y,/fill,xrange=xrange,/xsty,charsize=2,title='BAT Detected GRBs';,/noplot,bin=bin
  legend,['Long'],/top,/right,box=0,charsize=2
  x=where(strtrim(xrt_detection[b],2) eq 'Y',nx)
  plothist,frac[b[x]],x,y,fcolor=!red,/fill,xrange=xrange,/over
  w=where(xrt_onsource[b] lt 200. and xrt_onsource[b] ne 0.,n2x)
  w2=where(strtrim(xrt_detection[b[w]],2) eq 'Y',nx2)

  o=where(strtrim(opt_detection[b],2) eq 'Y',no)
  plothist,frac[b[o]],x,y,/over,fcolor=!green,/fill,xrange=xrange

  z=where(strtrim(redshift[b],2) ne 0,nz)

  u=where(strtrim(uvot_detection[b],2) eq 'Y',nu)
  plothist,frac[b[u]],x,y,/over,fcolor=!blue,/fill,xrange=xrange
  w=where(uvot_onsource[b] lt 200. and uvot_onsource[b] ne 0.,n2u)
  w2=where(strtrim(uvot_detection[b[w]],2) eq 'Y',nu2)

  i=where(strtrim(infra_detection[b],2) eq 'Y',ni)
  plothist,frac[b[i]],x,y,/over,fcolor=!purple,/fill,xrange=xrange

  r=where(strtrim(radio_detection[b],2) eq 'Y',nr)
  plothist,frac[b[r]],x,y,/over,fcolor=!magenta,/fill,xrange=xrange

  w=where(frac[b] ge 2012.)
  plothist,frac[b[w]],/fill,/fline,forient=45,/over,fcolor=!white

  multiplot
  bs=where(strtrim(bat_detection,2) eq 'Y' and t90 le 2.,nbs)
  plothist,frac[bs],x,y,/fill,xrange=xrange,xtitle='Year',/xsty,charsize=2;,/noplot,bin=bin
  legend,['Short'],/top,/right,box=0,charsize=2
  xs=where(strtrim(xrt_detection[bs],2) eq 'Y',nxs)
  plothist,frac[bs[xs]],x,y,fcolor=!red,/fill,xrange=xrange,/over
  w=where(xrt_onsource[bs] lt 200. and xrt_onsource[bs] ne 0.,n2xs)
  w2=where(strtrim(xrt_detection[bs[w]],2) eq 'Y',nx2s)

  os=where(strtrim(opt_detection[bs],2) eq 'Y',nos)
  plothist,frac[bs[os]],x,y,/over,fcolor=!green,/fill,xrange=xrange

  zs=where(strtrim(redshift[bs],2) ne 0,nzs)

  us=where(strtrim(uvot_detection[bs],2) eq 'Y',nus)
  plothist,frac[bs[us]],x,y,/over,fcolor=!blue,/fill,xrange=xrange
  w=where(uvot_onsource[bs] lt 200. and uvot_onsource[bs] ne 0.,n2us)
  w2=where(strtrim(uvot_detection[bs[w]],2) eq 'Y',nu2s)

  is=where(strtrim(infra_detection[bs],2) eq 'Y',nis)
  plothist,frac[bs[is]],x,y,/over,fcolor=!purple,/fill,xrange=xrange

  rs=where(strtrim(radio_detection[bs],2) eq 'Y',nrs)
  plothist,frac[bs[rs]],x,y,/over,fcolor=!magenta,/fill,xrange=xrange
  w=where(frac[bs] ge 2012.)
  plothist,frac[bs[w]],/fill,/fline,forient=45,/over,fcolor=!white

  axis,xaxis=0,xtickname=replicate(' ',7)
  legend,['BAT','XRT','UVOT','Optical','NIR','Radio'],box=0,textcolor=[!p.color,!red,!blue,!green,!purple,!magenta],charsize=2,position=[2013.8,20];,psym=[1,2,4,5,8],color=[!p.color,!magenta,!blue,!green,!red]

  xyouts,2003,5,'Number of GRBs per year',orientation=90,charsize=2
  multiplot,/reset,/default

  endplot
  spawn,'convert ~/Swift/Munich_swift_status_2012/burst_detection.eps ~/Swift/Munich_swift_status_2012/burst_detection.pdf'

  begplot,name='~/Swift/Munich_swift_status_2012/redshift.eps',/encap,/color,font='helvetica'
  !x.margin=[10,0]
  !y.margin=[4,15]
  multiplot,[1,2],/init
  multiplot
  
  plothist,frac[b[z]],x,y,fcolor=!orange,/fill,xrange=xrange
  w=where(frac[b[z]] ge 2012.)
  plothist,frac[b[z[w]]],/fill,/fline,forient=45,/over,fcolor=!white

  axis,xaxis=0,xtickname=replicate(' ',7)
  legend,['Long'],/top,/right,box=0,charsize=2
  multiplot
  plothist,frac[bs[zs]],x,y,fcolor=!orange,/fill,xrange=xrange,yrange=[0,4],xtitle='Year'

  w=where(frac[bs[zs]] ge 2012.)
;  plothist,frac[bs[zs[w]]],/fill,/fline,forient=45,/over,fcolor=!white

  legend,['Short'],/top,/right,box=0,charsize=2
  axis,xaxis=0,xtickname=replicate(' ',7)

  xyouts,2003,1,'Number of GRBs per year',orientation=90,charsize=2
  endplot
  spawn,'convert ~/Swift/Munich_swift_status_2012/redshift.eps ~/Swift/Munich_swift_status_2012/redshift.pdf'
  

  nb=nb*1.
  n2x=n2x*1.
  n2u=n2u*1.
  nbs=nbs*1.
  n2xs=n2xs*1.
  n2us=n2us*1.
  l=where(t90 gt 2.)
  s=where(t90 le 2.)
  n=where(strtrim(bat_detection,2) eq 'U',nn)
;  ns=where(strtrim(bat_detection[s],2) eq 'U',nns)
  
  print,'Total:'
  print,'BAT bursts = ',nb+nbs
  print,'Non-BAT bursts = ',nn
  print,'XRT detections = ',nx+nxs,(nx+nxs)/(nb+nbs)
  print,'XRT detections (<200s) = ',nx2+nx2s,(nx2+nx2s)/(n2x+n2xs)
  print,'UVOT detections = ',nu+nus,(nu+nus)/(nb+nbs)
  print,'UVOT detections (<200s) = ',nu2+nu2s,(nu2+nu2s)/(n2u+n2us)
  print,'Optical detections = ',no+nos,(no+nos)/(nb+nbs)
  print,'IR detections = ',ni+nis,(ni+nis)/(nb+nbs)
  print,'Radio detections = ',nr+nrs,(nr+nrs)/(nb+nbs)
  print,'Redshift detections = ',nz+nzs,(nz+nzs)/(nb+nbs)
  print
  print,'Long:'
  print,'BAT bursts = ',nb
  print,'Non-BAT bursts = ',nn
  print,'XRT detections = ',nx,nx/nb
  print,'XRT detections (<200s) = ',nx2,nx2/n2x
  print,'UVOT detections = ',nu,nu/nb
  print,'UVOT detections (<200s) = ',nu2,nu2/n2u
  print,'Optical detections = ',no,no/nb
  print,'IR detections = ',ni,ni/nb
  print,'Radio detections = ',nr,nr/nb
  print,'Redshift detections = ',nz,nz/nb
  print
  print,'Short:'
  print,'BAT bursts = ',nbs
;  print,'Non-BAT bursts = ',nns
  print,'XRT detections = ',nxs,nxs/nbs
  print,'XRT detections (<200s) = ',nx2s,nx2s/n2xs
  print,'UVOT detections = ',nus,nus/nbs
  print,'UVOT detections (<200s) = ',nu2s,nu2s/n2us
  print,'Optical detections = ',nos,nos/nbs
  print,'IR detections = ',nis,nis/nbs
  print,'Radio detections = ',nrs,nrs/nbs
  print,'Redshift detections = ',nzs,nzs/nbs

  stop

  return
end 
