pro fermi_swift_stats

  readcol,'~/Swift/grb_table_1382922859.txt',grb,trigtime,trignum,batra,batdec,batt90,batfluence,xrtra,xrtdec,xrttime,xrtflux,uvotra,uvotdec,uvottime,uvotmag,format='(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',delim='  ',skip=1
  
  ns=n_elements(grb)
  swift=create_struct('grb','','trigtime','','met',0d,'t90',0.,'xrtdet',0,'uvotdet',0,'redshift',0.,'gbmname','')
  swift=replicate(swift,ns)

  swift.grb=grb
  swift.trigtime=trigtime
  year=strmid(swift.grb,0,2)
  month=strmid(swift.grb,2,2)
  day=strmid(swift.grb,4,2)
  for i=0,ns-1 do begin
     swift[i].met=date2met('20'+year[i]+'-'+month[i]+'-'+day[i]+'-'+swift[i].trigtime)
  endfor 
;  swift.trignum=long(trignum)
;  swift.t90=batt90
  w=where(strtrim(xrtra,2) ne 'n/a')
  swift[w].xrtdet=1
  w=where(strtrim(uvotra,2) ne 'n/a')
  swift[w].uvotdet=1
;  w=where(strtrim(redshift,2) ne 'n/a')
;  swift[w].redshift=float(redshift[w])
  ns=485
  swift=swift[1:ns-1] ;;; cutting only overlap with fermi
  ns=ns-1
  s=ns-indgen(ns)
  swift=swift[s]

  ;;; cross correlated Davide's file to get redshifts through 2012
  dd=mrdfits('~/Swift/swiftgrb2.fits',1)
  nd=n_elements(dd)
  for i=0,ns-1 do begin
     w=where('GRB'+swift[i].grb eq dd.name,nw)
     if nw gt 0 then swift[i].redshift=dd[w].redshift
  endfor 
  swift[482].redshift=0.717
  swift[476].redshift=0.35
  swift[472].redshift=1.238
  swift[470].redshift=0.4791
  swift[454].redshift=1.155
  swift[447].redshift=2.006
  swift[446].redshift=2.092
  swift[442].redshift=5.913
  swift[439].redshift=0.3564
  swift[429].redshift=1.3033
  swift[427].redshift=2.27
  swift[424].redshift=2.78
  swift[423].redshift=0.34
  swift[421].redshift=1.297
  swift[419].redshift=1.218
  swift[418].redshift=3.757
  swift[410].redshift=0.597
  swift[407].redshift=2.539

  gbm=mrdfits('~/Swift/GBM_burstcat.fits',1)
  s=sort(gbm.name)
  gbm=gbm[s]
  ng=n_elements(gbm)
  for i=0,ng-1 do begin 
     v=date_conv(gbm[i].trigger_time+2400000.5,'V')
     utc=ntostr(v[0],4)+'-'+ntostr(fix(v[1]))+'-'+ntostr(v[2],2)+':'+ntostr(v[3],2)+':'+ntostr(v[4],4)
     met=date2met(utc)
     gbm[i].trigger_time=met
  endfor 

  for i=0,ns-1 do begin
     d=abs(swift[i].met-gbm.trigger_time)
    
     m=min(d,wm)
     if d[wm] lt 300 then swift[i].gbmname=gbm[wm].name

  endfor 

  help,where(swift.gbmname ne '') ;;; gbm/bat
  help,swift
  help,where(swift.xrtdet)
  help,where(swift.uvotdet)
  help,where(swift.xrtdet and swift.gbmname ne '')
  help,where(swift.xrtdet and swift.uvotdet)
  help,where(swift.gbmname ne '' and swift.redshift ne 0)
  help,where(swift.redshift ne 0)
  help,where(swift.redshift ne 0 and swift.xrtdet)
  help,where(swift.redshift ne 0 and swift.uvotdet)


  stop
return
end 
