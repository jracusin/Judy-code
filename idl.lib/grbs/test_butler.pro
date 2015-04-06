pro test_butler,ps=ps,domatch=domatch,noerr=noerr
  
;  butfits='~/astrometry/butler/butler.fits'
;  if not exist(butfits) then begin 
  ;;read butler file
  butfile='~/astrometry/butler/xrt_pos.html'
  
  ra='' & dec='' & err=0d & name='' & targid=0L
  openr,lun,butfile,/get_lun
  for i=0,6 do line=readline(lun)
  while not eof(lun) do begin
     
     line=readline(lun,delim='</td><td>')
;     chunks=str_sep(line)
     if n_elements(line) gt 4 then begin 
        tmpra=str_sep(line[1],' ')
        tmpdec=str_sep(line[2],' ')
        hms2radec,tmpra[1],tmpra[2],tmpra[3],tmpdec[1],tmpdec[2],tmpdec[3],ras,decs
        tmpnm1=str_sep(line[0],' ')
        tmpnm2=str_sep(tmpnm1[3],'GRB')
        
        tid1=str_sep(line[0],'(')
        tid2=str_sep(tid1[1],')')
        
        name=[name,tmpnm2[1]]
        ra=[ra,ras]
        dec=[dec,decs]
        err=[err,line[3]]
        targid=[targid,tid2[0]]
        
     endif 
     i=i+1
     
  endwhile
  
  name=name[1:*]
  ra=ra[1:*]
  dec=dec[1:*]
  err=err[1:*]
  targid=targid[1:*]
  

  butcat0=create_struct('grbname','',$
                        'targid',0L,$
                        'ra',0d,$
                        'dec',0d,$
                        'err',0d,$
                        'ora',0d,$
                        'odec',0d,$
                        'oerr',0d)
  butcat0=replicate(butcat0,n_elements(name))
  butcat0.grbname=name
  butcat0.ra=ra
  butcat0.dec=dec
  butcat0.err=err

  butcat0.targid=targid*1L
  
  readcol,'~/astrometry/butler/ot.txt',tid,grb,rah,ram,ras,decd,decm,decs,boerr,format='(l,a,a,a,a,a,a,a,a)'
  hms2radec,rah,ram,ras,decd,decm,decs,bora,bodec
  match,targid,tid,om1,om2
  butcat0[om1].ora=bora[om2]
  butcat0[om1].odec=bodec[om2]
  butcat0[om1].oerr=boerr[om2]
  
;     mwrfits,butcat0,butfits,/create
;     butcat00=butcat0
;  endif else butcat00=mrdfits(butfits,1)
;  butcat00.err=sqrt(butcat0.err^2+0.5^2)
  
  if keyword_set(domatch) then begin 
;     dontuse=['041223','050124','050126','050219B','050412','050502B','050509B','050716','050726',$
;              '050824','050904','051111','060111A','060115','060124','060204B','060323','060502B',$
;              '060906']
;     dont_match,butcat0.grbname,dontuse,mn1,mn2
     mn1=where(butcat0.ora ne 0)
     butcat=butcat0[mn1]
     title='Match Sample'
     msuf='_match'
  endif else begin
     butcat=butcat0
     title='Full Sample'
     msuf='_full'
  endelse 
  
  ;;read joe's file
  joefile='~/astrometry/butler/co_ord_4judy.csv'
  openr,lun,joefile,/get_lun
  line=readline(lun)
  grbname='' & targid2=0L & xra=0d & xdec=0d & xerr=0d & ora=0d & odec=0d & oerr=0d
  while not eof(lun) do begin
     line=readline(lun,delim=',')
     tmpname1=str_sep(line[1],' ')
     tmpname2=str_sep(tmpname1[1],'"')
     grbname=[grbname,tmpname2[0]]

     targid2=[targid2,line[2]]
     xra=[xra,line[3]*1d]
     xdec=[xdec,line[4]*1d]
     xerr=[xerr,line[5]*1d]
     ora=[ora,line[7]*1d]
     odec=[odec,line[8]*1d]
     oerr=[oerr,line[9]*1d]
     
  endwhile
  close,/all
  
  joecat=create_struct('grbname','',$
                       'targid',0L,$
                       'xra',0d,$
                       'xdec',0d,$
                       'xerr',0d,$
                       'ora',0d,$
                       'odec',0d,$
                       'oerr',0d)
  
  joecat=replicate(joecat,n_elements(grbname)-1)
  joecat.grbname=grbname[1:*]
  joecat.targid=targid2[1:*]
  joecat.xra=xra[1:*]*1d
  joecat.xdec=xdec[1:*]*1d
  joecat.xerr=xerr[1:*]*1d
  joecat.ora=ora[1:*]*1d
  joecat.odec=odec[1:*]*1d
  joecat.oerr=oerr[1:*]*1d
  
  wo=where(oerr eq 0,nwo)
  if nwo gt 0 then joecat[wo].oerr=0.5
  
  match,butcat.targid,joecat.targid,m1,m2
  n=n_elements(m1)
  xsep=separation(joecat[m2].xra,joecat[m2].xdec,joecat[m2].ora,joecat[m2].odec)
;  osep=separation(butcat[m1].ra,butcat[m1].dec,joecat[m2].ora,joecat[m2].odec)
  osep=separation(butcat[m1].ra,butcat[m1].dec,butcat[m1].ora,butcat[m1].odec)
  if keyword_set(noerr) then begin
     bsep=osep-butcat[m1].err 
     esuf='_noerr'     
  endif else begin
;     bsep=osep-butcat[m1].err-butcat[m1].oerr;joecat[m2].oerr
     bsep=osep-sqrt(butcat[m1].err^2+butcat[m1].oerr^2)
     esuf='_err'
  endelse 
;  xosep=osep-joecat[m2].xerr-joecat[m2].oerr
;  xosep=osep-sqrt(joecat[m2].xerr^2+joecat[m2].oerr^2)
  
  if keyword_set(ps) then begplot,name='test_butler'+msuf+esuf+'.ps' else wset,0
  
  !p.charsize=1
  
  
  yrange=[0,20]
  !p.multi=[0,2,3]
  
  w1=where(butcat[m1].err ge 0. and butcat[m1].err lt 1.,nw1)
  w2=where(butcat[m1].err ge 1. and butcat[m1].err lt 2.,nw2)
  w3=where(butcat[m1].err ge 2. and butcat[m1].err lt 3.,nw3)
  w4=where(butcat[m1].err ge 3.,nw4)
  help,w1,w2,w3,w4
  
  if keyword_set(noerr) then xtitle=!tsym.delta_cap+'X!LB!NO-X!LB,err!N' else $
     xtitle=!tsym.delta_cap+'X!LB!NO-X!LB,err!N-O!Lerr!N'
  
  plothist,bsep[w1],xtitle=xtitle,bin=0.5,xrange=[-10,10],yrange=yrange
  w=where(bsep[w1] lt 0.0,nw)
  print,nw*100./nw1*1.
  legend,['0" < X!LB,err!N < 1"',$
          'mean = '+sigfig(mean(bsep[w1]),3),'median = '+sigfig(median(bsep[w1]),3),$
          sigfig(nw*100./nw1*1.,3)+'% < 0"','N = '+ntostr(nw1)],box=0,/top,/right,charsize=1.
  
  plothist,bsep[w2],xtitle=xtitle,bin=0.5,xrange=[-10,10],yrange=yrange
  w=where(bsep[w2] lt 0.0,nw)
  print,nw*100./nw2*1.
  legend,['1" < X!LB,err!N < 2"',$
          'mean = '+sigfig(mean(bsep[w2]),3),'median = '+sigfig(median(bsep[w2]),3),$
          sigfig(nw*100./nw2*1.,3)+'% < 0"','N = '+ntostr(nw2)],box=0,/top,/right,charsize=1.
  
  plothist,bsep[w3],xtitle=xtitle,bin=0.5,xrange=[-10,10],yrange=yrange
  w=where(bsep[w3] lt 0.0,nw)
  print,nw*100./nw3*1.
  legend,['2" < X!LB,err!N < 3"',$
          'mean = '+sigfig(mean(bsep[w3]),3),'median = '+sigfig(median(bsep[w3]),3),$
          sigfig(nw*100./nw3*1.,3)+'% < 0"','N = '+ntostr(nw3)],box=0,/top,/right,charsize=1.
  
  plothist,bsep[w4],xtitle=xtitle,bin=0.5,xrange=[-10,10],yrange=yrange
  w=where(bsep[w4] lt 0.0,nw)
  print,nw*100./nw4*1.
  legend,['X!LB,err!N > 3"',$
          'mean = '+sigfig(mean(bsep[w4]),3),'median = '+sigfig(median(bsep[w4]),3),$
          sigfig(nw*100./nw4*1.,3)+'% < 0"','N = '+ntostr(nw4)],box=0,/top,/right,charsize=1.
  
  
  plothist,bsep,xtitle=xtitle,bin=0.5,xrange=[-10,10],yrange=yrange
  w=where(bsep lt 0.0,nw)
  print,nw*100./n*1.
  legend,['X!LB,err!N',$
          'mean = '+sigfig(mean(bsep),3),'median = '+sigfig(median(bsep),3),$
          sigfig(nw*100./n*1.,3)+'% < 0"','N = '+ntostr(n)],box=0,/top,/right,charsize=1.
  
  
;   goto,jump2
;   judycat=mrdfits('~/grbs/grb_astrometry_f100_sdss.fits',1)
;   w=where(judycat.new_xra ne 0)
;   judycat=judycat[w]
;   match,butcat.targid,judycat.tid,m1,m2
;   bjsep=separation(butcat[m1].ra,butcat[m1].dec,judycat[m2].new_xra,judycat[m2].new_xdec)
;   bjseperr=bjsep-butcat[m1].err-judycat[m2].new_xerr
  
;   !p.multi=[0,2,2]
;   plothist,bjsep,bin=0.5,xtitle=!tsym.delta_cap+'X!LB!NX!LJ!N'
;   plothist,bjseperr,bin=0.5,xtitle=!tsym.delta_cap+'X!LB!NX!LJ!N-X!LB,err!N-X!LJ,err!N'
  
;   !p.multi=0
;   jump2:
  
  !p.multi=0
  if not keyword_set(ps) then window,1 else begplot,name='butler_fig3'+msuf+'.ps',/land
  s=sort(butcat[m1].err)
  n=n_elements(m1)
;  s=indgen(n)
  bsep1=separation(butcat[m1[s]].ra,butcat[m1[s]].dec,joecat[m2[s]].ora,joecat[m2[s]].odec)
  bsep2=separation(butcat[m1[s]].ra,butcat[m1[s]].dec,butcat[m1[s]].ora,butcat[m1[s]].odec)
  plot,indgen(n),bsep1,psym=5,/ylog,yrange=[0.05,15],/ystyle,xrange=[0,n+2],/xstyle,$
     ytitle='Position offset from OT (arcsec)',title=title
  oplot,indgen(n),bsep2,psym=1,color=!red
  oplot,butcat[m1[s]].err
  oplot,butcat[m1[s]].err+joecat[m2[s]].oerr,line=1
  oplot,butcat[m1[s]].err+butcat[m1[s]].oerr,line=1,color=!red
  oplot,sqrt(butcat[m1[s]].err^2+joecat[m2[s]].oerr^2),line=2
  oplot,sqrt(butcat[m1[s]].err^2+butcat[m1[s]].oerr^2),line=2,color=!red
  oplot,joecat[m2[s]].xerr,psym=10,color=!green
  legend,['XRT team err','Butler err','Butler+Lorella optical','Butler+Butler optical',!tsym.sqrt+'(Butler!U2!N+Optical!U2!N)'],line=[0,0,1,1,2],/top,/left,box=0,color=[!green,!p.color,!p.color,!red,!p.color],textcolor=[!green,!p.color,!p.color,!red,!p.color]
  
  if keyword_set(ps) then endplot
  
;  w1=where(butcat[m1[s]].err ge 0. and butcat[m1[s]].err lt 2.,nw1)
;  q=bsep-butcat[m1[s]].err-joecat[m2[s]].oerr
;  wq=where(q gt 0.0)
  
  w1=where(bsep2 lt butcat[m1[s]].err,nw1)
  w2=where(bsep2 lt butcat[m1[s]].err+butcat[m1[s]].oerr,nw2)
  w3=where(bsep2 lt sqrt(butcat[m1[s]].err^2+butcat[m1[s]].oerr^2),nw3)
  print,nw1*1./n,nw2*1./n,nw3*1./n
  
;  dx=(butcat[m1[s]].ra-joecat[m2[s]].ora)*cos(butcat[m1[s]].dec)*3600.
;  dy=(butcat[m1[s]].dec-joecat[m2[s]].odec)*3600.
;  s2=sqrt(2.)
;  chi2x=dx^2/(0.5*butcat[m1[s]].err^2+0.5*joecat[m2[s]].oerr^2)
;  chi2y=dy^2/(0.5*butcat[m1[s]].err^2+0.5*joecat[m2[s]].oerr^2)
;  print,mean(chi2x),mean(chi2y)
  
  
  stop
  return
end 
