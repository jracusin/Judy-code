pro grb050822_astrometry,ps=ps,skip=skip
  
  ;;SX source
  sxra=51.09125d
  sxdec=-46.0363055556d
  sxerr=3.5
  
  usnora=51.0933333333d
  usnodec=-46.0368055556d
  usnoerr=0.2
  
  ;;grb
  grbra= 51.11125d
  grbdec=-46.0336666667d
  grberr=3.5
  ;03 24 26.7  and Dec (J2000) = -46 02 01.2
  
  butra=51.1134166667d
  butdec=-46.0333333333d
  buterr=0.7
  ; 03 24 27.22 	 -46 02 00.0 	 0.7
  
;  optra=
;  optdec=
;  opterr=

  ralist=[grbra,butra]
  declist=[grbdec,butdec]
  errlist=[grberr,buterr]
  names=['XRT refined','Butler']
  
  evtfiles=''
  exfiles=''
  
  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color  
  xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,mra,mdec,mraerr,dir='00*-xrt/',skip=skip,phamin=50,/filt,suffix='_f100',iter=4,/wav
  if keyword_set(ps) then endplot
  src=mrdfits('src_wavdetect_f100.fits',1)
  ralist=[ralist,src[16].ra]
  declist=[declist,src[16].dec]
  errlist=[errlist,3.59]
  names=[names,'xrtcentroid']
  
  if keyword_set(ps) then begplot,name='grb050822_astrometry.ps',/color
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title='GRB050822',/donames
  if keyword_set(ps) then endplot
  

  sx=20
  poserr=mrdfits('/bulk/pkg/caldb/data/swift/xrt/bcf/instrument/swxposerr20010101v003.fits',3)
  hpd=poserr.par1
  p=-poserr.par2
  newxra=src.ra-mra/3600./cos(src.dec*!dtor)
  newxdec=src.dec-mdec/3600.
  newxerr=sqrt(2.*mraerr^2+(hpd*(src.counts)^p)^2+0.9^2)
  newsxra=newxra[sx]
  newsxdec=newxdec[sx]
  newsxerr=newxerr[sx]
  srcerr=sqrt((hpd*(src.counts)^p)^2+poserr.errsys^2)
  
  if keyword_set(ps) then begplot,name='sxplot.ps',/color
  
  compare_plots,sxra,sxdec,[sxra,usnora,src[sx].ra],[sxdec,usnodec,src[sx].dec],[sxerr,usnoerr,srcerr[sx]],['SX','USNO','xcntd'],newsxra,newsxdec,newsxerr,/do
  if keyword_set(ps) then endplot
  colprint,src.ra,src.dec,srcerr,newxra,newxdec,newxerr,separation(src.ra,src.dec,newxra,newxdec)
  
  print,'source sx'
  print,src[sx].ra,src[sx].dec,srcerr[sx],newxra[sx],newxdec[sx],newxerr[sx]
  print,'grb'
  grb=16
  print,src[grb].ra,src[grb].dec,srcerr[grb],newxra[grb],newxdec[grb],newxerr[grb]
  print,'sep xcntrd & new',separation(src[sx].ra,src[sx].dec,newxra[sx],newxdec[sx])
  print,'sep Osx & new',separation(sxra,sxdec,newxra[sx],newxdec[sx])
  print,'sep USNO & new',separation(usnora,usnodec,newxra[sx],newxdec[sx])
  print,'sep USNO & Osx',separation(sxra,sxdec,usnora,usnodec)
  
  stop
  return
end 
