pro grb061217_astrometry,ps=ps,skip=skip
  
  grbra=160.409583333d
  grbdec=-21.1264333333d
  grberr=4.7
  
  butra=160.413833333d
  butdec=-21.1229444444d
  buterr=3.8d
  
;  optra=
;  optdec=
;  opterr=

  ralist=[grbra,butra];,optra]
  declist=[grbdec,butdec];,optdec]
  errlist=[grberr,buterr];,opterr]
  names=['XRT refined','Butler'];,'optical']
  
  evtfiles=''
  exfiles=''

  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,dir='00*-xrt/',skip=skip;,phamin=50,/filt,suffix='_f100'
  if keyword_set(ps) then endplot
  
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
