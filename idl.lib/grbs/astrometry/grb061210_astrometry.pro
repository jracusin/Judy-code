pro grb061210_astrometry,ps=ps,skip=skip
  
  grbra=144.521541667d
  grbdec=15.6211111111d
  grberr=4.2
  
;  butra=
;  butdec=-21.1229722222d
;  buterr=3.9d
  
;  optra=
;  optdec=
;  opterr=

  ralist=[grbra];,optra]
  declist=[grbdec];,optdec]
  errlist=[grberr];,opterr]
  names=['XRT refined'];,'optical']
  
  evtfiles=''
  exfiles=''

  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,dir='00*-xrt/',skip=skip,phamin=50,/filt,suffix='_f100',iter=6;,/wav
  if keyword_set(ps) then endplot
  begplot,name='grb061210_astrometry.ps',/color
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title='GRB061210',/donames
  endplot
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
