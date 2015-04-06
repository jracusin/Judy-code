pro grb070406_astrometry,ps=ps,skip=skip
  
  grbra=198.965708333d
  grbdec=16.5128333333d
  grberr=4.1
  
  butra=198.963166667d
  butdec=16.51325d
  buterr=6.4
  
  ;;; Source 2
  grbra=198.932875d  
  grbdec=16.5193611111d
  grberr=4.3d
  
;  optra=
;  optdec=
;  opterr=

  ralist=[grbra];,butra];,optra]
  declist=[grbdec];,butdec];,optdec]
  errlist=[grberr];,buterr];,opterr]
  names=['XRT refined'];,'Butler'];,'optical']
  
  evtfiles=''
  exfiles=''

  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,dir='00*-xrt/',skip=skip,/filt,suffix='_f100',/wav;,phamin=50
  if keyword_set(ps) then endplot
  
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
