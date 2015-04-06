pro grb050408_astrometry,ps=ps
  
  grbra=180.57204d
  grbdec=10.85315d
  grberr=6.
  
  optra=180.5722
  optdec=10.852631d
  opterr=0.25

  ralist=[grbra,optra]
  declist=[grbdec,optdec]
  errlist=[grberr,opterr]
  names=['XRT refined','optical']
  
  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,'sdss.csv',evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/sdss,dir='00*-xrt/',iter=3
  if keyword_set(ps) then endplot
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
