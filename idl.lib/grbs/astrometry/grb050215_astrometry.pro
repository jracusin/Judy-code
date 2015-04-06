pro grb050215_astrometry,ps=ps
  
  grbra=174.44817d
  grbdec=40.79651d
  grberr=6
  
  optra=174.450125d
  optdec=40.7953888889d
  opterr=1.

  ralist=[grbra,optra]
  declist=[grbdec,optdec]
  errlist=[grberr,opterr]
  names=['XRT refined','UVOT']
  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,'sdss.csv',evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/sdss,dir='00*-xrt/',iter=3
  if keyword_set(ps) then endplot
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb050215_astrometry
