pro grb051227_astrometry,ps=ps
  
  grbra=125.24133d
  grbdec=31.92512d
  grberr=6.
  
  optra=125.24225d
  optdec=31.9254722222d
  opterr=0.5

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
