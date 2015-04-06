pro grb060204b_astrometry,ps=ps
  
  grbra=211.81166666666d
  grbdec=27.6761111111d
  grberr=3.6
  
  optra=211.812083333333d
  optdec=27.67677777777777d
  opterr=1.

  ralist=[grbra,optra]
  declist=[grbdec,optdec]
  errlist=[grberr,opterr]
  names=['XRT refined','optical']
  
  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,'sdss.csv',evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/sdss,dir='00*-xrt/'
  if keyword_set(ps) then endplot
  
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
