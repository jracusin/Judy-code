pro grb060108_astrometry,ps=ps
  
  grbra=147.06666666667
  grbdec=31.91794444444
  grberr=3.9
  
  optra=147.00825
  optdec=31.919055555555
  opterr=1

  ralist=[grbra,optra]
  declist=[grbdec,optdec]
  errlist=[grberr,opterr]
  names=['XRT refined','optical']
  
  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,'sdss.csv','','',grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/sdss,dir='00*-xrt/',/filt
  if keyword_set(ps) then endplot
  
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
