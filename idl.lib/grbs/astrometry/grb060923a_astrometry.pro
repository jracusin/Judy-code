pro grb060923a_astrometry
  
  grbra=254.6175d
  grbdec=12.3611111111d
  grberr=6.0
  
  optra=254.617291667d
  optdec=12.3608055556d
  opterr=0.5
  
  ralist=[grbra,optra]
  declist=[grbdec,optdec]
  errlist=[grberr,opterr]
  names=['XRT refined','Optical']
  
  optfile='sdss.csv'
;  optfile='dposs.txt'
  xrt_astrometry,optfile,'','',grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/dposs,/filt,dir='00*-xrt/',iter=5
  
  return
end 
