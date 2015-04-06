pro grb060807_astrometry
  
  grbra=252.512375d
  grbdec=31.5918055556d
  grberr=4.2
  
  nordra=252.510833333d
  norddec=31.5918611111d
  norderr=0.5
  
  ralist=[grbra,nordra]
  declist=[grbdec,norddec]
  errlist=[grberr,norderr]
  names=['XRT refined','Nordic']
  
  xrt_astrometry,'sdss.csv','','',grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,/sdss,/filt,iter=7,snr=5,dir='00*-xrt/'
  
  return
end 
