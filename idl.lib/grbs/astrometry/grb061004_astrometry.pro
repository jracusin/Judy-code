pro grb061004_astrometry,skip=skip,ps=ps
  
  grbrabs=97.7949689192d;97.7945108929d   ;with new boresight
  grbdecbs=-45.9057484954d;-45.9062969383d 
  grberrbs=3.61
  grbra=97.7936666667d   ;with old boresight (from circular)
  grbdec=-45.9068333333d
  grberr=5.5
  
;  oldra=97.795678d
;  olddec=-45.906850d
  optra=97.794625d 
  optdec=-45.9079722222d
  opterr=0.3
  
  ralist=[grbra,grbrabs,optra]
  declist=[grbdec,grbdecbs,optdec]
  errlist=[grberr,grberrbs,opterr]
  
  names=['XRT refined','XRT boresight corr','optical']
  optfile='usno.txt'
;  optfile='2mass.txt'
  
  window,0
  xrt_astrometry,'','',grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,/filt,iter=6,dir='00*-xrt/',skip=skip,suffix='_f100';,/usno
  
  if not keyword_set(ps) then window,1
  if keyword_set(ps) then begplot,name='grb061004_astrometry.ps',/color
  
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title='GRB 061004',/donames,/iso
  
;  window,2
;  make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,_extra=_extra
  
  if keyword_set(ps) then begin 
     endplot
     spawn,'convert grb061004_astrometry.ps grb061004_astrometry.gif'
  endif 
  
  stop
  return
end 
