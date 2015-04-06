pro grb061028_astrometry,skip=skip,ps=ps
  
  grbrabs=97.2268705171d   ;with new boresight
  grbdecbs=46.2986769066d
  grberrbs=3.87
  grbra= 97.2272083333d    ;with old boresight (from circular)
  grbdec=46.2986666667d
  grberr=3.9
  
  optra=97.2283333333d  ;06:28:54.8 cenko gcn 5770
  optdec=46.2991111111d ;+46:17:56.8
  opterr=0
  
  optra2=97.2275d  ;06:28:54.6  ;bloom gcn 5768
  optdec2=46.3002777778   ;46:18:01
  opterr2=0
  
  butra=97.227542d
  butdec=46.299306
  buterr=2.7
  
  ralist=[grbra,grbrabs,optra2];,butra]
  declist=[grbdec,grbdecbs,optdec2];,butdec]
  errlist=[grberr,grberrbs,opterr2];,buterr]
  
  names=['XRT refined','XRT boresight corr','Bloom et al.'];,'Butler']
  
  n=[0,2,3]
  ralist=ralist[n]
  declist=declist[n]
  errlist=errlist[n]
  names=names[n]
  
  
;  window,0
  wset,0
  xrt_astrometry,'','',grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,/filt,iter=7,dir='00*-xrt-bs/',skip=skip,suffix='_f100';,/usno
  
  if not keyword_set(ps) then window,1
  if keyword_set(ps) then begplot,name='grb061028_astrometry.ps',/color
  
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title='GRB 061028',/donames,/iso
  
;  window,2
;  make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,_extra=_extra
  
  if keyword_set(ps) then begin 
     endplot
     spawn,'convert grb061028_astrometry.ps grb061028_astrometry.gif'
  endif 
  
  stop
  return
end 
