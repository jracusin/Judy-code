pro grb061201_astrometry,skip=skip,ps=ps
  
  grbra=332.134208333d
  grbdec=-74.5798888889d
  grberr=3.6

  optra=332.134208333d
  optdec=-74.5798055556d
  opterr=1.0

  optra2=332.133708333d
  optdec2=-74.5797444444d
  opterr2=0.2

  butra=332.133708333d
  butdec=-74.5800555556d
  buterr=3.6
  
  ralist=[grbra,optra,optra2,butra]
  declist=[grbdec,optdec,optdec2,butdec]
  errlist=[grberr,opterr,opterr2,buterr]
  
  names=['XRT refined','UVOT','VLT','Butler']
  
  n=[0,1,2,3]
  ralist=ralist[n]
  declist=declist[n]
  errlist=errlist[n]
  names=names[n]
  
 newra=grbra & newdec=grbdec & newerr=grberr 
;  window,0
  wset,0
;  xrt_astrometry,'','',grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,/filt,iter=7,dir='00*-xrt/',skip=skip,suffix='_f100',/wav
  
  if not keyword_set(ps) then window,1
  if keyword_set(ps) then begplot,name='grb061201_astrometry.ps',/color
  
  compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title='GRB 061028',/donames,/iso,/nonew
  
;  window,2
;  make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,_extra=_extra
  
  if keyword_set(ps) then begin 
     endplot
     spawn,'convert grb061201_astrometry.ps grb061201_astrometry.gif'
  endif 
  
  stop
  return
end 
