pro grb070621_astrometry,ps=ps,skip=skip
  
  grbra=323.79115d
  grbdec=-24.81827d
  grberr=3.64
  
  butra=323.791458333d
  butdec=-24.8175d
  buterr=2.5
  
  optra=323.79225d
  optdec=-24.81753d
  opterr=1.8

  ralist=[grbra,butra,optra]
  declist=[grbdec,butdec,optdec]
  errlist=[grberr,buterr,opterr]
  names=['XRT centroid','Butler','UVOT-XRT']
  
  evtfiles=''
  exfiles=''

  if keyword_set(ps) then begplot,name='astrometry.ps',/land,/color
  xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,merr,ralist,declist,errlist,names,dir='00*-xrt/',skip=skip,/filt,suffix='_f100',/wav;,phamin=50
  if keyword_set(ps) then begin 
     endplot
     begplot,name='grb070621_astrometry.ps',/color
  endif 

  make_astrom_plot,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,merr,title='GRB070621'

  if keyword_set(ps) then endplot
  
  return
end 

;get_sdc_data,2005,02,106107,17
;run_xrtpipeline
;grb_astrometry
