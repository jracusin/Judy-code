pro get_gbm_data,dir,det,file

  ;;; eventually make this download, but for now, just read

  file=file_search(dir+'/glg*tte*'+det+'*fit')
  

  return
end 

pro fermi_composite_lc

  ;; burst info
  dir='~/FermiData/bn160625945'
  dir2='~/GRBWorkdir/GRB160625945'
  ra=308.6
  dec=6.9
  trigtime=488587220.28d

  ;; find which GBM dets
  gbm_angles,ra,dec,trigtime,det
  s=sort(det.angles)
  colprint,det[s].det,det[s].angles
  wbgo=where(det[s].det eq 'b0' or det[s].det eq 'b1')
  bgo=det[s[wbgo[0]]].det
  wnai=where(det[s].det ne 'b0' and det[s].det ne 'b1')
  nai=det[s[wnai[0]]].det
  dets=[nai,bgo]

  ;;; get GBM data
  get_gbm_data,dir,dets,file
;  nai=mrdfits(file[0],2)
;  bgo=mrdfits(file[1],2)
  nai=mrdfits('glg_tte_n9_160625926_v00.fit',2,hdr)
  bgo=mrdfits('glg_tte_b1_160625926_v00.fit',2)
  tstart=sxpar(hdr,'TSTART')
  nai.time=nai.time+tstart
  bgo.time=bgo.time+tstart

  ;;; get LAT data
  lat1=mrdfits(dir2+'/gll_ft1_tr_bn160625945_v00_filt_transient20.fit',1)
  lat2=mrdfits(dir2+'/gll_ft1_tr_bn160625945_v00_filt_source.fit',1)
  xrange=[-30,1000]
;  xrange=[400,700]
  trigs=[trigtime,488587408.82d,488587880.03d]-trigtime

  begplot,name='composite_lc.ps',/land,/color
  !p.charsize=1.4
  multiplot,[1,4],/init
  multiplot
  plothist,nai.time-trigtime,xrange=xrange,/xsty,ytitle='Counts/bin',bin=0.1,xminor=4
;  gbm=mrdfits(dir+'/glg_cspec_n9_bn160625945_v00.pha',2)
;  cts=fltarr(n_elements(gbm))
;  for i=0,n_elements(gbm)-1 do cts[i]=total(gbm[i].counts[1:126])
;  plot,gbm.time,cts,xrange=xrange
  for i=0,2 do oplot,[trigs[i],trigs[i]],[0.01,10000],line=2,color=!blue
  legend,'NaI (n9)',/top,/right,box=0
  multiplot
  plothist,bgo.time-trigtime,xrange=xrange,/xsty,ytitle='Counts/bin',bin=0.1,xminor=4
  for i=0,2 do oplot,[trigs[i],trigs[i]],[0.01,10000],line=2,color=!blue
  legend,'BGO (b1)',/top,/right,box=0
  multiplot
  plothist,lat1.time-trigtime,xrange=xrange,/xsty,ytitle='Count Rate (s!U-1!N)',xminor=4
  for i=0,2 do oplot,[trigs[i],trigs[i]],[0.01,10000],line=2,color=!blue
  legend,'LAT Transient20',/top,/right,box=0
  multiplot
  plotsym,0,0.5,/fill
  plot,lat1.time-trigtime,lat1.energy,psym=8,xrange=xrange,/ylog,/xsty,xtitle='Time since GBM T0 (s)',ytitle='Energy (MeV)',xminor=4
  oplot,lat2.time-trigtime,lat2.energy,psym=8,color=!red
  for i=0,2 do oplot,[trigs[i],trigs[i]],[0.01,1e5],line=2,color=!blue
  legend,['LAT Transient20','LAT Source'],/top,/right,box=0,textcolor=[!p.color,!red]
  multiplot,/reset,/default
  endplot
  ps2pdf,'composite_lc.ps'

stop


  return
end 
