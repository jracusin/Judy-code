PRO bias_map_temp, infile,frameno,median_bias,ccd_t

;Reads a catalog file (file.cat) to plot bias values as a function
;of temperature, creates 2 ps.  The bias is calculated as the median
;of the 20 pixels in the header
;

  readcol,infile,frame,format='(a)'

  nf=n_elements(frame)
  ccd_t=fltarr(nf)
  uld=fltarr(nf)
  median_bias=fltarr(nf)
  sigma=fltarr(nf)
  frameno=lonarr(nf)

  for i=0,nf-1 do begin
     readframe=frame[i]
     tab=mrdfits(readframe,0,hd0,/silent)
     frameno[i]=sxpar(hd0,'CCD_FRAM')
     ccd_t[i]=sxpar(hd0,'T_CCD')
     uld[i]=sxpar(hd0,'ULD')
     tab2=tab[180:280,180:280]
     median_bias[i]=median(tab2)
     sigma[i]=stdev(tab2)
  endfor
  uld_ind=where(uld lt 4094)

  outprint=[transpose(ccd_t(sort(ccd_t))),transpose(median_bias(sort(ccd_t))),transpose(sigma(sort(ccd_t))),transpose(uld(sort(ccd_t)))]
  openw,nomelogico,'ccdtemp_biasmap_median.dat',/get_lun
  printf,nomelogico,'Bias Map: CCD temp, median and sigma and ULD'
  printf,nomelogico,'CCD temp       Bias Map Median       Sigma       ULD'
  printf,nomelogico,outprint
  free_lun,nomelogico
  
  ;  set_plot,'ps'
  psname=infile+'_map_vs_temp.ps'
  begplot,name=psname,/land
  multiplot,[1,5],/init
  !p.charsize=0.75
  !p.thick=1
  !x.thick=1
  !y.thick=1
;  device,filename=psname,xs=20,ys=15,/col
  tito='Bias Map Median value vs CCD temp - '+infile
  xrange=[min(ccd_t)-1,max(ccd_t)]
  multiplot
  plot,ccd_t,median_bias,psym=5,ytit='BIAS',tit=tito,xtick_get=xtick,xminor=4,xticklen=0.05,xrange=xrange
  multiplot
  plot,ccd_t,sigma,psym=5,ytit='Sigma',xminor=4,xticklen=0.05,xrange=xrange
  multiplot
  plot,ccd_t(uld_ind),ULD(uld_ind),psym=1,xtit='CCD temp',ytit='ULD',xminor=4,xticklen=0.05,xrange=xrange,xtickname=ntostr(xtick,5)
  multiplot
  multiplot
  plot,frameno,median_bias,psym=4,xtit='Frame Number',ytitle='BIAS',xrange=[min(frameno),max(frameno)],xtickformat='(i7)',xminor=5,xticklen=0.05
  multiplot,/reset
;  device,/close
;  !p.multi=0
  endplot
  
  return
end
