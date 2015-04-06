PRO pc_bias_temp, infile,frameno,bias_med,ccd_t

;Reads a catalog file (file.cat) to plot bias values as a function
;of temperature, creates 2 ps.  The bias is calculated as the median
;of the 20 pixels in the header
;

  readcol,infile,frame,format='(a)'
;forprint, frame

  timestart=lonarr(n_elements(frame))
  saa_flag=intarr(n_elements(frame))
  bias_sub=intarr(n_elements(frame))
  bias_thresh=intarr(n_elements(frame))
  bias_real=intarr(n_elements(frame))
  bias_diff=intarr(n_elements(frame))
  bias_med=intarr(n_elements(frame))
  frame_num=intarr(n_elements(frame))
  ccd_t=fltarr(n_elements(frame))
  sigma=fltarr(n_elements(frame))
  evthresh=fltarr(n_elements(frame))
  bias_pixel=intarr(20)
  frameno=intarr(n_elements(frame))

  for i=0,n_elements(frame)-1 do begin
     readframe=frame[i]
     tab=mrdfits(readframe,0,hd0,/silent)
     timestart[i]=sxpar(hd0,'TSTART')
     frameno[i]=sxpar(hd0,'CCD_FRAM')
     saa_flag[i]=sxpar(hd0,'SAA')
     ccd_t[i]=sxpar(hd0,'T_CCD')
     evthresh[i]=sxpar(hd0,'EVTHRESH')
;    bias_sub[i]=sxpar(hd0,'BIAS_LVL')
     for ii=0,19 do begin
        pix_num=string(ii)
        pixel_name='PIXEL'+strtrim(pix_num,2)
        bias_pixel[ii]=sxpar(hd0,pixel_name)
     endfor
     index_median=where(bias_pixel lt 400,n_pix_nosaa)
     if (n_pix_nosaa ne 0) then begin
        bias_median_values=bias_pixel(index_median)
        bias_med[i]=median(bias_median_values)
        sigma[i]=stdev(bias_median_values)
     endif else bias_med[i]=0
;      if ((ccd_t[i] lt -52) and (bias_med[i] gt 200)) then print,readframe 
  endfor
  ev_ind=where(evthresh lt 4095)

  outprint=[transpose(ccd_t),transpose(bias_med)]
  openw,nomelogico,infile+'.ccdtemp_bias.dat',/get_lun
  printf,nomelogico,'CCD temp, Bias,'
  printf,nomelogico,outprint
  free_lun,nomelogico

  res=poly_fit(ccd_t,bias_med,2)
  print,'Fit of BIAS=a+b*CCD_T+c*CCD_t^2'
  print,'a=',res[0]
  print,'b=',res[1]
  print,'c=',res[2]

;ave=fltarr(13)
;ccd_ave_plot=intarr(13)
;avesigma=fltarr(13)
;ave_evthresh=fltarr(13)
  aveindex=0
  round_ccd_t=ceil(ccd_t)
  max_t=max(round_ccd_t)
  if n_elements(where(round_ccd_t eq max_t)) lt 2 then max_t=max_t-1
  mt=70
  eles=max_t+mt+1
  ave=fltarr(eles)
  ccd_ave_plot=intarr(eles)
  avesigma=fltarr(eles)
  ave_evthresh=fltarr(eles)

  for temp_incr=-mt,max_t do begin
     index=where(ccd_t gt temp_incr-0.5 and ccd_t lt temp_incr+0.5)
     aveindex=(temp_incr+mt)
     if index(0) ne -1 then begin
        ave[aveindex]=mean(bias_med[index])
        ccd_ave_plot[aveindex]=temp_incr
        avesigma[aveindex]=mean(sigma[index])
        ave_evthresh[aveindex]=mean(evthresh[index])
     endif
  endfor

;set_plot,'ps'
;psname=infile+'_bias_onboard.ps'
;device,filename=psname,xs=20,ys=15,/col
;tito='Bias level subtracted on board vs CCD temp'
;plot,ccd_t,bias_sub,psym=5,xtit='CCD temp',ytit='BIAS',tit=tito
;device,/close

;  set_plot,'ps'
  psname=infile+'.bias_calc.ps'
  begplot,name=psname,/land
;  !p.multi=[0,1,6]
;  device,filename=psname,xs=20,ys=15,/col
  multiplot,[1,7],/init
  !p.charsize=0.75
  !p.thick=1
  !x.thick=1
  !y.thick=1
  tito='Bias level as median of 20 pixel (without thresh) vs CCD temp - '+infile
  multiplot
  xrange=[min(ccd_t)-1,max(ccd_t)]
  plot,ccd_t,bias_med,psym=5,ytit='BIAS',tit=tito,xtick_get=xtick,xminor=4,xticklen=0.05,xrange=xrange
  multiplot
  plot,ccd_t,sigma,ytit='Sigma',psym=4,xminor=4,xticklen=0.05,xrange=xrange
  multiplot
  plot,ccd_t(ev_ind),evthresh(ev_ind),ytit='LLD',psym=1,yrange=[min(evthresh(ev_ind))-1.,max(evthresh(ev_ind))+1.],xrange=xrange,xminor=4,xticklen=0.05
  ind=where(ccd_ave_plot ne 0.)
  multiplot
  plot,ccd_ave_plot(ind),ave(ind),ytit='Ave Bias',psym=6,xrange=xrange,xminor=4,xticklen=0.05
  multiplot
  plot,ccd_ave_plot(ind),avesigma(ind),xtit='CCD temp',ytit='Ave Sigma',psym=6,xrange=xrange,xtickname=ntostr(xtick,5),xminor=4,xticklen=0.05
;plot,ccd_ave_plot(ind),evthresh(ind),xtit='CCD temp',ytit='LLD',psym=1,yrange=[min(evthresh(ind))-1.,max(evthresh(ind))+1.]
  multiplot
  multiplot
  plot,frameno,bias_med,psym=4,xtit='Frame Number',ytitle='BIAS',xrange=[min(frameno),max(frameno)],xtickformat='(i7)',xminor=5,xticklen=0.05
  multiplot,/reset
  
  endplot
;  device,/close
;  !p.multi=0

  openw,nomelogico,infile+'.ccdtemp_bias.dat',/get_lun
  w=where(ccd_ave_plot ne 0)
  tabprint=[transpose(ccd_ave_plot[w]),transpose(ave[w]),transpose(avesigma[w]),transpose(ave_evthresh[w])]
  printf,nomelogico,'CCD temperature and average bias and sigma (from 20 header pixels) for PC frames'
  printf,nomelogico,'CCD temp          Bias           Sigma        LLD'
  printf,nomelogico,tabprint
  close,nomelogico
  free_lun,nomelogico

  return
end
