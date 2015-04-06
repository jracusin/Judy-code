PRO bias_row_temp, infile2, infile

;Reads a catalog file (file.cat) to plot bias values as a function
;of temperature, creates 2 ps.  The bias is calculated as the median
;of the 20 pixels in the header
;
  
  readcol,infile2,frame2,format='(a)'
  
  findtmp=0
  if n_elements(infile) eq 0 then begin 
     tpos=strpos(infile2,'science')
     fb=strmid(infile2,tpos-17,26)
     tfile=fb+'.timeline'
     read_timeline,tl,tfile
     if n_elements(tl) eq 0 then return
     findtmp=1
     frame=strarr(n_elements(frame2))
  endif else readcol,infile,frame,format='(a)'

  nf=n_elements(frame2)
  ccd_t=fltarr(nf)
  ULD=fltarr(nf)
  median_bias=fltarr(nf)
  sigma=fltarr(nf)
  frameno=lonarr(nf)
  
  for i=0,nf-1 do begin
     readframe=frame2[i]
     tab2=mrdfits(readframe,0,hd0,/silent,/dscale)
     ULD[i]=sxpar(hd0,'BR_ULD')
     rftime=sxpar(hd0,'TSTART')
     if findtmp then begin 
        m=min(abs(tl.met-rftime),w)
        frame[i]=fb+'_LDP'+ntostr(tl[w].ldp)+'.*'+ntostr(tl[w].frame)+'*'
     endif 
     readframe=frame[i]
     tab=mrdfits(readframe,0,hd0,/silent)
     frameno[i]=sxpar(hd0,'CCD_FRAM')
     ccd_t[i]=sxpar(hd0,'T_CCD')
     median_bias[i]=median(tab2[0:199,*])
;     print,tab2[0:199,*]
     sigma[i]=stdev(tab2[0:199,*])
;      if ((ccd_t[i] lt -52) and (bias_med[i] gt 200)) then print,readframe 
  endfor
  uld_ind=where(uld lt 4094)


  outprint=[transpose(ccd_t(sort(ccd_t))),transpose(median_bias(sort(ccd_t))),transpose(sigma(sort(ccd_t))),transpose(sigma(sort(ULD)))]
  openw,nomelogico,'ccdtemp_biasrow_median.dat',/get_lun
  printf,nomelogico,'CCD temperature and average bias and sigma (from 20 header pixels) for LrPD frames'
  printf,nomelogico,'CCD temp       Bias Row Median value       Sigma       ULD'
  printf,nomelogico,outprint
  close,nomelogico
  free_lun,nomelogico

;ave=fltarr(13)
;ccd_ave_plot=intarr(13)
;avesigma=fltarr(13)
;aveindex=0
;for temp_incr=-57,-45 do begin
;    index=where(ccd_t gt temp_incr-0.5 and ccd_t lt temp_incr+0.5)
;    aveindex=(temp_incr+57)
;    ave[aveindex]=mean(bias_med[index])
;    ccd_ave_plot[aveindex]=temp_incr
;    avesigma[aveindex]=mean(sigma[index])
;endfor

;  set_plot,'ps'
  psname=infile2+'_row_vs_temp.ps'
  begplot,name=psname,/land
  multiplot,[1,5],/init
  !p.charsize=0.75
  !p.thick=1
  !x.thick=1
  !y.thick=1
;  device,filename=psname,xs=20,ys=15,/col
  tito='Bias Row Median value vs CCD temp - '+infile2
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
