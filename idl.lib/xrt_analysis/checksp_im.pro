pro checksp_im,quicklook=quicklook
;determines the mean and sigma of any X-ray events with in the limits
; Note this is not a fit to the spectrum

outfile='im_spec_result_summary.xls'
spawn,'rm '+outfile+''
SPAWN,'ls *im_image*fits*  > tttim '
spawn,'wc tttim   >  wwcc2'
readcol,'wwcc2',nnn, format='(a)'
if nnn[0] gt 0  then begin
   readcol,'tttim',frame,format='(a)'
   thr=0		;noise threshold
   low_lim=200		;low limit Ka
   up_lim=213		;upper limit Ka
   eline=5895.0		; Energy of Mn Ka
   openw,1,outfile,/append
;  printf,1,'filename','thrsh','n_mean','n_sig','Ka mean', 'Ka sigma', '# ka Evnts','CCDtemp',format='(a,45x,a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a)'
   printf,1,'filename','#_ka_evnts','thrsh','Ka_mean', 'Ka_sigma','Cal(eV/dn)','FWHM(eV)', '#_ka_Evnts','FWHM_err','CCDtemp',format='(10(a,","))'
   close,1
    for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	print,'======='
	print, frame[ik]
;	print,'mean of all pixels =', mean(ima)
;	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=3000,bin=1)

;	iu=where(ima lt thr)
;	noise_events=n_elements(iu)
	fima=ima
	h1=histogram(fima,min=low_lim,max=up_lim,bin=1)
	temp=sxpar(hdr,'T_CCD')
;	print,'mean of pixels below threshold',mean(fima)
;	print,'sigma of pixels below threshold',stddev(fima)
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
    endif 
	ip=where(ima gt low_lim and ima lt up_lim,nip)
        if nip gt 1 then begin
	   xray_events=n_elements(ip)
	   fimb=ima[ip]
	   cal=eline/mean(fimb)
	   FWHM=cal*stddev(fimb)*2.35
	   FWHM_err=FWHM/((xray_events)^0.5)
	   print,'mean of events between thresholds',mean(fimb)
	   print,'sigma of events between thresholds',stddev(fimb)
	   print, 'calibration is',cal,'eV/dn'
	   print, 'FWHM is',FWHM,'eV'
	   openw,1,outfile,/append      

;   printf,1,frame[ik],thr,mean(fima),stddev(fima),mean(fimb), stdev(fimb), xray_events,temp,format='(a,2x,i,2x,f6.2,2x,f6.2,2x,f7.2,2x,f6.2,i,3x,f6.2)'
    printf,1,frame[ik],xray_events,thr,mean(fimb), stdev(fimb),cal,FWHM, xray_events,FWHM_err,temp,format='(a,",",2(i,","),4(f10.2,","),i,",",2(f10.2,","))'

    close,1
    
    if not keyword_set(quicklook) then begin
       plot,h0,yr=[1e-2,1e4],/ylog,xr=[130,300]
       oplot,h1,color=177,thick=3
    endif 

   endif else print,'no counts between',low_lim,up_lim
   endfor

endif else begin
	print, "no frames of this type"
endelse


end
