pro raw_filter,quicklook=quicklook
;determines the mean bias level and sigma of a single raw frame
;determines the mean and sigma of any X-ray events with in the limits


eline=5895.0                  ; Energy of the Fe Line used for Calibration
sbin=1.
outfile='raw_spec_result_summary.xls'
spawn,'rm '+outfile+''
frame=findfile('*raw_image*.fits*')
;SPAWN,'ls *raw_image*.fits*  > tttraw '
;spawn,'wc tttraw   >  wwcc1'
;readcol,'wwcc1',ppp,format='(i)'
if frame[0] ne '' then begin
;   readcol,'tttraw',frame,format='(a)'

   openw,1,outfile,/append
;  printf,1,'filename','thrsh','n_mean','n_sig','Ka mean', 'Ka sigma', '# ka Evnts','CCDtemp',format='(a,45x,a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a)'
   printf,1,'filename','thrsh','n_mean','n_sig','Ka mean', 'Ka sigma', 'cal(eV/dn)','FWHM', '# ka Evnts','FWHM_err','CCDtemp',format='(11(a,","))'
   close,1

   for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	amp=sxpar(hdr,'AMP')
	wavefm=sxpar(hdr,'WAVEFORM')
	print,"wavefrm is ",wavefm
	if (amp eq 1) then begin
;		low_lim=2100
;		up_lim=2300
		low_lim=2350
		up_lim=2550
		noise_low=0
		noise_up=200
;		thr=110
		thr=150
	endif else begin 
		if (amp eq 2) then begin
			if (wavefm eq 31) then begin
				low_lim=200
				up_lim=213
				thr=140
			endif else begin
				if (wavefm eq 140) then begin
					low_lim=3150
					up_lim=3350
					noise_low=825
					noise_up=1025
					thr=975
				endif else begin
					print,'ERROR: AMP2 WAVEFORM not equal to 31 (lowgain) or 140 (highgain)'
					print,'FILE: '+frame[ik]
				endelse
			endelse
		endif else begin
			print,'ERROR: AMP field in header not equal to 1 or 2'
			print,'FILE: '+frame[ik]
			stop
		endelse
	endelse	
	print,'======='
	print, frame[ik]
	print,'mean of all pixels =', mean(ima)
	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=3500,bin=1)

	iu=where(ima lt thr, noise_events)
	if noise_events gt 0 then fima=ima[iu] else fima=ima*0.


	h1=histogram(fima,min=0,max=3500,bin=1)
	temp=sxpar(hdr,'T_CCD')
	print,'mean of pixels below threshold',mean(fima)
    print,'sigma of pixels below threshold',stddev(fima)
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
    endif 
       
	ip=where(ima gt low_lim and ima lt up_lim)
	xray_events=n_elements(ip)
	fimb=ima[ip]
	print,'mean of events between thresholds',mean(fimb)
	print,'sigma of events between thresholds',stddev(fimb)
	noise_ind=where(ima lt thr)
	noise_evts=n_elements(noise_ind)
	fnoise=ima(noise_ind)

	hh=histogram(fimb,min=low_lim,max=up_lim,bin=sbin)
	hx=low_lim+sbin/2.+findgen((up_lim-low_lim)/sbin+1)*sbin
	hf=gaussfit(hx,hh,res,nterms=3)
	hnoise=histogram(fnoise,min=noise_low,max=noise_up,bin=sbin)
	hxnoise=sbin/2.+(findgen((noise_up-noise_low)/sbin+1)+noise_low)*sbin
	hfnoise=gaussfit(hxnoise,hnoise,resnoise,nterms=3)

;	stop
	thr=resnoise[1]+40
	print,'resnoise is ',resnoise[1]
	iu=where(ima lt thr, noise_events)
	if noise_events gt 0 then fima=ima[iu] else fima=ima*0.

	cal=eline/(res[1]-resnoise[1])
	FWHM=res[2]*2.354*cal
	FWHM_err=FWHM/((xray_events)^0.5)
;	print,'new numbers are ',cal,' ',FWHM,' ',FWHM_err,' ',res

        openw,1,outfile,/append      

;       printf,1,frame[ik],thr,mean(fima),stddev(fima),mean(fimb), stdev(fimb), xray_events,temp,format='(a,2x,i,2x,f6.2,2x,f6.2,2x,f7.2,2x,f6.2,i,3x,f6.2)'
	printf,1,frame[ik],thr,mean(fima),stddev(fima),mean(fimb), stdev(fimb), cal, FWHM, xray_events,FWHM_err,temp,format='(a,",",i,",",6(f10.2,","),i,",",2(f10.2,","))'

        close,1

   if not keyword_set(quicklook) then plot,h0,yr=[1e-2,1e4],/ylog,xr=[0,4000]
   oplot,hx,hf,color=177
   oplot,hxnoise,hfnoise,color=177
   endfor
endif else begin
	print, "no frames of this type"

endelse
end
