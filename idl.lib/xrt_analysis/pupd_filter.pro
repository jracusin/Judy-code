pro pupd_filter,quicklook=quicklook
;pupd_filter_n
;determines the mean bias level and sigma of a single PuPD frame
;determines the mean and sigma of any X-ray events with in the limits


outfile='pupd_spec_result_summary.xls'
spawn,'rm '+outfile+''
SPAWN,'ls *frame*.pupd.fits *frame*.pupd.fits.gz   > tttpupd '
spawn,'wc tttpupd   >  wwcc2'
readcol,'wwcc2',nnn, format='(a)'
if nnn[0] gt 0  then begin
   readcol,'tttpupd',frame,format='(a)'

   thr=183		;noise threshold

   low_lim=2300		;low limit Ka
   up_lim=2650		;upper limit Ka

   sbin=16

   low_lim_n=0          ; low limit for noise fit
   up_lim_n=200         ; higher limit for noise fit

   sbin_n=4

   energy=5895

   openw,1,outfile,/append
;  printf,1,'filename','thrsh','n_mean','n_sig','Ka mean', 'Ka sigma', '# ka Evnts','CCDtemp',format='(a,45x,a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a)'
   printf,1,'filename','thrsh','n_mean','n_sig','Ka_mean', 'Ka_sigma', 'Ka FWHM','#_ka_Evnts','CCDtemp',format='(9(a,","))'
   close,1
    for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	print,'======='
	print, frame[ik]
	print,'mean of all pixels =', mean(ima)
	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=3000,bin=1)

	iu=where(ima lt thr)
	noise_events=n_elements(iu)
	fima=ima[iu]
	h1=histogram(fima,min=low_lim_n,max=up_lim_n,bin=1)
    temp=sxpar(hdr,'T_CCD')
    
    hh_n=histogram(fima,min=low_lim_n,max=up_lim_n,bin=sbin_n)   
    hx_n=low_lim_n+sbin_n/2.+findgen((up_lim_n-low_lim_n)/sbin_n+1)*sbin_n
    hf_n=gaussfit(hx_n,hh_n,res_n,nterms=3)
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
       plot,hx_n,hh_n,psym=10,xr=[low_lim_n,up_lim_n],/xst
       oplot,hx_n,hf_n,li=5,color=177,thick=3   
    endif 
;	str='' & read,str

	print,'mean of pixels below threshold',mean(fima), res_n[1]
	print,'sigma of pixels below threshold',stddev(fima), res_n[2]

	ip=where(ima gt low_lim and ima lt up_lim)
	xray_events=n_elements(ip)
	fimb=ima[ip]
	hh=histogram(fimb,min=low_lim,max=up_lim,bin=sbin)   
    hx=low_lim+sbin/2.+findgen((up_lim-low_lim)/sbin+1)*sbin
    hf=gaussfit(hx,hh,res,nterms=3)
    if not keyword_set(quicklook) then begin 
       plot,hx,hh,psym=10,xr=[low_lim,up_lim],/xst
       oplot,hx,hf,li=5,color=177,thick=3
    endif 
;	str='' & read,str
	print,'mean of events between thresholds',mean(fimb), res[1]
	print,'sigma of events between thresholds',stddev(fimb), res[2]
;	print,'FWHM of events between thresholds',2.35*stddev(fimb)
	cal=energy/(res[1]-res_n[1])
	openw,1,outfile,/append      

;   printf,1,frame[ik],thr,mean(fima),stddev(fima),mean(fimb), stdev(fimb), xray_events,temp,format='(a,2x,i,2x,f6.2,2x,f6.2,2x,f7.2,2x,f6.2,i,3x,f6.2)'
    printf,1,frame[ik],thr,res_n[1],res_n[2],res[1], res[2], 2.35*res[2]*cal,xray_events,temp,format='(a,",",i,",",4(f10.2,","),i,",",2(f10.2,","))'

    close,1

    if not keyword_set(quicklook) then plot,h0,yr=[1e-2,1e4],/ylog,xr=[0,4000]
   endfor

endif else begin
	print, "no frames of this type"
endelse


end

