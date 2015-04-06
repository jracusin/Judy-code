pro tam2win_filter,quicklook=quicklook
;determines the mean bias level and sigma of each window in the tam 2win data
;writes to a file the mean bias level and sigma of pixels below a set threshold
;writes to a file the number of pixels occuring above the set threshold

outfile='tam2win_spec_result_summary.xls'
spawn,'rm '+outfile+''
frame=findfile('*tam_two_win*win1*.fits*')
;SPAWN,'ls *tam_two_win*win1*.fits*',frame
;ppp = size(frame)
if frame[0] ne '' then begin
   thr=115		;noise threshold

   openw,1,outfile,/append
   printf,1,'filename','thrsh','n_mean','n_sig','above_thrsh','LED_NUM',format='(8(a,","))'
   close,1

   for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	print,'======='
	print, frame[ik]
	print,'mean of all pixels =', mean(ima)
	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=1000,bin=1)

	iu=where(ima lt thr, noise_events)
	above_thrsh=2500.-noise_events
	if noise_events gt 0 then fima=ima[iu] else fima=ima*0.

	h1=histogram(fima,min=0,max=1000,bin=1)
	led_num=sxpar(hdr,'LED_NUM')
	print,'mean of pixels below threshold',mean(fima)
    print,'sigma of pixels below threshold',stddev(fima)
	tam_x1=sxpar(hdr,"TAM_X1")
	tam_y1=sxpar(hdr,"TAM_Y1")
	x1win=sxpar(hdr,"X1WIN")
	y1win=sxpar(hdr,"Y1WIN")
	print,'centroid is x='+strmid(string(tam_x1),2)+' y='+strtrim(string(tam_y1),2)
	radii=fltarr(50,50)
	for a=0,49 do begin
		for b=0,49 do begin
			radii(a,b)=sqrt((a-tam_x1+x1win)^2+(b-tam_y1+y1win)^2)
			if (radii(a,b) gt 10 AND ima(a,b) gt thr) then begin
				print,"WARNING: pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+") detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at x="+strtrim(string(a+x1win),2)+" y="+strtrim(string(b+y1win),2)
				window,1
				zm_arr=fltarr(250,250)
				for n=0,49 do begin
					for m=0,49 do begin
						for p=0,4 do begin
							for q=0,4 do begin
								zm_arr(n*5+p,m*5+q)=ima(n,m)-thr
								if (ima(n,m) lt thr) then zm_arr(n*5+p,m*5+q)=0.1
							endfor
						endfor
					endfor
				endfor
				tvscl,(zm_arr)^(1./4.)

				xyouts,25,200,'TAM2WIN XRT SPOT',/data
				xyouts,-10,150,"pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+")",/data
				xyouts,-10,100,"detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at",/data
				xyouts,-10,50,"x="+strtrim(string(a+x1win),2)+" y="+strtrim(string(b+y1win),2),/data
				set_plot,'ps'
				device,filename='bright_pix_win1.ps'
				tvscl,(zm_arr)^(1./4.)
				xyouts,.1,20000,'TAM2WIN XRT SPOT',/device
				xyouts,.1,19500,"pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+")",/device
				xyouts,.1,19000,"detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at",/device
				xyouts,.1,18500,"x="+strtrim(string(a+x1win),2)+" y="+strtrim(string(b+y1win),2),/device
				device,/close
				set_plot,'x'
				window,0
			endif
		endfor
	endfor
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
    endif 
       
        openw,1,outfile,/append      
	printf,1,frame[ik],thr,mean(fima),stddev(fima), above_thrsh, led_num,format='(a,",",i,",",2(f10.2,","),2(i,","))'
        close,1

   if not keyword_set(quicklook) then plot,h0,yr=[1e-2,1e4],/ylog;,xr=[0,1000]
   endfor
	diffs1=fltarr(n_elements(frame),2)
        for n=0,n_elements(frame)-1 do begin
                dat=mrdfits(frame(n),0)
                fit=gauss2dfit(dat,pars)
                diffs1(n,*)=[x1win+pars(4)-tam_x1,y1win+pars(5)-tam_y1]
        endfor
endif else begin
	print, "no tam2win (window1) frames"
endelse

;SPAWN,'ls *tam_two_win*win2*.fits*', frame
;ppp = size(frame)
frame=findfile('*tam_two_win*win2*.fits*')
if n_elements(frame) GT 0 then begin
;   readcol,'ttttam2win2',frame,format='(a)'
   thr=115		;noise threshold

;   openw,1,outfile,/append
;   printf,1,'filename','thrsh','n_mean','n_sig','above_thrsh','LED_NUM',format='(8(a,","))'
;   close,1

   for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	print,'======='
    print, frame[ik]
    
	print,'mean of all pixels =', mean(ima)
	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=1000,bin=1)

	iu=where(ima lt thr, noise_events)
	above_thrsh=2500.-noise_events
	if noise_events gt 0 then fima=ima[iu] else fima=ima*0.

	h1=histogram(fima,min=0,max=1000,bin=1)
	led_num=sxpar(hdr,'LED_NUM')
	print,'mean of pixels below threshold',mean(fima)
    print,'sigma of pixels below threshold',stddev(fima)
	tam_x2=sxpar(hdr,"TAM_X2")
	tam_y2=sxpar(hdr,"TAM_Y2")
	x2win=sxpar(hdr,"X2WIN")
	y2win=sxpar(hdr,"Y2WIN")
	print,'centroid is x='+strtrim(string(tam_x2),2)+' y='+strtrim(string(tam_y2),2)
	for a=0,49 do begin
		for b=0,49 do begin
			radii(a,b)=sqrt((a-tam_x2+x2win)^2+(b-tam_y2+y2win)^2)
			if (radii(a,b) gt 10 AND ima(a,b) gt thr) then begin
				print,"WARNING: pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+") detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at x="+strtrim(string(a+x2win),2)+" y="+strtrim(string(b+y2win),2)
				window,1
				zm_arr=fltarr(250,250)
				for n=0,49 do begin
					for m=0,49 do begin
						for p=0,4 do begin
							for q=0,4 do begin
								zm_arr(n*5+p,m*5+q)=ima(n,m)-thr
								if (ima(n,m) lt thr) then zm_arr(n*5+p,m*5+q)=0.1
							endfor
						endfor
					endfor
				endfor
				tvscl,(zm_arr)^(1./4.)
				xyouts,25,200,'TAM2WIN RETURN SPOT',/data
				xyouts,-10,150,"pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+")",/data
				xyouts,-10,100,"detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at",/data
				xyouts,-10,50,"x="+strtrim(string(a+x2win),2)+" y="+strtrim(string(b+y2win),2),/data
				set_plot,'ps'
				device,filename='bright_pix_win2.ps'
				tvscl,(zm_arr)^(1./4.)
				xyouts,.1,20000,'TAM2WIN RETURN SPOT',/device
				xyouts,.1,19500,"pixel above threshold (pix="+strtrim(string(ima(a,b)),2)+")",/device
				xyouts,.1,19000,"detected "+strtrim(string(radii(a,b)),2)+" pixels from centroid at",/device
				xyouts,.1,18500,"x="+strtrim(string(a+x2win),2)+" y="+strtrim(string(b+y2win),2),/device
				device,/close
				set_plot,'x'
				window,0
			endif
		endfor
	endfor
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
    endif 
       
        openw,1,outfile,/append      
	printf,1,frame[ik],thr,mean(fima),stddev(fima), above_thrsh, led_num,format='(a,",",i,",",2(f10.2,","),2(i,","))'
        close,1

   if not keyword_set(quicklook) then plot,h0,yr=[1e-2,1e4],/ylog;,xr=[0,1000]
   endfor
	diffs2=fltarr(n_elements(frame),2)
        for n=0,n_elements(frame)-1 do begin
                dat=mrdfits(frame(n),0)
                fit=gauss2dfit(dat,pars)
                diffs2(n,*)=[x2win+pars(4)-tam_x2,y2win+pars(5)-tam_y2]
        endfor
        set_plot,'ps'
        !p.multi=[0,2,2]
        device,filename=strmid(frame[0],0,16)+'_tam2windiffs.ps',/landscape
        !p.title="XRT TAM centroid differences (flight-ground)"
        plot,diffs1(*,0),diffs1(*,1),psym=1,title="TAM Window 1"
        plot,diffs2(*,0),diffs2(*,1),psym=1,title="TAM Window 2"
        device,/close
endif else begin
	print, "no tam2win (window2) frames"
endelse
end
