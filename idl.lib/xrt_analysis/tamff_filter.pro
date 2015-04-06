pro tamff_filter,quicklook=quicklook
;determines the mean bias level and sigma of a single tam full frame
;writes to a file the mean bias level and sigma of pixels below a set threshold
;writes to a file the number of pixels occuring above the set threshold

outfile='tamff_spec_result_summary.xls'
spawn,'rm '+outfile+''

;spawn,'/bin/ls *tam_full_frame*.fits*',frame
;ppp = size(frame)

frame=findfile('*tam_full_frame*.fits*')
if frame[0] ne '' then begin
   thr=100		;noise threshold
   openw,lun,outfile,/get_lun;,/append
   printf,lun,'filename','thrsh','n_mean','n_sig','above_thrsh','LED_NUM','XC_1','YC_1','DN1','XC_2','YC_2','DN2',format='(13(a,","))'
   close,1

   for ik=0,n_elements(frame)-1 do begin
	ima=readfits(frame[ik],hdr)
	print,'======='
	print, frame[ik]
	print,'mean of all pixels =', mean(ima)
	print,'sigma of all pixels =',stddev(ima)
	h0=histogram(ima,min=0,max=1000,bin=1)

	iu=where(ima lt thr, noise_events)
	above_thrsh=(512.*512.)-noise_events
	if noise_events gt 0 then fima=ima[iu] else fima=ima*0.

	h1=histogram(fima,min=0,max=1000,bin=1)
	led_num=sxpar(hdr,'LED_NUM')
	print,'mean of pixels below threshold',mean(fima)
    print,'sigma of pixels below threshold',stddev(fima)
    if not keyword_set(quicklook) then begin 
       plot,h0
       oplot,h1,color=177,thick=3
    endif 
;SECTION TO FIND AND PRINT TAM SPOT CENTROIDS
    	dat_orig=ima

        if (led_num ne 0) then begin
            win1_x=where(ima eq max(ima)) mod 512
            win1_y=floor(where(ima eq max(ima))/512)
            win1_x=win1_x[0]
            win1_y=win1_y[0]
            max_dn1=max(ima)
            
            fit1=gauss2dfit(ima[win1_x-15:win1_x+15,win1_y-15:win1_y+15],pars1)
            ima(win1_x-15:win1_x+15,win1_y-15:win1_y+15)=0.
            
            win2_x=where(ima eq max(ima)) mod 512
            win2_y=floor(where(ima eq max(ima))/512)
            win2_x=win2_x[0]
            win2_y=win2_y[0]
            max_dn2=max(ima)
            
            fit2=gauss2dfit(ima[win2_x-15:win2_x+15,win2_y-15:win2_y+15],pars2)
            ima(win2_x-15:win2_x+15,win2_y-15:win2_y+15)=0.
            x1=pars1[4]+win1_x-15
            y1=pars1[5]+win1_y-15
            x2=pars2[4]+win2_x-15
            y2=pars2[5]+win2_y-15
            
            print,pars1[2],pars1[3],pars1[4]+win1_x-15,pars1[5]+win1_y-15,pars1[6]
            print,pars2[2],pars2[3],pars2[4]+win2_x-15,pars2[5]+win2_y-15,pars2[6]
            
            
            if not keyword_set(quicklook) then begin
                !p.multi=[0,2,2]
                shade_surf,dat_orig
                shade_surf,ima
            end
            ima(win1_x-15:win1_x+15,win1_y-15:win1_y+15)=ima(win1_x-15:win1_x+15,win1_y-15:win1_y+15)+fit1
            ima(win2_x-15:win2_x+15,win2_y-15:win2_y+15)=ima(win2_x-15:win2_x+15,win2_y-15:win2_y+15)+fit2
            if not keyword_set(quicklook) then shade_surf,ima
        endif else begin
            max_dn1=median(ima(0,*))		
            max_dn2=median(ima(511,*))
            x1=0
            y1=255
            x2=511
            y2=255
        endelse
;END TAM SPOT SECTION
       
;        openw,1,outfile,/append      
        printf,lun,frame[ik],thr,mean(fima),stddev(fima), above_thrsh, led_num,x1,y1,max_dn1,x2,y2,max_dn2,format='(a,",",i,",",2(f10.2,","),i,",",i,",",2(f10.2,","),i,",",2(f10.2,","),i,",")'
    close,lun
    free_lun,lun

   if not keyword_set(quicklook) then plot,h0,yr=[1e-2,1e4],/ylog;,xr=[0,1000]
   endfor
endif else begin
	print, "no frames of this type"
endelse
end
