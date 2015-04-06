pro checkb
;determines the mean and sigma from the bias frame header (pass1 output)
;changing to add min/max

  outfile='mean_bias_summary.xls'
  spawn,'rm '+outfile+''
  openw,lun,outfile,/get_lun    ;/append
  printf,lun,'frame name','mean','std','min','max','temp',format='(6(a,","))'
;close,1

;    SPAWN,'ls *bias_map*',frame
  frame=findfile('*bias_map*')
    ppp = size(frame)
    if frame[0] ne '' then begin 
;    readcol,'tttb',frame,format='(a)'
    min_dat=fltarr(n_elements(frame))
    max_dat=fltarr(n_elements(frame))
    mean_dat=fltarr(n_elements(frame))
    std_dat=fltarr(n_elements(frame))
    for ik=0,n_elements(frame)-1 do begin
        ima=readfits(frame[ik],hdr)
;        mean=sxpar(hdr,'MEAN')
;	std=sxpar(hdr,'STDEV')

	sub=ima[300-sxpar(hdr,'WIDTH')/2+1:300+sxpar(hdr,'WIDTH')/2-2, $
                      300-sxpar(hdr,'HEIGHT')/2+1:300+sxpar(hdr,'HEIGHT')/2-2]

	min_dat(ik)=min(sub)
	max_dat(ik)=max(sub)
        mean_dat(ik)=mean(sub)
        std_dat(ik)=stddev(sub)
	iu=where(sub eq 0,niu) 
	if niu gt 0 then print,'ATTENTION  ',niu,'pixels =0 !!!!!!!!'

        temp=sxpar(hdr,'T_CCD')
;        openw,1,outfile,/append
;       printf,1,frame[ik],mean,std,temp,format='(a,3x,f6.2,f6.2,2x,f6.2)'
        printf,lun,frame[ik],mean_dat(ik),std_dat(ik),min_dat(ik),max_dat(ik),temp,format='(a,",",5(f10.2,","))'
;        close,1

    endfor
;    openw,1,outfile,/append
    printf,lun,''
    printf,lun,'Avg Mean, Avg StdDev, Avg Min, Avg Max'
    printf,lun,mean(mean_dat),',',mean(std_dat),',',mean(min_dat),',',mean(max_dat)
    close,lun
    free_lun,lun
endif else begin
	print, 'no files of this type'
endelse

fine:
end





