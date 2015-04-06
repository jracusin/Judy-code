pro checksp_wt,quicklook=quicklook
;Calculates the mean and sigma of all the WT frames in a snapshot


  sbin=1.		
  outfile='wt_spec_result_summary.xls'
  spawn,'rm '+outfile+''
  cpha=2.56                     ;calibration estimate
  lo_lim=2100.                  ;Ka low limit
  up_lim=2400.                  ; Ka upper limit
  Eline=5895.0                  ; Energy of the Fe Line used for Calibration



           

spectro_wt:
  str_wt='*wt.cat'
;	spawn,'/bin/ls  '+str_wt+' > tttwt2'
; 	spawn,'wc tttwt2   >  wwcc2'
  wtcat=findfile(str_wt)
;	readcol,'wwcc2',nnn, format='(a)'
  nnn=n_elements(wtcat)
  if wtcat[0] ne ''  then begin
       openw,lun,outfile,/get_lun ;/append

;	printf,1,'File name','#frames','Tot_evnt','mean(dn)','sigma(dn)','cal(eV/dn)','FWHM','Ka Events', 'FWHM_err', 'min_evnts', 'Max_evnts',format='(a,33x,a,2x,a,4x,a,2x,a,3x,a,a,8x,a,6x,a,2x,a,2x,a,2x,a)'

       printf,lun,'File name','#frames','Tot evnt','mean(dn)','sigma(dn)','cal(eV/dn)','FWHM','Ka Events', 'FWHM_err', 'min_evnts', 'Max_evnts',format='(11(a,","))'

;	close,1


;       readcol,'tttwt2',wtcat,format='(a)'
       n_wt_ldp=n_elements(wtcat)
       For il=0,n_wt_ldp-1 do begin	
          str_wt=wtcat[il]
;          spawn,'more ' +str_wt+ '> tttwt'
;          spawn,'wc tttwt   >  wwcc2'
;          readcol,'wwcc2',nnn,format='(i)'
;          readcol,'tttwt',frame,format='(a)'
          readcol,str_wt,frame,format='(a)'
          pha=[0]
          events=[0]
          wframe=strpos(frame,'.fits')
          wframe=where(wframe gt 0)
          frame=frame[wframe]
          for ik=0,n_elements(frame)-1 do begin
             teve=mrdfits(frame[ik],1,hdr,/silent)
             pha=[pha,teve.pha]
             events=[events,(n_elements(teve))]
          endfor
          events=events[1:*]
          min_evt=min(events)
          max_evt=max(events)
          pha=pha[1:*]
          tot_evnts=n_elements(pha)
          xray_test=where(pha gt lo_lim and pha lt up_lim)
          IF (xray_test[0] EQ -1) then begin
;             openw,1,outfile,/append	
                                ;printf,1,str_wt,nnn[0],tot_evnts,'N/A','N/A','N/A','N/A',0,'N/A',min_evt, max_evt, format='(a,i,i,10x,a,6x,a,6x,a,6x,a,7x,i,11x,a,2x,i,i)'
             printf,lun,str_wt,nnn,tot_evnts,'N/A','N/A','N/A','N/A',0,'N/A',min_evt, max_evt, format='(a,",",2(i,","),4(a,","),i,",",a,",",2(i,","))'

;       		 	close,1
          ENDIF ELSE BEGIN
             hh=histogram(pha,min=lo_lim,max=up_lim,bin=sbin)   
             hx=lo_lim+sbin/2.+findgen((up_lim-lo_lim)/sbin+1)*sbin
             hf=gaussfit(hx,hh,res,nterms=3)
             if not keyword_set(quicklook) then begin
                plot,hx,hh,psym=10,xr=[lo_lim,up_lim],/xst
                oplot,hx,hf,li=5,color=177,thick=3   
             endif 
             
             xray_num=n_elements(pha[where(pha gt lo_lim and pha lt up_lim)])
             sig=2.354*res[2]*cpha
             ave=(res[1])*cpha
             cal=eline/(res[1])			
             FWHM=res[2]*2.354*cal
             FWHM_err=FWHM/((xray_num)^0.5)
             
;             openw,1,outfile,/append	
                                ;printf,1,str_wt,nnn[0],tot_evnts,res[1],res[2],cal,FWHM,xray_num,FWHM_err,min_evt, max_evt, format='(a,i,i,7x,f7.2,2x,f7.2,2x,f7.2,2x,f7.2,2x,i,7x,f7.2,2x,i,i)'

             printf,lun,str_wt,nnn,tot_evnts,res[1],res[2],cal,FWHM,xray_num,FWHM_err,min_evt, max_evt, format='(a,",",2(i,","),3(f10.2,","),f10.4,",",i,",",f10.4,",",2(i,","))'

;        		close,1
          ENDELSE
       endfor
       
    endif else begin
       print, "no frames of this type"
    endelse
    close,lun
    free_lun,lun
fine:
;spawn,'rm wwcc2 tttwt tttwt2'
end






