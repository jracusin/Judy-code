pro checksp_lrpd,quicklook=quicklook
;Calculates the mean and sigma of all the LrPD frames in a snapshot
;Currently uses the first PuPD frame in the entire data set to calculate a bias
;this bias is subtracted off each frame


  sbin=4.		
  outfile='lrpd_spec_result_summary.xls'
  outfile2='lrpd_spec_result_summary_bias.xls'
  spawn,'rm '+outfile+''
  spawn,'rm '+outfile2+''
  cpha=2.55                     ;calibration estimate
  lo_lims=2300.                  ;Ka low limit
  up_lims=2650.                  ; Ka upper limit
  thr=183                       ;ULD for PuPD bias calculation
  Eline=5895.0                  ; Energy of the Fe Line used for Calibration
  off=122.0        ; expected off-set (bias) for LrPD frames. Use for LrPD data
                                ; until there is a PuPD frame to recalculate




;printf,1,'File name','#frames','Tot evnt','bias','mean(dn)','sigma(dn)','cal(eV/dn)','FWHM','Ka Events', 'FWHM_err', 'min_evnts', 'Max_evnts',format='(a,33x,a,2x,a,4x,a,2x,a,3x,a,a,8x,a,6x,a,2x,a,2x,a,2x,a)'

  str_lrpd='*lrpd.cat'
;  spawn,'/bin/ls  '+str_lrpd+'  > tttlrpd'
;  spawn,'wc tttlrpd   >  wwcc1'
;  readcol,'wwcc1',ppp
  file=findfile(str_lrpd)
  ppp=n_elements(file)
  if file[0] ne '' then begin   
     openw,lun,outfile,/get_lun;/append
     printf,lun,'File name','#frames','Tot evnt','bias','mean(dn)','sigma(dn)','cal(eV/dn)','FWHM','Ka Events', 'FWHM_err', 'min_evnts', 'Max_evnts',format='(12(a,","))'
;     close,1

     openw,lun2,outfile2,/get_lun;/append
     printf,lun2,'File name','#frames','Tot evnt','bias','mean(dn)','sigma(dn)','cal(eV/dn)','FWHM','Ka Events', 'FWHM_err', 'min_evnts', 'Max_evnts','bias','evtthresh',format='(14(a,","))'
;     close,1

     spettro_lr:
     str_lrpd='*lrpd.cat'
;     spawn,'/bin/ls  '+str_lrpd+' > tttlrpd2'
;     readcol,'tttlrpd2',lrpdcat,format='(a)'
     lrpdcat=findfile(str_lrpd)
     n_lrpd_ldp=n_elements(lrpdcat)

     For il=0,n_lrpd_ldp-1 do begin	
        str_lr=lrpdcat[il]
;        spawn,'more ' +str_lr+ '> tttlrpd'
;        spawn,'wc tttlrpd   >  wwcc2'
;        readcol,'wwcc2',nnn,format='(i)'
;        readcol,'tttlrpd',frame,format='(a)'
        readcol,str_lr,frame,format='(a)'
        nnn=n_elements(frame)
        pha=[0]
        events=[0]
        hbias=0
        wframe=strpos(frame,'.fits')
        wframe=where(wframe gt 0)
        frame=frame[wframe]
        for ik=0,n_elements(frame)-1 do begin
           teve=readfits(frame[ik],hdr,/exten,/silent)
           tpha=tbget(hdr,teve,'PHA')
           pha=[pha,tpha]
           events=[events,(n_elements(tpha))]
           ttt=mrdfits(frame[ik],0,hdt,/silent)
           htre=sxpar(hdt,'EVTHRESH')
           tbias=sxpar(hdt,'BIAS') & hbias=tbias+hbias
           bias_lvl = sxpar(hdt,'BIAS_LVL')
;           print,'BIAS LEVEL = ',bias_lvl
;           print,tbias
        endfor
        
        hbias=hbias/float(n_elements(frame))
        off=hbias
        events=events[1:*]
        min_evt=min(events)
        max_evt=max(events)
        pha=pha[1:*]
        lo_lim=lo_lims-bias_lvl
        up_lim=up_lims-bias_lvl
        hh=histogram(pha,min=lo_lim,max=up_lim,bin=sbin)   
        hx=lo_lim+sbin/2.+findgen((up_lim-lo_lim)/sbin+1)*sbin
        
        hf=gaussfit(hx,hh,res,nterms=3)
        if not keyword_set(quicklook) then begin
           plot,hx,hh,psym=10,xr=[lo_lim,up_lim],/xst
           oplot,hx,hf,li=5,color=177,thick=3   
        endif 

        xray_num=n_elements(pha[where(pha gt lo_lim and pha lt up_lim)])
        tot_evnts=n_elements(pha)
;        sig=2.354*res[2]*cpha
;        ave=(res[1]-off)*cpha

        lrpd_pos=strpos(frame[0],'.frame')	
        pu_pattern=strmid(frame(0),0,lrpd_pos) + '.frame*.pupd.fits*'
        pupdframe=findfile(pu_pattern,count=num_pu_files)
        if (num_pu_files GT 0 and bias_lvl eq 0) then begin
           bias_da_pupd:
           
;        		readcol,'tttpupd',pupdframe,format='(a)'
           print,'********',pupdframe[0]
           ima=readfits(pupdframe[0],hdr,/silent)       
;           bias_lvl = tbget(hdr,image,'BIAS_LVL')
;           print,'BIAS LEVEL = ',bias_lvl
           iu=where(ima lt thr)
           fima=ima[iu]
           off=mean(fima)
           std=stddev(fima)
        ENDIF

        if (bias_lvl gt 0) then cal=eline/(res[1]) else cal=eline/(res[1]-hbias)

        FWHM=res[2]*2.354*cal
        FWHM_err=FWHM/((xray_num)^0.5)

        nomeps=str_lr+'.ps'
        set_plot,'ps'
        device,filename=nomeps  	

        plot,hx,hh,psym=10,xr=[lo_lim,up_lim],/xst,title=str_lr
        oplot,hx,hf,li=5,thick=5   
        xyouts,lo_lim*1.005,max(hh)*0.9,'mean:'+strtrim(string(res[1]),2)
        xyouts,lo_lim*1.005,max(hh)*0.9/1.2,'stdev:'+strtrim(string(res[2]),2)	

        device,/close
        set_plot,'x'

;        openw,1,outfile,/append	
                                ;printf,1,str_lr,nnn[0],tot_evnts,off,res[1],res[2],cal,FWHM,xray_num,FWHM_err,min_evt, max_evt, format='(a,i,i,2x,f7.2,2x,f7.2, 2x,f7.2,2x,f7.2,2x,f15.2,i,2x,f7.2,i,i)'

        printf,lun,str_lr,nnn[0],tot_evnts,hbias,res[1],res[2],cal,FWHM,xray_num,FWHM_err,min_evt, max_evt, format='(a,",",2(i,","),4(f10.2,","),f10.4,",",i,",",f10.4,",",2(i,","))'
        
;        openw,lun2,outfile2,/append
        printf,lun2,str_lr,nnn[0],tot_evnts,off,res[1],res[2],cal,FWHM,xray_num,FWHM_err, $
           min_evt, max_evt,hbias,htre,  $
           format='(a,",",2(i,","),4(f10.2,","),f10.4,",",i,",",f10.4,",",2(i,","),",",f10.4,",",f10.4)'
       
     endfor
;    endif
     close,lun
     free_lun,lun
     close,lun2
     free_lun,lun2
  endif else begin
     print, 'no files of this type'
  endelse

fine:
end






