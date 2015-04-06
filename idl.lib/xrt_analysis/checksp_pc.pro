pro checksp_pc,quicklook=quicklook
;calculates mean and sigma for ACIS default grades

  lo_lim=2100.
  up_lim=2400.
  sbin=5.
  outfile='pc_spec_result_summary.xls'
  spawn,'rm '+outfile+''
  cpha=2.58
  rbin=8.
  thrsh=40
  eline=5895.0

  str_pc='*pc.cat'
  pccat=findfile(str_pc)
  
;  spawn,'ls  '+str_pc+' > tttpc2'
;  spawn,'wc tttpc2   >  wwcc3'
;  readcol,'wwcc3',ppp, format='(i)'
  if pccat[0] ne '' then begin
;     readcol,'tttpc2',pccat,format='(a)'	
     n_pc_ldp=n_elements(pccat) 
     openw,lun,outfile,/get_lun ;,/append   
     printf,lun,'File Name', '#frames','tot_evt', 'min_totevnts','max_totevnts','thrsh','splt_mean_ka','splt_sig_ka','splt_cal(eV/dn)','splt_FWHM','splt_tot_ka','splt_FWHM_err','iso_mean_ka','iso_sig_ka','iso_cal(eV/dn)','iso_FWHM','iso_tot_ka','iso_FWHM_err',format='(a,",",17(a,","))'
;     close,1

     For il=0,n_pc_ldp-1 do begin	
        str_pc=pccat[il]
;        spawn,'more ' +str_pc+ '> tttpc'
;        spawn,'wc tttpc   >  wwcc2'
;        readcol,'wwcc2',nnn, format='(a)'
        if n_elements(str_pc) gt 0  then begin
           pha_creation:
           readcol,str_pc,frame,format='(a)'
           spec1=fltarr(4096)
           spec2=fltarr(4096)
           num_evnts=[0]
           if n_elements(frame) gt 1 then begin 
              for ik=0,n_elements(frame)-1 do begin
;                 spawn,'rm ttt*spec'
                 print,frame[ik]
                 nullext=mrdfits(frame[ik],0,hdr0,/silent)
                 events=mrdfits(frame[ik],1,hdr,/silent)
                 state=sxpar(hdr0,'XRTSTATE')
                 
                 if (state eq 'Manual  ') then begin
                    neve=n_elements(events)
                    print,'***********',  neve
                    num_evnts=[num_evnts,neve]
                    
                    if neve gt 1 then begin
                       readevents,frame[ik]
                       acisfilter,'ttt1_c1.spec',thrsh,'dummy',status,/DN,acis_grades=[0],CIRCLE=long([40,40,30]) 
                       acisfilter,'ttt1_c2.spec',thrsh,'dummy',status,/DN,acis_grades=[0],CIRCLE=long([570,40,30])
                       acisfilter,'ttt1_c3.spec',thrsh,'dummy',status,/DN,acis_grades=[0],CIRCLE=long([40,570,30])
                       acisfilter,'ttt1_c4.spec',thrsh,'dummy',status,/DN,acis_grades=[0],CIRCLE=long([570,570,30])

                       acisfilter,'ttt2_c1.spec',thrsh,'dummy',status,/DN,acis_grades=[0,2,8,16,64],CIRCLE=long([40,40,30])
                       acisfilter,'ttt2_c2.spec',thrsh,'dummy',status,/DN,acis_grades=[0,2,8,16,64],CIRCLE=long([570,40,30])
                       acisfilter,'ttt2_c3.spec',thrsh,'dummy',status,/DN,acis_grades=[0,2,8,16,64],CIRCLE=long([40,570,30])
                       acisfilter,'ttt2_c4.spec',thrsh,'dummy',status,/DN,acis_grades=[0,2,8,16,64],CIRCLE=long([570,570,30])
                       
                       tt1_c1=mrdfits('ttt1_c1.spec',1,hdr)
                       tt1_c2=mrdfits('ttt1_c2.spec',1,hdr)
                       tt1_c3=mrdfits('ttt1_c3.spec',1,hdr)
                       tt1_c4=mrdfits('ttt1_c4.spec',1,hdr)
                       stop
                       cc1=0
                       cc2=0
                       if (n_elements(tt1_c1) gt 1) then cc1=cc1+tt1_c1.counts
                       if (n_elements(tt1_c2) gt 1) then cc1=cc1+tt1_c2.counts
                       if (n_elements(tt1_c3) gt 1) then cc1=cc1+tt1_c3.counts
                       if (n_elements(tt1_c4) gt 1) then cc1=cc1+tt1_c4.counts
                       if (n_elements(tt1_c1) gt 1 OR n_elements(tt1_c2) gt 1 OR n_elements(tt1_c3) gt 1 OR n_elements(tt1_c4) gt 1) then spec1=spec1+cc1

                       tt2_c1=mrdfits('ttt2_c1.spec',1,hdr)
                       tt2_c2=mrdfits('ttt2_c2.spec',1,hdr)
                       tt2_c3=mrdfits('ttt2_c3.spec',1,hdr)
                       tt2_c4=mrdfits('ttt2_c4.spec',1,hdr)

                       if (n_elements(tt2_c1) gt 1) then cc2=cc2+tt2_c1.counts
                       if (n_elements(tt2_c2) gt 1) then cc2=cc2+tt2_c2.counts
                       if (n_elements(tt2_c3) gt 1) then cc2=cc2+tt2_c3.counts
                       if (n_elements(tt2_c4) gt 1) then cc2=cc2+tt2_c4.counts
                       if (n_elements(tt2_c1) gt 1 OR n_elements(tt2_c2) gt 1 OR n_elements(tt2_c3) gt 1 OR n_elements(tt2_c4) gt 1) then spec2=spec2+cc2
                    endif
                 endif 
              endfor
              if (total(spec1 gt 0) AND total(spec2 gt 0)) then begin
                 num_evnts=num_evnts[1:*]
                 min_totevt=min(num_evnts)
                 max_totevt=max(num_evnts)
                 tot_evt=total(num_evnts)


                                ;Isolated default grades
                 hh1=rebin(spec1,4096./rbin)*rbin  & hx1=findgen(4096/rbin)*rbin
                 if not keyword_set(quicklook) then plot,hx1,hh1,xr=[2000,3000],psym=10
                 thh1=hh1 & thh1[0:lo_lim/rbin]=0. & thh1[up_lim/rbin:*]=0
                 hf1=gaussfit(hx1,thh1,res1,nterms=3)
                 if not keyword_set(quicklook) then oplot,hx1,hf1,li=5,color=177,thick=3   
                 tot_ka1=total(spec1[lo_lim:up_lim])
                 sig1=cpha*res1[2]
                 cal1=eline/res1[1]
                 FWHM1=res1[2]*cal1*2.354
                 FWHM_err1=FWHM1/(tot_ka1)^0.5

                                ;ACIS Events	   
                 hh2=rebin(spec2,4096./rbin)*rbin  & hx2=findgen(4096/rbin)*rbin
                 if not keyword_set(quicklook) then plot,hx2,hh2,xr=[2000,3000],psym=10
                 thh2=hh2 & thh2[0:lo_lim/rbin]=0. & thh2[up_lim/rbin:*]=0
                 hf2=gaussfit(hx2,thh2,res2,nterms=3)
                 if not keyword_set(quicklook) then oplot,hx2,hf2,li=5,color=177,thick=3   
                 tot_ka2=total(spec2[lo_lim:up_lim])
                 sig2=cpha*res2[2]
                 cal2=eline/res2[1]
                 FWHM2=res2[2]*cal2*2.354
                 FWHM_err2=FWHM2/(tot_ka2)^0.5	   

                 nomeps=str_pc+'.ps'
                 set_plot,'ps'
                 device,filename=nomeps  	
                 p1=[.05,.55,.95,.95]	
                 p2=[.05,.05,.95,.45]	

                 plot,hx1,hh1,psym=10,xr=[lo_lim,up_lim],/xst,title='G0 -'+str_pc,pos=p1
                 oplot,hx1,hf1,li=5,thick=5   
                 iu=where(hx1 gt lo_lim and hx1 lt up_lim)
                 xyouts,lo_lim*1.005,max(hh1[iu])*0.9,'mean:'+strtrim(string(res1[1]),2)
                 xyouts,lo_lim*1.005,max(hh1[iu])*0.9/1.2,'stdev:'+strtrim(string(res1[2]),2)	

                 plot,hx2,hh2,psym=10,xr=[lo_lim,up_lim],/xst,title='GS -'+str_pc,pos=p2,/noe
                 oplot,hx2,hf2,li=5,thick=5   
                 xyouts,lo_lim*1.005,max(hh2[iu])*0.9,'mean:'+strtrim(string(res2[1]),2)
                 xyouts,lo_lim*1.005,max(hh2[iu])*0.9/1.2,'stdev:'+strtrim(string(res2[2]),2)	

                 device,/close
                 set_plot,'x'
;              	openw,1,outfile,/append   
                 printf,lun,str_pc,nnn[0],tot_evt,min_totevt, max_totevt,thrsh,res2[1],res2[2],cal2,FWHM2,tot_ka2, FWHM_err2,res1[1],res1[2],cal1,FWHM1,tot_ka1, FWHM_err1,format='(a,",",5(i,","),3(f10.2,","),f10.4,",",i,",",f10.4,",",3(f10.2,","),f10.4,",",i,",",f10.4,",")'


              endif 
           endif
        endif 
     endfor
  endif else begin
     print, "no Manual state frames of this type"
  endelse 
  close,lun
  free_lun,lun
fine:
end


















