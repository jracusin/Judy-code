pro extract_lrpd_info
  
  ;;;;making lrpd files
  str_lrpd='*lrpd.cat'
  spawn,'ls  '+str_lrpd+' > tttlrpd2'
  readcol,'tttlrpd2',lrpdcat,format='(a)',/silent
  n_lrpd_ldp=n_elements(lrpdcat)
  
  cbias=0
  ct_ccd=0
  cmet=0
  for il=0,n_lrpd_ldp-1 do begin	
     filetmp=str_sep(lrpdcat[il],'.cat')
     outfile=filetmp[0]+'.extract'
     openw,elun,outfile,/get_lun
     printf,elun,'Frame      Bias          CCD Temp'
     str_lr=lrpdcat[il]
     spawn,'more ' +str_lr+ '> tttlrpd'
     spawn,'wc tttlrpd   >  wwcc2'
     readcol,'tttlrpd',frame,format='(a)',/silent
     wframe=strpos(frame,'.fits')
     wframe=where(wframe gt 0,nframe)

     for ik=0,nframe-1 do begin
        sep=str_sep(frame[ik],'frame')
        sep=sep[1]
        pos=strpos(sep,'.')
        fnum=strmid(sep,0,pos)
        ttt=mrdfits(frame[ik],0,hdt,/silent)
        bias=sxpar(hdt,'BIAS')
        t_ccd=sxpar(hdt,'T_CCD')
        cbias=[cbias,bias]
        ct_ccd=[ct_ccd,t_ccd]
        met_strt=sxpar(hdt,'MET_STRT')
        cmet=[cmet,met_strt]
        if fnum lt 10 then sp=' ' else sp=''
        printf,elun,fnum,sp,bias,t_ccd
     
      endfor
     close,/all
     free_lun,elun
 
  endfor 
  
  cbias=cbias[1:*]
  ct_ccd=ct_ccd[1:*]
  cmet=cmet[1:*]
  
  w=where(cbias gt 0)
  get_file_begin,fb
  
  f_ahk=fb+'_ahk.0.stripchart_table.fits'
  
  data_ahk=mrdfits(f_ahk,1,ahk_head)
  nl_ahk=n_elements(data_ahk)
  
  nf=n_elements(cmet)
  data_lrpd=dblarr(nf,3)
  data_lrpd[*,0]=cmet
  data_lrpd[*,1]=cbias
  data_lrpd[*,2]=ct_ccd
  
  data_lrpd_rebin=dblarr(nl_ahk,3)
  for n=0,2 do data_lrpd_rebin(*,n)=interpol(data_lrpd(*,n),data_lrpd(*,0),data_ahk.sctime)
  
  w=where((data_ahk.sctime gt min(data_lrpd[*,0])) and (data_ahk.sctime lt max(data_lrpd[*,0])),nw)
  w2=where((data_lrpd[*,0] ge min(data_ahk.sctime)) and (data_lrpd[*,0] le max(data_ahk.sctime)),nw2)
  
  data_lrpd_rebin=data_lrpd_rebin[w,*]
  data_ahk=data_ahk[w]
  
  ;;;;;;;;PLOTS
  
  
  if (nw gt 0) and (nw2 gt 0) then begin 
     !p.multi=[0,2,2]
     plot,data_lrpd[w2,1],data_lrpd[w2,0]-min(data_lrpd[w2,0]),$
        ytitle='MET Start - '+ntostr(min(data_lrpd[w2,0]))+' (s)',$
        xtitle='LRPD Bias',/ynozero,$
        title=fb+'_science.0',psym=1
     plot,data_lrpd_rebin[*,1],data_ahk.CAMERATEMP_YSI44004_,/ynozero,$
        xtitle='LRPD Bias',ytitle='FPCA Temp',psym=1
     plot,data_lrpd_rebin[*,1],data_ahk.SIGNALCHNTEMP_AD590_,/ynozero,$
        xtitle='LRPD Bias',ytitle='Signal Chain Temp',psym=1
     plot,data_lrpd[w2,1],data_lrpd[w2,2],/ynozero,$
        xtitle='LRPD Bias',ytitle='CCD Temp',psym=1
     !p.multi=0
  endif 

  return
end 
