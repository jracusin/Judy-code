pro lrpd_lcurve_ccdt
  
  cfiles='pass*lrpd*.cat'
  cfiles=findfile(cfiles)
  if cfiles[0] ne '' then begin 
     tmp=strpos(cfiles,'LDP')
     tmp2=strpos(cfiles,'.lrpd.cat')
     ldp=strmid(cfiles,tmp+3,tmp2-tmp-3)
     ldp=ldp[rem_dup(ldp)]
     nldp=n_elements(ldp)
     ldpn=ldp*1L
     day=strmid(cfiles[0],9,3)
     
     dir='/bulk/vader3/xrtitos/L_and_E0/LRPD_day'+day+'_ldp'+ntostr(min(ldpn))+'_'+ntostr(max(ldpn))+'/'
     
     print,'Making lrpd dir on vader3'
     print,dir
     spawn,'mkdir '+dir
     print,'Copying lrpd files:'
     print,cfiles
     spawn,'cp pass*lrpd* '+dir
     
     for i=0,nldp-1 do begin 
        file=cfiles[i]

;file='pass_20043511047_science.0_LDP1902.lrpd.cat'
;dir='/bulk/vader3/xrtitos/L_and_E0/LrPD_day351_ldp1884_1902/'
        readfile=dir+file
        readcol,readfile,frame,format='(a)'
        
        gain=2.58
        
        ccdt=fltarr(n_elements(frame))
        cnt=fltarr(n_elements(frame))
        exposure=fltarr(n_elements(frame))
        cnts=fltarr(n_elements(frame))
        bias=fltarr(n_elements(frame))
        median=fltarr(n_elements(frame))

        for k=0,n_elements(frame)-1 do begin
           tframe=dir+frame[k]
           tab=mrdfits(tframe,1,hdr,/silent)
           tt=mrdfits(tframe,0,hd0,/silent)
           ccdt[k]=sxpar(hd0,'T_CCD')
           cnt[k]=sxpar(hd0,'N_GT_LLD')
           exposure[k]=sxpar(hd0,'EXPOSURE')
           bias[k]=sxpar(hd0,'BIAS_LVL')
           median[k]=sxpar(hd0,'MEDIANPX')
        endfor
        cnts=cnt/exposure

        titstr=strmid(file,5,11)+'-'+strmid(file,27,7) 
        
        nomeps=strmid(file,27,7)+'_lcurve_bias_ccdt.ps'
        set_plot,'ps'
        print,'Writing out plot file: '+dir+nomeps
        device,filename=dir+nomeps
        
        !p.multi=[0,1,2]
        
        plot,ccdt,cnts,psym=1,xtit='CCD temperature',ytit='cnts',title='LrPD '+titstr+' Lightcurve vs CCD_Temp'
        
;        device,/close
;        set_plot,'x'
;        spawn,'gv '+dir+nomeps+''
        
        nomeps=strmid(file,27,7)+'_bias_ccdt.ps'
;        set_plot,'ps'
;        device,filename=dir+nomeps
        
        plot,ccdt,bias,yr=[140,280],psym=2,xtit='CCD temperature',ytit='Bias',title='LrPD '+titstr+' Bias vs CCD_Temp'
        oplot,ccdt,median,psym=6
        device,/close
        set_plot,'x'
;        spawn,'gv '+dir+nomeps+''
        !p.multi=0
     endfor
  endif 

return
end
