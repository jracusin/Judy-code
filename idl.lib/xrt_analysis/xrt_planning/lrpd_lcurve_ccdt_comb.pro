pro lrpd_lcurve_ccdt_comb,dirs,exclude_ldp=exclude_ldp
  
  if n_params() eq 0 then begin
     print,'syntax - lrpd_lcurve_ccdt_comb,dirs'
     print,'            (i.e.  dirs=[pass_20043511558,pass_2004***]'
     return
  endif 
  
  ccdt=0.
  cnt=0.
  exposure=0.
  bias=0.
  median=0.
  ll=0L
  
  for d=0,n_elements(dirs)-1 do begin 
     
     cfiles='pass*lrpd*.cat'
     cfiles=findfile(dirs[d]+'/'+cfiles)
     
     if cfiles[0] ne '' then begin 
        tmp=strpos(cfiles,'LDP')
        tmp2=strpos(cfiles,'.lrpd.cat')
        ldp=strmid(cfiles,tmp+3,tmp2-tmp-3)
        ldp=ldp[rem_dup(ldp)]
        nldp=n_elements(ldp)
        ldpn=ldp*1L
        ll=[ll,ldpn]
        day=strmid(cfiles[0],9,3)
        
        if n_elements(exclude_ldp) gt 0 then begin 
           wldp=where(ldpn ne exclude_ldp,nwldp)
           ldp=ldp[wldp]
;           ldpn=ldpn[wldp]
           cfiles=cfiles[wldp]
           nldp=nldp-1
        endif 
           
        dir='/bulk/vader3/xrtitos/L_and_E0/LRPD_day'+day+'_ldp'+ntostr(min(ldpn))+'_'+ntostr(max(ldpn))+'/'
        
        for i=0,nldp-1 do begin 
           file=strmid(cfiles[i],17,43)

           readfile=dir+file
           readcol,readfile,frame,format='(a)'
           
           gain=2.58
           
;           ccdt=fltarr(n_elements(frame))
;           cnt=fltarr(n_elements(frame))
;           exposure=fltarr(n_elements(frame))
;           cnts=fltarr(n_elements(frame))
;           bias=fltarr(n_elements(frame))
;           median=fltarr(n_elements(frame))

           for k=0,n_elements(frame)-1 do begin
              tframe=dir+frame[k]
              tab=mrdfits(tframe,1,hdr,/silent)
              tt=mrdfits(tframe,0,hd0,/silent)
              ccdt=[ccdt,sxpar(hd0,'T_CCD')]
              cnt=[cnt,sxpar(hd0,'N_GT_LLD')]
              exposure=[exposure,sxpar(hd0,'EXPOSURE')]
              bias=[bias,sxpar(hd0,'BIAS_LVL')]
              median=[median,sxpar(hd0,'MEDIANPX')]
           endfor
           cnts=cnt/exposure
           
;           titstr=strmid(file,5,11)+'-'+strmid(file,27,7) 
           
        endfor
     endif 
  endfor  
  
  ccdt=ccdt[1:*]
  cnt=cnt[1:*]
  exposure=exposure[1:*]
  bias=bias[1:*]
  median=median[1:*]
  ll=ll[1:*]
  
  lmin=ntostr(min(ll))
  lmax=ntostr(max(ll))
  nameps='/bulk/vader3/xrtitos/L_and_E0/lrpd_ldp'+lmin+'_'+lmax+'_lcurve_bias_ccdt_comb.ps'
  set_plot,'ps'
  print,'Writing out plot file: '+nameps
  device,filename=nameps
  !p.multi=[0,1,2]
  
  titstr='LDP '+lmin+'-'+lmax
  plot,ccdt,cnts,psym=1,xtit='CCD temperature',ytit='cnts',title='LrPD '+titstr+' Lightcurve vs CCD_Temp'
  
  nomeps=strmid(file,27,7)+'_bias_ccdt.ps'
  
  plot,ccdt,bias,yr=[140,280],psym=2,xtit='CCD temperature',ytit='Bias',title='LrPD '+titstr+' Bias vs CCD_Temp'
  oplot,ccdt,median,psym=6
        device,/close
        set_plot,'x'
;        spawn,'gv '+dir+nomeps+''
  !p.multi=0



  return
end
