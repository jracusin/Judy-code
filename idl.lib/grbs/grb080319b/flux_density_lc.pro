pro run_wt_xspec
  
  times=['1_14','2_14','3_14','4_14','5_10','6_10','7_8','8_0','9_0','10_0']
  files='seg'+times+'_wt.pha.grpmin40.nocr'
  outfiles='xspec_'+times+'.batch.nocr'
  outfiles2=outfiles+'.fd'
  euffile='seg'+times+'_wt_euf.data'
  trigtime=227599971.904d  

  for i=0,n_elements(times)-1 do begin 
     
     datfiles=file_search('seg'+ntostr(i+1)+'_*wt_Pow_nocr.dat')
     efiles=file_search('seg'+ntostr(i+1)+'_*_wt.evt')

     read_time_specfit,spec1,trigtime,datfiles=datfiles,efiles=efiles  

;;      openw,lun,outfiles[i],/get_lun
;;      printf,lun,'data '+files[i]
;;      printf,lun,'ignore 0.-0.3  '                                             
;;      printf,lun,'ignore bad      '                                            
;;      printf,lun,'query yes        '                                           
;;      printf,lun,'mo wabs*zwabs*pow '                                          
;;      printf,lun,'1.00000            '                                         
;;      printf,lun,'1.00000             '                                        
;;      printf,lun,'1.00000              '                                       
;;      printf,lun,'1.00000               '                                      
;;      printf,lun,'1.00000                '                                     
;;      printf,lun,'newpar 1 0.0112000 0    '                                   
;;      printf,lun,'newpar 3 0.937000        '                                   
;;      printf,lun,'newpar 2 0.085 0          '                              
;;      printf,lun,'fit 1000                   '                                 
;;      printf,lun,'fit 1000                    '                                
;;      printf,lun,'fit 1000                     '                               
;;      printf,lun,'cpd /ps                       '                              
;;      printf,lun,'setplot en                     '                             
;;      printf,lun,'plot ld res   '
;;      printf,lun,'exec mv pgplot.ps seg_'+times[i]+'.pha.grpmin40_Pow.ps '
;;      printf,lun,'set fileid [open seg_'+times[i]+'wt_Pow_nocr.dat w]        '
;;      printf,lun,'error stop 100,,maximum 20.0 2                            '  
;;      printf,lun,'tclout error 2                          '                    
;;      printf,lun,'set err2 [string trim $xspec_tclout]     '                   
;;      printf,lun,'regsub -all { +} $err2 { } cerr2          '                  
;;      printf,lun,'set lerr2 [split $cerr2]                   '                 
;;      printf,lun,'puts $fileid "nH_err [lindex $lerr2 0] [lindex $lerr2 1]"   '
;;      printf,lun,'error stop 100,,maximum 20.0 4                              '
;;      printf,lun,'tclout error 4                                              '
;;      printf,lun,'set err4 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $err4 { } cerr4                            '
;;      printf,lun,'set lerr4 [split $cerr4]                                    '
;;      printf,lun,'puts $fileid "Pow_err [lindex $lerr4 0] [lindex $lerr4 1]"  '
;;      printf,lun,'error stop 100,,maximum 20.0 5                              '
;;      printf,lun,'tclout error 5                                              '
;;      printf,lun,'set err5 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $err5 { } cerr5                            '
;;      printf,lun,'set lerr5 [split $cerr5]                                    '
;;      printf,lun,'puts $fileid "norm_err [lindex $lerr5 0] [lindex $lerr5 1]" '
;;      printf,lun,'tclout param 1                                              '
;;      printf,lun,'set par1 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $par1 { } cpar1                            '
;;      printf,lun,'set lpar1 [split $cpar1]                                    '
;;      printf,lun,'puts $fileid "nHgal [lindex $lpar1 0]"                      '
;;      printf,lun,'tclout param 2                                              '
;;      printf,lun,'set par2 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $par2 { } cpar2                            '
;;      printf,lun,'set lpar2 [split $cpar2]                                    '
;;      printf,lun,'puts $fileid "nH [lindex $lpar2 0]"                         '
;;      printf,lun,'tclout param 3                                              '
;;      printf,lun,'set par3 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $par3 { } cpar3                            '
;;      printf,lun,'set lpar3 [split $cpar3]                                    '
;;      printf,lun,'puts $fileid "z [lindex $lpar3 0]"                          '
;;      printf,lun,'tclout param 4                                              '
;;      printf,lun,'set par4 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $par4 { } cpar4                            '
;;      printf,lun,'set lpar4 [split $cpar4]                                    '
;;      printf,lun,'puts $fileid "PhInd [lindex $lpar4 0]"                      '
;;      printf,lun,'tclout param 5                                              '
;;      printf,lun,'set par5 [string trim $xspec_tclout]                        '
;;      printf,lun,'regsub -all { +} $par5 { } cpar5                            '
;;      printf,lun,'set lpar5 [split $cpar5]                                    '
;;      printf,lun,'puts $fileid "norm [lindex $lpar5 0]"                       '
;;      printf,lun,'tclout stat                                                 '
;;      printf,lun,'set stt [string trim $xspec_tclout]                         '
;;      printf,lun,'regsub -all { +} $stt { } cstt                              '
;;      printf,lun,'set lstt [split $cstt]                                      '
;;      printf,lun,'puts $fileid "chisq [lindex $lstt 0]"                       '
;;      printf,lun,'tclout dof                                                  '
;;      printf,lun,'set df [string trim $xspec_tclout]                          '
;;      printf,lun,'regsub -all { +} $df { } cdf                                '
;;      printf,lun,'set ldf [split $cdf]                                        '
;;      printf,lun,'puts $fileid "dof [lindex $ldf 0]"                          '
;;      printf,lun,'show rate                                                   '
;;      printf,lun,'tclout rate 1                                               '
;;      printf,lun,'set rte [string trim $xspec_tclout]                         '
;;      printf,lun,'regsub -all { +} $rte { } crte                              '
;;      printf,lun,'set lrte [split $crte]                                      '
;;      printf,lun,'puts $fileid "rate [lindex $lrte 0]"                        '
;;      printf,lun,'puts $fileid "model_rate [lindex $crte 2]"                  '
;;      printf,lun,'flux 0.3 10. err 10000 90                                   '
;;      printf,lun,'tclout flux 1                                               '
;;      printf,lun,'set flx [string trim $xspec_tclout]                         '
;;      printf,lun,'regsub -all { +} $flx { } cflx                              '
;;      printf,lun,'set lflx [split $cflx]                                      '
;;      printf,lun,'puts $fileid "flux [lindex $lflx 0]"                        '
;;      printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2]"   '
;;      printf,lun,'newpar 1 0 1 0 0 1e4 1e4                                    '
;;      printf,lun,'newpar 2 0 1 0 0 1e4 1e4                                    '
;;      printf,lun,'flux 0.3 10.0 err 10000 90                                  '
;;      printf,lun,'tclout flux 1                                               '
;;      printf,lun,'set flx [string trim $xspec_tclout]                         '
;;      printf,lun,'regsub -all { +} $flx { } cflx                              '
;;      printf,lun,'set lflx [split $cflx]                                      '
;;      printf,lun,'puts $fileid "unabs_flux [lindex $lflx 0]"                  '
;;      printf,lun,'close $fileid'
;;      printf,lun,'exit'
;;      close,lun
;;      free_lun,lun
;;      run='xspec - '+outfiles[i]+' > '+outfiles[i]+'.out'
;;      print,run
;;      spawn,run
     print,outfiles2[i]
     openw,lun,outfiles2[i],/get_lun
     printf,lun,'data '+files[i]
     printf,lun,'ignore 0.-0.3  '                                             
     printf,lun,'ignore bad      '                                            
     printf,lun,'query yes        '                                           
     printf,lun,'mo wabs*zwabs*pow '                                          
     printf,lun,'1.00000            '                                         
     printf,lun,'1.00000             '                                        
     printf,lun,'1.00000              '                                       
     printf,lun,'1.00000               '                                      
     printf,lun,'1.00000                '                                     
     printf,lun,'newpar 1 0.0112000 0    '                                   
     printf,lun,'newpar 3 0.937000        '                                   
     printf,lun,'newpar 2 0.085 0          '                              
     printf,lun,'newpar 4 '+ntostr(spec1.pow)+' 0'
     printf,lun,'fit 1000                   '                                 
     printf,lun,'fit 1000                    '                                
     printf,lun,'fit 1000                     '                               
     printf,lun,'newpar 1 0 1 0 0 1e4 1e4                                    '
     printf,lun,'newpar 2 0 1 0 0 1e4 1e4                                    '
     printf,lun,'cpd /xw                       '                              
     printf,lun,'setplot en                     '                             
     printf,lun,'iplot eufspec   '
     printf,lun,'wd '+euffile[i]
     printf,lun,'q'
     printf,lun,'exit'
     close,lun
     free_lun,lun
     print,euffile[i]
  endfor 
  
  return
end 

pro run_pc_xspec
  
  times=['4000_8000','1e4_1.3e4','1.6e4_4e4','4e4_2e5','2e5_1e6','1e6_1e7']
  files='spec_'+times+'.pha.grpmin20'
  outfiles='xspec_'+times+'.batch.nocr'
  outfiles2=outfiles+'.fd'
  euffile='spec_'+times+'_euf.data'
  for i=0,n_elements(times)-1 do begin 
     openw,lun,outfiles[i],/get_lun
     printf,lun,'data '+files[i]
     printf,lun,'ignore 0.-0.3  '                                             
     printf,lun,'ignore bad      '                                            
     printf,lun,'query yes        '                                           
     printf,lun,'mo wabs*zwabs*pow '                                          
     printf,lun,'1.00000            '                                         
     printf,lun,'1.00000             '                                        
     printf,lun,'1.00000              '                                       
     printf,lun,'1.00000               '                                      
     printf,lun,'1.00000                '                                     
     printf,lun,'newpar 1 0.0112000 0    '                                   
     printf,lun,'newpar 3 0.937000        '                                   
     printf,lun,'newpar 2 0.085 0          '                              
     printf,lun,'fit 1000                   '                                 
     printf,lun,'fit 1000                    '                                
     printf,lun,'fit 1000                     '                               
     printf,lun,'cpd /ps                       '                              
     printf,lun,'setplot en                     '                             
     printf,lun,'plot ld res   '
     printf,lun,'exec mv pgplot.ps spec_'+times[i]+'.pha.grpmin20_Pow.ps '
     printf,lun,'set fileid [open spec_'+times[i]+'_Pow_nocr.dat w]        '
     printf,lun,'error stop 100,,maximum 20.0 2                            '  
     printf,lun,'tclout error 2                          '                    
     printf,lun,'set err2 [string trim $xspec_tclout]     '                   
     printf,lun,'regsub -all { +} $err2 { } cerr2          '                  
     printf,lun,'set lerr2 [split $cerr2]                   '                 
     printf,lun,'puts $fileid "nH_err [lindex $lerr2 0] [lindex $lerr2 1]"   '
     printf,lun,'error stop 100,,maximum 20.0 4                              '
     printf,lun,'tclout error 4                                              '
     printf,lun,'set err4 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $err4 { } cerr4                            '
     printf,lun,'set lerr4 [split $cerr4]                                    '
     printf,lun,'puts $fileid "Pow_err [lindex $lerr4 0] [lindex $lerr4 1]"  '
     printf,lun,'error stop 100,,maximum 20.0 5                              '
     printf,lun,'tclout error 5                                              '
     printf,lun,'set err5 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $err5 { } cerr5                            '
     printf,lun,'set lerr5 [split $cerr5]                                    '
     printf,lun,'puts $fileid "norm_err [lindex $lerr5 0] [lindex $lerr5 1]" '
     printf,lun,'tclout param 1                                              '
     printf,lun,'set par1 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $par1 { } cpar1                            '
     printf,lun,'set lpar1 [split $cpar1]                                    '
     printf,lun,'puts $fileid "nHgal [lindex $lpar1 0]"                      '
     printf,lun,'tclout param 2                                              '
     printf,lun,'set par2 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $par2 { } cpar2                            '
     printf,lun,'set lpar2 [split $cpar2]                                    '
     printf,lun,'puts $fileid "nH [lindex $lpar2 0]"                         '
     printf,lun,'tclout param 3                                              '
     printf,lun,'set par3 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $par3 { } cpar3                            '
     printf,lun,'set lpar3 [split $cpar3]                                    '
     printf,lun,'puts $fileid "z [lindex $lpar3 0]"                          '
     printf,lun,'tclout param 4                                              '
     printf,lun,'set par4 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $par4 { } cpar4                            '
     printf,lun,'set lpar4 [split $cpar4]                                    '
     printf,lun,'puts $fileid "PhInd [lindex $lpar4 0]"                      '
     printf,lun,'tclout param 5                                              '
     printf,lun,'set par5 [string trim $xspec_tclout]                        '
     printf,lun,'regsub -all { +} $par5 { } cpar5                            '
     printf,lun,'set lpar5 [split $cpar5]                                    '
     printf,lun,'puts $fileid "norm [lindex $lpar5 0]"                       '
     printf,lun,'tclout stat                                                 '
     printf,lun,'set stt [string trim $xspec_tclout]                         '
     printf,lun,'regsub -all { +} $stt { } cstt                              '
     printf,lun,'set lstt [split $cstt]                                      '
     printf,lun,'puts $fileid "chisq [lindex $lstt 0]"                       '
     printf,lun,'tclout dof                                                  '
     printf,lun,'set df [string trim $xspec_tclout]                          '
     printf,lun,'regsub -all { +} $df { } cdf                                '
     printf,lun,'set ldf [split $cdf]                                        '
     printf,lun,'puts $fileid "dof [lindex $ldf 0]"                          '
     printf,lun,'show rate                                                   '
     printf,lun,'tclout rate 1                                               '
     printf,lun,'set rte [string trim $xspec_tclout]                         '
     printf,lun,'regsub -all { +} $rte { } crte                              '
     printf,lun,'set lrte [split $crte]                                      '
     printf,lun,'puts $fileid "rate [lindex $lrte 0]"                        '
     printf,lun,'puts $fileid "model_rate [lindex $crte 2]"                  '
     printf,lun,'flux 0.3 10. err 10000 90                                   '
     printf,lun,'tclout flux 1                                               '
     printf,lun,'set flx [string trim $xspec_tclout]                         '
     printf,lun,'regsub -all { +} $flx { } cflx                              '
     printf,lun,'set lflx [split $cflx]                                      '
     printf,lun,'puts $fileid "flux [lindex $lflx 0]"                        '
     printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2]"   '
     printf,lun,'newpar 1 0 1 0 0 1e4 1e4                                    '
     printf,lun,'newpar 2 0 1 0 0 1e4 1e4                                    '
     printf,lun,'flux 0.3 10.0 err 10000 90                                  '
     printf,lun,'tclout flux 1                                               '
     printf,lun,'set flx [string trim $xspec_tclout]                         '
     printf,lun,'regsub -all { +} $flx { } cflx                              '
     printf,lun,'set lflx [split $cflx]                                      '
     printf,lun,'puts $fileid "unabs_flux [lindex $lflx 0]"                  '
     printf,lun,'close $fileid'
     printf,lun,'exit'
     close,lun
     free_lun,lun
     run='xspec - '+outfiles[i]+' > '+outfiles[i]+'.out'
     print,run
     spawn,run
     
     openw,lun,outfiles2[i],/get_lun
     printf,lun,'data '+files[i]
     printf,lun,'ignore 0.-0.3  '                                             
     printf,lun,'ignore bad      '                                            
     printf,lun,'query yes        '                                           
     printf,lun,'mo wabs*zwabs*pow '                                          
     printf,lun,'1.00000            '                                         
     printf,lun,'1.00000             '                                        
     printf,lun,'1.00000              '                                       
     printf,lun,'1.00000               '                                      
     printf,lun,'1.00000                '                                     
     printf,lun,'newpar 1 0.0112000 0    '                                   
     printf,lun,'newpar 3 0.937000        '                                   
     printf,lun,'newpar 2 0.085 0          '                              
     printf,lun,'fit 1000                   '                                 
     printf,lun,'fit 1000                    '                                
     printf,lun,'fit 1000                     '                               
     printf,lun,'newpar 1 0 1 0 0 1e4 1e4                                    '
     printf,lun,'newpar 2 0 1 0 0 1e4 1e4                                    '
     printf,lun,'cpd /xw                       '                              
     printf,lun,'setplot en                     '                             
     printf,lun,'iplot eufspec   '
     printf,lun,'wd '+euffile[i]
     printf,lun,'q'
     printf,lun,'exit'
     close,lun
     free_lun,lun
     print,euffile[i]
  endfor 
  
  return
end 

pro read_time_specfit,specstr,trigtime,dir=dir,datfiles=datfiles,add=add,nocr=nocr,efiles=efiles
  
  if n_params() eq 0 then begin
     print,'syntax - read_specfit_wtpu,specstr,dir=dir'
     return
  endif 
  
  d2=dblarr(2)
  specstr=create_struct('seg',0,$
                        'mode','',$
                        'nev',0L,$
                        'nocr',0,$
                        'z',0.,$
                        'nhgal',0d,$
                        'nh',0d,$
                        'nh_err',d2,$
                        'pow',0d,$
                        'pow_err',d2,$
                        'norm',0d,$
                        'norm_err',d2,$
                        'chisq',0d,$
                        'dof',0L,$
                        'flux',0d,$
                        'flux_err',d2,$
                        'unabs_flux',0d,$
                        'rate',0d,$
                        'model_rate',0d,$
                        'pu_corr',0d,$
                        'exptime',0d,$
                        'tmin',0d,$
                        'tmax',0d,$
                        'regime','')
  
  if n_elements(add) eq 0 then add=''
  if n_elements(dir) eq 0 then dirs='./' else dirs=dir+'/'
  if n_elements(datfiles) eq 0 then datfiles=file_search(dirs+'seg*'+add+'.dat')
  n=n_elements(datfiles)
  
;  evfiles=file_search(dirs+'seg*.evt')
;  bgpos=strpos(evfiles,'bg')
;  wsrc=where(bgpos eq -1)
;  evfiles=evfiles[wsrc]

  specstr=replicate(specstr,n)
  pix=strarr(n)
  
  for j=0,n-1 do begin
;     for k=0,1 do begin
;        i=j*2+k
     i=j
        print,i
;        if k eq 0 then add='_nocr' else add=''
        add='_nocr'
;     file=datfiles[i]
;     segpos=strpos(file,'seg')
;     seg=strmid(file,segpos+3,2)
;     spos=strpos(seg,'w')
;     wpos=where(spos ne -1,nwpos)
;     if nwpos gt 0 then seg=strmid(seg,0,1)
     
        seg=j+1
        if n_elements(datfiles) eq 0 then begin 
           file=file_search('seg'+ntostr(seg)+'_*wt_Pow'+add+'.dat')
           if not exist(file) then file=file_search('seg'+ntostr(seg)+'_*pc_Pow'+add+'.dat')
        endif else file=datfiles[i]
        if n_elements(efiles) eq 0 then begin 
           efile='seg'+ntostr(seg)+'_*.evt'
        endif else efile=efiles[i]
     hdr=headfits(efile)
     tmin=sxpar(hdr,'TSTART')-trigtime
     tmax=sxpar(hdr,'TSTOP')-trigtime
     
     npos=strpos(file,'nocr')
     wno=where(npos ne -1)
     if wno[0] ne -1 then specstr[i].nocr=1

     mpos=strpos(file,'_Pow')
     md=strmid(file,mpos-2,2)
     specstr[i].mode=md
     
;     evfile='seg'+ntostr(seg)+'wt.evt'
;     if not exist(evfile) then evfile='seg'+ntostr(seg)+'pc.evt' 
;     ev=mrdfits(evfile,1)
;     specstr[i].tmin=min(ev.time)-trigtime
;     specstr[i].tmax=max(ev.time)-trigtime
    
     openr,lun,file,/get_lun
     line=readline(lun)
     
;     if line[0] ne 'N_ev' then begin 
        specstr[i].nh_err=[line[1],line[2]]
        
        line=readline(lun)
        specstr[i].pow_err=[line[1],line[2]]
        
        line=readline(lun)
        specstr[i].norm_err=[line[1],line[2]]
        line=readline(lun)

        specstr[i].nhgal=line[1]
        line=readline(lun)
        specstr[i].nh=line[1]
        line=readline(lun)
        if line[0] eq 'z' then begin
           specstr[i].z=line[1]
           line=readline(lun)
        endif 

        specstr[i].pow=line[1]
        
        line=readline(lun)
        specstr[i].norm=line[1]
        line=readline(lun)

        specstr[i].chisq=line[1]
        
        line=readline(lun)
        specstr[i].dof=line[1]
        
        line=readline(lun)
        specstr[i].rate=line[1]
        
        line=readline(lun)
        specstr[i].model_rate=line[1]
        
        line=readline(lun)
        specstr[i].flux=line[1]

        line=readline(lun)
        specstr[i].flux_err=[line[1],line[2]]

        line=readline(lun)
        specstr[i].unabs_flux=line[1]
        
;        line=readline(lun)
;        specstr[i].nev=line[1]
;     endif; else begin 
;        specstr[i].nev=line[1]
;     endelse 
     
     if not eof(lun) then begin
        line=readline(lun)
        if strtrim(line[0],2) eq 'exptime' then begin
           specstr[i].exptime=line[1]
        endif 
     endif 
     
;     if not eof(lun) then begin
;        line=readline(lun)
;        if strtrim(line[0],2) eq 'tmin' then begin
           specstr[i].tmin=tmin;line[1]
;        endif 
;     endif 
     
;     if not eof(lun) then begin
;        line=readline(lun)
;        if strtrim(line[0],2) eq 'tmax' then begin
           specstr[i].tmax=tmax;line[1]
;        endif 
;     endif 
     
     if not eof(lun) then begin
        line=readline(lun)
        if strtrim(line[0],2) eq 'regime' then begin
           specstr[i].regime=line[1]+' '+line[2]
        endif 
     endif 
        
     specstr[i].seg=seg
     
     close,lun
     free_lun,lun
  
     specstr[i].nh_err=abs(specstr[i].nh-specstr[i].nh_err)
     specstr[i].norm_err=abs(specstr[i].norm-specstr[i].norm_err)
     specstr[i].pow_err=abs(specstr[i].pow-specstr[i].pow_err)
     specstr[i].flux_err=abs(specstr[i].flux-specstr[i].flux_err)
;  endfor      
  endfor 
;  specstr=specstr[sort(specstr.seg)]
  mwrfits,specstr,'time_specfit.fits',/create
  return
end 

pro flux_density_lc
  
  trigtime=227599971.904d
  effen=2. ;;keV
  fdrange=[1e-6,5e-6]
;  effen=1. ;;keV
;  fdrange=[3e-6,7e-6]
  
  ;;WT
  fd1=fltarr(10)
  datfiles=strarr(10)
  efiles=strarr(10)
;  plot,[0.1,10],[0.1,10],/nodata,/xlog,/ylog
  for i=0,9 do begin
     datfiles[i]=file_search('seg'+ntostr(i+1)+'_*wt_Pow_nocr.dat')
     efiles[i]=file_search('seg'+ntostr(i+1)+'_*_wt.evt')
     file='seg'+ntostr(i+1)+'_*_wt_euf.data'
     file=file_search(file)
     readcol,file,en,enerr,fd,fderr,mo
     fd1[i]=interpol(mo,en,effen)
;     oplot,en,mo
;     oplot,[effen,effen],[0.1,10]
;     oplot,[0.1,10],[fd1[i],fd1[i]]
;     k=get_kbrd(10)
  endfor 
  read_time_specfit,spec1,trigtime,datfiles=datfiles,efiles=efiles
  
  ;;PC
  times=['4000_8000','1e4_1.3e4','1.6e4_4e4','4e4_2e5','2e5_1e6','1e6_1e7']
  datfiles='spec_'+times+'_Pow_nocr.dat'
  efiles='spec_'+times+'.evt'
  read_time_specfit,spec2,trigtime,datfiles=datfiles,efiles=efiles
  euffile='spec_'+times+'_euf.data'
  fd2=fltarr(6)
  for i=0,5 do begin
     readcol,euffile[i],en,enerr,fd,fderr,mo
     fd2[i]=interpol(mo,en,effen)
  endfor 
  
  concat_structs,spec1,spec2,spec
  fd=[fd1,fd2]
  
  fdtojy=1.509e3 ;;keV cm-2 s-1 keV-1
  fdfact=fd/spec.rate/fdtojy  ;; in Jy
  fdfacterr1=fdfact*spec.flux_err[0]/spec.flux
  fdfacterr2=fdfact*spec.flux_err[1]/spec.flux
  time=(spec.tmax-spec.tmin)/2.+spec.tmin
  
  !p.multi=[0,1,2]
  t=!tsym.times
  plot,time,fdfact,/yno,psym=3,/xlog,xtitle='Time (s)',ytitle=ntostr(fix(effen))+' keV Flux Density/CTR (Jy counts!U-1!N s!U-1!N)',yrange=fdrange,/ysty
  for i=0,n_elements(spec)-1 do begin
     oplot,[spec[i].tmin,spec[i].tmax],[fdfact[i],fdfact[i]]
     oplot,[time[i],time[i]],[fdfact[i]-fdfacterr1[i],fdfact[i]+fdfacterr2[i]]
  endfor 
  mfdfact=weighted_mean(fdfact,fdfacterr1,mfderr)
  oplot,[10,1e7],[mfdfact,mfdfact],line=2
  plot,time,spec.pow,psym=3,/xlog,yrange=[1.5,2.1],/ysty,xtitle='Time (s)',ytitle='PhInd'
  for i=0,n_elements(spec)-1 do begin 
     oplot,[spec[i].tmin,spec[i].tmax],[spec[i].pow,spec[i].pow]
     oplot,[time[i],time[i]],[spec[i].pow-spec[i].pow_err[0],spec[i].pow+spec[i].pow_err[1]]
  endfor 

  !p.multi=0
  
  stop
  lc=lcout2fits('../lc_newout_manual.txt')
  time=lc.time
  tbin=lc.tstop-lc.time
  rate=lc.src_rate
  rateerr=lc.src_rate_err
;  readcol,'xrt_lc_rate.dat',time,tbin,rate,rateerr
  !p.multi=[0,1,2]
;  yrange=[1e-15,1e-7]
  yrange=[1e-4,1e4]
  ploterror,time,rate,tbin,rateerr,/xlog,/ylog,/nohat,xrange=[10,1e7],yrange=yrange,xtitle='Time (s)',ytitle='Count rate (cts s!U-1!N)',psym=3
;  readcol,'published_XRT_lc.dat',btime,btbin,brate,brateerr
;  oploterror,btime,brate,btbin,brateerr,psym=3,/nohat,errcolor=!green
  
  fluxdens=rate
  fluxdenserr=rateerr
;  for i=0,n_elements(fdfact)-1 do begin
;     w=where(time ge spec[i].tmin and time le spec[i].tmax)
;     fluxdens[w]=rate[w]*fdfact[i]
;     fluxdenserr[w]=rateerr[w]*fdfact[i]
;;     fluxdenserr[w]=sqrt((rateerr[w]/rate[w])^2.+(fdfacterr1[i]/fdfact[i])^2)*fdfact[i]
;  endfor 
  fluxdens=rate*mfdfact
;  fluxdenserr=rateerr*mfdfact
  fluxdenserr=sqrt((rateerr/rate)^2.+(mfderr/mfdfact)^2.)*fluxdens
  ploterror,time,fluxdens,tbin,fluxdenserr,/xlog,/ylog,/nohat,xrange=[10,1e7],xtitle='Time (s)',ytitle='Flux Density (Jy)',psym=3
  readcol,'../xrt_lc_Jy_new_lastsent.dat',otime,otbin,ofluxdens,ofluxdenserr  ;;old calc
  oploterror,otime,ofluxdens,otbin,ofluxdenserr,/nohat,psym=3,errcolor=!red
;  readcol,'../xrt_lc_Jy_new.dat',otime,otbin,ofluxdens,ofluxdenserr  ;;older calc
;  oploterror,otime,ofluxdens,otbin,ofluxdenserr,/nohat,psym=3,errcolor=!blue
  
  !p.multi=0
  
  writecol,'xrt_lc_Jy_new4.dat',time,tbin,fluxdens,fluxdenserr,header=['Time','Tbin','FluxDens','FluxDensErr']
  stop
  return
end 
