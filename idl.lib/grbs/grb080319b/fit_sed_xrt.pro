pro print_sed_table,spec
  
  s=' || '
  pm=' +/- '
  p=' (+'
  m=' -'
  e=')'
  regime=' T+'+strmid(spec.regime,3,6)+'s'
  
  colprint,'||'+regime+s+numdec(spec.pow_pow,2)+p+numdec(spec.pow_pow_err[1],2)+m+numdec(spec.pow_pow_err[0],2)+e+s+numdec(spec.pow_nh*1e2,1)+p+numdec(spec.pow_nh_err[1]*1e2,1)+m+numdec(spec.pow_nh_err[0]*1e2,1)+e+s+numdec(spec.pow_chisq,2)+' / '+ntostr(spec.pow_dof)+' = '+numdec(spec.pow_chisq/spec.pow_dof,2)+s;+$
;     s+numdec(spec.bknpow_pow1,2)+p+numdec(spec.bknpow_pow1_err[1],2)+m+numdec(spec.bknpow_pow1_err[0],2)+e+s+numdec(spec.bknpow_ebreak,2)+p+numdec(spec.bknpow_ebreak_err[1],2)+m+numdec(spec.bknpow_ebreak_err[0],2)+e+s+numdec(spec.bknpow_pow2,2)+p+numdec(spec.bknpow_pow2_err[1],2)+m+numdec(spec.bknpow_pow2_err[0],2)+e+s+numdec(spec.bknpow_nh,2)+p+numdec(spec.bknpow_nh_err[1],2)+m+numdec(spec.bknpow_nh_err[0],2)+e+s+numdec(spec.bknpow_chisq,2)+' / '+ntostr(spec.bknpow_dof)+' = '+numdec(spec.bknpow_chisq/spec.bknpow_dof,2)+$
 ;    s+numdec(spec.cutpow_pow,2)+p+numdec(spec.cutpow_pow_err[1],2)+m+numdec(spec.cutpow_pow_err[0],2)+e+s+numdec(spec.cutpow_ecut,2)+p+numdec(spec.cutpow_ecut_err[1],2)+m+numdec(spec.cutpow_ecut_err[0],2)+e+s+numdec(spec.cutpow_nh,2)+p+numdec(spec.cutpow_nh_err[1],2)+m+numdec(spec.cutpow_nh_err[0],2)+e+s+numdec(spec.cutpow_chisq,2)+' / '+ntostr(spec.cutpow_dof)+' = '+numdec(spec.cutpow_chisq/spec.cutpow_dof,2)+$
  ;   s+numdec(spec.band_alpha,2)+p+numdec(spec.band_alpha_err[1],2)+m+numdec(spec.band_alpha_err[0],2)+e+s+numdec(spec.band_e0,2)+p+numdec(spec.band_e0_err[1],2)+m+numdec(spec.band_e0_err[0],2)+e+s+numdec(spec.band_beta,2)+p+numdec(spec.band_beta_err[1],2)+m+numdec(spec.band_beta_err[0],2)+e+s+numdec(spec.band_nh,2)+p+numdec(spec.band_nh_err[1],2)+m+numdec(spec.band_nh_err[0],2)+e+s+numdec(spec.band_chisq,2)+' / '+ntostr(spec.band_dof)+' = '+numdec(spec.band_chisq/spec.band_dof,2)+s
  
  return
end 

pro plot_nh_fits,time,spec,ps=ps
  
  if keyword_set(ps) then begplot,name='xrt_spec_model_nh.ps',/color,/land
;  plot,[100,1e6],[0,0.5],/nodata,/xlog,xtitle='Time of SED (s)',ytitle='NH'
  n=n_elements(spec)
  color=[!red,!blue,!green,!orange,!magenta,!cyan]
  plotsym,0,1,/fill
  psym=[2,4,5,6,7,8]
  model=['Pow','BknPow','CutoffPow','Band']
  
  erase
  multiplot2,[1,4],/init
  for i=0,3 do begin
     multiplot2
     xtitle='' & ytitle=''
     if i eq 1 then ytitle='nH x 10!U22!N cm!U-2!N'
     if i eq 3 then xtitle='Time of SED (s)'

     plot,[100,1e6],[0,0.4],/nodata,/xlog,xtitle=xtitle,ytitle=ytitle
     case i of
        0: begin
           nh=spec.pow_nh
           nherr=spec.pow_nh_err
        end
        1: begin
           nh=spec.bknpow_nh
           nherr=spec.bknpow_nh_err
        end
        2: begin
           nh=spec.cutpow_nh
           nherr=spec.cutpow_nh_err
        end
        3: begin
           nh=spec.band_nh
           nherr=spec.band_nh_err
        end
     endcase 
     oplot,time,nh,psym=psym[i],color=color[i]
     for j=0,n-1 do begin
        oplot,[spec[j].tmin,spec[j].tmax],[nh[j],nh[j]],color=color[i]
        oplot,[time[j],time[j]],[nh[j]-nherr[0,j],nh[j]+nherr[1,j]],color=color[i]
     endfor 
     meannh=mean(nh)
     print,meannh
     oplot,[100,1e6],[meannh,meannh],line=1
     legend,model[i],box=0,textcolor=color[i]
  endfor 
  multiplot,/reset,/default
  if keyword_set(ps) then endplot
stop
  return
end 

pro read_time_specfit,specstr,dir=dir,datfiles=datfiles
  
  if n_params() eq 0 then begin
     print,'syntax - read_specfit_wtpu,specstr,dir=dir'
     return
  endif 
  
  d2=dblarr(2)
  specstr=create_struct('seg',0,$
                        'mode','',$
                        'nev',0L,$
                        'z',0.,$
                        'nhgal',0d,$
                        'pow_nh',0d,$
                        'pow_nh_err',d2,$
                        'pow_pow',0d,$
                        'pow_pow_err',d2,$
                        'pow_norm',0d,$
                        'pow_norm_err',d2,$
                        'pow_chisq',0d,$
                        'pow_dof',0L,$
                        'bknpow_nh',0d,$
                        'bknpow_nh_err',d2,$
                        'bknpow_pow1',0d,$
                        'bknpow_pow1_err',d2,$
                        'bknpow_ebreak',0d,$
                        'bknpow_ebreak_err',d2,$
                        'bknpow_pow2',0d,$
                        'bknpow_pow2_err',d2,$
                        'bknpow_norm',0d,$
                        'bknpow_norm_err',d2,$
                        'bknpow_chisq',0d,$
                        'bknpow_dof',0L,$
                        'cutpow_nh',0d,$
                        'cutpow_nh_err',d2,$
                        'cutpow_pow',0d,$
                        'cutpow_pow_err',d2,$
                        'cutpow_ecut',0d,$
                        'cutpow_ecut_err',d2,$
                        'cutpow_norm',0d,$
                        'cutpow_norm_err',d2,$
                        'cutpow_chisq',0d,$
                        'cutpow_dof',0L,$
                        'band_nh',0d,$
                        'band_nh_err',d2,$
                        'band_alpha',0d,$
                        'band_alpha_err',d2,$
                        'band_e0',0d,$
                        'band_e0_err',d2,$
                        'band_beta',0d,$
                        'band_beta_err',d2,$
                        'band_norm',0d,$
                        'band_norm_err',d2,$
                        'band_chisq',0d,$
                        'band_dof',0L,$
                        'exptime',0d,$
                        'tmin',0d,$
                        'tmax',0d,$
                        'regime','')
  
;  if n_elements(dir) eq 0 then dirs='spec/' else 
  dirs='./'
;  if n_elements(datfiles) eq 0 then datfiles=file_search(dirs+'seg*dat')

  datfiles=['seg1*_Pow.dat','seg1*_Bknpow.dat','seg1*_Cutoff.dat','seg1*_Band.dat']
  datfiles=file_search(datfiles)
  n=n_elements(datfiles)  
  
;  specstr=replicate(specstr,n)
  pix=strarr(n)
  seg=1
  
  for i=0,n-1 do begin
     
     file=datfiles[i]
;     file='seg'+ntostr(seg)+'wt_Pow.dat'
;     if not exist(file) then file='seg'+ntostr(seg)+'pc_Pow.dat'
     
     mpos=strpos(file,'_Pow')
     md=strmid(file,mpos-2,2)
     specstr.mode=md
     
     if exist(file) then begin 
        openr,lun,file,/get_lun
        line=readline(lun)
        
        case i of 
           0: begin
              specstr.pow_nh_err=[line[1],line[2]]
              line=readline(lun)
              specstr.pow_pow_err=[line[1],line[2]]
              line=readline(lun)
              specstr.pow_norm_err=[line[1],line[2]]
           end 
           1: begin
              specstr.bknpow_nh_err=[line[1],line[2]]
              line=readline(lun)
              specstr.bknpow_pow1_err=[line[1],line[2]]
              line=readline(lun)
              specstr.bknpow_ebreak_err=[line[1],line[2]]
              line=readline(lun)
              specstr.bknpow_pow2_err=[line[1],line[2]]
              line=readline(lun)
              specstr.bknpow_norm_err=[line[1],line[2]]
           end 
           2 : begin
              specstr.cutpow_nh_err=[line[1],line[2]]
              line=readline(lun)
              specstr.cutpow_pow_err=[line[1],line[2]]
              line=readline(lun)
              specstr.cutpow_ecut_err=[line[1],line[2]]
              line=readline(lun)
              specstr.cutpow_norm_err=[line[1],line[2]]
           end 
           3 : begin
              specstr.band_nh_err=[line[1],line[2]]
              line=readline(lun)
              specstr.band_alpha_err=[line[1],line[2]]
              line=readline(lun)
              specstr.band_e0_err=[line[1],line[2]]
              line=readline(lun)
              specstr.band_beta_err=[line[1],line[2]]
              line=readline(lun)
              specstr.band_norm_err=[line[1],line[2]]
           end 
        endcase
        line=readline(lun)

        specstr.nhgal=line[1]
        
        line=readline(lun)
        case i of 
           0: specstr.pow_nh=line[1]
           1: specstr.bknpow_nh=line[1]
           2: specstr.cutpow_nh=line[1]
           3: specstr.band_nh=line[1]
        endcase 
        
        line=readline(lun)
        if line[0] eq 'z' then begin
           specstr.z=line[1]
           line=readline(lun)
        endif 

        case i of 
           0: begin
              specstr.pow_pow=line[1]
              line=readline(lun)
              specstr.pow_norm=line[1]
              line=readline(lun)
              specstr.pow_chisq=line[1]
              line=readline(lun)
              specstr.pow_dof=line[1]
              specstr.pow_norm_err=abs(specstr.pow_norm-specstr.pow_norm_err)
              specstr.pow_pow_err=abs(specstr.pow_pow-specstr.pow_pow_err)
              specstr.pow_nh_err=abs(specstr.pow_nh-specstr.pow_nh_err)
           end 
           1: begin
              specstr.bknpow_pow1=line[1]
              line=readline(lun)
              specstr.bknpow_ebreak=line[1]
              line=readline(lun)
              specstr.bknpow_pow2=line[1]
              line=readline(lun)
              specstr.bknpow_norm=line[1]
              line=readline(lun)
              specstr.bknpow_chisq=line[1]
              line=readline(lun)
              specstr.bknpow_dof=line[1]
              specstr.bknpow_norm_err=abs(specstr.bknpow_norm-specstr.bknpow_norm_err)
              specstr.bknpow_pow1_err=abs(specstr.bknpow_pow1-specstr.bknpow_pow1_err)
              specstr.bknpow_pow2_err=abs(specstr.bknpow_pow2-specstr.bknpow_pow2_err)
              specstr.bknpow_ebreak_err=abs(specstr.bknpow_ebreak-specstr.bknpow_ebreak_err)
              specstr.bknpow_nh_err=abs(specstr.bknpow_nh-specstr.bknpow_nh_err)
           end 
           2 : begin
              specstr.cutpow_pow=line[1]
              line=readline(lun)
              specstr.cutpow_ecut=line[1]
              line=readline(lun)
              specstr.cutpow_norm=line[1]
              line=readline(lun)
              specstr.cutpow_chisq=line[1]
              line=readline(lun)
              specstr.cutpow_dof=line[1]
              specstr.cutpow_norm_err=abs(specstr.cutpow_norm-specstr.cutpow_norm_err)
              specstr.cutpow_pow_err=abs(specstr.cutpow_pow-specstr.cutpow_pow_err)
              specstr.cutpow_ecut_err=abs(specstr.cutpow_ecut-specstr.cutpow_ecut_err)
              specstr.cutpow_nh_err=abs(specstr.cutpow_nh-specstr.cutpow_nh_err)
           end 
           3 : begin
              specstr.band_alpha=line[1]
              line=readline(lun)
              specstr.band_e0=line[1]
              line=readline(lun)
              specstr.band_beta=line[1]
              line=readline(lun)
              specstr.band_norm=line[1]
              line=readline(lun)
              specstr.band_chisq=line[1]
              line=readline(lun)
              specstr.band_dof=line[1]
              specstr.band_norm_err=abs(specstr.band_norm-specstr.band_norm_err)
              specstr.band_alpha_err=abs(specstr.band_alpha-specstr.band_alpha_err)
              specstr.band_beta_err=abs(specstr.band_beta-specstr.band_beta_err)
              specstr.band_e0_err=abs(specstr.band_e0-specstr.band_e0_err)
              specstr.band_nh_err=abs(specstr.band_nh-specstr.band_nh_err)
           end 
        endcase

;           line=readline(lun)
;           specstr.flux=line[1]

;           line=readline(lun)
;           specstr.flux_err=[line[1],line[2]]

;           line=readline(lun)
;           specstr.unabs_flux=line[1]

;           line=readline(lun)
;           specstr.rate=line[1]
        
;           line=readline(lun)
;           specstr.model_rate=line[1]
        
        line=readline(lun)
        specstr.nev=line[1]
;        endif else begin
;           specstr.nev=line[1]
;        endelse 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'exptime' then begin
              specstr.exptime=line[1]
           endif 
        endif 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'tmin' then begin
              specstr.tmin=line[1]
           endif 
        endif 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'tmax' then begin
              specstr.tmax=line[1]
           endif 
        endif 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'regime' then begin
              specstr.regime=line[1];+' '+line[2]
           endif 
        endif 
        
        specstr.seg=seg
        
        close,lun
        free_lun,lun
        
;        specstr[i].flux_err=abs(specstr[i].flux-specstr[i].flux_err)
     endif 
  endfor 
  
;  mwrfits,'combined_spec_outputs.fits',specstr,/create
;  specstr=specstr[sort(specstr.seg)]

  return
end 

pro run_xspec,j,z,mode,phaname,nh,regime,trigtime,tmin,tmax,evfile=evfile
  ;;xspec script
  ;; models to fit
  ;;;;  pow,pow+bb,pow+pow,cutoffpow,band
  ;; loop through models and change output names or append to output file?
  
  
  ;;important parameters
  ;;nHgal & nH for all
  for mo=0,3 do begin 
     parname=['nHgal','nH']
     parname_err=['nhgalerr','nH_err']
     guess=[1,1.]
     np=2
     if z eq 0 then begin 
        model='mo wabs*wabs'
        zz=0
     endif else begin
        model='mo wabs*zwabs'
        parname=[parname,'z']
        parname_err=['nhgalerr','nH_err','z_err']
        guess=[guess,1]
        zz=3
     endelse 
     
     case mo of
        0: begin
           momo='Pow'
           model=model+'*pow'
           parname=[parname,'PhInd','norm']
           parname_err=[parname_err,'Pow_err','norm_err']
           guess=[guess,1,1]
        end 
        1: begin
           momo='Bknpow'
           model=model+'(bknpow)'
           parname=[parname,'PhInd1','BreakE','PhInd2','norm']
           parname_err=[parname_err,'PhInd1_err','BreakE_err','PhInd2_err','norm_err']
           guess=[guess,1,5,2,1]
        end
        
           
;        1: begin
;           momo='Pow_BB'
;           model=model+'(pow+bb)'
;           parname=[parname,'PhInd','norm','kT','norm2']
;           parname_err=[parname_err,'Pow_err','norm_err','kT_err','norm_err2']
;           guess=[guess,1,1,0.1,1]
;        end 
;        2: begin
;           momo='Pow_Pow'
;           model=model+'(pow+pow)'
;           parname=[parname,'PhInd','norm','PhInd2','norm2']
;           parname_err=[parname_err,'Pow_err','norm_err','Pow_err2','norm_err2']
;           guess=[guess,1,1,1,1]
;        end 
        2: begin 
           momo='Cutoff'
           model=model+'*cut'
           parname=[parname,'PhInd','HighECut','norm']
           parname_err=[parname_err,'Pow_err','HighECut_err','norm_err']
           guess=[guess,1,1,1]
        end 
        3: begin
           momo='Band'
           model=model+'*grbm'
           parname=[parname,'alpha','beta','tem','norm']
           parname_err=[parname_err,'alpha_err','beta_err','tem_err','norm_err']
           guess=[guess,-1,-2,5,1]
        end 
     endcase
     print,momo
     np=n_elements(parname)
     impar=indgen(np)
     imparerr=[1,indgen(np-3)+zz]
     
;     print,impar
;     print,imparerr
;     print,parname,parname_err
     
     xbfile='xspec'+ntostr(j+1)+'.batch'
     openw,lun,xbfile,/get_lun

     printf,lun,'data '+phaname
     printf,lun,'ignore 0.-0.3'
     printf,lun,'ignore bad'

     printf,lun,'query yes'
     
     printf,lun,model
     for b=0,np-1 do printf,lun,ntostr(guess[b])
     
     npar=n_elements(impar)
     np0=ntostr(npar)
     printf,lun
     nimpar=n_elements(impar)
     nimparerr=n_elements(imparerr)
     if z eq 0 then begin 
        printf,lun,'newpar 1 '+ntostr(nh)+' 0 '
     endif else begin 
        printf,lun,'newpar 1 '+ntostr(nh)+' 0 '
        printf,lun,'newpar 3 '+ntostr(z)
     endelse 
     printf,lun
     printf,lun
     ;;sets nH guess & lower limit to galactic
     printf,lun,'fit 1000'
     printf,lun,'fit 1000'
     printf,lun,'fit 1000'
     printf,lun,'cpd /ps'
     printf,lun,'setplot en'
     printf,lun,'plot ld res'
     printf,lun,'exec mv pgplot.ps '+phaname+'_'+momo+'.ps'
     printf,lun
     ;;tcl stuff
     datfile='seg'+ntostr(j+1)+mode+'_'+momo+'.dat'
     printf,lun,'set fileid [open '+datfile+' w]'
           ;;;errors
     np=ntostr(npar+1)
     for p=0,nimparerr-1 do begin 
        pp=ntostr(imparerr[p]+1)
        printf,lun,'error stop 100,,maximum 20.0 '+pp
        printf,lun,'tclout error '+pp
        printf,lun,'set err'+pp+' [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $err'+pp+' { } cerr'+pp+''
        printf,lun,'set lerr'+pp+' [split $cerr'+pp+']'
        printf,lun,'puts $fileid "'+parname_err[imparerr[p]]+' [lindex $lerr'+pp+' 0] [lindex $lerr'+pp+' 1]"'
     endfor 
     
                ;;; fit params
     for p=0,n_elements(impar)-1 do begin 
        pp=ntostr(impar[p]+1)
        printf,lun,'tclout param '+pp
        printf,lun,'set par'+pp+' [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $par'+pp+' { } cpar'+pp
        printf,lun,'set lpar'+pp+' [split $cpar'+pp+']'

        printf,lun,'puts $fileid "'+parname[impar[p]]+' [lindex $lpar'+pp+' 0]"'
     endfor 
     
                ;;;stats
     printf,lun,'tclout stat'
     printf,lun,'set stt [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $stt { } cstt'
     printf,lun,'set lstt [split $cstt]'
     printf,lun,'puts $fileid "chisq [lindex $lstt 0]"'
     
     printf,lun,'tclout dof'
     printf,lun,'set df [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $df { } cdf'
     printf,lun,'set ldf [split $cdf]'
     printf,lun,'puts $fileid "dof [lindex $ldf 0]"'

                ;;;flux
;     printf,lun,'flux 0.3 10. err 10000 90'
;     printf,lun,'tclout flux 1'
;     printf,lun,'set flx [string trim $xspec_tclout]'
;     printf,lun,'regsub -all { +} $flx { } cflx'
;     printf,lun,'set lflx [split $cflx]'

;     printf,lun,'puts $fileid "flux [lindex $lflx 0]"'
;     printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2]"'
     
                ;;;unabs flux
;     printf,lun,'newpar 1 0 1 0 0 1e4 1e4'
;     if z ne 0 then printf,lun,'newpar 2 0 1 0 0 1e4 1e4'
;     printf,lun,'flux 0.3 10.0 err 10000 90'
;     printf,lun,'tclout flux 1'
;     printf,lun,'set flx [string trim $xspec_tclout]'
;     printf,lun,'regsub -all { +} $flx { } cflx'
;     printf,lun,'set lflx [split $cflx]'

;     printf,lun,'puts $fileid "unabs_flux [lindex $lflx 0]"'
     
                ;;;rate
;     printf,lun,'show rate'
;     printf,lun,'tclout rate 1'
;     printf,lun,'set rte [string trim $xspec_tclout]'
;     printf,lun,'regsub -all { +} $rte { } crte'
;     printf,lun,'set lrte [split $crte]'
;     printf,lun,'puts $fileid "rate [lindex $lrte 0]"'
;     printf,lun,'puts $fileid "model_rate [lindex $crte 2]"'
     printf,lun,'close $fileid'
     printf,lun,'exit'
     printf,lun

     close,lun
     free_lun,lun

     print,'XSPEC'
     spawn,'xspec - '+xbfile+' > xspec'+ntostr(j+1)+'.out'
     
;  datfile='seg'+ntostr(j+1)+mode+'.dat'
     
     if n_elements(evfile) eq 0 then begin 
        evfile='seg'+ntostr(j+1)+'wt.evt'
        if not exist(evfile) then evfile='seg'+ntostr(j+1)+'pc.evt' 
     endif 
     ev=mrdfits(evfile[0],1,hdr)
     exp_time=sxpar(hdr,'ONTIME')
     n_ev=n_elements(ev)
     tmin=min(ev.time)-trigtime
     tmax=max(ev.time)-trigtime
     
     openu,lun,datfile,/get_lun,/append
     printf,lun,'N_ev '+ntostr(n_ev)
     printf,lun,'exptime '+ntostr(exp_time)
     printf,lun,'tmin '+ntostr(tmin)
     printf,lun,'tmax '+ntostr(tmax)
     printf,lun,'regime '+regime
;  if corr ge 0 then printf,lun,'pu_corr '+ntostr(corrfact[corr])
     close,lun
     free_lun,lun
     
  endfor   

  return
end 

pro fit_sed_xrt,results=results,ps=ps
  
  cd,'~/Desktop/GRB080319B/xrt_spec_fits'
  j=0
  z=0.937
  trigtime=227599969.0
  seddir=['sed150','sed350','sed600','sed5856','sed1.17e4','sed7.89e4','sed2.2e5']
  time=[150,350,600,5856,1.17e4,7.89e4,2.2e5]
  mode=['wt','wt','wt','pc','pc','pc','pc']
  tmin=[65,320,544,4969,10704,16496,75632]
  tmax=[192,400,912,6929,12864,377232,752416]
  srcra=217.91954
  srcdec=36.30256
  
  ;;nH - get galactic nH
  spawn,'nh 2000. '+ntostr(srcra)+' '+ntostr(srcdec)+' > nhgal.out'
  readcol,'nhgal.out',blah1,format='(a)',delim='&'
  blah1=blah1[n_elements(blah1)-1]
  blah1=str_sep(blah1,' ')
  nh=blah1[n_elements(blah1)-1]*1e-22
  
  for i=0,5 do begin
     cd,seddir[i]
     print,seddir[i]
;     spawn,'rm *ps'
     evfile=file_search('spec*evt')
;     ev=mrdfits('spec*evt',1,hdr)
;     n_ev=n_elements(ev)
;     exp_time=sxpar(hdr,'ONTIME')
     regime=seddir[i]
     phaname=file_search('spec*.pha.grpmin*0')
     
     if not keyword_set(results) then $
        run_xspec,j,z,mode[i],phaname,nh,regime,trigtime,tmin[i],tmax[i],evfile=evfile else begin 
        read_time_specfit,spec1
        if i gt 0 then begin
           concat_structs,spec0,spec1,spec
           spec0=spec
        endif else spec0=spec1
     endelse

     cd,'..'
  endfor
  
  plot_nh_fits,time,spec,ps=ps
  print_sed_table,spec
  
  stop
  return
end 
