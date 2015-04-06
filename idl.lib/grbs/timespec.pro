pro timespec,specfiles

;;; good for spectral analysis of spectra taken from Phil's pages

  dowt=1
;  respfile='/Volumes/Apps_and_Docs/CALDB/data/swift/xrt/cpf/rmf/'+
  respfile=['swxwt0to2s6_20070901v012.rmf','swxpc0to12s6_20070901v012.rmf']
  cc=''
  for j=0,n_elements(specfiles)-1 do begin
     phaname=specfiles[j]
     wt=strpos(phaname,'wt')
     pc=strpos(phaname,'pc')
     dowt=0 & dopc=0
     if wt ne -1 then dowt=1
     if pc ne -1 then dopc=1
     mp=max([wt,pc],k)
     phaname2=strmid(phaname,0,mp+2)+'_20.pi'

     grp='20'
     grppha='grppha infile="'+phaname+'" outfile="'+phaname2+'" chatter=0 comm="chkey RESPFILE "'+respfile[k]+'" & group min '+grp+' & exit" clobber=yes > grppha.out'
     print,grppha
     spawn,grppha

     if dowt or dopc then begin 
        ;;xspec script
        xbfile='xspec'+ntostr(j+1)+cc+'.batch'
        openw,lun,xbfile,/get_lun
        printf,lun,'data '+phaname2
        printf,lun,'ignore 0.-0.3'
        printf,lun,'ignore bad'

        printf,lun,'query yes'
;     if grp eq '10' then printf,lun,'statistic cstat'
;        printf,lun,'mo wabs*wabs*pow'
        printf,lun,'mo phabs(bbody+pow)'
        printf,lun,'1.00001'
        printf,lun,'1.00002'
        printf,lun,'1.00003'
        printf,lun,'1.00004'
        printf,lun,'1.00005'
        npar=5
        impar=[0,1,2,3,4]
        imparerr=[0,1,2,3,4]
        parname=['nH','kT','norm1','Pow','norm2']
        parname_err=['nH_err','kT_err','norm1_err','Pow_err','norm2_err']
        np0=ntostr(npar)
        printf,lun              ;,'1.00002'
        printf,lun
     endif 
     printf,lun
     printf,lun,'fit 1000'
     printf,lun,'fit 1000'
     printf,lun,'fit 1000'
     printf,lun,'cpd /ps'
     printf,lun,'setplot en'
     printf,lun,'plot ld res'
     printf,lun,'exec mv pgplot.ps '+phaname+cc+'.bbpow.ps'
     printf,lun
     ;;tcl stuff
     datfile='specfit'+ntostr(j+1)+cc+'.dat'
     printf,lun,'set fileid [open '+datfile+' w]'
     ;;;errors
     np=ntostr(n_elements(imparerr))
           ;;;0.0027=99.73%  -  3 sigma
           ;;;0.1=90% 
;           delchi0=chisqr_cvf(0.1,n_elements(imparerr)-1)
;           delchi0=chisqr_cvf(0.1,2)  ;;; always only 2 parameters of interest (NH & pow)
     delchi0=chisqr_cvf(0.1,1) ;;; 1 par of interest when fitting CRs 90%
;     delchi0=chisqr_cvf(0.0455,1) ;;; 1 par of interest when fitting CRs 2 sigma
     print,delchi0
     for p=0,np-1 do begin 
        pp=ntostr(imparerr[p]+1)
        printf,lun,'error stop 100,,maximum 20.0 '+ntostr(delchi0)+' '+pp
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
     
                           ;;;rate
     printf,lun,'show rate'
     printf,lun,'tclout rate 1'
     printf,lun,'set rte [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $rte { } crte'
     printf,lun,'set lrte [split $crte]'
     printf,lun,'puts $fileid "rate [lindex $lrte 0]"' 
     printf,lun,'puts $fileid "model_rate [lindex $crte 2]"'

                ;;;flux
;           printf,lun,'flux 0.3 10. err 10000 90'
     printf,lun,'flux 0.3 10. err 10000 95.45'
     printf,lun,'tclout flux 1'
     printf,lun,'set flx [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $flx { } cflx'
     printf,lun,'set lflx [split $cflx]'
     printf,lun,'tclout flux 2'
     printf,lun,'set flx2 [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $flx2 { } cflx2'
     printf,lun,'set lflx2 [split $cflx2]'
     printf,lun,'puts $fileid "flux [lindex $lflx 0] [lindex $lflx2 0]"'
     printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2] [lindex $lflx2 1] [lindex $lflx2 2]"'
     
                ;;;unabs flux
     printf,lun,'newpar 1 0 1 0 0 1e4 1e4'
;     printf,lun,'newpar 2 0 1 0 0 1e4 1e4' ;;;should not be z specific
;           printf,lun,'flux 0.3 10.0 err 10000 90'
     printf,lun,'flux 0.3 10.0 err 10000 95.45'
     printf,lun,'tclout flux 1'
     printf,lun,'set flx [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $flx { } cflx'
     printf,lun,'set lflx [split $cflx]'
     printf,lun,'puts $fileid "unabs_flux [lindex $lflx 0]"'
            
;           printf,lun,'puts $fileid "unabs_flux_err [lindex $lflx 1] [lindex $lflx 2]"'
    
     printf,lun,'close $fileid'
     printf,lun,'exit'
     printf,lun

     close,lun
     free_lun,lun

     print,'XSPEC'
     spawn,'xspec - '+xbfile+' > xspec'+ntostr(j+1)+cc+'.out'
     datfile='seg'+ntostr(j+1)+cc+'.dat'
     openu,lun,datfile,/get_lun,/append
     printf,lun,'N_ev '+ntostrarr(n_ev)
     printf,lun,'exptime '+ntostrarr(exp_time)
     if corr ge 0 then printf,lun,'pu_corr '+ntostr(corrfact[corr])
     close,lun
     free_lun,lun
  endfor 

  return
end 
