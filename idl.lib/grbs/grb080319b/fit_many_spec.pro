pro write_time_spec_table
  
  nctsperbin=10000
  spec=mrdfits('fitmanyspec'+ntostr(nctsperbin)+'.fits',1)
  
  header='Tmin,Tmax,PhInd,PhInd_errlow,PhInd_errhigh,nH,nH_errlow,nH_errhigh,norm,norm_errlow,norm_errhigh,Chisq,dof'
  writecol,'xrt_spec_fits.dat',numdec(spec.tmin,1),numdec(spec.tmax,1),numdec(spec.pow,2),'-'+numdec(spec.pow_err[0],2),'+'+numdec(spec.pow_err[1],2),numdec(spec.nh,2),'-'+numdec(spec.nh_err[0],2),'+'+numdec(spec.nh_err[1],2),numdec(spec.norm,2),'-'+numdec(spec.norm_err[0],2),'+'+numdec(spec.norm_err[1],2),numdec(spec.chisq,1),numdec(spec.dof,1),delim=',',header=header
  
  return
end 

pro plot_time_spec_results,spec,ps=ps
  erase
  trigtime=227599969.0
  nctsperbin=10000
  read_time_specfit,spec,trigtime,dir='~/Desktop/GRB080319B/manyspec'+ntostr(nctsperbin)+'/'

  if keyword_set(ps) then begplot,name='manyspec'+ntostr(nctsperbin)+'.ps',/land
  multiplot2,[1,2],/init
  t=(spec.tmax-spec.tmin)/2.+spec.tmin
  multiplot2
  plot,t,spec.pow,psym=3,ytitle='PhInd',/yno,yrange=[1.5,2],/xlog,xrange=[10,max(spec.tmax)]
  for i=0,n_elements(spec)-1 do begin

     oplot,[spec[i].tmin,spec[i].tmax],[spec[i].pow,spec[i].pow]
     oplot,[t[i],t[i]],[spec[i].pow-spec[i].pow_err[0],spec[i].pow+spec[i].pow_err[1]]
  endfor 
  
  multiplot2
  plot,t,spec.nh,psym=3,xtitle='Time since BAT trigger (s)',ytitle='nH/10!U22!N',/yno,yrange=[0,0.4],/xlog,xrange=[10,max(spec.tmax)]
  for i=0,n_elements(spec)-1 do begin

     oplot,[spec[i].tmin,spec[i].tmax],[spec[i].nh,spec[i].nh]
     oplot,[t[i],t[i]],[spec[i].nh-spec[i].nh_err[0],spec[i].nh+spec[i].nh_err[1]]
  endfor 
  multiplot2,/reset,/default
  
  mwrfits,spec,'fitmanyspec'+ntostr(nctsperbin)+'.fits',/create
  if keyword_set(ps) then endplot
  
  stop
  return
end 
  

pro read_time_specfit,specstr,trigtime,dir=dir,datfiles=datfiles
  
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
  
  if n_elements(dir) eq 0 then dirs='spec/' else dirs=dir+'/'
  if n_elements(datfiles) eq 0 then datfiles=file_search(dirs+'seg*dat')
  n=n_elements(datfiles)
  
;  evfiles=file_search(dirs+'seg*.evt')
;  bgpos=strpos(evfiles,'bg')
;  wsrc=where(bgpos eq -1)
;  evfiles=evfiles[wsrc]

  specstr=replicate(specstr,n)
  pix=strarr(n)
  
  for i=0,n-1 do begin
;     file=datfiles[i]
;     segpos=strpos(file,'seg')
;     seg=strmid(file,segpos+3,2)
;     spos=strpos(seg,'w')
;     wpos=where(spos ne -1,nwpos)
;     if nwpos gt 0 then seg=strmid(seg,0,1)
     
     seg=i+1
     file='seg'+ntostr(seg)+'wt_Pow.dat'
     if not exist(file) then file='seg'+ntostr(seg)+'pc_Pow.dat'
     
     mpos=strpos(file,'_Pow')
     md=strmid(file,mpos-2,2)
     specstr[i].mode=md
     
;     evfile='seg'+ntostr(seg)+'wt.evt'
;     if not exist(evfile) then evfile='seg'+ntostr(seg)+'pc.evt' 
;     ev=mrdfits(evfile,1)
;     specstr[i].tmin=min(ev.time)-trigtime
;     specstr[i].tmax=max(ev.time)-trigtime
     
     if exist(file) then begin 
        openr,lun,file,/get_lun
        line=readline(lun)
        
        if line[0] ne 'N_ev' then begin 
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
           specstr[i].flux=line[1]

           line=readline(lun)
           specstr[i].flux_err=[line[1],line[2]]

           line=readline(lun)
           specstr[i].unabs_flux=line[1]

           line=readline(lun)
           specstr[i].rate=line[1]
           
           line=readline(lun)
           specstr[i].model_rate=line[1]
           
           line=readline(lun)
           specstr[i].nev=line[1]
        endif else begin 
           specstr[i].nev=line[1]
        endelse 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'exptime' then begin
              specstr[i].exptime=line[1]
           endif 
        endif 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'tmin' then begin
              specstr[i].tmin=line[1]
           endif 
        endif 
        
        if not eof(lun) then begin
           line=readline(lun)
           if strtrim(line[0],2) eq 'tmax' then begin
              specstr[i].tmax=line[1]
           endif 
        endif 
        
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
     endif 
  endfor 
  
;  mwrfits,'combined_spec_outputs.fits',specstr,/create
;  specstr=specstr[sort(specstr.seg)]

  return
end 

pro merge_expo_infiles,attfile,hkfile
  
  ;;;sum attfile & hkfile
  attfiles=file_search('00*/auxil/*sat.fits*')
  attfile='merged_satfiles.fits'
  hkfiles=file_search('*/*hdtc.hk*')
  hkfile='merged_hkfiles.fits'
  fmerge='ftmerge '+ntostrarr(attfiles,',')+' '+attfile+' clobber=yes'
  print,fmerge
  spawn,fmerge
  fmerge='ftmerge '+ntostrarr(hkfiles,',')+' '+hkfile+' clobber=yes'
  print,fmerge
  spawn,fmerge
  
  hk=mrdfits(hkfile,1,hkhdr)
  tstart=min(hk.time)
  tstop=max(hk.time)
  
  spawn,'fparkey '+sigfig(tstop,15)+' '+hkfile+'+0 tstop'
  spawn,'fparkey '+sigfig(tstop,15)+' '+hkfile+'+1 tstop'
  spawn,'fparkey '+sigfig(tstop-tstart,15)+' '+hkfile+'+1 telapse'
  
  sdate=round(met2date_judy(tstop))
  ydn2md,sdate[0],sdate[1],m,d
  if m lt 10 then m='0'+ntostr(m) else m=ntostr(m)
  if d lt 10 then d='0'+ntostr(d) else d=ntostr(d)
  if sdate[2] lt 10 then hr='0'+ntostr(sdate[2]) else hr=ntostr(sdate[2])
  if sdate[3] lt 10 then mn='0'+ntostr(sdate[3]) else mn=ntostr(sdate[3])
  if sdate[4] lt 10 then sc='0'+ntostr(sdate[4]) else sc=ntostr(sdate[4])
  dateend=ntostr(sdate[0])+'-'+m+'-'+d+'T'+hr+':'+mn+':'+sc
  spawn,'fparkey '+sigfig(dateend,15)+' '+hkfile+'+1 date-end'
  
  att=mrdfits(attfile,1,atthdr)
  tstop=max(att.time)
  spawn,'fparkey '+sigfig(tstop,15)+' '+attfile+'+0 tstop'
  spawn,'fparkey '+sigfig(tstop,15)+' '+attfile+'+1 tstop'
  spawn,'fparkey '+sigfig(tstop,15)+' '+attfile+'+2 tstop'
  
  return
end 

pro run_xspec,j,z,mode,phaname,nh,n_ev,exp_time,regime,trigtime,tmin,tmax
  ;;xspec script
  ;; models to fit
  ;;;;  pow,pow+bb,pow+pow,cutoffpow,band
  ;; loop through models and change output names or append to output file?
  
  
  ;;important parameters
  ;;nHgal & nH for all
;;;ONLY DOING POW FOR NOW!!!!!!!!!!!!!  
  for mo=0,0 do begin 
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
           momo='Pow_BB'
           model=model+'(pow+bb)'
           parname=[parname,'PhInd','norm','kT','norm2']
           parname_err=[parname_err,'Pow_err','norm_err','kT_err','norm_err2']
           guess=[guess,1,1,0.1,1]
        end 
        2: begin
           momo='Pow_Pow'
           model=model+'(pow+pow)'
           parname=[parname,'PhInd','norm','PhInd2','norm2']
           parname_err=[parname_err,'Pow_err','norm_err','Pow_err2','norm_err2']
           guess=[guess,1,1,1,1]
        end 
        3: begin 
           momo='Cutoff'
           model=model+'*cut'
           parname=[parname,'PhInd','HighECut','norm']
           parname_err=[parname_err,'Pow_err','HighECut_err','norm_err']
           guess=[guess,1,1,1]
        end 
        4: begin
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
     printf,lun,'flux 0.3 10. err 10000 90'
     printf,lun,'tclout flux 1'
     printf,lun,'set flx [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $flx { } cflx'
     printf,lun,'set lflx [split $cflx]'

     printf,lun,'puts $fileid "flux [lindex $lflx 0]"'
     printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2]"'
     
                ;;;unabs flux
     printf,lun,'newpar 1 0 1 0 0 1e4 1e4'
     if z ne 0 then printf,lun,'newpar 2 0 1 0 0 1e4 1e4'
     printf,lun,'flux 0.3 10.0 err 10000 90'
     printf,lun,'tclout flux 1'
     printf,lun,'set flx [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $flx { } cflx'
     printf,lun,'set lflx [split $cflx]'

     printf,lun,'puts $fileid "unabs_flux [lindex $lflx 0]"'
     
                ;;;rate
     printf,lun,'show rate'
     printf,lun,'tclout rate 1'
     printf,lun,'set rte [string trim $xspec_tclout]'
     printf,lun,'regsub -all { +} $rte { } crte'
     printf,lun,'set lrte [split $crte]'
     printf,lun,'puts $fileid "rate [lindex $lrte 0]"'
     printf,lun,'puts $fileid "model_rate [lindex $crte 2]"'
     printf,lun,'close $fileid'
     printf,lun,'exit'
     printf,lun

     close,lun
     free_lun,lun

     print,'XSPEC'
     spawn,'xspec - '+xbfile+' > xspec'+ntostr(j+1)+'.out'
     
;  datfile='seg'+ntostr(j+1)+mode+'.dat'
     
     evfile='seg'+ntostr(j+1)+'wt.evt'
     if not exist(evfile) then evfile='seg'+ntostr(j+1)+'pc.evt' 
     ev=mrdfits(evfile,1)
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

pro read_parfile,srcra,srcdec,bgra,bgdec,trigtime,evfiles,skipgrb,lcfile=lcfile,nowrite=nowrite
  
  print,'Reading LCWRAP.PAR file'
  lcfile='lc_wrap3.par'
  if not exist(lcfile) then lcfile='lc_wrap.par'
  if not exist(lcfile) then begin
     print,'Need lc_wrap par file!!!'
     skipgrb=1
     stop
  endif else begin
     print,lcfile
     skipgrb=0
     openr,lun,lcfile,/get_lun
     par=strarr(6)
     for l=0,5 do begin
        line=readline(lun,delim=' ')
        par[l]=line[1]
     endfor 
     close,lun
     free_lun,lun
     readcol,lcfile,par2,format='(a)',delim='$'
     tmp=par2[0]  
     srcra=par[1]
     srcdec=par[2]
     bgra=par[3]
     bgdec=par[4]
     if bgra eq '' then begin
        bgrasep=str_sep(par2[3],' ')
        bgra=bgrasep[1]
     endif 
     if bgdec eq '' then begin
        bgdecsep=str_sep(par2[4],' ')
        bgdec=bgdecsep[1]
     endif 
     if strpos(srcra,':') ne -1 then begin 
        sra=str_sep(srcra,':')
        sdec=str_sep(srcdec,':')
        if sra[0] ne -1 then $
           hms2radec,sra[0],sra[1],sra[2],sdec[0],sdec[1],sdec[2],srcra,srcdec
     endif 
     if strpos(bgra,':') ne -1 then begin 
        bra=str_sep(bgra,':')                    
        bdec=str_sep(bgdec,':')
        if bra[0] ne -1 then $
           hms2radec,bra[0],bra[1],bra[2],bdec[0],bdec[1],bdec[2],bgra,bgdec
     endif
     if not keyword_set(nowrite) then begin 
        print,'Writing region files'
        write_regfile,'src.reg',srcra,srcdec,20
        write_regfile,'bg.reg',bgra,bgdec,40
        spawn,'cp src.reg '+specdir
        spawn,'cp bg.reg '+specdir
     endif 
     trigtime=par[5]
     evfiles=str_sep(tmp,' ')
     evfiles='../'+evfiles[1:*]
  endelse 
  return
end 

pro fit_many_spec,z=z,file=file,lcfile=lcfile,specdir=specdir,phil=phil
  
  ;;BURST SPECIFIC
  cd,'~/Desktop/GRB080319B/'
  z=0.937
  
  if n_elements(z) eq 0 then z=0.
  
  mn=0d
  mx=1d9
  mode=['wt','pc']
  
  nctsperbin=10000
  
  respfile='/bulk/pkg/caldb/data/swift/xrt/cpf/rmf/'+['swxwt0to2s6_20010101v010.rmf','swxpc0to12s6_20010101v010.rmf']
  if n_elements(specdir) eq 0 then specdir='manyspec'+ntostr(nctsperbin)
;  spawn,'rm -r manyspec'+ntostr(nctsperbin)+'/'
  if not exist(specdir) then spawn,'mkdir '+specdir
                                ; lc=lcout2fits(phil=phil)
  lcfile0='sw00306757000xwtw2posr.lc'
  lcfile='uncorr_wtlc.qdp'
  readcol,lcfile,times,halfbin,ctr,ctrerr,exp
  hdr=headfits(lcfile0)
  xrt_t0=sxpar(hdr,'TSTART')
  trigtime=227599969.0
  times=times+xrt_t0-trigtime
  type=intarr(n_elements(times))
  tstart=times-halfbin
  tstop=times+halfbin
  
;  lcfile0='sw00306757000xpcw2posr.lc'
;  lcfile='uncorr_pclc.qdp'
;  readcol,lcfile,pctimes,pchalfbin,pcctr,pcctrerr,pcexp
;  hdr=headfits(lcfile0)
;  xrt_t0=sxpar(hdr,'TSTART')
;  pctimes=pctimes+xrt_t0-trigtime
;  type=intarr(n_elements(pctimes))
;  pctstart=pctimes-pchalfbin
;  pctstop=pctimes+pchalfbin
;  npc=n_elements(pctime)
  
 ; readcol,'PC.qdp',pctimes,pct1,pct2,pcctr,pcerr
  lc=lcout2fits('correct_xrt_lc_rate.txt')
  pc=where(lc.type eq 1)
  pctstart=lc[pc].tstart
  pctstop=lc[pc].tstop
  pctimes=lc[pc].time
  pcctr=lc.src_rate
;  pctstart=pctimes+pct2
;  pctstop=pctimes+pct1
  npc=n_elements(pctimes)

  
  times=[times,pctimes]
  type=[type,replicate(1,npc)]
  tstart=[tstart,pctstart]
  tstop=[tstop,pctstop]
  ctr=[ctr,pcctr]
  
  read_parfile,srcra,srcdec,bgra,bgdec,trigtime,evfiles,skipgrb,lcfile=lcfile,/nowrite
  
;  attfiles=file_search('00*/auxil/*sat.fits*')
;  hkfiles=file_search('*/*hdtc.hk*')
;  attfile=attfiles[0]
;  hdfile=hkfiles[0]
  
  merge_expo_infiles,pcattfile,pchdfile
  wtattfile=file_search('00*/auxil/*sat.fits*')
  wthkfile=file_search('00*0-xrt/*hdtc.hk*')
  
  cd,specdir
  
  if not exist(evfiles[0]) then stop
  
  ;;nH - get galactic nH
  spawn,'nh 2000. '+ntostr(srcra)+' '+ntostr(srcdec)+' > nhgal.out'
  readcol,'nhgal.out',blah1,format='(a)',delim='&'
  blah1=blah1[n_elements(blah1)-1]
  blah1=str_sep(blah1,' ')
  nh=blah1[n_elements(blah1)-1]*1e-22
  
  ;;FIND PILE-UP PERIODS
  ;;;;WILL NEED TO ALTER NUMBERS ONCE WE SWITCH TO LOREDANA'S LC
  wtpu1=where(ctr gt 1200. and ctr lt 1400. and type eq 0,nwtpu1) 
  wtpu2=where(ctr gt 1000. and ctr le 1200.  and type eq 0,nwtpu2)
  wtpu3=where(ctr gt 800. and ctr le 1000.  and type eq 0,nwtpu3)
  wtpu4=where(ctr gt 400. and ctr le 800.  and type eq 0,nwtpu4)
  wtpu5=where(ctr gt 300. and ctr le 400.  and type eq 0,nwtpu5)
  wtpu6=where(ctr gt 200. and ctr le 300.  and type eq 0,nwtpu6)
  wtpu7=where(ctr lt 200 and type eq 0,nwtpu7)
  
;  pcpu1=where(ctr gt 4 and type eq 1,npcpu1)
;  pcpu2=where(ctr gt 0.6 and ctr lt 4 and type eq 1,npcpu2) 
;  pcpu3=where(ctr lt 0.6 and type eq 1,npcpu3)
  pcpu1=where(times gt 4000 and times lt 1e4 and type eq 1,npcpu1)
  pcpu2=where(times gt 1e4 and times lt 1.5e4 and type eq 1,npcpu2)
  pcpu3=where(times gt 1.5e4 and type eq 1,npcpu3)

  wwt=strpos(evfiles,'wt')
  wt=where(wwt ne -1,nwt)
  pc=where(wwt eq -1 and strpos(evfiles,'w3') eq -1,npc)
  pcstart=dblarr(npc) & pcstop=pcstart
  for i=0,npc-1 do begin 
     hdr=headfits(evfiles[pc[i]])
     pcstart[i]=sxpar(hdr,'TSTART')
     pcstop[i]=sxpar(hdr,'TSTOP')
  endfor 

  find_wt_srcdetpos,evfiles[wt[0]],bgra,bgdec,x_int,y_int,pa,wtbgra,wtbgdec
  bgwtreg='bgwt.reg'
;  write_regfile,bgwtreg,wtbgra,wtbgdec,40
  a2p=2.36
  roll=pa-90
  write_regfile,bgwtreg,wtbgra,wtbgdec,40*a2p,20*a2p,roll=roll-90,/box
  
  bgpcreg='bgpc.reg'
  write_regfile,bgpcreg,bgra,bgdec,40
  
  rad=20

  for k=0,1 do begin 
     case k of
        0: begin 
           ni=6
           ni0=0
           attfile=wtattfile
           hdfile=wthkfile
        end
        1: begin
           ni=9
;           ni0=9
           ni0=7
           attfile=pcattfile
           hdfile=pchdfile
        end
     endcase 
     for i=ni0,ni do begin
        plus=0.        
        case i of
           0: begin
              regime='WT 1200ctr<1400 cts/s'
              wpu=wtpu1
              nwpu=nwtpu1
              purad=15.
              grp=50
           end 
           1: begin
              regime='WT 1000<ctr<1200 cts/s'
              wpu=wtpu2
              nwpu=nwtpu2
              purad=14.
              grp=50
           end 
           2: begin
              regime='WT 800<ctr<1000 cts/s'
              wpu=wtpu3
              nwpu=nwtpu3
              purad=12.
              grp=40
           end 
           3: begin
              regime='WT 400<ctr<800 cts/s'
              wpu=wtpu4
              nwpu=nwtpu4
              purad=11
              grp=40
           end 
           4: begin
              regime='WT 300<ctr<400 cts/s'
              wpu=wtpu5
              nwpu=nwtpu5
              purad=6
              grp=40
           end 
           5: begin
              regime='WT 200<ctr<300 cts/s'
              wpu=wtpu6
              nwpu=nwtpu6
              purad=1
              grp=40
           end 
           6: begin
              regime='WT ctr<200 cts/s'
              wpu=wtpu7
              nwpu=nwtpu7
              purad=0
              grp=40
           end 
           7: begin  ;;;orbit 1
              regime='PC ctr>1 cts/s'
              wpu=pcpu1
              nwpu=npcpu1
              purad=7.
              grp=30
           end
           8: begin ;;;orbit 2
              regime='PC 0.6<ctr<1 cts/s'
              wpu=pcpu2
              nwpu=npcpu2
              purad=4.
              grp=20
              plus=100.
           end 
           9: begin  ;;;beyond
              regime='PC ctr<0.6 cts/s'
              wpu=pcpu3
              nwpu=npcpu3
              purad=0.
              grp=20
              plus=100.
           end 
        endcase 
        
        print,regime
        if nwpu gt 0 then begin
           if i eq 0 then tmin0=min(tstart[wpu])+trigtime else begin
              if n_elements(tmax0) ne 0 then tmin0=tmax0+plus else begin
                 read_time_specfit,spec,trigtime,dir='./'
                 tmin0=max(spec.tmax)+trigtime+plus
              endelse 
           endelse 
           tmax0=max(tstop[wpu])+trigtime;+plus

           if k eq 0 then begin 
              find_wt_srcdetpos,evfiles[wt[0]],srcra,srcdec,srcx,srcy,pa,wtsrcra,wtsrcdec
              roll=pa-90
              srcx=round(srcx) & srcy=round(srcy)
              cosr=cos((90-roll)*!dtor) & sinr=sin((90-roll)*!dtor)
              srcreg='srcwt'+ntostr(fix(purad))+'.reg'
              if purad gt 0 then begin
                 
;                 r1=20-purad/2.
;                 x1=srcx-(20.-r1/2.)*cosr
;                 y1=srcy+(20.-r1/2.)*sinr
;                 r2=20-fix(purad/2.) ;-1.
;                 x2=srcx+(20.-r2/2.)*cosr
;                 y2=srcy-(20.-r2/2.)*sinr
;                 xx=[x1,x2]
;                 yy=[y1,y2]
;                 r=[r1,r2]
;                 pas=[roll,roll]        
;                 write_regfile,srcreg,xx,yy,[20,20],r,roll=pas,/box,/det
                 write_regfile,srcreg,srcx,srcy,[40,purad],20,roll=roll-90,/box,/det
                 
;                 write_regfile,srcreg,wtsrcra,wtsrcdec,30,purad,/box 
                 
              endif else $
                 write_regfile,srcreg,wtsrcra,wtsrcdec,40,20,roll=roll-90,/box
;                write_regfile,bgreg,bgra,bgdec,20,40,roll=roll,/box
              
        ;;;filters whole WT evt files for PU region on region and time interval
              make_xselect_file,i,evfiles,wt,tmin0,tmax0,mode[k],srcreg,xselfile,srcevfile,/src,/pu
              print,'Running XSELECT on WT event file to get spec time bins'
              spawn,'xselect @'+xselfile +' > xselect'+ntostr(i+1)+'.out'
              
              make_xselect_file,i,evfiles,wt,tmin0,tmax0,mode[k],bgwtreg,bgxselfile,bgevfile,/bg,/pu
              print,'Running XSELECT on WT event file to get spec time bins BACKGROUND'
              spawn,'xselect @'+bgxselfile +' > xselect'+ntostr(i+1)+'.out'
              bgreg=bgwtreg
              wev=wt
           endif else begin 
              wpc=where(pcstop ge tmax0,nwpc)              
;              wpc=wpc[0]
              print,wpc
              wpc=indgen(n_elements(pcstop))
                            
              ;;;FIX THIS!!!!!!!!!!!
;              if nwpc ne 1 then 
              stop
              
              srcreg='srcpc'+ntostr(fix(purad))+'.reg'
              if purad eq 0 then write_regfile,srcreg,srcra,srcdec,rad else $
                 write_regfile,srcreg,srcra,srcdec,rad,purad,/ann
              make_xselect_file,i,evfiles,pc[wpc],tmin0,tmax0,mode[k],srcreg,xselfile,srcevfile,/src,/pu
              print,'Running XSELECT on PC event file to get spec time bins'
              spawn,'xselect @'+xselfile +' > xselect'+ntostr(i+1)+'.out'
              
              make_xselect_file,i,evfiles,pc[wpc],tmin0,tmax0,mode[k],bgpcreg,bgxselfile,bgevfile,/bg,/pu
              print,'Running XSELECT on PC event file to get spec time bins BACKGROUND'
              spawn,'xselect @'+bgxselfile +' > xselect'+ntostr(i+1)+'.out'
              bgreg=bgpcreg
              wev=pc[wpc]
           endelse 
           
           ev=mrdfits(srcevfile,1)
           nev=n_elements(ev)
           if nev eq 0 then stop
           nind=round(nev/nctsperbin*1.)
           if nind gt 0 then begin 
              ind=(lindgen(nind)+1)*nctsperbin 

              time=ev[ind].time
              tmin=[tmin0,time]
              tmax=[time,max(ev.time)]
           endif else begin
              tmin=tmin0
              tmax=max(ev.time)
           endelse 

;        if tmin[0] eq tmin[1] then begin
;           tmin=tmin[0]
;           tmax=tmax[1]
;        endif 
           
           print,'Total time with '+regime,[tmin0,tmax0]-trigtime
           colprint,tmin-trigtime,tmax-trigtime
           if n_elements(nt) eq 0 then begin
              ;;if 10000 and redoing only PC
;              ntl=19
;              nt0=19
              ntl=0
              nt0=0
           endif else begin
              nt0=nt
              ntl=nt
           endelse 
           nt=n_elements(tmax)+ntl

           for j=nt0,nt-1 do begin
              jj=j-nt0
              print,tmin[jj]-trigtime,tmax[jj]-trigtime
        ;;;;use evname already region & pu filtered to just time filter
              make_xselect_file,j,evfiles,wev,tmin[jj],tmax[jj],mode[k],srcreg,srcxselfile,srcevname,phaname,/src
              print,'XSELECT SOURCE'
              spawn,'xselect @'+srcxselfile +' > xselect'+ntostr(j+1)+'.out'
              
;              start=0.
;              if k eq 0 then begin
;                 start=900.+trigtime-tmin[jj]
              make_xselect_file,j,evfiles,wev,tmin[jj],tmax[jj],mode[k],bgreg,bgxselfile,bgevname,bgphaname,/bg
              print,'XSELECT BACKGROUND'
              spawn,'xselect @'+bgxselfile +' > xselect'+ntostr(j+1)+'.out'
              
;              hdr=headfits(srcevname)
              sev=mrdfits(srcevname,1,hdr)
;              nev=sxpar(hdr,'NEVENTS')
              nev=n_elements(sev)
              exptime=sxpar(hdr,'EXPOSURE')
              if nev eq 0 then stop
              phaname2=phaname+'.grpmin'+ntostr(grp)
              arfname='seg'+ntostr(j+1)+mode[k]+'.arf'
              grps=ntostr(grp)
;              if nev gt 150 then grp='20' else grp='10'
              grppha='grppha infile="'+phaname+'" outfile="'+phaname2+'" chatter=0 comm="chkey ANCRFILE '+arfname+' & chkey RESPFILE '+respfile[k]+' & chkey BACKFILE '+bgphaname+' & group min '+grps+' & exit" clobber=yes > grppha.out'
              print,grppha
              spawn,grppha
              
              fappend='fappend '+evfiles[0]+'+3 '+srcevname
              spawn,fappend
              stemout='seg'+ntostr(j+1)+mode[k]
              xrtexpomap='xrtexpomap infile='+srcevname+' attfile=../'+attfile+' hdfile=../'+hdfile+' outdir=./ stemout='+stemout+' clobber=yes > xrtexpomap.out'
              print,xrtexpomap
              spawn,xrtexpomap
              
              expomap=stemout+'_ex.img'
              xrtmkarf='xrtmkarf outfile='+arfname+' phafile='+phaname+' srcx=-1 srcy=-1 psfflag=yes clobber=yes expofile='+expomap+' > xrtmkarf.out'
              print,xrtmkarf
              spawn,xrtmkarf

              run_xspec,j,z,mode[k],phaname2,nh,nev,exptime,regime,trigtime,tmin0,tmax0
              print,'Resulting time interval: ',tmin0,tmax0
              tmin0=tmin0+trigtime
              tmax0=tmax0+trigtime

           endfor
           
     ;;;now need to add loop to make time filter file and filter and fit spectrum for each one of these tmin,tmax pairs
        endif         
     endfor 
  endfor   
  
  ;;read in LC to get time regions where pu regions are needed
  ;;make combined region filtered, pu corrected, event files
  ;;loop over each of those collecting gtis of xxx count bins
  ;;break up evt files and do fits of various models
  
  return
end 
