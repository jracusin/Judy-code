@flux_density_lc
@fit_functions
pro flux_lc_080319b
  trigtime=227599971.904d
  cd,'~/Desktop/GRB080319B/'
  lc=lcout2fits('lc_newout_manual.txt')
  flc=lc[n_elements(lc)-1]
  w=where(lc.src_rate gt 0 and lc.src_rate_err gt 0,nlc)
  lc=lc[w]

  w=where(lc[1:*].tstart-lc[0:nlc-1].tstop ge 0,nlc)  
  lc=lc[w]
  concat_structs,lc,flc,tmp
  lc=tmp
  
  cd,'~/Desktop/GRB080319B/final_correct'
  datfiles=strarr(10) & efiles=datfiles
  for i=0,9 do begin 
     datfiles[i]=file_search('seg'+ntostr(i+1)+'_*wt_Pow_nocr.dat')
     efiles[i]=file_search('seg'+ntostr(i+1)+'_*_wt.evt')
  endfor
  read_time_specfit,spec1,trigtime,datfiles=datfiles,efiles=efiles
  
  times=['4000_8000','1e4_1.3e4','1.6e4_4e4','4e4_2e5','2e5_1e6','1e6_1e7']
  datfiles='spec_'+times+'_Pow_nocr.dat'
  efiles='spec_'+times+'.evt'
  read_time_specfit,spec2,trigtime,datfiles=datfiles,efiles=efiles
  
  concat_structs,spec1,spec2,spec
  
  n=n_elements(spec)
  flux=dblarr(n_elements(lc)) & fluxerr=flux
  fluxfact=spec.unabs_flux/spec.rate
  for i=0,n-1 do begin
     w=where(lc.tstop ge spec[i].tmin and lc.tstart le spec[i].tmax)
     flux[w]=lc[w].src_rate*fluxfact[i]
     fluxerr[w]=lc[w].src_rate_err*fluxfact[i]
  endfor 
  
  nlc=n_elements(lc)
  print,fluxfact
  ploterror,lc.time,flux,fluxerr,psym=3,/xlog,/ylog,xrange=[10,1e7],/nohat,yrange=[1e-15,1e-6],/ysty;,ytickf='loglabels'
  for i=0,nlc-1 do oplot,[lc[i].tstart,lc[i].tstop],[flux[i],flux[i]]
  
  
  intmo='int_double_bknpow'
  mo='double_bknpow'
  pname=['norm1','alpha1','tb1','alpha2','norm2','alpha3','tb2','alpha4']
  p=[1d-4,1.5,3000.,2.1,1d-7,1.1,1e6,2.]
;  p[0]=p[0]*1d10
;  p[4]=p[4]*1d10
  tt=dblarr(2,nlc)
  tt[0,*]=lc.tstart
  tt[1,*]=lc.tstop
  cts=flux
  err=fluxerr
;  cts=lc.src_rate
;  err=lc.src_rate_err
  time=lc.time
  np=8
  parinfo = parinfo_struct(np)
  parinfo.value=p
  parinfo.parname=pname
  parinfo[0].limits=[1d-15,1d5];*1d10 ;;norm1
  parinfo[0].limited=[1,1]
  parinfo[1].limits=[0,0] ;;pow1
  parinfo[1].limited=[1,0]
  parinfo[2].limits=[1e3,1e4] ;;tb1
  parinfo[2].limited=[1,1]
  parinfo[4].limits=[0,0] ;;norm2
  parinfo[4].limited=[1,0]
  parinfo[6].limits=[1d5,2d6] ;;tb2
  parinfo[6].limited=[1,1]
  parinfo[7].limits=[p[5],4] ;;alpha4
  parinfo[7].limited=[1,1]
  
;  ploterror,lc.time,cts,err,psym=3,/xlog,/ylog,xrange=[10,1e7],/nohat,yrange=[1d-4,1d4],/ysty ;,ytickf='loglabels'
;  for i=0,nlc-1 do oplot,[lc[i].tstart,lc[i].tstop],[cts[i],cts[i]]
  g=where(time gt 80)
  newp=mpfitfun(intmo,tt[*,g],cts[g],err[g],p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo,dof=dof)
;  newp=mpfitfun(mo,time,cts,err,p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo,dof=dof)
  print,newp
;  newp=p
  tmp=execute('yfit='+mo+'(time,newp,f1,f2)')
  oplot,time,yfit,color=!red
  oplot,time,f1,color=!green,line=1
  oplot,time,f2,color=!blue,line=2
  stop
  
  return
end

pro lc_corr_080319b,nospawn=nospawn,skipwt=skipwt,skippc=skippc
  
;  xrtlccorr lcfile regionfile outfile corrfile attfile outinstrfile infile hdfile
  trigtime=227599971.904d
  
  ;;first WT
  
  if not keyword_set(skipwt) then begin 
     cd,'~/Desktop/GRB080319B/psf_corr/'
     srcevname=file_search('seg'+ntostr(indgen(10)+1)+'_*wt.evt')
     tmin=dblarr(10) & tmax=tmin
     for i=0,9 do begin
        ev=mrdfits(srcevname[i],1)
        tmin[i]=min(ev.time)
        tmax[i]=max(ev.time)
     endfor 
     exs=[14,14,14,14,10,10,8,0,0,0]
     binsize=[1,1,1,2,2,3,2,2,2,5]
     colprint,indgen(10)+1,tmin-trigtime,tmax-trigtime,exs
     
  ;;;need to run xrtproducts on pipeline lc with specific pu region files and default binning
  ;;; then run xrtlccorr
  ;;; then bin using lccurve
     
     cd,'~/Desktop/GRB080319B/wt_lc_corr'
     
     evfile='sw00306757000xwtw2po_cl.evt'
;  ex=[14,10,8,0]
;  bin=[2,2,3,3]
     ex=exs
     bin=binsize
     n=n_elements(bin)
     regions='src_excl'+ntostr(ex)+'_alt.reg'
;  tmins=dblarr(n) & tmaxs=tmins
;  for i=0,n-1 do begin
;     w=where(exs eq ex[i],nw)
;     tmins[i]=tmin[w[0]]
;     tmaxs[i]=tmax[w[nw-1]]
;  endfor 
     tmins=tmin & tmaxs=tmax
     print,'New Time bins'
     colprint,indgen(n)+1,tmins-trigtime,tmaxs-trigtime

     for i=0,n-1 do begin
        gtifile='gti_wt_'+ntostr(i+1)+'.flt'
        stem='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)
        writecol,gtifile,long(tmins[i]),long(tmaxs[i])
        ;;need to run xselect on gtifile 
        xselfile='xsel'+ntostr(i+1)+'.xco'
        openw,lun,xselfile,/get_lun
        printf,lun
        printf,lun
        printf,lun,'clear all'
        printf,lun,'yes'
        printf,lun
        printf,lun,'query yes'
        printf,lun
        printf,lun,'read ev '+evfile
        printf,lun,'./'
        printf,lun
        printf,lun
        printf,lun,'ext cu'
        printf,lun,'filter time file '+gtifile
        printf,lun,'ext cu'
        printf,lun,'ext ev'
        printf,lun,'save ev'
        evname=stem+'.evt'
        printf,lun,evname
        printf,lun,'yes'
        printf,lun
        printf,lun
        printf,lun,'exit'
        printf,lun
        close,lun
        free_lun,lun
        com='xselect @'+xselfile +' > xselect'+ntostr(i+1)+'.out'
        print,com
        if not keyword_set(nospawn) then spawn,com
        com='fappend '+evfile+'+3 '+evname
        print,com
        if not keyword_set(nospawn) then spawn,com
        ;;need to run xrtexpomap?
        
;     k=get_kbrd(10)
;     if k eq 's' then stop
        infile=stem+'.evt'
;     com='xrtproducts infile=sw00306757000xwtw2po_cl.evt outdir=./ stemout='+stem+' regionfile='+regions[i]+' gtifile='+gtifile+' clobber=yes binsize=-1'
        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem+' regionfile='+regions[i]+' clobber=yes binsize=-1'
        print,com
        if not keyword_set(nospawn) then spawn,com
     ;;; run xrtproducts twice to get binned and unbinned LC, then apply corrfact to binned
        stem2='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)+'_bin_'
        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem2+' regionfile='+regions[i]+' clobber=yes binsize='+ntostr(bin[i])
        print,com
        if not keyword_set(nospawn) then spawn,com
        blcfile=stem2+'sr.lc'
     ;;; run xrtproducts again on the background LC except that bg is contaminated early
        stem3='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)+'_binbg_'
        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem3+' regionfile=bgwt_alt.reg'+' clobber=yes binsize='+ntostr(bin[i])
        bgfile=stem3+'sr.lc'
        print,com
        if not keyword_set(nospawn) then spawn,com
        
;     k=get_kbrd(10)
;     if k eq 's' then stop
        lcfile='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)+'sr.lc'

        attfile='sw00306757000sat.fits.gz'
        hdfile='sw00306757000xhdtc.hk'
        outinstrfile='DEFAULT'
        outfile='DEFAULT'
        corrfile='DEFAULT'
        
        com='xrtlccorr '+lcfile+' NONE '+outfile+' '+corrfile+' '+attfile+' '+outinstrfile+' '+infile+' '+hdfile+' createinstrmap=yes clobber=yes'
        print,com
        if not keyword_set(nospawn) then spawn,com
        
;     k=get_kbrd(10)
;     if k eq 's' then stop
;     corrfile='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)+'sr_corr.lc'
        corrfile=stem+'sr_corrfact.fits'
        cfile=stem+'sr_corr.lc'
        outfile=stem+'sr_corr.flc'
        clc=mrdfits(cfile,1)
        
        blc=mrdfits(blcfile,1)
        corr=mrdfits(corrfile,1)
        corr.time=corr.time-trigtime
        head=headfits(blcfile)
;        spos=strpos(head,'TSTART')
;        spos=where(spos eq 0)
;        spos2=strpos(head[spos],'TSTART')
;        tstart=strmid(head[spos],spos2+9,23)
        tstart=fxpar(head,'TSTART')
        
        c=tstart-trigtime
        c=c[0]
        blc.time=blc.time+c
        flc=lcout2fits(/empty)
        flc=replicate(flc,n_elements(blc))
        flc.src_counts=blc.rate*blc.fracexp*bin[i]
        flc.exptime=bin[i]*blc.fracexp
        flc.time=blc.time
        flc.tstart=flc.time-bin[i]/2.
        flc.tstop=flc.time+bin[i]/2.
        for j=0,n_elements(corr)-2 do begin
           w=where(fix(blc.time) ge fix(corr[j].time) and fix(blc.time) lt fix(corr[j+1].time))
           flc[w].src_rate=blc[w].rate*corr[j].corrfact
           flc[w].src_rate_err=blc[w].error*corr[j].corrfact
           flc[w].pu_corr=corr[j].corrfact
        endfor 
        w=where(fix(blc.time) gt fix(corr[j].time),nw)
        if nw gt 0 then begin 
           flc[w].src_rate=blc[w].rate*corr[j].corrfact
           flc[w].src_rate_err=blc[w].error*corr[j].corrfact
           flc[w].pu_corr=corr[j].corrfact
        endif 
        mwrfits,flc,outfile,/create
        
     ;;; scale bg rate by area
        bglc=mrdfits(bgfile,1)
        sarea=(40-ex[i])*20.
        barea=20.*40.
        afact=sarea/barea
        bglc.rate=bglc.rate*afact
        bglc.time=bglc.time+c
        
        if i eq 0 then begin 
           fnlc=flc 
           fbglc=bglc
        endif else begin
           concat_structs,fnlc,flc,tmp
           fnlc=tmp
           concat_structs,fbglc,bglc,tmp
           fbglc=tmp
        endelse 
        
        ploterror,fnlc.time,fnlc.src_rate,fnlc.time-fnlc.tstart,fnlc.src_rate_err,psym=3,/nohat,/xlog,/ylog,yrange=[1e-4,1e4],xrange=[10,1e4]

     endfor 
     
     ;;background subtraction
     w=where(fbglc.time gt 400 and fbglc.error ne 0) ;;; after source is faint and not influencing bg
     wtbg=weighted_mean(fbglc[w].rate,fbglc[w].error)   
     fnlc.src_rate=fnlc.src_rate-wtbg
     
     ploterror,fnlc.time,fnlc.src_rate,fnlc.time-fnlc.tstart,fnlc.src_rate_err,psym=3,/nohat,/xlog,/ylog,yrange=[1,1e4],xrange=[10,1e4]
     readcol,'../xrt_lc.dat',time,tbin,rate,err,format='(d,d,d,d)' 
     oploterror,time-4.,rate,tbin,err,psym=3,errcolor=!red,/nohat ;;stupid 4s offset from dbl not flt
     
     wtlc=fnlc
     stop                       ;!
  endif 
  
;;;now PC!  Ugh!
  if not keyword_set(skippc) then begin 
     cd,'~/Desktop/GRB080319B/pc_lc_corr'
     
     evfiles=file_search('sw*xpcw2po_cl.evt')
     nev=n_elements(evfiles)
     tstarts=dblarr(nev) & tstops=tstarts
     for i=0,nev-1 do begin 
        head=headfits(evfiles[i])
        tstarts[i]=fxpar(head,'TSTART')
        tstops[i]=fxpar(head,'TSTOP')
     endfor 
     colprint,indgen(nev),tstarts-trigtime,tstops-trigtime
        
     st='srcpc_cl_ex'
     tmins=[4000.,1e4,1.6e4,4e4,1e5,2e5,5e5,1e6,1.5e6]+trigtime
     tmaxs=[8000.,1.3e4,4e4,1e5,2e5,5e5,1e6,1.5e6,3.e6]+trigtime
     bin=[200,400,800,5000,2e4,5e4,8e4,2e5,4e5]
     ex=[7,7,4,0,0,0,0,0,0]
     outrad=[30,30,25,20,20,15,9,7,5]
     
     n=n_elements(bin)
     regions='srcpc'+ntostr(ex)+'_'+ntostr(outrad)+'.reg'

     print,'New Time bins'
     colprint,indgen(n),indgen(n)+1,tmins-trigtime,tmaxs-trigtime,bin,ex,outrad
     
     g=0
     stop
     for i=0,n-1 do begin
;        if g gt 0 and i lt g then nospawn=1 else nospawn=0
        gtifile='gti_pc_'+ntostr(i+1)+'.flt'
        stem=st+ntostr(i+1)
        writecol,gtifile,long(tmins[i]),long(tmaxs[i])
        ;;need to run xselect on gtifile 
        xselfile='xsel'+ntostr(i+1)+'.xco'
        openw,lun,xselfile,/get_lun
        printf,lun
        printf,lun
        printf,lun,'clear all'
        printf,lun,'yes'
        printf,lun
        printf,lun,'query yes'
        printf,lun
        printf,lun,'read ev '+evfiles[0]
        printf,lun,'./'
        printf,lun
        printf,lun
        if i gt 1 then for j=1,n_elements(evfiles)-1 do printf,lun,'read ev '+evfiles[j]
        printf,lun,'ext cu'
        printf,lun,'filter time file '+gtifile
        printf,lun,'ext cu'
        printf,lun,'ext ev'
        printf,lun,'save ev'
        evname=stem+'.evt'
        printf,lun,evname
        printf,lun,'yes'
        printf,lun
        printf,lun
        printf,lun,'exit'
        printf,lun
        close,lun
        free_lun,lun
        com='xselect @'+xselfile +' > xselect'+ntostr(i+1)+'.out'
        print,com
        if not keyword_set(nospawn) then spawn,com
        com='fappend '+evfiles[0]+'+3 '+evname
        print,com
        if not keyword_set(nospawn) then spawn,com
        ;;need to run xrtexpomap?
        
        k=get_kbrd(10)
        if k eq 's' then stop
        infile=stem+'.evt'

        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem+' regionfile='+regions[i]+' clobber=yes binsize=-1'
        print,com
        if not keyword_set(nospawn) then spawn,com
     ;;; run xrtproducts twice to get binned and unbinned LC, then apply corrfact to binned
        stem2=st+ntostr(i+1)+'_bin_'
        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem2+' regionfile='+regions[i]+' clobber=yes binsize='+ntostr(bin[i])
        print,com
        if not keyword_set(nospawn) then spawn,com
        blcfile=stem2+'sr.lc'
     ;;; run xrtproducts again on the background LC except that bg is contaminated early
        stem3=st+ntostr(i+1)+'_binbg_'
        com='xrtproducts infile='+infile+' outdir=./ stemout='+stem3+' regionfile=bgpc.reg'+' clobber=yes binsize='+ntostr(bin[i])
        bgfile=stem3+'sr.lc'
        print,com
        if not keyword_set(nospawn) then spawn,com
        
        k=get_kbrd(10)
        if k eq 's' then stop
        lcfile=st+ntostr(i+1)+'sr.lc'
        
        if i le 1 then begin 
           attfile='sw00306757000sat.fits.gz'
           hdfile='sw00306757000xhdtc.hk'
        endif else begin
           attfile='merged_satfiles.fits'
           hdfile='merged_hkfiles.fits'
        endelse 
        
        outinstrfile='DEFAULT'
        outfile='DEFAULT'
        corrfile='DEFAULT'
        
        com='xrtlccorr '+lcfile+' NONE '+outfile+' '+corrfile+' '+attfile+' '+outinstrfile+' '+infile+' '+hdfile+' createinstrmap=yes clobber=yes'
        print,com
        if not keyword_set(nospawn) then spawn,com
        
        k=get_kbrd(10)
        if k eq 's' then stop
;     corrfile='sw00306757000xwtw2po_cl_ex'+ntostr(i+1)+'sr_corr.lc'
        corrfile=stem+'sr_corrfact.fits'
        cfile=stem+'sr_corr.lc'
        outfile=stem+'sr_corr.flc'
        clc=mrdfits(cfile,1)
        
        blc=mrdfits(blcfile,1)
        corr=mrdfits(corrfile,1)
        corr.time=corr.time-trigtime
        head=headfits(blcfile)
;        spos=strpos(head,'TSTART')
;        spos=where(spos eq 0)
;        spos2=strpos(head[spos],'TSTART')
;        tstart=strmid(head[spos],spos2+9,23)
        tstart=fxpar(head,'TSTART')
        
        c=tstart-trigtime
        c=c[0]
        blc.time=blc.time+c
        flc=lcout2fits(/empty)
        flc=replicate(flc,n_elements(blc))
        flc.src_counts=blc.rate*blc.fracexp*bin[i]
        flc.exptime=bin[i]*blc.fracexp
        flc.time=blc.time
        flc.tstart=flc.time-bin[i]/2.
        flc.tstop=flc.time+bin[i]/2.
        for j=0,n_elements(corr)-2 do begin
           w=where(fix(blc.time) ge fix(corr[j].time) and fix(blc.time) lt fix(corr[j+1].time),nw)
           if nw gt 0 then begin 
              flc[w].src_rate=blc[w].rate*corr[j].corrfact
              flc[w].src_rate_err=blc[w].error*corr[j].corrfact
              flc[w].pu_corr=corr[j].corrfact
           endif 
        endfor 
        w=where(fix(blc.time) gt fix(corr[j].time),nw)
        if nw gt 0 then begin 
           flc[w].src_rate=blc[w].rate*corr[j].corrfact
           flc[w].src_rate_err=blc[w].error*corr[j].corrfact
           flc[w].pu_corr=corr[j].corrfact
        endif 
        mwrfits,flc,outfile,/create
        
     ;;; scale bg rate by area
        bglc=mrdfits(bgfile,1)
        sarea=outrad[i]^2-ex[i]^2
        barea=40.^2
        afact=sarea/barea
        wb0=where(bglc.rate eq 0,nwb0)
        if nwb0 gt 0 then bglc[wb0].rate=1e-5
        bglc.rate=bglc.rate*afact
        bglc.time=bglc.time+c
        
        ;;;background subtraction
        flc.src_rate=flc.src_rate-bglc.rate
        flc.src_rate_err=sqrt(flc.src_rate_err^2+bglc.error^2)
        flc.back_rate=bglc.rate
        flc.tot_back_cts=bglc.rate*flc.exptime
        
        if i eq 0 then begin 
           fnlc=flc 
           fbglc=bglc
        endif else begin
           concat_structs,fnlc,flc,tmp
           fnlc=tmp
           concat_structs,fbglc,bglc,tmp
           fbglc=tmp
        endelse 
        
        ploterror,fnlc.time,fnlc.src_rate,fnlc.time-fnlc.tstart,fnlc.src_rate_err,psym=3,/nohat,/xlog,/ylog,yrange=[1e-4,1e4],xrange=[10,1e7]
        
;        stop
     endfor 
     
     pclc=fnlc
     pclc.type=1.

     ;;background subtraction
;     pclc.rate=pclc.rate-fbglc.rate
     
;  w=where(fbglc.time gt 400 and fbglc.error ne 0) ;;; after source is faint and not influencing bg
;  pcbg=weighted_mean(fbglc[w].rate,fbglc[w].error)   
;  fnlc.src_rate=fnlc.src_rate-pcbg
;;;DON'T WANT TO DO SINGLE BGRATE  
     
     concat_structs,wtlc,pclc,fnlc
     
     ploterror,fnlc.time,fnlc.src_rate,fnlc.time-fnlc.tstart,fnlc.src_rate_err,psym=3,/nohat,/xlog,/ylog,yrange=[1e-4,1e4],xrange=[10,1e7]
     readcol,'../xrt_lc.dat',time,tbin,rate,err,format='(d,d,d,d)' 
     oploterror,time-4.,rate,tbin,err,psym=3,errcolor=!red,/nohat ;;stupid 4s offset from dbl not flt

  endif
  
  w=where(fnlc.time lt 5e4)
  fnlc=fnlc[w]
  
  plc=lcout2fits(pcfile='../PCCURVE_bin50_min50.qdp',/phil,wtfile='blah')
  w2=where(plc.time gt 5e4 and plc.time lt 9.6e5)
  plc=plc[w2]
  
  plc2=lcout2fits(pcfile='../PCCURVE_bin50_min25.qdp',/phil,wtfile='blah')
  w2=where(plc2.time gt 9.6e5)
  plc2=plc2[w2]
  concat_structs,plc,plc2,tmp
  plc=tmp
  
  concat_structs,fnlc,plc,tmp
  fnlc=tmp
  ploterror,fnlc.time,fnlc.src_rate,fnlc.time-fnlc.tstart,fnlc.src_rate_err,psym=3,/nohat,/xlog,/ylog,yrange=[1e-4,1e4],xrange=[10,1e7]
  
  stop
  
  write_lc,fnlc,'lc_newout_manual.txt'
  
  return
end 

