pro run_com,com
  for j=0,n_elements(com)-1 do begin
     print,com[j]
     spawn,com[j]
  endfor 
end
 
pro hardness_ratio,grb,rad

  cd,'~/Chandra/'+grb
  afile=file_search('*/acis_evt2_clean.fits')
  na=n_elements(afile)

;  e=[300,1500,10000]
  e=[300,2000,10000]
  se=ntostr(e)
  eng=[se[0]+':'+se[1],se[1]+':'+se[2]]
  engout=[se[0]+'_'+se[1],se[1]+'_'+se[2]]
  legeng=[numdec(e[0]/1000.,1)+'-'+numdec(e[1]/1000.,1),numdec(e[1]/1000.,1)+'-'+numdec(e[2]/1000.,1)]

  outfile=file_search('*/*'+se[0]+'*'+se[1]+'*.dat')
;  if not exist(outfile[0]) then begin 
  if outfile[0] eq '' then begin 
     for i=0,na-1 do begin ;;; each Chandra obs
        s=strsplit(afile[i],'/',/ex)
        cd,s[0]

        for j=0,1 do begin ;;; each energy range
           
           com='punlearn dmcopy'
           com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmcopy "acis_evt2.fits[energy='+eng[j]+']" acis_evt2_clean_'+engout[j]+'.fits opt=all clobber=yes']
           run_com,com
           
           mfile='acis_evt2_clean_'+engout[j]+'.fits'
           if n_elements(rad) eq 0 then rad=2
           mfile1='src_acis_evt2_clean_'+ntostr(rad)+'_'+engout[j]+'.fits'

           com='punlearn dmextract'
           com=[com,'pset dmextract infile="'+mfile+'[bin sky=region(src_'+ntostr(rad)+'.reg)]"'] ;circle('+ra+','+dec+',10)]"']
           com=[com,'pset dmextract outfile='+mfile1]
           com=[com,'pset dmextract bkg="'+mfile+'[bin sky=region(bg.reg)]"'] ;annulus('+ra+','+dec+',5,20)]"']
           com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmextract clobber=yes']
           run_com,com

           out=mrdfits(mfile1,1)
           com='/Users/jracusin/CIAO/ciao-4.6/bin/dmlist "'+mfile1+'[cols counts,area,bg_counts,bg_area,net_counts,net_err,exposure]" data > results'+ntostr(rad)+'_'+engout[j]+'.dat'
           print,com
           spawn,com

;stop
        endfor 
        cd,'..'
     endfor 
  endif 
  
  read_chandra_results,grb,str,rad=rad,add='*'+se[1]+'*' 
  lc=mrdfits('UL_lc_chandra.fits',1)

  useq=str[rem_dup(str.seq)].seq
  for i=0,n_elements(useq)-1 do begin
     w=where(str.seq eq useq[i])
     w1=where(strpos(str[w].file,engout[0]) ne -1)
     w2=where(strpos(str[w].file,engout[1]) ne -1)
     s=str[w[w1]].netcts
     h=str[w[w2]].netcts
     serr=str[w[w1]].neterr
     herr=str[w[w2]].neterr
;     h=c1/c2
;     err=sqrt((1/c2^2)*e1^2+c1^2/c2^4*e2^2)
     r=(h-s)/(h+s)
     dr_dh=1/(h+s)-(h-s)/(h+s)^2
     dr_ds=-1/(h+s)-(h-s)/(h+s)^2
     err=sqrt(dr_dh^2*herr^2+dr_ds^2*serr^2)

     print,str[w[w1]].seq,s,h,str[w[w1]].exposure,r,err
     lc[i].tot_hard=r
     lc[i].tot_hard_err=err
  endfor 

  read_qdp,'hardrat_2keV.qdp',time,tpos,tneg,rate,rerr,type=type
  wh=where(strpos(type,'hard data') ne -1,nwh)
  ws=where(strpos(type,'soft data') ne -1)
  h=rate[wh]
  s=rate[ws]
  herr=rerr[wh]
  serr=rerr[ws]

  r=(h-s)/(h+s)
  dr_dh=1/(h+s)-(h-s)/(h+s)^2
  dr_ds=-1/(h+s)-(h-s)/(h+s)^2
  err=sqrt(dr_dh^2*herr^2+dr_ds^2*serr^2)

  begplot,name=grb+'_hardness_ratio.ps',/land
  f2='bat_xrt_flux_with_gamma_DENSITY_BATSNR5.qdp'
  if exist(f2) then !p.multi=[0,1,2]
  plotsym,0,0.8,/fill
  ploterror,time[wh],r,err,/nohat,/xlog,psym=8,xtitle='Time since T!L0!N (s)',ytitle='(hard-soft)/(hard+soft)',xrange=[5e1,1e7],yrange=[-1,0.5],/xsty
  for i=0,nwh-1 do oplot,time[wh[i]]+[tneg[wh[i]],tpos[wh[i]]],[r[i],r[i]]
  oploterror,lc.time,lc.tot_hard,lc.tot_hard_err,psym=8,errcolor=!red,/nohat,color=!red
;  ploterror,lc.time,lc.tot_hard,lc.tot_hard_err,/nohat,/xlog,psym=8,xtitle='Time since T0 (s)',ytitle='(hard-soft)/(hard+soft)'
  legend,['hard band: ','soft band: ']+legeng+' kev',/top,/right,box=0

  if exist(f2) then begin
     read_qdp,'bat_xrt_flux_with_gamma_DENSITY_BATSNR5.qdp',Time,Poserr,Negerr,gamma,gammaposerr,gammanegerr,type=type,tpos=1
     w=where(strpos(type,'photon indices') ne -1)
     w2=where(strpos(type[w],'XRT') ne -1)
     w=w[w2]
     terr=fltarr(2,n_elements(time))
     terr[0,*]=-negerr
     terr[1,*]=poserr
     gerr=fltarr(2,n_elements(gamma))
     gerr[0,*]=-gammanegerr
     gerr[1,*]=gammaposerr
     ploterror2,time[w],gamma[w],terr[*,w],gerr[*,w],psym=8,xrange=[5e1,1e7],xtitle='Time since T!L0!N (s)',ytitle=!tsym.gamma_cap,/xlog,/xsty,yrange=[0.5,3]

     ;;; specific to GRB150314A
     if grb eq 'GRB150314A' then begin 
        nlc=n_elements(lc)
        cgam=[2.7,2.63,2.1]
        cgamerr=[[0.74,0.82],[0.82,0.96],[0.66,0.77]]
        terr=fltarr(2,3)
        oploterror2,lc[nlc-3:*].time,cgam,terr,cgamerr,psym=8,color=!red,/nohat,errcolor=!red
     endif 
     !p.multi=0
  endif 

  endplot
  ps2pdf,grb+'_hardness_ratio.ps'


stop
  return 
end 

pro write_datfile,grb=grb

  name='~/Chandra/'+grb+'/'+grb+'_xrt_cxo.txt'

  readcol,'~/Chandra/conv_factors.csv',grbss,xrtratio,xrtunabsratio,cxoratio,cxounabsratio,delim=',',format='(a,f,f,f,f)'
  g=where(strtrim(grbss,2) eq strtrim(grb,2))
  ratio=xrtratio[g[0]]
  lc=lcout2fits('lc_newout_phil.txt')
  
  mode=strarr(n_elements(lc))
  wts=where(lc.type eq 5,nwts)
  if nwts gt 0 then mode[wts]='WT_SETTLING'
  
  wt=where(lc.type eq 0,nwt)
  if nwt gt 0 then mode[wt]='WT'

  pc=where(lc.type eq 1,npc)
  if npc gt 0 then mode[pc]='PC'

  cxo=where(lc.type eq 2,ncxo)
  if ncxo gt 0 then mode[cxo]='CXO'

  writecol,name,lc.time,lc.tstop-lc.time,lc.src_rate*ratio,lc.src_rate_err*ratio,mode,header=['Time','Time_bin','Flux','Flux_err','Mode']

return
end 
pro chandra_cr_fits,ps=ps

  masterdir=!adata+'Chandra/'
  cd,masterdir

  dir=file_search('GRB*') 
  ndir=n_elements(dir)
  
  ;;; SPEC FITS (COMMON FOR ALL SEGMENTS - hmm?)
  read_specfits,spec
  n=12 ;; N GRBs

  for i=0,n-1 do begin 
   ;;; READ COMBINED XRT-CXO LC FITS
     grb=spec[i].grb
     cd,masterdir+grb
     lcfile='lc_fit_comb.dat'
     if not exist(lcfile) then lcfile='lc_fit_xrt.dat'
     read_lcfit,lcfile,pname,p,perror
     np=n_elements(p)
     case np of
        2: j=1
        4: j=[1,3]
        6: j=[1,3,5]
        8: j=[1,3,5,7]
        10: j=[1,3,5,7,9]
     endcase
     if np le 8 then begin 
        alpha=p[j]
        alphaerr0=perror[0,j]
        alphaerr1=perror[1,j]
        nb=n_elements(j)
        gamma=replicate(spec[i].phind,nb)
        gammaerr0=replicate(spec[i].pherr[0],nb)
        gammaerr1=replicate(spec[i].pherr[1],nb)
        
        fit_crs,alpha,alphaerr0,alphaerr1,gamma,gammaerr0,gammaerr1,/plotcompat,ps=ps,title=grb,name=grb+'_cr_fits.ps'
     endif 
     cd,'..'
;     k=get_kbrd(10)
  endfor 

  return
end 

pro sky_region,evfile,asolfiles,ra,dec,skyx,skyy,outfile,com=com,readsky=readsky

;  asolfiles=file_search('primary/pcad*asol*fits')
;  spawn,'ciao'
;  setenv,'ASCDS_INSTALL=/Users/CIAO/ciao-4.1'
;  setenv,'CALDB=/Users/CALDB'
;  com='/Users/CIAO/ciao-4.6/bin/dmcoords '+evfile+'  asolfile='+asolfiles+' opt=cel ra="'+ra+'" dec="'+dec+'" verbose=1  > sky_reg.txt'
  com='dmcoords '+evfile+' asolfile='+asolfiles+' opt=cel ra="'+ra+'" dec="'+dec+'" verbose=1 > '+outfile

  print,com
;  spawn,com
  
  if keyword_set(readsky) then begin 

     readcol,outfile,lines,delim='$',format='(a)'
     spos=strpos(lines,'SKY')
     w=where(spos ne -1)
     line=lines[w]
     sep=str_sep(line,' ')
     skyx=sep[1]
     skyy=sep[2]
     print,skyx,'  ',skyy
  endif 
;;;NEED TO WRITE OUT A FILE BECAUSE DOESN'T SEEM TO WORK FROM WITHIN IDL
  return
end 

pro what_if_no_jb

  masterdir=!adata+'Chandra/'
  cd,masterdir

  dir=file_search('GRB*')
  yrange=[1e-16,1e-8]

  maybe=create_struct('GRB','','tlastpos',0.,'ctr_tlastpos',0.)
  maybe=replicate(maybe,n_elements(dir))
  maybe.grb=dir
  colprint,dir,indgen(n_elements(dir))
  g=0
  stop
  
  for i=g,n_elements(dir)-1 do begin 
;     lcout='lc_newout_chandra_comb.txt'
     lcout='lc_newout_phil2.txt'
     if not exist(lcout) then lcout='lc_newout_chandra.txt'
     lcfile='lc_fit_comb.dat'
     if not exist(dir[i]+'/'+lcfile) then lcfile='lc_fit_xrt.dat'
     when_maybe_jetbreak,dir[i],altbreak,altbreak_rate,nsig=nsig,cr=cr,phil=phil,masterdir=masterdir,lcfile=lcfile,yrange=yrange,/go,lcout=lcout
     print,altbreak
     maybe[i].tlastpos=altbreak
     maybe[i].ctr_tlastpos=altbreak_rate

     k=get_kbrd(10)
     if k eq 's' then stop
  endfor

  mwrfits,maybe,masterdir+'maybe_jetbreak.fits',/create

  return
end 

@fit_functions
pro fit_comb_flux_lcs,ps=ps,justplot=justplot,rad=rad

  cd,!adata+'Chandra/'
;  readcol,'~/Chandra/conv_factors.csv',grbss,xrtratio,xrtunabsratio,cxoratio,cxounabsratio,delim=',',format='(a,f,f,f,f)'

  if exist('maybe_jetbreak.fits') then $
     jb=mrdfits('maybe_jetbreak.fits',1) else begin
     jb=create_struct('GRB','','tlastpos',0.,'ctr_tlastpos',0.)
     jb=replicate(jb,n_elements(grbss))
     jb.grb=grbss
  endelse 
        
  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)'
  g=0
  colprint,indgen(n),grbs
  stop
  for i=g,n-1 do begin
     grb=grbs[i]
     cd,grb
     print
     print,grb
     print
     read_psffits,grb,psfspec,rad=rad
     xrt=mrdfits('~/Chandra/'+grb+'/UL_specfits.fits',1)
     w=where(finite(psfspec.corrfact))
;     match,grb,grbss,m1,m2
     if not keyword_set(unabs) then begin 
;        xratio=xrtratio[m2[0]]
;        cratio=cxoratio[m2[0]]
        xratio=xrt[n_elements(xrt)-1].cfratio
        cratio=mean(psfspec[w].corrfact*psfspec[w].fluxfact)
     endif else begin
;        xratio=xrtunabsratio[m2[0]]
        xratio=xrt[n_elements(xrt)-1].unabs_cfratio
;        cratio=cxounabsratio[m2[0]]
        cratio=mean(psfspec[w].uncorrfact*psfspec[w].unfluxfact)
     endelse 

     lcc=lcout2fits();'UL_lc.fits');'lc_newout_phil.txt')
     lcc2=lcout2fits(/chandra);'lc_newout_chandra.txt')
     concat_structs,lcc,lcc2,lcc1
     lcc=lcc1
     write_lc,lcc,'lc_newout_phil2.txt'
     yrange=prange(lcc.src_rate,lcc.src_rate_err)
     yrange[0]=10d^(round(alog10(yrange[0])-1.))
     yrange[1]=10d^(round(alog10(yrange[1])+0.5))
     print,'yrange = ',yrange
     xrange=10d^(round(alog10([min(lcc.tstart),max(lcc.tstop)])+[-0.5,0.5]))
     print,'xrange = ',xrange

     ;;; fit XRT LC alone (save under xrt name)
     print,'ONLY XRT FIT'

     fit_lc,/nohard,/nochandra,yrange=yrange,xrange=xrange,ps=ps,charsize=1.,justplot=justplot,ytitle=ytitle,ffile='lc_fit_xrt.dat';,flux=xratio
     if exist('lc_fit_out_idl_int9.dat') then begin 
        spawn,'cp lc_fit_out_idl_int9.dat lc_fit_xrt.dat'
        spawn,'rm lc_fit_out_idl_int9.dat'
        spawn,'cp lc_fit_plot.ps lc_fit_plot_xrt.ps'
     endif 
     if keyword_set(justplot) then spawn,'cp lc_fit_plot.ps lc_fit_plot_xrt.ps'

     ;;; fit combined XRT/Chandra LC (save under chandra name)
     print,'COMBINED XRT-CHANDRA FIT'
     k=get_kbrd(10)
     wcxo=where(lcc.type eq 2 and lcc.src_rate_err gt 0,nwcxo)
     if nwcxo gt 0 then begin 
        if n_elements(jb) ge i and exist('lc_fit_comb.dat') then begin 
           if jb[i].tlastpos ne 0 then begin
              read_lcfit,'lc_fit_comb.dat',pname,p
              np=n_elements(p)
              lctime=[lcc.time,jb[i].tlastpos,1e8]
              lctime=lctime[sort(lctime)]
              case np of 
                 2: yfit=bknpow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 4: yfit=bkn2pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 6: yfit=bkn3pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 8: yfit=bkn4pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 else:
              endcase 
              wc=where(lctime ge jb[i].tlastpos)
;           oplot,lctime[wc],yfit[wc],line=2
              opx=lctime[wc]
              opy=yfit[wc]
           endif else begin
              opx=[1,1]
              opy=[1,1]
           endelse 
        endif 

        fit_lc,lc=lcc,/nohard,xrange=xrange,yrange=yrange,ytitle=ytitle,ffile='lc_fit_comb.dat',ps=ps,charsize=1.,justplot=justplot,opx=opx,opy=opy,/nochandra,file='lc_newout_phil2.txt',flux=xratio,/nouseflux;,nsig=1.,int='8'
        if keyword_set(ps) then endplot
        if exist('lc_fit_out_idl_int9.dat') then begin 
           spawn,'cp lc_fit_out_idl_int9.dat lc_fit_comb.dat'
           spawn,'more lc_fit_xrt.dat'
           spawn,'more lc_fit_comb.dat'
           spawn,'rm lc_fit_out_idl_int9.dat'
           spawn,'cp lc_fit_plot.ps lc_fit_plot_comb.ps'
        endif 
        if keyword_set(justplot) then spawn,'cp lc_fit_plot.ps lc_fit_plot_comb.ps'
     
        k=get_kbrd(10)
        if k eq 's' then stop
     endif else begin
        print,'No Chandra detections'
;        plot_like_qdp,file='lc_newout_chandra.txt',yrange=yrange,ytitle=ytitle,/nochandra,ps=ps
;        read_lcfit,'lc_fit_xrt.dat',pname,p
        if exist('flares_gtis.dat') then begin 
           readcol,'flares_gtis.dat',fstart,fstop
           for f=0,n_elements(fstart)-1 do begin 
              wnof=where(lcc.time le fstart[f] or lcc.time ge fstop[f])
              lcc=lcc[wnof]
           endfor 
           write_lc,lcc,'lc_newout_noflares.txt'
        endif 

        if n_elements(jb) gt 0 then begin 
           if jb[i].tlastpos ne 0 then begin
              read_lcfit,'lc_fit_xrt.dat',pname,p
              np=n_elements(p)
              lctime=[lcc.time,jb[i].tlastpos,1e8]
              lctime=lctime[sort(lctime)]
              case np of 
                 2: yfit=bknpow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 4: yfit=bkn2pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 6: yfit=bkn3pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 8: yfit=bkn4pow(lctime,[p,jb[i].tlastpos,p[np-1]+1.])
                 else:
              endcase 
              wc=where(lctime ge jb[i].tlastpos)
;           oplot,lctime[wc],yfit[wc],line=2
              opx=lctime[wc]
              opy=yfit[wc]
           endif 
        endif 
        fit_lc,file='lc_newout_chandra.txt',/nohard,yrange=yrange,ytitle=ytitle,/nochandra,ffile='lc_fit_xrt.dat',ps=ps,charsize=1.,/justplot,opx=opx,opy=opy

        spawn,'cp lc_fit_plot.ps lc_fit_plot_comb.ps'

        k=get_kbrd(10)
        if k eq 's' then stop
     endelse 
     ;;; compare fits
     cd,'..'
  endfor 
return
end 

pro make_comb_flux_lc_plots,rad=rad

  cd,!adata+'Chandra/'
  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  colprint,indgen(n),grbs
  g=0
  stop

  for i=g,n-1 do begin
     make_comb_flux_lcs,grbs[i],unabs=unabs,/ps,pfile=pfile,rad=rad,/withfit
     pos=strpos(pfile,'.ps')
     gfile=strmid(pfile,0,pos)
;     spawn,'convert -rotate 270 -background white '+pfile+'
;     '+gfile+'.png'
     spawn,'ps2pdf '+pfile+' '+gfile+'.pdf'
  endfor 
return
end 

pro make_comb_flux_lcs,grb,rad=rad,unabs=unabs,ps=ps,pfile=pfile,withfit=withfit
  
; readcol,'~/Chandra/conv_factors.csv',grbs,xrtratio,xrtunabsratio,cxoratio,cxounabsratio,delim=',',format='(a,d,d,d,d)'
  xrt=mrdfits('~/Chandra/'+grb+'/UL_specfits.fits',1)
 read_psffits,grb,psfspec,rad=rad
 w=where(finite(psfspec.corrfact))

;  match,grb,grbs,m1,m2
  if not keyword_set(unabs) then begin 
;     xratio=xrtratio[m2[0]]
;     cratio=cxoratio[m2[0]]
     xratio=xrt[n_elements(xrt)-1].cfratio
     cratio=mean(psfspec[w].corrfact*psfspec[w].fluxfact)
  endif else begin
;     xratio=xrtunabsratio[m2[0]]
;     cratio=cxounabsratio[m2[0]]
     xratio=xrt[n_elements(xrt)-1].unabs_cfratio
     cratio=mean(psfspec[w].uncorrfact*psfspec[w].unfluxfact)
  endelse 
  print,'cratio = ',cratio
  cd,!adata+'Chandra/'+grb
  if keyword_set(unabs) then un='_unabs_' else un=''

  xlc=lcout2fits()


  xlc.src_rate=xlc.src_rate*xratio
  xlc.src_rate_err=xlc.src_rate_err*xratio
  
  clc=lcout2fits(/empty)
  bin_chandra_data,grb,gg,rad=rad

  if n_elements(gg) gt 1 then clc=replicate(clc,n_elements(gg))
  clc.time=gg.time
  clc.tstart=gg.tstart
  clc.tstop=gg.tstop
  clc.src_counts=gg.counts-gg.bgcts
  clc.src_rate=gg.rate*cratio/xratio
  for i=0,n_elements(gg)-1 do $
     clc[i].src_rate_err=min([gg[i].raterr[0],gg[i].raterr[1]])*cratio/xratio
  clc.tot_back_cts=gg.bgcts
  clc.det_sig=(gg.counts-gg.bgcts)/sqrt(gg.counts)
  clc.exptime=gg.exposure
  clc.pu_corr=1.
  clc.type=2

;  lc=clc
;  write_lc,lc,'lc_newout_chandra'+un+'.txt'
  mwrfits,clc,'UL_lc_chandra.fits',/create
;  k=get_kbrd(10)

  clc.src_rate=clc.src_rate*xratio
  clc.src_rate_err=clc.src_rate_err*xratio
  concat_structs,xlc,clc,lc

;  w=where(lc.src_rate ne 0 and lc.src_rate_err ne 0)
  yrange=prange(lc.src_rate,lc.src_rate_err)
  yrange[0]=10d^(round(alog10(yrange[0])-0.5))
  yrange[1]=10d^(round(alog10(yrange[1])+0.5))

  pfile=grb+'_combined_lc'+un+'.ps'
  if keyword_set(ps) then begplot,name=pfile,/color,/land
  plot_like_qdp,yrange=yrange,ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)',title=grb,flux=1,lc=lc,chandra=chandra
  if keyword_set(withfit) then begin
     lcfile='lc_fit_comb.dat'
     if not exist(lcfile) then lcfile='lc_fit_xrt.dat'
     if not exist(lcfile) then lcfile='lc_fit_out_idl_int9.dat'
     print,lcfile
     read_lcfit,lcfile,pname,p,perror
     mo=fit_models(pname,p,np,basemo=basemo)
print,mo
;     np=n_elements(p)
     case np of 
        2: breaks=-1
        4: breaks=p[2]
        6: breaks=[p[2],p[4]]
        8: breaks=[p[2],p[4],p[6]]
        10: breaks=[p[2],p[4],p[6],p[8]]
     endcase 
     time=[lc.time,breaks]
     w=where(time gt 0)
     time=time[w]
     time=time[sort(time)]
;     case np of 
;        2: flux=pow(time,p)
;        4: flux=bknpow(time,p)
;        6: flux=bkn2pow(time,p)
;        8: flux=bkn3pow(time,p)
;        10: flux=bkn4pow(time,p)
;     endcase 
     flux=call_function(mo,time,p)
     f2=call_function(basemo,time,p)
     oplot,time,flux*xratio
     oplot,time,f2*xratio,line=1
     print,breaks
     if breaks[0] ne -1 then for i=0,n_elements(breaks)-1 do oplot,[breaks[i],breaks[i]],[1d-20,1d-5],line=2
  endif 

  if keyword_set(ps) then endplot
;  fit_lc,file='lc_newout_chandra.txt',/nohard,yrange=yrange
  
  return
end 

pro read_specfits,spec
;  readcol,'~/Chandra/spectral_fits.txt',name1,name2,par,e1,e2,format=('a,a,f,a,a'),delim=' '
  readcol,'~/Chandra/spectral_fits.txt',pars,format=('a'),delim='%'
  spec=create_struct('grb','','nhgal',0.,'nh',0.,'nherr',fltarr(2),'z',0.,'phind',0.,$
                     'pherr',fltarr(2))
  spec=replicate(spec,19)

  np=n_elements(pars)
  name1=strarr(np) & name2=name1 & par=name1 & e1=par & e2=par
  j=0
  for i=0,n_elements(pars)-1 do begin
     str=str_sep(pars[i],' ')
     if n_elements(str) gt 2 then begin 
        name1[i]=str[0]
        name2[i]=str[1]
        par[i]=str[2]
        if n_elements(str) gt 3 then e1[i]=str[3]
        if n_elements(str) gt 4 then e2[i]=str[4]
     endif 
     if str[0] eq 'GRB' then begin 
        spec[j].grb='GRB'+str[1]
        j=j+1
     endif 
  endfor 
    
  w=where(name2 eq '(Galactic)')
  spec.nhgal=par[w]*1.
  w=where(name2 eq '(intrinsic)',nw)
  spec.nh=par[w]*1.;*1.e21
  for i=0,nw-1 do begin 
     spec[i].nherr[1]=strsplit(e1[w[i]],'(+,',/ex)*1.e21
     spec[i].nherr[0]=strsplit(e2[w[i]],'-)',/ex)*1.e21
  endfor 
  w=where(name1 eq 'z')
  spec.z=e1[w]*1.
  w=where(name1 eq 'Photon',nw)
  spec.phind=par[w]*1.
  for i=0,nw-1 do begin 
     spec[i].pherr[1]=strsplit(e1[w[i]],'(+,',/ex)*1.
     spec[i].pherr[0]=strsplit(e2[w[i]],'-)',/ex)*1.
  endfor 

  return
end 

pro read_psffits,grb,psfspec,rad=rad

;  cd,!adata+'chandra/'+grb
  cd,'~/Chandra/'+grb

  if n_elements(rad) eq 0 then rad=2
  pifiles=file_search('GRB*_*'+ntostr(rad)+'_grp.pi')
  n=n_elements(pifiles)

  ;;;read outputs
  datfile=grb+'_'+ntostr(indgen(n)+1)+'_'+ntostr(rad)+'.dat'
  print,datfile
  psfspec=create_struct('norm',0d,'rate',0d,'mrate',0d,'flux1',0d,$
                        'unabs_flux1',0d,$
                        'cnorm',0d,'crate',0d,'cmrate',0d,'cflux1',0d,'cflux2',0d,$
                        'unabs_cflux1',0d,'unabs_cflux2',0d,$
                        'corrfact',0d,'uncorrfact',0d,$
                        'fluxfact',0d,'unfluxfact',0d)
  psfspec=replicate(psfspec,n)
  for i=0,n-1 do begin 
     if exist(datfile[i]) then begin 
        readcol,datfile[i],par,val,format='(a,d)'
        psfspec[i].norm=val[0]
        psfspec[i].rate=val[1]
        psfspec[i].mrate=val[2]
        psfspec[i].flux1=val[3]
        psfspec[i].unabs_flux1=val[4]
        psfspec[i].cnorm=val[5]
        psfspec[i].crate=val[6]
        psfspec[i].cmrate=val[7]
        psfspec[i].cflux1=val[8]
        psfspec[i].cflux2=val[9]
        psfspec[i].unabs_cflux1=val[10]
        psfspec[i].unabs_cflux2=val[11]
     endif 
  endfor 

  psfspec.corrfact=psfspec.cflux1/psfspec.flux1
  psfspec.uncorrfact=psfspec.unabs_cflux1/psfspec.unabs_flux1
  psfspec.fluxfact=psfspec.cflux2/psfspec.crate
  psfspec.unfluxfact=psfspec.unabs_cflux2/psfspec.crate

return
end 

pro psf_flux_corrfact,grb,psfspec,rad=rad

  cd,!adata+'chandra/'+grb

  ;;; after ./psscript.*.txt from psf_corr_cxo
  ;;; after psf_mkarf to get new corrected psf
  if n_elements(rad) eq 0 then rad=2

  read_specfits,spec
  match,strtrim(spec.grb,2),grb,g

  ;;; run xspec
  pifiles=file_search('GRB*_*'+ntostr(rad)+'_grp.pi')

  n=n_elements(pifiles)
  print,pifiles

  for i=0,n-1 do begin
     dfile=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_grp.pi'
     carf=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf'
     if exist(carf) then begin 
        print,dfile
        xbfile='xspec_'+grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.batch'
        xofile='xspec_'+grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.out'
        openw,lun,xbfile,/get_lun
        printf,lun,'data '+dfile
        printf,lun,'setplot en'
        printf,lun,'ignore bad'
        printf,lun,'ignore 0.0-0.5 8.0-**'
        printf,lun,'notice 0.5-8.0'
        printf,lun,'mo wabs*zwabs*pow'
        printf,lun,ntostr(spec[g].nhgal/1d22)+' 0 '
        printf,lun,ntostr(spec[g].nh/1d22)+' 0 '
        printf,lun,ntostr(spec[g].z)
        printf,lun,ntostr(spec[g].phind)+' 0 '
        printf,lun
        printf,lun,'statistic cstat'
        printf,lun,'fit 1000'
        printf,lun,'fit 1000'
        printf,lun,'fit 1000'
        datfile=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.dat'
        printf,lun,'set fileid [open '+datfile+' w]'
        printf,lun,'tclout param 5'
        printf,lun,'set par5 [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $par5 { } cpar5'
        printf,lun,'set lpar5 [split $cpar5]'
        printf,lun,'puts $fileid "norm [lindex $lpar5 0]"'

        printf,lun,'show rate'
        printf,lun,'tclout rate 1'
        printf,lun,'set rte [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $rte { } crte'
        printf,lun,'set lrte [split $crte]'
        printf,lun,'puts $fileid "rate [lindex $lrte 0]"' 
        printf,lun,'puts $fileid "model_rate [lindex $crte 2]"'
        ;;UNCORR FLUX
        printf,lun,'flux 0.5 8.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "flux0.5_8 [lindex $lflx 0]"'
        printf,lun,'newpar 1 0 1 0 0 1e4 1e4'
        printf,lun,'newpar 2 0 1 0 0 1e4 1e4'
        printf,lun,'flux 0.5 8.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "unabs_flux0.5_8 [lindex $lflx 0]"'
        
        printf,lun,'newpar 1 '+ntostr(spec[g].nhgal/1d22)+' 0 '
        printf,lun,'newpar 2 '+ntostr(spec[g].nh/1d22)+' 0 '
        ;;CORR FLUX
        printf,lun,'arf '+carf
        printf,lun,'fit 1000'
        printf,lun,'fit 1000'
        printf,lun,'fit 1000'
        printf,lun,'tclout param 5'
        printf,lun,'set par5 [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $par5 { } cpar5'
        printf,lun,'set lpar5 [split $cpar5]'
        printf,lun,'puts $fileid "cnorm [lindex $lpar5 0]"'
        printf,lun,'show rate'
        printf,lun,'tclout rate 1'
        printf,lun,'set rte [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $rte { } crte'
        printf,lun,'set lrte [split $crte]'
        printf,lun,'puts $fileid "crate [lindex $lrte 0]"' 
        printf,lun,'puts $fileid "cmodel_rate [lindex $crte 2]"'
        printf,lun,'flux 0.5 8.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "cflux0.5_8 [lindex $lflx 0]"'
        printf,lun,'flux 0.3 10.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "cflux0.3_10 [lindex $lflx 0]"'
        ;;unabs
        printf,lun,'newpar 1 0 1 0 0 1e4 1e4'
        printf,lun,'newpar 2 0 1 0 0 1e4 1e4'
        printf,lun,'flux 0.5 8.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "unabs_cflux0.5_8 [lindex $lflx 0]"'
        printf,lun,'flux 0.3 10.0'
        printf,lun,'tclout flux 1'
        printf,lun,'set flx [string trim $xspec_tclout]'
        printf,lun,'regsub -all { +} $flx { } cflx'
        printf,lun,'set lflx [split $cflx]'
        printf,lun,'puts $fileid "unabs_cflux0.3_10 [lindex $lflx 0]"'
        printf,lun,'close $fileid'
        printf,lun,'exit'
        printf,lun
        
        close,lun
        free_lun,lun

        print,'XSPEC'
        spawn,'xspec - '+xbfile+' > '+xofile
     endif 
     print,'done'
  endfor 
  read_psffits,grb,psfspec,rad=rad

  return
end 

pro psf_mkarf,grb,rad=rad

  cd,!adata+'Chandra/'+grb
  afile=file_search('*/acis_evt2_clean.fits')
  asolfiles=file_search('*/primary/pcad*asol*fits*')
  n=n_elements(afile)
  
  ;; get ra/dec to convert to x,y
  readcol,'~/Chandra/positions.txt',grbs,ra,dec,posinfo,trigtime,format='(a,a,a,a,d)',skipline=1,delim=','
  match,grb,grbs,m1,m2
  n=n_elements(afile)
  print,grbs[m2],trigtime[m2]
  ra=ra[m2[0]]
  dec=dec[m2[0]]

  for i=0,n-1 do begin
     sep=str_sep(afile[i],'/')
     newafile=sep[0]+'/reg_'+sep[1]
     arfname=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.arf'
     arfcorrname=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf'
     
     outfile='coords.txt'
     ;;; dmcoords
     com1='/Users/jracusin/CIAO/ciao-4.6/bin/dmcoords '+afile[i]+' asolfile='+asolfiles[i]+' opt=cel ra="'+ra+'" dec="'+dec+'" verbose=1 > '+outfile
     print,com1
     spawn,com1
 
     readcol,outfile,what,x,y,format='(a,f,f)',delim=' ',/silent
     w=where(strtrim(what,2) eq 'SKY(X,Y):')
     x=x[w]
     y=y[w]
     
;     com3='/Users/jracusin/CIAO/ciao-4.6/bin/dmcopy
;     "'+afile[i]+'[(x,y)=circle('+ntostr(x)+','+ntostr(y)+',50)]"
;     outfile='+newafile[i]+' clobber=yes'
     com3='/Users/jracusin/CIAO/ciao-4.6/contrib/bin/src_psffrac '+afile[i]+' "circle('+ntostr(x)+','+ntostr(y)+',3)" psffrac_'+ntostr(i+1)+'_'+ntostr(rad)+'.fits clobber=yes'
     print
     print,'RUN AT COMMAND LINE'
     print,com3
;     spawn,com3
     k=get_kbrd(10)

     arfname=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.arf'
     arf=mrdfits(arfname,1,hdr)
     psffrac=mrdfits('psffrac_'+ntostr(i+1)+'_'+ntostr(rad)+'.fits',1)
     r=psffrac.psffrac
     plot,arf.energ_lo,arf.specresp
     oplot,arf.energ_lo,arf.specresp*r,color=!red
     arf.specresp=arf.specresp*r
     arfname2=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf'
     mwrfits,arf,arfname2,hdr,/create

    
;     com4='/Users/jracusin/CIAO/ciao-4.6/bin/arfcorr infile='+newafile[i]+' arf='+arfname+' outfile='+arfcorrname+' region="circle('+ntostr(x)+','+ntostr(y)+',3)" x='+ntostr(x)+' y='+ntostr(y)+' energy=0.0 clobber=yes verbose=2'
;     print,com4
;     spawn,com4
  endfor 


;     arfcorr infile=16440/acis_evt2_clean.fits arf=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.arf' outfile=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf' region="circle(4082.5806,4113.4806,3)" x=4082.5806 y=4113.4806 energy=0.0
return
end

pro psf_mkarf_old,grb,rad=rad
  
  cd,!adata+'Chandra/'+grb
  afile=file_search('*/acis_evt2_clean.fits')
  asolfiles=file_search('*/primary/pcad*asol*fits')
  n=n_elements(afile)
  readcol,'~/Chandra/positions.txt',grbs,ra,dec,posinfo,trigtime,format='(a,a,a,a,d)',skipline=1,delim=','
  match,grb,grbs,m1,m2
  n=n_elements(afile)
  print,grbs[m2],trigtime[m2]
  ra=ra[m2[0]]
  dec=dec[m2[0]]
  if n_elements(rad) eq 0 then rad=2
;  run_command,/init,param_dir='~/pfiles/'
;  setenv,'CALDB='

  for i=0,n-1 do begin
     
     outfile='coords.txt'
     ;;; dmcoords
     com1='/Users/jracusin/CIAO/ciao-4.6/bin/dmcoords '+afile[i]+' asolfile='+asolfiles[i]+' opt=cel ra="'+ra+'" dec="'+dec+'" verbose=1 > '+outfile
;     print,com1
;     spawn,com1

     com2='/Users/jracusin/CIAO/ciao-4.6/bin/pget ~/pfiles/dmcoords.par theta phi > theta_phi.dat'
;     print,com2
;     spawn,com2

     print,'run the following in a terminal'
     print,com1
     print,com2
     print,'type key to continue'
     k=get_kbrd(100)

     readcol,'theta_phi.dat',par,format='(d,d)'
     theta=par[0]
     phi=par[1]
     print,theta,phi
     pix2arcsec=0.492 ;;; arcseconds per pixel

;arfcorr infile arf outfile region x y [energy] [e_step] [radlim]
;[nsubpix] [nfracpix] [ecffile] [clobber] [verbose] [mode]

     ;;; new version of arf correction from chandra, NEED TO MAKE
     ;;; GENERAL, RATHER THAN DOING SLSH BELOW
;     arfcorr infile=16440/acis_evt2_clean.fits arf=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.arf' outfile=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf' region="circle(4082.5806,4113.4806,3)" x=4082.5806 y=4113.4806 energy=0.0

     ;;; slsh psfFrac
     arfname=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'.arf'
     if exist (arfname) then begin 
        arfname2=grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+'_corr.arf'
        print,arfname
        arf=mrdfits(arfname,1,hdr)
        mm=minmax(arf.energ_lo)
        narf=n_elements(arf)
        ii=indgen(narf)
        diff=mean(arf[ii[1:*]].energ_lo-arf[ii[0:max(ii)-1]].energ_lo)
        energy='['+ntostr(mm[0])+':'+ntostr(mm[1]+diff)+':'+ntostr(diff,5)+'];'
        help,arf
        arffile='arf_corr'+ntostr(rad)+'.dat'
;        if exist(arffile) then spawn,'rm '+arffile
        openw,lun,'slsh_script.sl',/get_lun
        printf,lun,'#!/usr/bin/env /Users/jracusin/CIAO/ciao-4.6/bin/slsh'
        printf,lun
        printf,lun,'require("psf");' 
        printf,lun,'variable psf=psfInit("/Users/jracusin/CALDB/data/chandra/default/reef/hrmaD1996-12-20reefN0001.fits");'
        printf,lun,'provide ("psfFrac");'
        printf,lun,'variable frac = Double_Type[0];'
        printf,lun,'variable energy='+energy
        printf,lun,'variable theta='+ntostr(theta)+';' 
        printf,lun,'variable phi='+ntostr(phi)+'; '
        printf,lun,'variable radius='+ntostr(rad*pix2arcsec,4)+'; '
        printf,lun,'variable frac=array_map(Double_Type, &psfFrac, psf, energy, theta, phi, radius); '
;     printf,lun,'print(frac);'  ;; NEED FILE OUTPUT
        printf,lun,'variable pFile;'
        printf,lun,'pFile = fopen ("'+arffile+'","a") ;'
        printf,lun,'fwrite (frac,pFile)     ;'
        printf,lun,'fclose (pFile);'
        close,lun
        free_lun,lun
        com3='chmod u+x slsh_script.sl'
        com4='./slsh_script.sl'

        print,'run the following in a terminal'
        print,com3
        print,com4
        print,'type key to continue'
        k=get_kbrd(100)

        r=read_binary(arffile,dimensions=dim,data_type=5,data_dim=1078)
;     plot,arf.energ_lo,r,psym=3,/yno,/xlog,yrange=[0.7,1.0],xrange=[0.1,10],$
;          xtitle='Energy (keV)',ytitle='Enclosed energy frac'
        plot,arf.energ_lo,arf.specresp
        oplot,arf.energ_lo,arf.specresp*r,color=!red

        arf.specresp=arf.specresp*r
        mwrfits,arf,arfname2,hdr,/create
;        stop
     endif
  endfor 
;dmcoords 7567/acis_evt2_clean.fits asolfile=7567/primary/pcadf290433184N002_asol1.fits opt=cel ra="06:21:31.80" dec="-62:22:12.37" verbose=1
;pget dmcoords theta phi

;slsh - psfFrac as function of energy from arf

;dmtcalc - to modify arf as psfFrac tells - add column for r and then
;          multiply them?

;;then proceed as planned!
  
  return
end 

pro psf_corr_cxo,grb,rad=rad

  cd,!adata+'Chandra/'+grb
;  mfile='events_merged_'+grb+'.fits'  
;  if not exist(mfile) then 

  readcol,'~/Chandra/positions.txt',grbs,ra,dec,posinfo,trigtime,format='(a,a,a,a,d)',skipline=1,delim=','
  match,grb,grbs,m1,m2
  n=n_elements(files)
  print,grbs[m2],trigtime[m2]
  ra=ra[m2[0]]
  dec=dec[m2[0]]
  if n_elements(rad) eq 0 then rad=2
  srcreg='src_'+ntostr(rad)+'.reg'
;  if not exist(srcreg) then $
     write_regfile,srcreg,ra,dec,ntostr(rad)+'p'

;  if not exist('bg.reg') then $
  tmpra=str_sep(ra,':')
  bgra=tmpra[0]+':'+tmpra[1]+':'+ntostr(tmpra[2]*1.+1.)
  write_regfile,'bg.reg',bgra,dec,'20p','5p',/ann

  afile=file_search('*/acis_evt2_clean.fits')
  asolfiles=file_search('*/primary/pcad*asol*fits*')
  n=n_elements(afile)
;  skyx=fltarr(n) & skyy=skyx & bgskyx=skyx & bgskyy=skyx
;  openw,lun,'skyreg.batch',/get_lun
;  for i=0,n-1 do begin 
;     sky_region,afile[i],asolfiles[i],ra,dec,sx,sy,'sky_reg'+ntostr(i+1)+'.txt',com=com1
;     sky_region,afile[i],asolfiles[i],bgra,dec,bysx,bgsy,'bgsky_reg'+ntostr(i+1)+'.txt',com=com2
;     printf,lun,com1
;     printf,lun,com2
;     skyx[i]=sy & skyy[i]=sy & bgskyx[i]=bysx & bgskyy[i]=bgsy
;  endfor 
;  close,lun
;  free_lun,lun
;  spawn,'chmod u+x skyreg.batch'

;  print
;  print,'*************** RUN skyreg.batch **************************'
;  stop

;  mfiles=file_search('*/acis_evt2_clean.fits');+'[sky=circle('+skyx+','+skyy+','+ntostr(rad)+')]';region('+srcreg+')]';circle('+ra+','+dec+','+ntostr(rad)+')]'
;  mfiles=file_search('*/acis_evt2_clean.fits')+'[sky=region('+srcreg+')]';circle('+ra+','+dec+','+ntostr(rad)+')]'
;  bgfile=file_search('*/acis_evt2_clean.fits');+'[sky=annulus('+bgskyx+','+bgskyy+',5,20)]';region(bg.reg)]';=annulus('+ra+','+dec+',5,20)]'
  mfiles=file_search('*/acis_evt2_clean.fits')+'[sky=region('+srcreg+')]'
  bgfiles=file_search('*/acis_evt2_clean.fits')+'[sky=region(bg.reg)]';=annulus('+ra+','+dec+',5,20)]'
  pbkfiles=file_search('*/secondary/acis*pbk*fits*')
  mskfiles=file_search('*/secondary/acis*msk1.fits*')

;  spawn,'ciao'
  n=n_elements(mfiles)
  for i=0,n-1 do begin 
;     sky_region,afile[i],asolfiles[i],ra,dec,skyx,skyy,'sky_reg'+ntostr(i+1)+'.txt',/readsky
;     sky_region,afile[i],asolfiles[i],bgra,dec,bgskyx,bgskyy,'bgsky_reg'+ntostr(i+1)+'.txt',/readsky
;     mfile=mfiles[i]+'[sky=circle('+skyx+','+skyy+','+ntostr(rad)+')]'
;     bgfile=mfiles[i]+'[sky=annulus('+bgskyx+','+bgskyy+',5,20)]'
     mfile=mfiles[i]
     bgfile=bgfiles[i]

     com='punlearn specextract'
     com=[com,'pset specextract infile="'+mfile+'"']
     com=[com,'pset specextract bkgfile="'+bgfile+'"'] ;mfile+'[sky=annulus('+ra+','+dec+',5,20)]"']
;     com=[com,'pset specextract bgevents="'+afile[i]+'[sky=annulus('+ra+','+dec+',5,20)]"']
     com=[com,'pset specextract outroot='+grb+'_'+ntostr(i+1)+'_'+ntostr(rad)]
     com=[com,'pset specextract asp="'+asolfiles[i]+'"']
;     com=[com,'pset specextract bgasol=""']
     com=[com,'pset specextract grouptype=NUM_BINS']
     com=[com,'pset specextract binspec=1'] ;;; need to group entire spectrum in 1 bin
;     com=[com,'pset specextract bkg_grouptype=NUM_CTS bkg_binspec=10
;;; then notice flux range and fit actually works!!!
;     com=[com,'pset specextract pbkfile="'+pbkfiles[i]+'"']
     com=[com,'pset specextract dafile=CALDB']
;     com=[com,'pset specextract gtype="NUM_CTS"']
;     com=[com,'pset specextract gspec=20']
     com=[com,'/Users/jracusin/CIAO/ciao-4.6/contrib/bin/specextract clobber=yes']
;;; not doing this anymore

;; using this, but binning doesn't work
     com='/Users/jracusin/CIAO/ciao-4.6/contrib/bin/specextract "'+mfile+'" bkgfile="'+bgfile+'" outroot='+grb+'_'+ntostr(i+1)+'_'+ntostr(rad)+' asp='+asolfiles[i]+' mskfile='+mskfiles[i]+' grouptype=NUM_BINS binspec=1 dafile=CALDB clobber=yes'
     print,'RUN THIS AT THE COMMAND LINE'
     print,com
;     spawn,com
;     colprint,com
;     psfile='specextract'+ntostr(i+1)+'_'+ntostr(rad)+'.txt'
;     writecol,psfile,com
;     spawn,'chmod u+x '+psfile
  endfor 

;  writecol,'psscript_'+ntostr(rad)+'.txt','specextract'+ntostr(indgen(n)+1)+'_'+ntostr(rad)+'.txt'
;  spawn,'chmod u+x psscript_'+ntostr(rad)+'.txt'

;  com='/Users/jracusin/CIAO/ciao-4.6/contrib/bin/specextract "'+mfile[i]+'[time=1000:2000][sky=circle('+ra+','+dec+',1)]" bgevents="'+mfile[i]+'[time=1000:2000][sky=annulus('+ra+','+dec+',5,20)]" root='+grb+' asol="'+ntostrarr(asolfiles[i],',')+'" bgasol="" gtype="BIN" gspec="1:1024:20" pbkfile="'+ntostrarr(pbkfiles[i],',')+'" dafile=CALDB'
;  print,com
;  spawn,com
;endfor
;  stop
  return
end 

pro combine_chandra_obs,g,newg
  newg=create_struct('counts',0.,'bgcts',0.,'netcts',0.,'exposure',0d,$
                     'time',0d,'tstart',0d,'tstop',0d,'smin',0.,'smax',0.,$
                     'rate',0d,'raterr',dblarr(2),'flux',0d,'fluxerr',dblarr(2))
  
;  psffact=0.9 ;; from HRMA response for 2" region
  
  newg.counts=total(g.counts)
  newg.bgcts=total(g.bgcts*g.area/g.bgarea)
  newg.netcts=newg.counts-newg.bgcts
  newg.exposure=total(g.exposure)
  newg.tstart=min(g.tstart)
  newg.tstop=max(g.tstop)
  newg.time=(newg.tstop-newg.tstart)/2.+newg.tstart
  confidlev,newg.bgcts,newg.counts,0.954,smin95,smax95 ;;; 2-sigma for detection
  newg.smin=smin95
  newg.smax=smax95
  newg.rate=newg.netcts/newg.exposure;/psffact

  if smin95 gt 0 then begin 
     if newg.counts lt 15 then begin 
        confidlev,newg.bgcts,newg.counts,0.68,smin,smax ;;; 1-sigma for errors
        newg.raterr[0]=(newg.counts-smin)/newg.exposure ;/psffact
        newg.raterr[1]=(smax-newg.counts)/newg.exposure ;/psffact
;        if smin eq 0 then newg.raterr=0
     endif else begin
        newg.raterr[*]=sqrt(newg.counts+newg.bgcts)/newg.exposure
     endelse 
  endif else begin
     newg.raterr=0
     confidlev,newg.bgcts,newg.counts,0.9973,smin,smax ;;; 3-sigma for ULs
     newg.rate=smax/newg.exposure
  endelse 


  return
end 

pro bin_chandra_data,grb,gg,rad=rad
  
  read_chandra_results,grb,g,rad=rad
  ng=n_elements(g)
  for i=0,ng-1 do begin 
     confidlev,g[i].bgcts*g[i].area/g[i].bgarea,g[i].counts,0.9,smin,smax
     g[i].smin=smin
     g[i].smax=smax
     print,g[i].counts
  endfor 
  s=sort(g.time)
  g=g[s]
  
  day=g.time/86400.
  if grb eq 'GRB060729' then bin=25. else $ ;; 060729
     bin=10.                                ;; all others
  if ng gt 1 then begin
     plothist,day,x,y,bin=bin
     w=where(y gt 0,nw)
  endif else begin
     x=day
     nw=1
     w=0
  endelse 
     
  for i=0,nw-1 do begin
     q=where(day ge x[w[i]]-bin/2. and day le x[w[i]]+bin/2.,nq)
     combine_chandra_obs,g[q],newg
     if i eq 0 then gg=newg else begin
        concat_structs,gg,newg,gg2
        gg=gg2
     endelse 
  endfor 
  
  xrange=[min(gg.tstart),max(gg.tstop)]
  yrange=[min(gg.rate-gg.raterr[0]),max(gg.rate+gg.raterr[1])]
  plot,xrange,yrange,/xlog,/ylog,/nodata,xrange=xrange,yrange=yrange
  plots,gg.time,gg.rate,psym=3
  for i=0,n_elements(gg)-1 do begin
     oplot,[gg[i].tstart,gg[i].tstop],[gg[i].rate,gg[i].rate]
     oplot,[gg[i].time,gg[i].time],[gg[i].rate-gg[i].raterr[0],gg[i].rate+gg[i].raterr[1]]
  endfor 
  
  return
end 

pro read_chandra_results,grb,str,rad=rad,add=add
  
  dir=!adata+'Chandra/'+grb+'/'
  if n_elements(rad) eq 0 then rad=2
  if not keyword_set(add) then add=''
  files=file_search(dir+'*/results'+ntostr(rad)+add+'.dat')
  efiles=file_search(dir+'*/acis_evt2_clean'+add+'.fits')
  readcol,'~/Chandra/positions.txt',grbs,ra,dec,posinfo,trigtime,format='(a,a,a,a,d)',skipline=1,delim=','
  match,grb,grbs,m1,m2
  n=n_elements(files)
  print,grbs[m2],trigtime[m2]
  
  str=create_struct('file','','seq','','counts',0.,'area',0.,'bgcts',0.,'bgarea',0.,$
                    'netcts',0.,'neterr',0.,'exposure',0d,'time',0d,'date','',$
                    'tstart',0d,'tstop',0d,'smin',0.,'smax',0.)
  str=replicate(str,n)
  
  for i=0,n-1 do begin
     file=files[i]
     s=strpos(file,'results')
     dir=strmid(file,s-6,5)
     readcol,file,row,counts,area,bgcts,bgarea,netcounts,neterr,exposure,format='(i,f,d,i,d,d,d,d)',/silent
     str[i].file=file
     str[i].seq=dir
     str[i].counts=counts
     str[i].area=area
     str[i].bgcts=bgcts
     str[i].bgarea=bgarea
     str[i].netcts=netcounts
     str[i].neterr=neterr
     str[i].exposure=exposure
     head=headfits(efiles[i])
     dstart=sxpar(head,'DATE-OBS')
     str[i].date=dstart
     strput,dstart,'-',10
     dstop=sxpar(head,'DATE-END')
     strput,dstop,'-',10
     str[i].tstart=date2met(dstart)-trigtime[m2]
     str[i].tstop=date2met(dstop)-trigtime[m2]
     str[i].time=(str[i].tstop-str[i].tstart)/2.+str[i].tstart

  endfor 
  return
end 

pro run_chandra_analysis,rad=rad
  
  cd,!adata+'/Chandra'
  readcol,'~/Chandra/positions.txt',grb,ra,dec,posinfo,trigtime,format='(a,a,a,a,d)',skipline=1,delim=','
  colprint,indgen(n_elements(grb)),grb,ra,dec
  g=0
  nw=n_elements(grb)
  stop
  
  for i=g,nw-1 do begin 
     print,grb[i]
     chandra_analysis,grb[i],ra[i],dec[i],rad=rad;,/skipprocess
  endfor 
  return
end

pro chandra_merge,grb
  
;  dir=!adata+'chandra/'+grb
;  print,dir
;  cd,dir
;  spawn,'ciao'
  ;;; MERGE ALL CLEANED FILES
  evt2files=file_search('*/acis_evt2_clean.fits')
  asolfiles=file_search('*/primary/pcad*asol*fits')
  
  com='punlearn merge_all'
  com=[com,'pset merge_all evtfile="'+ntostrarr(evt2files,',')+'"']
  com=[com,'pset merge_all refcoord='+evt2files[0]]
  com=[com,'pset merge_all chip=7']
  com=[com,'pset merge_all asol="'+ntostrarr(asolfiles,',')+'"']
  mfile='events_merged_'+grb+'.fits'
  com=[com,'pset merge_all merged='+mfile]
  com=[com,'/Users/jracusin/CIAO/ciao-4.6/contrib/bin/merge_all clobber=yes']
;  for j=0,n_elements(com)-1 do begin
;     print,com[j]
;     spawn,com[j]
;  endfor 
  colprint,com
  writecol,'mergeall.txt',com
  
  
  return
end 
  
pro chandra_analysis,grb,ra,dec,skipextract=skipextract,skipprocess=skipprocess,rad=rad
  
  cd,!adata+'Chandra/'+grb
  
  dir1=file_search('1*')
  dir2=file_search('2*')
  dir3=file_search('3*')
  dir4=file_search('4*')
  dir5=file_search('5*')
  dir6=file_search('6*')
  dir7=file_search('7*')
  dir8=file_search('8*')
  dir9=file_search('9*')

  
  dir=[dir1,dir2,dir3,dir4,dir5,dir6,dir7,dir8,dir9]
  w=where(dir ne '')
  dir=dir[w]
  
  print,dir
  n=n_elements(dir)
  spawn,'ciao'

  if n gt 1 then begin 
     print,'what is i (0-'+ntostr(n-1)+')?'
     k=get_kbrd(10)
     if k eq 0 then i0=0 else i0=k*1
  endif else i0=0
  for i=i0,n-1 do begin 
     print,dir[i]
     cd,dir[i]
     if not keyword_set(skipprocess) then begin 
        acaofffile=file_search('primary/pcad*asol1.fits*')
        infile=file_search('secondary/acisf*'+dir[i]+'*_evt1.fits*')
        badpixfile=file_search('primary/acisf*'+dir[i]+'*_bpix1.fits*')
        mtlfile=file_search('secondary/acisf*'+dir[i]+'*_mtl*fits*')
        
     ;;; LEVEL 1 EVENT FILES
        com='punlearn acis_process_events'
        com=[com,'pset acis_process_events infile='+infile]
        com=[com,'pset acis_process_events outfile=acis_new_evt1.fits']
        com=[com,'pset acis_process_events acaofffile='+acaofffile]
        com=[com,'pset acis_process_events badpixfile='+badpixfile]
        com=[com,'pset acis_process_events mtlfile='+mtlfile]
        com=[com,'pset acis_process_events eventdef=")stdlev1"']
;        com=[com,'pset acis_process_events check_vf_pha=yes']
        com=[com,'pset acis_process_events trail=0.027']
        com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/acis_process_events clobber=yes']
        for j=0,n_elements(com)-1 do begin
           print,com[j]
           spawn,com[j]
        endfor 

     ;;; FILTER GRADES
        com='punlearn dmcopy'
        com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmcopy "acis_new_evt1.fits[EVENTS][grade=0,2,3,4,6,status=0]" acis_flt_evt1.fits clobber=yes']
        for j=0,n_elements(com)-1 do begin
           print,com[j]
           spawn,com[j]
        endfor 
        
     ;;; LEVEL 2 EVTS
        com='punlearn dmcopy'
        fltfile=file_search('secondary/acisf*'+dir[i]+'*_flt1.fits*')
        com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmcopy "acis_flt_evt1.fits[EVENTS][@'+fltfile+'][cols -phas]" acis_evt2.fits clobber=yes']
        for j=0,n_elements(com)-1 do begin
           print,com[j]
           spawn,com[j]
        endfor 
        
     ;;; FILTER ENERGY
        com='punlearn dmcopy'
        com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmcopy "acis_evt2.fits[energy=500:8000]" acis_evt2_clean.fits opt=all clobber=yes']
        for j=0,n_elements(com)-1 do begin
           print,com[j]
           spawn,com[j]
        endfor 
;     cd,'..'
;  endfor 
     endif 
     
     if not keyword_set(skipextract) then begin
        mfile='acis_evt2_clean.fits'
        if n_elements(rad) eq 0 then rad=2
        mfile1='acis_evt2_clean_'+ntostr(rad)+'.fits'

        srcreg='src_'+ntostr(rad)+'.reg'
        radd=ntostr(rad)+'p'
        if not exist(srcreg) then $
           write_regfile,srcreg,ra,dec,radd
;        if not exist('bg.reg') then $
        tmpra=str_sep(ra,':')
        bgra=tmpra[0]+':'+tmpra[1]+':'+ntostr(tmpra[2]*1.+3.)
           write_regfile,'bg.reg',ra,dec,'30p','10p',/ann

        
        com='punlearn dmextract'
        com=[com,'pset dmextract infile="'+mfile+'[bin sky=region(src_'+ntostr(rad)+'.reg)]"'];circle('+ra+','+dec+',10)]"']
        com=[com,'pset dmextract outfile=src_'+mfile1]
        com=[com,'pset dmextract bkg="'+mfile+'[bin sky=region(bg.reg)]"'];annulus('+ra+','+dec+',5,20)]"']
        com=[com,'/Users/jracusin/CIAO/ciao-4.6/bin/dmextract clobber=yes']
        for j=0,n_elements(com)-1 do begin
           print,com[j]
           spawn,com[j]
        endfor
        
;  com='/Users/jracusin/CIAO/ciao-4.6/bin/dmlist "src_'+mfile+'[HISTOGRAM][cols counts]" data'
;  spawn,com
        
        com='/Users/jracusin/CIAO/ciao-4.6/bin/dmlist "src_'+mfile1+'[cols counts,area,bg_counts,bg_area,net_counts,net_err,exposure]" data > results'+ntostr(rad)+'.dat'
        print,com
        spawn,com
     endif 
     cd,'..'
  endfor 
  
  return
end 
