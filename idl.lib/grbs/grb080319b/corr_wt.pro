pro correct_lc,tmin,tmax,ps=ps,lc=lc,bat=bat,nolines=nolines,rate=rate
  
  dir='~/Desktop/GRB080319B/'
  cd,dir
  trigtime=227599971.904;227599969.0
  cd,'psf_corr'

;  tmin=[68.189981,102.19785,132.20277,170.19413,220.18925,260.23553,314.19632,392.18947,538.25537,916.34337]
;  tmax=[102.18361,132.18320,170.18345,220.18747,260.17150,314.18386,392.18057, 538.10948,916.12988,1720.0655]
  
;  specfile='time_specfit.fits'
;  if not exist(specfile) then read_time_specfit,spec,trigtime else $
;     spec=mrdfits(specfile,1)
  read_time_specfit,spec,trigtime
  
  cr=where(spec.nocr eq 0)
  nocr=where(spec.nocr eq 1)
  tmin=spec[cr].tmin
  tmax=spec[cr].tmax
  pcspec=mrdfits('../final_correct/spec_pc_nocr.fits',1)
  tmin=[tmin,pcspec.tmin]
  tmax=[tmax,pcspec.tmax]
;  corrfact=(spec[cr].unabs_flux/spec[cr].model_rate)/(spec[nocr].unabs_flux/spec[nocr].model_rate)
  corrfact=(spec[cr].unabs_flux/spec[cr].rate)/(spec[nocr].unabs_flux/spec[nocr].rate)
;  corrfact=spec[cr].unabs_flux/spec[nocr].unabs_flux
;  corrfact=replicate(1.,10)
  if not keyword_set(rate) then begin
;     fluxfact=[spec[cr].unabs_flux/spec[cr].rate, 4.5891600d-11] 
;     fluxfact=[replicate(4.3e-11,10)*corrfact,5.1e-11] ;;rate
;     fluxfact=[spec[nocr].unabs_flux/spec[nocr].rate*corrfact,5.1e-11]
     fluxfact=[spec[nocr].unabs_flux/spec[nocr].rate*corrfact,pcspec.unabs_flux/pcspec.rate]
;      fluxfact=[replicate(3.9509571e-11,10)*corrfact,5.1e-11] ;;rate
;     fluxfact=[replicate(4.0e-11,10)*corrfact,4.4e-11] ;;model predicted rate
  endif else begin
     fluxfact=[corrfact,replicate(1.,n_elements(pcspec))]
  endelse 

;  corrfact=[13.75113065007,11.6403605490422,12.9368190324395,7.74556160496158,7.81113557087896,7.68796867119808,2.74011056061043,1.10019322856851,1.0498062021229,1]
  
  
;  cd,'..'
  ;;;GET NEW FILES FROM LV
;  file='loredana.qdp'
;  lc=lcout2fits(file,/qdp)
;  wl=where(lc.time lt 1e4)
;  lcl=lc[wl]

;  lcfile0='sw00306757000xwtw2posr.lc'
;  hdr=headfits(lcfile0)
;  xrt_t0=sxpar(hdr,'TSTART')
  
;  t=0. & ctr=0. & tstart=0. & tstop=0. & ctrerr=0. & exptime=0.
;  for i=1,10 do begin
;     lcfile='Loredana/lc'+ntostr(i)+'.dat'
;     lcfile='lcdir/lc'+ntostr(i)+'.dat'
;     readcol,lcfile,time,halfbin,lctr,lctrerr,exp,format='(f,f,f,f,f)'
;     t=[t,time+tmin[i-1]+halfbin];xrt_t0-trigtime]
;     ts=time-halfbin+tmin[i-1];xrt_t0-trigtime
;     tstart=[tstart,ts]
;     tt=time+halfbin+tmin[i-1];xrt_t0-trigtime
;     tstop=[tstop,tt]
;     ctr=[ctr,lctr]
;     ctrerr=[ctrerr,lctrerr]
;     exptime=[exptime,exp*(tt-ts)]
;  endfor 
;  nl=n_elements(t)-1
  

;  tmin=fltarr(10) & tmax=fltarr(10)
;  for i=0,9 do begin 
;     lcfile='../Loredana/seg'+ntostr(i+1)+'wt.evtsr_corr.lc'
;     lc1=mrdfits(lcfile,1,hdr)
;     xrt_t0=sxpar(hdr,'TSTART')
;     lc1.time=lc1.time+xrt_t0-trigtime
;     tmin[i]=lc1[0].time-0.5
;     tmax[i]=max(lc1.time)+0.5
;     if i eq 0 then lc0=lc1 else begin 
;        concat_structs,lc0,lc1,lcl
;        lc0=lcl
;     endelse
                                ; endfor 
  if keyword_set(bat) then begin 
     readcol,dir+'bat_in_xrt.qdp',time,halfbin,flux,fluxerr,format='(f,f,f,f)'
     nb=n_elements(time)
     lcb=lcout2fits(/empty)
     lcb=replicate(lcb,nb)
     lcb.time=time
     lcb.tstart=time-halfbin
     lcb.tstop=time+halfbin
     lcb.src_rate=flux
     lcb.src_rate_err=fluxerr
     lcb.type=2
  endif 
  
;  if keyword_set(konus) then begin
;     readcol,dir+'GRB080319B_KW_BG.dat',timekw,time,
     
  lcl=mrdfits('lcdir/lc_excl.fits',1)
  nl=n_elements(lcl)
  lc=lcout2fits(/empty)
  lc=replicate(lc,nl)
  
  lc.time=lcl.time
;  lc.tstart=lcl.time-0.5
;  lc.tstop=lcl.time+0.5
  lc.tstart=lcl.tstart          ;time-2.5
  lc.tstop=lcl.tstop            ;time+2.5
  lc.src_rate=lcl.rate-1.0 ;; mean bg rate for late LC when source not dominating
  lc.src_rate_err=lcl.error
  lc.exptime=(lc.tstop-lc.tstart)*lcl.fracexp
  
;  lc.time=t[1:*]
;  lc.tstart=tstart[1:*]
;  lc.tstop=tstop[1:*]
;  lc.src_rate=ctr[1:*]
;  lc.src_rate_err=ctrerr[1:*]
;  lc.exptime=exptime[1:*]
  wm=where(lc.time lt 3000)
  lcm=lc[wm]
;  lc=lcout2fits('lc_newout_exlc.txt')
  
;  lcl=lcout2fits('../Loredana/curve.qdp',/qdp)
;  readcol,'../Loredana/loredana_pc_orb12_final.qdp',time,halfbin,rate,raterr,format='(f,f,f,f)'
  readcol,'../Loredana/curve.qdp',time,halfbin,halfbin2,crate,raterr,format='(f,f,f,f)'
  nl=n_elements(time)
  lcl=lcout2fits(/empty)
  lcl=replicate(lcl,nl)
  lcl.time=time
  lcl.tstart=time-halfbin
  lcl.tstop=time+halfbin
  lcl.src_rate=crate
  lcl.src_rate_err=raterr
  lcl.type=1
  
  wl=where(lcl.time gt 4000 and lcl.tstop lt 1e5)    ; and lcl.time lt 13000.)
  lcl=lcl[wl]
  
  
;;  READ LOREDANA'S CORRETED 1ST 2 ORB PC
;;   lcl1=mrdfits(dir+'Loredana/orbit1.evtsr_corr.lc',1,hdr)
;;   xrt_t0=sxpar(hdr,'TSTART')
;;   lc1=lcout2fits(/empty)
;;   lc1=replicate(lc1,n_elements(lcl1))
;;   lc1.time=lcl1.time+xrt_t0-trigtime
;;   lc1.tstart=lcl1.time+xrt_t0-trigtime-2.51
;;   lc1.tstop=lcl1.time+xrt_t0-trigtime+2.51
;;   lc1.src_rate=lcl1.rate
;;   lc1.src_rate_err=lcl1.error
;;   lc1.exptime=(lc1.tstop-lc1.tstart)*lcl1.fracexp
  
;;   lcl2=mrdfits(dir+'Loredana/orbit2.evtsr_corr.lc',1,hdr)
;;   xrt_t0=sxpar(hdr,'TSTART')
;;   lc2=lcout2fits(/empty)
;;   lc2=replicate(lc2,n_elements(lcl2))
;;   lc2.time=lcl2.time+xrt_t0-trigtime
;;   lc2.tstart=lcl2.time+xrt_t0-trigtime-2.51
;;   lc2.tstop=lcl2.time+xrt_t0-trigtime+2.51
;;   lc2.src_rate=lcl2.rate
;;   lc2.src_rate_err=lcl2.error
;;   lc2.exptime=(lc2.tstop-lc2.tstart)*lcl2.fracexp
  
;;   concat_structs,lc1,lc2,lcl
;;   lcl.type=1
;;   wx=where(lcl.src_rate ne 0 and lcl.src_rate-lcl.src_rate_err ne 0)
;;   lcl=lcl[wx]
  
  concat_structs,lcm,lcl,lcml
  
  cd,'..'
;  lc=lcout2fits(pcfile='PCCURVE_old.qdp',wtfile='blah',/phil)
;  wp=where(lc.time gt 1.3e4 and lc.time lt 1e5)
;  lcp1=lc[wp]
  lc=lcout2fits(pcfile='PCCURVE.qdp',wtfile='blah',/phil)
  wp=where(lc.tstart gt 1e5)
  lcp=lc[wp]
;  concat_structs,lcp1,lcp2,lcp
  
  concat_structs,lcml,lcp,lc
;  lc=lcml
  
;  oldcorr=[12.0633,12.0617,11.9731,7.07089,7.07130,7.06963,7.06928,1.85293,1.29116,1.29116]
;  lc.src_rate=lc.src_rate
  
  corrctr=fltarr(n_elements(lc)) & correrr=corrctr
  ctr=lc.src_rate
  err=lc.src_rate_err
;  binsize=[2,2,4,4,5,5,5,5,10,20]
;  binsize=[1,1,2,2,2,2,5,5,10,20]
;  binsize=[1,1,1,2,2,3,5,5,5,10]
;  binsize=[1,1,1,1,1,2,2,2,3,5]
  binsize=[1,1,1,2,2,3,2,2,2,5]
  for i=0,n_elements(tmin)-1 do begin
;;      if i eq 0 then w=where(lc.time ge tmin[i] and lc.time le tmax[i]-2.*binsize[i])
;;      if i eq 9 then w=where(lc.time ge tmin[i]+2.*binsize[i] and lc.time le tmax[i])
;;      if i gt 0 and i lt 9 then $
;;         w=where(lc.time ge tmin[i]+2.*binsize[i] and lc.time le tmax[i]-2.*binsize[i])
     
;;      corrctr[w]=ctr[w]*fluxfact[i]
;;      correrr[w]=err[w]*fluxfact[i]
;;      if i gt 0 and i lt 9 then begin 
;;         w1=where(lc.time ge tmin[i]-2.*binsize[i] and lc.time le tmin[i]+2.*binsize[i])
;;         corrctr[w1]=ctr[w1]*(fluxfact[i-1]+fluxfact[i])/2.
;;         correrr[w1]=err[w1]*(fluxfact[i-1]+fluxfact[i])/2.
;;         w2=where(lc.time ge tmax[i]-2.*binsize[i] and lc.time le tmax[i]+2.*binsize[i])
;;         corrctr[w2]=ctr[w2]*(fluxfact[i]+fluxfact[i+1])/2.
;;         correrr[w2]=err[w2]*(fluxfact[i]+fluxfact[i+1])/2.
;;      endif
     
     w=where(lc.time ge tmin[i] and lc.time le tmax[i])
     corrctr[w]=ctr[w]*fluxfact[i]
     correrr[w]=err[w]*fluxfact[i]
     help,w
  endfor 
;  w=where(lc.tstart gt 3000)
;  corrctr[w]=ctr[w]*fluxfact[i]
;  correrr[w]=err[w]*fluxfact[i]
  lc.src_rate=corrctr
  lc.src_rate_err=correrr
  
  bb=''
  if keyword_set(bat) then begin 
     concat_structs,lcb,lc,tmp
     lc=tmp
     bb='_bat'
;     lc=lc[1:*]
  endif 
  rt=''
  if keyword_set(rate) then rt='_rate'
  
  convert_phil2lcout,file,/qdp,outfile='correct_xrt_lc'+bb+rt+'.txt',lc=lc  
  if keyword_set(ps) then begplot,name='GRB080319B_xrt_lc'+bb+'.ps',/color,/land
;  plot_like_qdp,lc=lc,file='correct_xrt_lc.txt',/flux;,xrange=[0.1,1e6]
                                ;,xrange=[10,1e4],yrange=[10,1e4]
  
  if keyword_set(bat) then begin
     bbat=where(lc.type eq 2,nbat)
     xrange=[1,1e7]
     yrange=[1e-14,1e-5]
  endif else begin
     xrange=[10,1e7]
     if not keyword_set(rate) then begin 
        yrange=[1e-14,1e-6]
     endif else yrange=[1e-4,1e4]
  endelse
  wt=where(lc.type eq 0,nwt)
  pc=where(lc.type eq 1,npc)
  plot,xrange,yrange,/nodata,xtitle='Time since BAT trigger (s)',ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,xrange=xrange,/xsty,yrange=yrange,/ysty,ytickinterval=1,yminor=9
  oploterror,lc[wt].time,lc[wt].src_rate,lc[wt].src_rate_err,psym=3,color=!blue,/nohat,errcolor=!blue
  for i=0,nwt-1 do oplot,[lc[wt[i]].tstart,lc[wt[i]].tstop],[lc[wt[i]].src_rate,lc[wt[i]].src_rate],color=!blue
  oploterror,lc[pc].time,lc[pc].src_rate,lc[pc].src_rate_err,psym=3,color=!red,/nohat,errcolor=!red
  for i=0,npc-1 do oplot,[lc[pc[i]].tstart,lc[pc[i]].tstop],[lc[pc[i]].src_rate,lc[pc[i]].src_rate],color=!red
  ul=where(lc.src_rate_err eq 0,nul)
  if nul gt 0 then begin 
     plotsym,1,3,thick=3
     plots,lc[ul].time,lc[ul].src_rate,psym=8,color=!red
  endif 
     
  if keyword_set(bat) then begin 
     oploterror,lc[bbat].time,lc[bbat].src_rate,lc[bbat].src_rate_err,psym=3,/nohat
     for i=0,nbat-1 do oplot,[lc[bbat[i]].tstart,lc[bbat[i]].tstop],[lc[bbat[i]].src_rate,lc[bbat[i]].src_rate]
  endif 
  
  if keyword_set(rate) then imfact=1./4.8866e-11 else imfact=1.
  xim=1.51e-7*imfact
  ximerr=0.2e-7*imfact
  oploterror,60.,xim,0.05,ximerr,errcolor=!green,/nohat
  
;  if keyword_set(bat) then begin 
;     oploterror,lcb.time,lcb.src_rate,lcb.src_rate_err,psym=3,/nohat
;     for i=0,n_elements(lcb)-1 do begin 
;        oplot,[lcb[i].tstart,lcb[i].tstop],[lcb[i].src_rate,lcb[i].src_rate]
;     endfor 
;  endif 
  
  if not keyword_set(nolines) then begin 
     for i=0,9 do oplot,[tmin[i],tmin[i]],[1e-14,1e5],line=1
     oplot,[tmax[9],tmax[9]],[1e-14,1e5],line=1
  endif 
  if keyword_set(ps) then endplot

;  ploterror,lc.time,corrctr,lc.src_rate_err,/xlog,/ylog,psym=3,/nohat
;  for i=0,n_elements(times)-1 do oploterror,[lc[i].tstart,lc[i].tstop],[corrctr[i],corrctr[i]]
  
  if keyword_set(bat) then begin 
     type=strarr(n_elements(lc))
     type[bbat]='   bat'
     type[wt]='    wt'
     type[pc]='    pc'
     writecol,'bat_xrt_lc.dat',lc.time,(lc.tstop-lc.tstart)/2.,lc.src_rate,lc.src_rate_err,type,header='time       tbin       flux       fluxerr      type'
  endif else begin
     type=strarr(n_elements(lc))
     type[wt]='    wt'
     type[pc]='    pc'
;     x=2.0678346d8
     writecol,'xrt_lc.dat',lc.time,(lc.tstop-lc.tstart)/2.,lc.src_rate,lc.src_rate_err,type,header='time       tbin       flux       fluxerr      type'
;     writecol,'xrt_lc.dat',lc.time,(lc.tstop-lc.tstart)/2.,lc.src_rate*x,lc.src_rate_err*x,header='time       tstart      tstop       flux       fluxerr'
  endelse 
  
  stop
  return
end 
    
pro read_time_specfit,specstr,trigtime,dir=dir,datfiles=datfiles,add=add,nocr=nocr
  
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
  
  for j=0,10-1 do begin
     for k=0,1 do begin
        i=j*2+k
        print,i
        if k eq 0 then add='_nocr' else add=''
;     file=datfiles[i]
;     segpos=strpos(file,'seg')
;     seg=strmid(file,segpos+3,2)
;     spos=strpos(seg,'w')
;     wpos=where(spos ne -1,nwpos)
;     if nwpos gt 0 then seg=strmid(seg,0,1)
     
     seg=j+1
     file=file_search('seg'+ntostr(seg)+'_*wt_Pow'+add+'.dat')
     if not exist(file) then file=file_search('seg'+ntostr(seg)+'_*pc_Pow'+add+'.dat')
     hdr=headfits('seg'+ntostr(seg)+'_*.evt')
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
  endfor      
  endfor 
;  specstr=specstr[sort(specstr.seg)]
  mwrfits,specstr,'time_specfit.fits',/create
  return
end 

pro corr_wt
  
  cd,'~/Desktop/GRB080319B/psf_corr/'
  
  gphafiles=file_search('seg*pha.grpmin*0')
  xspecfiles=file_search('xspec*batch')
  arffiles=file_search('seg*wt.arf')
  phafiles=file_search('seg*wt.pha')
  bpos=strpos(phafiles,'bg')
  expofiles=file_search('seg*ex.img')
  w=where(bpos eq -1)
  phafiles=phafiles[w]
  
  n=n_elements(phafiles)
  
  seg=strmid(gphafiles,3,2)
  spos=strpos(seg,'_')
  ws=where(spos ne -1)
  seg[ws]=strmid(seg[ws],0,1)
  s=sort(seg*1.)
  
  gphafiles=gphafiles[s]
  xspecfiles=xspecfiles[s]
  arffiles=arffiles[s]
  phafiles=phafiles[s]
  expofiles=expofiles[s]
  
  colprint,gphafiles,xspecfiles,arffiles,phafiles
  srcx='-1';'297'
  srcy='-1';'299'
  
  for i=0,n-1 do begin
     print,'Seg ',i+1
     xbfile=xspecfiles[i]
     
     phaname=phafiles[i]
     
     xrtmkarf='xrtmkarf outfile='+arffiles[i]+' phafile='+phaname+' srcx='+srcx+' srcy='+srcy+' psfflag=yes expofile= '+expofiles[i]+' clobber=yes > xrtmkarf'+ntostr(i+1)+'.out'
     spawn,xrtmkarf
     
     apos=strpos(arffiles[i],'.arf')
     newarf=strmid(arffiles[i],0,apos)+'_nocorr.arf'
     xrtmkarf='xrtmkarf outfile='+newarf+' phafile='+phaname+' srcx='+srcx+' srcy='+srcy+' psfflag=no clobber=yes > xrtmkarf'+ntostr(i+1)+'_nocr.out'
     print,xrtmkarf
     spawn,xrtmkarf
    
     newpha=gphafiles[i]+'.nocr'
     print,newpha
     cp='cp '+gphafiles[i]+' '+newpha
     spawn,cp
     
     fparkey='fparkey '+newarf+' '+newpha+'+1 ancrfile'
     print,fparkey
     spawn,fparkey
     
     print,'Re-running old XSPEC'
     ;;add modpredctr to old batch file
     readcol,xbfile,lines,format='(a)',delim='@'
     nlines=n_elements(lines)
;     newline='newpar 2 0 1 0 0 1e4 1e4'
     newline='newpar 2 0.085 0'
     lines=[lines[0:11],newline,lines[12:*]]
;     lines=[lines[0:nlines-16],newline,lines[nlines-15:*]]
;     newline='puts $fileid "model_rate [lindex $crte 2]"'
;     lines=[lines[0:nlines-3],newline,lines[nlines-2:*]]
;     lines[nlines-3]=newline
;     lines=[lines[0:11],lines[13:*]]
     xbfile0=xbfile+'0'
     writecol,xbfile0,lines
     spawn,'xspec - '+xbfile0+' > xspec'+ntostr(i+1)+'.out'
     
     print,'Running new xspec'
     newxbfile=xbfile+'.nocr'
     print,newxbfile
     newline='data '+newpha
     lines[0]=newline
     ppos=strpos(lines,'Pow.dat')
     wpos=where(ppos ne -1)
     newdat=strmid(lines[wpos[0]],0,ppos[wpos])+'Pow_nocr.dat'
     print,newdat
     lines[wpos[0]]=newdat+' w] '
     
     df=file_search('seg'+ntostr(i+1)+'_*Pow.dat')
     readcol,df,pname,p,format='(a,a)'
     wp=where(pname eq 'PhInd')
     wn=where(pname eq 'nH')
     newpline='newpar 4 '+p[wp]+' 0'
;     newnline='newpar 2 '+p[wn]+' 0'
;     newnline='newpar 2 0.085 0'

     lines=[lines[0:11],newpline,lines[12:*]]
;     lines=[lines[0:11],newpline,newnline,lines[12:*]]
     writecol,newxbfile,lines

     spawn,'xspec - '+newxbfile+' > xspec'+ntostr(i+1)+'_nocr.out'

  endfor 
  
  correct_lc
  return
end 
