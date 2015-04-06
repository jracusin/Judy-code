pro get_optcat,grbra,grbdec,optcat,sdss=sdss,usno=usno,twomass=twomass,dposs=dposs,dss=dss
  
  ;;first try sdss
  optcat='sdss.csv'
  if not exist(optcat) then begin 
     com='sqlcl.py -f csv -q "select """" p.ra,p.dec from fGetNearbyObjEq('+ntostr(grbra)+','+ntostr(grbdec)+',15) n, PhotoPrimary p where n.objID=p.objID" > '+optcat
     print,'Searching SDSS dr5 for data'
     print,com
     spawn,com
  endif else print,'sdss.csv already exists, using it'
  sdss=1
  if numlines(optcat) lt 10 then begin 
     ;;then try usno
     optcat='usno.txt'
     print,'Object not in SDSS, searching USNO-B1'
     if not exist(optcat) then grab_usno,grbra,grbdec else $
        print,'usno.txt already exists, using it'
     usno=1
     sdss=0
  endif 
      
  ;;probably don't need to bother with others
    
  return
end 

pro discard_data_point,x,xerr,y,yerr,sig,w

  nsrc=n_elements(x)
  sdx=dblarr(nsrc) & sdy=sdx
  for i=0,n_elements(x)-1 do begin
     ind=intarr(nsrc)
     ind[i]=1
     w=where(ind eq 0)
     sdx[i]=sqrt((total(x[w]^2/xerr[w]^2)/total(1./xerr[w]^2)-(total(x[w]/xerr[w]^2)/total(1./xerr[w]^2))^2)*nsrc/(nsrc-1d))
     sdy[i]=sqrt((total(y[w]^2/yerr[w]^2)/total(1./yerr[w]^2)-(total(y[w]/yerr[w]^2)/total(1./yerr[w]^2))^2)*nsrc/(nsrc-1d))
  endfor

  msdx=weighted_mean(sdx,xerr)
  msdy=weighted_mean(sdy,yerr)
  tsdx=sqrt((total(sdx^2/xerr^2)/total(1./xerr^2)-(total(sdx/xerr^2)/total(1./xerr^2))^2)*nsrc/(nsrc-1d))
  tsdy=sqrt((total(sdy^2/yerr^2)/total(1./yerr^2)-(total(sdy/yerr^2)/total(1./yerr^2))^2)*nsrc/(nsrc-1d))

  sig2=1.;sig
  m=where(sdx lt msdx-sig2*tsdx or sdy lt msdy-sig2*tsdy,nm)
;  msd=max(sd,m)
  nw=nsrc
  if nm gt 0 then begin
     ind=intarr(nsrc)
     ind[m]=1
     w=where(ind eq 0,nw)
;     x=x[w]
;     xerr=xerr[w]
;     y=y[w]
;     yerr=yerr[w]
  endif else w=indgen(nsrc)
;  n=where(ind eq 1)
  if nsrc ne nw then print,'removed data points: '+ntostr(nsrc-nw)

  return
end


pro filter_events,evtfiles,fevtfiles,exfiles,fexfiles,phamin=phamin,phamax=phamax,suffix=suffix,use=use,indir=indir

  if n_elements(phamin) eq 0 then phamin=20
  if n_elements(phamax) eq 0 then phamax=1000
  if n_elements(indir) eq 0 then indir=''
  
  use=1
  fevtfiles='';strarr(nfiles)
  fexfiles=fevtfiles
  
;  for i=0,nfiles-1 do begin

;     fevtfiles=evtfiles+'.filt'
  file=evtfiles
  ev=mrdfits(evtfiles,1,/silent)
  
  if n_elements(ev) gt 1 then begin 
     
     targtmp2=str_sep(exfiles,'_ex.img')
     targtmp=targtmp2[0]
     stemout=targtmp+suffix     ;exfiles+suffix
     if suffix eq '_f100' then begin 
     ;;;filter out first 100s
        outfile=file+suffix
        
        dir=str_sep(file,'/')
        if n_elements(dir) gt 1 then begin 
           hkfile=findfile(dir[0]+'/*xhdtc.hk')
        endif else begin
           targtmp1=str_sep(file,'sw')
           targtmp2=str_sep(targtmp1[1],'x')
           targtmp=targtmp2[0]
           hkfile=findfile('*'+targtmp+'*xhdtc.hk')
        endelse 
        
        hk=mrdfits(hkfile[0],1)
        ind=lindgen(n_elements(hk))
        p=hk[ind[1:*]].settled-hk[ind-1].settled*1
        w=where(p eq 128,nw)
        if nw eq 0 then w=0
        wend=where(p eq -128,nwend)
        gtistart=hk[w].hktime+100.
        gtistop=hk[wend].hktime
        if nw gt nwend then gtistop=[gtistop,gtistop[nwend-1]+10000]
        if nw lt nwend then gtistart=[min(hk.hktime),gtistart]
        writecol,'filter_settled.dat',gtistart,gtistop,delimiter=' '
        openw,lun,'xsel_filt100.xco',/get_lun
        printf,lun
        printf,lun
        printf,lun,'clear all'
        printf,lun,'yes'
        printf,lun,'read e '+file
        printf,lun,'./'
        printf,lun,' '
        printf,lun,'filter time file filter_settled.dat'
        printf,lun,'ext ev'
        printf,lun,'save ev '+outfile
        printf,lun,'yes'
        printf,lun
        printf,lun,'exit'
        printf,lun
        close,lun
        free_lun,lun
        spawn,'chmod u+x xsel_filt100.xco'
        spawn,'xselect @xsel_filt100.xco'
        com='fappend '+file+'+3 '+outfile
        print,com
        spawn,com
        
        if file_size(outfile) lt 80000d then use=0
        
        ;;create new expo maps
        infile=file+suffix
        targtmp1=str_sep(infile,'xpcw')
        targtmp=targtmp1[0]
        attfile=findfile(indir+targtmp+'*sat.fits*')
        if attfile[0] eq '' then begin
           targtmp1=str_sep(infile,'-xrt')
           targtmp=targtmp1[0]+'/auxil/'
           attfile=findfile(indir+targtmp+'*sat.fits*')
           if attfile eq '' then stop
        endif 
        hdfile=hkfile
        
        com='xrtexpomap infile='+infile+' attfile='+attfile+' hdfile='+hdfile+' outdir=./ stemout='+stemout+' clobber=yes'
        print,com    
        spawn,com 
     endif else infile=file
     
     outfile=infile+'.filt'
     ev=mrdfits(infile,1,hdr1)
     nev=n_elements(ev)
     amp=create_struct('amp',1B)
     amp=replicate(amp,nev)
     openw,lun,'amphead.txt',/get_lun
     printf,lun,"TTYPE1  = 'Amp     '           /"
     close,lun
     free_lun,lun
     mwrfits,amp,'amp.fits',/create
     spawn,'fmodhead infile=amp.fits tmpfil=amphead.txt'
     spawn,'faddcol infile='+infile+' colfile=amp.fits colname=Amp'
     spawn,'xrthotpix clobber=yes iterate=yes cleanflick=yes infile='+infile+' outfile='+outfile+' phamin=0 phamax=4095 overstatus=no hotneigh=yes usegoodevt=yes'
     
     spawn,'rm sw*hp.fits'
     ;;write out new evt files
     hdr0=headfits(outfile,exten=0)
     ev=mrdfits(outfile,1,hdr1)
     if n_elements(ev) gt 1 then begin 
        gti=mrdfits(outfile,2,hdr2)
        bp=mrdfits(outfile,3,hdr3)

;  bprxy=ntostr(bp.rawx)+ntostr(bp.rawy)
        nbp=n_elements(bp)
        keep=intarr(nev)
     
     ;;filter out bad pixels and corner sources and phacut from events
        for j=0,nev-1 do begin
           w=where((ev[j].rawx eq bp.rawx and ev[j].rawy eq bp.rawy) or $
                   ev[j].pha lt phamin or ev[j].pha gt phamax or $
                   (ev[j].rawx lt 70 and ev[j].rawy lt 70) or $
                   (ev[j].rawx lt 70 and ev[j].rawy gt 530) or $
                   (ev[j].rawx gt 530 and ev[j].rawy gt 530),nw)
           if nw eq 0 then keep[j]=1
           
        endfor

        w=where(keep eq 1,nw)
        outev=ev[w]

     ;;write out suffix+.filt ev files
;     outfile=fevtfiles       ;+'2'
        mwrfits,0,outfile,hdr0,/create
        mwrfits,outev,outfile,hdr1
        mwrfits,gti,outfile,hdr2
        mwrfits,bp,outfile,hdr3
     
        fevtfiles=outfile
        fexfiles=stemout+'_ex.img'
        if nw le 1 then begin 
           fevtfiles=''
           fexfiles=''
           use=0
        endif 
     endif else begin
        fevtfiles=''
        fexfiles=''
        use=0
     endelse 
  endif else use=0
;  endfor
;  wuse=where(use eq 1)
  
;  if suffix eq '_f100' then begin 
;     fevtfiles=fevtfiles+suffix
;     fexfiles=fexfiles+suffix
;  endif
  
  return
end


pro compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title,donames=donames,_extra=_extra,nonew=nonew
  
  w=where(ralist ne 0)
  !p.charsize=1
  dras=(ralist[w]-grbra)*3600.*cos(declist[w]*!dtor)
  ddecs=(declist[w]-grbdec)*3600.

  mra=(newra-grbra)*3600.*cos(newdec*!dtor)
  mdec=(newdec-grbdec)*3600.
  
  if keyword_set(donames) then b=2 else b=0
  xrange=[min([dras-errlist[w],mra-newerr])-b,max([dras+errlist[w],mra+newerr])+b]
  yrange=[min([ddecs-errlist[w],mdec-newerr])-b,max([ddecs+errlist[w],mdec+newerr])+b]

  roff=0.2
  doff=1
  xrange=reverse(xrange)
  plot,xrange,yrange,psym=1,/yno,/nodata,/xstyle,/ystyle,/iso,xtitle='RA offset (arcsec)',ytitle='Dec offset (arcsec)',title=title,xrange=xrange
  
;  color=[!green,!yellow,!red,!orange,!blue,!cyan,!magenta,!violet,!forestgreen]
  color=[!green,!cyan,!red,!magenta,!orange,!slategrey,!yellow,!forestgreen]
  
;  fud=[0,0.3,0.1,0.1,0,0,0,0,0,0]
  for i=0,n_elements(dras)-1 do begin
     if errlist[w[i]] ne 0 then tvcircle,errlist[w[i]],dras[i],ddecs[i],color=color[i],/data else $
        plots,dras[i],ddecs[i],color=color[i],psym=2
     if keyword_set(donames) then xyouts,dras[i]+errlist[w[i]]/4.,ddecs[i]+errlist[w[i]]*1.1,names[w[i]],color=color[i],/data;,charsize=0.5
  endfor

  if not keyword_set(nonew) then tvcircle,newerr,mra,mdec,color=!purple,/data
  if keyword_set(donames) then begin
      if not keyword_set(nonew) then $
        xyouts,mra+newerr/2.,mdec+newerr+0.2,'New position',color=!purple,/data ;,charsize=0.5
      arrow,xrange[0]-1.5,yrange[0]+0.5,xrange[0]-0.5,yrange[0]+0.5,/data,hsize=10,thick=2
      arrow,xrange[0]-1.5,yrange[0]+0.5,xrange[0]-1.5,yrange[0]+1.5,/data,hsize=10,thick=2
      xyouts,xrange[0]-1.4,yrange[0]+1.6,'N',/data
      xyouts,xrange[0]-0.2,yrange[0]+0.4,'E',/data
  endif 

  for i=0,n_elements(names[w])-1 do begin 
     sep=separation(newra,newdec,ralist[w[i]],declist[w[i]])
     print,'offset between New & '+names[w[i]]+': '+ntostr(sep)+'  '+ntostr(sep-newerr)+'  '+ntostr(sep-sqrt(newerr^2+errlist[w[i]]^2))
     
     
                                ;print,'offset between New and XRT refined: '+ntostr(separation(newra,newdec,mygcnra,mygcndec))
     
  endfor 

  return
end

pro check_image,expotime,grbra,grbdec,nospawn=nospawn,suffix=suffix,snr=snr,regfile=regfile
  
  write_regfile,'grb.reg',grbra,grbdec,30
  
  imfile='combined_image_wexpo'+suffix+'.fits'
  hdr=headfits(imfile)
  expotime=sxpar(hdr,'ONTIME')

  openw,lun,'xim_checkim.batch',/get_lun
  printf,lun,'read/fits/size=700 '+imfile
  printf,lun,'cpd /cps'
  printf,lun,'disp'
  if n_elements(regfile) eq 0 then begin 
     printf,lun,'detect/snr='+ntostr(snr)
  endif else begin
;     readcol,'wavdet.reg',lines,format='(a)',delim='$'
     
;     openw,lun2,regfile,/get_lun
;     for i=0,n_elements(lines)-1 do begin
;        printf,lun2,lines[i]+' # text = {'+ntostr(i+1)+'}'
;     endfor 
     
;     print,'making new region file'
     
;     readcol,'wavdet.reg',x1,y,format='(a,a)',delim=','
;     openw,lun2,'tmp.reg',/get_lun
;     for i=0,n_elements(y)-1 do begin
;        x2=str_sep(x1[i],'(')
;        x=x2[1]
;        printf,lun2,'text('+x+','+y[i]+',"'+ntostr(i+1)+'")'
;     endfor 
;     close,lun2
;     free_lun,lun2
;     spawn,'cat wavdet.reg tmp.reg > '+regfile
     printf,lun,'circle/disp/color=white/regionfile='+regfile
  endelse 
  printf,lun,'grid/ticks/only'
  printf,lun,'circle/disp/regionfile=grb.reg'
  printf,lun,'exit'
  close,lun
  free_lun,lun
  if not keyword_set(nospawn) then begin
      spawn,'ximage @xim_checkim.batch'
      spawn,'mv pgplot.ps check_image.ps'
  endif

  return
end
pro xrt_astrometry,evtfiles,exfiles,grbra,grbdec,newra,newdec,newerr,ralist,declist,errlist,names,mra,mdec,mraerr,nsrc,expotime,sdra,sddec,wavdetect=wavdetect,twomass=twomass,usno=usno,sdss=sdss,dposs=dposs,dss=dss,dir=dir,iter=iter,filt_hotpix=filt_hotpix,nospawn=nospawn,skipgv=skipgv,suffix=suffix,grb=grb,phamin=phamin,phamax=phamax,snr=snr,optcat=optcat,_extra=_extra,noprompt=noprompt,indir=indir,name=name

  if n_params() eq 0 then begin
     print,'syntax - xrt_astrometry,evtfiles,exfiles,'
     print,'               grbra,grbdec,newgrbra,newgrbdec,newerr'
     print,'               [,/sdss,/usno,/twomass,/dposs,/dss,iter=iter'
     print,'                ,optcat=,snr=,phamin=,phamax=,suffix=,grb=]'
     return
  endif

  ;;get/read reference catalog (sdss?) - what if sdss not covered?
  ;;read evt files, make summed image
  ;;create expomap if none input
  ;;detect sources (ximage or wavdetect)
  ;;centroid on those positions
  ;;match sources
  ;;make offset distribution
  ;;iterate to limit matches
  ;;get weighted mean offset
  ;;apply offset to grb and get new error
  ;;make plots
  
  get_optcat,grbra,grbdec,optcat,sdss=sdss,usno=usno,twomass=twomass,dposs=dposs,dss=dss
  
  
  if n_elements(dir) eq 0 then dir=''
  if n_elements(indir) eq 0 then indir=''
  if n_elements(suffix) eq 0 then suffix=''
  if n_elements(evtfiles) eq 0 or evtfiles[0] eq '' then begin
     evtfiles=file_search(dir+'sw*pc*po*cl*.evt') ;*'+suffix+'*filt')
     ww3=strpos(evtfiles,'w3')
     w3=where(ww3 eq -1)
     evtfiles=evtfiles[w3]
     wff=strpos(evtfiles,'ff')
     ff=where(wff eq -1)
     evtfiles=evtfiles[ff]
  endif 
  
  if n_elements(exfiles) eq 0 or exfiles[0] eq '' then begin
     exfiles=file_search(dir+'sw*pc*po_ex.img')
;     exfiles=file_search(dir+'sw*pc*'+suffix+'*ex.img*')
     ww3=strpos(exfiles,'w3')
     w3=where(ww3 eq -1)
     exfiles=exfiles[w3]
  endif 
  print,evtfiles
  print,exfiles
  if n_elements(evtfiles) ne n_elements(exfiles) then begin
     print,'N evt files != N expo map files'
     stop
     return
  endif
  
  fevtfiles=evtfiles+suffix+'.filt'
  stem=''
  nfiles=n_elements(evtfiles)

  for i=0,nfiles-1 do begin 
     targtmp1=str_sep(fevtfiles[i],'_')
     stem=[stem,targtmp1[0]]
  endfor 
  stem=stem[1:*]
  fexfiles=stem+suffix+'_ex.img'
  
  if keyword_set(filt_hotpix) then begin
     wuse=0
     for i=0,n_elements(fevtfiles)-1 do begin
        use=1
        if exist(fevtfiles[i]) then ev=mrdfits(fevtfiles[i],1,/silent) else ev=0
        
        if not exist(fevtfiles[i]) or (not exist(fexfiles[i]) and n_elements(ev) gt 1) then filter_events,evtfiles[i],fevtfiles[i],exfiles[i],fexfiles[i],phamin=phamin,phamax=phamax,suffix=suffix,use=use,indir=indir 
        if use then wuse=[wuse,i]
     endfor
     wuse=wuse[1:*]
     evtfiles=fevtfiles[wuse]
     exfiles=fexfiles[wuse]
  endif

  ;;grab sdss positions from http://cas.sdss.org/astrodr5/en/tools/search/radial.asp
  if not keyword_set(usno) and not keyword_set(sdss) and not keyword_set(twomass) and not keyword_set(dposs) and not keyword_set(dss) then begin
     print,'Need to specify input catalog type'
     return
  endif
  if keyword_set(usno) then readcol,optcat,blah1,blah2,ra,dec,blah3,format='(a,a,d,d,f)',delim='</TD><TD>',/silent
  if keyword_set(sdss) then readcol,optcat,ra,dec,format='(d,d)',delim=',',/silent ;readcol,optcat,objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z,format='(a,l,i,i,L,i,i,d,d,d,d,d,d,d,d,d,d,d,d)',/silent
  if keyword_set(twomass) then readcol,optcat,ra,dec,format='(d,d)',/silent
  if keyword_set(dposs) then begin

     readcol,optcat,blah,ra,dec,blah2,format='(d,d,d,d)',/silent
     if min(blah) gt 0 and max(blah) lt 360 then begin
        dec=ra
        ra=blah
     endif
  endif
  if keyword_set(dss) then begin
     readcol,optcat,rah,ram,ras,decd,decm,decs,format='(i,i,f,i,i,f)',/silent
     hms2radec,rah,ram,ras,decd,decm,decs,ra,dec
  endif
;  if not keyword_set(usno) and not keyword_set(twomass) then readcol,optcat,objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z,format='(a,l,i,i,L,i,i,d,d,d,d,d,d,d,d,d,d,d,d)',/silent else readcol,optcat,ra,dec,forma

  ;;grab pos err stuff from caldb
  poserr=mrdfits('/bulk/pkg/caldb/data/swift/xrt/bcf/instrument/swxposerr20010101v003.fits',3)
  hpd=poserr.par1
  p=-poserr.par2
  errsys=poserr.errsys

  searchdist=20.                ;search radius in arcsec for finding matches

  if not exist('combined_image_wexpo'+suffix+'.fits') then $
     combine_images,evtfiles,exfiles,outfile='combined_image_wexpo'+suffix ;combines evt files with ximage to make combined_images.fits

  ;;read exposure map files and apply them to image
                                ;  xrtexpomap infile=sw[obsid]x<mode><window><type>cl.evt attfile=sw[obsid]sat.fits hdfile=sw[obsid]xhd.hk outdir=./
  
  if n_elements(snr) eq 0 then snr=3.
  
  if keyword_set(wavdetect) then begin 
     detfile='combined_image_wexpo'+suffix+'.wavdet.fits'
     regfile='combined_image_wexpo'+suffix+'.reg'
     if not exist(detfile) then begin 
        com='ciao;/bulk/pkg/linux/ciao/bin/wavdetect combined_image_wexpo'+suffix+'.fits '+detfile+' cell_images.fits recon_image.fits norm_back.fits expfile=combined_image_wexpo'+suffix+'_ex.fits regfile='+regfile+' psftable="/bulk/pkg/caldb/data/swift/xrt/cpf/psf/swxpsf20010101v003.fits"'
        print,com
        spawn,com
     endif 
  endif else detfile='combined_image_wexpo'+suffix+'.det'

;;;make ps file of im w/o expomap to look for weird sources
  check_image,expotime,grbra,grbdec,nospawn=nospawn,suffix=suffix,snr=snr,regfile=regfile
  
  if expotime lt 10000L then begin
     newra=0
     newdec=0
     newerr=0
     mra=0
     mdec=0
     mraerr=0
     nsrc=0
     sdra=0
     sddec=0
     print,'Not enough exposure time ('+ntostr(expotime)+')'
     return
  endif 
  
;  spawn,'rm src_xrtcentroid.fits'
  imfile='combined_image_wexpo'+suffix+'_corr.fits'
  if keyword_set(wavdetect) then begin
     fsuf='wavdetect' 
     ximage=0
  endif else begin
     fsuf='xrtcentroid'
     ximage=1
     wavdetect=0
  endelse  
  if not exist('src_'+fsuf+suffix+'.fits') then $
     get_centroid_pos,imfile,detfile,src,ximage=ximage,wavdetect=wavdetect,suffix=suffix else $
     src=mrdfits('src_'+fsuf+suffix+'.fits',1)
  
  if not keyword_set(skipgv) then spawn,'gv check_image.ps &'
  ns=n_elements(src)
  
  wsrc=where(src.src_significance gt snr,ns)
  src=src[wsrc]
  
  ;;which source is the grb?
  grbsep=separation(src.ra,src.dec,grbra,grbdec)
  if n_elements(grb) eq 0 then begin
      tmin=min(grbsep,grb)
  endif else begin 
      if grb eq -1 then $
        tmin=min(grbsep,grb)
  endelse 

  ;;remove grb from src list
  if tmin gt 10 then begin  
     
     tmpsrc=src[0]
     if keyword_set(wavdetect) then src2=mrdfits('src_xrtcentroid'+suffix+'.fits',1) else $
        src2=mrdfits('src_wavdetect'+suffix+'.fits',1)
     grbsep=separation(src2.ra,src2.dec,grbra,grbdec)
     tmin=min(grbsep,grb)
     struct_assign,src2[grb],tmpsrc
     concat_structs,src,tmpsrc,src3
     src=src3
     ns=n_elements(src)
     grb=ns-1
     print,fsuf+' did not detect GRB, finding GRB pos from xrtcentroid'
  endif
  
  ns=n_elements(src)
  n=indgen(ns)
;  tn=intarr(ns)
;  tn[grb]=1
;  n=n[where(tn eq 0)]
;  ssrc=src[n] ;list of all sources except grb
  
  resp=''
  if exist('skip_det'+suffix+'.dat') then readcol,'skip_det'+suffix+'.dat',skipind,format='(a)',delim='$' else skipind='-1'
;  read,resp,prompt='Remove sources (i.e. [1,2,3], -1 for none)? '
  if not keyword_set(noprompt) then input,'Remove sources (i.e. [1,2,3], -1 for none)? ',resp,skipind else resp=skipind
  resp=ntostr(resp[0])
  writecol,'skip_det'+suffix+'.dat',resp
  com=execute('wrem='+resp)
  
  gb=0
  if exist('isgrb'+suffix+fsuf+'.dat') then readcol,'isgrb'+suffix+fsuf+'.dat',grb,format='(a)',delim='$'
  if not keyword_set(noprompt) then input,'GRB is source '+ntostr(grb+1)+'? (if no, which?)',gb,grb+1 else gb=grb+1
  gb=gb-1
  if gb ne grb then grb=gb
  writecol,'isgrb'+suffix+fsuf+'.dat',grb
  
  tmp=intarr(ns)
  if wrem[0] ne -1 then begin
     wrem=wrem-1
     tmp[wrem]=1
  endif 
  tmp[grb]=1
  whrem=where(tmp eq 0,ns)
  if ns gt 0 then begin 
     ssrc=src[whrem]
     n=indgen(n_elements(tmp))
     n=n[whrem]
  endif
  
;  if n_elements(snr) eq 0 then snr=4.
;  wsrc=where(src.src_significance gt snr,ns)
;  src=src[wsrc]

  if ns lt 5 then begin
     print,'Not enough serendipitous sources ('+ntostr(ns)+')'
     newra=0 & newdec=0 & newerr=0 & mra=0 & mdec=0 & mraerr=0 & nsrc=0
     sdra=0 & sddec=0 
     print,'type any key to continue'
     if not keyword_set(noprompt) then k=get_kbrd(10)
     return
  endif
;  stop
  
  ;;match sdss source to xray sources
  raoff=0d & decoff=0d
  sigra=0d & sigdec=0d
  sep=0d
  color=0
  sigmas=0
  nmatch=intarr(ns)
  for i=0,ns-2 do begin
     s=separation(ssrc[i].ra,ssrc[i].dec,ra,dec)
     w=where(s lt searchdist,nw)
     if nw gt 0 then begin
        raoff=[raoff,(ssrc[i].ra-ra[w])*cos(ssrc[i].dec*!dtor)*3600.]
        decoff=[decoff,(ssrc[i].dec-dec[w])*3600.]

;        sigra=[sigra,replicate(ssrc[i].ra_err,nw)*3600.]
;        sigdec=[sigdec,replicate(ssrc[i].dec_err,nw)*3600.]
        
        sigma=hpd*(ssrc[i].counts)^p/sqrt(2.)
        sigmas=[sigmas,sigma]
                
        
        sigra=[sigra,replicate(sigma,nw)]
        sigdec=[sigdec,replicate(sigma,nw)]
        sep=[sep,s[w]]
        color=[color,replicate(i,nw)]
        nmatch[i]=nw
     endif

  endfor
  
  w0=where(nmatch ne 0,nw0)
  if nw0 lt 2 then begin 
     print,'Not enough serendipitous sources ('+ntostr(nw0)+')'
     newra=0 & newdec=0 & newerr=0 & mra=0 & mdec=0 & mraerr=0 & nsrc=0
     sdra=0 & sddec=0 
     print,'type any key to continue'
     if not keyword_set(noprompt) then k=get_kbrd(10)
     return
  endif
  
  raoff=raoff[1:*]
  decoff=decoff[1:*]
  sigra=sigra[1:*]
  sigdec=sigdec[1:*]
  sep=sep[1:*]
  color=color[1:*]
  sigmas=sigmas[1:*]
  
  wsig=where(sigra gt (median(sigmas)-stddev(sigmas)),nwsig)
  print,nwsig,n_elements(sigra)
;  if nwsig gt 0 then begin
;     sigra=sigra[wsig]
;     sigdec=sigdec[wsig]
;     raoff=raoff[wsig]
;     decoff=decoff[wsig]
;     sep=sep[wsig]
;     color=color[wsig]
;     sigra[wsig]=sigra[wsig]*1.5
;     sigdec[wsig]=sigdec[wsig]*1.5
;  endif 
;  stop
  if n_elements(iter) eq 0 then iter=5
  !p.multi=[0,4,iter]
  for g=0,iter-1 do begin
     if n_elements(raoff) gt 2 then begin
        if not keyword_set(noplot) then begin
           xrange=[min(raoff-sigra),max(raoff+sigra)]
           yrange=[min(decoff-sigdec),max(decoff+sigdec)]
           ploterror,raoff,decoff,sigra,sigdec,/yno,psym=3,/iso,xtitle='RA offset (")',ytitle='Dec offset (")',/nohat,xrange=xrange,yrange=yrange
           uclr=color[uniq(color)]

           clr=[!red,!blue,!green,!orange,!yellow,!cyan,!magenta,!deeppink,!purple,!darkred,!royalblue,!salmon,!hotpink,!violet,!grey,!navyblue,!seagreen,!sienna,!forestgreen,!lightgreen,!midnightblue,!darkslategrey,!darkblue,!skyblue,!darkgreen]
           clr=[clr,clr,clr,clr,clr,clr,clr,clr,clr,clr,clr]
           
;           if g lt 2 then begin 
              mra=weighted_mean(raoff,sigra,mraerr)
              mdec=weighted_mean(decoff,sigdec,mdecerr)

;           histogauss,raoff,gauss,/noplot
;           mra=gauss[1]
;           mraerr=gauss[2]
           
;           histogauss,decoff,gauss,/noplot
;           mdec=gauss[1]
;           mdecerr=gauss[2]
           

           if g gt 1 then begin
           uclr=color[uniq(color)]
           craoff=0 & cdecoff=0 & csigra=0 & csigdec=0 & ccolor=0
           for i=0,n_elements(uclr)-1 do begin
              t=where(color eq uclr[i],nt)
              oploterror,raoff[t],decoff[t],sigra[t],sigdec[t],psym=3,/nohat,errcolor=clr[uclr[i]]

              if nt gt 1 then cmin=min(abs(raoff[t]-mra),c) else c=0
              craoff=[craoff,raoff[t[c]]]
              cdecoff=[cdecoff,decoff[t[c]]]
              csigra=[csigra,sigra[t[c]]]
              csigdec=[csigdec,sigdec[t[c]]]
              ccolor=[ccolor,color[t[c]]]
           endfor
           craoff=craoff[1:*]
           cdecoff=cdecoff[1:*]
           csigra=csigra[1:*]
           csigdec=csigdec[1:*]
           ccolor=ccolor[1:*]

           mra=weighted_mean(craoff,csigra,cmraerr)
           mdec=weighted_mean(cdecoff,csigdec,cmdecerr)
           
;           histogauss,craoff,gauss,/noplot
;           mra=gauss[1]
;           mraerr=gauss[2]
;           
;           histogauss,cdecoff,gauss,/noplot
;           mdec=gauss[1]
;           mdecerr=gauss[2]

              raoff=craoff
              decoff=cdecoff
              sigra=csigra
              sigdec=csigdec
              color=ccolor
           endif

;        mra=weighted_mean(raoff,sigra,mraerr)
;        mdec=weighted_mean(decoff,sigdec,mdecerr)

           nsrc=n_elements(raoff)
           print,mra,mdec,nsrc
           dra=mra-raoff
           ddec=mdec-decoff
           sdra=sqrt((total(raoff^2/sigra^2)/total(1./sigra^2)-(total(raoff/sigra^2)/total(1./sigra^2))^2)*nsrc/(nsrc-1d))
           sddec=sqrt((total(decoff^2/sigdec^2)/total(1./sigdec^2)-(total(decoff/sigdec^2)/total(1./sigdec^2))^2)*nsrc/(nsrc-1d))

;        p=1./(sdra*sqrt(2*!pi))*exp(-(raoff-mra)^2/(2.*sdra^2))


;     sdra=stddev(dra)
;     sddec=stddev(ddec)
;     sig=1.
           
           if g eq 0 then sig=1.
           if g ge 1 then sig=2.
           plots,mra,mdec,psym=2,color=!green
           tvellipse,mraerr,mdecerr,mra,mdec,/data,color=!green ;,thick=2
           tvellipse,sig*sdra,sig*sddec,mra,mdec,/data,color=!orange ;,thick=2


;     window,1
;     !p.multi=[0,2,1]
           plothist,raoff,bin=0.5,xtitle='RA off'
           oplot,[mra-sig*sdra,mra-sig*sdra],[0,100],color=!red
           oplot,[mra+sig*sdra,mra+sig*sdra],[0,100],color=!red
           plothist,decoff,bin=0.5,xtitle='Dec off'
           oplot,[mdec-sig*sddec,mdec-sig*sddec],[0,100],color=!red
           oplot,[mdec+sig*sddec,mdec+sig*sddec],[0,100],color=!red

           newerr=sqrt(2.*mraerr^2+(hpd*(src[grb].counts)^p)^2+0.9^2)
;     if newerr lt 1. then newerr=1.
;        print,mraerr,src[grb].counts
           newdec=src[grb].dec-mdec/3600.
           newra=src[grb].ra-mra/3600./cos(newdec*!dtor)
;           newra=grbra-mra/3600./cos(grbdec*!dtor)
;           newdec=grbdec-mdec/3600.
;     compare_plots,newra,newdec,newerr

           compare_plots,grbra,grbdec,ralist,declist,errlist,names,newra,newdec,newerr,title=title
        endif 
        if g lt iter-1 then begin

;           if g lt iter-2 then begin
           w=where(abs(dra) lt sig*sdra and abs(ddec) lt sig*sddec,nw)
           print,'removed data points: '+ntostr(n_elements(dra)-nw)
           
;              help,uniq(color[w])
;              help,raoff
           noplot=0
           if n_elements(raoff) lt 4 then begin
              print,'Warning: too few final matches, position may be not reliable'
              print,'type any key to continue'
              noplot=1
;                 k=get_kbrd(10)
           endif 
           
;              raoff=raoff[w] & decoff=decoff[w] & sigra=sigra[w] & sigdec=sigdec[w]
;              color=color[w] & nmatch=nmatch[w]
;           endif
           if g eq 2 then begin
              uclr=color[w[uniq(color[w])]]
              tmp1=intarr(nw)
              for c=0,n_elements(uclr)-1 do begin
                 wclr=where(color[w] eq uclr[c])
                 mint=min(sqrt(raoff[w[wclr]]^2+decoff[w[wclr]]^2),mclr)
                 tmp1[wclr[mclr]]=1
              endfor 
              wclr=where(tmp1 eq 1)
              w=w[wclr]
              
           endif 
           
           if g gt 2 and n_elements(raoff) gt 2 then begin 
              
              discard_data_point,raoff,sigra,decoff,sigdec,sig,w2

              wind=intarr(n_elements(raoff))
              wind[w]=wind[w]+1
              wind[w2]=wind[w2]+1
              w=where(wind eq 2)
;              w=[w,w2]
;              w=w[uniq(w)]
           endif 
           raoff=raoff[w] & decoff=decoff[w] & sigra=sigra[w] & sigdec=sigdec[w]
           color=color[w] & nmatch=nmatch[w]
;              stop
;           endif
           
        endif 
        
;        if g eq iter-2 then stop
     endif 

  endfor

;  !p.multi=[0,1,2]

;  plots,mra,mdec,psym=2,color=!green
;  tvellipse,mraerr,mdecerr,mra,mdec,/data,color=!green
  
  print
  if n_elements(name) gt 0 then print,'GRB'+name
  print,'Expotime: '+ntostr(expotime)
  print,'N sources: '+ntostr(n_elements(raoff))
  print,'Frame shift: '+ntostr(-mra/cos(grbdec*!dtor)/15.)+' s +/- '+ntostr(mraerr/15.)+' s , '+ntostr(-mdec)+' +/- '+ntostr(mraerr)+'"'
  print,'             '+ntostr(-mra)+' '+ntostr(-mdec)+' '+ntostr(mraerr)
;  sep2=sqrt((raoff-mra)^2+(decoff-mdec)^2)
;  msep2=weighted_mean(sep2,sqrt(sigra^2+sigdec^2),unc)
;  msep2sig=stddev(sep2)
  
  print,'XRTCENTROID check: '+ntostr(separation(src[grb].ra,src[grb].dec,grbra,grbdec))
  newerr=sqrt(2.*mraerr^2+(hpd*(src[grb].counts)^p)^2+0.9^2)
  print,'New err: '+ntostr(newerr)
  
  newra=src[grb].ra-mra/3600./cos(src[grb].dec*!dtor)
  newdec=src[grb].dec-mdec/3600.

  print,'New RA/Dec: '+ntostr(newra)+' '+ntostr(newdec)
  print,'            '+adstring(newra,newdec,1)
;  radec,newra,newdec,h,m,s,d,mm,ss
;  print,'            '+ntostr(h)+' '+ntostr(m)+' '+sigfig(s,4)+'   '+ntostr(d)+' '+ntostr(mm)+ ' '+sigfig(ss,4)
  print,'type any key to continue'
  if not keyword_set(noprompt) then k=get_kbrd(10)

  !p.multi=0
  stop
  grb=-1

  return
end
