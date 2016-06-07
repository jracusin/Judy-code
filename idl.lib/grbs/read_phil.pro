pro read_phil,time,tposerr,tnegerr,cts,err,fracexp,bgrate,bgerr,corr_fact,cts_in_src,bg_in_src,exposure,sigma,wtfile=wtfile,pcfile=pcfile,batfile=batfile,lc=lc,withbat=withbat,dir=dir,pculfile=pculfile
  
  if n_elements(dir) eq 0 then dir=''
;  if n_elements(file) eq 0 then file='curve.qdp'
  if n_elements(wtfile) eq 0 then begin
     wtfile=dir+'WTCURVE.qdp'
     nowt=0
     if not exist(wtfile) then nowt=1 ;print,'No WT file' 
  endif 
  if n_elements(pcfile) eq 0 then begin
     pcfile=dir+'PCCURVE.qdp'
     if not exist(pcfile) then nopc=1 ;print,'No WT file' 
  endif
  if n_elements(pculfile) eq 0 then begin 
     pculfile=dir+'PCUL.qdp'
     if not exist(pculfile) then nopcul=1
  endif 
  if n_elements(batfile) eq 0 then begin 
     batfile=dir+'bat_flux_snr5_with_cf_XRTBAND.qdp.gz'
     nobat=0
     if not exist(batfile) then nobat=1
  endif 
  if n_elements(wtsfile) eq 0 then begin 
     wtsfile=dir+'curve.qdp'
     nowts=0
  endif 
  ;;; old file or new file - March 2014
  new=0
  if exist(pcfile) then begin 
     openr,lun,pcfile,/get_lun
     chunks=readline(lun,skip=2)
     free_lun,lun
     if n_elements(chunks) eq 12 then new=1
  endif else begin 
     if exist(wtfile) then begin 
        openr,lun,wtfile,/get_lun
        chunks=readline(lun,skip=2)
        free_lun,lun
        if n_elements(chunks) eq 12 then new=1
     endif 
  endelse      
;  outfile='comb_curve.qdp'
;  if exist(pcfile) and exist(wtfile) then spawn,'cat '+wtfile+' '+pcfile+' > '+outfile else $
;     outfile=pcfile
  
  nwt=0 & npc=0 & nbat=0
  if exist(wtfile) then begin
     if not new then readcol,wtfile,wttime,wttposerr,wttnegerr,wtcts,wterr,wtfracexp,wtbgrate,wtbgerr,wtcorr_fact,wtcts_in_src,wtbg_in_src,wtexposure,wtsigma,format='(d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent else $
        readcol,wtfile,wttime,wttposerr,wttnegerr,wtcts,wterr,wtnegerr,wtfracexp,wtbgrate,wtbgerr,wtcorr_fact,wtcts_in_src,wtbg_in_src,wtexposure,wtsigma,wtsnr,format='(d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent
     nwt=n_elements(wttime)
     if nwt gt 0 then wttype=replicate(0.,nwt)
  endif 
  if exist(pcfile) then begin 
     if not new then readcol,pcfile,time,tposerr,tnegerr,cts,err,fracexp,bgrate,bgerr,corr_fact,cts_in_src,bg_in_src,exposure,sigma,format='(d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent else $
        readcol,pcfile,time,tposerr,tnegerr,cts,err,negerr,fracexp,bgrate,bgerr,corr_fact,cts_in_src,bg_in_src,exposure,sigma,snr,format='(d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent 
     npc=n_elements(time)
     pctype=replicate(1,npc)
     if exist(pculfile) then begin 
        readcol,pculfile,ultime,ultposerr,ultnegerr,ulcts,ulerr,ulnegerr,ulfracexp,ulbgrate,ulbgerr,ulcorr_fact,ulcts_in_src,ulbg_in_src,ulexposure,ulsigma,ulsnr,format='(d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)',/silent 
        nul=n_elements(ultime)
        time=[time,ultime]
        tposerr=[tposerr,ultposerr]
        tnegerr=[tnegerr,ultnegerr]
        cts=[cts,ulcts]
        err=[err,ulerr]
        fracexp=[fracexp,ulfracexp]
        bgrate=[bgrate,ulbgrate]
        bgerr=[bgerr,ulbgerr]
        corr_fact=[corr_fact,ulcorr_fact]
        cts_in_src=[cts_in_src,ulcts_in_src]
        bg_in_src=[bg_in_src,ulbg_in_src]
        exposure=[exposure,ulexposure]
        sigma=[sigma,ulsigma]
        pctype=[pctype,replicate(1,nul)]
     endif
  endif
  if exist(wtsfile) then begin 
;     readcol,wtsfile,wtstime,wtstposerr,wtstnegerr,wtscts,wtserr,format='(d,d,d,d,d)',/silent
     readcol,wtsfile,line,format='(a)',delim='$',/silent
     w=where(strtrim(line,2) eq '! WTSLEW data',nw)
     if nw gt 0 then begin 
        wend=where(strtrim(line,2) eq '! WT data' or strtrim(line,2) eq '! PC data',nwend)
        nwts=wend[0]-w[0]-3
        line=line[w+2:wend[0]-2]
        wtstime=dblarr(nwts) & wtstposerr=dblarr(nwts) & wtstnegerr=dblarr(nwts)
        wtscts=dblarr(nwts) & wtserr=dblarr(nwts)
        for i=0,nwts-1 do begin 
           chunks=strsplit(line[i],/ex)
           wtstime[i]=chunks[0]
           wtstposerr[i]=chunks[1]
           wtstnegerr[i]=chunks[2]
           wtscts[i]=chunks[3]
           wtserr[i]=chunks[4]
        endfor 
        
;        nwts=n_elements(wtstime)
        if n_elements(wttime) gt 0 then begin 
           wttime=[wtstime,wttime]
           wttposerr=[wtstposerr,wttposerr]
           wttnegerr=[wtstnegerr,wttnegerr]
           wtcts=[wtscts,wtcts]
           wterr=[wtserr,wterr]
           wtfracexp=[replicate(1.,nwts),wtfracexp]
           wtbgrate=[replicate(0.,nwts),wtbgrate]
           wtbgerr=[replicate(0.,nwts),wtbgerr]
           wtcorr_fact=[replicate(1.,nwts),wtcorr_fact]
           wtcts_in_src=[wtcts*(wtstposerr-wtstnegerr),wtcts_in_src]
           wtbg_in_src=[replicate(0.,nwts),wtbg_in_src]
           wtexposure=[(wtstposerr-wtstnegerr),wtexposure]
           wtsigma=[replicate(5.,nwts),wtsigma]
           wttype=[replicate(5,nwts),wttype]
        endif else begin
           wttime=wtstime
           wttposerr=wtstposerr
           wttnegerr=wtstnegerr
           wtcts=wtscts
           wterr=wtserr
           wtfracexp=replicate(1.,nwts)
           wtbgrate=replicate(0.,nwts)
           wtbgerr=replicate(0.,nwts)
           wtcorr_fact=replicate(1.,nwts)
           wtcts_in_src=wtcts*(wtstposerr-wtstnegerr)
           wtbg_in_src=replicate(0.,nwts)
           wtexposure=(wtstposerr-wtstnegerr)
           wtsigma=replicate(5.,nwts)
           wttype=replicate(5,nwts)
           nwt=n_elements(wttime)
        endelse 
     endif 
  endif 
  if exist(batfile) and keyword_set(withbat) then begin
     readcol,batfile,battime,batposerr,batnegerr,batflux,batfpos,batfneg,format='(d,d,d,d,d,d)',comment='!' ;/silent
     nbat=n_elements(battime)
     if nbat gt 0 then begin 
        b=battime[1:*]-battime[0:nbat-2]
        wb=where(b le 0,nb)
;        b=indgen(nbat-1)
;        nbat=nbat-1
;        c=replicate(nbat,1)

        if nb gt 0 then begin 
           b=indgen(wb[0]+1)
           nbat=n_elements(b)
           c=indgen(nbat-wb[0]+1)+wb[0]+1
           spec=mrdfits('UL_specfits.fits',1)
           cfratio=spec[n_elements(spec)-1].unabs_cfratio
        endif                   ; else nbat=0
     endif
  endif 
  if nwt gt 0 and npc gt 0 then begin 
     time=[wttime,time]
     tposerr=[wttposerr,tposerr]
     tnegerr=[wttnegerr,tnegerr]
     cts=[wtcts,cts]
     err=[wterr,err]
     fracexp=[wtfracexp,fracexp]
     bgrate=[wtbgrate,bgrate]
     bgerr=[wtbgerr,bgerr]
     corr_fact=[wtcorr_fact,corr_fact]
     cts_in_src=[wtcts_in_src,cts_in_src]
     bg_in_src=[wtbg_in_src,bg_in_src]
     exposure=[wtexposure,exposure]
     sigma=[wtsigma,sigma]
     type=[wttype,pctype]
;     type=[intarr(nwt),replicate(1,npc)]
  endif else begin 
     if npc gt 0 then type=pctype
     if nwt gt 0 then begin 
        time=wttime
        tposerr=wttposerr
        tnegerr=wttnegerr
        cts=wtcts
        err=wterr
        fracexp=wtfracexp
        bgrate=wtbgrate
        bgerr=wtbgerr
        corr_fact=wtcorr_fact
        cts_in_src=wtcts_in_src
        bg_in_src=wtbg_in_src
        exposure=wtexposure
        sigma=wtsigma
        type=wttype
     endif 
  endelse 
  if nbat gt 0 and n_elements(time) gt 0 then begin 
     time=[battime[b],time]
     tposerr=[batposerr[b],tposerr]
     tnegerr=[batnegerr[b],tnegerr]
     cts=[batflux[b]/cfratio,cts]
     err=[-batfneg[b]/cfratio,err]
     fracexp=[replicate(1.,nbat),fracexp]
     bgrate=[replicate(1d-10,nbat),bgrate]
     bgerr=[replicate(1d-11,nbat),bgerr]
     corr_fact=[batflux[c],corr_fact]
     cts_in_src=[batflux[b]/batflux[c]*(batposerr[b]-batnegerr[b]),cts_in_src]
     bg_in_src=[replicate(1d-11,nbat),bg_in_src]
     exposure=[(batposerr[b]-batnegerr[b]),exposure]
     sigma=[replicate(5.,nbat),sigma]
     type=[replicate(3,nbat),type]
  endif 
;  timeerr=(tposerr-tnegerr)/2.
  
  s=sort(time)
  if n_elements(time) gt 1 then begin 
     lc=replicate(lc,n_elements(time))
     lc.time=time[s]
     lc.tstart=time[s]+tnegerr[s]
     lc.tstop=time[s]+tposerr[s]
     lc.src_rate=cts[s]
     lc.src_rate_err=err[s]
     lc.exptime=exposure[s]
     lc.src_counts=cts_in_src[s]-bg_in_src[s]
     lc.tot_back_cts=bg_in_src[s]
     lc.det_sig=sigma[s]
     lc.type=type[s]
     lc.pu_corr=corr_fact[s]
     lc.back_ctrate=bgrate[s]
  endif 
  
  return
end 
