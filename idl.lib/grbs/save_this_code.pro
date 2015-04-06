  goto,skipcrap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  xlum_spec=dblarr(n)
  for i=0,n-1 do begin
     print,grb[i]
     cd,grb[i]
     lc=lcout2fits(/phil,/silent)
     dist=0.
     tf=1.
     if exist('lc_fit_out_idl_int7.dat') and exist('UL_specfits.fits') then begin 
        if keyword_set(flux) or keyword_set(lum) then begin 
           spec=mrdfits('UL_specfits.fits',1,/silent)
           ns=n_elements(spec)
           fluxfact=spec[ns-1].cfratio
           wz=where(grb[i] eq 'GRB'+zgrbs,nwz)
           if nwz gt 0 then z=zs[wz[0]] else z=0
           if keyword_set(lum) and z gt 0 then begin
              print,z,spec[0].z
              tf=1./(1.+z)
              dist=lumdist(z)*mpc2cm
              beta=spec[ns-1].phind-1
              read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perr
              np=n_elements(p)
              if np eq 2 then begin
                 alp=p[1]
                 f=pow(t_spec/tf,p)
              endif 
              if np eq 4 then begin
                 alp=p[3]
                 f=bknpow(t_spec/tf,p)
              endif 
              if np eq 6 then begin
                 if p[1] gt p[3] then alp=p[5] else alp=p[3]
                 f=bkn2pow(t_spec/tf,p)
              endif 
              if np eq 8 then begin
                 alp=p[5]
                 f=bkn3pow(t_spec/tf,p)
              endif 
              wlate=where(lc.time*tf gt 1e4,nwlate)
;              alpha=alp
              which_alpha,t_spec/tf,p,aspec
              alpha=aspec
;;; NEED TO SAVE SPECIFIC ALPHA FOR T_SPEC - CRAP!!!                 
              if nwlate gt 0 then begin
                 alphas=[alphas,alpha]
                 betas=[betas,beta]
                 sgrbs=[sgrbs,grb[i]]
                 odd=[odd,grb[i]]
              endif ;;; what is slope at t>10^4 s?
;              alpha=1.;;; needs to be normal decay??????
              print,4.*!pi*dist^2*(1.+z)^(beta-alpha-1.),z,dist,alpha,beta
              fluxfact=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-alpha-1.)
              xlum_spec[i]=f*fluxfact
           endif else dist=1.
        endif else begin
           fluxfact=1.
           dist=1.
        endelse 
;        lat=where(grb[i] eq latgrbs,nlat)
;        if nlat gt 0 then color=colors[lat] else 
        color=!grey50
;        wt=where(grb[i] eq tgrb,nwt)
;        if nwt gt 0 then color=!grey20

        if dist gt 0 then begin 
           det=where(lc.src_rate_err gt 0,ndet)
           if ndet gt 1 then begin 
              oplot,lc[det].time*tf,lc[det].src_rate*fluxfact,psym=3,color=color
              for j=0,ndet-1 do begin
                 oplot,[lc[det[j]].tstart,lc[det[j]].tstop]*tf,[lc[det[j]].src_rate,lc[det[j]].src_rate]*fluxfact,color=color
                 oplot,[lc[det[j]].time,lc[det[j]].time]*tf,[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*fluxfact,color=color
              endfor 
           endif 
        endif 
     endif 
     cd,'..'
  endfor 

  galphas=0d
  gbetas=0d
  ggrbs=''
  ng=n_elements(tgrb)
  odd=''
  xglum_spec=dblarr(ng)
  for i=0,ng-1 do begin
     print,tgrb[i]
     cd,tgrb[i]
     lc=lcout2fits(/phil,/silent)
     dist=0.
     tf=1.
     if exist('lc_fit_out_idl_int7.dat') and exist('UL_specfits.fits') then begin 
        if keyword_set(flux) or keyword_set(lum) then begin 
           spec=mrdfits('UL_specfits.fits',1,/silent)
           ns=n_elements(spec)
           fluxfact=spec[ns-1].cfratio
           wz=where(tgrb[i] eq 'GRB'+zgrbs,nwz)
           if nwz gt 0 then z=zs[wz[0]] else z=0
           if keyword_set(lum) and z gt 0 then begin
              print,z,spec[0].z
              tf=1./(1.+z)
              dist=lumdist(z)*mpc2cm
              beta=spec[ns-1].phind-1
              read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perr
              np=n_elements(p)
              if np eq 2 then begin
                 alp=p[1]
                 f=pow(t_spec/tf,p)
              endif 
              if np eq 4 then begin
                 alp=p[3]
                 f=bknpow(t_spec/tf,p)
              endif 
              if np eq 6 then begin
                 if p[1] gt p[3] then alp=p[5] else alp=p[3]
                 f=bkn2pow(t_spec/tf,p)
              endif 
              if np eq 8 then begin
                 alp=p[5]
                 f=bkn3pow(t_spec/tf,p)
              endif 
              wlate=where(lc.time*tf gt 1e4,nwlate)
;              alpha=alp
              which_alpha,t_spec/tf,p,aspec
              alpha=aspec
              if nwlate gt 0 then begin
                 galphas=[galphas,alpha]
                 gbetas=[gbetas,beta]
                 ggrbs=[ggrbs,tgrb[i]]
                 odd=[odd,grb[i]]
              endif ;;; what is slope at t>10^4 s?
;              alpha=1.;;; needs to be normal decay??????
              print,4.*!pi*dist^2*(1.+z)^(beta-alpha-1.),z,dist,alpha,beta
              fluxfact=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-alpha-1.)
              xglum_spec[i]=f*fluxfact
           endif else dist=1.
        endif else begin
           fluxfact=1.
           dist=1.
        endelse 
;        lat=where(grb[i] eq latgrbs,nlat)
;        if nlat gt 0 then color=colors[lat] else 
;        color=!grey50
;        wt=where(grb[i] eq tgrb,nwt)
;        if nwt gt 0 then color=!grey20
        color=!grey20

        if dist gt 0 then begin 
           det=where(lc.src_rate_err gt 0,ndet)
           oplot,lc[det].time*tf,lc[det].src_rate*fluxfact,psym=3,color=color
           for j=0,ndet-1 do begin
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop]*tf,[lc[det[j]].src_rate,lc[det[j]].src_rate]*fluxfact,color=color
              oplot,[lc[det[j]].time,lc[det[j]].time]*tf,[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*fluxfact,color=color
           endfor 
        endif 
     endif 
     cd,'..'
  endfor 

  lalphas=0d
  lbetas=0d
  lgrbs=''
  xllum_spec=dblarr(n_elements(latgrbs))
  for i=0,n_elements(latgrbs)-1 do begin 
     print,latgrbs[i]
     cd,latgrbs[i]
     lc=lcout2fits(/phil,/silent)
     dist=0.
     tf=1.
     if keyword_set(flux) or keyword_set(lum) then begin 
        spec=mrdfits('UL_specfits.fits',1,/silent)
        ns=n_elements(spec)
        fluxfact=spec[ns-1].cfratio
        wz=where(latgrbs[i] eq 'GRB'+zgrbs,nwz)
        if nwz gt 0 then z=zs[wz[0]] else z=0
        if keyword_set(lum) and z gt 0 then begin
           dist=lumdist(z)*mpc2cm
           print,z,spec[0].z
           tf=1./(1.+z)
           beta=spec[ns-1].phind-1
           read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perr
           np=n_elements(p)
           if np eq 2 then begin
              alp=p[1]
              f=pow(t_spec/tf,p)
           endif 
           if np eq 4 then begin
              alp=p[3]
              f=bknpow(t_spec/tf,p)
           endif 
           if np eq 6 then begin
              if p[1] gt p[3] then alp=p[5] else alp=p[3]
              f=bkn2pow(t_spec/tf,p)
           endif 
           if np eq 8 then begin
              alp=p[5]
              f=bkn3pow(t_spec/tf,p)
           endif 
;           alpha=alp
           which_alpha,t_spec/tf,p,aspec
           alpha=aspec
;print,alp,aspec
           lalphas=[lalphas,alpha]
           lbetas=[lbetas,beta] 
           lgrbs=[lgrbs,latgrbs[i]]
;           alpha=1. ;;; needs to be normal decay??????
           print,4.*!pi*dist^2*(1.+z)^(beta-alpha-1.),z,dist,alpha,beta
           fluxfact=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-alpha-1.)
           xllum_spec[i]=f*fluxfact
        endif else dist=1.
     endif else begin
        fluxfact=1.
        dist=1.
     endelse 
     if dist gt 0 then begin 
        det=where(lc.src_rate_err gt 0,ndet)
        oplot,lc[det].time*tf,lc[det].src_rate*fluxfact,psym=3,color=colors[i]
        for j=0,ndet-1 do begin
           oplot,[lc[det[j]].tstart,lc[det[j]].tstop]*tf,[lc[det[j]].src_rate,lc[det[j]].src_rate]*fluxfact,color=colors[i]
           oplot,[lc[det[j]].time,lc[det[j]].time]*tf,[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*fluxfact,color=colors[i]
        endfor
     endif 
     cd,'..'
  endfor 
  
  legend,['LAT '+latgrbs,'Swift GRBs','BAT/GBM no LAT'],textcolor=[colors,!grey50,!grey20],box=0,/top,/right
  if keyword_set(lat) then plot_lat_lcs_too
  if keyword_set(ps) then endplot

  alphas=alphas[1:*]
  lalphas=lalphas[1:*]
  galphas=galphas[1:*]
  betas=betas[1:*]
  lbetas=lbetas[1:*]
  gbetas=gbetas[1:*]
  sgrbs=sgrbs[1:*]
  ggrbs=ggrbs[1:*]
  lgrbs=lgrbs[1:*]

skipcrap:

;;;;;;;;;;;;;;;;;;;;
  goto,skipcrap2
  ind=0 ;; u filter
  plot,[10,1e7],[1d38,1d45],/xlog,/ylog,xtitle='Time since trigger (s) / (1+z)',ytitle='u Luminosity (erg s!U-1!N)',/nodata,/ysty,yticks=7,charsize=charsize
  alpha=dblarr(ngrb)
  lum_spec=alpha
  for i=0,ngrb-1 do begin
     readcol,gfile[i],time,terr,rate,raterr,format='(d,d,d,d)',/silent
     w=where(rate gt 0 and rate-raterr gt 0,nw)
     rate=rate[w]
     raterr=raterr[w]
     time=time[w]
     terr=terr[w]

     gdir='Swift_LC/GRB'+grb[i]
     if not exist(gdir) then spawn,'mkdir '+gdir
     lcfile=gdir+'/lc_newout.txt'
     if not exist(lcfile) then begin 
        lc=lcout2fits(/empty)
        lc=replicate(lc,nw)
        lc.time=time
        lc.src_rate=rate
        lc.src_rate_err=raterr
        lc.tstart=lc.time-terr
        lc.tstop=lc.time+terr
        write_lc,lc,lcfile
     endif 
     ;;; extinction correction
     calc_dust_corr,ra[i],dec[i],corr,/noplot,filter=filter

     mag=zpt[ind]-2.5*alog10(rate)
     mag2=mag-corr[ind]
;     print,filter[ind],corr[ind]
     rate2=10.^((mag2-zpt[ind])/(-2.5))
;     colprint,rate,rate2,mag,mag2
     flux=vfluxfact*rate2
     fluxerr=vfluxfact*raterr
;     oploterror,time,flux,terr,fluxerr,/nohat,errcolor=!grey50,psym=3

     dist=lumdist(z[i])*mpc2cm
     tf=1./(1.+z[i])
;     beta=1d
     b=where(xgrb eq 'GRB'+grb[i])
     beta=xbeta[b[0]]
     help,time,flux
     w=where(time gt 1e4)
;     f=linfit(alog10(time[w]),alog10(flux[w]))
;     alpha[i]=-f[1]
     read_lcfit,gdir+'/lc_fit_out_idl_int7.dat',pname,p,perr
     np=n_elements(p)
     if np eq 2 then begin
        alp=p[1]
        f=pow(t_spec/tf,p)
     endif 
     if np eq 4 then begin
        alp=p[3]
        f=bknpow(t_spec/tf,p)
     endif 
     if np eq 6 then begin
        if p[1] gt p[3] then alp=p[5] else alp=p[3]
        f=bkn2pow(t_spec/tf,p)
     endif 
     if np eq 8 then begin
        alp=p[5]
        f=bkn3pow(t_spec/tf,p)
     endif 
     alpha[i]=alp
     print,alpha[i]
     lum=flux*4.*!pi*dist^2*(1.+z[i])^(beta-alpha[i]-1.)
     lumerr=fluxerr*4.*!pi*dist^2*(1.+z[i])^(beta-alpha[i]-1.)
     oploterror,time*tf,lum,terr*tf,lumerr,/nohat,errcolor=!grey50,psym=3
     lum_spec[i]=f*vfluxfact*4.*!pi*dist^2*(1.+z[i])^(beta-alpha[i]-1.)


;; need to convert from v to u or vice versa
;; need to calculate luminosity
;; need to do k-corr

;; need to do same for LAT bursts & GBM (once Sam does those)
  endfor 

  ;;;LAT bursts

  ex=0
  lamu=lam_eff[0]
  lamv=lam_eff[2]
  ufluxfact=fluxfact[0]
  lalpha=dblarr(n_elements(latgrbs))
  llum_spec=lalpha
  for i=0,n_elements(latgrbs)-1 do begin
     lfile='LAT_LC/'+latgrbs[i]+'.txt'
     if exist(lfile) then begin
;        print,lfile
        readcol,lfile,time,terr,rate,raterr,format='(d,d,d,d)',/silent
        
        w=where(rate gt 0 and rate-raterr gt 0,nw)
        rate=rate[w]
        raterr=raterr[w]
        time=time[w]
        terr=terr[w]

        gdir='LAT_LC/'+latgrbs[i]
        if not exist(gdir) then spawn,'mkdir '+gdir
        lcfile=gdir+'/lc_newout.txt'
        if not exist(lcfile) then begin 
           lc=lcout2fits(/empty)
           lc=replicate(lc,nw)
           lc.time=time
           lc.src_rate=rate
           lc.src_rate_err=raterr
           lc.tstart=lc.time-terr
           lc.tstop=lc.time+terr
           write_lc,lc,lcfile
        endif 

     ;;; extinction correction
        calc_dust_corr,lra[i],ldec[i],corr,/noplot,filter=filter

        mag=zpt[ind]-2.5*alog10(rate)
        mag2=mag-corr[ind]
;        print,filter[ind],corr[ind]
        rate2=10.^((mag2-zpt[ind])/(-2.5))
;        colprint,rate,rate2,mag,mag2
        b=where(xgrb eq latgrbs[i])
        beta=xbeta[b[0]]
;        beta=1d
        w=where(time gt 1e4)
        tf=1./(1.+lz[i])
;        f=linfit(alog10(time[w]),alog10(flux[w]))
;        lalpha[i]=-f[1]
        read_lcfit,gdir+'/lc_fit_out_idl_int7.dat',pname,p,perr
        np=n_elements(p)
        if np eq 2 then begin
           alp=p[1]
           f=pow(t_spec/tf,p)
        endif 
        if np eq 4 then begin
           alp=p[3]
           f=bknpow(t_spec/tf,p)
        endif 
        if np eq 6 then begin
           if p[1] gt p[3] then alp=p[5] else alp=p[3]
           f=bkn2pow(t_spec/tf,p)
        endif 
        if np eq 8 then begin
           alp=p[5]
           f=bkn3pow(t_spec/tf,p)
        endif 

        lalpha[i]=alp

        print,lalpha[i]
;        uvconv=(lamv/lamu)^beta
        uvconv=1d

        flux=uvconv*ufluxfact*rate2
        fluxerr=uvconv*ufluxfact*raterr
        dist=lumdist(lz[i])*mpc2cm

;        oploterror,time,flux,terr,fluxerr,/nohat,errcolor=colors[i],psym=3

        lum=flux*4.*!pi*dist^2*(1.+lz[i])^(beta-lalpha[i]-1.)
        lumerr=fluxerr*4.*!pi*dist^2*(1.+lz[i])^(beta-lalpha[i]-1.)
        oploterror,time*tf,lum,terr*tf,lumerr,/nohat,errcolor=colors[i],psym=3
        llum_spec[i]=f*uvconv*ufluxfact*4.*!pi*dist^2*(1.+lz[i])^(beta-alpha[i]-1.)

        ex=[ex,i]
     endif 
  endfor 
  ex=ex[1:*]
  legend,['LAT '+latgrbs[ex],'Swift GRBs'],textcolor=[colors[ex],!grey50],box=0,/top,/right
  if keyword_set(ps) then endplot

skipcrap2:
