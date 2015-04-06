@swift_lat_pop_studies
@fit_functions
pro match_dan_list

  g=mrdfits('~/stuff_for_people/Dan/xrt_flux.fits',1)
  readcol,'~/stuff_for_people/Dan/stacking_list.txt',grb,format='(a)'
  ind=0
  for i=0,n_elements(grb)-1 do begin
     w=where(strtrim(g.grb,2) eq strtrim(grb[i],2),nw)
     if nw ne 0 then ind=[ind,w] else print,grb[i]
  endfor 
  ind=ind[1:*]
  help,g,ind,grb
  stop
  return
end 

pro stuff_for_dan

;  send Dan 11 hr fluxes, luminosities (GRB, z, 11 hr rest frame time, flux(t11), lum(t11), gamma(t11))
  
;  lum=flux*4.*!pi*dist[i]^2*(1.+z[i])^(beta-alpha-1.) 
  mpc2cm=3.08568025d24
  g=create_struct('grb','','z',0.,$
                  't1obs',0.,'t1rest',0.,$
                  't11obs',0.,'t11rest',0.,$
                  'tstart',0d,'tstop',0d,'tlastdet',0d,$
                  'alpha_1obs',0.,'alphaerr_1obs',fltarr(2),$
                  'alpha_1rest',0.,'alphaerr_1rest',fltarr(2),$
                  'alpha_11obs',0.,'alphaerr_11obs',fltarr(2),$
                  'alpha_11rest',0.,'alphaerr_11rest',fltarr(2),$
                  'beta',0.,'betaerr',fltarr(2),$
                  'nh',0.,'nherr',fltarr(2),$
                  'fluxfact',0d,'unfluxfact',0d,$
                  'cts_1obs',0d,'ctserr_1obs',dblarr(2),'flux_1obs',0d,'fluxerr_1obs',dblarr(2),$
                  'cts_1rest',0d,'ctserr_1rest',dblarr(2),'flux_1rest',0d,'fluxerr_1rest',dblarr(2),$
                  'lum_1obs',0d,'lumerr_1obs',dblarr(2),'lum_1rest',0d,'lumerr_1rest',dblarr(2),$
                  'cts_11obs',0d,'ctserr_11obs',dblarr(2),'flux_11obs',0d,'fluxerr_11obs',dblarr(2),$
                  'cts_11rest',0d,'ctserr_11rest',dblarr(2),'flux_11rest',0d,'fluxerr_11rest',dblarr(2),$
                  'lum_11obs',0d,'lumerr_11obs',dblarr(2),'lum_11rest',0d,'lumerr_11rest',dblarr(2))

  cd,'~/GRBs/'
  grbs=file_search('GRB*')
  n=n_elements(grbs)
  
  t1=3600.4
  t11=3600.*11.
  g=replicate(g,n)
  g.grb=grbs
  g.t1obs=t1
  g.t11obs=t11
  rem=intarr(n)
  for i=0,n-1 do begin
     cd,grbs[i]
     print,grbs[i]
;     print,exist('lc_newout.txt'),exist('lc_fit_out_idl_int7.dat') ,exist('UL_specfits.fits') 
;     stop
     if (exist('lc_newout.txt') or exist('lc_newout_phil.txt')) and (exist('lc_fit_out_idl_int7.dat') or exist('lc_fit_out_idl_int8.dat')) and exist('UL_specfits.fits') then begin
        lcfile='lc_newout.txt'
        if not exist(lcfile) then lcfile='lc_newout_phil.txt'
        lc=lcout2fits(lcfile)
        spec=mrdfits('UL_specfits.fits',1)
        lcfitfile='lc_fit_out_idl_int8.dat'
        if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
        read_lcfit,lcfitfile,pname,p,perr,np=np
        nspec=n_elements(spec)
        g[i].tstart=lc[0].tstart
        g[i].tstop=lc[n_elements(lc)-1].tstop
        w=where(lc.src_rate_err gt 0,nw)
        g[i].tlastdet=lc[w[nw-1]].tstop
        if g[i].tstart lt t11 and g[i].tstop gt t11 then begin 
           g[i].z=spec[nspec-1].z
           g[i].nh=spec[nspec-1].nh
           g[i].nherr=spec[nspec-1].nherr
           g[i].t11rest=t11*(1.+g[i].z)
           g[i].fluxfact=spec[nspec-1].cfratio
           g[i].unfluxfact=spec[nspec-1].unabs_cfratio
           g[i].beta=spec[nspec-1].phind-1.
;           np=n_elements(p)
           case np of
              2: mo='pow'
              4: mo='bknpow'
              6: mo='bkn2pow'
              8: mo='bkn3pow'
              10: mo='bkn4pow'
           endcase 
     ;;; need p of when rest frame 11 hours 
           which_alpha,g[i].t11obs,p,aspec_obs,xobs,np=np
           which_alpha,g[i].t11rest,p,aspec_rest,xrest,np=np
           g[i].alpha_11obs=aspec_obs
           g[i].alphaerr_11obs=perr[*,xobs]
           g[i].alpha_11rest=aspec_rest
           g[i].alphaerr_11rest=perr[*,xrest]
           ;; if lc measured during RF time, need ct rate
           tmp=execute('cobs='+mo+'(g[i].t11obs,p)')
           tmp=execute('crest='+mo+'(g[i].t11rest,p)')
           g[i].cts_11obs=cobs
           g[i].ctserr_11obs=sqrt((g[i].alphaerr_11obs/g[i].alpha_11obs)^2)*g[i].cts_11obs ;;; should i include error for other fit parameters???
           g[i].cts_11rest=crest
           g[i].ctserr_11rest=sqrt((g[i].alphaerr_11obs/g[i].alpha_11obs)^2)*g[i].cts_11rest

           g[i].flux_11obs=g[i].cts_11obs*g[i].unfluxfact
           g[i].fluxerr_11obs=sqrt((g[i].alphaerr_11obs/g[i].alpha_11obs)^2+(g[i].betaerr/g[i].beta)^2.+(g[i].nherr/g[i].nh)^2)*g[i].flux_11obs
           g[i].flux_11rest=g[i].cts_11rest*g[i].unfluxfact
           g[i].fluxerr_11rest=sqrt((g[i].alphaerr_11rest/g[i].alpha_11rest)^2+(g[i].betaerr/g[i].beta)^2.+(g[i].nherr/g[i].nh)^2)*g[i].flux_11rest

           dist=lumdist(g[i].z,h0=71,omega_m=0.27,lambda0=0.73)*mpc2cm
           g[i].lum_11obs=g[i].flux_11obs*4*!pi*dist^2*(1.+g[i].z)^(g[i].beta-g[i].alpha_11obs-1.) 
           g[i].lumerr_11obs=g[i].fluxerr_11obs/g[i].flux_11obs*g[i].lum_11obs
           g[i].lum_11rest=g[i].flux_11rest*4*!pi*dist^2*(1.+g[i].z)^(g[i].beta-g[i].alpha_11rest-1.) 
           g[i].lumerr_11rest=g[i].fluxerr_11rest/g[i].flux_11rest*g[i].lum_11rest           

           if g[i].z eq 0 then begin 
              g[i].t11rest=0
              g[i].alpha_11rest=0
              g[i].alphaerr_11rest=[0,0]
              g[i].cts_11rest=0
              g[i].ctserr_11rest=[0,0]
              g[i].flux_11rest=0
              g[i].fluxerr_11rest=[0,0]
              g[i].lum_11rest=0
              g[i].lumerr_11rest=[0,0]
              g[i].lum_11obs=0
              g[i].lumerr_11obs=[0,0]
           endif 
           
        endif 
        if g[i].tstart lt t1 and g[i].tstop gt t1 then begin 
           g[i].t1rest=t1*(1.+g[i].z)
           which_alpha,g[i].t1obs,p,aspec_obs,xobs,np=np
           which_alpha,g[i].t1rest,p,aspec_rest,xrest,np=np
           g[i].alpha_1obs=aspec_obs
           g[i].alphaerr_1obs=perr[*,xobs]
           g[i].alpha_1rest=aspec_rest
           g[i].alphaerr_1rest=perr[*,xrest]
           ;; if lc measured during RF time, need ct rate
           tmp=execute('cobs='+mo+'(g[i].t1obs,p)')
           tmp=execute('crest='+mo+'(g[i].t1rest,p)')
           g[i].cts_1obs=cobs
           g[i].ctserr_1obs=sqrt((g[i].alphaerr_1obs/g[i].alpha_1obs)^2)*g[i].cts_1obs ;;; should i include error for other fit parameters???
           g[i].cts_1rest=crest
           g[i].ctserr_1rest=sqrt((g[i].alphaerr_1obs/g[i].alpha_1obs)^2)*g[i].cts_1rest

           g[i].flux_1obs=g[i].cts_1obs*g[i].unfluxfact
           g[i].fluxerr_1obs=sqrt((g[i].alphaerr_1obs/g[i].alpha_1obs)^2+(g[i].betaerr/g[i].beta)^2.+(g[i].nherr/g[i].nh)^2)*g[i].flux_1obs
           g[i].flux_1rest=g[i].cts_1rest*g[i].unfluxfact
           g[i].fluxerr_1rest=sqrt((g[i].alphaerr_1rest/g[i].alpha_1rest)^2+(g[i].betaerr/g[i].beta)^2.+(g[i].nherr/g[i].nh)^2)*g[i].flux_1rest

           g[i].lum_1obs=g[i].flux_1obs*4*!pi*dist^2*(1.+g[i].z)^(g[i].beta-g[i].alpha_1obs-1.) 
           g[i].lumerr_1obs=g[i].fluxerr_1obs/g[i].flux_1obs*g[i].lum_1obs
           g[i].lum_1rest=g[i].flux_1rest*4*!pi*dist^2*(1.+g[i].z)^(g[i].beta-g[i].alpha_1rest-1.) 
           g[i].lumerr_1rest=g[i].fluxerr_1rest/g[i].flux_1rest*g[i].lum_1rest           

           if g[i].z eq 0 then begin 
              g[i].t1rest=0
              g[i].alpha_1rest=0
              g[i].alphaerr_1rest=[0,0]
              g[i].cts_1rest=0
              g[i].ctserr_1rest=[0,0]
              g[i].flux_1rest=0
              g[i].fluxerr_1rest=[0,0]
              g[i].lum_1rest=0
              g[i].lumerr_1rest=[0,0]
              g[i].lum_1obs=0
              g[i].lumerr_1obs=[0,0]
           endif 
           
        endif 

        colprint,g[i].grb,g[i].flux_11rest,g[i].flux_1rest
     endif else rem[i]=1
     cd,'..'
  endfor 
  w=where(rem eq 0)
  g=g[w]
  mwrfits,g,'~/stuff_for_people/Dan/xrt_flux.fits',/create


  return
end   

pro xray_lumfunc

return
end 
