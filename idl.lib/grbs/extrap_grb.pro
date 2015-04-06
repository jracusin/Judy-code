@fit_functions
@fit_functions_flares
pro numbers4carrie,lcmod=lcmod

  cd,'~/GRBs/'
;  g=mrdfits('/Volumes/Apps_and_Docs/jracusin/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
;  ng=n_elements(g)
;  sw=mrdfits('~/Swift/swiftgrb.fits',1)
  g=mrdfits('~/Swift/swiftgrb_latgtis.fits',1)
  ng=n_elements(g)
  s1=round(1000.*(1-0.67)/2.)
  s2=round(1000.*(1.-(1-0.67)/2.))
  if not keyword_set(lcmod) then begin 
     openw,lun,'~/stuff_for_people/Carrie/LAT_gti_XRT_ctrate.txt',/get_lun
     printf,lun,'GRB               TRIGTIME            GTI_START       GTI_STOP         GTI_MEAN         CTRATE            CTRATE_ERR0      CTRATE_ERR1         RA           DEC          ERR'
;     openw,lun,'~/stuff_for_people/Carrie/LAT_gtis.txt',/get_lun
;     printf,lun,'GRB               TRIGTIME            GTI_START        GTI_STOP        RA           DEC          ERR'
  endif 
  for i=0,ng-1 do begin
     if keyword_set(lcmod) then begin
        if not exist('~/stuff_for_people/Carrie/'+strtrim(g[i].grb)) then $
           spawn,'mkdir ~/stuff_for_people/Carrie/'+strtrim(g[i].grb)
        openw,lun,'~/stuff_for_people/Carrie/'+strtrim(g[i].grb)+'/lc_model.dat',/get_lun
        printf,lun,'        TIME     MOD_CTRATE   MOD_CTRATE_ERR0   MOD_CTRATE_ERR1'
     endif 
     if exist(strtrim(g[i].grb),2) then begin 
        cd,strtrim(g[i].grb,2)
;     q=where(strtrim(g[i].grb,2) eq strtrim(sw.grb,2))
;;; big mess, consolodate everything to be the new g, no matching of files
        ffile='lc_fit_out_idl_int9.dat'
        mcfile='lc_fit_out_idl_int9_mc.fits'
        if not exist(ffile) then begin
           stop
;           ffile='lc_fit_out_idl_int7.dat'
;           mcfile='lc_fit_out_idl_int7_mc.fits'
        endif 
        if exist(ffile) and exist(mcfile) then begin 
           read_lcfit,ffile,pnames,p,perror
           if pnames[0] ne 'nofit' then begin 
              mcfit=mrdfits(mcfile,1)
              w=where(g[i].gti_start ne 0,n)
;           t=g[i].gti_mean[w];sqrt(g[i].gti_start[w]*g[i].gti_stop[w])
;           t=(g[i].gti_start[w]+g[i].gti_stop[w])/2.
              f=dblarr(n)
              model=fit_models(pnames,p,basemo=mo,breaks=breaks) ;strtrim(g[i].model,2)
;           tmp=execute('f='+mo+'(t,p)')
              tt=dblarr(2,n)
              tt[0,*]=g[i].gti_start[w]
              w0=where(tt[0,*] le 0,nw0)
              if nw0 gt 0 then tt[0,w0]=1.
              tt[1,*]=g[i].gti_stop[w]
              f=call_function('int'+mo,tt,p)
              g[i].ctrate[w]=f

              lc=lcout2fits()
              if lc[0].tstart gt g[i].gti_start[0] and lc[0].tstart lt g[i].gti_stop[0] then begin 
                 g[i].first_gti=[lc[0].tstart,g[i].gti_stop[0]]
                 g[i].first_gti_ctrate=call_function('int'+mo,g[i].first_gti,p)
                 first=1
              endif else first=0

              alpha=which_alpha(pnames,p,g[i].gti_start[w])
              t=fltarr(n)
              ferr0=dblarr(n) & ferr1=dblarr(n) & fsim1=dblarr(1000)
              for l=0,n-1 do begin 
                 t[l]=((1./(1.-alpha[l]))*((tt[1,l]^(1.-alpha[l])-tt[0,l]^(1.-alpha[l]))/(tt[1,l]-tt[0,l])))^(-1./alpha[l])
                 if first and l eq 0 then t1=((1./(1.-alpha[0]))*((g[i].first_gti[1]^(1.-alpha[l])-g[i].first_gti[0]^(1.-alpha[l]))/(g[i].first_gti[1]-g[i].first_gti[0])))^(-1./alpha[0])

                 fsim=dblarr(1000) 
                 for j=0,999 do begin
                    p00=0d
                    for k=0,n_tags(mcfit)-1 do p00=[p00,mcfit[j].(k)]
                    p00=p00[1:*]
                    fsim[j]=call_function(mo,t[l],p00)
                    if first and l eq 0 then fsim1[j]=call_function(mo,t1,p00)
                 endfor 
                 s=sort(fsim)
                 ferr0[l]=fsim[s[500]]-fsim[s[s1]]
                 ferr1[l]=fsim[s[s2]]-fsim[s[500]]
              endfor 

              g[i].ctrate_err[*,w]=[ferr0,ferr1]
              if first then begin 
                 s=sort(fsim1)
                 g[i].first_gti_ctrate_err=[fsim1[s[500]]-fsim1[s[s1]],fsim1[s[s2]]-fsim1[s[500]]]
              endif 

;        lc=lcout2fits()
;        ploterror,lc.time,lc.src_rate,lc.src_rate_err,/nohat,/xlog,/ylog,psym=3
;        oplot,t,f,color=!red,psym=4
;        for r=0,n-1 do begin
;           oplot,[t[r],t[r]],[f[r]-ferr0[r],f[r]+ferr1[r]],color=!red
;           oplot,[tt[0,r],tt[1,r]],[f[r],f[r]],color=!red
;        endfor 
;        k=get_kbrd(10)
;        if k eq 's' then stop
              if not keyword_set(lcmod) then begin 
                 st='        '
                 for k=0,n-1 do printf,lun,g[i].grb+st+string(g[i].trigtime,format='(f12.2)')+st+ntostr(g[i].gti_start[w[k]])+st+ntostr(g[i].gti_stop[w[k]])+st+ntostr(t[k])+st+ntostr(f[k])+st+ntostr(ferr0[k])+st+ntostr(ferr1[k])+st+ntostr(g[i].ra)+st+ntostr(g[i].dec)+st+ntostr(g[i].err/3600.)

;              for k=0,n-1 do printf,lun,g[i].grb+st+string(g[i].trigtime,format='(f12.2)')+st+ntostr(g[i].gti_start[w[k]])+st+ntostr(g[i].gti_stop[w[k]])+st+ntostr(g[i].ra)+st+ntostr(g[i].dec)+st+ntostr(g[i].err/3600.)
              endif else begin 
                 
                 tarr=[(findgen(9)+1)*10.,(findgen(9)+1)*100.,(findgen(9)+1)*1000.,(findgen(9)+1)*1e4,(findgen(9)+1)*1e5]
                 tarr=[tarr,p[breaks]]
                 tarr=tarr[sort(tarr)]
                 ntarr=n_elements(tarr)

                 f=call_function(mo,tarr,p)

                 ferr0=dblarr(ntarr) & ferr1=dblarr(ntarr)
                 for l=0,ntarr-1 do begin 
                    for j=0,999 do begin
                       p00=0d
                       for k=0,n_tags(mcfit)-1 do p00=[p00,mcfit[j].(k)]
                       p00=p00[1:*]
                       tmp=execute('fsim[j]='+mo+'(tarr[l],p00)')
                    endfor 
                    s=sort(fsim)
                    ferr0[l]=fsim[s[500]]-fsim[s[s1]]
                    ferr1[l]=fsim[s[s2]]-fsim[s[500]]
                    printf,lun,tarr[l],f[l],ferr0[l],ferr1[l]
                 endfor 
              endelse
           endif 
        endif 
        close,lun
        free_lun,lun
           
        cd,'..'
     endif 
  endfor 
  if not keyword_set(lcmod) then begin 
     close,lun
     free_lun,lun
     free_all
  endif 
  mwrfits,g,'~/Swift/swiftgrb_latgtis.fits',/create

  stop
  return
end 

pro extrap_grb,gamma,emin,emax,eminin=eminin,emaxin=emaxin

;  cd,'~/GRBs'
;  ngrb=n_elements(grb)
  if n_elements(eminin) eq 0 then eminin=0.3
  if n_elements(emaxin) eq 0 then emaxin=10.0

;  for i=0,ngrb-1 do begin 
;     cd,grb[i]
;     if exist('lc_newout_phil.txt') and exist('UL_specfits.fits') then begin 
;        lc=lcout2fits(/phil)
;        spec=mrdfits('UL_specfits.fits',1)

;        gamma=spec[n_elements(spec)-1].phind
        
        a=-gamma
;        ecin=qpint1d('x*x^p',eminin,emaxin,a,/expr)/qpint1d('x^p',eminin,emaxin,a,/expr)
;        ecout=qpint1d('x*x^p',emin,emax,a,/expr)/qpint1d('x^p',emin,emax,a,/expr)
        ecin=sqrt(eminin*emaxin)
        ecout=sqrt(emin*emax)

        a=-gamma+2
        fact1=ecout^a/ecin^a

        a=-gamma+1
        fact2=qpint1d('x^p',emin,emax,a,/expr)/qpint1d('x^p',eminin,emaxin,a,/expr);*(emaxin-eminin)/(emax-emin)
;        fact3=intpow([emin,emax],[1,-a])/intpow([eminin,emaxin],[1.,-a])*(emax-emin)/(emaxin-eminin)

        print,gamma,ecin,ecout,fact1,fact2;,fact3
;        k=get_kbrd(10)
;        if k eq 's' then stop
;     endif 
;     cd,'..'
;  endfor 

  stop
  return
end 
