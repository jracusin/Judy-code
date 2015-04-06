@swift_lat_pop_studies
@fit_functions
@fit_functions_flares
@calc_eiso

pro plot_for_sam,noplot=noplot
  cd,'~/GRBs/'

  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  t200=200.

  if not exist('~/stuff_for_people/Sam/lum_decay_corr.fits') then begin 
     g=create_struct('grb','','z',0.,'model','',$
                     'tstart',0.,'tstop',0.,$
                     't200',0.,$
                     'f200_fit',0.,'f200_fit_err',fltarr(2),$
                     'f200_noflare',0.,'f200_noflare_err',fltarr(2),$
                     'f200_plateau',0.,'f200_plateau_err',fltarr(2),$
                     'lum200_fit',0d,'lum200_fit_err',dblarr(2),$
                     'lum200_noflare',0d,'lum200_noflare_err',dblarr(2),$
                     'lum200_plateau',0d,'lum200_plateau_err',dblarr(2),$
                     'alpha200_fit',0.,'alpha200_fit_err',fltarr(2),$
                     'alpha200_plateau',0.,'alpha200_plateau_err',fltarr(2),$
                     'unabs_cfratio',0d,'gamma',0.,'gamma_err',fltarr(2),$
                     'norm',0d,'norm_err',dblarr(2),$
                     't90',0.,'eiso',0d,'eiso_err',0d,$
                     'epeak',0d)
     
     g=replicate(g,n)

     readcol,'~/jetbreaks/grb_tid_z_table.txt',grb,tid,z,delim='|',format='(a,l,f)'
;     match,strtrim(g.grb,2),strtrim(strcompress(grb,/rem),2),m1,m2
;     g[m1].z=z[m2]

     for i=0,n-1 do begin
        cd,grbs[i]
        g[i].grb=grbs[i]
        lcfile='lc_fit_out_idl_int8.dat'
        if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
        if exist(lcfile) and exist('UL_specfits.fits') then begin 
           read_lcfit,lcfile,pnames,p,perr
           if pnames[0] ne 'nofit' then begin 
              lc=lcout2fits(/phil)
              spec=mrdfits('UL_specfits.fits',1)
              if n_elements(spec) eq 1 then xs=n_elements(spec)-1 else begin 
                 wpc=where(lc.type eq 1)
                 if lc[wpc[0]].time ge 200. then xs=0 else xs=1
              endelse
              corr=spec[xs].unabs_cfratio
              g[i].unabs_cfratio=corr
              g[i].gamma=spec[xs].phind
              g[i].gamma_err=spec[xs].phinderr

              g[i].tstart=lc[0].tstart
              g[i].tstop=max(lc.tstop)
              
              wz=where(g[i].grb eq strcompress(grb,/rem),nwz)
              if nwz gt 0 then begin 
                 g[i].z=z[wz]
                 g[i].t200=200.*(1.+g[i].z)
                 np=n_elements(p)
                 mo=fit_models(pnames,p,np,nf,basemo=basemo)
                 rmo=fit_models(pnames,p,rnp,rnf,basemo=rbasemo,/rem)
                 g[i].model=mo
                 q=where(p ne 0)
                 which_alpha,g[i].t200,p,alpha,x,np=np
        ;;; straight interpolation
                 tmp=execute('c200='+mo+'(g[i].t200,p)')
                 g[i].f200_fit=c200*corr
;              tb=
;              g[i].f200_fit_err[0]=p[0]*t200^(-p[x])*(1.-(t200/tb)^perr[0,1])
;              g[i].f200_fit_err[1]=p[0]*t200^(-p[x])*((tb/t200)^perr[0,1]-1.)
                 g[i].f200_fit_err[0]=sqrt((perr[0,x]/p[x])^2+(g[i].gamma_err[0]/g[i].gamma)^2)*g[i].f200_fit
                 g[i].f200_fit_err[1]=sqrt((perr[1,x]/p[x])^2+(g[i].gamma_err[1]/g[i].gamma)^2)*g[i].f200_fit

        ;;; no flare
                 tmp=execute('c200='+basemo+'(g[i].t200,p)')
                 g[i].f200_noflare=c200*corr
                 g[i].f200_noflare_err[0]=sqrt((perr[0,x]/p[x])^2+(g[i].gamma_err[0]/g[i].gamma)^2)*g[i].f200_noflare
                 g[i].f200_noflare_err[1]=sqrt((perr[1,x]/p[x])^2+(g[i].gamma_err[1]/g[i].gamma)^2)*g[i].f200_noflare
                 
        ;;; extrapolate plateau
                 g[i].f200_plateau=g[i].f200_noflare
                 
                 t=lc.time
                 np2=np
                 p2err=perr
                 if np ge 4 then begin 
                    w=where(p[1:*] gt 100)
                    t=[t,p[w+1]]
                    t=t[sort(t)]
                    tt=p[2]*2.
                    if p[1] gt p[3] then begin 
                       tmp=execute('n2='+basemo+'(tt,p)/'+rbasemo+'(tt,[1.,p[3:*]])')
                       p2=[n2,p[3:*]]
                       p2err=[[perr[*,0]/p[0]*n2[0]],[perr[*,3:*]]]
                       np2=np-2
                       tmp=execute('g[i].f200_plateau='+rbasemo+'(g[i].t200,p2)*corr') 
                    endif else begin 
                       p2=p
                       rbasemo=basemo
                    endelse 
                 endif else p2=p
                 which_alpha,g[i].t200,p2,alpha2,x2,np=np2

                 g[i].alpha200_fit=alpha
                 g[i].alpha200_fit_err=perr[*,x2]
                 g[i].alpha200_plateau=alpha2
                 g[i].alpha200_plateau_err=p2err[*,x2]
                 g[i].norm=p[0]
                 g[i].norm_err=perr[*,0]

                 g[i].f200_plateau_err[0]=sqrt((p2err[0,x2]/p2[x2])^2+(g[i].gamma_err[0]/g[i].gamma)^2)*g[i].f200_plateau
                 g[i].f200_plateau_err[1]=sqrt((p2err[1,x2]/p2[x2])^2+(g[i].gamma_err[1]/g[i].gamma)^2)*g[i].f200_plateau
                 
                 mpc2cm=3.08568025d24 
                 dist=lumdist(g[i].z)*mpc2cm
                 g[i].lum200_fit=g[i].f200_fit*4.*!pi*dist^2*(1.+g[i].z)^(g[i].gamma-1.-g[i].alpha200_fit-1.) 
                 g[i].lum200_plateau=g[i].f200_plateau*4.*!pi*dist^2*(1.+g[i].z)^(g[i].gamma-1.-g[i].alpha200_plateau-1.) 
                 g[i].lum200_noflare=g[i].f200_noflare*4.*!pi*dist^2*(1.+g[i].z)^(g[i].gamma-1.-g[i].alpha200_fit-1.) 

                 print,i,grbs[i],lc[0].tstart,g[i].f200_fit,g[i].f200_noflare,g[i].f200_plateau
                 print,mo
                 tmp=execute('f='+mo+'(t,p)*corr')
                 tmp=execute('f1='+basemo+'(t,p)*corr')
                 tmp=execute('f2='+rbasemo+'(t,p2)*corr')
                 if keyword_set(noplot) then begin 
                    plot,t,f,/xlog,/ylog
                    oplot,t,f1,color=!red
                    oplot,t,f2,color=!green
                    oplot,[200,200],[1e-15,1e-8],line=2
                 endif 
;           k=get_kbrd(10)
;           if k eq 's' then stop
              endif 
           endif
        endif  
        cd,'..'
     endfor 

     w=where(g.model ne '' and g.f200_fit gt 0)
     g=g[w]

     mwrfits,g,'~/stuff_for_people/Sam/lum_decay_corr.fits',/create
  endif else g=mrdfits('~/stuff_for_people/Sam/lum_decay_corr.fits',1)

  w=where(g.t90 ne 0,nw)
  if nw eq 0 then begin 
     bat=mrdfits('~/jetbreaks/batcat.fits',1)
     match,strtrim(bat.grb,2),strtrim(g.grb,2),m1,m2
     g[m2].t90=bat[m1].t90
     g[m2].eiso=bat[m1].eiso1
     g[m2].epeak=bat[m1].epeak_band*(1.+g[m2].z)
;  g[m2].eiso_err=bat[m1].eiso1_err ;;; no error calc yet

;     readcol,'~/jetbreaks/grb_tid_z_table.txt',grb,tid,z,delim='|',format='(a,l,f)'
;     match,strtrim(g.grb,2),strtrim(strcompress(grb,/rem),2),m1,m2
;     g[m1].z=z[m2]
     mwrfits,g,'~/stuff_for_people/Sam/lum_decay_corr.fits',/create
  endif      

;  w=where(g.tstart lt 200 and g.f200_fit_err[0]/g.f200_fit lt 1. and g.f200_plateau_err[0]/g.f200_plateau lt 1.) ;;; NOT SURE I SHOULD BE MAKING THIS CUT ON ERRORS - IT REDUCES THE CORRELATION
;  g=g[w]
  w=where(g.tstart lt 200 and g.lum200_fit ne 0)
  g=g[w]
;  s=where(g.t90 le 2.)
  l=where(g.t90 gt 2.)
  g=g[l]
  xrange=[1d45,1d50]
  
  begplot,name='~/stuff_for_people/Sam/flux_decay.eps',/color
  !p.multi=[0,1,3]
  plot,g.lum200_fit,g.alpha200_fit,psym=3,/xlog,xtitle='Lum!L200!N (erg cm!U-2!N s!U-1!N)',ytitle=!tsym.alpha+'!L200!N',title='Fit (including flares)',xrange=xrange
  for i=0,n_elements(g)-1 do begin 
;     if g[i].t90 le 2. then color=!red else color=!p.color
;     xerr=g[i].lum200_fit-g[i].lum200_fit_err[0]
     xerr=g[i].lum200_fit*0.9
     xerrp=g[i].lum200_fit*1.1
     if xerr lt 0 then xerr=1e-14
;     oplot,[xerr,g[i].lum200_fit+g[i].lum200_fit_err[1]],[g[i].alpha200_fit,g[i].alpha200_fit],color=color
     oplot,[xerr,xerrp],[g[i].alpha200_fit,g[i].alpha200_fit],color=color
     oplot,[g[i].lum200_fit,g[i].lum200_fit],[g[i].alpha200_fit-g[i].alpha200_fit_err[0],g[i].alpha200_fit+g[i].alpha200_fit_err[1]],color=color
  endfor 
  c=r_correlate(alog10(g.lum200_fit),g.alpha200_fit)
  legend,['R='+ntostr(c[0]),'p='+ntostr(c[1])],/top,/right,box=0
;  legend,['long','short'],box=0,/top,/right,textcolor=[!p.color,!red]

  plot,g.lum200_noflare,g.alpha200_fit,psym=3,/xlog,xtitle='Lum!L200!N (erg cm!U-2!N s!U-1!N)',ytitle=!tsym.alpha+'!L200!N',title='Fit (excluding flares)',xrange=xrange
  for i=0,n_elements(g)-1 do begin 
     if g[i].t90 le 2. then color=!red else color=!p.color
;     xerr=g[i].lum200_noflare-g[i].lum200_noflare_err[0]
     xerr=g[i].lum200_noflare*0.9
     xerrp=g[i].lum200_noflare*1.1
     if xerr lt 0 then xerr=1e-14
;     oplot,[xerr,g[i].lum200_noflare+g[i].lum200_noflare_err[1]],[g[i].alpha200_fit,g[i].alpha200_fit],color=color
     oplot,[xerr,xerrp],[g[i].alpha200_fit,g[i].alpha200_fit],color=color
     oplot,[g[i].lum200_noflare,g[i].lum200_noflare],[g[i].alpha200_fit-g[i].alpha200_fit_err[0],g[i].alpha200_fit+g[i].alpha200_fit_err[1]],color=color
  endfor 
  c=r_correlate(alog10(g.lum200_noflare),g.alpha200_fit)
  legend,['R='+ntostr(c[0]),'p='+ntostr(c[1])],/top,/right,box=0
;  legend,['long','short'],box=0,/top,/right,textcolor=[!p.color,!red]

  plot,g.lum200_plateau,g.alpha200_plateau,psym=3,/xlog,xtitle='Lum!L200!N (erg cm!U-2!N s!U-1!N)',ytitle=!tsym.alpha+'!L200!N',title='Plateau Extrapolated (no flares)',xrange=xrange
  for i=0,n_elements(g)-1 do begin 
     if g[i].t90 le 2. then color=!red else color=!p.color
;     xerr=g[i].lum200_plateau-g[i].lum200_plateau_err[0]
     xerr=g[i].lum200_plateau*0.9
     xerrp=g[i].lum200_plateau*1.1
     if xerr lt 0 then xerr=1e-14
;     oplot,[xerr,g[i].lum200_plateau+g[i].lum200_plateau_err[1]],[g[i].alpha200_plateau,g[i].alpha200_plateau],color=color
     oplot,[xerr,xerrp],[g[i].alpha200_plateau,g[i].alpha200_plateau],color=color
     oplot,[g[i].lum200_plateau,g[i].lum200_plateau],[g[i].alpha200_plateau-g[i].alpha200_plateau_err[0],g[i].alpha200_plateau+g[i].alpha200_plateau_err[1]],color=color
  endfor 
  c=r_correlate(alog10(g.lum200_plateau),g.alpha200_plateau*1d)
  legend,['R='+ntostr(c[0]),'p='+ntostr(c[1])],/top,/right,box=0
;  legend,['long','short'],box=0,/top,/right,textcolor=[!p.color,!red]

  !p.multi=0
  endplot
  stop
  
  ;; need to add z & eiso & t90 & errors
  ;; need to make plots
  ;;   separate out short/long
  ;;   need to separate out extrapolations


  return
end 

pro compare_all_grbs,ps=ps,gbm=gbm

  cd,'~/GRBs/'

  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  
  if keyword_set(ps) then begplot,name='~/Fermi/Swift_Fermi/LAT_swift_grb_lc_compare_counts.ps',/color,/land,font='helvetica',/encap
  !x.margin=[12,1]
  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB100728A','GRB110625A','GRB110731A'] 
  colors=[!red,!blue,!green,!forestgreen,!cyan,!magenta,!purple,!orange,!sienna,!salmon,!violet]
;  plot,[10,1e7],[1e-5,1e4],/nodata,/xlog,/ylog,xtitle='Time Since
;  Trigger (s)',ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)'
;  plot,[10,1e7],[1e-15,1e-5],/nodata,/xlog,/ylog,xtitle='Time Since
;  Trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N
;  s!U-1!N)',charsize=2
  plot,[10,1e7],[1e-5,1e4],/nodata,/xlog,/ylog,xtitle='Time Since Trigger (s)',ytitle='Count rate (0.3-10 keV) (counts s!U-1!N)',charsize=2,yminor=9,yticks=9

  match,latgrbs,grbs,m1,m2
  dont_match,latgrbs,grbs,dm1,dm2

  lat=grbs[m2]
  nlat=n_elements(lat)
  ogrbs=grbs[dm2]
  w=where(ogrbs ne 'GRB110328A')
  ogrbs=ogrbs[w]

  goto,skipgbm1
  gbm=strtrim(gbm,2)
  match,ogrbs,gbm,m1,m2
  gbm=ogrbs[m1]
  ngbm=n_elements(gbm)
  dont_match,ogrbs,gbm,dm1,dm2
  ogrbs=ogrbs[dm1]
  skipgbm1:

  for i=0,n_elements(ogrbs)-1 do begin

     cd,ogrbs[i]
     print,ogrbs[i]
     
     color=!grey50
     lc=lcout2fits(/phil,/silent)
     if exist('UL_specfits.fits') then begin 
        spec=mrdfits('UL_specfits.fits',1)
;        corr=spec[n_elements(spec)-1].unabs_cfratio
corr=1.
        if n_elements(lc) gt 1 then begin 
           det=where(lc.src_rate_err ne 0,ndet)
;        oploterror,lc[det].time,lc[det].src_rate,lc[det].src_rate_err,/nohat,errcolor=color,psym=3
           for j=0,ndet-1 do begin 
              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*corr,color=color
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*corr,color=color
           endfor 

        endif 
     endif 
     cd,'..'
  endfor 

  goto,skipgbm
  for i=0,ngbm-1 do begin

     cd,gbm[i]
     print,gbm[i]

     lc=lcout2fits(/phil,/silent)
     if exist('UL_specfits.fits') then begin 
        spec=mrdfits('UL_specfits.fits',1)
        corr=spec[n_elements(spec)-1].unabs_cfratio
        if n_elements(lc) gt 1 then begin 
           det=where(lc.src_rate_err ne 0,ndet)
;        oploterror,lc[det].time,lc[det].src_rate,lc[det].src_rate_err,/nohat,errcolor=color,psym=3

           for j=0,ndet-1 do begin 
              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*corr,color=!grey20
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*corr,color=!grey20
           endfor
        endif 
     endif 

     cd,'..'
  endfor 
  skipgbm:

  for i=0,nlat-1 do begin

     cd,lat[i]
     print,lat[i]

     wlat=where(lat[i] eq latgrbs,nwlat)
     color=colors[wlat[0]]
     lc=lcout2fits(/phil,/silent)
     if exist('UL_specfits.fits') then begin 
        spec=mrdfits('UL_specfits.fits',1)
;        corr=spec[n_elements(spec)-1].unabs_cfratio
corr=1.
        if n_elements(lc) gt 1 then begin 
           det=where(lc.src_rate_err ne 0,ndet)
;        oploterror,lc[det].time,lc[det].src_rate,lc[det].src_rate_err,/nohat,errcolor=color,psym=3

           for j=0,ndet-1 do begin 
              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*corr,color=color
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*corr,color=color
           endfor
        endif 
     endif 

     cd,'..'
  endfor 


;  legend,[lat,'GBM GRBs','Swift
;  GRBs'],textcolor=[colors[0:nlat-1],!grey20,!grey50],/top,/right,box=0
  legend,[lat,'Swift GRBs'],textcolor=[colors[0:nlat-1],!grey50],/top,/right,box=0,charsize=1.5

  if keyword_set(ps) then endplot

  return
end 
