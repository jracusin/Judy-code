@fit_functions
pro kstest_beta,ps=ps

  if keyword_set(ps) then begin
     begplot,name='kstest_beta.ps',/land
     psm=!tsym
  endif else psm=!vsym

;zlcstat=mrdfits(!mdata+'lc_stats_z.fits',1)
  specstats=mrdfits(!mdata+'spec_stats.fits',1)
  lcstat=mrdfits(!mdata+'lc_stats_z.fits',1)

  w=where(lcstat.z eq 0)
  specstat=specstats[w]
  w0=where(specstat.pow ne 0,nw0)
  pow=specstat[w0].pow
  w1=where(specstat.pow1 ne 0,nw1)
  pow1=specstat[w1].pow1
  w2=where(specstat.pow2 ne 0,nw2)
  pow2=specstat[w2].pow2
  w3=where(specstat.pow3 ne 0,nw3)
  pow3=specstat[w3].pow3
;w4=where(specstat.pow4 ne 0,nw4)
;pow4=specstat[w4].pow4

  z=where(lcstat.z ne 0)
  zspecstat=specstats[z]
  wz0=where(zspecstat.pow ne 0,nw0)
  zpow=zspecstat[wz0].pow
  wz1=where(zspecstat.pow1 ne 0,nw1)
  zpow1=zspecstat[wz1].pow1
  wz2=where(zspecstat.pow2 ne 0,nw2)
  zpow2=zspecstat[wz2].pow2
  wz3=where(zspecstat.pow3 ne 0,nw3)
  zpow3=zspecstat[wz3].pow3
;wz4=where(zspecstat.pow4 ne 0,nw4)
;zpow4=zspecstat[wz4].pow4

;;kstest each pow distribution
  !p.multi=[0,2,2]
  pks='p!LKS!N = '
  kstwop,pow,zpow,d,prob,/plot,xtitle='Only 1 '+psm.beta,ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,pow1,zpow1,d,prob,/plot,xtitle=psm.beta+'!L1!N',ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,pow2,zpow2,d,prob,/plot,xtitle=psm.beta+'!L2!N',ytitle='p',xrange=[-2,3]
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/left,box=0
  kstwop,pow3,zpow3,d,prob,/plot,xtitle=psm.beta+'!L3!N',ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/left,box=0
;kstwop,pow4,zpow4,d,prob,/plot,title='Pow4'
;legend,['d = '+ntostr(d),'prob = '+ntostr(prob)],/bottom,/right,box=0

  !p.multi=0

  if keyword_set(ps) then endplot

  return
end
pro which_flux,seg,spec,flux,fluxerr,rate

  try=0
again:
  flux=dblarr(n_elements(seg)) & rate=flux & fluxerr=dblarr(n_elements(seg),2)
  w1=where(seg eq 1,nw1)
  w2=where(seg eq 2,nw2)
  w3=where(seg eq 3,nw3)
  w4=where(seg eq 4,nw4)

  if nw1 gt 0 then begin  
     flux[w1,*]=spec.flux1
;    fluxerr[w1,*]=spec.flux1err;[1]
     fluxerr[w1,0]=spec.flux1err[0]
     fluxerr[w1,1]=spec.flux1err[1]
  endif 
  if nw2 gt 0 then begin  
     flux[w2,*]=spec.flux2
;    fluxerr[w2,*]=spec.flux2err;[1]
     fluxerr[w2,0]=spec.flux2err[0]
     fluxerr[w2,1]=spec.flux2err[1]
  endif 
  if nw3 gt 0 then begin  
     flux[w3,*]=spec.flux3
;    fluxerr[w3,*]=spec.flux3err;[1]
     fluxerr[w3,0]=spec.flux3err[0]
     fluxerr[w3,1]=spec.flux3err[1]
  endif 
  if nw4 gt 0 then begin  
     flux[w4,*]=spec.flux4
;    fluxerr[w4,*]=spec.flux4err;[1]
     fluxerr[w4,0]=spec.flux4err[0]
     fluxerr[w4,1]=spec.flux4err[1]
  endif 

  if nw1 gt 0 then rate[w1]=spec.rate1
  if nw2 gt 0 then rate[w2]=spec.rate2
  if nw3 gt 0 then rate[w3]=spec.rate3
  if nw4 gt 0 then rate[w4]=spec.rate4

  w=where(flux eq 0,nw)
  if nw gt 0 then begin
     if seg[w[0]] ne 1 then seg[w]=seg[w]-1 else seg[w]=seg[w]+1
     try=try+1
     if try lt 3 then goto,again
  endif 
;    if w[0] ne 0 then begin
;        flux[w]=flux[w-1]
;        rate[w]=rate[w-1]
;    endif else begin
;        flux[w[0]]=flux[w[0]+1]
;        rate[w[0]]=rate[w[0]+1]
;    endelse 
;endif 

  return
end 
pro flux_times,fluxes,fluxerrors1,fluxerrors2,ps=ps
  specstat=mrdfits(!mdata+'spec_stats.fits',1)
  lcstat=mrdfits(!mdata+'lc_stats.fits',1)
  ngrb=n_elements(lcstat)

  lc_stats,tstart,tstop,exptime  ;;need to compile fit_lc2

  times=[1000.,3600.,1e4,3.6e4,1e5] ;1000s,1hr,1e4s,10hr,1e5s
  fluxes=dblarr(5,ngrb) & fluxerrors1=dblarr(5,ngrb) & fluxerrors2=fluxerrors1
  for i=0,ngrb-1 do begin 
     breaks=lcstat[i].nbreaks
     case breaks of
        -1: begin               ;NO FIT
        end
        0: begin                ;POW
           p=[lcstat[i].norm,lcstat[i].pow]
           perror=[lcstat[i].normerr,lcstat[i].powerr]

           fitcts=pow(times,p,perror=perror,yerror=yerror)
           if specstat[i].flux ne 0 then begin
              flux=specstat[i].flux
              rate=specstat[i].rate
              fluxerr=specstat[i].fluxerr
              fluxes[*,i]=flux/rate*fitcts

;fluxerrors[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr^2*(fitcts/rate)^2)
;                fluxerrors[*,i]=sqrt(fluxerr^2*(fitcts/rate)^2)
           endif 
        end 
        1: begin                ;BKNPOW
           if lcstat[i].pow1 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2]
              perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err]
           endif
           if lcstat[i].pow1 eq 0 and lcstat[i].pow2 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow2,lcstat[i].break2,lcstat[i].pow3]
              perror=[lcstat[i].normerr,lcstat[i].pow2err,lcstat[i].break2err,lcstat[i].pow3err]
           endif 
           if lcstat[i].pow1 eq 0 and lcstat[i].pow2 eq 0 and lcstat[i].pow3 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow3,lcstat[i].break3,lcstat[i].pow4]
              perror=[lcstat[i].normerr,lcstat[i].pow3err,lcstat[i].break3err,lcstat[i].pow4err]
           endif 
           pnames=['norm','pow1','break','pow2']

           fitcts=bknpow(times,p,seg=seg,perror=perror,yerror=yerror)
           which_flux,seg,specstat[i],flux,fluxerr,rate

           fluxes[*,i]=flux/rate*fitcts
;            fluxerrors[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr^2*(fitcts/rate)^2)
;            fluxerrors[*,i]=sqrt(fluxerr^2*(fitcts/rate)^2)
        end 
        2: begin                ;BKN2POW
           if lcstat[i].pow1 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2,$
                 lcstat[i].break2,lcstat[i].pow3]
              perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err,$
                      lcstat[i].break3err,lcstat[i].pow3err]
           endif else begin 
              p=[lcstat[i].norm,lcstat[i].pow2,lcstat[i].break2,lcstat[i].pow3,$
                 lcstat[i].break3,lcstat[i].pow4]
              perror=[lcstat[i].normerr,lcstat[i].pow2err,lcstat[i].break2err,lcstat[i].pow3err,$
                      lcstat[i].break3err,lcstat[i].pow4err]
           endelse 
           pnames=['norm','pow1','break1','pow2','break2','pow3']

           fitcts=bkn2pow(times,p,seg=seg,perror=perror,yerror=yerror)
           which_flux,seg,specstat[i],flux,fluxerr,rate
           fluxes[*,i]=flux/rate*fitcts
;            fluxerrors[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr^2*(fitcts/rate)^2)
;            fluxerrors[*,i]=sqrt(fluxerr^2*(fitcts/rate)^2)
        end 
        3: begin                ;BKN3POW
           p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2,$
              lcstat[i].break2,lcstat[i].pow3,lcstat[i].break3,lcstat[i].pow4]
           perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err,$
                   lcstat[i].break3err,lcstat[i].pow3err,lcstat[i].break3err,lcstat[i].pow4err]
           pnames=['norm','pow1','break1','pow2','break2','pow3','break3','pow4']

           fitcts=bkn3pow(times,p,seg=seg,perror=perror,yerror=yerror)
           which_flux,seg,specstat[i],flux,fluxerr,rate
           fluxes[*,i]=flux/rate*fitcts
;            fluxerrors[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr^2*(fitcts/rate)^2)
;            fluxerrors[*,i]=sqrt(fluxerr^2*(fitcts/rate)^2)
        end 
     endcase 
     
     fluxerrors1[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr[0]^2*(fitcts/rate)^2)
     fluxerrors2[*,i]=sqrt(yerror^2*(flux/rate)^2+fluxerr[1]^2*(fitcts/rate)^2)
     
;            fluxerrors[*,i]=sqrt(fluxerr^2*(fitcts/rate)^2)

;stop
  endfor

;;;NEED TO CALC FLUX ERRORS
  if keyword_set(ps) then begplot,name='flux_times.ps',/color
  color=[!red,!blue,!green,!magenta,!cyan]
  leg=['1 ks','1 hour','10 ks','10 hours','100 ks']
  fmean=dblarr(5) & fstd=fmean
  erase
  
  multiplot,[1,5],/init
;plot,[-10,140],[1e-14,1e-9],/nodata,xtitle=xtitle,ytitle=ytitle,/ylog,/xstyle

  for i=0,4 do begin 
     if i eq 4 then xtitle='Burst Number' else xtitle=''
     if i eq 2 then ytitle='Flux!L0.2-10.0 keV!N (erg cm!U-2!N s!U-1!N)' else ytitle=''
     multiplot
     w=where(finite(fluxes[i,*]) and fluxes[i,*] ne 0 and fluxes[4,*] gt 1e-15 and fluxes[i,*] lt 1e-8,nw) ; and fluxerrors1[i,*] ne 0 and finite(fluxerrors1[i,*]) and fluxerrors2[i,*] ne 0 and finite(fluxerrors2[i,*]),nw)
     plot,[0,200],[1e-15,1e-8],/nodata,xtitle=xtitle,ytitle=ytitle,/ylog
     plotsym,0,1.0,/fill
     wextrap=where(tstart[w] gt times[i] or tstop[w] lt times[i] or fluxes[i,w] lt 5d-15,nwextrap)
     psym=intarr(nw)
     psym[*]=8
     if nwextrap gt 0 then psym[wextrap]=5
     crazy=where(fluxes[i,w] lt 1d-15,nc)
     
     if nc gt 0 then print,psym[crazy],lcstat[w[crazy]].grb,w[crazy]
;oploterror,w+1,fluxes[i,w],fluxerrors[i,w],color=color[i],psym=8,errcolor=color[i],/nohat
     err0=fluxes[i,w]-fluxerrors1[i,w]
     w0=where(err0 lt 0)
     err0[w0]=1d-20
     for j=0,nw-1 do begin
        plots,w[j]+1,fluxes[i,w[j]],color=color[i],psym=psym[j],symsize=0.7

;        oplot,[w[j]+1,w[j]+1],[err0[j],fluxes[i,w[j]]+fluxerrors2[i,w[j]]],color=color[i]
     endfor 
;    fmean[i]=median(fluxes[i,w])
     fmean[i]=10^mean(alog10(fluxes[i,w]))
;    fstd[i]=sqrt(total(fluxerrors2[i,w]^2))
     fstd[i]=stddev(alog10(fluxes[i,w]))
;    fmean[i]=weighted_mean(fluxes[i,w],fluxerrors1[i,w],err)
;    fstd[i]=err
     oplot,[0,200],[fmean[i],fmean[i]],line=0,color=color[i]
;    oplot,[0,200],[fmean[i]+fstd[i],fmean[i]+fstd[i]],line=1,color=color[i]
;    oplot,[0,200],[fmean[i]-fstd[i],fmean[i]-fstd[i]],line=1,color=color[i]    

;    err=(10^(fstd[i]+alog10(fmean[i]))-10^(alog10(fmean[i])-fstd[i]))/2.
     legend,leg[i],/top,/right,charsize=1.5,box=0,margin=0
     legend,['mean (log flux) ='+sigfig(alog10(fmean[i]),3),!tsym.sigma+'(log flux) ='+sigfig(fstd[i],2)],/bottom,/right,box=0,charsize=1,margin=0
;    legend,['interpolation','extrapolation'],psym=[8,5],symsize=0.7
;    legend,[leg[i],'','','','','','median='+sigfig(fmean[i],3,/sci),'log '+!tsym.sigma+'='+sigfig(fstd[i],3)],/top,/right,box=0,charsize=1,margin=0

  endfor  
  multiplot,/reset
;legend,leg,textcolor=color,/top,/right,box=0
  if keyword_set(ps) then endplot else k=get_kbrd(10)
  erase
  if keyword_set(ps) then begplot,name='flux_means.ps',/land,/color
  plot,[800,2e5],[1e-14,1e-8],/nodata,/xlog,/ylog,xtitle='Time since BAT trigger (s)',ytitle='Flux!L0.2-10.0 keV!N (erg cm!U-2!N s!U-1!N)'
  plotsym,0,1,/fill
  err1=10^(fstd+alog10(fmean))
  err2=10^(alog10(fmean)-fstd)
  for i=0,4 do begin
;    oploterror,times[i],fmean[i],err[i],/nohat,color=color[i],errcolor=color[i],psym=8
     plots,times[i],fmean[i],psym=8,color=color[i]
     oplot,[times[i],times[i]],[err2[i],err1[i]],color=color[i]
;    oplot,times[i],[fmean[i]-fstd[i],fmean[i]
  endfor 
  legend,leg,textcolor=color,box=0,/top,/right

  if keyword_set(ps) then endplot
  stop
  return
end 
pro plot_spec_lc_stats,ps=ps  

  if keyword_set(ps) then begin
     begplot,name='alpha_beta.ps',/color
     psm=!tsym
  endif else psm=!vsym
  specstat=mrdfits(!mdata+'spec_stats.fits',1)
  lcstat=mrdfits(!mdata+'lc_stats.fits',1)

  w0=where(specstat.pow ne 0,nw0)
  w1=where(specstat.pow1 ne 0 and lcstat.pow1err lt 100,nw1)
  w2=where(specstat.pow2 ne 0 and abs(lcstat.pow2err) gt 0,nw2)
  w3=where(specstat.pow3 ne 0,nw3)
  w4=where(specstat.pow4 ne 0,nw4)
;wset,0
;!p.multi=[0,2,1]


;;;ALPHA-BETA PLOT
  plotsym,0,0.7,/fill
  plot,[-3,7],[-2,14],/nodata,xtitle=psm.beta,ytitle=psm.alpha,/iso,/xstyle,/ystyle
  oploterror,specstat[w0].pow,lcstat[w0].pow,specstat[w0].powerr[1],lcstat[w0].powerr,psym=8,/nohat,color=!green,errcolor=!green
  oploterror,specstat[w1].pow1,lcstat[w1].pow1,specstat[w1].pow1err[1],lcstat[w1].pow1err,psym=8,/nohat,color=!p.color,errcolor=!p.color
  oploterror,specstat[w2].pow2,lcstat[w2].pow2,specstat[w2].pow2err[1],lcstat[w2].pow2err,psym=8,/nohat,color=!red,errcolor=!red
  oploterror,specstat[w3].pow3,lcstat[w3].pow3,specstat[w3].pow3err[1],lcstat[w3].pow3err,psym=8,/nohat,color=!blue,errcolor=!blue
  oploterror,specstat[w4].pow4,lcstat[w4].pow4,specstat[w4].pow4err[1],lcstat[w4].pow4err,psym=8,/nohat,color=!orange,errcolor=!orange

  legend,['Only 1 '+psm.Alpha+' ('+ntostr(nw0)+')',''+psm.Alpha+'1 ('+ntostr(nw1)+')',$
          ''+psm.Alpha+'2 ('+ntostr(nw2)+')',''+psm.Alpha+'3 ('+ntostr(nw3)+')',$
          ''+psm.Alpha+'4 ('+ntostr(nw4)+')'],$
         textcolor=[!green,!p.color,!red,!blue,!orange],box=0,/top,/right
  if keyword_set(ps) then endplot

  if keyword_set(ps) then begplot,name='beta_hist.ps',/color else k=get_kbrd(10)

;;;BETA HISTOGRAMS
  erase
  xrange=[0,6] & yrange=[0,8]
  bin=0.05
  multiplot,[1,5],/init
  multiplot
  plot,xrange,yrange,/nodata
  plothist,specstat[w0].pow,bin=bin,color=!green,/overplot
  legend,['Only 1 '+psm.beta],/top,/right,box=0,textcolor=!green
  multiplot
  plot,xrange,yrange,/nodata
  plothist,specstat[w1].pow1,bin=bin,color=!p.color,/overplot
  legend,[psm.beta+'!L1!N'],/top,/right,box=0,textcolor=!p.color
  multiplot
  plot,xrange,yrange,/nodata
  plothist,specstat[w2].pow2,bin=bin,color=!red,/overplot
  legend,[psm.beta+'!L2!N'],/top,/right,box=0,textcolor=!red
  multiplot
  plot,xrange,yrange,/nodata
  plothist,specstat[w3].pow3,bin=bin,color=!blue,/overplot
  legend,[psm.beta+'!L3!N'],/top,/right,box=0,textcolor=!blue
  multiplot
  plot,xrange,yrange,/nodata,xtitle=psm.beta
  plothist,specstat[w4].pow4,bin=bin,color=!orange,/overplot
  legend,[psm.beta+'!L4!N'],/top,/right,box=0,textcolor=!orange

  multiplot,/reset

  if keyword_set(ps) then endplot

;;;DELTA BETA HISTOGRAMS
  w12=where(specstat.pow1 ne 0 and specstat.pow2 ne 0)
  dbeta12=specstat[w12].pow1-specstat[w12].pow2
  w23=where(specstat.pow2 ne 0 and specstat.pow3 ne 0)
  dbeta23=specstat[w23].pow2-specstat[w23].pow3
  if keyword_set(ps) then begplot,name='delta_beta.ps',/land,/color else k=get_kbrd(10)
  xrange=[-2,2]
  bin=0.1
  !p.multi=[0,2,1]
  aplothist,dbeta12,bin=bin,xtitle=psm.delta_cap+psm.beta+'!L12!N',xrange=xrange,xstyle=1
  aplothist,dbeta23,bin=bin,xtitle=psm.delta_cap+psm.beta+'!L23!N',xrange=xrange,xstyle=1
  !p.multi=0
  if keyword_set(ps) then endplot

;;;ADDING BETA WEIGHTED MEANS
;window,1
;ploterror,lcstat[w1].pow1,2.+specstat[w1].pow1,lcstat[w1].pow1err,specstat[w1].pow1err[1],/nohat,psym=3,/iso

;awmean=dblarr(5) & awmeanerr=awmean & bwmean=awmean & bwmeanerr=awmean
;awmean[0]=weighted_mean(lcstat[w0].pow,lcstat[w0].powerr,err)
;awmeanerr[0]=err
;bwmean[0]=weighted_mean(specstat[w0].pow,specstat[w0].powerr[1],err)
;bwmeanerr[0]=err
;awmean[1]=weighted_mean(lcstat[w1].pow1,lcstat[w1].pow1err,err)
;awmeanerr[1]=err
;bwmean[1]=weighted_mean(specstat[w1].pow1,specstat[w1].pow1err[1],err)
;bwmeanerr[1]=err
;awmean[2]=weighted_mean(lcstat[w2].pow2,lcstat[w2].pow2err,err)
;awmeanerr[2]=err
;bwmean[2]=weighted_mean(specstat[w2].pow2,specstat[w2].pow2err[1],err)
;bwmeanerr[2]=err
;awmean[3]=weighted_mean(lcstat[w3].pow3,lcstat[w3].pow3err,err)
;awmeanerr[3]=err
;bwmean[3]=weighted_mean(specstat[w3].pow3,specstat[w3].pow3err[1],err)
;bwmeanerr[3]=err
;awmean[4]=weighted_mean(lcstat[w4].pow4,lcstat[w4].pow4err,err)
;awmeanerr[4]=err
;bwmean[4]=weighted_mean(specstat[w4].pow4,specstat[w4].pow4err[1],err)
;bwmeanerr[4]=err

;;wset,1
;color=[!green,!p.color,!red,!blue,!orange]
;xrange=[1,3];prange(bwmean,bwmeanerr)
;yrange=[0,8];prange(awmean,awmeanerr)
;;plot,xrange,yrange,/nodata,/iso,xtitle=psm.beta,ytitle=psm.alpha
;for i=0,4 do oploterror,bwmean[i],awmean[i],bwmeanerr[i],awmeanerr[i],color=color[i],errcolor=color[i],psym=2,/nohat,symsize=2

;!p.multi=0

  stop
  return
end 
pro specstats,specstat
  cd,!mdata
  lcstat=mrdfits(!mdata+'lc_stats.fits',1)
  dir=strtrim(lcstat.grb,2)
  wdir=where(lcstat.nbreaks ne -1,nw)

  specstat=replicate({ grb:'',$
                       mode:'',$
                       nh:0d,$
                       nherr:dblarr(2),$
                       pow:0d,$
                       powerr:dblarr(2),$
                       norm:0d,$
                       normerr:dblarr(2),$
                       chisq:0d,$
                       dof:0,$
                       flux:0d,$
                       fluxerr:dblarr(2),$
                       rate:0d,$
                       nev:0L,$
                       mode1:'',$
                       nh1:0d,$
                       nh1err:dblarr(2),$
                       pow1:0d,$
                       pow1err:dblarr(2),$
                       norm1:0d,$
                       norm1err1:dblarr(2),$
                       chisq1:0d,$
                       dof1:0,$
                       flux1:0d,$
                       flux1err:dblarr(2),$
                       rate1:0d,$
                       nev1:0L,$
                       mode2:'',$
                       nh2:0d,$
                       nh2err:dblarr(2),$
                       pow2:0d,$
                       pow2err:dblarr(2),$
                       norm2:0d,$
                       norm2err:dblarr(2),$
                       chisq2:0d,$
                       dof2:0,$
                       flux2:0d,$
                       flux2err:dblarr(2),$
                       rate2:0d,$
                       nev2:0L,$
                       mode3:'',$
                       nh3:0d,$
                       nh3err:dblarr(2),$
                       pow3:0d,$
                       pow3err:dblarr(2),$
                       norm3:0d,$
                       norm3err:dblarr(2),$
                       chisq3:0d,$
                       dof3:0,$
                       flux3:0d,$
                       flux3err:dblarr(2),$
                       rate3:0d,$
                       nev3:0L,$
                       mode4:'',$
                       nh4:0d,$
                       nh4err:dblarr(2),$
                       pow4:0d,$
                       pow4err:dblarr(2),$
                       norm4:0d,$
                       norm4err:dblarr(2),$
                       chisq4:0d,$
                       dof4:0,$
                       flux4:0d,$
                       flux4err:dblarr(2),$
                       rate4:0d,$
                       nev4:0L},n_elements(lcstat))


  for ii=0,nw-1 do begin
     i=wdir[ii]
     specdir=dir[i]+'/spec2'
     if not exist(specdir) then specdir=dir[i]+'/spec'
     if exist(specdir) then begin 
        cd,specdir
        specfile='specstat.fits'
        if exist(specfile) then begin 
           specdat=mrdfits(specfile,1,/silent)
           lc=lcstat[i]
           breaks=lc.nbreaks
           specstat[i].grb=lc.grb
           case breaks of
              0: begin
                 mchi=max(specdat.chisq/specdat.dof,j1)
                 for k=1,13 do  specstat[i].(k)=specdat[j1].(k)
              end
              1: begin
                 ;; 1-2
                 if lc.break2 eq 0 and lc.break3 eq 0 then begin
                    seg1=1 & seg2=2
                    krange1=[14,26] & krange2=[27,39]
                 endif 
                 ;; 2-3
                 if lc.break1 eq 0 and lc.break3 eq 0 then begin
                    seg1=2 & seg2=3
                    krange1=[27,39] & krange2=[40,52]
                 endif
                 ;; 3-4
                 if lc.break1 eq 0 and lc.break2 eq 0 then begin
                    seg1=2 & seg2=3
                    krange1=[40,52] & krange2=[53,65]
                 endif
                 w1=where(specdat.segment eq seg1,nw1)
                 if nw1 gt 1 then mchi=max(specdat[w1].chisq1/specdat[w1].dof1,j1) else j1=0
                 w2=where(specdat.segment eq seg2,nw2)
;                    if nw1 eq 0 then begin
;                        w1=w2
;                        j1=j2
;                    endif 
;                    if nw2 eq 0 then begin
;                        w2=w1
;                        j2=j1
;                    endif 
                 if nw1 ne 0 then begin 
                    if nw2 gt 1 then mchi=max(specdat[w2].chisq2/specdat[w2].dof2,j2) else j2=0
                    for k=krange1[0],krange1[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w1[j1]].(j)
                    endfor 
                 endif 
                 if nw2 ne 0 then begin 
                    for k=krange2[0],krange2[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w2[j2]].(j)
                    endfor 
                 endif 
              end
              2: begin
                 ;; 1-2-3
                 if lc.break3 eq 0 then begin
                    seg1=1 & seg2=2 & seg3=3
                    krange1=[14,26] & krange2=[27,39] & krange3=[40,52] 
                 endif

                 ;; 2-3-4
                 if lc.break1 eq 0 then begin
                    seg1=2 & seg2=3 & seg3=4
                    krange1=[27,39] & krange2=[40,52] & krange3=[53,65]
                 endif
                 
                 w1=where(specdat.segment eq seg1,nw1)
                 if nw1 gt 1 then mchi=max(specdat[w1].chisq1/specdat[w1].dof1,j1) else j1=0
                 w2=where(specdat.segment eq seg2,nw2)
                 if nw2 gt 1 then mchi=max(specdat[w2].chisq2/specdat[w2].dof2,j2) else j2=0
                 w3=where(specdat.segment eq seg3,nw3)
                 if nw3 gt 1 then mchi=max(specdat[w3].chisq3/specdat[w3].dof3,j3) else j3=0
                 if nw2 eq 0 then begin
                    w2=w1
                    j2=j1
                 endif 
;                    if nw3 eq 0 then begin
;                        w3=w2
;                        j3=j2
;                    endif 
                 if nw1 ne 0 then begin 
                    for k=krange1[0],krange1[1] do begin
                       j=k mod 13
                       specstat[i].(k)=specdat[w1[j1]].(j)
                    endfor 
                 endif 
                 if nw2 ne 0 then begin 
                    for k=krange2[0],krange2[1] do begin
                       j=k mod 13
                       specstat[i].(k)=specdat[w2[j2]].(j)
                    endfor 
                 endif 
                 if nw3 ne 0 then begin 
                    for k=krange3[0],krange3[1] do begin
                       j=k mod 13
                       specstat[i].(k)=specdat[w3[j3]].(j)
                    endfor 
                 endif 
              end
              3: begin
                 seg1=1 & seg2=2 & seg3=3 & seg4=4
                 krange1=[14,26] & krange2=[27,39] & krange3=[40,52] & krange4=[53,65]
                 w1=where(specdat.segment eq seg1,nw1)
                 if nw1 gt 1 then mchi=max(specdat[w1].chisq1/specdat[w1].dof1,j1) else j1=0
                 w2=where(specdat.segment eq seg2,nw2)
                 if nw2 gt 1 then mchi=max(specdat[w2].chisq2/specdat[w2].dof2,j2) else j2=0
                 w3=where(specdat.segment eq seg3,nw3)
                 if nw3 gt 1 then mchi=max(specdat[w3].chisq3/specdat[w3].dof3,j3) else j3=0
                 w4=where(specdat.segment eq seg4,nw4)
                 if nw4 gt 1 then mchi=max(specdat[w4].chisq4/specdat[w4].dof4,j4) else j4=0
;                    if nw2 eq 0 then begin
;                        w2=w1
;                        j2=j1
;                    endif 
;                    if nw3 eq 0 then begin
;                        w3=w2
;                        j3=j2
;                    endif 
;                    if nw4 eq 0 then begin
;                        w4=w3
;                        j4=j3
;                    endif 
                 if nw1 ne 0 then begin 
                    for k=krange1[0],krange1[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w1[j1]].(j)
                    endfor 
                 endif 
                 if nw2 ne 0 then begin 
                    for k=krange2[0],krange2[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w2[j2]].(j)
                    endfor 
                 endif 
                 if nw3 ne 0 then begin 
                    for k=krange3[0],krange3[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w3[j3]].(j)
                    endfor 
                 endif 
                 if nw4 ne 0 then begin 
                    for k=krange4[0],krange4[1] do begin 
                       j=k mod 13
                       specstat[i].(k)=specdat[w4[j4]].(j)
                    endfor 
                 endif 
              end 
           endcase 

        endif
     endif  
     cd,!mdata
  endfor 
  tags=tag_names(specstat)
  w=where(strpos(tags,'ERR') ne -1,nw)
  for i=0,nw-1 do begin
     t1=w[i]
     t2=t1-1
     value=specstat.(t2)
     val=dblarr(2,n_elements(specstat))
     val[0,*]=value
     val[1,*]=value
     specstat.(t1)=specstat.(t1)-val
  endfor 
  mwrfits,specstat,'spec_stats.fits',/create


  return
end 

pro spec_dats,lcstat
  specstat={ segment:0,$
             mode:'',$
             nh:0d,$
             nh_err:dblarr(2),$
             pow:0d,$
             pow_err:dblarr(2),$
             norm:0d,$
             norm_err:dblarr(2),$
             chisq:0d,$
             dof:0,$
             flux:0d,$
             flux_err:dblarr(2),$
             rate:0d,$
             nev:0L}

  cd,!mdata
  lcstat=mrdfits(!mdata+'lc_stats.fits',1)
  dir=strtrim(lcstat.grb,2)
  wdir=where(lcstat.nbreaks ne -1,nw)

  for i=0,nw-1 do begin
     specdir=dir[wdir[i]]+'/spec2'
     if not exist(specdir) then specdir=dir[wdir[i]]+'/spec'
     if exist(specdir) then begin 
        cd,specdir
        segfiles=file_search('seg*dat')
        if segfiles[0] ne '' then begin 
           nseg=n_elements(segfiles)
           sp=replicate(specstat,nseg)
           for j=0,nseg-1 do begin
              m=strpos(segfiles[j],'wt')
              if m eq -1 then mode='pc' else mode='wt'
              if numlines(segfiles[j]) le 2 then begin
                 readcol,segfiles[j],blah,nev,format='(a,l)',/silent
                 sp[j].segment=j+1    
                 sp[j].mode=mode
                 sp[j].nev=nev[0]
              endif else begin 
                 openr,lun,segfiles[j],/get_lun
                 sp[j].segment=j+1    
                 sp[j].mode=mode
                 line=readline(lun,delim=' ')
                 sp[j].nh_err=line[1:2]
                 line=readline(lun,delim=' ')
                 sp[j].pow_err=line[1:2]
                 line=readline(lun,delim=' ')
                 sp[j].norm_err=line[1:2]
                 line=readline(lun,delim=' ')
                 sp[j].nh=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].pow=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].norm=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].chisq=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].dof=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].flux=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].flux_err=line[1:2]
                 line=readline(lun,delim=' ')
                 sp[j].rate=line[1]
                 line=readline(lun,delim=' ')
                 sp[j].nev=line[1]
                 close,lun
                 free_lun,lun
              endelse 
           endfor 
           mwrfits,sp,'specstat.fits',/create
        endif 
     endif 
     cd,!mdata
  endfor 

  return
end 

pro fit_lc_spec

  g=0
  cd,!mdata
  lcstat=mrdfits(!mdata+'lc_stats.fits',1)

  dir=strtrim(lcstat.grb,2)     ;file_search('GRB*')

  mn=0d
  mx=1d9
  mode=['wt','pc']
  wdir=where(lcstat.nbreaks ne -1,nw)
  respfile='/bulk/pkg/caldb/data/swift/xrt/cpf/rmf/'+['swxwt0_20010101v008.rmf','swxpc0to12_20010101v008.rmf']
  specdir='spec'
  stop
  for i=g,nw-1 do begin 
     cd,dir[wdir[i]]
     if not exist(specdir) then spawn,'mkdir '+specdir
     print,i
     print,dir[wdir[i]]
     lc=lcstat[wdir[i]]
     breaks=lc.nbreaks

     skipgrb=0
     if exist(specdir+'/seg1pc.dat') then skipgrb=1

     lcfile='lc_wrap3.par'
     if not exist(lcfile) then lcfile='lc_wrap.par'
     if not exist(lcfile) then begin
        print,'Need lc_wrap par file!!!'
        skipgrb=1
        stop
     endif else begin
        skipgrb=0
;        readcol,lcfile,parname,par,format='(a,a)',/silent
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

        cd,specdir

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
        write_regfile,'src.reg',srcra,srcdec,20
        write_regfile,'bg.reg',bgra,bgdec,40
        trigtime=par[5]
        evfiles=str_sep(tmp,' ')
        evfiles='../'+evfiles[1:*]
     endelse 

     if not skipgrb then begin 
        ffile='flares_gtis.dat'
;    if exist(ffile) then flares=1 else flare=0
        if exist(ffile) then begin
           readcol,ffile,fstart,fstop,format='(d,d)',delim=' '
           fstart=fstart+trigtime
           fstop=fstop+trigtime
           flares=1
        endif else flares=0

        case breaks of
           0: begin
              tmin=mn
              tmax=mx
           end
           1: begin
              break1=lc.break1
              if lc.break1 eq 0 then $
                 if lc.break2 eq 0 then break1=lc.break3 else break1=lc.break2
              tmin=[mn,break1]
              tmax=[break1,mx]
           end
           2: begin
              break1=lc.break1
              break2=lc.break2
              if lc.break1 eq 0 then begin
                 break1=lc.break2
                 break2=lc.break3
              endif 
              tmin=[mn,break1,break2]
              tmax=[break1,break2,mx]
           end
           3: begin
              tmin=[mn,lc.break1,lc.break2,lc.break3]
              tmax=[lc.break1,lc.break2,lc.break3,mx]
           end 
        endcase 

        tmin=tmin+trigtime ;;need to subtract off time from start of ev file
        tmax=tmax+trigtime

        for j=0,breaks do begin 

           wwt=strpos(evfiles,'wt')
           wt=where(wwt ne -1,nwt)
           pc=where(wwt eq -1 and strpos(evfiles,'w3') eq -1,npc)
           for k=0,1 do begin  ;;WT/PC
              nogo=0
              if k eq 0 and nwt gt 0 then begin 
                 w=wt 
                 find_wt_srcdetpos,evfiles[w[0]],srcra,srcdec,x_int,y_int,pa,wtsrcra,wtsrcdec
                 find_wt_srcdetpos,evfiles[w[0]],bgra,bgdec,x_int,y_int,pa,wtbgra,wtbgdec
                 srcreg='src_wt.reg'
                 bgreg='bg_wt.reg'
                 write_regfile,srcreg,wtsrcra,wtsrcdec,40
                 write_regfile,bgreg,wtbgra,wtbgdec,40

              endif else nogo=1
              if k eq 1 then begin 
                 nogo=0
                 w=pc
                 srcreg='src.reg'
                 bgreg='bg.reg'
              endif 
;            ehdr=headfits(evfiles[w[0]])
;            tsub=sxpar(ehdr,'TSTART')
              if not nogo then begin 
                 ev=mrdfits(evfiles[w[0]],1)
                 tsub=ev[0].time
                 if tmax[j]-tsub gt 0 then begin 

                    nw=n_elements(w)
                    ;;xselect script
                    xselfile='xsel.xco'
                    openw,lun,xselfile,/get_lun
                    printf,lun
                    printf,lun
                    printf,lun,'clear all'
                    printf,lun,'yes'
                    printf,lun
                    printf,lun,'query yes'
                    printf,lun
                    printf,lun,'read ev '+evfiles[w[0]]
                    printf,lun,'./'
                    printf,lun
                    if nw gt 1 then printf,lun,'read ev '+evfiles[w[1:*]]
                    printf,lun
                    printf,lun
                    printf,lun,'ext cu'
                    ;;src region spec
                    printf,lun,'filter region '+srcreg
                    if flares then begin 
                       printf,lun,'filter time scc'
                       printf,lun,'/ps'
                       printf,lun,'q'
                       for f=0,n_elements(fstart)-1 do printf,lun,'e '+sigfig(fstart[f]-tsub,12)+','+sigfig(fstop[f]-tsub,12)
                       printf,lun,'x'
                    endif 
                    printf,lun,'ext cu'
                    printf,lun,'filter time scc'
                    printf,lun,'/ps'
                    printf,lun,'q'
                    printf,lun,'i '+sigfig(tmin[j]-tsub,12)+','+sigfig(tmax[j]-tsub,12)
                    printf,lun,'x'
                    
;            printf,lun,'filter time scc "'+sigfig(tmin[j],12)+','+sigfig(tmax[j],12)+'"'
;            if flares then printf,lun,'filter time scc -"'+sigfig(fstart,12)+','+sigfig(fstop,12)+'"'
                    printf,lun,'ext cu'
                    printf,lun,'ext ev'
                    printf,lun,'save ev'
                    evname='seg'+ntostr(j+1)+mode[k]+'.evt'
                    printf,lun,evname
                    printf,lun,'yes'
                    printf,lun
                    printf,lun
                    printf,lun,'ext spec'
                    printf,lun,'save spec'
                    phaname='seg'+ntostr(j+1)+mode[k]+'.pha'
                    printf,lun,phaname
                    printf,lun
                    printf,lun

                    ;;bg region spec
                    printf,lun,'clear region'
                    printf,lun,'clear ev'
                    printf,lun,'ext cu'
                    printf,lun,'filter region '+bgreg
                    printf,lun,'ext cu'
                    printf,lun,'ext ev'
                    printf,lun,'save ev'
                    bgevname='seg'+ntostr(j+1)+mode[k]+'bg.evt'
                    printf,lun,bgevname
                    printf,lun
                    printf,lun
                    printf,lun
                    printf,lun,'ext spec'
                    printf,lun,'save spec'
                    bgphaname='seg'+ntostr(j+1)+mode[k]+'bg.pha'
                    printf,lun,bgphaname
                    printf,lun
                    printf,lun
                    
                    printf,lun,'exit'
                    printf,lun
                    close,lun
                    free_lun,lun
                    arfname='seg'+ntostr(j+1)+mode[k]+'.arf'
; stop
                    spawn,'xselect @'+xselfile +' > xselect.out'

                    ;;check for enough events to make spectrum
                    ev=mrdfits(evname,1)
                    nev=n_elements(ev)
                    print,nev

                    if nev ge 100 then begin 
                       ;;grppha & xrtmkarf script
                       phaname2=phaname+'.grpmin20'
                       grppha='grppha infile="'+phaname+'" outfile="'+phaname2+'" chatter=0 comm="chkey ANCRFILE '+arfname+' & chkey RESPFILE '+respfile[k]+' & chkey BACKFILE '+bgphaname+' & group min 20 & exit" clobber=yes'
                       print,grppha
                       spawn,grppha
                       
                       xrtmkarf='xrtmkarf outfile='+arfname+' phafile='+phaname+' srcx=-1 srcy=-1 psfflag=yes clobber=yes'
                       print,xrtmkarf
                       spawn,xrtmkarf

                       ;;nH - get galactic nH
                       spawn,'nh 2000. '+ntostr(srcra)+' '+ntostr(srcdec)+' > nhgal.out'
                       readcol,'nhgal.out',blah1,blah2,blah3,nh,format='(a,a,a,d)',delim=' '
                       nh=nh[n_elements(nh)-1]*1e-22

                       ;;xspec script
                       xbfile='xspec'+ntostr(j+1)+'.batch'
                       openw,lun,xbfile,/get_lun
                       printf,lun,'data '+phaname2
                       printf,lun,'ignore bad'
                       printf,lun,'ignore 0.-0.2'
                       printf,lun,'query yes'
                       printf,lun,'mo wabs(pow)'
                       printf,lun,'1.00001'
                       printf,lun,'1.00002'
                       printf,lun,'1.00003'
                       printf,lun
                       printf,lun,'newpar 1 '+ntostr(nh)
                       printf,lun,'fit 1000'
                       printf,lun,'fit 1000'
                       printf,lun,'fit 1000'
                       printf,lun,'cpd /ps'
                       printf,lun,'setplot en'
                       printf,lun,'plot ld res'
                       printf,lun,'exec mv pgplot.ps '+phaname+'.pow.ps'
                       printf,lun
                       ;;tcl stuff
                       datfile='seg'+ntostr(j+1)+mode[k]+'.dat'
                       printf,lun,'set fileid [open '+datfile+' w]'

                        ;;;errors
                       printf,lun,'error stop 100,,maximum 20.0 1'
                       printf,lun,'tclout error 1'
                       printf,lun,'set err1 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $err1 { } cerr1'
                       printf,lun,'set lerr1 [split $cerr1]'
                       printf,lun,'puts $fileid "nH_err [lindex $lerr1 0] [lindex $lerr1 1]"'

                       printf,lun,'error stop 100,,maximum 20.0 2'
                       printf,lun,'tclout error 2'
                       printf,lun,'set err2 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $err2 { } cerr2'
                       printf,lun,'set lerr2 [split $cerr2]'
                       printf,lun,'puts $fileid "Pow_err [lindex $lerr2 0] [lindex $lerr2 1]"'

                       printf,lun,'error stop 100,,maximum 20.0 3'
                       printf,lun,'tclout error 3'
                       printf,lun,'set err3 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $err3 { } cerr3'
                       printf,lun,'set lerr3 [split $cerr3]'
                       printf,lun,'puts $fileid "norm_err [lindex $lerr3 0] [lindex $lerr3 1]"'

                        ;;; fit params
                       printf,lun,'tclout param 1'
                       printf,lun,'set par1 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $par1 { } cpar1'
                       printf,lun,'set lpar1 [split $cpar1]'
                       printf,lun,'puts $fileid "nH [lindex $lpar1 0]"'

                       printf,lun,'tclout param 2'
                       printf,lun,'set par2 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $par2 { } cpar2'
                       printf,lun,'set lpar2 [split $cpar2]'
                       printf,lun,'puts $fileid "PhInd [lindex $lpar2 0]"'
                       
                       printf,lun,'tclout param 3'
                       printf,lun,'set par3 [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $par3 { } cpar3'
                       printf,lun,'set lpar3 [split $cpar3]'
                       printf,lun,'puts $fileid "norm [lindex $lpar3 0]"'

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
                       printf,lun,'flux 0.2 10. err 10000 90'
                       printf,lun,'tclout flux 1'
                       printf,lun,'set flx [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $flx { } cflx'
                       printf,lun,'set lflx [split $cflx]'
                       printf,lun,'puts $fileid "flux [lindex $lflx 0]"'
                       printf,lun,'puts $fileid "flux_err [lindex $lflx 1] [lindex $lflx 2]"'
                       
                ;;;rate
                       printf,lun,'show rate'
                       printf,lun,'tclout rate 1'
                       printf,lun,'set rte [string trim $xspec_tclout]'
                       printf,lun,'regsub -all { +} $rte { } crte'
                       printf,lun,'set lrte [split $crte]'
                       printf,lun,'puts $fileid "rate [lindex $lrte 0]"'

                       printf,lun,'close $fileid'
                       printf,lun,'exit'
                       printf,lun

                       close,lun
                       free_lun,lun

                       spawn,'xspec - '+xbfile+' > xspec.out'
                       
                       openu,lun,datfile,/get_lun,/append
                       printf,lun,'N_ev '+ntostr(nev)
                       close,lun
                       free_lun,lun
                    endif else print,'Not enough events for spectrum'
                 endif else begin
                    print,'Problem with times'
                    stop 
                 endelse 
              endif  
           endfor  
        endfor 
     endif 
     cd,!mdata
  endfor 

;spec_dats,lcstat






;;need GTIs for segments (breaks) and flares to exclude & WT/PC
;;xselect on regions and time intervals
;;check if Nevt < 500, then use previous segment or whole spectrum together
;;grppha on evt files
;;xrtmkarf
;;xspec script on each segment fitting wabs(pow)
;;get flux to counts conversion



;;refit all LCs with flares to get flare_gtis - done
;;run this program on everything - done
;;plot alpha vs beta
;;plot flux LCs without flares
;;calc flux at 1 hour, etc. (like Nousek et al.)
;;test closure relations 

  return
end 
