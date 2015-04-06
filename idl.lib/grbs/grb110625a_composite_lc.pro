pro prompt_emission
  
  lc4=mrdfits('~/GRBs/GRB110625A/BAT/00456073000-bat/lc/sw00456073000b_4chan_64ms.lc',1)
  lc1=mrdfits('~/GRBs/GRB110625A/BAT/00456073000-bat/lc/sw00456073000b_1chan_64ms.lc',1)
  nb=mrdfits('~/GRBs/GRB110625A/GBM/nb_lc.fits',1)
  b1=mrdfits('~/GRBs/GRB110625A/GBM/b1_lc.fits',1)

  trigtime=330728915.52d
  gtrigtime=330728900d
  erange=[[15,350],[15,25],[25,50],[50,100],[100,350],[8,1000],[1000,40000]]

  begplot,name='~/Dropbox/Papers/InPrep/GRB110625A/BAT_LC.eps',/encap,font='helvetica',/color
  multiplot,[1,6],/init
  for i=1,6 do begin
     j=i
     if i gt 0 then begin 
        lc=lc4 
        j=i-1
     endif else lc=lc1
     if i eq 5 then lc=nb
     if i eq 6 then lc=b1

     multiplot
     if i eq 6 then xtitle='Time since BAT trigger (s)' else xtitle=''
     if i eq 3 then ytitle='Mask-tagged Count Rate (counts s!L-1!N)' else ytitle=''
     if i le 4 then $
        ploterror,lc.time-trigtime,lc.rate[j],lc.error[j],/nohat,xtitle=xtitle,ytitle=ytitle,xrange=[-20,30],psym=10
     if i ge 5 then ploterror,lc.time-trigtime+4.,lc.counts,lc.error,/nohat,xtitle=xtitle,ytitle=ytitle,xrange=[-20,30],psym=10
     oplot,[-100,100],[0,0],line=2,color=!red
     legend,ntostr(erange[0,i])+' - '+ntostr(erange[1,i])+' keV',/top,/left,box=0
  endfor 
  multiplot,/default,/reset
  endplot
  return
end 

pro grb110625a_composite_lc,justflux=justflux,ps=ps,dofit=dofit

  cd,'~/GRBs/GRB110625A/'
  readcol,'LAT/LAT_lc.dat',fitnum,tstart,tend,emin,emax,ts,nobstot,Npred_100,Npred_1000,index,errorI,Flux,ErrorF,EnergyFlux,ErrorEF,Fluence

  ;;; Nicola's energy flux is in MeV/cm2/s
  kev2erg=1.602e-12*1d3  
  energyflux=energyflux*1e3*kev2erg
  erroref=erroref*1e3*kev2erg

  ;;;From Vlasios' spectra
  vtstart=[160,281,531.]
  vtend=[281,531,1210.]
;  vindex=[1.9,3.4,2.25]
;  vindex=[2.38,2.72,1.07]
  ;; energy flux from xspec assuming indices 1.9,3.4,2.25 in erg/cm2/s
;  vflux=[3.902e-08,1.0561e-08,1.6077e-08]
;  vindex=[1.89,1.81,1.74]
;  vflux=[1.3748e-8,1.6371e-8,2.2918e-8]
;  vfluxerr=[[5.747e-09,3.166e-08],[9.359e-09,2.756e-08],[1.487e-08,3.165e-08]]
;  vfluxerr[0,*]=vflux-vfluxerr[0,*]
;  vfluxerr[1,*]=vfluxerr[1,*]-vflux

  ;; using single flux factor from first LAT fit in joint fit
;  vrate=[4.8e-3,1.278e-3,2.323e-3]
;  vraterr=[8.974e-4,6.028e-4,4.626e-4]
;  ratio=1.3748e-08/4.8e-3
;  vflux=vrate*ratio
;  vfluxerr=vraterr*ratio
;  vindex=[1.89,1.89,1.89]
;  vcts=[4.8e-3,1.278e-3,2.323e-3] ;;; measured count rates in each LAT spec
;  vratio=1.5965e-05/4.99271E-03  ;;; ph/cm2/s  / model predicted rate for 1st LAT spec
;  vflux=vcts*vratio
;  vfluxerr=[[7.824e-06,4.134e-05],[1.060e-05,2.515e-05],[1.471e-05,3.104e-05]]
;  vflux2=[1.5965e-05,1.6801e-05,2.1044e-05]
;  vfluxerr[0,*]=vflux2-vfluxerr[0,*]
;  vfluxerr[1,*]=vfluxerr[1,*]-vflux2
;  vfluxerr[0,*]=vfluxerr[0,*]/vflux2*vflux
;  vfluxerr[1,*]=vfluxerr[1,*]/vflux2*vflux

  ;;LAT only fit in XSPEC
  vindex=[2.34,2.6,2.6]
  vflux=[5.2843e-05,3.9534e-05,2.9594e-05]
  vfluxerr=[2.309e-05,1.732e-05,0]

  vfd=dblarr(3) & vfluxes=vfd
  for i=0,2 do begin 
     lat_xrt_sed,'',[100e3,10e6],vflux[i],vindex[i],eflux
     vfluxes[i]=eflux
;     vfd[i]=flux2jy(vflux[i],vindex[i],low=100e3,high=10e6)
     vfd[i]=flux2jy(eflux,vindex[i],low=100e3,high=10e6)
  endfor 
;  vfderr=vfluxerr[0,*]/vflux*vfd
  vfderr=vfluxerr/vflux*vfd

;  index[5:*]=index[4]
  ;;; BAT T0=GBM T0+10
  ;;; BAT T0=21:08:28
  ;;; GBM T0=21:08:18
  tstart=tstart+10.
  tend=tend+10.
  n=n_elements(tstart)
  lfd=dblarr(n)
  efluxs=lfd
  for i=0,n-1 do begin 
;     lfd[i]=flux2jy(energyflux[i],-index[i],low=100e3,high=10e6,eeff=1e6)
     lat_xrt_sed,'GRB110625A',[emin[i],emax[i]]*1e3,flux[i],-index[i],eflux
     efluxs[i]=eflux
     lfd[i]=flux2jy(eflux,-index[i],low=100e3,high=10e6)
  endfor 
  lfderr=erroref/energyflux*lfd

  plotsym,1,3

  lc=lcout2fits(/phil)
  xul=where(lc.src_rate_err eq 0)
  spec=mrdfits('UL_specfits.fits',1)
  x=n_elements(lc)
  xfd=flux2jy(lc.src_rate*spec[1].cfratio,spec[1].phind)
  xfderr=lc.src_rate_err/lc.src_rate*xfd

  readcol,'BAT/456073/bat/bat_flux_snr4_DENSITY.qdp',time,tpos,tneg,bfluxdens,bfluxdenspos,bfluxdensneg

  if keyword_set(ps) then begplot,name='GRB110625A_XRT_LAT_lc.ps',/color,/land

  if not keyword_set(justflux) then begin 
     plot,[0,0],/xlog,/ylog,ytitle='Flux Density (Jy)',xtitle='Time since BAT Trigger (s)',yrange=[1e-11,1e-1],/ysty,xrange=[10,1e6],charsize=1.5,/nodata
     oploterror,lc.time,xfd,xfderr,psym=3,/nohat,errcolor=!red,color=!red
     for i=0,x-1 do oplot,[lc[i].tstart,lc[i].tstop],[xfd[i],xfd[i]],color=!red
     plots,lc[xul].time,xfd[xul],psym=8,color=!red
     if keyword_set(dofit) then begin 
        read_lcfit,'lc_fit_out_idl_int7.dat',pnames,p
        oplot,lc.time,bknpow(lc.time,[p[0]*xfd[0]/lc[0].src_rate,p[1:*]]),color=!green,line=2
     endif 
;     ul=where(erroref eq 0)
;     tmid=(tend-tstart)/2.+tstart
;     for i=0,n-1 do begin 
;        oplot,[tstart[i],tend[i]],[lfd[i],lfd[i]]
;        oplot,[tmid[i],tmid[i]],[lfd[i]-lfderr[i],lfd[i]+lfderr[i]]
;     endfor 
;     oplot,tmid[ul],lfd[ul],psym=8

     tmid=(vtend-vtstart)/2.+vtstart
     ul=where(vfderr eq 0)
     for i=0,2 do begin 
        oplot,[vtstart[i],vtend[i]],[vfd[i],vfd[i]];,color=!cyan
        oplot,[tmid[i],tmid[i]],[vfd[i]-vfderr[i],vfd[i]+vfderr[i]];,color=!cyan
     endfor 
     plots,tmid[ul],vfd[ul],psym=8;,color=!cyan

     vfderr[ul]=10.*vfd[ul]
     tt=fltarr(2,3)
     tt[0,*]=vtstart
     tt[1,*]=vtend
     p=[1e-6,1.1]
     newp=mpfitfun('intpow',tt,vfd,vfderr,p,yfit=yfit,perror=perror,dof=dof)
     chisq=total(((yfit-vfd)/vfderr)^2)
     perror=perror*sqrt(chisq/dof)
     if keyword_set(dofit) then oplot,tmid,pow(tmid,newp),color=!green,line=2


;  oplot,time,bfluxdens,color=!blue,psym=3
     for i=0,n_elements(time)-1 do begin
        oplot,[time[i]+tneg[i],time[i]+tpos[i]],[bfluxdens[i],bfluxdens[i]],color=!blue
        oplot,[time[i],time[i]],[bfluxdens[i]+bfluxdensneg[i],bfluxdens[i]+bfluxdenspos[i]],color=!blue
     endfor 
     
     legend,['XRT','BAT','LAT'],box=0,/top,/right,textcolor=[!red,!blue,!p.color]
  endif else begin 

     readcol,'BAT/456073/bat/bat_flux_snr4_XRTBAND.qdp',time,tpos,tneg,bflux,bfluxpos,bfluxneg

     plot_like_qdp,/phil,flux=spec[1].unabs_cfratio,yrange=[1e-14,1e-7],xrange=[10,1e6],charsize=1.5
     c=1.
     ul=where(erroref eq 0)
     plotsym,1,3,thick=5
     tmid=(tend-tstart)/2.+tstart
     for i=0,n-1 do begin 
        oplot,[tstart[i],tend[i]],[energyflux[i],energyflux[i]]*c
        oplot,[tmid[i],tmid[i]],[energyflux[i]-erroref[i],energyflux[i]+erroref[i]]*c
     endfor 
     oplot,tmid[ul],energyflux[ul]*c,psym=8
     for i=0,n_elements(time)-1 do begin
        oplot,[time[i]+tneg[i],time[i]+tpos[i]],[bflux[i],bflux[i]],color=!magenta
        oplot,[time[i],time[i]],[bflux[i]+bfluxneg[i],bflux[i]+bfluxpos[i]],color=!magenta
     endfor 
     
     legend,['XRT','BAT','LAT'],box=0,/top,/right,textcolor=[!p.color,!magenta,!green]

     v=[160,281,531,1210]
     for i=0,3 do oplot,[v[i],v[i]],[1e-15,1e15],line=2
  
  endelse 
  ;;; lat engflux is prob erg cm-2 s-1 in the 100 MeV to 10 GeV band -
  ;;; need to extrapolate down to keV?
  if keyword_set(ps) then endplot

  stop
  
  return
end 
