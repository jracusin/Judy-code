@fit_functions
@fit_functions_flares
pro grbz_plot

  readcol,'~/Lobster/2014_proposal/redshift_accum_data_all.txt',z,t0s,t500s,t1000s,t1500s,t2000s,t2500s,t3000s,t3500s,t4000s,t4500s,t5000s,All_detected_withoutGTM,intrinsic,GTM,All_detected_withGTM,comment='#',delim=' '

  wfi2014=All_detected_withGTM

;  readcol,'~/Lobster/TAO_2016/redshift_accum_data_all_eta2.txt',z,t0s,t500s,t1000s,t1500s,t2000s,t2500s,t3000s,t3500s,t4000s,t4500s,t5000s,All_detected_withoutGTM,intrinsic,GTM,All_detected_withGTM,comment='#',delim=' '

;  wfi60cm=All_detected_withGTM
;  wfi60cm_prompt=t0s
;  wfi60cm_afterglow=All_detected_withoutGTM-t0s
;  wfi60cm_gtm=GTM

  readcol,'~/Lobster/TAO_2016/redshift_accum_data_all_tao_max_expo_1000s.txt',z,t0s,t1030s,t2060s,t3090s,t4120s,t5150s,All_detected_withoutGTM,intrinsic,GTM,All_detected_withGTM,comment='#',delim=' '

  wfi45cm=All_detected_withGTM
  wfi45cm_prompt=t0s
  wfi45cm_afterglow=All_detected_withoutGTM-t0s
  wfi45cm_gtm=GTM

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  w=where(g.z gt 0,n)
  g=g[w]
  zeff=0.3
  lobzeff=0.9

  s=sort(g.z)
  g=g[s]

  x=fltarr(n)
  y=fltarr(n)
  for i=0,n-1 do begin 
     y[i]=n-i
     x[i]=g[i].z
  endfor 
  swiftlen=9.33
  xrange=[0,10]
  yrange=[0.1,400]
;  plot,[0,14],yrange,/nodata,xtitle='Redshift z!L0!N',ytitle='z>z!L0!N GRB rate (yr !U-1!N)',/ylog,yrange=yrange
;  polyfill,[5,xrange[1],xrange[1],5,5],yrange[[0,0,1,1,0]],color=!yellow
;  oplot,x,y/swiftlen,psym=10
  p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/histogram,ytitle='Cumulative (> z) GRB rate [yr !U-1!N]',/ylog,ytickformat='loglabels',xtitle='Redshift z',font_size=14)
  p1=polygon([5,xrange[1],xrange[1],5,5],yrange[[0,0,1,1,0]],fill_color='light cyan',transparency=80,/data)
  p2=plot(x,y/swiftlen,xrange=xrange,yrange=yrange,/histogram,/overplot)
;  p3=plot(z,wfi2014*lobzeff,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='blue',linestyle='none',sym_size=5,sym_filled=1)
;  p3=plot(z,wfi2014*lobzeff*3.,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='red',linestyle='none',sym_size=5,sym_filled=1)
;  p3=plot(z,wfi2014*lobzeff*2.,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='green',linestyle='none',sym_size=5,sym_filled=1)
  p3=plot(z,wfi45cm*lobzeff,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='magenta',linestyle='none',sym_size=5,sym_filled=1)
  p3=plot(z,wfi45cm*0.33,xrange=xrange,yrange=yrange,/overplot,color='magenta',linestyle='--')

  p3=plot(z,wfi45cm_prompt*lobzeff,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='blue',linestyle='none',sym_size=5,sym_filled=1)
  p3=plot(z,wfi45cm_afterglow*lobzeff,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='red',linestyle='none',sym_size=5,sym_filled=1)
  p3=plot(z,wfi45cm_gtm*lobzeff,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='green',linestyle='none',sym_size=5,sym_filled=1)
  p3=plot(z,intrinsic,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='black',linestyle='--',sym_size=5,sym_filled=1)


  t1=text(5.3,100,'Targets for JWST',/data,font_size=12)
  a1=arrow([5,7],[80,80],/data,thick=2)
  t2=text(1.8,7,'Swift GRBs with Measured Redshift',orientation=-39,/data,font_size=14)
;  t3=text(3,25,'Predicted Redshifts for ISS-Lobster
;  GRBs',/data,color='blue',orientation=-35,font_size=14)
  wficonfig=['    $30\circ\times30\circ$','$3\times30\circ\times30\circ$','$3\times30\circ\times20\circ$','$3\times15\circ\times10\circ$']
;  t3=text(7.5,30,wficonfig[0],/data,color='blue',font_size=14)
;  t3=text(7.5,20,wficonfig[1],/data,color='red',font_size=14)
;  t3=text(7.5,14,wficonfig[2],/data,color='green',font_size=14)
;  t3=text(7.5,10,wficonfig[3],/data,color='magenta',font_size=14)
;  t3=text(5.1,20,'Predicted Redshifts for 30 cm WFI',/data,color='red',orientation=-35,font_size=14)

  t3=text(7,40,'WFI Total',/data,color='magenta',font_size=14)
  t3=text(7,20,'Prompt',/data,color='blue',font_size=14)
  t3=text(7,10,'Afterglow',/data,color='red',font_size=14)
  t3=text(7,5,'WFI via GTM',/data,color='green',font_size=14)



  p.save,'~/Lobster/TAO_2016/grbz_plot.pdf',/transparent,/landscape
  p.close
  print
  print,'          z           2014          45cm          4*45cm'
  colprint,z,wfi2014,wfi45cm,4.*wfi45cm
  print
  print,'applying z efficiency'
  colprint,z,wfi2014*0.3,wfi45cm*0.3,wfi45cm*4.*0.9

  stop
  return
end 

function loginterpol,x,y,ix

  iy=10^interpol(alog10(x),alog10(y),alog10(ix))

  return,iy
end 

pro gw_counterparts,fovs,wficonfig,time,grbflux,grbflux2,grbflux3,flux4,wfisens

  mpc2cm=3.08568025d24

  ;; GW counterparts
  ;; ndets
;  ndets=[3,2,1,3,2,1]
;  ifnow=[1,1,1,0,0,0]
;  iflater=[0,0,0,1,1,1]
;  gwexptimes=[2670,1290]
;  times=[45.*60,]

  ;;; scale LCs to 440 Mpc
  ;;; calc how long detectable given 2,3 GW det exposure sensitivities
  ;;; if >45 min, then det=1, if <45 then det=prob based on that
  ;;; time/45 min
  ;;; add up det fraction * all sky * 0.85 *0.87




;  gwexptimes=[10,840,2640,840,2640]
;  times=[180,180+840,180+2640,45.*60.,45.*60.]
;  wfisens=interpol(grbflux,time,gwexptimes) ;;; interpolating old sensitivity curve
;  wfisens=interpol(grbflux,time,gwexptimes)
;  wfisens4=interpol(grbflux2,time,gwexptimes)

  volrate=10/1e3^3 ;; Mpc-3 yr-1
  h=[200,400]
  obj=['NS-NS','NS-BH']
;  wficonfig=['30x30','3x30x30','3x30x20','3x15x10','20x20']
  xrteff=0.87 ;; fraction of sGRBs w afterglow (from JD's table)
  gweff=0.95  ;;; x # pointings gets gweff
;  eff=0.85 ;; excluding SAA and other overheads
;  maxrate=volrate*4./3.*!pi*h^3

                                ; from Peter's presentation (removing Kagra)
  f3=[44.8+11.2]/100.
  f2=[11.2+6.4+6.4+2.8+1.6+1.6]/100.
  f1=[1.6+1.6+6.4+0.4+0.4+1.6]/100.
  f0=[0.4+1.6]/100.
  f=f0+f1+f2+f3
  f1=(f0+f1)/f
  f2=f2/f
  f3=f3/f
  
  ;;; for 1,2,3 GW det case
;  npointings=[4,2,2,3,6]
;  npointings=[4,6,2]
  npointings=[2,3,1,100]
  for p=0,n_elements(npointings)-1 do begin 
     print,wficonfig[p]
     npoint=npointings[p]*2.
     if p eq 0 then gf=grbflux
     if p eq 1 then gf=grbflux2
     if p eq 2 then gf=grbflux3
     if p eq 3 then gf=flux4
     orbit=90.*60.
     slew=30.
     start=180. ;;; when we get LIGO positions
     exptimes=[orbit/2-slew,orbit/2-slew,replicate(orbit/npoint-slew,npoint)]
     wfisens=loginterpol(gf,time,exptimes)
     ;; 2 pointings to get 95% enclosed
     tobs_stop=start+[orbit/2.,orbit,(findgen(npoint)+1)*(orbit/npoint-slew)]
     tobs_start=start+[0,orbit/2.,findgen(npoint)*(orbit/npoint-slew)]
     nobs=n_elements(tobs_stop)

     print,'N Pointings = '+ntostr(fix(npoint))
     ;; obs0=start at 180 and end at 45*60/2. - gw2 good side
     ;; obs1=start at 45*60/2. and end at 45*60 - gw2 bad side
     ;; obs2=start at 180 and end at 45 - gw3 good side
     ;; obs3=start at 45*60 and end at 90*60 - gw3 bad side

     t=logarr(180,1e4,bin=0.1)
     dt=t[1:*]-t[0:n_elements(t)-2]
     
  ;;; X-ray afterglows of short GRBs
     grb=mrdfits('~/Swift/swift_grb_properties.fits',1)
     w=where(grb.t90 le 2. and grb.t90 gt 0. and grb.grb ne 'GRB100628A' and grb.grb ne 'GRB140311B',ngrb)
     grb=grb[w]

     z=grb.z
     wz=where(z eq -1)
     z[wz]=0.7
;     plot,[0,0],[0,0],/nodata,/xlog,/ylog,xrange=[10,1e6],yrange=[1e-13,1e-7]
     for k=0,1 do begin  ;;; loop over NS-NS & NS-BH
        print,obj[k]
        gwhorizon=h[k]*1.5*1.5*1.5  ;*1e-3
        gwallskyrate=volrate*gwhorizon^3*4./3.*!pi
        
        distfull=gwhorizon*mpc2cm

        detuntil=fltarr(nobs,ngrb)
        det=fltarr(nobs,ngrb)
        for i=0,n_elements(grb)-1 do begin 
           dist=lumdist(z[i],h0=71,lambda=0.73,omega_m=0.27,/silent)*mpc2cm
           eng=dindgen(98)/10.+0.3
           de=0.1
           conv=total(pow(eng[0:47],[1.,grb[i].phind])*de*eng[0:47])/total(pow(eng,[1.,grb[i].phind])*de*eng)
;print,conv
           rat=grb[i].cfratio*conv*dist^2/distfull^2/kcorr(z[i],[1.,grb[i].phind],/pow)  

           ;; at what time is the GRB flux equal to the sensitivity 
           ;; no, actually want when avg flux >wfisens
           for j=0,nobs-1 do begin  ;;; loop over gw2 or gw3
              f=call_function(strtrim(grb[i].model,2),t,grb[i].p)
              w=where(t ge tobs_start[j] and t le tobs_stop[j])
              fmean=total(f[w]*rat*dt[w])/total(dt[w]) ;;;; fix this
              if fmean gt wfisens[j] then det[j,i]=1
;print,fmean
;           detuntil[nobs,i]=loginterpol(t,f*rat,wfisens[j])
              
           endfor 
;           oplot,t,f*rat
;           oplot,[10,1e6],wfisens[[0,0]],color=!red
;           oplot,[10,1e6],wfisens[[1,1]],color=!red
;           oplot,tobs_stop[[0,0]],[1e-13,1e-7],color=!red
;           oplot,tobs_stop[[1,1]],[1e-13,1e-7],color=!red
        endfor

;     wdet_gw3_good=where(det[0,*] eq 1,nw3g)
;     wdet_gw3_bad=where(det[1,*] eq 1,nw3b)
;     wdet_gw3_good=where(det[2:*,*] eq 1,nw3g)
;     wdet_gw3_bad=where(det[3:*,*] eq 1,nw3b)
        gw3=where(det[0:1,*] eq 1,nw3)
        gw2=where(det[2:*,*] eq 1,nw2)


        ngrb=ngrb*1.
;     n2g=nw2g/ngrb*0.5*f2
;     n2b=nw2b/ngrb*0.5*f2
;     n3g=nw3g/ngrb*0.5*f3
;     n3b=nw3b/ngrb*0.5*f3

;     print,n2g,n2b,n3g,n3b
;     tot=n2g+n2b+n3g+n3b
        n2=nw2/ngrb*f2/npoint
        n3=nw3/ngrb*f3/2.
        tot=n2+n3
;print,tot
        print,'Frac detectable = ',tot*xrteff*gweff
        print,'# detectable = ',tot*gwallskyrate*xrteff*gweff
;stop
     ;;; on either side of earth for either gw2 or gw3
;     wdet_either=where(detuntil[0,*] gt 45.*60. or detuntil[1,*] gt 45.*60.,nyes)
;     wdet_good_gw2_1=where(detuntil[0,*] gt tobs[0] and detuntil[0,*] le tobs[1],ngw2a) ;;; those that we'll only catch if on right side of earth in 2 pointings
;     wdet_good_gw2_2=where(detuntil[0,*] gt tobs[1],ngw2b)  ;;; those that we'll catch if on wrong side in 2 pointings
;     wdet_good_gw3=where(detuntil[1,*] gt tobs[1],ngw3) ;;; those we will surely catch in the first orbit

;     help,wdet_either,wdet_good_gw2_1,wdet_good_gw2_2,wdet_good_gw3

;     n3=f3*ngw3/ngrb
;     n2a=f2*0.5*ngw2a/ngrb
;     n2b=f2*0.5*ngw2b/ngrb

;     print,n3,n2a,n2b

     endfor 
  endfor 
  stop

;;      nsim=1000
;;      d=randomu(seed,nsim)*4./3.*!pi*distfull^3
;;      ddist=(3./4./!pi*d)^(1./3.)
;;      ngrb2=round(ngrb/0.87) ;;; accounting for non-detections of sGRB afterglow
;;      r=round(randomu(seed,nsim)*ngrb2) ;;; which random grb to use for each sim
;;      nodet=intarr(ngrb2)
;;      nodet[ngrb:*]=1

;;   ;; assign tiling requirement based on f3,f2,f1% distribution

;;      ngwdet=intarr(nsim)
;;      ngwdet[0:round(nsim*f1)-1]=1
;;      ngwdet[round(nsim*f1):round(nsim*(f1+f2))-1]=2
;;      ngwdet[round(nsim*(f1+f2)):round(nsim*(f1+f2+f3))-1]=3

;; ;;; real (or assumed) GRB distance & z
;;      z=replicate(0.7,ngrb2)
;;      z[0:ngrb-1]=grb.z
;;      z[ngrb:*]=0.7
;;      wz=where(z eq 0)
;;      z[wz]=0.7
;;      dist=lumdist(z,h0=71,lambda=0.73,omega_m=0.27,/silent)
;;      dist=dist*mpc2cm  

;;      eng=dindgen(98)/10.+0.3
;;      de=0.1

;;      det=fltarr(nsim)
;; ;     det2=fltarr(nsim,n_elements(times))
;; ;     flux=fltarr(nsim,n_elements(times))
;;      for i=0,nsim-1 do begin 
;;         z1=1.+z[r[i]]

;;         ;; randomly assign visible now or later
;;         nowlater=round(randomu(seed,1))
;;         now=0 & later=0
;;         if nowlater eq 0 then now=1 else later=1


;; ;        times
;; ;        exptimes

;; ;        if now then times=

;;         if nodet[r[i]] eq 0 then begin ;; for each sim GRB, if detected calc det
;;            f=call_function(strtrim(grb[r[i]].model,2),times/z1,grb[r[i]].p)
;;            conv=total(pow(eng[0:47],[1.,grb[i].phind])*de)/total(pow(eng,[1.,grb[r[i]].phind])*de)
  
;;            rat=grb[r[i]].cfratio*conv*dist^2/distfull^2/kcorr(z[r[i]],[1.,grb[r[i]].phind],/pow)
;;            for j=0,n_elements(times)-1 do begin 
;;               if f[j]*rat ge wfisens[j] then det[i,j]=1
;;               if f[j]*rat ge wfisens4[j] then det2[i,j]=1
;;            endfor 
;;            flux[i,*]=f*rat
;;         endif  
;; ;print,dist,distfull
;;      endfor  
  
;;      print,obj[k]
;;      print,'Old WFI sensitivity'
;;      for j=0,n_elements(times)-1 do begin
;;         v=total(det[*,j]/ngrb*1.)
;;         print,'Detectable at '+ntostr(fix(times[j]))+' s (Texp='+ntostr(fix(gwexptimes[j]))+'s) = '+ntostr(v)+' ('+ntostr(v*xrteff*gwallskyrate)+')'
;;         ; on right side of Earth
;;      endfor 
;;      print,'New WFI sensitivity'
;;      for j=0,n_elements(times)-1 do begin
;;         v=total(det2[*,j]/ngrb*1.)
;;         print,'Detectable at '+ntostr(fix(times[j]))+' s (Texp='+ntostr(fix(gwexptimes[j]))+'s) = '+ntostr(v)+' ('+ntostr(v*xrteff*gwallskyrate)+')'
;;      endfor 
;; ;     f=0.5
;;      ;;; of 50% on right side of Earth, frac detectable in 
;; ;     r3=total(det[*,0]/ngrb*1.)*xrteff*gwallskyrate*f*f3
;;   endfor 

                                ; half of bursts are visible at trigger other half visible 45 later
                                ; 57% have positions good enough for 1 pointing (full exposure)
                                ; 30% require 6, 2, 2, 9 pointings (check w Peter)
                                ; 12% are 1 detector and only hope is GTM trigger

                                ; simulation
  ;; draw XRT lcs randomly w z or assumed z, scale to some distance
  ;; corresponding to volumetric density making large sample of 1000
  ;; sGRBs, include 13% not detected
  ;; assign randomly visible now or visible in 45 min
  ;; assign tiling requirement based on f3,f3,f2% distribution which
  ;; corresponds to exptime
  ;; what frac detectable * all sky rate

  return
end 
pro wfi_rates

  cd,'~/Lobster/TAO_2016/'
;  readcol,'~/Lobster/2014_proposal/lobster_sensitivity_0.3_5_Ptak.dat',time,bcount,mcount,grbflux
  readcol,'/Users/jracusin/TAO/simulations/sensitivity_curves/TAO-ISS_sensitivity_8_0.3_5.dat',time,bcount, mcount, crabflux, grbflux, sgrbflux

  fovs=[30*30.,18.6^2,4.*18.6^2,1.]
  wficonfig=['30x30','20x20','4x20x20','XRI 1x1']
  t1=[0.1,0.15,0.2,0.3,0.4,0.5,0.7,1.]
;  t2=[4e5,1e6,2e6,1e7]
  t2=[2e4,4e4,7e4,9e4,1e5,1.3e5,1.7e5,2e5,2.5e5,3e5,3.5e5,4e5,5e5,8e5,1e6]
  nl=n_elements(time)
;  time2=[t1,time,t2]
  time2=time
  flux=grbflux;[grbflux[0]/(t1/time[0]),grbflux,grbflux[nl-1]/sqrt(t2/time[nl-1])]
;  w=where(time2 ge 3e5)
;  w=where(flux le 1d-12)
;  flux[w]=1d-12
;  flux2=flux/4.  ;;; scaling the old sensitivity
;  w=where(flux2 le 1d-12)
;  flux2[w]=1d-12
;  flux3=flux/1.5^2.  ;;; scaling the old sensitivity
;  w=where(flux3 le 1d-12)
;  flux3[w]=1d-12
;  flux2=flux3
  flux2=flux
  flux3=flux

  ;;; XRI rates
  readcol,'~/Lobster/TAP/XRI_sensitivity_15arcmin.dat',xri_time,xri_flux,skip=1
  flux4=loginterpol(xri_flux,xri_time,time2) ;;; daily

  begplot,name='sensitivity_trade.ps',/land,/color
  !x.margin=[4,1]
  plot,time2,flux,/xlog,/ylog,yrange=[1e-13,1e-7],charsize=2.,/xsty,/ysty,xtitle='Exposure Time (s)',ytitle='WFI Sensitivity (erg cm!U-2!N s!U-1!N)',xrange=[0.1,1e6],xticks=7,xtickformat='loglabels',xminor=9,yticks=6,yminor=9;,psym=1
  oplot,time2,flux3,color=!red,line=2
;  xyouts,20,1e-9,'30 cm focal length',/data,charsize=2.
  xyouts,1.5,9e-12,'45 cm focal length',/data,charsize=2.;,color=!red
  xyouts,2e4,5e-13,'Confusion Limit',/data,charsize=1.5
  endplot
  ps2pdf,'sensitivity_trade.ps'
  time=time2
  grbflux=flux
  grbflux2=flux2
  grbflux3=flux3
  constraints=0.83
  allsky=4.*!pi*(180d/!pi)^2.;41253.
  pointings=allsky/fovs*constraints
  exptimes=86400*0.85/pointings-30 ;;; 0.85 for SAA, 30 s for slewing
  wfisens=loginterpol(flux,time,exptimes) ;;; daily
  wfisens[1]=loginterpol(flux2,time,exptimes[1])
  wfisens[2]=loginterpol(flux3,time,exptimes[2])
  wfisens[3]=loginterpol(flux4,time,exptimes[3])

  weekly_exptimes=exptimes*7d ;; min to sec exposures over 1 week
  weekly_wfisens=loginterpol(grbflux,time,weekly_exptimes) ;;; interpolating old sensitivity curve
  weekly_wfisens[1]=loginterpol(grbflux2,time,weekly_exptimes[1])
  weekly_wfisens[2]=loginterpol(grbflux3,time,weekly_exptimes[2])
  weekly_wfisens[3]=loginterpol(flux4,time,weekly_exptimes[3])

  mpc2cm=3.08568025d24
  z0=findgen(100)/100.
  ld=lumdist(z0,h0=71,lambda=0.73,omega_m=0.27,/silent)

  gw_counterparts,fovs,wficonfig,time,grbflux,grbflux2,grbflux3,flux4,gwwfisens

  ;;; ccSNE
  fov=fovs/allsky  ;; old 30x30 WFI
;  fov=(30^2.)/(23.6/60.)^2
;  lstar=0.05*1e3^3 ;; 0.05 Mpc-3 -> Gpc-3
;  rate=lstar*1e-2  ;;; rate of ccSNe
  rate=0.258e-4;*1e3^3 ;; SN Mpc-3 yr-1 ;; SN Ibc rate from Li et al. 2011
;  dist=27.;*1e-3    ;; for XRT
  lum=6d43 ;; erg s-1
  exp=400.
  print
  print,wficonfig
;  print,'XRT Distance = ',round(dist),' Mpc'
  ;;; 300 s exposure, 2008D was ~1 cts/s in XRT
  wfisens400=loginterpol(grbflux,time,400) ;;; interpolating old sensitivity curve
;  xrt2wfi=3.18e-11 ;; erg/cm2/(cts/s) from webpimms for phind=2
;  spectrum
  xrt2wfi= 0.7074d ;; from 0.3-10 keV to 0.3-5 keV
;  fluxfact=(1.*xrt2wfi)/wfisens300 ;;ratio between 2008D flux and WFI sensitivity
;  wfidist=sqrt((dist^2*fluxfact))  ;;;  f~1/(4*!pi*D^2)
  wfidist=sqrt(lum*xrt2wfi/(4.*!pi*wfisens400))/mpc2cm
  z=interpol(z0,ld,wfidist)
  vol=comovingvol(z)
;  vol=4./3.*!pi*wfidist^3
  wfirate=rate*vol*fov

  print,'WFI Distance = ',round(wfidist),' Mpc'
  colprint,'ccSne rate ('+wficonfig[0]+') = ',wfirate[0]
;  print,'ccSNe rate (30x30 WFI) = ',wfirate[0]
;  print,'ccSNe rate (30x90 WFI) = ',wfirate[1]
;  print,'ccSNe rate (20x90 WFI) = ',wfirate[2]

  wfisens400=loginterpol(grbflux2,time,400) ;;; simply reducing sensitivity by x4
  wfisens400a=loginterpol(grbflux3,time,400) ;;; simply reducing sensitivity by x4

;  fluxfact=(1.*xrt2wfi)/(wfisens300) ;;ratio between 2008D flux and WFI sensitivity
;  fov=(10*15*3.)/40000.              ;; old 30x30 WFI
;  fov=(10*15.*3.)/(23.6/60.)^2
;  fov=fovs[3]/allsky

;  wfidist=sqrt((dist^2*fluxfact)) ;;;  f~1/(4*!pi*D^2)
  wfidist=sqrt(lum*xrt2wfi/(4.*!pi*wfisens400))/mpc2cm
  z=interpol(z0,ld,wfidist)
  vol=comovingvol(z)
  print,'WFI Distance = ',round(wfidist),' Mpc'
;  vol=4./3.*!pi*wfidist^3
  wfirate=rate*vol*fov
;  print,'ccSNe rate (10x45 WFI) = ',wfirate
  print,'ccSne rate ('+wficonfig[1]+') = ',wfirate[1]

;  fov=fovs[4]/allsky
  wfidist=sqrt(lum*xrt2wfi/(4.*!pi*wfisens400a))/mpc2cm
  z=interpol(z0,ld,wfidist)
  vol=comovingvol(z)
  print,'WFI Distance = ',round(wfidist),' Mpc'
  wfirate=rate*vol*fov
;  print,'ccSNe rate (20x20 WFI) = ',wfirate
  print,'ccSne rate ('+wficonfig[2]+') = ',wfirate[2]

  print
;  stop

  ;;; TDFs

  rate=1e-4 ;;; yr-1 galaxy-1
  galdens=1e-2 ;;; Mpc-3 of SMBH (10^6-10^8 Msolar)
  xrt2wfi=0.594
  frac=0.61

  ;;; non-jetted
  lum=1d44;*xrt2wfi*frac ;; erg/s  ;;; all thermal, don't lose anything >5 keV
  dist=sqrt(lum/(4.*!pi*weekly_wfisens))/mpc2cm
  z=interpol(z0,ld,dist)
;  print,z
  vol=comovingvol(z)
;  vol=
;  print,vol
;  print,4./3.*!pi*dist^3
  print
  print,wficonfig
  print,dist
  print,'non-jetted'
  wfirate=rate*galdens*vol
  colprint,'TDF rate ('+wficonfig+') = '+ntostr(wfirate)

  ;;; jetted
  f=0.1  ;; 10% are beamed (though very uncertain)
  theta=5.*!dtor ;;; deg->radian
  area=2*!pi*(1.-cos(theta))
  areafrac=area/(4*!pi)*2.
  lum=1d47*xrt2wfi*frac ;;; erg/s
  dist=sqrt(lum/(4.*!pi*weekly_wfisens))/mpc2cm
  print,dist
  z=interpol(z0,ld,dist)
  print,z
  vol=comovingvol(z)
;  vol=
;print,vol
;print,4./3.*!pi*dist^3
  print,'jetted'
  wfirate2=rate*galdens*vol*f*areafrac
  colprint,'TDF rate ('+wficonfig+') = '+ntostr(wfirate2)

  print
  colprint,'Combined rate ('+wficonfig+') = '+ntostr(wfirate+wfirate2)

  ;;;;; AGN
  
  print
  print,wficonfig
  print,'AGN'
  ; need volumetric rate
  ; need luminosities
  ; convert to distance that WFI can detect
  ; convert to volume
  ; wfirate=rate*vol
;  exptime=[30.,90.,60.,15.]*60.*7d ;; min to sec exposures over 1 week
;  wfisens=interpol(grbflux,time,exptime) ;;; interpolating old sensitivity curve
;  wfisens[3]=wfisens[3]/4.
;  wfisens[3]=interpol(grbflux2,time,exptime[3])

  nsim=1000.
  lum=10^(dindgen(nsim)/((nsim-1)/10.)+38.)  ;;; list of luminosities

  alpha=-1.37;-1.35
  lstar=10^(43.66d);43.7d)
  phistar=10^(-4.74d);-4.5d)
  dl=lum[1:*]-lum[0:n_elements(lum)-2] ;lstar;wfisens[0]
  xmm2wfi=0.796  ;;; convert from 0.5-7 to 0.3-5, assumes PL=1.8, Nh=3e21
  begplot,name='AGN_density.ps',/color
  !p.multi=[0,1,2]
  !x.margin=[4,2]
  title=['Weekly','Daily']
  yr=[35,6]
  for j=0,1 do begin ;;; loop over weekly and daily
     case j of 
        0: ws=weekly_wfisens
        1: ws=wfisens
     endcase 
     plot,[0,2],[1,100],xrange=[0,0.5],yrange=[0,yr[j]],xtitle='z',ytitle='N',/nodata,charsize=2.,/ysty,title=title[j]
     print,title[j]
     color=[!red,!blue,!green,!magenta,!orange]
;  !p.multi=[0,2,2]
     for i=0,n_elements(fovs)-1 do begin 
        d=sqrt(lum*xmm2wfi/(4.*!pi*ws[i]))/mpc2cm ;;; distance WFI can detect lums
        z0=findgen(1000)/100.
        ld=lumdist(z0,h0=71,lambda=0.73,omega_m=0.27,/silent)
        z=interpol(z0,ld,d) ;;; redshift of those dists
        phi=phistar*(lum/lstar)^alpha*exp(-lum/lstar)*dl/lstar ;;; density at those lums
        vol=comovingvol(z)  ;;; comoving volume
        wfirate=total(vol*phi) ;;; rate=vol*density
        print,'AGN rate ('+wficonfig[i]+') = '+ntostr(wfirate)
        oplot,z,vol*phi,color=color[i]
;     stop
     endfor 
     legend,['WFI Configuration',wficonfig],textcolor=[!p.color,color],box=0,/top,/right,charsize=2.
  endfor 
;  !p.multi=0
  endplot
  ps2pdf,'AGN_density.ps'

  ;;;; Blazars
  ;;; Padovani et al 2007
  print
  print,wficonfig
  print,'Blazars'
;  print,weekly_exptimes,weekly_wfisens
  f1=[7.506645E-14,1.3298641E-13,2.986495E-13,6.521481E-13,1.8288595E-12,6.4100483E-12]
  f2=[8.303352E-14,3.7369985E-13,1.4834874E-12,4.8445995E-12]
  f3=[1.0131099E-13,1.7693365E-13,3.5526947E-13,1.0536481E-12,6.540384E-12,1.2409822E-11,6.057032E-11]

  n1=[0.0024451583,0.0020690372,0.0016649443,0.0012539582,5.198891E-4,1.8254212E-4]
  n2=[0.015891278,0.0028273796,5.288385E-4,1.0750702E-4]
  n3=[0.13020536,0.0903178,0.061599646,0.027740812,0.0068690353,0.0029465847,1.123024E-4]

  ;; 1=low lum BLs, 2=blazar sequence, 3=other surveys?

  fconv=1.35
  
;  !p.multi=0
;  !x.margin=[8,4]
;  !y.margin=[4,2]
;  plot,[1,1],[1,1],xrange=[1e-14,1e-10],yrange=[1e-3,1e2],/xlog,/ylog
  color=[!red,!green,!magenta]

  for j=0,1 do begin ;;; loop over weekly and daily
     case j of 
        0: ws=weekly_wfisens
        1: ws=wfisens
     endcase 
     print,title[j]

     for i=0,2 do begin
        case i of
           0: begin
              n=n1
              f=f1
           end 
           1: begin
              n=n2
              f=f2
           end 
           2: begin
              n=n3
              f=f3
           end 
        endcase

        arr=logarr(1e-14,1e-10,bin=0.1)
        farr=loginterpol(n,f*fconv,arr)
;        oplot,arr,farr,color=color[i],line=2
        cfarr=farr;total(farr,/cumulative)
;        oplot,arr,cfarr,color=color[i],line=2
;        wfidens=10^interpol(alog10(n),alog10(f*fconv),alog10(ws))
        wfinum=loginterpol(cfarr,arr,ws)

;        oplot,f*fconv,n,color=color[i]
;        oplot,ws,wfidens,psym=2,color=color[i]
;        oplot,ws,wfinum,psym=2,color=color[i]
;        print,wfinum
        print,wfinum*fovs
     endfor 

;     print,wfif,wfif2,wfif3
;stop
  endfor 

;  stop

  ;;; Stellar superflares
  print
  print, 'Stellar Superflares'
  ;; scaling from 2010 rates which were 30-300 /yr
  wfirate0=fovs/fovs[0]*30.
  wfirate1=fovs/fovs[0]*300.
  colprint,wficonfig,wfirate0,wfirate1


  ;;; classical novae 35 +/- 11 yr-1 in our Galaxy (Shafter 1997),
  ;;;                        41+/-20 yr-1 (Hatano et al. 1997)
  ;;; initial thermonuclear runaway burning phase lasts only a few x100s
  print
  print,wficonfig
  print,'Novae'
  lpeak=1d38 ;; erg cm-2 s-1
  dist=8*1e-3*mpc2cm ;; kpc to cm
  distm31=780*1e-3*mpc2cm
  fgal=lpeak/(4*!pi*dist^2)
  fm31=lpeak/(4*!pi*distm31^2)

  print,'Flux at GC = ',fgal
  print,'Flux at M31 = ',fm31

  wfirate=35.*fovs/allsky
  colprint,'WFI rate ('+wficonfig+') = ',wfirate

  ;;; thermonuclear bursts
  print
  print,wficonfig
  print,'thermonuclear bursts'

  ;; Keek et al. (2010)
  expo=[83.1,30.9,53.9,52.4,60.6,49.8,60.1,47.9,54.7,56.3,48.3,55.1,71.5,34.3,55.7]*86400.
  bursts=[269,17,41,67,241,27,125,18,50,269,24,31,63,60,55]
  ;; adding Cornelisse et al. (2003)
  expo=[expo,[7.4,6.7,6.5,7,8.9,6.9,7.1]*1e6]
  bursts=[bursts,[423,339,260,178,104,61,49]]

  rates=bursts/expo
;  plothist,rates,bin=0.01
  wfirates=total(rates)*86400*0.85*365*fovs/allsky
  colprint,wficonfig,wfirates

stop
  return
end 
