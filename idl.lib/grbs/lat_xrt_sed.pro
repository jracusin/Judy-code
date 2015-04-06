@fit_functions
pro test
  ;;; testing with plot in de Pasquale et al. (2010) - 090510

  phflux=[2.49e-2,1.89e-2,5.7e-3,6.4e-3,8.4e-4,2.0e-4,1.67e-4,4.4e-5,1.6e-5]
  engind=[0.85,1.2,0.93,1.41,0.76,0.86,2.27,0.85,1.74]
;  engind[*]=1.
  t1=[0.38,0.48,0.92,1.5,2.5,5.5,11.5,37.,69.5]
  t2=[0.48,0.92,1.5,2.5,5.5,11.5,37.,69.5,200.]
  low=100e3
  high=4e6
  n=n_elements(phflux)
  efluxs=dblarr(n) & jy=efluxs & norm=jy
  for i=0,n-1 do begin 
     lat_xrt_sed,'',[low,high],phflux[i],engind[i],eflux,norm1=norm1
     efluxs[i]=eflux
     norm[i]=norm1
     jy[i]=flux2jy(efluxs[i],engind[i],low=low,high=high);,eeff=500.e3)
  endfor 

  t=(t2-t1)/2.+t1
  plot,t,jy,/xlog,/ylog,psym=1,yrange=[2e-12,5e-2],xrange=[5e-2,2e5],charsize=1.5
  for i=0,n-1 do oplot,[t1[i],t2[i]],[jy[i],jy[i]]
  oplot,[0.01,1e6],[1e-9,1e-9],line=1
  oplot,[0.01,1e6],[1e-8,1e-8],line=1

  colprint,t1,t2,engind,phflux,norm,efluxs,jy ;; erg cm-2 s-1?


stop

  return
end 

pro grb110625a,ps=ps,justflux=justflux,sep=sep,jy=jy

;  if not keyword_set(sep) then cd,'~/GRBs/GRB110625A/joint/' else
;  cd,'~/GRBs/GRB110625A/XRT'
  if not keyword_set(sep) then cd,'~/GRBs/GRB110625A/joint/' else cd,'~/GRBs/GRB110625A/joint/XRT_BAT'


  ;;; pow fit
  fits=mrdfits('wabs_wabs_pow.fits',1)
  fit=fits[0]
  readcol,'wabs_wabs_pow_eemo.dat',meng,mengbin,mofit,format='(d,d,d)'
  readcol,'wabs_wabs_pow_eeuf.dat',eng,engbin,euf,euferr,format='(d,d,d,d)'

  if not keyword_set(sep) then begin 
     dir='~/GRBs/GRB110625A/LAT/'
     readcol,dir+'pow_eemo.dat',lmeng,lmengbin,lmofit,format='(d,d,d)'
     readcol,dir+'pow_eeuf.dat',leng,lengbin,leuf,leuferr,format='(d,d,d,d)'
     lfits=mrdfits(dir+'pow.fits',1)
     lfit=lfits[0]
  endif 

  h=4.135e-15*1e-3 ;; keV s
  eng=eng*h
  engbin=engbin*h
  freq=eng/h
  freqbin=engbin/h

  if keyword_set(ps) then begin
     nu=!tsym.nu 
     tbeta=!tsym.beta
     gamma=!tsym.gamma
  endif else begin
     nu=!vsym.nu
     tbeta=!vsym.beta
     gamma=!tsym.gamma
  endelse 

;;; need to separate out 3 different seds
; flux=euf/mofit
  beta=[fit.phoindex3,fit.phoindex7,fit.phoindex11]
  betaerr=[[fit.ephoindex3],[fit.ephoindex7],[fit.ephoindex11]]
  betaerr[0,*]=betaerr[0,*]-beta
  betaerr[1,*]=betaerr[1,*]-beta

;  lat_xrt_sed,grb,eng,flux,beta,eflux,norm1=norm1

  w=where(eng[1:*]-eng le 0.0 or (eng[1:*]-eng gt 10. and eng lt 10.) or (eng[1:*]-eng gt 100000. and eng lt 200.))
  w=[0,w+1,n_elements(eng)]
  s1=indgen(w[1])
  s2=indgen(w[2]-w[1])+w[1]
  s3=indgen(w[3]-w[2])+w[2]

  meng=meng*h
  mw=where(meng[1:*]-meng le 0.0 or (meng[1:*]-meng gt 10. and meng lt 10.) or (meng[1:*]-meng gt 10000. and meng lt 10000.))
  mw=[0,mw+1,n_elements(meng)]
  ms1=indgen(mw[1])
  ms2=indgen(mw[2]-mw[1])+mw[1]
  ms3=indgen(mw[3]-mw[2])+mw[2]

;  if not keyword_set(sep) then begin 
  s4=indgen(w[4]-w[3])+w[3]
  s5=indgen(w[5]-w[4])+w[4]
  s6=indgen(w[6]-w[5])+w[5]
  ms4=indgen(mw[4]-mw[3])+mw[3]
  ms5=indgen(mw[5]-mw[4])+mw[4]
  ms6=indgen(mw[6]-mw[5])+mw[5]

  s7=indgen(w[7]-w[6])+w[6]
  s8=indgen(w[8]-w[7])+w[7]
  s9=indgen(w[9]-w[8])+w[8]
  
  s14=[s1,s4,s7]
  s25=[s2,s5,s8]
  s36=[s3,s6,s9]
  
;     if not keyword_set(sep) then begin 
  ms7=indgen(mw[7]-mw[6])+mw[6]
  ms8=indgen(mw[8]-mw[7])+mw[7]
  ms9=indgen(mw[9]-mw[8])+mw[8]

  ms14=[ms1,ms4,ms7]
  ms25=[ms2,ms5,ms8]
  ms36=[ms3,ms6,ms9]

  if not keyword_set(sep) then begin 
     title='XRT-BAT-LAT joint fit'
     b='!Lx'+gamma+'!N'
     add='_joint'
  endif else begin 
     b='!Lx-hx!N'
;    b=[replicate('!Lx!N',3),replicate('!Lhx!N',3),replicate('!L'+gamma+'!N',3)]
     title='XRT/BAT joint fit + LAT separate fit'
     add='_sep'
     ;;;; overplot LAT data? and extrapolate for comparison?
  endelse 

  jy2erg=1d23*h
  if keyword_set(jy) then begin 
     m=1.
     ytitle=nu+' F!L'+nu+'!N (Jy)'
     yrange=[1d-11,1d-3]
     add=add+'_jy'
  endif else begin 
     m=1.                       ;jy2erg
     ytitle=nu+' F!L'+nu+'!N (erg cm!U-2!N s!U-1!N)'
     yrange=[1e-14,1e-4]
;     add=''
  endelse 

  if keyword_set(ps) then begplot,name='~/GRBs/GRB110625A/XRT_LAT_SED'+add+'.ps',/color
  multiplot,[1,3],/init
  for i=0,2 do begin 
     multiplot
     if i gt 0 then title=''
     xtickformat='' & xtitle=''
     if i eq 2 then begin 
        xtickformat='loglabels' 
        xtitle='Energy (keV)'
     endif 

     plot,[0.1,1e9],yrange,/nodata,/xlog,/ylog,xtitle=xtitle,ytitle=ytitle,xrange=[0.1,1e8],yrange=yrange,/xsty,/ysty,title=title,xtickformat=xtickformat

;     color=[!red,!blue,!green]
;;  if not keyword_set(sep) then begin 
  
     case i of 
        0: begin
           ms=ms14
           s=s14
           norm=fit.norm4
           enorm=fits.enorm4
           phind=fit.phoindex3
           ephind=fits.ephoindex3
           color=!red
           leg='T0+160 < T < T0+281'
        end 
        1: begin
           ms=ms25
           s=s25
           norm=fit.norm8
           enorm=fits.enorm8
           phind=fit.phoindex7
           ephind=fits.ephoindex7
           color=!blue
           leg='T0+281 < T < T0+531'
        end 
        2: begin
           ms=ms36
           s=s36
           norm=fit.norm12
           enorm=fits.enorm12
           phind=fit.phoindex11
           ephind=fits.ephoindex11
           color=!green
           leg='T0+531 < T < T0+1210'
        end 
     endcase 

     nh=fit.nh2
     enh=fits.enh2
     inh=fit.nh5
     off=[1.,1.,1.];10.^(-i*2)
     y=wabs(meng[ms],nh+inh)*pow(meng[ms],[norm,phind])

     if i eq 0 then begin 
        k=approx(meng[ms],1.,m,n)
        n=mofit[ms[m]]/y[m];*1e-6
     endif 

     yup1=wabs(meng[ms],enh[0,1]+inh)*pow(meng[ms],[enorm[0,1],ephind[0,1]])
     ydo1=wabs(meng[ms],enh[1,1]+inh)*pow(meng[ms],[enorm[1,1],ephind[1,1]])

     yup2=wabs(meng[ms],enh[0,2]+inh)*pow(meng[ms],[enorm[0,2],ephind[0,2]])
     ydo2=wabs(meng[ms],enh[1,2]+inh)*pow(meng[ms],[enorm[1,2],ephind[1,2]])

     yup3=wabs(meng[ms],enh[0,3]+inh)*pow(meng[ms],[enorm[0,3],ephind[0,3]])
     ydo3=wabs(meng[ms],enh[1,3]+inh)*pow(meng[ms],[enorm[1,3],ephind[1,3]])


     w1=where(meng[ms] gt 0.3 and ydo1*meng[ms]^2*n gt 1e-14 and meng[ms] lt 1e8,nw1)
     w2=where(meng[ms] gt 0.3 and ydo2*meng[ms]^2*n gt 1e-14 and meng[ms] lt 1e8,nw2)
     w3=where(meng[ms] gt 0.3 and ydo3*meng[ms]^2*n gt 1e-14 and meng[ms] lt 1e8,nw3)
     r=100
;     w1=w1[[0,(nw1/r)*indgen(r),nw1-1]]
;     w2=w2[[0,(nw2/r)*indgen(r),nw2-1]]
;     w3=w3[[0,(nw3/r)*indgen(r),nw3-1]]

     xx1=[meng[ms[w1]],reverse(meng[ms[w1]]),meng[ms[w1[0]]]]
     xx2=[meng[ms[w2]],reverse(meng[ms[w2]]),meng[ms[w2[0]]]]
     xx3=[meng[ms[w3]],reverse(meng[ms[w3]]),meng[ms[w2[0]]]]
     yy1=[yup1[w1],reverse(ydo1[w1]),yup1[w1[0]]]*xx1^2*n
     yy2=[yup2[w2],reverse(ydo2[w2]),yup2[w2[0]]]*xx2^2*n
     yy3=[yup3[w3],reverse(ydo3[w3]),yup3[w3[0]]]*xx3^2*n
     polyfill,xx3,yy3,color=!grey40
     polyfill,xx2,yy2,color=!grey60
     polyfill,xx1,yy1,color=!grey80
;     oplot,xx,yy,color=!grey70
;     oplot,meng[ms],yup*meng[ms]^2*n*off,color=!grey70
;     oplot,meng[ms],ydo*meng[ms]^2*n*off,color=!grey70

     oplot,meng[ms],y*meng[ms]^2*n,color=color
     oplot,meng[ms],y*meng[ms]^2*n,line=1

     oploterror,eng[s],euf[s],engbin[s],euferr[s],psym=3,/nohat,errcolor=color
     leg=[leg,tbeta+b+'='+ntostr(beta[i],4)+'!S!I'+ntostr(betaerr[0,i],5)+'!R!E+'+ntostr(betaerr[1,i],4)+'!N'] 
     legend,leg,textcolor=[color,color],box=0,/top,/left
     
  endfor 

;  off=10^(-findgen(3)*2)
;  oploterror,eng[s14],euf[s14]*off[0],engbin[s14],euferr[s14]*off[0],psym=3,/nohat,errcolor=!red
;  oploterror,eng[s25],euf[s25]*off[1],engbin[s25],euferr[s25]*off[1],psym=3,/nohat,errcolor=!blue
;  oploterror,eng[s36],euf[s36]*off[2],engbin[s36],euferr[s36]*off[2],psym=3,/nohat,errcolor=!green
  multiplot,/reset,/default
  if keyword_set(ps) then endplot


  stop
;;;;;;;;;;;;;;;   OLD OLD OLD OLD
  cd,'~/GRBs/GRB110625A/XRT/'
  grb='GRB110625A'
  
  t1=[237.1,316.2]+10.
  t2=[421.7,562.3]+10.

  flux1=4.89e-5 ;; ph/cm2/s
  flux2=3.66e-5 ;; ph/cm2/s
  fluxerr1=1.93e-5
  fluxerr2=1.469e-5

  beta1=2.1
  beta2=3.6

  eng=[100,10e3]*1e3 ;; keV

  lat_xrt_sed,'GRB110625A',eng,flux1,beta1,eflux1

  lat_xrt_sed,'GRB110625A',eng,flux2,beta2,eflux2

;  eflux1=eflux1/1e6
;  eflux2=eflux2/1e6
;  cwt1=7.4964e-10/0.093641  ;;;; am i converting from photons to ergs?
;  cpc1=6.2679e-10/0.078295
;  cpc2=2.9713e-10/0.037474
  cpc1=6.8459e-10/1.129e+00 ;;; count rate to energy flux
  cpc2=3.2203e-10/8.602e-01 ;;; count rate to energy flux
  cpc1a=6.8459e-10/0.082218 ;;; counts flux to energy flux
  cpc2a=3.2203e-10/0.039276 ;;; counts flux to energy flux
  t1f=mrdfits('t1/t1_fits.fits',1)
  t2f=mrdfits('t2/t2_fits.fits',1)
  xbeta1=t1f.phoindex3
  xbeta2=t2f.phoindex3
;  nh=5.4
  norm1=t1f.norm4
  norm2=t2f.norm4

  readcol,'t1/t1_grp10_euf.dat',e1a,e1bina,efe1,efe1_low,efe1_high
  readcol,'t2/t2_grp10_euf.dat',e2a,e2bina,efe2,efe2_low,efe2_high


  if keyword_set(ps) then begplot,name='~/GRBs/GRB110625A/XRT_LAT_SED.ps',/color
  !p.multi=[0,1,2]

  if keyword_set(justflux) then begin 
     en=[0.3+findgen(7)/10.,findgen(10)+1,findgen(10)*10.+10.,findgen(10)*100.+100.,findgen(10)*1e3+1000.,findgen(10)*1e4+1e4,findgen(10)*1e5+1e5,findgen(10)*1e6+1e6]
     
     plot,[0.1,1e8],[1e-12,1e-6],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle=!tsym.nu+' F!L'+!tsym.nu+'!N (erg cm!U-1!N s!U-1!N)',charsize=1.5
;     oploterror2,e1[0:3],efe1[0:3]*cwt1,[[e1bin[0:3]],[e1bin[0:3]]],[[efe1_low[0:3]],[efe1_high[0:3]]]*cwt1,psym=3,/nohat
;     oploterror2,e1[4:*],efe1[4:*]*cpc1,[[e1bin[4:*]],[e1bin[4:*]]],[[efe1_low[4:*]],[efe1_high[4:*]]]*cpc1,psym=3,/nohat,color=!red
     oploterror2,e1a,efe1*cpc1,[[e1bina],[e1bina]],[[efe1_low],[efe1_high]]*cpc1,psym=3,/nohat,color=!red
     oplot,[1e5,1e7],[eflux1,eflux1],color=!green
     oplot,[1e6,1e6],eflux1*[(1.-fluxerr1/flux1),(1.+fluxerr1/flux1)],color=!green
     oplot,[1e5,1e7,1e7,1e5,1e5],eflux1*[(1.+fluxerr1/flux1),(1.+fluxerr1/flux1),(1.-fluxerr1/flux1),(1.-fluxerr1/flux1),(1.+fluxerr1/flux1)],color=!green

;     oplot,en,pow(en,[0.101646*cwt1,xbeta1])*en,line=2
;     oplot,en,pow(en,[8.49882E-02*cpc1,xbeta1])*en,color=!red,line=2
     oplot,en,pow(en,[norm1*cpc1,xbeta1])*en,line=2
     oplot,en,wabs(en,nh)*pow(en,[norm1*cpc1,xbeta1])*en,line=2,color=!cyan
     
     plot,[0.1,1e8],[1e-12,1e-6],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle=!tsym.nu+' F!L'+!tsym.nu+'!N (erg cm!U-1!N s!U-1!N)',charsize=1.5
     oploterror2,e2a,efe2*cpc2,[[e2bina],[e2bina]],[[efe2_low],[efe2_high]]*cpc2,psym=3,/nohat,color=!red
     oplot,[1e5,1e7],[eflux2,eflux2],color=!green
     oplot,[1e6,1e6],eflux2*[(1.-fluxerr2/flux2),(1.+fluxerr2/flux2)],color=!green
     oplot,en,pow(en,[norm2*cpc2,xbeta2])*en,line=2
  endif else begin 

     readcol,'t1/t1_grp10_euf_jy_hz.dat',nu1,nu1bin,fefe1,fefe1_low,fefe1_high
     readcol,'t2/t2_grp10_euf_jy_hz.dat',nu2,nu2bin,fefe2,fefe2_low,fefe2_high

     h=4.135e-15*1e-3 ;; keV s
     e1=nu1*h
     e2=nu2*h
     e1bin=nu1bin*h
     e2bin=nu2bin*h

     en=[0.3+findgen(7)/10.,findgen(10),findgen(10)*10.,findgen(10)*100.,findgen(10)*1e3,findgen(10)*1e4,findgen(10)*1e5,findgen(10)*1e6]
     
;     c1=[replicate(cwt1,4),replicate(cpc1,8)]
     fefe1a=flux2jy(efe1*cpc1a,xbeta1,eeff=1.)
     fefe2a=flux2jy(efe2*cpc2a,xbeta2,eeff=1.)
;     fefe1_low=flux2jy(efe1_low*c1,xbeta1)
;     fefe1_high=flux2jy(efe1_high*c1,xbeta1)
;     fefe2_low=flux2jy(efe2_low*cpc2,xbeta2)
;     fefe2_high=flux2jy(efe2_high*cpc2,xbeta2)

     feflux1=flux2jy(eflux1,beta1,low=100e3,high=10e6)
     feflux2=flux2jy(eflux2,beta2,low=100e3,high=10e6)

     plot,[0.1,1e8],[1e-10,1e-2],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle='F (Jy)',charsize=1.5
;     oploterror2,e1[0:3],fefe1[0:3],[[e1bin[0:3]],[e1bin[0:3]]],[[fefe1_low[0:3]],[fefe1_high[0:3]]],psym=3,/nohat
;     oploterror2,e1[4:*],fefe1[4:*],[[e1bin[4:*]],[e1bin[4:*]]],[[fefe1_low[4:*]],[fefe1_high[4:*]]],psym=3,/nohat,color=!red
     oploterror2,e1,fefe1,[[e1bin],[e1bin]],[[fefe1_low],[fefe1_high]],psym=3,/nohat,color=!red
     oplot,[1e5,1e7],[feflux1,feflux1],color=!green
     oplot,[1e6,1e6],feflux1*[(1.-fluxerr1/flux1),(1.+fluxerr1/flux1)],color=!green
;     oplot,en,pow(en,[0.101646*fefe1[0]/efe1[0],xbeta1])*en,line=2
     oplot,en,pow(en,[norm1*fefe1[0]/efe1[0],xbeta1])*en,line=2
     
     plot,[0.1,1e8],[1e-10,1e-2],/nodata,/xlog,/ylog,xtitle='Energy (keV)',ytitle='F (Jy)',charsize=1.5
     oploterror2,e2,fefe2,[[e2bin],[e2bin]],[[fefe2_low],[fefe2_high]],psym=3,/nohat,color=!red
     oplot,[1e5,1e7],[feflux2,feflux2],color=!green
     oplot,[1e6,1e6],feflux2*[(1.-fluxerr2/flux2),(1.+fluxerr2/flux2)],color=!green
     oplot,en,pow(en,[norm2*fefe2[0]/efe2[0],xbeta2])*en,line=2

  endelse 
  
  !p.multi=0
  
  if keyword_set(ps) then endplot
  stop
;We detect the GRB in two distinct time periods. 
;In the first time bin (from 237.1 seconds to 316.2 seconds after the GBM trigger) we measure 
;a flux (100 MeV-10 GeV) of (4.9 +/- 1.9) x 1e-05 ph/cm^2/s and a spectral index beta of -2.1 +/- 0.3, with; a Test Statistic TS=31.

;In the second time bin, from 421.7 to 562.3 seconds after the GBM trigger, 
;the measured flux is (3.7 +/- 1.5) x 1e-5  ph/cm^2/s with spectral index beta of -3.6 +/- 0.8 and a TS=19 (stat err
  return
end 

pro lat_xrt_sed,grb,en,flux,beta,eflux,norm1=norm1

  kev2erg=1.602e-12*1d3  ;;; 1 ev = 1.6e-12 erg ; 1kev = 1.6e-9 erg
  gev2erg=kev2erg*1d6
  eng2=(findgen(1000)*0.1+0.1)*1d6
  de=eng2[1]-eng2[0]
  de1=en[1]-en[0]
  normg1=flux/(intpow(en,[1.,beta],tnorm=1e6)*de1) ;; norm at 1 GeV
  norm1=normg1*1d6^beta                                     ;;; norm at 1 keV
  w=where(eng2 ge en[0] and eng2+de le en[1])
  f1=pow(eng2[w]+de/2.,[norm1,beta])
;  eflux=10^mean(alog10(f1*(eng2[w]+de/2.)*kev2erg))*de1
  eflux=mean(f1*(eng2[w]+de/2.)*kev2erg)*de1

;  meane=10^mean(alog10(en))
;  eflux1=intpow(en,[norm1,beta])*de1*kev2erg*meane
;  eflux=eflux1

;  print,eflux,eflux0,eflux1
  
  return
end 
