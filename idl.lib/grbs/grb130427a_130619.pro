@fit_functions
pro plot_seds

  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_2sed_wLAT_sbpl.dat',freq,freqerr,nfnu,nfnuerr,mnfnu,format='(a,a,a,a,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_noabs_Hz_2sed_wLAT_sbpl.dat',freq0,freqerr0,nfnu0,nfnuerr0,mnfnu0,format='(a,a,a,a,a)'
;  norms=[3.86e-3,3.21e-3,3.20e-3,3.86e-3,6.06e-4,3.65e-4,3.74e-4,6.06e-4]
;  norms=[3.83e-3,3.15e-3,3.15e-3,3.83e-3,6.0e-4,3.57e-4,3.66e-4,6.0e-4]
;  norms=[3.89e-3,3.32e-3,3.29e-3,3.89e-3,6.17e-4,3.79e-4,3.92e-4,6.17e-4]  ;; 100bin BPL
;  norms=[2.46e-6,2.29e-6,2.27e-6,2.46e-6,1.93e-11,1.18e-11,1.22e-11,1.94e-11] ;; 100bin SBPL
;  norms=[2.81e-6,2.59e-6,2.57e-6,2.81e-6,4.99e-13,2.98e-13,3.09e-13] ;; 100bin SBPL w/ LAT extrap
  norms=[6.9e-7,6.32e-7,6.27e-7,6.9e-7,3.11e-6,1.91e-6,1.97e-6] ;; 100bin SBPL w/ LAT extrap tied phind
  normratio=[norms[2]/norms[0],norms[2]/norms[1],norms[2]/norms[2],norms[2]/norms[3],$
             norms[6]/norms[4],norms[6]/norms[5],norms[6]/norms[6]];,norms[6]/norms[7]]
;; show parameters 16,32,48,64,80,96,112

;; normalize to second nustar epoch

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')
  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=(388741688d)-51.
  tnu1=nu1-battrig
  tnu2=nu2-battrig+1000
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

;  tnustart=[tnu1,tnu3,tnu5]
;  tnustop=[tnu2,tnu4,tnu6]
  tnustart=[tnu1,tnu3]
  tnustop=[tnu2,tnu6]

;  w=where(freq eq 'NO',nw) ;;xrt1,xrt2,nu1a,nu1b,nu2a,nu2b,nu3a,nu3b
;  w1=indgen(w[0]-1)
;  w2=indgen(w[1]-w[0]-1)+w[0]+1
;  w3=indgen(w[2]-w[1]-1)+w[1]+1
;  w4=indgen(w[3]-w[2]-2)+w[2]+1
;  w5=indgen(w[4]-w[3]-1)+w[3]+1
;  w6=indgen(w[5]-w[4]-1)+w[4]+1
;;  w7=indgen(w[6]-w[5]-1)+w[5]+1
;  w7=indgen(n_elements(freq)-nw-w[5])+w[5]+1

  w=where(freq eq 'NO',nw) ;;opt1,xrt1,nu1a,nu1b,lat1,opt2,xrt2,nu2a,nu2b,lat2
  w1=indgen(w[0]-1)
  w2=indgen(w[1]-w[0]-1)+w[0]+1
  w3=indgen(w[2]-w[1]-1)+w[1]+1
  w4=indgen(w[3]-w[2]-1)+w[2]+1
  w5=indgen(w[4]-w[3]-1)+w[3]+1
  w6=indgen(w[5]-w[4]-1)+w[4]+1
  w7=indgen(n_elements(freq)-nw-w[5])+w[5]+1
;  w7=indgen(w[6]-w[5]-1)+w[5]+1
;  w8=indgen(w[7]-w[6]-1)+w[6]+1
;  w9=indgen(w[8]-w[7]-1)+w[7]+1
;  w8=indgen(n_elements(freq)-nw-w[6])+w[6]+1

;  wi1=indgen(426)
;  wi2=indgen(531-426)+426
;  wi3=indgen(623-532)+532
;  nwi1=n_elements(wi1)
;  nwi2=n_elements(wi2)
;  nwi3=n_elements(wi3)

  wi1=[w1,w2,w3,w4]
  nwi1=[n_elements(w1),n_elements(w2),n_elements(w3),n_elements(w4)]
  wi2=[w5,w6,w7];,w8]
  nwi2=[n_elements(w5),n_elements(w6),n_elements(w7)];,n_elements(w8)]
;  wi3=[w7]
  freq1=double(freq[wi1])
  freqerr1=double(freqerr[wi1])
  nfnu1=double(nfnu[wi1])
  nfnuerr1=double(nfnuerr[wi1])
  wneg=where(nfnu1-nfnuerr1 lt 0)
;  nfnuerr1[wneg]=nfnu1[wneg]-1e-17
  mnfnu1=double(mnfnu[wi1])
  normratio1=[replicate(normratio[0],nwi1[0]),replicate(normratio[1],nwi1[1]),replicate(normratio[2],nwi1[2]),replicate(normratio[3],nwi1[3])]
  absratio1=double(mnfnu0[wi1])/mnfnu1*normratio1
  freq2=double(freq[wi2])
  freqerr2=double(freqerr[wi2])
  nfnu2=double(nfnu[wi2])
  nfnuerr2=double(nfnuerr[wi2])
  wneg=where(nfnu2-nfnuerr2 lt 0)
;  nfnuerr2[wneg]=nfnu2[wneg]-1e-14
  mnfnu2=double(mnfnu[wi2])
  normratio2=[replicate(normratio[4],nwi2[0]),replicate(normratio[5],nwi2[1]),replicate(normratio[6],nwi2[2])];,replicate(normratio[7],nwi2[3])]
  absratio2=double(mnfnu0[wi2])/mnfnu2*normratio2

;;   freq3=double(freq[wi3])
;;   freqerr3=double(freqerr[wi3])
;;   nfnu3=double(nfnu[wi3])
;;   nfnuerr3=double(nfnuerr[wi3])
;;   mnfnu3=double(mnfnu[wi3])
;;   normratio3=[replicate(normratio[6],nwi3[0])]
;;   absratio3=double(mnfnu0[wi3])/mnfnu3*normratio3
  

;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int1.dat',freq1,freqerr1,nfnu1,nfnuerr1,mnfnu1,format='(d,d,d,d,d)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int2.dat',freq2,freqerr2,nfnu2,nfnuerr2,mnfnu2,format='(d,d,d,d,d)'
;  readcol,'~/Desktop/GRB130427A/NuSTAR/eeuf_Hz_int3.dat',freq3,freqerr3,nfnu3,nfnuerr3,mnfnu3,format='(d,d,d,d,d)'

  h=4.1357d-15*(1e-3/1.)        ; keV s
  c=3e8                         ; cm/s
  eng1=h*freq1
  engerr1=h*freqerr1
  eng2=h*freq2
  engerr2=h*freqerr2
;  eng3=h*freq3
;  engerr3=h*freqerr3


;  nfnu1=mnfnu1
;  nfnu2=mnfnu2
;  nfnu3=mnfnu3

  begplot,name='~/Desktop/GRB130427A/NuSTAR/joint_sed.ps',/color,font='helvetica'
;  !p.multi=[0,1,2]
  multiplot,[1,2],/init
;  iz=[2,0,1]
  color=[!green,!blue,!red]
  xrange=[1e-3,100e6]
  yrange=[1e-14,1e-09]
  for z=0,1 do begin 
;     z=iz[zz]
     multiplot
     if z eq 1 then xtitle='Energy (keV)'
     plotsym,0,0.5,/fill

     plot,xrange,yrange,/nodata,/xlog,/ylog,/xsty,/ysty,ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)',xtitle=xtitle
 
     case z of 
        0: begin 
           oplot,eng1,nfnu1*absratio1,psym=8,color=!lightgreen
;           plot,eng1,nfnu1*absratio1,/xlog,/ylog,psym=3,xrange=[1e-3,100e6],/xsty,/ysty,yrange=[1e-14,4e-10],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)' ;,xtitle='Energy (keV)'
;           a=where(eng1[1:*]-eng1[0:*] lt 0)
;           color=[replicate(!lightgreen,a[0]),replicate(!forestgreen,a[1]-a[0]),replicate(!forestgreen,n_elements(eng1)-a[1])]
           for i=0,n_elements(eng1)-1 do begin
              if nfnu1[i]-nfnuerr1[i] lt 0 then errlow=1e-14 else errlow=nfnu1[i]-nfnuerr1[i]
              oplot,[eng1[i]-engerr1[i],eng1[i]+engerr1[i]],[nfnu1[i],nfnu1[i]]*absratio1[i],color=!lightgreen
              oplot,[eng1[i],eng1[i]],[errlow,nfnu1[i]+nfnuerr1[i]]*absratio1[i],color=!lightgreen
           endfor 
        end 
        1: begin 
           oplot,eng2,nfnu2*absratio2,psym=8,color=!lightblue

;           plot,eng2,nfnu2*absratio2,/xlog,/ylog,psym=3,xtitle='Energy (keV)',xrange=[0.3,100e6],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)',yrange=[1e-13,4e-10],/xsty,/ysty
;           a=where(eng2[1:*]-eng2[0:*] lt 0)
;           color=[replicate(!dodgerblue,a[0]),replicate(!royalblue,a[1]-a[0]),replicate(!royalblue,a[2]-a[1])]
        
           oplot,eng2,nfnu2*absratio2,psym=3,color=!blue
           for i=0,n_elements(eng2)-1 do begin
              if nfnu2[i]-nfnuerr2[i] lt 0 then errlow=1e-14 else errlow=nfnu2[i]-nfnuerr2[i]              
              oplot,[eng2[i]-engerr2[i],eng2[i]+engerr2[i]],[nfnu2[i],nfnu2[i]]*absratio2[i],color=!dodgerblue
              oplot,[eng2[i],eng2[i]],[errlow,nfnu2[i]+nfnuerr2[i]]*absratio2[i],color=!dodgerblue;color[i]
           endfor 
        end 
        2: begin
           oplot,eng3,nfnu3*absratio3,psym=3

;           plot,eng3,nfnu3*absratio3,/xlog,/ylog,psym=3,xtitle='Energy (keV)',xrange=[0.3,100e6],ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (ergs cm!U-2!Ns!U-1!N)',yrange=[1e-13,4e-10],/xsty,/ysty
;           a=where(eng3[1:*]-eng3[0:*] lt 0)
;           color=replicate(!salmon,a[0])
        
           oplot,eng3,nfnu3*absratio3,psym=3,color=!red
           for i=0,n_elements(eng3)-1 do begin
              oplot,[eng3[i]-engerr3[i],eng3[i]+engerr3[i]],[nfnu3[i],nfnu3[i]]*absratio3[i],color=!salmon;color[i]
              oplot,[eng3[i],eng3[i]],[nfnu3[i]-nfnuerr3[i],nfnu3[i]+nfnuerr3[i]]*absratio3[i],color=!salmon;color[i]
           endfor
        end 
     endcase            

;;   a=where(eng3[1:*]-eng3[0:*] lt 0)
;;   color=[replicate(!red,a[0]),replicate(!salmon,a[1]-a[0]),replicate(!orange,n_elements(eng3)-a[1])]

;;   c=1.;0.2
;;   oplot,eng3,c*nfnu3,psym=3,color=!red
;;   for i=0,n_elements(eng3)-1 do begin
;;      oplot,[eng3[i]-engerr3[i],eng3[i]+engerr3[i]],c*[nfnu3[i],nfnu3[i]],color=color[i]
;;      oplot,[eng3[i],eng3[i]],c*[nfnu3[i]-nfnuerr3[i],nfnu3[i]+nfnuerr3[i]],color=color[i]
;;   endfor 

     s1=sort(eng1)
     s2=sort(eng2)
     s3=sort(eng3)
;  s3=sort(eng3)
     eng=[(dindgen(9)+1.)*1e-3,(dindgen(9)+1.)*1e-2,(dindgen(9)+1.)/10.,(dindgen(99)+1.),(dindgen(9)+1)*1e2,(dindgen(9)+1)*1e3,(dindgen(9)+1.)*1e4,(dindgen(9)+1.)*1e5,(dindgen(9)+1.)*1e6,(dindgen(10)+1.)*1e7]
;     engerr=[replicate(1e-2,9),replicate(0.1,9),replicate(1.,99),replicate(1e3,9),replicate(1e4,9),replicate(1e5,9),replicate(1e6,9)]

;     p=[1.84910,1.92259]
;     perr=[0.032,0.078]
;     flux0=[-10.4655,-11.2168]
;     fluxerr0=[0.013,0.046]

     ;;SBPL
;     p=[1.69,1.77]
     p=[1.69,1.69]
     perr=[0.02,0.02]
;     be=[87.,1.7e4]
;     be=[82.7,1.42571E+05]
     be=[178,28.]
     beerr=[10,10.]
;     flux=[3.35e-11,3.77e-12]
;     flux=[3.36e-11,3.84e-12]
     flux=[3.42e-11,3.46e-12]
     fluxerr=[1e-12,1e-13]
;     readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance.txt',ebvc,nhc,p1ac,be1c,n1c,n2c,n3c,be2c,p1bc,n4c,n5c,n6c,format=('f,f,f,f,f,f,f,f,f,f,f'),skip=4
     readcol,'~/Desktop/GRB130427A/NuSTAR/sbpl_covariance2.txt',p1ac,be1c,n1c,format='(f,f,f)'
     ;; BPL
;     p=[1.72,1.765]
;     perr=[0.011,0.018]
;     be=[17.64,32.6]
;     beerr=[1.7,12.]
     q=p+0.5
     s=0.85
;     flux=[3.08e-11,3.82e-12]
;     fluxerr=[4e-12,2e-13]

;; BPL
;     readcol,'~/Desktop/GRB130427A/NuSTAR/bpl_covariance2.txt',ebvc,nhc,p1ac,be1c,n1c,n2c,n3c,be2c,p1bc,n4c,n5c,n6c,format=('f,f,f,f,f,f,f,f,f,f,f')
;     readcol,'~/Desktop/GRB130427A/NuSTAR/bpl_covariance.txt',p1ac,be1c,n1c,format='(f,f,f)'
;     par1=[2,3,6]
;     par2=[7,8,11]
;     m=[[ebvc],[nhc],[p1ac],[be1c],[n1c],[n2c],[n3c],[be2c],[p1bc],[n4c],[n5c],[n6c]]
     par1=[0,1,2]
     par2=[3,4,5]
     m=transpose([[p1ac],[be1c],[n1c]])
     m1=[[m[par1,par1[0]]],[m[par1,par1[1]]],[m[par1,par1[2]]]]
     m2=[[m[par1,par2[0]]],[m[par1,par2[1]]],[m[par1,par2[2]]]] ;;; if cov2
;     m2=[[m[par2,par2[0]]],[m[par2,par2[1]]],[m[par2,par2[2]]]] ;;; if cov1
;     m1=[transpose([p1ac[0:2]]),transpose([be1c[0:2]]),transpose(n1c[0:2])]
;     m2=[transpose([p1ac[3:*]]),transpose([be1c[3:*]]),transpose(n1c[3:*])]
     if z eq 0 then mm=m1 else mm=m2
     kev2erg=1.602e-12*1d3  ;; 1kev= # ergs
;     fluxerr=10.^(flux0+fluxerr0)-10^flux0
;     flux=10^flux0
     emin=3.0
     emax=79.0
     w=where(eng ge emin and eng le emax)
     norm=[norms[2],norms[6]];,norms[6]]

     normerr=(fluxerr/flux)*norm

     f=smbknpow(eng,[norm[z],p[z],be[z],q[z],s])
;     f=bknpow(eng,[norm[z],p[z],be[z],q[z]])
     arb=1.6e-9                 ;0.09 ;;kev2erg
     ;; SBPL
     ebe=eng/be[z]
     dfdn=(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s)
     dfdbe=-norm[z]/s*(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s-1.)*(-q[z]*s*eng^(s*q[z])*be[z]^(-q[z]*s-1)-p[z]*s*eng^(s*p[z])*be[z]^(-p[z]*s-1.))
;     dfdp=-norm[z]/s*(ebe^(s*p[z])+ebe^(s*q[z]))^(-1./s-1.)*(s*ebe^(s*p[z])-alog(be[z])*s*ebe^(s*p[z]))
     dfdp=-norm[z]/s*(ebe^(s*(p[z]+0.5))+ebe^(p[z]*s))^(-1./s-1)*(s*ebe^(s*(p[z]+0.5))-alog(be[z])*s*ebe^(s*(p[z]+0.5))+s*ebe^(s*p[z])-alog(be[z])*s*ebe^(p[z]*s))

;     dfdq=-norm[z]*(ebe^(s*p[z])+ebe^(s*q[z]))^(1./s-1.)*ebe^(s*q[z])*(alog(eng)-alog(be[z]))
     sigf2=dblarr(n_elements(eng))
     for ee=0,n_elements(eng)-1 do begin 
        dm=[[dfdp[ee]],[dfdbe[ee]],[dfdn[ee]]]
        sigf2[ee]=transpose(dm)##mm##dm
     endfor 
     sigf=sqrt(sigf2)

     ;; BKNPOW
     goto,bpl
     wbb=where(eng lt be[z]) ;; where before break
     wab=where(eng gt be[z]) ;; where after break
     dfdn=[eng[wbb]^(-p[z]),be[z]^(q[z]-p[z])*eng[wab]^(-q[z])]
;     dfdn=be[z]^(q[z]-p[z])*eng^(-q[z])
     dfdp=[norm[z]*eng[wbb]^(-p[z])*alog(eng),norm[z]*be[z]^(q[z]-p[z])*eng[wab]^(-q[z])*alog(be[z])]
;     dfdp=norm[z]*be[z]^(q[z]-p[z])*eng^(-q[z])*alog(be[z])
;     dfdq=[eng[wbb]*0.,norm[z]*be[z]^(q[z]-p[z])*eng[wab]^(-q[z])*(alog(be[z])-alog(eng[wab]))]
     dfdbe=[eng[wbb]*0.,(q[z]-p[z])*norm[z]*be[z]^(q[z]-p[z]-1.)*eng[wab]^(-q[z])]
;     dfdbe=(q[z]-p[z])*norm[z]*be[z]^(q[z]-p[z]-1.)*eng^(-q[z])
     sigf2=dblarr(n_elements(eng))
     for ee=0,n_elements(eng)-1 do begin 
        dm=[[dfdp[ee]],[dfdbe[ee]],[dfdn[ee]]]
        sigf2[ee]=transpose(dm)##mm##dm
     endfor 
;     sigf=sqrt(sigf2)
     bpl:
     ;; OLD
;     dfdn=eng^(-p[0])
;     dfdp=norm[0]*eng^(-p[0])*alog(eng) ;p[0]*norm[0]*eng^(-p[0]-1.)
;  x=[eng,reverse(eng)]
;  y=[eng^2*(f+sqrt(dfdn^2*normerr[0]^2+dfdp^2*perr[0]^2)),reverse(eng^2*(f-sqrt(dfdn^2*normerr[0]^2+dfdp^2*perr[0]^2)))]
;  polyfill,x,y,color=!green,/transparent
;     if z eq 0 then begin 
        oplot,eng,eng^2*f*arb,color=color[z]
        x=eng
        ;; just wrong, doesn't use covariance
;        sigf=sqrt(dfdn^2*normerr[z]^2+dfdp^2*perr[z]^2+dfdq^2*perr[z]^2+dfdbe^2*beerr[z]^2)
        y1=eng^2*(f+sigf)*arb
        y2=eng^2*(f-sigf)*arb
;        polyfill,[x,reverse(x)],[y1,reverse(y2)],color=color[z]
;;        oplot,eng,y1,line=1,color=color[z]
;;        oplot,eng,y2,line=1,color=color[z]
;     endif 
;     f=pow(eng,[norm[1],p[1]])
;     dfdn=eng^(-p[1])
;     dfdp=norm[1]*eng^(-p[1])*alog(eng) ;p[1]*norm[1]*eng^(-p[1]-1.)
;     if z eq 1 then begin 
;        oplot,eng,eng^2*f*arb,color=!blue
;        oplot,eng,eng^2*(f+sqrt(dfdn^2*normerr[1]^2+dfdp^2*perr[1]^2+dfdpp^2*pp[1]^2+dfdbe^2*be[1]^2))*arb,line=1,color=!blue
;        oplot,eng,eng^2*(f-sqrt(dfdn^2*normerr[1]^2+dfdp^2*perr[1]^2+dfdpp^2*pp[1]^2+dfdbe^2*be[1]^2))*arb,line=1,color=!blue
;     endif 

;     f=pow(eng,[norm[2],p[2]])
;     dfdn=eng^(-p[2])
;     dfdp=norm[2]*eng^(-p[2])*alog(eng) ;p[1]*norm[1]*eng^(-p[1]-1.)
;     if z eq 2 then begin 
;        oplot,eng,eng^2*f*arb,color=!red
;        oplot,eng,eng^2*(f+sqrt(dfdn^2*normerr[2]^2+dfdp^2*perr[2]^2+dfdpp^2*pp[2]^2+dfdbe^2*be[2]^2))*arb,line=1,color=!red
;        oplot,eng,eng^2*(f-sqrt(dfdn^2*normerr[2]^2+dfdp^2*perr[2]^2+dfdpp^2*pp[2]^2+dfdbe^2*be[2]^2))*arb,line=1,color=!red
;     endif 
     

     ;; LAT UL
     latgam=q;[1.85,1.92]
     lateng=[100e3,100e6]
     leflux=[3.08e-10,3.43e-10] ;/lfact ;; for converting to nuFnu
     plotsym,1,5,thick=10
     w=where(eng ge 100e3 and eng le 1e8,nw)

     ff1_0=[(lateng[1]/lateng[0])^(2-latgam[0]) - 1.]/(2-latgam[0])
     ff1_1=[(lateng[1]/lateng[0])^(2-latgam[1]) - 1.]/(2-latgam[1])
     nfact0=[(eng[w]/lateng[0])^(latgam[0]-2)*ff1_0[0]]
     nfact1=[(eng[w]/lateng[0])^(latgam[1]-2)*ff1_1[0]]
     if z eq 0 then begin 
        plots,eng[w[14]],leflux[0]/nfact0[14],psym=8,color=!green
        oplot,eng[w],leflux[0]/nfact0,color=!green,thick=10
        oplot,[lateng[0],lateng[1]],[1.15e-12,1.15e-12],color=!green
        mlateng=10^mean(alog10(lateng))
        oplot,[mlateng,mlateng],[1.15e-12-6e-13,1.15e-12+7e-13],color=!green
     endif else begin 
        plots,eng[w[12]],leflux[1]/nfact1[12],psym=8,color=!blue
        oplot,eng[w],leflux[1]/nfact1,color=!blue,thick=10
     endelse 


     ;; MORE FUN FROM YONI - LAT EXTRAP

  ;;; need to make sure latgam is consistent between LC & SPEC & UL
     llc=mrdfits('~/Desktop/GRB130427A/NuSTAR/lat/latlc.fits',1)
;  w0=where(llc.time ge tnustart[0] and llc.time le tnustop[0],nw0)
;  w1=where(llc.time ge tnustart[1] and llc.time le tnustop[1],nw1)
     leflux=llc.flux
     lefluxerr0=llc.fluxerr[0]
     lefluxerr1=llc.fluxerr[1]
     ff1_0=[(lateng[1]/lateng[0])^(2-latgam[0]) - 1.]/(2-latgam[0])
     ff1_1=[(lateng[1]/lateng[0])^(2-latgam[1]) - 1.]/(2-latgam[1])
     nfact0=[(eng[w]/lateng[0])^(latgam[0]-2)*ff1_0[0]]
     nfact1=[(eng[w]/lateng[0])^(latgam[1]-2)*ff1_1[0]]

     
     slope=-(alog10(llc[11].flux)-alog10(llc[0].flux))/(alog10(llc[11].time)-alog10(llc[0].time)) ;; LAT temporal decay index???
     slopep=-(alog10(llc[11].flux+llc[11].fluxerr[1])-alog10(llc[0].flux+llc[0].fluxerr[1]))/(alog10(llc[11].time)-alog10(llc[0].time))
     slopem=-(alog10(llc[11].flux-llc[11].fluxerr[0])-alog10(llc[0].flux-llc[0].fluxerr[0]))/(alog10(llc[11].time)-alog10(llc[0].time))
     norm=median(llc.flux/pow(llc.time,[1.,slope]))
     normerr0=median((llc.flux-llc.fluxerr[0])/pow(llc.time,[1.,slopem]))
     normerr1=median((llc.flux+llc.fluxerr[1])/pow(llc.time,[1.,slopep]))
     iflux0=intpow([tnustart[0],tnustop[0]],[norm,slope])
     iflux0errm=intpow([tnustart[0],tnustop[0]],[normerr0,slopem])
     iflux0errp=intpow([tnustart[0],tnustop[0]],[normerr1,slopep])
     iflux1=reform(intpow([tnustart[1],tnustop[1]],[norm,slope]))
     iflux1errm=intpow([tnustart[1],tnustop[1]],[normerr0,slopem])
     iflux1errp=intpow([tnustart[1],tnustop[1]],[normerr1,slopep])

;  polyfill,[eng[w],reverse(eng[w])],[iflux0errp[0]/nfact0,reverse(iflux0errm[0]/nfact0)],/line_fill,orient=45,color=!green
;  polyfill,[eng[w],reverse(eng[w])],[iflux1errp[0]/nfact1,reverse(iflux1errm[0]/nfact1)],/line_fill,orient=45,color=!blue

;  oplot,[eng[w],reverse(eng[w]),eng[w[0]]],[iflux0errp[0]/nfact0,reverse(iflux0errm[0]/nfact0),iflux0errp[0]/nfact0[0]],color=!green
;  oplot,[eng[w],reverse(eng[w]),eng[w[0]]],[iflux1errp[0]/nfact1,reverse(iflux1errm[0]/nfact1),iflux1errp[0]/nfact1[0]],color=!blue

     g=!tsym.gamma_cap
     pm=!tsym.plusminus
     if z eq 0 then $
        legend,['T+T0=1.03-1.61 x 10!U5!N s ('+g+'='+numdec(p[0],2)+pm+numdec(perr[0],2)+')'],box=0,/top,/left,textcolor=!green
     if z eq 1 then $
        legend,['T+T0=4.11-4.96 x 10!U5!N s ('+g+'='+numdec(p[1],2)+pm+numdec(perr[1],2)+')'],box=0,/top,/left,textcolor=!blue
     if z eq 2 then $
        legend,['T+T0=2-50 x 10!U4!N s ('+g+'='+numdec(p[2],2)+pm+numdec(perr[2],2)+')'],box=0,/top,/left,textcolor=!red

  endfor 
  multiplot,/reset,/default
;  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/joint_sed.ps ~/Desktop/GRB130427A/NuSTAR/joint_sed.pdf'


  stop
  return
end 

pro rebin_lc,t,c,terr,cerr,mincts,lc=lc

  if mincts eq 0 then begin 
     lc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
     lc=replicate(lc,n_elements(t))
     lc.time=t
     lc.tstart=t-terr
     lc.tstop=t+terr
     lc.ctr=c/(terr*2.)
     lc.ctrerr=cerr/(terr*2.)
     lc.exp=terr*2.
     return
  endif

;  w=where(c gt 0)
;  t=t[w]
;  c=c[w]
;  terr=terr[w]
;  cerr=cerr[w]
  nt=n_elements(t)
  tnew=t[0]
  cnew=c[0]
  texp=0
  cerrnew=cerr[0]
  tstart=0d
  tstop=0d
  i=0

  n=mincts
  for i=0,nt-1,n do begin
     if i+n gt nt then n=nt-i-1
     print,i,i+n-1
     tnew=[tnew,mean(t[i:i+n-1])]
     texp=[texp,n]
     cnew=[cnew,total(c[i:i+n-1])]
     cerrnew=[cerrnew,sqrt(total((cerr[i:i+n-1])^2))];*mean(c[i:i+1])]
     tstart=[tstart,t[i]-terr[i]]
     tstop=[tstop,t[i+n-1]+terr[i+n-1]]
  endfor 

  goto,skip2
  for i=0,nt-1 do begin
     if c[i] ge mincts then begin;or ((t[i+1]-t[i])/t[i] lt 1.) then begin 
        ctmp=c[i]
        ttmp=t[i]
        cetmp=cerr[i]
        etmp=1
        tmin=t[i]-terr[i]
        tmax=t[i]+terr[i]
     endif else begin
        j=i
        ctmp=c[i]
        while (ctmp lt mincts and j lt nt-1) do begin 
           ctmp=ctmp+c[j]
           j=j+1
        endwhile 
        ttmp=(t[j-1]-t[i])/2.+t[i];mean(t[i:j-1])
        tmin=t[i]-terr[i]
        tmax=t[j-1]+terr[i]
        etmp=j-i+1
;        ctmp=ctmp/etmp
;        cetmp=sqrt(ctmp)
        cetmp=sqrt(total((cerr[i:j-1]/c[i:j-1])^2))*ctmp;/etmp
        i=j
;        colprint,ctmp,ttmp,etmp
     endelse 
     cnew=[cnew,ctmp]
     tnew=[tnew,ttmp]
;     terrnew=[terrnew,terr[0]*etmp]
     tstart=[tstart,tmin]
     tstop=[tstop,tmax]
     cerrnew=[cerrnew,cetmp]
     texp=[texp,etmp]
        
  endfor 

skip2:
  cnew=cnew[1:*]
  tnew=tnew[1:*]
  texp=texp[1:*]
  cerrnew=cerrnew[1:*]
  tstart=tstart[1:*]
  tstop=tstop[1:*]

  lc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
  lc=replicate(lc,n_elements(cnew))
  lc.time=tnew
  lc.tstart=tstart
  lc.tstop=tstop
  lc.exp=texp*5000.
  lc.ctr=cnew/lc.exp
  lc.ctrerr=cerrnew/lc.exp

;  ploterror,t,c,cerr,/xlog,/ylog,psym=3,/nohat,yrange=[1,1e3]
;  for i=0,nt-1 do oplot,[t[i]-terr[i],t[i]+terr[i]],[c[i],c[i]]
;  oploterror,tnew,cnew,cerrnew,psym=3,errcolor=!red,/nohat
;  for i=0,n_elements(tnew)-1 do oplot,[tstart[i],tstop[i]],[cnew[i],cnew[i]],color=!red;oplot,[tnew[i]-terrnew[i],tnew[i]+terrnew[i]],[cnew[i],cnew[i]],color=!red
;stop

return
end

pro plot_lcs

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')
  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=(388741688d)-51.
  tnu1=nu1-battrig
  tnu2=nu2-battrig+1000
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

;  tnustart=[tnu1,tnu3,tnu5]
;  tnustop=[tnu2,tnu4,tnu6]
  tnustart=[tnu1,tnu3]
  tnustop=[tnu2,tnu6]

  xlc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE_binned2.qdp',/phil)
;  xlc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE.qdp',/phil)
  spec=mrdfits('~/Desktop/GRB130427A/UL_specfits.fits',1)
  readcol,'~/Desktop/GRB130427A/NuSTAR/xrt.dat',xtime,xexp,xflux,xerr,format='(d,d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/uvot.dat',utime,uexp,uflux,uerr,ufilt,format='(d,d,d,d,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_3-10.txt',ntime1,nrate1,nrate1err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_10-30.txt',ntime2,nrate2,nrate2err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_30-79.txt',ntime3,nrate3,nrate3err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/lc/ctrate_lightcurve_3-79.txt',ntime4,nrate4,nrate4err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/lat/LAT_extrapolation_po.txt',ltime,lflux,lfluxerr0,lfluxerr1
  
;  w=where((xlc.tstart gt 103379.00 and xlc.tstop lt 160079.00) or
;  (xlc.tstart gt 411179.00 and xlc.tstop lt 495179.00),nw)
  w=where(xlc.time gt tnustart[0] and xlc.time lt tnustop[1],nw)
  xlc2=xlc[w]

  xlc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',0d)
  xlc=replicate(xlc,nw)
  xlc.time=xlc2.time
  xlc.tstart=xlc2.tstart
  xlc.tstop=xlc2.tstop
  xlc.ctr=xlc2.src_rate;/xlc2.pu_corr
  xlc.ctrerr=xlc2.src_rate_err;/xlc2.pu_corr
  xlc.exp=xlc2.exptime

  llc=create_struct('time',0d,'tstart',0d,'tstop',0d,'ctr',0d,'ctrerr',0d,'exp',0d,'fluxfact',0d,'fluxfacterr',0d,'flux',0d,'fluxerr',dblarr(2))
  llc=replicate(llc,n_elements(ltime))
  llc.time=ltime
  llc.flux=lflux
  llc.fluxerr[0]=lfluxerr0
  llc.fluxerr[1]=lfluxerr1
;  w=where(llc.time gt tnustart[0] and llc.time lt tnustop[2],nw)
  w=where(llc.time ge 1e5 and llc.time lt 5e5,nw)
  llc2=llc[w]
  llc=llc2
  mwrfits,llc,'~/Desktop/GRB130427A/NuSTAR/lat/latlc.fits',/create

  nexp=5000.
  cts1=nrate1*nexp
  cts2=nrate2*nexp
  cts3=nrate3*nexp
  cts4=nrate4*nexp

;  rb=[300,300,50,300]
  rb=[0.,0.,10.,0.]
  rebin_lc,ntime1,cts1,replicate(nexp/2.,n_elements(ntime1)),nrate1err*nexp,rb[0],lc=lc1
  rebin_lc,ntime2,cts2,replicate(nexp/2.,n_elements(ntime2)),nrate2err*nexp,rb[1],lc=lc2
  rebin_lc,ntime3,cts3,replicate(nexp/2.,n_elements(ntime3)),nrate3err*nexp,4,lc=lc3a
  rebin_lc,ntime3,cts3,replicate(nexp/2.,n_elements(ntime3)),nrate3err*nexp,4,lc=lc3b
  rebin_lc,ntime4,cts4,replicate(nexp/2.,n_elements(ntime4)),nrate4err*nexp,rb[3],lc=lc4

  w1=where(lc3a.time lt 2e5)
  w2=where(lc3b.time gt 2e5)
  concat_structs,lc3a[w1],lc3b[w2],lc3

  ;; use XSPEC fit with phind over intervals

  ;; xrt 3 intervals
  flux0=[-10.5625,-11.3692]
  flux0err=[0.021,0.044]
  rate0=[0.293699,8.742e-2]
  xfact=[2.057,1.12]

;  flux0=[-10.5642,-11.3207,-11.4358]
;  flux0err=[0.021,0.070,0.099]
;  rate0=[0.293703,9.98601e-2,7.60682e-2]
;  xfact=[2.057,1.0987,1.136]

  ;; nustar 3-10 all 3 intervals
  flux1=[-10.97,-11.65]
  flux1err=[0.0111,0.038]
  rate1=[0.146+0.1347,(2.44e-2+2.3979e-2+2.1574e-2+2.034e-2)/2.]
;  flux1=[-10.97,-11.62,-11.67]
;  flux1err=[0.011,0.034,0.048]
;  rate1=[0.14,2.38e-2,2.16e-2]*2

  ;; nustar 10-30 all 3 intervals
  flux2=[-10.93,-11.65]
  flux2err=[0.0125,0.042]
  rate2=[7.86e-2+7.054e-2,(1.228e-2+1.176e-2+1.086e-2+9.976e-3)/2.]

;  flux2=[-10.93,-11.62,-11.72]
;  flux2err=[0.013,0.0415,0.060]
;  rate2=[7.46e-2,1.195e-2,9.88e-3]*2
;  fluxfact2=[9.777E-11,9.621E-11,9.461E-11]

  ;; nustar 30-79 all 3 intervals
  flux3=[-10.935,-11.68]
  flux3err=[0.009,0.033]
  rate3=[6.33e-3+5.845e-3,(9.084e-4+8.915e-4+8.04e-4+7.565e-4)/2.]

;  flux3=[-10.92,-11.65,-11.77]
;  flux3err=[0.009,0.027,0.084]
;  rate3=[6.10e-3,8.85e-4,6.65e-4]*2
;  fluxfact3=[8.188E-10,8.058E-10,7.925E-10]

  ;; nustar 3-79 all 3 intervals
  flux4=[-10.46,-11.18]
  flux4err=[0.013,0.04]
  rate4=[0.232+0.211,(3.77e-2+3.675e-2+3.335e-2+3.1177e-2)/2.]
;;; LOOK INTO CHANGE IN RATE IN FIRST INT
;  flux4=[-10.46,-11.15,-11.24]
;  flux4err=[0.013,0.039,0.054]
;  rate4=[3.0e-1,5.46e-2,4.6e-2]*2
;  fluxfact4=[7.469E-11,6.974E-11,6.515E-11]

  ;;;; need to get intervals right
  for i=0,4 do begin ;;; loop over light curves
     case i of
        0: lc=xlc   ;; 0.3-10 xrt
        1: lc=lc1   ;; 3-10 nustar
        2: lc=lc2   ;; 10-30 nustar
        3: lc=lc3   ;; 30-79 nustar
        4: lc=lc4   ;; 3-79 nustar
     endcase
     tmp=execute('rate=rate'+ntostr(i))
     tmp=execute('flux=flux'+ntostr(i))
     tmp=execute('fluxerr=flux'+ntostr(i)+'err')
     for j=0,1 do begin ;;; loop over times
        w=where(lc.time ge tnustart[j] and lc.time lt tnustop[j],nw)
        f=10.^flux[j]
        ferr=10.^(flux[j]+fluxerr[j])-f
;        ferr=f-10.^(flux[j]-fluxerr[j])
        if i eq 0 then begin
           lc[w].fluxfact=(f/rate[j])/xfact[j];/xlc2[w].pu_corr
           lc[w].fluxfacterr=(ferr/rate[j])/xfact[j];/xlc2[w].pu_corr
        endif else begin
;           tmp=execute('ff=fluxfact'+ntostr(i))
;           tmp=execute('fferr=fluxfacterr+'+ntostr(i))
           lc[w].fluxfact=f/rate[j]
           lc[w].fluxfacterr=ferr/rate[j]
        endelse 
     endfor 
     lc.flux=lc.ctr*lc.fluxfact
     lc.fluxerr=sqrt((lc.ctrerr/lc.ctr)^2+(lc.fluxfacterr/lc.fluxfact)^2)*lc.flux
     q=where(lc.flux eq 0,nq)
     if nq gt 0 then begin 
        help,q
        lc[q].fluxfact=lc[0].fluxfact
        lc[q].fluxfacterr=lc[0].fluxfacterr
        lc[q].flux=lc[q].ctr*lc[q].fluxfact
        lc[q].fluxerr=sqrt((lc[q].ctrerr/lc[q].ctr)^2+(lc[q].fluxfacterr/lc[q].fluxfact)^2)*lc[q].flux
     endif 
;     lc=lc[q]

     case i of 
        0: xlc=lc
        1: lc1=lc
        2: lc2=lc
        3: lc3=lc
        4: lc4=lc
     endcase
;  q=where(lc.flux eq 0)
;  stop  

  endfor 


  begplot,name='~/Desktop/GRB130427A/NuSTAR/xrt_nustar_lc.ps',/color,font='helvetica',/land
  !p.charsize=1
;; !p.multi=[0,1,2]
;;  plot,[1e5,6e5],[1e-3,1],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Count Rate (counts s!U-1!N)',/nodata,xrange=[1e5,1e6],/xsty,/ysty
;;  oploterror,xlc.time,xlc.ctr,xlc.ctrerr,/nohat,psym=3,errcolor=!blue
;;  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].ctr,xlc[i].ctr],color=!blue
;;  oploterror,lc1.time,lc1.ctr,lc1.ctrerr,/nohat,psym=3,errcolor=!red
;;  for i=0,n_elements(lc1)-1 do oplot,[lc1[i].tstart,lc1[i].tstop],[lc1[i].ctr,lc1[i].ctr],color=!red
;;  oploterror,lc2.time,lc2.ctr,lc2.ctrerr,/nohat,psym=3,errcolor=!cyan
;;  for i=0,n_elements(lc2)-1 do oplot,[lc2[i].tstart,lc2[i].tstop],[lc2[i].ctr,lc2[i].ctr],color=!cyan
;;  oploterror,lc3.time,lc3.ctr,lc3.ctrerr,/nohat,psym=3,errcolor=!orange
;;  for i=0,n_elements(lc3)-1 do oplot,[lc3[i].tstart,lc3[i].tstop],[lc3[i].ctr,lc3[i].ctr],color=!orange
;;  oploterror,lc4.time,lc4.ctr,lc4.ctrerr,/nohat,psym=3,errcolor=!green
;;  for i=0,n_elements(lc4)-1 do oplot,[lc4[i].tstart,lc4[i].tstop],[lc4[i].ctr,lc4[i].ctr],color=!green


  plot,[1e5,6e5],[1e-12,4e-10],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',/nodata,xrange=[1e5,1e6],/xsty,/ysty
  x=[llc.time,reverse(llc.time),llc[0].time]
  y=[llc.flux+llc.fluxerr[1],reverse(llc.flux-llc.fluxerr[0]),llc[0].flux+llc[0].fluxerr[1]]
  oplot,x,y,color=!magenta
  polyfill,x,y,color=!magenta,/line_fill,orient=45
;  oploterror,llc.time,llc.flux,llc.fluxerr,/nohat,psym=3,errcolor=!magenta

  oploterror,xlc.time,xlc.flux,xlc.fluxerr,/nohat,psym=3,errcolor=!blue
  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].flux,xlc[i].flux],color=!blue
  oploterror,lc1.time,lc1.flux,lc1.fluxerr,/nohat,psym=3,errcolor=!red
  for i=0,n_elements(lc1)-1 do oplot,[lc1[i].tstart,lc1[i].tstop],[lc1[i].flux,lc1[i].flux],color=!red
  oploterror,lc2.time,lc2.flux,lc2.fluxerr,/nohat,psym=3,errcolor=!cyan
  for i=0,n_elements(lc2)-1 do oplot,[lc2[i].tstart,lc2[i].tstop],[lc2[i].flux,lc2[i].flux],color=!cyan
  oploterror,lc3.time,lc3.flux,lc3.fluxerr,/nohat,psym=3,errcolor=!orange
  for i=0,n_elements(lc3)-1 do oplot,[lc3[i].tstart,lc3[i].tstop],[lc3[i].flux,lc3[i].flux],color=!orange
  oploterror,lc4.time,lc4.flux,lc4.fluxerr,/nohat,psym=3,errcolor=!green
  for i=0,n_elements(lc4)-1 do oplot,[lc4[i].tstart,lc4[i].tstop],[lc4[i].flux,lc4[i].flux],color=!green

;  legend,['PL','BPL'],/right,/center,line=[2,1],box=0
  ;;;; lc fits
  ;; xrt
  model='pow'
  p=[1e-4,1.4]
  newp0=mpfitfun(model,xlc.time,xlc.flux,xlc.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror0)
;  lc_monte_pow,xlc,newp,pnames,chisq,dof,outperr,nsim=1000
  oplot,lc.time,pow(lc.time,newp0),color=!blue,line=2
  print,newp0,perror0,chisq/dof

  ;;; 3-10
  model='pow'
  p=[1e-4,1.4]
  newp1=mpfitfun(model,lc1.time,lc1.flux,lc1.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror1)  
  oplot,lc1.time,pow(lc1.time,newp1),color=!red,line=2
  print,newp1,perror1,chisq/dof
;;   model='bknpow'
;;   p=[1e-4,1.4,2e5,2.]
;;   newp=mpfitfun(model,lc1.time,lc1.ctr*c1,lc1.ctrerr*c1,p,bestnorm=chisq,dof=dof,/quiet)  
;;   t2=[lc1.time,newp[2]]
;;   t2=t2[sort(t2)]
;;   oplot,t2,bknpow(t2,newp),color=!red,line=1
;;   print,newp,chisq/dof


  ;; 10-30
  model='pow'
  p=[1e-4,1.2]
  newp2=mpfitfun(model,lc2.time,lc2.flux,lc2.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror2)  
  oplot,lc2.time,pow(lc2.time,newp2),color=!cyan,line=2
  print,newp2,perror2,chisq/dof

;;   model='bknpow'
;;   p=[1e-4,1.4,2e5,2.]
;;   newp=mpfitfun(model,lc2.time,lc2.ctr*c2,lc2.ctrerr*c2,p,bestnorm=chisq,dof=dof,/quiet)  
;;   t2=[lc2.time,newp[2]]
;;   t2=t2[sort(t2)]
;;   oplot,t2,bknpow(t2,newp),color=!cyan,line=1
;;   print,newp,chisq/dof

  ;; 30-79
  model='pow'
  p=[1e-4,1.4]
  newp3=mpfitfun(model,lc3.time,lc3.flux,lc3.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror3)  
  oplot,lc3.time,pow(lc3.time,newp3),color=!orange,line=2
  print,newp3,perror3,chisq/dof
;  model='bknpow'
;  p=[1e-4,1.4,2e5,2.]
;  newp=mpfitfun(model,lc3.time,lc3.ctr*c3,lc3.ctrerr*c3,p,bestnorm=chisq,dof=dof,/quiet)  
;  t2=[lc3.time,newp[2]]
;  t2=t2[sort(t2)]
;  oplot,t2,bknpow(t2,newp),color=!orange,line=1
;  print,newp,chisq/dof


  ;; 3-79
  model='pow'
  p=[1e-4,1.4]
  newp4=mpfitfun(model,lc4.time,lc4.flux,lc4.fluxerr,p,bestnorm=chisq,dof=dof,/quiet,perror=perror4)  
  oplot,lc4.time,pow(lc4.time,newp4),color=!green,line=2
  print,newp4,perror4,chisq/dof


  ;;; LAT
  
  leflux=[3.08e-10,3.43e-10]
  tle=[mean([tnu1,tnu2]),mean([tnu3,tnu6])]
  plotsym,1,3
  oplot,tle,leflux,psym=8,color=!magenta
  oplot,[tnu1,tnu2],[leflux[0],leflux[0]],color=!magenta
  oplot,[tnu3,tnu6],[leflux[1],leflux[1]],color=!magenta


  a=!tsym.alpha
  pm=!tsym.plusminus
  legend,['XRT 0.3-10 keV ('+a+'='+ntostr(newp0[1],4)+pm+ntostr(perror0[1]*1.4,4)+')',$
          'NuSTAR 3-10 keV ('+a+'='+ntostr(newp1[1],4)+pm+ntostr(perror1[1]*1.4,4)+')',$
          'NuSTAR 10-30 keV ('+a+'='+ntostr(newp2[1],4)+pm+ntostr(perror2[1]*1.4,4)+')',$
          'NuSTAR 30-79 keV ('+a+'='+ntostr(newp3[1],4)+pm+ntostr(perror3[1]*1.4,4)+')',$
          'NuSTAR 3-79 keV ('+a+'='+ntostr(newp4[1],4)+pm+ntostr(perror4[1]*1.4,4)+')',$
          'LAT 100 MeV-100 GeV ('+a+'=1.16'+pm+'0.06)'],$
         textcolor=[!blue,!red,!cyan,!orange,!green,!magenta],box=0,/bottom,/left,charsize=1.2

  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Desktop/GRB130427A/NuSTAR/xrt_nustar_lc.ps ~/Desktop/GRB130427A/NuSTAR/xrt_nustar_lc.pdf'

stop
;  plot,[1e5,6e5],[1e-6,1],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux Density (mJy)',/nodata,xrange=[1e5,6e5],/xsty

;  oploterror,xtime,xflux,xexp/2.,xerr,/nohat,psym=3,color=!blue,errcolor=!blue
;  oploterror,ntime1,nflux1,replicate(250.,n_elements(ntime1)),nflux1err,/nohat,psym=3,errcolor=!grey80
;  oploterror,ntime2,nflux2,replicate(250.,n_elements(ntime2)),nflux2err,/nohat,psym=3,errcolor=!grey50
;  oploterror,ntime3,nflux3,replicate(250.,n_elements(ntime3)),nflux3err,/nohat,psym=3,errcolor=!grey30



return
end 

pro lcs,xtime,xexp,xflux,xerr,utime,uexp,uflux,uerr,ufilt,ntime1,nfd1,nfd1err,ntime2,nfd2,nfd2err,ntime3,nfd3,nfd3err;,xtime2,xexp2,xflux2,xerr2

  plot,[1e5,6e5],[1e-6,1],/xlog,/ylog,xtitle='Time since GBM Trigger (s)',ytitle='Flux Density (mJy)',/nodata,xrange=[1e5,6e5],/xsty
  oploterror,xtime,xflux,xexp/2.,xerr,/nohat,psym=3,color=!blue,errcolor=!blue
;  oploterror,xtime2,xflux2,xexp2/2.,xerr2,/nohat,psym=3,color=!blue,errcolor=!blue

  oploterror,ntime1,nfd1,replicate(250.,n_elements(ntime1)),nfd1err,/nohat,psym=3,errcolor=!grey80
  oploterror,ntime2,nfd2,replicate(250.,n_elements(ntime2)),nfd2err,/nohat,psym=3,errcolor=!grey50
;  oploterror,ntime3,nfd3,nfd3err,/nohat,psym=3,errcolor=!grey30

;  oploterror,utime,uflux,uexp/2.,uerr,/nohat,psym=3,color=!red,errcolor=!red

  color=[!cyan,!royalblue,!darkgreen,!green,!pink,!salmon,!purple,!violet,!p.color,!orange,!orangered,!sienna,!firebrick,!darkred,!yellow]
  filter=['W2','M2','W1','U','B','V','r','i','z','j','h','k']
  plotsym,0,1,/fill
  for i=0,n_elements(filter)-1 do begin
     w=where(filter[i] eq strtrim(ufilt,2),nw)
     plots,utime[w],uflux[w],color=color[i],psym=8
;     if nw gt 1 then oplot,utime[w],uflux[w],color=color[i]
     for j=0,nw-1 do begin
        oplot,[utime[w[j]]-uexp[w[j]]/2.,utime[w[j]]+uexp[w[j]]/2.],[uflux[w[j]],uflux[w[j]]],color=color[i]
        oplot,[utime[w[j]],utime[w[j]]],[uflux[w[j]]-uerr[w[j]],uflux[w[j]]+uerr[w[j]]],color=color[i]
     endfor 
  endfor 

  stop
;;; bin 500 s in nustar

  return
end 

pro seds

  lc=lcout2fits(pcfile='~/Desktop/GRB130427A/PCCURVE_binned.qdp',/phil)
  spec=mrdfits('~/Desktop/GRB130427A/UL_specfits.fits',1)
  readcol,'~/Desktop/GRB130427A/NuSTAR/xrt.dat',xtime,xexp,xflux,xerr,format='(d,d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/uvot.dat',utime,uexp,uflux,uerr,ufilt,format='(d,d,d,d,a)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_3-10.txt',ntime1,nflux1,nflux1err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_10-30.txt',ntime2,nflux2,nflux2err,format='(d,d,d)'
  readcol,'~/Desktop/GRB130427A/NuSTAR/nustar/products/flux_lightcurve_30-79.txt',ntime3,nflux3,nflux3err,format='(d,d,d)'


  filter=['V','B','U','W1','M2','W2','white','r','i','z','j','h','k','Rc','Ic']
  lam_eff1=[5402,4329,3501,2634,2231,2030,3471]*1d-8 ;; cm
  lam_eff2=[0.64,0.79,0.91,1.26,1.6,2.22,0.64,0.79]*1d-4 ;; cm
  lam_eff=[lam_eff1,lam_eff2]
  ueng=dblarr(n_elements(utime))
  h=4.1357d-15; eV s
  c=3e8; cm/s
  for i=0,n_elements(utime)-1 do begin
     w=where(filter eq strtrim(ufilt[i],2))
     ueng[i]=h*c/lam_eff[w];*1d-3
  endfor 

;  uflux=uflux*1e-3
;  uerr=uerr*1e-3
  erg2jy = 1d23/2.42d17
  xflux=xflux*erg2jy*1e3
  xerr=xerr*erg2jy*1e3
  xeng=replicate(1e3,n_elements(xtime))

;  w=where(lc.tstart gt 1e5 and lc.tstop lt 1e6)
  w=where((lc.tstart gt 103379.00 and lc.tstop lt 160079.00) or (lc.tstart gt 411179.00 and lc.tstop lt 495179.00))
  lc=lc[w]
  xflux2=lc.src_rate*spec[1].unabs_cfratio ;;; count rate to flux
  xerr2=lc.src_rate_err*spec[1].unabs_cfratio
  xfd=flux2jy(xflux2,spec[1].phind,eeff=1.) ;; flux to flux density @ 1 keV
  xfderr=xerr2/xflux2*xfd
  xflux2=xfd*1e3 ;;; Jy to mJy
  xerr2=xfderr*1e3
  xexp2=(lc.tstop-lc.tstart)
  xtime2=lc.time

  nfd1=flux2jy(nflux1,1.8,eeff=6.5,low=3,high=10)*1e3
  nfd2=flux2jy(nflux2,1.8,eeff=15,low=10,high=30)*1e3
  nfd3=flux2jy(nflux3,1.8,eeff=54.5,low=30,high=79)*1e3
  nfd1err=nflux1err/nflux1*nfd1
  nfd2err=nflux2err/nflux2*nfd2
  nfd3err=nflux3err/nflux3*nfd3
stop
  lcs,xtime2,xexp2,xflux2,xerr2,utime,uexp,uflux,uerr,ufilt,ntime1,nfd1,nfd1err,ntime2,nfd2,nfd2err,ntime3,nfd3,nfd3err

stop

  !p.multi=[0,1,2]
  plot,[1e-2,1e5],[1e-8,10],/nodata,xtitle='Energy (eV)',ytitle='Flux Density (Jy)',/xlog,/ylog
  w1=where(utime lt 1.5e5)
  wx1=where(xtime lt 1.5e5)
  oploterror,ueng[w1],uflux[w1],uerr[w1],/nohat,psym=5,color=!red
  oploterror,xeng[wx1],xflux[wx1],xerr[wx1],/nohat,psym=5,color=!blue
  
  plot,[1e-2,1e5],[1e-8,10],/nodata,xtitle='Energy (eV)',ytitle='Flux Density (Jy)',/xlog,/ylog
  w2=where(utime gt 1.5e5)
  wx2=where(xtime gt 1.5e5)
  oploterror,ueng[w2],uflux[w2],uerr[w2],/nohat,psym=5,color=!red
  oploterror,xeng[wx2],xflux[wx2],xerr[wx2],/nohat,psym=5,color=!blue
  !p.multi=0

  stop
return
end 

pro grb130427a

  ;;; nustar predictions

  cd,'~/Desktop/GRB130427A/'
  begplot,name='XRT_LC.eps',/color,font='helvetica',/land
;  !p.multi=[0,1,3]
;  plot_like_qdp,yrange=[1e-1,1e5],/phil,/ysty,ytitle='XRT Count Rate (0.3-10 keV) (cts/s)'
;  plot_like_qdp,/phil,ytitle='XRT Flux (0.3-10 keV) (erg/cm2/s)',/useflux,flux=4e-11
;  plot_like_qdp,/phil,ytitle='Predicted NuSTAR (3-80 keV) (cts/s)',/useflux,flux=0.88,yrange=[1e-1,1e5],/ysty

  lc=lcout2fits(/phil)
  nu3_80=8.875e-01
  nu3_10=6.327E-01
  nu10_20=1.86e-1
  nu20_40=5.51e-2
  nu40_80=1.286e-2

;2013-04-28T12:31:07 - 2013-04-29T04:16:07 (1.048487634999924E+08 -
;1.049044494249942E+08)

;2013-05-02T02:01:07 - 2013-05-02T14:01:07 (1.051574488800000E+08 -
;1.051992000000000E+08)

;2013-05-02T18:06:07 - 2013-05-03T01:21:07 (1.052159018200000E+08 -
;1.052399999999999E+08)

  nu1=date2met('2013:118:12:31:07')
  nu2=date2met('2013:119:04:16:07')

  nu3=date2met('2013:122:02:01:07')
  nu4=date2met('2013:122:14:01:07')
  
  nu5=date2met('2013:122:18:06:07')
  nu6=date2met('2013:123:01:21:07')

  battrig=388741688d
  tnu1=nu1-battrig
  tnu2=nu2-battrig
  tnu3=nu3-battrig
  tnu4=nu4-battrig
  tnu5=nu5-battrig
  tnu6=nu6-battrig

  ffact=4e-11

  plot,[100,1e6],[1e-3,1e5],xrange=[1e4,2e6],yrange=[1e-4,10],/xlog,/ylog,/nodata,xtitle='Time since BAT trigger',ytitle='XRT scaled to NuSTAR Count Rate (cts/s)',/xsty
  polyfill,[tnu1,tnu2,tnu2,tnu1,tnu1],[1e-4,1e-4,10,10,1e-4],color=!grey70
  polyfill,[tnu3,tnu4,tnu4,tnu3,tnu3],[1e-4,1e-4,10,10,1e-4],color=!grey70
  polyfill,[tnu5,tnu6,tnu6,tnu5,tnu5],[1e-4,1e-4,10,10,1e-4],color=!grey70
;  axis,xaxis=0
;  axis,xaxis=1,xrange=[1e4,1e6]
  
  nu=[nu3_10,nu10_20,nu20_40,nu40_80,nu3_80]
  color=[!red,!blue,!green,!orange,!cyan]
  for j=0,4 do begin

     oploterror,lc.time,lc.src_rate*nu[j],lc.src_rate_err*nu[j],psym=3,/nohat,color=color[j]
     for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate*nu[j],lc[i].src_rate*nu[j]],color=color[j]
  endfor 

  legend,['NuSTAR Obs','3-80 keV','3-10 keV','10-20 keV','20-40 keV','40-80 keV'],textcolor=[!grey70,color[4],color[0:3]],box=0,/top,/right

  endplot

stop

return
end 
