@fit_functions
@fit_functions_flares
pro shb_plot

  grb=mrdfits('~/Swift/swift_grb_properties.fits',1)

  w=where(grb.t90 le 2. and grb.t90 gt 0. and grb.grb ne 'GRB100628A' and grb.grb ne 'GRB140311B')
  grb=grb[w]

;  begplot,name='~/Lobster/TAO_2016/sgrb_plot.eps',/land,/color,font='helvetica'
  !x.margin=[5,1]
  !y.margin=[2,0]
  xrange=[10,1e6]
  yrange=[1e-13,1e-5]
  plot,[10,1e6],[1e-13,1e-5],/xlog,/ylog,/nodata,xtitle='Time (s)',ytitle='0.3-5.0 keV Flux (erg cm!U-2!N s!U-1!N)',xrange=xrange,yrange=yrange,/xsty,/ysty,charsize=2,yticks=8,yminor=9,title='X-ray afterglows of sGRBs scaled to 440 Mpc' ;,title='X-ray afterglows real flux'
  t=15.*60.
  t2=t+40.*60.
  thresh=1e-11
  mthresh=25./8*3.5e-10

  readcol,'~/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',time, bcount, mcount, grbflux
  lobtime=time
  lobflux=grbflux

  f10=interpol(lobflux,lobtime,10.)
  f50=interpol(lobflux,lobtime,50.)
  f500=interpol(lobflux,lobtime,500.)
  f2000=interpol(lobflux,lobtime,2000.)

  lobcounts=lobflux/2.5e-9*time
  c=248.
  g=3.
  R90= c*(mcount^2/(mcount+g*bcount))^(-0.5)
  tp=[10.,500.,2000.]
  r90p=interpol(r90,time,tp)/60.

  nl=n_elements(lobtime)

  axis,xrange=xrange,/xsty,xtickname=replicate(' ',7)
  axis,xaxis=1,xrange=xrange,/xsty,xtickname=replicate(' ',7)
  axis,yaxis=0,yrange=yrange,/ysty,ytickname=replicate(' ',9),yticks=8,yminor=9
  mpc2cm=3.08568025d24
  dist200=440*mpc2cm
  eng=dindgen(98)/10.+0.3
  de=0.1
  plotsym,1,3
  detect=intarr(n_elements(grb))
  mdetect=intarr(n_elements(grb))
  f100=fltarr(n_elements(grb))
  f1000=fltarr(n_elements(grb))
  mos=strarr(n_elements(grb))
  s=findgen(100)/10.+1.
  times=[s*10,s*100,s*1000,s*1e4,s*1e5,s*1e6]
  for i=0,n_elements(grb)-1 do begin

     w=where(times ge grb[i].tstart and times le grb[i].tlastdet,nw)
     time=times[w]
     f=call_function(strtrim(grb[i].model,2),time,grb[i].p)
     z=grb[i].z
     color=!p.color
     if z le 0. then begin 
        z=0.7
        color=!p.color;!grey70
     endif 
     dist=lumdist(z,h0=71,lambda=0.73,omega_m=0.27)
     dist=dist*mpc2cm
     z1=1.+z

        ;;; need to convert from 0.3-10 to 0.3-5 keV
     conv=total(pow(eng[0:47],[1.,grb[i].phind])*de)/total(pow(eng,[1.,grb[i].phind])*de)
     
     r=grb[i].cfratio*conv*dist^2/dist200^2/kcorr(z,[1.,grb[i].phind],/pow)

     if nw gt 1 then begin
        if f[0]*r gt 1e-11 then begin 
           oplot,time/z1,f*r,color=color
        endif 
        mo=strtrim(grb[i].model,2) ;mo
        f100[i]=call_function(mo,100.,grb[i].p)*r
        f1000[i]=call_function(mo,1000.,grb[i].p)*r
     endif 

     w=where(time gt t and f*r gt thresh,nw)
     if nw gt 0 then detect[i]=1
     w=where(time gt 45.*60. and f*r gt mthresh,nw)
     if nw gt 0 then mdetect[i]=1
  endfor

  ;;; add bursts with only 1 detection
;;   bursts=['GRB050813','GRB060502B','GRB061217','GRB070729','GRB100206A','GRB101224A','GRB130822A','GRB140320A']
;;   plotsym,0,1,/fill
;;   for i=0,n_elements(bursts)-1 do begin
;;      lc=lcout2fits(dir='~/GRBs/'+bursts[i])
;;      z=0.7
;;      dist=lumdist(z,h0=71,lambda=0.73,omega_m=0.27)
;;      dist=dist*mpc2cm
;;      z1=1.+z
;;      conv=total(pow(eng[0:47],[1.,2.])*de)/total(pow(eng,[1.,2.])*de)
;;      r=4e-11*conv*dist^2/dist200^2/kcorr(z,[1.,2.],/pow)
;; ;     plots,lc[0].time/z1,lc[0].src_rate*r,color=!grey70,psym=8
;; ;     oplot,[lc[0].tstart,lc[0].tstop]/z1,[lc[0].src_rate,lc[0].src_rate]*r,color=!grey70
;; ;     oplot,[lc[0].time,lc[0].time]/z1,[lc[0].src_rate-lc[0].src_rate_err,lc[0].src_rate+lc[0].src_rate_err]*r,color=!grey70
;;      print,bursts[i],lc[0].src_rate*r
;;   endfor 

  !p.charthick=1
  !p.charsize=2
  c=!p.color
  c=!orange
  
  oplot,[11,11],[yrange[0],6e-6],thick=20,color=c
  xyouts,12.,3e-6,'Start of WFI scan from GTM trigger',color=c

  c=!blue
  oplot,[3.*60,3.*60],[yrange[0],2e-7],thick=20,color=c
  xyouts,3.*60+60.,1e-7,'LIGO skymap available',color=c

  c=!cyan
  oplot,[45.*60,45.*60],[yrange[0],2e-8],thick=20,color=c
  xyouts,45.*60+1000.,1e-8,'Max Time to Observation',color=c

  !p.charsize=1.8
  c=!red
  oplot,[10,1e6],[f10,f10],thick=15,color=c,line=5
  xyouts,7e3,f10*1.7,'R!L90!N<'+numdec(r90p[0],1)+"'     "+'10 s Sensitivity',color=c

  oplot,[500,1e6],[f500,f500],thick=15,color=c,line=5
  xyouts,7e3,f500*1.7,'R!L90!N<'+numdec(r90p[1],1)+"'   "+'500 s Sensitivity',color=c

  oplot,[2000,1e6],[f2000,f2000],thick=15,color=c,line=5
  xyouts,7e3,f2000*0.4,'R!L90!N<'+numdec(r90p[2],1)+"' "+'2000 s Sensitivity',color=c
  
;  endplot
;  spawn,'ps2pdf ~/Lobster/TAO_2016/sgrb_plot.eps ~/Lobster/TAO_2016/sgrb_plot.pdf'
  help,detect
  nd=n_elements(detect)
  w0=where(detect eq 0,nw0)
  w1=where(detect eq 1,nw1)
  w2=where(detect eq 3,nw2)
  print,nd,nw0,nw1,nw2
  n=nd-nw2
  print,n,nw1*1./n
  
  stop

  return
end

pro transient_plot

  readcol,'~/Lobster/Lobster_2010/Proposal/soderberg.csv',time,lum
  time=[time[0:19],time[24:*]]
  lum=[lum[0:19],lum[24:*]]
  n=n_elements(time)/4

  trans=create_struct('type1','','type2','','type3','','dist1',0d,'dist2',0d,'lum',dblarr(4),$
                      'time',dblarr(4),'flux',dblarr(4),'color','','tcolor','',$
                      'xoff',0.,'yoff',0.,'ang',0.)
  
  n=13
  trans=replicate(trans,n)
  trans.tcolor='black'
  i=0
  trans[i].type1='LGRBs'
  trans[i].dist1=lumdist(3,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].lum=[51,45,43,48]
  trans[i].flux=[1d-6,5d-10,5d-12,1d-9]
  trans[i].time=[1,1d4,1d4,1]
  trans[i].color='magenta'
  trans[i].ang=-1
  trans[i].xoff=0.
  trans[i].yoff=5e2

  i=1
  trans[i].type1='sGRBs'; (GWs)'
  trans[i].type2='(GWs)'
  trans[i].dist1=lumdist(0.5,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].flux=[1e-7,1d-11,1d-12,1e-9]
  trans[i].flux=[8e-7,1d-11,1d-12,5e-9]
  trans[i].time=[0.1,1d4,1d4,0.1]
  trans[i].color='sea green'
  trans[i].ang=-1
  trans[i].xoff=0.05
  trans[i].yoff=4e3

;  i=2
;  trans[i].type1='Sub-Luminous GRBs'
;  trans[i].type2='       GRBs'
;  trans[i].dist1=lumdist(0.1,h=72,omega_m=0.27,lambda=0.73)*1e6
;  trans[i].lum=lum[24:27]
;  trans[i].time=time[24:27]
;  trans[i].color=!green
;  trans[i].yoff=0.02
;  trans[i].xoff=-10
  
;  i=3
;  trans[i].type1='Magnetar'
;  trans[i].type2='Flares'
;  trans[i].dist1=15e3 ;;pc
;  trans[i].flux=[1d-6,1d-6,1d-8,1d-8]
;  trans[i].time=[0.05,0.1,0.1,0.05]
;  trans[i].yoff=1.
;  trans[i].xoff=-0.02
;  trans[i].color=!cyan

  i=8
  trans[i].type1='Thermonuclear Bursts'
  trans[i].dist1=10e3 ;;pc
  trans[i].lum=[39,39,38,38]
  trans[i].time=[3,1e4,1e4,3]
  trans[i].xoff=5
  trans[i].yoff=0.5
  trans[i].color='pink'

  i=3
  trans[i].type1='  Tidal'
  trans[i].type2='Disruptions'
;  trans[i].type3='  tion'
  trans[i].dist1=200e6 ;;pc
  trans[i].flux=[2.1e-10,5e-11,5.2e-13,2.1e-12]
  trans[i].time=[100,1e6,1e6,100];time[28:31]
  trans[i].color='yellow'
  trans[i].yoff=0.8
  trans[i].xoff=2e4
  trans[i].ang=-0.01

  i=9
  trans[i].type1='SN Ibc/II Breakout'
  trans[i].dist1=50e6 ;;pc
  trans[i].lum=[44,45,44.5,43.5];lum[20:23]
  trans[i].time=time[20:23]
  trans[i].color='turquoise'
  trans[i].ang=12
  trans[i].yoff=0.08
  trans[i].xoff=2
  
  i=4
  trans[i].type1='Flare'
  trans[i].type2='Stars'
  trans[i].dist1=100. ;;pc
  trans[i].flux=[4d-10,4d-10,1d-12,1d-12]
  trans[i].time=time[40:43]
  trans[i].color='red'
  trans[i].yoff=0.03
  trans[i].xoff=60

;  i=8
;  trans[i].type1='SFXT'
;  trans[i].dist1=2e3
;  trans[i].lum=[35,35,33,33]
;  trans[i].time=[1e3,1e4,1e4,1e3]
;  trans[i].yoff=0.05
;  trans[i].color=!dodgerblue

  i=6
  trans[i].type1=' SN Ia'
  trans[i].type2='Breakout'
  trans[i].dist1=50e6 ;;pc
  trans[i].lum=[44,44,43,43]
  trans[i].time=[1e-1,5e3,5e3,1e-1];[100,2d3,2d3,100]
  trans[i].yoff=0.8
  trans[i].xoff=0.1
  trans[i].color='purple'
  trans[i].tcolor='white'

  i=10
  trans[i].type1='Classical Novae'
  trans[i].dist1=3e3 ;;pc
  trans[i].lum=lum[32:35]
  trans[i].time=time[32:35]
  trans[i].color='orange'
  trans[i].ang=18
  trans[i].xoff=5e3
  trans[i].yoff=0.008

;  i=11
;  trans[i].type1='AGN'
;  trans[i].dist1=lumdist(0.01,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].dist2=lumdist(0.05,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].lum=[43,43,41,41]
;  trans[i].time=[1e4,6e5,6e5,1e4]
;  trans[i].color=!dodgerblue
;  trans[i].xoff=1e4
;  trans[i].yoff=5
;  trans[i].ang=-0.01

;  i=12
;  trans[i].type1='Blazars'
;  trans[i].dist1=lumdist(0.1,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].dist2=lumdist(1,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].lum=[46.5,46.5,44.5,44.5]
;;  trans[i].flux=[1.1e-10,1.1e-12,6.2e-14,6.2e-12]
;  trans[i].time=[7e4,6e5,6e5,7e4]
;  trans[i].color=!blue
;  trans[i].xoff=0
;  trans[i].yoff=1
;  trans[i].ang=-0.01
  
  
;; blazars, agn, long grbs, short grbs
;  trans[11].type1='Long GRBs'
;  trans[11].dist=lumdist(3.)*1e3
;  trans[11].lum=
;  trans[11].time=[1e-2,1e3,1e3,1e-2,1e-2]
  pc2cm=3.08568025d18  
  trans.dist1=trans.dist1*pc2cm
  trans.dist2=trans.dist2*pc2cm

  ;Integration time    Flux in 0.3-6 keV [erg/cm2/s]
 
;  lobtime=[1,2,5,10,20,50,100,200,400]
;  lobflux=[0.7347E-08,0.3847E-08,0.2304E-08,0.1310E-08,0.6966E-09,0.3351E-09,0.2234E-09,0.1396E-09,0.9077E-10]
;  lobflux=[7.3e-9,3.5e-9,1.5e-9,1.1e-9,6.2e-10,2.4e-10,1.5e-10,7.0e-11,4.9e-11]
;  lobtime=[10,20,50,100,200,400,1000,2000,1e4,2e4,4e4,8e4]
;  lobflux=[9.29e-10,4.64e-10,1.86e-10,9.29e-11,4.64e-11,2.61e-11,1.33e-11,7.71e-12,2.7e-12,1.88e-12,1.27e-12,1e-12];8.47e-13] ;4sigma
;  readcol,'~/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',time, bcount, mcount, grbflux
  readcol,'~/Lobster/TAO_2016/simulations/Ptak/tau_flux_limits_2018_prob1e-10.csv',time, bcount, mcount, grbflux,delim=',',skipline=1

  readcol,'/Users/jracusin/Lobster/TAP/XRI_sensitivity_15arcmin.dat',starx_time,starx_flux

  t1=[0.1,0.5,1,5]
  xri_time=[t1,starx_time]
  xri_flux=[starx_flux[0]/(t1/starx_time[0]),starx_flux]
;stop
;  readcol,'~/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',time2,bcount2,mcount2,grbflux2

  lobtime=time
  lobflux=grbflux
  flux=grbflux

  time2=[time,1e7]
  flux2=[flux,3d-13]

  time=[time,1e7]
  flux=[flux,1d-12]
  w=where(flux le 1d-12)
  flux[w]=1d-12


  nl=n_elements(lobtime)
;  !x.margin=[14,0]
  xrange=[1e-1,1e7]
  p=plot([1e-1,1e7],[1e-14,1e-5],/nodata,xrange=xrange,yrange=[1e-15,1e-5],xtitle='Timescale (s)',ytitle='0.4-4 keV X-ray Flux (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,xmajor=9,ymajor=11,xminor=9,yminor=9);,xtickname=replicate(' ',9),yticks=9)
;  p4=plot(time,flux2,linestyle=2,/over,/current,thick=3);,color=!grey50
;  p.save,'~/Lobster/TAO_2016/transient_plot_justcurve.pdf'

  x=0
  square=intarr(n)
  square[*]=1
  w=where(trans.ang ne 0)
  square[w]=0
  
;;  t1=[0.01,0.02,0.05,0.1,0.2,0.5]
;  t1=[0.1,0.2,0.5,1.]
;  nt1=n_elements(t1)
;  t2=[4e5,1e6,2e6,1e7]
;  nt2=n_elements(t2)
;  time=[t1,lobtime,t2]
;  time3=[t1,time2,t2]
;  flux=[lobflux[0]/(t1/lobtime[0]),lobflux,lobflux[nl-1]/sqrt(t2[0]/lobtime[nl-1]),replicate(1e-12,3)];,lobflux[nl-1]/sqrt(t2/lobtime[nl-1])]
;  flux2=[lobflux[0]/(t1/lobtime[0]),lobflux,lobflux[nl-1]/sqrt(t2/lobtime[nl-1])]
;  flux3=[grbflux2[0]/(t1/time2[0]),grbflux2,grbflux2[nl-1]/sqrt(t2/time2[nl-1])]

;  flux2=10^interpol(alog10(lobflux),alog10(lobtime),alog10(time))
;  oplot,lobtime,lobflux,psym=1
  p2=polygon([time,1e7,xrange[0],time[0]],[flux,1e-15,1e-15,flux[0]],color='grey',/data,fill_color='grey',transparency=50)
;  p2=polygon([xri_time,1e7,xrange[0],xri_time[0]],[xri_flux,1e-15,1e-15,xri_flux[0]],color='dim grey',/data,fill_color='dim grey',transparency=50)

;  t2=text(1e4,1e-6,'WFI Detectability',/data)

  asym=[0,2,1,3]
  for i=0,n-1 do begin
     if square[i] eq 1 then begin 
        xsq=[0,1,1,0,0] 
        ysq=[0,0,2,2,0] 
     endif else begin 
        xsq=[0,1,2,3,0]
        ysq=[0,1,2,3,0]
     endelse 
     t=trans[i].time[xsq]
     if trans[i].flux[0] eq 0 then trans[i].flux=10^trans[i].lum/(4.*!pi*trans[i].dist1^2.)
     if trans[i].dist2 ne 0 then begin 
        trans[i].flux[0]=10^trans[i].lum[0]/(4.*!pi*trans[i].dist1^2.)
        trans[i].flux[1]=10^trans[i].lum[2]/(4.*!pi*trans[i].dist1^2.)
        trans[i].flux[2]=10^trans[i].lum[2]/(4.*!pi*trans[i].dist2^2.)
        trans[i].flux[3]=10^trans[i].lum[0]/(4.*!pi*trans[i].dist2^2.)
     endif 

     l=trans[i].flux[ysq]
     p3=polygon(t,l,color=trans[i].color,/data,fill_color=trans[i].color,transparency=30)
     xx=t[0]*1.1;(t[1]-t[0])/8.+t[0]
     yy=(l[2]-l[1])/2.+l[1]
     print,xx,yy
     t3=text(xx+trans[i].xoff,yy*trans[i].yoff,trans[i].type1,color=trans[i].tcolor,orientation=trans[i].ang,/data)
     t4=text(xx+trans[i].xoff,yy/3.*trans[i].yoff,trans[i].type2,color=trans[i].tcolor,orientation=trans[i].ang,/data)
     t5=text(xx+trans[i].xoff,yy/6.*trans[i].yoff,trans[i].type3,color=trans[i].tcolor,orientation=trans[i].ang,/data)
     x=x+4
  endfor
  
  i=5
  xx=trans[i].time[0]
  yy=(trans[i].flux[2]-trans[i].flux[1])/2.+trans[i].flux[1]
  t6=text(xx+trans[i].xoff,yy*trans[i].yoff,trans[i].type1,color='black',orientation=trans[i].ang,/data)
  t7=text(xx+trans[i].xoff,yy/3.*trans[i].yoff,trans[i].type2,color='black',orientation=trans[i].ang,/data)
  w=where(time le 4e4)
  p4=plot(time[w],flux[w],linestyle=2,/over,/current,thick=3);,color=!grey50
  p4=plot(time2,flux2,linestyle=2,/over,/current,thick=3);,color='green');,color=!grey50
  p4=plot([6e4,1e7],[1e-12,1e-12],linestyle=1,/over,/current,thick=3);,color=!grey50

;  p5=plot(xri_time,xri_flux,linestyle=3,/over,/current,thick=3)
;  p5=plot([6e5,1e7],[1e-12,1e-12],linestyle=1,thick=4,/over,/current)
;  p6=plot([1e-2,1e7],[1e-14,1e-14],/over,/current)
;  p7=plot([1e7,1e7],[1e-14,1e-2],/over,/current)


;  t8=text(5e2,1e-13,'WFI (0.3-5 keV) Sensitivity',/data)
  t8=text(5e2,1e-13,'WFI 4'+!tsym.sigma+' Sensitivity',/data)
  a=arrow([5e3,5e3],[8e-13,5e-12],/data,thick=3)
;  t8=text(0.2,5e-15,'XRT (0.5-2 keV) Sensitivity',/data)
;  a=arrow([10,10],[1.5e-14,3e-13],/data,thick=3)
  t9=text(5e5,3e-12,'Confusion',/data)
  t9=text(5e5,1.4e-12,'    Limit',/data)
  t10=text(1e3,3e-14,'Minimum Integration Time = 2 s',/data)

  p.save,'~/Lobster/TAO_2016/transient_plot.png'
  p.save,'~/Lobster/TAO_2016/transient_plot.eps'
  p.save,'~/Lobster/TAO_2016/transient_plot.pdf'
  p.close

return
end 

pro grbz_plot

  readcol,'~/Lobster/TAO_2016/redshift_accum_data_all_tao.txt',z,t0s,t500s,t1000s,t1500s,t2000s,t2500s,t3000s,t3500s,t4000s,t4500s,t5000s,All_detected_withoutGTM,intrinsic,GTM,All_detected_withGTM,comment='#',delim=' '

  zfact=0.33

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  w=where(g.z gt 0,n)
  g=g[w]

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
  yrange=[0.1,1e2]
;  plot,[0,14],yrange,/nodata,xtitle='Redshift z!L0!N',ytitle='z>z!L0!N GRB rate (yr !U-1!N)',/ylog,yrange=yrange
;  polyfill,[5,xrange[1],xrange[1],5,5],yrange[[0,0,1,1,0]],color=!yellow
;  oplot,x,y/swiftlen,psym=10
  p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/histogram,ytitle='Cumulative (> z) GRB rate [yr !U-1!N]',/ylog,ytickformat='loglabels',xtitle='Redshift z',font_size=14)
  p1=polygon([5,xrange[1],xrange[1],5,5],yrange[[0,0,1,1,0]],fill_color='light cyan',/data)
  p1=plot([5,5],yrange,/overplot)
  p2=plot(x,y/swiftlen,xrange=xrange,yrange=yrange,/histogram,/overplot)
  p3=plot(z,all_detected_withgtm*zfact,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='blue',sym_size=5,sym_filled=1,linestyle='none')
  t1=text(5.3,39,'Targets for JWST',/data,font_size=12)
  a1=arrow([5,7],[30,30],/data,thick=2)
  t2=text(1.9,6,'Swift GRBs with Measured Redshift',orientation=-39,/data,font_size=14)
  t3=text(3,25,'Predicted Redshifts for TAO-ISS GRBs',/data,color='blue',orientation=-35,font_size=14)
;  p4=plot(z,t0s*0.3,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='green',sym_size=5,sym_filled=1)
;  p5=plot(z,[t500s+t1000s+t1500s+t2000s+t2500s+t3000s+t3500s+t4000s+t4500s+t5000s]*0.3,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='orange',sym_size=5,sym_filled=1)
;  p6=plot(z,gtm*0.3,xrange=xrange,yrange=yrange,symbol='.',/overplot,color='red',sym_size=5,sym_filled=1)
;  t4=text(0.5,0.7,'prompt',color='green',/data)
;  t4=text(0.5,0.5,'afterglow',color='orange',/data)
;  t4=text(0.5,0.35,'GTM',color='red',/data)


  colprint,z,all_detected_withgtm,all_detected_withgtm*zfact,gtm

  p.save,'~/Lobster/TAO_2016/grbz_plot.eps',/transparent,/landscape
  p.save,'~/Lobster/TAO_2016/grbz_plot.png',/transparent,/landscape
  p.close
  ;;; add "Swift GRBs with Measured z"
  ;;; add "Targets for JWST" shaded region


  stop
  return
end 


pro sensitivity

  cd,'~/Lobster/TAO_2016/'
  readcol,'~/Lobster/2014_proposal/lobster_sensitivity_0.3_5_Ptak_45cm.dat',time,bcount,mcount,grbflux
  fovs=[29*29.,19.1*19.1]
  wficonfig=['30x30','20x20']
  t1=[0.1,0.15,0.2,0.3,0.4,0.5,0.7,1.]
;  t2=[4e5,1e6,2e6,1e7]
  t2=[2e4,4e4,7e4,9e4,1e5,1.3e5,1.7e5,2e5,2.5e5,3e5,3.5e5,4e5,5e5,8e5,1e6]
  nl=n_elements(time)
  time2=[t1,time,t2]
  flux=[grbflux[0]/(t1/time[0]),grbflux,grbflux[nl-1]/sqrt(t2/time[nl-1])]
;  w=where(time2 ge 3e5)
  w=where(flux le 1d-12)
  flux[w]=1d-12
  flux3=flux/1.5^2.  ;;; scaling the old sensitivity
  w=where(flux3 le 1d-12)
  flux3[w]=1d-12

  f=flux3[30]*(time2/time2[30])^(-0.75)

  begplot,name='sensitivity_trade.ps',/land,/color
  !x.margin=[4,1]
  plot,time2,flux,/xlog,/ylog,yrange=[1e-13,1e-7],charsize=2.,/xsty,/ysty,xtitle='Exposure Time (s)',ytitle='WFI Sensitivity (erg cm!U-2!N s!U-1!N)',xrange=[0.1,1e6],xticks=7,xtickformat='loglabels',xminor=9,yticks=6,yminor=9;,psym=1
  oplot,time2,flux3,color=!red,line=2
  oplot,time2,f,color=!green
  xyouts,20,1e-9,'30 cm focal length',/data,charsize=2.
  xyouts,1.5,9e-12,'45 cm focal length',/data,charsize=2.;,color=!red
  xyouts,2e4,5e-13,'Confusion Limit',/data,charsize=1.5
  endplot
  ps2pdf,'sensitivity_trade.ps'
stop
  return
end
pro wfi_sens_compare

  begplot,name='~/Lobster/TAO_2016/compare_tao.eps',/encap,font='helvetica',/color,/land

  !x.margin=[6,2]
  xrange=[2016,2026]
  plot,xrange,[1e-12,1e-9],/nodata,xtitle='Year',/ylog,/xsty,/ysty,charsize=2,yrange=[5e-12,1e-9],title='Full Sky Sensitivity in 1 day',ytitle='0.3-5.0 keV Sensitivity (erg cm!U-2!Ns!U-1!N)'

  readcol,'~/Lobster/2014_proposal/lobster_sensitivity.dat',time, bcount, mcount, crabflux, grbflux, hgrbflux
  lobtime=time
  lobflux=grbflux/1.5^2 ;;; from going from 60 to 45 cm focal lengh

  constraints=0.83 ;; assuming 45 deg sun & 13 deg moon contraints
  fovs=[19.1*19.1,29*29.]
  allsky=4.*!pi*(180d/!pi)^2.;41253.
  pointings=allsky/fovs*constraints
  exptimes=86400*0.85/pointings-30 

  exptime=exptimes[0]
  lflux=interpol(lobflux,lobtime,exptime)

  exptime30=exptimes[1]
  f2000=interpol(grbflux,lobtime,exptime30)
print,exptimes
;  xyouts,xrange[0]-1.6,3e-11,'0.3-5.0 keV Sensitivity',charsize=2,orient=90
;  xyouts,xrange[0]-1,5e-11,'(erg cm!U-2!Ns!U-1!N)',charsize=2,orient=90

  ;;;lob,bat,maxi,asm
  ltime=[2021,2024,2025]
;  lflux=1.24e-11
  print,f2000/lflux

  mtime=[2009.75,2018]
  mflux=3.2e-10

  btime=[2004.9,2018]
  bflux=5e-10

  atime=[1996,2011]
  aflux=4e-10

  print,mflux/f2000
  print,mflux/lflux

;  oplot,ltime,[lflux,lflux]
;  oplot,mtime,[mflux,mflux]
;  oplot,atime,[aflux,aflux],thick=10
;  xyouts,atime[0]+8.5,aflux*0.7,'RXTE/ASM',charsize=2

  arrow,ltime[0],lflux,ltime[1],lflux,/data,thick=10,hsize=!D.X_SIZE / 64.
  oplot,[ltime[1],ltime[2]],[lflux,lflux],line=2,thick=10
  xyouts,ltime[0],lflux*1.2,'TAO-ISS/WFI',charsize=3.

  arrow,xrange[0],bflux,btime[1],bflux,/data,thick=10,hsize=!D.X_SIZE / 64.
  xyouts,btime[0]+12.0,bflux*1.2,'Swift/BAT',charsize=2

  arrow,xrange[0],mflux,mtime[1],mflux,/data,thick=11,hsize=!D.X_SIZE / 64.
  xyouts,mtime[0]+7,mflux*0.7,'MAXI',charsize=2

  endplot
  spawn,'ps2pdf ~/Lobster/TAO_2016/compare_tao.eps ~/Lobster/TAO_2016/compare_tao.pdf'

stop
  return 
end
