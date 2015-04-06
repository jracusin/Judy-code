pro transient_plot

  readcol,'~/Lobster/Proposal/soderberg.csv',time,lum
  time=[time[0:19],time[24:*]]
  lum=[lum[0:19],lum[24:*]]
  n=n_elements(time)/4

  trans=create_struct('type1','','type2','','type3','','dist1',0d,'dist2',0d,'lum',dblarr(4),$
                      'time',dblarr(4),'flux',dblarr(4),'color',0L,'xoff',0.,'yoff',0.,'ang',0.)
  begplot,name='~/iLobster/transient_plot.eps',/land,/color,/encap,font='helvetica'
  
  n=13
  trans=replicate(trans,n)
  i=0
  trans[i].type1='LGRBs'
  trans[i].dist1=lumdist(3,h=72,omega_m=0.27,lambda=0.73)*1d6
;  trans[i].lum=[51,45,43,48]
  trans[i].flux=[1d-6,5d-10,5d-12,1d-9]
  trans[i].time=[1,1d4,1d4,1]
  trans[i].color=!deeppink
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
  trans[i].color=!seagreen
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
  trans[i].color=!magenta  

  i=3
  trans[i].type1='  Tidal'
  trans[i].type2='Disruptions'
;  trans[i].type3='  tion'
  trans[i].dist1=200e6 ;;pc
  trans[i].flux=[2.1e-10,5e-11,5.2e-13,2.1e-12]
  trans[i].time=[100,1e6,1e6,100];time[28:31]
  trans[i].color=!yellow
  trans[i].yoff=0.8
  trans[i].xoff=2e4
  trans[i].ang=-0.01

  i=9
  trans[i].type1='SN Ibc/II Breakout'
  trans[i].dist1=50e6 ;;pc
  trans[i].lum=[44,45,44.5,43.5];lum[20:23]
  trans[i].time=time[20:23]
  trans[i].color=!turquoise
  trans[i].ang=12
  trans[i].yoff=0.08
  trans[i].xoff=2
  
  i=4
  trans[i].type1='Flare'
  trans[i].type2='Stars'
  trans[i].dist1=100. ;;pc
  trans[i].flux=[4d-10,4d-10,1d-12,1d-12]
  trans[i].time=time[40:43]
  trans[i].color=!red
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
  trans[i].color=!purple

  i=10
  trans[i].type1='Classical Novae'
  trans[i].dist1=3e3 ;;pc
  trans[i].lum=lum[32:35]
  trans[i].time=time[32:35]
  trans[i].color=!orange
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
  lobtime=[10,20,50,100,200,400,1000,2000,1e4,2e4,4e4,8e4]
  lobflux=[9.29e-10,4.64e-10,1.86e-10,9.29e-11,4.64e-11,2.61e-11,1.33e-11,7.71e-12,2.7e-12,1.88e-12,1.27e-12,1e-12];8.47e-13] ;4sigma
;   lobflux=[9.29e-10,4.64e-10,1.86e-10,1.28e-10,7.22e-11,4.01e-11,2.07e-11,1.24e-11,4.38e-12,2.90e-12,1.82e-12,1.01e-12] ;; 6 sigma

  nl=n_elements(lobtime)

  !x.margin=[14,0]
  xrange=[1e-1,1e7]
  plot,[1e-1,1e7],[1e-14,1e-5],/nodata,xrange=xrange,yrange=[1e-14,1e-5],/xsty,/ysty,xtitle='Timescale (s)',ytitle='X-ray Flux (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,xtickname=replicate(' ',9),yticks=9
  x=0
  square=intarr(n)
  square[*]=1
  w=where(trans.ang ne 0)
  square[w]=0
  
;  t1=[0.01,0.02,0.05,0.1,0.2,0.5]
  t1=[0.1,0.2,0.5,1.]
  nt1=n_elements(t1)
  t2=[1e5,1e6,2e6,1e7]
  nt2=n_elements(t2)
  time=[t1,lobtime,t2]
  flux=[lobflux[0]/(t1/lobtime[0]),lobflux,replicate(1e-12,4)];,lobflux[nl-1]/sqrt(t2/lobtime[nl-1])]
  flux2=[lobflux[0]/(t1/lobtime[0]),lobflux,lobflux[nl-1]/sqrt(t2/lobtime[nl-1])]
;  flux2=10^interpol(alog10(lobflux),alog10(lobtime),alog10(time))
;  oplot,lobtime,lobflux,psym=1
  polyfill,[time,1e7,xrange[0],time[0]],[flux,1e-14,1e-14,flux[0]],color=!grey70
  xyouts,1e4,1e-6,'WFI Detectability'

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
     polyfill,t,l,color=trans[i].color
     xx=t[0]*1.1;(t[1]-t[0])/8.+t[0]
     yy=(l[2]-l[1])/2.+l[1]
     print,xx,yy
     xyouts,xx+trans[i].xoff,yy*trans[i].yoff,trans[i].type1,color=!black,charsize=1.5,orientation=trans[i].ang
     xyouts,xx+trans[i].xoff,yy/3.*trans[i].yoff,trans[i].type2,color=!black,charsize=1.5,orientation=trans[i].ang
     xyouts,xx+trans[i].xoff,yy/6.*trans[i].yoff,trans[i].type3,color=!black,charsize=1.5,orientation=trans[i].ang
;     k=get_kbrd(10)
;     if k eq 's' then stop
     x=x+4
  endfor
  
  i=5
  xx=trans[i].time[0]
  yy=(trans[i].flux[2]-trans[i].flux[1])/2.+trans[i].flux[1]
  xyouts,xx+trans[i].xoff,yy*trans[i].yoff,trans[i].type1,color=!black,charsize=1.5,orientation=trans[i].ang
  xyouts,xx+trans[i].xoff,yy/3.*trans[i].yoff,trans[i].type2,color=!black,charsize=1.5,orientation=trans[i].ang
  oplot,time,flux2,line=2;,color=!grey50
  oplot,[8e4,1e7],[1e-12,1e-12],line=1,thick=10
;  oplot,t2,replicate(1e-12,nt2),line=1
;  oplot,time,flux2,line=1,color=!grey20
  oplot,[1e-2,1e7],[1e-14,1e-14]
  oplot,[1e7,1e7],[1e-14,1e-2]
  axis,yaxis=0,yticks=9,yminor=9,ytickname=replicate(' ',10)
  axis,yaxis=1,yticks=9,yminor=9,ytickname=replicate(' ',10)
  axis,xaxis=0,xticks=8,xtickformat='loglabels',xminor=9
  axis,xaxis=1,xticks=8,xtickname=replicate(' ',9),xminor=9


  xyouts,5e2,1e-13,'WFI 4'+!tsym.sigma+' Sensitivity'
  arrow,5e3,3e-13,5e3,3e-12,/data,thick=5,hthick=3,/solid
  xyouts,1e5,1.4e-12,'Confusion Limit'
  xyouts,0.15,3e-14,'Minimum Integration Time = 2s'
;  xyouts,2d-2,3e-6,'WFI 4'+!tsym.sigma,color=!black
;  xyouts,2d-2,1e-6,'Sensitivity',color=!black
;  arrow,1e-1,7d-7,1e-1,1e-7,/data,thick=5,hthick=3,/solid

;  oplot,lobtime,lobflux,psym=1

  endplot
  spawn,'convert ~/iLobster/transient_plot.eps ~/iLobster/transient_plot.pdf'

stop
return
end 
