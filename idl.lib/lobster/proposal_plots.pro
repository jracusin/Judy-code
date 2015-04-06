pro janus_compare
  begplot,name='~/Lobster/Proposal/z_mission_lifetime_jns.eps',/encap,font='helvetica',/color
  z=[0,5,6,10,12,14]
  det=[290,28,14,2.1,1.1,0.5]
  bl=[313,65,42,6.7,2.7,1.1]
  jns=[0.7*365.*2.,61,33,17,9,5,2,1,1.,0.01]/2. ;; from Burrows et al. 2011
  jz=[0.,5,6,7,8,9,10,11,12.,14.]
  sz=[0,1,2,3,4,5,6,8,9]
  sdet=[147,102,73,37,14,6,3,2,1.]/6.
  xca=[492.,27.,13.2,1.5,0.6,0.3]/3. ;; our JANUS estimates from Lobster proposal
  xca_bl=[501,66.,39,4.8,1.8,0.6]/3.

  aplot,1,[0,14],[0.1,1000],/nodata,/ylog,xtitle='Redshift z',ytitle='Number of GRBs with Redshift > z per year',yrange=[0.1,1000],/ysty,/xsty
  zz=indgen(141)/10.
  d=10^interpol(alog10(det),z,zz,/spline)
  b=10^interpol(alog10(bl),z,zz,/spline)
  s=10^interpol(alog10(sdet),sz,zz[0:99],/spline)
  j=10^interpol(alog10(jns),jz,zz,/spline)
  x=10^interpol(alog10(xca),z,zz,/spline)
  x2=10^interpol(alog10(xca_bl),z,zz,/spline)
;  oplot,z,det*3,psym=1,color=!red,symsize=5
;  oplot,z,bl*3,psym=1,color=!red,symsize=5
;  oplot,zz,d
  oplot,zz,d,thick=10,color=!blue
  oplot,zz,b,line=1,thick=5,color=!blue
  oplot,zz,s,line=2,thick=5,color=!green
  oplot,zz,j,thick=10,color=!cyan
  oplot,zz,x,thick=10,color=!magenta
  oplot,zz,x2,line=1,thick=5,color=!magenta

;  oplot,z,det,psym=1
;  oplot,z,bl,psym=1
;  oplot,sz,sdet,psym=1
;  oplot,jz,jns,psym=1
;  oplot,z,xca,psym=1
;  oplot,z,xca_bl,psym=1

  legend,['Butler et al. 2010','Bromm & Loeb 2002'],/top,/right,line=[0,1],thick=[10,5],box=0,color=[!p.color,!p.color],charsize=1.5
;  xyouts,1,100,'1 yr'
;  xyouts,2.5,100,'Butler et al. 2010';'3 yrs'
;  xyouts,8.5,100,'Bromm & Loeb 2002';color=!red,'6 yrs'
  legend,['Lobster','JANUS','X-ray Coded Aperture','Swift Observed'],textcolor=[!blue,!cyan,!magenta,!green],/bottom,/left,box=0
;  xyouts,1,20,'Butler et al. 2010';'3 yrs'
;  xyouts,7,200,'Bromm & Loeb 2002';color=!red,'6 yrs'

  endplot
stop
return
end 

pro crap

  mag=[25.,25.1,24.2,24.0,21.6,21.2,20.9]
  f=['g','r','i','z','J','H','K']

  filter=['u','b','v','r','i','z','J','H','K','Rc','Ic']
  lam_eff=[0.36,0.44,0.545,0.64,0.79,0.91,1.26,1.6,2.22,0.64,0.79]*1d-4 ;; cm
  match,f,filter,m1,m2
  lam=lam_eff[m2]
  s=sort(m1)
  f=f[s]
  mag=mag
  lam=lam[s]*1d6

  plot,lam,mag,psym=3,yrange=[26,20],xtitle=!tsym.lambda+' ('+!tsym.mu+'m)',ytitle='Magnitude'
  plotsym,1,2,thick=3
  oplot,lam[0:3],mag[0:3],psym=8
  plotsym,0,1,/fill

  oploterror,lam[4:*],mag[4:*],[0.1,0.1,0.1],/nohat,psym=8

stop
  return
end 
pro proposal_plots

  ;; D.2.2.4 (sensitivity vs FoV)
  
  begplot,name='~/Lobster/Proposal/Sensitivity_FoV.eps',/encap,/color,font='helvetica'
  x='10!U'+(ntostr(indgen(8)-6))+'!N'
  x[6]='1'
  x[7]='10'
  x=[x,' ']
  xv=[10.^(indgen(8)-6.),12]
  aplot,1,[1d-6,12],[1d-14,1d-6],/nodata,/xlog,/ylog,xtitle='Field of View (steradians)',ytitle='Sensitivity in 100 s (erg cm!U-2!N s!U-1!N)',xrange=[1d-6,12.],/xsty,yrange=[1d-14,1d-6],/ysty,yticks=8,ytickformat='loglabels',yminor=9,xtickname=['10!U-6','10!U-5','10!U-4','10!U-3','10!U-2','10!U-1','1','10'],xticks=7,xtickv=xv,xminor=9

;  oplot,[4.*!pi,4.*!pi],[1e-14,1d-6],line=2
;  polyfill,[4.*!pi,100,100,4.*!pi,4.*!pi],[1d-14,1d-14,1d-6,1d-6,1d-14],color=!grey70
;  xyouts,20,5e-10,'All Sky',orient=270;,color=!white

  fov=[9.1e-6,3.9e-5,4.7e-1,1.62]
  sens=[1.6e-13,1.9e-12,2.4e-10,1.49e-8]
  ;; chandra, xrt, lobster, bat
  ;; in 500 s sens=[?,1d-12,8.4e-11,

  plotsym,0,1,/fill
  i=[0,1,3]
  oplot,fov[i],sens[i],psym=8
  plots,fov[2],sens[2],psym=8,color=!red
  y=0.7
  xyouts,fov[0]*2,sens[0]*y,'Chandra'
  xyouts,fov[1]*2,sens[1]*y,'Swift/XRT'
  xyouts,fov[2]/30.,sens[2]*y,'Lobster',color=!red
  xyouts,fov[3]/70.,sens[3]*y,'Swift/BAT'

  arrow,0.5,3e-13,5,3e-13,/data,thick=5,hthick=3,/solid
  arrow,0.5,3e-13,0.5,3e-14,/data,thick=5,hthick=3,/solid
  xyouts,0.5,4e-13,'best'
  xyouts,0.3,5e-14,'best',orient=90

  endplot


  ;;; D.2.2.2d

  begplot,name='~/Lobster/Proposal/z_mission_lifetime.eps',/encap,font='helvetica',/color
  z=[0,5,6,10,12,14]
  det=[290,28,14,2.1,1.1,0.5]
  bl=[313,65,42,6.7,2.7,1.1]
  sz=[0,1,2,3,4,5,6,8]
  sdet=[146,101,72,36,13,5,2,1]

  aplot,1,[0,14],[1,3000],/nodata,/ylog,xtitle='Redshift z',ytitle='Number of GRBs with Redshift > z',yrange=[1,3000],/ysty,/xsty
  zz=indgen(141)/10.
  d=10^interpol(alog10(det),z,zz,/spline)
  b=10^interpol(alog10(bl),z,zz,/spline)
  s=10^interpol(alog10(sdet),sz,zz,/spline)
;  oplot,z,det*3,psym=1,color=!red,symsize=5
;  oplot,z,bl*3,psym=1,color=!red,symsize=5
;  oplot,zz,d
  oplot,zz,d*3,thick=10,color=!blue
  oplot,zz,d*6,thick=10,color=!red
  oplot,zz,b*3,line=1,thick=5,color=!blue
  oplot,zz,b*6,line=1,thick=5,color=!red
  oplot,zz,s,line=2,thick=5,color=!green

  legend,['Butler et al. 2010','Bromm & Loeb 2002','Swift Observed'],/top,/right,line=[0,1,2],thick=[10,5,5],box=0,color=[!p.color,!p.color,!green],charsize=1.5
;  xyouts,1,100,'1 yr'
;  xyouts,2.5,100,'Butler et al. 2010';'3 yrs'
;  xyouts,8.5,100,'Bromm & Loeb 2002';color=!red,'6 yrs'
  legend,['3 yrs','6 yrs'],textcolor=[!blue,!red],/bottom,/left,box=0
;  xyouts,1,20,'Butler et al. 2010';'3 yrs'
;  xyouts,7,200,'Bromm & Loeb 2002';color=!red,'6 yrs'

  endplot
stop
return
end 
