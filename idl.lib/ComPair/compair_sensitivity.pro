pro real_compair_sensitivity

  readcol,'~/proposals/COMPAIR/sensitivity.dat',energy,sens,format='(f,f)'

  p=plot([0,1],/xlog,/ylog,xtitle='Energy (MeV)',ytitle='3'+!tsym.sigma+' Continuum Sensitivity * E!U2!N ['+!tsym.gamma+' MeV s!U-1!Ncm!U-2!N]',xrange=[1e-2,1e6],yrange=[1e-8,1e-2],/xstyle,/ystyle,/nodata,font_size=15,xminor=9,xtickvalues=[1e-2,1e-1,1,10,1e2,1e3,1e4,1e4,1e5,1e6]);,xtickformat='loglabels()')


  ;; LAT
  readcol,'~/ComPair/differential_flux_sensitivity_p8r2_source_v6_all_10yr_zmax100_n10.0_e1.50_ts25_000_090.txt',emin,emax,e2diff,tmp
  lat_eng=fltarr(n_elements(emin))
  for i=0,n_elements(emin)-1 do lat_eng[i]=(emin[i]+emax[i])/2.
  erg2mev=624151d
  trat=1.;sqrt(10*86400.*365./1e6/5.)
  p2=plot(lat_eng,e2diff*trat*erg2mev,color='magenta',thick=4,/overplot,/current)
  t=text(1e3,2e-6,'Fermi-LAT',color='magenta',/data,/overplot,font_size=15)

  ;;;; EGRET
  ind=69+indgen(74-69)
  p2=plot(energy[ind],sens[ind],color='medium blue',thick=4,/overplot,/current)
  t=text(1e2,4e-5,'EGRET',color='medium blue',/data,/overplot,font_size=15)
stop

  ;;;; SPI
  ind=20+indgen(46-20)
  p2=plot(energy[ind],sens[ind],thick=4,/overplot,/current,color='green')
  t=text(10,2e-3,'SPI',/data,/overplot,font_size=15,color='green')
;  arr[ind]=arr[ind]+1

  ;; COMPTEL

  x=[0.73295844,0.8483429,1.617075,5.057877,16.895761,29.717747]
  y=[6.566103E-4,3.6115389E-4,1.4393721E-4,1.6548172E-4,2.36875E-4,3.390693E-4]

  p2=plot(x,y,color='brown',thick=4,/overplot,/current)
  t=text(1,8e-4,'COMPTEL',color='brown',/data,/overplot,font_size=15)


  ;;; NuSTAR
  ind=84+indgen(147-84)
  p2=plot(energy[ind]/1e3,sens[ind]*(energy[ind]/1e3)^2*1e3,color='purple',thick=4,/overplot,/current)
  t=text(1.2e-1,1e-7,'NuSTAR',color='purple',/data,/overplot,font_size=15)


  ;;;; ComPair

  eng=[0.316,1,3.16,10,31.6,100,316.]
;  tracked=[  2.48254691e-04,   1.94994449e-05,   2.29602950e-05, 1.88148581e-04, 0, 0, 0]
;  untracked=[  9.59511292e-06,   7.19413663e-06,   5.00115394e-05, 3.45139969e-04, 0, 0, 0]
;  pair=[0, 0, 6.93329478e-04, 7.01540586e-04,   6.11810517e-05,   8.92135307e-05, 8.89913469e-05]

  ;; on-axis (cos1, th=0)
  tracked=[  5.53430172e-05,   4.40376468e-06,   4.84006281e-06, 4.07660054e-05, 0, 0, 0]
  untracked=[  2.19637409e-06,   1.57804734e-06,   5.17230145e-06, 7.79770234e-05, 0, 0, 0] 
  pair=[0, 0,   1.58565563e-04, 1.59741956e-04,   1.36598581e-05,   1.79031358e-05, 1.79781875e-05]

  ;; off-axis (cos0.8, th=37)
  tracked=[  1.54877155e-05,   4.84546681e-06,   5.28735667e-06, 6.53265846e-05, 0, 0, 0]
  untracked=[  2.49626245e-06,   1.82264874e-06,   1.54100276e-05, 9.59603201e-05, 0, 0, 0]
  pair=[ 0, 0,   5.62236032e-05, 3.19254897e-05,   1.71183233e-05,   1.61203804e-05, 2.19339e-05]


  p2=plot(eng,tracked,thick=4,/overplot,/current)
;  p2=plot(eng,untracked,thick=4,linestyle='--',/overplot,/current)
  p2=plot(eng,pair,thick=4,linestyle='-.',/overplot,/current)

;  t=text(1e3,1e-3,'Pair',/data,/overplot,font_size=15)  
;  t=text(1e-1,1e-6,'Untracked',/data,/overplot,font_size=12)
;  t=text(0.4,2e-5,'Tracked',/data,/overplot,font_size=12)
;  t=text(2e1,6e-5,'Pair',/data,/overplot,font_size=12)

;  t=text(1e3,2e-3,'Exposure = 10!U6!N s',/data,/overplot,font_size=15)

  t=text(1,1e-6,'ComPair',/data,/overplot,font_size=20,font_style='bold')

  p.save,'new_compair_sensitivity.png'
  p.close



stop
  return
end 

pro compair_sensitivity

  readcol,'~/proposals/COMPAIR/sensitivity.dat',energy,sens,format='(f,f)'

  p=plot([0,1],/xlog,/ylog,xtitle='Energy (MeV)',ytitle='3'+!tsym.sigma+' Continuum Sensitivity * E!U2!N ['+!tsym.gamma+' MeV s!U-1!Ncm!U-2!N]',xrange=[1e-2,1e6],yrange=[1e-8,1e-2],/xstyle,/ystyle,/nodata,font_size=15,xminor=9,xtickvalues=[1e-2,1e-1,1,10,1e2,1e3,1e4,1e4,1e5,1e6])
;  oplot,energy,sens,psym=1
  arr=intarr(n_elements(energy))

  t=text(1e4,3e-7,'T!Lobs!N=10!U6!N s',font_size=15,/data)
  t=text(1e4,1e-7,!tsym.delta_cap+'E=E',font_size=15,/data)


  ;;;; JEM-X
  ind=[indgen(8)]
  p2=plot(energy[ind],sens[ind],color='hot pink',thick=4,/overplot,/current)
  t=text(1.5e-2,4e-5,'JEM-X',color='hot pink',/data,font_size=15)
  arr[ind]=arr[ind]+1
  
  ;;;; IBIS-ISGRI
  ind=8+indgen(20-8)
  p2=plot(energy[ind],sens[ind],color='blue',thick=4,/overplot,/current)
  t=text(1.5e-2,2e-4,'IBIS-ISGRI',color='blue',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;;; SPI
  ind=20+indgen(46-20)
  p2=plot(energy[ind],sens[ind],thick=4,/overplot,/current)
  t=text(10,2e-3,'SPI',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;; IBIS-PICsIT
  ind=46+indgen(60-46)
  p2=plot(energy[ind],sens[ind],color='green',thick=4,/overplot,/current)
  t=text(5e-2,2e-3,'IBIS-PICsIT',color='green',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;;; COMPTEL
  ind=60+indgen(66-60)
  p2=plot(energy[ind],sens[ind],color='brown',thick=4,/overplot,/current)
  t=text(1,7e-5,'COMPTEL',color='brown',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;;; COS-B
  ind=66+indgen(69-66)
  p2=plot(energy[ind],sens[ind],color='navy',thick=4,/overplot,/current)
  t=text(100,5e-4,'COS-B',color='navy',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;;; EGRET
  ind=69+indgen(74-69)
  p2=plot(energy[ind],sens[ind],color='medium blue',thick=4,/overplot,/current)
  t=text(50,2e-5,'EGRET',color='medium blue',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;;; ComPair
;  ind=74+indgen(80-74)
;  ind=147+indgen(158-147)
  ind=158+indgen(167-158)
  p2=plot(energy[ind],sens[ind],thick=4,/overplot,/current,linestyle='--')
  t=text(0.7,1.5e-6,'ComPair',/data,/overplot,font_size=18,font_style='bold')
  arr[ind]=arr[ind]+1

  ;;;; Fermi-LAT
  ind=80+indgen(84-80)
  p2=plot(energy[ind],sens[ind],color='magenta',thick=4,/overplot,/current)
  t=text(5e3,1e-6,'Fermi-LAT',color='magenta',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;; NuSTAR
  ind=84+indgen(147-84)
  p2=plot(energy[ind]/1e3,sens[ind]*(energy[ind]/1e3)^2*1e3,color='purple',thick=4,/overplot,/current)
  t=text(1.2e-1,1e-7,'NuSTAR',color='purple',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1
  
  ;;; BAT - estimated from 70 month survey
  sens=1.18*1.03e-11/0.43*6.24e5*(1./1.)^(-0.5) ;;; erg cm-2 s-1 /mcrab * erg2mev *(T/1Ms)^(-0.5)
  beng=[14,195]*1e-3 ;; keV to MeV
  p2=plot(beng,[sens,sens],color='orange',thick=4,/overplot,/current)
  t=text(3e-1,1.5e-5,'Swift-BAT',color='orange',/data,/overplot,font_size=15)

  p.save,'~/proposals/COMPAIR/instruments_sensitivities.png'
  p.close
  
;  colprint,indgen(n_elements(energy)),energy,arr

stop
  return
end 
