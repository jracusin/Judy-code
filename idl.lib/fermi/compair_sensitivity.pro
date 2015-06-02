pro compair_psf

  

  return
end 

pro compair_sensitivity

  readcol,'~/proposals/COMPAIR/sensitivity.dat',energy,sens,format='(f,f)'

  p=plot([0,1],/xlog,/ylog,xtitle='Energy (MeV)',ytitle='3'+!tsym.sigma+' Continuum Sensitivity * E!U2!N ['+!tsym.gamma+' MeV s!U-1!Ncm!U-2!N]',xrange=[1e-2,1e6],yrange=[5e-8,1e-2],/xstyle,/ystyle,/nodata,font_size=15,xminor=9,xtickvalues=[1e-2,1e-1,1,10,1e2,1e3,1e4,1e4,1e5,1e6])
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
  ind=74+indgen(80-74)
  p2=plot(energy[ind],sens[ind],thick=4,/overplot,/current,linestyle='--')
  t=text(1,3e-6,'ComPair',/data,/overplot,font_size=18,font_style='bold')
  arr[ind]=arr[ind]+1

  ;;;; Fermi-LAT
  ind=80+indgen(84-80)
  p2=plot(energy[ind],sens[ind],color='magenta',thick=4,/overplot,/current)
  t=text(5e3,1e-6,'Fermi-LAT',color='magenta',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1

  ;;; NuSTAR
  ind=84+indgen(147-84)
  p2=plot(energy[ind]/1e3,sens[ind]*(energy[ind]/1e3)^2*1e3,color='purple',thick=4,/overplot,/current)
  t=text(1e-1,1e-7,'NuSTAR',color='purple',/data,/overplot,font_size=15)
  arr[ind]=arr[ind]+1
  

  p.save,'~/proposals/COMPAIR/instruments_sensitivities.png'
  p.close
  
;  colprint,indgen(n_elements(energy)),energy,arr

stop
  return
end 
