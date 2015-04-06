@calc_eiso
function cosfunc,x,p

  yfit=p[0]*(cos(x))^p[1]
  return,yfit
end 

pro effarea_plots

goto,skip
  file='~/BurstCube/gitrep/BurstCube/Simulations/GEANT3/effarea_vs_e-15deg.dat'
  readcol,file,loge,allener,enerr,fullpeak,peakerr,fullesc,fullescerr,skip=4
  eng=10^loge

  file='~/BurstCube/gitrep/BurstCube/Simulations/GEANT3/gbm_effective_area.dat'
  readcol,file,genergy,garea,skip=2
  genergy[n_elements(genergy)-1]=1e4

;  bfunc=band(eng,[1.,-1,162.,-2.5,100])
;  bfunc0=band(100.,[1.,-1,162.,-2.5,100])
;  bfunc=bfunc/bfunc0[0];max(bfunc*eng^2)

;  gint=interpol(alog10(garea),alog10(genergy),alog10(eng))
;  gint=10^gint

;  print,int_tabulated(eng,gint*bfunc)
;  w0=where(eng ge 10 and eng le 1000)
;  print,int_tabulated(eng[w0],gint[w0]*bfunc[w0])
;  w=where(eng ge 50 and eng le 300)
;  print,int_tabulated(eng[w],gint[w]*bfunc[w])

;stop
;  print,int_tabulated(genergy,garea)
;  w0=where(genergy ge 10 and genergy le 1000)
;  print,int_tabulated(genergy[w0],garea[w0])
;  w=where(genergy ge 50 and genergy le 300)
;  print,int_tabulated(genergy[w],garea[w])


  readcol,'~/BurstCube/gitrep/BurstCube/Notebooks/atten_ga.txt',ga_eng,ga_fact
  readcol,'~/BurstCube/gitrep/BurstCube/Notebooks/atten_al.txt',al_eng,al_fact
  iga=interpol(ga_fact,ga_eng,eng)
  ial=interpol(al_fact,al_eng,eng)
  f=iga*ial

  p=errorplot(eng,allener*f,enerr*f,/xlog,/ylog,xrange=[1,1e4],yrange=[1,200],xtitle='Photon Energy (keV)',ytitle='Effective Area (cm!U2!N)',errorbar_capsize=0,symbol='o',sym_size=0.3,/sym_filled,font_size=15)
  p2=errorplot(eng,fullpeak*f,peakerr*f,/overplot,errorbar_capsize=0,symbol='o',sym_size=0.3,/sym_filled,sym_fill_color='blue',color='blue',errorbar_color='blue')
  p3=errorplot(eng,fullesc*f,fullescerr*f,/overplot,errorbar_capsize=0,symbol='o',sym_size=0.3,/sym_filled,sym_fill_color='red',color='red',errorbar_color='red')
  p4=plot(genergy,garea,color='green',thick=5,/overplot)

  t=text(3,100,'GBM NaI',color='green',/data,font_size=15)
  t=text(1500,12,'BurstCube',/data,font_size=15)
  t=text(2200,9,'(total)',/data,font_size=15)
  t=text(130,4,'BurstCube',color='blue',/data,font_size=15)
  t=text(70,3,'(energy absorption)',color='blue',/data,font_size=15)
  t=text(25,20,'BurstCube',color='red',/data,font_size=15)
  t=text(18,15,'(energy absorption',color='red',/data,font_size=15)
  t=text(20,11,'+escape fraction)',color='red',/data,font_size=15)

  p.save,'~/BurstCube/burstcube_effarea_energy.pdf'
  wait,1
  p.close
  stop

;  ploterror,ytickformat='loglabels',xtickformat='loglabels'
  ;; oplot,eng,allener
  ;; oplot,eng,fullpeak,color=!blue
  ;; oploterror,eng,fullpeak,peakerr,psym=8,errcolor='blue',/nohat,color='blue'
  ;; oploterror,eng,fullesc,fullescerr,psym=8,/nohat,errcolor='red',color='red'
  ;; oplot,eng,fullesc,color=!red
  ;; oplot,genergy,garea,thick=20,color=!green

  ;; xyouts,10,10,'GBM',color=!green,/data
  ;; xyouts,5e2,50,'BurstCube All Energies',/data
  ;; xyouts,2,40,'BurstCube 100% energy absorption',/data,color=!blue
  ;; xyouts,10,1,'BurstCube Including Escape Fraction',/data,color=!red
  skip:

;;;; eff area vs enegy
  file='~/BurstCube/gitrep/BurstCube/Simulations/GEANT3/effarea.dat'
  readcol,file,angle,area,error,skip=4
  p=errorplot(angle,area,error,xtitle='Incident Angle (deg)',ytitle='Effective Area (cm!U2!N)',xrange=[0,95],/xstyle,symbol='circle',/sym_filled,linestyle='none',font_size=15)

  angle[n_elements(angle)-1]=89.99
  pnew=mpfitfun('cosfunc',angle*!dtor,area,error,[71.,1.],yfit=yfit)
  angles=findgen(900)/10.
  p2=plot(angles,cosfunc(angles*!dtor,pnew),/overplot,/current)
  t=text(45,70,'Area='+numdec(pnew[0],1)+'*cos(Angle)!U'+numdec(pnew[1],2)+'!N',/data,font_size=15)
  p.save,'effarea_angle.pdf'
  p.close

stop
  return
end 
