function band,e,k,e0,alpha,beta,intf=intf
  
;  A(E)=K(E/100.)^alpha_1 * exp(-E/E_c)     E < (alpha_1-alpha_2)*E_c
;  A(E)=K[(alpha_1-alpha_2)*E_c/100]^(alpha_1-alpha_2) *(E/100)^alpha_2* exp-(alpha_1-alpha_2)   E > (alpha_1 - alpha_2)*E_c
  
  f=dblarr(n_elements(e))
  
  w1=where(e le (alpha-beta)*e0,n1)
  w2=where(e ge (alpha-beta)*e0,n2)
  
  if n1 gt 0 then f[w1]=k*(e[w1]/50.)^alpha*exp(-e[w1]/e0)
  if n2 gt 0 then f[w2]=k*((alpha-beta)*e0/50.)^(alpha-beta)*exp(beta-alpha)*(e[w2]/50.)^beta
  
  intf=int_tabulated(e,f*e,/double)
;  plot,en,a,/xlog,/ylog;,xrange=[emin,emax],xstyle=1,yrange=minmax(a),ystyle=1
  
  return,f
end 

function simple_pl,e,k,alpha,intf=intf
  
  f=k*(e/50.)^(alpha)
;  intf=k/50.^alpha*(1./(alpha+2.))*(max(e)^(alpha+2.)-min(e)^(alpha+2.))
  intf=int_tabulated(e,f*e,/double)
  
  return,f
end 

function cutoff_pl,e,k,alpha,epeak,intf=intf
  
  f=k*(e/50.)^alpha*exp(-e*(2.+alpha)/epeak)
  intf=int_tabulated(e,f*e,/double)
  
  return,f
end

pro compare_functions,epeak,sbol_pl,sbol_cpl,sbol_band
  
  emin=1.
  emax=1.e4
;  emin=15.
;  emax=150.
  e=findgen(emax-emin)+emin
  w=where(e eq 50.)
  w=w[0]
  k=1.
  
  kpl=1.
  alpha_pl=-1.
  fpl=simple_pl(e,kpl,alpha_pl,intf=sbol_pl)
  
;  epeak=1000.
  alpha_cpl=-1.
  fcpl=cutoff_pl(e,k,alpha_cpl,epeak,intf=sbol_cpl)
  kcpl=1./fcpl[w]
  fcpl=cutoff_pl(e,kcpl,alpha_cpl,epeak,intf=sbol_cpl)
  
  alpha=-1.
  beta=-2.5
  e0=epeak/(2.+alpha)
  fband=band(e,k,e0,alpha,beta,intf=sbol_band)
  kband=1./fband[w]
  fband=band(e,kband,e0,alpha,beta,intf=sbol_band)
  
  plot,e,fpl,/xlog,/ylog
  oplot,e,fcpl,color=!green,line=2
  oplot,e,fband,color=!red,line=1
  legend,['PL','CPL','Band'],box=0,line=[0,2,1],color=[!p.color,!green,!red],/top,/right
  oplot,[15,15],[1e-6,1e6],line=1
  oplot,[150,150],[1e-6,1e6],line=1
  
  bandfrac=sbol_band/sbol_pl
  cplfrac=sbol_cpl/sbol_pl
  
;  stop
  return
end 

pro simulate_eiso
  
  epeak=[findgen(100)+1.,findgen(100)*9.+100.,findgen(10)*900.+1000.]
  n=n_elements(epeak)
  
  sbol_pl=fltarr(n) & sbol_cpl=sbol_pl & sbol_band=sbol_pl
  for i=0,n-1 do begin
     compare_functions,epeak[i],pl,cpl,band
     sbol_pl[i]=pl
     sbol_cpl[i]=cpl
     sbol_band[i]=band
  endfor 
  begplot,name='~/jetbreaks/sbol_epeak.ps',/color,/land 
;  plot,epeak,sbol_pl,/xlog,xtitle='E!Lpeak!N',ytitle='Sbol/Sbol!LPL!N'
  plot,[1,1e4],[0,2],/xlog,/nodata,xtitle='E!Lpeak!N',ytitle='Sbol/Sbol!LPL!N'
  oplot,[1,1e4],[1,1]
  oplot,epeak,sbol_band/sbol_pl,line=2,color=!red
  oplot,epeak,sbol_cpl/sbol_pl,line=1,color=!green
  legend,['PL','CPL','Band'],box=0,line=[0,1,2],color=[!p.color,!green,!red],/top,/right
  oplot,[15,15],[0,5],line=1
  oplot,[150,150],[0,5],line=1
  endplot
  
  stop
  return
end 
