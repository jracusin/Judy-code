@fit_functions
pro compare_eiso
  begplot,name='~/papers/jetbreaks1/eiso_eta.ps',/color
  !p.multi=[0,1,2]
  read_butler_eiso,grb_b,eiso_b,eisoerr_b,z_b
  read_liang_eiso,grb_l,eisok,z_l,limit
  
  match,grb_b,grb_l,mb,ml
  
  logeisoerr_b=dblarr(2,n_elements(grb_b))
  logeisoerr_b[0,*]=alog10((eiso_b-eisoerr_b[0,*])*1d52)
  logeisoerr_b[1,*]=alog10((eiso_b+eisoerr_b[1,*])*1d52)

  logeiso_b=alog10(eiso_b*1d52)
  logeisok=alog10(eisok*1d52)
  logeisoerr=dblarr(n_elements(ml))
  
  plotsym,0,/fill
  plot,logeisok[ml],logeiso_b[mb],psym=8,/yno,/iso,xtitle='log E!Lk,iso!N (Liang)',ytitle='log E!Liso!N (Butler)'
  for i=0,n_elements(ml)-1 do begin
     oplot,[logeisok[ml[i]],logeisok[ml[i]]],[logeisoerr_b[0,mb[i]],logeisoerr_b[1,mb[i]]]
     logeisoerr[i]=mean(logeisoerr_b[*,mb[i]])
  endfor 

  p=-2d
  newp=mpfitfun('offset',logeisok[ml],logeiso_b[mb],logeisoerr,p,/quiet,perror=perror)
  yfit=offset(logeisok[ml],newp)
  oplot,logeisok[ml],yfit,color=!blue
;  fiteta=10.^newp
  fiteta=(10.^newp)/(1.+10.^newp)
  print,perror
;  conf_error,logeisok[ml],logeiso_b[mb],logeisoerr,newp,0.2,'offset',newerr
  print,'All: ',newp,fiteta;,newerr,10.^(newp-newerr[0]),10.^(newp+newerr[1])
  
  legend,[!tsym.eta+'='+sigfig(fiteta,2)],/top,/left,box=0,textcolor=!blue
  
;  eta=eiso_b[mb]/eisok[ml]
  eta=eiso_b[mb]/(eiso_b[mb]+eisok[ml])
  plothist,alog10(eta),bin=0.2,xtitle='log '+!tsym.eta,ytitle='N'
  oplot,[newp,newp],[0,100],color=!blue
  legend,['mean  '+!tsym.eta+'='+sigfig(mean(eta),3),'median  '+!tsym.eta+'='+sigfig(median(eta),2),!tsym.eta+'=E!Liso!N/(E!Liso!N+E!Lk,iso!N)'],/top,/left,box=0
  
;  plothist,[-5.,alog10(fiteta)],bin=0.2,xrange=[-4,1],/overplot,color=!red
  !p.multi=0
  endplot
  stop
  return
end 
