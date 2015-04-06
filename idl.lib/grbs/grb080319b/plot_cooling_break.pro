@fit_functions
pro plot_cooling_break
  
  h=4.1356692d-18               ;kev s
  ebk=[9e-4,4.7e-3,0.12,0.02,3e-3,4e-4]/h
  ebkerr1=[8.999e-4,0.02,0.02,0.03,6e-4,1e-4]/h
  ebkerr2=[2e-4,0.02,0.02,0.03,4e-4,2e-4]/h
  time=[350,720,1500,5856,1.2e4,2e5]
  tstart=[314,680,1300,5050,10810,1e5]
  tstop=[392,780,1700,6850,12810,4e5]
  n=n_elements(time)
  
  plot,time,ebk,/xlog,/ylog,xtitle='Time (s)',ytitle=!tsym.nu+'!Lc!N (Hz)',psym=3
  elow=ebk-ebkerr1
  ehigh=ebk+ebkerr2
  w=where(elow le 0,nw)
  elow[w]=1e-5
  for i=0,n-1 do begin
     oplot,[time[i],time[i]],[elow[i],ehigh[i]]
     oplot,[tstart[i],tstop[i]],[ebk[i],ebk[i]]
  endfor 
  
  ;;pow_pow,x,p,fe,fp1,fp2
  
;;   newp=mpfitfun('sed_model',x,flux,fluxerr,p,weights=weights,yfit=yfit,nprint=100,parinfo=parinfo)   
  
  p=[1e13,-0.5,2e3,0.5]
  np=n_elements(p)
  parinfo=parinfo_struct(np)
  parinfo[2].relstep=0.01
  parinfo[1].limits=[-5,0]
  parinfo[1].limited=[1,1]
  parinfo[3].limits=[0,0]
  parinfo[3].limited=[1,0]
  parinfo.value=p
  
;  w=[0,1,2,3,5]
  w=indgen(6)
  ebkerr=(ebkerr1+ebkerr2)/2.
  newp=mpfitfun('bknpow',time[w],ebk[w],ebkerr[w],p,yfit=yfit,nprint=100,parinfo=parinfo,perror=perror)
  t=[(findgen(19)+1)*100,(findgen(8)+2)*1e3,(findgen(9)+1)*1e4,(findgen(9)+1)*1e5]
  yfit=bknpow(t,newp)
  oplot,t,yfit,color=!green
  
  w1=[0,1,2]
  w2=[2,3,4,5]
  newp1=mpfitfun('pow',time[w1],ebk[w1],ebkerr[w1],p[0:1],nprint=100,parinfo=parinfo[0:1],yfit=yfit1)
  newp2=mpfitfun('pow',time[w2],ebk[w2],ebkerr[w2],p[2:3],nprint=100,parinfo=parinfo[2:3],yfit=yfit2)
  
  f1=pow(t,newp1)
  f2=pow(t,newp2)
  oplot,t,f1,line=1,color=!red
  oplot,t,f2,line=2,color=!red
  
  parinfo[1].fixed=1
  parinfo[3].fixed=1
  newp3=mpfitfun('bknpow',time[w],ebk[w],ebkerr[w],p,yfit=yfit,nprint=100,parinfo=parinfo,perror=perror)
  f3=bknpow(t,newp3)
  oplot,t,f3,line=4,color=!blue
  
  stop
  return
end 
