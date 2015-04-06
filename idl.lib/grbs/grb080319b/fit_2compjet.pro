@fit_functions
pro fit_2compjet,ps=ps,rate=rate,nosmooth=nosmooth
  
  dir='~/Desktop/GRB080319B/'
  if keyword_set(ps) then begplot,name='xrt_2comp_fit.eps',/land,font='helvetica',/encap;,/color,/cmyk
  if keyword_set(rate) then begin
     lc=lcout2fits(dir+'correct_xrt_lc_rate.txt')
;     lc=lcout2fits(dir+'lc_newout_manual.txt')
     ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)'
     yrange=[1e-4,1e4]
     normf=1e8
  endif else begin 
     lc=lcout2fits(dir+'correct_xrt_lc.txt')
     ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)'
     yrange=[1e-15,1e-6]
     normf=1.
  endelse 
  nlc=n_elements(lc)
;  w=where(lc.src_rate_err gt 0,nw) 
  w=where(lc.src_rate_err gt 0 and lc.time gt 80,nw)
; and lc.time lt 1.8e6,nw); and lc.time gt 80,nw)
  
  multiplot2,/reset,/default
  erase     
  multiplot2,[1,2],/init,xgap=0.05
  multiplot2,ydowngap=0.
  ploterror,lc[w].time,lc[w].src_rate,lc[w].src_rate_err,/nohat,psym=3,/xlog,/ylog,ytitle=ytitle,yminor=9,yticks=9,yrange=yrange,/ysty,ytickf='loglabels'
  
  for i=0,nw-1 do begin
     oplot,[lc[w[i]].tstart,lc[w[i]].tstop],[lc[w[i]].src_rate,lc[w[i]].src_rate]
  endfor 
  
  plotsym,0,/fill
  oploterror,3.25e6,6.13e-15,5e3,2.44e-15,psym=8,/nohat
  
;  p=[1,3000.,1.5,2.5,1.,1.2,1.]
;  p=[1.e5,1.5,2000.,2.5,10,1.2,9.e5,4.]
  
  if keyword_set(nosmooth) then begin 
     pname=['norm1','alpha1','tb1','alpha2','norm2','alpha3','tb2','alpha4']
     p=[1d-4*normf,1.5,3000,2.,1d-7*normf,0.9,1d6,2.5]
     np=n_elements(p)
     parinfo = parinfo_struct(np)
     parinfo.value=p
     parinfo.parname=pname
     
     parinfo[0].limits=[0,0] ;;norm1
     parinfo[0].limited=[1,0]
     parinfo[1].limits=[1,0] ;;pow1
     parinfo[1].limited=[1,0]
     parinfo[2].limits=[1000,0] ;;tb1
     parinfo[2].limited=[1,0]
     parinfo[3].limits=[1,0] ;;pow2
     parinfo[3].limited=[1,0]
     parinfo[4].limits=[0,0] ;;;norm2
     parinfo[4].limited=[1,0]
     parinfo[5].limits=[0,0] ;;pow3
     parinfo[5].limited=[1,0]
     parinfo[6].limits=[1e5,2e6] ;;tb2
     parinfo[6].limited=[1,0]
     parinfo[7].limits=[2,0] ;;pow4
     parinfo[7].limited=[1,0]
     
     model='double_bknpow'
     intmodel='int_double_bknpow'
  endif else begin 
     p=[1e-5,1e-8,2000,1e6,1.5,2.1,0.9,2.7,1,1]*1d
     pname=['norm1','norm2','tb1','tb2','alpha1','alpha2','alpha3','alpha4','s1','s2']
     np=n_elements(p)
     
     parinfo = parinfo_struct(np)
     parinfo.value=p
     parinfo.parname=pname
     
     ;;norm
     parinfo[0].limits=[0,0] ;;norm > 0
     parinfo[0].limited=[1,0]
     parinfo[1].limits=[0,0]
     parinfo[1].limited=[1,0]
     ;;tbreak
     parinfo[2].limits=[100,0]
     parinfo[2].limited=[1,0]
     parinfo[3].limits=[1e4,1.5e6] ;max(lc[w].time)]
     parinfo[3].limited=[1,1]
     parinfo[3].fixed=1
     ;;alpha
     parinfo[4].limits=[0,0]  ;;alpha1
     parinfo[4].limited=[1,0]
     parinfo[5].limits=[1,0] ;;alpha2
     parinfo[5].limited=[1,0]
     parinfo[6].limits=[0,0] ;;alpha3
     parinfo[6].limited=[1,0]
     parinfo[7].limits=[2,0]  ;;alpha4
     parinfo[7].limited=[1,0]
     ;;smooth
     parinfo[8].fixed=1
     parinfo[9].fixed=1
;  parinfo[8].limits=[0,0]
;  parinfo[8].limited=[1,0]
;  parinfo[9].limits=[0,0]
;  parinfo[9].limited=[1,0]
     
     model='double_sm_bknpow'
     intmodel='double_sm_bknpow'
  endelse 
  
  tt=dblarr(2,nw)
  tt[0,*]=lc[w].tstart
  tt[1,*]=lc[w].tstop
  time=lc[w].time
  
  ;;;not doing int model!
  time=[time,1e7]
  if keyword_set(nosmooth) then begin 
     newp=mpfitfun('int_double_bknpow',tt,lc[w].src_rate,lc[w].src_rate_err,p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo)
     time=[time,p[2]]
     time=time[sort(time)]
     yfit=double_bknpow(time,newp,f1,f2)
  endif else begin 
     newp=mpfitfun('double_sm_bknpow',time,lc[w].src_rate,lc[w].src_rate_err,p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo)
     yfit=double_sm_bknpow(time,newp,f1,f2)
  endelse 

  
  oplot,time,yfit;,color=!red
  oplot,time,f1,line=1;,color=!blue     ;,line=1
  oplot,time,f2,line=2;,color=!green    ;,line=2
  
;  seds=[150,350,600,5856,1.17e4,7.89e4,2.2e5]
;  for ss=0,6 do oplot,[seds[ss],seds[ss]],[1e-4,1e5]

  stop
  
  delchi0=chisqr_cvf(0.1,n_elements(newp))
  print,delchi0
;  if keyword_set(nosmooth) then conf_error,tt,lc[w].src_rate,lc[w].src_rate_err,newp,perror,'int_double_bknpow',perror2,pvarunit,bestchisq,yfits,log=log,pmin0=pmin0,delchi0=delchi0 else $
;     conf_error,time,lc[w].src_rate,lc[w].src_rate_err,newp,perror,'double_sm_bknpow',perror2,pvarunit,bestchisq,yfits,log=log,pmin0=pmin0,par=[2,3,4,5,6,7,8,9],delchi0=delchi0 ;,/doplot
  
  chisq=total(((lc[w].src_rate-yfit)/lc[w].src_rate_err)^2)
  dof=nw-8
  
;  b=[1,2,3,5,6,7]
  b=[2,3,4,5,6,7,8,9]
;  legend,[pname[b]+'='+sigfig(newp[b],3)+'!S!E+'+sigfig(perror2[1,b],3)+'!R!I-'+sigfig(perror2[0,b],3),!tsym.chi+'!U2!N/dof='+sigfig(chisq,5)+'/'+ntostr(dof)+'='+sigfig(chisq/dof,3)],box=0,/top,/right
  
  multiplot2,ydowngap=0.2
  chi=((lc[w].src_rate-yfit)/lc[w].src_rate_err)
  wneg=where(chi lt 0)
  chi=chi^2                     ;*chisq                   ;^2;-chisq
  chi[wneg]=chi[wneg]*(-1.)
  chierr=replicate(1.,nw)
  plot,[10,1e7],[-10,10],/nodata,xtitle='Time since BAT trigger (s)',ytitle=!tsym.chi+'!U2!N contrib.',/xlog
  oploterror,lc[w].time,chi,chierr,psym=3,/nohat
  for i=0,nw-1 do oplot,[lc[w[i]].tstart,lc[w[i]].tstop],[lc[w].src_rate,lc[w].src_rate]
  oplot,[10,1e8],[0,0];,color=!red
  
  multiplot2,/reset,/default
  if keyword_set(ps) then endplot
  
  stop
  return
end 
