@fit_functions
pro jbtimerange

  cd,'~/Chandra/GRB141028A'
  begplot,name='tbreak_fit.ps',/color
  lc=lcout2fits(/chandra)
  read_lcfit,'lc_fit_comb.dat',pnames,p,perror
  t=logarr(8e4,1e6,bin=0.01)
  nt=n_elements(t)
  mo=fit_models(pnames,p,np,nf)
  
  !p.multi=[0,1,2]
  chisq=fltarr(nt)
  wdet=where(lc.src_rate_err gt 0)
  plot_like_qdp,lc=lc
  for i=0,nt-1 do begin
     newp=[p[0:1],t[i],p[3]]
     yfit=call_function(mo,lc.time,newp)
     chisq[i]=total((lc[wdet].src_rate-yfit[wdet])^2/lc[wdet].src_rate_err^2)
;     oplot,lc.time,yfit,line=2
  endfor 


  mchisq=min(chisq,m)
  print,t[m]
  w=where(t lt t[m])
  low=interpol(t[w],chisq[w],mchisq+2.706)
  w=where(t gt t[m])
  high=interpol(t[w],chisq[w],mchisq+2.706)

  t2=logarr(1e4,3e6,bin=0.1)

  newp=[p[0:1],low,p[3]]
  yfit=call_function(mo,t2,newp)
  oplot,t2,yfit,line=2

  newp=[p[0:1],high,p[3]]
  yfit=call_function(mo,t2,newp)
  oplot,t2,yfit,line=2


  plot,t,chisq-mchisq,/xlog,yrange=[0,5],xtitle='Time (s)',ytitle=!tsym.delta_cap+!tsym.chi+'!U2!N'
  oplot,[low,low],[0,20],line=2
  oplot,[high,high],[0,20],line=2
  legend,['90% confidence','low='+numdec(low,2,/sci,/idl)+' s','high='+numdec(high,2,/sci,/idl)+' s'],/top,/left,box=0

  !p.multi=0
  endplot
  spawn,'ps2pdf tbreak_fit.ps tbreak_fit.pdf'
  stop


  return
end 

pro grb141028A

  begplot,name='~/GRBs/GRB141028A/opt_x_lc.ps',/land,/color
  ;;; xray
  cd,'~/Chandra/GRB141028A'
  lc=lcout2fits(/chandra)
  spec=mrdfits('UL_specfits.fits',1)

  fdensfact=flux2jy(1.,2.04)*spec[0].unabs_cfratio*1e3
  flux=lc.src_rate*fdensfact
  fluxerr=lc.src_rate_err*fdensfact
  terr=rotate([[lc.time-lc.tstart],[lc.tstop-lc.time]],4)

  ploterror2,lc.time,flux,terr,fluxerr,psym=3,/xlog,/ylog,xtitle='Time since Trigger (s)',ytitle='Flux Density (mJy)',xrange=[3e4,3e6],/xsty,yrange=[5e-8,1e-1],/ysty,charsize=2.,yminor=9
  ul=where(lc.src_rate_err eq 0)
  plotsym,1,2,thick=5
  plots,lc[ul].time,flux[ul],psym=8

  n=n_elements(lc)
  plots,lc[n-1].time,flux[n-1],color=!green,psym=4
  oplot,[lc[n-1].tstart,lc[n-1].tstop],[flux[n-1],flux[n-1]],color=!green
  oplot,[lc[n-1].time,lc[n-1].time],[flux[n-1]-fluxerr[n-1],flux[n-1]+fluxerr[n-1]],color=!green

  read_lcfit,'lc_fit_comb.dat',pnames,p,perror
  t=logarr(3e4,3e7,bin=0.1)
  oplot,t,bknpow(t,p)*fdensfact,line=2

  writecol,'~/GRBs/GRB141028A/xray_lc_fluxdens_mjy.txt',lc.time,lc.tstart,lc.tstop,flux,fluxerr,header='  time      tstart     tstop    fluxdens    fluxdenserr'


  ;;;; optical
  readcol,'~/GRBs/GRB141028A/Judy_ilc.txt',itime,iflux,ierror,skip=1
  readcol,'~/GRBs/GRB141028A/Judy_rlc.txt',rtime,rflux,rerror,skip=1
;  j=[0,1,2,4,5]
;  rtime=rtime[j]
;  rflux=rflux[j];*1e-3
;  rerror=rerror[j];*1e-3
;  iflux=iflux*1e-3
;  ierror=ierror*1e-3

  plotsym,0,1,/fill
  oploterror,itime,iflux,ierror,psym=8,color=!red,/nohat,errcolor=!red
  oplot,rtime,rflux,psym=8,color=!blue
  for i=0,n_elements(rtime)-1 do begin
     oplot,[rtime[i],rtime[i]],[rflux[i]-rerror[i],rflux[i]+rerror[i]],color=!blue
  endfor 

  ip=mpfitfun('pow',itime,iflux,ierror,[1.,1.])
  rp=mpfitfun('pow',rtime,rflux,rerror,[1.,1.])
  
  oplot,t,pow(t,ip),color=!red,line=1
  oplot,t,pow(t,rp),color=!blue,line=1

  parinfo = parinfo_struct(4)

  parinfo[1:*].fixed=1
  ip2=mpfitfun('bknpow',itime,iflux,ierror,p,parinfo=parinfo)
  rp2=mpfitfun('bknpow',rtime,rflux,rerror,p,parinfo=parinfo)
  
;  oplot,t,bknpow(t,[p[0],p[1:3]])*fdensfact,color=!red,line=1
  oplot,t,bknpow(t,ip2),color=!red,line=2
  oplot,t,bknpow(t,rp2),color=!blue,line=2

  print,ip,rp;,ip2,rp2

  legend,['XRT','Chandra','i band','r band'],box=0,/top,/right,psym=[1,4,8,8],color=[!p.color,!green,!red,!blue]

  endplot
  spawn,'ps2pdf ~/GRBs/GRB141028A/opt_x_lc.ps ~/GRBs/GRB141028A/opt_x_lc.pdf'

  print,jet_angle(p[2]/86400,z=2.332,eiso=1d55,n=100.,eta=1.)

stop

return
end 
