;@fit_functions
@fit_lc
pro replot_dave_cxo_cycle13

  cd,'~/GRBs/GRB091127'
  begplot,name='~/Chandra/GRB091127_lc_w_chandra.ps',/land,/color
  spec=mrdfits('UL_specfits.fits',1)                                                 
  cfratio=spec[1].unabs_cfratio
;  plot_like_qdp,flux=spec[1].unabs_cfratio,/phil,xrange=[1e3,1e8],yrange=[1e-15,1e-9],lc=lc
  lc=lcout2fits(/phil) 
  newp=[2.5e-6,1.08,30994.,1.53]
  breaks=1
  erase
  multiplot2,[1,2],/init
  multiplot2
  xrange=[1e3,1e8]
  plot_like_qdp,flux=cfratio,/phil,xrange=xrange,yrange=[1e-15,1e-9],lc=lc,xtitle=' '
  plotsym,0,1,/fill    
  oploterror,[8.5e6,1.6e7],[7.5e-15,3.5e-15],[1.5e-15,7.5e-16],psym=8,color=!green,/nohat,errcolor=!green
  t=[lc.time,30994.,1.6e7]    
  t=t[sort(t)]  
  f=bknpow(t,newp)
  oplot,t,f 

  clc=lc[0:1]
  clc.src_rate=[7.5e-15,3.5e-15]/cfratio
  clc.src_rate_err=[1.5e-15,7.5e-16]/cfratio
  clc.time=[8.5e6,1.6e7]
  clc.tstart=clc.time-3e5
  clc.tstop=clc.time+3e5
  concat_structs,lc,clc,tmp
  lc=tmp

  plot_lcfit_residuals,lc,newp,breaks,wdet=wdet,xrange=xrange,charsize=charsize,flux=cfratio

  multiplot2,/reset,/default
  endplot
;  yfit=f
;  res=lc.src_rate/yfit*flux
;  reserr=lc.src_rate_err/yfit*flux  

  stop
return
end 
