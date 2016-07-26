@fit_lc
pro mc_movie,grb
  
  cd,'~/GRBs/'+grb
  lc=lcout2fits()
  read_lcfit,'lc_fit_out_idl_int9.dat',pnames,p,perror,chisq,dof,breaks,np=np,nf=nf
  begplot,name='mc_movie/mc_plots.ps',/land,/color
  lc_monte_pow,lc,p,pnames,outdir='mc_movie/',nsim=1000,breaks=breaks,/nowrite,/ps
  endplot
  
  stop
;convert -delay 100 -dispose Background mc_plots.ps -loop 0 mc_plots.gif
  return
end 

pro subtract_flares,lc,p,mo,basemo,mc,xrange=xrange,yrange=yrange

  ploterror,lc.time,lc.src_rate,lc.src_rate_err,/nohat,xrange=xrange,/xsty,yrange=yrange,psym=3,/xlog
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]
  oplot,lc.time,call_function(mo,lc.time,p),color=!green
  oplot,lc.time,call_function(basemo,lc.time,p),color=!green,line=2

  stop
  f=lc.src_rate-call_function(basemo,lc.time,p)

  plot,lc.time,f,psym=3,xrange=xrange,yrange=yrange
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[f[i],f[i]]
  
  ;;1 sigma
  n=n_elements(mc)
  conf=0.68
  conf1=(1.-conf)/2.
  conf2=(1.-conf)/2.+conf

  n16=round(n*conf1)-1
  n84=round(n*conf2)-1

  err=fltarr(2,n_elements(f))
  for i=0,n_elements(lc)-1 do begin
     fs=fltarr(n)
     for j=0,n_elements(f)-1 do begin
        fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow1,mc[j].break1,mc[j].pow2,mc[j].break2,mc[j].pow3])
     endfor 
     s=sort(fs)
     err[0,i]=fs[s[n16]]
     err[1,i]=fs[s[n84]]
  endfor 
  ferr=fltarr(2,n_elements(lc))
  ferr[0,*]=sqrt(lc.src_rate_err^2+err[0,*]^2)
  ferr[1,*]=sqrt(lc.src_rate_err^2+err[1,*]^2)
  for i=0,n_elements(lc)-1 do oplot,[lc[i].time,lc[i].time],[f[i]-ferr[0,i],f[i]+ferr[1,i]];,color=!green
  oplot,xrange,[0,0],line=2,color=!green

stop
return
end 
