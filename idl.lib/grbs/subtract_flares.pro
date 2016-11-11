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

pro make_jon_files

  grbs=['GRB050502B','GRB090621A','GRB100902A','GRB121217A','GRB121229A','GRB100802A','GRB060904B','GRB060929','GRB070704','GRB100619A','GRB110820A','GRB121123A','GRB140108A','GRB110801A']
  ngrbs=n_elements(grbs)

  l=mrdfits('~/Swift/swift_grb_properties.fits',1)

  bindir=['bin5.0','bin2.5']
  for i=0,ngrbs-1 do begin
     print,grbs[i]
     for b=0,1 do begin 
        if not exist('~/stuff_for_people/Jon/outfiles/'+strtrim(grbs[i],2)+'_'+bindir[b]+'_sublc.txt') then begin 
           if not exist('~/stuff_for_people/Jon/'+bindir[b]+'/'+strtrim(grbs[i],2)) then spawn,'mkdir '+'~/stuff_for_people/Jon/'+bindir[b]+'/'+strtrim(grbs[i],2) else begin 
              cd,'~/stuff_for_people/Jon/'+bindir[b]+'/'+strtrim(grbs[i],2)
              w=where(strtrim(l.grb,2) eq strtrim(grbs[i],2))
              lc=lcout2fits()
              mc=mrdfits('~/GRBs/'+strtrim(grbs[i],2)+'/lc_fit_out_idl_int9_mc.fits',1)
    
              subtract_flares,lc,l[w].p,strtrim(l[w].model,2),strtrim(l[w].basemodel,2),mc,subflux=subflux,subferr=subferr,subtime=subtime,timebin=timebin,title=grbs[i]+' '+bindir[b]
              
              writecol,'~/stuff_for_people/Jon/outfiles/'+strtrim(grbs[i],2)+'_'+bindir[b]+'_sublc.txt',subtime,timebin,subflux,subferr[0,*],subferr[1,*],header='GRB  Time  Timebin  Subtracted_flux  Subflux_err_neg  Subflux_err_pos'

              k=get_kbrd(10)
              if k eq 's' then stop
           endelse 
        endif 
     endfor
  endfor  


  return
end 

pro subtract_flares,lc,p,mo,basemo,mc,subflux=f,subferr=ferr,title=title,subtime=subtime,timebin=timebin

  !p.multi=[0,1,2]
  !p.color=!black
  ploterror,lc.time,lc.src_rate,lc.src_rate_err,/nohat,yrange=[1e-4,1e4],psym=3,/xlog,xtitle='Time since trigger (s)',ytitle='Count Rate (cts/s)',/ylog,title=title
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]
  oplot,lc.time,call_function(mo,lc.time,p),color=!green
  oplot,lc.time,call_function(basemo,lc.time,p),color=!green,line=2

  f0=lc.src_rate-call_function(basemo,lc.time,p)

  read, x1, prompt='Flare Tstart? '
  read, x2, prompt='Flare Tstop? '
  oplot,[x1,x1],[1e-4,1e5],line=2,color=!red
  oplot,[x2,x2],[1e-4,1e5],line=2,color=!red

  w=where(lc.time ge x1 and lc.time lt x2)
  lc0=lc
  lc=lc0[w]
  f=f0[w]
  subtime=lc.time
  timebin=(lc.tstop-lc.tstart)/2.
  
  plot,lc.time,f,psym=3,xrange=xrange,yrange=yrange,xtitle='Time since trigger (s)',ytitle='Subtracted Count Rate (cts/s)'

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
     for j=0,n-1 do begin
        case basemo of 
           'pow': fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow])
           'bknpow': fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow1,mc[j].break,mc[j].pow2])
           'bkn2pow': fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow1,mc[j].break1,mc[j].pow2,mc[j].break2,mc[j].pow3])
           'bkn3pow': fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow1,mc[j].break1,mc[j].pow2,mc[j].break2,mc[j].pow3,mc[j].break3,mc[j].pow4])
           'bkn4pow': fs[j]=call_function(basemo,lc[i].time,[mc[j].norm,mc[j].pow1,mc[j].break1,mc[j].pow2,mc[j].break2,mc[j].pow3,mc[j].break3,mc[j].pow4,mc[j].break4,mc[j].pow5])          
        endcase 
     endfor 
     s=sort(fs)
     err[0,i]=fs[s[n16]]
     err[1,i]=fs[s[n84]]
  endfor 
  ferr=fltarr(2,n_elements(lc))
  ferr[0,*]=sqrt(lc.src_rate_err^2+err[0,*]^2)
  ferr[1,*]=sqrt(lc.src_rate_err^2+err[1,*]^2)
  for i=0,n_elements(lc)-1 do oplot,[lc[i].time,lc[i].time],[f[i]-ferr[0,i],f[i]+ferr[1,i]];,color=!green
  oplot,minmax(lc.time),[0,0],line=2,color=!green
  
  !p.multi=0

return
end 
