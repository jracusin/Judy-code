pro replot_xrt_newlc,xrange=xrange,_extra=_extra,file=file,hardness=hardness,ps=ps,yrange=yrange,siglim=siglim
  
  if keyword_set(ps) and keyword_set(hard) then begplot,name='xrt_lc.ps',/color
  if keyword_set(ps) and not keyword_set(hard) then begplot,name='xrt_lc.ps',/color,/land
  
  simpctable
  if n_elements(file) eq 0 then begin
     file='lc_out.txt'
     if not exist(file) then file='lc_newout.txt'
  endif 
  
  readcol,file,time,tstart,tstop,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  
;  cts2=dblarr(n_elements(time))
;  for i=0,n_elements(time)-1 do begin
;     if sigma[i] lt 3 or cts[i] lt 0 then begin
;        bkgrt=bg[i]
;        confidlev,bkgrt,src[i],0.9973,smin,smax
;        cts2[i]=smax/expt[i]
;     endif 
;        cts2[i]=calc_3sig_ul(src[i],bg[i],expt[i],exp[i])
;  endfor 
;  w=where(cts2 ne 0,nw)
;  if nw gt 0 then cts[w]=cts2[w]
  lc=lcout2fits(file)
  back=lc.back_ctrate*lc.exptime;/lc.back_area_corr
  sigma=(lc.src_counts-back)/sqrt(lc.src_counts+back)
  
  if n_elements(siglim) eq 0 then siglim=3.
  w=where(sigma ge siglim,nw)
  wless=where(sigma lt siglim)
  print,sigma[wless]

  if keyword_set(hardness) then !p.multi=[0,1,2]
  if n_elements(xrange) eq 0 then xrange=[min(tstart),max(tstop)]
  if n_elements(yrange) eq 0 then begin 
     yrange=[min(cts-err),max(cts+err)]
     if yrange[0] lt 1e-5 then yrange[0]=1e-5
  endif
  plot,time,cts,psym=3,xtitle='seconds since BAT trigger',ytitle='cts/s',title=title,/xlog,/ylog,xrange=xrange,yrange=yrange,_extra=_extra,/nodata
  
  wwt=where(curr_ftype eq 0,nwwt)
  wpc=where(curr_ftype eq 1,nwpc)
  plotsym,0,0.7,/fill
  
  if nwpc gt 0 then begin 
     notwul=where(sigma[wpc] ge siglim,notnwul)
     oplot,time[wpc[notwul]],cts[wpc[notwul]],psym=8,color=!red
     for i=0,notnwul-1 do oplot,[tstart[wpc[notwul[i]]],tstop[wpc[notwul[i]]]],[cts[wpc[i]],cts[wpc[i]]],color=!red
     
;     oploterror,time[wpc],cts[wpc],err[wpc],psym=3,/nohat,color=!red,errcolor=!red
     oploterror,time[wpc[notwul]],cts[wpc[notwul]],err[wpc[notwul]],psym=3,/nohat,color=!red,errcolor=!red
     wul=where(sigma[wpc] lt siglim,nwul)
     if nwul gt 0 then begin 
        plotsym,1,5,thick=4
        for k=0,nwul-1 do begin 
           calc_3sig_ul,src[wpc[wul[k]]],back[wpc[wul[k]]],10,10,lc[wpc[wul[k]]].exptime,lc[wpc[wul[k]]].psf_corr,rate,raterr,sigmaul
           print,rate,raterr,sigmaul,lc[wpc[wul]].exptime
           cts[wpc[wul]]=rate
        endfor 
        oplot,time[wpc[wul]],cts[wpc[wul]],psym=8,color=!red
        oplot,[tstart[wpc[wul]],tstop[wpc[wul]]],[cts[wpc[wul]],cts[wpc[wul]]],color=!red
     endif 
  endif 
  

  if nwwt gt 0 then begin 
     plotsym,0,0.7,/fill
     notwul=where(sigma[wwt] ge siglim,notnwul)
     oplot,time[wwt[notwul]],cts[wwt[notwul]],psym=8,color=!blue
     for i=0,notnwul-1 do oplot,[tstart[wwt[notwul[i]]],tstop[wwt[notwul[i]]]],[cts[wwt[i]],cts[wwt[i]]],color=!blue
     oploterror,time[wwt[notwul]],cts[wwt[notwul]],err[wwt[notwul]],psym=3,/nohat,color=!blue,errcolor=!blue     
     
;     for i=0,nwwt-1 do oplot,[tstart[wwt[i]],tstop[wwt[i]]],[cts[wwt[i]],cts[wwt[i]]],color=!blue
;     oploterror,time[wwt],cts[wwt],err[wwt],psym=3,/nohat,color=!blue,errcolor=!blue
     wul=where(sigma[wwt] lt siglim,nwul)
     plotsym,1,5,thick=4
;     if nwul gt 0 then oplot,time[wwt[wul]],cts[wwt[wul]],psym=8,color=!blue

     if nwul gt 0 then begin 
        plotsym,1,5,thick=4
        for k=0,nwul-1 do begin 
           calc_3sig_ul,src[wwt[wul[k]]],back[wwt[wul[k]]],10,10,lc[wwt[wul[k]]].exptime,lc[wwt[wul[k]]].psf_corr,rate,raterr,sigmaul
           print,rate,raterr,sigmaul,lc[wwt[wul[k]]].exptime
           cts[wwt[wul]]=rate
        endfor 
        oplot,time[wwt[wul]],cts[wwt[wul]],psym=8,color=!blue
        oplot,[tstart[wwt[wul]],tstop[wwt[wul]]],[cts[wwt[wul]],cts[wwt[wul]]],color=!blue
     endif 
  endif 
  
  ;;;;;;;;;;;hardness plot
  if keyword_set(hardness) then begin
     plot,time,hard,psym=3,xtitle='seconds since BAT trigger',ytitle='hardness ratio',title=title,/xlog,/ylog,xrange=xrange,yrange=[min(hard-harderr),max(hard+harderr)],_extra=_extra,/nodata
       plotsym,0,0.7,/fill
  
  if nwpc gt 0 then begin 
     oplot,time[wpc],hard[wpc],psym=8,color=!red
     for i=0,nwpc-1 do oplot,[tstart[wpc[i]],tstop[wpc[i]]],[hard[wpc[i]],hard[wpc[i]]],color=!red
;     oploterror,time[wpc],hard[wpc],harderr[wpc],psym=3,/nohat,color=!red,errcolor=!red
     oploterror,time[wpc[notwul]],hard[wpc[notwul]],harderr[wpc[notwul]],psym=3,/nohat,color=!red,errcolor=!red
     wul=where(sigma[wpc] lt siglim,nwul)
     plotsym,1,5,thick=4
     if nwul gt 0 then oplot,time[wpc[wul]],hard[wpc[wul]],psym=8,color=!red
  endif 
  

  if nwwt gt 0 then begin 
     plotsym,0,0.7,/fill
     for i=0,nwwt-1 do oplot,[tstart[wwt[i]],tstop[wwt[i]]],[hard[wwt[i]],hard[wwt[i]]],color=!blue
     oploterror,time[wwt],hard[wwt],harderr[wwt],psym=3,/nohat,color=!blue,errcolor=!blue
     wul=where(sigma[wwt] lt siglim,nwul)
     plotsym,1,5,thick=4
     if nwul gt 0 then oplot,time[wwt[wul]],hard[wwt[wul]],psym=8,color=!blue
  endif 

     !p.multi=0
  endif 
     
  if keyword_set(ps) then endplot

end
