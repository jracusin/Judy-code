pro plot_like_qdp,t,yfit,name=name,sigma,ps=ps,file=file,siglim=siglim,_extra=_extra,multi=multi,noconfidlev=noconfidlev,pname=pname,charsize=charsize,lc=lc

;  if n_params() eq 0 then begin 
;     print,'syntax - plot_like_qdp,t,yfit,name,ps=ps,file=file'
;     return
;  end 
  
  if n_elements(pname) eq 0 then pname='lc_fit_like_qdp.ps'
  if n_elements(name) eq 0 then name=''
  if n_elements(siglim) eq 0 then siglim=3.;2.2
  if keyword_set(ps) then begplot,name=pname,/color,/land,font='helvetica'
  if n_elements(file) eq 0 then file='lc_newout.txt'
;  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sig,exp,junk1,curr_ftype,rate1,rate2,rate2,rate1err,rate2err,rate3err,hard1,hard2,hard1err,hard2err,/silent
  if n_elements(lc) eq 0 then lc=lcout2fits(file)
  time=lc.time
  tstarted=lc.tstart
  tstoped=lc.tstop
  cts=lc.src_rate
  err=lc.src_rate_err
  type=lc.type
  bg=lc.tot_back_cts
  src=lc.src_counts
  sig=lc.det_sig
  expt=lc.exptime
  
  
  timerr=((time-tstarted)+(tstoped-time))/2.
  type=fix(type)
  w=where(cts gt 0 and finite(err))
  time=time[w] & timerr=timerr[w] & cts=cts[w] & err=err[w] & type=type[w]
  bg=bg[w] & src=src[w] & expt=expt[w] & tstarted=tstarted[w] & tstoped=tstoped[w]
  sig=sig[w]
;  sigma=(src-bg*expt)/sqrt(src+bg*expt)
;  sigma=src/sqrt(src+bg*2)
  sigma=sig
;  sigma=sqrt(src)
;  sigma=sigma[w]
;  sigma=sig
;  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill

  if keyword_set(multi) then plotsym,0,0.4,/fill else plotsym,0,0.7,/fill
  
  if n_elements(yrange) eq 0 then yrange=[min(cts-err),max(cts+err)]
  winf=where(abs(err) gt 1e4,nwinf)
  if nwinf gt 0 then begin
     wninf=where(abs(err) lt 1e4)
     yrange=[min(cts[wninf]+err[wninf]),max(cts[wninf]+err[wninf])]
  endif 
  if yrange[0] lt 1e-4 then yrange[0]=1e-4

  xrange=[min(time-timerr),max(time+timerr)]
  logxr=alog10(xrange)
  xrange=[10.^(round(logxr[0]-0.5)),10.^(round(logxr[1]+0.5))]
  if n_elements(title) eq 0 then title='blue WT - red PC              '+name+'                                     '
  
  xorder=round(alog10(xrange[1])-alog10(xrange[0])+0.5)
  yorder=round(alog10(yrange[1])-alog10(yrange[0])+0.5)
 
  plot,xrange,yrange,/nodata,/xlog,/ylog,yrange=yrange,xrange=xrange,_extra=_extra,xmargin=[12,3],charsize=charsize,xtick_get=x,ytick_get=y,xtickname=replicate(' ',xorder+1),ytickname=replicate(' ',yorder+1),yminor=0,color=!white,xtickinterval=xrange[0],ytickinterval=1.;,xticks=xorder,yticks=yorder;,/xsty,/ysty
  
;  if x[0] gt (time[0]-timerr[0]) then x=[x[0]/10.,x]
;  if y[0] gt min(cts-err) then y=[y[0]/10.,y]
  
  xt=round(alog10(x))
  yt=round(alog10(y))
  xtickname='10!U'+ntostr(xt)+'!N'
  ytickname='10!U'+ntostr(yt)+'!N'

  wt=where(yt eq -1,nwt)
  if nwt gt 0 then ytickname[wt]='0.1'
  wt=where(yt eq 0,nwt)
  if nwt gt 0 then ytickname[wt]='1'
  wt=where(yt eq 1,nwt)
  if nwt gt 0 then ytickname[wt]='10'
  
  plot,xrange,yrange,/nodata,xtitle='Time since BAT trigger (s)',ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)',/xlog,/ylog,yrange=yrange,xrange=xrange,_extra=_extra,title=title,xmargin=[12,3],charsize=charsize,xtickname=xtickname,ytickname=ytickname,/noerase,yminor=9,xtickinterval=xrange[0],ytickinterval=1.;,xticks=xorder,yticks=yorder;,/xsty,/ysty;,xticks=xorder+1,yticks=yorder+1
  
;  plot,xrange,yrange,/nodata,/xlog,/ylog,xtitle='Time since BAT trigger (s)',ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)'
  
  ul=where(sigma lt siglim,nul)
  det=intarr(n_elements(time))
  if not keyword_set(noconfidlev) then begin 
     for u=0,nul-1 do begin
        confidlev,bg[ul[u]],src[ul[u]]+bg[ul[u]],0.9973,smin,smax
        if smin eq 0 then begin 
           cts[ul[u]]=smax/expt[ul[u]]
           det[ul[u]]=1
        endif 
     endfor
  endif 
  ul=where(det eq 1,nul)
  
  
  wt=where(type eq 0,nwt)
  if nwt gt 0 then $
     oploterror,time[wt],cts[wt],timerr[wt],err[wt],psym=8,/nohat,color=!blue,errcolor=!blue
  
  pc=where(type eq 1 and det ne 1,npc); and sigma ge siglim,npc)
  if npc gt 0 then $
     oploterror,time[pc],cts[pc],timerr[pc],err[pc],psym=8,/nohat,color=!red,errcolor=!red
  

;  ul=where(sigma lt siglim,nul)
  if not keyword_set(multi) then plotsym,1,5,thick=4 else plotsym,1,2,thick=4
  if nul gt 0 then begin 
;     if not keyword_set(noconfidlev) then begin 
;        for u=0,nul-1 do begin
;           confidlev,bg[ul[u]]*expt[ul[u]],src[ul[u]],0.9973,smin,smax
;           confidlev,bg[ul[u]],src[ul[u]]+bg[ul[u]],0.9973,smin,smax
;           cts[ul[u]]=smax/expt[ul[u]]
;        endfor
;     endif 
     wb=where(type[ul] eq 0,nwb)
     wr=where(type[ul] eq 1,nwr)

     if nwb gt 0 then begin 
        plots,time[ul[wb]],cts[ul[wb]],psym=8,color=!blue
        for u=0,nwb-1 do oplot,[tstarted[ul[wb[u]]],tstoped[ul[wb[u]]]],[cts[ul[wb[u]]],cts[ul[wb[u]]]],color=!blue
     endif 
     if nwr gt 0 then begin 
        plots,time[ul[wr]],cts[ul[wr]],psym=8,color=!red
        for u=0,nwr-1 do oplot,[tstarted[ul[wr[u]]],tstoped[ul[wr[u]]]],[cts[ul[wr[u]]],cts[ul[wr[u]]]],color=!red
     endif 
     colprint,time[ul],cts[ul],sigma[ul],sig[ul]
  endif 

  if n_elements(t) gt 2 then oplot,t,yfit,thick=3

;  if n_elements(w) gt 0 then begin
;     pc=where(type[w] eq 1,npc)
;     wt=where(type[w] eq 0,nwt)
;     color=!red
;     if nwt gt 0 then $
;        oploterror,time[w[wt]],cts[w[wt]],timerr[w[wt]],err[w[wt]],psym=3,/nohat,color=color,errcolor=color
;     if npc gt 0 then $
;        oploterror,time[w[pc]],cts[w[pc]],timerr[w[pc]],err[w[pc]],psym=8,/nohat,color=color,errcolor=color
;  endif 
  

  if keyword_set(ps) then endplot
;  stop
  return 
end 
