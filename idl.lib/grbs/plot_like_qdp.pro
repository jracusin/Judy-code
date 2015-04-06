pro plot_like_qdp,t,yfit,name=name,sigma,ps=ps,file=file,siglim=siglim,_extra=_extra,multi=multi,noconfidlev=noconfidlev,pname=pname,charsize=charsize,lc=lc,flux=flux,phil=phil,pmulti=pmulti,arrowsize=arrowsize,symsize=symsize,xtitle=xtitle,noxaxis=noxaxis,ytitle=ytitle,nocolor=nocolor,xrange=xrange,xtickname=xtickname,qdp=qdp,yrange=yranges,uvot=uvot,withbat=withbat,chandra=chandra

;  if n_params() eq 0 then begin 
;     print,'syntax - plot_like_qdp,t,yfit,name,ps=ps,file=file'
;     return
;  end 
  
  if n_elements(pname) eq 0 then pname='lc_fit_like_qdp.ps'
  if n_elements(name) eq 0 then name=''
  red=!p.color & blue=red & green=red & cyan=red & magenta=red & purple=red
  if not keyword_set(nocolor) then begin
     red=!red
     blue=!blue
     green=!green
     magenta=!magenta
     purple=!purple
     cyan=!cyan
     salmon=!salmon
  endif 
;  if n_elements(file) eq 0 then file='lc_newout.txt'
;  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sig,exp,junk1,curr_ftype,rate1,rate2,rate2,rate1err,rate2err,rate3err,hard1,hard2,hard1err,hard2err,/silent
  if n_elements(lc) eq 0 then lc=lcout2fits(file,phil=phil,qdp=qdp,uvot=uvot,withbat=withbat,chandra=chandra)
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

  useflux=0
  if n_elements(flux) ne 0 then begin
     if flux ne 1. then begin 
        cts=lc.src_rate*flux
        err=lc.src_rate_err*flux
        if n_elements(ytitle) eq 0 then ytitle='Flux (0.3-10.0 keV) (erg cm!U-2!N s!U-1!N)'
        useflux=1
     endif else if n_elements(ytitle) eq 0 then ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)'  
  endif else if n_elements(ytitle) eq 0 then ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)'  

  timerr=((time-tstarted)+(tstoped-time))/2.
  type=fix(type)
  w=where(cts gt 0 and finite(err))
  time=time[w] & timerr=timerr[w] & cts=cts[w] & err=err[w] & type=type[w] 
  bg=bg[w] & src=src[w] & expt=expt[w] & tstarted=tstarted[w] & tstoped=tstoped[w]
  sig=sig[w]
;  sigma=(src-bg*expt)/sqrt(src+bg*expt)
;  sigma=src/sqrt(src+bg*2)
  sigma=sig
;  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill
  
  if not keyword_set(symsize) then symsize=0.7 
  if keyword_set(multi) then plotsym,0,0.4,/fill else plotsym,0,symsize,/fill
  
  wdet=where(err gt 0,nwdet)
  wnodet=where(err lt 0.,nwnodet)
  if nwnodet eq 0 then wnodet=0
  if n_elements(yranges) eq 0 then begin 
     if nwdet eq 0 then yrange=[cts[0],cts[0]] else $
        if n_elements(yranges) eq 0 then yrange=[min([cts[wdet]-err[wdet],cts[wnodet]]),max(cts[wdet]+err[wdet])] else yrange=yranges
     yrange=[10d^round(alog10(yrange[0])-0.5),10d^round(alog10(yrange[1])+0.5)]
     winf=where(abs(err) gt 1e5,nwinf)
     if nwinf gt 0 then begin
        wninf=where(abs(err) lt 1e4)
        yrange=[min(cts[wninf]+err[wninf]),max(cts[wninf]+err[wninf])]
        yrange=[10^round(alog10(yrange[0])-0.5),10^round(alog10(yrange[1])+0.5)]
     endif 
;     if yrange[0] lt 1e-4 and not useflux then yrange[0]=1e-5
  endif else yrange=yranges

  if n_elements(xrange) eq 0 then begin 
     xrange=[min(time-timerr),max(time+timerr)]
     if xrange[0] lt 10 and not keyword_set(withbat) then xrange[0]=10.
     if xrange[0] lt 10 and keyword_set(withbat) then xrange[0]=1e-1
     logxr=alog10(xrange)
     xrange=[10.^(round(logxr[0]-0.5)),10.^(round(logxr[1]+0.5))]
  endif 
  if n_elements(title) eq 0 then title='blue WT - red PC              '+name+'                                     '
;  if yrange[1] gt 1e8 then yrange[1]=1e8
  if yrange[1] lt yrange[0] then yrange[1]=yrange[0]
  
;endif 

  xorder=round(alog10(xrange[1])-alog10(xrange[0]));+0.5)
  yorder=round(alog10(yrange[1])-alog10(yrange[0]));+0.5)
;  if yrange[1] gt 1 then yorder=yorder+1 else yorder=yorder-1
  print,xrange,yrange,yorder  
;stop
  if yorder eq 1 then yorder=yorder+1
  if xorder eq 1 then xorder=xorder+1
  if n_elements(pmulti) gt 0 then !p.multi=pmulti
  
  if keyword_set(ps) then begplot,name=pname,/color,/land,font='helvetica'
  if n_elements(xtitle) eq 0 then xtitle='Time since BAT trigger (s)' 

  plot,xrange,yrange,/nodata,/xlog,/ylog,yrange=yrange,xrange=xrange,_extra=_extra,xmargin=[15,2],charsize=charsize,xtitle=xtitle,ytitle=ytitle,/xsty;,yminor=9,yticks=yorder,xminor=9;,xtickf='loglabels',ytickf='loglabels'

;,xtick_get=x,ytick_get=y,xtickname=replicate(' ',xorder+1),ytickname=replicate(' ',yorder+3),yminor=0,xtickinterval=xrange[0],ytickinterval=1.
;  erase
;  axis,yrange[0],/yaxis,ytick_get=y,ytickname=replicate(' ',yorder+1),yminor=0.,ytickinterval=1.,xmargin=[12,3],xrange=xrange,/xlog
;  axis,xrange[0],/xaxis,xtick_get=x,xtickname=replicate(' ',xorder+1),xtickinterval=xrange[0],yrange=yrange,/ylog
  
;  xt=round(alog10(x))
;  yt=round(alog10(y))
;  xtickname='10!U'+ntostr(xt)+'!N'
;  ytickname='10!U'+ntostr(yt)+'!N'

;  wt=where(yt eq -1,nwt)
;  if nwt gt 0 then ytickname[wt]='0.1'
;  wt=where(yt eq 0,nwt)
;  if nwt gt 0 then ytickname[wt]='1'
;  wt=where(yt eq 1,nwt)
;  if nwt gt 0 then ytickname[wt]='10'
  
;  plot,xrange,yrange,/nodata,xtitle='Time since BAT trigger (s)',ytitle=ytitle,/xlog,/ylog,yrange=yrange,xrange=xrange,_extra=_extra,title=title,xmargin=[12,3],charsize=charsize,xtickname=xtickname,ytickname=ytickname,yminor=9,xtickinterval=xrange[0],ytickinterval=1.,/noerase
  

;  if not keyword_set(noxaxis) then $
;     axis,xrange[0],xtickname=xtickname,xtickinterval=xrange[0],xrange=xrange,xtitle=xtitle,xaxis=0,xmargin=[12,3],/xlog,charsize=charsize ;,/noerase
;  axis,ytickname=ytickname,yminor=9,ytickinterval=1.,yaxis=0,yrange=yrange,ytitle=ytitle,/ylog,charsize=charsize
  
;  plot,xrange,yrange,/nodata,/xlog,/ylog,xtitle='Time since BAT trigger (s)',ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)'
  
  det=intarr(n_elements(err))
  det[*]=1
  
  if nwdet gt 0 then begin 
     det[wdet]=0
     bat=where(type eq 3 and det ne 1 and time gt 0, nbat)
     if nbat gt 0 then begin
        wbat0=where(tstarted[bat] lt 0,nwbat0)
        if nwbat0 gt 0 then tstarted[bat[wbat0]]=xrange[0]
        oploterror,time[bat],cts[bat],err[bat],psym=3,/nohat,color=magenta,errcolor=magenta
        for z=0,nbat-1 do oplot,[tstarted[bat[z]],tstoped[bat[z]]],[cts[bat[z]],cts[bat[z]]],color=magenta
     endif 
     wt=where(type eq 0,nwt)
     if nwt gt 0 then begin 
        oploterror,time[wt],cts[wt],err[wt],psym=3,/nohat,color=blue,errcolor=blue
        for z=0,nwt-1 do oplot,[tstarted[wt[z]],tstoped[wt[z]]],[cts[wt[z]],cts[wt[z]]],color=blue
     endif 
     
     pc=where(type eq 1 and det ne 1,npc)
     if npc gt 0 then begin
        oploterror,time[pc],cts[pc],err[pc],psym=3,/nohat,color=red,errcolor=red
        for z=0,npc-1 do oplot,[tstarted[pc[z]],tstoped[pc[z]]],[cts[pc[z]],cts[pc[z]]],color=red
     endif 

     cxo=where(type eq 2 and det ne 1,ncxo)
     if ncxo gt 0 then begin
        plots,time[cxo],cts[cxo],psym=8,color=purple
        oploterror,time[cxo],cts[cxo],err[cxo],psym=8,/nohat,color=purple,errcolor=purple
        for z=0,ncxo-1 do oplot,[tstarted[cxo[z]],tstoped[cxo[z]]],[cts[cxo[z]],cts[cxo[z]]],color=purple
     endif 

     wuvot=where(type eq 4 and det ne 1,nuvot)
     if nuvot gt 0 then begin
        oploterror,time[wuvot],cts[wuvot],err[wuvot],psym=3,/nohat,color=purple,errcolor=purple
        for z=0,nuvot-1 do oplot,[tstarted[wuvot[z]],tstoped[wuvot[z]]],[cts[wuvot[z]],cts[wuvot[z]]],color=purple
     endif      

     wts=where(type eq 5,nwts)
     if nwts gt 0 then begin 
        oploterror,time[wts],cts[wts],err[wts],psym=3,/nohat,color=cyan,errcolor=cyan
        for z=0,nwts-1 do oplot,[tstarted[wts[z]],tstoped[wts[z]]],[cts[wts[z]],cts[wts[z]]],color=cyan     
     endif 
  endif 
  
  if not keyword_set(arrowsize) then arrowsize=3
  if not keyword_set(multi) then plotsym,1,arrowsize,thick=4 else plotsym,1,2,thick=4
  
  ul=where(err le 0,nul)

  if nul gt 0 then begin 
     wb=where(type[ul] eq 0,nwb)
     wr=where(type[ul] eq 1,nwr)
     wg=where(type[ul] eq 2,nwg)

     if nwb gt 0 then begin 
        plots,time[ul[wb]],cts[ul[wb]],psym=8,color=blue
        for u=0,nwb-1 do oplot,[tstarted[ul[wb[u]]],tstoped[ul[wb[u]]]],[cts[ul[wb[u]]],cts[ul[wb[u]]]],color=blue
     endif 
     if nwr gt 0 then begin 
        plots,time[ul[wr]],cts[ul[wr]],psym=8,color=red
        for u=0,nwr-1 do oplot,[tstarted[ul[wr[u]]],tstoped[ul[wr[u]]]],[cts[ul[wr[u]]],cts[ul[wr[u]]]],color=red
     endif 
     if nwg gt 0 then begin 
        plots,time[ul[wg]],cts[ul[wg]],psym=8,color=purple
        for u=0,nwg-1 do oplot,[tstarted[ul[wg[u]]],tstoped[ul[wg[u]]]],[cts[ul[wg[u]]],cts[ul[wg[u]]]],color=purple
     endif 
;     colprint,time[ul],cts[ul],sigma[ul],sig[ul]
  endif 

  if n_elements(t) gt 2 then oplot,t,yfit,thick=3
  
  if keyword_set(ps) then endplot
;stop
  return 
end 
