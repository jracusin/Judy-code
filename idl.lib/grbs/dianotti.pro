@fit_functions
@fit_functions_flares
pro corr_analysis,x,y,xerr,yerr,xtitle,ytitle,xlog=xlog,ylog=ylog
  lcolor=!red
  plotsym,0,1,/fill

  w=where(x-xerr[0,*] lt 0)
  xerr[0,w]=x[w]-1.
  w=where(y-yerr[0,*] lt 0)
  yerr[0,w]=y[w]-1.
  nw=n_elements(x)

  plot,x,y,psym=8,/xlog,/ylog,xtitle=xtitle,ytitle=ytitle
  myerr=dblarr(nw)
  for i=0,nw-1 do begin
     oplot,[x[i]-xerr[0,i],x[i]+xerr[1,i]],[y[i],y[i]]
     oplot,[x[i],x[i]],[y[i]-yerr[0,i],y[i]+yerr[1,i]]
     myerr[i]=mean(yerr[*,i])
  endfor 
  if keyword_set(xlog) then x=alog10(x)
  if keyword_set(ylog) then y=alog10(y)
  c=r_correlate(x,y,zd=zd,probd=probd)
  print,c,zd,probd
  r=regress(x,y,const=const,yfit=yfit,correlation=linearcorr,measure_error=myerr,sigma=sigma)
  line=(r[0]*x+const)
  oplot,10^x,10^yfit,color=lcolor
  mdist=fltarr(n_elements(x))
  for i=0,n_elements(x)-1 do begin
     dists=sqrt((x[i]-x)^2+(-y[i]-line)^2)
     mdist[i]=min(dists)
  endfor 
  plothist,mdist,x2,y2,bin=0.1,/noplot
  ga=gaussfit(x2,y2,a,nterms=3)
     
  plus=r[0]*x+const+a[2]*cos(r[0])*3.
  minus=r[0]*x+const-a[2]*cos(r[0])*3.

  s=sort(x)
  if keyword_set(xlog) then xx=10^x[s] else xx=x[s]
  if keyword_set(ylog) then begin 
     yy1=10^plus[s]
     yy2=10^minus[s]
  endif else begin
     yy1=plus[s]
     yy2=minus[s]
  endelse 
  oplot, xx,yy1, line=2,color=lcolor
  oplot, xx,yy2,line=2,color=lcolor
  legend,['R='+numdec(c[0],3)+'  p='+numdec(c[1],2,/sci)+'  slope='+numdec(1./r[0],2)+!tsym.plusminus+numdec((1./r[0])-(1./(r[0]+sigma[0])),2)+'  const='+numdec(const,2)],/top,/right,box=0,charsize=1.

  return
end

pro dianotti

  ;;; plot flux at end of plateau vs time of end of plateau
  g=mrdfits('~/stuff_for_people/Sam/lum_decay_corr.fits',1)
;  w=where(strpos(g.type,'II-III') ne -1 or strpos(g.type,'II-IV') ne
;  -1,nw) 
  plat=['0-I-II-III-IV','0-I-II-III','I-II-III','I-II-III-IV','II-III','II-IV','0-I-II-IV','I-II-IV']
  w=where(g.t90 gt 2)
  g=g[w]
  w=where(strtrim(g.type,2) eq plat[0] or $
          strtrim(g.type,2) eq plat[1] or $
          strtrim(g.type,2) eq plat[2] or $
          strtrim(g.type,2) eq plat[3] or $
          strtrim(g.type,2) eq plat[4] or $
          strtrim(g.type,2) eq plat[5] or $
          strtrim(g.type,2) eq plat[6] or $
          strtrim(g.type,2) eq plat[7],nw)
;; I-II-III, I-II-IV, II-III, II-IV, II-III-IV, I-II-III-IV

  g=g[w] ;; only keep bursts with plateau

  dflux=dblarr(nw)
  dfluxerr=dblarr(2,nw)
  dtime=dblarr(nw)
  dtimeerr=dblarr(2,nw)
  alpha=dblarr(nw)
  alphaerr=dblarr(2,nw)
     ;;;; using 1 sigma errors
  s1=round(1000.*(1-0.67)/2.)
  s2=round(1000.*(1.-(1-0.67)/2.))

  for i=0,nw-1 do begin 
     if g[i].p[1] gt g[i].p[3] then ind=4 else ind=2
     if strpos(g[i].type,'0-I') ne -1 then ind=6

     dtime[i]=g[i].p[ind]
     dtimeerr[*,i]=g[i].perr[*,ind]
     mo=fit_models(g[i].pnames,g[i].p,np,basemo=basemo)
     mo=basemo
     tmp=execute('dflux[i]='+mo+'(dtime[i],g[i].p)*g[i].unabs_cfratio')

     f=fltarr(1000)
     mcfile='~/GRBs/'+strtrim(g[i].grb,2)+'/lc_fit_out_idl_int8_mc.fits'
     if not exist(mcfile) then mcfile='~/GRBs/'+strtrim(g[i].grb,2)+'/lc_fit_out_idl_int7_mc.fits'
     if exist(mcfile) then begin 
        mcfit=mrdfits(mcfile,1)
        norm=mcfit.norm
        if np eq 4 then $
           tmp=execute('for j=0,999 do f[j]='+mo+'(dtime[i],[norm[j],mcfit[j].(1),mcfit[j].(2),mcfit[j].(3)])*g[i].unabs_cfratio')
        if np eq 6 then $
           tmp=execute('for j=0,999 do f[j]='+mo+'(dtime[i],[norm[j],mcfit[j].(1),mcfit[j].(2),mcfit[j].(3),mcfit[j].(4),mcfit[j].(5)])*g[i].unabs_cfratio')
        if np eq 8 then $
           tmp=execute('for j=0,999 do f[j]='+mo+'(dtime[i],[norm[j],mcfit[j].(1),mcfit[j].(2),mcfit[j].(3),mcfit[j].(4),mcfit[j].(5),mcfit[j].(6),mcfit[j].(7)])*g[i].unabs_cfratio')
        if np eq 10 then $
           tmp=execute('for j=0,999 do f[j]='+mo+'(dtime[i],[norm[j],mcfit[j].(1),mcfit[j].(2),mcfit[j].(3),mcfit[j].(4),mcfit[j].(5),mcfit[j].(6),mcfit[j].(7),mcfit[j].(8),mcfit[j].(9)])*g[i].unabs_cfratio')

        s=sort(f)
        dfluxerr[0,i]=f[s[500]]-f[s[s1]]
        dfluxerr[1,i]=f[s[s2]]-f[s[500]]

     endif 
     if strtrim(g[i].type,2) eq 'II-III' or strtrim(g[i].type,2) eq 'II-IV' or strtrim(g[i].type,2) eq 'II-III-IV' then aind=1
     if strpos(g[i].type,'I-II-') ne -1 then aind=3
     if strpos(g[i].type,'0-I') ne -1 then aind=5

     alpha[i]=g[i].p[aind]
     alphaerr[*,i]=g[i].perr[*,aind]

  endfor 

  begplot,name='~/stuff_for_people/Sam/dianotti.eps',/encap,font='helvetica',/color
  !p.multi=[0,1,3]
  lcolor=!red
  plotsym,0,1,/fill

  x=dtime
  y=dflux*g.lfact
  xerr=dtimeerr
  yerr=dfluxerr
  yerr[0,*]=yerr[0,*]*g.lfact
  yerr[1,*]=yerr[1,*]*g.lfact
  xtitle='t!Lplateau end!N (s)'
  ytitle='L (t!Lplateau end!N) (erg/s)'
  corr_analysis,x,y,xerr,yerr,xtitle,ytitle,/xlog,/ylog



;;   plot,dtime,dflux*g.lfact,psym=8,/xlog,/ylog,xtitle='t!Lplateau end!N (s)',ytitle='L (t!Lplateau end!N) (erg/s)'
;;   myerr=dblarr(nw)
;;   for i=0,nw-1 do begin
;;      oplot,[dtime[i]-dtimeerr[0,i],dtime[i]+dtimeerr[1,i]],[dflux[i],dflux[i]]*g[i].lfact
;;      yerr=[dflux[i]-dfluxerr[0,i],dflux[i]+dfluxerr[1,i]]*g[i].lfact
;;      if yerr[0] lt 0 then yerr[0]=dflux[i]-1d44
;;      oplot,[dtime[i],dtime[i]],yerr
;;      myerr[i]=mean(dfluxerr[*,i])*g[i].lfact
;;      if dflux[i]-myerr[i] lt 0 then myerr[i]=dflux[i]*g[i].lfact-1d44
;;   endfor 
;;   x=alog10(dtime)
;;   y=alog10(dflux*g.lfact)
;;   c=r_correlate(x,y,zd=zd,probd=probd)
;;   print,c,zd,probd
;;   r=regress(x,y,const=const,yfit=yfit,correlation=linearcorr,measure_error=myerr,sigma=sigma)
;;   line=(r[0]*x+const)
;;   oplot,10^x,10^yfit,color=lcolor
;;   mdist=fltarr(n_elements(x))
;;   for i=0,n_elements(x)-1 do begin
;;      dists=sqrt((x[i]-x)^2+(-y[i]-line)^2)
;;      mdist[i]=min(dists)
;;   endfor 
;;   plothist,mdist,x2,y2,bin=0.1,/noplot
;;   ga=gaussfit(x2,y2,a,nterms=3)
     
;;   plus=r[0]*x+const+a[2]*cos(r[0])*3.
;;   minus=r[0]*x+const-a[2]*cos(r[0])*3.

;;   s=sort(x)
;;   oplot, 10^x[s],10^plus[s], line=2,color=lcolor
;;   oplot, 10^x[s],10^minus[s],line=2,color=lcolor
;;   legend,['R='+numdec(c[0],3)+'  p='+numdec(c[1],2,/sci)+'  slope='+numdec(1./r[0],2)+!tsym.plusminus+numdec((1./r[0])-(1./(r[0]+sigma[0])),2)+'  const='+numdec(const,2)],/top,/right,box=0,charsize=1.


  plot,dtime,alpha,psym=8,/xlog,xtitle='t!Lplateau end!N (s)',ytitle=!tsym.alpha+'!Lplateau!N',yrange=[-2,3],/ysty
  yerr=dblarr(nw)
  for i=0,nw-1 do begin
     oplot,[dtime[i]-dtimeerr[0,i],dtime[i]+dtimeerr[1,i]],[alpha[i],alpha[i]]
     oplot,[dtime[i],dtime[i]],[alpha[i]-alphaerr[0,i],alpha[i]+alphaerr[1,i]]
     yerr[i]=mean(alphaerr[*,i])
  endfor 

  x=alog10(dtime)
  y=alpha
  c=r_correlate(x,y,zd=zd,probd=probd)
  print,c,zd,probd
  r=regress(x,y,const=const,yfit=yfit,correlation=linearcorr,measure_error=yerr,sigma=sigma)
  line=(r[0]*x+const)
  oplot,10^x,yfit,color=lcolor
  mdist=fltarr(n_elements(x))
  for i=0,n_elements(x)-1 do begin
     dists=sqrt((x[i]-x)^2+(-y[i]-line)^2)
     mdist[i]=min(dists)
  endfor 
  plothist,mdist,x2,y2,bin=0.1,/noplot
  ga=gaussfit(x2,y2,a,nterms=3)
     
  plus=r[0]*x+const+a[2]*cos(r[0])*3.
  minus=r[0]*x+const-a[2]*cos(r[0])*3.
  
  s=sort(x)
  oplot, 10^x[s],plus[s], line=2,color=lcolor
  oplot, 10^x[s],minus[s],line=2,color=lcolor
  legend,['R='+numdec(c[0],3)+'  p='+numdec(c[1],2,/sci)+'  slope='+numdec(1./r[0],2)+!tsym.plusminus+numdec((1./r[0])-(1./(r[0]+sigma[0])),2)+'  const='+numdec(const,2)],/top,/right,box=0,charsize=1.

  lum=g.lum_avg2
  w=where(lum eq 0,nw)
  lum[w]=g[w].lum_avg
  lumerr=g.lum_avg2_err
  lumerr[*,w]=g[w].lum_avg_err
  q=where(lum-lumerr[0,*] lt 0)
  lumerr[0,q]=lum[q]-1d44
  plot,lum,dflux*g.lfact,psym=8,/xlog,/ylog,xtitle='L!L200!N (erg/s)',ytitle='L (t!Lplateau end!N) (erg/s)'
  myerr=dblarr(nw)
  for i=0,nw-1 do begin
     oplot,[lum[i]-lumerr[0,i],lum[i]+lumerr[1,i]],[dflux[i],dflux[i]]*g[i].lfact
     yerr=[dflux[i]-dfluxerr[0,i],dflux[i]+dfluxerr[1,i]]*g[i].lfact
     if yerr[0] lt 0 then yerr[0]=dflux[i]-1d44
     oplot,[lum[i],lum[i]],yerr
     myerr[i]=mean(dfluxerr[*,i])*g[i].lfact
  endfor 
  x=alog10(lum)
  y=alog10(dflux*g.lfact)
  c=r_correlate(x,y,zd=zd,probd=probd)
  print,c,zd,probd
  r=regress(x,y,const=const,yfit=yfit,correlation=linearcorr,measure_error=myerr,sigma=sigma)
  line=(r[0]*x+const)
  oplot,10^x,10^yfit,color=lcolor
  mdist=fltarr(n_elements(x))
  for i=0,n_elements(x)-1 do begin
     dists=sqrt((x[i]-x)^2+(-y[i]-line)^2)
     mdist[i]=min(dists)
  endfor 
  plothist,mdist,x2,y2,bin=0.1,/noplot
  ga=gaussfit(x2,y2,a,nterms=3)
     
  plus=r[0]*x+const+a[2]*cos(r[0])*3.
  minus=r[0]*x+const-a[2]*cos(r[0])*3.

  s=sort(x)
  oplot, 10^x[s],10^plus[s], line=2,color=lcolor
  oplot, 10^x[s],10^minus[s],line=2,color=lcolor
  legend,['R='+numdec(c[0],3)+'  p='+numdec(c[1],2,/sci)+'  slope='+numdec(1./r[0],2)+!tsym.plusminus+numdec((1./r[0])-(1./(r[0]+sigma[0])),2)+'  const='+numdec(const,2)],/top,/right,box=0,charsize=1.


  !p.multi=0
  endplot
  spawn,'ps2pdf ~/stuff_for_people/Sam/dianotti.eps ~/stuff_for_people/Sam/dianotti.pdf'
  
     stop

return
end 
