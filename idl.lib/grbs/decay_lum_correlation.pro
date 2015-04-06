@fit_lc
@swift_lat_pop_studies
pro test_correlation,x,y,xerr,yerr,z,_extra=_extra

  ;;w1=where(g.t90 gt 2. and g.tstart lt g.t200 and g.alpha_avg lt 3)

  ;;; assumes xlog 
  w=where(x-xerr[0,*] lt 0.)
  xerr[0,w]=x[w]-1e25

  w=where(y ne 0 and x gt 0 and finite(yerr[0,*]) and finite(xerr[0,*]),nw)
  plotsym,0,1
  if n_elements(xrange) eq 0 then begin 
     xrange=round(alog10(prange(x[w],xerr[0,w]))+0.5)
     xticks=xrange[1]-xrange[0]
     xrange=10d^xrange
  endif 

  ploterror2,x[w],y[w],xerr[*,w],yerr[*,w],psym=8,/xlog,/nohat,charsize=2,_extra=_extra,xminor=9,xticks=xticks,/xsty,xrange=xrange

  fitexy,alog10(x[w]),y[w],x_sig=alog10(x[w])-alog10(x[w]-xerr[0,w]),y_sig=yerr[0,w],a,b
  line=alog10(x[w])*b+a
  oplot,x[w],line;,color=!green
  print,'slope=',b
  print,'constant=',a
  c=r_correlate(alog10(x[w]),y[w])
  rsig=mpnormlim(c[1],/SLEVEL)
  print,'Spearman rank coeff & prob & sig: ',c[0],c[1],rsig
  pr=pr_correlate(x,y,z,/silent)
  prsig=mpnormlim(pr[1],/SLEVEL)
  print,'partial spearman rank coeff & prob & sig: ',pr[0],pr[1],prsig

  a0=a
  b0=b
  
  aa=fltarr(1e4) & bb=fltarr(1e4)
  n=nw
  for o=0,1e4-1 do begin
     r=round(randomu(seed,n)*n)
     fitexy,alog10(x[w[r]]),y[w[r]],x_sig=alog10(x[w[r]])-alog10(x[w[r]]-xerr[0,w[r]]),y_sig=yerr[0,w[r]],a,b
     aa[o]=a
     bb[o]=b
  endfor 
  a_err=mc_error(aa,sig=1.)
  b_err=mc_error(bb,sig=1.)
  
  mdist=fltarr(nw)
  for i=0,nw-1 do begin
     dists=sqrt((alog10(x[i])-alog10(x))^2+(-y[i]-line)^2)
     mdist[i]=min(dists)
  endfor 
  plothist,mdist,x2,y2,bin=0.1,/noplot
;  ga=gaussfit(x2,y2,a1,nterms=3)
  am=sqrt(mean((mdist-mean(mdist))^2))
  plus=b0*alog10(x)+a0+am*cos(b0)*2.  ;;; 2 sigma error
  minus=b0*alog10(x)+a0-am*cos(b0)*2.
  s=sort(x)
  oplot, x[s],plus[s], line=2;,color=!green
  oplot, x[s],minus[s],line=2;,color=!green

  legend,['slope='+numdec(b0,2)+'!S!D-'+numdec(b_err[0],2)+'!R!U+'+numdec(b_err[1],2)+'!N','const='+numdec(a0,2)+'!S!D-'+numdec(a_err[0],2)+'!R!U+'+numdec(a_err[1],2)+'!N','R!Dsp!N='+numdec(c[0],2),'signif='+numdec(mpnormlim(c[1],/SLEVEL),1)+!tsym.sigma],/top,/left,box=0,spacing=2.5,margin=-0.5
;stop
  return
end 

pro test_policy_shift
  
  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  yr=strmid(g.grb,3,2)+2000.
  mn=strmid(g.grb,5,2)*1.
  dy=strmid(g.grb,7,2)*1.
  doy=ymd2dn(yr,mn,dy)
  date=yr+doy/365.

  n=19
  ndiv=2.
  mdate=fltarr(n)
  mtime=fltarr(n)

  begplot,name='~/Swift/decay_lum_corr/tstop_trend.ps',/color
  !p.multi=[0,1,2]
  plot,date,g.tstop,/ylog,psym=1,xtitle='Swift Mission Year',ytitle='Stop Time of GRB Observations (s)'

  for i=0,n-1 do begin
     w=where(date ge 2005.+i/ndiv and date lt 2005+(1.+i)/ndiv,nw)
     if nw gt 1 then begin 
        mdate[i]=median(date[w])
        mtime[i]=median(g[w].tstop)
     endif else begin
        if nw eq 1 then begin
           mdate[i]=date[w]
           mtime[i]=g[w].tstop
        endif 
     endelse 

  endfor 
;  oplot,mdate,mtime,psym=2,color=!green
;  for i=0,n-1 do oplot,[2005.+i/ndiv,2005+(1.+i)/ndiv],[mtime[i],mtime[i]],color=!green

  s=sort(date)
  x=date[s];mdate
  y=g[s].tstop;mtime
  c=r_correlate(x,alog10(y))
  print,'sig=',mpnormlim(c[1],/SLEVEL)


  ab=linfit(x,alog10(y))
  print,ab
  oplot,[2004,x,2016],10^([2004,x,2016]*ab[1]+ab[0]),color=!red

  legend,['Individual GRBs','Regression (3.6'+!tsym.sigma+')'],textcolor=[!p.color,!red],/top,/right,box=0,charsize=1.5

  alpha=g.alpha_avg
  w=where(g.alpha_avg2 gt 0)
  alpha[w]=g[w].alpha_avg2
;  x=date[s]
;  y=alpha[s]
  s=sort(g.tstop)
  x=g[s].tstop
  y=alpha[s]

  plot,x,y,psym=1,xtitle='Stop Time of GRB Observations (s)',ytitle=!tsym.alpha+'!Lavg,x,t>200s!N',/xlog

  ab=linfit(alog10(x),y)
  print,ab
  oplot,x,alog10(x)*ab[1]+ab[0],color=!red
  c=r_correlate(alog10(x),y)
  print,'sig=',mpnormlim(c[1],/SLEVEL)

;  oplot,[2004,x,2016],[2004,x,2016]*ab[1]+ab[0],color=!red
  legend,['Individual GRBs','Regression (1.8'+!tsym.sigma+')'],textcolor=[!p.color,!red],/top,/right,box=0,charsize=1.5

  !p.multi=0
  endplot
  spawn,'ps2pdf ~/Swift/decay_lum_corr/tstop_trend.ps ~/Swift/decay_lum_corr/tstop_trend.pdf'

stop
return
end 

pro exiso
  
  ;;; need to integrate fluence under steep decay and compare to Eiso

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  
  ind1=strpos(g.type,'I-II-')
  w=where(ind1 ne -1 and g.z gt 0 and g.eiso gt 0 and g.sam_samp eq 1,nw)
;  print,g[w].type

  t=logarr(1,1e5,bin=0.1)
  tt=fltarr(2,n_elements(t)-1)
  tt[0,*]=t[0:n_elements(t)-2]
  tt[1,*]=t[1:*]
  flue=dblarr(nw)  & exiso1=dblarr(nw) & exiso2=exiso1

  for i=0,nw-1 do begin
     f=call_function('int'+strtrim(g[w[i]].model,2),tt,g[w[i]].p)
     tbreak=g[w[i]].p[ind1[w[i]]+2]
     wt=where(t ge g[w[i]].tstart and t le tbreak)
     flue[i]=total(f[wt])*g[w[i]].unabs_cfratio
     exiso1[i]=calc_eiso2(flue[i],0.3,10,g[w[i]].z,-(g[w[i]].beta+1.),0.,0.,emin=0.3,emax=10.,/pl)
     exiso2[i]=calc_eiso2(flue[i],0.3,10,g[w[i]].z,-(g[w[i]].beta+1.),0.,0.,emin=10,emax=10.e3,/pl)
  endfor 

  !p.multi=[0,1,2]
  plotloghist,exiso1/g[w].eiso,bin=0.2,xtickformat='loglabels',xtitl='log E!Lx,iso!N/E!L'+!tsym.gamma+',iso!N (0.3 - 10 keV)/(10 keV - 10 MeV)',ytitle='N'
  plotloghist,exiso2/g[w].eiso,bin=0.2,xtickformat='loglabels',xtitl='log E!Lx,iso!N/E!L'+!tsym.gamma+',iso!N (10 keV - 10 MeV)',ytitle='N'
  !p.multi=0

  return
end 

pro check_corr
  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  w=where(strpos(g.type,'II-III') ne -1,nw) ;; has II & III
  g=g[w]

  alpha2=fltarr(nw) & alpha2err=fltarr(2,nw)
  alpha3=fltarr(nw) & alpha3err=fltarr(2,nw)
 
  for i=0,nw-1 do begin
     if strpos(g[i].type,'0-I-II') ne -1 then j=5 else begin 
        if strpos(g[i].type,'I-II-') ne -1 then j=3 else begin
           if strpos(g[i].type,'II-III') ne -1 then begin
              j=1
;              print,g[i].p[j+2]
           endif 
        endelse 
     endelse 
print,j,' ',g[i].type
     alpha2[i]=g[i].p[j]
     alpha2err[*,i]=g[i].perr[*,j]
     alpha3[i]=g[i].p[j+2]
     alpha3err[*,i]=g[i].perr[*,j+2]

  endfor 

  ploterror2,alpha2,alpha3,alpha2err,alpha3err,psym=3,xtitle='II',ytitle='III',xrange=[-1,1],yrange=[0,2]
  plot,alpha2,alpha3,psym=1,xtitle='II',ytitle='III',xrange=[-1,1],yrange=[0,2]

  


stop
  return
end 

pro dainotti

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  w=where(g.t90 gt 2.)
  g=g[w]

  ng=n_elements(g)
  ptime=fltarr(ng) & ptimerr=fltarr(2,ng)
  lump=fltarr(ng) & lumperr=fltarr(2,ng)
  for i=0,n_elements(g)-1 do begin 
     segs=str_sep(g[i].type,'-')
     wp=where(segs eq 'II',nw)
     if nw gt 0 then begin 
        tp=g[i].p[wp[0]*2+2]
        ptime[i]=tp
        ptimerr[0,i]=g[i].perr[0,wp[0]*2+2]
        ptimerr[1,i]=g[i].perr[1,wp[0]*2+2]
        f=call_function(strtrim(g[i].model,2),tp,g[i].p)*g[i].unabs_cfratio
        lump[i]=flux2jy(f,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=ferr)*1d-23*g[i].lfact
        lumperr[0,i]=g[i].flux_avg_err[0]/g[i].flux_avg*lump[i]
        lumperr[1,i]=g[i].flux_avg_err[1]/g[i].flux_avg*lump[i]
     endif 
  endfor 
  w=where(ptime ne 0 and ptime-ptimerr[0,*] gt 0)

  begplot,name='~/Swift/decay_lum_corr/dianotti.ps',/color,/land,font='helvetica'
  ploterror2,ptime[w],lump[w],ptimerr[*,w],lumperr[*,w],psym=3,/xlog,/ylog,xtitle='Time at end of plateau (s)',ytitle='Luminosity at end of Plateau (erg s!U-1!N Hz!U-1!N)'
  fitexy,alog10(ptime[w]),alog10(lump[w]),x_sig=alog10(ptime[w])-alog10(ptime[w]-ptimerr[0,w]),y_sig=alog10(lump[w])-alog10(lump[w]-lumperr[0,w]),a,b
  oplot,ptime[w],10^(alog10(ptime[w])*b+a),color=!green
  print,a,b
  c=r_correlate(alog10(ptime[w]),lump[w],zd=zd)
  print,c,mpnormlim(c[1],/SLEVEL)
  legend,['slope='+numdec(b,2),'spearman='+numdec(c[0],2),'sig='+numdec(mpnormlim(c[1],/SLEVEL),1)],/top,/right,box=0
  endplot
  spawn,'ps2pdf ~/Swift/decay_lum_corr/dianotti.ps ~/Swift/decay_lum_corr/dianotti.pdf'


  alphaavg=g.alpha_avg
  q=where(g.alpha_avg2 ne 0)
  alphaavg[q]=g[q].alpha_avg2
  alphaavgerr=g.alpha_avg_err
  alphaavgerr[*,q]=g[q].alpha_avg_err
;  lumavg=g.lum_avg
;  lumavg[q]=g[q].lum_avg2
;  lumavgerr=g.lum_avg_err
;  lumavgerr[*,q]=g[q].lum_avg_err

  !p.multi=[0,1,4]
  plot,lump[w],alphaavg[w],psym=1,/xlog,xtitle='Lum at end of plateau',ytitle='alpha avg'
  c=r_correlate(alphaavg[w],alog10(lump[w]),zd=zd)
  print,c,mpnormlim(c[1],/slevel)

  plot,ptime[w],alphaavg[w],psym=1,/xlog,xtitle='Time at end of plateau',ytitle='alpha avg'
  c=r_correlate(alphaavg[w],ptime[w],zd=zd)
  print,c,mpnormlim(c[1],/slevel)

  ploterror2,lump[w],lumavg[w],lumperr[*,w],lumavgerr[*,w],psym=3,/xlog,xtitle='Lum at end of plateau',ytitle='Lum 200,avg',/ylog
  c=r_correlate(alog10(lumavg[w]),alog10(lump[w]),zd=zd)
  print,c,mpnormlim(c[1],/slevel)
  
  ploterror2,ptime[w],lumavg[w],ptimerr[*,w],lumavgerr[*,w],psym=3,/xlog,xtitle='Time at end of plateau',ytitle='Lum 200,avg',/ylog
  c=r_correlate(alog10(lumavg[w]),alog10(ptime[w]),zd=zd)
  print,c,mpnormlim(c[1],/slevel)


  !p.multi=0

stop
  return
end 

pro explore_plateau

  begplot,name='~/Swift/decay_lum_corr/plateau_plots_lum.ps',/land,/color,font='helvetica'
  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  w1=where((strpos(g.type,'II-III') ne -1 or strpos(g.type,'II-IV') ne -1) and g.t90 gt 2. and g.alpha_avg2 le 3,nw1) ;; yes plateau
  w2=where((strpos(g.type,'II-III') eq -1 and strpos(g.type,'II-IV') eq -1) and g.t90 gt 2. and g.alpha_avg2 le 3,nw2) ;; no plateau

  plot,[10,1e7],[1e26,1e33],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Luminosity (erg s!U-1!N Hz!U-1!N)'
  s=findgen(100)/10.+1.
  times=[s*10,s*100,s*1000,s*1e4,s*1e5,s*1e6]
  loadct,39
  ptime=fltarr(n_elements(g))
  for i=0,n_elements(g)-1 do begin 
     j=i
     cd,'~/GRBs/'+strtrim(g[j].grb,2)

     segs=str_sep(g[i].type,'-')
     wp=where(segs eq 'II',nw)
     if nw gt 0 then begin 
        tp=g[i].p[wp[0]*2+2]
        ptime[i]=tp
        f=call_function(strtrim(g[i].model,2),tp,g[i].p)*g[i].unabs_cfratio
        lump=flux2jy(f,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=ferr)*1d-23*g[i].lfact
        color=round((alog10(lump)-23)*29.)
;        color=round((alog10(tp)-2.05)/3.*256)
     endif else color=!p.color

     w=where(times ge g[j].tstart and times le g[j].tstop)
     f=call_function(strtrim(g[j].model,2),times,g[j].p)*g[j].unabs_cfratio;*g[i].lfact
     lum=flux2jy(f,g[j].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[j].flux_avg_err[0]/g[j].flux_avg*f,ferr=ferr)*1d-23*g[j].lfact
;     h[i]=round((alog10(f*yfit200)-42)*31.)
     oplot,times[w],lum[w],color=color
  endfor 
;  legend,['Plateau','No
;  Plateau'],/top,/right,box=0,textcolor=[!p.color,200]
  xyouts,1e6,3e33,'P',/data,color=250
  xyouts,1.35e6,3e33,'l',/data,color=210
  xyouts,1.5e6,3e33,'a',/data,color=195
  xyouts,1.9e6,3e33,'t',/data,color=170
  xyouts,2.15e6,3e33,'e',/data,color=150
  xyouts,2.7e6,3e33,'a',/data,color=130
  xyouts,3.5e6,3e33,'u',/data,color=100
  xyouts,7e5,1e33,'No Plateau',/data

  endplot
  spawn,'ps2pdf ~/Swift/decay_lum_corr/plateau_plots_lum.ps ~/Swift/decay_lum_corr/plateau_plots_lum.pdf'

stop
  return
end

pro gendre,doplot=doplot,reallyredo=reallyredo

  ;;; get lum at end of plateau & 1 day, and compare to decay rate
  ;;; after plateau
  s1=round(1000.*(1-0.67)/2.)
  s2=round(1000.*(1.-(1-0.67)/2.))

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  w=where(g.t90 gt 2.)
  g=g[w]
  ng=n_elements(g)
  lum1=dblarr(ng)
  lump=dblarr(ng)
  alpha=fltarr(ng)
  alphaerr=fltarr(2,ng)
  lum1err=fltarr(2,ng)
  lumperr=fltarr(2,ng)

  lum1av=dblarr(ng) & lum1averr=fltarr(2,ng) & alphaav=fltarr(ng) & alphaaverr=fltarr(2,ng)

  times=logarr(10,1e7)
  aftersegs=intarr(ng)
  ptime=fltarr(ng)
  for i=0,ng-1 do begin 

     f=call_function(strtrim(g[i].model,2),86400.*(1.+g[i].z),g[i].p)*g[i].unabs_cfratio;*g[i].lfact
     lum1[i]=flux2jy(f,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=ferr)*1d-23*g[i].lfact
     lum1err[0,i]=g[i].flux_avg_err[0]/g[i].flux_avg*lum1[i]
     lum1err[1,i]=g[i].flux_avg_err[1]/g[i].flux_avg*lum1[i]
     f=call_function(strtrim(g[i].model,2),1000.*(1.+g[i].z),g[i].p)*g[i].unabs_cfratio;*g[i].lfact
     if strpos(g[i].type,'II-III') ne -1 or strpos(g[i].type,'II-IV') ne -1 then begin
        segs=str_sep(g[i].type,'-')
        w=where(segs eq 'II',nw)
        aftersegs[i]=n_elements(segs)-w[0]-1
        if nw gt 0 then begin 
           tp=g[i].p[w[0]*2+2]
           ptime[i]=tp
           f=call_function(strtrim(g[i].model,2),tp,g[i].p)*g[i].unabs_cfratio
           lump[i]=flux2jy(f,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=ferr)*1d-23*g[i].lfact
           lumperr[0,i]=g[i].flux_avg_err[0]/g[i].flux_avg*lump[i]
           lumperr[1,i]=g[i].flux_avg_err[1]/g[i].flux_avg*lump[i]
           alpha[i]=g[i].p[w*2+3]
           alphaerr[*,i]=g[i].perr[*,w*2+3]

           cd,'~/GRBs/'+strtrim(g[i].grb)
           lc=lcout2fits()
           wdet=where(lc.src_rate_err gt 0 and lc.time ge tp,nwdet)
           if nwdet gt 0 then begin 
              t=lc.time
              f=lc.src_rate
              ferr=lc.src_rate_err
              terr=rotate([[lc.time-lc.tstart],[lc.tstop-lc.time]],4)
              ab=linfit(alog10(t[wdet]),alog10(f[wdet]))
              p=[10^ab[0],-ab[1]]
              perror=0.1*p
              intmo='intpow'
              pnames=['norm','pow']
              
              fit_pow_model,t[wdet],f[wdet],terr[*,wdet],ferr[wdet],p,intmo,pnames,yfit,newp,perror,chisq,dof,weights,lc[wdet].src_counts,lc[wdet].tot_back_cts,status=status,breaks=0,noint=noint,pmin0=pmin0,/silent
              yfit=call_function('pow',86400.*(1.+g[i].z),newp)*g[i].unabs_cfratio
              lum1av[i]=flux2jy(yfit,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=yfiterr)*1d-23*g[i].lfact
              lum1averr[0,i]=g[i].flux_avg_err[0]/g[i].flux_avg*lum1av[i]
              lum1averr[1,i]=g[i].flux_avg_err[1]/g[i].flux_avg*lum1av[i]
              alphaav[i]=newp[1]
              if not exist('lc_fit_out_idl_int5_mc.fits') or keyword_set(reallyredo) then begin 
                 lc_monte_pow,lc[wdet],newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int='5',file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit
              endif else mcfit=mrdfits('lc_fit_out_idl_int5_mc.fits',1)
              s=sort(mcfit.pow)
              alphaaverr[*,i]=[mcfit[s[500]].pow-mcfit[s[s1]].pow,mcfit[s[s2]].pow-mcfit[s[500]].pow]
              if keyword_set(doplot) then begin
                 lumfact=flux2jy(1.,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0]/g[i].flux_avg*f,ferr=yfiterr)*1d-23*g[i].lfact*g[i].unabs_cfratio
                 ploterror2,t,f*lumfact,terr,ferr*lumfact,psym=3,/nohat,/xlog,/ylog,title=g[i].grb+'  '+g[i].type+' (z = '+numdec(g[i].z,2)+')'
                 oplot,times,call_function(strtrim(g[i].basemodel,2),times,g[i].p)*lumfact,color=!red
                 oplot,t[wdet],pow(t[wdet],newp)*lumfact,color=!green
                 oplot,[86400*(1.+g[i].z),86400*(1.+g[i].z)],[1e20,1e35],line=1
                 legend,['1 day lum = '+numdec(lum1[i],2,/sci),'alpha av = '+numdec(alphaav[i],2)],box=0,/top,/right
                 k=get_kbrd(10)
                 if k eq 's' then stop
              endif 
           endif 
           cd,'~/GRBs/'

        endif 
     endif 
  endfor 
;  begplot,name='~/stuff_for_people/Sam/gendre.ps',/color
  begplot,name='~/stuff_for_people/Sam/lum_plateau_decay.ps',/color,/land
;  !p.multi=[0,1,3]
  plotsym,0,1
  ;; w=where(alpha ne 0)
  ;; ploterror2,lum1[w],alpha[w],lum1err[*,w],alphaerr[*,w],psym=8,/xlog,xtitle='Lum at rest frame  1 day (erg/s)',ytitle='PL decay after plateau',/nohat,yrange=[-1,5],/ysty,charsize=2
  ;; fitexy,alog10(lum1[w]),alpha[w],x_sig=alog10(lum1[w])-alog10(lum1[w]-lum1err[0,w]),y_sig=alphaerr[0,w],a,b
  ;; r=[a,b]
  ;; oplot,10^alog10(lum1[w]),alog10(lum1[w])*r[1]+r[0],color=!green
  ;; print,r
  ;; c=r_correlate(alog10(lum1[w]),alpha[w],zd=zd)
  ;; print,c,mpnormlim(c[1],/SLEVEL)
  ;; legend,['slope='+numdec(r[1],2),'spearman='+numdec(c[0],2),'sig='+numdec(mpnormlim(c[1],/SLEVEL),1)],/top,/left,box=0

  ;; ploterror2,lump[w],alpha[w],lumperr[*,w],alphaerr[*,w],psym=8,/xlog,xtitle='Lum at end of plateau (erg/s)',ytitle='PL decay after plateau',/nohat,yrange=[-1,5],/ysty,charsize=2
  ;; fitexy,alog10(lump[w]),alpha[w],x_sig=alog10(lump[w])-alog10(lump[w]-lumperr[0,w]),y_sig=alphaerr[0,w],a,b
  ;; r=[a,b]
  ;; oplot,10^alog10(lump[w]),alog10(lump[w])*r[1]+r[0],color=!green
  ;; print,r
  ;; c=r_correlate(alog10(lump[w]),alpha[w],zd=zd)
  ;; print,c,mpnormlim(c[1],/SLEVEL)
  ;; legend,['slope='+numdec(r[1],2),'spearman='+numdec(c[0],2),'sig='+numdec(mpnormlim(c[1],/SLEVEL),1)],/top,/left,box=0

  w=where(alphaav ne 0 and lum1 gt 0 and finite(alphaaverr[0,*]))
  test_correlation,lum1[w],-alphaav[w],lum1err[*,w],alphaaverr[*,w],g[w].z,xtitle='Lum at rest frame 1 day (erg/s)',ytitle='avg PL decay after plateau',xrange=[1d23,1d29]

  ;; ploterror2,lum1[w],alphaav[w],lum1err[*,w],alphaaverr[*,w],psym=8,/xlog,xtitle='Lum at rest frame 1 day (erg/s)',ytitle='avg PL decay after plateau',/nohat,yrange=[-1,5],/ysty,charsize=2
  ;; fitexy,alog10(lum1[w]),alphaav[w],x_sig=alog10(lum1[w])-alog10(lum1[w]-lum1err[0,w]),y_sig=alphaaverr[0,w],a,b
  ;; r=[a,b]
  ;; oplot,10^alog10(lum1[w]),alog10(lum1[w])*r[1]+r[0],color=!green
  ;; print,r
  ;; c=r_correlate(alog10(lum1[w]),alphaav[w],zd=zd)
  ;; print,c,mpnormlim(c[1],/SLEVEL)
  ;; legend,['slope='+numdec(r[1],2),'spearman='+numdec(c[0],2),'sig='+numdec(mpnormlim(c[1],/SLEVEL),1)],/top,/left,box=0

  ;; w=where(alphaav ne 0 and lum1av gt 0 and finite(alphaaverr[0,*]))
  ;; ploterror2,lum1av[w],alphaav[w],lum1averr[*,w],alphaaverr[*,w],psym=8,/xlog,xtitle='Lum at 1 day rest frame on avg decay (erg/s)',ytitle='avg PL decay after plateau',/nohat,yrange=[-1,5],/ysty,charsize=2
  ;; fitexy,alog10(lum1av[w]),alphaav[w],x_sig=alog10(lum1av[w])-alog10(lum1av[w]-lum1averr[0,w]),y_sig=alphaaverr[0,w],a,b
  ;; r=[a,b]
  ;; oplot,10^alog10(lum1av[w]),alog10(lum1av[w])*r[1]+r[0],color=!green
  ;; print,r
  ;; c=r_correlate(alog10(lum1av[w]),alpha[w],zd=zd)
  ;; print,c,mpnormlim(c[1],/SLEVEL)
  ;; legend,['slope='+numdec(r[1],2),'spearman='+numdec(c[0],2),'sig='+numdec(mpnormlim(c[1],/SLEVEL),1)],/top,/left,box=0

  ;; !p.multi=0
  endplot
;  spawn,'ps2pdf ~/stuff_for_people/Sam/gendre.ps ~/stuff_for_people/Sam/gendre.pdf'
  spawn,'ps2pdf ~/stuff_for_people/Sam/lum_plateau_decay.ps ~/stuff_for_people/Sam/lum_plateau_decay.pdf'
 
stop

  return
end

pro results_table

  fit=mrdfits('~/Swift/decay_lum_corr/fit_results.fits',1)
  a=' & '

  i=[0,2,3,4,5,6,7,10,11,12,26,40]

  stop
  return
end 

pro paper_plots
  
  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  ;; T90start
  
  bin=5.
  plothist,g.t90start,x,y,bin=bin,/noplot
  p=plot([-250,350],[0,100],/nodata,xtitle='T!L05!N -T!L0!N (s)',ytitle='N',yrange=[0,130],xrange=[-250,350],aspect_ratio=1.7)
  s=sort(g.t90start)
;  p=polygon([-5,-5,5,5,-5],[0,1e4,1e4,0,0],transparency=20,/data,fill_color='gray',/overplot)
  b=barplot(x+bin/2.,y,linestyle='none',/overplot,width=bin,color='medium blue',transparency=20)

  p.save,'~/papers/decay_lum_corr/t90start.pdf'
  p.refresh
;  k=get_kbrd(10)
;  if k eq 's' then stop
  p.close
  
  ;;; individual segments at t200

  begplot,name='~/Swift/decay_lum_corr/flux_decay_und.ps',/land
  w1=where(g.t90 gt 2 and g.tstart lt g.t200*2. and g.alpha_avg lt 3)
  test_correlation,g[w1].lumdens_final,-g[w1].alpha_und,g[w1].lumdens_final_err,g[w1].alpha_und_err,g[w1].z,yrange=[-7,3],/ysty,xtitle='Lum!LX,200s!N (erg s!U-1!N Hz!U-1!N)',ytitle=!tsym.alpha+'!LX,fit,>200s!N'
;;add in by color which ones are SPL, I, II, III
  endplot
  spawn,'ps2pdf ~/Swift/decay_lum_corr/flux_decay_und.ps ~/Swift/decay_lum_corr/flux_decay_und.pdf'
  stop

  return
end 

pro large_errors

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  bursts='GRB'+['050802','060512','060526','060607A','060729','061007','061021','070318','080310','080319B','080916A','081008','090812','100901A','101219B']
  match,strtrim(g.grb,2),bursts,m1,m2
  g=g[m1]

  lum=g.lum_avg
  w=where(g.lum_avg2 ne 0)
  colprint,g.grb,g.lum_avg_err[0]/g.lum_avg,g.lum_avg2_err[0]/g.lum_avg2  
  
  stop
return
end 
pro simulate,g,doplot=doplot

  if n_elements(g) eq 0 then g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  lum=g.lum_avg
  lumerr=g.lum_avg_err
  w=where(g.tstart lt g.t200 and g.lum_avg ne 0)
  lum=lum[w]
  lumerr=lumerr[*,w]
  g=g[w]
  ng=n_elements(g)

;  w=where(g.lum_avg2 ne 0)
;  lum[w]=g[w].lum_avg2
  z=g.z
  
  mpc2cm=3.08568025d24 
  ratio=4e-11 ;; erg/cm2/s/(cts/s)
  xrtlim=2e-4
  nsim=1e3
  out=mrdfits('~/Swift/decay_lum_corr/sim_decay_slope.fits',1)
  if n_elements(out) eq 0 then begin
     out=create_struct('r',0d,'p',0d,'slope',0d,'const',0d,'zd',0d)
     out=replicate(out,nsim)
     jstart=0
  endif else begin 
     wo=where(out.r eq 0)
     jstart=wo[0]
  endelse 

  for j=jstart,nsim-1 do begin 
     pt=systime(1)
     print,j
     alpha200=dblarr(ng) & lum200=alpha200 & norm200=lum200 & alpha200err=alpha200 
     lum200err=dblarr(2,ng) & t200=alpha200
     for i=0L,ng-1 do begin 
;,g2*5
        if i mod 100 eq 0 then print,i
        r1=randomu(seed,1)*ng-1  ;;; select random index for z
        r2=randomu(seed,1)*ng-1  ;;; select random index for lum
;     r3=randomu(seed,1)*ng-1  ;;; select random index for lc
        
;     r1=i
;     r2=i
        r3=i
        
        dist=lumdist(z[r1],/silent)*mpc2cm
        t200[i]=200.*(1.+z[r1])

        cr2=lum[r2]/(4.*!pi*dist^2)/g[r2].unabs_cfratio ;;; random luminosity and random z = count rate at t_200*(1+z)
        dir='~/GRBs/'+strtrim(g[r3].grb,2)+'/'
        lc=lcout2fits(dir=dir,/silent)
        t=lc.time
        f=lc.src_rate
        ferr=lc.src_rate_err
        terr=rotate([[lc.time-lc.tstart],[lc.tstop-lc.time]],4)
        lcfile='lc_fit_out_idl_int9.dat'
;        if not exist(dir+lcfile) then lcfile='lc_fit_out_idl_int7.dat'
        if exist(dir+lcfile) then begin 
           read_lcfit,dir+lcfile,pnames,p,perror
           mo=fit_models(pnames,p,np,nf,basemo=basemo)
           tmp=execute('cr1='+mo+'(t200[i],p)')
           norm=cr2/cr1
           w=where(f*norm[0] gt xrtlim and t gt t200[i] and ferr gt 0,nw)
;        print,norm,nw
           if nw gt 2 then  begin 
;           fitexy,alog10(t[w]),alog10(f[w]),a,b,x_sig=alog10(terr[0,w]),y_sig=alog10(ferr[w])
              ab=linfit(alog10(t[w]),alog10(f[w]))
              a=ab[0]
              b=ab[1]

              p=[10^a,-b]
              perror=0.1*p
              intmo='intpow'
              pnames=['norm','pow']
              
              fit_pow_model,t[w],f[w]*norm[0],terr[w],ferr[w]*norm[0],p,intmo,pnames,yfit,newp,perror,chisq,dof,weights,lc[w].src_counts,lc[w].tot_back_cts,status=status,breaks=0,noint=noint,pmin0=pmin0,/silent
              alpha200[i]=newp[1]
              alpha200err[i]=perror[1]
              lum200[i]=lum[r2]
              lum200err[*,i]=lumerr[*,r2]
              norm200[i]=norm

              if keyword_set(doplot) then begin
                 ploterror,t,f*norm[0],terr,ferr*norm[0],psym=3,/nohat,/xlog,/ylog,title=g[r3].grb,xtitle='Time (s)',ytitle='Flux'
                 oploterror,t[w],f[w]*norm[0],terr[w],ferr[w]*norm[0],psym=3,/nohat,color=!orange
                 oplot,t[w],pow(t[w],newp),color=!magenta
                 oplot,[g[r3].t200,g[r3].t200],[1e-5,1e5],line=1
                 oplot,[t200[i],t200[i]],[1e-5,1e5],line=1,color=!orange
                 print,g[r3].t200,t200[i]
                 legend,['alpha='+ntostr(newp[1]),'a200='+ntostr(g[r3].alpha_avg)],/top,/right,box=0
                 
                 k=get_kbrd(10)
                 if k eq 's' then stop
              endif 
           endif   
        endif 
     endfor 

     w=where(lum200 ne 0 and alpha200 ne 0,nw)
     ploterror,alog10(lum200[w]),-alpha200[w],alpha200err[w],psym=3,xtitle='Lum',ytitle=!tsym.alpha,yrange=[-5,0],/nohat
     for i=0,nw-1 do oplot,alog10([lum200[w[i]]-lum200err[0,w[i]],lum200[w[i]]+lum200err[1,w[i]]]),-[alpha200[w[i]],alpha200[w[i]]],color=color
     r=regress(alog10(lum200[w]),-alpha200[w],const=const,yfit=yfit,correlation=linearcorr,measure_error=alpha200err[w])
     c=r_correlate(alog10(lum200[w]),-alpha200[w],zd=zd)
     line=(r[0]*alog10(lum200[w])+const)
     oplot,alog10(lum200[w]),line,color=!magenta
     legend,['R='+numdec(c[0],3)+'  p='+ntostr(c[1])+'  slope='+numdec(r[0],3)+'  const='+numdec(const,3)],/top,/right,box=0
     print,c[0],r[0],const,zd,mpnormlim(c[1],/SLEVEL)

     ptime,systime(1)-pt
     out[j].r=c[0]
     out[j].p=c[1]
     out[j].slope=r[0]
     out[j].const=const
     out[j].zd=zd
     if j mod 10 eq 0 then mwrfits,out,'~/Swift/decay_lum_corr/sim_decay_slope.fits',/create
  endfor 
  stop
  return
end 

pro hypothesis,grb,simr,noplot=noplot,plotone=plotone,pind=pind,ps=ps
  
;  t=[(dindgen(9)+1)*10.,(findgen(9)+1)*100,(findgen(9)+1)*1e3,(findgen(9)+1)*1e4,(findgen(9)+1)*1e5,(findgen(9)+1)*1e6]
                                ;grb='GRB060507'
  if n_elements(grb) eq 0 then begin 
     print,'Need GRB input' 
     stop
  endif 

  ngrb=n_elements(grb)
  simr=fltarr(ngrb)
  if n_elements(pind) eq 0 then pind=2
  g=0
  stop
  lums=0.
  slopes=0.
  for j=g,ngrb-1 do begin 
     print,j,' ',grb[j]
     !p.multi=[0,2,4]
     if keyword_set(ps) then begplot,name='~/Swift/decay_lum_corr/'+grb[j]+'_decay_lum_sim.ps',/color
     !x.margin=[4,4]
     !y.margin=[4,2]

     lc=lcout2fits(dir=grb[j]);'~/GRBs/'+grb[j]+'/lc_newout_phil.txt')
     read_lcfit,'~/GRBs/'+grb[j]+'/lc_fit_out_idl_int9.dat',pnames,p0,perror0

     tb=p0[pind]
     fb=bknpow(tb,p0)
     w=where(lc.src_rate_err gt 0) ; and lc.time gt 1e3)
     lc=lc[w]
     if pind gt 2 then begin
        w=where(lc.time gt p0[pind])
        lc=lc[w]
     endif 
     t=lc.time

     fact=1.                    ;5d-11
     nt=n_elements(t)
;  exptime=[5.,(10d^round(alog10(t)-0.5d))/2d]
;  exptime=(t[1:*]-t[0:nt-2])/2.
;  w=where(exptime gt 0)
;  exptime[w]=exptime[w-1]
;  exptime=exptime[0:n_elements(t)-1]
     exptime=lc.tstop-lc.tstart
     limlim=0
     lum=dblarr(100)
     slope=lum & norm=lum
;  ind=reverse((findgen(1000.)+1.)/5000.)
     n=1.                       ;0.;e5
     limset=0
     t200=10000.
;     !p.multi=[0,1,2]
     for i=1,100 do begin 
;     p=[n,1.2,1e5,2.2] ;; cts/s
;     r=bknpow(t,p)
        r=lc.src_rate*n
;     cts=r*exptime
;     ctserr=sqrt(cts)
        cts=lc.src_counts
        ctserr=sqrt(cts)

        f=r*fact
        ferr=lc.src_rate_err*fact*n
;     ferr=ctserr/exptime*fact
        lim=fact*5e-4           ;*1./n
        tnear=approx(t,t200,ni)
        w=where(f gt lim,nw)

        if not keyword_set(noplot) then begin 
           ploterror,t,f,exptime/2.,ferr,/xlog,/ylog,psym=3,/nohat,xrange=[10,1e7],yrange=[1e-4,1e5]*fact,xtitle='Time (s)',ytitle='Arbitrary Count Rate' ;,color=!p.color
           oplot,[10,1e7],[lim,lim],line=2,color=!red
           oplot,t,bknpow(t,p0)*fact*n,line=1,color=!cyan
        endif 
;        if n_elements(nnw) eq 0 then nnw=nw+1
;        print,nw,nnw
        if nw gt 1 then begin 
;        print,nw,nnw
           if not keyword_set(noplot) then oploterror,t[w],f[w],exptime[w],ferr[w],errcolor=!blue,psym=3,/nohat
           fitexy,alog10(t[w]),alog10(f[w]),a,b,x_sig=alog10(exptime[w]/2.),y_sig=alog10(ferr[w])
;           ab=linfit(alog10(lc[w].time),alog10(lc[w].src_rate))
           p=[10^a,-b]
           perror=0.1*p
           intmo='intpow'
           pnames=['norm','pow']
           
           fit_pow_model,t[w],f[w],exptime[w],ferr[w],p,intmo,pnames,yfit,newp,perror,chisq,dof,weights,lc[w].src_counts,lc[w].tot_back_cts,status=status,breaks=0,noint=noint,pmin0=pmin0,/silent
           
;        oplot,t,10^(a+b*alog10(t)),color=!green
;        if limset eq 0 and fb*fact*n lt lim then begin
           if limset eq 0 and t[w[nw-1]] le tb then begin
              limlim=fb*fact*n*bknpow(t200,p0)/bknpow(tb,p0)
              limset=1
              print,limlim
           endif
           
           if not keyword_set(noplot) then begin 
              oplot,t,pow(t,newp),color=!green
              oplot,[tb,tb],[1e-4,1e6],line=1,color=!red
              legend,['slope='+ntostr(newp[1])],/top,/left,box=0
           endif 
;     print,a,b
        endif else i=100

;        if nw lt nnw then begin 
        w0=where(lum ne 0,nw0)
;        if abs((-newp[1]-slope[i-2])/(max(slope[w0])-min(slope[w0]))) gt 0.05 then begin 
           norm[i-1]=n
           lum[i-1]=pow(t200,newp);f[ni]
           slope[i-1]=-newp[1]  ;b
;        endif else print,'what?'
        n=n*0.9
;        nnw=nw

;        w0=where(lum ne 0,nw0)
        if not keyword_set(noplot) then begin 
           if nw0 gt 1 then begin 
              plot,lum[w0],slope[w0],/xlog,psym=1,xtitle='Lum',ytitle=!tsym.alpha
              if limset then oplot,[limlim,limlim],[-2,2],lin=2,color=!red
           endif else plot,[0,1],[0,1],/nodata
           if not keyword_set(ps) then begin 
              k=get_kbrd(10)
              if k eq 's' then stop
           endif 
        endif 
     endfor 

     w0=where(slope ne 0,nw0)
     mm=minmax(slope[w0])
     mm=mm[1]-mm[0]
     wg=where(abs((slope[1:*]-slope[0:nw0-2])) gt 0.001,nwg)
     if nwg lt 10 then wg=where(abs((slope[1:*]-slope[0:nw0-2])) gt 0.01,nwg)
     if nwg lt 3 then wg=where(abs((slope[1:*]-slope[0:nw0-2])) gt 0.1,nwg)

;     help,wg

     slope=slope[wg]
     lum=lum[wg]
     norm=norm[wg]

     if keyword_set(plotone) then begin 
        !p.multi=[0,1,2]
        f=lc.src_rate*fact
        ferr=lc.src_rate_err*fact
        ploterror,t,f,exptime/2.,ferr,/xlog,/ylog,psym=3,/nohat,xrange=[10,1e7],yrange=[1e-4,1e5]*fact,xtitle='Time (s)',ytitle='Arbitrary Count Rate' ;,color=!p.color
        w=where(f ge lim and t ge t200,nw)
        oploterror,t[w],f[w],exptime[w]/2.,ferr[w],psym=3,errcolor=!orange,/nohat
        oplot,[10,1e7],[lim,lim],line=2,color=!red
        oplot,t,bknpow(t,p0)*fact,color=!cyan
        oplot,[tb,tb],[1e-15,1e15],line=1,color=!green
        legend,[grb[j],'t>t'+ntostr(fix(t200)),'Original Bknpow fit','t!Lbreak!N','Obs Limit'],box=0,/top,/right,line=[-1,0,0,1,2],color=[!p.color,!orange,!cyan,!green,!red]

        wr=where(slope gt -5 and slope lt 5)
        plot,lum,slope,/xlog,psym=1,xtitle='Lum',ytitle=!tsym.alpha,yrange=prange(slope[wr],0.1),xtickformat='loglabels'
        if limset then oplot,[limlim,limlim],[-2,2],lin=2,color=!green
        legend,['Lum<Lum!Lbreak!N','Fit for t>t_lum'],box=0,color=[!green,!magenta],line=[2,0],/top,/right
     endif 

     wf=where(lum gt limlim[0],nwf)
     if nwf gt 2 then begin 
        r=regress(alog10(lum[wf]),slope[wf],const=const,yfit=yfit,correlation=linearcorr,measure_error=yerr)
        line=(r[0]*alog10(lum[wf])+const)
        if keyword_set(plotone) then oplot,lum[wf],line,color=!magenta
        print,const,r
        lums=[lums,lum[wf]]
        slopes=[slopes,slope[wf]]
     endif else r=0
     
;  ab=linfit(alog10(lum[wf]),slope[wf])
;  print,'correlation',ab
;  oplot,lum,ab[0]+alog10(lum)*ab[1],color=!magenta


     if keyword_set(plotone) then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
     simr[j]=r
     !p.multi=0
     if keyword_set(ps) then endplot
  endfor 

  stop

  !p.multi=[0,1,2]
  lums=lums[1:*]
  slopes=slopes[1:*]
  plot,alog10(lums),slopes,psym=1,xrange=[-5,5]
  r=regress(alog10(lums),slopes,const=const,yfit=yfit,correlation=linearcorr)
  c=r_correlate(alog10(lums),slopes,zd=zd)
  line=(r[0]*alog10(lums)+const)
  oplot,alog10(lums),line,color=!magenta
  print,c,zd
  q=where(simr ne 0)
  plothist,1./simr[q],xrange=[-20,5]
  oplot,1./[median(simr[q]),median(simr[q])],[0,100],color=!magenta,line=1

  !p.multi=0
  stop

  return
end 

pro extract_redshift
  
  ;;; correlation: alpha=-6*loglum+7
 
  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  flux=g.flux_avg2
  w=where(flux eq 0.)
  flux[w]=g[w].flux_avg
  
  alpha=-g.alpha_avg2
  alpha[w]=-g[w].alpha_avg
  lum=g.lum_avg2
  lum[w]=g[w].lum_avg

  slope=1./(-6.012)
  const=7.022
  lum0=10.^((alpha-const)/(slope))

  d=sqrt(lum0/(4d*!pi*flux))

  z0=dindgen(1000)/100.+0.01
  mpc2cm=3.08568025d24 
  d0=lumdist(z0)*mpc2cm

  ng=n_elements(g)
  z=dblarr(ng)
  for i=0,ng-1 do begin
     if flux[i] and alpha[i] ne 0. then begin

        dt=approx(d0,d[i],ind)
        z[i]=z0[ind]
        colprint,g[i].z,z[i],d[i],dt
     endif 
  endfor 
  plot,g.z,z,psym=1,/iso
  oplot,[0,10],[0,10]
;;;; compare to distance from correlation line

  stop
  return
end 

pro investigate

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  n=n_elements(g)
  np=fltarr(n)
  tb=dblarr(n)
  for i=0,n-1 do begin
     np[i]=n_elements(g[i].p)
     basemo=strtrim(g[i].basemodel)
     if basemo eq 'bknpow' or basemo eq 'bkn2pow' or basemo eq 'bkn3pow' or basemo eq 'bkn4pow' then $
        tb[i]=g[i].p[2]
  endfor 
  w=where(tb ne 0)
  plot,tb[w],g[w].t200,psym=1,/xlog,/ylog,/iso,xtitle='Break time (s)',ytitle='t!L200!N (s)'
  oplot,[1,1e5],[1,1e5]

  stop
  return
end
 
pro plot_avg_lc,g

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  mpc2cm=3.08568025d24
  n=n_elements(g)
  for i=0,n-1 do begin 
     grb=g[i].grb
     print,grb
     begplot,name='~/Swift/decay_lum_corr/decay_lum/'+strtrim(grb,2)+'_fit.ps',/color,/land
     file='~/GRBs/'+strtrim(grb,2)+'/lc_newout_phil.txt'
     lc=lcout2fits(dir='~/GRBs/'+strtrim(grb,2)+'/')
     lc.time=lc.time-g[i].t90start
     lc.tstart=lc.tstart-g[i].t90start
     lc.tstop=lc.tstop-g[i].t90start
     dist=lumdist(g[i].z)*mpc2cm
;     lfact=g[i].unabs_cfratio*g[i].lfact;4.*!pi*dist^2*(1.+g[i].z)^(g[i].beta-1.)
     lfact=g[i].lfact
     fdensfact=flux2jy(1.,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0],ferr=ferr)*1d-23
     lfact=lfact*fdensfact*g[i].unabs_cfratio

     yrange=10d^[round(alog10(min(lc.src_rate*lfact))-0.5),round(alog10(max(lc.src_rate*lfact))+0.5)]
     plot_like_qdp,lc=lc,title=grb+'  '+g[i].type,flux=lfact,ytitle='Luminosity at 1 keV (erg s!U-1!N Hz!U-1!N)',yrange=yrange,xtitle='Time since BAT T90start (s)'

     read_lcfit,'~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int3.dat',pnames,p
     oplot,lc.time,pow(lc.time,p)*lfact,color=!magenta
;     print,p
;     if exist('~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int4.dat')
;     then begin 
     if g[i].flux_avg2 gt 0 and exist('~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int4.dat') then begin 
        read_lcfit,'~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int4.dat',pnames,p
        oplot,lc.time,pow(lc.time,p)*lfact,color=!cyan
     endif 
     legend,['PL fits',$
             'avg fit t>t200 ('+!tsym.alpha+'='+ntostr(g[i].alpha_avg,4)+', L='+numdec(g[i].fluxdens_avg*g[i].lfact,2,/sci)+')',$
             'avg fit t>tbreak ('+!tsym.alpha+'='+ntostr(g[i].alpha_avg2,4)+', L='+numdec(g[i].fluxdens_avg2*g[i].lfact,2,/sci)+')',$
             'T90!Lstart!N = '+ntostr(g[i].t90start,4)+' s',$
             'z='+ntostr(g[i].z,4)],/top,/right,box=0,textcolor=[!green,!magenta,!cyan,!p.color,!p.color]
     legend,['t200','tbreak'],/right,/center,box=0,line=[2,1]

     lcfile='~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int9.dat'
;     if not exist(lcfile) then lcfile='~/GRBs/'+strtrim(grb,2)+'/lc_fit_out_idl_int7.dat'
     read_lcfit,lcfile,pnames,p
     mo=fit_models(pnames,p,np,nf,basemo=basemo,breaks=breaks)
     time=[lc.time,g[i].p[breaks]]
     time=time[sort(time)]
     f=call_function(mo,time,g[i].p)
     f2=call_function(basemo,time,g[i].p)

;     fdens=flux2jy(f,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0],ferr=ferr)*1d-23

     oplot,time,f*lfact,color=!green
     oplot,time,f2*lfact,color=!green,line=1
     plots,g[i].t200,g[i].lumdens_final,psym=2,symsize=2.
     oplot,[g[i].t200,g[i].t200],[1d10,1d40],line=2
     bind=0
     if strpos(strtrim(g[i].type,2),'I-II') ne -1 then bind=2
     if strpos(strtrim(g[i].type,2),'0-I') ne -1 then bind=4  ;;; make sure to grab break after steep decay
     if bind ne 0 then $
        oplot,[g[i].p[bind],g[i].p[bind]],[1d10,1d40],line=1
     xyouts,g[i].t200+100,lc[0].src_rate_err*lfact/10.,'t!L200!N',/data,charsize=2
;     k=get_kbrd(10)
     endplot
;      if k eq 's' then stop
endfor 
  return
end 

pro numbers_for_sam

  g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)
  s=where(g.sam_samp eq 1,ns)
  g=g[s]

  w=where(g.eiso eq 0)
  lim=strarr(ns)
  lim[*]=' '
  w=where(g.tbreaklim eq 1)
  lim[w]='>'
  w=where(g.tbreaklim eq 2)
  lim[w]='<'
  
  print,'   ',strsame('GRB',12),strsame('Type',12),strsame('JB',10),strsame('Tbreak',10),'  ',strsame('theta',10),'   ',strsame('Eiso',8),strsame('Egam',8),strsame('Epeak',8),strsame('Eiso Source',8)
  colprint,strsame(g.grb,12),strsame(g.type,12),strsame(g.jb,10),lim+strsame(numdec(g.tbreak,2,/sci),10),lim+strsame(numdec(g.theta,2),10),strsame(numdec(g.eiso,2,/sci),8),lim+strsame(numdec(g.egam,2,/sci),8),strsame(numdec(g.epeak,1),8),strsame(g.eiso_source,8)

  stop
  return
end 

pro decay_lum_correlation,noplot=noplot,redo=redo,reallyredo=reallyredo,noerror=noerror
  cd,'~/GRBs/'

  ;;; fit = real fit to data including flares
  ;;; und = fit to underlying afterglow - no flares
  ;;; avg = average decay index after some time t200
  ;;; avg2= average decay after steep->shallow break if >t200

  grbs=file_search('GRB*/')
  n=n_elements(grbs)
;  n=860;n-4 ;;; FREEZING SAMPLE TO ONLY BE THROUGH 2012
  t200=200.

  if not exist('~/Swift/decay_lum_corr/lum_decay_corr.fits') or keyword_set(redo) then begin 
     g=create_struct('grb','','z',0.,'model','','basemodel','',$
                     'tstart',0.,'tstop',0.,$
                     't200',0.,$
                     'p',dblarr(30),'perr',dblarr(2,30),$
                     'pnames',strarr(30),'np',0,$
                     'lfact',0d,'lfact_bat',0d,$
                     'alpha_avg',0d,'alpha_avg_err',dblarr(2),$
                     'flux_avg',0d,'flux_avg_err',dblarr(2),$
;                     'lum_avg',0d,'lum_avg_err',dblarr(2),$
                     'norm_avg',0d,'norm_avg_err',dblarr(2),$
                     'alpha_avg2',0d,'alpha_avg2_err',dblarr(2),$
                     'flux_avg2',0d,'flux_avg2_err',dblarr(2),$
                     'norm_avg2',0d,'norm_avg2_err',dblarr(2),$
;                     'lum_avg2',0d,'lum_avg2_err',dblarr(2),$
                     'alpha_fit',0d,'alpha_fit_err',dblarr(2),$
                     'flux_fit',0d,'flux_fit_err',dblarr(2),$
;                     'lum_fit',0d,'lum_fit_err',dblarr(2),$
                     'alpha_und',0d,'alpha_und_err',dblarr(2),$
                     'flux_und',0d,'flux_und_err',dblarr(2),$
                     'flux_und2',0d,'flux_und2_err',dblarr(2),$
;                     'lum_und',0d,'lum_und_err',dblarr(2),$
;                     'lum_und2',0d,'lum_und2_err',dblarr(2),$
                     'intflux_fit',0d,'intflux_fit_err',dblarr(2),$
                     'intflux_und',0d,'intflux_und_err',dblarr(2),$
                     'intflux_extrap',0d,'intflux_extrap_err',dblarr(2),$
                     'fluxdens_avg',0d,'fluxdens_avg_err',dblarr(2),$
                     'fluxdens_avg2',0d,'fluxdens_avg2_err',dblarr(2),$
                     'fluxdens_fit',0d,'fluxdens_fit_err',dblarr(2),$
                     'fluxdens_und',0d,'fluxdens_und_err',dblarr(2),$
                     'fluxdens_und2',0d,'fluxdens_und2_err',dblarr(2),$
                     'lumdens_final',0d,'lumdens_final_err',dblarr(2),$
                     'alpha_final',0d,'alpha_final_err',dblarr(2),$
                     'unabs_cfratio',0d,'unabs_cfratioerr',dblarr(2),$
                     'beta',0.,'beta_err',fltarr(2),$
                     't90',0.,$,
                     't90start',0.,'t90end',0.,$
                     'tplateau_start',0.,'tplateau_end',0.,$
                     'batmodel_bestfit','','altmodel_bestfit','',$
                     'eiso',0d,'eiso_err',0d,'eiso_source','',$
                     'epeak',0d,'epeak_err',dblarr(2),'epeak_source','',$
                     'type','','jb','','tbreak',0d,$
                     'tbreaklim',0,'theta',0.,'egam',0d,$
                     'lum_peak',0d,'lum_peak_err',0d,$
                     'nflares',0,'sam_samp',0)
     
     g=replicate(g,n)
     
;     readcol,'~/jetbreaks/grb_tid_z_table.txt',grb,tid,z,delim='|',format='(a,a,a)',skip=3
;     tid=tid*1L
;     z=z*1.

     
;     readcol,'~/Swift/decay_lum_corr/GRBs_for_Judy.txt',sgrb,sz,spz,format='(a,a,a)',skip=1
     readcol,'~/stuff_for_people/Sam/ListofGRBs.txt',sgrb,format='(a)'
;     sz=sz*1.
;     spz=spz*1.

     ;;;; using 1 sigma errors
     s1=round(1000.*(1-0.67)/2.)
     s2=round(1000.*(1.-(1-0.67)/2.))

     g.grb=grbs
;     g2=mrdfits('~/Swift/swiftgrb_list.fits',1) ;;; combination
;     Davide's list & JD's

     ;;; grabbing both Greiner & Perley redshifts - ugh need better
     ;;;                                            composite list
;     g2=mrdfits('~/Swift/jochen_z_list.fits',1)
;     match,g.grb,g2.grb,m1,m2
;     match,strtrim(g.grb,2),strtrim(g2.grb,2),m1,m2
;     g[m1].z=g2[m2].z
;     match,strtrim(g.grb,2),strtrim(g2.grb,2)+'A',m1,m2
;     g[m1].z=g2[m2].z
;     g3=mrdfits('~/Swift/grbs_perley_z.fits',1)
;     match,strtrim(g.grb,2),strtrim(g3.grb,2),m1,m2
;     g[m1].z=g3[m2].z
;     match,strtrim(g.grb,2),strtrim(g3.grb,2)+'A',m1,m2
;     g[m1].z=g3[m2].z

     collect_grb_z,g


     ;;; prompt emission properties
     bat=mrdfits('~/jetbreaks/batcat.fits',1)
     match,strtrim(bat.grb,2),strtrim(g.grb,2),m1,m2
     g[m2].t90=bat[m1].t90
     g[m2].eiso=bat[m1].eiso1

     g[m2].batmodel_bestfit=bat[m1].batmodel_bestfit
     g[m2].altmodel_bestfit=bat[m1].model_bestfit

     g[m2].epeak=bat[m1].epeak_band ;*(1.+g[m2].z)
     wep=where(bat[m1].epeak_band ne 0 and bat[m1].epeak_band_err[0] ne 0)
     g[m2[wep]].epeak_err=bat[m1[wep]].epeak_band_err
     g[m2[wep]].epeak_source='Band'
     wep=where(bat[m1].epeak_band ne 0 and bat[m1].epeak_band_err[0] eq 0)
     g[m2[wep]].epeak_source='Taka'
     wep=where(bat[m1].epeak_cpl ne 0 and (bat[m1].batmodel_bestfit eq 'CPL' or strpos(bat[m1].model_bestfit,'COMP') ne -1))
     g[m2[wep]].epeak_err=bat[m1[wep]].epeak_cpl_err     
     g[m2[wep]].epeak_source='CPL'
     wep=where(strpos(bat[m1].model_bestfit,'SBPL') ne -1)
     g[m2[wep]].epeak_err=bat[m1[wep]].epeak_sbpl_err     
     g[m2[wep]].epeak_source='SBPL'

     wb=where(bat[m1].eiso1-bat[m1].eiso_bestfit eq 0 and bat[m1].batmodel_bestfit eq 'CPL')  ;; BAT best CPL
     g[m2[wb]].eiso_source='CPL'
     wb=where(bat[m1].eiso1-bat[m1].eiso_bestfit eq 0 and bat[m1].batmodel_bestfit eq 'PL ') ;; BAT best PL
     g[m2[wb]].eiso_source='PL'
     wb=where(bat[m1].eiso1-bat[m1].eiso_bestfit ne 0) ;; BAT PL - assume band
     g[m2[wb]].eiso_source='pBAND'
     wb=where(strpos(bat[m1].model_bestfit,'BAND') ne -1) ;; GBM best Band
     g[m2[wb]].eiso_source=bat[m1[wb]].model_bestfit;'BAND'

     sam=mrdfits('~/Swift/Sam_BATtable.fits',1)
     w0=where(sam.grb ne '0.0')
     sam=sam[w0]
     match,strtrim(g.grb),'GRB'+sam.grb,sm1,sm2
     g[sm1].t90start=sam[sm2].t90start
     g[sm1].t90end=sam[sm2].t90end


;     grbcat=mrdfits('~/Swift/swiftgrb.fits',1)
;     w0=where(g.t90 eq 0,nw0)
;     if nw0 ne 0 then begin
;        match,strtrim(g[w0].grb,2),strtrim(grbcat.name,2),m1,m2
;        g[w0[m1]].t90=grbcat[m2].bat_t90
;     endif 

     w0z=where(g.z ne 0 and g.epeak ne 0,nw0z)
;     match,strtrim(g[w0z].grb,2),strtrim(grbcat.name,2),m1,m2
     mpc2cm=3.08568025d24 

     dist=lumdist(g[w0z].z)*mpc2cm
     k=dblarr(nw0z)
     for kk=0,nw0z-1 do k[kk]=kcorr(g[w0z[kk]].z,[-1.,g[w0z[kk]].epeak,-2.5],/band)
     g[w0z].lfact_bat=4.*!pi*dist^2*k;(1.+g[w0z[m1]].z)^(g[w0z[m1]].beta-1.)
;     g[w0z[m1]].lum_peak=grbcat[m2].bat_peak_flux*g[w0z[m1]].lfact_bat
;     g[w0z[m1]].lum_peak_err=grbcat[m2].bat_peak_flux_err*g[w0z[m1]].lfact_bat

     ;;;; NEED TO GET THESE QUANTITIES FROM BATCAT
     ;;; MOVED BAT STUFF EARLIER BECAUSE NEED TO MAKES ALL TIMES TIME-T90


;     mwrfits,g,'~/stuff_for_people/Sam/lum_decay_corr.fits',/create


     ;;; fitting X-ray
     start=0
     print,'start: ',start
     stop
     for i=start,n-1 do begin
        cd,grbs[i]
        print,i,grbs[i]
;        wz=where(g[i].grb eq strcompress(grb,/rem),nwz)
;        if nwz gt 0 then begin 
        if g[i].z ne 0 then begin 
           lcfile='lc_fit_out_idl_int9.dat'
           lcerrfile='lc_fit_out_idl_int9_mc.fits'
           int='9'
;           if not exist(lcfile) then begin
;              lcfile='lc_fit_out_idl_int7.dat'
;              lcerrfile='lc_fit_out_idl_int7_mc.fits'
;              int='7'
;           endif 
           if exist(lcfile) and exist('UL_specfits.fits') then begin 
              read_lcfit,lcfile,pnames0,p0,perr0,nf=nf0,np=np0
              g[i].p[0:n_elements(p0)-1]=p0
              g[i].perr[*,0:n_elements(p0)-1]=perr0
              g[i].pnames[0:n_elements(pnames0)-1]=pnames0
              g[i].np=np0
              g[i].nflares=nf0
              if pnames0[0] ne 'nofit' then begin 
                 lc=lcout2fits(/phil)
                 lc.time=lc.time-g[i].t90start
                 spec=mrdfits('UL_specfits.fits',1)
                 if n_elements(spec) eq 1 then xs=n_elements(spec)-1 else begin 
                    wpc=where(lc.type eq 1)
                    if lc[wpc[0]].time ge t200 then xs=0 else xs=1
                 endelse
                 corr=spec[xs].unabs_cfratio
                 g[i].unabs_cfratio=corr/1.645 ;; convert 90% to 1-sigma via sigprob(0.9)
                 if tag_exist(spec,'unabs_cfratioerr') then g[i].unabs_cfratioerr=spec[xs].unabs_cfratioerr
                 g[i].beta=spec[xs].phind-1.
                 g[i].beta_err=spec[xs].phinderr/1.645
                 
                 g[i].tstart=lc[0].tstart
                 g[i].tstop=max(lc.tstop)
                 
                 if g[i].z gt 0 then begin 
                    g[i].t200=200.*(1.+g[i].z);+g[i].t90start ;;; don't need this because adjusted lc times
                    np=n_elements(p0)
                    mo=fit_models(pnames0,p0,np0,nf0,basemo=basemo,breaks=breaks)
;                 rmo=fit_models(pnames,p,rnp,rnf,basemo=rbasemo,/rem)
                    g[i].p[breaks]=g[i].p[breaks]-g[i].t90start
                    g[i].model=mo
                    g[i].basemodel=basemo
                    mo0=fit_models(pnames0,p0,np0,nf0,basemo=rbasemo,breaks=breaks0,/rembreak)


                    q=where(p0 ne 0)

                    ;;redo fits using data at t>t200
                    wt=where(lc.time gt g[i].t200 and lc.src_rate_err ne 0,nwt)
                    if nwt gt 2 then begin 
                       timerr=fltarr(2,n_elements(lc))
                       timerr[0,*]=lc.time-lc.tstart
                       timerr[1,*]=lc.tstop-lc.time
                       ab=linfit(alog10(lc[wt].time),alog10(lc[wt].src_rate))
                       p=[10^ab[0],-ab[1]]
                       intmo='intpow'
                       pnames=['norm','pow']
                       
                       fit_pow_model,lc[wt].time,lc[wt].src_rate,timerr[*,wt],lc[wt].src_rate_err,p,intmo,pnames,yfit,newp,perror,chisq,dof,weights,lc[wt].src_counts,lc[wt].tot_back_cts,status=status,breaks=0,noint=noint,pmin0=pmin0

;                    fitexy,alog10(lc[wt].time),alog10(lc[wt].src_rate),a,b,x_sig=lc[wt].tstop-lc[wt].time,y_sig=lc[wt].src_rate_err,sigab

                       g[i].alpha_avg=newp[1]
                       if not exist('lc_fit_out_idl_int3_mc.fits') or keyword_set(reallyredo) then begin 
                          lc_monte_pow,lc[wt],newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int='3',file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit
                       endif else mcfit=mrdfits('lc_fit_out_idl_int3_mc.fits',1)
                       s=sort(mcfit.pow)
                       g[i].alpha_avg_err=[mcfit[s[500]].pow-mcfit[s[s1]].pow,mcfit[s[s2]].pow-mcfit[s[500]].pow]
                       g[i].norm_avg=newp[0]
                       s=sort(mcfit.norm)                       
                       g[i].norm_avg_err=[mcfit[s[500]].norm-mcfit[s[s1]].norm,mcfit[s[s2]].norm-mcfit[s[500]].norm]

                       which_alpha,g[i].t200,p0,alpha_fit,aa,np=np0
                       g[i].alpha_fit=alpha_fit
                       mcfit0=mrdfits(lcerrfile,1)
                       s=sort(mcfit0.(aa))                       
                       g[i].alpha_fit_err=[mcfit0[s[500]].(aa)-mcfit0[s[s1]].(aa),mcfit0[s[s2]].(aa)-mcfit0[s[500]].(aa)]
                       g[i].alpha_und=g[i].alpha_fit
                       g[i].alpha_und_err=g[i].alpha_fit_err

                       g[i].flux_fit=call_function(g[i].model,g[i].t200,g[i].p)*g[i].unabs_cfratio
                       g[i].flux_und=call_function(g[i].basemodel,g[i].t200,g[i].p)*g[i].unabs_cfratio
                     
                       mcfit0=mrdfits('lc_fit_out_idl_int'+int+'_mc.fits',1)
                       f200=fltarr(1000) & f200a=fltarr(1000) & f200b=fltarr(1000)
                       for j=0,999 do begin
                          p00=0d
                          for k=0,n_tags(mcfit0)-1 do p00=[p00,mcfit0[j].(k)]
                          p00=p00[1:*]
                          f200[j]=call_function(mo,g[i].t200,p00)*g[i].unabs_cfratio
                          f200a[j]=call_function(basemo,g[i].t200,p00)*g[i].unabs_cfratio
                       endfor 

                       s=sort(f200)
                       g[i].flux_fit_err[0]=sqrt((f200[s[500]]-f200[s[s1]])^2+g[i].unabs_cfratioerr[0]^2*(g[i].flux_fit/g[i].unabs_cfratio)^2)
                       g[i].flux_fit_err[1]=sqrt((f200[s[s2]]-f200[s[500]])^2+g[i].unabs_cfratioerr[1]^2*(g[i].flux_fit/g[i].unabs_cfratio)^2)
                       s=sort(f200a)
                       g[i].flux_und_err[0]=sqrt((f200a[s[500]]-f200a[s[s1]])^2+g[i].unabs_cfratioerr[0]^2*(g[i].flux_und/g[i].unabs_cfratio)^2)
                       g[i].flux_und_err[1]=sqrt((f200a[s[s2]]-f200a[s[500]])^2+g[i].unabs_cfratioerr[1]^2*(g[i].flux_und/g[i].unabs_cfratio)^2)
                       
                       g[i].flux_avg=pow(g[i].t200,[g[i].norm_avg,g[i].alpha_avg])*g[i].unabs_cfratio
                       f200=fltarr(1000)
                       norm=mcfit.norm
                       for j=0,999 do f200[j]=pow(g[i].t200,[norm[j],mcfit[j].pow])*g[i].unabs_cfratio
                       s=sort(f200)
                       ;; fluxerr=sqrt((flux/unabs_cfratio)^2*unabs_cfratio^2+unabs_cfratioerr^2*(flux/unabs_cfratio)^2)
                       g[i].flux_avg_err[0]=sqrt((f200[s[500]]-f200[s[s1]])^2+g[i].unabs_cfratioerr[0]^2*(g[i].flux_avg/g[i].unabs_cfratio)^2)
                       g[i].flux_avg_err[1]=sqrt((f200[s[s2]]-f200[s[500]])^2+g[i].unabs_cfratioerr[1]^2*(g[i].flux_avg/g[i].unabs_cfratio)^2)

                       colprint,g[i].flux_avg,f200[s[500]],g[i].flux_avg_err[0],g[i].flux_avg_err[1]
                       dist=lumdist(g[i].z)*mpc2cm
                       lfact=4.*!pi*dist^2*(1.+g[i].z)^(g[i].beta-1.)
                       g[i].lfact=lfact

                       ;; g[i].lum_avg=g[i].flux_avg*lfact              
                       ;; g[i].lum_avg_err[0]=g[i].flux_avg_err[0]*lfact
                       ;; g[i].lum_avg_err[1]=g[i].flux_avg_err[1]*lfact

                       ;; g[i].lum_fit=g[i].flux_fit*lfact
                       ;; g[i].lum_fit_err[0]=g[i].flux_fit_err[0]*lfact
                       ;; g[i].lum_fit_err[1]=g[i].flux_fit_err[1]*lfact

                       ;; g[i].lum_und=g[i].flux_und*lfact
                       ;; g[i].lum_und_err[0]=g[i].flux_und_err[0]*lfact
                       ;; g[i].lum_und_err[1]=g[i].flux_und_err[1]*lfact

                       g[i].fluxdens_avg=flux2jy(g[i].flux_avg,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg_err[0],ferr=ferr)*1d-23
                       g[i].fluxdens_avg_err=ferr*1d-23
                       g[i].fluxdens_fit=flux2jy(g[i].flux_fit,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_fit_err[0],ferr=ferr)*1d-23
                       g[i].fluxdens_fit_err=ferr*1d-23
                       g[i].fluxdens_und=flux2jy(g[i].flux_und,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_und_err[0],ferr=ferr)*1d-23
                       g[i].fluxdens_und_err=ferr*1d-23

                       find_jetbreak,g[i].grb,type,jb,tbreak,tbreaklim,/go
                       g[i].type=type
                       if jb le 0 then g[i].jb='jb-after'
                       if jb eq 1 then g[i].jb='yes      '
                       if jb eq 2 then g[i].jb='jb-before'
                       if jb eq 3 then g[i].jb='jb-before'
                       g[i].theta=jet_angle(tbreak/86400.,z=g[i].z,eiso=g[i].eiso,eta=eta)
                       g[i].tbreak=tbreak
                       g[i].tbreaklim=tbreaklim
                       g[i].egam=g[i].eiso*(1.-cos(g[i].theta*!dtor))

                             ;; add times at start and end of plateau
                       segs=str_sep(g[i].type,'-')
                       wp0=where(segs eq '0',nw0)
                       wp1=where(segs eq 'I',nw1)
                       wp2=where(segs eq 'II',nw2)	
                       wp3=where(segs eq 'III',nw3)	
                       if nw1 gt 0 and (nw2 gt 0 or nw3 gt 0) then g[i].tplateau_start=g[i].p[wp1[0]*2+2]
                       if nw2 gt 0 and nw3 gt 0 then g[i].tplateau_end=g[i].p[wp2[0]*2+2]

                       ;;; extrapolation correction
                       if g[i].tplateau_start gt g[i].t200 then begin
                          if nw0 gt 0 and n_elements(breaks) ge 2 then rbasemo='bkn'+ntostr(n_elements(breaks)-2)+'pow'
                          if rbasemo eq 'bkn1pow' then rbasemo='bknpow'
                          if rbasemo eq 'bkn0pow' then rbasemo='pow'
                          newnorm=call_function(basemo,g[i].p[wp1*2+2],g[i].p)/call_function(rbasemo,g[i].p[wp1*2+2],[1.,g[i].p[wp1*2+3:*]])
                          g[i].flux_und2=g[i].unabs_cfratio*call_function(rbasemo,g[i].t200,[newnorm,g[i].p[wp1*2+3:*]])

                          f200b=fltarr(1000)
                          print,g[i].grb
                          for j=0,999 do begin
                             p00=0d
                             for k=0,n_tags(mcfit0)-1 do p00=[p00,mcfit0[j].(k)]
                             p00=p00[1:*]
                             newnorm=call_function(basemo,g[i].p[wp1*2+2],p00)/call_function(rbasemo,g[i].p[wp1*2+2],[1.,p00[wp1*2+3:*]])

                             f200b[j]=call_function(rbasemo,g[i].t200,[newnorm,p00[wp1*2+3:*]])*g[i].unabs_cfratio
                          endfor 
                          sb=sort(f200b)

                          g[i].flux_und2_err[0]=sqrt((f200b[sb[500]]-f200b[sb[s1]])^2+g[i].unabs_cfratioerr[0]^2*(g[i].flux_und2/g[i].unabs_cfratio)^2)
                          g[i].flux_und2_err[1]=sqrt((f200b[sb[s2]]-f200b[sb[500]])^2+g[i].unabs_cfratioerr[1]^2*(g[i].flux_und2/g[i].unabs_cfratio)^2)
                          g[i].fluxdens_und2=flux2jy(g[i].flux_und2,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_und2_err[0],ferr=ferr)*1d-23
                          g[i].fluxdens_und2_err=ferr*1d-23
;                          if g[i].flux_und2-g[i].flux_und2_err[0] lt 0 then stop
                       endif

                       tstart=200./(1+g[i].z)
                       tstop=1e5/(1+g[i].z)
                       g[i].intflux_fit=qpint1d(strtrim(g[i].model,2),tstart,tstop,g[i].p)*g[i].unabs_cfratio
                       g[i].intflux_fit_err=g[i].intflux_fit*g[i].flux_fit_err/g[i].flux_fit

                       g[i].intflux_und=qpint1d(strtrim(g[i].basemodel,2),tstart,tstop,g[i].p)*g[i].unabs_cfratio
                       g[i].intflux_und_err=g[i].intflux_und*g[i].flux_und_err/g[i].flux_und
                       if g[i].np gt 2 and g[i].p[1] gt g[i].p[3] then begin 
                          wp0=where(g[i].p ne 0)
                          rmo=fit_models(g[i].pnames,g[i].p[wp0],np,/rembreak,basemo=rbasemo)
                          intmo='int'+strtrim(rbasemo,2)
                          tt=g[i].p[2]*2.
                          tmp=execute('norm='+g[i].basemodel+'(tt,g[i].p)/'+rbasemo+'(tt,[1.,g[i].p[3:*]])')
                          p=[norm,g[i].p[3:*]]
                          g[i].intflux_extrap=qpint1d(strtrim(rbasemo,2),tstart,tstop,p)*g[i].unabs_cfratio
                          g[i].intflux_extrap_err=g[i].intflux_extrap*g[i].flux_und_err/g[i].flux_und
         
                       endif 


                       if strpos(type,'0-I') ne -1 then bind=4 else bind=2  ;;; make sure to grab break after steep decay
;;; need delta alpha>1 added, but careful of 0-I
                       if strpos(type,'I-II') ne -1 then delalp=g[i].p[bind-1]-g[i].p[bind+1] else delalp=0
                       if strpos(type,'II-III') eq 0 then delalp=10.
                       print,type,delalp, g[i].p[bind]-g[i].t200

                       if strpos(type,'I-II') ne -1 and type ne 'II-III' and type ne 'II-III-IV' and type ne 'II-III-IV-V' and type ne 'II-III-IV-V-VI' and g[i].tplateau_start gt g[i].t200 and delalp gt 1. or keyword_set(reallyredo) and n_elements(lc) gt 3 then begin                 
;                       if g[i].tplateau_start gt t[i].t200 or keyword_set(reallyredo) then begin 
                          if not exist('lc_fit_out_idl_int4.dat') then begin 
                             wt2=where(lc.time gt g[i].p[bind] and lc.src_rate_err ne 0,nwt)
                             if nwt ge 3 then begin 
                                ab=linfit(alog10(lc[wt2].time),alog10(lc[wt2].src_rate))
                                p=[10^ab[0],-ab[1]]
                                intmo='intpow'
                                pnames=['norm','pow']
                                
                                fit_pow_model,lc[wt2].time,lc[wt2].src_rate,timerr[*,wt2],lc[wt2].src_rate_err,p,intmo,pnames,yfit,newp,perror,chisq,dof,weights,lc[wt2].src_counts,lc[wt2].tot_back_cts,status=status,breaks=0,noint=noint,pmin0=pmin0
                                lc_monte_pow,lc[wt2],newp,pnames,chisq,dof,perror2,ps=ps,nsim=1000,int='4',file=file,/noplot,nsig=nsig,breaks=0,mcfit=mcfit
                             endif 
                          endif else begin 
;else $
;                          if exist('lc_fit_out_idl_int4_mc.fits') then spawn,'rm lc_fit_out_idl_int4.dat lc_fit_out_idl_int4_mc.fits'

;                       if exist('lc_fit_out_idl_int4_mc.fits') then begin 
                             mcfit=mrdfits('lc_fit_out_idl_int4_mc.fits',1)
                             s=sort(mcfit.pow)
                             read_lcfit,'lc_fit_out_idl_int4.dat',pnames2,newp2
                             g[i].alpha_avg2=newp2[1]
                             g[i].alpha_avg2_err=[mcfit[s[500]].pow-mcfit[s[s1]].pow,mcfit[s[s2]].pow-mcfit[s[500]].pow]
                             g[i].norm_avg2=newp2[0]
                             s=sort(mcfit.norm)                       
                             g[i].norm_avg2_err=[mcfit[s[500]].norm-mcfit[s[s1]].norm,mcfit[s[s2]].norm-mcfit[s[500]].norm]

                             g[i].flux_avg2=pow(g[i].t200,[g[i].norm_avg2,g[i].alpha_avg2])*g[i].unabs_cfratio
                             f200=fltarr(1000)
                             norm=mcfit.norm
                             for j=0,999 do f200[j]=pow(g[i].t200,[norm[j],mcfit[j].pow])*g[i].unabs_cfratio
                             s=sort(f200)
                             g[i].flux_avg2_err[0]=f200[s[500]]-f200[s[s1]]
                             g[i].flux_avg2_err[1]=f200[s[s2]]-f200[s[500]]
;                             g[i].lum_avg2=g[i].flux_avg2*lfact
;                             g[i].lum_avg2_err[0]=g[i].flux_avg2_err[0]*lfact
;                             g[i].lum_avg2_err[1]=g[i].flux_avg2_err[1]*lfact
                             g[i].fluxdens_avg2=flux2jy(g[i].flux_avg2,g[i].beta+1.,gammaerr=g[i].beta_err,fluxerr=g[i].flux_avg2_err[0],ferr=ferr)*1d-23
                             g[i].fluxdens_avg2_err=ferr*1d-23
                       
                          endelse 
                       endif
                    endif 
                 endif 
              endif 
           endif
        endif  
        cd,'..'
     endfor 

     w=where(g.model ne '' and finite(g.flux_avg) and g.flux_avg gt 0 and g.flux_und2-g.flux_und2_err[0] ge 0)
     g=g[w]

     match,strtrim(g.grb,2),strtrim(sgrb,2),m1,m2
     g[m1].sam_samp=1

     w=where(g.eiso eq 0)
     g[w].egam=0
     g[w].theta=0

     ;;; special case of only steep
     wbad=where(g.alpha_avg gt 3 or g.fluxdens_und2 gt 1)
     g[wbad].alpha_avg2=g[wbad].alpha_avg
     g[wbad].alpha_avg2_err=g[wbad].alpha_avg_err
     g[wbad].flux_avg2=g[wbad].flux_avg
     g[wbad].flux_avg2_err=g[wbad].flux_avg_err
;     g[wbad].lum_avg2=g[wbad].lum_avg
;     g[wbad].lum_avg2_err=g[wbad].lum_avg_err
     g[wbad].fluxdens_avg2=g[wbad].fluxdens_avg
     g[wbad].fluxdens_avg2_err=g[wbad].fluxdens_avg_err
     g[wbad].norm_avg2=g[wbad].norm_avg
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;;; get final results
     g.lumdens_final=g.fluxdens_und*g.lfact
     g.lumdens_final_err[0]=g.fluxdens_und_err[0]*g.lfact
     g.lumdens_final_err[1]=g.fluxdens_und_err[1]*g.lfact
     w=where(g.fluxdens_und2 gt 0 and g.alpha_avg2 gt 0)
     g[w].lumdens_final=g[w].fluxdens_und2*g[w].lfact
     g[w].lumdens_final_err[0]=g[w].fluxdens_und2_err[0]*g[w].lfact
     g[w].lumdens_final_err[1]=g[w].fluxdens_und2_err[1]*g[w].lfact
     g.alpha_final=g.alpha_avg
     g.alpha_final_err=g.alpha_avg_err
     g[w].alpha_final=g[w].alpha_avg2
     g[w].alpha_final_err=g[w].alpha_avg2_err

     ;;; resetting to use same criteria for both avg2 & und2
     w2=where(g.fluxdens_und2 gt 0 and g.alpha_avg2 eq 0)
     g[w2].flux_und2=0
     g[w2].flux_und2_err=0

     g[w2].fluxdens_und2=0
     g[w2].fluxdens_und2_err=0

     mwrfits,g,'~/Swift/decay_lum_corr/lum_decay_corr.fits',/create
  endif else g=mrdfits('~/Swift/decay_lum_corr/lum_decay_corr.fits',1)

  w=where(g.tstart lt g.t200 and finite(g.flux_avg)); and g.lum_avg ne 0 and g.lum_avg_err[0] ne 0)
  g=g[w]
  s=where(g.t90 le 2.)
  l=where(g.t90 gt 2.)
;  f=where(g.nflares gt 0)
;  nf=where(g.nflares eq 0)
;  g=g[l]
;  xrange=[1d45,1d51]
  sn=sqrt((g.alpha_avg_err[1]/g.alpha_avg)^2+(g.flux_avg_err[1]/g.flux_avg)^2)
  w=where(sn lt 3.*mean(sn))
;  g=g[w]
  
;  xrange=[44,53]
  xrange=[1d26,1d33]
;  yrange=[-2.5,0.5]
  yrange=[-4,2]
  
  fit=create_struct('lc','','char','',$
                    'slope',0.,'slope_err',fltarr(2),'const',0.,'const_err',fltarr(2),$
                    'spearman_rank',0.,'null_hypo',0.,'signif',0.,$
                    'partspear_rank',0.,'partspear_null_hypo',0.,'partspear_signif',0.)
  fit=replicate(fit,42)

  begplot,name='~/Swift/decay_lum_corr/flux_decay.eps',/color,/land;,font='helvetica'
  !x.margin=[3,2]
  !y.margin=[3,2]
;  !p.multi=[0,1,3]
  plotsym,0,1

  fi=0
  for j=0,0 do begin ;;; loop over fluxes (avg, fit, und) - don't bother?
     case j of
        0: begin
;           lum=g.lum_avg
;           lumerr=g.lum_avg_err
           lum=g.lumdens_final;g.fluxdens_avg*g.lfact
;           lumerr=dblarr(2,n_elements(g))
           lumerr=g.lumdens_final_err
;           lumerr[0,*]=g.fluxdens_avg_err[0]*g.lfact
;           lumerr[1,*]=g.fluxdens_avg_err[1]*g.lfact
           alpha=g.alpha_avg
           alphaerr=g.alpha_avg_err
           add='avg'
        end 
        1: begin
;           lum=g.lum_fit
;           lumerr=g.lum_fit_err
           lum=g.fluxdens_fit*g.lfact
           lumerr=dblarr(2,n_elements(g))
           lumerr[0,*]=g.fluxdens_fit_err[0]*g.lfact
           lumerr[1,*]=g.fluxdens_fit_err[1]*g.lfact
           alpha=g.alpha_fit
           alphaerr=g.alpha_fit_err
           add='fit'
        end
        2: begin
;           lum=g.lum_und
;           lumerr=g.lum_und_err
           lum=g.fluxdens_und*g.lfact
           lumerr=dblarr(2,n_elements(g))
           lumerr[0,*]=g.fluxdens_und_err[0]*g.lfact
           lumerr[1,*]=g.fluxdens_und_err[1]*g.lfact
           alpha=g.alpha_und
           alphaerr=g.alpha_und_err
           add='und'
        end
     endcase 
     for k=0,6 do begin  ;;; loop over sample cuts (long/short, flares/no flares)
        case k of
           0: begin ;; all
              w1=indgen(n_elements(g))
              w2=-1
              leg=['All']
           end 
           1: begin ;; t200>tb or t200<tb
              leg=['t!Lb!N>t!L200!N - steep decay contam','t!Lb!N<t!L200!N - good']
              w1=where((g.alpha_avg2 ne 0 and abs(g.alpha_avg2-g.alpha_avg) gt 0.05) or g.alpha_avg gt 3.)
              w2=where((g.alpha_avg2 eq 0 or abs(g.alpha_avg2-g.alpha_avg) lt 0.05) and g.alpha_avg lt 3.)
           end
           2: begin ;; short/long
              w1=where(g.t90 le 2. and g.t90 ne 0 and g.alpha_avg2 le 3)
              w2=where(g.t90 gt 2. and g.alpha_avg2 le 3)
              leg=['short','long']
              ;;; fix steep decay contamination
;              wcontam=where((g.alpha_avg2 ne 0 and abs(g.alpha_avg2-g.alpha_avg) gt 0.05) or g.alpha_avg gt 3.)
;              alpha[wcontam]=g[wcontam].alpha_avg2
;              alphaerr[*,wcontam]=g[wcontam].alpha_avg2_err
;              lum[wcontam]=g[wcontam].fluxdens_avg2*g[wcontam].lfact
;              lumerr[0,wcontam]=g[wcontam].fluxdens_avg2_err[0]*g[wcontam].lfact
;              lumerr[1,wcontam]=g[wcontam].fluxdens_avg2_err[1]*g[wcontam].lfact
              alpha=g.alpha_final
              alphaerr=g.alpha_final_err
              lum=g.lumdens_final
              lumerr=g.lumdens_final_err
           end
           3: begin ;; flares/noflares
              w1=where(g.nflares gt 0 and g.t90 gt 2. and g.alpha_avg2 le 3)
              w2=where(g.nflares eq 0 and g.t90 gt 2. and g.alpha_avg2 le 3)
              leg=['Flares','No Flares']
           end
           4: begin ;; pow or other
              model=strtrim(g.model,2)
              ind=intarr(n_elements(g))
              w1=where((model eq 'pow' or model eq 'gauss1_pow' or model eq 'gauss2_pow' or model eq 'gauss3_pow' or model eq 'gauss4_pow' or model eq 'gauss5_pow') and g.t90 gt 2. and g.alpha_avg2 le 3)
              ind[w1]=1
              w2=where(ind eq 0 and g.t90 gt 2. and g.alpha_avg2 le 3)
              leg=['PL, long','breaks, long']
           end
           5: begin ;; plateau/ no plateau
              w1=where((strpos(g.type,'II-III') ne -1 or strpos(g.type,'II-IV') ne -1) and g.t90 gt 2. and g.alpha_avg2 le 3)
              w2=where((strpos(g.type,'II-III') eq -1 and strpos(g.type,'II-IV') eq -1) and g.t90 gt 2. and g.alpha_avg2 le 3)
              leg=['Have Plateau, long, corrected','No Plateau, long, corrected']
           end 
           6: begin
              w1=where(g.t90 gt 2. and g.alpha_avg2 le 3)
              w2=-1
              leg=['Long, steep decay corrected']
           end 
        endcase 

        plot,[1d26,1d33],[-2.5,0],/nodata,xtitle='Lum!LX,200s!N (erg s!U-1!N Hz!U-1!N)',ytitle=!tsym.alpha+'!L'+add+',X,>200s!N',xrange=xrange,yrange=yrange,charsize=2.,/xsty,/ysty,/xlog,xminor=9
        print,leg

        for q=0,1 do begin
           case q of
              0: begin
                 w=w1
                 color=!p.color
                 lcolor=!p.color
                 psym=8
              end
              1: begin
                 w=w2
                 color=!red
                 lcolor=!red
                 psym=5
              end
           endcase 
           if w[0] ne -1 then begin
              fit[fi].char=leg[q]
              fit[fi].lc=add
              oploterror2,lum[w],-alpha[w],10^[[alog10(lum[w])-alog10(lum[w]-lumerr[0,w])],[alog10(lum[w]+lumerr[1,w])-alog10(lum[w])]],alphaerr[*,w],psym=psym,color=color

              x=alog10(lum[w])
              y=-alpha[w]
              x_sig=alog10(lum[w])-alog10(lum[w]-lumerr[0,w])
              y_sig=alphaerr[0,w]
              
;              fitexy,alog10(lum[w]),-alpha[w],a,b,x_sig=alog10(lum[w])-alog10(lum[w]-lumerr[0,w]),y_sig=alphaerr[0,w]
              fitexy,x,y,a,b,x_sig=x_sig,y_sig=y_sig
;stop
;              r=mpfitexy(x,y,x_sig,y_sig,/quiet,guess=[-0.2,1],minchi=chi,dof=dof,errors=errors) ;;; winner!
;              b=r[0]
;              const=r[1]
              const=a
              fit[fi].slope=b
              fit[fi].const=a;const
;              r1=[b,a]
              ;;;;inverse: slope=1/b, intercept=-a/b


              ;;; HOW TO GO FROM BOOTSTRAP ERROR ON PARAMETERS TO
              ;;; ERROR CONTOURS?????????
              ;;; bootstrap error
              if not keyword_set(noerror) then begin 
                 aa=fltarr(1e4) & bb=fltarr(1e4)
                 n=n_elements(x)
                 for o=0,1e4-1 do begin
                    r=round(randomu(seed,n)*n)
;                    r1=mpfitexy(x[r],y[r],x_sig[r],y_sig[r],/quiet,guess=[-0.2,1],minchi=chi,dof=dof)
                    fitexy,x[r],y[r],x_sig=x_sig[r],y_sig=y_sig[w],a0,b0
                    aa[o]=a0
                    bb[o]=b0
;                    r1=[a0,b0]
;                    oplot,x,aa[o]+bb[o]*x,color=!pink
                 endfor 
                 a_err=mc_error(aa,sig=1.)
                 b_err=mc_error(bb,sig=1.)
                 fit[fi].slope_err=b_err
                 fit[fi].const_err=a_err
              endif 

              c=r_correlate(alog10(lum[w]),-alpha[w],zd=zd,probd=probd)

;              sig=sigprob((1d)-c[1])
              rsig=mpnormlim(c[1],/SLEVEL)
              fit[fi].spearman_rank=c[0]
              fit[fi].null_hypo=c[1]
              fit[fi].signif=rsig
              print,'Spearman rank coeff & prob & sig: ',c[0],c[1],rsig
              pr=pr_correlate(x,y,g[w].z,/silent)
              prsig=mpnormlim(pr[1],/SLEVEL)
              fit[fi].partspear_rank=pr[0]
              fit[fi].partspear_null_hypo=pr[1]
              fit[fi].partspear_signif=prsig

              print,'partial spearman rank coeff & prob & sig: ',pr[0],pr[1],prsig

;              good=where(yerr gt 0)
;              r=regress(x[good],y[good],const=const,yfit=yfit,correlation=linearcorr,measure_error=yerr[good])
;              print,'regress: ',r[0],const
;              line=(r[0]*x+const)
;stop

              line=b*x+const
              oplot,10^x,line,color=lcolor
;              oplot,x[good],yfit,color=lcolor
              mdist=fltarr(n_elements(w))
              for i=0,n_elements(w)-1 do begin
                 dists=sqrt((x[i]-x)^2+(-y[i]-line)^2)
                 mdist[i]=min(dists)
              endfor 
              plothist,mdist,x2,y2,bin=0.1,/noplot
              if j eq 0 then begin 
                 ga=gaussfit(x2,y2,a2,nterms=3)

;                 plus=r[0]*x+r[1]+a[2]*cos(r[0])*2.
;                 minus=r[0]*x+r[1]-a[2]*cos(r[0])*2.
                 am=sqrt(mean((mdist-mean(mdist))^2))
                 plus=b*x+const+am*cos(b)*2.  ;;; 2 sigma error
                 minus=b*x+const-am*cos(b)*2.
                 s=sort(x)
                 oplot, 10^x[s],plus[s], line=2,color=lcolor
                 oplot, 10^x[s],minus[s],line=2,color=lcolor

              endif 
              if q eq 0 then begin
                 c1=c
;                 r1=r
                 a1=a
                 b1=b
;                 const1=const
                 sig=rsig
              endif
           endif  
           fi=fi+1
        endfor 
        if n_elements(leg) eq 2 then begin
           legend,['R='+numdec(c1[0],3)+'  p='+numdec(c1[1],2,/sci)+'  sig='+numdec(sig,1)+'  slope='+numdec(b1,3)+'  const='+numdec(a1,3),$
                   'R='+numdec(c[0],3)+'  p='+numdec(c[1],2,/sci)+'  sig='+numdec(rsig,1)+'  slope='+numdec(b,3)+'  const='+numdec(a,3)],/top,/right,box=0,textcolor=[!p.color,!red]
           legend,leg,box=0,/bottom,/left,textcolor=[!p.color,!red],psym=[8,5],color=[!p.color,!red]
        endif else begin
           
           legend,['R='+numdec(c1[0],3)+'  p='+numdec(c[1],2,/sci)+'  sig='+numdec(rsig,1)+'  slope='+numdec(b,3)+'  const='+numdec(a,3)],/top,/right,box=0
;           legend,leg,box=0,/bottom,/left,psym=8
        endelse 

;  legend,['long','short','Flares','No
;  Flares'],box=0,/bottom,/left,textcolor=[!p.color,!red,!p.color,!p.color],psym=[3,3,8,2]
     endfor 
     print
  endfor 

  if not keyword_set(noerror) then mwrfits,fit,'~/Swift/decay_lum_corr/fit_results.fits',/create
  endplot
  spawn,'ps2pdf ~/Swift/decay_lum_corr/flux_decay.eps ~/Swift/decay_lum_corr/flux_decay.pdf'

  ;;; need to remake this distance into a line

;;   number=n_elements(g)
;;   sum=0 & plus=fltarr(number) & minus=fltarr(number)
;;   for i=0, number-1 do sum=sum+(y[i]-(r*x[i]+const))^2
;;   sd=(sum/(number))^0.5

;;   for i=0, number-1 do plus[i]=r*x[i]+const+(3*sd)
;;   for i=0, number-1 do minus[i]=r*x[i]+const-(3*sd)
;;   s=sort(x)
;;   oplot, x[s],plus[s], line=2,color=!blue
;;   oplot, x[s],minus[s],line=2,color=!blue

;  legend,['long','short'],box=0,/top,/right,textcolor=[!p.color,!red]

  stop
  
  ;; need to add z & eiso & t90 & errors
  ;; need to make plots
  ;;   separate out short/long
  ;;   need to separate out extrapolations


  return
end 
