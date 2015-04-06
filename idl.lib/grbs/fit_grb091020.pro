@fit_functions
pro plot_uvot_mag_lc,ps=ps

  if keyword_set(ps) then begplot,name='uvot_mag_lc.ps',/land,font='helvetica'
  file3='~/Desktop/GRB091020/TOnormalize.txt'
  readcol,file3,time3,terr3,rate3,err3,tmp,flux3,fluxerr3,filter3,files3,format='(d,d,d,d,a,d,d,a,a)'
  w=where(filter3 eq 'WHITE' and rate3 gt 0,nw)
  mag=20.29-2.5*alog10(rate3[w])
  magerr1=20.29-2.5*alog10(rate3[w]+err3[w])-mag
  rme=rate3[w]-err3[w]
  q=where(rme lt 0)
  rme[q]=rate3[w[q]]
  magerr0=mag-(20.29-2.5*alog10(rme))

  plot,time3[w],mag,psym=3,/xlog,xrange=[40,1.5e6],yrange=[25,15],xtitle='Time since BAT trigger (s)',ytitle='white filter (magnitudes)',/xsty  
  for i=0,nw-1 do begin
     oplot,[time3[w[i]]-terr3[w[i]],time3[w[i]]+terr3[w[i]]],[mag[i],mag[i]]
     oplot,[time3[w[i]],time3[w[i]]],[mag[i]-magerr0[i],mag[i]+magerr1[i]]
  endfor 
  if keyword_set(ps) then endplot
stop
end 
function int_pow_2bkn2pow,t,p,f1,f2,f3

  p1=p[0:1]
  p2=p[2:7]
  p3=p[8:13]
  
  w=where(t[0,1:*]-t[0,0:n_elements(t[0,*])-2] lt 0)
;  tx=time[0:w[0]]
;  to=time[w[0]+1:*]
  w1=indgen(w[0]+1)
  w2=indgen(n_elements(t[0,*])-w[0])+w[0]+1

  f1=intpow(t,p1)
  f2=intbkn2pow(t,p2)
  f3=intbkn2pow(t,p3)
  f1[0,w2]=0
  f2[0,w2]=0
  f3[0,w1]=0

  yfit=f1+f2+f3

  return,yfit
end 

function pow_2bkn2pow,t,p,f1,f2,f3

  p1=p[0:1]
  p2=p[2:7]
  p3=p[8:13]

  w=where(t[1:*]-t[0:n_elements(t)-2] lt 0)
  w1=indgen(w[0]+1)
  w2=indgen(n_elements(t)-w[0])+w[0]+1
;  tx=time[0:w[0]]
;  to=time[w[0]+1:*]

  f1=pow(t,p1)
  f2=bkn2pow(t,p2)
  f3=bkn2pow(t,p3)
  f1[w2]=0
  f2[w2]=0
  f3[w1]=0

  yfit=f1+f2+f3
  return,yfit
end 

pro read_alldata,data=data

  d=create_struct('time',0d,'terr',dblarr(2),'fd',0d,'fderr',0d,$
                  'filter','','telescope','','color',0L,'freq',0d)
  data=replicate(d,1000)

  readcol,'vla.dat',grb,tel,year,mon,day,delt,freq,rflux,rfluxerr,format='(a,a,a,a,f,f,f,f,f)'
;  rdate=21.900517
  rdate=date2met('2009-10-20-21:36:44.67')
  month=month_cnv(mon)
  dd=fix(day)
  hr=fix((day-dd)*24.)
  mn=fix(((day-dd)*24.-hr)*60.)
  sec=((((day-dd)*24.-hr)*60.)-mn)*60.
  rtime=dblarr(n_elements(day))
  for i=0,n_elements(day)-1 do rtime[i]=date2met(year[i]+'-'+ntostr(month[i])+'-'+ntostr(dd[i])+'-'+ntostr(hr[i])+':'+ntostr(mn[i])+':'+ntostr(sec[i]))-rdate
  rtime=delt*24.*60.*60.
  nr=n_elements(rtime)
  data[0:nr-1].time=rtime
  data[0:nr-1].fd=rflux*1d-6
  data[0:nr-1].fderr=rfluxerr*1d-6
  data[0:nr-1].filter=ntostr(freq,3)+'GHz'
  data[0:nr-1].telescope='VLA'
  data[0:nr-1].color=!magenta
  data[0:nr-1].freq=freq*1d9

  ;;; apply extinction correction
  ra=175.730123d
  dec=50.97833d
  calc_dust_corr,ra,dec,corr,/noplot,filter=filter

  readcol,'tls.dat',dt,filt,tmag,terr,format='(f,a,f,f)'
  ttime=dt*24.*60.*60.
  match_filters,filt,ind,filt1=filt1
  tmag2=tmag-corr[ind]
  colprint,filt1[ind],filt,corr[ind],tmag,tmag2
  tfd=opt2jy(tmag2,filt,freq=freq)
  tfderr=opt2jy(tmag2-terr,filt)-tfd
  nt=n_elements(tmag2)
  data[nr:nr+nt-1].time=ttime
  data[nr:nr+nt-1].fd=tfd
  data[nr:nr+nt-1].fderr=tfderr
  data[nr:nr+nt-1].filter=filt
  data[nr:nr+nt-1].telescope='TLS'
  data[nr:nr+nt-1].freq=freq

  readcol,'master.dat',mdt,mmag,merr,format='(f,f,f)'
  mtime=mdt*60.*60.
  filt='R'
  match_filters,filt,ind,filt1=filt1
  mmag2=mmag-corr[ind[0]]
  nm=n_elements(mmag2)
  colprint,replicate(filt1[ind],nm),replicate(filt,nm),replicate(corr[ind],nm),mmag,mmag2
  mfd=opt2jy(mmag2,filt1[ind[0]],freq=freq)
  mfderr=opt2jy(mmag2-merr,filt1[ind])-mfd
  data[nr+nt:nr+nt+nm-1].time=mtime
  data[nr+nt:nr+nt+nm-1].fd=mfd
  data[nr+nt:nr+nt+nm-1].fderr=mfderr
  data[nr+nt:nr+nt+nm-1].filter=filt
  data[nr+nt:nr+nt+nm-1].telescope='MASTER'
  data[nr+nt:nr+nt+nm-1].freq=freq
stop
  readcol,'nickel.dat',ntstart,ntstop,nexp,nfilt,nmag,nerr,format='(f,f,f,a,f,f)'
  ntime=((ntstop-ntstart)/2.+ntstart)*86400.
  nterr=(ntstop-ntstart)/2.*86400.
  match_filters,nfilt,ind,filt1=filt1
  nmag2=nmag-corr[ind]
  colprint,filt1[ind],nfilt,corr[ind],nmag,nmag2
  nfd=opt2jy(nmag2,nfilt,freq=freq)
  nfderr=opt2jy(nmag2-nerr,nfilt)-nfd
  nn=n_elements(nmag2)
  data[nr+nt+nm:nr+nt+nm+nn-1].time=ntime
  data[nr+nt+nm:nr+nt+nm+nn-1].fd=nfd
  data[nr+nt+nm:nr+nt+nm+nn-1].fderr=nfderr
  data[nr+nt+nm:nr+nt+nm+nn-1].filter=nfilt
  data[nr+nt+nm:nr+nt+nm+nn-1].telescope='Nickel'
  data[nr+nt+nm:nr+nt+nm+nn-1].freq=freq

  read_uvot,uvot,/noplot
  u=where(uvot.filter ne 'norm',nu)
  u=u[sort(uvot[u].time)]
  w=where(uvot[u].rate lt 0); or uvot[u].rate-uvot[u].raterr lt 0)
  uvot[u[w]].rate=uvot[u[w]].rate+3.*uvot[u[w]].raterr
  uvot[u[w]].raterr=-1.
  uvot=uvot[u]
;  w=where(uvot[u].rate gt 0,nu); and uvot[u].raterr/uvot[u].rate lt 0.1,nu)
;  u=u[w]

  ufilt=uvot[rem_dup(uvot.filter)].filter
  filter=['uu','bb','vv','uvw1','uvw2','uvm2','wh']
  zp=[17.89,19.11,18.34,17.49,16.82,17.35,20.29]
  urate2=dblarr(nu)
  umag=urate2 & umag2=umag

;  for i=0,n_elements(ufilt)-1 do begin
;     wi=where(uvot.filter eq ufilt[i])
;     wj=where(filter eq ufilt[i])
;     umag[wi]=zp[wj[0]]-2.5*alog10(uvot[wi].rate)
;     umag2[wi]=umag[wi]-corr[wj[0]]
;     urate2[wi]=10.^((umag2[wi]-zp[wj[0]])/(-2.5))
;  endfor 
  match_filters,uvot.filter,ind,filt1=filt1,/uvot

  for i=0,n_elements(ufilt)-1 do begin
     wi=where(uvot.filter eq ufilt[i])
     umag[wi]=zp[ind[wi]]-2.5*alog10(uvot[wi].rate)
  endfor 
  umag2=umag-corr[ind]
  colprint,filt1[ind],uvot.filter,corr[ind],umag,umag2
  urate2=10.^((umag2-zp[ind])/(-2.5))

  ufd=uvot2jy(urate2,uvot.filter,freq=freq)
  ufderr=uvot.raterr/uvot.rate*ufd
  w=where(uvot.raterr eq -1,nw)
  if nw gt 0 then ufderr[w]=-1.
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].time=uvot.time
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].terr[0]=uvot.time-uvot.tstart
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].terr[1]=uvot.tstop-uvot.time
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].fd=ufd
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].fderr=ufderr
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].filter=uvot.filter
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].telescope='UVOT'
  data[nr+nt+nm+nn:nr+nt+nm+nn+nu-1].freq=freq

  lc=lcout2fits('lc_newout_chandra.txt')
  wdet=where(lc.src_rate_err gt 0,ndet)
  lc=lc[wdet]
  time=lc.time
  tstarted=lc.tstart
  tstoped=lc.tstop
  cts=lc.src_rate
  err=lc.src_rate_err
  flux=1.;4.5d-11
  eeff=1. ;; 1 keV
  h=4.13566733d-15 ;; eV*s
  xfd=flux2jy(cts*flux,2.2,eeff=eeff)  
  xfderr=err/cts*xfd
  nx=ndet
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].time=time
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].terr[0]=time-tstarted
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].terr[1]=tstoped-time
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].fd=xfd
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].fderr=xfderr
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].filter='X-ray'
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].telescope='XRT'
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].color=!p.color
  data[nr+nt+nm+nn+nu:nr+nt+nm+nn+nu+nx-1].freq=eeff*1d3/h

  d=where(data.time ne 0)
  data=data[d]

  f=rem_dup(data.filter)
  filt=data[f].filter

  b=where(data.filter eq 'B')
  data[b].color=!blue
  i=where(data.filter eq 'I')
  data[i].color=!orange
  ic=where(data.filter eq 'Ic')
  data[ic].color=!yellow
  r=where(data.filter eq 'R')
  data[r].color=!red
  rc=where(data.filter eq 'Rc')
  data[rc].color=!firebrick
  v=where(data.filter eq 'V')
  data[v].color=!green
  z=where(data.filter eq 'Z')
  data[z].color=!navyblue
  b=where(data.filter eq 'b')
  data[b].color=!cyan
  u=where(data.filter eq 'u')
  data[u].color=!purple
  uvw1=where(data.filter eq 'uvw1')
  data[uvw1].color=!violet
  v=where(data.filter eq 'v')
  data[v].color=!salmon
  w=where(data.filter eq 'white')
  data[w].color=!grey50

  xrange=[10,1e7]
  yrange=[1e-9,1e-1]
  plot,xrange,yrange,psym=3,/nodata,/xlog,/ylog,ytitle='Flux Density (Jy)',yrange=yrange,ytickformat='loglabels',xrange=xrange,/xsty,xtitle='Time (s)'

  plotsym,1,2
  for i=0,n_elements(filt)-1 do begin 
     w=where(data.filter eq filt[i],nw); and data.fd-data.fderr gt 0,nw)
     w2=where(data[w].fderr gt 0,nw2)
     w3=where(data[w].fderr lt 0,nw3)
     if nw2 gt 0 then oploterror,data[w[w2]].time,data[w[w2]].fd,data[w[w2]].fderr,psym=3,errcolor=data[w[0]].color,/nohat
;     oploterror,data[w].time,data[w].fd,data[w].fderr,psym=3,errcolor=data[w[0]].color,/nohat
     if nw3 gt 0 then plots,data[w[w3]].time,data[w[w3]].fd,psym=8,color=data[w[0]].color
     for j=0,nw-1 do begin
        oplot,[data[w[j]].time-data[w[j]].terr[0],data[w[j]].time+data[w[j]].terr[1]],[data[w[j]].fd,data[w[j]].fd],color=data[w[j]].color
     endfor 
  endfor 

  legend,filt,textcolor=data[f].color,/top,/left,box=0

;  lim=strarr(n_elements(data))
;  w=where(data.fderr lt 0)
;  lim[w]='UL'
 
  writecol,'grb091020_lc_ujy.dat',data.freq,data.time/86400.,data.fd*1d6,data.fderr*1d6,header='# nu(Hz), dt(day), flux(microJy), dflux';,lim
  stop
  ;;; get final numbers from Sam
  ;;; add NOT (if ever get from Dong!)

  return 
end

pro joint_lcfits,ps=ps


;;   readcol,'vla.dat',grb,tel,year,mon,day,delt,freq,rflux,rfluxerr,format='(a,a,a,a,f,f,f,f,f)'
;; ;  rdate=21.900517
;;   rdate=date2met('2009-10-20-21:36:44.67')
;;   month=month_cnv(mon)
;;   dd=fix(day)
;;   hr=fix((day-dd)*24.)
;;   mn=fix(((day-dd)*24.-hr)*60.)
;;   sec=((((day-dd)*24.-hr)*60.)-mn)*60.
;;   rtime=dblarr(n_elements(day))
;;   for i=0,n_elements(day)-1 do rtime[i]=date2met(year[i]+'-'+ntostr(month[i])+'-'+ntostr(dd[i])+'-'+ntostr(hr[i])+':'+ntostr(mn[i])+':'+ntostr(sec[i]))-rdate
;;   rtime=delt*24.*60.*60.

;;   readcol,'tls.dat',dt,filt,tmag,terr,format='(f,a,f,f)'
;;   ttime=dt*24.*60.*60.
;;   tfd=opt2jy(tmag,filt)
;;   tfderr=opt2jy(tmag+terr,filt)-tfd
;;   readcol,'nickel.dat',ntstart,ntstop,nexp,nfilt,nmag,nerr,format='(f,f,f,a,f,f)'
;;   ntime=((ntstop-ntstart)/2.+ntstart)*86400.
;;   nterr=(ntstop-ntstart)/2.*86400.
;;   nfd=opt2jy(nmag,nfilt)
;;   nfderr=opt2jy(nmag+nerr,nfilt)-nfd

;;   read_uvot,uvot
;;   u=where(uvot.filter ne 'norm',nu)
;;   u=u[sort(uvot[u].time)]
;;   w=where(finite(uvot[u].rate),nu); and uvot[u].raterr/uvot[u].rate lt 0.1,nu)
;;   u=u[w]
;;   ufd=uvot2jy(uvot[u].rate,uvot[u].filter)
;;   ufderr=uvot[u].raterr/uvot[u].rate*ufd

;;   if keyword_set(ps) then begplot,name='joint_fits_091020.ps',/color,/land
;;   flux=1.;4.4d-11
;;   lc=lcout2fits(/phil)
;;   wdet=where(lc.src_rate_err gt 0,ndet)
;;   lc=lc[wdet]
;;   time=lc.time
;;   tstarted=lc.tstart
;;   tstoped=lc.tstop
;;   cts=lc.src_rate*flux
;;   err=lc.src_rate_err*flux
;;   type=lc.type
;;   bg=lc.tot_back_cts
;;   src=lc.src_counts
;;   sig=lc.det_sig
;;   expt=lc.exptime
;;   corr_fact=lc.pu_corr
;;   timerr=fltarr(2,n_elements(time))
;;   timerr[0,*]=time-tstarted
;;   timerr[1,*]=tstoped-time
;;   flux=4.4d-11
;;   xfd=flux2jy(cts*flux,2.2)
;;   xfderr=err/cts*xfd

;;;NEED TO CONVERT OPTICAL DATA FROM MAGNITUDES TO FLUX DENSITY (JY)

;  wdet=where(err gt 0,ndet)

  erase
  multiplot2,[1,2],/init
  multiplot2,yupgap=0
;  ploterror,time,cts,err,psym=3,/nohat,/xlog,/ylog,ytitle='Count rate',yrange=[1e-4,1e3],ytickformat='loglabels',xrange=[10,1e7],/xsty
;  for i=0,ndet-1 do begin
;     oplot,[tstarted[i],tstoped[i]],[cts[i],cts[i]]
;  endfor 
  ploterror,time,xfd,xfderr,psym=3,/nohat,/xlog,/ylog,ytitle='Flux Density (Jy)',yrange=[1e-9,0.1],ytickformat='loglabels',xrange=[10,1e7],/xsty
  for i=0,ndet-1 do begin
     oplot,[tstarted[i],tstoped[i]],[xfd[i],xfd[i]]
  endfor 

;  oploterror,uvot[u].time,uvot[u].rate,uvot[u].raterr,psym=3,errcolor=!orange,/nohat
;  for i=0,nu-1 do
;  oplot,[uvot[u[i]].tstart,uvot[u[i]].tstop],[uvot[u[i]].rate,uvot[u[i]].rate],color=!orange

  fcolors=[!grey50,!blue,!yellow,!orange,!purple]
  ufilt=uvot[uniq(uvot.filter)].filter
  uu=where(ufilt ne 'norm')
  ufilt=ufilt[uu]
  for j=0,n_elements(ufilt)-1 do begin 
     w=where(uvot[u].filter eq ufilt[j],nw)
     oploterror,uvot[u[w]].time,ufd[w],ufderr[w],psym=3,errcolor=fcolors[j],/nohat
     for i=0,nw-1 do oplot,[uvot[u[w[i]]].tstart,uvot[u[w[i]]].tstop],[ufd[w[i]],ufd[w[i]]],color=fcolors[j]
  endfor 

  plotsym,0,0.5,/fill
  
;  oploterror,ttime,tmag,terr,psym=3,color=!cyan,errcolor=!cyan,/nohat
;  oploterror,ntime,nmag,nterr,nerr,psym=3,color=!green,errcolor=!green,/nohat
  oploterror,ttime,tfd,tfderr,psym=3,color=!cyan,errcolor=!cyan,/nohat
  oploterror,ntime,nfd,nterr,nfderr,psym=3,color=!green,errcolor=!green,/nohat

  oploterror,rtime,rflux*1d-6,rfluxerr*1d-6,psym=8,errcolor=!magenta,/nohat,color=!magenta
stop
  ;;; NEED POW+RISING_BKN2POWx2 but with all pars tied except norm
  mo='pow_2bkn2pow'
  model='int_pow_2bkn2pow'
  slope=[3.,-0.5,0.9,1.4]
  bt=[200.,7000.]
  norm1=5e7
  norm2=5e-2
  norm3=50.
  p=[norm1,slope[0],norm2,slope[1],bt[0],slope[2],bt[1],slope[3],$
     norm3,slope[1],bt[0],slope[2],bt[1],slope[3]]*1d
  pnames=['norm1','pow1','norm2','pow2','break2','pow3','break3','pow4','onorm2','opow2','obreak2','opow3','obreak3','opow4']

  np=n_elements(p)
  parinfo = parinfo_struct(np)

  parinfo.parname=pnames
  parinfo.value=p
  parinfo[0].limits=[1e-8,0] ;;norm > 0
  parinfo[0].limited=[1,0]
  parinfo[2].limits=[1e-8,0] ;;norm > 0
  parinfo[2].limited=[1,0]
  parinfo[8].limits=[1,0] ;;norm > 0
  parinfo[8].limited=[1,0]

  mint=min(time)*1.1
  maxt=max(time)*0.9
;;pow 1 limits > 0
  parinfo[1].limits=[-2,0]
  parinfo[1].limited=[1,0]
  pmargin=4.
  ;;break limits
  parinfo[4].limits=[mint,p[6]]
  parinfo[6].limits=[p[4],maxt]
  parinfo[[4,6]].limited=[1,1]
  parinfo[[4,6]].mpminstep=1.
  ;;pow limits
  parinfo[1].limits=[p[1]-pmargin,p[1]+pmargin]
  parinfo[3].limits=[p[3]-pmargin,p[3]+pmargin]
  parinfo[5].limits=[p[5]-pmargin,p[5]+pmargin]
  parinfo[7].limits=[p[7]-pmargin,p[7]+pmargin]
  parinfo[[1,3,5,7]].limited=[1,1]
  ;; optical bkn2pow
  parinfo[9].tied='p[3]'
  parinfo[10].tied='p[4]'
  parinfo[11].tied='p[5]'
  parinfo[12].tied='p[6]'
  parinfo[13].tied='p[7]'  

  tt=dblarr(2,n_elements(time)+nu)
  tt[0,*]=[time-timerr[0,*],uvot[u].tstart]
  tt[1,*]=[time+timerr[1,*],uvot[u].tstop]
  cts=[cts,uvot[u].rate]
  err=[err,uvot[u].raterr]

  newp=mpfitfun(mo,[time,uvot[u].time],cts,err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                yfit=yfit,status=status,nprint=10,$
                ftol=1e-15,xtol=1e-15,gtol=1e-25);,/quiet)
  chisq=total(((yfit-cts)/err)^2)
  
  print,chisq,dof,chisq/dof
  time=[time,uvot[u].time]
  tmp=execute('yfit2='+mo+'(time,newp,y1,y2,y3)')
  wx=indgen(ndet)
  wu=ndet+indgen(nu)
;  wx=indgen(n_elements(time)) & wu=wx
  oplot,time[wx],yfit2[wx],color=!red
  oplot,time[wu],yfit2[wu],color=!red
  oplot,time[wx],y1[wx],line=1,color=!red
  oplot,time[wx],y2[wx],line=2
;  oplot,uvot[u].time,y3[wu],line=3,color=!green

  colprint,indgen(n_elements(p)),pnames,newp,parinfo.tied
  day7=7.*86400.
  oplot,[day7,day7],[1e-4,1e4],line=1,color=!blue
  legend,['X-ray','Normalized UVOT','Radio'],box=0,/top,/left,textcolor=[!p.color,!orange,!magenta],charsize=1.

  multiplot2,yupgap=0.08

  res=cts/yfit
  reserr=err/yfit
  nlc=n_elements(lc)
  ploterror,time[0:nlc-1],res[0:nlc-1]+1,reserr[0:nlc-1],psym=3,/xlog,xrange=[10,1e7],/xsty,/nohat,xtitle='Time (s)',yrange=[0,3],ytitle='Residuals'
  tstart=lc.tstart
  tstop=lc.tstop

  for i=0,nlc-1 do oplot,[tstart[i],tstop[i]],[res[i],res[i]]+1
  tstart=uvot[u].tstart
  tstop=uvot[u].tstop
  oploterror,uvot[u].time,res[nlc:*],reserr[nlc:*],errcolor=!orange,psym=3,/nohat
  for i=0,nu-1 do oplot,[tstart[i],tstop[i]],[res[nlc+i],res[nlc+i]],color=!orange


  oplot,[10,1e6],[2,2],line=2
  oplot,[10,1e6],[1,1],line=2,color=!orange

  oplot,[day7,day7],[1e-4,1e4],line=1,color=!blue
  multiplot2,/reset,/default

  if keyword_set(ps) then endplot
stop

  return
end 

pro read_uvot,uvot,noplot=noplot

  file='~/Desktop/GRB091020/TOnormalize_binned_v2.txt'
;  readcol,file,time0,terr0,rate0,err0,format='(d,d,d,d)'
;  file2='~/Desktop/GRB091020/CATOnormalize.txt'
  readcol,file,time2,terr2,rate2,err2,format='(d,d,d,d)'
;  file3='~/Desktop/GRB091020/TOnormalize.txt'
;  readcol,file3,time3,terr3,rate3,err3,tmp,flux3,fluxerr3,filter3,files3,format='(d,d,d,d,a,d,d,a,a)'
  
  time=time2 & terr=terr2 & rate=rate2 & err=err2
;  time=[time0,time2];,time3]
;  terr=[terr0,terr2];,terr3]
;  rate=[rate0,rate2];,rate3]
;  err=[err0,err2];,err3]
;  n1=n_elements(time0)
  n2=n_elements(time2)
;  n3=n_elements(time3)
  n=n_elements(time)
  w=where(time[1:*]-time[0:n2-1] lt 0)
;  w=where(time[1:*]-time[0:n1+n2-2] lt 0)
;  w2=where(time3[1:*]-time3[0:n3-2] lt 0)
;  w=[0,w,n1+n2-1,w2,n3+n2+n1]
;  w=[0,w,n1+n2-1]
  w=[-1,w,n2-1]
  nw=n_elements(w)
  help,w
  
  filter=['white','v','b','u','uvw1'] ;,'norm'];,'white','v','b','u','uvw1','uvw2']
  nf=n_elements(filter)
  color=[!p.color,!red,!blue,!purple,!cyan,!orange] ;,!p.color,!red,!blue,!purple,!cyan,!orange]
  uvot=create_struct('time',0d,'tstart',0d,'tstop',0d,$
                     'rate',0d,'raterr',0d,'filter','')
  uvot=replicate(uvot,n)

  uvot.time=time
  uvot.tstart=time-terr
  uvot.tstop=time+terr
  uvot.rate=rate
  uvot.raterr=err
  for i=0,nw-2 do begin
     uvot[w[i]+1:w[i+1]].filter=filter[i]
;     print,w[i]+1,w[i+1]
  endfor

  if not keyword_set(noplot) then begin 
     plot,uvot.time,uvot.rate,psym=3,/xlog,/ylog,/nodata,xtitle='Time since trigger (s)',ytitle='UVOT count rate',yrange=[1e-2,1e2]
     for i=0,nf-1 do begin
        w=where(strtrim(uvot.filter,2) eq filter[i] and uvot.rate gt 0,nw)
        oploterror,uvot[w].time,uvot[w].rate,uvot[w].raterr,/nohat,errcolor=color[i],psym=3
        for j=0,nw-1 do oplot,[uvot[w[j]].tstart,uvot[w[j]].tstop],[uvot[w[j]].rate,uvot[w[j]].rate],color=color[i]
     endfor 
  endif 
;  oploterror,time,rate,terr,err,psym=3,color=!orange,errcolor=!orange,/nohat

  return
end 

pro crs_grb091020
  
  ;;;;;;;; 90% confidence errors (use 95%?)
  alpha=[3.52,0.89,1.37]
  alphaerr0=[0.08,0.07,0.04]
  alphaerr1=[0.08,0.03,0.04]
  gamma=[2.34,2.15,2.22]
  gammaerr0=[0.14,0.13,0.07]
  gammaerr1=[0.14,0.12,0.08]

  title='GRB 091020'
  ps=1
  fit_crs,alpha,alphaerr0,alphaerr1,gamma,gammaerr0,gammaerr1,plotevery=plotevery,ps=ps,name=name,plotlc=plotlc,nocolor=nocolor,skipfirst=skipfirst,noconsis=noconsis,twocomp=twocomp,latebr=latebr,title=title,lc=lc,crstr=crstr,/plotcompat,plotcomm=plotcomm

  return
end 

pro fit_grb091020,powbknpow=powbknpow,gausspow=gausspow,ps=ps,noerr=noerr
  
  ;;; create model of guassian + (b)-pows
  ;;; rising pow+pow

  flux=1.
  lc=lcout2fits(/phil)
  wdet=where(lc.src_rate_err gt 0,ndet)
  lc=lc[wdet]

  time=lc.time
  tstarted=lc.tstart
  tstoped=lc.tstop
  cts=lc.src_rate*flux
  err=lc.src_rate_err*flux
  type=lc.type
  bg=lc.tot_back_cts
  src=lc.src_counts
  sig=lc.det_sig
  expt=lc.exptime
  corr_fact=lc.pu_corr
  timerr=fltarr(2,n_elements(time))
  timerr[0,*]=time-tstarted
  timerr[1,*]=tstoped-time

  ;;; Power-law+ rising double broken power-law

  xrange=[10,1e7]
  yrange=[1e-4,1e3]
  multiplot2,/default,/reset
  charsize=1.
  if keyword_set(ps) then begplot,name='XRT_lc_fits.ps',/color else erase

;  if not keyword_set(gausspow) then begin 
;     !p.multi=[0,1,2]

     multiplot2,[1,4],/init
     multiplot2,yupgap=0
     xtitle=''
     plot_like_qdp,_extra=_extra,lc=lc,title=name,arrowsize=arrowsize,xrange=xrange,xtitle=xtitle,noxaxis=noxaxis,qdp=qdp,file=file,flux=flux,yrange=yrange,nocolor=nocolor,ytickformat='loglabels',charsize=charsize

     mo='pow_bkn2pow'
     model='int_pow_bkn2pow'
     slope=[3.,-0.5,1.2,1.8]
     bt=[300.,7000.]
     norm1=5e7
     norm2=5e-1
     p=[norm1,slope[0],norm2,slope[1],bt[0],slope[2],bt[1],slope[3]]*1d
     pnames=['norm1','pow1','norm2','pow2','break2','pow3','break3','pow4']

     np=n_elements(p)
     parinfo = parinfo_struct(np)

     parinfo.parname=pnames
     parinfo.value=p
     parinfo[0].limits=[0,0] ;;norm > 0
     parinfo[0].limited=[1,0]
     parinfo[2].limits=[0,0] ;;norm > 0
     parinfo[2].limited=[1,0]
     mint=min(time)*1.1
     maxt=max(time)*0.9
;;pow 1 limits > 0
     parinfo[1].limits=[-2,0]
     parinfo[1].limited=[1,0]
     pmargin=2.
     ;;break limits
     parinfo[4].limits=[mint,p[6]]
     parinfo[6].limits=[p[4],maxt]
     parinfo[[4,6]].limited=[1,1]
     parinfo[[4,6]].mpminstep=1.
     ;;pow limits
     parinfo[1].limits=[p[1]-pmargin,p[1]+pmargin]
     parinfo[3].limits=[p[3]-pmargin,p[3]+pmargin]
     parinfo[5].limits=[p[5]-pmargin,p[5]+pmargin]
     parinfo[7].limits=[p[7]-pmargin,p[7]+pmargin]
     parinfo[[1,3,5,7]].limited=[1,1]

     tt=dblarr(2,n_elements(time))
     tt[0,*]=time-timerr[0,*]
     tt[1,*]=time+timerr[1,*]

     newp=mpfitfun(model,tt,cts,err,p,parinfo=parinfo,$
                   bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                   perror=perror,yfit=yfit,status=status,nprint=10,$
                   ftol=1e-15,xtol=1e-15,gtol=1e-25,/quiet)
     chisq=total(((yfit-cts)/err)^2)

     tmp=execute('yfit2='+mo+'(time,newp,y1,y2)')
     oplot,time,yfit2
     oplot,time,y1,line=1
     oplot,time,y2,line=2

     if not keyword_set(noerr) then lc_monte,time,tt,expt,cts,err,corr_fact,src,bg,newp,mo,pnames,outperr,/noplot,nsim=nsim,nsig=nsig,parinfo=parinfo else outperr=fltarr(2,n_elements(newp))
     print
     colprint,pnames,newp,outperr[0,*],outperr[1,*]
     print,chisq,dof
     print,'chisq/dof='+ntostr(chisq/dof)
     legend,['PL + Rising Double Broken PL',!tsym.chi+'!U2!N/dof='+ntostr(round(chisq))+'/'+ntostr(dof)+'='+numdec(chisq/dof,2)],/top,/right,box=0,charsize=charsize

     res=lc.src_rate/yfit2*flux
     reserr=lc.src_rate_err/yfit2*flux
     w=where(res+reserr lt 3,nw)
     if nw eq 0 then w=indgen(n_elements(res))
;  res=lc.src_rate-yfit
;  w=where(abs(lc.src_rate/yfit) gt 0.1 and abs(lc.src_rate/yfit) lt 10)
     ryrange=round(prange(res[w],reserr[w]))
     multiplot2,yupgap=0.1
     plot,lc.time,res,/xlog,psym=3,yrange=ryrange,xtitle='Time since BAT trigger (s)',xrange=xrange,ytitle='Residuals',charsize=charsize,xtickformat='loglabels'
     n=ndet
     for i=0,n-1 do begin
        oplot,[lc[i].tstart,lc[i].tstop],[res[i],res[i]]
        oplot,[lc[i].time,lc[i].time],[res[i]-reserr[i],res[i]+reserr[i]]
     endfor 

;     xrange=minmax(lc.time)
     oplot,[xrange[0]*0.1,xrange[1]*10],[1,1]
;     multiplot2,/reset,/init

;  endif
;stop
;  if not keyword_set(powbknpow) then begin 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; gaussian + bkn2pow

;     multiplot2,[1,2],/init
     mo='gauss_bkn2pow'
     model='int_gauss_bkn2pow'
     slope=[3.,1.0,1.5d]
     bt=[100.,7d3]
     g=[5.,190.,50d]
     norm2=5e7
     p=[g[0],g[1],g[2],norm2,slope[0],bt[0],slope[1],bt[1],slope[2]]
     pnames=['height','center','sdev','norm2','pow1','break1','pow2','break2','pow3']

     np=n_elements(p)
     parinfo = parinfo_struct(np)

     parinfo.parname=pnames
     parinfo.value=p
     parinfo[0].limits=[1,0] ;;norm > 0
     parinfo[0].limited=[1,0]
     parinfo[3].limits=[1,0] ;;norm > 0
     parinfo[3].limited=[1,0]
     mint=min(time)*1.1
     maxt=max(time)*0.9
;;pow 1 limits > 0
     parinfo[1].limits=[p[1]-200,p[1]+200]
     parinfo[1].limited=[1,1]
     parinfo[2].limits=[10,400]
     parinfo[2].limited=[1,1]
     pmargin=2.
     ;;break limits
     parinfo[5].limits=[mint,p[7]]
     parinfo[7].limits=[p[5],maxt]
     parinfo[[5,7]].limited=[1,1]
     parinfo[[5,7]].mpminstep=1.
     ;;pow limits
     parinfo[4].limits=[p[4]-pmargin,p[4]+pmargin]
     parinfo[6].limits=[0.5,p[6]+pmargin]
     parinfo[8].limits=[p[8]-pmargin,p[8]+pmargin]
     parinfo[[4,6,8]].limited=[1,1]

;  tt=dblarr(2,n_elements(time))
;  tt[0,*]=time-timerr[0,*]
;  tt[1,*]=time+timerr[1,*]

     newp=mpfitfun(mo,time,cts,err,p,parinfo=parinfo,$
                   bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                   perror=perror,yfit=yfit,status=status,nprint=10,$
                   ftol=1e-15,xtol=1e-15,gtol=1e-25,/quiet)
     chisq=total(((yfit-cts)/err)^2)
     multiplot2,yupgap=0
     plot_like_qdp,_extra=_extra,lc=lc,title=name,arrowsize=arrowsize,xrange=xrange,xtitle=xtitle,noxaxis=noxaxis,qdp=qdp,file=file,flux=flux,nocolor=nocolor,ytickformat='loglabels',charsize=charsize,yrange=yrange

     tmp=execute('yfit2='+mo+'(time,newp,y1,y2)')
     oplot,time,yfit2           ;,color=!green
     oplot,time,y1,line=1       ;,color=!green
     oplot,time,y2,line=2       ;,color=!green
     if not keyword_set(noerr) then lc_monte,time,tt,expt,cts,err,corr_fact,src,bg,newp,mo,pnames,outperr,/noplot,nsim=nsim,nsig=nsig,parinfo=parinfo,/noint else outperr=fltarr(2,n_elements(newp))
     print
     colprint,pnames,newp,outperr[0,*],outperr[1,*]
     print,chisq,dof
     print,'chisq/dof='+ntostr(chisq/dof)
     legend,['Gaussian + Double Broken PL',!tsym.chi+'!U2!N/dof='+ntostr(round(chisq))+'/'+ntostr(dof)+'='+numdec(chisq/dof,2)],/top,/right,box=0,charsize=charsize

     res=lc.src_rate/yfit*flux
     reserr=lc.src_rate_err/yfit*flux
     w=where(res+reserr lt 3,nw)
     if nw eq 0 then w=indgen(n_elements(res))
;  res=lc.src_rate-yfit
;  w=where(abs(lc.src_rate/yfit) gt 0.1 and abs(lc.src_rate/yfit) lt 10)
     yrange=round(prange(res[w],reserr[w]))
     multiplot2,yupgap=0.1
     plot,lc.time,res,/xlog,psym=3,yrange=yrange,xtitle='Time since BAT trigger (s)',xrange=xrange,ytitle='Residuals',charsize=charsize,xtickformat='loglabels'
     n=ndet
     for i=0,n-1 do begin
        oplot,[lc[i].tstart,lc[i].tstop],[res[i],res[i]]
        oplot,[lc[i].time,lc[i].time],[res[i]-reserr[i],res[i]+reserr[i]]
     endfor 

;     xrange=minmax(lc.time)
     oplot,[xrange[0]*0.1,xrange[1]*10],[1,1]
     multiplot2,/reset,/init

;     !p.multi=0
;  endif 

  if keyword_set(ps) then endplot

  stop
  return
end 
