@fit_functions
pro chandra_plots,ps=ps

  grb='GRB090423'
  cd,'~/Chandra/chandra/'+grb

  if keyword_set(ps) then begplot,name='GRB090423_paper_lc.eps',/color,/encap,/land,font='helvetica' else erase
  file='lc_newout_chandra.txt'
  lc=lcout2fits(file)
;  lc=lcout2fits(/phil)
;  flux=4.1e-11
;  lc.src_rate=lc.src_rate*flux
;  lc.src_rate_err=lc.src_rate_err*flux
;  lc[n_elements(lc)-2:*].type=2

  !x.margin=[15,0]
  multiplot2,[1,2],/init
  multiplot2,yupgap=0
  wdet=where(lc.src_rate_err gt 0)
  lc=lc[wdet]
  x=where(lc.type eq 0 or lc.type eq 1)
  xlc=lc[x]
  c=where(lc.type eq 2)
  clc=lc[c]
  xcolor=!p.color;!grey20
  ccolor=!p.color
  xsym=3
  xrange=[10,1e7]
  yrange=[1e-17,1e-9]
  charsize=1.5
  plot,xrange,yrange,/nodata,/xlog,/ylog,ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',charsize=charsize,/ysty,/xsty

  read_lcfit,'lc_fit_out_idl_int8.dat',pname,p,perror
  newp0=p-perror[0,*]
  newp1=p+perror[1,*]
  j=[2,4,6]
  breaks=[p[j],newp0[j],newp1[j]]
  lctime=lc.time
  lctime2=[lctime,breaks]
  lctime2=lctime2[sort(lctime2)]
  polyx=[lctime2,reverse(lctime2)]
  polyy=[bkn3pow(lctime2,newp0),reverse(bkn3pow(lctime2,newp1))]
  polyfill,polyx,polyy,color=!grey80
;  oplot,lctime,bkn3pow(lctime,newp0),color=!orange
;  oplot,lctime,bkn3pow(lctime,newp1),color=!grey50


  oploterror,xlc.time,xlc.src_rate,xlc.src_rate_err,/nohat,psym=xsym,color=xcolor,errcolor=xcolor
  for i=0,n_elements(xlc)-1 do $
     oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].src_rate,xlc[i].src_rate],color=xcolor

  plotsym,0,1,/fill
  oploterror,clc.time,clc.src_rate,clc.src_rate_err,/nohat,psym=8,color=ccolor
  for i=0,n_elements(clc)-1 do $
     oplot,[clc[i].tstart,clc[i].tstop],[clc[i].src_rate,clc[i].src_rate],color=ccolor

;  read_lcfit,'lc_fit_comb_wjb.dat',pnames,p
  breaks=p[[2,4,6]]
  for i=0,2 do oplot,[breaks[i],breaks[i]],yrange,color=!grey50,line=1
  yfit=bkn3pow(lc.time,p)
  time=[lc.time,breaks]
  time=time[sort(time)]
  yfits=bkn3pow(time,p)
  oplot,time,yfits

;  read_lcfit,'lc_fit_comb_nojb.dat',pnames,p2
;  altyfit=bkn2pow(time,p2)
;  oplot,time,altyfit,line=1
  t=lc.time
  tw=where(t ge 3900)
  t=t[tw]
  t=[3900,t]
  oplot,t,bkn2pow(t,[p[0]*1.5,p[1:3],3900.,1.35]),line=3

  ;; add maybe break
  jb=mrdfits(!adata+'chandra/maybe_jetbreak.fits',1)
  jb=jb[7]
  print,jb.grb

  if jb.tlastpos ne 0 then begin
     np=n_elements(p)
     lctime=[lc.time,jb.tlastpos,1e8]
     lctime=lctime[sort(lctime)]
     pp=[p,jb.tlastpos,p[np-1]+1.]
     case np of 
        2: myfit=bknpow(lctime,pp)
        4: myfit=bkn2pow(lctime,pp)
        6: myfit=bkn3pow(lctime,pp)
        8: myfit=bkn4pow(lctime,pp)
        else:
     endcase 
     wc=where(lctime ge jb.tlastpos)
     opx=lctime[wc]
     opy=myfit[wc]
     oplot,opx,opy,line=2
     
;     oplot,lctime,bkn3pow(lctime,newp0);,color=!orange
;     oplot,lctime,bkn3pow(lctime,newp1);,color=!grey50
;     for j=0,n_elements(breaks)-1 do oplot,[breaks[j],breaks[j]],[1e-18,1d-8],line=3,color=colors[j]
  endif 

  multiplot2,yupgap=0.2
  res=lc.src_rate/yfit
  reserr=lc.src_rate_err/yfit
  readcol,'flares_gtis.dat',tstart,tstop
  w=where(lc.time le tstart[0] or lc.time ge tstop[0],nw)
;  w=where(res+reserr lt 3,nw)
  yrange=round(prange(res[w],reserr[w]))
  plot,lc[w].time,res[w],/xlog,psym=xsym,yrange=[0,yrange[1]],xtitle='Time since BAT trigger (s)',xrange=xrange,ytitle='Residuals',charsize=charsize,/ysty,/xsty
  oplot,clc.time,res[c],psym=8
;  n=nw+n_elements(c)
;  w=[w,c]
  for i=0,nw-1 do begin
     oplot,[lc[w[i]].tstart,lc[w[i]].tstop],[res[w[i]],res[w[i]]]
     oplot,[lc[w[i]].time,lc[w[i]].time],[res[w[i]]-reserr[w[i]],res[w[i]]+reserr[w[i]]]
  endfor 

;  xrange=minmax(lc.time)
  oplot,[xrange[0]*0.1,xrange[1]*10],[1,1]

  multiplot2,/reset,/default
  if keyword_set(ps) then endplot
stop
return
end 
