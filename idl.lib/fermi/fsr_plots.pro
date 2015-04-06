@fit_functions
@calc_eiso
pro dm_sensitivity
  mass =[5.,10.,15.,20.,50.,100.,150.,300.,600.,1000.]
                     
  ;;2 Year Limits
  lim2yr = [  1.02000000d-26,   1.58212587d-26,   2.15622780d-26,$
              2.60484428d-26,   4.12000027d-26,   6.05205423d-26,$
              8.10358705d-26,   1.52359891d-25,   3.35514282d-25,$
              6.56918953d-25]
                     
  ;;6 Year Limits
  lim6yr =[  2.96518546e-27,   4.57132669e-27,   6.19078241e-27,$
             7.42985525e-27,   1.12392387e-26,   1.50128757e-26,$
             1.78411716e-26,   2.27481358e-26,   3.80233262e-26,$
             7.30178911e-26]
                     
  ;;10 Year Limits
  lim10yr = [ 2.22490506e-27,   3.42519801e-27,   4.63179734e-27,$
              5.55036108e-27,   8.30954192e-27,   1.08569379e-26,$
              1.25493298e-26,   1.46193890e-26,   2.29190422e-26,$
              4.38144873e-26]
;  lim10yr = [4.12653601e-27,   6.37213649e-27,   8.64418593e-27,$
;             1.03925457e-26,   1.59095266e-26,   2.17985103e-26,$
;             2.67453015e-26,   3.78348173e-26,   6.80811555e-26,$
;             1.31419251e-25]

  begplot,name='~/Fermi/Senior_Review/dm_sensitivity.eps',/land,/color,font='helvetica'
  !x.margin=[4,0]
  !y.margin=[2,0]
  
  langle= '!9' + String("341B) + '!X'
  rangle= '!9' + String("361B) + '!X'
  plot,[5,1000],[1d-27,1d-23],/nodata,yrange=[1d-27,1d-24],xrange=[5,1000],/xsty,/ysty,/xlog,/ylog,xtitle='Mass (GeV)',charsize=2,ytitle=langle+!tsym.sigma+'v'+rangle+' (cm!U3!N s!U-1!N)'
  plotsym,0,2,/fill
  oplot,mass,lim2yr,psym=8,thick=10,color=!red
  oplot,mass,lim2yr,color=!red,thick=10
;  oplot,mass,lim6yr,color=!blue,psym=8
;  oplot,mass,lim6yr,color=!blue,line=1,thick=15
  oplot,mass,lim10yr,color=!blue,psym=8
  oplot,mass,lim10yr,line=1,color=!blue,thick=15
  oplot,[5,1000],[3d-26,3d-26],line=2
  s='       '
;  legend,s+['2 yr bb limit (Abdo et al. 2011, 10 dSph)','6 yr
;  prediction (30 dSph)','10 yr prediction (30 dSph)','theoretical
;  limit'],box=0,/top,/left,charsize=2;,line=[0,1,1],color=[!red,!blue,!green],thick=[10,15,15]
  legend,s+['2 yr bb limit (Ackermann et al. 2011, 10 dSph)','10 yr prediction (30 dSph)','thermal cross section'],box=0,/top,/left,charsize=2
  oplot,[6,9],[5.5e-25,5.5e-25],color=!red,thick=10,line=0
  oplot,[6,9],[4e-25,4e-25],color=!blue,thick=15,line=1
;  oplot,[6,9],[2.7e-25,2.7e-25],color=!green,thick=15,line=1
  oplot,[6,9],[2.7e-25,2.7e-25],thick=10,line=2
;  oplot,[6,9],[2.e-25,2.e-25],thick=10,line=2


  oplot,[16.8,18.3],[6.7e-25,6.7e-25],thick=10

;  arrow,[6,6],[1.1e-26,2.5e-27],[6,6],[2.5e-27,1.1e-26],/data,/solid,thick=2
;  arrow,[800,800],[4.7e-25,3.5e-26],[800,800],[3.5e-26,4.7e-25],/data,/solid,thick=2
  arrow,6,1.1e-26,6,2.5e-27,/data,/solid,thick=8,hthick=3
  arrow,800,4.7e-25,800,3.5e-26,/data,/solid,thick=8,hthick=3

;  xyouts,20,2e-27,'LAT sensitivity improves faster at higher energies',orient=10

  endplot
  spawn,'convert ~/Fermi/Senior_Review/dm_sensitivity.eps ~/Fermi/Senior_Review/dm_sensitivity.pdf'

return
end 

pro fermi_grbs

  cd,'~/Fermi/Senior_Review'
  goto,skip
  cd,'111026-FULL-GRB-Extended'
  files=file_search('like*txt')
  grbs=strmid(files,5,9)
  n=n_elements(files)
  mev2erg=1.602e-12*1d6
  begplot,name='~/Fermi/Senior_Review/GRB_fig.eps',/land,/encap,/color,font='helvetica'
  potential=!violet
  multiplot,[1,2],/init
  plotsym,1,1,thick=2
  multiplot
  plot,[0.1,1e7],[1e-10,1e-5],/xlog,/ylog,/nodata,ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,xminor=9,xticks=8,xrange=[0.1,1e7];,yminor=10
  polyfill,[1e3,8*3600.,8*3600.,1e3,1e3],[1.e-10,1.e-10,1e-5,1e-5,1.1e-10],color=potential
  oplot,[0.1,1e7],[1e-10,1e-10]
  oplot,[0.1,1e7],[1e-5,1e-5]

;  axis,0.1,1e7,xaxis=1,/xsty,xticks=8,xminor=10,xrange=[0.1,1e7],xtickv=[10^(findgen(9)-1)],xticklen=0.1
;  axis,xaxis=1
;  axis,xaxis=0
;  axis,1e-2,1e7,xaxis=0,xtickname=[' ',' ',' ',' ',' ',' '],xticks=6,/xsty,xminor=1
;  axis,2008.5,10000,xaxis=1,xrange=[2008.5,2018.5],xstyle=1,xticks=6,xtickv=[findgen(6)*2+2008.5],xminor=3,xtickname=ntostr([0,2,4,6,8,10]),xtitle='Year of Mission'

  simul=intarr(n)
  simul[[11,24,32,33]]=1
  fup=intarr(n)
  fup[[1,9,10,14,15,16,21]]=1
  for i=0,n-1 do begin
;     print,grbs[i]
     readcol,files[i],fitnum,tstart,tend,tmed,Emin,Emax,Ts,NobsTot,Npred_100,Npred_1000,index,errorI,Flux,ErrorF,EnergyFlux,ErrorEF,Fluence,format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)',/silent

     w=where(erroref ne 0,nw)
     ul=where(erroref eq 0,nul)
     color=!p.color
     if simul[i] eq 1 then color=!cyan
     if fup[i] eq 1 then color=!blue
     if nw gt 0 then begin 
;        colprint,tstart[w],tmed[w],tend[w],tend[w]-tmed[w],energyflux[w]*mev2erg,erroref[w]*mev2erg
        oploterror,tmed[w],energyflux[w]*mev2erg,erroref[w]*mev2erg,/nohat,psym=3,errcolor=color
;        print,nw
        for j=0,nw-1 do oplot,[tstart[w[j]],tend[w[j]]],[energyflux[w[j]],energyflux[w[j]]]*mev2erg,color=color
     endif 
;     if nul gt 0 then oplot,tmed[ul],energyflux[ul]*mev2erg,psym=8
;     for j=0,n_elements(tstart)-1 do oplot,[tstart[j],tend[j]],[energyflux[j],energyflux[j]]*mev2erg

     ;;;upper limits
     
  endfor 
  legend,['      LAT','0.1 - 100 GeV'],/bottom,/left,box=0
  xyouts,150,1e-6,'Future Follow-up',/data
  xyouts,400,2e-7,' Potential',/data


  legend,['Joint Fermi/Swift','Swift Follow-up','Other'],/top,/right,box=0,textcolor=[!cyan,!blue,!p.color]

  multiplot
  ;; XRT LCs
  cd,'~/GRBs'
  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB100728A','GRB110625A','GRB110731A'] 
  simul=[0,0,0,1,0,0,0,0,1,1,1]
  nl=n_elements(latgrbs)
  plot,[0.1,1e7],[1e-14,1e-7],/xlog,/ylog,/nodata,xtitle='Time (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,xminor=9,xticks=8,xrange=[0.1,1e7],xtickformat='loglabels',yticks=7,/ysty,yrange=[1e-14,1e-7],yminor=9
  polyfill,[1e3,8*3600.,8*3600.,1e3,1e3],[1.e-14,1.e-14,1e-6,1e-6,1.e-14],transparent=1,color=potential
;  axis,0.1,1e7,xaxis=0,/xsty,xticks=9,xminor=10,xrange=[0.1,1e7]
;  axis,xaxis=0
  oplot,[0.1,1e7],[1e-14,1e-14]
  oplot,[0.1,1e7],[1e-7,1e-7]

  for i=0,nl-1 do begin
     cd,latgrbs[i]
     lc=lcout2fits(/phil,/silent)
     spec=mrdfits('UL_specfits.fits',1,/silent)
     ns=n_elements(spec)
     fluxfact=spec[ns-1].unabs_cfratio ;;; GET's LEICESTER PC FIT, IF NOT PC THEN WT
     if simul[i] eq 1 then color=!cyan else color=!blue
     oploterror,lc.time,lc.src_rate*fluxfact,lc.src_rate_err*fluxfact,/nohat,psym=3,errcolor=color
     nlc=n_elements(lc)
     for j=0,nlc-1 do oplot,[lc[j].tstart,lc[j].tstop],[lc[j].src_rate,lc[j].src_rate]*fluxfact,color=color


     cd,'..'
  endfor 
  legend,[' Swift-XRT','0.3 - 10 keV'],/bottom,/left,box=0
  multiplot,/reset,/default
  
  ;;; indicate seperately the joint triggers, and which bursts are common
  endplot
skip:
  ;;; NEW idea - schematic of GRB SED demonstrating cutoffs and
  ;;;            wonders of LLE

  ;;; 8 kev - 100 GeV
  f=findgen(100)/10.+1
  eng=[f/100.,f/10.,f[0]+f[0:8]/10.,f[1:*],f[1:*]*10,f[1:*]*100,f[1:*]*1e3] ;; MeV

  alpha=-0.5
  epeak=0.7 ;; MeV
  beta=-2.5
  e0=epeak/(2.+alpha)
  b=band(eng,1.,alpha,e0,beta,1.)

  x=[0.01,1e4]
  y=[1e-3,1.]
  begplot,name='~/Fermi/Senior_Review/GRB_fig2.eps',/land,/color,/encap,font='helvetica'
  !x.margin=[3,1]
  !y.margin=[4,0]
  nu=!tsym.nu
  plot,x,y,/xlog,/ylog,xtitle='Energy (MeV)',ytitle=nu+'F'+nu,/xsty,/ysty,xrange=x,yrange=y,ytickformat='loglabels',xtickformat='loglabels',/nodata
;  polyfill,[0.01,40.,40.,0.01,0.01],[y[0],y[0],y[1],y[1],y[0]],/fill,color=!grey60
;  polyfill,[100,1e4,1e4,100,100],[y[0],y[0],y[1],y[1],y[0]],/fill,color=!grey80
  
  z=[1.03,1.03,0.97,0.97,1.03]
  polyfill,[20,100,100,20,20],[y[0],y[0],y[1],y[1],y[0]],/fill,color=!red
;  fill_box,25.,y[1],30.,6,ymin=y[0],color=!salmon
;  oplot,[40,40],[y[0],y[1]],color=!grey60,thick=5,line=1

  axis,xaxis=0,xtickformat='(A1)'
  axis,xaxis=1,xtickformat='(A1)'
  axis,yaxis=0,ytickformat='(A1)'
  axis,yaxis=1,ytickformat='(A1)'

  oplot,eng,b*eng^2
  cc=!blue
  p1=cutoff_pow(eng,[1e-2,1.7,10.])
  oplot,eng,p1*eng^2,line=2,color=cc

  p1=cutoff_pow(eng,[1e-2,1.7,100.])
  oplot,eng,p1*eng^2,line=2,color=cc
  
  p1=cutoff_pow(eng,[1e-2,1.7,500.])
  oplot,eng,p1*eng^2,line=2,color=cc

  e1=where(eng eq 1.)
  p1=cutoff_pow(eng,[b[e1],-beta,700.])
  w=where(eng gt 1)
  oplot,eng[w],p1[w]*eng[w]^2,line=1

  e1=where(eng eq 1.)
  p1=cutoff_pow(eng,[b[e1],-beta,100.])
  w=where(eng gt 1)
  oplot,eng[w],p1[w]*eng[w]^2,line=1

  p1=pow(eng,[1e-2,1.7])
  w=where(eng le 10)
  up=where(eng ge 10)
  oplot,eng[w],p1[w]*eng[w]^2,thick=5,color=cc
  oplot,eng[up],p1[up]*eng[up]^2,line=2,color=cc

  xyouts,0.1,0.2,'GBM',/data
  xyouts,22,0.2,'LLE',/data,charsize=3
  xyouts,600,0.2,'LAT',/data

  endplot
stop
return
end 
pro atel_stats,bmet,bind,bpmet,bpind,noplot=noplot

  readcol,'~/Fermi/Senior_Review/ciprini_atel/data_ATels_until_24Aug2011.txt',atel,jd,day,timesep,skip=1

  bmet=day/365.25+2008.56
  bmet=bmet[sort(bmet)]
  ind=intarr(n_elements(bmet))
  ind[*]=1
  bind=cumulative(ind[bmet])
  
  if not keyword_set(noplot) then begin 
     begplot,name='~/Fermi/Senior_Review/Atels.eps',/encap,/land,/color,font='helvetica'
     plot,bmet,bind,xtitle='Year',ytitle='Number of LAT ATels',charsize=2,xrange=[2008.5,2012],/xsty,xticks=3,xtickv=[2009,2010,2011,2012]
     endplot
     spawn,'convert ~/Fermi/Senior_Review/Atels.eps ~/Fermi/Senior_Review/Atels.pdf'
  endif 
  f=linfit(bmet,bind)
  bpmet=findgen(100)/90.*(2018-2011.65)+2011.65
  bpind=f[0]+f[1]*bpmet

;;   cd,'~/Fermi/Senior_Review'
;;   today=met2date(today())
;;   today=strmid(today,0,8)
;;   com='wget https://www-glast.stanford.edu/cgi-bin/pub_rapid --no-check-certificate -O atel_'+today+'.txt'
;;   if not exist('atel_'+today+'.txt') then spawn,com

;;   readcol,'atel_'+today+'.txt',text,format='(a)',delim='^'
;;   d=strpos(text,'GCN')
;;   w=where(d ne -1)
;;   atel=text[0:w[2]-1]
;;   gcn=text[w[2]:*]

;;   d=strpos(atel,'monospace')
;;   w=where(d ne -1,nw)
;;   dlines=atel[w]
;;   date=strarr(nw) & met=dblarr(nw)
;;   for i=0,nw-1 do begin 
;;      sep=strsplit(dlines[i],'><',/extract)
;;      date[i]=sep[2]
;;      year=strmid(date[i],0,4)
;;      mon=month(strmid(date[i],5,3))
;;      day=strmid(date[i],10,2)
;;      doy=ymd2dn(year,mon,day)
;;      met[i]=year+doy/365.
;;   endfor 
;;   s=sort(met)
;;   met=met[s]

;;   d=strpos(atel[w+1],'blazar')
;;   b=where(d ne -1,nb)
;;   bmet=met[b-1]

;;   ind=intarr(nw)
;;   ind[*]=1
;;   bind=cumulative(ind[bmet])

;;   if not keyword_set(noplot) then begin 
;;      begplot,name='Atels.eps',/land,font='helvetica',/color
;;      plot,met,cumulative(ind),psym=10,xtitle='Year',ytitle='# Atels on Fermi-LAT Discoveries',xrange=[2008.5,2012],/xsty,xticks=3,xtickv=[2009,2010,2011,2012],xminor=12

;;      oplot,bmet,bind,psym=10,color=!blue
;;      legend,['All ATels','Flaring Blazars'],/top,/left,textcolor=[!p.color,!blue]
;;      endplot
;;   endif 
;  plotsym,5,2,/fill
;  plots,2008.5,10,color=!red,psym=8
;  oplot,[2008.5,2008.5],[10,40],color=!red,thick=5
;  xyouts,2008.4,50,'Launch',color=!red,charsize=2

  return
end 

pro object_stats

;  !p.background=!white
;  !p.color=!black
  begplot,name='~/Fermi/Senior_Review/object_stats_gray.eps',/encap,/color,font='helvetica'
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 

  !x.margin=[10,0]
  !y.margin=[6,0]
;  yrange=[0,400]
  multiplot,[1,3],/init,ygap=0.005
  ;;; 0-100, 100-400,1000-4000?
  for z=0,2 do begin 
     multiplot
     case z of 
        0: yrange=[1000,4000]
        1: yrange=[100,1000]
        2: yrange=[0,100]
     endcase 
     if z eq 1 then ytitle='N Objects Detected by !4Fermi!X' else ytitle=''
     if z eq 2 then xtitle='Year' else xtitle=''
     yticks=0 & ytickname=''
     if z eq 0 then begin 
        yticks=3 
;        ytickname=[' ','2000','3000','4000']
     endif 
     plot,[2008.5,2018.5],yrange,xtitle=xtitle,ytitle=ytitle,/nodata,xrange=[2008.5,2018.5],/xsty,xminor=4,yminor=4,yrange=yrange,/ysty,yticks=yticks;,ytickname=ytickname
     
     polyfill,[2008.5,2013.5,2013.5,2008.5,2008.5],[yrange[0],yrange[0],yrange[1],yrange[1],yrange[0]],color=!white ;grey90
     polyfill,[2013.5,2018.5,2018.5,2013.5,2013.5],[yrange[0],yrange[0],yrange[1],yrange[1],yrange[0]],color=!grey90
;  polyfill,[2008,2008.5,2008.5,2008,2008],[1,1,10000,10000,1],color=!grey40

     t=findgen(101)/10.
  ;;; GRBs (GBM & LAT)
                                ; add LLE only bursts?
     ;; predictions
     ggrbs=230*t
     lgrbs=10*t
     ggrbs[0]=0.1
     lgrbs[0]=0.1
     g=where(t+2008.5 gt 2012)
     l=where(t+2008.7 gt 2011.8)
     sg=ggrbs[g[0]]
     sl=lgrbs[l[0]]
     gbmgrbyearp=t[g]+2008.5
     gbmgrbpp=ggrbs[g]
     latgrbyearp=t[l]+2008.7
     latgrbpp=lgrbs[l]

     ;; actual measurements
     ;; GBM
;     readcol,'~/Fermi/Swift_Fermi/GBM_triggers_111021.txt',trigger_name,gname,gtrigtime,loc,format='(a,a,a,a)',delim='|',/silent,skip=4
;     g=where(strtrim(loc,2) eq 'GRB',ng)
;     sf=where(strtrim(loc,2) eq 'SFLARE',nsf)
;     sgr=where(strtrim(loc,2) eq 'SGR',nsgr)

;     nt=n_elements(trigger_name)-1
;     gmet=dblarr(nt)
;;  gname=gname[s] & gtrigtime=gtrigtime[s] & loc=loc[s] & ggrb=ggrb[s]
;     year=fltarr(nt)
;     gtrigtime=gtrigtime[1:*]
;     for i=0,nt-1 do begin
;        gmet[i]=date2met(gtrigtime[i],/fermi)
;        date=met2date_judy(gmet[i])
;        year[i]=date[0]+date[1]/365.+date[2]/24./365+date[3]/60./24./3600.
;     endfor 
;     s=sort(year[g])
;     g=g[s]
;     gname=gname[g]
;     gtrigtime=gtrigtime[g]
;     loc=loc[g]
;     ggrb=strmid(gname,3,6)

;     s=sort(year[sf])
;     sfdate=year[sf[s]]
;     s=sort(year[sgr])
;     sgrdate=year[sgr[s]]
;     sf=intarr(nsf)
;     sf[*]=1
;     sgr=intarr(nsgr)
;     sgr[*]=1

     readcol,'~/Fermi/Senior_Review/GBM_triggers.csv',mwk,grbs,particles,tgfs,sgrs,sfs,other,skip=1

     gbmgrbyear=2008.5+mwk/52.
     gbmgrbp=cumulative(grbs)
     

;     b=intarr(ng)
;     b[*]=1
;     b=[0,b]
;     gbmgrbyear=[2008.5,year[g],2011.8]
;     gbmgrbp=[cumulative(b),sg]

     ;; Flare data from Andy's group
     restore,'flare_rate.sav'
     syear=flare_rate[0,*]
     mnth=flare_rate[1,*]
     p95=flare_rate[2,*]
     p50=flare_rate[3,*]
     p5=flare_rate[4,*]
     gbm=flare_rate[5,*]
     syear=mnth/12.+syear
     sp=where(syear gt 2008.5)
     syear=syear[sp] & p50=p50[sp] & gbm=gbm[sp]
     w=where(syear lt 2011.8)

     sfyear=[2008.5,syear[w]]
     sfp=[0.1,cumulative(gbm[w])]
;     sfyear=2008.5+mwk/52.
;     sfp=cumulative(sfs)

     sp2=where(syear gt 2011.8)
     cp50=cumulative(p50*0.7)

     sfyearp=syear[sp2]
     sfpp=cp50[sp2]
;  year  month  95%probability  50%probability  5%probability  gbmrate
     ;; LAT SF
     
     nlsf=n_elements(lat_dates)
     lsfyear=fltarr(nlsf)
     for i=0,nlsf-1 do begin
        date=str_sep(lat_dates[i],'-')
        lsfyear[i]=date[2]+(month(date[1])-1.)/12.+date[0]/30./12.
     endfor 
     
     lsfp=cumulative(replicate(1.,nlsf))
     lsfyear=lsfyear

     ;; LAT GRB
     readcol,'~/Fermi/Swift_pop_study/lat_summary.csv',grb,format='(a)',skip=1
     w=where(grb ne '',n)
     grb=grb[w]
     date=fltarr(n)
     for i=0,n-1 do begin
        year='20'+strmid(grb[i],0,2)
        mnth=strmid(grb[i],2,2)
        day=strmid(grb[i],4,2)
        doy=ymd2dn(year,mnth,day)
        date[i]=year*1.+doy/365.
     endfor 
     b=intarr(n)
     b[*]=1
     b=[0,b]

     latgrbyear=[2008.7,date,2011.8]
     latgrbp=[cumulative(b),sl]

     ;;Pulsars - from Alice

     readcol,'~/Fermi/Senior_Review/Pulsars_BigFile_v012detect/Total-Table\ 1.csv',lines,delim='$',format='(a)',skip=1
     nlines=n_elements(lines)

     pdate=fltarr(nlines)
     for i=0,nlines-1 do begin 
        chunks=str_sep(lines[i],',')
        pdate[i]=chunks[4]
     endfor 

     w=where(pdate gt 2008,nw)
     pdate=pdate[w]
     p=intarr(nw)
     p[*]=1
     psryear=pdate
     psrp=cumulative(p)

     ;; PREDICTIONS FROM ALICE
     pdate=[0.5,1.,2.,3.,5,6,8,10.]+2008.5
     pnum=[34,51,75,93,183,197,221,246]
     w=where(pdate gt 2011)
     psryearp=pdate[w]
     psrpp=pnum[w]

;6 months    24            10            34
;1 year         36            15            51
;2 years       53            22            75
;3 years       66            27            93
;5 years       89            34            123
;10 years     128          48            176
     ;; SNR - from Jack

     snrdate=[0, 393, 430, 503, 559, 586, 613, 614, 913, 954,1031,1038,1060,1068,1082,1124,1168] ;;days
     snryear=snrdate/365.+2008.7
     snryearp=findgen(25)/3.+2012
     snrpp=7.*(snryearp-max(snryear))+17. ;;; linearly with time (7 per year)
     nsnr=n_elements(snrdate)
     p=intarr(nsnr)
     p[*]=1
     snrp=cumulative(p)

     ;; blazar flares from ATels
     atel_stats,bmet,bind,bpmet,bpind,/noplot

     ;; FSRQs & BL Lacs from Marco
     bltime=[0,0.25,11/12.,2.,3.,5.,7,10.]+2008.7
;  fsrq=[0.01,64.,296.,360.,414,459,492,600]
;  bllac=[0.01,46.,300,423.,436,535,610,700]
     fsrq=[0.01,64.,466,580.,646,716,767,936]
     bllac=[0.01,46,441,675,861,1159,1404,1716]

     ;; add 1LAC, 2LAC

     ;; 0FGL
     cat0=mrdfits('0FGL_data.fits',1)
     agn=where(cat0.class eq 'AGN' or cat0.class eq 'agn',nagn0)  ;;non-blazar AGN
     bzb=where(cat0.class eq 'BZB' or cat0.class eq 'bzb',nbzb0)  ;;BL Lac type of blazar
     bzq=where(cat0.class eq 'BZQ' or cat0.class eq 'bzq',nbzq0)  ;;FSRQ type of blazar
     gal=where(cat0.class eq 'GAL' or cat0.class eq 'gal',ngal0)  ;;Normal galaxy
     hmb=where(cat0.class eq 'HMB',nhmb0)                         ;;High Mass Binary
     nov=where(cat0.class eq 'NOV',nnov0)                         ;;Nova
     psr=where(cat0.class eq 'PSR' or cat0.class eq 'PSR LAT',npsr0)  ;;Pulsar
     pwn=where(cat0.class eq 'PWN',npwn0)                             ;;Pulsar Wind Nebula
     rdg=where(cat0.class eq 'RDG' or cat0.class eq 'rdg',nrdg0)      ;;Radio Galaxy
     sey=where(cat0.class eq 'SEY',nsey0)                             ;;Seyfert Galaxy
     snr=where(cat0.class eq 'SNR' or cat0.class eq 'snr',nsnr0) ; or cat0.class eq 'spp',nsnr2)   ;;SNR
     agu=where(cat0.class eq 'agu',nagu0)                        ;;Active galaxy of unknown type
     glc=where(cat0.class eq 'glc',nglc0)                        ;;Globular Cluster
     unc=where(cat0.class eq '       ',nunc0)                    ;;Unassociated

     ;; 1FGL
     nrdg=0 & nsey=0 & nnov=0
     cat1=mrdfits('gll_psc_v03.fit',1)
     agn=where(cat1.class1 eq 'AGN' or cat1.class1 eq 'agn',nagn)  ;;non-blazar AGN
     bzb=where(cat1.class1 eq 'BZB' or cat1.class1 eq 'bzb',nbzb)  ;;BL Lac type of blazar
     bzq=where(cat1.class1 eq 'BZQ' or cat1.class1 eq 'bzq',nbzq)  ;;FSRQ type of blazar
     gal=where(cat1.class1 eq 'GAL' or cat1.class1 eq 'gal',ngal)  ;;Normal galaxy
     hmb=where(cat1.class1 eq 'HXB' or cat1.class1 eq 'MQO',nhmb)  ;;High Mass Binary
     psr=where(cat1.class1 eq 'PSR' or cat1.class1 eq 'psr',npsr)  ;;Pulsar
     pwn=where(cat1.class1 eq 'pwn',npwn)                          ;;Pulsar Wind Nebula
;  rdg=where(cat1.class1 eq 'RDG' or cat1.class1 eq 'rdg',nrdg)  ;;Radio Galaxy
;  sey=where(cat1.class1 eq 'SEY',nsey)                         ;;Seyfert Galaxy
     snr=where(cat1.class1 eq 'SNR',nsnr) ; or cat1.class1 eq 'spp',nsnr)   ;;SNR
     agu=where(cat1.class1 eq 'agu',nagu) ;;Active galaxy of unknown type
     glc=where(cat1.class1 eq 'glc',nglc) ;;Globular Cluster
     unc=where(cat1.class1 eq '   ',nunc) ;;Unassociated

  ;;; 2FGL
     cat2=mrdfits('gll_psc_v06.fit',1)
     agn=where(cat2.class1 eq 'AGN' or cat2.class1 eq 'agn',nagn2)  ;;non-blazar AGN
     bzb=where(cat2.class1 eq 'BZB' or cat2.class1 eq 'bzb',nbzb2)  ;;BL Lac type of blazar
     bzq=where(cat2.class1 eq 'BZQ' or cat2.class1 eq 'bzq',nbzq2)  ;;FSRQ type of blazar
     gal=where(cat2.class1 eq 'GAL' or cat2.class1 eq 'gal',ngal2)  ;;Normal galaxy
     hmb=where(cat2.class1 eq 'HMB',nhmb2)                          ;;High Mass Binary
     nov=where(cat2.class1 eq 'NOV',nnov2)                          ;;Nova
     psr=where(cat2.class1 eq 'PSR' or cat2.class1 eq 'psr',npsr2)  ;;Pulsar
     pwn=where(cat2.class1 eq 'PWN',npwn2)                          ;;Pulsar Wind Nebula
     rdg=where(cat2.class1 eq 'RDG' or cat2.class1 eq 'rdg',nrdg2)  ;;Radio Galaxy
     sey=where(cat2.class1 eq 'SEY',nsey2)                          ;;Seyfert Galaxy
     snr=where(cat2.class1 eq 'SNR' or cat2.class1 eq 'snr',nsnr2) ; or cat2.class1 eq 'spp',nsnr2)                         ;;SNR
     agu=where(cat2.class1 eq 'agu',nagu2)                         ;;Active galaxy of unknown type
     glc=where(cat2.class1 eq 'glc',nglc2)                         ;;Globular Cluster
     unc=where(cat2.class1 eq '   ',nunc2)                         ;;Unassociated

     agn=[0.1,nagn0+nagu0+nrdg0+nsey0,nagn+nagu+nrdg+nsey,nagn2+nagu2+nrdg2+nsey2]
     blazars=[0.1,nbzb0+nbzq0,nbzb+nbzq,nbzb2+nbzq2]
     rdg=[nrdg0,nrdg,nrdg2]
     gal=[0.1,ngal0,ngal,ngal2]
     hxb=[0.1,nhmb0,nhmb,nhmb2]
     nov=[0.1,nnov0,nnov,nnov2]
     psr=[0.1,npsr0,npsr,npsr2]
     pwn=[0.1,npwn0,npwn,npwn2]
     snr=[0.1,nsnr0,nsnr,nsnr2]
     glc=[0.1,nglc0,nglc,nglc2]
     unc=[0.1,nunc0,nunc,nunc2]

     other=nov+glc+hxb+pwn

     rdgyearp=findgen(11)
     rdgpp=11*rdgyearp^0.75
     rdgyearp=rdgyearp+2008.7

     galyearp=findgen(10)+1
     galpp=7.*galyearp^0.75
     galyearp=galyearp+2008.7
     
     hardyear=[0.,1.5,2.,3.]+2008.7
     hardp=[0.,284.,323.,495.]
     hardyearp=findgen(9)+3.
     hardpp=1.28*255.*(hardyearp/2.) ;495*(years/3)
     hardyearp=hardyearp+2008.7

     ;; gal other
     ;; extra gal other
     ;; separate out FSRQ & BL Lac

     cattime=[0.,0.25,11./12.,2.]
     catcolor=[!sienna,!seagreen,!purple,!salmon]
     catname=['AGN','Galaxies','Unassociated','Other'] ;,'Solar Flares']
     

     th=8
;;      oplot,gbmgrbyearp,gbmgrbpp,color=!red,thick=th,line=2
;;      oplot,latgrbyearp,latgrbpp,color=!magenta,thick=th,line=2
;;      oplot,gbmgrbyear,gbmgrbp,color=!red,psym=10,thick=th
;;      oplot,latgrbyear,latgrbp,color=!magenta,psym=10,thick=th
;; ;     oplot,lsfyear,lsfp,color=!salmon,psym=10,thick=th
;;      oplot,bltime[0:3],fsrq[0:3],color=!cyan,thick=th
;;      oplot,bltime[3:*],fsrq[3:*],color=!cyan,line=2,thick=th
;;      oplot,bltime[0:3],bllac[0:3],color=!dodgerblue,thick=th
;;      oplot,bltime[3:*],bllac[3:*],color=!dodgerblue,line=2,thick=th
;;      oplot,bmet,bind,color=!orange,psym=10,thick=th
;;      oplot,bpmet,bpind,color=!orange,psym=10,thick=th,line=2
;;      oplot,snryear,snrp,color=!blue,psym=10,thick=th
;;      oplot,snryearp,snrpp,color=!blue,line=2,thick=th
;;      oplot,rdgyearp,rdgpp,color=!sienna,line=2,thick=th
;; ;     oplot,galyearp,galpp,color=!seagreen,line=2,thick=th

     grey=!grey50
     oplot,gbmgrbyearp,gbmgrbpp,color=grey,thick=th,line=2
     oplot,latgrbyearp,latgrbpp,color=grey,thick=th,line=2
     oplot,gbmgrbyear,gbmgrbp,color=grey,psym=10,thick=th
     oplot,latgrbyear,latgrbp,color=grey,psym=10,thick=th
     oplot,bltime[0:3],fsrq[0:3],color=grey,thick=th
     oplot,bltime[3:*],fsrq[3:*],color=grey,line=2,thick=th
     oplot,bltime[0:3],bllac[0:3],color=grey,thick=th
     oplot,bltime[3:*],bllac[3:*],color=grey,line=2,thick=th
     oplot,bmet,bind,color=grey,psym=10,thick=th
     oplot,bpmet,bpind,color=grey,psym=10,thick=th,line=2
     oplot,snryear,snrp,color=grey,psym=10,thick=th
     oplot,snryearp,snrpp,color=grey,line=2,thick=th
     oplot,rdgyearp,rdgpp,color=grey,line=2,thick=th

     ;;; keep in color
     oplot,sfyear,sfp,color=!darkgreen,psym=10,thick=th
     oplot,sfyearp,sfpp,color=!darkgreen,line=2,thick=th
     oplot,psryear,psrp,color=!violet,psym=10,thick=th
     oplot,psryearp,psrpp,color=!violet,line=2,thick=th
     oplot,hardyearp,hardpp,color=!purple,line=2,thick=th
     oplot,hardyear,hardp,color=!purple,thick=th

     plotsym,0,/fill
     oplot,cattime+2008.7,rdg,color=grey;catcolor[0]
     oplot,cattime+2008.7,gal,color=grey;catcolor[1]
     if z eq 0 then begin 
        xyouts,2009.3,yrange[1]*0.9,'Prime Mission',/data,charsize=1.9,charthick=5
        xyouts,2013.8,yrange[1]*0.9,'Extended Mission',/data,charsize=1.9,charthick=5
     endif 
     axis,xaxis=0,xrange=[2008.5,2018.5],/xsty,/data,xtickformat='(A1)'
     
     if z eq 0 then axis,2008.5,yrange[1],xaxis=1,xrange=[2008.5,2018.5],xstyle=1,xticks=6,xtickv=[findgen(6)*2+2008.5],xminor=4,xtickname=ntostr([0,2,4,6,8,10]),xtitle='Year of Mission' else begin 
        axis,2008.5,yrange[1],xaxis=1,xrange=[2008.5,2018.5],xstyle=1,xticks=6,xtickv=[findgen(6)*2+2008.5],xminor=4,xtickformat='(A1)'
     endelse 
     if z eq 0 then begin 
        axis,yaxis=0,ytickformat='(A1)',yminor=4,yrange=yrange,/ysty,yticks=3;,ytickname=[' ','2000','3000','4000'] 
        axis,yaxis=1,yminor=4,yrange=yrange,/ysty,yticks=3;,ytickname=[' ','2000','3000','4000'] 
     endif
     if z ge 1 then begin
        axis,yaxis=0,ytickformat='(A1)',yminor=4,yrange=yrange,/ysty
        axis,yaxis=1,yminor=4,yrange=yrange,/ysty
     endif
     if z eq 1 then begin
        xyouts,2007.65,90,'100',/data
        xyouts,2018.61,90,'100',/data
     endif 
     if z eq 0 then begin 
        xyouts,2016,2500,'GBM GRBs',/data,color=grey;!red
        xyouts,2010.5,2000,'GBM Solar Flares',/data,color=!darkgreen
     endif 
     if z eq 1 then begin 
        xyouts,2011.5,300,'Blazar Flares',/data,color=grey;!orange
        xyouts,2008.8,550,'FSRQ',/data,color=grey;!cyan
        xyouts,2008.8,700,'BL Lac',/data,color=grey;!dodgerblue
        xyouts,2012.5,500,'Sources (>10 GeV)',/data,color=!purple
     endif         
     if z eq 2 then begin
        xyouts,2011.8,90,'Pulsars',/data,color=!violet
;        xyouts,2015.,18,'Normal Galaxies',/data,color=!seagreen
        xyouts,2017.,40,'SNR',/data,color=grey;!blue
        xyouts,2015.,50,'Radio Galaxies',/data,orient=10,color=grey;!sienna
        xyouts,2014.5,80,'LAT GRBs',/data,color=grey;!magenta
;        xyouts,2012,5,'LAT Solar Flares',/data,color=!salmon
     endif 
;     xyouts,2010.4,350,'GBM GRBs',/data,color=!red
;     xyouts,2016,100,'LAT GRBs',/data,color=!magenta
;     xyouts,2011.8,75.,'Pulsars',/data,color=!violet
;     xyouts,2017.5,40,'SNR',/data,color=!blue
;     xyouts,2011.8,160,'Blazar Flares',/data,color=!orange
;     xyouts,2008.6,350,'FSRQ',/data,color=!cyan
;     xyouts,2008.6,370,'BL Lac',/data,color=!dodgerblue
;     xyouts,2011.3,200,'GBM Solar Flares',/data,color=!darkgreen
;;  xyouts,2012,5,'LAT Solar Flares',/data,color=!salmon
;     xyouts,2015.5,55,'Radio Galaxies',/data,color=!sienna
;     xyouts,2016,15,'Normal Galaxies',/data,color=!seagreen
;     xyouts,2010.1,250,'Sources (>10 GeV)',/data,color=!purple
;;  legend,[catname],textcolor=[catcolor],box=0,/center,/left

  ;;; INSET PLOTS
;;   yrange=[0,4000]
;;   plot,[2008.5,2018.5],yrange,xtitle='Year',ytitle='N Objects Detected by Fermi',/nodata,xrange=[2008.5,2018.5],/xsty,position=[0.62,0.55,0.92,0.85],/noeras,charsize=1.2
;;   polyfill,[2008.5,2013.5,2013.5,2008.5,2008.5],[yrange[0],yrange[0],yrange[1],yrange[1],yrange[0]],color=!white ;grey90
;;   polyfill,[2013.5,2018.5,2018.5,2013.5,2013.5],[yrange[0],yrange[0],yrange[1],yrange[1],yrange[0]],color=!grey90


;;   oplot,gbmgrbyearp,gbmgrbpp,color=!red,thick=th,line=2
;;   oplot,latgrbyearp,latgrbpp,color=!magenta,thick=th,line=2
;;   oplot,gbmgrbyear,gbmgrbp,color=!red,psym=10,thick=th
;;   oplot,latgrbyear,latgrbp,color=!magenta,psym=10,thick=th
;;   oplot,sfyear,sfp,color=!darkgreen,psym=10,thick=th
;;   oplot,sfyearp,sfpp,color=!darkgreen,line=2,thick=th
;;   oplot,bltime[0:3],fsrq[0:3],color=!cyan,thick=th
;;   oplot,bltime[3:*],fsrq[3:*],color=!cyan,line=2,thick=th
;;   oplot,bltime[0:3],bllac[0:3],color=!dodgerblue,thick=th
;;   oplot,bltime[3:*],bllac[3:*],color=!dodgerblue,line=2,thick=th
;;   oplot,bmet,bind,color=!orange,psym=10,thick=th
;;   oplot,snryear,snrp,color=!blue,psym=10,thick=th
;;   oplot,snryearp,snrpp,color=!blue,line=2,thick=th
;;   oplot,psryear,psrp,color=!violet,psym=10,thick=th
;;   oplot,psryearp,psrpp,color=!violet,line=2,thick=th
;;   oplot,rdgyearp,rdgpp,color=!sienna,line=2,thick=th
;;   oplot,hardyearp,hardpp,color=!purple,line=2,thick=th
;;   oplot,hardyear,hardp,color=!purple,thick=th


;;   plotsym,0,0.5,/fill
;;   oplot,cattime+2008.7,rdg,color=catcolor[0]
;; ;  oplot,cattime+2008.7,gal,color=catcolor[1],psym=8
;; ;  oplot,cattime+2008.7,unc,color=catcolor[2],psym=8
;; ;  oplot,cattime+2008.7,other,color=catcolor[3],psym=8

;;   xyouts,2009.5,yrange[1]*0.9,' Prime',/data,charsize=1.2
;;   xyouts,2009.5,yrange[1]*0.8,'Mission',/data,charsize=1.2
;;   xyouts,2013.8,yrange[1]*0.9,'Extended',/data,charsize=1.2
;;   xyouts,2013.8,yrange[1]*0.8,' Mission',/data,charsize=1.2

;;   xyouts,2015.2,2400,'GBM GRBs',/data,color=!red,charsize=0.8
;;   xyouts,2016.7,300.,'Pulsars',/data,color=!violet,charsize=0.8
;;   xyouts,2017,1000,'FSRQ',/data,color=!cyan,charsize=0.8
;;   xyouts,2016,1700,'BL Lac',/data,color=!dodgerblue,charsize=0.8
;;   xyouts,2011,2600,'GBM Solar Flares',/data,color=!darkgreen,charsize=0.8
;;   xyouts,2012.4,300,'Sources (>10 GeV)',/data,color=!purple,charsize=0.8
;; ;  xyouts,2010.3,200,'AGN',/data,color=!sienna,charsize=0.5


;;   axis,xaxis=0,xrange=[2008.5,2018.5],/xsty,/data,xtickformat='(A1)'

;;   axis,2008.5,yrange[1],xaxis=1,xrange=[2008.5,2018.5],xstyle=1,xticks=6,xtickv=[findgen(6)*2+2008.5],xminor=4,xtickname=ntostr([0,2,4,6,8,10]),xtitle='Year of Mission',charsize=1.2
;;   axis,yaxis=0,charsize=1,ytickformat='(A1)',yminor=5
;;   axis,yaxis=1,charsize=1,yminor=5,ytickformat='(A1)'

  endfor 
  multiplot,/reset,/default
  endplot
  spawn,'convert ~/Fermi/Senior_Review/object_stats_gray.eps ~/Fermi/Senior_Review/object_stats_gray.pdf'
  stop
  return
end 

pro detthresh_plot
  
  readcol,'~/Fermi/Senior_Review/detthresh_time_P7v6Source.txt',time,ind1,ind2,ind3,format='(f,f,f,f)'
  readcol,'~/Fermi/Senior_Review/detthresh_envelope_P7v6Source_v2.txt',eng,d1,d2,d5,d10,format='(f,f,f,f,f)'
  ;;; 0.1-100 GeV detection threshold in eV/cm2/s 

  begplot,name='~/Fermi/Senior_Review/detthresh_time.ps',/land
  plot,time,ind2,xtitle='Years of Exposure',ytitle='Detection Threshold (0.1-100 GeV) (ev/cm2/s)'
  endplot

  begplot,name='~/Fermi/Senior_Review/detthresh_envelope.eps',/land,/encap,/color,font='helvetica'
;  !x.margin=[15,2]
  !x.margin=[6,1]
  !y.margin=[4,0]

  nu=!tsym.nu
  ev2erg=1.602d-12 ;; ev->erg

;  plot,[0.1,300],[1e-13,1e-10],xtitle='Energy (GeV)',ytitle='Detection Threshold   '+nu+'F'+nu+' (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,/nodata,/xsty,xrange=[0.1,300]
  plot,[0.1,300],[1e-13,1e-10],xtitle='Energy (GeV)',ytitle=nu+'F!L'+nu+'!N (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,/nodata,/xsty,xrange=[0.1,300],title='Power-Law Detection Threshold',charsize=2.

  legend,['Fermi LAT'],box=0,/top,/right,charsize=2
  th=8
  oplot,eng,d1*ev2erg,color=!blue,thick=th
  oplot,eng,d2*ev2erg,color=!green,thick=th
  oplot,eng,d5*ev2erg,color=!orange,thick=th
  oplot,eng,d10*ev2erg,color=!red,thick=th

  xyouts,1,1.*ev2erg,'1 year',/data,color=!blue
  xyouts,2.5,0.47*ev2erg,'2 years',/data,color=!green
  xyouts,4.5,0.25*ev2erg,'5 years',/data,color=!orange
  xyouts,5.,0.11*ev2erg,'10 years',/data,color=!red

  nsync=10^(-5.38)
  sync=3.99
;sync = (3.99 + 0.12 - 0.08)
  nic=10^(-11.6)
  ic=1.64

  x=10^(findgen(100)/20d)/20.*1000.
  y1=pow(x,[nsync,sync])*x^2
  y2=pow(x,[nic,ic])*x^2
  oplot,x/1000.,y1+y2,color=!grey50,line=1,thick=th
  oplot,x/1000.,(y1+y2)*0.1,color=!grey50,line=1,thick=th
  oplot,x/1000.,(y1+y2)*0.01,color=!grey50,line=1,thick=th

  xyouts,2,3e-11,'Crab Nebula',color=!grey50
  xyouts,10,5e-12,'10% Crab Nebula',color=!grey50
  xyouts,0.2,2.5e-13,'1% Crab Nebula',color=!grey50

  endplot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  begplot,name='~/Fermi/Senior_Review/differential_sensitivity.eps',/land,/encap,/color,font='helvetica'
  !x.margin=[6,1]
  !y.margin=[4,0]
  nu=!tsym.nu
  ev2erg=1.602d-12 ;; ev->erg
  readcol,'~/Fermi/Senior_Review/detthresh_narrowbins_P7v6Source.txt',eng,d1,d2,d3,d5,d10,format='(f,f,f,f,f,f)'

  plot,[0.1,300],[1e-13,1e-10],xtitle='Energy (GeV)',ytitle='Detection Threshold   '+nu+'F!L'+nu+'!N (erg cm!U-2!N s!U-1!N)',/xlog,/ylog,/nodata,/xsty,xrange=[0.1,300]
  th=8
  oplot,eng,d1*ev2erg,color=!blue,thick=th
  oplot,eng,d2*ev2erg,color=!green,thick=th
  oplot,eng,d5*ev2erg,color=!orange,thick=th
  oplot,eng,d10*ev2erg,color=!red,thick=th

  xyouts,1,2.5*ev2erg,'1 year',/data,color=!blue
  xyouts,2.3,1.4*ev2erg,'2 years',/data,color=!green
  xyouts,4.2,0.9*ev2erg,'5 years',/data,color=!orange
  xyouts,5.,0.3*ev2erg,'10 years',/data,color=!red

  nsync=10^(-5.38)
  sync=3.99
;sync = (3.99 + 0.12 - 0.08)
  nic=10^(-11.6)
  ic=1.64

  x=10^(findgen(100)/20d)/20.*1000.
  y1=pow(x,[nsync,sync])*x^2
  y2=pow(x,[nic,ic])*x^2
  oplot,x/1000.,y1+y2,color=!grey50,line=1,thick=th
  oplot,x/1000.,(y1+y2)*0.1,color=!grey50,line=1,thick=th
  oplot,x/1000.,(y1+y2)*0.01,color=!grey50,line=1,thick=th

  xyouts,2,3e-11,'Crab Nebula',color=!grey50
  xyouts,4,1e-11,'10% Crab Nebula',color=!grey50
  xyouts,0.2,2.5e-13,'1% Crab Nebula',color=!grey50

  endplot

 
stop
  return
end 

pro curved_polygon,x0,x1,y0,y1,x,y,color=color,noplot=noplot

  ang=indgen(90)*!dtor
  r=0.1d
  ry=0.5d
  if y1-y0 lt ry then ry=0.2d
  ay0=alog10(y0)
  ay1=alog10(y1)
  ca=r*cos(ang)
  sa=ry*sin(ang)
  x=[-ca+x0+r,reverse(ca+x1-r),ca+x1-r,reverse(-ca+x0+r),-ca[0]+x0+r]
  y=10^[ay1+sa-ry,reverse(ay1+sa-ry),ay0-sa+ry,reverse(ay0-sa+ry),ay1+sa[0]-ry]
;plot,[0,12],[10,1e4],/ylog  
  if not keyword_set(noplot) then polyfill,x,y,color=color

;  stop
return
end

pro fsr_mission_time_plot
  
  year=[2008,2009,2010,2011]
  ss=[99.3,97,97.4,93.]
  arr=[0.4,1.7,0.9,0.4]
  too=[0,0.,1.6,6.2]
  eng=[0.3,1.3,0.1,0.2]

  plot,[2008,2012],[0,100],/nodata,xtitle='Year',ytitle='Percent',/ylog,yrange=[0.01,100]

  oplot,year,ss
  oplot,year,arr,color=!red,line=1
  oplot,year,too,color=!blue,line=2
  oplot,year,eng,color=!green,line=3

stop
  return
end 
  
pro fsr_plots,usewav=usewav,usefreq=usefreq,ps=ps,land=land

  ;;;;; PLOT FOR FERMI SENIOR REVIEW
  ;;;;; WAVELENGTH VS YEAR INDICATING MISSIONS AND SURVEY CLASS VS
  ;;;;; NARROW-FIELD

  add='_eng'
  q='eng'
  if keyword_set(usefreq) then begin 
     ytitle='Frequency (Hz)'
     yrange=[1e6,1e30]
     add='_freq'
     q='freq'
  endif 

  if keyword_set(usewav) then begin
     ytitle='Wavelength (cm)'
     yrange=[1e-18,1e4]
     add='_wavelen'
     q='wavelength'
  endif 

  add2=''
;  if keyword_set(land) then add2='_land'
;  if keyword_set(ps) then 
  begplot,name='~/Fermi/Senior_Review/missions'+add+add2+'.eps',/color,font='helvetica',/land
  DEVICE, /PALATINO, /ITALIC,/BOLD, FONT_INDEX=4 
  
  obs=create_struct('name','','ops',fltarr(2),'ext_ops',0.,'wavelength',dblarr(2),$
                    'eng',dblarr(2),'freq',dblarr(2),'survey',0,'point',0,'space',0,'ground',0,$
                    'messenger','','color',0d,'xoffset',0.,'yoffset',0.,'fcolor',0d,'charsize',0.)
  n=70
  obs=replicate(obs,n)
  h=4.135e-15;*1e-3 ;; keV s
  h=h;*1d-3 ;; MeV s
  c=3e10 ;;cm/s

  obs.ext_ops=2018
  opt=c/300e-7 ;;; blue?

;  if keyword_set(ps) then 
  obs.color=!white
  obs.yoffset=0.4
  obs.xoffset=0.1
  obs.charsize=1.0

  i=58
;  obs[i].name='Fermi-GBM'
  obs[i].name='!4Fermi!X'
  obs[i].ops=[2008.5,2013.5]
  obs[i].ext_ops=2018
;  obs[i].eng=[15.,30e3] ;;keV
  obs[i].eng=[14,300e6] ;; keV
  obs[i].survey=1
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!red
  obs[i].charsize=3.
  obs[i].xoffset=1.7
;  obs[i].yoffset=1.5

;  i=59
;  obs[i].name='Fermi-LAT'
;  obs[i].ops=[2008.5,2013]
;  obs[i].ext_ops=2018
;  obs[i].eng=[100e3,300e6] ;;keV
;  obs[i].survey=1
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].fcolor=!blue

  ;;; VHE GAMMA-RAY

  i=0
  obs[i].name='HESS II'
  obs[i].yoffset=2.
  obs[i].ops=[2004,2020]
  obs[i].eng=[0.03,100]*1e9 ;;(TeV->keV)
;  obs[i].survey=1 ;;;????
  obs[i].point=1
  obs[i].ground=1
  obs[i].fcolor=!purple


  i=1
  obs[i].name='MAGIC II'
  obs[i].ops=[2005,2020]
  obs[i].yoffset=0.2
  obs[i].eng=[0.03,30]*1e9 ;;(TeV->keV)
;  obs[i].survey=1 ;;;????
  obs[i].point=1
  obs[i].ground=1
  obs[i].fcolor=!purple
  obs[i].yoffset=1.05

  i=2
  obs[i].name='VERITAS'
  obs[i].ops=[2009,2012.5]
  obs[i].ext_ops=2012.5
  obs[i].eng=[0.1,50]*1e9 ;;(TeV->keV)
;  obs[i].survey=1 ;;;????
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=1.
  obs[i].fcolor=!purple

  i=7
  obs[i].name='VERITAS II'
  obs[i].ops=[2012.5,2020]
  obs[i].eng=[0.1,50]*1e9 ;;(TeV->keV)
;  obs[i].survey=1 ;;;????
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.2
  obs[i].fcolor=!purple

  i=27
  obs[i].name='CTA'
  obs[i].ops=[2018,2020]
  obs[i].eng=[10e6,100e9] ;;keV
  obs[i].ground=1
  obs[i].point=1
  obs[i].fcolor=!purple

  i=41
  obs[i].name='HAWC'
  obs[i].ops=[2014,2020] ;;?????
  obs[i].eng=[0.1,100]*1e9 ;; keV
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!purple
  obs[i].color=!p.color

  ;;; GAMMA-RAY
  i=8
  obs[i].name='Swift-BAT'
  obs[i].ops=[2004,2012]
  obs[i].ext_ops=2018
  obs[i].eng=[15.,150.] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].fcolor=!cyan
  obs[i].color=!p.color
  obs[i].yoffset=0.35
  obs[i].xoffset=0.05

  i=62
  obs[i].name='SVOM-GRM/ECLAIRs'
  obs[i].ops=[2016,2020]
  obs[i].space=1
  obs[i].eng=[4,5e3] ;;keV
  obs[i].survey=1
  obs[i].fcolor=!cyan
;  obs[i].yoffset=1.
  obs[i].xoffset=0.1
  obs[i].yoffset=8
  obs[i].color=!p.color

;  i=61
;  obs[i].name='SVOM-ECLAIRs'
;  obs[i].ops=[2016,2020]
;  obs[i].space=1
;  obs[i].eng=[4,250] ;;keV
;  obs[i].survey=1
;  obs[i].fcolor=!cyan
;  obs[i].color=!p.color

  i=44
  obs[i].name='AGILE'
  obs[i].eng=[250,5e6] ;; keV
  obs[i].ops=[2007,2013.6]
  obs[i].ext_ops=2010
  obs[i].space=1
  obs[i].point=1
  obs[i].survey=1
  obs[i].fcolor=!salmon;!dodgerblue
  obs[i].color=!p.color
  obs[i].xoffset=0.01

;  i=60
;  obs[i].name='AGILE'
;  obs[i].eng=[250,5e6] ;; keV
;  obs[i].ops=[2010,2013]
;  obs[i].ext_ops=2013
;  obs[i].space=1
;  obs[i].survey=1
;  obs[i].fcolor=!salmon;!DodgerBlue
;  obs[i].color=!p.color


  ;;; X-RAY

  i=68
  obs[i].name='SVOM-MXT'
  obs[i].ops=[2016,2020]
  obs[i].space=1
  obs[i].eng=[0.3,6] ;; keV
  obs[i].point=1
  obs[i].fcolor=!green
  obs[i].color=!white
  obs[i].yoffset=0.3

  i=10
  obs[i].name='Swift-XRT'
  obs[i].ops=[2004,2012]
  obs[i].ext_ops=2018
  obs[i].eng=[0.3,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!green
  obs[i].color=!white
  obs[i].xoffset=2.5

  i=28
  obs[i].name='Chandra'
  obs[i].ops=[1999,2012]
  obs[i].ext_ops=2012
  obs[i].eng=[0.2,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].color=!white
  obs[i].fcolor=!green
;  obs[i].yoffset=1.2

  i=21
  obs[i].name='MAXI'
  obs[i].ops=[2009,2020]
  obs[i].eng=[0.3,30] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].fcolor=!green
  obs[i].color=!p.color
  obs[i].xoffset=3.2

  i=64
  obs[i].name='ASTRO-H'
  obs[i].ops=[2014,2020]
  obs[i].eng=[0.3,600] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!cyan
  obs[i].color=!white
  obs[i].yoffset=2

  i=63
  obs[i].name='NuStar'
  obs[i].ops=[2012,2014]
  obs[i].ext_ops=2018
  obs[i].eng=[5,80] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!cyan
  obs[i].color=!white

  i=65
  obs[i].name='eRosita'
  obs[i].ops=[2013,2020]
  obs[i].eng=[0.5,10] ;; keV
  obs[i].point=1
  obs[i].survey=1
  obs[i].space=1
  obs[i].fcolor=!green
  obs[i].color=!white
  obs[i].yoffset=0.3

  i=4
  obs[i].name='XMM-Newton'
  obs[i].ops=[1999,2012]
  obs[i].ext_ops=2018
  obs[i].eng=[0.1,10]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!green
  obs[i].color=!white
  obs[i].yoffset=1.7
;  obs[i].xoffset=1.

  i=3
  obs[i].name='Integral'
  obs[i].ops=[2002,2012]
  obs[i].ext_ops=2012
  obs[i].eng=[15.,10e3]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!cyan
;  obs[i].xoffset=0.01
  obs[i].yoffset=0.12
  obs[i].color=!p.color

  i=67
  obs[i].name='GEMS'
  obs[i].ops=[2014,2016]
  obs[i].ext_ops=2016
  obs[i].eng=[2,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!green
  obs[i].color=!white
  obs[i].xoffset=0.1
  obs[i].yoffset=0.7

;  i=66
;  obs[i].name='NICER'
;  obs[i].ops=[2016,2018]
;  obs[i].ext_ops=2018
;  obs[i].eng=[0.2,12.]
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].fcolor=!green
;  obs[i].color=!white
;  obs[i].yoffset=1.2

  ;;; OPTICAL
  i=5
  obs[i].name='Pan-STARRS'
  obs[i].color=!black
  obs[i].ops=[2010,2020]
  obs[i].wavelength=[0.5,1.0]*1e-4 ;; cm
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!orange
  obs[i].yoffset=0.7
  obs[i].xoffset=1

  i=9
  obs[i].name='Swift-UVOT'
  obs[i].ops=[2004,2012]
  obs[i].ext_ops=2018
  obs[i].wavelength=[150.,650.]*1e-7 ;; nm->cm
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!orange
  obs[i].yoffset=2.5
  obs[i].xoffset=2
  obs[i].color=!p.color

;  i=22
;  obs[i].name='SVOM-VT'
;  obs[i].ops=[2016,2020]
;  obs[i].space=1
;  obs[i].wavelength=[400,950]*1e-7 ;; cm
;  obs[i].point=1
;  obs[i].fcolor=!orange
;  obs[i].yoffset=0.03
;  obs[i].color=!p.color

  i=11
  obs[i].name='Hubble'
  obs[i].ops=[1990,2012]
  obs[i].ext_ops=2018
  obs[i].wavelength=[300,1000]*1e-7
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!orange
  obs[i].yoffset=5
  obs[i].color=!p.color


  i=26
  obs[i].name='LSST'
  obs[i].ops=[2018,2020]
  obs[i].wavelength=[320,1080]*1e-7
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!orange

  i=40
  obs[i].name='SKYMAPPER'
  obs[i].color=!black
  obs[i].ops=[2008,2020]
  obs[i].wavelength=[300,950]*1e-7
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor=!orange
  obs[i].yoffset=0.6
;  obs[i].yoffset=0.02

  i=34
  obs[i].name='BOSS'
  obs[i].ops=[2009,2014]
  obs[i].ext_ops=2014
  obs[i].wavelength=[340,1060]*1e-7
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor=!orange
  obs[i].color=!p.color
  obs[i].yoffset=4.

  i=35
  obs[i].name='BigBOSS'
  obs[i].ops=[2016,2018]
  obs[i].wavelength=[340,1060]*1e-7
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor=!orange
  obs[i].color=!p.color
  obs[i].yoffset=5.

  i=29
  obs[i].name='Spitzer'
  obs[i].ops=[2003,2012]
  obs[i].ext_ops=2018
  obs[i].wavelength=[3,180]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!orange
  obs[i].color=!white
;  obs[i].yoffset=0.2

  i=42
  obs[i].name='Herschel'
  obs[i].ops=[2009.5,2012.5]
  obs[i].ext_ops=[2012.5]
  obs[i].wavelength=[55,672]*1e-4
;  obs[i].freq=[490,1910]*1e9
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!orange
  obs[i].color=!white
  obs[i].yoffset=1.1

  i=45
  obs[i].name='WISE'
  obs[i].ops=[2010,2012]
  obs[i].wavelength=[3,22]*1e-4
  obs[i].ext_ops=2012
  obs[i].point=1
  obs[i].space=1
  obs[i].fcolor=!orange
  obs[i].color=!white
;  obs[i].xoffset=-0.5
  obs[i].yoffset=0.7

  ;;;RADIO
  i=6
  obs[i].name='LOFAR'
  obs[i].ops=[2011,2020]
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].color=!p.color

  i=12
  obs[i].name='VLBA'
  obs[i].ops=[1993,2020]
  obs[i].freq=[0.312,90]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].color=!white

  i=13
  obs[i].name='EVLA'
  obs[i].ops=[2010,2020]
  obs[i].freq=[1.,50.]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].color=!white

  i=18
  obs[i].name='SKA'
  obs[i].ops=[2015,2020]
  obs[i].freq=[0.1,25]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor=!blue
  obs[i].color=!p.color
  obs[i].yoffset=0.09

  i=31
  obs[i].name='Apertif'
  obs[i].color=!black
  obs[i].ops=[2012,2020]
  obs[i].freq=[1.,1.7]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].yoffset=2.
  obs[i].color=!white

  i=32
  obs[i].name='MeerKAT'
  obs[i].ops=[2013,2020]
  obs[i].freq=[0.58,14.5]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor=!blue
  obs[i].color=!p.color
  obs[i].yoffset=1.2

  i=33
  obs[i].name='ASKAP'
;  obs[i].yoffset=0.1
  obs[i].xoffset=-0.6
  obs[i].ops=[2012,2020]
  obs[i].freq=[0.7,1.8]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].color=!p.color
  obs[i].color=!white

  i=37
  obs[i].name='ALMA'
  obs[i].ops=[2013,2020]
  obs[i].freq=[84,720]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].fcolor=!blue
  obs[i].color=!white

  i=43
  obs[i].name='Planck'
  obs[i].ops=[2009.5,2012]
  obs[i].ext_ops=2012
  obs[i].freq=[27,1000]*1d9 ;; Hz
  obs[i].survey=1
  obs[i].space=1
  obs[i].fcolor=!blue
  obs[i].color=!p.color
  
  ;;; MULTI-MESSENGER
  i=30
  obs[i].name='IceCube'
  obs[i].ops=[2012,2020]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].color=!white

  i=16
  obs[i].name='Auger'
  obs[i].ops=[2004,2020]
  obs[i].messenger='UHECRs'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].color=!white

  i=36

  obs[i].name='CALET'
  obs[i].ops=[2013,2020]
  obs[i].space=1
  obs[i].survey=1
  obs[i].messenger='electron'
  obs[i].color=!white

  i=38
  obs[i].name='ALIGO'
  obs[i].ops=[2015,2020]
  obs[i].messenger='GW'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].color=!p.color

  i=17
  obs[i].name='AMS'
  obs[i].ops=[2011,2020]
  obs[i].messenger='Cosmic Rays'
  obs[i].space=1
  obs[i].survey=1
  obs[i].color=!white
  
  i=15
  obs[i].name='ANTARES'
  obs[i].ops=[2008,2020]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].color=!white

;  i=23
;  obs[i].name='SVOM-GWAC'
;  obs[i].wavelength=[400,950]*1e-7
;  obs[i].survey=1
  
;  i=24
;  obs[i].name='SVOM-GFT-F'
;  obs[i].wavelength=[400,1700]*1e-7
;  obs[i].point=1
  
;  i=25
;  obs[i].name='SVOM-GFT-C'
;  obs[i].wavelength=[400,900]*1e-7
;  obs[i].point=1
 
;; SVOM
;  i=[19,20,42,22,23,24,25]
;  obs[i].ops=[2016,2020]
;  obs[i[0:3]].space=1
 ; obs[i[3:*]].ground=1



;;; lots of SKA pathfinders
;  i=34
;  obs[i].name='LWA'
;  obs[i].ops=

;  obs.eng=obs.eng*1e-3  ;;; convert from keV to meV
  obs.eng=obs.eng*1e3  ;;; convert from keV to eV
    
  w=where(obs.freq[0] ne 0)
  f=where(obs.wavelength[0] ne 0)
  en=where(obs.eng[0] ne 0)

  obs[w].wavelength=reverse(c/obs[w].freq)
  obs[w].eng=h*obs[w].freq

  obs[f].freq=reverse(c/obs[f].wavelength)
  obs[f].eng=h*obs[f].freq

  obs[en].freq=obs[en].eng/h
  obs[en].wavelength=reverse(h*c/obs[en].eng)

  ;;; set messenger at top
  m=where(obs.messenger ne '')

  obs[m].freq=[1e30,1e31]
  obs[m].wavelength=[1e4,1e5]
  obs[m].eng=[1e14,1d15] 

  w=where(obs.freq[0] ne 0)
;  obs=obs[w]

;  s=sort(obs.freq[1])
;  obs=obs[s]

  w=where(obs.ops[0] lt 2008)
  obs[w].ops[0]=2008.02

  w=where(obs.ops[1] gt 2018)
  obs[w].ops[1]=2017.98

  w=where(obs.ops[0] lt 2018)
  obs=obs[w]

;  obs[m].freq=[1e30,1e31]
;  obs[m].wavelength=[1e4,1e5]
;  obs[m].eng=[1e13,1d14]

  ;;;; freq or wavelength or energy????? also label
  ytitle='Energy (eV)'

  !y.margin=[0,5]
  !x.margin=[2,3]
  !p.charsize=1.
  !p.charthick=5.
;  title=['Survey Telescopes','Pointing Telescopes']
  s=where(obs.survey eq 1)
  p=where(obs.point eq 1)
  oobs=obs
;  multiplot,[1,2],/init
;  for k=0,1 do begin 
;     case k of
;        0: begin
;           obs=oobs[s]
;           yrange=[1e-14,1e10]*1e6
;        end 
;        1: begin
;           obs=oobs[p]
           yrange=[1e-14,1e10]*1e6
;        end 
;     endcase 
           obs=oobs
     tmp=execute('quan=obs.'+q)
     k=1
     if k eq 1 then xtitle='Year' else xtitle=''
     colprint,obs.name,obs.eng[0],obs.eng[1],obs.freq[0],obs.freq[1],obs.messenger
;     multiplot
     ytv=10.^[-12,-9,-6,-3,0,3,6,9,12]
     plot,[2008,2018],yrange,/ylog,/nodata,xtitle=xtitle,/ysty,/xsty,yticks=1,yminor=1,ytickname=[' ',' '],charsize=1.5

     axis,yaxis=0,/data,ytickv=ytv,ytitle=ytitle,/ysty,yticks=10,ytickformat='loglabels',charsize=1.5
     
     fr=[1e6,1e9,1e12,1e15,1e18,1e21,1e24,1e27];,1e30]
     en=h*fr
     axis,yaxis=1,/data,ytickv=en,yminor=0,ytickname='10!U'+ntostr(fix(alog10(fr)))+'!N',yticks=9,charsize=1.5

     xyouts,2018.7,1e9,'Frequency (Hz)',orientation=270,charsize=1.5

     color=[!darkred,!sienna,!salmon,!red,!orange,!yellow,!lightgreen,!seagreen,!green,!darkgreen,!forestgreen,!cyan,!turquoise,!skyblue,!dodgerblue,!blue,!darkblue,!navyblue,!magenta,!purple,!hotpink,!pink,!deeppink,!violet]
     mcolor=[!grey20,!grey40,!grey50,!grey60,!grey70,!grey90]

     spos=strpos(obs.name,'Fermi')
     fermi=intarr(70)
     wf=where(spos ne -1)
     fermi[wf]=1
     m=where(obs.messenger eq '',nm)
     for j=0,nm-1 do begin
        i=m[j]
;        x=[obs[i].ops[0],obs[i].ops[1],obs[i].ops[1],obs[i].ops[0],obs[i].ops[0]]
;        y=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]
        ex=[obs[i].ops[1],obs[i].ext_ops,obs[i].ext_ops,obs[i].ops[1]]
        ey=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]
;        color=obs[i].fcolor
        if obs[i].survey and not obs[i].point then begin
           fcolor=!green
        endif 
        if not obs[i].survey and obs[i].point then begin
           fcolor=!blue
        endif 
        if fermi[i] then begin
           fcolor=!red
        endif
        curved_polygon,obs[i].ops[0],obs[i].ops[1],quan[0,i],quan[1,i],x,y,color=fcolor
        if min(ex) ge 2008 then polyfill,ex,ey,color=fcolor,line_fill=1.,orientation=45.
        oplot,ex,ey,line=0,color=fcolor
        oplot,x,y
     endfor 
     for j=0,nm-1 do begin 
        i=m[j]
;        x=[obs[i].ops[0],obs[i].ops[1],obs[i].ops[1],obs[i].ops[0],obs[i].ops[0]]
;        y=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]
;        oplot,x,y,thick=5
        curved_polygon,obs[i].ops[0],obs[i].ops[1],quan[0,i],quan[1,i],x,y,/noplot

;        ex=[obs[i].ops[1],obs[i].ext_ops,obs[i].ext_ops,obs[i].ops[1]]
;        ey=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]
;        if fermi[i] then 
;        oplot,ex,ey,line=0,color=fcolor
        mfreq=10^mean(alog10(quan[*,i]))*obs[i].yoffset*1.1
        if obs[i].survey and not obs[i].point then begin
           fcolor=!green
        endif 
        if not obs[i].survey and obs[i].point then begin
           fcolor=!blue
        endif 
        if fermi[i] then begin
           fcolor=!red
        endif 
        color=obs[i].color
        xyouts,obs[i].ops[0]+obs[i].xoffset,mfreq,obs[i].name,color=color,charsize=obs[i].charsize,charthick=3
     endfor 

     m=where(obs.messenger ne '',nm)
     
     for j=0,nm-1 do begin
        i=m[j]
        print,quan[0,i];*10^(j+1d)
;        x=[obs[i].ops[0],obs[i].ops[1],obs[i].ops[1],obs[i].ops[0],obs[i].ops[0]]
;        y=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]*10^(j+1)
        curved_polygon,obs[i].ops[0],obs[i].ops[1],quan[0,i]*10.^(j+1d),quan[1,i]*10.^(j+1d),x,y,color=mcolor[j]
;        polyfill,x,y,color=mcolor[j]
        ex=[obs[i].ops[1],obs[i].ext_ops,obs[i].ext_ops,obs[i].ops[1]]
;        oplot,ex,y,line=2,color=mcolor[j]
;        oplot,x,y,thick=5
     endfor 
     for j=0,nm-1 do begin 
        i=m[j]
;        x=[obs[i].ops[0],obs[i].ops[1],obs[i].ops[1],obs[i].ops[0],obs[i].ops[0]]
;        y=[quan[0,i],quan[0,i],quan[1,i],quan[1,i],quan[0,i]]*10^(j+1)
;        oplot,x,y,thick=5
;        curved_polygon,obs[i].ops[0],obs[i].ops[1],quan[0,i]*10^(j+1),quan[1,i]*10^(j+1),x,y
        mfreq=quan[0,i]*10^(j+1d)*1.5
        xyouts,obs[i].ops[0]+0.1,mfreq,obs[i].name+' ('+obs[i].messenger+')',color=obs[i].color,charsize=obs[i].charsize,charthick=3
        print,obs[i].name,mfreq,quan[0,i],j
     endfor 

;     if k eq 0 then ts=1e5 else ts=0.005
;     xyouts,2012,yrange[1]*ts,title[k],charthick=15,charsize=1.2

     oplot,[2018,2018],[1e-11,1e16]
;     if not keyword_set(ps) then skip=get_kbrd(10)
;  endfor 
;  multiplot,/reset,/default
;  if keyword_set(ps) then 

     xyouts,2008,1e20,'Survey',color=!green,/data,charsize=2.
     xyouts,2008,5e18,'Pointing',color=!blue,/data,charsize=2.
     endplot

     spawn,'convert ~/Fermi/Senior_Review/missions'+add+add2+'.eps ~/Fermi/Senior_Review/missions'+add+add2+'.pdf'
    

stop
  return
end 
