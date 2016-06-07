@fit_functions
@fit_functions_flares
pro histoit

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)
  
  ng=n_elements(g)
  f1=fltarr(ng) & f2=f1

;  t=[50e3,35e3]
  t=86400.

  for i=0,ng-1 do begin
     f1[i]=call_function(strtrim(g[i].model,2),t[0],g[i].p)
;     f2[i]=call_function(strtrim(g[i].model,2),t[1],g[i].p)
  endfor 

  w1=where(f1 gt 1e-5 and f1 lt 1e8 and g.tstop gt t[0] and g.model ne '' and g.unabs_cfratio ne 0)
  f1=f1[w1]*g[w1].unabs_cfratio
;  w2=where(f2 gt 1e-5 and f2 lt 1e8 and g.tstop gt t[1])
;  f2=f2[w2]

  plothist,alog10(f1),bin=0.1,xtitle='Flux @'+ntostr(t[0],2)+' ks';,xtickname=['10!U'+ntostr(indgen(5)-5)+'!N','1','10']



  goto,latskip
  gbm1=where(strtrim(g[w1].gbmname,2) ne '')
  gbm2=where(strtrim(g[w2].gbmname,2) ne '')
  
  lat=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090531B','GRB090902B','GRB090926A','GRB091003','GRB091208B','GRB100414A','GRB100728A','GRB110625A','GRB110709A','GRB110731A','GRB120624B','GRB120711A','GRB121011A','GRB130305A','GRB130427A','GRB130502B','GRB130504C','GRB130518A','GRB130606B','GRB130702A','GRB130907A','GRB131014A','GRB131108A','GRB131231A','GRB140102A','GRB140323A']
  match,strtrim(g[w1].grb,2),lat,m1,m2
  lat1=w1[m1]
  match,strtrim(g[w2].grb,2),lat,m1,m2
  lat2=w2[m1]

  flu1=where(g[w1].fluence gt 4.45e-6,ng)
  flu2=where(g[w2].fluence gt 4.45e-6,ng)

  begplot,name='~/Swift/xrt_stats_lat_times.ps',/land,/color;,font='helvetica'
  !p.multi=[0,1,2]
  plothist,alog10(f1),bin=0.1,xtitle='Count Rate @'+ntostr(t[0],2)+' ks',xtickname=['10!U'+ntostr(indgen(5)-5)+'!N','1','10']
  plothist,alog10(f1[gbm1]),bin=0.1,color=!green,/overplot  
  plothist,alog10(f1[lat1]),bin=0.1,color=!magenta,/overplot
  plothist,alog10(f1[flu1]),bin=0.1,color=!blue,/overplot
  oplot,[-2,-2],[0,50],line=1
  xyouts,-1.9,45,'10 counts in 1 ks exposure',/data,charsize=1

  legend,['All Swift','GBM','LAT','BAT High Fluence'],box=0,/top,/left,textcolor=[!p.color,!green,!magenta,!blue]

  plothist,alog10(f2),bin=0.1,xtitle='Count Rate @'+ntostr(t[1],2)+' ks',xtickname=['10!U'+ntostr(indgen(5)-5)+'!N','1','10']
  plothist,alog10(f2[gbm2]),bin=0.1,color=!green,/overplot
  plothist,alog10(f2[lat2]),bin=0.1,color=!magenta,/overplot
  plothist,alog10(f2[flu2]),bin=0.1,color=!blue,/overplot
  oplot,[-2,-2],[0,50],line=1
  xyouts,-1.9,45,'10 counts in 1 ks exposure',/data,charsize=1
  endplot
  
  !p.multi=0
  spawn,'ps2pdf ~/Swift/xrt_stats_lat_times.ps ~/Swift/xrt_stats_lat_times.pdf'

  latskip:
  stop


  return
end 

pro gbm,rate=rate,flux=flux

;;   readcol,'~/Swift/grb_table_1382922859.txt',grb,trigtime,trignum,batra,batdec,batt90,batfluence,xrtra,xrtdec,xrttime,xrtflux,uvotra,uvotdec,uvottime,uvotmag,format='(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',delim='  ',skip=1
  
;;   ns=n_elements(grb)
;;   swift=create_struct('grb','','trigtime','','met',0d,'t90',0.,'xrtdet',0,'uvotdet',0,'redshift',0.,'gbmname','')
;;   swift=replicate(swift,ns)

;;   swift.grb=grb
;;   swift.trigtime=trigtime
;;   year=strmid(swift.grb,0,2)
;;   month=strmid(swift.grb,2,2)
;;   day=strmid(swift.grb,4,2)
;;   for i=0,ns-1 do begin
;;      swift[i].met=date2met('20'+year[i]+'-'+month[i]+'-'+day[i]+'-'+swift[i].trigtime)
;;   endfor 
;; ;  swift.trignum=long(trignum)
;; ;  swift.t90=batt90
;;   w=where(strtrim(xrtra,2) ne 'n/a')
;;   swift[w].xrtdet=1
;;   w=where(strtrim(uvotra,2) ne 'n/a')
;;   swift[w].uvotdet=1
;; ;  w=where(strtrim(redshift,2) ne 'n/a')
;; ;  swift[w].redshift=float(redshift[w])
;;   ns=485
;;   swift=swift[1:ns-1] ;;; cutting only overlap with fermi
;;   ns=ns-1
;;   s=ns-indgen(ns)
;;   swift=swift[s]

  ;; gbm=mrdfits('~/Swift/GBM_burstcat.fits',1)
  ;; s=sort(gbm.name)
  ;; gbm=gbm[s]
  ;; ng=n_elements(gbm)
  ;; for i=0,ng-1 do begin 
  ;;    v=date_conv(gbm[i].trigger_time+2400000.5,'V')
  ;;    utc=ntostr(v[0],4)+'-'+ntostr(fix(v[1]))+'-'+ntostr(v[2],2)+':'+ntostr(v[3],2)+':'+ntostr(v[4],4)
  ;;    met=date2met(utc)
  ;;    gbm[i].trigger_time=met
  ;; endfor 

  ;; for i=0,ns-1 do begin
  ;;    d=abs(swift[i].met-gbm.trigger_time)
    
  ;;    m=min(d,wm)
  ;;    if d[wm] lt 300 then swift[i].gbmname=gbm[wm].name

  ;; endfor 

  ;; update via collect_grb_properties
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  w=where(strtrim(g.gbmname,2) ne '')

  plot_all_xrt_lcs,overgrb=strtrim(g[w].grb,2),/nocolor,add='gbm_',overcolor='!grey30',rate=rate,flux=flux,/noleg
  

  return
end 

pro short_long,flux=flux,rate=rate,lum=lum
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

;  l=where(g.t90 ge 2.)
;  s=where(g.t90 le 2. and g.t90 ne 0)
  sgrb=mrdfits('~/Swift/jochen_z_list.fits',1)

  match,strtrim(sgrb.grb,2),strtrim(g.grb,2),m1,m2
  help,g,sgrb
  help,m1,m2

  s=where(sgrb[m1].shb eq 1)
  s=m2[s]
  l=where(sgrb[m1].shb eq 0)
  l=m2[l]

  plot_all_xrt_lcs,overgrb=strtrim(g[s].grb),/noleg,overcolor='!grey30',add='short_long_',flux=flux,rate=rate,lum=lum

return
end

pro lat_other,lum=lum,flux=flux,rate=rate

  if keyword_set(lum) then begin
     lat=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB091208B','GRB100414A','GRB100728A','GRB110731A','GRB120711A','GRB130427A','GRB130518A','GRB130702A','GRB130907A','GRB131108A','GRB131231A','GRB141028A','GRB150314A','GRB150403A']
     z=[4.35,3.57,0.73,0.903,1.822,2.1062,0.8969,1.0633,1.368,1.567,2.83,1.405,0.34,2.49,0.145,1.2,2.4,0.642,2.33,1.758,2.06]
     plot_all_xrt_lcs,/lum,overgrb=lat,z=z,add='lat_',/overdata
  endif else begin 
     lat=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090531B','GRB090902B','GRB090926A','GRB091003','GRB091208B','GRB100414A','GRB100728A','GRB110625A','GRB110709A','GRB110731A','GRB120624B','GRB120711A','GRB121011A','GRB130305A','GRB130427A','GRB130502B','GRB130504C','GRB130518A','GRB130606B','GRB130702A','GRB130907A','GRB131014A','GRB131108A','GRB131231A','GRB140102A','GRB140323A','GRB140928A','GRB141028A','GRB150202B','GRB150314A','GRB150403A','GRB150514A','GRB150523A','GRB150627A','GRB150724B','GRB150902A','GRB151006A','GRB160325A','GRB160422A','GRB160509A']
     overgcom='print,p'
     overcolor='[!blue'
     for i=1,n_elements(lat)-2 do overcolor=overcolor+',!blue'
     overcolor=overcolor+',!red]'

  tstart=fltarr(n_elements(lat))
  for i=0,n_elements(lat)-1 do begin
     lc=lcout2fits(dir=lat[i])
     tstart[i]=lc[0].tstart
  endfor 

     plot_all_xrt_lcs,flux=flux,rate=rate,overgrb=lat,add='lat_',/overdata,xrange=[40,4e7],overgcom=overgcom,overcolor=overcolor
  endelse

  ;; LUM with z

  ;; need to separate out LLE only, with/without redshift
  ;;; then plot all w/ flux diff color for lle_only/lat, lum with redshift

  return
end 

pro plot_all_xrt_lcs,t,val,rate=rate,flux=flux,lum=lum,data=data,overgrb=overgrb,z=z,add=add,nocolor=nocolor,overcolor=overcolor,overdata=overdata,basemodel=basemodel,noleg=noleg,dir=dir,overcom=overcom,overmodel=overmodel,lastthick=lastthick,nounder=nounder,overgcom=overgcom,xrange=xrange,ldens=ldens

  if not keyword_set(rate) and not keyword_set(flux) and not keyword_set(lum) and not keyword_set(ldens) then begin
     print,'syntax - plot_all_xrt_lcs,rate=rate,flux=flux,lum=lum,ldens=ldens'
     print,'         NEED TO SPECIFY'
     return
  end 

  cd,'~/GRBs/'  
  grbcat=mrdfits('~/Swift/swift_grb_properties.fits',1)
;  g=mrdfits('~/Swift/swiftgrb.fits',1)
;  w=where(g.bat_t90 gt 2.) ;;; cutting out short bursts
;  g=g[w]
;  match,strtrim(grbcat.grb,2),strtrim(g.name,2),m1,m2
;  t90=g[m2].bat_t90
;  w=where(t90 gt 2)  
;  grbcat=grbcat[m1]
  ngrbs=n_elements(grbcat)
  wz=where(grbcat.z ne 0,nz)
  mpc2cm=3.08568025d24 
  if n_elements(xrange) eq 0 then xrange=[10,1e7]
  xtitle='Time since trigger (s)'
  if n_elements(add) eq 0 then add=''
  if keyword_set(rate) then begin
     yrange=[1e-4,1e4]
     ytitle='Count Rate (cts s!U-1!N)'
     add=add+'rate'
     yrange2=[-5,-1]
     bin=0.2
  endif 
  if keyword_set(flux) then begin
     yrange=[1e-15,1e-6]
     ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)'
     add=add+'flux'
     yrange2=[-16,-12]
     bin=0.2
  endif 
  if keyword_set(lum) then begin 
     yrange=[1d42,1d52]
     ytitle='Luminosity!L0.3-10 keV!N (erg s!U-1!N)'
     xtitle='Time (s) / (1+z)'
     add=add+'lum'
     grbcat=grbcat[wz]
     ngrbs=nz
     yrange2=[40,48]
     bin=0.5
  endif 
  if keyword_set(ldens) then begin 
     yrange=[1d24,1d34]
     ytitle='Luminosity [1 keV] (erg s!U-1!N Hz!U-1!N)'
     xtitle='Time (s) / (1+z)'
     add=add+'ldens'
     grbcat=grbcat[wz]
     ngrbs=nz
     yrange2=[25,33]
     bin=0.5
  endif 

  if keyword_set(data) then add=add+'_data'
  
  if n_elements(dir) eq 0 then dir='~/GRBs/'

  if n_elements(val) eq 0 then begin 
     begplot,name=dir+'all_xrt_lc_'+add+'.ps',/land,/color;,font='helvetica'
     if n_elements(overgrb) eq 0 and not keyword_set(nocolor) then loadct,39
     !x.margin=[4,2]
     !y.margin=[3,1]
     plot,xrange,yrange,/xlog,/ylog,xtitle=xtitle,ytitle=ytitle,/nodata,/ysty,/xsty,xtickformat='loglabels',ytickformat='loglabels',charsize=2,xminor=9
     val=dblarr(ngrbs)
     h=intarr(ngrbs)
     if not keyword_set(nounder) then begin 
        for i=0,ngrbs-1 do begin
           skip=0
           z1=1.
           grb=strtrim(grbcat[i].grb,2)
           print,grb
           if keyword_set(rate) then f=1.
           if keyword_set(flux) or keyword_set(lum) or keyword_set(ldens) then begin
              if exist(grb+'/UL_specfits.fits') then begin 
                 spec=mrdfits(grb+'/UL_specfits.fits',1)
                 ws=n_elements(spec)-1
                 f=spec[ws].unabs_cfratio
              endif else skip=1
           endif 
           if keyword_set(lum) or keyword_set(ldens) then begin
              dist=lumdist(grbcat[i].z)*mpc2cm
              f=f*4.*!pi*dist^2*(1.+grbcat[i].z)^(spec[ws].phind-1.-1.)
              z1=1./(1+grbcat[i].z)
              if keyword_set(ldens) then begin
                 fdens=flux2jy(1.,grbcat[i].phind)*1d-23
                 f=f*fdens
              endif 
;        g[i].flux_avg*4.*!pi*dist^2*(1.+g[i].z)^(g[i].beta-1.)
           endif 
;        if skip eq 0 then begin 
           bdir='~/GRBs/'+grb+'/'
;           lcfile='lc_newout_phil2.txt'
;           if not exist(dir+lcfile) then lcfile='lc_newout_phil.txt'
;           lc=lcout2fits(dir=bdir)
;           lc=lcout2fits('~/GRBs/'+grb+'/lc_newout_phil.txt') ;
           lc=mrdfits(bdir+'/UL_lc.fits',1)
           wdet=where(lc.src_rate_err gt 0 and lc.time gt 0)
           lc=lc[wdet]
           if not keyword_set(data) then begin 
              lcfit='~/GRBs/'+grb+'/lc_fit_out_idl_int9.dat'
;              if not exist(lcfit) then lcfit='~/GRBs/'+grb+'/lc_fit_out_idl_int7.dat'
              read_lcfit,lcfit,pnames,p,perror
              mo=fit_models(pnames,p,np,nf,basemo=basemo,breaks=breaks)
              if keyword_set(basemodel) then mo=basemo
              if breaks[0] ne 0 then time=[lc.time,p[breaks]] else time=lc.time
              time=[time,time*2]
              wt=where(time le max(lc.tstop))
              time=time[wt]
              time=time[sort(time)]
              tmp=execute('yfit='+mo+'(time,p)')                
              tmp=execute('yfit200='+mo+'(200./z1,p)')
              if keyword_set(lum) then h[i]=round((alog10(f*yfit200)-42.5)*31.)
              if keyword_set(ldens) then h[i]=round((alog10(f*yfit200)-24.5)*31.)

;           tmp=execute('color=!grey'+ntostr(h))
;           print,h
              if n_elements(time) gt 1 then begin 
                 if n_elements(overgrb) eq 0 and not keyword_set(nocolor) then $
                    oplot,time*z1,yfit*f,color=h[i] else $
                       oplot,time*z1,yfit*f,color=!grey70
;           if n_elements(t) ne 0 and max(lc.tstop) gt t then 
                 if n_elements(t) gt 0 then begin
                    if max(lc.time*z1) gt t then tmp=execute('val[i]='+mo+'(t*z1,p)*f')
                 endif 
              endif 
           endif else begin
;              oploterror,lc.time*z1,lc.src_rate*f,lc.src_rate_err*f,/nohat,errcolor=!grey50,color=!grey50,psym=3
;           if nwdet gt 0 then plots,lc[nwdet].time*z1,lc[nwdet].src_rate*f,color=!grey70,psym=8
              for q=0,n_elements(lc)-1 do begin
                 oplot,[lc[q].tstart,lc[q].tstop],[lc[q].src_rate,lc[q].src_rate],color=!grey70
                 oplot,[lc[q].time,lc[q].time]*z1,[lc[q].src_rate-lc[q].src_rate_err,lc[q].src_rate+lc[q].src_rate_err]*f,color=!grey70

;colprint,lc[q].tstart,lc[q].tstop
              endfor 
;              weird=where(lc.time lt 100 and lc.src_rate lt 1e-2,nweird)
;              if nweird gt 0 then stop
           endelse 
;        endif 

        endfor
     endif 
  endif        

  if n_elements(overgrb) gt 0 then begin
     novergrb=n_elements(overgrb)
     zlist=fltarr(novergrb)
     if novergrb ne 0 then begin 
        if n_elements(overcolor) eq 0 then begin 
           color=[!LightSteelBlue,!SkyBlue,!blue,!sienna,!green,!orange,!cyan,!magenta,!salmon,!darkgreen,!violet,!navyblue,!red,!royalblue,!turquoise,!forestgreen,!sienna,!seagreen,!deeppink,!purple,!violet,!pink,!hotpink,!firebrick,!darkblue,!navyblue,!orangered,!darkred,!midnightblue]
           color=[color,color,color]
        endif else begin
           tmp=execute('ocolor='+overcolor)
           if n_elements(ocolor) eq 1 then color=replicate(ocolor,novergrb) else color=ocolor
        endelse 
        for i=0,novergrb-1 do begin 
           print,overgrb[i],color[i],i
           lc=mrdfits('~/GRBs/'+overgrb[i]+'/UL_lc.fits',1)

           nwdet=where(lc.src_rate_err eq 0,nnwdet)
           if keyword_set(rate) then f=1.
           if keyword_set(flux) or keyword_set(lum) or keyword_set(ldens) then begin
              if exist(overgrb[i]+'/UL_specfits.fits') then begin 
                 spec=mrdfits(overgrb[i]+'/UL_specfits.fits',1,/silent)
                 ws=n_elements(spec)-1
                 f=spec[ws].unabs_cfratio
                 if n_elements(z) gt 0 then zlist[i]=z[i] else zlist[i]=spec[0].z
              endif else skip=1
           endif 
           if keyword_set(lum) or keyword_set(ldens) then begin
              dist=lumdist(zlist[i])*mpc2cm
              f=f*4.*!pi*dist^2*(1.+zlist[i])^(spec[ws].phind-1.-1.)
              z1=1./(1+zlist[i])
              if keyword_set(ldens) then begin
                 fdens=flux2jy(1.,grbcat[i].phind)*1d-23
                 f=f*fdens
              endif 
           endif else z1=1.
           plotsym,1,4,thick=5
           lcfit='~/GRBs/'+overgrb[i]+'/lc_fit_out_idl_int9.dat'
           if exist(lcfit) then begin 
              read_lcfit,lcfit,pnames,p,perror
              if n_elements(pnames) gt 1 then begin 
                 mo=fit_models(pnames,p,np,nf,basemo=basemo,breaks=breaks)
                 if keyword_set(basemodel) then mo=basemo
                 if breaks[0] ne 0 then time=[lc.time,p[breaks]] else time=lc.time
                 time=[time,time*2]
                 wt=where(time le max(lc.time))
                 time=time[wt]
                 time=time[sort(time)]
                 tmp=execute('yfit='+mo+'(time,p)')                
                 if i eq novergrb-1 and keyword_set(lastthick) then thick=10 else thick=5 ;;; temporary for special case
                 oplot,time*z1,yfit*f,color=color[i],thick=thick
              endif 
           endif 
;              endif
           if keyword_set(overdata) then begin 
              if nnwdet gt 0 then begin 
                 plotsym,1,2,thick=5
                 plots,lc[nwdet].time*z1,lc[nwdet].src_rate*f,color=color[i],psym=8
              endif 
              for q=0,n_elements(lc)-1 do begin 
                 oplot,[lc[q].tstart,lc[q].tstop]*z1,[lc[q].src_rate,lc[q].src_rate]*f,color=color[i]
                 oplot,[lc[q].time,lc[q].time]*z1,[lc[q].src_rate-lc[q].src_rate_err,lc[q].src_rate+lc[q].src_rate_err]*f,color=color[i]
              endfor 
              wc=where(lc.type eq 2,nwc)
              if nwc gt 0 then begin 
                 plotsym,0,1,/fill
                 plots,lc[wc].time*z1,lc[wc].src_rate*f,color=color[i],psym=8
              endif 
           endif 
           if n_elements(overgcom) gt 0 then begin
              for j=0,n_elements(overgcom)-1 do tmp=execute(overgcom[j])
              
           endif 

;        oplot,lc.time*z1,lc.src_rate*f,color=color[i],thick=2
        endfor
        if not keyword_set(noleg) then begin
           if keyword_set(lum) or keyword_set(ldens) then legend,['All GRBs',overgrb+' (z='+numdec(zlist,2)+')'],textcolor=[!grey50,color[0:novergrb-1]],box=0,/top,/right,charsize=1.0 else begin
              if n_elements(overgrb) lt 30 then begin 
                 legend,['All GRBs',overgrb],textcolor=[!grey50,color[0:novergrb-1]],box=0,/top,/right,charsize=1.0
              endif else begin 
                 legend,['All GRBs',overgrb[0:19]],textcolor=[!grey50,color[0:19]],box=0,/top,/right,charsize=1.0,position=[0.83,0.95],/normal
                 legend,[overgrb[20:*]],textcolor=[color[20:novergrb-1]],box=0,/top,/right,charsize=1.0
              endelse 
           endelse 
        endif 
     endif 
  endif 

  if n_elements(overcom) gt 0 then begin
     for i=0,n_elements(overcom)-1 do tmp=execute(overcom[i])
  endif 


  endplot
  spawn,'ps2pdf '+dir+'/all_xrt_lc_'+add+'.ps  '+dir+'/all_xrt_lc_'+add+'.pdf'

;     stop

  return
end 
