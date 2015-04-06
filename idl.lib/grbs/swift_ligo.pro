@fit_functions
@fit_functions_flares
pro swift_ligo

;  read_swift_grb_table
  grbs=mrdfits('~/Swift/swiftgrb.fits',1)

  s=where(grbs.bat_t90 lt 2. and grbs.bat_detection eq 'Y' and grbs.bat_t90 ne 0.,ns)
  x=where(grbs[s].xrt_detection eq 'Y',nx)
  z=where(grbs[s].redshift ne 0,nz)
  d=where(grbs[s].xrt_detection ne 'Y' and grbs[s].xrt_onsource ne 0 and grbs[s].xrt_onsource lt 500.,nd)

  print,'BAT Short bursts:  ',ns
  print,'  XRT detections:  ',nx
  print,'       Redshifts:  ',nz
  print,'Why Non-detection: ',nd
  print
colprint,grbs[s[d]].name,grbs[s[d]].xrt_onsource

  cd,'~/GRBs'
  
  begplot,name='~/Swift/LIGO/shb_plots.ps',/color,font='helvetica'
  !p.charsize=1.
;  !x.margin=[10,3]
;  !y.margin=[4,2]
  ;;count rate
  plotsym,1,5,thick=4
  !p.multi=[0,1,2]
  xrange=[1d-5,100]
  plot,[10,1e6],[1e-5,1e2],xrange=[10,1e7],yrange=xrange,/xlog,/ylog,ytitle='Count Rate (0.3-10 keV) [counts s!U-1!N]',/nodata,xtitle='Time (s)',/ysty
  color=[!sienna,!red,!orangered,!orange,!salmon,!yellow,!seagreen,!green,!forestgreen,!skyblue,!cyan,!blue,!navyblue,!magenta,!purple]
  color=[color,color,color]
  j=0
  f30=dblarr(nx)
  t30=30.*60d
  f1=f30
  t1=86400d
  for i=0,ns-1 do begin
     grb=strcompress(grbs[s[i]].name,/rem)
     print,grb
     if exist(grb) then begin 
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits(/phil)
           r=1.
           oploterror,lc.time,lc.src_rate*r,lc.src_rate_err*r,psym=3,/nohat,errcolor=color[j]
           for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop],[lc[q].src_rate,lc[q].src_rate]*r,color=color[j]
           ul=where(lc.src_rate_err eq 0,nul)
           if nul gt 0 then plots,lc[ul].time,lc[ul].src_rate*r,psym=8,color=color[j]
           j=j+1
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           if pnames[0] ne 'nofit' then begin 
              mo=fit_models(pnames,p)
              if max(lc.tstop) gt t30 then tmp=execute('f30[i]='+mo+'(t30,p)')
              if max(lc.tstop) gt t1 then tmp=execute('f1[i]='+mo+'(t1,p)')
           endif 
        endif
        cd,'..'
     endif
  endfor 
  oplot,[t30,t30],[1d-20,1d50],line=2
  oplot,[t1,t1],[1d-20,1d50],line=2,color=!red

  w=where(f30 ne 0)
  plothist,alog10(f30[w]),bin=0.5,xrange=alog10(xrange),yrange=[0,4],xtitle='Count Rate',ytitle='N',/fill
  w=where(f1 ne 0)
  plothist,alog10(f1[w]),bin=0.5,color=!red,/over,xrange=alog10(xrange),yrange=[0,4],/fill,forient=45,fcolor=!red,/fline
  legend,['@30 min','@1 day'],/top,/left,textcolor=[!p.color,!red],box=0
  

  ;; flux
  !x.margin=[10,3]
  !y.margin=[4,2]

  xrange=[1d-15,1d-8]
  plot,[10,1e6],[1e-5,1e2],xrange=[10,1e7],yrange=xrange,/xlog,/ylog,ytitle='Flux (0.3-10 keV) [erg cm!U-2!N s!U-1!N]',/nodata,xtitle='Time (s)'
  j=0  
  f30=dblarr(nx)
  t30=30.*60d
  f1=f30
  t1=86400d

  for i=0,ns-1 do begin
     grb=strcompress(grbs[s[i]].name,/rem)
     print,grb
     if exist(grb) then begin 
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits(/phil)
           spec=mrdfits('UL_specfits.fits',1)
           r=spec[0].unabs_cfratio
           oploterror,lc.time,lc.src_rate*r,lc.src_rate_err*r,psym=3,/nohat,errcolor=color[j]
           for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop],[lc[q].src_rate,lc[q].src_rate]*r,color=color[j]
           ul=where(lc.src_rate_err eq 0,nul)
           if nul gt 0 then plots,lc[ul].time,lc[ul].src_rate*r,psym=8,color=color[j]
           j=j+1
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           if pnames[0] ne 'nofit' then begin 
              mo=fit_models(pnames,p)
              if max(lc.tstop) gt t30 then tmp=execute('f30[i]='+mo+'(t30,p)*r')
              if max(lc.tstop) gt t1 then tmp=execute('f1[i]='+mo+'(t1,p)*r')
           endif 
        endif
        cd,'..'
     endif
  endfor 
  oplot,[t30,t30],[1d-20,1d50],line=2
  oplot,[t1,t1],[1d-20,1d50],line=2,color=!red
  w=where(f30 ne 0)
  plothist,alog10(f30[w]),bin=0.5,xrange=alog10(xrange),yrange=[0,6],xtitle='Flux',ytitle='N',/fill
  w=where(f1 ne 0)
  plothist,alog10(f1[w]),bin=0.5,color=!red,/over,xrange=alog10(xrange),yrange=[0,6],/fill,forient=45,fcolor=!red,/fline
  legend,['@30 min','@1 day'],/top,/left,textcolor=[!p.color,!red],box=0

  ;; luminosity
  mpc2cm=3.08568025d24
  
  xrange=[1d40,1d50]
  plot,[10,1e6],[1e-5,1e2],xrange=[10,1d7],yrange=xrange,/xlog,/ylog,ytitle='Luminosity (0.3-10 keV) [erg s!U-1!N]',/nodata,xtitle='Time/(1+z) [s]'
  j=0
  f30=dblarr(nz)
  f1=f30

  for i=0,nz-1 do begin
     grb=strcompress(grbs[s[z[i]]].name,/rem)
     print,grb
     if exist(grb) then begin 
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits(/phil)
           spec=mrdfits('UL_specfits.fits',1)
           dist=lumdist(grbs[s[z[i]]].redshift,h0=71,lambda=0.73,omega_m=0.27)
           dist=dist*mpc2cm
           r=spec[0].unabs_cfratio*4.*!pi*dist^2
           z1=1.+grbs[s[z[i]]].redshift
           oploterror,lc.time/z1,lc.src_rate*r,lc.src_rate_err*r,psym=3,/nohat,errcolor=color[j]
           for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop]/z1,[lc[q].src_rate,lc[q].src_rate]*r,color=color[j]
           ul=where(lc.src_rate_err eq 0,nul)
           if nul gt 0 then plots,lc[ul].time/z1,lc[ul].src_rate*r,psym=8,color=color[j]
           j=j+1
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           if pnames[0] ne 'nofit' then begin 
              mo=fit_models(pnames,p)

              if max(lc.tstop) gt t30 then tmp=execute('f30[i]='+mo+'(t30*z1,p)*r')
              if max(lc.tstop) gt t1 then tmp=execute('f1[i]='+mo+'(t1*z1,p)*r')
           endif 
        endif
        cd,'..'
     endif
  endfor 
  oplot,[t30,t30],[1d-20,1d50],line=2
  oplot,[t1,t1],[1d-20,1d50],line=2,color=!red
  w=where(f30 ne 0)
  plothist,alog10(f30[w]),bin=0.5,xrange=alog10(xrange),yrange=[0,4],xtitle='Luminosity',ytitle='N',/fill
  w=where(f1 ne 0)
  plothist,alog10(f1[w]),bin=0.5,color=!red,/over,xrange=xrange,yrange=[0,4],/fill,forient=45,fcolor=!red,/fline
  legend,['@30 min','@1 day'],/top,/left,textcolor=[!p.color,!red],box=0
  

  ;; Flux @ 200 Mpc
  dist200=200*mpc2cm

  xrange=[1d-14,1d-5]
  plot,[10,1e6],[1e-5,1e2],xrange=[10,1e7],yrange=xrange,/xlog,/ylog,ytitle='Flux @200 Mpc (0.3-10 keV) [erg cm!U-2!N s!U-1!N]',/nodata,xtitle='Time/(1+z) [s]'
  j=0
  f30=dblarr(nz)
  t30=30.*60d
  f1=f30
  t1=86400d

  for i=0,nz-1 do begin
     grb=strcompress(grbs[s[z[i]]].name,/rem)
     print,grb
     if exist(grb) then begin 
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits(/phil)
           spec=mrdfits('UL_specfits.fits',1)
           dist=lumdist(grbs[s[z[i]]].redshift,h0=71,lambda=0.73,omega_m=0.27)
           dist=dist*mpc2cm
           z1=1.+grbs[s[z[i]]].redshift
           r=spec[0].unabs_cfratio*dist^2/dist200^2
           oploterror,lc.time/z1,lc.src_rate*r,lc.src_rate_err*r,psym=3,/nohat,errcolor=color[j]
           for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop]/z1,[lc[q].src_rate,lc[q].src_rate]*r,color=color[j]
           ul=where(lc.src_rate_err eq 0,nul)
           if nul gt 0 then plots,lc[ul].time/z1,lc[ul].src_rate*r,psym=8,color=color[j]
           j=j+1
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           if pnames[0] ne 'nofit' then begin 
              mo=fit_models(pnames,p)

              if max(lc.tstop) gt t30 then tmp=execute('f30[i]='+mo+'(t30*z1,p)*r')
              if max(lc.tstop) gt t1 then tmp=execute('f1[i]='+mo+'(t1*z1,p)*r')
           endif 
        endif
        cd,'..'
     endif
  endfor 
  oplot,[t30,t30],[1d-20,1d50],line=2
  oplot,[t1,t1],[1d-20,1d50],line=2,color=!red
  w=where(f30 ne 0)
  plothist,alog10(f30[w]),bin=0.5,xrange=alog10(xrange),yrange=[0,4],xtitle='Flux',ytitle='N',/fill
  w=where(f1 ne 0)
  plothist,alog10(f1[w]),bin=0.5,color=!red,/over,xrange=alog10(xrange),yrange=[0,4],/fill,forient=45,fcolor=!red,/fline
  legend,['@30 min','@1 day'],/top,/left,textcolor=[!p.color,!red],box=0
  

  ;;Count Rate @ 200 Mpc
  dist200=200*mpc2cm

  xrange=[1d-3,1d5]
  plot,[10,1e6],[1e-5,1e2],xrange=[10,1e7],yrange=xrange,/xlog,/ylog,ytitle='Count Rate @200 Mpc (0.3-10 keV) [counts s!U-1!N]',/nodata,xtitle='Time/(1+z) [s]'
  j=0
  f30=dblarr(nz)
  t30=30.*60d
  f1=f30
  t1=86400d

  for i=0,nz-1 do begin
     grb=strcompress(grbs[s[z[i]]].name,/rem)
     print,grb
     if exist(grb) then begin 
        cd,grb
        if exist('PCCURVE.qdp') then begin 
           lc=lcout2fits(/phil)
           spec=mrdfits('UL_specfits.fits',1)
           dist=lumdist(grbs[s[z[i]]].redshift,h0=71,lambda=0.73,omega_m=0.27)
           dist=dist*mpc2cm
           r=dist^2/dist200^2
           z1=1.+grbs[s[z[i]]].redshift
           oploterror,lc.time/z1,lc.src_rate*r,lc.src_rate_err*r,psym=3,/nohat,errcolor=color[j]
           for q=0,n_elements(lc)-1 do oplot,[lc[q].tstart,lc[q].tstop]/z1,[lc[q].src_rate,lc[q].src_rate]*r,color=color[j]
           ul=where(lc.src_rate_err eq 0,nul)
           if nul gt 0 then plots,lc[ul].time/z1,lc[ul].src_rate*r,psym=8,color=color[j]
           j=j+1
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           if pnames[0] ne 'nofit' then begin 
              mo=fit_models(pnames,p)
              if max(lc.tstop) gt t30 then tmp=execute('f30[i]='+mo+'(t30*z1,p)*r')
              if max(lc.tstop) gt t1 then tmp=execute('f1[i]='+mo+'(t1*z1,p)*r')
           endif 
        endif
        cd,'..'
     endif
  endfor 
  oplot,[t30,t30],[1d-20,1d50],line=2
  oplot,[t1,t1],[1d-20,1d50],line=2,color=!red
  w=where(f30 ne 0)
  plothist,alog10(f30[w]),bin=0.5,xrange=alog10(xrange),yrange=[0,4],xtitle='Count Rate',ytitle='N',/fill
  w=where(f1 ne 0)
  plothist,alog10(f1[w]),bin=0.5,color=!red,/over,xrange=alog10(xrange),yrange=[0,4],/fill,forient=45,fcolor=!red,/fline
  legend,['@30 min','@1 day'],/top,/left,textcolor=[!p.color,!red],box=0


  endplot
  spawn,'convert ~/Swift/LIGO/shb_plots.ps ~/Swift/LIGO/shb_plots.pdf'

;;; make panel for count rate, flux, luminosity, scale to 200 Mpc
;;; get histgrams at 30 min and 1 day
  stop

  return
end 
