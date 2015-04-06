@fit_functions
@fit_functions_flares
pro chandra_cycle16,go=go

  ;;; eiso
;  goto,skip
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  begplot,name='~/proposals/Chandra_cycle16/eiso_plots.eps',/encap,/color,font='helvetica',/land
  sgrbs=where(grbstr.who eq 'BAT' and grbstr.eiso ne 0)
  lgrbs=where(grbstr.who eq 'LAT' and grbstr.eiso ne 0)
  ggrbs=where(grbstr.who eq 'GBM' and grbstr.eiso ne 0)
  grbs=['GRB120711A','GRB130427A','GRB130518A','GRB130702A','GRB130907A','GRB131108A','GRB131231A'] 
  eiso=[1.95d54,8.5d53,1.7d54,3d50,3.3d54,5.8d53,1.7d53]
  z=[1.405,0.34,2.49,0.145,1.2,2.4,0.642]
  tb=[238844.92,15697806.,402247.86,19509008.,33699.558,36750.337,430965.75]
  lim=[1,1,1,1,0,0,1]
  wlim=where(lim eq 1)
  wb=where(lim eq 0)
  thetaj=fltarr(7)
  for i=0,6 do thetaj[i]=jet_angle(tb[i]/86400.,z=z[i],eiso=eiso[i])
  egam=eiso*(1.-cos(thetaj*!dtor))

  plot,[48,56],[0,25],/nodata,/xsty,ytitle='N',xtitle='log E!L'+!tsym.gamma+',iso, 10 keV-10 MeV!N (erg)',charsize=2
  plothist,alog10(grbstr[sgrbs].eiso),bin=0.3,/over,/fill,fcolor=110,color=110
;  plothist,alog10(grbstr[ggrbs].eiso),bin=0.3,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
  plothist,alog10([grbstr[lgrbs].eiso,eiso]),bin=0.3,/over,color=!red,line=0
  oplot,[0,100],[0,0]
;  legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red] ,charsize=2
  legend,['BAT','LAT'],/top,/right,box=0,textcolor=[!grey50,!red] ,charsize=2
  endplot
  spawn,'ps2pdf ~/proposals/Chandra_cycle16/eiso_plots.eps ~/proposals/Chandra_cycle16/eiso_plots.pdf'

  ;;; should plot GBM fluence vs burst date - highlighting co-swift &
  ;;;                                         swift follow-up
  ;;; shit - don't have these numbers past GBM catalog
  ;;; need also only those in LAT FoV

  ;; energetics_long

  w=where(grbstr.egam gt 0 and grbstr.dur eq 'long ')
  sj=where(grbstr[w].who ne 'LAT' and grbstr[w].xjb eq 1)
  snoj=where(grbstr[w].who ne 'LAT' and grbstr[w].xjb eq 0)
  lj=where(grbstr[w].who eq 'LAT' and grbstr[w].xjb eq 1)
  lnoj=where(grbstr[w].who eq 'LAT' and grbstr[w].xjb eq 0)

  begplot,name='~/proposals/Chandra_cycle16/energetics.eps',/encap,/color,font='helvetica',/land

  tbin=0.5
  ebin=0.3
  !x.margin=[2,2]
  !y.margin=[4,0]
  multiplot,[2,2],/init

  multiplot
  plothist,grbstr[w[sj]].thetaj,bin=tbin,xrange=[0,20],yrange=[0,15],/fill,fcolor=110,ytitle='N'
;  plothist,grbstr[w[lj]].thetaj,bin=tbin,xrange=[0,20],yrange=[0,15],color=!red,/over
  plothist,thetaj[wb],bin=tbin,xrange=[0,20],yrange=[0,15],color=!red,/over
  xyouts,10,12,'Measurements'

  multiplot
  plothist,alog10(grbstr[w[sj]].egam),bin=ebin,xrange=[47,53],yrange=[0,15],/fill,fcolor=110
;  plothist,alog10(grbstr[w[lj]].egam),bin=ebin,xrange=[47,53],yrange=[0,15],/over,color=!red
  plothist,alog10(egam[wb]),bin=ebin,xrange=[47,53],yrange=[0,15],/over,color=!red
  legend,['BAT','LAT'],textcolor=[!grey50,!red],box=0,/top,/right

  multiplot
  plothist,grbstr[w[snoj]].thetaj,bin=tbin,xrange=[0,20],yrange=[0,15],/fill,fcolor=110,xtitle=!tsym.theta+'!Lj!N (deg)',ytitle='N',xtickname=['0','5','10','15',' ']
  plothist,[grbstr[w[lnoj]].thetaj,thetaj[wlim]],bin=tbin,xrange=[0,20],yrange=[0,15],color=!red,/over
  arrow,10,7,13,7,/data,/solid,thick=10,hthick=3
  xyouts,10,12,'Lower Limits'

  multiplot
  plothist,alog10(grbstr[w[snoj]].egam),bin=ebin,xrange=[47,53],yrange=[0,15],/fill,fcolor=110,xtitle='log E!L'+!tsym.gamma+'!N (erg)',xtickname=[' ','48','49','50','51','52','53']
  plothist,alog10([grbstr[w[lnoj]].egam,egam[wlim]]),bin=ebin,xrange=[47,53],yrange=[0,15],/over,color=!red
  arrow,51.6,7,52.6,7,/solid,thick=10,/data,hthick=3


  multiplot,/reset,/default
  endplot

  spawn,'ps2pdf ~/proposals/Chandra_cycle16/energetics.eps ~/proposals/Chandra_cycle16/energetics.pdf'

  stop

  ;;; table data

  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB110731A',$
          'GRB120711A','GRB130427A','GRB130518A','GRB130702A','GRB130907A','GRB131108A','GRB131231A'] 
  z=[4.35,3.57,0.73,0.903,1.822,2.1062,0.8969,1.368,2.83,$
     1.405,0.34,2.49,0.145,1.2,2.4,0.642]
  eiso=[0,0,0,0,0,0,0,0,0,1.95d54,8.5d53,1.7d54,3d50,3.3d54,5.8d53,1.7d53]
  nlat=n_elements(latgrbs)

  cd,'~/GRBs/'

  t30=86400.*30.
  t60=86400.*60.
  f1=5d-15
  f2=2d-15
;  f30=dblarr(nlat) & f60=f30 & r30=f30 & r60=f30 & x30=f30 & x60=f30
;  & xr30=f30 & xr60=f30
  if not exist('~/proposals/Chandra_cycle16/lat_bursts.fits') or keyword_set(go) then begin 
     lat=create_struct('grb','','pnames',strarr(25),'p',fltarr(25),$
                       'phind',0.,'cfratio',0d,'z',0d,'model','',$
                       'xrt_rate1',0d,'xrt_rate2',0d,$
                       'time1',0d,'time2',0d,$
                       'cxo_rate1',0d,'cxo_rate2',0d,$
                       'flux1',0d,'flux2',0d,$
                       'exp1',0d,'exp2',0d)
     lat=replicate(lat,nlat)
     d=dindgen(9)+1.
     t=[d*100.,d*1e3,d*1e4,d*1e5,d*1e6,d*1e7]

     for i=0,nlat-1 do begin 
        lat[i].grb=latgrbs[i]
        lcfitfile=latgrbs[i]+'/lc_fit_out_idl_int8.dat'
        if not exist(lcfitfile) then lcfitfile=latgrbs[i]+'/lc_fit_out_idl_int7.dat'
        read_lcfit,lcfitfile,pnames,p,perr,np=np
        spec=mrdfits(latgrbs[i]+'/UL_specfits.fits',1)
        ns=n_elements(spec)
        mo=fit_models(pnames,p,np,nf)
        lat[i].model=mo
        lat[i].p[0:n_elements(p)-1]=p
        lat[i].pnames[0:n_elements(pnames)-1]=pnames
        lat[i].cfratio=spec[ns-1].cfratio
        lat[i].xrt_rate1=f1/lat[i].cfratio
        lat[i].xrt_rate2=f2/lat[i].cfratio
        ;; at what time is flux = f1, f2
        lat[i].time1=interpol(t,lat[i].xrt_rate1/call_function(mo,t,p),1.)
        lat[i].time2=interpol(t,lat[i].xrt_rate2/call_function(mo,t,p),1.)

;        tmp1=execute('lat[i].xrt_rate30='+mo+'(t30,p)')
;        tmp2=execute('lat[i].xrt_rate60='+mo+'(t60,p)')
;        if tmp1 eq 0 or tmp2 eq 0 then stop
        lat[i].phind=spec[ns-1].phind
        lat[i].z=z[i]
        lat[i].flux1=lat[i].xrt_rate1*lat[i].cfratio
        lat[i].flux2=lat[i].xrt_rate2*lat[i].cfratio

        pimmsfile='pimms.xco'
        com1='model pl '+ntostr(spec[ns-1].phind)+' '+ntostr(spec[ns-1].nh)+' z '+ntostr(spec[ns-1].z)+' '+ntostr(spec[ns-1].nhgal*1d22)
        com2='from flux photons 0.3-10.0'
        com3='instrument chandra acis-s 0.5-8.0'
        com4='go '+ntostr(lat[i].flux1)+' flux ergs 0.3-10.0'
        com5='output pimms.dat 0.5 8.0 0.005'
        com6='quit'
        writecol,pimmsfile,[com1,com2,com3,com4,com5,com6]
        spawn,'pimms @'+pimmsfile+'> pimms.log'
        readcol,'pimms.log',lines,delim='$',format='(a)'
        for l=0,n_elements(lines)-1 do begin
           spos=strpos(lines[l],'predicts')
           if spos ne -1 then begin
              str=str_sep(lines[l],' ')
              ctr1=str[3]
           endif
        endfor 
        lat[i].cxo_rate1=ctr1
        lat[i].cxo_rate2=lat[i].flux2/lat[i].flux1*ctr1
 
        ;; what exposure is needed for 10 counts at these fluxes
        ncounts=10.
        lat[i].exp1=ncounts/lat[i].cxo_rate1
        lat[i].exp2=ncounts/lat[i].cxo_rate2
;        lat[i].exp30=ncounts/lat[i].cxo_rate30
;        lat[i].exp60=ncounts/lat[i].cxo_rate60

     endfor 

     mwrfits,lat,'~/proposals/Chandra_cycle16/lat_bursts.fits',/create
  endif else lat=mrdfits('~/proposals/Chandra_cycle16/lat_bursts.fits',1)
stop
  a=' & '
  f=1e-14
  day=86400.
  for i=0,nlat-1 do begin
     
     w=where(lat[i].p ne 0)
     mo=fit_models(lat[i].pnames[w],lat[i].p[w],np,nf)
     
;     print,lat[i].grb,a,numdec(lat[i].p[np-1],2,/tex),a,numdec(lat[i].flux30,2,/sci,/tex),a,numdec(lat[i].flux60,2,/sci,/tex),a,numdec(lat[i].cxo_rate30,2,/sci,/tex),a,numdec(lat[i].cxo_rate60,2,/sci,/tex),a,numdec(lat[i].exp30*1e-3,1,/tex),a,numdec(lat[i].exp60*1e-3,1,/tex),' \\'
;     stuff=[
     print,lat[i].grb,a,numdec(z[i],2,/tex),a,numdec(eiso[i],1,/sci,/tex),a, $
           numdec(lat[i].p[np-1],2,/tex),a,$
           numdec(lat[i].time1/day,2,/tex),a,numdec(lat[i].time2/day,2,/tex),a,$
;           numdec(lat[i].flux1,2,/sci,/tex),a,numdec(lat[i].flux2,2,/sci,/tex),a,$
           numdec(lat[i].cxo_rate1,2,/sci,/tex),a,numdec(lat[i].cxo_rate2,2,/sci,/tex),a,$
           numdec(lat[i].exp1*1e-3,1,/tex),a,numdec(lat[i].exp2*1e-3,1,/tex),' \\'
  endfor 
;  colprint,latgrbs,xr30,xr60,f30,f60,r30,r60,x30,x60

     stop
     return
  end 
