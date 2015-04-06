pro swift_lat_gi_figs,skip2uvot=skip2uvot,skip2grb=skip2grb


  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',1)
  if keyword_set(skip2grb) then goto,skip
  bat=where(grbstr.who eq 'BAT' or grbstr.who eq 'GBM',nbat)
  lat=where(grbstr.who eq 'LAT',nlat)
  s=[bat,lat]
  grbstr=grbstr[s]

  if keyword_set(skip2uvot) then goto,skipuvot
  grbs=grbstr.grb
  ngrbs=n_elements(grbs)
  cd,'~/GRBs'

  ;;; XRT
  begplot,name='~/proposals/swift_gi_7/xrt_fig.eps',/color,/land,/encap,font='helvetica'
  !x.margin=[15,3]
  colors=[!red,!blue,!green,!orange,!cyan,!magenta,!purple,!darkgreen]

  plot,[1,1e7],[1e-15,1e-5],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[1,1e7],yrange=[1e-15,1e-4],yticks=11,xtickformat='loglabels'
  j=0
  leg=''
  for i=0,ngrbs-1 do begin 
     lcfile=strtrim(grbs[i],2)+'/lc_newout_phil.txt'
     lcfitfile=strtrim(grbs[i],2)+'/lc_fit_out_idl_int7.dat'
     if exist(lcfile) and exist(lcfitfile) then begin 
        lc=lcout2fits(lcfile)
        wdet=where(lc.src_rate_err gt 0,nwdet)
        if nwdet gt 2 then begin 
           if grbstr[i].who ne 'LAT' then color=!grey50 else begin
              color=colors[j]
              leg=[leg,grbs[i]]
              j=j+1
           endelse 
           terr=dblarr(2,nwdet)
           terr[0,*]=lc[wdet].time-lc[wdet].tstart
           terr[1,*]=lc[wdet].tstop-lc[wdet].time
           xfluxfact=grbstr[i[0]].xfluxfact
           fluxerr=dblarr(2,nwdet)
           fluxerr[0,*]=lc[wdet].src_rate_err*xfluxfact
           fluxerr[1,*]=lc[wdet].src_rate_err*xfluxfact
           oploterror2,lc[wdet].time,lc[wdet].src_rate*xfluxfact,terr,fluxerr,psym=3,/nohat,color=color
        endif 
     endif 
  endfor 

  leg=[leg[1:*],'Swift GRBs']
  legend,leg,/top,/right,box=0,textcolor=[colors[0:j-1],!grey50]

  oplot,[5,3e3],[1e-5,1e-5],line=0,thick=5
  xyouts,2,2e-5,'LAT extended emission time frame',charsize=1.5

  endplot
  
  skipuvot:
  ;;; UVOT
  begplot,name='~/proposals/swift_gi_7/uvot_fig.eps',/color,/land,/encap,font='helvetica'
  !x.margin=[15,3]
  colors=[!red,!blue,!green,!orange,!cyan,!magenta,!purple,!darkgreen]

  scolor=[replicate(!grey50,nbat),colors[0:nlat-1]]
  w=where(grbstr.oalpha ne 0,ngrbs)
  grbstr=grbstr[w]
  grbs=grbstr.grb
  scolor=scolor[w]

  cd,'~/Fermi/Swift_pop_study/'
  
  plot,[1,1e7],[1e-20,1e-10],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (u band) (erg cm!U-2!N s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[1,1e7],yrange=[1e-20,1e-11],xtickformat='loglabels',yticks=9
  j=0
  leg=''
  for i=0,ngrbs-1 do begin 
     lcfile=grbstr[i].who+'_UVOT_LC/'+strtrim(grbs[i],2)+'/lc_newout.txt'
     lcfitfile=grbstr[i].who+'_UVOT_LC/'+strtrim(grbs[i],2)+'/lc_fit_out_idl_int7.dat'
     if exist(lcfile) and exist(lcfitfile) then begin 
        lc=lcout2fits(lcfile)
        wdet=where(lc.src_rate_err gt 0,nwdet)
        if nwdet gt 2 then begin 
           if grbstr[i].who eq 'LAT' then begin
              leg=[leg,grbs[i]]
              j=[j,i]
           endif 
           color=scolor[i]
           terr=dblarr(2,nwdet)
           terr[0,*]=lc[wdet].time-lc[wdet].tstart
           terr[1,*]=lc[wdet].tstop-lc[wdet].time
           fluxfact=grbstr[i[0]].ofluxfact
           fluxerr=dblarr(2,nwdet)
           fluxerr[0,*]=lc[wdet].src_rate_err*fluxfact
           fluxerr[1,*]=lc[wdet].src_rate_err*fluxfact
           oploterror2,lc[wdet].time,lc[wdet].src_rate*fluxfact,terr,fluxerr,psym=3,/nohat,color=color
        endif 
     endif 
  endfor 

  leg=[leg[1:*],'Swift GRBs']
  legend,leg,/top,/right,box=0,textcolor=[scolor[j[1:*]],!grey50]

  oplot,[5,3e3],[1e-12,1e-12],line=0,thick=5
  xyouts,2,2e-12,'LAT extended emission time frame',charsize=1.5

  endplot

  spawn,'convert -density 100 -flatten ~/proposals/swift_gi_7/xrt_fig.eps ~/proposals/swift_gi_7/xrt_fig_flat.eps'
  spawn,'convert -density 100 -flatten ~/proposals/swift_gi_7/uvot_fig.eps ~/proposals/swift_gi_7/uvot_fig_flat.eps'

  skip:

  ;;; GRB 090328A
  g=where(grbstr.grb eq 'GRB090328A')
  latlc=lcout2fits('~/Fermi/Swift_pop_study/LAT/GRB090328A/lc_newout.txt')
  xlc=lcout2fits('~/GRBs/GRB090328A/lc_newout_phil.txt')
  olc=lcout2fits('~/Fermi/Swift_pop_study/LAT_UVOT_LC/GRB090328A/lc_newout.txt')
  xfluxfact=grbstr[g].xfluxfact
  ofluxfact=grbstr[g].ofluxfact

  begplot,name='~/proposals/swift_gi_7/grb090328.eps',/encap,/land,/color,font='helvetica'
  xrange=[10,1d7]
  yrange=[1d-20,1d-4]
  plot,xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)',/xsty,/ysty,title='GRB 090328A'
  oploterror,latlc.time,latlc.src_rate,latlc.src_rate_err,psym=3,/nohat
  for i=0,n_elements(latlc)-1 do oplot,[latlc[i].tstart,latlc[i].tstop],[latlc[i].src_rate,latlc[i].src_rate]
  plotsym,1,4,thick=5
  oplot,latlc[3:*].time,latlc[3:*].src_rate,psym=8
  
  oploterror,xlc.time,xlc.src_rate*xfluxfact,xlc.src_rate_err*xfluxfact,psym=3,/nohat,errcolor=!red
  for i=0,n_elements(xlc)-1 do oplot,[xlc[i].tstart,xlc[i].tstop],[xlc[i].src_rate,xlc[i].src_rate]*xfluxfact,color=!red

  oploterror,olc.time,olc.src_rate*ofluxfact,olc.src_rate_err*ofluxfact,psym=3,/nohat,errcolor=!blue
  for i=0,n_elements(olc)-1 do oplot,[olc[i].tstart,olc[i].tstop],[olc[i].src_rate,olc[i].src_rate]*ofluxfact,color=!blue

  legend,['LAT (>100 MeV)','XRT (0.3 - 10 keV)','UVOT (u band)'],box=0,/bottom,/left,textcolor=[!p.color,!red,!blue]

  endplot
  stop
  return
end 
pro swift_lat_gi_figs_old

  begplot,name='~/proposals/swift_gi_6/xrt_fig/xrt_fig_ctr.eps',/color,/land,/encap
  !x.margin=[15,3]
  simpctable
;  plot,[1,1e7],[1e-15,1e-5],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[1,1e7],yrange=[1e-15,1e-4],yticks=11
  plot,[1,1e7],[1e-5,1e3],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[1,1e7],yrange=[1e-5,1e3],yticks=8

  cd,'/Volumes/Firewire1/racusin/grbs'
  grb=file_search('GRB*')
  n=n_elements(grb)
  for i=0,n-2 do begin
     print,grb[i]
     cd,grb[i]
     lc=lcout2fits(/phil)
     if n_elements(lc) gt 10 then begin 
        print,lc[0].tstart,lc[0].tstart/3600.
;        read_specfit,spec,append='_2s'
;        if n_elements(spec) gt 0 then begin 
;           ns=n_elements(spec)-1
;           if spec[ns].flux eq 0 then ns=ns-1
;           ff=spec[ns].flux/spec[ns].rate
           ff=1.
           det=where(lc.src_rate_err gt 0,ndet)
           oplot,lc[det].time,lc[det].src_rate*ff,psym=3,color=!grey50 ;colors[i]
           for j=0,ndet-1 do begin
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*ff,color=!grey50 ;colors[i]
              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*ff,color=!grey50
           endfor 
;        endif 
     endif 
     cd,'..'
  endfor 

  colors=[!red,!blue,!green,!orange,!cyan,!magenta];,!purple,!yellow]
  cd,'~/proposals/swift_gi_6/xrt_fig/'
  grb=file_search('GRB*')
  n=n_elements(grb)
  fluxfact=[5.1,3.7,5.8,4.1,4.1,3.6]*1d-11
  fluxfact[*]=1

;  plotsym,0,0.5,/fill
  for i=0,n-1 do begin
     print,grb[i]
     cd,grb[i]
     lc=lcout2fits(/phil)
     det=where(lc.src_rate_err gt 0,ndet)
     print,lc[0].tstart,lc[0].tstart/3600.
     oplot,lc[det].time,lc[det].src_rate*fluxfact[i],psym=3,color=colors[i]
     for j=0,ndet-1 do begin
        oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*fluxfact[i],color=colors[i]
        oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*fluxfact[i],color=colors[i]
     endfor 

     cd,'..'
  endfor 
  legend,['LAT '+grb,'Swift GRBs'],textcolor=[colors,!grey50],box=0,/top,/right

  oplot,[5,3e3],[1e-5,1e-5],line=0,thick=5
  xyouts,2,2e-5,'LAT extended emission time frame',charsize=1.5

  endplot
  spawn,'convert -density 100 -flatten ../xrt_fig/xrt_fig.eps ../xrt_fig.eps'

return
end 
