;; fselect racusin-AstroServer-00002-ft2-1s.fits 2008-ft2-1s.fits "START <252460800"
pro get_lat_angle,ra,dec,trigtime,dist1

  get_ft2,trigtime,ft2

  if n_elements(ft2) gt 0 then begin 
     w1=where(ft2.stop le trigtime+3600. and ft2.start ge trigtime-100 and ft2.in_saa eq 'F',nw1)
     if nw1 gt 0 then begin 
        ft2=ft2[w1]
        
;     start=dindgen(nw1*30d)+ft2[0].start
        times=[trigtime,trigtime+30,trigtime+60]
        
        d1=separation(ra,dec,ft2.ra_scz,ft2.dec_scz)/3600.
        d2=separation(ra,dec,ft2.ra_zenith,ft2.dec_zenith)/3600.
        
        dist1=interpol(d1,ft2.start,times)
        dist2=interpol(d2,ft2.start,times)
        
     endif 
  endif 

  return
end 

pro download_ft2s

  cd,'~/Fermi/weekly_ft2'
  ft2s=file_search('lat_1sec_spacecraft_weekly*fits')

  week=strmid(ft2s,28,3)
  
  com='wget ftp://legacy.gsfc.nasa.gov/fermi/data/lat/weekly/1s_spacecraft/ -O ft2_list.txt'
  spawn,com

  readcol,'ft2_list.txt',b1a,b2a,b3a,b4a,b5a,b6a,b7a,b8a,format='(a,a,a,a,a,a,a,a)'
  readcol,'ft2_list.txt',b1,b2,b3,b4,b5,b6,b7,b8,b9,format='(a,a,a,a,a,a,a,a,a)'
  weeks=strmid([b6a,b7],100,3)
  weeks=weeks[sort(weeks)]
  w=where(strtrim(weeks,2) ne '')
  weeks=weeks[w]

  dont_match,week,weeks,m1,m2
  print,weeks[m2]
  newfiles='ftp://legacy.gsfc.nasa.gov/fermi/data/lat/weekly/1s_spacecraft/lat_1sec_spacecraft_weekly_w'+weeks[m2]+'_p203_v001.fits'
stop
  for i=0,n_elements(m2)-1 do spawn,'wget '+newfiles[i]

end 

pro test_1sft2

  mweekstr='010'
  ft2file='~/Fermi/weekly_ft2/lat_spacecraft_weekly_w'+mweekstr+'_p130_v001.fits'
  ft2file1s='~/Fermi/weekly_ft2/lat_1sec_spacecraft_weekly_w'+mweekstr+'_p202_v001.fits'

  ft2=mrdfits(ft2file,1)
  ft2a=mrdfits(ft2file1s,1)

  ra=100.
  dec=10.
  trigtime=median(ft2.start)

  orbit=96.*60.
  w1=where(ft2.stop le trigtime+orbit and ft2.start ge trigtime-100,nw1)
  ft2=ft2[w1]
  w1a=where(ft2a.stop le trigtime+orbit and ft2a.start ge trigtime-100,nw1a)
  ft2a=ft2a[w1a]

  start=indgen(nw1*30d)+ft2[0].start

  d1=separation(ra,dec,ft2.ra_scz,ft2.dec_scz)/3600.
  d2=separation(ra,dec,ft2.ra_zenith,ft2.dec_zenith)/3600.
    
  dist1=interpol(d1,ft2.start,start)
  dist2=interpol(d2,ft2.start,start)

  d1a=separation(ra,dec,ft2a.ra_scz,ft2a.dec_scz)/3600.
  d2a=separation(ra,dec,ft2a.ra_zenith,ft2a.dec_zenith)/3600.

  !p.multi=[0,1,2]
  plot,ft2.start-trigtime,d1,psym=2,xrange=[-100,orbit],/xsty
  oplot,start-trigtime,dist1,psym=2,color=!red
  oplot,ft2a.start-trigtime,d1a,psym=4,color=!blue

  plot,ft2.start-trigtime,d2,psym=2,xrange=[-100,orbit],/xsty
  oplot,start-trigtime,dist2,psym=2,color=!red
  oplot,ft2a.start-trigtime,d2a,psym=4,color=!blue
  !p.multi=0

  stop
  

  return
end 
pro swift_grbs,go=go
  jim=mrdfits('~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
  g=mrdfits('~/Swift/swiftgrb_list.fits',1)
  ng=n_elements(g)
  fermistart=date2met('2008-09-01 00:00:00',/fermi)

  out=create_struct('grb','','ra',0.,'dec',0.,'err',0.,'trigtime',0d,$
                    'gti_start',dblarr(25),'gti_stop',dblarr(25),'gti_mean',dblarr(25),$
                    'ctrate',dblarr(25),'ctrate_err',dblarr(2,25),$
                    'first_gti',dblarr(2),'first_gti_ctrate',0.,'first_gti_ctrate_err',dblarr(2))
  out=replicate(out,ng)
  if keyword_set(go) then begin 
     go=1 
     noplot=1
  endif else begin
     go=0
     noplot=0
  endelse 
  istart=0
  stop
  for i=istart,ng-1 do begin 
     trigdate=met2date(g[i].met)
     trigtime=date2met(trigdate,/fermi)
     if trigtime gt fermistart then begin 
        print,g[i].grb
        j=where(jim.grb eq g[i].grb,nj)
        jj=where(jim[j].ltstart ne 0)
        print,j,jj
        if nj gt 0 then begin
           altstart=jim[j].ltstart[jj]
           altstop=jim[j].ltstop[jj]
        endif else begin
           altstart=0 & altstop=0
        endelse 
        lat_gtis,g[i].xrt_ra,g[i].xrt_dec,trigtime,gti_start,gti_stop,altstart=altstart,altstop=altstop,ft2=ft2,noplot=noplot

        out[i].grb=g[i].grb
        out[i].ra=g[i].xrt_ra
        out[i].dec=g[i].xrt_dec
        out[i].err=g[i].xrt_err
        out[i].trigtime=trigtime
        out[i].gti_start=gti_start
        out[i].gti_stop=gti_stop
        w=where(gti_start lt 0,nw)
        if nw gt 0 then gti_start[w]=0.1
        out[i].gti_mean=sqrt(gti_start*gti_stop)
;        colprint,gti_start,gti_stop
;          stop      
        if not go then begin 
           print
           colprint,jim[j].ltstart[jj],jim[j].ltstop[jj]
           k=get_kbrd(10)
           if k eq 's' then stop
        endif 
     endif 
  endfor 
  w=where(out.grb ne '          ' and out.ra ne 0.)
  out=out[w]

  mwrfits,out,'~/Swift/swiftgrb_latgtis.fits',/create
  stop

return
end 

pro get_ft2,trigtime,ft2

  columns=['START','STOP','RA_SCZ','DEC_SCZ','RA_SCX','DEC_SCX','RA_ZENITH','DEC_ZENITH','DATA_QUAL','LAT_CONFIG','IN_SAA']
  week=86400.*7
  mweek0=fix((trigtime-date2met('2008-06-05-00:00:00'))/week-1)
  mweek1=fix((trigtime-date2met('2008-06-05-00:00:00'))/week+0)
  mweek2=fix((trigtime-date2met('2008-06-05-00:00:00'))/week+1)
  mweek=[mweek0,mweek1,mweek2]
  if mweek[0] ge 8 then begin
     for j=0,2 do begin 
        mweekstr=ntostr(mweek[j])
        if mweek[j] lt 100 then mweekstr='0'+mweekstr
        if mweek[j] lt 10 then mweekstr='0'+mweekstr
        ft2file=file_search('~/Fermi/weekly_ft2/lat_1sec_spacecraft_weekly_w'+mweekstr+'_*_v001.fits')
;        ft2file='~/Fermi/weekly_ft2/lat_1sec_spacecraft_weekly_w'+mweekstr+'_p202_v001.fits'
        ft2file=ft2file[0]
        print,ft2file
        if exist(ft2file) and strtrim(ft2file,2) ne '' then begin 
           ft2=mrdfits(ft2file,1,/silent,column=columns)
           if j eq 0 then ft20=ft2 else ft20=0
           if j eq 1 then ft21=ft2 else ft21=0
           if j eq 2 then ft22=ft2 else ft22=0
        endif 
     endfor 
     if n_elements(ft20) gt 1 and n_elements(ft21) gt 1 and n_elements(ft22) gt 1 then begin 
        concat_structs,ft20,ft21,ft_temp
        concat_structs,ft_temp,ft22,ft2
     endif else begin
        if n_elements(ft20) eq 1 and n_elements(ft21) gt 1 and n_elements(ft22) gt 1 then concat_structs,ft21,ft22,ft2
        if n_elements(ft20) gt 1 and n_elements(ft21) eq 1 and n_elements(ft22) gt 1 then concat_structs,ft20,ft22,ft2
        if n_elements(ft20) gt 1 and n_elements(ft21) gt 1 and n_elements(ft22) eq 1 then concat_structs,ft20,ft21,ft2
        if n_elements(ft20) eq 1 and n_elements(ft21) eq 1 and n_elements(ft22) gt 1 then ft2=ft22
        if n_elements(ft20) eq 1 and n_elements(ft21) gt 1 and n_elements(ft22) eq 1 then ft2=ft21
        if n_elements(ft20) gt 1 and n_elements(ft21) eq 1 and n_elements(ft22) eq 1 then ft2=ft20
        if n_elements(ft20) eq 1 and n_elements(ft21) eq 1 and n_elements(ft22) eq 1 then ft2=0
     endelse 

  endif
  return 
end 

pro lat_gtis,ra,dec,trigtime,gtistart,gtistop,noplot=noplot,altstart=altstart,altstop=altstop,ft2=ft2

  get_ft2,trigtime,ft2

  day=86400d
  orbit=96.*60.
  w1=where(ft2.stop le trigtime+day and ft2.start ge trigtime-100 and ft2.in_saa eq 'F',nw1)
  if nw1 gt 0 then begin 
     ft2=ft2[w1]
;     print,nw1*30d

;     start=dindgen(nw1*30d)+ft2[0].start
;     stop=start+1
     start=ft2.start
     stop=ft2.stop

     d1=separation(ra,dec,ft2.ra_scz,ft2.dec_scz)/3600.
     d2=separation(ra,dec,ft2.ra_zenith,ft2.dec_zenith)/3600.
     
;     dist1=interpol(d1,ft2.start,start)
;     dist2=interpol(d2,ft2.start,start)
     dist1=d1 ;; use 1s FT2
     dist2=d2

;     data_qual=intarr(n_elements(start)) & lat_config=data_qual
;     for j=0L,nw1-1 do begin 
;        data_qual[j*30:(j+1)*30-1]=ft2[j].data_qual
;        lat_config[j*30:(j+1)*30-1]=ft2[j].lat_config
;     endfor 
     data_qual=ft2.data_qual
     lat_config=ft2.lat_config

     w=where(dist1 le 65. and dist2 le 105. and data_qual eq 1,nw); and lat_config eq 1,nw)
     
     if nw gt 0 then begin 
        gtis=start[w]-trigtime
        wgti=where(gtis[1:*]-gtis[0:*] gt 30)
        gtistart=gtis[[0,wgti+1]]
        gtistop=gtis[[wgti,n_elements(gtis)-1]]
;     w0=where(gtistart lt 0.)
;     gtistart[w0]=1e-1

        if not keyword_set(noplot) then begin 
           !p.multi=[0,1,2]
           plot,start-trigtime,dist2,ytitle='Zenith-RA,DEC',psym=1,xtitle='Time (s)'
;        oplot,ft2.start-trigtime
           oplot,start[w]-trigtime,dist2[w],color=!blue,psym=1
           oplot,[0,2.5e4],[105,105],line=2

           if n_elements(altstart) gt 0 then begin
              if altstart[0] ne 0 then begin 
                 for j=0,n_elements(altstart)-1 do begin 
                    jj=where(start-trigtime ge altstart[j] and stop-trigtime le altstop[j])
                    plots,start[jj]-trigtime,dist2[jj],color=!red,psym=1,symsize=0.5
                 endfor 
              endif 
           endif 

           plot,start-trigtime,dist1,ytitle='SCZ-RA,DEC',psym=1,xtitle='Time (s)'
           oplot,start[w]-trigtime,dist1[w],color=!blue,psym=1
           oplot,[0,2.5e4],[65,65],line=2

           if n_elements(altstart) gt 0 then begin
              if altstop[0] ne 0 then begin 
                 for j=0,n_elements(altstart)-1 do begin 
                    jj=where(start-trigtime ge altstart[j] and stop-trigtime le altstop[j])
                    plots,start[jj]-trigtime,dist1[jj],color=!red,psym=1,symsize=0.5
                 endfor 
              endif 
           endif 

;  for i=0,n_elements(gtis)-1 do print,gtis[i],dist1[w[i]],dist2[w[i]]
;        w=where(start-trigtime gt 600 and start-trigtime lt 900)
;        dist1f=separation(ra,dec,ft2.ra_scz,ft2.dec_scz)/3600.
;        plot,start[w]-trigtime,dist1[w],psym=1
;        oplot,ft2.start-trigtime,dist1f,psym=2,color=!red
;        for i=0,n_elements(gtistart)-1 do print,gtistart[i],gtistop[i]
           !p.multi=0
        endif 
     endif 
  endif 

  return
end 
