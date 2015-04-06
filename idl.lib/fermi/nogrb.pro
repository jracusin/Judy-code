pro radec2sclonlat,rapol,decpol,rascz,decscz,sclon,sclat

  inc=25.6d
  sdp = sin(decpol/!radeg)
  cdp = sqrt(1.-sdp^2)
  ras = rascz/!radeg - rapol/!radeg
  sdec = sin(decscz/!radeg)
  cdec = sqrt(1.-sdec^2)
  sgb = sdec*sdp + cdec*cdp*cos(ras)
  sclat = !radeg * asin(sgb)
  cgb = sqrt(1.-sgb^2)
  sine = cdec * sin(ras) / cgb
  cose = (sdec-sdp*sgb) / (cdp*cgb)
  sclon = inc - !radeg*atan(sine,cose)
  ltzero=where(sclon lt 0., Nltzero)
  if Nltzero ge 1 then sclon[ltzero]=sclon[ltzero]+360d

  return
end 

pro sclonlat2radec,rapol,decpol,sclon,sclat,rascz,decscz

  inc=25.6d
;  radhrs=!radeg/15.0d0
  sdp = sin(decpol/!radeg)
  cdp = sqrt(1.-sdp^2)
  sgb = sin(sclat/!radeg)
  cgb = sqrt(1.-sgb^2)
  sdec = sgb*sdp + cgb*cdp*cos((inc-sclon)/!radeg)
  decscz = !radeg * asin(sdec)
  cdec = sqrt(1.-sdec^2)
  sinf = cgb * sin((inc-sclon)/!radeg) / cdec
  cosf = (sgb-sdp*sdec) / (cdp*cdec)
  rascz = rapol + !radeg*atan(sinf,cosf)
;  rascz = rascz*15.0d
  gt36 = where(rascz gt 360.0, Ngt36)
  if Ngt36 ge 1 then rascz[gt36] = rascz[gt36] - 360d
  lt0=where(rascz lt 0., nlt0)
  if nlt0 ge 1 then rascz[lt0]=rascz[lt0]+360d

  return
end 

pro nogrb,gbm=gbm

  ;; take list of ra/dec/theta/trigtime for all LAT detected GRBs
  ;; figure out mission week and read ft2
  ;; calculate theta and make sure matches
  ;; take all with rocking angle of 35, and shift to 50, see new theta
  ;; plot theta vs time, noting new/old rock

  if not keyword_set(gbm) then begin 
     file='~/CVS/ScienceGroups/GRB/FirstLATGRBCatalog/tables/tab_GRBs.tex'
     readcol,file,grb,trig,met,ra,dec,theta,locerr,like,lle,z,gcn,delim='&',format='(a,a,d,f,f,f,f,a,a,a,a'
     add='lat'
  endif else begin 
     
     g=mrdfits('~/Fermi/gbm_cat.fits',1)
     s=sort(g.trigger_time)
     g=g[s]
     g=g[15:*]
     grb=g.name
     met=(g.trigger_time-51910.)*86400d
     ra=g.ra
     dec=g.dec
     add='gbm'
  endelse 
  
  
  week=86400.*7

  outfile='~/Fermi/modify_rocking_angle_'+add
  begplot,name=outfile+'.ps',/land,/color,font='helvetica'

  ngrb=n_elements(grb)
  rock1=dblarr(ngrb) & d=rock1 & rock2=d & rock3=rock1 & rock4=rock1 ; & rock3a=rock1 & rock4a=rock1
  d35=rock1 & d50=rock1
  scra35=rock1 & scdec35=rock1 & scra50=rock1 & scdec50=rock1
  for i=0,ngrb-1 do begin 
     mweek=fix((met[i]-date2met('2008-06-11-00:00:00'))/week+2)
     if mweek ge 8 and mweek le 243 then begin
        mweekstr=ntostr(mweek)
        if mweek lt 100 then mweekstr='0'+mweekstr
        if mweek lt 10 then mweekstr='0'+mweekstr
        ft2file='~/Fermi/weekly_ft2/lat_spacecraft_weekly_w'+mweekstr+'_p130_v001.fits'
        print,ft2file
        ft2=mrdfits(ft2file,1)
        w=where(ft2.start lt met[i] and ft2.stop gt met[i])
        rock1[i]=separation(ft2[w].ra_zenith,ft2[w].dec_zenith,ft2[w].ra_scz,ft2[w].dec_scz)/3600.*sign(ft2[w].dec_scz) ;90-separation(ft2[w].ra_npole,ft2[w].dec_npole,ft2[w].ra_scz,ft2[w].dec_scz)/3600.  ;;; sanity check with rock2
        rs=sign(ft2[w].rock_angle)
        rock2[i]=rs*median(abs(ft2.rock_angle))  ;;; actual rocking angle
;     if abs(rock1[i]-rock2[i]) gt 2 then w=w-5
        radec2sclonlat,ft2[w].ra_npole,ft2[w].dec_npole,ft2[w].ra_scz,ft2[w].dec_scz,sclon,sclat
        if abs(round(rock2[i])) gt 45 and abs(round(rock2[i])) lt 55 then begin
           sclat2=sclat-15.*sign(sclat)
           sclonlat2radec,ft2[w].ra_npole,ft2[w].dec_npole,sclon,sclat2,scra,scdec
           rock3[i]=separation(ft2[w].ra_zenith,ft2[w].dec_zenith,scra,scdec)/3600.*sign(sclat)
;        rock3[i]=90-separation(ft2[w].ra_npole,ft2[w].dec_npole,ft2[w].ra_scz,ft2[w].dec_scz-15.*sign(ft2[w].dec_scz))/3600. 
           scra35[i]=scra
           scdec35[i]=scdec
        endif else begin ;;; moving everything to 35 deg rock
           rock3[i]=rock2[i]
           scra35[i]=ft2[w].ra_scz
           scdec35[i]=ft2[w].dec_scz
        endelse 
        if abs(round(rock2[i])) gt 30 and abs(round(rock2[i])) lt 40 then begin
           sclat2=sclat+15.*sign(sclat)
           sclonlat2radec,ft2[w].ra_npole,ft2[w].dec_npole,sclon,sclat2,scra,scdec
           rock4[i]=separation(ft2[w].ra_zenith,ft2[w].dec_zenith,scra,scdec)/3600.*sign(sclat)
;        rock4[i]=90-separation(ft2[w].ra_npole,ft2[w].dec_npole,ft2[w].ra_scz,ft2[w].dec_scz+15.*sign(ft2[w].dec_scz))/3600. 
           scra50[i]=scra
           scdec50[i]=scdec
        endif else begin ;;; moving everything to 50 deg rock
           rock4[i]=rock2[i]
           scra50[i]=ft2[w].ra_scz
           scdec50[i]=ft2[w].dec_scz
        endelse 

        d[i]=separation(ra[i],dec[i],ft2[w].ra_scz,ft2[w].dec_scz)/3600.
        d35[i]=separation(ra[i],dec[i],scra35[i],scdec35[i])/3600.
        d50[i]=separation(ra[i],dec[i],scra50[i],scdec50[i])/3600.
;     radec2sclonlat,ft2[w].ra_npole,ft2[w].dec_npole,ft2[w].ra_scz,ft2[w].dec_scz,sclon,sclat
;     stop
     endif 
  endfor 

  plotsym,0,2
  plot,indgen(ngrb)+1,d,yrange=[0,200],/ysty,xtitle='GRB #',ytitle='theta',psym=8 ;10
  oplot,indgen(ngrb)+1,d35,color=!red,psym=2   ;10,line=2
  oplot,indgen(ngrb)+1,d50,color=!green,psym=2 ;10,line=2
  if not keyword_set(gbm) then oplot,[12.5,12.5],[0,200],line=2 else $
     oplot,[250,250],[0,200],line=2
  oplot,[0,5000],[70,70],line=2
  if not keyword_set(gbm) then $
     for i=0,ngrb-1 do xyouts,i+1.2,140,grb[i],orient=90,charsize=0.9 else $
        for i=0,ngrb-1,100 do xyouts,i+1.2,140,grb[i],orient=90,charsize=0.9

  legend,['Real theta','w/ rock=35','w/ rock=50'],box=0,/right,/center,textcolor=[!p.color,!red,!green]

  endplot
  spawn,'convert '+outfile+'.ps '+outfile+'.pdf'
;  colprint,grb,rock,d,theta

  stop
  return
end
