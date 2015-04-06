pro grb101225a
  
  ;;; UVOT LC for GCN report (Stephen's LCs)

  begplot,name='~/Desktop/GRB101225A/uvotlc.ps',/land,font='helvetica'

;  goto,skip
  readcol,'~/Desktop/GRB101225A/UVOT/grb101225a_uvot_mag_lc.dat',filter1,tstart1,tstop1,mag,err,format='(a,f,f,f,f)'
  readcol,'~/Desktop/GRB101225A/UVOT/grb101225a_uvot_mag_lc.dat',filter2,tstart2,tstop2,ul,format='(a,f,f,f)'

  dont_match,mag,ul,dm1,dm2
  filter2=filter2[dm2]
  tstart2=tstart2[dm2]
  tstop2=tstop2[dm2]
  ul=ul[dm2]

  filter=[filter1,filter2]
  tstart=[tstart1,tstart2]
  tstop=[tstop1,tstop2]
  time=(tstop-tstart)/2.+tstart
  bin=(tstop-tstart)/2.
  mag=[mag,ul]
  err=[err,replicate(0.,n_elements(ul))]
  
  n=n_elements(filter)
  f=['v','b','u','uvw1','uvw2','uvm2']
;  multiplot,[1,6],/init
  plotsym,1,2
  erase
  for i=4,4 do begin
;     multiplot
     xrange=[1000,1e6]
     yrange=[25,18]
     if i eq 4 then xtitle='Time since BAT trigger (s)'
;     if i eq 3 then ytitle='Mag' else ytitle=''
     ytitle='w2 Magnitude'
     plot,xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xsty,/ysty,/xlog,xtitle=xtitle,ytitle=ytitle;,ytickv=[18,20,22,24],yticks=3
     w=where(filter eq f[i] and err ne 0,nw)
     if nw gt 0 then begin 
        oploterror,time[w],mag[w],bin[w],err[w],/nohat,psym=2
        colprint,filter[w],time[w],mag[w],err[w]
     endif 
     w=where(filter eq f[i] and err eq 0,nw)
     if nw gt 0 then oploterror,time[w],mag[w],bin[w],replicate(0.,nw),psym=8,/nohat
     colprint,filter[w],time[w],mag[w],err[w]
;     xyouts,2000,24,f[i]
  endfor 
  multiplot,/reset,/init
  endplot
;  skip:

;  stop
  goto,skip
  ;;; Sam's LC
  
  readcol,'~/Desktop/GRB101225A/UVOT/CTOnormalize_including_shifted.txt',time,bin,rate,raterr
  f=['v','b','u','w1','m2','w2']
  filter=[replicate(f[0],17),replicate(f[1],33-16),replicate(f[2],44-33),replicate(f[3],55-44),replicate(f[4],68-55),replicate(f[5],87-68)]
  zpt=[17.89,19.11,18.34,17.49,16.82,17.35,20.29]
  multiplot,[1,6],/init
  plotsym,1,2
  erase
  for i=0,5 do begin
     multiplot
     xrange=[1000,1e6]
     yrange=[25,18]
     if i eq 5 then xtitle='Time since BAT trigger (s)'
     if i eq 3 then ytitle='Mag' else ytitle=''
     plot,xrange,yrange,xrange=xrange,yrange=yrange,/nodata,/xsty,/ysty,/xlog,yticks=3,ytickv=[18,20,22,24],xtitle=xtitle,ytitle=ytitle
     w=where(filter eq f[i] and rate gt 0,nw)
     mag=-2.5*alog10(rate[w])+zpt[i]
     err=-2.5*alog10(rate[w]+raterr[w])+zpt[i]-mag
     if nw gt 0 then begin 
        oploterror,time[w],mag,bin[w],err,/nohat,psym=2
        colprint,filter[w],time[w],mag,err
     endif 
;     w=where(filter eq f[i] and err eq 0,nw)
;     if nw gt 0 then oploterror,time[w],mag[w],bin[w],replicate(0.,nw),psym=8,/nohat
;     colprint,filter[w],time[w],mag[w],err[w]
     xyouts,2000,24,f[i]
  endfor 
  multiplot,/reset,/init
  endplot
  skip:

  ;;; BAT 
  readcol,'~/Desktop/GRB101225A/batlc.dat',start,dur,cr,crerr

  bin=dur/2.
  time=start+bin
  
  xrange=[-1000,3000]
  yrange=[0.,5e-3]
  begplot,name='~/Desktop/GRB101225A/batlc.ps',/land
  ploterror,time,cr,bin,crerr,xrange=xrange,yrange=yrange,xtitle='Time since BAT trigger (s)',ytitle='14-50 keV Count Rate (counts s!U-1!N det!U-1!N)',/ysty,/nohat,psym=3
  endplot
  
stop
  return
end 
