pro bib_authors

;;; go to Fermi bib search page and search by year or 6-month
;;; interval, click go to ADS
;;; at bottom of ADS search, select all records, click return custom
;;; format, save to file, %1000M, retreive

  cd,'~/Fermi/Bibliography/auth_year'
  nyears=8
  years=indgen(nyears)+2008
  nauth=intarr(nyears)

  for i=0,n_elements(years)-1 do begin
     files=file_search(ntostr(years[i])+'A+A*txt')
     authors=''
     for j=0,n_elements(files)-1 do begin
        readcol,files[j],alist,format='a',delim='%',/silent
        for k=0,n_elements(alist)-1 do begin
           a=strsplit(alist[k],'and',/ex,/regex)
           for l=0,n_elements(a)-1 do begin
              a1=strsplit(a[l],',',/ex)
              authors=[authors,a1]
           endfor 
        endfor   
     endfor
     authors=authors[1:*]
     authors=strtrim(authors(rem_dup(strtrim(authors,2))),2)
;     w=where(strlen(authors) gt 2)
;     authors=authors[w]
     w=where(strlen(authors) gt 2 and strtrim(authors,2) ne '' and strpos(authors,'&') eq -1 and strpos(authors,'Collaboration') eq -1 and strpos(authors,'collaboration') eq -1 and strpos(authors,';') eq -1 and strpos(authors,'(') eq -1 and strpos(authors,'Team') eq -1 and strtrim(authors,2) ne 'von' and strtrim(authors,2) ne 'van' and strtrim(authors,2) ne 'den')
     authors=authors[w]
     if i eq 0 then begin 
        nauth[i]=n_elements(authors) 
        newauth=authors
        totauth=authors
     endif else begin
        na=n_elements(totauth)
        totauth=[authors0,authors]
        totauth=totauth[rem_dup(totauth)]
        nauth[i]=n_elements(totauth)-na;total(nauth[0:i-1])
        dont_match,authors0,authors,dm1,dm2
        newauth=authors[dm2]
     endelse 
     print,nauth[i]
     authors0=totauth
     print,years[i]
     help,newauth
  endfor 

  print,nauth
  print,cumulative(nauth)
  p=barplot(years+0.5,cumulative(nauth),xrange=[2008,2016],yrange=[0,6000],ytitle='Cumulative Unique           Authors',xminor=0,xtitle='Year',position=[0.18,0.15,0.96,0.6],yminor=0,fill_color="green",xtickval=2008+indgen(8),width=0.8,font_size=14,thick=2)
  t=text(0.078,0.42,'Fermi',font_style=2,font_size=14,orientation=90)
  p.save,'~/Fermi/Senior_Review/SR2016/cumulative_bib_authors.png'
  p.close

  p=barplot(years+0.5,nauth,xrange=[2008,2016],yrange=[0,1000],ytitle='Unique Fermi Authors',xminor=0,xtitle='Year',position=[0.18,0.15,0.96,0.6],yminor=0,fill_color="blue",xtickval=2008+indgen(8),width=0.8,color='blue',font_size=14)
  p.save,'~/Fermi/Senior_Review/SR2016/bib_authors.pdf'
  p.close

  writecol,'~/Fermi/Senior_Review/SR2016/totauthors.txt',totauth,format='(a)'

stop

  return
end 

pro dm_plot

  xrange=[5,1e4]
  yrange=[1d-28,5d-24]
  ;;; thermal relic
  p=plot(xrange,yrange,/nodata,font_size=14,xtitle='Dark Matter WIMP Particle Mass (GeV)',ytitle='Annihilation Cross Section (cm!U3!N s!U-1!N)',/xlog,/ylog,xrange=xrange,yrange=yrange,thick=2,position=[0.15,0.1,0.93,0.7])
  
  mass = [1.00e-01, 1.78e-01, 3.16e-01, 5.62e-01, 1.00e+00, 1.78e+00,$
          3.16e+00, 5.62e+00, 1.00e+01, 1.78e+01, 3.16e+01, 5.62e+01,$
          1.00e+02, 1.78e+02, 3.16e+02, 5.62e+02, 1.00e+03, 1.78e+03,$
          3.16e+03, 5.62e+03, 1.00e+04, 5.00e+04]
  sigmav = [4.8, 4.9, 5.1, 5.0, 4.7, 4.5, 3.9, 2.8, 2.5,$
            2.3, 2.2, 2.2, 2.2, 2.3, 2.3, 2.3, 2.3, 2.3,$
            2.3, 2.3, 2.4, 2.4]*1e-26
  p1=plot(mass,sigmav,linestyle=':',/overplot,color='slate gray',thick=2)
  t=text(380,1.1e-26,'Thermal Relic Cross Section',color='slate gray',font_size=12,/data)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/daylan2014_v2_bb_2s',m,sig,format='(f,f)'
  p2=plot(m,sig,color='cyan',/overplot,thick=2)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/gordon2013_bb_2s',m,sig,format='(f,f)'
  p3=plot(m,sig,color='blue',/overplot,thick=2)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/calore2014_bb_2s',m,sig,format='(f,f)'
  p4=plot(m,sig,color='green',/overplot,thick=2)

  ;Abazajian
  x=39.4
  y=5.1e-26*0.562
  xerr=[-7.9,7.9]+x
  yerr=[-2.4e-26,2.4e-26]*0.562+y
  p5a=plot([x,x],yerr,color='orange',/overplot,thick=2)
  p5b=plot(xerr,[y,y],color='orange',/overplot,thick=2)

  t=text(8,2.5e-25,'Galactic Center Excess',font_size=12,/data)
  t=text(7,1.3e-25,'Dark Matter Interpretations',font_size=12,/data)
  a=arrow([23,33],[9e-26,4e-26],/data,/current,head_size=0.5,thick=2)

;  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/LAT_xcorr_all_bb',m,sig,format='(f,f)'
  m1=[6, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
  s1=[5.2418e-27, 5.1845e-27, 8.284e-27, 1.3061e-26, 2.1326e-26, 4.5579e-26,8.9707e-26, 1.8736e-25, 5.6658e-25, 1.4274e-24, 3.86e-24]

;  m1=[5.987513,10.07233,25.189621,50.743637,100.65425,249.14426,499.3147,965.25604,2662.1172,4964.0947,9897.535]
;  m2=[5.987513,10.020595,25.319674,50.223694,101.17393,250.43057,501.89264,1000.68604,2502.5889,4989.724,9948.636]
;  s1=[5.2330294E-27,5.1204807E-27,8.261223E-27,1.3328398E-26,2.1738662E-26,4.5527693E-26,8.9328735E-26,1.791221E-25,6.2530125E-25,1.4131253E-24,3.8002447E-24]
;  s2=[1.206115E-27,1.2326255E-27,2.0323905E-27,3.2435422E-27,5.8977533E-27,1.5689035E-26,3.3216994E-26,7.5067455E-26,2.32519E-25,5.6701996E-25,1.4920617E-24]
  m2=[6, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
  s2=[1.21e-27, 1.22328e-27, 2.03084e-27, 3.20988e-27, 5.8286e-27,1.55624e-26,3.302e-26, 7.41404e-26, 2.31368e-25, 5.682e-25, 1.510e-24]
  p6a=plot(m1,s1,/overplot,thick=2)
  p6b=plot(m2,s2,/overplot,thick=2,linestyle='--')

;  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/silverwood2014_cta_einasto_bb',m,sig,format='(f,f)'
;  p4=plot(m,sig,color='tomato',/overplot,thick=2,linestyle='--')
;  t=text(160,1e-25,'CTA Galactic Center (100h)',color='tomato',/data,orientation=28)

  t=text(250,6e-26,'LAT dSph Limit (6 years)',/data,orientation=27)
  t=text(750,2.4e-26,'LAT dSph Limit (12 years)',/data,orientation=29)
  for i=1,n_elements(m1)-2,2 do a=arrow([m1[i],m1[i]],[s1[i]*0.9,s2[i]*1.2],/data,/current,color='gray',head_size=0.5,thick=2)

  t=text(4.8e3,1.5e-28,'b',/data,font_style=2,font_size=18)
  t=text(5e3,1.5e-28,'!9`!S!R!Xb',/data,font_style=2,font_size=18)

  p.save,'~/Fermi/Senior_Review/SR2016/dm_plot.png'
  p.close

;;;; tau tau bar

  xrange=[5,1e4]
  yrange=[1d-28,5d-24]
  ;;; thermal relic
  p=plot(xrange,yrange,/nodata,font_size=14,xtitle='Dark Matter WIMP Particle Mass (GeV)',ytitle='Annihilation Cross Section (cm!U3!N s!U-1!N)',/xlog,/ylog,xrange=xrange,yrange=yrange,thick=2,position=[0.15,0.1,0.93,0.7])
  
  mass = [1.00e-01, 1.78e-01, 3.16e-01, 5.62e-01, 1.00e+00, 1.78e+00,$
          3.16e+00, 5.62e+00, 1.00e+01, 1.78e+01, 3.16e+01, 5.62e+01,$
          1.00e+02, 1.78e+02, 3.16e+02, 5.62e+02, 1.00e+03, 1.78e+03,$
          3.16e+03, 5.62e+03, 1.00e+04, 5.00e+04]
  sigmav = [4.8, 4.9, 5.1, 5.0, 4.7, 4.5, 3.9, 2.8, 2.5,$
            2.3, 2.2, 2.2, 2.2, 2.3, 2.3, 2.3, 2.3, 2.3,$
            2.3, 2.3, 2.4, 2.4]*1e-26
  p1=plot(mass,sigmav,linestyle=':',/overplot,color='slate gray',thick=2)
  t=text(300,1.e-26,'Thermal Relic Cross Section',color='slate gray',font_size=12,/data)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/daylan2014_v2_taumix_2s',m,sig,format='(f,f)'
  p2=plot(m,sig,color='cyan',/overplot,thick=2)

;  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/gordon2013_bb_2s',m,sig,format='(f,f)'
;  p3=plot(m,sig,color='blue',/overplot,thick=2)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/calore2014_tautau_2s',m,sig,format='(f,f)'
  p4=plot(m,sig,color='green',/overplot,thick=2)

  ;Abazajian
  x=9.43
  y=0.51e-26*0.562
  xerr=[-1.2,1.2]+x
  yerr=[-0.24e-26,0.24e-26]*0.562+y
  p5a=plot([x,x],yerr,color='orange',/overplot,thick=2)
  p5b=plot(xerr,[y,y],color='orange',/overplot,thick=2)

  t=text(10,4e-25,'Galactic Center Excess',font_size=12,/data)
  t=text(9,2e-25,'Dark Matter Interpretations',font_size=12,/data)
  a=arrow([23,14],[1.5e-25,8e-27],/data,/current,head_size=0.5,thick=2)

  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/LAT_xcorr_all_bb',m,sig,format='(f,f)'
  m1=[5.987513,10.07233,25.189621,50.743637,100.65425,249.14426,499.3147,965.25604,2662.1172,4964.0947,9897.535]
 m2=[5.987513,10.020595,25.319674,50.223694,101.17393,250.43057,501.89264,1000.68604,2502.5889,4989.724,9948.636]
;  s1=[5.2330294E-27,5.1204807E-27,8.261223E-27,1.3328398E-26,2.1738662E-26,4.5527693E-26,8.9328735E-26,1.791221E-25,6.2530125E-25,1.4131253E-24,3.8002447E-24]
;  s2=[1.206115E-27,1.2326255E-27,2.0323905E-27,3.2435422E-27,5.8977533E-27,1.5689035E-26,3.3216994E-26,7.5067455E-26,2.32519E-25,5.6701996E-25,1.4920617E-24]
  m1=[2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
  s1=[3.2648e-27, 2.8453e-27, 4.1731e-27, 7.4512e-27, 1.3734e-26, 2.7774e-26,8.4041e-26, 2.2413e-25, 7.1085e-25, 4.4645e-24, 2.2199e-23, 1.2332e-22]
  m2=[2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
  s2=[ 7.70e-28, 7.07e-28, 9.82e-28, 2.11e-27, 4.93e-27, 1.08e-26, 3.04e-26, 8.30e-26, 2.36e-25, 1.32e-24, 6.13e-24, 3.36e-23]

  p6a=plot(m1,s1,/overplot,thick=2)
  p6b=plot(m2,s2,/overplot,thick=2,linestyle='--')

;  readcol,'~/Fermi/Senior_Review/SR2016/DM_wp/data/dm_limits/silverwood2014_cta_einasto_tautau',m,sig,format='(f,f)'
;  p4=plot(m,sig,color='tomato',/overplot,thick=2,linestyle='--')
;  t=text(430,2.5e-26,'CTA Galactic Center (100h)',color='tomato',/data,orientation=10)

  t=text(150,7e-26,'LAT dSph Limit (6 years)',/data,orientation=33)
  t=text(700,6e-26,'LAT dSph Limit (12 years)',/data,orientation=36)
  for i=3,n_elements(m1)-3,2 do a=arrow([m1[i],m1[i]],[s1[i]*0.9,s2[i]*1.2],/data,/current,color='gray',head_size=0.5,thick=2)

  t=text(4.8e3,1.5e-28,!tsym.tau+!tsym.tau,/data,font_style=2,font_size=20)
;  t=text(5e3,1.5e-28,'!9`!S!R!X'+!tsym.tau,/data,font_style=2,font_size=20)

  p.save,'~/Fermi/Senior_Review/SR2016/dm_plot_tt.png'
  p.close


  ;;;; time mass plot

  xrange=[6,15]
  yrange=[230,420]
  p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle='Observation Time (years)',ytitle='Maximum WIMP Mass Limit (GeV)',position=[0.15,0.15,0.93,0.7],font_size=14)
  t=[6.0160713,6.4982142,6.9964285,7.4785714,8.008928,8.539286,9.010715,9.492857,10.007143,10.521428,11.014286,11.517858,12.0,12.5035715,13.017858,13.564285,14.014286,14.55,14.989285]
  m=[241.34616,254.03847,265.57693,277.1154,288.07693,300,309.42307,319.42307,329.80768,339.03845,347.8846,356.73077,364.80768,372.5,380.19232,388.26923,394.80768,401.34616,407.1154]
  p2=plot(t,m,color='red',thick=4,/overplot)
  poly=polygon([6,t,15,15,6],[0,m,max(m),0,0],/data,fill_color='tomato',color='red')
  t=text(10,280,'Excluded WIMP Mass',/data,font_size=18)
  t=text(6.5,400,'b',/data,font_style=2)
  t=text(6.58,400,'!9`!S!R!Xb',/data,font_style=2)

  p.save,'dm_limit_time.pdf'
  p.close


stop
  return
end 
pro data_latency

  cd,'~/Fermi/Senior_Review/SR2016'
  readcol,'data_latency_v2.dat',met,slac,nasa,format='(d,d,d)',skip=1

  n=n_elements(met)
  year=fltarr(n)
  for i=0,n-1 do begin 
     date=met2date(met[i],/fermi)
     date=strsplit(date,'-:',/ex)
     year[i]=date[0]*1.+date[1]/365.+date[2]/24./365.+date[3]/60/24./365.
  endfor 


  bin=3./365.

  plothist,year,x,y,bin=bin,/noplot
  n=n_elements(x)
  t=dblarr(n)
  s=dblarr(n)
  na=dblarr(n)
  for i=0,n-1 do begin
     w=where(year ge x[i]-bin/2. and year lt x[i]+bin/2.,nw)
     if nw gt 1 then begin 
        t[i]=median(year[w])
        s[i]=median(slac[w])
        na[i]=median(nasa[w])
     endif
  endfor 

  p=plot(year,nasa+slac,symbol='.',xrange=[2015,2016],yrange=[0,20],linestyle='none',xtitle='Year',ytitle='Data Latency (hours)',color='tomato',font_size=14)
;  p1=plot(year,nasa,symbol=3,color='tomato',/overplot,linestyle='none')
  p2=plot(t,na+s,/overplot,color='red',thick=2)
  p3=plot([2015.75,2015.75],[0,20],linestyle='--',/overplot,thick=2)

;  p3=plot(t,na,color='tomato',/overplot,thick=2)
;  t1=text(0.75,0.8,'LAT Only',font_size=14,color='gray')
;  t2=text(0.75,0.75,'Total',color='red',font_size=14)
;  oplot,t,s,color=!gray
;  oplot,t,na,color=!salmon
;  plot,t,s,xrange=[2015,2016],yrange=[0,10]
;  oplot,t,na,color=!red

;  k=get_kbrd(10)
;  if k eq 's' then stop
  p.save,'data_latency.png'
  p.close

  stop
  

  return
end 
pro plot_lmc_binary

  cd,'~/Fermi/Senior_Review/SR2016'

;  readcol,'lmc_aperture_fold.dat',time1,flux1,fluxerr1,timeerr1,format='(f,f,f,f)'
  readcol,'lmc_likelihood_fold.dat',time2,flux2,fluxerr2,timeerr2,ts,format='(f,f,f,f,f)'

  fact=1e8

;  t=[time1,time1+1]
;  f=[flux1,flux1]*fact
;  terr=[timeerr1,timeerr1]
;  ferr=[fluxerr1,fluxerr1]*fact

  t=[time2,time2+1]
  f=[flux2,flux2]*fact
  terr=[timeerr2,timeerr2]
  ferr=[fluxerr2,fluxerr2]*fact

  p=errorplot(t,f,terr,ferr,errorbar_capsize=0,xtitle='Phase',ytitle='Flux (>100 MeV) (10!U-8!N ph cm!U-2!N s!U-1!N)',linestyle='none',symbol='.',sym_size=10,color='blue',sym_filled=1,font_size=14,position=[0.13,0.4,0.92,0.9])

;  p2=errorplot(time2,flux2*f,timeerr2,fluxerr2*f,color='red',/overplot,linestyle='none')
  k=get_kbrd(10)
  if k eq 's' then stop
  p.save,'lmc_binary.pdf'
  p.close

  return
end

pro plot_j1023

  cd,'~/Fermi/Senior_Review/SR2016'

  readcol,'J1023.6+0040Flux_month100MeV.txt',t_met,dt_met,t_mjd,dt_mjd,photon_flux,dph_flux,energy_flux,denergy_flux,TS,format='(l,l,d,d,d,d,d,d,d)',skip=2

  ul=where(dph_flux eq 0,nul)
  det=where(dph_flux ne 0,ndet)
  year=fltarr(n_elements(t_met))
  dyear=fltarr(n_elements(t_met))
  for i=0,n_elements(t_met)-1 do begin 
     date=met2date(t_met[i],/fermi)
     date=strsplit(date,'-',/ex)
     year[i]=date[0]*1.+date[1]/365.
     dyear[i]=dt_met[i]*1./(86400.*365.)
  endfor 
  
;  f=1d8
;  flux=photon_flux
;  dflux=dph_flux
  f=1d11
  flux=energy_flux
  dflux=denergy_flux

  p=errorplot(year[det],flux[det]*f,dyear[det],dflux[det]*f,errorbar_capsize=0,xtitle='Year',ytitle='Flux (>100 MeV) (10!U-11!N erg cm!U-2!N s!U-1!N)',linestyle='none',symbol='.',sym_size=10,color='blue',sym_filled=1,font_size=14,position=[0.1,0.1,0.95,0.7],yminor=1)

  for i=0,nul-1 do begin
     a=arrow([year[ul[i]],year[ul[i]]],[flux[ul[i]]*f,flux[ul[i]]*f-0.3],/data,/current,color='steel blue',head_size=0.5)
     p2=plot(year[ul[i]]+[-dyear[ul[i]],dyear[ul[i]]],[flux[ul[i]]*f,flux[ul[i]]*f],/overplot,color='steel blue')
  endfor 
  w=where(year[det] lt 2013.5)
  m1=mean(flux[det[w]])
  w=where(year[det] ge 2013.5)
  m2=mean(flux[det[w]])

  l1=plot([2008.5,2013.5],[m1,m1]*f,linestyle='--',color='red',/overplot,thick=2)
  l2=plot([2013.5,2016],[m2,m2]*f,linestyle='--',color='red',/overplot,thick=2)
  l3=plot([2013.48,2013.48],[0,5],linestyle=':',color='dark slate grey',/overplot,thick=3)
  t1=text(2009,1.95,'Radio Pulsations Vanished',/data,color='dark slate grey',font_size=14)
  a1=arrow([2012.6,2013.3],[2,2],/data,/current,color='dark slate grey',thick=2)
  t2=text(2009,4,'Average '+!tsym.gamma+'-ray Flux ',font_size=14,/data,color='red')
  t2=text(2009,3.7,'Before/After Transition',font_size=14,/data,color='red')

  p.save,'J1023_plot.pdf'
  p.close

stop
  return
end 

pro theses
  
  readcol,'~/Fermi/Senior_Review/SR2016/Fermi_thesis_information_2015_1.csv',year,skip=1,format='(i)'
  plothist,year,x,y,/noplot
  b=barplot(x,y,ytitle='       Ph.D. Theses per year',fill_color='red',xrange=[2000,2016],yrange=[0,60],xmajor=9,xminor=0,position=[0.12,0.25,0.93,0.7],yminor=1,xtitle='Year',font_size=14)
;  xaxis=axis(0,location=[0,60],/data,coord_transform=[-2008.44,1.],title='Mission Year',textpos=1,tickdir=1,minor=1)
;  xaxis=axis('X',location=[0,0],/data,title='Year',minor=1)

  p=plot([2008.44,2008.44],[0,60],/overplot,linestyle=':')
  t=text(2008.2,20,'Launch',orientation=90,/data)
  t=text(0.049,0.225,'Fermi',font_style='it',orientation=90,font_size=14)

  b.save,'~/Fermi/Senior_Review/SR2016/theses_plot.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close

return
end 

pro plot_j2021

  cd,'~/Fermi/Senior_Review/SR2016'
  readcol,'J2021.5+4026Flux_month1GeV.txt',met,metbin,mjd,mjdbin,phflux,phfluxerr,enflux,enfluxerr,ts,format='(l,d,d,f,d,d,d,d,f)',skip=1

  year=fltarr(n_elements(met))
  for i=0,n_elements(met)-1 do begin 
     date=met2date(met[i],/fermi)
     sep=strsplit(date,'-:',/ex)
     year[i]=sep[0]*1.+sep[1]/365.
  endfor 
  yearbin=metbin/(86400.*365d)

  p=errorplot(year,enflux*1d10,yearbin,enfluxerr*1d10,ytitle='        Flux (10!U-10!N erg cm!U-2!N s!U-1!N)',linestyle='none',errorbar_capsize=0.,position=[0.12,0.5,0.95,0.9],xrange=[2009,2016],/xstyle,xtickname=['','','','',''],symbol='.',sym_size=10.,/sym_filled,sym_color='blue',errorbar_color='blue',ytickval=[2.5,3,3.5,4,4.5,5.0],font_size=15,yminor=0)

  p2=plot([2011.8,2011.8],[2,5],/overplot,color='red',linestyle='--',thick=2.)
  p2=plot([2014.75,2014.75],[2,5],/overplot,color='red',linestyle='--',thick=2.)


  readcol,'MonitorResult_delta4320000.0.txt',n,t1_MET,t2_MET,dt,T0_MET,T0_MJD,F0,F1,F0exp,F0res,bestZ2n,format='(i,l,d,f,d,d,d,d,d,d,d)',skip=1

  year=fltarr(n_elements(t1_met))
  year0=year
  for i=0,n_elements(t1_met)-1 do begin 
     date=met2date(t1_met[i],/fermi)
     sep=strsplit(date,'-:',/ex)
     year[i]=sep[0]*1.+sep[1]/365.

     date=met2date(t0_met[i],/fermi)
     sep=strsplit(date,'-:',/ex)
     year0[i]=sep[0]*1.+sep[1]/365.

  endfor 
  yearbin=(t2_met-t1_met)/(86400.*365d*2.)
;  yearbin=dt/365d.
  year=year+yearbin
  
  k=6.9e-8
  fkt=((f0+k*(t0_mjd))-3.772855)*1d6
  p1=plot(year,fkt,xtitle='Year',ytitle='Frequency Drift (10!U-6!N Hz)',linestyle='none',position=[0.12,0.1,0.95,0.5],xrange=[2008,2016],yrange=[2,5],/xstyle,/current,symbol='.',sym_size=10.,/sym_filled,sym_color='blue',ytickval=[2.5,3,3.5,4,4.5],font_size=15,yminor=0)
  t1=text(0.16,0.53,'Flux/Timing Changes',color='red',font_size=14)
  a1=arrow([0.45,0.51],[0.54,0.54],color='red',/current,thick=2)

  p2=plot([2011.8,2011.8],[2,5],/overplot,color='red',linestyle='--',thick=2.)
  p2=plot([2014.75,2014.75],[2,5],/overplot,color='red',linestyle='--',thick=2.)

  
  p.save,'PSR_J2021_plot.pdf'
  p.close

  stop
  return 
end 
pro plot_pg1553

  cd,'~/Fermi/Senior_Review/SR2016'
  
  readcol,'PG1553p113_like_lc_judith.dat_pass845days',time,flux,err_flux,half_bin,TS,Npred,format='(d,d,d,f,f,f)'
  ;;time (mjd) flux (ph/cm2/s) err_flux (ph/cm2/s) half_bin (day) TS  Npred
  
  daycnv,time+2400000.5,yr,mn,day,hr
  year=yr+mn/12.+day/365.+hr/(24.*365.)
  yearbin=half_bin/365.

  p=errorplot(year,flux*1e8,yearbin,err_flux*1e8,symbol='dot',errorbar_capsize=0,linestyle='none',sym_size=10.,/sym_filled,xtitle='Year',ytitle='Flux (>100 MeV) (10!U-8!N cm!U-2!N s!U-1!N)',color='blue',errorbar_color='blue',pos=[0.13,0.3,0.93,0.9],font_size=14)

  p.save,'PG1553_100MeV_45daybin.pdf'
  p.close
stop
  return
end 

pro plot_3c279

  cd,'~/Fermi/Senior_Review/SR2016'

  readcol,'3C279_2015June_3min.txt',bin3,ts3,flux3,fluxerr3,ind3,inderr3,npred,startmet3,endmet3,format='(i,d,f,f,f,f,f,d,d)'
  readcol,'3C279_2015June_5min.txt',bin5,ts5,flux5,fluxerr5,ind5,inderr5,npred,startmet5,endmet5,format='(i,d,f,f,f,f,f,d,d)'

  tstart=date2met('2015-06-16-00:00:00',/fermi)
  t3=((endmet3-startmet3)/2.+startmet3-tstart)/60.-115
  t5=((endmet5-startmet5)/2.+startmet5-tstart)/60.-115
  p=errorplot(t3,flux3/1e-5,replicate(1.5,n_elements(t3)),fluxerr3/1e-5,xtitle='Time (minutes)',ytitle='Flux (>100 MeV) (10!U-5!N ph cm!U-2!N s!U-1!N)',symbol='dot',errorbar_capsize=0,linestyle='none',sym_size=10.,/sym_filled,xrange=[0,30],yrange=[1,7],/xstyle,yminor=4,color='blue',errorbar_color='blue',font_size=14,position=[0.1,0.15,0.95,0.65]);,/ylog,yrange=[1e-7,1e-4])

  t=findgen(301)/10.
  s=spline(t3,flux3/1e-5,t)

  p2=plot(t,s,color='blue',/overplot,linestyle='--')
;  p2=errorplot(t5,flux5,replicate(2.5,n_elements(t5)),fluxerr5,/overplot,/current,symbol='dot',sym_color='red',errorbar_capsize=0,errorbar_color='red',linestyle='none',sym_size=10.,/sym_filled)
;  p3=plot([0,6],[1e-6,1e-6],/overplot,line='--')

;; make x-axis in minutes

  k=get_kbrd(10)
  p.save,'3C279_1orbit.pdf'
  p.close
  if k eq 's' then stop

  return
end 

pro lgplot


  outdir='~/LIGO/GBM_LIGO/'
  sim=mrdfits(outdir+'sim_results.fits',1)
  w=where(sim.ligo_area[0] ne 0)
  sim=sim[w]
  plothist,alog10(sim.ligo_area[0]),lx,ly,bin=0.1,/noplot
  plothist,alog10(sim.gbm_area[0]),gx,gy,bin=0.1,/noplot
  plothist,alog10(sim.ligo_gbm_area[0]),lgx,lgy,bin=0.1,/noplot
;  plotloghist,sim.ligo_area[0],lx,ly,yrange=[0,250],bin=0.1,/noplot
;  plotloghist,sim.gbm_area[0],gx,gy,bin=0.1,/noplot;,color=!green,/overplot
;  plotloghist,sim.ligo_gbm_area[0],lgx,lgy,bin=0.1,/noplot;,color=!red,/overplot

;  num=[112,165,182,199,184,945,965]
  num=945
;  pos=[0.65,0.5,0.95,0.8]
  pos=[0.15,0.45,0.5,0.8]
  ims=outdir+'ligo_gbm_healpix_plot_'+ntostr(num)+'.png'
;  ims='~/Fermi/Senior_Review/SR2016/ligo_healpix_map_197_blue.png'
  v=['1','10','10!U2!N','10!U3!N']
  yv=numdec([0.001,50,100,150,200.,250]/1000.,2)

  for i=0,n_elements(ims)-1 do begin 
     im=image(ims[i])

;     poly=polygon(pos[[0,0,2,2,0]],pos[[1,3,3,1,1]],fill_color='white',color='white',/fill_background,/current)
     p=barplot(lx,ly,/current,fill_color='blue',position=pos,width=1,xtitle='68% Probability Area (deg!U2!N)',ytitle='Fraction',xtickname=v,xtickval=[indgen(4)],transparency=10,linestyle='none',xrange=[1,3],ytickname=yv,ytickvalue=[0,50,100,150,200,250],yrange=[0,0.25],yminor=1)
     p=barplot(gx,gy,/overplot,fill_color='green',position=pos,width=1,transparency=40,linestyle='none')
     p=barplot(lgx,lgy,/overplot,fill_color='red',position=pos,width=1,transparency=40,linestyle='none')
     t=text(0.17,0.75,'LIGO/Virgo',color='blue',/current,font_size=12)
     t=text(0.17,0.72,'GBM',color='green',/current,font_size=12)
     t=text(0.17,0.69,'GBM+LIGO/Virgo',color='red',/current,font_size=12)
     t=text(0.65,0.5,'Typical GBM',/current,font_size=12,color='forest green')
     t=text(0.645,0.47,'Error Contour',/current,font_size=12,color='forest green')
     t=text(0.5,0.34,'Typical Simulated',/current,font_size=11,color='medium blue')
     t=text(0.46,0.31,'LIGO/Virgo Error Region',/current,font_size=11,color='medium blue')

     im.save,outdir+'ligo_gbm_healpix_plot_'+ntostr(num[i])+'_wdist.png'
;     k=get_kbrd(10)
;     if k eq 's' then stop
     im.close
  endfor 
  spawn,'cp '+outdir+'ligo_gbm_healpix_plot_'+ntostr(num[i-1])+'_wdist.png ~/Fermi/Senior_Review/SR2016/'

  return
end

pro ligo_gbm_plot

  ;; HIDL
  ;; plot typical ligo 2016 error contour
  ;; overplot typical GBM contour
  ;; show inset with distribution plots of 50%/90% sq deg for LIGO
  ;; alone, GBM alone, convolution
  ;; issues - doesn't line up - projection?
  ;; get LIGO numbers from Leo's paper?

;  outdir='~/Fermi/Senior_Review/SR2016/'
  dir='~/Fermi/gbmtrig/'
  gbmfiles=file_search(dir+'glg_locprob_all*fit')
  ngbm=n_elements(gbmfiles)
  ;; need to filter on short bursts - need to update locprob list

  ldir='~/LIGO/2016_fits/'
;  ligofiles=file_search(ldir+'*/bayestar*fits')
;  nligo=n_elements(ligofiles)
  readcol,ldir+'2016_inj.txt',evid,simid,mjd,ra,dec,inc,format='(a,l,d,f,f,f)'
  w=where(inc lt 30 or inc gt 150)
  ligofiles=ldir+strtrim(evid[w],2)+'/bayestar.fits'
;  ligoimfiles=ldir+strtrim(evid[w],2)+'_lin.png'
  nligo=n_elements(ligofiles)
  ;;; filtering on inc angle <30 or >150
  
  outdir='~/LIGO/GBM_LIGO/'

  nsim=1000
  if not exist(outdir+'sim_results.fits') then begin 
     sim=create_struct('simnum',0,'ligofile','','gbmfile','',$
                       'ligo_area',fltarr(3),'gbm_area',fltarr(3),$
                       'ligo_gbm_area',fltarr(3))
     sim=replicate(sim,nsim)
     mwrfits,sim,outdir+'sim_results.fits',/create
  endif
  sim=mrdfits(outdir+'sim_results.fits',1)

  done=file_search(outdir+'ligo_healpix_map_*.fits')
  if done[0] ne '' then begin 
     dd=intarr(n_elements(done))
     for i=0,n_elements(done)-1 do begin
        d=strsplit(done[i],'_.',/ex)
        dd[i]=d[4]
     endfor
  endif else dd=0

  md=max(dd)
  for s=md,nsim-1 do begin
     time=systime(1)
     l=randomu(seed,1.)*nligo
     g=randomu(seed,1.)*ngbm
     ligofile=ligofiles[l]
;     ligoimfile=ligoimfiles[l]
     gbmfile=gbmfiles[g]
     sim[s].ligofile=ligofile
     sim[s].gbmfile=gbmfile

     read_fits_map,ligofile,hmap,nside=nside
     print,nside
     hdr=headfits(ligofile,exten=1)
     rn=sxpar(hdr,'ORDERING')
     if strtrim(rn,2) eq 'NESTED' then nest=1 else nest=0
     if strtrim(rn,2) eq 'RING' then ring=1 else ring=0
     if nest then pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
     if ring then pix2ang_ring,nside,lindgen(n_elements(hmap)),theta,phi
     lra=360.-phi*!radeg
     ldec=(0.5*!pi-theta)*!radeg

     maxpix=max(hmap,mmax)
     cra=lra[mmax]
     cdec=ldec[mmax]

     ;;; GBM Map
     probmap=mrdfits(gbmfile,1,hdr)
     pix=sxpar(hdr,'CDELT1')
     smap=size(probmap)
     xmap=fltarr(smap[1],smap[2])
     ymap=fltarr(smap[1],smap[2])
     xmap[0,0]=cra-pix*smap[1]/2.
     ymap[0,0]=cdec-pix*smap[2]/2.
     for i=0,smap[1]-1 do xmap[i,*]=xmap[0,0]+pix*i
     for i=0,smap[2]-1 do ymap[*,i]=ymap[0,0]+pix*i
     w=where(xmap lt 0,nw)
     if nw gt 0 then xmap[w]=xmap[w]+360
     gra=xmap
     gdec=ymap
     phi=(360.-gra)/!radeg
     theta=-(gdec/!radeg-0.5*!pi)
     w1=where(theta lt 0,nw1)
     if nw1 gt 0 then theta[w1]=theta[w1]+!pi
     w2=where(theta gt !pi,nw2)
     if nw2 gt 0 then theta[w2]=theta[w2]-!pi
     w1=where(theta lt 0,nw1)
     if nw1 gt 0 then theta[w1]=theta[w1]+!pi
     w2=where(theta gt !pi,nw2)
     if nw2 gt 0 then theta[w2]=theta[w2]-!pi

     if min(theta) lt 0 or max(theta) gt !pi then stop
     if nest then ang2pix_nest, nside, theta, phi, ipnest 
     if ring then ang2pix_ring, nside, theta, phi, ipnest 
     gmap=hmap
     gmap[*]=0.

     gmap[ipnest]=probmap
     gmap=gmap/total(gmap)

     cmap=gmap*hmap
     cmap=cmap/total(cmap)

     mollview,ligofile,coord='C',colt=-1,png=outdir+'ligo_healpix_map_'+ntostr(s)+'.png',window=-1,rot=180
     write_fits_map,outdir+'ligo_healpix_map_'+ntostr(s)+'.fits',hmap,coordsys='C',ordering='nest'

     write_fits_map,outdir+'gbm_healpix_map_'+ntostr(s)+'.fits',gmap,coordsys='C',ordering='nest'
     mollview,outdir+'gbm_healpix_map_'+ntostr(s)+'.fits',coord='C',colt=-8,png=outdir+'gbm_healpix_map_'+ntostr(s)+'.png',window=-1,rot=180

     write_fits_map,outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.fits',cmap,coordsys='C',ordering='nest'
     mollview,outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.fits',coord='C',colt=20,png=outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.png',window=-1,rot=180
     
     im=image(outdir+'ligo_healpix_map_'+ntostr(s)+'.png',/clip,yrange=[50,450],xrange=[10,790],position=pos)
;     im=image(ligoimfile,/clip,yrange=[50,450],xrange=[10,790],position=pos)
     m=map('mollweide',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=180,$
           color='grey',position=pos,limit=[-90,0,90,360],/hide)

     m2=map('mollweide',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=180,$
           color='grey',position=pos,limit=[-90,0,90,360])

     c=contour(probmap,xmap,ymap,xrange=[0,360],yrange=[-90,90],c_value=1.-[0.68,0.955,0.997],c_color=0,c_label_show=0,overplot=m,position=pos,color='green')

     p=symbol(cra,cdec,symbol='star',sym_size=2.,/sym_filled,/data)
     m=max(probmap,wm)
     p2=symbol(xmap[wm],ymap[wm],symbol='star',sym_size=1.5,sym_color='red',/sym_filled,/data)


     sim[s].simnum=s
     healprob,hmap,lsigarea
     healprob,gmap,gsigarea
     healprob,cmap,csigarea
     sim[s].ligo_area=lsigarea
     sim[s].gbm_area=gsigarea
     sim[s].ligo_gbm_area=csigarea

     print,lsigarea
     print,gsigarea
     print,csigarea

;     k=get_kbrd(10)
;     if k eq 's' then begin 
;        mwrfits,sim,outdir+'sim_results.fits',/create
;        stop
;     endif 
     if s mod 10 eq 0 then mwrfits,sim,outdir+'sim_results.fits',/create

     im.save,outdir+'ligo_gbm_healpix_plot_'+ntostr(s)+'.png'
     im.close
     ptime,systime(1)-time
  endfor 
  mwrfits,sim,outdir+'sim_results.fits',/create

stop
return
end 

pro gi_authors_cross_corr

  cd,'~/Fermi/Senior_Review/SR2016'

  readcol,'~/Fermi/Senior_Review/SR2016/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent
  last=last[rem_dup(last)]
  last=[last,'THE FERMI-LAT COLLABORATION','THE FERMI LAT COLLABORATION','FERMI-LAT COLLABORATION']

;;   b=mrdfits('Fermi_bibliography.fits',1)
;;   m=intarr(n_elements(b))
;;   for i=0,n_elements(b)-1 do begin
;;      chunks=strsplit(b[i].author,' .',/ex)
;;      len=strlen(chunks)
;;      w=where(len gt 1)
;;      auth=chunks[w]
;;      match,strtrim(last,2),strtrim(strupcase(auth[0]),2),m1,m2
;;      if m2[0] ne -1 then begin
;;         m[i]=1
;; ;        print,last[m1]
;;      endif 
;;   endfor 

;  readcol,'~/Fermi/Bibliography/authors.txt',lines,format='(a)'
  openr,lun,'~/Fermi/Bibliography/authors.txt',/get_lun
  i=0
  authors=''
  notauth=''
  m=0
  line=''
  ln=0
  while not eof(lun) do begin 
     readf,lun,line;,format='(a)'
     ;; many authors

     if strpos(line,', and') eq -1 then $
        authors=[authors,strsplit(line,',;',/ex)] 

     if strpos(line,' and ') ne -1 then begin
        authors=[authors,strsplit(line,',;',/ex)] 
        w=where(strtrim(authors) ne '')
        authors=authors[w]
        lauth=str_sep(authors[n_elements(authors)-1],' and ')
        if n_elements(lauth) gt 1 then $
           authors=[authors[0:n_elements(authors)-2],lauth[1]]
     endif 
     ;; 2 authors
     if strpos(line,' and ') ne -1 and strpos(line,',') eq -1 then $
        authors=[str_sep(line,' and ')]
     ;; single author
     if strpos(line,' and ') eq -1 and strpos(line,',') eq -1 and line ne '' then $
        authors=['',line]

     ;; gap line
     if strtrim(line) eq '' then begin
        authors=authors[1:*]
        w=where(strtrim(authors) ne '')
        authors=authors[w]
        match,strtrim(last,2),strtrim(strupcase(authors),2),m1,m2
        if m2[0] ne -1 then begin
           m=[m,1]
        endif else begin
           ;; aren't Co-I's
           colprint,authors
           notauth=[notauth,authors]
        endelse 
;stop
        i=i+1
;        print,authors
;        k=get_kbrd(10)
;        if k eq 's' then stop
        authors=''
     endif 
     ln=ln+1
  endwhile 
  close,lun
  free_lun,lun
  
  notauth=notauth[1:*]
  m=m[1:*]
  help,m,i
;  help,b
  w=where(m eq 1,nw)
;  help,w
  print,nw*1./i;n_elements(b)
stop
  return
end 

pro p8increase

  eng=[29.974926,55.432686,96.13984,146.6698,212.57652,281.68668,397.91937,622.6682,846.48413,1195.8016,1604.9442,2386.3179,4083.7163,6639.7983,11361.051,16461.363,22953.062,31195.146,43492.62,59106.195,74394.414,93627.05,117839.6,146409.0,201466.06,303067.7]

  inc=[1.5852493,1.4518211,1.3545957,1.313932,1.2891028,1.2778375,1.2665842,1.2598764,1.248616,1.2328357,1.2238363,1.2058021,1.2081645,1.212781,1.2400409,1.2650068,1.292229,1.326237,1.3715665,1.4168913,1.4531485,1.5075128,1.5505601,1.6162392,1.7158884,1.8879833]

  eng2=[29.547823,51.953167,79.30773,109.23613,166.66933,271.0597,475.94354,999.58057,2266.711,4522.318,9740.953,16244.334,25737.633,54708.004,83401.85,150077.05,273426.5]

  inc2=[1.8500664,1.5989312,1.4541504,1.3908339,1.3297994,1.2914109,1.2734073,1.2577002,1.2487975,1.2625053,1.2988614,1.3464874,1.3986309,1.514204,1.5957648,1.7588391,1.9875548]

  eng3=[30.39605,92.5449,241.608,554.9685,999.68713,2046.708,3880.48,9260.118,15643.998,28898.88,53377.28,84562.305,140975.67,226120.9,311132.88]

  inc3=[1.3883346,1.3093202,1.2732824,1.2553284,1.2395929,1.2080371,1.1877843,1.206052,1.2242559,1.2764276,1.3512335,1.4214842,1.5211687,1.6615877,1.7725539]

  p=plot([0,0],[0,0],xrange=[20,3e5],yrange=[0.9,2],/xlog,xtitle='Minimum Energy',ytitle='Pass 8 Improvement',/nodata)
  p1=plot(eng,inc,thick=3,/overplot)
  p2=plot(eng2,inc2,/overplot,line='--')
  p3=plot(eng3,inc3,/overplot,line='--')

  p.save,'~/Fermi/Senior_Review/SR2016/Pass8_gains.png'
  p.close


  return
end

pro too

  ;;; PLOT FERMI TOOS AS A FUNCTION OF MISSION TIME

  readcol,'~/Fermi/Senior_Review/SR2014/TOOs.csv',date,pi,target,duration,format='(a,a,a,i)',delim=',',skip=1,/silent

  w=where(date ne '')
  n=n_elements(date[w])
  
  ;; skip duplicates
  skip=[2,8]
  ind=intarr(n)
  ind[*]=1
  ind[skip]=0
  w=where(ind eq 1,n)
  date=date[w]

  met=dblarr(n)
  for i=0,n-1 do begin
     met[i]=date2met(date[i],/fermi)
  endfor 

  met0=date2met('2008-06-11-12:00:00')
  d=met-met0
  year=365.25*86400.
  years=d/year

  plothist,years,x,y,/noplot,bin=6/12.

;  x=[2010,2011,2012,2013]
;  y=[18,16,12.5,43.5]
  
;  too=[[2010,098,3.5],[2010,266,4.5],[2010,361,10],[2011,087,4],[2011,104,7],[2011,243,2],[2011,250,3],[2012,068,2],[2012,186,3.5],[2012,268,7],[2013,66,4],[2013,104,1.5],[2013,134,5.5],[2013,228,5.5],[2013,234,6],[2013,240,7.5],[2013,240,7.5],[2013,291,3.5],[2013,298,4],[2013,313,1],[2013,339,5]]

;  spawn,'wget "http://fermi.gsfc.nasa.gov/ssc/observations/timeline/too/" -O ~/Fermi/Senior_Review/SR2016/toos.html'
  readcol,'~/Fermi/Senior_Review/SR2016/toos.html',lines,format='a',delim='$',/silent
  w=where(strpos(lines,'<tr class=') ne -1,nw)
  date=strarr(nw) & time=fltarr(nw) & years=fltarr(nw)
  for i=0,nw-1 do begin
     chunks=strsplit(lines[w[i]],'<>/',/ex)
;colprint,indgen(n_elements(chunks)),chunks
     date[i]=chunks[7]
     time[i]=chunks[25]*1.
     y=strmid(date[i],0,4)
     m=strmid(date[i],6,2)
     d=strmid(date[i],9,2)
     doy=ymd2dn(y,m,d)
     years[i]=fix(y)+doy/365.
  endfor 
  
  dur=time/86.4
;  years=too[0,*]+too[1,*]/365.
;  dur=too[2,*]

  bin=3./12.
  n=24
  y=fltarr(n) & x=y
  for i=0,n-1 do begin
     w=where(years ge 2010+i*bin and years lt 2010+(i+1)*bin,nw)
     if nw gt 0 then y[i]=total(dur[w])
     x[i]=2010+i*bin
;     if nw gt 0 then print,years[w]
;     print,x[i],y[i]
;     print
  endfor 
;  b=barplot(2008.5+x,y,ytitle='         Target of Opportunity
;  Observations',fill_color='blue',xrange=[2009,2014],yrange=[0,6],xmajor=0,xminor=0,aspect_ratio=0.4,yminor=0)
  b=barplot(x,y,ytitle='Days spent on TOO Observations',fill_color='blue',xrange=[2009,2016],yrange=[0,40],xmajor=0,xminor=0,aspect_ratio=0.1,yminor=0)
  p=plot([2009.65,2009.65],[0,40],/overplot,linestyle='--')
  xaxis=axis(0,location=[0,40],coord_transform=[-2008.5,1.],title='Mission Year',textpos=1,tickdir=1,minor=3)
  xaxis=axis(0,location=[0,0],title='Year',minor=3)
  t=text(2009.55,5,'TOO capability became available to community',orientation=90,/data,font_size=8)
;  t=text(0.067,0.22,'Fermi',font_style='it',orientation=90)

  b.save,'~/Fermi/Senior_Review/SR2016/too_plot.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close
  
stop

return
end 

pro bib
  ;; which plots use?
  ;;; cumulative papers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  plot of papers as function of time (single panel of previous plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  cd,'~/Fermi/Senior_Review/SR2016'
  bib=mrdfits('Fermi_bibliography.fits',1)
  nbib=n_elements(bib)
  w=where(bib.date gt 2008.5,nbib)
  bib=bib[w]
  ind=intarr(nbib)
  ind[*]=1
  bin=1.;/12;3./12
  nyear=max(bib.year+1)-2008
  n=nyear/bin
;  cind=cumulative(ind)

;  goto,skip
;  plothist,bib.date,x,y,bin=bin,/noplot
  x=fltarr(n+2) & y=fltarr(n+2)
  for i=0,n+1 do begin 
     w1=where(bib.date ge 2008+i*bin and bib.date lt 2008+(1+i)*bin,nw1)
     x[i]=2008+i*bin
     y[i]=nw1
  endfor 
  p=barplot(x+bin*0.5,y/bin,xrange=[2008,2016],yrange=[0,400],ytitle='             Papers per Year',xminor=3,xtitle='Year',position=[0.12,0.25,0.93,0.7],yminor=4,fill_color="blue",xtickval=2008+indgen(9),width=0.8,color='blue',font_size=14)
  t=text(0.035,0.305,'Fermi',font_style='it',orientation=90,font_size=14)
  plotsym,0,1,/fill

;  p=plot([2009.,2009.]+7./12.,[0,2000],linestyle='--',/overplot,thick=2)
;  t=text(2009.5,30,'Pass 6',orientation=90,/data,font_color='white')
;  t=text(2009.85,40,'Data',orientation=90,/data,font_color='white')
;  t=text(2010.2,40,'Release',orientation=90,/data,font_color='white')
  ;; pass 7 aug 5, 2011
;  p7=2011.+ymd2dn(2011,8,5)/365.
;  p=plot([p7,p7],[0,2000],linestyle='--',/overplot,thick=2)
;  t=text(2011.5,50,'Pass 7',orientation=90,/data,font_color='white')
  for i=0,n+1 do colprint,2008+i*bin,2008+(i+1)*bin,n_elements(where(bib.date ge 2008+i*bin and bib.date lt 2008+(1+i)*bin))

;  p7r=2013.92;2015.+ymd2dn(2015,6,24)/365.
;  p=plot([p7r,p7r],[0,2000],linestyle='--',/overplot,thick=2)
;  t=text(2013.8,50,'Pass 7 Rep',orientation=90,/data,font_color='white')

;  p8=2015.+ymd2dn(2015,6,24)/365.
;  p=plot([p8,p8],[0,2000],linestyle='--',/overplot,thick=2)
;  t=text(2015.4,50,'Pass 8',orientation=90,/data,font_color='white')

  p.save,'~/Fermi/Senior_Review/SR2016/bib_papers_total.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close

return
end

pro read_bib,p
  
  cd,'~/Fermi/Senior_Review/SR2016'
  
  readcol,'fermi_bibcodes.csv',title,author,date,bibcode,citation,category,subject,format='(a,a,a,a,a,a,a)',delim=',',skip=1
  ;;; SEARCH AND REPLACE COMMAS WITH NOTHING

;  readcol,'fermi_bibcodes.txt',title,date,citation,category,subject,format='(a,a,a,a,a)',delim='	',skip=1;,stringskip='"'

;  file='fermi_bibcodes.csv'
;  readcol,file,lines,format='(a)',delim='%',stringskip='ERRATUM'
;  n=numlines(file)
  n=n_elements(title)
  p=create_struct('bibcode','','month',0,'year',0.,'date',0.,'citation',0,'analysis',0,'refer',0,$
                  'predict',0,'instr',0,'nocat',0,$
                  'agn',0,'catalogs',0,'crs',0,'dm',0,'ebl',0,'grb',0,'binary',0,'diffuse',0,$
                  'galaxies',0,'iamr',0,'other',0,'pulsars',0,'snr',0,'ss',0,'unid',0,$
                  'author','','title','')
  p=replicate(p,n)
  
  p.bibcode=bibcode
  w=where(citation eq '-')
  citation[w]=0
  p.citation=citation
  p.author=author
  p.title=title

  for i=0,n-1 do begin 
     p[i].month=month(strmid(date[i],0,3))
     year=strmid(date[i],4,4)
     if strmid(year,0,2) ne '20' then stop
     p[i].year=year
     
     if strpos(category[i],'analysis') ne -1 then p[i].analysis=1
     if strpos(category[i],'Refer') ne -1 then p[i].refer=1
     if strpos(category[i],'Predict') ne -1 then p[i].predict=1
     if strpos(category[i],'Instr') ne -1 then p[i].instr=1
     if strpos(category[i],'Cannot') ne -1 then p[i].nocat=1
     if strpos(subject[i],'AGN') ne -1 then p[i].agn=1
     if strpos(subject[i],'Catalogs') ne -1 then p[i].catalogs=1
     if strpos(subject[i],'Cosmic') ne -1 then p[i].crs=1
     if strpos(subject[i],'Dark') ne -1 then p[i].dm=1
     if strpos(subject[i],'EBL') ne -1 then p[i].ebl=1
     if strpos(subject[i],'burst') ne -1 then p[i].grb=1
     if strpos(subject[i],'binary') ne -1 then p[i].binary=1
     if strpos(subject[i],'diffuse') ne -1 then p[i].diffuse=1
     if strpos(subject[i],'Galaxies') ne -1 then p[i].galaxies=1
     if strpos(subject[i],'methods') ne -1 then p[i].iamr=1
     if strpos(subject[i],'Other') ne -1 then p[i].other=1
     if strpos(subject[i],'Pulsars') ne -1 then p[i].pulsars=1
     if strpos(subject[i],'SNR') ne -1 then p[i].snr=1
     if strpos(subject[i],'Solar') ne -1 then p[i].ss=1
     if strpos(subject[i],'Unid') ne -1 then p[i].unid=1

;     if not exist('~/Fermi/Bibliography/'+bibcode[i]+'.txt') then $
;        spawn,'wget "http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode='+bibcode[i]+'&data_type=BIBTEX&db_key=AST&nocookieset=1" -O ~/Fermi/Bibliography/'+bibcode[i]+'.txt'
;     readcol,'~/Fermi/Bibliography/'+bibcode[i]+'.txt',col,skip=4,format='(a)',delim='{},'
  endfor 

  p.date=(p.month-1.)/12.+p.year ;;; does this -1 make sense?
  s=sort(p.date)
  p=p[s]

  mwrfits,p,'~/Fermi/Senior_Review/SR2016/Fermi_bibliography.fits',/create

  by=200
  for i=0,n-1,by do begin
;     bib=''
;     for j=0,by-1 do bib=bib+'%0D%0A'+strtrim(bibcode[i+j],2)
;     url='http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?db_key=ALL&warnings=YES&version=1&bibcode='+bib+'&nr_to_return='+ntostr(by)+'&start_nr=1'
;     print
;     print
;     print
;     print,url
     print
     print
     print
     colprint,p[i:i+by-1].bibcode
;     com="open "'+url+"'"
;     com='open "'+url+'"'
;     print,com
;     spawn,com
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor 

stop  
  return
end 

pro data_queries
  
;  readcol,'~/Fermi/Senior_Review/SR2016/daily_queries.csv',date,q,format='(a,f)',skip=1
  readcol,'~/Fermi/Senior_Review/SR2016/monthly_queries.csv',date,q,format='(a,f)',skip=1
  year2=strmid(date,0,4)
  mn=strmid(date,5,2)
;  day=strmid(date,8,2)
;  year2=year2+day/30./12.+(mn-1.)/12.
  year2=year2+((mn)/12.)

  y2=0
  for i=0,n_elements(year2)-1 do if q[i] gt 0 then y2=[y2,replicate(year2[i],q[i])]
  y2=y2[1:*]
  w=where(y2 le 2016.)
  y2=y2[w]

;  readcol,'~/Fermi/Senior_Review/SR2016/helpdesk_queries.csv',week,queries
  ;; weeks since Feb 1, 2009
  readcol,'~/Fermi/Senior_Review/SR2016/HelpDeskTracker_Nov03.csv',week,date,queries,format='(f,a,f)',skip=1
;  year=2009.+(31.+week*7.)/365.
  year=strmid(date,0,2)*1./12.+strmid(date,3,4)*1.
  y=0
  for i=0,n_elements(year)-1 do if queries[i] gt 0 then y=[y,replicate(year[i],queries[i])]
  y=y[1:*]

  p=plot([2009.5,2016],[0,7e4],/nodata,xrange=[2009.5,2016],yrange=[100,7d4],position=[0.15,0.4,0.9,0.9],font_size=14.,xminor=3,yminor=0,xtitle='Year',ytitle='Number of Queries per Month')

  plothist,y2,x,y,/noplot,bin=0.0834,xrange=[2009,2016]
  p1=barplot(x,y,/overplot,fill_color='blue',width=1,color='blue')
;  t1=text(0.2,0.8,'Data Queries',color='blue',font_size=14.)

;  p2=plot([2009,2016],[0,100],/nodata,xrange=[2009,2016],yrange=[0,100],xminor=4,position=[0.15,0.1,0.9,0.4],/current,xtitle='Year',font_size=14.)
;  t2=text(0.04,0.25,'Number of Queries per Month',orientation=90,font_size=14.)
;  p3=barplot(year,queries,/overplot,fill_color='red',width=1,color='red')
;  t3=text(0.2,0.3,'Helpdesk Queries',color='red',font_size=14.)
  p.save,'~/Fermi/Senior_Review/SR2016/data_queries.pdf'
  p.close

 stop
  begplot,name='~/Fermi/Senior_Review/SR2016/data_queries.eps',/land,/color,/encap,font='helvetica'
;  !y.margin=[0,0]
  !x.margin=[15,0]
  multiplot2,[1,2],/init
  multiplot2
  ytitle='Number of Queries per Month'
  plot,[2009,2016],[0,7e4],/nodata,xrange=[2009,2016],/xsty,charsize=2,yrange=[100,7d4],xminor=4,/ysty,/ylog,ytickformat='loglabels';,yticks=4,ytickv=[0,2e4,4e4,6e4],yminor=4;

  plothist2,y2,color=!black,/over,bin=4.*7./365.,/fill,fcolor=!blue,xrange=[2009,2016]
  xyouts,2010,30000,'  Data',color=!blue,charsize=2
  xyouts,2010,15000,'Queries',color=!blue,charsize=2
  
  axis,xaxis=0,xtickformat='(A1)',xminor=4;,xticks=2,xtickv=[2010,2011,2012]
  xyouts,2009.5,110,'Release of photon data',orient=90,charsize=1.6

  multiplot2,yupgap=0.2
  plot,[0,0],[0,0],/nodata,xrange=[2009,2016],/xsty,psym=10,xtitle='Year',charsize=2,yrange=[0,100],yminor=4,xminor=4,xticklen=0.05,yticks=4,/ysty,ytickv=[0,40,80]
;  plothist,y,x,y1,color=!black,/over,bin=4.*7./365.,/fill,fcolor=!red,xminor=12
  nq=n_elements(queries)
  
  polyfill,[year[0],year,year[nq-1],year[0]]+2/365.,[0,queries,0,0],color=!red
  oplot,[year[0],year+2/365.,year[nq-1]],[0,queries,0],psym=10,color=!black
  xyouts,2008.1,0,ytitle,orient=90,charsize=2
  xyouts,2013.3,75,'Helpdesk Queries',color=!red,charsize=2

  oplot,[2011.+(7./12.),2011.+(7./12.)],[0,75],line=2
  xyouts,2011.5,80,'Pass 7',charsize=1.6

  oplot,[2015.+(6./24.),2015.+(6./24.)],[0,45],line=2
  xyouts,2014.9,52,'Pass 8',charsize=1.6

  oplot,[2011.5,2011.5],[0,65],line=2
  xyouts,2010.8,60,'2FGL',charsize=1.6

;  oplot,[2010.,2010.]+(8./12),[0,38],line=2
;  xyouts,2010.3,70,'software release',charsize=1.6

  oplot,[2010.,2010.]+0.5/12.,[0,60],line=2
  xyouts,2009.9,66,'1FGL',charsize=1.6

  oplot,[2009.13,2009.13],[0,100],line=2
  xyouts,2009.19,60,'0FGL',charsize=1.6

  oplot,[2013.92,2013.92],[0,50],line=2
  xyouts,2013.1,55,'Pass 7 Rep',charsize=1.6

  axis,xaxis=0,xtickformat='(A1)',xminor=4,xticks=5
  multiplot2,/reset,/default
  endplot
;  spawn,'convert -rotate 270 ~/Fermi/Senior_Review/SR2016/data_queries.eps ~/Fermi/Senior_Review/SR2016/data_queries.png'

stop
return
end 


pro prop_stats

;  prop=0L & lastname='' & dup='' & firstname='' & acycle=0

  readcol,'~/Fermi/Senior_Review/SR2016/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

;  prop=prop[1:*]
  lastname=last;[1:*]
  firstname=first;[1:*]
;  approv=approv[1:*]
;  pc=pc[1:*]
;  inst=inst[1:*]
  acycle=prop;strmid(prop,0,1)

  prop=long(prop)
  w=where(pc eq 'PI',nw)
  nw=nw*1.
  c2=where(prop[w] ge 20000 and prop[w] lt 30000,nc2)
  c3=where(prop[w] ge 30000 and prop[w] lt 40000,nc3)
  c4=where(prop[w] ge 40000 and prop[w] lt 50000,nc4)
  c5=where(prop[w] ge 50000 and prop[w] lt 60000,nc5)
  c6=where(prop[w] ge 60000 and prop[w] lt 70000,nc6)
  c6=where(prop[w] ge 70000 and prop[w] lt 80000,nc7)
  c6=where(prop[w] ge 80000 and prop[w] lt 90000,nc8)

  print,nc2,nc3,nc4,nc5,nc6,nc7,nc8
  name=lastname+firstname

  w=where(pc eq 'PI' and approv eq 'Y')
  s=rem_dup(name[w])
  i=rem_dup(inst[w])

  help,s,i
  
  w=where(approv eq 'Y')
  s2=rem_dup(name[w])
  help,s2
  s=rem_dup(name)
  names=name[s]
  props=prop[s]
  approvs=approv[s]
  lastnames=lastname[s]
  firstnames=firstname[s]
  acycle=acycle[s]

  wa=where(approvs eq 'Y')
  accname=names[wa]
  accprop=props[wa]

  c2=where(accprop ge 20000 and accprop lt 30000,anc2)
  c3=where(accprop ge 30000 and accprop lt 40000,anc3)
  c4=where(accprop ge 40000 and accprop lt 50000,anc4)
  c5=where(accprop ge 50000 and accprop lt 60000,anc5)
  c6=where(accprop ge 60000 and accprop lt 70000,anc6)
  c6=where(accprop ge 70000 and accprop lt 80000,anc7)
  c7=where(accprop ge 80000 and accprop lt 90000,anc8)
  print,anc2,anc3,anc4,anc5,anc6,anc7,anc8

  print,anc2*1./nc2,anc3*1./nc3,anc4*1./nc4,anc5*1./nc5,anc6*1./nc6,anc7*1./nc7,anc8*1./nc8

  stop
  return
end 

pro coi_plot,names,lastnames,firstnames,acycle,both=both
  
  if keyword_set(both) then add='_acc' else add=''
  ncycle=7
  cycle=indgen(ncycle)+2
  
  prop=0L & lastname='' & dup='' & firstname='' & acycle=0
  readcol,'~/Fermi/Senior_Review/SR2016/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

  prop=prop[1:*]
  lastname=last[1:*]
  firstname=first[1:*]
  approv=approv[1:*]
  pc=pc[1:*]
  acycle=prop;strmid(prop,0,1)

  name=lastname+firstname

  s=rem_dup(name)
  names=name[s]
  props=prop[s]
  approvs=approv[s]
  lastnames=lastname[s]
  firstnames=firstname[s]
  acycle=acycle[s]

;  name=last+first
  wa=where(approvs eq 'Y')
  accname=names[wa]
  accprop=props[wa]

  n=lonarr(ncycle) & nacc=n
  for i=1,ncycle+1 do begin
     w=where(props ge 10000L*i and props lt 10000L*(i+1),nw)
     ws=where(accprop ge 10000L*i and accprop lt 10000L*(i+1),nwacc)
     n[i-2]=nw
     nacc[i-2]=nwacc
     print,10000L*i,10000L*(i+1),nw,nwacc
  endfor 
  cc=cumulative(n)
  cc5=cumulative(nacc)

  b=barplot(cycle+2007,cc,xrange=[2008.5,2015.5],ytitle='Cumulative Guest Investigators',fill_color='blue',width=0.4,xtitle='Year',yrange=[0,1600],aspect_ratio=0.002,xminor=0,font_size=14)
  b2=barplot(cycle+2007+0.1,cc5,/overplot,transparency=25,fill_color='red',width=0.4)
  a=axis(0,location=[0,1600],coord_transform=[-2007,1.],title='Cycle',textpos=1,tickdir=1,minor=0,tickfont_size=14)
  t=text(2008.7,1450,'Submitted',color='blue',/data,font_style='bold',font_size=14)
  t=text(2008.7,1330,'Accepted',color='red',/data,font_style='bold',font_size=14)

  b.save,'~/Fermi/Senior_Review/SR2016/gis.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close

return
end 

pro curved_polygon,x0,x1,y0,y1,x,y,rightside=rightside,ylog=ylog,xlog=xlog,ry=ry,rx=rx,xm=xm,ym=ym ;,color=color,noplot=noplot

  ang=indgen(90)*!dtor
  if n_elements(rx) eq 0 then rx=0.1d
  if n_elements(ry) eq 0 then begin 
     ry=0.5d
     if y1-y0 lt ry then ry=0.2d
  endif 
  up=0
  if n_elements(xm) gt 0 then if xm[0] ne 0 then up=1

  if keyword_set(xlog) then begin 
     ax0=alog10(x0)
     ax1=alog10(x1)
     if up then axm=alog10(xm)
  endif else begin 
     ax0=x0
     ax1=x1
     if up then axm=xm
  endelse 
  if keyword_set(ylog) then begin 
     ay0=alog10(y0)
     ay1=alog10(y1)
     if up then aym=alog10(ym)
  endif else begin 
     ay0=y0
     ay1=y1
     if up then aym=ym
  endelse 
  
  ca=rx*cos(ang)
  sa=ry*sin(ang)
  x=[-ca+ax0+rx,reverse(ca+ax1-rx),ca+ax1-rx,reverse(-ca+ax0+rx),-ca[0]+ax0+rx]
  y=[ay1+sa-ry,reverse(ay1+sa-ry),ay0-sa+ry,reverse(ay0-sa+ry),ay1+sa[0]-ry]
  if keyword_set(rightside) then begin
     x=[ax0,ax0,reverse(ca+ax1-rx),ca+ax1-rx,ax0]        
     y=[ay0,ay1,reverse(ay1+sa-ry),ay0-sa+ry,ay0]
  endif 
  if up then begin
     x=[-ca+ax0+rx,axm,axm,reverse(ca+ax1-rx),ca+ax1-rx,axm,axm,reverse(-ca+ax0+rx),-ca[0]+ax0+rx]
     y=[ay1+sa-ry,ay1,aym[1],reverse(aym[1]+sa-ry),aym[0]-sa+ry,aym[0],ay0,reverse(ay0-sa+ry),ay1+sa[0]-ry]
     if keyword_set(rightside) then begin
     x=[ax0,ax0,axm,axm,reverse(ca+ax1-rx),ca+ax1-rx,axm,axm,ax0]        
     y=[ay0,ay1,ay1,aym[1],reverse(aym[1]+sa-ry),aym[0]-sa+ry,aym[0],ay0,ay0]
     endif 
  endif      


  if keyword_set(xlog) then x=10^x
  if keyword_set(ylog) then y=10^y
 
;plot,[0,12],[10,1e4],/ylog  
;  if not keyword_set(noplot) then polyfill,x,y,color=color
  return
end

pro mw_plot_redux

  cd,'~/Fermi/Senior_Review/SR2016/'
  obs=mrdfits('observatories.fits',1)

  h=4.135e-15 ;; eV s
  yearstart=2014; 2008
  yrange=[yearstart,2021]
  xrange=[1e-8,1e21]
;  xrange=[0,1]

  p=plot(xrange,yrange,/nodata,ytitle='Year',xrange=xrange,yrange=yrange,xminor=0,margin=[0.13,0.1,0.13,0.1],yminor=3,/xlog,xmajor=0)

  i=50
  im=image('Fermi_spacecraft.png',/overplot,image_dimensions=[alog10(obs[i].eng[1])-alog10(obs[i].eng[0]),(alog10(obs[i].eng[1])-alog10(obs[i].eng[0]))*0.13],image_location=[alog10(obs[i].eng[0]),2015],xrange=xrange,yrange=yrange,aspect_ratio=3.5)

  i=9
  im=image('HAWC.jpg',/overplot,image_dimensions=[alog10(obs[i].eng[1])-alog10(obs[i].eng[0]),(alog10(obs[i].eng[1])-alog10(obs[i].eng[0]))*0.25],image_location=[alog10(obs[i].eng[0]),2014.5],xrange=xrange,yrange=yrange,aspect_ratio=3.5)

  i=25
  im=image('lofar.jpg',/overplot,image_dimensions=[alog10(obs[i].eng[1])-alog10(obs[i].eng[0]),(alog10(obs[i].eng[1])-alog10(obs[i].eng[0]))*0.25],image_location=[alog10(obs[i].eng[0]),2014.5],xrange=xrange,yrange=yrange,aspect_ratio=3.5)

  i=42
  im=image('magellan.jpg',/overplot,image_dimensions=[alog10(obs[i].eng[1])-alog10(obs[i].eng[0]),(alog10(obs[i].eng[1])-alog10(obs[i].eng[0]))*0.5],image_location=[alog10(obs[i].eng[0]),2014.5],xrange=xrange,yrange=yrange,aspect_ratio=3.5)

  x=10^(findgen(8+15)-8)
  y=replicate([yrange[0]+0.08],n_elements(x))
  p1=plot(x,y,/overplot,linestyle='none',symbol='|')

  xaxis=axis('X',location=[1.,2021],coord_transform=[1.,1./h],textpos=1,tickdir=1,/log,title='Frequency (Hz)',tickvalues=[1e10,1e15,1e20,1e25],minor=0)
  xaxis=axis('X',location=[1.,yearstart],/log,title='Energy (eV)',tickformat='loglabels',tickvalues=[1e-5,1,1e5,1e10,1e15],minor=0)
  p.save,'mw_plot_v2.png'
  p.close

  stop
return
end 

pro mw_plot

  h=4.135e-15 ;; eV s
  c=3e10      ;;cm/s
  yearstart=2014; 2008
  yrange=[yearstart,2021]
  xrange=[1e-8,1e23]

;;; rounded edges not right at beginning for already started missions

  p=plot(xrange,yrange,xrange=xrange,yrange=yrange,/nodata,ytitle='Year',/xlog,xminor=0,xmajor=0,margin=[0.13,0.1,0.05,0.4],yminor=3);,ytitle='Energy (eV)')
;  fr=[1e6,1e9,1e12,1e15,1e18,1e21,1e24,1e27] ;,1e30]
;  en=h*fr
;  yaxis=axis('X',location=[1.,2021],coord_transform=[1.,1./h],textpos=1,tickdir=1,/log,title='Frequency (Hz)',tickvalues=[1e10,1e15,1e20,1e25],minor=0)

  yaxis=text([0.2,0.3,0.35,0.43,0.52,0.65,0.8,0.83],replicate(0.61,8),['Radio','Infrared','Optical','X-ray',!tsym.gamma+'-ray','VHE','Multi-','Messenger'],orientation=45,font_size=10)

  obs=create_struct('name','','ops',fltarr(2),'ext_ops',0.,'wavelength',dblarr(2),$
                    'eng',dblarr(2),'eng_upgrade',dblarr(2),'time_upgrade',0.,$
                    'freq',dblarr(2),$
                    'survey',0,'point',0,'space',0,'ground',0,$
                    'messenger','','color','','backcolor','','lcolor','',$
                    'xoffset',0.,'yoffset',0.,$
                    'fcolor','','font_size',0.,$
                    'font_style',0,'transparency',0,'orientation',0.)
  n=70
  obs=replicate(obs,n)
  obs.font_size=9
  obs.font_style=0
  obs.fcolor='white'
  obs.transparency=30
  obs.yoffset=1.
  obs.orientation=90.

  ;;; Gamma-ray

  i=0
  obs[i].name='Fermi'
  obs[i].ops=[2008.5,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[8,300e6]
;  obs[i].eng_upgrade=[8,2e9]
;  obs[i].time_upgrade=2015.5
  obs[i].color='red'
  obs[i].font_style=3
  obs[i].font_size=20
  obs[i].space=1
  obs[i].transparency=20
  obs[i].xoffset=0.8;1.5
  obs[i].yoffset=0.01
  obs[i].backcolor='tomato'
  obs[i].lcolor='black'
  obs[i].orientation=0.

;  i=1
;  obs[i].name='VERITAS'
;  obs[i].ops=[2009,2018]
;  obs[i].ext_ops=2021
;  obs[i].eng=[0.1,50]*1e9
;  obs[i].point=1
;  obs[i].ground=1
;  obs[i].yoffset=0.8
;;  obs[i].xoffset=0.7

;  i=2
;  obs[i].name='HESS'
;  obs[i].ops=[2004,2020];2012.67]
;  obs[i].ext_ops=2021;2012.67
;  obs[i].eng=[0.3,50]*1e9
;  obs[i].eng_upgrade=[0.05,50]*1e9 
;  obs[i].time_upgrade=2012.67
;  obs[i].ground=1
;  obs[i].point=1
;  obs[i].yoffset=4

;  i=5
;  obs[i].name='HESS II'
;  obs[i].ops=[2012.67,2021]
;  obs[i].ext_ops=2021
;  obs[i].eng=[0.05,50]*1e9
;  obs[i].ground=1
;  obs[i].point=1
;;  obs[i].xoffset=0.1
;  obs[i].yoffset=8

  i=3
  obs[i].name='MAGIC/VERITAS/HESS'
  obs[i].ops=[2005,2021]
  obs[i].eng=[0.03,30]*1e9 ;;(TeV->keV)
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=1.5
  
  i=4
  obs[i].name='CTA'
  obs[i].ops=[2018,2021]
  obs[i].eng=[10e6,100e9] ;;keV
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=1.5
  obs[i].xoffset=1.

  i=5
  obs[i].name='HAWC'
  obs[i].ops=[2015+2./12.,2021] 
  obs[i].eng=[0.1,100]*1e9 ;; keV
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=0.5
  obs[i].yoffset=4

  i=6
  obs[i].name='Swift-BAT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[15.,150.] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=1
  obs[i].font_style=2


;  i=7
;  obs[i].name='SVOM-GRM/ECLAIRs'
;  obs[i].ops=[2016,2021]
;  obs[i].space=1
;  obs[i].eng=[4,5e3] ;;keV
;  obs[i].survey=1
;  obs[i].yoffset=20

;  i=8
;  obs[i].name='AGILE'
;  obs[i].eng=[250,5e6] ;; keV
;  obs[i].ops=[2007,2014]
;  obs[i].ext_ops=2010
;  obs[i].space=1
;  obs[i].point=0 ;; ???
;  obs[i].survey=1
;  obs[i].yoffset=5

  ;; X-ray
;  i=9
;  obs[i].name='SVOM-MXT'
;  obs[i].ops=[2016,2021]
;  obs[i].space=1
;  obs[i].eng=[0.3,6] ;; keV
;  obs[i].point=1
;  obs[i].xoffset=1

  i=7
  obs[i].name='Swift-XRT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[0.3,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5
  obs[i].font_style=2

  i=8
  obs[i].name='Chandra/XMM'
  obs[i].ops=[1999,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[0.2,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=0.8

  i=9
  obs[i].name='MAXI'
  obs[i].ops=[2009,2021]
  obs[i].eng=[0.3,30] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].xoffset=1.5
  obs[i].yoffset=2.

  i=10
  obs[i].name='ASTRO-H'
  obs[i].ops=[2016+2./12,2021]
  obs[i].eng=[0.3,600] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5

  i=11
  obs[i].name='NuSTAR'
  obs[i].ops=[2012.5,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[3,80] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=2.5
  obs[i].font_style=2

  i=12
  obs[i].name='eRosita'
  obs[i].ops=[2017.5,2021]
  obs[i].eng=[0.5,10] ;; keV
  obs[i].point=1
  obs[i].survey=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=2.

  i=13
  obs[i].name='Astrosat-UV/opt'
  obs[i].ops=[2015.74,2021]
  obs[i].eng=[2.34e-3,9.5e-3]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1.
  obs[i].yoffset=2.;0.15

  i=16
  obs[i].name='Astrosat-X-ray'
  obs[i].ops=[2015.74,2021]
  obs[i].eng=[0.3,150]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=0.03;0.15
  obs[i].fcolor='black'

;  i=13
;  obs[i].name='XMM-Newton'
;  obs[i].ops=[1999,2015]
;  obs[i].ext_ops=2021
;  obs[i].eng=[0.1,10]  ;; keV
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].xoffset=1.7
;  obs[i].yoffset=0.8

  i=14
  obs[i].name='INTEGRAL'
  obs[i].ops=[2002,2020]
  obs[i].ext_ops=2020
  obs[i].eng=[15.,10e3]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=3
  obs[i].yoffset=1

  i=15
  obs[i].name='NICER'
  obs[i].ops=[2016.9,2018.5]
  obs[i].eng=[0.2,12]
  obs[i].point=1
  obs[i].space=1
  
;  i=16
;  obs[i].name='Suzaku'
;  obs[i].ops=[2005,2015.5]
;  obs[i].eng=[0.3,600]
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].xoffset=1
;  obs[i].yoffset=1.5

  ;; Optical
  i=17
  obs[i].name='Optical Pointing/Surveys' ;; Pan-STARRS, PTF, DES, Gaia, Robopol
  obs[i].ops=[2010,2021]
  obs[i].wavelength=[0.4,1.2]*1e-4
  obs[i].ground=1
  obs[i].survey=1
  obs[i].fcolor='black'
  obs[i].font_size=8
  obs[i].yoffset=1.5

;  obs[i].name='Pan-STARRS'
;  obs[i].color=!black
;  obs[i].ops=[2010,2021]
;  obs[i].wavelength=[0.5,1.0]*1e-4 ;; cm
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].yoffset=3
;  obs[i].xoffset=0.9;1.3

  i=18
  obs[i].name='Swift-UVOT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[150.,650.]*1e-7 ;; nm->cm
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2.
  obs[i].font_size=8
  obs[i].font_style=2
;  obs[i].fcolor='black'

  i=19
  obs[i].name='HST'
  obs[i].ops=[1990,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[300,1000]*1e-7
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2

;  i=20
;  obs[i].name='PTF'
;  obs[i].ops=[2009,2021]
;  obs[i].ext_ops=2021
;  obs[i].wavelength=[490,630]*1e-7 ;; nm->cm
;  obs[i].survey=1
;  obs[i].space=1
;  obs[i].yoffset=2
;  obs[i].xoffset=0.5;0.8

;  i=21
;  obs[i].name='Robopol'
;  obs[i].ops=[2013,2021]
;  obs[i].ext_ops=2021
;  obs[i].wavelength=[500,800]*1e-7
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].xoffset=2
;  obs[i].yoffset=2

;  i=22
;  obs[i].name='DES'
;  obs[i].ops=[2012.75,2021]
;  obs[i].ext_ops=2021
;  obs[i].wavelength=[400,1200]*1e-7
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].xoffset=3
;  obs[i].yoffset=2

;  i=23
;  obs[i].name='Gaia'
;  obs[i].ops=[2013.97,2018]
;  obs[i].ext_ops=2018
;  obs[i].wavelength=[320,1000]*1e-7
;  obs[i].survey=1
;  obs[i].space=1
;;  obs[i].xoffset=0.4
;  obs[i].yoffset=2

  ;; IR
  i=24
  obs[i].name='Spitzer'
  obs[i].ops=[2003,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[3.6,4.8]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=1.5

;  i=25
;  obs[i].name='Herschel'
;  obs[i].ops=[2009.5,2013]
;  obs[i].ext_ops=[2012.5]
;  obs[i].wavelength=[55,672]*1e-4
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].yoffset=1.5
;  obs[i].xoffset=1.3
;  obs[i].fcolor='black'

;  i=26
;  obs[i].name='WISE'
;  obs[i].ops=[2010,2013]
;  obs[i].wavelength=[3,22.]*1e-4
;  obs[i].ext_ops=2013
;  obs[i].point=1
;  obs[i].space=1
;  obs[i].yoffset=1.8

  i=27
  obs[i].name='JWST'
  obs[i].ops=[2018,2030]
  obs[i].wavelength=[0.6,28]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2
  obs[i].xoffset=0.2

  ;; RADIO

  i=28
  obs[i].name='LOFAR/MWA'
  obs[i].ops=[2011,2021]
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2
  obs[i].fcolor='black'

  i=29
  obs[i].name='VLBA'
  obs[i].ops=[1993,2021]
  obs[i].freq=[0.312,90]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=0.4

;  i=27
;  obs[i].name='JVLA'
;  obs[i].ops=[2010,2021]
;  obs[i].freq=[1.,50.]*1e9 ;; Hz
;  obs[i].point=1
;  obs[i].ground=1
 ; obs[i].xoffset=0.6
 ; obs[i].yoffset=2

  i=30
  obs[i].name='SKA'
  obs[i].ops=[2018,2021]
  obs[i].freq=[0.1,25]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].yoffset=0.4
  obs[i].xoffset=0.3

  i=31
  obs[i].name='SKA Prototypes'
  obs[i].ops=[2016,2021]
  obs[i].freq=[0.58,14.5]*1e9
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=1.4
  obs[i].xoffset=0.2
;  obs[i].name='Apertif'
;  obs[i].ops=[2016,2021]
;  obs[i].freq=[1.,1.7]*1e9 ;; Hz
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].yoffset=1.5

;  i=48
;  obs[i].name='MeerKAT'
;  obs[i].ops=[2016,2021]
;  obs[i].freq=[0.58,14.5]*1e9 ;; Hz
;  obs[i].ground=1
;  obs[i].survey=1
;  obs[i].yoffset=4
;  obs[i].xoffset=0.6

;  i=49
;  obs[i].name='ASKAP'
;  obs[i].ops=[2015,2021]
;  obs[i].freq=[0.7,1.8]*1e9 ;; Hz
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].yoffset=1.5
;;  obs[i].xoffset=0.5

  i=32
  obs[i].name='ALMA'
  obs[i].ops=[2013,2021]
  obs[i].freq=[84,720]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=1.
  obs[i].xoffset=1.5
;  obs[i].fcolor='black'

  i=33
  obs[i].name='Planck'
  obs[i].ops=[2009.5,2013]
  obs[i].ext_ops=2013
  obs[i].freq=[27,1000]*1d9 ;; Hz
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=1
;  obs[i].fcolor='black'

  i=34
  obs[i].name='Arecibo'
  obs[i].ops=[1997,2021]
  obs[i].ext_ops=2021
  obs[i].freq=[0.312,10.2]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.9

;  i=45
;  obs[i].name='MWA'
;  obs[i].ops=[2012,2021]
;  obs[i].ext_ops=2021
;  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
;  obs[i].survey=1
;  obs[i].ground=1
;  obs[i].yoffset=2
;  obs[i].xoffset=1

  i=35
  obs[i].name='ATCA/JVLA'
  obs[i].ops=[2010,2021]
  obs[i].ext_ops=2021
  obs[i].freq=[1.1,150]*1e9 ;; GHz->Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=1.5

  ;;; MULTI-MESSENGER
  i=36
  obs[i].name='IceCUBE'
  obs[i].ops=[2010,2021]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e16,1e17]
  obs[i].color='dim grey'
  obs[i].yoffset=2

  i=37
  obs[i].name='Auger'
  obs[i].ops=[2004,2021]
  obs[i].messenger='UHECRs'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e12,1e13]
  obs[i].color='black'
  obs[i].yoffset=2

  i=38
  obs[i].name='CALET'
  obs[i].ops=[2015.62,2016.67]
;  obs[i].ext_ops=[2018]
  obs[i].space=1
  obs[i].survey=1
  obs[i].messenger='Electrons'
  obs[i].eng=[1e17,1e18]
  obs[i].color='grey'
  obs[i].fcolor='black'
  obs[i].yoffset=2
  obs[i].xoffset=-0.1

  i=39
  obs[i].name='LIGO/Virgo'
  obs[i].ops=[2015.67,2021]
  obs[i].messenger='Gravitational Waves'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e18,1e19]
  obs[i].color='dark grey'
  obs[i].fcolor='black'
  obs[i].yoffset=2

  i=40
  obs[i].name='AMS'
  obs[i].ops=[2011.4,2021]
  obs[i].messenger='Cosmic Rays'
  obs[i].space=1
  obs[i].survey=1
  obs[i].eng=[1e13,1e14]
  obs[i].color='slate grey'
  obs[i].yoffset=2

  i=41
  obs[i].name='ANTARES'
  obs[i].ops=[2008,2021]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e15,1e16]
  obs[i].color='dark slate grey'
  obs[i].yoffset=2

  i=42
  obs[i].name='DAMPE'
  obs[i].ops=[2015.9,2019.5]
  obs[i].messenger='Cosmic Rays'
  obs[i].space=1
  obs[i].survey=1
  obs[i].eng=[1e14,1e15]
  obs[i].color='dim grey'
  obs[i].yoffset=2

  order=['LOFAR/MWA','VLBA','Arecibo','ATCA/JVLA','SKA Prototypes','SKA','Planck','ALMA','Spitzer','Astrosat-UV/opt','Swift-UVOT','Astrosat-X-ray','Optical Pointing/Surveys','JWST','Chandra/XMM','Swift-XRT','Swift-BAT','MAXI','NuSTAR','NICER','eRosita','INTEGRAL','Astro-H','MAGIC/VERITAS/HESS','HAWC','CTA','Auger','ANTARES','AMS','DAMPE','IceCUBE','CALET','LIGO/Virgo','Fermi']
  
  o=0
  for i=0,n_elements(order)-1 do begin
     w=where(obs.name eq order[i])
     o=[o,w]
  endfor 
  o=o[1:*]
  obs=obs[o]

  w=where(obs.ops[0] lt yearstart)
  obs[w].ops[0]=yearstart
  right=intarr(n_elements(obs))
  right[w]=1
;  w=where(obs.ops[1] gt 2019)
;  obs[w].ops[1]=2019
  obs.eng=obs.eng*1e3  ;;; convert from keV to eV
  obs.eng_upgrade=obs.eng_upgrade*1e3  ;;; convert from keV to eV
 
  w=where(obs.freq[0] ne 0)
  f=where(obs.wavelength[0] ne 0)
  en=where(obs.eng[0] ne 0)

  obs[w].wavelength=reverse(c/obs[w].freq)
  obs[w].eng=h*obs[w].freq

  obs[f].freq=reverse(c/obs[f].wavelength)
  obs[f].eng=h*obs[f].freq

  obs[en].freq=obs[en].eng/h
  obs[en].wavelength=reverse(h*c/obs[en].eng)

  mwrfits,obs,'~/Fermi/Senior_Review/SR2016/observatories.fits',/create

  w=where(obs.point eq 1 and obs.messenger eq '')
  obs[w].color='blue'
  obs[w].backcolor='light blue'
  obs[w].lcolor='grey'

  w=where(obs.survey eq 1 and obs.messenger eq '')
  obs[w].color='lime green'
  obs[w].backcolor='pale green'
  obs[w].lcolor='black'

;  w=where(obs.survey eq 1 or obs.name eq 'Fermi')
;  obs=obs[w] ;;; ONLY SURVEY
  n=n_elements(obs)
;  right=right[w]

  p.refresh,/disable
  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        curved_polygon,obs[i].ops[0],obs[i].ops[1],obs[i].eng[0],obs[i].eng[1],xx,yy,right=right[i],/ylog
;;; insert ext_eng into xx,yy somehow - merge 2 curved polys?
        poly=polygon(yy,xx,/data,transparency=obs[i].transparency,fill_color=obs[i].color,color=obs[i].lcolor)
        if obs[i].ext_ops ne obs[i].ops[1] and obs[i].ext_ops ne 0. then begin
           curved_polygon,obs[i].ops[1],obs[i].ext_ops,obs[i].eng[0],obs[i].eng[1],xx,yy,/right,/ylog,ym=obs[i].eng_upgrade,xm=obs[i].time_upgrade
           poly=polygon(yy,xx,/data,fill_color=strtrim(obs[i].backcolor,2),transparency=70,thick=2,color=obs[i].color,linestyle='--')
        endif 
     endif 
  endfor 

  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        if obs[i].messenger ne '' then mess=' ('+strtrim(obs[i].messenger,2)+')' else mess=''
        t=text(10^((alog10(obs[i].eng[1])+alog10(obs[i].eng[0]))/2.)*obs[i].yoffset,obs[i].ops[0]+obs[i].xoffset+0.1,obs[i].name+mess,/data,font_color=obs[i].fcolor,font_style=obs[i].font_style,font_size=obs[i].font_size,orientation=obs[i].orientation)
     endif 
  endfor      

  v=10^(dindgen(25)-10)
  p0=errorplot(v,replicate(2014,25),replicate(0.25,25),errorbar_capsize=0,/overplot,linestyle='none',/current)
  p0=errorplot(v,replicate(2021,25),replicate(0.25,25),errorbar_capsize=0,/overplot,linestyle='none',/current)

  yaxis=axis('X',location=[1.,yearstart],/log,title='Energy (eV)',tickformat='loglabels',tickvalues=[1e-5,1,1e5,1e10],minor=0)

;  leg=legend(label=['Survey','Pointed'],target=p,/data,position=[2010.1,1e20],text_color='black')
  l1=text(0.8,0.07,'Survey',font_color='lime green',font_style=1)
  l2=text(0.8,0.04,'Pointed',font_color='blue',font_style=1)
  p.save,'~/Fermi/Senior_Review/SR2016/mw_plot.pdf'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close
  
  stop
  return
end 
