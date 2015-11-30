pro plot_3c279

  cd,'~/Fermi/Senior_Review/SR2016'

  readcol,'3C279_2015June_3min.txt',bin3,ts3,flux3,fluxerr3,ind3,inderr3,npred,startmet3,endmet3,format='(i,d,f,f,f,f,f,d,d)'
  readcol,'3C279_2015June_5min.txt',bin5,ts5,flux5,fluxerr5,ind5,inderr5,npred,startmet5,endmet5,format='(i,d,f,f,f,f,f,d,d)'

  tstart=date2met('2015-06-16-00:00:00',/fermi)
  t3=((endmet3-startmet3)/2.+startmet3-tstart)/60.-110
  t5=((endmet5-startmet5)/2.+startmet5-tstart)/60.-110
  p=errorplot(t3,flux3/1e-5,replicate(1.5,n_elements(t3)),fluxerr3/1e-5,xtitle='Time (minutes)',ytitle='Flux (10!U-15!N ph cm!U-2!N s!U-1!N)',symbol='dot',errorbar_capsize=0,linestyle='none',sym_size=10.,/sym_filled,xrange=[0,40],/xstyle);,/ylog,yrange=[1e-7,1e-4])
;  p2=errorplot(t5,flux5,replicate(2.5,n_elements(t5)),fluxerr5,/overplot,/current,symbol='dot',sym_color='red',errorbar_capsize=0,errorbar_color='red',linestyle='none',sym_size=10.,/sym_filled)
;  p3=plot([0,6],[1e-6,1e-6],/overplot,line='--')

;; make x-axis in minutes

  k=get_kbrd(10)
  p.save,'3C279_1orbit.png'
  p.close
  if k eq 's' then stop
  

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

  outdir='~/Fermi/Senior_Review/SR2016/'
  dir='~/Fermi/gbmtrig/'
  gbmfiles=file_search(dir+'glg_locprob_all*fit')
  ngbm=n_elements(gbmfiles)

  ldir='/Users/jracusin/Swift/Tiling/ligo_gbm/sim_same_center/'
  ligofile=ldir+'ligo_gbm_healpix_map_6.fits'
  ligoimmap=ldir+'ligo_healpix_map_6.png'
  read_fits_map,ligofile,hmap,nside=nside
  print,nside
  pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
  lra=360.-phi*!radeg
  ldec=(0.5*!pi-theta)*!radeg
  maxpix=max(hmap,mmax)
  cra=lra[mmax]+5
  cdec=ldec[mmax]+5

  for j=0,ngbm-1 do begin
     gbmfile=gbmfiles[j]
     probmap=mrdfits(gbmfile,1,hdr)
     pix=sxpar(hdr,'CDELT1')
     smap=size(probmap)
     xmap=fltarr(smap[1],smap[2])
     ymap=fltarr(smap[1],smap[2])
     xmap[0,0]=cra-pix*smap[1]/2.
     ymap[0,0]=cdec-pix*smap[2]/2.
     for i=0,smap[1]-1 do xmap[i,*]=xmap[0,0]+pix*i
     for i=0,smap[2]-1 do ymap[*,i]=ymap[0,0]+pix*i
;     pos=[[0.05,0.05,0.95,0.95],[0.05,0.05,0.95,0.95]]
     im=image(ligoimmap,/clip,yrange=[50,450],xrange=[10,790],position=pos)
     m=map('mollweide',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=0,$
           color='grey',position=pos,/hide)

     c=contour(probmap,xmap,ymap,xrange=[180,540],yrange=[-90,90],c_value=1.-[0.68,0.955,0.997],c_color=0,/overplot,c_label_show=0)
     m=map('mollweide',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=0,$
           color='grey',position=pos,/overplot)
     
     k=get_kbrd(10)
     if k eq 's' then stop
     im.close
  endfor 

stop
return
end 

pro gi_authors_cross_corr

  

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
  ;;; theses

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
  bin=1.;3./12
  nyear=max(bib.year)-2008
;  cind=cumulative(ind)

;  goto,skip
;  plothist,bib.date,x,y,bin=bin,/noplot
  x=fltarr(nyear+1) & y=fltarr(nyear+1)
  for i=0,nyear do begin 
     w1=where(bib.date ge 2008+i and bib.date lt 2008+1+i,nw1)
     x[i]=2008+i
     y[i]=nw1
  endfor 
  p=barplot(x+0.5,y,xrange=[2008,2016],yrange=[0,400],ytitle='             Papers',xminor=3,xtitle='Year',aspect_ratio=0.008,yminor=4,fill_color="blue")
  t=text(0.055,0.42,'Fermi',font_style='it',orientation=90)
  plotsym,0,1,/fill

  p=plot([2009.,2009.]+7./12.,[0,2000],linestyle='--',/overplot)
  t=text(2009.5,30,'Public Data',orientation=90,/data,font_color='white')
  t=text(2009.8,40,'Release',orientation=90,/data,font_color='white')
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  p=plot([p7,p7],[0,2000],linestyle='--',/overplot)
  t=text(2011.5,100,'Pass 7 Release',orientation=90,/data,font_color='white')
  for i=0,8 do colprint,2008+i,2009+i,n_elements(where(bib.year ge 2008+i and bib.year lt 2008+1+i))

  p7r=2013.92;2015.+ymd2dn(2015,6,24)/365.
  p=plot([p7r,p7r],[0,2000],linestyle='--',/overplot)
  t=text(2013.8,50,'Pass 7 Rep',orientation=90,/data,font_color='white')

  p8=2015.+ymd2dn(2015,6,24)/365.
  p=plot([p8,p8],[0,2000],linestyle='--',/overplot)
  t=text(2015.4,50,'Pass 8 Release',orientation=90,/data,font_color='white')

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
  
;  p.bibcode=bibcode
  w=where(citation eq '-')
  citation[w]=0
  p.citation=citation
;  p.author=author
;  p.title=title

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
  endfor 

  p.date=(p.month-1.)/12.+p.year ;;; does this -1 make sense?
  s=sort(p.date)
  p=p[s]

  mwrfits,p,'~/Fermi/Senior_Review/SR2016/Fermi_bibliography.fits',/create
stop  
  return
end 

pro data_queries

  readcol,'~/Fermi/Senior_Review/SR2016/daily_queries.csv',date,q,format='(a,f)',skip=1
  year2=strmid(date,0,4)
  mn=strmid(date,5,2)
  day=strmid(date,8,2)
  year2=year2+day/30./12.+(mn-1.)/12.
 
  y2=0
  for i=0,n_elements(year2)-1 do if q[i] gt 0 then y2=[y2,replicate(year2[i],q[i])]
  y2=y2[1:*]
  w=where(y2 lt 2015.99)
  y2=y2[w]

;  readcol,'~/Fermi/Senior_Review/SR2016/helpdesk_queries.csv',week,queries
  ;; weeks since Feb 1, 2009
  readcol,'~/Fermi/Senior_Review/SR2016/HelpDeskTracker_Nov03.csv',week,date,queries,format='(f,a,f)',skip=1
;  year=2009.+(31.+week*7.)/365.
  year=strmid(date,0,2)*1./12.+strmid(date,3,4)*1.
  y=0
  for i=0,n_elements(year)-1 do if queries[i] gt 0 then y=[y,replicate(year[i],queries[i])]
  y=y[1:*]

  p=plot([2009,2016],[0,7e4],/nodata,xrange=[2009,2016],yrange=[100,7d4],xminor=4,xtickname=replicate(' ',8),position=[0.15,0.4,0.9,0.9],font_size=14.)

  plothist,y2,x,y,/noplot,bin=4.*7./365,xrange=[2009,2016]
  p1=barplot(x,y,/overplot,fill_color='blue',width=1,color='blue')
  t1=text(0.2,0.8,'Data Queries',color='blue',font_size=14.)

  p2=plot([2009,2016],[0,100],/nodata,xrange=[2009,2016],yrange=[0,100],xminor=4,position=[0.15,0.1,0.9,0.4],/current,xtitle='Year',font_size=14.)
  t2=text(0.04,0.25,'Number of Queries per Month',orientation=90,font_size=14.)
  p3=barplot(year,queries,/overplot,fill_color='red',width=1,color='red')
  t3=text(0.2,0.3,'Helpdesk Queries',color='red',font_size=14.)
  p.save,'~/Fermi/Senior_Review/SR2016/data_queries.png'
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

  prop=0L & lastname='' & dup='' & firstname='' & acycle=0

  readcol,'~/Fermi/Senior_Review/SR2016/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

  prop=prop[1:*]
  lastname=last[1:*]
  firstname=first[1:*]
  approv=approv[1:*]
  pc=pc[1:*]
  inst=inst[1:*]
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

  b=barplot(cycle+2007,cc,xrange=[2008.5,2015.5],ytitle='Cumulative Number of Guest Investigators',fill_color='blue',width=0.4,xtitle='Year',yrange=[0,1600],aspect_ratio=0.002,xminor=0)
  b2=barplot(cycle+2007+0.1,cc5,/overplot,transparency=25,fill_color='red',width=0.4)
  a=axis(0,location=[0,1600],coord_transform=[-2007,1.],title='Cycle',textpos=1,tickdir=1,minor=0)
  t=text(2008.7,1450,'Submitted',color='blue',/data,font_style='bold')
  t=text(2008.7,1330,'Accepted',color='red',/data,font_style='bold')

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

pro mw_plot

  h=4.135e-15 ;; eV s
  c=3e10      ;;cm/s
  yearstart=2014; 2008
  xrange=[yearstart,2021]
  yrange=[1e-8,1e21]

;;; rounded edges not right at beginning for already started missions

  p=plot(xrange,yrange,xrange=xrange,yrange=yrange,/nodata,xtitle='Year',/ylog,yminor=0,ymajor=0,margin=[0.13,0.1,0.13,0.1],xminor=3);,ytitle='Energy (eV)')
  fr=[1e6,1e9,1e12,1e15,1e18,1e21,1e24,1e27] ;,1e30]
  en=h*fr
  yaxis=axis('Y',location=[2021,1.],coord_transform=[1.,1./h],textpos=1,tickdir=1,/log,title='Frequency (Hz)',tickvalues=[1e10,1e15,1e20,1e25],minor=0)
  yaxis=axis('Y',location=[yearstart,1.],/log,title='Energy (eV)',tickformat='loglabels',tickvalues=[1e-5,1,1e5,1e10],minor=0)

  obs=create_struct('name','','ops',fltarr(2),'ext_ops',0.,'wavelength',dblarr(2),$
                    'eng',dblarr(2),'eng_upgrade',dblarr(2),'time_upgrade',0.,$
                    'freq',dblarr(2),$
                    'survey',0,'point',0,'space',0,'ground',0,$
                    'messenger','','color','','backcolor','','xoffset',0.,'yoffset',0.,$
                    'fcolor','','font_size',0.,$
                    'font_style',0,'transparency',0)
  n=70
  obs=replicate(obs,n)
  obs.font_size=9
  obs.font_style=0
  obs.fcolor='white'
  obs.transparency=30
  obs.yoffset=1.

  ;;; Gamma-ray

  i=50
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
  obs[i].backcolor='tomato'

  i=1
  obs[i].name='VERITAS'
  obs[i].ops=[2009,2018]
  obs[i].ext_ops=2021
  obs[i].eng=[0.1,50]*1e9
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=0.8
;  obs[i].xoffset=0.7

  i=2
  obs[i].name='HESS'
  obs[i].ops=[2004,2020];2012.67]
  obs[i].ext_ops=2021;2012.67
  obs[i].eng=[0.3,50]*1e9
  obs[i].eng_upgrade=[0.05,50]*1e9 
  obs[i].time_upgrade=2012.67
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=4

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
  obs[i].name='MAGIC'
  obs[i].ops=[2005,2021]
  obs[i].eng=[0.03,30]*1e9 ;;(TeV->keV)
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=0.3
  
  i=4
  obs[i].name='CTA'
  obs[i].ops=[2019,2021]
  obs[i].eng=[10e6,100e9] ;;keV
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=0.05

  i=9
  obs[i].name='HAWC'
  obs[i].ops=[2015+2./12.,2021] 
  obs[i].eng=[0.1,100]*1e9 ;; keV
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=0
  obs[i].yoffset=2

  i=6
  obs[i].name='Swift-BAT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[15.,150.] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=1.5

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

  i=10
  obs[i].name='Swift-XRT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[0.3,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5

  i=11
  obs[i].name='Chandra'
  obs[i].ops=[1999,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[0.2,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=0.8

  i=12
  obs[i].name='MAXI'
  obs[i].ops=[2009,2021]
  obs[i].eng=[0.3,30] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=1.2

  i=13
  obs[i].name='ASTRO-H'
  obs[i].ops=[2016+2./12,2021]
  obs[i].eng=[0.3,600] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5

  i=16
  obs[i].name='NuStar'
  obs[i].ops=[2012.5,2017]
  obs[i].ext_ops=2021
  obs[i].eng=[3,80] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1.7

  i=15
  obs[i].name='eRosita'
  obs[i].ops=[2017.5,2021]
  obs[i].eng=[0.5,10] ;; keV
  obs[i].point=1
  obs[i].survey=1
  obs[i].space=1
;  obs[i].xoffset=1
  obs[i].yoffset=2.

  i=14
  obs[i].name='XMM-Newton'
  obs[i].ops=[1999,2015]
  obs[i].ext_ops=2021
  obs[i].eng=[0.1,10]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1.7
  obs[i].yoffset=0.8

  i=17
  obs[i].name='Integral'
  obs[i].ops=[2002,2016]
  obs[i].ext_ops=2018
  obs[i].eng=[15.,10e3]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=3
  obs[i].yoffset=10

  i=43
  obs[i].name='NICER'
  obs[i].ops=[2016.9,2018.5]
  obs[i].eng=[0.2,12]
  obs[i].point=1
  obs[i].space=1
  
  i=44
  obs[i].name='Suzaku'
  obs[i].ops=[2005,2015.5]
  obs[i].eng=[0.3,600]
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=1.5

  ;; Optical
  i=20
  obs[i].name='Pan-STARRS'
  obs[i].color=!black
  obs[i].ops=[2010,2021]
  obs[i].wavelength=[0.5,1.0]*1e-4 ;; cm
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=3
  obs[i].xoffset=0.9;1.3

  i=19
  obs[i].name='Swift-UVOT'
  obs[i].ops=[2004,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[150.,650.]*1e-7 ;; nm->cm
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=10
  obs[i].fcolor='black'

  i=18
  obs[i].name='HST'
  obs[i].ops=[1990,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[300,1000]*1e-7
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2

  i=21
  obs[i].name='PTF'
  obs[i].ops=[2009,2021]
  obs[i].ext_ops=2021
  obs[i].wavelength=[490,630]*1e-7 ;; nm->cm
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=2
  obs[i].xoffset=0.5;0.8

  i=41
  obs[i].name='Robopol'
  obs[i].ops=[2013,2021]
  obs[i].ext_ops=2021
  obs[i].wavelength=[500,800]*1e-7
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=2
  obs[i].yoffset=2

  i=42
  obs[i].name='DES'
  obs[i].ops=[2012.75,2021]
  obs[i].ext_ops=2021
  obs[i].wavelength=[400,1200]*1e-7
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=3
  obs[i].yoffset=2

  i=46
  obs[i].name='Gaia'
  obs[i].ops=[2013.97,2018]
  obs[i].ext_ops=2018
  obs[i].wavelength=[320,1000]*1e-7
  obs[i].survey=1
  obs[i].space=1
;  obs[i].xoffset=0.4
  obs[i].yoffset=2

  ;; IR
  i=22
  obs[i].name='Spitzer'
  obs[i].ops=[2003,2017]
  obs[i].ext_ops=2021
  obs[i].wavelength=[3,180]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=2

  i=23
  obs[i].name='Herschel'
  obs[i].ops=[2009.5,2013]
  obs[i].ext_ops=[2012.5]
  obs[i].wavelength=[55,672]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=0.5
  obs[i].xoffset=1.1

  i=24
  obs[i].name='WISE'
  obs[i].ops=[2010,2013]
  obs[i].wavelength=[3,22]*1e-4
  obs[i].ext_ops=2013
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=1.8

  i=34
  obs[i].name='JWST'
  obs[i].ops=[2018,2030]
  obs[i].wavelength=[0.6,28]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2
  obs[i].xoffset=0.2

  ;; RADIO

  i=25
  obs[i].name='LOFAR'
  obs[i].ops=[2011,2021]
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2

  i=26
  obs[i].name='VLBA'
  obs[i].ops=[1993,2021]
  obs[i].freq=[0.312,90]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=0.5

  i=27
  obs[i].name='JVLA'
  obs[i].ops=[2010,2021]
  obs[i].freq=[1.,50.]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.6
  obs[i].yoffset=2

  i=28
  obs[i].name='SKA-1'
  obs[i].ops=[2018,2021]
  obs[i].freq=[0.1,25]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].yoffset=0.4
  obs[i].xoffset=0.3

  i=29
  obs[i].name='Apertif'
  obs[i].ops=[2016,2021]
  obs[i].freq=[1.,1.7]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
;  obs[i].xoffset=1.5
  obs[i].yoffset=1.5

  i=48
  obs[i].name='MeerKAT'
  obs[i].ops=[2016,2021]
  obs[i].freq=[0.58,14.5]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].yoffset=4
  obs[i].xoffset=0.6

  i=49
  obs[i].name='ASKAP'
  obs[i].ops=[2015,2021]
  obs[i].freq=[0.7,1.8]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=1.5
;  obs[i].xoffset=0.5

  i=7
  obs[i].name='ALMA'
  obs[i].ops=[2013,2021]
  obs[i].freq=[84,720]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
;  obs[i].yoffset=2
  obs[i].xoffset=0.6

  i=33
  obs[i].name='Planck'
  obs[i].ops=[2009.5,2013]
  obs[i].ext_ops=2013
  obs[i].freq=[27,1000]*1d9 ;; Hz
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=2

  i=32
  obs[i].name='Arecibo'
  obs[i].ops=[1997,2021]
  obs[i].ext_ops=2021
  obs[i].freq=[0.312,10.2]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.75

  i=45
  obs[i].name='MWA'
  obs[i].ops=[2012,2021]
  obs[i].ext_ops=2021
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2
  obs[i].xoffset=1

  i=47
  obs[i].name='ATCA'
  obs[i].ops=[2010,2021]
  obs[i].ext_ops=2021
  obs[i].freq=[1.1,150]*1e9 ;; GHz->Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=1.5

  ;;; MULTI-MESSENGER
  i=35
  obs[i].name='IceCube'
  obs[i].ops=[2010,2021]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e14,1e15]
  obs[i].color='dim grey'
  obs[i].yoffset=2

  i=36
  obs[i].name='Auger'
  obs[i].ops=[2004,2021]
  obs[i].messenger='UHECRs'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e11,1e12]
  obs[i].color='black'
  obs[i].yoffset=2

  i=37
  obs[i].name='CALET'
  obs[i].ops=[2015.62,2016.67]
;  obs[i].ext_ops=[2018]
  obs[i].space=1
  obs[i].survey=1
  obs[i].messenger='Electron'
  obs[i].eng=[1e15,1e16]
  obs[i].color='grey'
  obs[i].fcolor='black'
  obs[i].yoffset=2

  i=38
  obs[i].name='Advanced LIGO/Virgo'
  obs[i].ops=[2015.67,2021]
  obs[i].messenger='Gravitational Waves'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e16,1e17]
  obs[i].color='dark grey'
  obs[i].fcolor='black'
  obs[i].yoffset=2

  i=39
  obs[i].name='AMS'
  obs[i].ops=[2011.4,2021]
  obs[i].messenger='Cosmic Rays'
  obs[i].space=1
  obs[i].survey=1
  obs[i].eng=[1e13,1e14]
  obs[i].color='slate grey'
  obs[i].yoffset=2

  i=40
  obs[i].name='ANTARES'
  obs[i].ops=[2008,2021]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e12,1e13]
  obs[i].color='dark slate grey'
  obs[i].yoffset=2

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

  w=where(obs.point eq 1 and obs.messenger eq '')
  obs[w].color='blue'
  obs[w].backcolor='light blue'

  w=where(obs.survey eq 1 and obs.messenger eq '')
  obs[w].color='lime green'
  obs[w].backcolor='pale green'

  w=where(obs.survey eq 1 or obs.name eq 'Fermi')
  obs=obs[w] ;;; ONLY SURVEY
  n=n_elements(obs)
  right=right[w]

  p.refresh,/disable
  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        curved_polygon,obs[i].ops[0],obs[i].ops[1],obs[i].eng[0],obs[i].eng[1],xx,yy,right=right[i],/ylog
;;; insert ext_eng into xx,yy somehow - merge 2 curved polys?
        poly=polygon(xx,yy,/data,transparency=obs[i].transparency,fill_color=obs[i].color,color='black')
        if obs[i].ext_ops ne obs[i].ops[1] and obs[i].ext_ops ne 0. then begin
           curved_polygon,obs[i].ops[1],obs[i].ext_ops,obs[i].eng[0],obs[i].eng[1],xx,yy,/right,/ylog,ym=obs[i].eng_upgrade,xm=obs[i].time_upgrade
           poly=polygon(xx,yy,/data,fill_color=strtrim(obs[i].backcolor,2),transparency=70,thick=2,color=obs[i].color,linestyle='--')
        endif 
     endif 
  endfor 

  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        if obs[i].messenger ne '' then mess=' ('+strtrim(obs[i].messenger,2)+')' else mess=''
        t=text(obs[i].ops[0]+obs[i].xoffset+0.1,10^((alog10(obs[i].eng[1])+alog10(obs[i].eng[0]))/2.)/5.*obs[i].yoffset,obs[i].name+mess,/data,font_color=obs[i].fcolor,font_style=obs[i].font_style,font_size=obs[i].font_size)
     endif 
  endfor      
;  leg=legend(label=['Survey','Pointing'],target=p,/data,position=[2010.1,1e20],text_color='black')
;  l1=text(yearstart+.1,1.5e19,'Survey',font_color='lime green',/data,font_style=1)
;  l2=text(yearstart+.1,1.5e18,'Pointing',font_color='blue',/data,font_style=1)
  p.save,'~/Fermi/Senior_Review/SR2016/mw_plot.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close
  
  stop
  return
end 
