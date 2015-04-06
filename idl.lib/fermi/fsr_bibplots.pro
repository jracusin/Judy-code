pro theses_plot

  cd,'~/Fermi/Senior_Review'
  readcol,'Fermi_thesis_information.csv',title,author,institution,date,$;bib_ref,link,cat,InstrAnalysisRev,AGN,Catalogs,Dark_Matter,Extragal_Dif,GRBs,Gal_Binaries,Galactic_Dif,Galaxies,Pulsars,SNRPWN,Solar_System,Other
delim=',',skip=1,format='(a,a,a,a)';,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)'

  !y.margin=[2,8]
  begplot,name='Fermi_theses.eps',/land,/color,font='helvetica'
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 

;  w=where(date ge 2008 and date le 2011)
  w=where(date le 2011)
  plothist,date[w],xtitle='Year',ytitle='!4Fermi !XTheses per year',/fill,fcolor=!blue,xrange=[2000,2012],/xsty,charsize=2
  axis,xaxis=0,xrange=[2000,2012],/xsty,xtickformat='(A1)'

  endplot

  spawn,'convert Fermi_theses.eps Fermi_theses.pdf'
  stop
return
end 
pro read_bib,p
  
  readcol,'fermi_bibcodes.csv',title,author,date,bibcode,citation,category,subject,format='(a,a,a,a,a,a,a)',delim=',',skip=1

;  file='fermi_bibcodes.csv'
;  readcol,file,lines,format='(a)',delim='%',stringskip='ERRATUM'
;  n=numlines(file)
  n=n_elements(title)
  p=create_struct('bibcode','','month',0,'year',0,'date',0.,'citation',0,'analysis',0,'refer',0,$
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
     p[i].year=strmid(date[i],4,4)
     
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

  p.date=(p.month-1)/12.+p.year ;;; does this -1 make sense?
  s=sort(p.date)
  p=p[s]

  mwrfits,p,'~/Fermi/Senior_Review/Fermi_bibliography.fits',/create
  
  return
end 

pro fsr_bibplots_stats

  readcol,'fermi-stats-NA/Sheet1-Table\ 1.csv',lines,format='(a)',delim='$'
;  readcol,'fermi_bibcodes_112211.txt',bibcode,month,year,citation,category,subject,author,title,format='(a,a,a,a,a,a,a,a)',delim=' '
  n=n_elements(lines)
  
  ny=7
  bib=create_struct('year',intarr(ny),'subject','','papers',fltarr(ny),'total_papers',0,'total_cit',0,'total_cit_pap',0.)
  bib=replicate(bib,30)

  ;; total subject/year citations/subject papers/year citations/year citations/paper/year
  chunks=strsplit(lines[2],',',/extract)
  bib.year=chunks[1:7]

  for i=4,31-1 do begin
     j=i-4
;     print,lines[i],j
     chunks=strsplit(lines[i],',',/extract)
     if chunks[0] ne '' then begin 
        bib[j].subject=chunks[0]
        bib[j].papers=chunks[1:7]*1.
        bib[j].total_papers=chunks[8]*1.
        if n_elements(chunks) gt 9 and i ne 25 then begin 
           bib[j].total_cit=chunks[9]*1.
           if n_elements(chunks) gt 10 then bib[j].total_cit_pap=chunks[10]*1.
        endif 
;print,bib[j].subject,i,j
     endif 
  endfor 
  w=where(bib.subject ne 'Other')
  bib=bib[w]
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot of subjects as separate histograms per year
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  begplot,name='~/Fermi/Senior_Review/bib_plot_subject_year.eps',/color,/land,font='helvetica'
  !y.margin=[1,1]
  !x.margin=[2,0]
  plot,[0,15],[0,80],/nodata,ytitle='# of Papers per Year',/xsty,/ysty,xrange=[0,14.5],xtickformat='(A1)',yrange=[0,85],charsize=2
  off=findgen(9)/9.
  off=off-off[3]
  color=[!navyblue,!blue,!dodgerblue,!cyan,!orange,!red,!firebrick]

;  y=[85,15,53,73,30,50,35,40,30,25,15,60,40,15,20]
  y=[-3,-3,-3,-3,-3,-5,-3,-3,-5,-3,-3,-3,-3,-3]
  x=[0,-0.2,0,0,0,0,-0.2,0,-0.1,0.1,0.1,0.1,0,0]

  for i=0,13 do begin
;     oplot,(i+1.)+off,[0,bib[i].papers,0],psym=10
     for j=1,7 do begin 
        x0=i+1.+off[j-1]
        x1=i+1+off[j]
        y0=0
        y1=bib[i].papers[j-1]
        polyfill,[x0,x1,x1,x0,x0],[y0,y0,y1,y1,y0],color=color[j-1],/fill
     endfor 
     sub=bib[i].subject
     st=strsplit(sub,' /',/extract)
     f=[0,2,4]
;     for k=0,n_elements(st)-1 do xyouts,i+0.8,y[i]-f[k],st[k]
     for k=0,n_elements(st)-1 do xyouts,i+0.6+x[i],y[i]-f[k],st[k],charsize=1
;     xyouts,i+0.8,-6-f+4*(i mod 2),st,charsize=1.
  endfor 

  legend,ntostr(bib[0].year),box=0,/top,/right,textcolor=color
  endplot
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot of the number of papers & citations per year split into
;; categories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='~/Fermi/Senior_Review/bib_plot_papers_citations_year.eps',/color,/land,font='helvetica'
  !y.margin=[2,1]
  !x.margin=[4,4]
  plot,[2004,2012],[0,1500],/nodata,ytitle='Papers',/xsty,/ysty,xrange=[2005,2011],yrange=[0,1200],ytick_get=ytick,xtitle='Year',charsize=2
  year=indgen(7)+2005
  plotsym,0,1,/fill
  ;; total
  oplot,year,cumulative(bib[16].papers),thick=20
  oplot,year,cumulative(bib[17].papers/20.),color=!red,thick=20

  ;; analysis
  oplot,year,cumulative(bib[21].papers),thick=10,line=1
;  oplot,year,cumulative(bib[21].papers/10.),color=!red,thick=2,line=1
  
  ;; refers to results
  oplot,year,cumulative(bib[22].papers),thick=10,line=2
;  oplot,year,cumulative(bib[22].papers/10.),color=!red,thick=2,line=2

  ;; predicts results
  oplot,year,cumulative(bib[23].papers),thick=10,line=3
;  oplot,year,cumulative(bib[23].papers/10.),color=!red,thick=2,line=3

  ;; instrumentation
  oplot,year,cumulative(bib[24].papers),thick=10,line=4
;  oplot,year,cumulative(bib[24].papers/10.),color=!red,thick=2,line=4

  axis,yaxis=1,ytitle='Citations',yrange=[0,12000],/ysty,ytickname=ntostr(fix(ytick*10.)),color=!red,charsize=2
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 

  oplot,[2008.5,2008.5],[0,3000],line=2,color=!blue
  xyouts,2008.3,1500,'!4Fermi !XLaunch',orientation=90,color=!blue,charsize=2
  
;  legend,['Cumulative Papers','Cumulative Citations','Average Citations/paper'],/top,/left,textcolor=[!p.color,!red,!grey50],box=0
;  legend,['Cumulative Papers','Cumulative Citations'],/top,/left,textcolor=[!p.color,!red],box=0
  legend,'Papers',box=0,/top,/left,charsize=2
  legend,'Citations',box=0,/top,/right,textcolor=!red,charsize=2

  legend,['Analysis of Data','Interprets Results','Predicts Results','Instrumentation','Total'],box=0,/left,/center,line=[1,2,3,4,0],thick=[10,10,10,10,20]
;  xyouts,-100,2004,'* 2011 data not fully populated',/data

  endplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='~/Fermi/Senior_Review/bib_plot_papers_subject_year.eps',/color,/land,font='helvetica'
  !y.margin=[0,1]
  !x.margin=[2,1]
  plot,[2004,2012],[0,600],/nodata,ytitle='# of Papers per year',xtitle='Year',/xsty,/ysty,xrange=[2003,2012],yrange=[0,100],xticks=9;,xtickname=[ntostr(year)]

  ;axis,yaxis=1,ytitle='# of Citations',yrange=[0,8000],/ysty,ytickname=ntostr(fix(ytick*10.)),color=!red
  
  off=findgen(15)/20.
  off=off-off[7]
  year=indgen(7)+2005
  
  colors=[!darkred,!sienna,!red,!salmon,!orange,!lightgreen,!green,!seagreen,!dodgerblue,!blue,!cyan,!magenta,!violet,!purple]
  avg=fltarr(7)
  for i=0,6 do begin
     for j=0,13 do begin 
        x0=year[i]+off[j]
        x1=year[i]+off[j+1]
        y0=0
        y1=bib[j].papers[i]
        polyfill,[x0,x1,x1,x0,x0],[y0,y0,y1,y1,y0],color=colors[j],/fill
     endfor 
     avg[i]=mean(bib[0:13].papers[i])
;     sub=bib[i].subject
;     st=strsplit(sub,' /',/extract)
;     f=[0,2,4]
;     for k=0,n_elements(st)-1 do xyouts,i+0.6+x[i],y[i]-f[k],st[k],charsize=1
  endfor 

;  oplot,year,bib[14].papers/10.,psym=8
  oplot,year,cumulative(bib[14].papers/20.),thick=30
;  oplot,year,bib[17].papers/100.,psym=8,color=!grey50
  oplot,year,cumulative(bib[17].papers/200.),color=!grey50,thick=30
;  oplot,year,bib[18].papers,psym=8,color=!grey80
  oplot,year,bib[18].papers/2.,thick=30,color=!grey70

;  oplot,year,avg,psym=8
;  oplot,year,avg
  legend,bib[0:13].subject,textcolor=colors,box=0,/top,/left
;  xyouts,2003.6,34,'Cumulative'

  xyouts,2003.4,30,'Cumulative Citations / 200',color=!grey40
  xyouts,2003.4,26,'Cumulative Papers / 20',/data
  xyouts,2003.4,22,'Average Citations/paper / 2',color=!grey60


  endplot


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot of subjects (stacked in different colors) as function of time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  year=indgen(7)+2005
  
  begplot,name='~/Fermi/Senior_Review/bib_plots_papers_stacked.eps',/land,/color,font='helvetica'
  colors=[!darkred,!sienna,!red,!salmon,!orange,!green,!seagreen,!darkgreen,!dodgerblue,!blue,!cyan,!magenta,!violet,!purple]
    plot,[2004,2012],[0,600],/nodata,ytitle='# of Papers per year',xtitle='Year',/xsty,/ysty,xrange=[2004,2012],yrange=[0,600],xticks=8;,xminor=4;,xtickname=[ntostr(year)]

  avg=fltarr(7)
  for i=0,6 do begin
     y1=0
     for j=0,13 do begin 
        x0=year[i]-0.2
        x1=year[i]+0.3
        y0=y1
        y1=bib[j].papers[i]+y1
        if bib[j].papers[i] gt 0 then $
           polyfill,[x0,x1,x1,x0,x0],[y0,y0,y1,y1,y0],color=colors[j],/fill
;        print,year[i],bib[j].subject,bib[j].papers[i]
     endfor 
     if i eq 6 then oplot,[x1,x1,x0,x0],[y1,y1*4./3,y1*4./3,y1],line=2
     print,total(bib[0:13].papers[i])
     avg[i]=mean(bib[0:13].papers[i])
;     sub=bib[i].subject
;     st=strsplit(sub,' /',/extract)
;     f=[0,2,4]
;     for k=0,n_elements(st)-1 do xyouts,i+0.6+x[i],y[i]-f[k],st[k],charsize=1
  endfor 

  axis,xaxis=0,xticks=8,xminor=4
;  polyfill,[2004.2,2007,2007,2004.2,2004.2],[250,250,580,580,250],color=!grey80

  legend,bib[0:13].subject,textcolor=colors,box=0,/top,/left
  endplot
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot distribution of number of authors on Fermi papers, to
;; show it's not the team that dominates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  nauth=0
;  for i=0,n_elements(auth)-1 do begin
  files=['2009APh____32__193A.txt','2008A+A___492__923S.txt']
  for j=0,n_elements(files)-1 do begin 
     openr,lun,files[j],/get_lun
     
     i=0
     a=0
     au=''

     if j eq 0 then na=0

      while not eof(lun) do begin 
        line=readline(lun)
        if line[0] eq '' then begin 
           na=[na,a-1]
;           print,i,na[i+1],au
           a=0
           au='' 
           i=i+1 
;           k=get_kbrd(10)
        endif else begin 
           a=a+n_elements(line)
           au=[au,line]

        endelse 
     endwhile
  endfor 
  na=na[1:*]

;  !x.margin=[6,2]
;  !y.margin=[4,2]
  
  begplot,name='~/Fermi/Senior_Review/bib_plot_n_authors.eps',/land,/color,font='helvetica'
  plothist,na,bin=5,xtitle='Number of Authors on Papers presenting analysis of Fermi data',ytitle='Number of Papers',/fill
  endplot
  free_lun,lun
  
  stop
return
end 

pro fsr_bibplots
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; plot using new bib interface
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  read_bib,p

  cd,'~/Fermi/Senior_Review/'
  bib=mrdfits('Fermi_bibliography.fits',1)
  nbib=n_elements(bib)
;  year=(bib.month-1)/12.+bib.year ;;; does this -1 make sense?
;  s=sort(year)
;  bib=bib[s]
;  year=year[s]
  year=bib.date

  w=where(year gt 2008.5,nbib)
  bib=bib[w]
  year=year[w]
  ind=intarr(nbib)
  ind[*]=1
  cind=cumulative(ind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  plot of papers & citations as function of time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='bib_papers_citations_total.eps',/land,/encap,/color,font='helvetica'
  !y.margin=[4,0]
  !x.margin=[5,5]
  th=10

  multiplot,[2,1],/init
  multiplot
  plot,[2008,2012],[0,1000],/nodata,/xsty,/ysty,xrange=[2008.5,2012],yrange=[0,1000],xtitle='Year',charsize=2,xticks=2,xtickv=[2009,2010,2011],xminor=11,ytick_get=ytick
  plotsym,0,1,/fill

  axis,yaxis=0,ytitle='Papers',yrange=[0,1000],/ysty,ytickname=ntostr(fix(ytick)),color=!blue,charsize=2
  
  oplot,year,cind,thick=th,color=!blue
  legend,['Analysis of Data','Refers to Results','Predicts Results','Instrumentation','Total'],box=0,/left,/top,line=[1,2,3,4,0],thick=[10,10,10,10,10]

  a=where(bib.analysis)
  r=where(bib.refer)
  p=where(bib.predict)
  i=where(bib.instr)

  oplot,year[a],cumulative(ind[a]),line=1,thick=th,color=!blue
  oplot,year[r],cumulative(ind[r]),line=2,thick=th,color=!blue
  oplot,year[p],cumulative(ind[p]),line=3,thick=th,color=!blue
  oplot,year[i],cumulative(ind[i]),line=4,thick=th,color=!blue

  multiplot
  plot,[2008,2012],[0,15000],/nodata,/xsty,/ysty,xrange=[2008.5,2012],yrange=[0,20000],ytick_get=ytick2,xtitle='Year',charsize=2,xticks=3,xtickv=[2009,2010,2011,2012],xminor=11

  oplot,year,cumulative(bib.citation),color=!red,thick=th
  oplot,year[a],cumulative(bib[a].citation),line=1,thick=th,color=!red
  oplot,year[r],cumulative(bib[r].citation),line=2,thick=th,color=!red
  oplot,year[p],cumulative(bib[p].citation),line=3,thick=th,color=!red
  oplot,year[i],cumulative(bib[i].citation),line=4,thick=th,color=!red

  axis,yaxis=1,ytitle='Citations',yrange=[0,20000],/ysty,ytickname=ntostr(fix(ytick2)),color=!red,charsize=2

;  oplot,[2008.5,2008.5],[0,3000],line=2,color=!blue
;  xyouts,2008.3,500,'Fermi Launch',orientation=90,color=!blue,charsize=2
  
;  legend,'Papers',box=0,/top,/left,charsize=2
;  legend,'Citations',box=0,/top,/right,textcolor=!red,charsize=2

  endplot

  spawn,'convert bib_papers_citations_total.eps bib_papers_citations_total.pdf'

  multiplot,/reset,/default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  plot of papers as function of time (single panel of previous plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='bib_papers_category.eps',/land,/encap,/color,font='helvetica'
  !y.margin=[5,6]
  !x.margin=[5,5]
  th=10
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 
  plot,[2008,2012],[0,1000],/nodata,/xsty,/ysty,xrange=[2008.5,2012],yrange=[0,1000],xtitle='Year',charsize=2,xminor=0,ytitle='Cumulative !4Fermi !XPapers',ytick_get=ytick,xticks=3,xtickv=[2009,2010,2011,2012]
  plotsym,0,1,/fill
  axis,xaxis=0,xminor=12,xticks=3,xtickv=[2009,2010,2011,2012],xtickformat='(A1)'
;  axis,yaxis=0,ytitle='Papers',yrange=[0,1000],/ysty,ytickname=ntostr(fix(ytick)),color=!blue,charsize=2
  n=n_elements(year)
  oplot,[year+1./24,2012],[cind,cind[n-1]],thick=th
;  legend,['Analysis of Data','Refers to Results','Predicts Results','Total'],box=0,/left,/top,line=[1,2,3,0],thick=[10,10,10,10],color=[!blue,!red,!green,!p.color],charsize=2

  a=where(bib.analysis)
  r=where(bib.refer)
  p=where(bib.predict)
  i=where(bib.instr)

  oplot,[2009.,2009.]+7./12.,[0,1000],line=1
  xyouts,2009.5,250,'Public Data Release',orient=90
;  oplot,year[a],cumulative(ind[a]),line=1,thick=th,color=!blue
;  oplot,year[r],cumulative(ind[r]),line=2,thick=th,color=!red
;  oplot,year[p],cumulative(ind[p]),line=3,thick=th,color=!green
;  oplot,year[i],cumulative(ind[i]),line=4,thick=th,color=!orange

  endplot

  spawn,'convert bib_papers_category.eps bib_papers_category.pdf'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot of subjects (stacked in different colors) as function of time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='~/Fermi/Senior_Review/bib_plots_papers_stacked.eps',/color,font='helvetica'
  colors=[!darkred,!sienna,!red,!salmon,!orange,!green,!seagreen,!darkgreen,!dodgerblue,!blue,!cyan,!purple,!violet,!magenta,!deeppink]
  !y.margin=[1,0]
  !x.margin=[4,0]
  multiplot,[1,4],/init

  month=3./12
  if month eq 1 then yrange=[0,400]
  if month eq 3./12. then yrange=[0,150]
  if month eq 6./12. then yrange=[0,200]
  what=['Total','Analysis of Data','Interprets Results','Predicts Results','Instrumentation','Total']

  yr=[120,60,90,50,10]
  bib0=bib
  year0=year
  for b=0,3 do begin 
     yrange=[0,yr[b]]
     multiplot
     bib=bib0
     year=year0
     case b of
        0: z=indgen(n_elements(bib))
        1: z=where(bib.analysis)
        2: z=where(bib.refer)
        3: z=where(bib.predict)
        4: z=where(bib.instr)
     endcase 
     bib=bib[z]
     year=year[z]

     if b eq 2 then ytitle='!4Fermi !XPapers' else ytitle=''
     if b eq 3 then xtitle='Year' else xtitle=''
     plot,[2008,2012],[0,1],/nodata,ytitle=ytitle,xtitle=xtitle,/xsty,/ysty,xrange=[2008.5,2012],yrange=yrange,charsize=2,xticks=3,xtickv=[2009,2010,2011,2012],yminor=4

     nbiny=fix(1./month)
;     if month ne 1. then nmis=3.5 else nmis=4.
     nmis=3.5
     y1=lonarr(nbiny*nmis)
     y0=y1
     tags=tag_names(bib)
     tags[[10,11,15,16,17,18,19,20,22]]=['Catalogs','CRs','Binary','Diffuse','Galaxies','Instr/Analysis/Review','Other','Pulsars','Solar System']
     p=intarr(n_elements(bib))
     tnw=0
     tnq=0
     for j=0,14 do begin
        k=10+j
        w=where(bib.(k) eq 1,nw)
        if nw gt 0 then begin 
           q=where(p[w] eq 0,nq)
           if nq gt 0 then begin 
;           print,nw,nq
           w=w[q]
           p[w]=1
           tnw=tnw+nw
           tnq=tnq+nq
;           print,tags[k]

           n=nbiny*nmis+1
           x=[2008.5+indgen(n)*month,2013]
           y=intarr(n)
           for u=0,n-1 do begin 
;              crap=where(year[w]-1./12 ge x[u] and year[w]-1./12 lt x[u+1],nu)
              crap=where(year[w] ge x[u] and year[w] lt x[u+1],nu)
              y[u]=nu
           endfor 

;if b eq 4 then stop
           x=x[0:n-2]
;           if nq gt 2 then plothist,year[w]-1./12.,x,y,bin=month,/noplot,xrange=[2008,2012] else begin
;              x=year[w[0]]
;              y=nq
;           endelse 
;           if month eq 1 then l=x-2008.5 else l=(x-2008.5)*nbiny-0.5
           l=(x-2008.5)*nbiny
           y1[l]=y[l]+y1[l]
           x1=2008.5+findgen(nbiny*nmis)*month
           if month eq 0.5 then missing=2.
           if month eq 3./12. then missing=0
           if month eq 1. then missing=4./3
;           y1[nbiny*nmis-1]=y0[nbiny*nmis-1]+(y1[nbiny*nmis-1]-y0[nbiny*nmis-1])*missing
           for i=0,nbiny*nmis-1 do begin
;           if b eq 1 and i eq 12 then stop

              x2=[x1[i],x1[i],x1[i]+month,x1[i]+month,x1[i]]
;        if i gt 0 then 
              y2=[y0[i],y1[i],y1[i],y0[i],y0[i]] ; else y2=[0,y1[i],y1[i],0,0]
;              if b eq 4 then print,x1,y1
              if y2[1] ne 0 then polyfill,x2,y2,color=colors[j],/fill
;              if i eq 12 then stop
           endfor
           y0=y1
        endif 
        endif 
     endfor 
     if b eq 0 then legend,tags[10:15],textcolor=colors[0:5],box=0,charsize=1.5,pos=[2008.6,115]
     if b eq 1 then legend,tags[16:20],textcolor=colors[6:10],box=0,charsize=1.5,pos=[2008.6,55]
     if b eq 2 then legend,tags[21:24],textcolor=colors[11:14],box=0,charsize=1.5,pos=[2008.6,70]
;     print,tnw,tnq
     axis,xaxis=0,xtickformat='(A1)'
     axis,yaxis=1,ytickformat='(A1)',yminor=4
     legend,what[b],box=0,/top,/right
  endfor
  multiplot,/reset,/default
  endplot

  spawn,'convert bib_plots_papers_stacked.eps bib_plots_papers_stacked.pdf'
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; papers stacked by subject - only total
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  begplot,name='~/Fermi/Senior_Review/bib_plots_papers_stacked_simp.eps',/color,font='helvetica',/land,/encap
  colors=[!darkred,!sienna,!red,!salmon,!orange,!green,!seagreen,!darkgreen,!dodgerblue,!blue,!cyan,!purple,!violet,!magenta,!deeppink]
  !y.margin=[4,0]
  !x.margin=[4,0]
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 

  year=[2008,2009,2010,2011,2012.]
  ny=n_elements(year)
  plot,[2007,2017],[0,1000],/nodata,/xsty,/ysty,xrange=[2007.5,2011.5],yrange=[0,500],charsize=2,ytitle='!4Fermi !XPapers',xtitle='Year',xticks=3,xtickv=[2008,2009,2010,2011],xminor=11
  m=0.2
  for i=0,ny-2 do begin 
     q=where(year0 ge year[i] and year0 lt year[i+1])
     np=0
     x1=[-m,m,m,-m,-m]+year[i]
     for j=0,14 do begin 
        k=9+j
        w=where(bib0[q].(k) eq 1,nw)
        y1=[nw+np,nw+np,np,np,nw+np]
        polyfill,x1,y1,color=colors[j],/fill 
;        print,x1,y1
        
        np=np+nw
     endfor 
  endfor 

  axis,xaxis=0,xtickformat='(A1)',xminor=11,xticks=3,xtickv=[2008,2009,2010,2011]
  legend,tags[9:23],textcolor=colors,box=0,charsize=1.5,/top,/left
  endplot

  spawn,'convert bib_plots_papers_stacked_simp.eps bib_plots_papers_stacked_simp.pdf'


  stop




end
