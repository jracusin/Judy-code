pro gi_cross

  readcol,'LAT_people.txt',fname,lname,format='(a,a)',delim=' '
  i=indgen(n_elements(lname))
  nasa=i[0:38]
  doe=i[39:63]
  gbm=i[64:*]

  readcol,'GI_coIs2.csv',prop,last,first,mid,inst,country,gi,format='(l,a,a,a,a,a,a)',delim=','
  w=where(strtrim(gi,2) eq 'PI')
  prop=prop[w]
  last=last[w]
  first=first[w]
  n=intarr(5)
  for i=1,4 do begin
     print,'Cycle ',i,' NASA-LAT'
     q=where(prop ge 10000L*i and prop lt 10000L*(i+1),nw)
     n[i]=nw
     match,strupcase(lname[nasa]),last[q],m1,m2
     if m1[0] ne -1 then begin 
        print,'N people = ',n_elements(m1)
        colprint,fname[nasa[m1]]+' '+lname[nasa[m1]]
        print,'------------'
        colprint,first[q[m2]]+' '+last[q[m2]]
        print,'**********************'
     endif  else print,'N people = 0'

     print,'Cycle ',i,' NASA-GBM'
     q=where(prop ge 10000L*i and prop lt 10000L*(i+1),nw)
     match,strupcase(lname[gbm]),last[q],m1,m2
     if m1[0] ne -1 then begin 
        print,'N people = ',n_elements(m1)
        colprint,fname[gbm[m1]]+' '+lname[gbm[m1]]
        print,'------------'
        colprint,first[q[m2]]+' '+last[q[m2]]
        print,'**********************'
     endif  else print,'N people = 0'

     match,strupcase(lname[doe]),last[q],m1,m2
     print,'Cycle ',i,' DOE'
     if m1[0] ne -1 then begin 
        print,'N people = ',n_elements(m1)
        colprint,fname[doe[m1]]+' '+lname[doe[m1]]
        print,'------------'
        colprint,first[q[m2]]+' '+last[q[m2]]
     endif else print,'N people = 0'

     print,'++++++++++++++++++++++++++++++++++++++++++++++++++++'
  endfor 
  print,n
  return
end 

pro em_spec

  d2=dblarr(2)
  em=create_struct('reg','','energy',d2,'freq',d2,'wave',d2,'color',0L,'yoff',0.)
  em=replicate(em,11)
  begplot,name='~/Fermi/Senior_Review/spectrum.eps',/encap,/land,font='helvetica',/color

  i=0
  em[i].reg='Radio'
  em[i].freq=[3e6,3e9]
  em[i].color=!grey20
  i=1
  em[i].reg='NIR'
  em[i].wave=[0.7,4]*1e-6
  em[i].color=!grey30
  i=2
  em[i].reg='IR'
  em[i].wave=[4,1000]*1e-6
  em[i].color=!red
;  em[i].yoff=-0.05
  i=3
  em[i].reg='Optical'
  em[i].wave=[300.,700]*1e-9
  em[i].color=!green
  em[i].yoff=-0.03
  i=4
  em[i].reg='UV'
  em[i].wave=[10,100]*1e-9
  em[i].color=!blue
  i=5
  em[i].reg='';Far-UV'
  em[i].wave=[1,50]*1e-9
  em[i].color=!purple
  em[i].yoff=-0.03
  i=6
  em[i].reg='X-ray'
  em[i].energy=[0.1,10.0]*1e3
  em[i].color=!grey40
  i=7
  em[i].reg='Hard X-ray'
  em[i].energy=[10,500]*1e3
  em[i].color=!grey60
  em[i].yoff=-0.03
  i=8
  em[i].reg='Gamma-ray'
  em[i].energy=[500,1e8]*1e3
  em[i].color=!grey70
;  i=9
;  em[i].reg='HE Gamma-ray'
;  em[i].energy=[10,100e3]*1e6
;  em[i].color=!grey80
;  i=10
;  em[i].reg='VHE'
;  em[i].energy=[100,10e3]*1e9
;  em[i].color=!grey90

  c=3e8
  h=4.135d-15 ;; ev s
;  h2=6.626d-34 ;; J s
;  j2ev = 6.24150974d18 

  w=where(em.freq[0] eq 0 and em.energy[0] eq 0)
  em[w].freq=c/em[w].wave
  em[w].energy=h*em[w].freq
  w=where(em.wave[0] eq 0 and em.energy[0] eq 0)
  em[w].wave=c/em[w].freq
  em[w].energy=h*em[w].freq
  w=where(em.freq[0] eq 0)
  em[w].freq=em[w].energy/h
  em[w].wave=c/em[w].freq

  wrange=[1e-18,1e3]
  erange=h*c/wrange
  frange=erange/h
  x=[0.2,0.27,0.34]
  w=(10^(dindgen(23)-20.)) ;;1e-22 1e3
  f=reverse(10^(dindgen(23)+6));e/h
  wf=c/f
  e=reverse(10^(dindgen(23)-8))
  we=h*c/e
;; e=h*c/lambda
;; e=h*nu
;; nu=c/lambda
;; lambda=h*c/nu
;; lambda=c/e
  
  !p.background=!blue
;  !p.color=!white
  !p.charsize=0.7
  plot,[1e-21,1e4],[0,1],/nodata,/xlog,/xsty,xrange=[1e4,1e-21];,color=!white

;     oplot,[1e-22,1e-22,1e-22,1e3,1e3,1e3],[x,x],psym=8

  for i=0,8 do begin
     q=em[i].wave
     xx=[q[0],q[0],q[1],q[1],q[0]]
     yy=[0,1,1,0,0]
;     polyfill,xx,yy,color=em[i].color,/data
     xyouts,max(q),0.4+em[i].yoff,em[i].reg,/data
;     oplot,[q[0],q[0]],[0,1]
;     oplot,[q[1],q[1]],[0,1]
  endfor 
  oplot,[1e3,1e-20],[x[0],x[0]],thick=5;,color=!red
  oplot,[1e3,1e-20],[x[1],x[1]],thick=5
  oplot,[1e3,1e-20],[x[2],x[2]],thick=5
  tx=[1,0.4,0.4,1]
  tx2=[0.4,1,1,0.4]
  ty=[0,0.015,-0.015,0]
  for i=0,2 do begin 
     polyfill,tx2*1d-20,ty+x[i]
     polyfill,tx*1d3,ty+x[i]
  endfor 

  fact=0.7
  a=0.01
  b=0.03
  c=0.05
  for i=1,n_elements(w)-1 do begin
;     if alog10(w[i]) mod 5 eq 0 then begin
;        a=0.02 
        if alog10(w[i]) le -10 then exp='10!U'+ntostr(alog10(w[i]),3)+'!N' 
        if alog10(w[i]) gt -10 then exp='10!U'+ntostr(alog10(w[i]),2)+'!N' 
        if alog10(w[i]) gt 0 then exp='10!U'+ntostr(alog10(w[i]),1)+'!N' 
        if alog10(w[i]) eq 0 then exp='1'
        xyouts,w[i]*fact,x[0]-b,exp,/data
        if alog10(e[i]) gt 0 then exp='10!U'+ntostr(alog10(e[i]),1)+'!N' 
        if alog10(e[i]) ge 10 then exp='10!U'+ntostr(alog10(e[i]),2)+'!N' 
        if alog10(e[i]) le 0 then exp='10!U'+ntostr(alog10(e[i]),2)+'!N' 
        if alog10(e[i]) eq 0 then exp='1'
        xyouts,we[i]*fact,x[1]-b,exp,/data
        if alog10(f[i]) gt 0 then exp='10!U'+ntostr(alog10(f[i]),1)+'!N' 
        if alog10(f[i]) ge 10 then exp='10!U'+ntostr(alog10(f[i]),2)+'!N' 
        if alog10(f[i]) le 0 then exp='10!U'+ntostr(alog10(f[i]),2)+'!N' 
        if alog10(f[i]) eq 0 then exp='1'
        xyouts,wf[i]*fact,x[2]-b,exp,/data
;     endif ;else a=0.01
     oplot,[w[i],w[i]],[x[0]-a,x[0]+a]
     oplot,[we[i],we[i]],[x[1]-a,x[1]+a]
     oplot,[wf[i],wf[i]],[x[2]-a,x[2]+a]
  endfor 
  xyouts,1e-7,x[0]-c,'Wavelength (m)'
  xyouts,1.5e-7,x[1]-c,'Energy (eV)'
  xyouts,1e-7,x[2]-c,'Frequency (Hz)'

  endplot
  spawn,'convert spectrum.eps spectrum.pdf'

stop

  return
end 

pro check_double_count

  coi_plot,names,lastnames,firstnames,acycle

  ulast=lastnames[rem_dup(lastnames)]

  for i=0,n_elements(ulast)-1 do begin
     w=where(strtrim(lastnames,2) eq strtrim(ulast[i],2) or strtrim(firstnames,2) eq strtrim(ulast[i],2),nw)
     if nw gt 1 then colprint,replicate(ntostr(i),nw),names[w],lastnames[w],firstnames[w],acycle[w]
     
  endfor 

  stop

  return
end 

pro coi_plot,names,lastnames,firstnames,acycle,both=both
  
  if keyword_set(both) then add='_acc' else add=''
  begplot,name='~/Fermi/Senior_Review/general/cumulative_gis'+add+'.eps',/encap,/color,/land,font='helvetica'
  cycle=indgen(5)+2
  
  prop=0L & lastname='' & dup='' & firstname='' & acycle=0
;  for i=0,4 do begin 
;     file='~/Fermi/Senior_Review/Fermi_AO-2-6_PIs_and_CoIs.csv'
;     print,file
;     readcol,file,prop,lastname,firstname,format='(l,a,a)',skip=1,delim=',',/silent
;;; remove duplicate column in future additions
;     prop=[prop,prop0]
;     lastname=[lastname,lastname0]
;     firstname=[firstname,firstname0]
;     acycle=[acycle,replicate(cycle[i],n_elements(lastname0))]
;  endfor 
  readcol,'~/Fermi/Senior_Review/general/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

  prop=prop[1:*]
  lastname=last[1:*]
  firstname=first[1:*]
  approv=approv[1:*]
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
;  s=rem_dup(name)
;  name=name[s]
;  prop=prop[s]

  n=lonarr(5) & nacc=n
  for i=2,6 do begin
     w=where(props ge 10000L*i and props lt 10000L*(i+1),nw)
     ws=where(accprop ge 10000L*i and accprop lt 10000L*(i+1),nwacc)
     n[i-2]=nw
     nacc[i-2]=nwacc
     print,10000L*i,10000L*(i+1),nw,nwacc
  endfor 
  cc=cumulative(n)
  cc5=cumulative(nacc)

  plot,[2008.5,2012.5],[0,1300],yrange=[0,1400],/ysty,/nodata,xtitle='Year',ytitle='Cumulative Number of Guest Investigators',/xsty,xrange=[2008.5,2013.5],xtickv=[2009,2010,2011,2012,2013];,xtickv=[2008,2009,2010,2011,2012],xticks=4
;  oplot,cycle+2007,cc,psym=10,color=!blue
  axis,xaxis=1,xtickname=ntostr([2,3,4,5,6]),xtitle='Cycle',/data,xrange=[2008.5,2013.5],/xsty
  x=2008+[0.8,0.8,1.2,1.2,1.8,1.8,2.2,2.2,2.8,2.8,3.2,3.2,3.8,3.8,4.2,4.2,4.8,4.8,5.2,5.2,0.8]
  y=[0,cc[0],cc[0],0,0,cc[1],cc[1],0,0,cc[2],cc[2],0,0,cc[3],cc[3],0,0,cc[4],cc[4],0]
  polyfill,x,y,color=!blue
  if keyword_set(both) then begin 
     x=2008+[0.8,0.8,1.2,1.2,1.8,1.8,2.2,2.2,2.8,2.8,3.2,3.2,3.8,3.8,4.2,4.2,4.8,4.8,5.2,5.2]+0.1 ;,3.8,3.8,4.2,4.2,0.8]+0.1
;     y=[0,cc5[0],cc5[0],0,0,cc5[1],cc5[1],0,0,cc5[2],cc5[2],0,0,675,675,0,0,0,0,0,0];,cc5[3],cc5[3],0,0,0]
     y=[0,cc5[0],cc5[0],0,0,cc5[1],cc5[1],0,0,cc5[2],cc5[2],0,0,cc5[3],cc5[3],0,0,cc5[4],cc5[4],0]

     polyfill,x,y,color=!red
     legend,['Submitted','Accepted'],textcolor=[!blue,!red],/top,/left,box=0
  endif 
  axis,xaxis=0,xrange=[2008.5,2013.5],xtickv=[2009,2010,2011,2012,2013],/xsty,xtickformat='(A1)'
  endplot
  spawn,'convert ~/Fermi/Senior_Review/general/cumulative_gis'+add+'.eps ~/Fermi/Senior_Review/general/cumulative_gis'+add+'.pdf'

;  axis,xaxis=0,xtickformat='(A1)',xticks=4,xtickv=[2008,2009,2010,2011,2012]  

stop
  return
end


pro gi_crosscheck

  ;;; ADS custom format %M%z0
  ;;; query dates to get to output (all subjects, all categories, time
  ;;; split)
;01-2001   12-2007
;01-2008   07-2009
;08-2009   04-2010
;05-2010   11-2010
;12-2010   05-2011
  readcol,'~/Fermi/Senior_Review/GI_coIs.csv',prop,name,format='(l,a)',skip=1,delim=',',/silent
  names=name[rem_dup(name)]
  names=names[sort(names)]

;  file='~/Fermi/Senior_Review/Fermi_papers.txt'
;  file='~/Fermi/Senior_Review/Fermi_papers_analysis.txt'
  file='~/Fermi/Senior_Review/Fermi_papers_interprets.txt'
  readcol,file,line,format='(a)',delim='$',/silent
  count=0
  print,names
  nl=n_elements(line)*1.
  for i=0,nl-1 do begin 
     
     tmp=strsplit(line[i],',',/ex)
     n=n_elements(tmp)
     auth=tmp
     if n gt 1 then begin 
        tmp2=strsplit(tmp[n-1],'and',/ex,/regex)
        auth=[tmp[0:n-2],tmp2] 
     endif 
     if n eq 1 then auth=strsplit(tmp,'and',/ex,/regex)
     auth=strupcase(auth)
     n=n_elements(auth)
     match,strtrim(auth,2),strtrim(names,2),m1,m2
     if m1[0] eq -1 then begin 
        count=count+1
     endif 
;     print,auth
;     print,m1[0]
;stop
  endfor 
  print,count, nl, count/nl
stop
  
return
end 

pro fsr_helpdesk

  readcol,'helpdesk_queries.csv',week,queries
  ;; weeks since Feb 1, 2009
  year=2009.+(31.+week*7.)/365.
;  year=[2009.0,year,2012]
;  queries=[0,queries,0]
  y=0
  for i=0,n_elements(year)-1 do if queries[i] gt 0 then y=[y,replicate(year[i],queries[i])]
  y=y[1:*]

  plot,[2009,2012],[0,80],xtickv=[2009,2010,2011,2012],xticks=3,xminor=11,xtitle='Year',ytitle='Helpdesk Queries',/nodata
  plothist,y,color=!blue,/over,bin=0.038,/fill,fcolor=!blue
  axis,xaxis=0,xtickformat='(A1)',xticks=3,xtickv=[2009,2010,2011,2012]  
;  oplot,year,queries,psym=10

stop
return
end 
pro fsr_queries

  readcol,'daily_queries.txt',date,q,format='(a,i)'

  year2=strmid(date,0,4)
  mn=strmid(date,5,2)
  day=strmid(date,8,2)
  year2=year2+day/30./12.+(mn-1.)/12.

;;   sm=7
;;   qu=intarr(fix(n_elements(q)/sm)+1)
;;   y=fltarr(fix(n_elements(q)/sm)+1)
;;   j=0
;;   for i=0,n_elements(q)-1,sm do begin
;;      if i lt n_elements(q)-sm then begin 
;;         qu[j]=total(q[i:i+sm])
;;         y[j]=mean(year[i:i+sm])
;;      endif else begin 
;;         qu[j]=total(q[i:*])
;;         y[j]=mean(year[i:*])
;;      endelse 
;;      j=j+1
;;   endfor 

  y2=0
  for i=0,n_elements(year2)-1 do if q[i] gt 0 then y2=[y2,replicate(year2[i],q[i])]
  y2=y2[1:*]

  readcol,'helpdesk_queries.csv',week,queries
  ;; weeks since Feb 1, 2009
  year=2009.+(31.+week*7.)/365.
  y=0
  for i=0,n_elements(year)-1 do if queries[i] gt 0 then y=[y,replicate(year[i],queries[i])]
  y=y[1:*]


 
  begplot,name='data_queries.eps',/land,/color,/encap,font='helvetica'
  !y.margin=[0,0]
  !x.margin=[5,0]
  multiplot2,[1,2],/init
  multiplot2
  ytitle='Number of Queries per Week'
  plot,[0,0],[0,0],/nodata,xrange=[2009,2012],/xsty,psym=10,charsize=2,yrange=[0,1000],xminor=12,xticks=3

;  polyfill,[min(year),y,max(year),min(year),min(year)],[0,qu,0,0,y[0]],/fill,color=!blue
  plothist,y2,color=!black,/over,bin=7./365.,/fill,fcolor=!blue
;  legend,'Data Queries',textcolor=!blue,box=0,/top,/left,charsize=2
  xyouts,2009.1,850,'  Data',color=!blue,charsize=2
  xyouts,2009.1,750,'Queries',color=!blue,charsize=2
  
  axis,xaxis=0,xtickformat='(A1)',xminor=12,xticks=3;,xticks=2,xtickv=[2010,2011,2012]
  xyouts,2009.6,50,'Release of photon data',orient=90,charsize=1.6

  multiplot2,yupgap=0.2
  plot,[0,0],[0,0],/nodata,xrange=[2009,2012],/xsty,psym=10,xtitle='Year',charsize=2,yrange=[0,50],yminor=2,xminor=12,xticks=3
  plothist,y,x,y1,color=!black,/over,bin=7./365.,/fill,fcolor=!red
;  oplot,x,y1*10.,color=!red,psym=10
  xyouts,2008.7,10,ytitle,orient=90,charsize=2
;  legend,'Helpdesk Queries',textcolor=!red,box=0,/top,/left,charsize=2
  xyouts,2009.1,40,'Helpdesk',color=!red,charsize=2
  xyouts,2009.1,32,' Queries',color=!red,charsize=2

  oplot,[2011.+(7./12.),2011.+(7./12.)],[0,50],line=2
  xyouts,2011.6,44,'Pass 7',charsize=1.6
  oplot,[2011.5,2011.5],[0,40],line=2
  xyouts,2011.3,44,'2FGL',charsize=1.6
;  oplot,[2011.,2011.]+(4./12),[0,50],line=2
;  xyouts,2011.1,40,'  Fermi',charsize=1.6
;  xyouts,2011.1,35,'Symposium',charsize=1.6

  oplot,[2010.,2010.]+(8./12),[0,44],line=2
  xyouts,2010.5,44,'software release',charsize=1.6

  oplot,[2010.,2010.]+0.5/12.,[0,40],line=2
  xyouts,2009.9,43,'1FGL',charsize=1.6

  oplot,[2009.13,2009.13],[0,32],line=2
  xyouts,2009.19,22,'0FGL',charsize=1.6

;  oplot,[2009.,2009.]+(10./12),[0,40],line=2
;  xyouts,2009.7,45,'  Fermi',charsize=1.6
;  xyouts,2009.7,40,'Symposium',charsize=1.6


;  xyouts,2010.68,40,'release',charsize=1.6
  axis,xaxis=0,xtickformat='(A1)',xminor=12,xticks=3
  multiplot2,/reset,/default
  endplot
  spawn,'convert data_queries.eps data_queries.pdf'

stop
return
end 

pro fsr_pie

  cd,'~/Fermi/Senior_Review'
  begplot,name='fsr_pie_gi.eps',/land,/encap,/color,font='helvetica'
;  !p.multi=[0,1,2]
;  aplot,1,[0,1],[0,1],/nodata
  perc=[33,5,19,19,12,12]
  color=[!blue,!red,!green,!purple,!cyan,!orange]
  pie_chart,perc,color=color,/outline,rad=8000
  pienames=['AGN','EBL, Clusters','GRBs','Pulsars','Galactic: SNRs, Diffuse, XRBs','Other: Methods, Surveys, Dark Matter']
  xc=!d.x_size/2. & yc=!d.y_size/2.
  print,xc,yc
  xyouts,xc+2000,yc-6000,ntostr(perc[0])+'%',charsize=2,color=!white
  xyouts,xc-4700,yc-5500,ntostr(perc[1])+'%',charsize=2,color=!white
  xyouts,xc-7000,yc-600,ntostr(perc[2])+'%',charsize=2
  xyouts,xc-3000,yc+4500,ntostr(perc[3])+'%',charsize=2,color=!white
  xyouts,xc+2000,yc+5000,ntostr(perc[4])+'%',charsize=2
  xyouts,xc+5000,yc+2000,ntostr(perc[5])+'%',charsize=2

  xyouts,xc+5000,yc-8000,pienames[0],charsize=2
  xyouts,xc-8000,yc-6000,pienames[1],charsize=2
  xyouts,xc-12000,yc-1000,pienames[2],charsize=2
  xyouts,xc-8000,yc+6000,pienames[3],charsize=2
  xyouts,xc+5000,yc+6000,pienames[4],charsize=2
  xyouts,xc+7000,yc+3000,pienames[5],charsize=2


;  ,title='GI Program Topics'

;  aplot,1,[0,1],[0,1],/nodata
  endplot
  begplot,name='fsr_pie_papers.eps',/land,/encap,/color,font='helvetica'
  pie_chart,[27,3,12,15,24,19],color=[!blue,!red,!green,!purple,!cyan,!orange],/outline,title='Paper Topics'
  pienames=['Extragalactic:AGN','Extragalactic: EBL, Clusters','GRBs','Pulsars','Galactic:SNRs, Diffuse, XRBs','Other: Methods, Surveys, Dark Matter']
  !p.multi=0
  endplot
  spawn,'convert fsr_pie_gi.eps fsr_pie_gi.pdf'
  spawn,'convert fsr_pie_papers.eps fsr_pie_papers.pdf'

  return
end 

pro fsr_techplots
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data latency
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  readcol,'~/Fermi/Senior_Review/GBM_data_latency.csv',trig,time,format='(a,a)'
  gbm=fltarr(n_elements(time))
  for i=1,n_elements(time)-1 do begin 
     c=strsplit(time[i],':',/extra)
     day=c[0]
     hour=c[1]
     minute=c[2]
     sec=c[3]
     gbm[i]=hour+minute/60.+sec/3600.+day*24.
  endfor 
  w=where(gbm lt 100 and gbm gt 0)
  gbm=gbm[w]+1.

  readcol,'~/Fermi/Senior_Review/LAT_data_latency.csv',slac,nasa,runstart,format='(a,a)'
  lat=double(slac)+double(nasa)+1.

  begplot,name='~/Fermi/Senior_Review/data_latency.eps',/land,/color,/encap,font='helvetica'
  !x.margin=[4,0]
  !y.margin=[4,0]
  yrange=[0,100]
  plot,[0,2],[0,300],/nodata,xtitle='Data Processing Latency (hours)',ytitle='Arbitrary Units',xtickformat='(A1)',charsize=2,xminor=1,xticks=1,xrange=[0,2],/xsty,yrange=yrange,/ysty
  plothist,alog10(gbm),x1,y1,/over,color=!blue,/fill,fcolor=!blue,xrange=[0,2],bin=0.02,yrange=yrange,peak=95
  plothist,alog10(lat),x0,y0,/fill,xrange=[0,2],bin=0.01,/over,xminor=1,yrange=yrange,peak=95
  n0=n_elements(x0)
;  polyfill,[x0,x0[n0-1],x0[0]],[y0/10.,0,0]
  oplot,x0,y0,psym=10
  oplot,x1,y1,psym=10,color=!blue

  axis,xaxis=0,xticks=3,xtickv=[1,10,100],charsize=2,xrange=[1,100],/xlog,/xsty;,xminor=9
  axis,xaxis=1,xticks=3,xtickv=[1,10,100],charsize=2,xrange=[1,100],/xlog,/xsty,xtickformat='(A1)'
  for j=0,1 do begin 
     for i=0,9 do begin 
        x=(i+j)*10^j
        oplot,alog10([x,x]),[0,1]
        oplot,alog10([x,x]),[yrange[1]-1,yrange[1]]
     endfor 
  endfor 

  legend,'LAT data runs',/top,/right,textcolor=!p.color,box=0,charsize=2
  legend,'GBM triggers',/top,/left,textcolor=!blue,box=0,charsize=2
  endplot
  spawn,'convert ~/Fermi/Senior_Review/data_latency.eps ~/Fermi/Senior_Review/data_latency.pdf'
  
stop
  return
end 
