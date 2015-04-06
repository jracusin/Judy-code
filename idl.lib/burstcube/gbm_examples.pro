pro plot_lcs

  cd,'~/proposals/BurstCube/GBMgrbs'

  readcol,'GBM_short_bursts.dat',name,fluence,t90,whichdet,whichdetang,format='(a,f,f,a,f)',skip=1
  tte=file_search('glg_tte_'+whichdet+'_bn'+strmid(name,3,9)+'_v0*.fit')
  n=n_elements(name)

  begplot,name='short_burst_lcs.ps',/land,font='helvetica'
  !p.multi=[0,6,5]
  for i=0,n-1 do begin
     g=mrdfits(tte[i],2,hdr)
     w=where(g.time gt -10 and g.time lt 10)
     mwrfits,g[w],'glg_tte_'+whichdet[i]+'_bn'+strmid(name[i],3,9)+'_10s.fit',/create,hdr
     plothist,g[w].time,bin=1e-1,xtitle='Time since Trigger (s)',ytitle='N/bin',title=name[i],xrange=[-10,10]
;     k=get_kbrd(10)
;     if k eq 's' then stop
  endfor 
  !p.multi=0
  endplot

stop

  return
end 

pro gbm_examples

  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  s=where(gbm.t90 le 2. and gbm.fluence gt 0,n)
  plotloghist,gbm[s].fluence,bin=0.1,xrange=[1e-8,1e-4],yrange=[0,25]

;  val=[4e-8,1e-7,3e-7,1e-6,1e-5]
;  n=n_elements(val)
  whichdet=strarr(n)
  whichdetang=fltarr(n)

  for i=0,n-1 do begin 
;     a=approx(gbm[s].fluence,val[i],w)
;     w=s[w]
     w=s[i]
 
;     print,gbm[w].name
;     print,'T90 = ',gbm[w].t90
     
     daycnv,gbm[w].trigger_time+2400000.5,yr,mn,day,hr
     mnt=(hr-fix(hr))*60.
     sec=(mnt-fix(mnt))*60.
     hr=fix(hr)
     mnt=fix(mnt)
     met=date2met(ntostr(yr)+'-'+ntostr(mn)+'-'+ntostr(day)+'-'+ntostr(hr)+':'+ntostr(mnt)+':'+ntostr(sec,4),/fermi)
     
     gbm_angles,gbm[w].ra,gbm[w].dec,met,det
     mindet=min(det[0:11].angles,wm)
     whichdet[i]=det[wm].det
     whichdetang[i]=det[wm].angles
 ;    print,det[wm].det,det[wm].angles
  endfor 

  w=where(whichdetang lt 10. and whichdetang ne 0,nw)
  plotloghist,gbm[s[w]].fluence,bin=0.1,/overplot,color=!green,xrange=[1e-8,1e-4],yrange=[0,25]

  f=sort(gbm[s[w]].fluence)
  q=s[w[f]]

  yr='20'+strmid(gbm[q].name,3,2)
  url='http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/'+yr+'/bn'+strmid(gbm[q].name,3,9)+'/current/glg_tte_'+whichdet[w[f]]+'_bn'+strmid(gbm[q].name,3,9)+'_v00.fit'

  cd,'~/proposals/BurstCube/GBMgrbs'

  header='GRB             fluence    T90(s)  NearDet NearDetAng'
  writecol,'GBM_short_bursts.dat',gbm[q].name,numdec(gbm[q].fluence,3,/sci),numdec(gbm[q].t90,3),whichdet[w[f]],numdec(whichdetang[w[f]],1),delim='    ',header=header

  
  for i=0,nw-1 do begin
;     spawn,'wget '+url[i]
     if not exist('glg_tte_'+whichdet[w[f[i]]]+'_bn'+strmid(gbm[q[i]].name,3,9)+'_v00.fit') then begin
        url[i]='http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/'+yr[i]+'/bn'+strmid(gbm[q[i]].name,3,9)+'/current/glg_tte_'+whichdet[w[f[i]]]+'_bn'+strmid(gbm[q[i]].name,3,9)+'_v01.fit'
        spawn,'wget '+url[i]
     endif 
  endfor 

  stop
  return
end 
