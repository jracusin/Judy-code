pro data_processing_stats

  cd,'~/Fermi/'

  begplot,name='LAT_data_latency.ps',/land,/color
  readcol,'lat_data_processing_stats.dat',run,slac,nasa
  tot=slac+nasa
  
  nrun=n_elements(run)
  year=fltarr(nrun)
  for i=0,nrun-1 do begin 
     date=met2date(run[i],/fermi)
     chunks=strsplit(date,'-:',/ex)
     year[i]=chunks[0]+chunks[1]/365.+(chunks[2]+chunks[3]/60.+chunks[4]/3600.)/24./365.
  endfor 
  
  del=0.25
  num=round((max(year)-min(year))/del+0.5)
  t=fltarr(num)
  l90=fltarr(3,num)
  for i=0.,num*del-del,del do begin
     w=where(year ge min(year)+i and year le min(year)+i+0.1,nw)
     s=sort(tot[w])
     n05=round(nw*0.05)
     n50=round(nw*0.5)
     n95=round(nw*0.95)
     
     t[i/del]=min(year)+i
     l90[0,i/del]=tot[w[s[n05]]]
     l90[1,i/del]=tot[w[s[n50]]]
     l90[2,i/del]=tot[w[s[n95]]]
  endfor 

  plotsym,0,1,/fill
  plot,year,tot,psym=3,yrange=[1,40],xtitle='Year',ytitle='Hours',title='LAT Data Latency',charsize=2
  oplot,t,l90[1,*],color=!red,thick=10
  oplot,t,l90[0,*],color=!salmon
  oplot,t,l90[2,*],color=!salmon
  xyouts,2016.8,4,'Median',color=!red
  xyouts,2016.8,8,'95%',color=!salmon
  xyouts,2016.8,2.5,'5%',color=!salmon
;  oploterror2,t,l90[1,*],fltarr(2,num),[l90[0,*],l90[2,*]],color=!red,thick=10
;  polyfill,[t,reverse(t)],[l90[0,*],reverse(l90[2,*])],/fill,color=!green
;  legend,['90%'],/top,/right,textcolor=!red,box=0

  endplot
  ps2pdf,'lat_data_latency.ps'

;  plot,[0,50],[0,12000],/nodata
;  plothist,slac,bin=0.1,/overplot,color=!red
;  plothist,nasa,bin=0.1,/overplot,color=!blue
;  plothist,slac+nasa,bin=0.1,/overplot,color=!green
  
  

  stop

  return
end
