pro plot_ppst_ccdt,day
  
  
  plot_ppst,file,/sun,/noplot,angsun=sunang,begtime=begtime,endtime=endtime,$
     begday=begday,endday=endday,targroll=targroll
    
  print,file
  pos=strpos(file,'PPST')
  year=strmid(file,pos+5,4)*1

  day1=strmid(file,pos+9,3)*1

  if n_elements(day) ne 0 then begin
     d=day-day1
     wd=where(begday ge d-0.01)
     begday=begday[wd]-d
     endday=endday[wd]-d
     targroll=targroll[wd]
     sunang=sunang[wd]
  endif else day=strmid(file,pos+9,3)*1
  plot_ccdt,year,day,ahk=ahk,/noplot
  
  erase
  multiplot,[1,3],/init 
  
  scdate=met2date(ahk.sctime)
  scday=fix(strmid(scdate,5,3))+fix(strmid(scdate,9,2))/24.+fix(strmid(scdate,12,2))/24./60.+double(strmid(scdate,15,15))/3600./24.
  
  scday=(scday-day*1D)*24.
  starttime=min(scday);ahk.sctime)
  stoptime=max(scday);ahk.sctime)
  xrange=[0,24];[starttime,stoptime]
  
  ;;plotting ccdt
  multiplot

  plot,scday,ahk.ccdtemp1_rtd_,xrange=xrange,ytitle='CCD Temp',xstyle=1,/ynozero,title='Day '+ntostr(day)
  
  ;;plotting sunang
  t1=[begday,endday-0.001]
  time=[begday,endday]*24.
  s=sort(t1)
  time=time[s]
  sa=[sunang,sunang]
  sa=sa[s]
  multiplot
  plot,time,sa,xrange=xrange,ytitle='Sun Angle',xstyle=1,/ynozero,yrange=[50,180],ystyle=1
  
  ;;plotting rollang
  roll=[targroll,targroll]
  roll=roll[s]
  multiplot
  plot,time,roll,xrange=xrange,ytitle='Roll Angle',xtitle='hour',xstyle=1,/ynozero,yrange=[0,360],ystyle=1
  
  
  multiplot,/reset
  
  return
end 
