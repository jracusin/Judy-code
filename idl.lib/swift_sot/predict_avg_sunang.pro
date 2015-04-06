pro predict_avg_sunang,year,day,avgday,avgsun
  
  if n_elements(day) ne 0 then begin 
     file='PPST_'+ntostr(year)+ntostr(day)+'*_'+ntostr(year)+'*.txt'
     file=findfile(file)
     oneday=1
     title='Day '+ntostr(day)
;     if file eq '' then begin
;        file='PPST_'+ntostr(year)+'*_'+ntostr(year)+ntostr(day+1)+'*.txt'
;        file=findfile(file)
;     endif 
  endif else oneday=0

  
  plot_ppst,file,begday=begday,angsun=sunang,/noplot,/sun,begtime=begtime,endday=endday
  pos=strpos(file,'PPST')
  year=strmid(file,pos+5,4)*1
  pfile=strmid(file,pos,50)
  
  day=strmid(file,pos+9,3)
  day2=strmid(file,pos+21,3)
  if (day2*1 ne day+1) and oneday then begin
     w=where(begday ge 0 and begday le 1)
     begday=begday[w]
     sunang=sunang[w]
     begtime=begtime[w]
     endday=endday[w]
  endif
     
  if (day2*1 eq day+1) and not oneday then title='Day '+day
  
  if (day2*1 ne day+1) and not oneday then title='Day '+day+' to '+ntostr(day2*1-1)
  
  print,pfile
;  plot_ccdt,year,day,time=tim,temp=temp,/noplot
;  stt=date2met(ntostr(year)+'-'+day+'-0:00:00.000')
;  edt=date2met(ntostr(year)+'-'+ntostr(day*1+1)+'-0:00:00.000')
;  wg=where(begtime gt stt and begtime lt edt)
;  begtime=begtime[wg]
;  sunang=sunang[wg]
  
  t1=[begday,endday-0.001]
  time=[begday,endday]*24.
  s=sort(t1)
  time=time[s]
  sa=[sunang,sunang]
  sa=sa[s]
  
  begday=time/24.
  sunang=sa
  
  tpos=strpos(pfile,'.txt')
  ofile=strmid(pfile,0,tpos)+'_avgsun.ps'
  begplot,name=ofile,/color,/land
  
  tmp=histogram(begday,reverse_indices=r,binsize=96./60./24.)
  simpctable
 
  plot,begday*24.,sunang,xtitle='Hours',title=title,/ynozero,ytitle='Sun Angle',xstyle=1,xrange=[min(time),max(time)]
  legend,['Orbit Averaged Sun Angle'],color=!red,box=0,psym=2
  time=dblarr(r[0]-1)
  sun=dblarr(r[0]-1)
  for i=0,r[0]-2 do begin
     IF R[i] ne R[i+1] and (r[r[i]] lt r[r[i+1]-1]) THEN begin
        k=r[r[i]:r[i+1]-1]
        if n_elements(k) gt 1 then begin
           bwe=endday[k]-begday[k]
           time[i]=total(begday[k]*bwe)/total(bwe)*24.
           sun[i]=total(sunang[k]*bwe)/total(bwe)
        endif else begin
           time[i]=begday[k]*24.
           sun[i]=sunang[k]
        endelse 

     endif 
     
  endfor 
  oplot,time,sun,color=!red,psym=2
;  oplot,time,sun,color=!red
  
  endplot
  
  return
end 
