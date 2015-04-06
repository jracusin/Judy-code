pro bias_voltages
  
  times=0L
  simpctable
  dir='/bulk/yankees/xrt/pass_data/'
;  datasets=['pass_20041530234','pass_20041530750','pass_20041531147','pass_20041531435','pass_20041531544','pass_20041550235','pass_20041550351','pass_20041550508','pass_20041550628','pass_20041550848','pass_20041551123']
  datasets=['pass_20041550235','pass_20041550351','pass_20041550508','pass_20041550628','pass_20041550848','pass_20041551123']
  voltages=['v_baseline_a','v_baseline_b','v_od1','v_od2']
  nsets=n_elements(datasets)
  volt1=0. & volt2=0. & volt3=0. & volt4=0. & temp=0. & mtemp=0.
  x=fltarr(nsets,4)
  begplot,name='bias_voltages.ps',/color,/landscape
  for i=0,n_elements(datasets)-1 do begin
     
     file=dir+datasets[i]+'/'+datasets[i]+'_ahk.0.stripchart_table.fits'
     
     schart=mrdfits(file,1)
     
     time=schart.sctime
     q=where(time gt 1e7)
     schart=schart[q]
     time=time[q]
     times=[times,time]
     min_time=min(time)
     units = strtrim(min(time),1)+' + (s)' 
     time=time-min_time
     temp=[temp,schart.SIGNALCHNTEMP_AD590_]
     mtemp=[mtemp,mean(schart.SIGNALCHNTEMP_AD590_)]
     !p.multi=[0,2,2]
     for j=0,3 do begin 
        com='volt=schart.'+voltages[j]
        tmp=execute(com)
;     volt=schart.v_baseline_a
        com='volt'+ntostr(j+1)+'=[volt'+ntostr(j+1)+',volt]'
        tmp=execute(com)
        nvolt=n_elements(schart)
        
        sigma=stddev(volt)
        mean=median(volt)
        x[i,j]=mean
;print,sigma        
        diff=abs(volt-mean)
        w=where(diff gt 3.*sigma,nw)
        ;help,w
     
        plot,time,volt,/ynozero,xtitle=units,ytitle='volts',title=voltages[j],xrange=[min(time),max(time)]
        oplot,[time[0],time[nvolt-1]],[mean,mean],color=!red
        if nw gt 0 then oplot,time[w],volt[w],psym=1,color=!blue
        if j eq 0 then legend,datasets[i],box=0,/top,/left
      endfor 
     !p.multi=0
;     if i ne nsets-1 then k=get_kbrd(1)
  endfor 
  endplot  
  
  begplot,name='combined_bias_voltages.ps',/color  
  times=times[1:*]
  volt1=volt1[1:*]
  volt2=volt2[1:*]
  volt3=volt3[1:*]
  volt4=volt4[1:*]
  temp=temp[1:*]
  mtemp=mtemp[1:*]
  
  tmp=linfit(times,volt1,yfit=fit1)
  tmp=linfit(times,volt2,yfit=fit2)
  tmp=linfit(times,volt3,yfit=fit3)
  tmp=linfit(times,volt4,yfit=fit4)
  
  !p.multi=[0,1,5]
  xrange=[min(times),max(times)]
  plot,times,volt1,xtitle='sctime',ytitle=voltages[0],/ynozero,xrange=xrange,xstyle=1
  oplot,times,fit1,color=!red
  plot,times,volt2,xtitle='sctime',ytitle=voltages[1],/ynozero,xrange=xrange,xstyle=1
  oplot,times,fit2,color=!red
  plot,times,volt3,xtitle='sctime',ytitle=voltages[2],/ynozero,xrange=xrange,xstyle=1
  oplot,times,fit3,color=!red
  plot,times,volt4,xtitle='sctime',ytitle=voltages[3],/ynozero,xrange=xrange,xstyle=1
  oplot,times,fit4,color=!red
  plot,times,temp,xtitle='sctime',ytitle='Signal Chain Temp (C)',/ynozero,xrange=xrange,xstyle=1
  !p.multi=0
  endplot  
  stop
  return
end 
     
     
