pro process_comandstats_data,filein
  
  get_lun,clun
  openr,clun,filein+'.comandstats.txt'
  line=''
  readf,clun,line
  time=0D
  scui=0D
  while not EOF(clun) do begin
     readf,clun,line
     tmp=strsplit(line,/ex)
     time=[time,tmp[0]]
     scui=[scui,tmp[1]]
  endwhile
  
  time=time[1:*]
  scui=scui[1:*]
  
  set_plot,'ps'
  device,filename=filein+'.comandstats.ps',/landscape
  
  plot,time-min(time),scui/(1024.^2),$
     xtitle='SCTIME - '+ntostr(min(time))+'(s)',$
     ytitle='SCUI Low Priority Buffer Depth (Mb)'
  
  device,/close
  close,clun
  free_lun,clun

  return
end 
