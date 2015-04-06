pro find_ppst_gaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SALSA - Scheduling Anomaly Locator Search Algorithm
;;      salsa finds gaps in PPST's and tells you where they are at
;;           and outputs a file with the ppstfilename.salsa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ppstfile=pickfile(filter='PPST*')
  gapbegin=''
  gapend=''
  
  openr,plun,ppstfile,/get_lun
  while (not EOF(plun)) do begin
     line=''
     readf,plun,line
     data=strsplit(line,' | ',/extract)
     if ((data[1] eq 'PPT') and (data[2] eq 'End') and (not EOF(plun))) then begin
        starttime=data[0]
        readf,plun,line
        data=strsplit(line,' | ',/extract)
        if (data[1] eq 'PPT') and (data[2] eq 'Begin') then begin
           endtime=data[0]
        
           if (endtime ne starttime) then begin
              gapbegin=[gapbegin,starttime]
              gapend=[gapend,endtime]
           endif 
        endif 
     endif 
  endwhile
  
  if n_elements(gapbegin) gt 1 then begin
     gapbegin=gapbegin[1:*]
     gapend=gapend[1:*]
     ngaps=n_elements(gapbegin)
  endif else ngaps=0
  
  openw,slun,ppstfile+'.salsa'
  printf,ppstfile
  printf,'*************************************************'
  printf,'          There are '+ntostr(ngaps)+' gaps in this PPST'
  if ngaps gt 0 then begin 
     printf,'       Gap begins at:           Gap ends at:'
     for i=0,ngaps-1 do printf,'     '+gapbegin[i]+'       '+gapend[i]
  endif 
  printf,'*************************************************'
  close,/all
  
  print,ppstfile
  print,'*************************************************'
  print,'          There are '+ntostr(ngaps)+' gaps in this PPST'
  if ngaps gt 0 then begin 
     print,'       Gap begins at:           Gap ends at:'
     for i=0,ngaps-1 do print,'     '+gapbegin[i]+'       '+gapend[i]
  endif 
  print,'*************************************************'
        
  
  return
end 
