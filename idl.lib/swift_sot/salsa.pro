pro salsa,ppstfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SALSA - Scheduling Anomaly Locator Search Algorithm
;;      salsa finds gaps in PPST's and tells you where they are,
;;           and outputs a file called ppstfilename.salsa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  read_ppst,ppstfile,ppst
;  if n_elements(ppstfile) eq 0 then ppstfile=pickfile(filter='PPST*')
  gapbegin=''
  gapend=''
  
;  openr,plun,ppstfile,/get_lun
;  while (not EOF(plun)) do begin
;     line=''
;     readf,plun,line
;     data=strsplit(line,' | ',/extract)
;     if ((data[1] eq 'PPT') and (data[2] eq 'End') and (not EOF(plun))) then begin
;        starttime=data[0]
;        readf,plun,line
;        data=strsplit(line,' | ',/extract)
;        if (data[1] eq 'PPT') and (data[2] eq 'Begin') then begin
;           endtime=data[0]
  
  for i=1,n_elements(ppst)-1 do begin 
     endtime=ppst[i].begdate
     starttime=ppst[i-1].enddate
     if (endtime ne starttime) then begin
        gapbegin=[gapbegin,starttime]
        gapend=[gapend,endtime]
     endif 
  endfor 
;        endif 
;     endif 
;  endwhile
  
  if n_elements(gapbegin) gt 1 then begin
     gapbegin=gapbegin[1:*]
     gapend=gapend[1:*]
     ngaps=n_elements(gapbegin)
  endif else ngaps=0
  
  openw,slun,ppstfile+'.salsa',/get_lun
  printf,slun,'SALSA output:'
  printf,slun,''
  printf,slun,ppstfile
  printf,slun,'*************************************************'
  if ngaps gt 1 or ngaps eq 0 then $
     printf,slun,'          There are '+ntostr(ngaps)+' gaps in this PPST'
  if ngaps eq 1 then $
     printf,slun,'          There is '+ntostr(ngaps)+' gap in this PPST'
  if ngaps gt 0 then begin 
     printf,slun,'       Gap begins at:           Gap ends at:'
     for i=0,ngaps-1 do printf,slun,'     '+gapbegin[i]+'       '+gapend[i]
  endif 
  printf,slun,'*************************************************'
  close,/all
  
  print,ppstfile
  print,'*************************************************'
  if ngaps gt 1 or ngaps eq 0 then $
     print,'          There are '+ntostr(ngaps)+' gaps in this PPST'
  if ngaps eq 1 then $
     print,'          There is '+ntostr(ngaps)+' gap in this PPST'
  if ngaps gt 0 then begin 
     print,'       Gap begins at:           Gap ends at:'
     for i=0,ngaps-1 do print,'     '+gapbegin[i]+'       '+gapend[i]
  endif 
  print,'Note - if SALSA cannot find gaps, they may be at very beginning or end of PPST'
  print,'*************************************************'
        
  ;running ppst2radec if you want to look at map_viewer outputs
  ppst2radec,ppstfile
  
  
  return
end 
