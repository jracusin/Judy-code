pro read_positionmessage, time, targetid, segno
  
  get_file_begin,filebegin
  file=filebegin+'_positionmessage.txt'
  
  openr,lun,file,/get_lun
  line=''
  readf,lun,line
  while (not EOF(lun)) do begin 
     
     if strpos(line,'Message secs') ne -1 then begin 
        chunks=str_sep(line,' ')
        w=where(chunks ne '')
        chunks=chunks[w]
        time=chunks[6] 
     endif 
     if strpos(line,'Target ID') ne -1 then begin 
        chunks=str_sep(line,' ')
        w=where(chunks ne '')
        chunks=chunks[w]
        targetid=chunks[6] 
     endif 
     if strpos(line,'Obs. Number') ne -1 then begin 
        chunks=str_sep(line,' ')
        w=where(chunks ne '')
        chunks=chunks[w]
        segno=chunks[6] 
     endif 
     
     readf,lun,line
     
  endwhile
     
  close,lun
  free_lun,lun
  
  return
end 
