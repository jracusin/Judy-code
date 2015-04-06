pro find_errors
  
  get_file_begin,fb
  
  err1='1803f001'
  err2='19033800'
  
  errfile=fb+'_errors.txt'
  oerrfile=fb+'_errors.find.txt'
  
  line=''
  line2=''
  tmp=''
  openr,flun,errfile,/get_lun
  openw,olun,oerrfile,/get_lun
  while not EOF(flun) do begin 
     readf,flun,line
     sterr=strsplit(line,/ex)
     if n_elements(sterr) gt 3 then begin 
        if (sterr[2] eq err1) then begin
           readf,flun,tmp
           readf,flun,line2
           sterr=strsplit(line2,/ex)
           if sterr[2] eq err2 then begin
              printf,olun,line
              printf,olun,line2
           endif
        endif 
     endif 
  endwhile 
  
  close,/all
  free_lun,flun
  free_lun,olun
  
  if file_size(oerrfile) eq 0 then spawn,'rm -f '+oerrfile
  
  return
end 
  
