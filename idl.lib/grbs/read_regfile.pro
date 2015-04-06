pro read_regfile,file,ra,dec,rad
  
  if n_params() eq 0 then begin
     print,'syntax - read_regfile,file,ra,dec,rad'
     return
  endif 
  
  openr,lun,file,/get_lun
  while not eof(lun) do begin
     line=readline(lun,delim='(')
     if n_elements(line) eq 2 then begin
        chunks=str_sep(line[1],',')
        ra=chunks[0]
        dec=chunks[1]
        tmp=str_sep(chunks[2],'"')
        rad=tmp[0]
     endif 
  endwhile
  
  return
end 
