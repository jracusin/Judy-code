function readline,lun,delim=delim,skip=skip
  
  if n_params() eq 0 then begin 
     print,'syntax - chunks=read_line(lun,[delim=delim])'
     return,0
  end
  line=''
  if n_elements(skip) eq 0 then skip=0
  for i=0,skip do readf,lun,line
  if n_elements(delim) eq 0 then begin 
     delim=' '
     chunks=str_sep(line,delim)
     w=where(chunks ne '',nw)
     if nw gt 0 then chunks=chunks[w]
  endif else chunks=str_sep(line,delim)
  
  return,chunks
end 
