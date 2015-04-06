pro update_flare_flags,flfile
  
;  if n_elements(dir) eq 0 then dir='.' else cd,dir
  if n_elements(flfile) eq 0 then flfile='flare_flags.txt'
  
  if numlines(flfile) eq 12 then begin
     print,flfile+' does not need to be updated'
     return
  endif 
  if exist(flfile) then spawn,'cp '+flfile+' '+flfile+'.old'
  
  openr,lun,flfile,/get_lun
  col=''
  while not eof(lun) do begin
     line=readline(lun,delim='|')
     if line[0] ne ' ' then col=[col,line[0]+'    |'+line[1]]
  endwhile 
  col=col[1:*]
  close,lun,/file
  free_lun,lun
  
  openw,lun,flfile,/get_lun
  
  yesline='If yes, where needed (rise,decay,both)?    | none'
  printf,lun,col[0]
  printf,lun,yesline
  printf,lun,'Need fluence corr for overlapping flares?  | no'
  printf,lun,yesline
  printf,lun,col[3]
  printf,lun,yesline
  for i=4,9 do printf,lun,col[i]
  
  close,lun,/file
  free_lun,lun
  
  return
end 
