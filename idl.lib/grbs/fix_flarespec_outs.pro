pro fix_flarespec_outs
  
  file='fit_result_flare.pha.dat.rfmt'
  
  if file_lines(file) eq 169 then begin
     
     openr,lun,file,/get_lun
     
     lines=''
     for i=0,168 do begin
        line=readline(lun,delim='$')
        lines=[lines,line]
     endfor 
     close,lun,/file
     lines=[lines[1:107],lines[117:118],lines[108:116],lines[127:*]]
     openw,lun,file,/get_lun
     for i=0,n_elements(lines)-1 do printf,lun,lines[i]
     close,lun,/file
     
  endif
  
  return
end 
