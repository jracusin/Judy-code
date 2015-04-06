pro tmp_grbs
  
  files=file_search('*xrt/sw*xpc*cl.evt')
  
  f0=mrdfits(files[0],1)
  for i=1,n_elements(files)-1 do begin
     f=mrdfits(files[i],1)
     concat_structs,f0,f,evt
     f0=evt
  endfor 


stop


return
end 
