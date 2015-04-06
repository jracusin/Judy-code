function run_pass1_again,dosci=dosci
  
  sci=file_search('pass_*.sci')
  nsci=n_elements(sci)
  
  if nsci eq 0 then return,0
  if nsci gt 1 then n=nsci-2 else n=nsci-1
  lpos=strpos(sci[n],'.sci')
  ldps=strmid(sci[n],0,lpos)
  files=file_search(ldps+'*')
  nfiles=n_elements(files)
  
  if keyword_set(dosci) then begin 
     for i=0,nsci-1 do begin 
        lpos=strpos(sci[i],'.sci')
        ldps=strmid(sci[i],0,lpos)
        files=file_search(ldps+'*')
        nf=n_elements(files)
        if nf eq 1 then return,ldps
     endfor 
     if nfiles eq 1 then return,'1' else return,'0'
  endif 
  
  if nfiles eq 1 then return,1 else return,0
  
end 
  
