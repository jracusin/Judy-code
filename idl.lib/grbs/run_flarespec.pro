pro run_flarespec
  
;  bdir='/bulk/shadow/racusin/grbs/'
  bdir='/bulk/axiom/morris/GRB_flares_output2/'
  cd,bdir
  spawn,'ls -d GRB*/fl* > dir.txt'
  readcol,'dir.txt',dir,format='(a)'
  
  n=n_elements(dir)
  for i=0,n-1 do begin
     cd,bdir+dir[i]
     spawn,'pwd'
     parfile=file_search('flare*.par')
     if parfile[0] ne '' and exist(parfile[0]) then $
        flarespec,parfile[0]
  endfor 
  
  return
end 
