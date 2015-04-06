pro rerun_scanerr_pars1
  
  spawn,'ls > filedirs'
  readcol,'filedirs',filedirs,format='(a)',/silent
  wdir=strmatch(filedirs,'xrt_200*')
  bad=strmatch(filedirs,'*.tar.gz')
  wbad=where(bad eq 1,nbad)
  if nbad gt 0 then wdir[wbad]=0
  wdir=where(wdir eq 1,ndir)
  filedirs=filedirs[wdir]
     
  for i=0,ndir-1 do begin
     
     cd,filedirs[i]
     get_file_begin,fb
     if (file_size(fb+'_errors.0') gt 0) then begin
        spawn,'scanerr -d '+fb+'_errors.0 > '+fb+'_errors.txt'
        find_errors
     endif 
     if (file_size(fb+'_comandstats.0') gt 0) then spawn,'pars1 '+fb+'_comandstats.0 >'+fb+'_comandstats.txt'
     cd,'..'
     
  endfor 
  spawn,'rm -f filedirs'
  return
end 
