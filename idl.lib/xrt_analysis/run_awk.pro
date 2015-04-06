pro run_awk
  
  get_file_begin,filebegin
  pfile=filebegin+'_science.0'
  tfile=pfile+'.timeline'
  
  openw,alun,'awk.txt',/get_lun
  printf,alun,'/New LDP/{print}'
  printf,alun,'/Product/{print}'
  printf,alun,'/IM/{print}'
  printf,alun,'/RAW/{print}'
  close,alun
  spawn,'awk -f awk.txt '+tfile+' > '+pfile+'.short.timeline'
  spawn,'rm awk.txt'
  
end 
