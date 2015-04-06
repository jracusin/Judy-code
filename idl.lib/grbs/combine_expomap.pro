pro combine_expomap,dirs
  
  files=file_search(dirs+'/sw*pc*ex.img')
  print,files
  openw,lun,'merge_expomap.xim',/get_lun
  for i=0,n_elements(files)-1 do begin
     fits_info,files[i],n_ext=next,/silent
     for j=1,next do begin 
        printf,lun,'read/expo/size=512/file="'+files[i]+'"'+'/gtiext='+ntostr(j)   
        if i gt 0 then printf,lun,'sum'
        printf,lun,'save'
     endfor 
     printf,lun,'write/fits/expo/file="combined_expomap'+ntostr(i)+'.fits"'
  endfor 

  printf,lun,'write/fits/expo/file="combined_expomap.fits"'
  printf,lun,'cpd /xw'
  printf,lun,'disp'
  close,lun
  free_lun,lun
  
  spawn,'ximage @merge_expomap.xim'
  
  return
end 
