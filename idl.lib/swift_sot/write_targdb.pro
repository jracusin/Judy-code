pro write_targdb,db,file
  
  openw,lun,file,/get_lun
  printf,lun,'target_name, type, target_ID, segment, FoM, RA, dec, roll, BATmode, XRTmode, UVOTmode, duration, comment, priority, SSmin, SSmax,'
  for i=0,n_elements(db)-1 do begin
     line=''
     for j=0,15 do line=line+ntostr(db[i].(j))+','
     printf,lun,line
  endfor 
  
  
  close,lun,/file
  return
end 
