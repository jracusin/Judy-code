pro write_regfile,filename,ra,dec,rad,rad2,color=color,annulus=annulus,box=box,roll=roll,det=det
  
  if n_params() eq 0 then begin
     print,'syntax - write_regfile,filename,ra,dec,rad,rad2,color=color,annulus=annulus,box=box,roll=roll,det=det'
     return
  endif 
  
  if n_elements(color) eq 0 then color='green' else begin
     sep=str_sep(color,'!')
     sep=sep[1]
     color=sep
  endelse 
  openw,lun,filename,/get_lun

  n=n_elements(ra)
  printf,lun,'# Region file format: DS9 version 3.0'
  printf,lun,'# Filename: '
  printf,lun,'global color='+color+' font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source=1'
  aa=''
  if not keyword_set(det) then begin
     t='fk5'
  endif else t=''
;     printf,lun,'fk5'
;     aa='"'
;  endif 
;  for i=0,n_elements(ra)-1 do begin 

  if type(rad) eq 7 then radd=rad else radd=ntostr(rad)
  if n_elements(radd) eq 1 and n gt 1 then radd=replicate(radd,n)

  if keyword_set(annulus) then begin
     if type(rad2) eq 7 then radd2=rad2 else radd=ntostr(rad2)
     for i=0,n-1 do printf,lun,t+';annulus('+ntostr(ra[i])+','+ntostr(dec[i])+','+radd2[i]+','+radd[i]+')'
  endif 
  if keyword_set(box) then begin
;        if keyword_set(det) then $
     for i=0,n-1 do printf,lun,'box('+ntostr(ra[i])+','+ntostr(dec[i])+','+radd[0]+aa+','+radd2+aa+','+ntostr(roll[i])+')' 
     if n_elements(rad) gt 1 then printf,lun,'-box('+ntostr(ra)+','+ntostr(dec)+','+radd[1]+aa+','+radd2+aa+','+ntostr(roll)+')'
     
     
        
;           printf,lun,'fk5;box('+ntostr(ra)+','+ntostr(dec)+','+ntostr(rad)+','+ntostr(rad2)+','+ntostr(roll)+')'
  endif 
  
  if not keyword_set(annulus) and not keyword_set(box) then $
     for i=0,n-1 do printf,lun,t+';circle('+ntostr(ra[i])+','+ntostr(dec[i])+','+radd[i]+')'
;  endfor 
  close,lun
  free_lun,lun
  
  return
end 
