pro cen2reg,centroidfile,regionfile,rad=rad
  
  if n_params() eq 0 then begin
     print,'syntax - cen2reg,centroidfile,regionfile,rad=rad'
     print,'         converts output from xrtcentroid to ds9 region file'
     return
  endif 
  
  readcol,centroidfile,text,equal,number,delim=' ',format='(a,a,a)'
  ra=number[0]
  dec=number[1]
  
  if n_elements(regionfile) eq 0 then regionfile='src.reg'
  if n_elements(rad) eq 0 then rad='47.14624' ;20 pix
  
  openw,lun,regionfile,/get_lun
  printf,lun,'# Region file format: DS9 version 3.0'
  printf,lun,'# Filename: '+centroidfile
  printf,lun,'global color=green font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  printf,lun,'fk5;circle('+ra+','+dec+','+rad+'")'
  
  close,lun
  
  return
end 
