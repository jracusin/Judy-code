pro deproject_image,im,newim,inc=inc,doplot=doplot
  
  if n_elements(inc) eq 0 then inc=43.               ;degrees
  angle=81.2
  
  im2=gal_flat(im,angle,inc,cen)
  
  if keyword_set(doplot) then begin 
     !p.multi=[0,1,2]
     rdis,im
     rdis,im2
     !p.multi=0
  endif 
  
  newim=im2
  
  return
end 
