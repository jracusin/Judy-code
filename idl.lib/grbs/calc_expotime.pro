function calc_expotime,files
  
  expotime=0d
  for i=0,n_elements(files)-1 do begin
     hdr=headfits(files[i],exten=0)
     ontime=sxpar(hdr,'ONTIME')
     expotime=expotime+ontime
     
  endfor 
  
  return,expotime
  
end 
