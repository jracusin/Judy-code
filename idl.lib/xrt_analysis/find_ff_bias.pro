function find_ff_bias,bf,dir=dir
  
  if n_elements(dir) eq 0 then dir=''
  biasfiles=file_search(dir+'*bias_map*')
  
  if biasfiles[0] eq '' then return,0
  
  n=n_elements(biasfiles)
  s=lonarr(n)
  for i=0,n-1 do begin
     s[i]=file_size(biasfiles[i])
  endfor 
  
  m=max(s,ffs)
  
  hdr=headfits(biasfiles[ffs])
  
  bf=''
  if sxpar(hdr,'XRTSTATE') eq 'Manual  ' and sxpar(hdr,'WIDTH') eq 600 then $
     bf=biasfiles[ffs] else begin
     for i=0,n-1 do begin
        hdr=headfits(biasfiles[i])
        if sxpar(hdr,'XRTSTATE') eq 'Manual  '  and sxpar(hdr,'WIDTH') eq 600 $
           then bf=biasfiles[i]
     endfor 
     
  endelse 
  if bf eq '' then dop=0 else begin
     spawn,'gzip '+bf
     bf=str_sep(bf,'.gz')
     bf=bf[0]+'.gz'
     dop=1
  endelse 
  
  return,dop
end 
