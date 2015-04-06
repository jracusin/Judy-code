pro extract_aspect_pos,catfile,ra,dec,evtfiles
  
  readcol,catfile,evtfiles,format='A'
  
  nevt=n_elements(evtfiles)
  fnum=fltarr(nevt)
  for i=0,nevt-1 do begin 
     fpos=strpos(evtfiles[i],'frame')
     fnum[i]=strmid(evtfiles[i],fpos+5,2)
  endfor 
  s=sort(fnum*1.)
  evtfiles=evtfiles[s]
  
  ra=fltarr(nevt) & dec=fltarr(nevt)
  for i=0,nevt-1 do begin
     evt=mrdfits(evtfiles[i],0,hdr)
     ra[i]=sxpar(hdr,'RA')
     dec[i]=sxpar(hdr,'DEC')
  endfor 
  
  
  return
end 
