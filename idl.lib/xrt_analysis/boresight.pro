pro boresight,daymin,daymax
  
;  days=130+indgen(14)
  days=daymin+indgen(daymax-daymin)
  
  begplot,name='~/boresight/frames_d'+ntostr(min(days))+'_'+ntostr(max(days))+'.ps',/color;,/land
  
  !p.multi=[0,2,4]
  for i=0,n_elements(days)-1 do begin
     
     file='~/boresight/frames_d'+ntostr(days[i])+'.fits'
     
     fr=mrdfits(file,1)
     
     position_tracking,fr,/skip,title='Day '+ntostr(days[i])

  endfor
  endplot
  !p.multi=0
  return
end 
