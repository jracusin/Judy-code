pro hms2radec,rhh,rmm,rss,dhh,dmm,dss,ra,dec
  
  if n_params () eq 0 then begin 
     print,'syntax - hms2radec,rhh,rmm,rss,dhh,dmmdss,ra,dec'
     return
  endif 
  
  ra=tenv(rhh*15.,rmm*15.,rss*15.)
  w=where(dhh eq '-00',nw) 
  if nw gt 0 then dmm[w]=-1.*dmm[w]
  dec=tenv(dhh,dmm,dss)
  
  return
end 
