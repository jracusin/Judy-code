pro dtas2chile,file,met,ra,dec
  
  readcol,file,time,coord,delim=',',format='(a,d)'
  
  w=where(time eq time[0])
  n=w[1]-1
  
  met=lonarr(n)
  for i=0L,n-1 do met[i]=date2met(time[i])
  
  dec=coord[0:n-1]
  ra=coord[n:*]
  
  return
end 
