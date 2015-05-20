pro read_qdp,file,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10

  readcol,file,line,delim='$',format='(a)',/silent
  wcom=where(strpos(line,'!') ne -1)
  wd=where(strpos(line,'!') eq -1)
  wsep=where(strpos(line,'NO NO NO NO NO') ne -1,nsep)
  if nsep gt 0 then begin 
     wsep=[wsep,n_elements(line)-1]
  endif else wsep=n_elements(line)-1
  nsep=nsep+1

  v0=0. & v1=0. & v2=0. & v3=0. & v4=0. & v5=0. & v6=0. & v7=0. & v8=0. & v9=0. & v10=0.
  for i=0,nsep-1 do begin ;;; loop over each section
     w=where(wcom lt wsep[i],nw)
     wstart=wcom[w[nw-1]]
     set=line[wstart:wsep[i]-1]
     nset=n_elements(set)
     for j=0,nset-1 do begin ;;; loop over each line
        vals=strsplit(set[j],' ',/ex)
        for k=0,n_elements(vals)-1 do tmp=execute('v'+ntostr(k)+'=[v'+ntostr(k)+','+vals[k]+']')
           ;;; loop over each value
stop
     endfor 
  endfor 

return
end 

  
  
