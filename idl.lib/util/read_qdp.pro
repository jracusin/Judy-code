pro read_qdp,file,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,type=type,tpos=tpos

  if n_elements(tpos) eq 0 then tpos=2
  readcol,file,line,delim='$',format='(a)',/silent
  w=where(strpos(line,'!#') eq -1 and strpos(line,'!1') eq -1 and strpos(line,'!2') eq -1 and strpos(line,'!3') eq -1 and strpos(line,'!4') eq -1 and strpos(line,'!5') eq -1 and strpos(line,'!6') eq -1 and strpos(line,'!7') eq -1 and strpos(line,'!8') eq -1 and strpos(line,'!9') eq -1,nw)

  if nw gt 0 then line=line[w]
  wcom=where(strpos(line,'!') ne -1)
  wd=where(strpos(line,'!') eq -1)
  wsep=where(strpos(line,'NO') ne -1,nsep)
  if nsep gt 0 then begin 
     wsep=[wsep,n_elements(line)]
  endif else wsep=n_elements(line)
  nsep=nsep+1

  v0=0. & v1=0. & v2=0. & v3=0. & v4=0. & v5=0. & v6=0. & v7=0. & v8=0. & v9=0. & v10=0.
  type=''
  for i=0,nsep-1 do begin ;;; loop over each section
     w=where(wcom lt wsep[i],nw)
     wstart=wcom[w[nw-1]]+1
     t=strtrim(strmid(line[wstart-tpos],1,100),2)
     set=line[wstart:wsep[i]-1]
     nset=n_elements(set)
     for j=0,nset-1 do begin ;;; loop over each line
        type=[type,t]
        vals=strsplit(set[j],' ',/ex)
        for k=0,n_elements(vals)-1 do tmp=execute('v'+ntostr(k)+'=[v'+ntostr(k)+','+vals[k]+']')
           ;;; loop over each value
     endfor 
  endfor 
  type=type[1:*]

  for i=0,10 do tmp=execute('if n_elements(v'+ntostr(i)+') gt 1 then v'+ntostr(i)+'=v'+ntostr(i)+'[1:*]')

return
end 

  
  
