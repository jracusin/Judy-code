pro batse_fluence_calc
  
  file='~/Desktop/GRB080319B/batse_fluence_table.txt'
  
;  readcol,file,trig,f1,f1err,f2,f2err,format='(i,d,d,d,d)'
;  readcol,file,f3,f3err,f4,f4err,format='(d,d,d,d)'
  
  readcol,file,lines,delim='%',format='(a)',comment='#'
  n=n_elements(lines)
  
  f1=0. & f2=0. & f3=0. & f4=0.
  for i=0,n-1 do begin
     chunks=str_sep(lines[i],' ')
     nch=n_elements(chunks)
     if nch eq 5 then begin
        f1=[f1,chunks[1]]
        f2=[f2,chunks[3]]
     endif 
     if nch eq 4 then begin
        f3=[f3,chunks[0]]
        f4=[f4,chunks[2]]
     endif 
     
  endfor 
  
  f=f1+f2+f3+f4
  w=where(f ne 0,nw)
  f=f[w]
  
  plothist,alog10(f)
  flu19b=6.1e-4
  oplot,alog10([flu19b]),[0,500],color=!green
  wb=where(f gt flu19b,nwb)
  print,nwb*1./nw
  
  stop
  return
end 
