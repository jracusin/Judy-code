pro get_ppst,datestart,datestop,file=file
  
  if n_params() eq 0 then begin
     print,'syntax - get_ppst,datestart,datestop,file'
     print,"             date should be in format '2005001'"
     return
  endif 
  
  url='http://swiftopen.swift.psu.edu/~swift/content/ArchPPSTL/'
  ppstlist=webget(url)
  n=n_elements(ppstlist.text)
  text=ppstlist.text[8:n-4]
  n=n_elements(text)
  
  idmin=strarr(n) & idmax=strarr(n) & pfile=strarr(n)
  for i=0,n-1 do begin 
     stuff=strsplit(text[i],'_>.',/ex)
     idmin[i]=strmid(stuff[3],0,7)
     idmax[i]=strmid(stuff[4],0,7)
     stuff2=strsplit(text[i],'><',/ex)
     pfile[i]=stuff2[3]
  endfor 
  
  w=where(idmin*1. le datestart*1. and idmax*1. ge datestop*1.,nw)
  if nw gt 0 then begin 
     file=pfile[w[nw-1]]
     ppst=webget(url+file)
     text=ppst.text
  endif else begin 
     w=where(idmin*1. ge datestart*1.,nw)
     w2=where(idmax[w] ge datestop*1.,nw2)
     if nw2 gt 0 then files=pfile[w[0:w2[0]]] else begin
        print,'End date not available'
        return
     endelse 
     ppst=webget(url+files[0])
     text=ppst.text
     for i=1,w[w2[0]]-w[0] do begin
        ppst2=webget(url+files[i])
        text2=ppst2.text
        text=[text,text2]
     endfor 
  endelse 
  file='MasterPPST_'+datestart+'0000_'+datestop+'0000.txt'
  
  n=n_elements(text)
  date=strarr(n)
  for i=0,n-1 do begin 
     stuff=strsplit(text[i],'-|',/ex)
     date[i]=stuff[0]+stuff[1]
  endfor 
  w=where(date*1. ge datestart*1. and date*1. lt datestop)
  print,file
  openw,lun,file,/get_lun
  printf,lun,text[w]
  close,lun
  free_lun,lun

  return
end 
