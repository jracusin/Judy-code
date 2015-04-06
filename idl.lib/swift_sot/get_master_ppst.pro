pro get_master_ppst,date1,date2,outfile,noppst=noppst
  
;  url='http://swiftopen.swift.psu.edu/~swift/content/ArchPPSTL/'
  
;  datelist=webget(url)
  
;  n=n_elements(datelist.text)
;  text=datelist.text[8:n-4]
;  n=n_elements(text)
  
;  iapos=strpos(text,'MasterPPST')
;  itpos=strpos(text,'txt')
;  for i=0,n-1 do begin 
;     text2=strmid(text,iapos[i],itpos[i]-iapos[i]+3)
;     idmin=strmid(text2,11,7)
;     idmax=strmid(text2,23,7)
;  endfor 
  
;  w=where(date1[0] ge idmin and date2[0] le idmax,nw)
;  noppst=0
;  if nw gt 0 then begin
;     w=w[n_elements(w)-1]
;     line=text[w]
;     apos=strpos(line,'MasterPPST')
;     tpos=strpos(line,'txt">')
;     file=strmid(line,apos,tpos-apos+3)
;     ppst=webget(url+file)
;     ppst=ppst.text
;  endif 
;  if nw eq 0 then begin
;     w1=where(date1 ge idmin,nw1)
;     w2=where(date2 le idmax,nw2)
;     if nw1 gt 0 and nw2 gt 0 then begin 
;        w=[max(w1),min(w2)]
;;     w=[indgen(min(w2)-max(w1))+max(w1)]
;        line=text[w]
;        for i=0,n_elements(w)-1 do begin
;           apos=strpos(line[i],'MasterPPST')
;           tpos=strpos(line[i],'txt">')
;           file=strmid(line[i],apos,tpos-apos+3)
;           ppst1=webget(url+file)
;           ppst1=ppst1.text
;           if i gt 0 then ppst=[ppst0,ppst1]
;           ppst0=ppst1
;        endfor
;     endif else noppst=1
;  endif 
  
  url='http://swiftopen.swift.psu.edu/~swift/content/PPSTL/MasterPPST.txt'
  ppst=webget(url)
  ppst=ppst.text
  
  noppst=1
;  if noppst eq 0 then begin 
  if n_elements(ppst) gt 0 then begin 
     pdate=strarr(n_elements(ppst))
     for i=0,n_elements(ppst)-1 do begin
        chunks=str_sep(ppst[i],' | ')
        pdates=chunks[0]
        pdate[i]=strmid(pdates,0,4)+strmid(pdates,5,3)
     endfor 
     w=where(pdate*1L ge date1[0]*1L and pdate*1L lt date2[0]*1L,nw)

     if nw gt 2 then begin 
        ppst=ppst[w]


        outfile='MasterPPST_'+date1+'0000_'+date2+'0000_00.txt'
        openw,lun,outfile,/get_lun
        for i=0L,n_elements(ppst)-1 do printf,lun,ppst[i]
        close,lun
        free_lun,lun
        noppst=0
     endif else noppst=1
  endif else print,'Date is not included in MasterPPST'
  
  return
end 
