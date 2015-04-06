pro get_afst,date,file,onlydate=onlydate,partialday=partialday
  
  if n_params() eq 0 then begin
     print,'syntax - get_afst,date,file'
     print,"             date should be in format '2005001'"
     return
  endif 
  
  if n_elements(file) gt 0 then infile=file else infile=''
  webdir='~/webdir/PPST/'
;  url='http://swiftopen.swift.psu.edu/~swift/content/asflownTL/'
  url='http://swiftopen.swift.psu.edu/~swift/content/sdc/'
  afstlist=webget(url)
  n=n_elements(afstlist.text)
  text=afstlist.text[8:n-4]
  n=n_elements(text)
  
  iapos=strpos(text,'AFST')
  itpos=strpos(text,'txt')
  idmin=strarr(n) & idmax=idmin
  for i=0,n-1 do begin 
     text2=strmid(text[i],iapos[i],itpos[i]-iapos[i]+3)
     idmin[i]=strmid(text2,5,7)
     idmax[i]=strmid(text2,17,7)
  endfor 
  
  q=where(date gt idmin and date lt idmax*1+1 or date eq idmin and date eq idmax,nq)

  if nq eq 1 then w=q else begin 
     
     url='http://swiftopen.swift.psu.edu/~swift/content/asflownTL/'
     afstlist=webget(url)
     n=n_elements(afstlist.text)
     text=afstlist.text[8:n-4]
     n=n_elements(text)
     w=n_elements(text)-2
     iapos=strpos(text,'AFST')
     itpos=strpos(text,'txt')
     idmin=strarr(n) & idmax=idmin
     for i=0,n-1 do begin 
        text2=strmid(text[i],iapos[i],itpos[i]-iapos[i]+3)
        idmin[i]=strmid(text2,5,7)
        idmax[i]=strmid(text2,17,7)
     endfor 
;     q=where((date gt idmin and date lt idmax*1L+1L) or (date eq idmin and date eq idmax),nq)
     q=where(date ge idmin and date le idmax,nq)
     if nq gt 1 then begin 
        q=q[n_elements(q)-1]
        nq=n_elements(q)
        if q eq -1 then nq=0
     endif 
     w=n_elements(text)-2
     
     if nq eq 1 then w=q else begin 
     
        url='http://swiftopen.swift.psu.edu/~swift/content/asflownTL/archive/'
        afstlist=webget(url)
        n=n_elements(afstlist.text)
        text=afstlist.text[8:n-4]
        n=n_elements(text)
        w=n_elements(text)-2

     endelse  
  endelse    

  line=text[w]
  apos=strpos(line,'AFST')
  tpos=strpos(line,'txt">')
  tmpfile=strmid(line,apos,tpos-apos+3)
  
  infile2=infile

  if strmid(tmpfile,5,7) ge date then begin
     
     if nq eq 0 then begin 
        dmin=strmid(text,apos+5,tpos-apos-25)
        dmin=dmin[0:n_elements(dmin)-2]
        dmax=strmid(text,apos+17,tpos-apos-25)
        dmax=dmax[0:n_elements(dmax)-2]
        w=where(date[0]*1L ge dmin*1L and date[0]*1L+1L le dmax*1L)
        w=max(w)
     endif 

     if infile ne '' then begin 
        iapos=strpos(infile,'AFST')
        itpos=strpos(infile,'txt')
        infile2=strmid(infile,iapos,itpos-iapos+3)
        
        infile3=str_sep(infile2,'_')
        infile3=infile3[1:*]
        infile4=infile3[0]+'*'+infile3[1]+'*'+infile3[2]
        infile2=infile4
        
        idmin=strmid(infile2,iapos+5,itpos-iapos-25)
        idmax=strmid(infile2,iapos+17,itpos-iapos-25)
        q=where(date gt idmin and date lt idmax*1L+1,nq)
        if nq gt 0 then w=where(strpos(text,infile2) ne -1)

     endif 
      
     line=text[w]
     apos=strpos(line,'AFST')
     tpos=strpos(line,'txt">')
     file=strmid(line,apos,tpos-apos+3)
  endif else file=tmpfile
  
  if date+0.9d gt (strmid(tmpfile,17,7)*1D)+(strmid(tmpfile,24,4)/2400D) and not keyword_set(partialday) then begin
     print,'As-flown is not yet completed'
     file=''
     
     return
  endif 

  afst=webget(url+file)
  
  openw,lun,webdir+file,/get_lun
  printf,lun,afst.text
  close,lun
  free_lun,lun
  
  if keyword_set(onlydate) then begin
     year=strmid(date,0,4)
     day=strmid(date,4,3)
     outfile='AFST_'+date+'0000_'+ntostr(date*1L+1)+'0000_00.txt'
     spawn,'grep "'+year+'-'+day+'" '+webdir+file+' > '+outfile
     file=outfile
     
  endif 
  
  return
end 
