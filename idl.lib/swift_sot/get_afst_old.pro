pro get_afst,date,file,onlydate=onlydate
  
  if n_params() eq 0 then begin
     print,'syntax - get_afst,date,file'
     print,"             date should be in format '2005001'"
     return
  endif 
  
  if n_elements(file) gt 0 then infile=file else infile=''
  webdir='~/webdir/PPST/'
  url='http://swiftopen.swift.psu.edu/~swift/content/asflownTL/'
  afstlist=webget(url)
  n=n_elements(afstlist.text)
  text=afstlist.text[8:n-4]
;  w=where(strpos(text,string(date)) ne -1)
;  if w eq -1 then return
  w=n_elements(text)-1
  
  line=text[w]
  apos=strpos(line,'AFST')
  tpos=strpos(line,'txt">')
  tmpfile=strmid(line,apos,tpos-apos+3)
  
  infile2=infile
;  print,infile2
  if strmid(tmpfile,5,7) gt date then begin
     
     dmin=strmid(text,apos+5,tpos-apos-25)
     dmax=strmid(text,apos+17,tpos-apos-25)
     w=where(date gt dmin and date*1L+1 le dmax)
;     w=where(strpos(text,string(date)) ne -1)
     w=max(w)
     
     if infile ne '' then begin 
        iapos=strpos(infile,'AFST')
        itpos=strpos(infile,'txt')
        infile2=strmid(infile,iapos,itpos-iapos+3)
        idmin=strmid(infile2,iapos+5,itpos-iapos-25)
        idmax=strmid(infile2,iapos+17,itpos-iapos-25)
        q=where(date gt idmin and date lt idmax*1+1,nq)
        if nq gt 0 then w=where(strpos(text,infile2) ne -1)
     
     endif 
      
     line=text[w]
     apos=strpos(line,'AFST')
     tpos=strpos(line,'txt">')
     file=strmid(line,apos,tpos-apos+3)
  endif else file=tmpfile
  
  if date*1L+1L gt (strmid(tmpfile,17,7)*1D)+(strmid(tmpfile,24,4)/2400D) $
     then file='' else begin 
     if not exist(file) and file ne '' then begin 
        if infile2 ne file then begin 
           afst=webget(url+file)
           openw,lun,webdir+file,/get_lun
           printf,lun,afst.text
           close,lun
           free_lun,lun
        endif else file=infile2
     endif 
  endelse 
  
  if keyword_set(onlydate) then begin
     year=strmid(date,0,4)
     day=strmid(date,4,3)
     outfile='AFST_'+date+'0000_'+ntostr(date*1L+1)+'0000_00.txt'
     spawn,'grep "'+year+'-'+day+'" '+webdir+file+' > '+outfile
     file=outfile
     
  endif 
  
  return
end 
