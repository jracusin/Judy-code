pro make_st_pages,file,all=all,redo_afst=redo_afst,nositecopy=nositecopy
  
  ;;need to get wrapper to work for single PPST and not everything everytime
  dir='~/TAKO.data/'
  
  if keyword_set(all) then begin 
     file=findfile(dir+'PPST_20*.txt')
  endif else begin 
     if n_elements(file) eq 0 then file=pickfile(filter=dir+'PPST_20*.txt')
  endelse 
  
  nfile=n_elements(file)
  daymin=strarr(nfile) & daymax=strarr(nfile)
  dates='' & files=''

  for i=0,nfile-1 do begin 
     ppos=strpos(file[i],'PPST_')
     daymin[i]=strmid(file[i],ppos+5,7)
     daymax[i]=strmid(file[i],ppos+17,7)
     year=strmid(file[i],ppos+5,4)
     if year*1L mod 4 eq 0 then dinyear=366 else dinyear=365 ;;leap year calc

     if (daymin[i]*1L-year*1000L) ge dinyear then begin
        newyear=ntostr(long(year*1L+1L))
        ndays=daymax[i]*1L+dinyear-daymin[i]*1L-1000L
        olddays=ntostr(dinyear+lindgen(dinyear+year*1000L+1L-daymin[i]*1L))
        w=where(olddays*1L le 100,nw)
        if nw gt 0 then olddays[w]='0'+olddays[w]
        w=where(olddays*1L le 10,nw)
        if nw gt 0 then olddays[w]='0'+olddays[w]
        newdays=ntostr(long(lindgen(daymax-(newyear*1e3))+1L))
        w=where(newdays*1L le 100,nw)
        if nw gt 0 then newdays[w]='0'+newdays[w]
        w=where(newdays*1L le 10,nw)
        if nw gt 0 then newdays[w]='0'+newdays[w]

        days=[year+olddays,newyear+newdays]
        
     endif else begin 
        ndays=daymax[i]*1L-daymin[i]*1L
        days=ntostr(long(daymin[0]+lindgen(ndays)))
     endelse 
     print,ndays,days

     if ndays lt 10 then begin 
        for j=0,ndays-1 do begin 
;           date=ntostr(daymin[i]*1L+j)
           date=days[j]
           dates=[dates,date]
           files=[files,file[i]]
        endfor 
     endif else begin 
        dates=[dates,date]
        files=[files,file[i]]
     endelse 
  endfor 
  
  dates=dates[1:*]
  files=files[1:*]
  r=rem_dup(dates)
  dates=dates[r]
  files=files[r]
  
  for i=0,n_elements(dates)-1 do begin 
     ppst_page,files[i],date=dates[i]
     indiv_targ_pages,files[i],datestr=dates[i]
  endfor 
     
  webdir='~/webdir/PPST/'
;  spawn,'rm '+webdir+'afstdatelist.html'
  afst_page,redo_afst=redo_afst
  if exist(webdir+'AFST*txt') then spawn,'rm '+webdir+'AFST*txt'
  
  if not keyword_set(nositecopy) then begin 
     print,'Uploading Webpage'
     spawn,'sitecopy -uk ppst'
  endif 
  
  return
end 
