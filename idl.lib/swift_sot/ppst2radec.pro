pro ppst2radec,file, outlines=outlines, outpts=outpts,afst=afst,includeslew=includeslew,grablast=grablast
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ppst2radec - reads in PPST file and writes out ppst.lines 
;;               and ppst.pts for plotting timeline with 
;;               map_viewer (plot points - plot lines button)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  if not keyword_set(afst) then begin 
     if n_elements(file) eq 0 then begin
        file=file_search('PPST*.txt*')
        if n_elements(file) eq 1 then file=file[0] else $
           file=pickfile(filter='PPST*.txt*')
        print,file
     endif 
     a=strpos(file,'PPST')
     read_ppst,file,ppst,grablast=grablast
  endif else begin
     if n_elements(file) eq 0 then file=pickfile(filter='AFST*.txt*')
     a=strpos(file,'AFST')
     read_afst,file,ppst,includeslew=includeslew
  endelse 
  b=strpos(file,'txt')
  filebase=strmid(file,a,b-a)
  if n_elements(outlines) eq 0 then outlines=filebase[0]+'lines'
  if n_elements(outpts) eq 0 then outpts=filebase[0]+'pts'
  
  openr,plun,file,/get_lun
  openw,luout,outlines,/get_lun
  openw,pluout,outpts,/get_lun
  printf,luout,'celestial'
  printf,pluout,'celestial'
  
;  read_ppst,file,ppst
  r=rem_dup(ntostr(ppst.ra)+ntostr(ppst.dec))
  used=intarr(n_elements(ppst))
  for i=0,n_elements(ppst)-1 do begin 
;     dd=str_sep(ppst[i].begdate,'-')
     dd=str_sep(ppst[i].slewbeg,'-')
     dd2=str_sep(dd[2],':')
     day=dd[1]+(dd2[0]/24D)+(dd2[1]/24D/60D)+(dd2[2]/24D/60D/60D)
     year=dd[0]
     name=ppst[i].targname
     printf,luout,ppst[i].ra,ppst[i].dec,day,'  ',year
     if used[i] eq 0 then begin 
        w=where(r eq i,nw)
        if nw gt 0 then begin 
           used[i]=1
           printf,pluout,ntostr(ppst[i].ra)+','+ntostr(ppst[i].dec)+','+ntostr(day)+','+name
        endif 
     endif 
  endfor 
     
     
;   id=0
;   go=1
;   first=0
;   while (not EOF(plun)) do begin
;      line=''
;      readf,plun,line
;      data=strsplit(line,'|',/extract)
;      data=strtrim(data,2)
;      if ((data[1] eq 'PPT') and (data[2] eq 'Begin') or ((data[1] eq 'PPT') and (data[2] eq 'End') and first eq 0)) then begin
;         dd=str_sep(data[0],'-')
;         dd2=str_sep(dd[2],':')
;         year=dd[0]
;         day=dd[1]
;         hour=dd2[0]
;         minute=dd2[1]
;         second=dd2[2]
;         date=[year,day,hour,minute,second]
        
; ;        met=date2met(data[0])
; ;        date=met2date_judy(met)
;         printf,luout,data[7],'     ',data[8],'    ',date[1]+(date[2]/24D)+(date[3]/24D/60D)+(date[4]/24D/3600D)
;         if go then begin 
;            wid=where(id eq data[4],nwid)
;            if nwid eq 0 then begin
             
;               printf,pluout,data[7],'     ',data[8],'    ',date[1]+(date[2]/24D)+(date[3]/24D/60D)+(date[4]/24D/3600D)
;               id=[id,data[4]] 
;            endif; else go=0
;         endif 
;      endif else first=1 
;   endwhile
;   close,/all
  close,plun
  close,luout
  close,pluout
  free_lun,plun
  free_lun,luout
  free_lun,pluout
  
  return
end 
