pro sort_html_list,file,date=date,htmldate=htmldate,remdup=remdup
  
  openr,flun,file,/get_lun
  
  line=''
  lines=''
  scrit=''
  while (not eof(flun)) do begin 
     
     readf,flun,line
     
     lines=[lines,line]
     
;     scr=str_sep(line,"'")
     scr1=str_sep(line,">")
     scr2=str_sep(scr1[1],"<")
     scr=scr2[0]
     scrit=[scrit,scr]
;     if n_elements(scr) le 1 then scr=str_sep(line,'"')
;     scrit=[scrit,scr[1]]
     
  endwhile
  close,flun
  free_lun,flun
  
  lines=lines[1:*]
  scrit=scrit[1:*]
  
  nlines=n_elements(lines)

  if keyword_set(remdup) then begin
     rd=rem_dup(lines)
     lines=lines[rd]
     scrit=scrit[rd]
     nlines=n_elements(rd)
  endif 

  if nlines gt 1 then begin 
     if keyword_set(date) then begin 
        year=strarr(nlines)
        day=year
        month=year
        mono=year
        for i=0,nlines-1 do begin
           year[i]=strmid(scrit[i],7,4)
           day[i]=strmid(scrit[i],4,2)
           month[i]=strmid(scrit[i],0,3)
           mono[i]=month_cnv(month[i])
           scrit[i]=string(year[i],mono[i],day[i],format='(a4,i2.2,a2)')
        endfor
;        scrit=year+ntostr(mono)+day
     endif 
     if keyword_set(htmldate) then begin
        for i=0,nlines-1 do begin 
           ypos=strpos(lines[i],'200')
           year=strmid(lines[i],ypos,4)
           dpos=strpos(lines[i],'Day')+4
           doy=strmid(lines[i],dpos,3)
           scrit[i]=year+doy
        endfor 
     endif 
     
     s=sort(scrit)
     
     openw,flun,file,/get_lun
     for i=0,nlines-1 do begin 
        printf,flun,lines[s[i]]
     endfor
     close,flun
     free_lun,flun
  endif else begin 
     if nlines eq 1 then begin 
        openw,flun,file,/get_lun
        printf,flun,lines
        close,flun
        free_lun,flun
     endif 
  endelse 
     
  
  return
end 
