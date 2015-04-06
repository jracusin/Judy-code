pro fill_in_roll,file,stk,day,tidstart=tidstart,noroll=noroll
  
  if n_params() lt 3 then begin
     print,'syntax - fill_in_roll,csvfile,stk,day,tidstart=tidstart,/noroll'
     return
  endif 
  readcol,file,tname,ttype,tid,seg,fom,ra,dec,roll,bmode,xmode,umode,dur,com,priority,smin,smax,format='(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',delim=','
  
  ntarg=n_elements(tname)
  outroll=strarr(ntarg)
  
  if n_elements(tidstart) eq 1 then begin 
     tid=[tidstart+indgen(ntarg),tidstart+indgen(ntarg)]
     tid=tid[sort(tid)]
     tid=['target_ID',ntostr(tid)]
  endif 
  for i=1,ntarg-1 do begin
     tq=str_sep(tname[i],'"')
     if n_elements(tq) eq 3 then tname[i]=tq[1]
     tq=str_sep(com[i],'"')
     if n_elements(tq) eq 3 then com[i]=tq[1]
     tq=str_sep(ttype[i],'"')
     if n_elements(tq) eq 3 then ttype[i]=tq[1]
     tq=str_sep(bmode[i],'"')
     if n_elements(tq) eq 3 then tq=tq[1]
     if strlen(tq) lt 6 then bmode[i]='0x'+to_hex(tq,4) else $
        bmode[i]=tq
     tq=str_sep(xmode[i],'"')
     if n_elements(tq) eq 3 then tq=tq[1]
     if strlen(tq) lt 6 then xmode[i]='0x'+to_hex(tq,4) else $
        xmode[i]=tq
     tq=str_sep(umode[i],'"')
     if n_elements(tq) eq 3 then tq=tq[1]
     if strlen(tq) lt 6 then umode[i]='0x'+to_hex(tq,4) else $
        umode[i]=tq
     
     if not keyword_set(noroll) and i gt 0 then begin 
        print,'roll_range.py '+stk+' '+ntostr(day)+' '+ra[i]+' '+dec[i]+' > roll.tmp'
        spawn,'roll_range.py '+stk+' '+ntostr(day)+' '+ra[i]+' '+dec[i]+' > roll.tmp'
        openr,lun,'roll.tmp',/get_lun
        while not eof(lun) do line=readline(lun,delim='$')
        close,lun
        free_lun,lun
        tmp=str_sep(line,',')
        tmp2=str_sep(tmp[0],' ')
        outroll[i]=tmp2[n_elements(tmp2)-1]
     endif else outroll[i]=roll[i]
        print,outroll[i]
  endfor 
  
  c=' , '
  openw,lun,file+'.r',/get_lun
  printf,lun,'target_name ,  type , target_ID ,  segment ,  FoM ,  RA ,  dec ,  roll ,  BATmode ,  XRTmode ,  UVOTmode ,  duration ,  comment ,  priority ,  SSmin ,  SSmax'

  printf,lun,tname[1:*]+c+ttype[1:*]+c+tid[1:*]+c+seg[1:*]+c+fom[1:*]+c+ra[1:*]+c+dec[1:*]+c+outroll[1:*]+c+bmode[1:*]+c+xmode[1:*]+c+umode[1:*]+c+dur[1:*]+c+com[1:*]+c+priority[1:*]+c+smin[1:*]+c+smax[1:*]
  close,lun
  free_lun,lun
  
  
  
  return
end 
