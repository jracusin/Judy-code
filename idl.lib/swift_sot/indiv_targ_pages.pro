pro indiv_targ_pages,ppst,datestr=datestr
 
  webdir='~/webdir/PPST/'
  if strpos(ppst,'AFST') eq -1 then begin
     read_ppst,ppst,pout 
     ppstpos=strpos(ppst,'PPST')
     afst=0
     prefix='PPST'
  endif else begin 
     read_afst,ppst,pout,date=datestr
     afst=1
     ppstpos=strpos(ppst,'AFST')
     prefix='AFST'
  endelse 
  
  txtpos=strpos(ppst,'txt')
  
  if n_elements(datestr) gt 0 then begin
     day=strmid(pout.begdate,5,3)
     year=strmid(pout.begdate,0,4)
     date=ntostr(year+day)
     w=where(date ge datestr*1L and date lt datestr*1L+1L)
     pout=pout[w]
     name=prefix+'_'+datestr+'0000_'+ntostr(datestr*1L+1L)+'0000_'+strmid(ppst,txtpos-3,3)
     datedir=datestr+'/'
  endif else begin 
     name=strmid(ppst,ppstpos,txtpos-ppstpos)
     datedir=strmid(pname,0,7)+'/'
  endelse 
  
  rd=rem_dup(pout.targname)
  
  for i=0,n_elements(rd)-1 do begin 
     tname=pout[rd[i]].targname
     t=where(pout.targname eq tname,nt)
     
     if afst then suffix='_at' else suffix='_ppt'
     spechar=[' ','(',')','/',':','[',']','{','}',"'"]
     tmp=tname
     for sc=0,n_elements(spechar)-1 do begin
        tmp2=strsplit(tmp,spechar[sc],/ex)
        tmp2=strjoin(tmp2)
        tmp=tmp2
     endfor 
     tname2=strjoin(tmp,'_')
     pname=tname2+suffix+'.html'
     
     td='<TD> <FONT size="2">'
     tr='<TR>'
     th='<TH>'
     tdc='<TD> <FONT size="2" color="#FF0000">'
     openw,lun,webdir+datedir+pname,/get_lun
     printf,lun,'<title> '+tname+' </title>'
     printf,lun,'<TABLE border="1">'
     printf,lun,'<CAPTION>'+name+'txt - '+tname+'</CAPTION>'
     printf,lun,tr+th+'Begin Obs'+th+'End Obs'+th+'Target Name'+th+'RA'+th+'Dec'+th+'Roll'+th+'Target ID'+th+'Seg No'+th+'XRT mode'+th+'UVOT mode'+th+'FOM'+th+'Obs Time (s)'
     tottime=0L
     for j=0,nt-1 do begin
        if pout[t[j]].saa eq 1 then tdd=tdc else tdd=td
        tdiff=ntostr(round(pout[t[j]].endtime-pout[t[j]].begtime))
        case pout[t[j]].xmode of 
           '0x0007': xmode='PC'
           '0x0006': xmode='WT'
           '0x0000': xmode='AUTO'
           else: xmode=''
        endcase
        printf,lun,tr+tdd+pout[t[j]].begdate+tdd+pout[t[j]].enddate+tdd+pout[t[j]].targname+tdd+ntostr(pout[t[j]].ra)+tdd+ntostr(pout[t[j]].dec)+tdd+ntostr(pout[t[j]].roll)+tdd+ntostr(pout[t[j]].targetid)+tdd+ntostr(pout[t[j]].obsseg)+tdd+xmode+tdd+pout[t[j]].umode+tdd+ntostr(pout[t[j]].fom)+tdd+tdiff
        tottime=tottime+tdiff
     endfor 
     
     printf,lun,tr+th+th+th+th+th+th+th+th+th+th+td+'Total = '+ntostr(tottime)
     printf,lun,'</TABLE>'
;     printf,lun,'<FONT size="2" color="#FF0000"> Spacecraft enters SAA during this observation'
     close,lun
     free_lun,lun
     spawn,'chmod 655 '+webdir+datedir+pname
  endfor 
  
  return
end 
