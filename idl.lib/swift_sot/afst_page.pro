pro afst_html_table,ppst,pname,ppstname,datestr=datestr
  
  webdir='~/webdir/PPST/'
  
  if n_elements(ppst) eq 0 then ppst=pickfile(filter='*ST*txt')
  
  ;;making page of individual ppst tables
  print,'Making AFST page for: ',ppst
  read_afst,ppst,pout,date=datestr
  if n_elements(pout) eq 0 then return
  
  txtpos=strpos(ppst,'txt')
  ppstpos=strpos(ppst,'AFST')
  
  if n_elements(datestr) gt 0 then begin
     day=strmid(pout.begdate,5,3)
     year=strmid(pout.begdate,0,4)
     date=ntostr(year+day)
     w=where(date ge datestr*1L and date lt datestr*1L+1L)
     pout=pout[w]
     pname='AFST_'+datestr+'0000_'+ntostr(datestr*1L+1L)+'0000_'+strmid(ppst,txtpos-3,3)+'html'
     ppstname='AFST_'+datestr+'0000_'+ntostr(datestr*1L+1L)+'0000_'+strmid(ppst,txtpos-3,3)+'txt'
     datedir=datestr+'/'
  endif else begin 
     pname=strmid(ppst,ppstpos,txtpos-ppstpos)+'html'
     ppstname=strmid(ppst,ppstpos,txtpos+3)
     datedir=strmid(pname,0,7)+'/'
  endelse 
  
;  pname=strmid(ppst,ppstpos,txtpos-ppstpos)+'html'
;  pname='AFST_'+date+'0000_'+ntostr(date+1L)+'0000.html'
  
;  ppstname=strmid(ppst,ppstpos,txtpos+3)
  
  td='<TD> <FONT size="2">'
  tr='<TR>'
  th='<TH>'
  tdc='<TD> <FONT size="2" color="#FF0000">'
  
  if exist(webdir+datedir+pname) then spawn,'rm '+webdir+datedir+pname
  
  openw,lun,webdir+datedir+pname,/get_lun
  printf,lun,'<title> '+ppstname+' </title>'
  printf,lun,'<TABLE border="1">'
  printf,lun,'<CAPTION>'+ppstname+'</CAPTION>'
  printf,lun,tr+th+'Begin Obs'+th+'End Obs'+th+'Target Name'+th+'RA'+th+'Dec'+th+'Roll'+th+'Target ID'+th+'Seg No'+th+'XRT mode'+th+'UVOT mode'+th+'FOM'+th+'Time Observed (s)'
  for i=0,n_elements(pout)-1 do begin
     tname=pout[i].targname
     
     spechar=[' ','(',')','/',':','[',']','{','}',"'"]
     tmp=tname
     for sc=0,n_elements(spechar)-1 do begin
        tmp2=strsplit(tmp,spechar[sc],/ex)
        tmp2=strjoin(tmp2)
        tmp=tmp2
     endfor 
     tname2=strjoin(tmp,'_')

     if pout[i].saa eq 1 then tdd=tdc else tdd=td
     tdiff=ntostr(round(pout[i].endtime-pout[i].begtime))
     case pout[i].xmode of 
        '0x0007': xmode='PC'
        '0x0006': xmode='WT'
        '0x0000': xmode='AUTO'
        else: xmode=''
     endcase
     printf,lun,tr+tdd+pout[i].begdate+tdd+pout[i].enddate+tdd+'<a href = "'+tname2+'_at.html">'+pout[i].targname+'</a>'+tdd+ntostr(pout[i].ra)+tdd+ntostr(pout[i].dec)+tdd+ntostr(pout[i].roll)+tdd+ntostr(pout[i].targetid)+tdd+ntostr(pout[i].obsseg)+tdd+xmode+tdd+pout[i].umode+tdd+ntostr(pout[i].fom)+tdd+tdiff
  endfor 
  
  
  printf,lun,'</TABLE>'
;  printf,lun,'<FONT size="2" color="#FF0000"> Spacecraft enters SAA during this observation'
  
  close,lun
  free_lun,lun
  
  return
end

pro afst_page,redo_afst=redo_afst
  
  webdir='~/webdir/PPST/'
  
                                ;  ;;making html table for afst
;  st_html_table,afst,aname,afstname
  
  ;;making ppst date list
  ppstdatelist=webdir+'ppstdatelist.html'
  
  readcol,ppstdatelist,dates,format='A',delim=',',/silent
  
  afstdatelist=webdir+'afstdatelist.html'
  aa=0
  if exist(afstdatelist) then begin
     readcol,afstdatelist,adates,format='A',delim=',',/silent
     nd=n_elements(adates)
     if nd gt 0 then begin 
        dont_match,adates,dates,ma,mp
        nd=n_elements(mp)
     endif 
     aa=1
     if mp[0] eq -1 then begin
        print,'No AFST pages to add'
        return
     endif
     openw,alun,afstdatelist,/get_lun,/append
  endif else begin
     openw,alun,afstdatelist,/get_lun
     nd=n_elements(dates)
     print,'opening: '+afstdatelist
  endelse 
 
  dpos=strpos(dates,'PPST/')
  dates=strmid(dates,dpos[0]+13,11)
  
  for i=0,nd-1 do begin 
     if not aa then dd=dates[i] else dd=dates[mp[i]]
     
     date=readdate2filedate(dd)
     datedir=date+'/'
     if not exist(webdir+datedir) then spawn,'mkdir '+webdir+date
     day=strmid(date,4,3)
     print,dd+' - '+date
     
;     alun,"<a href = '"+dd+"'>"+webdir+dd+'.html'+'</a> <br><br>'
     get_afst,date,file,/only

     if file ne '' then begin 
        printf,alun,"<a href = 'PPST/"+datedir+dd+".html'>"+dd+'</a> -- Day '+day+'<br>'
;        afst=webdir+file
        afst=file
        
        afst_html_table,afst,aname,afstname,date=date
        if n_elements(aname) eq 0 then return
        
        datefile=webdir+datedir+dd+'.html'
        readcol,datefile,lines,format='(a)',delim='$',/silent
        
        if not keyword_set(redo_afst) then begin 
           openw,lun,datefile,/get_lun
           for t=0,4 do printf,lun,lines[t]
           printf,lun,'<br>As Flown Science Timeline (AFST) <br>'
           printf,lun,"<a href = '"+aname+"'>"+afstname+'</a> <br>'
        endif 
        ;;need to add plot of afst
;     psname=strmid(afstname,0,strpos(afstname,'txt'))+'ps'
        f=str_sep(afstname,'.txt')
        f=f[0]
        enddate=ntostr(date*1L+1L)
        stpos=strpos(f,'ST_20')
        psname=strmid(f,stpos-2,5)+date+'0000_'+enddate+'0000_'+strmid(f,stpos+27,2)+'.ps'
        pdfname=strmid(f,stpos-2,5)+date+'0000_'+enddate+'0000_'+strmid(f,stpos+27,2)+'.pdf'
        
        if not exist(webdir+datedir+pdfname) then begin 
           plot_ppst,afst,psname,date=date
           spawn,'ps2pdf '+psname
           spawn,'mv '+pdfname+' '+webdir+datedir
           spawn,'rm '+psname
        endif 
        psoname=strmid(pdfname,strpos(pdfname,'AFST_'))
        psname=psoname
        if not keyword_set(redo_afst) then begin 
           printf,lun,"<a href = '"+psname+"'>"+psoname+'</a> <br> <br>'
           close,lun
           free_lun,lun
        endif 
        
        spawn,'chmod 655 '+webdir+datedir+aname
        spawn,'chmod 655 '+webdir+datedir+psname
        spawn,'chmod 655 '+datefile
        
        indiv_targ_pages,afst,datestr=date
     endif
  endfor    
  
  close,alun
  free_lun,alun
  
  sort_html_list,afstdatelist,/htmldate,/remdup
  return
end 
     
     
