pro update_targid_table,ppst
  
  ;;making page of individual ppst tables
  print,'Updating targid mySQL database for: ',ppst
  read_ppst,ppst,pout
  
  txtpos=strpos(ppst,'txt')
  ppstpos=strpos(ppst,'PPST')
  
  for i=0,n_elements(pout)-1 do begin
          
     case pout[i].xmode of 
        '0x0007': xmode='7'
        '0x0006': xmode='6'
        '0x0000': xmode='0'
        else: xmode='0'
     endcase

	 str=strsplit(pout[i].umode,'x',/EXTRACT)
	 umode=strtrim(str[1],2)
     targid=strtrim(pout[i].targetid,2)
     seg=strtrim(pout[i].obsseg,2)

     spawn,'update_targdb_ppst.py '+targid+' '+seg+' '+umode+' '+xmode
  endfor

  return
end

pro ppst_html_table,ppst,pname,ppstname,datestr=datestr
  
  webdir='~/webdir/PPST/'
  
  if n_elements(ppst) eq 0 then ppst=pickfile(filter='PPST*txt')
  
  ;;making page of individual ppst tables
  print,'Making PPST page for: ',ppst
  read_ppst,ppst,pout
  
  txtpos=strpos(ppst,'txt')
  ppstpos=strpos(ppst,'PPST')
  
  if n_elements(datestr) gt 0 then begin
     day=strmid(pout.begdate,5,3)
     year=strmid(pout.begdate,0,4)
     date=ntostr(year+day)
     w=where(date ge datestr*1L and date lt datestr*1L+1L)
     pout=pout[w]
     pname='PPST_'+datestr+'0000_'+ntostr(datestr*1L+1L)+'0000_'+strmid(ppst,txtpos-3,3)+'html'
     ppstname='PPST_'+datestr+'0000_'+ntostr(datestr*1L+1L)+'0000_'+strmid(ppst,txtpos-3,3)+'txt'
     datedir=datestr+'/'
  endif else begin 
     pname=strmid(ppst,ppstpos,txtpos-ppstpos)+'html'
     ppstname=strmid(ppst,ppstpos,txtpos+3)
     datedir=strmid(pname,0,7)+'/'
  endelse 
  
  if not exist(webdir+datedir) then spawn,'mkdir '+webdir+datedir
  
  td='<TD><FONT size="2">'
  tr='<TR>'
  th='<TH>'
  tdc='<TD><FONT size="2" color="#FF0000">'
  tre='</TR>'
  tde='</TD>'
  the='</TH>'
  
  pfile=webdir+datedir+pname
  if exist(pfile) then spawn,'rm '+pfile
  openw,lun,pfile,/get_lun
  printf,lun,'<title> '+ppstname+' </title>'
  printf,lun,'<TABLE border="1">'
  printf,lun,'<CAPTION>'+ppstname+'</CAPTION>'
  printf,lun,tr+th+'Begin Obs'+th+'End Obs'+the+th+'Target Name'+the+th+'RA'+the+th+'Dec'+the+th+'Roll'+the+th+'Target ID'+the+th+'Seg No'+the+th+'XRT mode'+the+th+'UVOT mode'+the+th+'FOM'+th+'Obs Time (s)'+the
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
     
     printf,lun,tr+tdd+pout[i].begdate+tde+tdd+pout[i].enddate+tde+tdd+'<a href = "'+tname2+'_ppt.html">'+pout[i].targname+'</a>'+tde+tdd+ntostr(pout[i].ra)+tdd+ntostr(pout[i].dec)+tde+tdd+ntostr(pout[i].roll)+tde+tdd+ntostr(pout[i].targetid)+tdd+ntostr(pout[i].obsseg)+tde+tdd+xmode+tde+tdd+pout[i].umode+tde+tdd+ntostr(pout[i].fom)+tde+tdd+tdiff+tde
  endfor 
  
  
  printf,lun,'</TABLE>'
;  printf,lun,'<FONT size="2" color="#FF0000"> Spacecraft enters SAA during this observation'
  
  close,lun
  free_lun,lun

  update_targid_table,ppst
  
  return
end

pro ppst_page,ppst,date=date
  
  webdir='~/webdir/PPST/'
  
  ;;making html table for ppst
  ppst_html_table,ppst,pname,ppstname,date=date
  
;  ;;making html table for afst
;  if n_elements(date) eq 0 then datestr=str_sep(ppstname,'_') else $
;     datestr=['',strmid(date,0,4)+strmid(date,4,3)]
  
  datestr=str_sep(ppstname,'_')
  date2=filedate2readdate(datestr[1])
  day=strmid(datestr[1],4,3)
  year=strmid(datestr[1],0,4)
  
  datedir=year+day+'/'
  
;  get_afst,year+day,afst
;  print,afst
;  st_html_table,afst,aname,afstname
  
  ;;making ppst date list
  ppstdatelist=webdir+'ppstdatelist.html'
  openu,lun,ppstdatelist,/get_lun,/append
  
  outline="<a href = 'PPST/"+datedir+date2+".html'>"+date2+'</a> -- Day '+day+'<br>'
  printf,lun,outline
  close,lun
  free_lun,lun
  sort_html_list,ppstdatelist,/remdup,/htmldate
  
  ;;remove date from afst page list so that it will rerun
  afstdatelist=webdir+'afstdatelist.html'
  if exist(afstdatelist) then begin
     readcol,afstdatelist,adates,format='A',delim=',',/silent
     nd=n_elements(adates)
     if nd gt 0 then begin 
        wa=where(adates ne outline,nwa)
        if nwa gt 0 then adates=adates[wa]
        openw,alun,afstdatelist,/get_lun
        for i=0,nwa-1 do printf,alun,adates[i]
        close,alun
        free_lun,alun
     endif else spawn,'rm '+afstdatelist
  endif 
  
  ;;making ppst date webpage
  datefile=webdir+datedir+date2+'.html'
  openw,lun,datefile,/get_lun
  printf,lun,'<title> '+date2+' -- Day '+day+' </title>'
  printf,lun,'<center> <b>'+date2+' -- Day '+day+'</b> </center><br>'
  printf,lun,'Pre-Planned Science Timeline (PPST) <br>'
  printf,lun,"<a href = '"+pname+"'>"+ppstname+'</a> <br>'
  psname=strmid(ppstname,0,strpos(ppstname,'txt'))+'ps'
  pdfname=strmid(ppstname,0,strpos(ppstname,'txt'))+'pdf'
  
  if not exist(webdir+datedir+pdfname) then begin 
     plot_ppst,ppst,psname,date=date
     spawn,'ps2pdf '+psname
     spawn,'mv '+pdfname+' '+webdir+datedir
     spawn,'rm '+psname
  endif 
  psoname=strmid(pdfname,strpos(pdfname,'PPST_'))
  
  psname=psoname
  printf,lun,"<a href = '"+psname+"'>"+psoname+'</a> <br> <br>'
  
;  printf,lun,'As Flown Science Timeline (AFST) <br>'
;  printf,lun,"<a href = '"+aname+"'>"+afstname+'</a> <br><br>'
  ;;need to add plot of afst
  
  ;;need to add xrt ccd temp plots and anything else normally put of docushare?
  close,lun
  free_lun,lun
  
  ;;making main ppst list webpage
  ppstlist='~/webdir/obsdata.html';'science_planning.html'
  openw,lun,ppstlist,/get_lun
;  printf,lun,'<html>'
;  printf,lun,'<head>'
;  printf,lun,'<title>Swift Science Planning</title>'
;  printf,lun,'</head>'
;  printf,lun,'<center> <b> Swift Observatory Science Planning Products</b></center><br>'
;  printf,lun,'<body>'
;  printf,lun,'<br>'
;  printf,lun,'<p class="toop_header" align="center">Observing Schedule</p>'
  printf,lun,'<div align="center">'
  readcol,ppstdatelist,v1,format='A',delim=',',/silent
  for i=n_elements(v1)-1,0,-1 do printf,lun,v1[i]
  printf,lun,'</div>'
;  printf,lun,'</body>'
;  printf,lun,'</html>'
  close,lun
  free_lun,lun
  
;  spawn,'cat '+webdir+'obssch_p1.htm '+webdir+'science_planning.html '+webdir+'obssch_p2.htm > '+webdir+'obsschedule.htm'
  
  spawn,'chmod 655 '+webdir+datedir+pname
  spawn,'chmod 655 '+webdir+datedir+psname
  spawn,'chmod 655 '+datefile
  spawn,'chmod 655 '+ppstlist
  
;  if exist(webdir+'afstdatelist.html') then spawn,'rm '+webdir+'afstdatelist.html'
  
  return
end 
