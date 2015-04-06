pro find_fillin,tstart,tstop,csvfile,nospawn=nospawn,no60=no60,nodone=nodone,sortra=sortra,sortdec=sortdec,filename=filename,sortha=sortha,sortfermi=sortfermi
  
  if n_params() eq 0 then begin
     print,'syntax - find_fillin,tstart,tstop,csvfile,[/sortha,/nodone,'
     print,'                    /sortra,/sortdec,/sortfermi,/nospawn]'
     return
  endif 
  
;  stkfile=file_search(stkfile)
  csvfile=file_search(csvfile)
  
;  if not exist('outfile.csv') then begin
;     print,'running fill_in_progress.py'
;     spawn,'fill_in_progress.py'
;  endif 
  
  year=strmid(tstart,0,4)
  day=ntostr(fix(strmid(tstart,5,3)))
  hr=strmid(tstart,9,2)
  mn=strmid(tstart,12,2)
  sec=strmid(tstart,15,2)
  tstartfrac=(hr/24d)+(mn/24./60d)+(sec/24./3600d)
  tstartfrac=strmid(ntostr(tstartfrac),2,4)
  
  year2=strmid(tstop,0,4)
  day2=ntostr(fix(strmid(tstop,5,3)))
  hr2=strmid(tstop,9,2)
  mn2=strmid(tstop,12,2)
  
  if (mn2*1.)-(mn*1.) gt 30. then begin
     print,'DURATION EXCEEDS 30 MINUTES - LIMIT DURATION AND RERUN'
     return
  endif 
  
  com='read_targ_database_exact.py '+tstart+' '+tstop+' '+csvfile
  print,com
  if not keyword_set(nospawn) then spawn,com
  
  if tstartfrac eq '0000' then filename='output_day'+day+'.0e.csv'
  filename='output_start_'+tstart+'.csv'
  filename=findfile(filename)
  print,filename
  readcol,filename[0],tname,tid,segno,ra,dec,roll,polldist,sunha,sunang,priority,vis,firststart,goodperc,meaneea,badwhen,comment,request,xrtexp,xrtperc,fermi,skip=1,delim=',',format='(a,l,i,d,d,d,a,d,d,i,i,a,i,d,a,a,a)'
  n=n_elements(tname)
;  readcol,'outfile.csv',tid2,tname2,comment2,ra2,dec2,xtime,utime,btime,request,xrtperc,done,delim=',',format='(l,a,aa,d,d,d,d,d,d,d,a)'
 
;  match,tid,tid2,w1,w2
  
;  if not keyword_set(no60) then w60=where(sunang[w1] gt 60) else w60=indgen(n_elements(w1))
;  if not keyword_set(nodone) then wx=where(xrtperc[w2[w60]] lt 90.) else wx=indgen(n_elements(w60))
  
;  s=sort(meaneea[w1[w60[wx]]])
  
;  i1=w1[w60[wx[s]]]
;  i2=w2[w60[wx[s]]]
  
  no60=1
  if not keyword_set(no60) then w60=where(sunang gt 60) else w60=indgen(n)
  if not keyword_set(nodone) then wx=where(xrtperc[w60] lt 90. and request[w60] gt 0) else wx=indgen(n_elements(w60))
  
  if keyword_set(sortra) then s=sort(ra[w60[wx]])
  if keyword_set(sortdec) then s=sort(dec[w60[wx]])
  if keyword_set(sortha) then s=sort(sunha[w60[wx]])
  if keyword_set(sortfermi) then s=sort(fermi[w60[wx]])
  if n_elements(s) eq 0 then s=sort(meaneea[w60[wx]])
  i1=w60[wx[s]]
  
  ;;no calibration
  wcal=where(tid[i1] lt 50000 or tid[i1] gt 90000L,nwcal)
  if nwcal gt 0 then i1=i1[wcal]
  
  ;;;search comment field for extra important fill-ins
  imparr=strarr(n_elements(comment))
  ;;search for IBIS targets
;  iibis=strpos(comment,'IBIS')
;  wibis=where(iibis ne -1,nwibis)
;  if nwibis gt 0 then imparr[wibis]=' IBIS'
  ;;search for Tueller targets
  ituel=strpos(comment,'Tueller')
  wtuel=where(ituel ne -1,nwtuel)
  if nwtuel gt 0 then imparr[wtuel]=' Tueller'
  ;;search for Falcone targets
;  iabe=strpos(comment,'Falcone')
;  wabe=where(iabe ne -1,nwabe)
;  if nwabe gt 0 then imparr[wabe]=' Falcone'
  ;;search for Romani targets
;  irom=strpos(comment,'Romani')
;  wrom=where(irom ne -1,nwrom)
;  if nwrom gt 0 then imparr[wrom]=' Romani'
  ;;search for GI unconstrained
  wgi=where((tid eq 90101) or (tid ge 90110 and tid le 90235) or (tid ge 90264 and tid le 90265),nwgi)
  if nwgi gt 0 then imparr[wgi]=' GI fillin'
  
  slen=[14,strlen(tname[i1])]
  
  str=strarr(n_elements(i1))
  for i=0,n_elements(i1)-1 do begin
     for j=0,max(slen)-slen[i+1] do str[i]=str[i]+' '
  endfor 
  
  sp='      '
  req=ntostr(fix(request[i1]))
  w=where(request[i1] lt 1000,nw)
  if nw gt 0 then req[w]=req[w]+' '
  w=where(request[i1] lt 10000,nw)
  if nw gt 0 then req[w]=req[w]+' '
  w=where(request[i1] lt 100000,nw)
  if nw gt 0 then req[w]=req[w]+' '
  
  xrtp=ntostr(fix(xrtperc[i1]))
  w=where(xrtperc[i1] lt 10,nw)
  if nw gt 0 then xrtp[w]=xrtp[w]+' '
  
  
;  print,'    Targ Name'+str[0]+'Targ ID     Seg       Roll          RA              Dec         SunHA          Sunang         MeanEEA        XRT %     Requested       Fermi       Special'
;  colprint,tname[i1]+str[1:*]+ntostr(tid[i1])+sp+ntostr(segno[i1])+sp+ntostr(roll[i1])+sp+ntostr(ra[i1],8)+sp+ntostr(dec[i1],8)+sp+ntostr(sunha[i1])+sp+ntostr(sunang[i1])+sp+ntostr(meaneea[i1])+sp+ntostr(xrtperc[i1])+sp+ntostr(request[i1])+sp+ntostr(fermi[i1])+sp+imparr[i1]
  
   print,'    Targ Name'+str[1]+'Targ ID        RA           Dec       SunHA     MeanEEA   XRT % Requested   Fermi       Special'
  colprint,tname[i1]+str+ntostr(tid[i1])+sp+ntostr(ra[i1],8)+sp+ntostr(dec[i1],8)+sp+ntostr(numdec(sunha[i1],1))+sp+ntostr(numdec(meaneea[i1],1))+sp+xrtp+sp+req+sp+ntostr(fix(fermi[i1]))+sp+imparr[i1]
  
  ;;need to make sort by coolest or furthest anti-sun option
  
  
  return
end 
