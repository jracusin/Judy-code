PRO targ_lookup,ppst,uname,ra,dec,wm,tid

  wm=where(ppst.targname EQ uname,nwm)

  IF strpos(uname,'(') NE -1 THEN $
    uname=strmid(uname,0,strpos(uname,'('))
  
  file='~/outtable.txt'
  cmd='snr "'+uname+'" > '+file
  
  spawn,cmd

  tid=ppst[wm[rem_dup(ppst[wm].targetid)]].targetid
  
  openr,lun,'~/outtable.txt',/get_lun
  IF NOT eof(lun) THEN begin 
     line=readline(lun)
     if n_elements(line) eq 3 then begin 
        ra=line[0]*1D
        dec=line[1]*1D
     endif else begin 
        ra=0D
        dec=0D
     endelse 
  endif else begin 
     ra=0D
     dec=0D
  endelse 
  
  close,lun
  free_lun,lun

  return
END 

pro nacho,pname,ppst=ppst,outfile=outfile,all=all
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NAChO - Name Atribute CHeck of Observations
;;        tool in Science Planning suite for comparing target database or 
;;        PPST target positions to known positions in SIMBAD/NED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  if n_elements(pname) eq 0 then begin
     pname=pickfile(title='Choose PPST or TargetDB')
  endif 
  
  IF strpos(pname,'PPST') NE -1 THEN read_ppst,pname,ppst ELSE BEGIN
      IF strpos(pname,'csv') NE -1 THEN read_targdb,pname,ppst ELSE return
  ENDELSE 
  
  if n_elements(outfile) eq 0 then outfile=pname+'.nacho'
  
  print,'Writing output file: '+outfile
  openw,olun,outfile,/get_lun
  
  uniq=rem_dup(ppst.targname)

  ntarg=n_elements(ppst[uniq])
  
  unames=ppst[uniq].targname

  smax=max(strlen(unames))
  print,'Target Name'+smult(smax-11)+' Target ID        DB(ra,dec)             SIMBAD(ra,dec)           del(ra,dec) (arcsec)'
  printf,olun,'Target Name'+smult(smax-11)+' Target ID        DB(ra,dec)             SIMBAD(ra,dec)           del(ra,dec) (arcsec)'

  FOR i=0,ntarg-1 DO BEGIN
     
     uname=unames[i]
     wm=where(ppst.targname EQ uname,nwm)
     
     if keyword_set(all) or (strpos(ppst[wm[0]].targname,'GRID') eq -1) then begin 
        
        targ_lookup,ppst,uname,ra,dec,wm,tid
        
        uname=unames[i]
        
        FOR j=0,n_elements(tid)-1 DO BEGIN 
           wj=where(ppst[wm].targetid EQ tid[j])
           
           slen=strlen(uname)
           sname=''
;           IF j EQ 0 THEN sname=unames[i]+smult(smax-slen) ELSE $
;              sname=smult(smax)
           sname=unames[i]+smult(smax-slen)
           
           if ra ne 0D and dec ne 0D then begin 
              rdiff=abs(mean(ra)-mean(ppst[wm[wj]].ra))*3600.
              ddiff=abs(mean(dec)-mean(ppst[wm[wj]].dec))*3600.
              
              if keyword_set(all) or ((ppst[wm[wj[0]]].targetid mod 10 eq 0) and ((rdiff gt 60.) or (ddiff gt 60.))) then begin 
                 
                 print,sname+'  '+ntostr(tid[j])+'      ('+ntostr(ppst[wm[wj[0]]].ra)+','+ntostr(ppst[wm[wj[0]]].dec)+')     ('+ntostr(ra)+','+ntostr(dec)+')     ('+ntostr(rdiff)+','+ntostr(ddiff)+')'
                 
                 printf,olun,sname+'  '+ntostr(tid[j])+'      ('+ntostr(ppst[wm[wj[0]]].ra)+','+ntostr(ppst[wm[wj[0]]].dec)+')     ('+ntostr(ra)+','+ntostr(dec)+')     ('+ntostr(rdiff)+','+ntostr(ddiff)+')'
              endif 
           ENDIF ELSE begin
              if keyword_set(all) or (ppst[wm[wj[0]]].targetid mod 10 eq 0) and (strpos(ppst[wm[wj[0]]].targname,'GRID') eq -1) then begin 
                 print,sname+'  '+ntostr(tid[j])+'      Unknown target name'
                 printf,olun,sname+'  '+ntostr(tid[j])+'      Unknown target name'
              endif 
           endelse 
        ENDFOR  
     endif     
  ENDFOR  
  
  spawn,'rm ~/outtable.txt'
  
  close,olun
  free_lun,olun
  
  return
end 
