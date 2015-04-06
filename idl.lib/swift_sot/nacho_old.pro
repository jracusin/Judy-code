PRO targ_lookup,ppst,uname,radius,table,ra,dec,name,wm,tid,targknow

  wm=where(ppst.targname EQ uname,nwm)

  IF strpos(uname,'(') NE -1 THEN $
    uname=strmid(uname,0,strpos(uname,'('))

  cmd='xbrowse_extract.pl position="'+uname+'" table='+table+' radius='+ntostr(radius)+' outfile=~/outtable.txt'
  spawn,cmd

  tid=ppst[wm[rem_dup(ppst[wm].targetid)]].targetid

  go=1
  openr,lun,'~/outtable.txt',/get_lun
  IF NOT eof(lun) THEN line=readline(lun)
  IF NOT eof(lun) THEN line=readline(lun) ELSE BEGIN
      go=0
      targknow=0
  ENDELSE 
      
  name=''
  ra=0D
  dec=0D
  WHILE NOT eof(lun) AND go DO BEGIN
      line=readline(lun,delim='|')
      IF n_elements(line) EQ 8 OR n_elements(line) EQ 7 THEN BEGIN
          IF n_elements(line) EQ 8 THEN BEGIN 
              name=[name,line[0]]
              r=str_sep(line[1],' ')*1D
              d=str_sep(line[2],' ')*1D
          ENDIF 
          IF n_elements(line) EQ 7 THEN BEGIN 
              name=[name,line[0]]
              r=str_sep(line[2],' ')*1D
              d=str_sep(line[3],' ')*1D
          ENDIF 
          hms2radec,r[0],r[1],r[2],d[0],d[1],d[2],rr,dd
          ra=[ra,rr]
          dec=[dec,dd]
          go=1
      ENDIF ELSE go=0
      targknow=1
  ENDWHILE 

  close,lun
  free_lun,lun

return
END 

pro nacho,pname,ppst=ppst
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NAChO - Name Atribute CHeck of Observations
;;        tool in Science Planning suite for comparing target database or 
;;        PPST target positions to known positions in SIMBAD/NED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  if n_elements(pname) eq 0 then begin
     pname=pickfile(title='Choose PPST or TargetDB')
     return
  endif 
  
  IF strpos(pname,'PPST') NE -1 THEN read_ppst,pname,ppst ELSE BEGIN
      IF strpos(pname,'DB') NE -1 THEN read_targdb,pname,ppst ELSE return
  ENDELSE 

  uniq=rem_dup(ppst.targname)

  ntarg=n_elements(ppst[uniq])
  
  unames=ppst[uniq].targname

  smax=max(strlen(unames))
  print,'Target Name'+smult(smax-11),' Target ID   del(ra,dec) (arcsec)'

  radius=0.5

  FOR i=0,ntarg-1 DO BEGIN
      
      uname=unames[i]
      
      table='xray'
      targ_lookup,ppst,uname,radius,table,ra,dec,name,wm,tid,targknow
      
      IF NOT targknow THEN BEGIN
          table='xray'
          uname=unames[i]
          targ_lookup,ppst,uname,radius*10.,table,ra,dec,name,wm,tid,targknow
      ENDIF 
      IF NOT targknow THEN BEGIN
          uname=unames[i]
          table='optical'
          targ_lookup,ppst,uname,radius,table,ra,dec,name,wm,tid,targknow
      ENDIF 
      IF NOT targknow THEN BEGIN
          uname=unames[i]
          table='optical'
          targ_lookup,ppst,uname,radius*10.,table,ra,dec,name,wm,tid,targknow
      ENDIF 
      uname=unames[i]

      FOR j=0,n_elements(tid)-1 DO BEGIN 
          wj=where(ppst[wm].targetid EQ tid[j])

          slen=strlen(uname)
          sname=''
          IF j EQ 0 THEN sname=unames[i]+smult(smax-slen) ELSE $
            sname=smult(smax)

          IF n_elements(ra) GT 1 THEN BEGIN 
              ra=ra[1:*]
              dec=dec[1:*]
              name=name[1:*]

              rdiff=abs(mean(ra)-mean(ppst[wm[wj]].ra))*3600.
              ddiff=abs(mean(dec)-mean(ppst[wm[wj]].dec))*3600.
              
              print,sname+' '+ntostr(tid[j])+'      ('+ntostr(rdiff)+','+ntostr(ddiff)+')'

          ENDIF ELSE print,sname+' '+ntostr(tid[j])+'      Unknown target name'

      ENDFOR 
  ENDFOR 

  spawn,'rm ~/outtable.txt'

;xbrowse_extract.pl position="Her x-1" table=xray radius=0.1 name_resolver=NED outfile=outtable.txt
  
  return
end 
