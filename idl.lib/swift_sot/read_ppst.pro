pro read_ppst,file,outstr,grablast=grablast
  
  if n_elements(file) eq 0 then file=pickfile(filter='PPST*.txt*')
  
  begtime=''
  endtime=''
  type=''
  action=''
  name=''
  targid=0L
  segno=0
  ra=0d
  dec=0d
  roll=0d
  xmode=0
  umode=0
  fom=0
  saa=0
  insaa=0
  settled=''
  begmet=0d
  endmet=0d
  settime=''
  firstppt=0
  begtype=''
  
  
  
  openr,lun,file,/get_lun
  while not eof(lun) do begin 
     line=readline(lun,delim='|')
     line=strtrim(line,2)
;     if n_elements(line) eq 17 then begin
        if line[1] eq 'PPT' and line[2] eq 'End' and firstppt eq 0 and keyword_set(grablast) then begin 
           begtime=[begtime,line[0]] ;only parameter that's right in begin
           begmet=[begmet,date2met(line[0])]
           begtype=line[1]
           settime=line[0]
           settled=[settled,settime]
           endtime=[endtime,line[0]]
           endmet=[endmet,date2met(line[0])]
           type=[type,line[1]]
           action=[action,line[2]]
           if line[3] eq '' then nm=line[1] else nm=line[3]
           name=[name,nm]
           targid=[targid,line[4]]
           segno=[segno,line[5]]
           ra=[ra,line[7]]
           dec=[dec,line[8]]
           roll=[roll,line[9]]
           xmode=[xmode,line[11]]
           umode=[umode,line[12]]
           fom=[fom,line[13]]
           saa=[saa,insaa]
           settime=''
           begtype=''
        endif 
        if (line[1] eq 'PPT' or line[1] eq 'TOO/AT') $
           and line[2] eq 'Begin' then begin 
           firstppt=1
           begtime=[begtime,line[0]] ;only parameter that's right in begin
           begmet=[begmet,date2met(line[0])]
           begtype=line[1]
        endif 
        ;;set flag in SAA
        if line[1] eq 'saa' and line[2] eq 'Begin' then insaa=1
        if line[1] eq 'saa' and line[2] eq 'End' then insaa=0
        if line[1] eq 'mnv' and line[2] eq 'End' then settime=line[0]
        if (line[1] eq 'PPT' or line[1] eq 'TOO/AT') $
           and line[2] eq 'End' and firstppt then begin 
           
           if begtype ne line[1] then line[1]=begtype
;           if settime eq '' then settime=begtime[n_elements(begtime)-1]
           settled=[settled,settime]
           endtime=[endtime,line[0]]
           endmet=[endmet,date2met(line[0])]
           type=[type,line[1]]
           action=[action,line[2]]
           if line[3] eq '' then nm=line[1] else nm=line[3]
           name=[name,nm]
           targid=[targid,line[4]]
           segno=[segno,line[5]]
           ra=[ra,line[7]]
           dec=[dec,line[8]]
           roll=[roll,line[9]]
           xmode=[xmode,line[11]]
           umode=[umode,line[12]]
           fom=[fom,line[13]]
           saa=[saa,insaa]
           settime=''
           begtype=''
        endif 
;     endif 
  
  endwhile
  close,lun
  free_lun,lun
  
  
  nbeg=n_elements(begtime)
  nend=n_elements(endtime)
  if nend ne nbeg then begin 
     n=min([nbeg,nend])-1
  endif else n=nbeg-1
   
  outstr=create_struct('targetid',0L,'obsseg',0L,'begdate','','enddate','',$
                       'targname','','begtime',0D,'endtime',0D,$
                       'slewbeg','','slewend','',$
                       'ra',0D,'dec',0D,'roll',0D,'saa',0,$
                       'xmode','','umode','','fom',0)

  outstr=replicate(outstr,n)
  
  outstr.targetid=targid[1:n]
  outstr.obsseg=segno[1:n]
  outstr.begdate=begtime[1:n]
  outstr.enddate=endtime[1:n]
  outstr.begtime=begmet[1:n]
  outstr.endtime=endmet[1:n]
  outstr.slewbeg=begtime[1:n]
  outstr.slewend=settled[1:n]
  outstr.targname=name[1:n]
  outstr.ra=ra[1:n]
  outstr.dec=dec[1:n]
  outstr.roll=roll[1:n]
  outstr.xmode='0x'+to_hex(xmode[1:n],4)
  outstr.umode='0x'+to_hex(umode[1:n],4)
  outstr.fom=fom[1:n]
  outstr.saa=saa[1:n]

  return
end 


;date/time | commmand | begin/end | objname | target_id | obs_num | ?? | ra | dec | roll angle | BAT mode | XRT mode | UVOT mode | priority | ??
 
