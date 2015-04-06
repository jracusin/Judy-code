pro read_timeline,outstr,file
  
  if n_elements(file) eq 0 then begin
     file=pickfile(filter='*science*timeline')
  endif 
  
  openr,lun,file,/get_lun
  
  ldps=0L
  targetid=0L
  obsseg=0L
  time=0D
  ccdt=0D
  frame=0L
  settled=0
  ten_amin=0
  ccsdstime=0D
  frame=0L
  cps=0D
  state=''
  timestamp=''
  safe=0
  saa=0
  deltat=0D
  dd=0D
  
  tfile=''
  mode=''
  
  while not eof(lun) do begin 
     chunks=readline(lun)

     if n_elements(chunks) ge 22 then begin 
        
        tmp=readline(lun)
        while (n_elements(tmp) ne 14) and (n_elements(tmp) ne 22) $
           and (not eof(lun)) do $
           tmp=readline(lun)
        
        if n_elements(tmp) eq 14 then cks=tmp else cks=0
        
        while n_elements(cks) eq 14 and not eof(lun) do begin
           frame=[frame,cks[0]]
           mode=[mode,cks[1]]
           cps=[cps,cks[2]]
           state=[state,cks[3]]
           time=[time,cks[4]]
           timestamp=[timestamp,cks[5]]
           if cks[6] eq '************' then cks[6]=0D
           deltat=[deltat,cks[6]]
           dd=[dd,cks[7]]
           ccdt=[ccdt,cks[8]]
           safe=[safe,cks[9]]
           saa=[saa,cks[10]]
           ten_amin=[ten_amin,cks[11]]
           settled=[settled,cks[12]]
           ccsdstime=[ccsdstime,cks[13]]
           ldps=[ldps,chunks[10]*1L]
           targetid=[targetid,chunks[13]*1L]
           obsseg=[obsseg,chunks[16]*1L]
           
           cks=readline(lun)
        endwhile
     endif
  endwhile
  
  close,lun
  free_lun,lun
  
  if n_elements(ldps) eq 1 then return
  ldps=ldps[1:*]
  targetid=targetid[1:*]
  obsseg=obsseg[1:*]
  time=time[1:*]
  ccdt=ccdt[1:*]
  mode=mode[1:*]
  settled=settled[1:*]
  frame=frame[1:*]
  ten_amin=ten_amin[1:*]
  ccsdstime=ccsdstime[1:*]
  cps=cps[1:*]
  state=state[1:*]
  timestamp=timestamp[1:*]
  safe=safe[1:*]
  saa=saa[1:*]
  deltat=deltat[1:*]
  dd=dd[1:*]
  
  tid=rem_dup(targetid)
  
  outstr=create_struct('targetid',0L,'obsseg',0L,'LDP',0L,$
                       'frame',0L,'mode','','cps',0D,'state','',$
                       'met',0D,'timestamp','','deltat',0D,'dd',0D,'ccdt',0D,$
                       'safe',0,'saa',0,'ten_amin',0,'settled',0,$
                       'ccsdstime',0D,'date',fltarr(5),'timeline','')
  outstr=replicate(outstr,n_elements(targetid))
  outstr.targetid=targetid
  outstr.ldp=ldps
  outstr.obsseg=obsseg
  outstr.frame=frame
  outstr.mode=strtrim(mode,2)
  outstr.cps=cps
  outstr.state=state
  outstr.met=time
  outstr.timestamp=timestamp
  outstr.deltat=deltat
  outstr.dd=dd
  outstr.safe=safe
  outstr.saa=saa
  outstr.settled=settled
  outstr.ten_amin=ten_amin
  outstr.ccsdstime=ccsdstime
  for i=0L,n_elements(time)-1 do outstr[i].date[*]=met2date_judy(time[i])
  outstr.ccdt=ccdt
  outstr.timeline=file
  
  return
end 
