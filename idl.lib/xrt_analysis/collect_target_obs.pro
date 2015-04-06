pro collect_target_obs,targids,outstr,daystr=daystr,filedir=filedir,instr=instr,dir=dir
  
  if n_params() lt 2 then begin
     print,'syntax - collect_target_obs,targids,outstr,filedir=filedir,daystr=daystr,dir=dir'
     return
  end
  
  
  if n_elements(filedir) eq 0 then filedir=''
;  dir='/bulk/yankees/xrt/pass_data/'
;  dir='/bulk/axiom/xrt/pass_data/'
  if n_elements(dir) eq 0 then dir='/bulk/yankees2/xrt/pass_data/'
  
  openw,wlun,filedir+'targids.txt',/get_lun
 
  for i=0,n_elements(targids)-1 do begin 
     if targids[i] gt 100000L then s='' else s=' '
     printf,wlun,'Target ID: '+s+ntostr(targids[i])
  endfor 
  close,wlun
  free_lun,wlun
  
  if n_elements(daystr) eq 0 then daystr=''
  cmd='grep -f '+filedir+'targids.txt '+dir+'pass_'+daystr+'*/*science.0.timeline -H > '+filedir+'targouts.txt'
  print,cmd
  spawn,cmd
;  spawn,'grep -f '+filedir+'targids.txt pass_*/*science.0.timeline -H > '+filedir+'targouts.txt'
  
  openr,rlun,filedir+'targouts.txt',/get_lun
  line=''
  tfiles=''
  ldp=0L
  while not eof(rlun) do begin
     chunks=readline(rlun)
     tfiles=[tfiles,strmid(chunks[0],0,strlen(chunks[0])-1)]
     ldp=[ldp,chunks[4]]
  endwhile
  
  tfiles=tfiles[1:*]
  ldp=ldp[1:*]

  if n_elements(instr) ne 0 then begin 
     inldp=instr[rem_dup(instr.ldp)].ldp
     dont_match,inldp,ldp,dm1,dm2
     tfiles=tfiles[dm2] ;only tfiles that haven't been used before (dont match)
     ldp=ldp[dm2]
     print,'only retrieving new ldps'
  endif 

  
  ldps=0L
  targetid=0L
  obsseg=0L
  time=0D
  ccdt=0D
  frame=0L
  settled=0
  ten_amin=0
  ccsdstime=0D
  frame=0
  cps=0D
  state=''
  timestamp=''
  safe=0
  saa=0
  deltat=0D
  dd=0D
  
  tfile=''
  mode=''
  uniq=tfiles[rem_dup(tfiles)]
  for i=0,n_elements(uniq)-1 do begin 
     
     timeline=uniq[i]
     wq=where(tfiles eq uniq[i],nwq)
     openr,lun,timeline,/get_lun

     while not eof(lun) do begin 
        chunks=readline(lun)

        if n_elements(chunks) ge 22 then begin 
           
           for q=0,nwq-1 do begin 
              qq=wq[q]
              if chunks[10]*1L eq ldp[qq] then begin
                 
                 tmp=readline(lun)
                 while (n_elements(tmp) ne 14) and (n_elements(tmp) ne 22) $
                    and (not eof(lun)) do $
                    tmp=readline(lun)
                 
                 if n_elements(tmp) eq 14 then cks=tmp else cks=0
                 
                 while n_elements(cks) eq 14 and not eof(lun) do begin
                    tfile=[tfile,timeline]
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
           endfor 
        endif 
     endwhile
  
     close,lun
     free_lun,lun
     print,timeline
  endfor      
  
  tfile=tfile[1:*]
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
                       'frame',0,'mode','','cps',0D,'state','',$
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
  outstr.timeline=tfile
  
;  spawn,'rm '+filedir+'targids.txt'
;  spawn,'rm '+filedir+'targouts.txt'
  
  mwrfits,outstr,filedir+'targets.fits',/create
  if n_elements(instr) gt 1 then begin 
     tmpstr=outstr
     outstr=0
     stop
     concat_structs,instr,tmpstr,outstr
     mwrfits,outstr,filedir+'targets.fits',/create
  endif 
  
  
;  dt=outstr.date[1]+outstr.date[2]/24.+outstr.date[3]/24./60.+outstr.date[3]/24./3600.

  return
end 
