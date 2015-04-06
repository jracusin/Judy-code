pro read_targdb,file,outstr
  
  if n_elements(file) eq 0 then file=pickfile(filter='*DB*.csv')
  
  f=str_sep(file,'.csv')
  f=f[0]
  
  targetid=0L
  segno=0L
  targname=''
  targra=0D  
  targdec=0D 
  targroll=0D
  fom=0L
  type=''
  batmode=''
  xrtmode=''
  uvotmode=''
  duration=0.
  comment=''
  priority=0
  ssmin=0L
  ssmax=0L

  openr,plun,file,/get_lun
  data=readline(plun) ;header
  while (not EOF(plun)) do begin
     data=readline(plun,delim=',')
     targname=[targname,data[0]]
     type=[type,data[1]]
     targetid=[targetid,data[2]]
     segno=[segno,data[3]]
     fom=[fom,data[4]]
     targra=[targra,data[5]]
     targdec=[targdec,data[6]]
     targroll=[targroll,data[7]]
     batmode=[batmode,data[8]]
     xrtmode=[xrtmode,data[9]]
     uvotmode=[uvotmode,data[10]]
     duration=[duration,data[11]]
     comment=[comment,data[12]]
     priority=[priority,data[13]]
     ssmin=[ssmin,data[14]]
     ssmax=[ssmax,data[15]]

  endwhile
  close,plun
  free_lun,plun
  
  targetid=targetid[1:*]
  segno=segno[1:*]
  targname=targname[1:*]
  targra=targra[1:*]
  targdec=targdec[1:*]
  targroll=targroll[1:*]
  batmode=batmode[1:*]
  xrtmode=xrtmode[1:*]
  uvotmode=uvotmode[1:*]
  duration=duration[1:*]
  comment=comment[1:*]
  priority=priority[1:*]
  ssmin=ssmin[1:*]
  ssmax=ssmax[1:*]
  type=type[1:*]
  fom=fom[1:*]
  
  outstr=create_struct('targname','','type','','targetid',0L,'obsseg',0L,$
                       'fom',0L,'ra',0D,'dec',0D,'roll',0D,$
                      'batmode','','xrtmode','','uvotmode','',$
                      'duration',0.,'comment','',$
                      'priority',0,'ssmin',0L,'ssmax',0L)

  outstr=replicate(outstr,n_elements(targetid))
  outstr.targname=targname
  outstr.type=type
  outstr.targetid=targetid
  outstr.obsseg=segno
  outstr.fom=fom
  outstr.ra=targra
  outstr.dec=targdec
  outstr.roll=targroll
  outstr.batmode=batmode
  outstr.xrtmode=xrtmode
  outstr.uvotmode=uvotmode
  outstr.duration=duration
  outstr.comment=comment
  outstr.priority=priority
  outstr.ssmin=ssmin
  outstr.ssmax=ssmax


  return
end 


;date/time | commmand | begin/end | objname | target_id | obs_num | ?? | ra | dec | roll angle | BAT mode | XRT mode | UVOT mode | priority | ??
 
