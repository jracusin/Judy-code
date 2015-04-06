pro read_frames,frinfo,dir=dir,ldp=ldp
  
  frinfo=create_struct('targetid',0L,'obs_seg',0L,'ldp',0L,'frame_no',0L,$
                       'tstart',0D,'tstop',0D,$
                       'obs_mode','','datamode','','ra',0D,'dec',0D,'roll',0D,$
                      'tam_x1',0.,'tam_y1',0.,'tam_x2',0.,'tam_y2',0.)
                       
  
  if n_elements(dir) eq 0 then dir=''
  if n_elements(ldp) eq 0 then ldp=''
  files=file_search(dir+'*science*'+ldp+'*frame*fits')
  nfiles=n_elements(files)
  
  
  frinfo=replicate(frinfo,nfiles)
  
  for i=0L,nfiles-1 do begin 
     file=files[i]
     
     frame=mrdfits(file,0,hdr,/silent)
     
     frinfo[i].targetid=sxpar(hdr,'TARGETID')
     frinfo[i].obs_seg=sxpar(hdr,'OBS_SEG')
     frinfo[i].tstart=sxpar(hdr,'TSTART')
     frinfo[i].tstop=sxpar(hdr,'TSTOP')
     if frinfo[i].tstart eq 0 then begin 
        frinfo[i].tstart=sxpar(hdr,'T_START')
        frinfo[i].tstop=sxpar(hdr,'T_STOP')
     endif 
     frinfo[i].obs_mode=sxpar(hdr,'OBS_MODE')
     frinfo[i].datamode=sxpar(hdr,'DATAMODE')
     frinfo[i].ra=sxpar(hdr,'RA')
     frinfo[i].dec=sxpar(hdr,'DEC')
     frinfo[i].roll=sxpar(hdr,'ROLL')
     frinfo[i].tam_x1=sxpar(hdr,'TAM_X1')
     frinfo[i].tam_y1=sxpar(hdr,'TAM_Y1')
     frinfo[i].tam_x2=sxpar(hdr,'TAM_X2')
     frinfo[i].tam_y2=sxpar(hdr,'TAM_y2')
     ldppos=strpos(file,'LDP')
     frpos=strpos(file,'.frame')
     frinfo[i].ldp=strmid(file,ldppos+3,frpos-ldppos-3)
     frinfo[i].frame_no=sxpar(hdr,'CCD_FRAM')
     
     
  endfor 
  
  return
end 
