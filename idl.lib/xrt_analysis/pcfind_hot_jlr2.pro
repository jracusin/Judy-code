pro pcfind_hot
  
  list=file_search('*LDP*.frame*.pc.events.fits')
  nlist=n_elements(list)
  if nlist gt 2000 then begin
     n=nlist/2000
     list=list[indgen(2000)*n]
     nlist=2000L
  endif 
  
  print,list
  ldps=strarr(nlist)
  
  for n=0,nlist-1 do begin
     flds=strsplit(list(n),'.',/extract)
     ldps(n)=strmid(flds(1),5,strlen(flds(1))-5)
  endfor
  dat_ctr=0L

;  evt_dat=dblarr(nlist*1000.,8)
  for m=0L,nlist-1 do begin
     flds=strsplit(list(m),'.',/extract)
     frame=strmid(flds(2),5,strlen(flds(2))-5)
     dat0=mrdfits(list(m),0,hd0,/silent)
     tccd=sxpar(hd0,'T_CCD')
     if tccd le -50 then begin 
        time=sxpar(hd0,'MET_STRT')
        dat1=mrdfits(list(m),1,hd1,/silent)
        nd=n_elements(dat1)
        if nd gt 1 then begin 
           wth=where(dat1.phas[0] ge 100,nd)
           if nd gt 1 then begin 
              dat1=dat1[wth]
;           if nd gt 1 then begin 
              if m eq 0 or n_elements(evt_dat) eq 0 then begin
                 ndat1=n_elements(dat1)
                 if ndat1 lt 1000 then ndat1=1000L
                 if ndat1 gt 10000 then ndat1=10000L
                 evt_dat=dblarr(nlist*ndat1,8)
              endif 
              
              evt_dat[dat_ctr:dat_ctr+nd-1,*]=[[dat1.detx],[dat1.dety],[dat1.phas[0]],$
                                               [replicate(tccd,nd)],$
                                               [replicate(time,nd)],$
                                               [replicate(frame,nd)],$
                                               [replicate(ldps[m],nd)],[dat1.phas[0]]]
              dat_ctr=dat_ctr+nd
;           for i=0L,n_elements(dat1)-1 do begin
;              detx=dat1(i).detx
;              dety=dat1(i).dety
;              pha=dat1(i).phas(0)
;              evt_dat(dat_ctr,*)=[detx,dety,pha,tccd,time,frame,ldps(m),pha]
;              dat_ctr=dat_ctr+1L
;           endfor
;           endif 
           endif 
        endif
     endif 
  endfor
  if n_elements(evt_dat) gt 1 then evt_dat=evt_dat(0:dat_ctr-1,*) else evt_dat=0
  
  w0=where(evt_dat ne 0,nw0)
  if nw0 eq 0 then begin
     openw,lun,'hotpix_none.txt',/get_lun
     close,lun
     free_lun,lun
     return
  endif 
  
  fout_name='hotpix_out.str'
  used_pos=''
  start_flag=1
  ext_ct=0.d
  tmpstr=create_struct('detx',0,'dety',0,'ldp',0L,'frame',0L,'time',0D,'pha',0,'tccd',0.)
  
  nwrite=0L
  i_ctr=0.
  nomatch_list=where(evt_dat(*,0) ne -999)
  while nomatch_list(0) ne -1 do begin
     evt_dat=evt_dat(nomatch_list,*)
     nomatch_list=where(evt_dat(*,0) ne evt_dat(0,0) or evt_dat(*,1) ne evt_dat(0,1),nml) 
     i=evt_dat(i_ctr,0)
     j=evt_dat(i_ctr,1)
;    print,i,j,n_elements(nomatch_list)
     printed_flag=0.
     pass_ind=where(evt_dat(*,0) eq i and evt_dat(*,1) eq j,npass_ind)
     percent=0.1
     if min(evt_dat[pass_ind,3]) gt -55 then percent=0.3
     if ((npass_ind gt nlist*percent) and nlist gt 10 and printed_flag eq 0.) then begin
        print,i,j,nml
        printed_flag=1.
        ind=where(evt_dat(pass_ind,2) ne 0.)
        frames_ind=pass_ind;where(evt_dat(*,0) eq i and evt_dat(*,1) eq j)
        ldps_list=evt_dat(frames_ind,6)
        frames=evt_dat(frames_ind,5)
        times=evt_dat(frames_ind,4)
        phas=evt_dat(frames_ind,2)
        tccds=evt_dat(frames_ind,3)
        outstr=tmpstr
        outstr=replicate(outstr,n_elements(phas))
        outstr.detx=fix(i)
        outstr.dety=fix(j)
        outstr.ldp=long(ldps_list)
        outstr.frame=long(frames)
        outstr.time=times
        outstr.pha=fix(phas)
        outstr.tccd=float(tccds)
;        write_test=where(used_pos eq i*1000.+j)
        write_test=where(used_pos eq i*ndat1+j)
        if write_test(0) eq -1 then begin
           if start_flag eq 1 then begin
              mwrfits,outstr,'hotpix_'+strtrim(string(fix(i)),2)+'_'+strtrim(string(fix(j)),2)+'.fits',/create
              ext_ct=ext_ct+1.d
           endif
           if start_flag eq 0 then begin
              mwrfits,outstr,fout_name
              ext_ct=ext_ct+1.d
           endif
           nwrite=nwrite+1
        endif
;        used_pos=[used_pos,i*1000.+j]
        used_pos=[used_pos,i*ndat1+j]
     endif
  endwhile
  if nwrite eq 0 then begin
     openw,lun,'hotpix_none.txt',/get_lun
     close,lun
     free_lun,lun
  endif 
  
  return
end
