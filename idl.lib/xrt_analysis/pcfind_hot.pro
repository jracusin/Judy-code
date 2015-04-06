pro pcfind_hot
  
  list=file_search('*pc.events.fits')

  print,list
  ldps=strarr(n_elements(list))
  for n=0,n_elements(list)-1 do begin
     flds=strsplit(list(n),'.',/extract)
     ldps(n)=strmid(flds(1),5,strlen(flds(1))-5)
  endfor
  ldps_u=uniq(ldps)
  ldps=ldps(ldps_u)

;  openw,lunout,'hotpixout.txt',/get_lun
;  begplot,name='hotpixout.ps'
;  !p.multi=[0,2,4]
  dat_ctr=0.d

  evt_dat=dblarr(n_elements(list)*1000.,8)
  for n=0,n_elements(ldps)-1 do begin
     print,'LDP ',ldps(n)
     list=findfile('*LDP'+ldps(n)+'.frame*.pc.events.fits')
;;    evts=fltarr(631,602,n_elements(list),2)

;;    evt_dat=dblarr(n_elements(list)*1000.,8)
;    evts2=fltarr(631,602,n_elements(list),2)
;    dat_ctr=0.d
     for m=0,n_elements(list)-1 do begin
        flds=strsplit(list(m),'.',/extract)
        frame=strmid(flds(2),5,strlen(flds(2))-5)
        dat0=mrdfits(list(m),0,hd0,/silent)
        tccd=sxpar(hd0,'T_CCD')
        time=sxpar(hd0,'MET_STRT')
        dat1=mrdfits(list(m),1,hd1,/silent)
        for i=0,n_elements(dat1)-1 do begin
           detx=dat1(i).detx
           dety=dat1(i).dety
           pha=dat1(i).phas(0)
;;            evts(detx,dety,m,0)=pha
;;            evts(detx,dety,m,1)=ldps(n)
;;            print,dat_ctr,detx,dety,pha,tccd,time,frame,ldps(n),pha
           evt_dat(dat_ctr,*)=[detx,dety,pha,tccd,time,frame,ldps(n),pha]
           dat_ctr=dat_ctr+1.d
;            evts1(detx,dety,m,1)=tccd
;            evts2(detx,dety,m,0)=time
;            evts2(detx,dety,m,1)=frame
        endfor
     endfor
;;evt_dat=evt_dat(0:dat_ctr-1,*)
  endfor
  evt_dat=evt_dat(0:dat_ctr-1,*)
;evt_ind=where(evt_dat(*,4) ne 0.)
;evt_dat=evt_dat(0:dat_ctr-1,*)

;;for n=0,n_elements(ldps)-1 do begin
;;    for i=0,630 do begin
;;        for j=0,601 do begin

  fout_name='hotpix_out.str'
  used_pos=''
  start_flag=1
  ext_ct=0.d
;for i_ctr=0,n_elements(evt_dat(*,0))-1 do begin
  for i_ctr=0,500 do begin
     i=evt_dat(i_ctr,0)
     j=evt_dat(i_ctr,1)
;    used_pos=[used_pos,i*1000.+j]
     print,i,j,i_ctr
     printed_flag=0.
     for n=0,n_elements(ldps)-1 do begin
        ldp_ind=where(evt_dat(*,0) eq i and evt_dat(*,1) eq j and evt_dat(*,6) eq ldps(n),nldp_ind)
;;           ldp_ind=where(evts(i,j,*,1) eq ldps(n))
        list=findfile('*LDP'+strtrim(string(ldps(n)),2)+'.frame*.pc.events.fits')
        if ((nldp_ind gt n_elements(list)*0.1) and printed_flag eq 0.) then begin
           printed_flag=1.
;;            if n_elements(where(evts(i,j,ldp_ind,0) gt 200.)) gt n_elements(list)*0.5 then begin
;                outstr=create_struct('detx',i,'dety',j,
;                print,'hotpixel found in LDP '+ldps(n)+' at detx='+string(i)+' dety='+string(j)
;                printf,lunout,'hotpixel found in LDP '+ldps(n)+' at
;                detx='+string(i)+' dety='+string(j)
           ind=where(evt_dat(ldp_ind,2) ne 0.)
;;                ind=where(evts(i,j,ldp_ind,0) ne 0.)
;                plot,evts(i,j,ldp_ind(ind),1),evts1(i,j,ldp_ind(ind),0),psym=1,ytitle='pha',xtitle='MET'
           frames_ind=where(evt_dat(*,0) eq i and evt_dat(*,1) eq j)
           ldps_list=evt_dat(frames_ind,6)
           frames=evt_dat(frames_ind,5)
           times=evt_dat(frames_ind,4)
           phas=evt_dat(frames_ind,2)
           tccds=evt_dat(frames_ind,3)
;;;                outstr=create_struct('detx',fix(i),'dety',fix(j),'ldp',long(ldps_list),'frame',long(frames),'time',times,'pha',fix(phas),'tccd',float(tccds))
           outstr=create_struct('detx',0,'dety',0,'ldp',0L,'frame',0L,'time',0D,'pha',0,'tccd',0.)
           outstr=replicate(outstr,n_elements(phas))
           outstr.detx=fix(i)
           outstr.dety=fix(j)
           outstr.ldp=long(ldps_list)
           outstr.frame=long(frames)
           outstr.time=times
           outstr.pha=fix(phas)
           outstr.tccd=float(tccds)
           write_test=where(used_pos eq i*1000.+j)
           if write_test(0) eq -1 then begin
              if start_flag eq 1 then begin
;                        mwrfits,outstr,fout_name,/create
                 mwrfits,outstr,'hotpix_'+strtrim(string(fix(i)),2)+'_'+strtrim(string(fix(j)),2)+'.fits',/create
;;;                        start_flag=0
                 ext_ct=ext_ct+1.d
;                        stop
              endif
              if start_flag eq 0 then begin
                 mwrfits,outstr,fout_name
                 ext_ct=ext_ct+1.d
;                        stop
              endif
           endif
           used_pos=[used_pos,i*1000.+j]
;                times=evts(i,j,m,2)
;                phas=evts(i,j,m,0)
        endif
     endfor

  endfor
;  endplot
;  !p.multi=0
;;    endfor
;;endfor
;;;spawn,'mv hotpix_out.str hotpix_out_'+strtrim(string(fix(ext_ct)),2)+'.str'
  return
end
