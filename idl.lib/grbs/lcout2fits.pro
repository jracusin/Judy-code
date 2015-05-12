function lcout2fits,file,phil=phil,silent=silent,qdp=qdp,empty=empty,pcfile=pcfile,wtfile=wtfile,uvot=uvot,withbat=withbat,dir=dir,chandra=chandra

  if n_elements(dir) eq 0 then dir='./'
  nowrite=0
  if not keyword_set(empty) then begin
     if n_elements(file) eq 0 then begin
        if keyword_set(withbat) then begin
           lc=mrdfits(dir+'/UL_lc_wbat.fits',1,silent=silent)
           if lc[0].time ne 0. then return,lc
        endif 
        if exist(dir+'/UL_lc.fits') then begin
           lc=mrdfits(dir+'/UL_lc.fits',1,silent=silent)
           if keyword_set(chandra) and exist(dir+'/UL_lc_chandra.fits') then begin 
              clc=mrdfits(dir+'/UL_lc_chandra.fits',1,silent=silent)
              concat_structs,lc,clc,lc2
              lc=lc2
           endif 
           if lc[0].time ne 0. then return,lc
        endif 
        phil=1
     endif else begin 
        nowrite=1
        if file eq 'lc_newout_phil.txt' then begin 
           if exist(dir+'/UL_lc.fits') then begin 
              lc=mrdfits(dir+'/UL_lc.fits',1,silent=silent)
              return,lc
           endif 
        endif 
     endelse 
  endif 
  if not keyword_set(silent) then print,'LCOUT2FITS'

;  readcol,file,time,t_start,t_stop,src_rate,src_rate_err,tot_hard,tot_hard_err,exptime,src_counts,back_ctrate,det_sig,psf_corr,junk,junk,rate1,rate2,rate3,rate1_err,rate2_err,rate3_err,hard1,hard2,hard1_err,hard2_err,src_rate,back_rate,back_area_corr,pu_corr,psf_corr,format='(d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d)',skip=1
  
  lc=create_struct('time',0d,$
                   'tstart',0d,$
                   'tstop',0d,$
                   'src_rate',0d,$
                   'src_rate_err',0d,$
                   'tot_hard',0d,$
                   'tot_hard_err',0d,$
                   'exptime',0d,$
                   'src_counts',0d,$
                   'back_ctrate',0d,$
                   'det_sig',0d,$
                   'psf_corr',0d,$
;                   'junk1',0d,$
                   'type',0d,$
;                   'rate1',0d,$
;                   'rate2',0d,$
;                   'rate3',0d,$
;                   'rate1_err',0d,$
;                   'rate2_err',0d,$
;                   'rate3_err',0d,$
;                   'hard1',0d,$
;                   'hard2',0d,$
;                   'hard1_err',0d,$
;                   'hard2_err',0d,$
;                   'src_rate2',0d,$
;                   'back_rate',0d,$
;                   'back_area_corr',0d,$
                   'pu_corr',0d,$
                   'psf_corr2',0d,$
;                   'junk3',0d,$
;                   'e1cts',0d,$
;                   'e2cts',0d,$
;                   'e3cts',0d,$
;                   'junk4',0d,$
;                   'junk5',0d,$
;                   'junk6',0d,$
                   'tot_back_cts',0d)
  lc2=lc
  if keyword_set(empty) then return,lc
  if not keyword_set(phil) and not keyword_set(qdp) then begin 
     if n_elements(file) eq 0 then file=dir+'lc_newout.txt'
     n=numlines(file)
     
     lc=replicate(lc,n-2)
     openr,lun,file,/get_lun
     line=readline(lun)
     line=readline(lun)
     for i=0,n-3 do begin
        line=readline(lun,delim=' ')
        w=where(line ne '')
        line=line[w]
        if n_elements(line) gt 24 and line[0] ne 'time' then begin 
           line=double(line)
           
           lc[i].time=line[0]
           lc[i].tstart=line[1]
           lc[i].tstop=line[2]
           lc[i].src_rate=line[3]
           lc[i].src_rate_err=line[4]
           lc[i].tot_hard=line[5]
           lc[i].tot_hard_err=line[6]
           lc[i].exptime=line[7]
           lc[i].src_counts=line[8]
           lc[i].back_ctrate=line[9]
           lc[i].det_sig=line[10]
           lc[i].psf_corr=line[11]
;           lc[i].junk1=line[12]
           lc[i].type=line[13]
;           lc[i].rate1=line[14]
;           lc[i].rate2=line[15]
;           lc[i].rate3=line[16]
;           lc[i].rate1_err=line[17]
;           lc[i].rate2_err=line[18]
;           lc[i].rate3_err=line[19]
;           lc[i].hard1=line[20]
;           lc[i].hard2=line[21]
;           lc[i].hard1_err=line[22]
;           lc[i].hard2_err=line[23]
;           lc[i].src_rate2=line[24]
;           lc[i].back_rate=line[25]
;           lc[i].back_area_corr=line[26]
           lc[i].pu_corr=line[27]
           lc[i].psf_corr2=line[28]
;           lc[i].junk3=line[29]
;           lc[i].e1cts=line[30]
;           lc[i].e2cts=line[31]
;           lc[i].e3cts=line[32]
;           lc[i].junk4=line[33]
;           lc[i].junk5=line[34]
           lc[i].tot_back_cts=line[35]
        endif 
     endfor 
     close,lun
     free_lun,lun
     
     w=where(lc.time ne 0)
     lc=lc[w]
     
  endif
  if keyword_set(phil) then $
     read_phil,lc=lc,pcfile=pcfile,wtfile=wtfile,withbat=withbat,dir=dir
  s=sort(lc.time)
  lc=lc[s]
  if keyword_set(qdp) then begin
     readcol,dir+file,time,tposerr,tnegerr,cts,err,/silent
     w=where(tposerr gt  5)

     lc=replicate(lc,n_elements(time))
     lc.time=time
     lc.tstart=time+tnegerr
     lc.tstop=time+tposerr
     lc.src_rate=cts
     lc.src_rate_err=err
     lc[w].type=1.
;     lc.exptime=exposure
;     lc.src_counts=cts_in_src-bg_in_src
;     lc.tot_back_cts=bg_in_src
;     lc.det_sig=sigma
;     lc.type=type
  endif 
  if keyword_set(uvot) then begin 
     readcol,dir+file,time,terr,cts,err,/silent
     lc=replicate(lc,n_elements(time))
     lc.time=time
     lc.tstart=time-terr
     lc.tstop=time+terr
     lc.src_rate=cts
     lc.src_rate_err=err
     lc.exptime=terr*2
     lc.src_counts=lc.src_rate*lc.exptime
     lc.tot_back_cts=0.1*lc.exptime
     lc.pu_corr=1.
     lc.type=4.
     w=where(lc.src_rate-lc.src_rate_err lt 1e-5)
     lc[w].src_rate_err=0
  endif 

  if not keyword_set(withbat) and not nowrite then mwrfits,lc,'UL_lc.fits',/create else $
     mwrfits,lc,'UL_lc_wbat.fits',/create

  return,lc
end
