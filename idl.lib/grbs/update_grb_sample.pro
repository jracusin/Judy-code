pro weird_missing_stuff

  cd,'~/GRBs'
  g=mrdfits('~/Swift/swiftgrb_list.fits',1)
  grbs=file_search('GRB*')
  ngrbs=n_elements(grbs)
  for i=0,ngrbs-1 do begin
     if exist(grbs[i]) then begin
;        cd,strtrim(grbs[i],2)

        ;;; specific case of missing spectra, but have LC
        if exist(grbs[i]+'/PCCURVE.qdp') and not exist (grbs[i]+'/interval0pc_fit.fit') then begin
;           stop
           w=where(strtrim(g.grb,2) eq strtrim(grbs[i],2))
           print,grbs[i],'  ',g[w].trignum,w
;           if w[0] ne -1 then $
;              download_phil_spec,strtrim(g[w].grb,2),ntostr(g[w].trignum)            

        endif 
;        cd,'~/GRBs/'
     endif 
  endfor 
  return
end

pro duplicate_bursts
  
  g=mrdfits('~/Swift/swiftgrb_list.fits',1)
  grbs=file_search('GRB*')
  ngrbs=n_elements(grbs)

  for i=0,ngrbs-1 do begin
     w=where(grbs eq grbs[i]+'A',nw)
     q=where(strtrim(g.grb,2) eq strtrim(grbs[i],2))
     a=where(g.grb eq grbs[i]+'A')
     if nw gt 0 then begin 
        print,grbs[i],grbs[w]
        print,g[q].grb,' ',g[a].grb
        print,q,a
        print
     endif 
  endfor 


  return
end 

pro make_fits
  
  cd,'~/GRBs'
  grblist=file_search('GRB*')
  for i=0,n_elements(grblist)-1 do begin 
     cd,grblist[i]
     if not exist('UL_lc.fits') and (exist('PCCURVE.qdp') or exist('WTCURVE.qdp')) then begin 
        print,grblist[i]
        lc=lcout2fits(/phil)
        mwrfits,lc,'UL_lc.fits',/create
     endif 
     cd,'..'
  endfor 
end 

pro update_list

  ;;; first go to http://swift.gsfc.nasa.gov/archive/grb_table/
  ;;; click time, trig num, redshift, xrt ra, dec, err
  ;;; add updates to output file to ~/Swift/grb_table_13-14.txt or
  ;;; make new version and remove non-detections (n/a)
  ;;; run this program

  
  cd,'~/Swift'
  
  g0=mrdfits('~/Swift/swiftgrb.fits',1)  ;;; Davide's table through 2012
  w=where(g0.xrt_detection eq 'Y' or g0.xrt_detection eq 'P')
  g0=g0[w]
  n0=n_elements(g0)

  file='grb_table_13-14.txt'
  readcol,file,grb,trigtime,trignum,ra,dec,err,format='(a,a,a,a,a,f)'
  readcol,file,bla,bla,trig,bla,bla,bla,z,format='(a,a,a,a,a,f,a)'
  match,trignum,trig,m1,m2

  n=n_elements(grb)

  g=create_struct('grb','','trigtime','','met',0d,'trignum',0L,'xrt_ra',0.,'xrt_dec',0.,'xrt_err',0.,'z',0.)
  g=replicate(g,n+n0)

  g[0:n0-1].grb=g0.name
  g[0:n0-1].trigtime=strmid(g0.trigger_time,11,8)
  for i=0,n0-1 do g[i].met=date2met(g0[i].trigger_time)
  g[0:n0-1].trignum=g0.target_id
  g[0:n0-1].xrt_ra=g0.xrt_ra
  g[0:n0-1].xrt_dec=g0.xrt_dec
  g[0:n0-1].xrt_err=g0.xrt_pos_err
  g[0:n0-1].z=g0.redshift

  g[n0:*].grb='GRB'+grb
  g[n0:*].trigtime=trigtime
  yr=strmid(grb,0,2)
  mn=strmid(grb,2,2)
  day=strmid(grb,4,2)
  for i=0,n-1 do begin
     g[i+n0:*].met=date2met('20'+yr[i]+'-'+mn[i]+'-'+day[i]+'-'+trigtime[i])
     str_ra=str_sep(ra[i],':')     
     str_dec=str_sep(dec[i],':')
     hms2radec,str_ra[0],str_ra[1],str_ra[2],str_dec[0],str_dec[1],str_dec[2],radeg,decdeg
     g[i+n0:*].xrt_ra=radeg
     g[i+n0:*].xrt_dec=decdeg
     
  endfor
  g[n0:*].trignum=trignum
  g[n0+m1].z=z[m2]
  g[n0:*].xrt_err=err

  s=sort(g.grb)
  g=g[s]

  mwrfits,g,'~/Swift/swiftgrb_list.fits',/create

  print,'wrote out new ~/Swift/swiftgrb_list.fits'

return
end 

pro update_grb_sample,addgrbs=addgrbs,replace=replace

  g=mrdfits('~/Swift/swiftgrb_list.fits',1)
  cd,'~/GRBs'
  grblist=file_search('GRB*')
;;; ADD GRBS TO SAMPLE
;; run update_list

  if keyword_set(addgrbs) then begin 
     dont_match,strtrim(g.grb),strtrim(grblist,2),dm1,dm2
     ndm=n_elements(dm1)

     print,g[dm1].grb
stop
     for j=0,ndm-1 do begin 
        i=dm1[j]
        grb=strtrim(g[i].grb,2)
        if not exist(grb) then begin
           print,'No directory exists for '+grb+', making one'
           spawn,'mkdir '+grb
        endif 

        download_phil_lc,grb,ntostr(g[i].trignum)   ;,dir='~/GRBs/'
        download_phil_spec,grb,ntostr(g[i].trignum) ;,dir='~/GRBs/'
     endfor 
  endif 

;;; REPLACE OLD VERSIONS OR MISSING FILES WITH NEW ONES
  if keyword_set(replace) then begin
     match,strtrim(g.grb),strtrim(grblist,2),m1,m2
     ngrbs=n_elements(m1)

     for j=0,ngrbs-1 do begin
        cd,'~/GRBs/'
        grb=strtrim(g[m1[j]].grb,2)

        if exist(grb) then begin 
           cd,grb
           new=0

           if exist('PCCURVE.qdp') then begin 
              openr,lun,'PCCURVE.qdp',/get_lun
              chunks=readline(lun,skip=2)
              free_lun,lun
              if n_elements(chunks) eq 12 then new=1
           endif
           qdpfiles=file_search('*qdp')
;           if qdpfiles[0] ne '' and n_elements(qdpfiles) gt 0 and n_elements(qdpfiles) lt 4 then redo=1 else redo=0
           if new eq 0 then begin;or redo eq 1 then begin
              print,grb         ;,new,redo

              download_phil_lc,grb,ntostr(g[m1[j]].trignum) ;,dir='~/GRBs/'
;              download_phil_spec,grb,ntostr(g[m1[j]].trignum) ;,dir='~/GRBs/'
           endif 
        endif 
     endfor 
  endif 



  return
end 
