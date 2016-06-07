pro reread_files

  cd,'~/GRBs/'
  grb=file_search('GRB*')
  ngrbs=n_elements(grb)
  for i=0,ngrbs-1 do begin
     if exist(grb[i]+'/interval0pc_fit.fit') then begin 
        download_phil_spec,grb[i]
     endif 
  endfor 

  return
end 

pro download_phil_spec,grb,tid,dir=dir
  
  if n_params() eq 0 then begin
     print,'syntax - download_phil_spec,grb,tid,dir=dir'
     print,"         (e.g. grb='GRB080123')"
     return
  endif 

;; readcol,'~/jetbreaks/grb_tid_z_table.csv',grb,tid,format='(a,l)'
;; download_phil_spec,'GRB'+grb,ntostr(tid)  

  spec0=create_struct('mode','','nhgal',0d,'nh',0d,'nherr',dblarr(2),'z',0.,$
                      'phind',0.,'phinderr',fltarr(2),$
                      'cfratio',0d,'cfratioerr',dblarr(2),$
                      'unabs_cfratio',0d,'unabs_cfratioerr',dblarr(2))

  g=0
;  cd,!grbs
;  colprint,dir,indgen(n_elements(dir))
;  stop
  if n_elements(dir) eq 0 then cd,'~/GRBs/' else cd,dir
  for i=g,n_elements(grb)-1 do begin 

     if n_elements(dir) eq 0 then cd,grb[i]
     wt=0 & pc=0 & pc2=0
     if not exist('interval0wt_fit.fit') then wt=1
     if not exist('interval0pc_fit.fit') then pc=1
     if not exist('late_timepc_fit.fit') then pc2=1
;     if wt and pc and pc2 then begin
;        spec=replicate(spec0,wt+pc+pc2)
;        spec[0].mode='WT'
;        spec[1].mode='PC'
;        if pc2 then spec[2].mode='PC late'
;        j=1
;     endif else begin
;        j=0
;        spec=spec0
;     endelse 
     spec=replicate(spec0,3)
;     if pc and not wt then spec.mode='PC'
;     if wt and not pc then spec.mode='WT'
     if wt then begin 
        file='interval0wt_fit.fit'
        if not exist(file) then begin 
           if tid[i]*1d lt 100000 then tid2='000'+tid[i] else tid2='00'+tid[i]
           print,grb[i],'  ',tid[i],i

           url='http://www.swift.ac.uk/xrt_spectra/'+tid2+'/interval0wt_fit.fit'
           com='wget '+url
;           url='http://www.swift.ac.uk/team/xrt_spectra/'+tid2+'/interval0wt_fit.fit'
;           com='wget --http-user=team --http-passwd=grb05 '+url
           print,com
           spawn,com
        endif 
        if exist(file) then begin 
           if numlines(file) ge 9 then begin
              j=0
              spec[j].mode='WT'
              readcol,file,pname,p,perr0,perr1,format='(a,f,f,f)',delim=' (',/silent
              spec[j].nhgal=p[0]
              spec[j].nh=p[1]*1d22
              spec[j].nherr=[p[1]-perr0[1],perr1[1]-p[1]]*1d22
              if numlines(file) ge 10 then begin
                 spec[j].z=p[2]
                 a=1
              endif else a=0
              spec[j].phind=p[a+2]
              spec[j].phinderr=[p[a+2]-perr0[a+2],perr1[a+2]-p[a+2]]
              readcol,'interval0wt_fit.fit',pname1,pname2,p,unit1,unit2,unit3,format='(a,a,f,a,a,a)',delim=' (',/silent
              obsflux=p[0]
              unabsflux=p[1]
              obsfluxerr=[obsflux-unit1[0],unit2[0]-obsflux]
              unabsfluxerr=[unabsflux-unit1[1],unit2[1]-unabsflux]
              readcol,'interval0wt_fit.fit',pname1,p,format='(a,f)',delim=':',/silent
              rate=p[0]
              corr=p[1]
              spec[j].cfratio=obsflux/rate/corr
              spec[j].unabs_cfratio=unabsflux/rate/corr
              spec[j].cfratioerr=obsfluxerr/rate/corr
              spec[j].unabs_cfratioerr=unabsfluxerr/rate/corr
              
           endif ;else begin
;              spec=spec0
;              j=0
;              spec[j].mode='PC'
;           endelse 
        endif 
     endif 
;     if pc then begin 
        file='interval0pc_fit.fit'
        if not exist(file) then begin 
           if tid[i]*1d lt 100000 then tid2='000'+tid[i] else tid2='00'+tid[i]
           print,grb[i],'  ',tid[i],i

;           url='http://www.swift.ac.uk/team/xrt_spectra/'+tid2+'/interval0pc_fit.fit'
;           com='wget --http-user=team --http-passwd=grb05 '+url
           url='http://www.swift.ac.uk/xrt_spectra/'+tid2+'/interval0pc_fit.fit'
           com='wget '+url
           print,com
           spawn,com
        endif 
        if exist(file) then begin 
           if numlines(file) gt 2 then begin 
              j=1
              spec[j].mode='PC'
              readcol,file,pname,p,perr0,perr1,format='(a,f,f,f)',delim=' (',/silent
              spec[j].nhgal=p[0]
              spec[j].nh=p[1]*1d22
              spec[j].nherr=[p[1]-perr0[1],perr1[1]-p[1]]*1d22
              if numlines(file) ge 10 then begin
                 spec[j].z=p[2]
                 a=1
              endif else a=0
              spec[j].phind=p[a+2]
              spec[j].phinderr=[p[a+2]-perr0[a+2],perr1[a+2]-p[a+2]]
              readcol,'interval0pc_fit.fit',pname1,pname2,p,unit1,unit2,unit3,format='(a,a,f,a,a,a)',delim=' (',/silent
              obsflux=p[0]
              unabsflux=p[1]
              obsfluxerr=[obsflux-unit1[0],unit2[0]-obsflux]
              unabsfluxerr=[unabsflux-unit1[1],unit2[1]-unabsflux]
              readcol,'interval0pc_fit.fit',pname1,p,format='(a,f)',delim=':',/silent
              rate=p[0]
              corr=p[1]
              spec[j].cfratio=obsflux/rate/corr
              spec[j].unabs_cfratio=unabsflux/rate/corr
              spec[j].cfratioerr=obsfluxerr/rate/corr
              spec[j].unabs_cfratioerr=unabsfluxerr/rate/corr
           endif 
        endif 
;     endif 
     file='late_timepc_fit.fit'
     if pc2 then begin 
        if not exist(file) then begin 
           if tid[i]*1d lt 100000 then tid2='000'+tid[i] else tid2='00'+tid[i]
           print,grb[i],'  ',tid[i],i
        endif               
        url='http://www.swift.ac.uk/xrt_spectra/'+tid2+'/'+file
        com='wget '+url
        print,com
        spawn,com
     endif  

     if exist(file) then begin 
        if numlines(file) gt 2 then begin 
           j=2
           spec[j].mode='PC late'
           readcol,file,pname,p,perr0,perr1,format='(a,f,f,f)',delim=' (',/silent
           spec[j].nhgal=p[0]
           spec[j].nh=p[1]*1d22
           spec[j].nherr=[p[1]-perr0[1],perr1[1]-p[1]]*1d22
           if numlines(file) ge 10 then begin
              spec[j].z=p[2]
              a=1
           endif else a=0
           spec[j].phind=p[a+2]
           spec[j].phinderr=[p[a+2]-perr0[a+2],perr1[a+2]-p[a+2]]
           readcol,file,pname1,pname2,p,unit1,unit2,unit3,format='(a,a,f,a,a,a)',delim=' (',/silent
           obsflux=p[0]
           unabsflux=p[1]
           obsfluxerr=[obsflux-unit1[0],unit2[0]-obsflux]
           unabsfluxerr=[unabsflux-unit1[1],unit2[1]-unabsflux]
           readcol,file,pname1,p,format='(a,f)',delim=':',/silent
           rate=p[0]
           corr=p[1]
           spec[j].cfratio=obsflux/rate/corr
           spec[j].unabs_cfratio=unabsflux/rate/corr
           spec[j].cfratioerr=obsfluxerr/rate/corr
           spec[j].unabs_cfratioerr=unabsfluxerr/rate/corr
        endif 
     endif 

     w=where(spec.cfratio ne 0,nw)
     if nw gt 0 then begin 
        spec=spec[w]
        mwrfits,spec,'UL_specfits.fits',/create
     endif 
     if n_elements(dir) eq 0 then cd,'..'
  endfor

  return
end 
