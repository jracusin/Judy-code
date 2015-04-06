pro sample_stats
  
  cd,!mdata
  
;  file='~/jetbreaks/grb_zs_070608.csv'
;  file=!mdata+'grb_zs_table.csv'
;  readcol,file,zgrb,ztid,zz,format='(a,a,f)',delim=','
;  read_butler_eiso,zgrb,eiso,eisoerr,zz
  grbstr=mrdfits(!mdata+'grb_info_z_epeak.fits',1)
  zgrb=strtrim(grbstr.grb,2)
  zz=grbstr.z
  dir=file_search('GRB*')
  n=n_elements(dir)

  tid=strarr(n)
  z=fltarr(n)
  nfit=intarr(n) & nspec=nfit
  f1=0 & f2=0 & f3=0 & f4=0
  for i=0,n-1 do begin
     cd,dir[i]
     ev=file_search('*/sw*xpc*po*cl.evt')
     ev=ev[0]
     pos=strpos(ev,'sw')
     tid[i]=strmid(ev,pos+4,6)
     file='lc_newout_phil.txt'
     if exist(file) then begin 
        lc=lcout2fits(file,/phil)
        wdet=where(lc.src_rate_err gt 0,nwdet)
        if nwdet gt 2 then f1=[f1,i]
     endif else nwdet=0
;     if exist(file) then f1=[f1,i]
     file='lc_fit_out_idl_int8.dat'
     if exist(file) and nwdet gt 2 then $
        if numlines(file) gt 3 then f2=[f2,i]
     file='spec/seg1.dat'
     if exist(file) and nwdet gt 2 then begin
        if numlines(file) gt 6 then f3=[f3,i] else begin 
           file='spec/seg2.dat'
           if exist(file) then begin 
              if numlines(file) gt 6 then f3=[f3,i]
           endif else if exist('closure_relations_3sig.fits') then stop
        endelse 
     endif
     file='closure_relations_3sig.fits'
     if exist(file) then f4=[f4,i]
     w=where('GRB'+zgrb eq dir[i],nw)
     if nw gt 0 then z[i]=zz[w]     
     
     
     cd,'..'
     
  endfor 
  f1=f1[1:*] & f2=f2[1:*] & f3=f3[1:*] & f4=f4[1:*]
  print,'                           total      non-Swift  with z'
  
  tmp=where(tid lt 100000,n0)
  ztmp=where(z gt 0,z0)
  print,'Ndir:              ',n,n0,z0
  
;  files=file_search('GRB*/lc_newout.txt')
;  n=n_elements(files)
  n=n_elements(f1)
  tmp=where(tid[f1] lt 100000,n1)
  ztmp=where(z[f1] gt 0,z1)
  print,'GRBs with LCs:     ',n,n1,z1
  
;  files=file_search('GRB*/lc_fit_out_idl_int.dat')
;  n=n_elements(files)
  n=n_elements(f2)
  tmp=where(tid[f2] lt 100000,n2)
  ztmp=where(z[f2] gt 0,z2)
  print,'GRBs with LC fits: ',n,n2,z2
  
;  files=file_search('GRB*/spec/seg1.dat')
;  n=n_elements(files)
  n=n_elements(f3)
  tmp=where(tid[f3] lt 100000,n3)
  ztmp=where(z[f3] gt 0,z3)
  print,'GRBs with spec:    ',n,n3,z3
  
  n=n_elements(f4)
  tmp=where(tid[f4] lt 100000,n4)
  ztmp=where(z[f4] gt 0,z4)
  print,'GRBs with CR fits: ',n,n4,z4
  
  
  
  stop
  
  
  return
end 
