pro lcout2xcm,file,outfile,filt=filt
  
  if n_elements(file) eq 0 then file='lc_out.txt'
  if not exist(file) then file='lc_newout.txt'
  if not exist(file) then begin
     print,'Need LC file'
     return
  endif 
  readcol,file,time,tstart,tstop,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct

  s=sort(time)
  time=time[s] & cts=cts[s] & tstart=tstart[s] & tstop=tstop[s] & err=err[s]
  if keyword_set(filt) then w=where(cts gt 0) else w=indgen(n_elements(cts))

  if n_elements(outfile) eq 0 then outfile='lc_out.dat'
  openw,lun,outfile,/get_lun
  for i=0,n_elements(time[w])-1 do printf,lun,tstart[w[i]],((time[w[i]]-tstart[w[i]])+(tstop[w[i]]-time[w[i]]))/2.,cts[w[i]],err[w[i]]
  close,lun
  free_lun,lun
  
  spawn,'rm xlc*'
  com='format2.pl -i lc_out.dat -o xlc.txt -r xlc clobber=yes'
  print,com
  spawn,com
  openw,lun,'xlc.xcm',/get_lun
  printf,lun,'data xlc.pha'
  printf,lun,'res xlc.rsp'
  printf,lun,'@xlcign'
  printf,lun,'cpd /xw'
  printf,lun,'setplot en'
;  printf,lun,'iplot ldata'
;  printf,lun,'label x "Seconds since BAT trigger"'
  printf,lun,'plot'
  close,lun
  free_lun,lun
;  spawn,'xspec xlc.xcm'
  
  return
end 
