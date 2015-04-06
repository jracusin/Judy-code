pro merge_evt_files,file1,file2,outfile
  
;  nfiles=n_elements(filelist)
  
  fa0=mrdfits(file1,0,hdra0)
  fa1=mrdfits(file1,1,hdra1)
  fa2=mrdfits(file1,2,hdra2)
  fa3=mrdfits(file1,3,hdra3)
  
  fb0=mrdfits(file2,0,hdrb0)
  fb1=mrdfits(file2,1,hdrb1)
  fb2=mrdfits(file2,2,hdrb2)
  fb3=mrdfits(file2,3,hdrb3)
  
  concat_structs,fa1,fb1,events
  concat_structs,fa2,fb2,gti
  concat_structs,fa3,fb3,badpix
  
  hdr0=hdra0
  hdr1=hdra1
  hdr2=hdra2
  hdr3=hdra3
;  tstop=max(fa1.time)
;  sxaddpar,hdr0,'TSTOP',tstop
;  dateend=sxpar(hdrb0,'DATE-END')
;  sxaddpar,hdr0,'DATE-END',dateend
  ont1=sxpar(hdra0,'ONTIME')
  ont2=sxpar(hdrb0,'ONTIME')
  ontime=ntostr((ont1*1D)+ont2*1D)
  sxaddpar,hdr0,'ONTIME',ontime
  sxaddpar,hdr1,'ONTIME',ontime
  sxaddpar,hdr2,'ONTIME',ontime
  
  
  
;  stop
  
  
  mwrfits,0,outfile,hdr0,/create
  mwrfits,events,outfile,hdr1
  mwrfits,gti,outfile,hdr2
  mwrfits,badpix,outfile,hdr3
  
  return
end 
