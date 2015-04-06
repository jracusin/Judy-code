pro run_sally,file
  
  readcol,file,targname,type,targid,seg,fom,ra,dec,rol,bat,xrt,uvot,dur,com,pri,ssmin,ssmax,skip=1,delim=',',format='(a,a,l,i,i,d,d,d,a,a,a,d,a,i,l,l)'
  
  openw,lun,'targets.txt',/get_lun
  
  for i=0,n_elements(ra)-1 do printf,lun,ntostr(ra[i])+' '+ntostr(dec[i])
  
  close,lun,/file
  
  spawn,'cp ~racusin/bin/cntrate.tbl .'
  spawn,'chkbrt4'
  spawn,'chkdist2'
  spawn,'chkdist4'
  
  return
end 
