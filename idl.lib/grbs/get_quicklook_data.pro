pro get_quicklook_data,targetid,segment,version
  
  if n_params() eq 0 then begin
     print,'syntax - get_quicklook_data,targetid,segment,version'
     return
  endif 
  
  if targetid lt 100000 then tid='0'+ntostr(targetid) else tid=ntostr(targetid)
  if segment lt 100 then seg='0'+ntostr(segment) else seg=ntostr(segment)
  if segment lt 10 then seg='0'+seg
  if version lt 10 then ver='00'+ntostr(version) else ver='0'+ntostr(version)

  dir='00'+tid+seg
  sequence=dir+'.'+ver
  print,sequence
;  print,dir
;  com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/"+y+'_'+m+'//00'+tid+seg+'/auxil/'
  com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://swift.gsfc.nasa.gov/SDC/data/local/data1/data/sw"+sequence+'/auxil/'
  print,com
  spawn,com
     
;  com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/"+y+'_'+m+'//00'+tid+seg+'/xrt/'
  com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://swift.gsfc.nasa.gov/SDC/data/local/data1/data/sw"+sequence+'/xrt/'

  print,com
  spawn,com
     
  if not exist(dir) then begin 
      spawn,'mkdir '+dir
      spawn,'mv sw'+sequence+'/* '+dir
      spawn,'rmdir sw'+sequence
  endif 

  return
end
