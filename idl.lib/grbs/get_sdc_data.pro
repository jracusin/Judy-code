pro get_sdc_data,year,month,targetid,nsegment=nsegment,startseg=startseg
  
  if n_params() eq 0 then begin
     print,'syntax - get_sdc_data,year,month,targetid'
     return
  endif 
  
  month0=month
  if month lt 10 then m='0'+ntostr(month) else m=ntostr(month)
  y=ntostr(year)
  
  if targetid lt 100000 then tid='0'+ntostr(targetid) else tid=ntostr(targetid)
  
  if n_elements(startseg) gt 0 then i=startseg else i=0
  try=0
  if n_elements(nsegment) eq 0 then nseg=i+4 else nseg=nsegment
  while i lt nseg do begin 
     if i lt 10 then seg='00'+ntostr(i) else seg='0'+ntostr(i)
     
     dir='00'+tid+seg
     print,dir
     com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/"+y+'_'+m+'//00'+tid+seg+'/auxil/'
     ;http://swift.gsfc.nasa.gov/SDC/data/local/data1/data/sw00240801000.009/auxil/
     print,com
     if not exist(dir+'/auxil') then spawn,com
     
     com="wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/"+y+'_'+m+'//00'+tid+seg+'/xrt/'
     print,com
     if not exist(dir+'/xrt') then spawn,com
     
     if exist(dir) then try=0
     
     if try eq 1 then begin
        i=i+1
        if month gt month0 then month=month-1
        
        if month lt 10 then m='0'+ntostr(month) else m=ntostr(month)
        if exist(dir) then try=0
     endif 
     
     if not exist(dir) and try eq 0 then begin 
        print,'Directory does not exist in '+y+'_'+m
        month=month+1
        if month eq 13 then begin
           year=year+1
           y=ntostr(year)
           month=1
        endif 
        if month lt 10 then m='0'+ntostr(month) else m=ntostr(month)
        print,'    trying '+y+'_'+m
;        i=i-1
        try=try+1
     endif

     if try eq 0 then i=i+1

     if exist(dir) then try=0
     print,seg,nseg
     if n_elements(nsegment) eq 0 then $
        if exist(dir) then nseg=nseg+1
     
  endwhile

  return
end 
  
;  wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/2006_08//00223217007/auxil/
;wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/2006_08//00223217007/xrt/

;wget -q -nH --cut-dirs=5 -r -l0 -c -N -np -R 'index*' -erobots=off --retr-symlinks http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/2005_02//00106107010/auxil/
