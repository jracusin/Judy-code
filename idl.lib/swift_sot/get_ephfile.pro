function get_ephfile,file
  
  if n_elements(file) eq 0 then file=pickfile(filter='PPST*.txt*')

  ypos=strpos(file,'PPST_')
  if ypos eq -1 then ypos=strpos(file,'AFST_')
  year=strmid(file,ypos+5,4)
  day=strmid(file,ypos+9,3)

  ephdir=''
  tmpeph=ephdir+'STK_EPH_*'
  if not exist(tmpeph) then begin 
     ephdir='/home/swift/stkproducts/'
     tmpeph=ephdir+'STK_EPH_'+year+day+'*'
     while not exist(tmpeph) do begin
        day=ntostr(day-1)
        if day lt 100 then day='0'+day
        if day lt 10 then day='0'+day
        tmpeph=ephdir+'STK_EPH_'+year+day+'*'
     endwhile
  endif

  ephfile=findfile(tmpeph)
  neph=n_elements(ephfile)
  ephfile=ephfile[neph-1]
  
  epos=strpos(ephfile,'STK_EPH')
  ephdate1=strmid(ephfile,epos+8,7)
  ephdate2=strmid(ephfile,epos+16,7)
  date=year+day
  if date lt ephdate1 or date gt ephdate2 then begin
     ephdir='/home/swift/stkproducts/'
     tmpeph=ephdir+'STK_EPH_'+year+day+'*'
     while not exist(tmpeph) do begin
        day=ntostr(day-1)
        if day lt 100 then day='0'+day
        if day lt 10 then day='0'+day
        tmpeph=ephdir+'STK_EPH_'+year+day+'*'
     endwhile

     ephfile=findfile(tmpeph)
     neph=n_elements(ephfile)
     ephfile=ephfile[neph-1]
     
     epos=strpos(ephfile,'STK_EPH')
     ephdate1=strmid(ephfile,epos+8,7)
     ephdate2=strmid(ephfile,epos+16,7)
     date=year+day
     if date lt ephdate1 or date gt ephdate2 then begin
        print,'No valid ephemeris available'
        return,0
     endif 
  endif 

  return,ephfile
end 
