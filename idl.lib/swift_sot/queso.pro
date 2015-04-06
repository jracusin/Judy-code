pro queso
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  QUESO - Quickly Update Every Segment Observation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  filename = pickfile(filter="*.csv")
  
  file=strsplit(filename,'.csv',/extract,/regex)
  file=file[0]
  
  posqueso=strpos(filename,'.queso')
  if posqueso gt -1 then begin 
     noqueso=strmid(filename,0,posqueso)
     noqueso=noqueso[0]
     ;;figure out how to incriment queso # in file name
     i=0L
     while exist(noqueso+'.queso.'+ntostr(i)+'.csv') do i=i+1L
     outfile=file+'.queso.'+ntostr(i)+'.csv'

  endif else outfile = file+'.queso.0.csv'
  
  print,outfile
  openr,inlun,filename,/get_lun
  openw,outlun,outfile,/get_lun
  
  a=' '
  readf,inlun,a                  ; read header
  printf,outlun,a
  
  while (not(eof(inlun))) do begin
     readf,inlun,a
     data = strsplit(a,',',/extract)
     data[3]=ntostr(data[3]+1)
     out=''
     for i=0,14 do out=out+data[i]+','
     printf,outlun,out
  endwhile

  close,/all
  
  return
end
