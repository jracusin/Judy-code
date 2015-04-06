function rdfile,file_in,columns,lun=lun

count=1L
line=''

openr,lun, file_in,/get_lun ;opens file, determines number of lines

while not eof(lun) do begin
  readf,lun,line
  count=count+1
endwhile

  point_lun, lun, 0
  count=count-1
  array=dblarr(columns,count)
  readf,lun, array
  free_lun, lun
  
  return, array

end






