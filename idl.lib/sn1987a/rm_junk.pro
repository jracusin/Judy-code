pro rm_string, fname,string

;This program search the file contained in fname
;and removes any line which begins with the give 
;string (leading white space removed).

line=''
c=strlen(string) ;determines the number if characters

openr, lun, fname, /get_lun
openw, lun2,fname+'_clean'

N=0

while not eof(lun) do begin
  readf,lun,line
  a=strtrim(line,1)
  b=strmid(a,0,c)
  if b eq. string then N=N+1
  else print, lun2,line
endwhile

free_lun, lun
free_lun, lun2

end