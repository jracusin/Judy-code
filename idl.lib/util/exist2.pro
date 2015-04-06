function exist2,filename,count
;+
; NAME:
;   EXIST
; PURPOSE:
;   A very simple check to see if a file exists...
; CALLING SEQEUNCE:
;   tmp = Exist('STARS.DAT')
; INPUT:
;   FILENAME  This is the filename or search spec. that should be checked.
; OUTPUT:
;   TMP       The returned result is 1 is the file or files exist and 0 if
;               the file of files do(es) not exist.
; OPTIONAL OUTPUT:
;   COUNT     Number of occurances of the given search spec.
; EXAMPLE:
;   if exist('tmp.tmp') then print,'Yes' else print,'No'
;   if not exist('tmp.tmp') then print,'Create'
;   if exist('*.hhh',count) then print,strn(count),' Header files available'
; HISTORY:
;   22-JUN-2005 created exist2 to use file_search rather than findfile (JLR)
;   27-JUL-1992 Header added to old routine  (E. Deutsch)
;		
;-	

a=file_search(filename,count=count)	

  return,count<1

end
