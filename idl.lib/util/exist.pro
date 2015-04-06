function exist,filename,count

;name   :exist
;author :Judy Racusin
;date   :05/27/04
;land   :IDL

;purpose: checks if a file exists and returns 1 if exists, 0 if does not exist


file=findfile(filename,count=count)

   return,count < 1

end 
