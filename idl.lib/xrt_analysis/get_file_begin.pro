pro get_file_begin,filebegin,startdir

; run in data directory that you want analyzed
; part of automated analysis

;  dir=file_which('',/include_current_dir)
  dir=file_expand_path('./')+'/'
  
  startdir=dir
  
  s=strpos(dir,'xrt_200')
  if s eq -1 then s=strpos(dir,'pass_200')
  e=strpos(dir,'/',/reverse_search)
  
  filebegin=strmid(dir,s,e-s)

;	filebegin=strmid(dir,39,15)

   return
end
