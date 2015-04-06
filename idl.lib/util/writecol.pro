function spacearr,nspace
  
  arr=''
  for i=0,nspace-1 do arr=arr+' '
  return,arr
end 

pro writecol,name,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
             v16,v17,v18,v19,v20,v21,v22,v23,v24,v25,header=header,$
             delimiter=delimiter,format=format
  
;+
; NAME:
;       WRITECOL
; PURPOSE:
;       Write out free-format ASCII file with columns of data from IDL vectors
;
; CALLING SEQUENCE:
;       WRITECOL,name,v1,[v2,v3,v4,v5, ... v25, delimiter= , header= ]
;
; INPUTS:
;       NAME - Name of file to output
;
; OPTIONAL INPUT KEYWORDS:
;       HEADER - string containing header to put at beginning of file
;
;       DELIMITER - ascii delimiter to add between columns
;
; PROCEDURES CALLED
;       
;
; REVISION HISTORY:
;       Written by J. Racusin July 24, 2006
;-
  
  
  if n_params() lt 2 then begin 
     print,'Syntax - writecol,name,v1,[v2,v3,v4,v5, ... v25, delimiter= , header= ]'
     return
  endif 
  
  if n_elements(delimiter) eq 0 then delimiter=' '
  openw,lun,name,/get_lun
  if n_elements(header) gt 0 then printf,lun,header
  nrow=n_elements(v1)
  ncol = N_params() - 1
  vv = 'v' + strtrim( indgen(ncol)+1, 2)
  
  varr=strarr(nrow)
  for c=0,ncol-1 do begin 
     tmp0=execute('len=strlen('+vv[c]+')')
     maxlen=max(len)
     tmp1=execute('for i=0,nrow-1 do varr[i]=varr[i]+ntostr('+vv[c]+'[i])+spacearr(maxlen-len[i])+delimiter')
     
  endfor 
  
  for i=0,nrow-1 do printf,lun,varr[i]
    
  close,lun
  free_lun,lun
  
  return
end 
