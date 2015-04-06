;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    ID2STRING
;       
; PURPOSE: 
;    Function outputs a string containing id of a photo object
;    in the correct format.
;	
;
; CALLING SEQUENCE: 
;    result = id2string(id)
;      
; INPUTS: 
;    id in integer form (may be an array)
;       
; OUTPUTS: 
;    id string
; 
; PROCEDURE: 
;    id should have 5 characters padded with zero's if its
;    not big enough.  e.g. if input 33, output is '00033'
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  UChicago 25-Feb-2004
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION id2string, id

  if N_params() eq 0 then begin
     print,'-Syntax: result = id2string(id)'
     print,''
     print,'Use doc_library,"id2string"  for more help.'  
     return,''
  endif

  on_error,2


  len = 5

  nid = n_elements(id) 
  IF nid EQ 1 THEN BEGIN 
      return,strn(long(id),len=len,padchar='0')
  ENDIF ELSE BEGIN 
      retarr = strarr(nid)
      FOR i=0L, nid-1 DO retarr[i] = strn(long(id[i]),len=len,padchar='0')
      return,retarr
  ENDELSE 

END 
