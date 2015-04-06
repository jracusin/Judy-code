FUNCTION field2string, field

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    FIELD2STRING
;       
; PURPOSE: 
;    Function outputs a string containing field of a photo tsObj 
;    file in the correct format.
;	
;
; CALLING SEQUENCE: 
;    result = field2string(field)
;      
; INPUTS: 
;    field in integer form.  (May be an array)
;       
; OUTPUTS: 
;    field string
; 
; PROCEDURE: 
;      field should have 4 characters padded with zero's if its
;      not big enough.  e.g. if input 33, output is '0033'
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 5/25/99
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() eq 0 then begin
     print,'-Syntax: result = field2string(field)'
     print,''
     print,'Use doc_library,"field2string"  for more help.'  
     return,''
  endif

  on_error,2

  len=4

  nfield = n_elements(field) 
  IF nfield EQ 1 THEN BEGIN 
      return,strn(long(field),len=len,padchar='0')
  ENDIF ELSE BEGIN 
      retarr = strarr(nfield)
      FOR i=0L, nfield-1 DO BEGIN
          retarr[i] = strn(long(field[i]),len=len,padchar='0')
      ENDFOR 
      return,retarr
  ENDELSE 

end






