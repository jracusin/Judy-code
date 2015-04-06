FUNCTION stripe2string, stripe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    STRIPE2STRING
;       
; PURPOSE: 
;    Function outputs a string containing stripe of a photo tsObj 
;    file in the correct format.
;	
;
; CALLING SEQUENCE: 
;    result = stripe2string(stripe)
;      
; INPUTS: 
;    stripe in integer form (May be an array).
;       
; OUTPUTS: 
;    stripe string or '' if error.
; 
; PROCEDURE: 
;    stripe should have 2 characters padded with zero's if its
;    not big enough.  e.g. if input 9, output is '09'
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 23-OCT-2001
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() eq 0 then begin
     print,'-Syntax: result = stripe2string(stripe)'
     print,''
     print,'Use doc_library,"stripe2string"  for more help.'  
     return,''
  endif

  on_error,2

  len = 2

  nstripe = n_elements(stripe) 
  IF nstripe EQ 1 THEN BEGIN 
      return,strn(long(stripe),len=len,padchar='0')
  ENDIF ELSE BEGIN 
      retarr = strarr(nstripe)
      FOR i=0L, nstripe-1 DO BEGIN
          retarr[i] = strn(long(stripe[i]),len=len,padchar='0')
      ENDFOR 
      return,retarr
  ENDELSE 

end

