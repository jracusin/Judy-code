FUNCTION run2string, run

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    RUN2STRING
;       
; PURPOSE: 
;    Function outputs a string containing run of a photo tsObj 
;    file in the correct format.
;	
;
; CALLING SEQUENCE: 
;    result = run2string(run)
;      
; INPUTS: 
;    run in integer form (may be an array)
;       
; OUTPUTS: 
;    run string
; 
; PROCEDURE: 
;    run should have 6 characters padded with zero's if its
;    not big enough.  e.g. if input 33, output is '000033'
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 5/25/99
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() eq 0 then begin
     print,'-Syntax: result = run2string(run)'
     print,''
     print,'Use doc_library,"run2string"  for more help.'  
     return,''
  endif

  on_error,2

  len = 6

  nrun = n_elements(run) 
  IF nrun EQ 1 THEN BEGIN 
      return,strn(long(run),len=len,padchar='0')
  ENDIF ELSE BEGIN 
      retarr = strarr(nrun)
      FOR i=0L, nrun-1 DO retarr[i] = strn(long(run[i]),len=len,padchar='0')
      return,retarr
  ENDELSE 

end






