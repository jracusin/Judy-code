FUNCTION  tsobj_name, run, camcol, rerun, field

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    TSOBJ_NAME
;       
; PURPOSE: 
;    ouputs the filename for a photo tsObj file for the given input 
;    parameters
;	
;
; CALLING SEQUENCE: 
;    name=tsobj_name(run, camcol, rerun, field)
;
; INPUTS: 
;    run:  the photo run in integer form
;    camcol: the photo camera ccd column in integer form.
;    rerun: rerun number 
;    field: field number
;
; INPUT KEYWORD PARAMETERS:
;    None.
;       
; OUTPUTS: 
;    tsObj file name.
;
;
; CALLED ROUTINES:
;                   RUN2STRING
;                   FIELD2STRING
; 
; PROCEDURE: 
;	files have form: tsObj-run-camcol-rerun-field.fit
;
;       run in six characters padded with zeroes.
;       field in 4 characters padded with zeroes.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  Umich 5/25/99
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() LT 4 then begin
     print,'-Syntax: name=tsobj_name(run, camcol, rerun, field)'
     print
     print,'Use doc_library,"tsobj_name"  for more help.'  
     return,''
  endif

  runstr = run2string(run)
  camcolstr = ntostr(long(camcol))
  rerunstr = ntostr(long(rerun))
  fieldstr = field2string(field)
  
  name = 'tsObj-'+runstr+'-'+camcolstr+'-'+rerunstr+'-'+fieldstr+'.fit'

  return, name
END






