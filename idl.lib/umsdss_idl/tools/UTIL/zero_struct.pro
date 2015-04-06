
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    ZERO_STRUCT
;       
; PURPOSE:
;    "Zero" all the elements in a structure. Numbers are set to zero,
;           strings to '' etc.
;
; CALLING SEQUENCE:
;    zero_struct, struct
;
; INPUTS: 
;    struct: Structure to be zeroed. Can be an array of structures.
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    struct: struct with elements zeroed.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Created 26-OCT-2000 Erin Scott Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO zero_struct, struct

  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: zero_struct, struct'
     print,''
     print,'Use doc_library,"zero_struct"  for more help.'  
     return
  ENDIF 

  IF datatype(struct) NE 'STC' THEN BEGIN
      print,'ERROR: input value must be a structure'
      return
  ENDIF 
  
  nst=n_elements(struct)
  instname = tag_names(struct[0], /structure_name)

  addst = ntostr( long(round(10000.*randomu(seed))) )
  
  stname = 'zero'+addst+ntostr(long(systime(1)))
  tmpstruct = create_struct(name=stname, struct[0])

  delvarx, tmpstruct
  command = 'tmpstruct = create_struct(name="'+instname+'", {'+stname+'} )'
  tmp=execute(command)
  IF tmp EQ 0 THEN BEGIN 
      print,'ERROR: struct unchanged'
      return
  ENDIF 
  delvarx,struct
  struct = replicate(temporary(tmpstruct), nst)

  return
END
