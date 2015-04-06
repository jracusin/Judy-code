PRO read_tsobj_make_struct, indices, bigstruct, str

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    READ_TSOBJ_MAKE_STRUCT
;
; PURPOSE:
;   Creates a structure with the user's input parameters.
;
; CALLING SEQUENCE:
;    read_tsobj_make_struct, indices, bigstruct, substruct
;
; INPUTS: 
;   indices: the index for each tag in bigstruct 
;   bigstruct: the structure from which the new struct
;      is to be created.
;
; Outputs: substruct: A structure with requested tags in it
;
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified:  01/12/99  Comment: Made run,rerun,camcol,field defaults
; 26-Aug-2002: Minimal tags added in read_tsobj_make_tags now.  This
;              just copies them.  Tags now gotten from bigstruct
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 2 THEN BEGIN 
      print, '-syntax: make_struct, indices, bigstruct, substruct'
      return
  ENDIF 

  delvarx, str
  tags = (tag_names(bigstruct))[indices]

  FOR i=0L, n_elements(tags)-1 DO BEGIN 
      IF n_elements(str) EQ 0 THEN BEGIN 
          str = create_struct( tags[i], bigstruct[0].(indices[i]) )
      ENDIF ELSE BEGIN 
          str = create_struct( str, tags[i], bigstruct[0].(indices[i]) )
      ENDELSE 
  ENDFOR 

  return 
END 


