PRO sphoto_match, struct1, struct2, m1 ,m2, count=count, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    SPHOTO_MATCH
;       
; PURPOSE:
;    Match two photo structs by their run, rerun, camcol, field, 
;    and rerun.  This uniquely defines each object.  Same as
;    PHOTO_MATCH but works on structs
;
; CALLING SEQUENCE:
;    sphoto_match, pstruct1, pstruct2, matches1, matches2, $
;                  count=count, /silent
;
; INPUTS: 
;    pstruct1, pstruct2: photo structures. must contain:
;       run, rerun, camcol, field, id:   unique info for each object
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    /silent: don't print out removal of duplicates
;       
; OUTPUTS: 
;    matches1, matches2:  match indices for each list.
;
; OPTIONAL OUTPUTS:
;    count: number of matches
;
; CALLED ROUTINES:
;    MATCH
; 
; PROCEDURE: 
;    Call PHOTO_MATCH 
;
; REVISION HISTORY:
;    ??/??/00  Erin S. Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() EQ 0 THEN BEGIN 
      print,'-syntax sphoto_match,struct1, struct2, m1, m2, count=count, silent=silent'
      print
      print,'Use doc_library, "sphoto_match"  for more help'
      return
  ENDIF 

  photo_match, struct1.run, struct1.rerun, struct1.camcol, struct1.field, struct1.id, $
               struct2.run, struct2.rerun, struct2.camcol, struct2.field, struct2.id, $
               m1, m2, $
               count=count, silent=silent

return
END 

