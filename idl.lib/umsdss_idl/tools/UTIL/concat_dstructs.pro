pro concat_dstructs,str1,str2,strsum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    CONCAT_DSTRUCTS
;       
; PURPOSE:
;    Concatenate two possibly DIFFERENT structures that have common tags 
;    but not necessarily identical tags. 
;   
;    WARNING: CONCAT_DSTRUCTS just takes the common tags, so some tags
;    might be lost!
;
; CALLING SEQUENCE:
;    concat_dstructs,str1,str2,strsum
;
; INPUTS: 
;    str1, str2: two structures.
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    strsum: concatenated structure with only common tags from str1,str2
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    REM_DUP
;    COPY_STRUCT
; 
; PROCEDURE: 
;    
; SIDE EFFECTS:
;    The order of tags will follow the order in str1.
;	
;
; REVISION HISTORY:
;    Created: 10/12/2000 Erin Scott Sheldon UofM
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: concat_dstructs,str1,str2,strsum'
     print,''
     print,'Use doc_library,""  for more help.'  
     return
  ENDIF 

  nstr1=n_elements(str1)
  nstr2=n_elements(str2)
  ntot=nstr1+nstr2

  tags1 = tag_names(str1)
  tags2 = tag_names(str2)

  ntags1=n_elements(tags1)

  ;; tags will be ordered as in str1
  FOR i=0L, ntags1-1 DO BEGIN 
      w=where( tags2 EQ tags1[i], nw)
      IF (nw NE 0) THEN BEGIN 
          IF n_elements(cstr) EQ 0 THEN BEGIN 
              cstr = create_struct(tags1[i], str1[0].(i))
          ENDIF ELSE BEGIN 
              cstr = create_struct(cstr, tags1[i], str1[0].(i))
          ENDELSE 
      ENDIF 
  ENDFOR 

  IF n_elements(cstr) EQ 0 THEN BEGIN 
      print,'No common tags found'
      return
  ENDIF 

  delvarx, strsum
  strsum = replicate(cstr, ntot)
  newtags = tag_names(cstr)
  nmatch=n_elements(newtags)

  match, tags1, newtags, m1, mnew
  FOR i=0L, nmatch-1 DO strsum[0:nstr1-1].(mnew[i]) = str1.(m1[i])
  match, tags2, newtags, m2, mnew
  FOR i=0L, nmatch-1 DO strsum[nstr1:ntot-1].(mnew[i]) = str2.(m2[i])

  return
END 
