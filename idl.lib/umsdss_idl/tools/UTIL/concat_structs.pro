pro concat_structs,str1,str2,strsum
;+
; NAME:
;    CONCAT_STRUCTS
; 
; PURPOSE:
;  for concatenating two arrays of structures of the same type
;  but not necessarily the same name or order of tag names.
;
; CALLING SEQUENCE:
;   concat_structs, str1, str2, strsum
;
; INPUTS:
;   str1, str2: 2 structures with the same tags to be concatenated.
;
; OUTPUTS:
;   strsum: concatenetion of str1, str2, with tag names in the order
;        of str1. If tags have same name but different type, then
;        a type conversion is performed, with result having type
;        of tag in str1.  Resulting structure has name of str1.
;
; Author: Dave Johnston  UofM
; Used MATCH procedre to make sure tags all match. Use match indices
;   to copy values.
;-
  if n_params() LT 2 then begin 
      print,'-syntax concat_structs,str1,str2,strsum'
      return
  endif

  tags=tag_names(str1)
  tags2 = tag_names(str2)
  ntags=n_elements(tags)
  IF ntags NE n_elements(tags2) THEN BEGIN
      print,'Structures must have the same tags'
      return
  ENDIF 
  match, tags, tags2, m, m2
  IF (n_elements(m) NE ntags)  THEN BEGIN 
      print,'Structures must have the same tags'
      return
  ENDIF 
  IF m[0] EQ -1 THEN BEGIN
      print,'No matching tags'
      return
  ENDIF 

  s1=size(str1)
  s2=size(str2)
  l1=s1(1)
  l2=s2(1)
  tot=l1+l2
  
  st=str1(0) 
  strsum=replicate(st,tot)
  strsum(0:l1-1)=str1

  FOR i=0, ntags-1 DO strsum[l1:tot-1].(m[i]) = str2.(m2[i])
 
  return
end


 
