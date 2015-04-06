;+
; NAME:
;  COMBINE_DSTUCTS
;
;
; PURPOSE:
;  Combine 2 equal length arrays of structures into one array of structures,
;  with the UNION of the tags.  Values from the common tags will be from the
;  first structure.  This is the same as COMBINE_STRUCTS but that program only
;  allows disjoint sets of tags.
;
;
; CATEGORY:
;  Structures
;
;
; CALLING SEQUENCE:
;  combine_dstructs, struct_array1, struct_array2, newStruct,structyp=structyp
;
;
; INPUTS:
;  struct_array1, struct_array2: equal length arrays of structures.
;
; OPTIONAL INPUTS:
;  structyp: a name for the structure. Cannot use the same structyp for two
;            different structures in the same session!
;
; OUTPUTS:
;  newStruct: the output struct with the UNION of the tags.  Values from the
;             common tags will be from the first structure.
;
; PROCEDURE:
;  Get the union of the tags and tag descriptions (datatype).  The names are
;  concatenated and the duplicates removed.  If the datatypes don't match an
;  error may occur in the subsequent copy.  MRD_STRUCT is used to create the
;  new struct, which is replicated. Then copy_struct is used to copy, first
;  from struct_array2 then from struct_array1.
;
; MODIFICATION HISTORY:
;  04-June-2004: created, E. Sheldon, UofChicago
;-

PRO combine_dstructs, struct_array1, struct_array2, newStruct, $
                      structyp=structyp

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: combine_dstructs, struct1, struct2, newStruct, structyp=structyp'
      print
      print,'Values of common tags copy from 1 over 2'
      return
  ENDIF 

;  on_error, 2

  n1 = n_elements(struct_array1)
  n2 = n_elements(struct_array2)

  IF (n1 NE n2) THEN $
    message,'Arrays of structures must be same size '

  struct1 = struct_array1[0]
  struct2 = struct_array2[0]

  names1 = tag_names(struct1)
  names2 = tag_names(struct2)
  nt1 = n_elements(names1)
  nt2 = n_elements(names2)

  match, names1, names2, m1, m2

  IF (n_elements(m1) EQ nt1) AND (n_elements(m2) EQ nt2) THEN BEGIN 
      print,'Structures are the same'
      return
  ENDIF 

  types1 = strarr(nt1)
  desc1  = strarr(nt1)
  types2 = strarr(nt2)
  desc2  = strarr(nt2)

  ;; Get description of each tag. Strings are a special case because
  ;; we send strings to mrd_struct
  FOR i=0L, nt1-1 DO BEGIN
      types1[i] = datatype(struct1.(i))
      desc1[i]  = datatype(struct1.(i),/desc)
      IF (types1[i] EQ 'STR') AND (n_elements(struct1.(i)) EQ 1) $
        THEN desc1[i] = '" "'
  ENDFOR 
  FOR i=0L, nt2-1 DO BEGIN
      types2[i] = datatype(struct2.(i))
      desc2[i]  = datatype(struct2.(i),/desc)
      IF (types2[i] EQ 'STR') AND (n_elements(struct2.(i)) EQ 1) $
        THEN desc2[i] = '" "'
  ENDFOR 

  names = [names1,names2]
  desc = [desc1,desc2]

  gg=rem_dup(names)
  names = names[gg]
  desc = desc[gg]

  newStruct = mrd_struct(names, desc, 1, structyp=structyp)
  newStruct = replicate(newStruct, n1)

  copy_struct, struct_array2, newStruct
  copy_struct, struct_array1, newStruct
    
END 
