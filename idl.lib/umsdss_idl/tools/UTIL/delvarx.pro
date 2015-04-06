PRO delvarx, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, $
             v15, v16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    DELVARX
;       
; PURPOSE:
;    Make input variables undefined. Similar to intrinsic DELVAR, but
;    works at any calling level (e.g. within procedures)
;    This is a factor of 40 times faster than the astronomy
;    library version of delvarx.
;
; CALLING SEQUENCE:
;    delvarx, v1, v2, ..., v16
;
; INPUTS: 
;    v1,v2,..... any idl variable
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    NONE
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    NONE (uses intrinsic temporary function)
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Created 23-Apr-2001, Erin Scott Sheldon UofMich
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
      print,'-Syntax: delvarx, v1, v2, ....'
      print,'  Up to 16 arguments'
      print,''
      print,'Use doc_library,"delvarx"  for more help.'  
     return
  ENDIF 

  np=n_params()

  CASE np OF 
      1:  BEGIN & v1=0L & tmp=temporary(v1) & END 
      2:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & END 
      3:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & END 
      4:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & END 
      5:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & END 
      6:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & END 
      7:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & END 
      8:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & END 
      9:  BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & END
      10: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & END
      11: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & END
      12: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & v12=0L & tmp=temporary(v12) & END
      13: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & v12=0L & tmp=temporary(v12) & v13=0L & tmp=temporary(v13) & END
      14: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & v12=0L & tmp=temporary(v12) & v13=0L & tmp=temporary(v13) & v14=0L & tmp=temporary(v14) & END
      15: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & v12=0L & tmp=temporary(v12) & v13=0L & tmp=temporary(v13) & v14=0L & tmp=temporary(v14) & v15=0L & tmp=temporary(v15) & END
      16: BEGIN & v1=0L & tmp=temporary(v1) & v2=0L & tmp=temporary(v2) & v3=0L & tmp=temporary(v3) & v4=0L & tmp=temporary(v4) & v5=0L & tmp=temporary(v5) & v6=0L & tmp=temporary(v6) & v7=0L & tmp=temporary(v7) & v8=0L & tmp=temporary(v8) & v9=0L & tmp=temporary(v9) & v10=0L & tmp=temporary(v10) & v11=0L & tmp=temporary(v11) & v12=0L & tmp=temporary(v12) & v13=0L & tmp=temporary(v13) & v14=0L & tmp=temporary(v14) & v15=0L & tmp=temporary(v15) & v16=0L & tmp=temporary(v16) & END
      ELSE: message,'Too many parameters: only 16 parameters allowed' 
  ENDCASE 
END 
