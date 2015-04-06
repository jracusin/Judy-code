FUNCTION ntostr, num, pos2, pos1, format=format, round=round

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    ntostr
;       
; PURPOSE: 
;    convert a number to a string.  Cuts off white spaces.
;	
;
; CALLING SEQUENCE: 
;    result = ntostr(num, pos2, pos1)
;
; INPUTS: 
;    num:  the number to be converted
;
; OPTIONAL INPUTS:
;   pos2: The number of characters to keep beginning with pos1. Default
;         is to return the whole string.
;   pos1: starting position.  Default is 0, the first position.
;       
;   /round: round the number off based on the number of characters to
;           be output.  e.g.:  
;                    IDL> print,ntostr(34.645893,5)
;                    34.64
;                    IDL> print,ntostr(34.645893,5,/round)
;                    34.65
;           Not useful if integers are input.  Also will not work if non-scalar
;           data are input, in which case the /round keyword is ignored.
;
; OUTPUTS: 
;   The string.
;
; CALLED ROUTINES:
;                   STRTRIM
;                   STRMID
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofM 6/1/99
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: string = ntostr( num [, pos2, pos1, round=round] )'
     print,''
     print,'Use doc_library,"ntostr"  for more help.'  
     return,''
  ENDIF 

  ;; type checking: 8=struct 10=pointer 11=object
  typestruct = size(num, /struct)
  type = typestruct.type
  nnum = typestruct.n_elements

  IF (type EQ 8) OR (type EQ 10) OR (type EQ 11) THEN BEGIN
      message,'Input must be a number or string (scalar or array)',/inf
      return,''
  ENDIF 

  ;; remove leading and trailing blanks
  tmp = strtrim(string(num, format=format), 2)

  np1 = n_elements(pos1)
  np2 = n_elements(pos2)
  IF np1 EQ 0 THEN pos1 = 0
  IF np2 EQ 0 THEN pos2 = strlen(tmp)

  np1 = n_elements(pos1)
  np2 = n_elements(pos2)
  
  IF keyword_set(round) THEN BEGIN 

      ;; check if scalar
      IF typestruct.n_dimensions EQ 0 THEN BEGIN 

          ;; number of positions left of decimal
          nleft = long( alog10( abs(num) ) ) + 1 > 1
          ;; add extra for minus sign
          nleft = nleft + (num LT 0.0)
          
          ;; so how many are available to the right of the decimal,
          ;; given pos2 (length of string)?
          nav = pos2 - nleft - 1 ;1 for decimal place
          
          IF nav GE 0 THEN BEGIN 
              tnum = rnd(num, nav)
          ENDIF ELSE BEGIN 
              ;; deal with decimal place: 0 means just including decimal as
              ;; above. -1 means not including decimal, but rounding should
              ;; be same
              nav = nav+1
              tnum = rnd(num, nav)
          ENDELSE 
          tmp = strtrim(string(tnum, format=format), 2)
          
          return, strmid( tmp, pos1, pos2)
      ENDIF ELSE message,'Cannot round non-scalar input',/inf
  ENDIF 

  IF np1 LT nnum THEN usepos1 = replicate(pos1[0],nnum) ELSE usepos1=pos1
  IF np2 LT nnum THEN usepos2 = replicate(pos2[0],nnum) ELSE usepos2=pos2
  FOR i=0L, nnum-1 DO tmp[i] = strmid(tmp[i], usepos1[i],usepos2[i])

  return,tmp

END
