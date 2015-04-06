;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;   STATUS_SELECT
; PURPOSE:
;   Makes cuts based on status flags using input status structure. 
;   These cuts are strictly "anded" together, so they must all be true for the 
;   object to survive.
; 
; INPUTS:  pstruct: a photo output structure (must have .flags tag...)
;          status_struct: Premade status structure. This will require any
;               flags set to 'Y' and insist that any flag set to 'N' be
;               off
;          input_index: you can input an index, from an earlier selection
;               for instance. If this has size(input_index)(0)=0 then
;               the returned selection index will be -1
;
; OUTPUTS: select_index: indices of selected objects....
;
; Author:  Ryan Scranton
; Date: 11/4/99
; Modified from Tim McKay's flag_select.pro
; Fixed poor memory usage. 11-Feb-2004 E.S.S.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro status_select, pstruct, status_struct,select_index, input_index=input_index

  on_error,2

  if n_params() LT 3 then begin
   print,'-syntax status_select, pstruct, status_struct, select_index, input_index=input_index'
   return
  endif

  if keyword_set(input_index) then begin
        if ((size(input_index))(0) eq 0) then begin
           select_index = -1
           return
        endif else begin
           k = input_index
        endelse
  endif else begin
        k=lindgen(n_elements(pstruct))
  endelse
  
  f=long(pstruct.status)

  ss=status_struct

  if (ss.SET eq 'Y') then begin
        h=where((f(k) and 2L^0) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.SET eq 'N') then begin
        h=where((f(k) and 2L^0) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.GOOD eq 'Y') then begin
        h=where((f(k) and 2L^1) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.GOOD eq 'N') then begin
        h=where((f(k) and 2L^1) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.DUPLICATE eq 'Y') then begin
        h=where((f(k) and 2L^2) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.DUPLICATE eq 'N') then begin
        h=where((f(k) and 2L^2) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.OK_RUN eq 'Y') then begin
        h=where((f(k) and 2L^3) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.OK_RUN eq 'N') then begin
        h=where((f(k) and 2L^3) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.RESOLVED eq 'Y') then begin
        h=where((f(k) and 2L^4) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.RESOLVED eq 'N') then begin
        h=where((f(k) and 2L^4) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.PSEGMENT eq 'Y') then begin
        h=where((f(k) and 2L^5) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.PSEGMENT eq 'N') then begin
        h=where((f(k) and 2L^5) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.FIELD eq 'Y') then begin
        h=where((f(k) and 2L^6) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.FIELD eq 'N') then begin
        h=where((f(k) and 2L^6) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.SCANLINE eq 'Y') then begin
        h=where((f(k) and 2L^7) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.SCANLINE eq 'N') then begin
        h=where((f(k) and 2L^7) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.STRIPE eq 'Y') then begin
        h=where((f(k) and 2L^8) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.STRIPE eq 'N') then begin
        h=where((f(k) and 2L^8) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.SECONDARY eq 'Y') then begin
        h=where((f(k) and 2L^9) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.SECONDARY eq 'N') then begin
        h=where((f(k) and 2L^9) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.PRIMARY eq 'Y') then begin
        h=where((f(k) and 2L^10) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.PRIMARY eq 'N') then begin
        h=where((f(k) and 2L^10) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  if (ss.TARGET eq 'Y') then begin
        h=where((f(k) and 2L^11) ne 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif 
  if (ss.TARGET eq 'N') then begin
        h=where((f(k) and 2L^11) eq 0)
        if ((size(h))(0) eq 0) then begin
             select_index = -1
             return
        endif
        k=k(h)
  endif

  select_index=k 


return

end
