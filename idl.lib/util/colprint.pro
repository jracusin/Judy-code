pro colprint, v1, v2, v3, v4, v5, v6, v7, v8, v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20, $
              decimals = decimals, sigfig = sigfig, $
              delimiter = delimiter, endstr = endstr, $
              latex = latex, file = file
;+
; NAME:
;   COLPRINT
; PURPOSE:
;   Print out input variables in a column.  Do some simple string
;   processing.  Even dumps LaTeX if you like.  
;
; CALLING SEQUENCE:
;   COLPRINT,V1,...,V20 [,decimals=decimals,sigfig=sigfig
;            DELIMITER = string, ENDSTR = str, /LATEX]
;
; INPUTS:
;   V1,... V20, -- Up to 20 named variables.
;
; KEYWORD PARAMETERS:
;   Decimals -- Round figures to this many decimal places
;   Sigfig -- Output this many significant figures.  SIGFIG beats decimals.
;   DELIMITER -- The text delimiter to place between columns
;   ENDSTR -- The end delimiter of the line
;   /LATEX -- Set this flag to dump LaTeX style table formatting.
; OUTPUTS:
;   Screen output
;
; MODIFICATION HISTORY:
;
;       Tue Apr 20 09:09:33 2004, Erik Rosolowsky <eros@cosmic>
;		Added LaTeX dump capacity.
;
;       Sat Feb 21 19:14:31 2004, <eros@master>
;		'Bout damned time!
;
;-

;nvar = n_params()
  forward_function sigfig, decimals
  if n_elements(file) gt 0 then openw, lun, file, /get_lun
  nelts = n_elements(v1) 
  ncols = 20
  if keyword_set(latex) then begin
     endstr = ' \\'
     delimiter = ' & '
  endif
  if n_elements(endstr) eq 0 then endstr = ''
  if n_elements(v20) eq 0 then begin 
     v20 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v19) eq 0 then begin 
     v19 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v18) eq 0 then begin 
     v18 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v17) eq 0 then begin 
     v17 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v16) eq 0 then begin 
     v16 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v15) eq 0 then begin 
     v15 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v14) eq 0 then begin 
     v14 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v13) eq 0 then begin 
     v13 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v12) eq 0 then begin 
     v12 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v11) eq 0 then begin 
     v11 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v10) eq 0 then begin 
     v10 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v9) eq 0 then begin 
     v9 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v8) eq 0 then begin 
     v8 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v7) eq 0 then begin 
     v7 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v6) eq 0 then begin 
     v6 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v5) eq 0 then begin 
     v5 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v4) eq 0 then begin 
     v4 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v3) eq 0 then begin 
     v3 = strarr(nelts)
     ncols = ncols-1
  endif
  if n_elements(v2) eq 0 then begin 
     v2 = strarr(nelts)
     ncols = ncols-1
  endif

  if n_elements(sigfig) gt 0 then begin
     type = size(v1, /tname)
     if type ne 'STRING' then v1 = sigfig(v1, sigfig)
     type = size(v2, /tname)
     if type ne 'STRING' then v2 = sigfig(v2, sigfig)
     type = size(v3, /tname)
     if type ne 'STRING' then v3 = sigfig(v3, sigfig)
     type = size(v4, /tname)
     if type ne 'STRING' then v4 = sigfig(v4, sigfig)
     type = size(v5, /tname)
     if type ne 'STRING' then v5 = sigfig(v5, sigfig)
     type = size(v6, /tname)
     if type ne 'STRING' then v6 = sigfig(v6, sigfig)
     type = size(v7, /tname)
     if type ne 'STRING' then v7 = sigfig(v7, sigfig)
     type = size(v8, /tname)
     if type ne 'STRING' then v8 = sigfig(v8, sigfig)
     type = size(v9, /tname)
     if type ne 'STRING' then v9 = sigfig(v9, sigfig)
     type = size(v10, /tname)
     if type ne 'STRING' then v10 = sigfig(v10, sigfig)
     type = size(v11, /tname)
     if type ne 'STRING' then v11 = sigfig(v11, sigfig)
     type = size(v12, /tname)
     if type ne 'STRING' then v12 = sigfig(v12, sigfig)
     type = size(v13, /tname)
     if type ne 'STRING' then v13 = sigfig(v13, sigfig)
     type = size(v14, /tname)
     if type ne 'STRING' then v14 = sigfig(v14, sigfig)
     type = size(v15, /tname)
     if type ne 'STRING' then v15 = sigfig(v15, sigfig)
     type = size(v16, /tname)
     if type ne 'STRING' then v16 = sigfig(v16, sigfig)
     type = size(v17, /tname)
     if type ne 'STRING' then v17 = sigfig(v17, sigfig)
     type = size(v18, /tname)
     if type ne 'STRING' then v18 = sigfig(v18, sigfig)
     type = size(v19, /tname)
     if type ne 'STRING' then v19 = sigfig(v19, sigfig)
     type = size(v20, /tname)
     if type ne 'STRING' then v20 = sigfig(v20, sigfig)
  endif else begin
     if n_elements(decimals) gt 0 then begin
        type = size(v1, /tname)
        if type ne 'STRING' then v1 = decimals(v1, decimals)
        type = size(v2, /tname)
        if type ne 'STRING' then v2 = decimals(v2, decimals)
        type = size(v3, /tname)
        if type ne 'STRING' then v3 = decimals(v3, decimals)
        type = size(v4, /tname)
        if type ne 'STRING' then v4 = decimals(v4, decimals)
        type = size(v5, /tname)
        if type ne 'STRING' then v5 = decimals(v5, decimals)
        type = size(v6, /tname)
        if type ne 'STRING' then v6 = decimals(v6, decimals)
        type = size(v7, /tname)
        if type ne 'STRING' then v7 = decimals(v7, decimals)
        type = size(v8, /tname)
        if type ne 'STRING' then v8 = decimals(v8, decimals)
        type = size(v9, /tname)
        if type ne 'STRING' then v9 = decimals(v9, decimals)
        type = size(v10, /tname)
        if type ne 'STRING' then v10 = decimals(v10, decimals)
        type = size(v11, /tname)
        if type ne 'STRING' then v11 = decimals(v11, decimals)
        type = size(v12, /tname)
        if type ne 'STRING' then v12 = decimals(v12, decimals)
        type = size(v13, /tname)
        if type ne 'STRING' then v13 = decimals(v13, decimals)
        type = size(v14, /tname)
        if type ne 'STRING' then v14 = decimals(v14, decimals)
        type = size(v15, /tname)
        if type ne 'STRING' then v15 = decimals(v15, decimals)
        type = size(v16, /tname)
        if type ne 'STRING' then v16 = decimals(v16, decimals)
        type = size(v17, /tname)
        if type ne 'STRING' then v17 = decimals(v17, decimals)
        type = size(v18, /tname)
        if type ne 'STRING' then v18 = decimals(v18, decimals)
        type = size(v19, /tname)
        if type ne 'STRING' then v19 = decimals(v19, decimals)
        type = size(v20, /tname)
        if type ne 'STRING' then v20 = decimals(v20, decimals)
     endif
  endelse 
  if n_elements(decimals) eq 0 and n_elements(sigfig) eq 0 then begin
     type = size(v1, /tname)
     if type ne 'STRING' then v1 = string(v1)
     type = size(v2, /tname)
     if type ne 'STRING' then v2 = string(v2)
     type = size(v3, /tname)
     if type ne 'STRING' then v3 = string(v3)
     type = size(v4, /tname)
     if type ne 'STRING' then v4 = string(v4)
     type = size(v5, /tname)
     if type ne 'STRING' then v5 = string(v5)
     type = size(v6, /tname)
     if type ne 'STRING' then v6 = string(v6)
     type = size(v7, /tname)
     if type ne 'STRING' then v7 = string(v7)
     type = size(v8, /tname)
     if type ne 'STRING' then v8 = string(v8)
     type = size(v9, /tname)
     if type ne 'STRING' then v9 = string(v9)
     type = size(v10, /tname)
     if type ne 'STRING' then v10 = string(v10)
     type = size(v11, /tname)
     if type ne 'STRING' then v11 = string(v11)
     type = size(v12, /tname)
     if type ne 'STRING' then v12 = string(v12)
     type = size(v13, /tname)
     if type ne 'STRING' then v13 = string(v13)
     type = size(v14, /tname)
     if type ne 'STRING' then v14 = string(v14)
     type = size(v15, /tname)
     if type ne 'STRING' then v15 = string(v15)
     type = size(v16, /tname)
     if type ne 'STRING' then v16 = string(v16)
     type = size(v17, /tname)
     if type ne 'STRING' then v17 = string(v17)
     type = size(v18, /tname)
     if type ne 'STRING' then v18 = string(v18)
     type = size(v19, /tname)
     if type ne 'STRING' then v19 = string(v19)
     type = size(v20, /tname)
     if type ne 'STRING' then v20 = string(v20)
  endif
  for k = 0, nelts-1 do begin
     if n_elements(delimiter) eq 0 then delimiter = ' '
     
     strout = v1[k]+delimiter+v2[k]+delimiter+v3[k]+delimiter+$
              v4[k]+delimiter+v5[k]+delimiter+v6[k]+delimiter+$
              v7[k]+delimiter+v8[k]+delimiter+v9[k]+delimiter+$
              v10[k]+delimiter+v11[k]+delimiter+v12[k]+delimiter+$
              v13[k]+delimiter+v14[k]+delimiter+v15[k]+delimiter+$
              v16[k]+delimiter+v17[k]+delimiter+v18[k]+delimiter+$
              v19[k]+delimiter+v20[k]+delimiter
; Trim to last delimiter...

     for ctr = 0, 20-ncols do begin
        strout = strmid(strout, 0, strpos(strout, delimiter, $
                                          /reverse_search))
     endfor
     strout = strout+endstr
;    if not keyword_set(latex) then begin
;      strout = strcompress(strjoin(strsplit(strout, delimiter, /extract), $
;                                   delimiter))+endstr
;    endif else begin
;      strout = strout+endstr
;    endelse
     if n_elements(file) gt 0  then printf, lun, strout else print, strout
  endfor 
  if n_elements(file) gt 0  then begin
     close, lun
     free_lun, lun
  endif

  return
end
