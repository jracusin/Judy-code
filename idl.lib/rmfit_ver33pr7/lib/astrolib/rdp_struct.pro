;+
; NAME:
;       RDP_STRUCT
; PURPOSE:
;       Return a structure as defined in the names and values data.
; CALLING SEQUENCE:
;       struct = rdp_struct(NAMES, VALUES, NROW,                $
;                   STRUCTYP=structyp,                          $
;                   TEMPDIR=tempdir, /OLD_STRUCT)
; INPUT PARAMETERS:
;       NAMES   = A string array of names of structure fields.
;       VALUES  = A string array giving the values of the structure
;                 fields.  See examples below.
;       NROW    = The number of elements in the structure array.
;       
; RETURNS:
;       A structure as described in the parameters or 0 if an error
;       is detected.
;
; OPTIONAL KEYWORD PARAMETERS:
;       STRUCTYP = The structure type. Since IDL does not allow the
;                  redefinition of a named structure it is an error
;                  to call MRD_STRUCT with different parameters but
;                  the same STRUCTYP in the same session.  If this
;                  keyword is not set an anonymous structure is created.
;       TEMPDIR  = Also ignored; not needed.
;       OLD_STRUCT=Also ignored; not needed.
;
; RESTRICTIONS:
;       This code is coupled very tightly with RDPFITS and should 
;       only be called from it. The parsing of the VALUES array can 
;       only work assuming the values are exactly as those placed
;       there by RDPFITS. 
; PROCEDURE:
;       A structure definition is created using the parameter values.
;       The VALUES array is parsed for the type and the structure is 
;       generated in pieces using the create_struct command.
; EXAMPLES:
;       str = rdp_struct(['fld1', 'fld2'], ['0','dblarr(10,10)'],3)
;       print, str(0).fld2(3,3)
;
;       str = rdp_struct(['a','b','c','d'],['0', '0.', '0.d0', '" "'],1)
;               ; returns a structure with integer, float, double and string
;               ; fields.
; MODIFICATION HISTORY:
;       Based upon MRD_STRUCT, Created by T. McGlynn October, 1994.
;       RDP, Aug. 11, 2004:
;       EXECUTE is not allowed in IDLVM programs at all. I have ripped out 
;       the code that invoked EXECUTE, since CREATE_STRUCT is 'good enough'
;       for most FITS files. Use the old routines for huge FITS files! 
;
;       RDP, Mar. 30, 2005:
;       Variable length arrays were not being handled correctly; now 'P' always
;       goes to lonarr(2) to correctly index into the heap. Problems with string
;       arrays emerged in the original code; now they are read into bytearrays. 
;-
function rdp_struct, names, values, nrow, $
    structyp=structyp, tempdir=tempdir, silent=silent, old_struct=old_struct

    types =  ['L', 'X', 'B', 'I', 'J', 'K', 'A', 'E', 'D', 'C', 'M', 'P']
    arrstr = ['strarr(', 'bytarr(', 'bytarr(', 'intarr(', 'lonarr(', 'lon64arr(',      $ 
              'string(replicate(32b,', 'fltarr(', 'dblarr(', 'complexarr(',            $ 
              'dcomplexarr(', 'lonarr(2*', 'uintarr(', 'ulonarr(', 'ulon64arr(', $
              'string(bytarr(']

    sclstr = ["'T'", '0b', '0b', '0', '0l', '0ll', '" "', '0.', '0.d0', 'complex(0.,0.)', $ 
              'dcomplex(0.d0,0.d0)', 'lonarr(2)', '0u',        '0ul',      '0ull', '0b']

    numScl = N_ELEMENTS(sclstr)
    snglstruct = {a:'T', b:0B, c:0B, d:0, e:0L, f:0LL, g:" ", h:0., $
                  i:0.d0, j:complex(0.,0.), k:dcomplex(0.d0,0.d0), l:lonarr(2), $
                  m:0U, n:0UL, o:0ULL, p:0B}

    ; Check that the number of names is the same as the number of values.
    nel = n_elements(names)
    if nel ne n_elements(values) then return, 0

    ; Start formatting the structure.
    FOR i = 0, nel - 1 DO BEGIN 
        myName = names[i]
        myValName = values[i]
        
        ;Try the simple values first:
        found = 0
        nn = 0
        REPEAT BEGIN
            IF (STRLOWCASE(myValName) EQ sclstr[nn]) THEN BEGIN
                found = 1
                myValue = snglstruct.(nn)
            ENDIF
            nn = nn + 1
        ENDREP UNTIL (found OR (nn EQ numScl))
        
        ;Now we need to look for arrays:
        nn = 0
        IF (not found) THEN BEGIN
            REPEAT BEGIN
                IF (STRPOS(myValName, arrstr[nn]) EQ 0) THEN BEGIN
                    found = 1
                    myResult = STRSPLIT(myValName,',',COUNT = co, /EXTRACT)
                    cutStr = arrstr[nn]
                    ; Take care of string arrays
                    IF (nn EQ 6) THEN BEGIN
                        myResult = myResult[1:*]
                        cutStr = ''
                        co = co - 1
                    ENDIF
                    dimOne = STRMID(myResult[0], STRLEN(cutStr))
                    myDims = [LONG(dimOne)]
                    IF (co GT 1) THEN BEGIN
                        FOR jj = 1, co - 1 DO BEGIN
                            myDims = [myDims, LONG(myResult[jj])]
                        ENDFOR
                    ENDIF 
                    ; Got an error if myDims = 0
                    IF myDims[0] EQ 0 THEN BEGIN
                        myValue = 0
                    ENDIF ELSE BEGIN
                        ; Again, handle strings with care
                        IF (nn EQ 6) THEN BEGIN
                            myValue = string(replicate(32b, myDims))
                        ENDIF ELSE $
                            myValue = MAKE_ARRAY(DIMENSION = myDims, $
                                      TYPE = (SIZE(snglstruct.(nn)))[1])
                    ENDELSE
                ENDIF
                nn = nn + 1
            ENDREP UNTIL (found OR (nn EQ numScl))
        ENDIF
        
        ;== Need to keep track of first time:
        IF (i EQ 0) THEN BEGIN
            myStruct = CREATE_STRUCT(myName, myValue)
        ENDIF ELSE BEGIN
            myStruct = CREATE_STRUCT(myStruct, myName, myValue)
        ENDELSE
    ENDFOR
        
    ; Put in the structure name if the user has specified one.
    if keyword_set(structyp) then begin
        myStruct = CREATE_STRUCT(name = structyp, myStruct) 
    endif 
    
    if (nrow GT 1) then a = REPLICATE(myStruct, nrow) $
        else a = myStruct
    
    return, a

end
