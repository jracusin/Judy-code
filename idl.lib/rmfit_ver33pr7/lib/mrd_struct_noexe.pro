;+
; NAME:
;       MRD_STRUCT
; PURPOSE:
;       Return a structure as defined in the names and values data.
; CALLING SEQUENCE:
;       struct = MRD_STRUCT(NAMES, VALUES, NROW,                $
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
;       STRUCTYP = The structure type.  Since IDL does not allow the
;                  redefinition of a named structure it is an error
;                  to call MRD_STRUCT with different parameters but
;                  the same STRUCTYP in the same session.  If this
;                  keyword is not set an anonymous structure is created.
;       TEMPDIR  = If the structure is more than modestly complex a
;                  temporary file is created.  This file will be
;                  created in the current directory unless the TEMPDIR
;                  keyword is specified.  Note that the temporary directory
;                  should also be in the IDL search path.
;       OLD_STRUCT=Use old format structures.
; COMMON BLOCKS:
;       MRD_COMMON
; SIDE EFFECTS:                                                            
;       May create a temporary file if the structure definition is too long 
;       for the EXECUTE function and using old style structures
;
; RESTRICTIONS:
;       By default this program uses a series of execute
;       commands and create_struct's to create the structure.
;       If the old_struct keyword is set, then a program may
;       be dynamically compiled.  The nominal maximum length
;       of the execute string is 131 characters, but many systems
;       seem to allow longer values.  This code may execute more
;       efficiently with a longer execute buffer.
; PROCEDURE:
;       A structure definition is created using the parameter values.
;       MRD_NSTRUCT is called if the OLD_STRUCT keyword is not specified
;       and generates the structure in pieces using the
;       execute and create_struct keywords.
;
;       If the old_struct flag is set, then the program tries to compile
;       the structure with a single execute command.  If the structure
;       definition is too long  MRD_FSTRUCT is called to write, compile and
;       execute a function which will define the structure.
; EXAMPLES:
;       str = mrd_struct(['fld1', 'fld2'], ['0','dblarr(10,10)'],3)
;       print, str(0).fld2(3,3)
;
;       str = mrd_struct(['a','b','c','d'],['1', '1.', '1.d0', "'1'"],1)
;               ; returns a structure with integer, float, double and string
;               ; fields.
; PROCEDURE CALLS:
;       CONCAT_DIR - Used to concatenate temporary directory with filename
; MODIFICATION HISTORY:
;       Created by T. McGlynn October, 1994.
;       Modified by T. McGlynn September, 1995.
;          Added capability to create substructures so that structure
;          may contain up to 4096 distinct elements.  [This can be
;          increased by futher iteration of the process used if needed.]
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Removed V4.0 reference to common block  October 1997
;       Allowed unlimited number of structure elements if the version
;       is greater than 5.0.  Put back in code to handle prior versions.
;       The [] will need to be translated back to () for this to
;       work.  T. McGlynn December 15 1998.
;       Add MRD_NSTRUCT since IDL has mysterious problems compiling
;       very large structures.
;       Ripped out all references to EXECUTE for compatibility with the 
;       IDLVM. RDP June 2004
;-

function mrd_struct_noexe, names, values, nrow, $
    structyp=structyp, tempdir=tempdir, silent=silent


; Create an instance of A, since an execute function cannot
; create an instance of an undefined variable.

a = 0

; Check that the number of names is the same as the number of values.
nel = n_elements(names)
if nel ne n_elements(values) then return, 0

; Start formatting the string.
strng = "a="

; If we are going to return an array of structures, then we use
; the replicate function.
;if nrow gt 1 then begin
;        strng = strng + "replicate({"
;endif else begin
;        strng = strng + "{"
;endelse

; Put in the structure name if the user has specified one.
;if keyword_set(structyp) then begin
;        strng = strng + structyp + ","
;endif

; Now for each element put in a name/value pair.
for i=0, nel-1 do begin
	IF i EQ 0 THEN begin
		myStruct = CREATE_STRUCT(names[i], values[i])
	ENDIF ELSE BEGIN
		myStruct = CREATE_STRUCT(myStruct, names[i], values[i])
	ENDELSE
;        if i ne 0 then strng = strng + ','
;        strng = strng + names[i] + ':'
;        strng = strng + values[i]
endfor

if keyword_set(structyp) then begin
     myStruct = CREATE_STRUCT(structyp, myStruct)
endif

;strng = strng + "}"

; Put in the second argument to the REPLICATE function if
; needed.
if nrow gt 1 then begin
        strng = strng + "," + strtrim(long(nrow),2)+ ")"
endif

; The IDL documentation implies that 131 is the maximum length
; for EXECUTE although many implementations seem to support longer
; strings.  We'll use this value though to be safe.

;if strlen(strng) gt 131 then begin
;        return, mrd_fstruct(names, values, nrow, structyp=structyp, $
;           tempdir=tempdir, silent=silent, old_struct=old_struct)
;endif else begin
;        ; Execute the string.  RES should be 1 if the execution was successful.
;        res = execute(strng)
;
;        if res eq 0 then return, 0 else return, a
;endelse

end
