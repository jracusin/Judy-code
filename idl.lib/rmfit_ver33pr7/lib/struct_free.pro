; -----------------------------------------------------------------------------
;+
; NAME:
;
;     STRUCT_FREE (PROCEDURE)
;
; PURPOSE:
;
;     Given an input structure, free all pointer data members, and destroy
;     all object members.
;
;     WARNING: this function does not recursively free data - only
;     first level data elements are freed.
;
;     This procedure can be used, for example, from within an object
;     CLEANUP method to free object member data.  Define object member
;     data in a structure, then from within the CLEANUP method
;
;         STRUCT_FREE, self.dataStruct
;
;     If new structure elements are added to dataStruct, the CLEANUP
;     method will not have to be altered.
;
; INPUTS:
;       
;     Any IDL structure
; 
; KEYWORD PARAMETERS:
;       
;     VERBOSE
;         Set this keyword to print the structure tag names as
;         they are freed
;
; OUTPUTS:
;
;     NONE
;
; RESTRICTIONS:
;	
;     Does not recursively free structure elements.  Only first level 
;     data elements are freed.
;
; MODIFICATION HISTORY:
;	
;       2000 February, RSM, allow OBJECT structure members
;       1999 November, written, Robert.Mallozzi@msfc.nasa.gov
;-
; -----------------------------------------------------------------------------

PRO STRUCT_FREE, struct, VERBOSE = verbose

    IF (SIZE (struct, /TNAME) NE 'STRUCT') THEN $
       RETURN
       
    verbose = KEYWORD_SET (verbose)    
    nTags   = N_TAGS (struct)
    names   = TAG_NAMES (struct)    
    sName   = TAG_NAMES (struct, /STRUCTURE)    
    
    IF (sName EQ '') THEN sName = 'ANONYMOUS'
    
    FOR i = 0, nTags - 1 DO BEGIN

        name = SIZE (struct.(i), /TNAME)
        
        CASE (name) OF
        
            'POINTER' : BEGIN
                PTR_FREE, struct.(i)
                IF (verbose) THEN $
                   MESSAGE, /CONTINUE, 'Struct ' + sName + $
                       ', freed pointer member: ' + names[i]
                END
                      
            'OBJREF' : BEGIN
                OBJ_DESTROY, struct.(i)
                IF (verbose) THEN $
                   MESSAGE, /CONTINUE, 'Struct ' + sName + $
                       ', freed object member: ' + names[i]
                END

            ELSE :
       
       ENDCASE
            
    ENDFOR

END
