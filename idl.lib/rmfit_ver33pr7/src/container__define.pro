; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Container (OBJECT)
;
; PURPOSE:
;
;     This class implements a container to hold either POINTER or OBJREF
;     datatypes.  It provides a convenient interface for memory
;     management of these IDL datatypes, which must be manually freed by
;     the IDL programmer.  POINTERs or OBJECTs can be stored in the
;     Container class, and are freed when the Container is destroyed.
;     Recursion is not supported - only top level POINTERs and OBJECTs
;     are freed.
;
;     NOTE : This class currently does not provide any methods for data
;     retrieval from the container.  It was designed as a convenience for
;     memory management only, although I might get motivated to extend it
;     in the future.  Try the List class if you need a random access data
;     storage container.
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     NONE
;
; PUBLIC METHODS:
;     
;     add (PROCEDURE) - Add data to the container
;         Inputs: A POINTER or OBJECT reference
;        Outputs: NONE 
;       Keywords: NONE 
;
;     clear (PROCEDURE) - Delete all data from the container
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE 
;
;     count (FUNCTION) - Return number of elements in the container
;         Inputs: NONE
;        Outputs: LONG number of elements currently stored in the list 
;       Keywords: NONE 
;
; EXAMPLE:
;
;     ;== Create an instance of a Container
;     
;     container = OBJ_NEW ('Container')
;
;     ;== Add some data to the container
;
;     x = PTR_NEW (INDGEN (100))
;     container->add, x
;
;     o = OBJ_NEW ('IDLgrModel')
;     container->add, o
;
;     PRINT, container->count ()
;     ;      2
;
;     ;== Destroy the container, freeing the contained data
;
;     OBJ_DESTROY, container
;     HELP, /HEAP
;     ; Heap Variables:
;     ;     # Pointer: 0
;     ;     # Object : 0      
;
; MODIFICATION HISTORY:
;
;     Written, 2000 February, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Container::init

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Container::cleanup

    n = self->countPtr ()
    FOR i = 0L, n - 1 DO BEGIN
        PTR_FREE, (*self.dataPtr)[i]
    ENDFOR
    PTR_FREE, self.dataPtr

    n = self->countObj ()
    FOR i = 0L, n - 1 DO BEGIN
        OBJ_DESTROY, (*self.dataObj)[i]
    ENDFOR
    PTR_FREE, self.dataObj

END


; ----------------------------------------------------------------------------
; Add a data element - data can be of any IDL type.
; ----------------------------------------------------------------------------
PRO Container::add, data

    name = SIZE (data, /TNAME)

    IF (name NE 'POINTER' AND name NE 'OBJREF') THEN RETURN
    
    CASE (name) OF
        
        'POINTER' : ptr = self.dataPtr
        'OBJREF'  : ptr = self.dataObj
    
    ENDCASE

    IF (NOT PTR_VALID (ptr)) THEN BEGIN

       ptr = PTR_NEW (data)

    ENDIF ELSE BEGIN

       list = *ptr
       PTR_FREE, ptr
       ptr = PTR_NEW ([list, data])

    ENDELSE

    CASE (name) OF
        
        'POINTER' : self.dataPtr = ptr
        'OBJREF'  : self.dataObj = ptr
    
    ENDCASE
    
END


; ----------------------------------------------------------------------------
; Retrieve the number of elements in the container
; ----------------------------------------------------------------------------
FUNCTION Container::countPtr
; COMPILE_OPT HIDDEN

    RETURN, (PTR_VALID (self.dataPtr) ? N_ELEMENTS (*self.dataPtr) : 0L)   
    
END

FUNCTION Container::countObj
; COMPILE_OPT HIDDEN

    RETURN, (PTR_VALID (self.dataObj) ? N_ELEMENTS (*self.dataObj) : 0L)   
    
END

FUNCTION Container::count

    RETURN, self->countPtr () + self->countObj ()
    
END


;------------------------------------------------------------------------------
; Free all data elements
;------------------------------------------------------------------------------
PRO Container::clear

    n = self->countPtr ()
    FOR i = 0L, n - 1 DO BEGIN
        PTR_FREE, (*self.dataPtr)[i]
    ENDFOR
    PTR_FREE, self.dataPtr
    
    n = self->countObj ()
    FOR i = 0L, n - 1 DO BEGIN
        OBJ_DESTROY, (*self.dataObj)[i]
    ENDFOR
    PTR_FREE, self.dataObj

END


; ----------------------------------------------------------------------------
; Container
; ----------------------------------------------------------------------------
PRO Container__define

    obj = { CONTAINER, $

        dataPTR : PTR_NEW (), $
        dataOBJ : PTR_NEW ()  $

    }

END
