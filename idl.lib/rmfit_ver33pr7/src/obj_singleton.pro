; ----------------------------------------------------------------------------
;+
; NAME:
;     OBJ_SINGLETON (FUNCTION)
;
; PURPOSE:
;     An object class for which only one instance can exist.  If the
;     singleton object does not yet exist on the call to OBJ_SINGLETON (), 
;     a new object is created and its reference is returned.  This object
;     must be destroyed with the usual OBJ_DESTROY procedure.  If the object
;     does already exist, a new object is not created, and a reference to 
;     the existing object is returned.  If any singleton reference is 
;     freed with OBJ_DESTROY, all the singleton references will become
;     invalid, since they all reference the same object.
;
;     This base class should not be instantiated directly, but should be
;     inherited by subclasses to implement the singleton functionality.
;
;     NOTE: If your subclass implements its own INIT and CLEANUP methods,
;     you must call the Singleton INIT and CLEANUP methods explicitly. For
;     example, in your INIT method:
;         
;         FUNCTION mySingleton::init
;             
;             ok = self->Singleton::init () 
;
;             ; mySingleton INIT code...
;             
;             RETURN, ok
;         END
; 
; CALLING SEQUENCE:
;     o = OBJ_SINGLETON (name)
;
; INPUTS:
;
;     name : STRING name of the singleton class to instantiate.  This
;            class must inherit from the SINGLETON class.
;
; KEYWORDS:
;     NONE
;
; COMMON BLOCKS:
;     
;     CB_SINGLETON 
;         This common block stores a list of singletons.  Since IDL has 
;         no concept of a C static variable or a FORTRAN SAVEed variable,
;         singleton object references must be stored in a global access area.  
;         Common blocks are one way to achieve this functionality.
;
; METHODS:
;     NONE
;
; EXAMPLE:
;
;     IDL> HELP, /HEAP
;     Heap Variables:
;         # Pointer: 0
;         # Object : 0
;     IDL>
;     IDL> ; Create an instance of a singleton class.
;     IDL> ; mySingleton must inherit from Singleton.
;     IDL> ;
;     IDL> singleton_1 = OBJ_SINGLETON ('mySingleton')
;     IDL> singleton_2 = OBJ_SINGLETON ('mySingleton')
;     IDL>
;     IDL> HELP, singleton_1
;     SINGLETON_1     OBJREF    = <ObjHeapVar1(MYSINGLETON)>
;     IDL> HELP, singleton_2
;     SINGLETON_2     OBJREF    = <ObjHeapVar1(MYSINGLETON)>
;     IDL>
;     IDL> PRINT, singleton_1 EQ singleton_2
;        1
;     IDL> OBJ_DESTROY, singleton_1
;     IDL> HELP, /HEAP
;     Heap Variables:
;         # Pointer: 0
;         # Object : 0
; 
; MODIFICATION HISTORY:
;
;     Written, 1999 November, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Singleton::init

    COMMON CB_SINGLETON, cb_singletonlist

    
    IF (PTR_VALID (cb_singletonlist)) THEN BEGIN
           
       idx = WHERE (OBJ_VALID (*cb_singletonlist), cnt)
       
       IF (cnt EQ 0) THEN BEGIN
          
          ; New list
          PTR_FREE, cb_singletonlist
          cb_singletonlist = PTR_NEW (self)
       
       ENDIF ELSE BEGIN
       
          *cb_singletonlist = (*cb_singletonlist)[idx]
          
          ; Search the list
          idx = WHERE (OBJ_ISA (*cb_singletonlist, OBJ_CLASS (self)), cnt)
          IF (cnt EQ 0) THEN $
             *cb_singletonlist = [*cb_singletonlist, self]  

       ENDELSE
    
    ENDIF ELSE BEGIN

       ; New list    
       ;PTR_FREE, cb_singletonlist
       cb_singletonlist = PTR_NEW (self)
    
    ENDELSE

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destuctor
; ----------------------------------------------------------------------------
PRO Singleton::cleanup

   COMMON CB_SINGLETON, cb_singletonlist

   IF (PTR_VALID (cb_singletonlist)) THEN BEGIN
   
      idx = WHERE (NOT OBJ_ISA (*cb_singletonlist, OBJ_CLASS (self)), cnt)
      
      IF (cnt NE 0) THEN BEGIN
   
         IF (N_ELEMENTS (*cb_singletonlist) EQ 1) THEN BEGIN
         
            PTR_FREE, cb_singletonlist
         
         ENDIF ELSE BEGIN
         
            *cb_singletonlist = (*cb_singletonlist)[WHERE (idx)]

         ENDELSE      
   
      ENDIF

    ENDIF
      
END


;------------------------------------------------------------------------------
; Singleton pattern
;------------------------------------------------------------------------------
PRO Singleton__define

    obj = { SINGLETON, $

        singletonData : 0B $

    }


END

;------------------------------------------------------------------------------
FUNCTION OBJ_SINGLETON, name, _EXTRA = extra

    COMMON CB_SINGLETON, cb_singletonlist

    
    IF (NOT PTR_VALID (cb_singletonlist)) THEN BEGIN

       o = OBJ_NEW (name, _EXTRA = extra)

    ENDIF ELSE BEGIN

       ; Search the list for the requested object

       idx = (WHERE (OBJ_ISA (*cb_singletonlist, OBJ_CLASS (name)), cnt))[0]
       IF (cnt NE 0) THEN BEGIN 

          o = (*cb_singletonlist)[idx]

       ENDIF ELSE BEGIN

          o = OBJ_NEW (name, _EXTRA = extra)

       ENDELSE
   
    ENDELSE

    RETURN, o

END
