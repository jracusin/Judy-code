; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     List
;
; PURPOSE:
;     An object that implements a random access data storage mechanism.  
;     Any IDL datatype can be added, and types can be mixed within a 
;     single instance of the list.
;
; TYPE:
;     OBJECT
;
; CATEGORY:
;     Data structures
;
; CALLING SEQUENCE:
;     myList = OBJ_NEW ('List')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; DEPENDENCIES:
;     NONE
;
; METHODS:
;     
;     add (PROCEDURE) - Add data to the list
;         Inputs: KEY  : STRING name denoting the data. Keys are case-sensitive.
;                 DATA : data to add to the list (can be any IDL datatype)
;        Outputs: NONE 
;       Keywords: NONE 
;
;     data (FUNCTION) - Retrieve data from the list
;         Inputs: KEY : STRING name denoting the data.  Keys are case-sensitive.
;        Outputs: DATA corresponding to the specified KEY
;       Keywords: ERROR : Set to a named variable to return the success 
;                     status of the method call.  If data could not be 
;                     extracted, ERROR is set to 1, else ERROR = 0.
;
;     count (FUNCTION) - Return number of elements in the list
;         Inputs: NONE
;        Outputs: INTEGER number of elements currently stored in the list 
;       Keywords: NONE 
;
;     names (FUNCTION) - Return STRARR of the names of the list elements
;         Inputs: NONE
;        Outputs: STRARR of list element keys 
;       Keywords: NONE 
;
;     print (PROCEDURE) - Print all keys currently stored
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE 
;
;     delete (PROCEDURE) - Delete data from the list
;         Inputs: KEY : STRING name denoting the data
;        Outputs: NONE
;       Keywords: NONE 
;
;     clear (PROCEDURE) - Delete all data from the list
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE 
;
;     inList (FUNCTION) - Check if a data item is in the list
;         Inputs: KEY : STRING name denoting the data
;        Outputs: 1 if KEY is in the list, otherwise 0
;       Keywords: NONE 
;
; EXAMPLE:
;
;     ; Create an instance of a data list
;     ;
;     list = OBJ_NEW ('List')
;
;     ; Add some data to the list.  The data names are case-sensitive.
;     ;
;     list->add, 'intData', 1
;     list->add, 'floatData', 1.0
;     list->add, 'structData', { DATA, x: 0, y: PTR_NEW (), z: OBJ_NEW () }
;
;     ; Print the current entries
;     ;
;     list->print
;
;     ; Retrieve some data from the list
;     ;
;     myInt = list->get ('intData')
;
;     ; Retrieve some data from the list, and check for error
;     ;
;     myStruct = list->get ('structData', ERROR = error)
;     IF (error) THEN $
;        PRINT, 'Could not find data in list: structData'
;
;     ; Delete an entry from the list
;     ;
;     list->delete, 'floatData'
;
;     ; All done
;     ;
;     OBJ_DESTROY, list
;
; MODIFICATION HISTORY:
;
;     26 Nov 1999, RSM, changed key names to be case-sensitive
;     Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 


;------------------------------------------------------------------------------
; Constructor
;------------------------------------------------------------------------------
FUNCTION List::init

    RETURN, 1

END


;------------------------------------------------------------------------------
; Destuctor
;------------------------------------------------------------------------------
PRO List::cleanup

    numData = (PTR_VALID (self.data) ? N_ELEMENTS (*self.data) : 0)
    FOR i = 0L, numData - 1 DO BEGIN

        PTR_FREE, (*(*self.data)[i]).data
        PTR_FREE, (*self.data)[i]

    ENDFOR

    PTR_FREE, self.data

END



;------------------------------------------------------------------------------
; Add a data element.  userData can be data of any IDL type.
;------------------------------------------------------------------------------
PRO List::add, key, userData


    ; Ensure request is a string variable
    key = STRING (key)

    str = { USER_DATA, $
        key  : key, $
        data : PTR_NEW (userData) $
    }


    IF (NOT PTR_VALID (self.data)) THEN BEGIN

       self.data = PTR_NEW (PTR_NEW (str))

    ENDIF ELSE BEGIN

       index = self->lookupKey (key)

       IF (index EQ -1) THEN BEGIN
          
	  *self.data = [*self.data, PTR_NEW (str)]

       ENDIF ELSE BEGIN
       
          PTR_FREE, (*(*self.data)[index]).data
          PTR_FREE, (*self.data)[index]
	  (*self.data)[index] = PTR_NEW (str)

       ENDELSE

    ENDELSE

END



;------------------------------------------------------------------------------
; Given a key, find the index into the self.data structure.
; This could be a fairly expensive operation, since it REPEATs until
; the key is found, or until all keys are checked (a loop is
; required, since only scalar pointer dereferences are allowed).
;
; TODO: Implement a hash table?
;------------------------------------------------------------------------------
FUNCTION List::lookupKey, key


    CATCH, keyNotFound
    IF (keyNotFound NE 0) THEN BEGIN
       CATCH, /CANCEL
       RETURN, -1
    ENDIF

    i = -1L
    REPEAT BEGIN

        i = i + 1L
        testKey = (*(*self.data)[i]).key

    ENDREP UNTIL (testKey EQ key)

    CATCH, /CANCEL
    RETURN, i


END



;------------------------------------------------------------------------------
; Print names of all currently stored data.  If list is empty, print nothing.
;------------------------------------------------------------------------------
PRO List::print

    numData = (PTR_VALID (self.data) ? N_ELEMENTS (*self.data) : 0)

    FOR i = 0L, numData - 1 DO $
        PRINT, (*(*self.data)[i]).key
    
END



;------------------------------------------------------------------------------
; Retrieve names of all data elements in list
;------------------------------------------------------------------------------
FUNCTION List::names
    
    s = ''
    IF (NOT PTR_VALID (self.data)) THEN $
       RETURN, s

    FOR i = 0, N_ELEMENTS (*self.data) - 1 DO $
        s = [s, (*(*self.data)[i]).key]   
    s = s[1:*]

    RETURN, s

END


;------------------------------------------------------------------------------
; Retrieve the number of elements in the list
;------------------------------------------------------------------------------
FUNCTION List::count
    
    RETURN, (PTR_VALID (self.data) ? N_ELEMENTS (*self.data) : 0)

END


;------------------------------------------------------------------------------
; Retrieve a list data element.  Sets ERROR to 1 if data not found in list.
;------------------------------------------------------------------------------
FUNCTION List::data, key, ERROR = error, VERBOSE = verbose
    
    error = 0    
       
    ; Ensure request is a string variable
    key = STRING (key)

    index = self->lookupKey (key)

    IF (index NE -1) THEN $
       RETURN, *(*(*self.data)[index]).data

    IF (KEYWORD_SET (verbose)) THEN $
       MESSAGE, /CONTINUE, 'Unknown data: ' + STRTRIM (key, 2)
    error = 1

    RETURN, 0

END


;------------------------------------------------------------------------------
; Delete a data element.  Sets ERROR to 1 if data not found in list.
;------------------------------------------------------------------------------
PRO List::delete, key, ERROR = error, VERBOSE = verbose

    error = 0
    
    ; Ensure request is a string variable
    key = STRING (key)

    numData = (PTR_VALID (self.data) ? N_ELEMENTS (*self.data) : 0)

    index = self->lookupKey (key)

    IF (index NE -1) THEN BEGIN

       PTR_FREE, (*(*self.data)[index]).data
       PTR_FREE, (*self.data)[index]

       IF (numData EQ 1) THEN BEGIN

          PTR_FREE, *self.data 
          PTR_FREE, self.data 
          self.data = PTR_NEW ()

       ENDIF ELSE BEGIN
       
          lookup = INTARR (numData) + 1
          lookup[index] = 0
           
          *self.data = (*self.data)[WHERE (lookup)]

       ENDELSE      

    ENDIF ELSE BEGIN
    
       IF (KEYWORD_SET (verbose)) THEN $
          MESSAGE, /CONTINUE, 'Unknown data: ' + STRTRIM (key, 2)
       error = 1
    
    ENDELSE
       
END


;------------------------------------------------------------------------------
; Delete all data elements
;------------------------------------------------------------------------------
PRO List::clear

    names = self->names ()
    count = self->count ()
    
    FOR i = 0, count - 1 DO $
        self->delete, names[i]

END


;------------------------------------------------------------------------------
; Check if a data element is in the list
;------------------------------------------------------------------------------
FUNCTION List::inList, key
    
    index = self->lookupKey (STRING (key))
    
    RETURN, (index NE -1)
   
END

;------------------------------------------------------------------------------
; Object that stores arbitrary data
;------------------------------------------------------------------------------
PRO List__define

    obj = { LIST, $

        data : PTR_NEW () $

    }


END
