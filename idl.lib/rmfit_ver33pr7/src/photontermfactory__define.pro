; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PhotonTermFactory (OBJECT)
;
; PURPOSE:
;
;     An object to define differential photon flux model terms.  This object
;     should be instantiated only once (singleton pattern).  Other objects 
;     that require access should accept a PhotonTermFactory object reference 
;     in their constructor.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('PhotonTermFactory')
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     NONE
;
; DEPENDENCIES:
;
;     list__define.pro
;
; METHODS:
;     
;     names (FUNCTION) - Return available photon term names
;         Inputs: NONE
;        Outputs: STRARR of available photon term names
;       Keywords: ADDITIVE : Return names of additive terms (DEFAULT) 
;                 MULTIPLICATIVE : Return names of multiplicative terms
;
;     count (FUNCTION) - Return number of available photon terms
;         Inputs: NONE
;        Outputs: LONG containing number of available photon terms
;       Keywords: ADDITIVE : Return number of additive terms (DEFAULT) 
;                 MULTIPLICATIVE : Return number of multiplicative terms
;
;     copy (FUNCTION) - Copy a photon term object
;         Inputs: name - STRING denoting the term name
;        Outputs: A PhotonTerm object containing the term definition
;       Keywords: NONE
;
; MODIFICATION HISTORY:
;
;     Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION PhotonTermFactory::init, INFO_FILENAME = info_filename

    IF (N_ELEMENTS  (info_filename) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: INFO_FILENAME'
       RETURN, 0
    ENDIF
    self.infoFilename = info_filename

    ;== Containers for the builtin photon model terms
    
    self.addTerms = OBJ_NEW ('List')
    self.mulTerms = OBJ_NEW ('List')

    ;== Define the terms, and populate the containers
    
    ;self->initDefaultTerms
            
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PhotonTermFactory::cleanup

    ;== Additive terms
    
    names = self.addTerms->names ()
    n = self.addTerms->count ()
    
    FOR i = 0, n - 1 DO BEGIN
        obj = self.addTerms->data (names[i])
        OBJ_DESTROY, obj
    ENDFOR        
    OBJ_DESTROY, self.addTerms
    
    ;== Multiplicative terms
    
    names = self.mulTerms->names ()
    n = self.mulTerms->count ()
    
    FOR i = 0, n - 1 DO BEGIN
        obj = self.mulTerms->data (names[i])
        OBJ_DESTROY, obj
    ENDFOR        
    OBJ_DESTROY, self.mulTerms
            
END    


; ----------------------------------------------------------------------------
; Return available terms
; ----------------------------------------------------------------------------
FUNCTION PhotonTermFactory::names, $
    ADDITIVE = additive, MULTIPLICATIVE = multiplicative

    IF (KEYWORD_SET (multiplicative)) THEN BEGIN
    
       ;== Multiplicative terms
       
       RETURN, self.mulTerms->names ()
    
    ENDIF ELSE BEGIN
    
       ;== Additive terms
       
       RETURN, self.addTerms->names ()
    
    ENDELSE
       
END


; ----------------------------------------------------------------------------
; Return number of available terms
; ----------------------------------------------------------------------------
FUNCTION PhotonTermFactory::count, $
    ADDITIVE = additive, MULTIPLICATIVE = multiplicative

    IF (KEYWORD_SET (multiplicative)) THEN BEGIN
    
       ;== Multiplicative terms
       
       RETURN, self.mulTerms->count ()
    
    ENDIF ELSE BEGIN
    
       ;== Additive terms
       
       RETURN, self.addTerms->count ()
    
    ENDELSE
       
END


; ----------------------------------------------------------------------------
; Return a copy of a term.  
; NOTE: Caller is responsible for memory management of the returned object.
; ----------------------------------------------------------------------------
FUNCTION PhotonTermFactory::copy, name, DEFAULTS = defaults

    ;== Assume NAME is an additive term
    
    IF (KEYWORD_SET (defaults)) THEN BEGIN
        self->initDefaultTerms
    ENDIF
    
    error = 0
    term = self.addTerms->data (name, ERROR = error)
    
    ;== If error, try a multiplicative term
       
    IF (error) THEN BEGIN
        
       error = 0
       term = self.mulTerms->data (name, ERROR = error)
    
    ENDIF

    IF (error) THEN BEGIN
       MESSAGE, /CONTINUE, 'Unknown term: ' + STRING (name) + '.  Copy failed.'
       RETURN, 0
    ENDIF
    
    ;== Copy the term object
    
    obj = OBJ_NEW ('PhotonTerm')
    
    name   = term->name ()
    index  = term->index ()
    params = term->params ()
    
    obj->setName, name
    obj->setIndex, index
    obj->setParams, params
                  
    RETURN, obj

END


; ----------------------------------------------------------------------------
; Initialize the default model terms
; ----------------------------------------------------------------------------
PRO PhotonTermFactory::initDefaultTerms, ERROR = error

    ;== Photon model term definitions
    ;==
    ;==   Term name
    ;==   Number of parameters used in the term
    ;==   Parameter names, units, default variation status (0=VARY, 1=FIXED),
    ;==   default value, relative change limits, absolute change limit, 
    ;==   allowed sign code (0 = any sign, -1 = negative values only, 
    ;==   +1 = positive values only), batch fit fixing priority (0 = never 
    ;==   held fixed; smaller numbers checked first), relative undetermined 
    ;==   threshold, absolute underdetermined threshold.
    
    OPENR, FL, self.infoFilename, /GET_LUN, ERROR = error
    IF (error NE 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Error reading info file: ' + $
           STRING (self.infoFilename)
       error = 1
       RETURN
    ENDIF

    ON_IOERROR, READ_ERROR
        
    line = ''
    READF, FL, line
    photModelVersion = STRMID (line, 0, 7)
    
    ;== Skip comment lines, read 1 blank line, 
    ;== then read number of additive terms
    
    s = '!!!!'
    WHILE (s EQ '!!!!') DO BEGIN
        READF, FL, line
        s = STRMID (line, 0, 4)
    ENDWHILE

    num_add_terms = 0L
    nt_add_line   = 0L
    READF, FL, num_add_terms
    READF, FL, nt_add_line

    ;== Ensure TERM_DATA struct (defined in the photonTerm object) is defined
    
    term = OBJ_NEW ('PhotonTerm')
    OBJ_DESTROY, term        

    ;== Read each of the additive terms
    
    idx = 1
    WHILE (NOT EOF (FL)) DO BEGIN
        
        READF, FL, line ; blank 
        
        name = ''
        READF, FL, name
        bang = STRPOS (name, '!')
        IF (bang NE -1) THEN name = STRMID (name, 0, bang)
        name = STRTRIM (name, 2)
   
        nparams = 0
        READF, FL, nparams
        
        paramsArr = (params = { TERM_DATA })
                
        FOR j = 0, nparams - 1 DO BEGIN

            d0 = ''
            d1 = ''
            d2 = ''
            d3 = 0.0
            d4 = 0.0
            d5 = 0.0
            d6 = ''
            d7 = 0
            d8 = 0.0
            d9 = 0.0
            
            READF, FL, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, $
                FORMAT = '(A12, 2X, A12, 2X, A1, 2X, F8.0, 2X, F6.0, ' +  $
                         '2X, F6.0, 2X,A1, 2X, I3, 2X, F6.0, 2X, F6.0)'
            
            d0 = STRTRIM (d0, 2)
            d1 = STRTRIM (d1, 2)
            d2 = (STRUPCASE (d2) EQ 'V') ? 0 : 1
            
            CASE (STRUPCASE (d6)) OF
                 'P'  : d6 =  1       ; parameter must be positive
;                 'N'  : d6 = -1       ; parameter must be negative
                 ELSE : d6 =  0       ; parameter not limited
            ENDCASE
                                 
            params = { TERM_DATA, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9}
            paramsArr = [paramsArr, params]
            
        ENDFOR
 
        IF (nparams NE 0) THEN BEGIN

           paramsArr = paramsArr[1:*]
           term = OBJ_NEW ('PhotonTerm', name, idx, paramsArr)        
        
           ; Add the term to the term container
           ; TODO: Multiplicative terms? 
           ;   
           self.addTerms->add, name, term 

        ENDIF

        idx = idx + 1
       
    ENDWHILE
    CLOSE, fl
    FREE_LUN, FL        
    
    RETURN
    
READ_ERROR:

    MESSAGE, /CONTINUE, 'Failed to initialize photon model terms'
    MESSAGE,            'File read error: ' + file

    
END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PhotonTermFactory__define

    obj = { PHOTONTERMFACTORY, $

        ;== MFIT photon term info file

        infoFilename  : '', $ 
        Mfit_TermInfo : OBJ_NEW (),   $

        ;== Containers for available photonTerm objects
         
        addTerms  : OBJ_NEW (), $  ; additive terms
        mulTerms  : OBJ_NEW ()  $  ; multiplicative terms
        
    }

END
