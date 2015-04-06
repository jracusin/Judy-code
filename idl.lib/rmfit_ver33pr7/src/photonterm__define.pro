; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PhotonTerm (OBJECT)
;
; PURPOSE:
;
;     An object to encapsulate a single photon model term.
;
; CALLING SEQUENCE:
;
;     term = OBJ_NEW ('PhotonTerm' [, name, index, params])
;
; INPUTS (optional):
;
;     name   : STRING name of the photon model term
;     index  : unique integer index of the term
;     params : Array of TERM_DATA structures containing the parameter data
;
;     * See object definition at the bottom of this file for the format
;       of the TERM_DATA structure
;
; KEYWORDS:
;
;     NONE
;
; METHODS:
;     
;     setName (PROCEDURE) - Set the term name
;         Inputs: STRING denoting the term name
;        Outputs: NONE 
;       Keywords: NONE
;
;     setParams (PROCEDURE) - Set the term parameter data
;         Inputs: Array of TERM_DATA structures containing the parameter data
;        Outputs: NONE 
;       Keywords: NONE
;
;     name    (FUNCTION) - Return a STRING denoting the term name
;     nparams (FUNCTION) - Return INT number of term parameters
;     params  (FUNCTION) - Return array of TERM_DATA structs of length nparams
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
FUNCTION PhotonTerm::init, name, index, params

    self.params = PTR_NEW ({ TERM_DATA })
    
    IF (N_PARAMS () GE 1) THEN $
       self->setName, name
       
    IF (N_PARAMS () GE 2) THEN $
       self->setIndex, index

    IF (N_PARAMS () GE 3) THEN $
       self->setParams, params

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PhotonTerm::cleanup

    PTR_FREE, self.params
        
END    


; ----------------------------------------------------------------------------
; Set object data
; ----------------------------------------------------------------------------
PRO PhotonTerm::setName, name
    
    self.name = name

END    

PRO PhotonTerm::setIndex, index
    
    self.index = FIX (index)

END    

PRO PhotonTerm::setParams, params
    
    PTR_FREE, self.params
    self.params = PTR_NEW (params)

    self.nparams = FIX (N_ELEMENTS (params))
    
END


; ----------------------------------------------------------------------------
; Return a copy of a photon term.  
; NOTE: Caller is responsible for memory management of the returned object.
; ----------------------------------------------------------------------------
FUNCTION PhotonTerm::copy, name
    
    ;== Copy the photon term object

    obj = OBJ_NEW ('PhotonTerm')
    
    name   = self->name ()
    index  = self->index ()
    params = self->params ()
    
    obj->setName, name
    obj->setIndex, index
    obj->setParams, params
                  
    RETURN, obj

END


; ----------------------------------------------------------------------------
; Return object data
; ----------------------------------------------------------------------------
FUNCTION PhotonTerm::name    & RETURN,  self.name    & END    
FUNCTION PhotonTerm::index   & RETURN,  self.index   & END    
FUNCTION PhotonTerm::nparams & RETURN,  self.nparams & END    
FUNCTION PhotonTerm::params  & RETURN, *self.params  & END    



; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PhotonTerm__define

    param_template = { TERM_DATA, $
    
        name          : '',  $
        units         : '',  $
        fixed         : 0,   $
        value         : 0.0, $
        rel_limits    : 0.0, $
        abs_limit     : 0.0, $
        allowed_sign  : 0,   $
        fix_priority  : 0,   $
        rel_threshold : 0.0, $
        abs_threshold : 0.0  $

    }

    obj = { PHOTONTERM, $

        name    : '', $
        index   : 0,  $
        nparams : 0,  $
        params  : PTR_NEW () $
        
    }

END
