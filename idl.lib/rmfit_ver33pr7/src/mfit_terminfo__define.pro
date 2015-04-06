; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Mfit_TermInfo (OBJECT)
;
; PURPOSE:
;
;     An object to provide an interface to the PhotonTermFactory & MFIT. 
;     As such, it is an ADAPTER pattern, since the termInfo & 
;     PhotonTermFactory terms have different interfaces. We use this so 
;     that the term info is carried around in only one place.
;     This object should be instantiated only once (singleton pattern).  
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('Mfit_TermInfo', INFO_FILENAME = info_filename)
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     INFO_FILENAME : A keyword parameter that has the name of the photon term
;                     information file.
;
; DEPENDENCIES:
;
;     singleton__define.pro
;
; METHODS:
;     
;     setParams (PROCEDURE) - Puts the result of a fit into the default 
;             parameter for the model term, as a starting point for the next fit
;         Inputs: index   - photon term index number
;                 nParams - number of parameters to be set
;                 params  - the new default parameters
;        Outputs: NONE
;       Keywords: NONE
;
;     getInfo (FUNCTION) - Returns the MFIT_termInfo structure
;         Inputs: NONE
;        Outputs: A PhotonTerm object containing the definitions of all terms
;       Keywords: NONE
;
; MODIFICATION HISTORY:
;
;     Written, 2000 August, Rob.Preece@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::init, INFO_FILENAME = info_filename, $
        MAX_TERMS = max_terms,  $
        MAX_PPT = max_ppt
        

    IF (N_ELEMENTS  (info_filename) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: INFO_FILENAME'
       RETURN, 0
    ENDIF
    
    self.infoFilename = info_filename

    ok = self->Singleton::init ()

    ;== Read the photon term info file
    
    self.MAX_TERMS  = MAX_TERMS
    self.MAX_PPT    = MAX_PPT

    self->readInfoFile, ERROR = error, $
        phot_model_ver, num_terms_avail, num_add_terms, nt_add_line,   $
        num_param_of_term, term_names, param_names, param_units,       $
        param_vary_store, param_defaults, rel_change_limit,            $
        abs_change_limit, allowed_sign, undet_priority, undet_rel_req, $
        undet_abs_req
    
    IF (error) THEN BEGIN
       MESSAGE, /CONTINUE, 'Error reading function info file.'
       RETURN, 0
    ENDIF
    
    self.terminfo.phot_model_ver    = phot_model_ver
    self.terminfo.num_terms_avail   = num_terms_avail
    self.terminfo.num_add_terms     = num_add_terms
    self.terminfo.nt_add_line       = nt_add_line
    self.terminfo.num_param_of_term = PTR_NEW (num_param_of_term)    
    self.terminfo.term_names        = PTR_NEW (term_names)
    self.terminfo.param_names       = PTR_NEW (param_names)
    self.terminfo.param_units       = PTR_NEW (param_units)
    self.terminfo.param_vary_store  = PTR_NEW (param_vary_store)
    self.terminfo.param_defaults    = PTR_NEW (param_defaults)
    self.paramDefaults              = PTR_NEW (param_defaults)
    self.pvaryDefaults              = PTR_NEW (param_vary_store)
    self.terminfo.rel_change_limit  = PTR_NEW (rel_change_limit)
    self.terminfo.abs_change_limit  = PTR_NEW (abs_change_limit)
    self.terminfo.allowed_sign      = PTR_NEW (allowed_sign)
    self.terminfo.undet_priority    = PTR_NEW (undet_priority)
    self.terminfo.undet_rel_req     = PTR_NEW (undet_rel_req)
    self.terminfo.undet_abs_req     = PTR_NEW (undet_abs_req)

    
    RETURN, ok

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Mfit_TermInfo::cleanup

    STRUCT_FREE, self.terminfo  
    PTR_FREE, self.paramDefaults
    PTR_FREE, self.pvaryDefaults

    self->Singleton::cleanup
        
END    


; ----------------------------------------------------------------------------
; Read the function info file.
; This method reads in the description of the available model terms
; and their parameters from the MFIT function info file.
; ----------------------------------------------------------------------------
PRO Mfit_TermInfo::readInfoFile, ERROR = error, $
    PHOT_MODEL_VER, NUM_TERMS_AVAIL, NUM_ADD_TERMS, NT_ADD_LINE, $
    NUM_PARAM_OF_TERM, TERM_NAMES, PARAM_NAMES, PARAM_UNITS, $
    PARAM_VARY_STORE, PARAM_DEFAULTS, REL_CHANGE_LIMIT, $
    ABS_CHANGE_LIMIT, ALLOWED_SIGN, UNDETERMINE_PRIORITY, $
    UNDETERMINE_REL_REQ, UNDETERMINE_ABS_REQ
    
    error = 0
    
    TERM_NAMES = REPLICATE (STRING (REPLICATE (32B, 28)), self.MAX_TERMS)

    NUM_PARAM_OF_TERM = LONARR (self.MAX_TERMS)

    PARAM_NAMES = REPLICATE (STRING (REPLICATE (32B, 12)), $
        self.MAX_TERMS, self.MAX_PPT)

    PARAM_UNITS = REPLICATE (STRING (REPLICATE (32B, 12)), $
        self.MAX_TERMS, self.MAX_PPT)

    PARAM_VARY_STORE = REPLICATE (STRING (REPLICATE (32B, 1)), $
        self.MAX_TERMS, self.MAX_PPT)

    PARAM_DEFAULTS       = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    REL_CHANGE_LIMIT     = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    ABS_CHANGE_LIMIT     = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    UNDETERMINE_PRIORITY = LONARR (self.MAX_TERMS, self.MAX_PPT)
    UNDETERMINE_REL_REQ  = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    UNDETERMINE_ABS_REQ  = FLTARR (self.MAX_TERMS, self.MAX_PPT)

    ALLOWED_SIGN = REPLICATE (STRING (REPLICATE (32B, 1)), $
        self.MAX_TERMS, self.MAX_PPT)

    PHOTON_MODEL_VER    = STRING (REPLICATE (32B, 5))
    NUM_TERMS_AVAIL     = 0L
    NUM_ADD_TERMS       = 0L
    HIGHEST_UD_PRIORITY = 0L
    NT_ADD_LINE         = 0L
    SPARE_TERM          = LONARR (self.MAX_TERMS)


    ; Open file and read header information

    OPENR, FL, self.infoFilename, /GET_LUN, ERROR = error
    IF (error NE 0) THEN BEGIN
       MESSAGE, /CONT, 'Error reading info file: ' + STRING (self.infoFilename)
       error = 1
       RETURN
    ENDIF
           
    TEMP_INT = 0L
    TEMP_STR = ''
    READF, FL, TEMP_STR
    PHOT_MODEL_VER = STRMID (TEMP_STR, 0, 5)

    ; Skip comment lines, read 1 blank line, 
    ; then read number of additive terms
    ;
    s = '!!!!'
    WHILE (s EQ '!!!!') DO BEGIN
        READF, FL, TEMP_STR
        s = STRMID (TEMP_STR, 0, 4)
    ENDWHILE

    READF, FL, NUM_ADD_TERMS
    READF, FL, NT_ADD_LINE

    HIGHEST_UD_PRIORITY = 0L

    ; Read in term name and number of parameters in term

    JTERM = 0                          ; count the terms as they are read
    WHILE NOT EOF(FL) DO BEGIN

        READF, FL, s                 ; skip a blank line
        READF, FL, TEMP_STR & TERM_NAMES[JTERM] = STRMID (TEMP_STR, 0, 28)

        READF, FL, TEMP_INT & NUM_PARAM_OF_TERM[JTERM] = TEMP_INT

        IF (NUM_PARAM_OF_TERM[JTERM] GT self.MAX_PPT) THEN BEGIN
           MESSAGE, /CONTINUE, 'NUM_PARAM_OF_TERM = ' + $
               STRTRIM (NUM_PARAM_OF_TERM[JTERM], 2) + $
               ' for term ' + STRTRIM (JTERM, 2)   
           MESSAGE, /CONTINUE, 'Max allowed is MAX_PPT = ' + $
               STRTRIM (self.MAX_PPT, 2)
           error = 1
           RETURN
        ENDIF

        ; If the number of parameters is listed in the function information 
        ; file as 0, that means that the model is a "spare" or "dummy" 
        ; model that has not yet been implemented.  Mark spare terms as 
        ; such; for other terms read in the information about their 
        ; parameters

        IF (NUM_PARAM_OF_TERM[JTERM] LE 0) THEN $
           SPARE_TERM[JTERM] = 1L

        ; Read info for each parameter of this term

        FOR KPARAM = 0, NUM_PARAM_OF_TERM[JTERM] - 1 DO BEGIN

            READF, FL, TEMP_STR
            PARAM_NAMES[JTERM, KPARAM] = STRMID (TEMP_STR, 0, 12)
            PARAM_UNITS[JTERM, KPARAM] = STRMID (TEMP_STR, 14, 12)
            TEMP_STR = STRMID (TEMP_STR, 28, 999)
            WORDARRAY, TEMP_STR, TEMP_ARR
            PARAM_VARY_STORE[JTERM, KPARAM] = TEMP_ARR[0]
            PARAM_DEFAULTS[JTERM, KPARAM]   = TEMP_ARR[1]
            REL_CHANGE_LIMIT[JTERM, KPARAM] = TEMP_ARR[2]
            ABS_CHANGE_LIMIT[JTERM, KPARAM] = TEMP_ARR[3]

            IF (N_ELEMENTS (TEMP_ARR) EQ 8) THEN BEGIN
               ALLOWED_SIGN[JTERM, KPARAM] = TEMP_ARR[4]
               SHIFT = 1
            ENDIF ELSE BEGIN
               SHIFT = 0
            ENDELSE

            UNDETERMINE_PRIORITY[JTERM, KPARAM] = TEMP_ARR[4 + SHIFT] 
            UNDETERMINE_REL_REQ[JTERM, KPARAM]  = TEMP_ARR[5 + SHIFT]
            UNDETERMINE_ABS_REQ[JTERM, KPARAM]  = TEMP_ARR[6 + SHIFT]

            HIGHEST_UD_PRIORITY = $
              MAX ([HIGHEST_UD_PRIORITY, UNDETERMINE_PRIORITY[JTERM, KPARAM]])

            ALLOWED_SIGN[JTERM, KPARAM] = $
                STRUPCASE (STRTRIM (ALLOWED_SIGN[JTERM, KPARAM], 2))
            PARAM_VARY_STORE[JTERM, KPARAM] = $
                STRUPCASE (STRTRIM (PARAM_VARY_STORE[JTERM, KPARAM], 2))

            IF (STRMID (ALLOWED_SIGN[JTERM, KPARAM], 0, 1) NE 'P') THEN $
               ALLOWED_SIGN[JTERM, KPARAM] = ' '
            IF (STRMID (PARAM_VARY_STORE[JTERM, KPARAM], 0,1) NE 'F') THEN $
               PARAM_VARY_STORE[JTERM, KPARAM] = 'V'

        ENDFOR
        
        ;== Handle the 'Effective Area Correction' term
        IF (JTERM EQ (NUM_ADD_TERMS)) THEN BEGIN
            NUM_PARAM_OF_TERM[JTERM] = 0
        ENDIF

        JTERM = JTERM + 1

    ENDWHILE ; next term

    CLOSE, fl
    FREE_LUN, FL

    ; Number of model terms available
    NUM_TERMS_AVAIL = LONG (JTERM)              

    IF (NUM_TERMS_AVAIL GT self.MAX_TERMS) THEN BEGIN
       MESSAGE, /CONTINUE, 'NUM_TERMS_AVAIL > MAX_TERMS'
       error = 1
       RETURN
    ENDIF


END


; ----------------------------------------------------------------------------
; Adjust the Effective Area Correction term to reflect the number of detectors
; ----------------------------------------------------------------------------
PRO Mfit_TermInfo::adjustEffCorrTerm, numDets

	numParamsOfTerm = numDets - 1
	IF (numParamsOfTerm LT 0) THEN RETURN
	index = self.terminfo.num_add_terms
	(*self.terminfo.num_param_of_term)[index] = numParamsOfTerm

END


; ----------------------------------------------------------------------------
; Update the parameter values from a recent fit
; ----------------------------------------------------------------------------
PRO Mfit_TermInfo::setParams, index, nParams, params, paramVary

    (*self.terminfo.param_defaults)[index - 1, 0: nparams - 1] = params[0: nparams - 1] 

    jj = WHERE (paramVary[0: nparams - 1] EQ 1, co)
    IF (co GT 0) THEN $
        (*self.termInfo.param_vary_store)[index - 1, jj] = 'V'
    kk = WHERE (paramVary[0: nparams - 1] EQ 0, co)
    IF (co GT 0) THEN $
        (*self.termInfo.param_vary_store)[index - 1, kk] = 'F'
        
END    


; ----------------------------------------------------------------------------
; Extract informational notes from an MFIT_DETECTOR structure
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::getInfo

    RETURN, self.termInfo
        
END


; ----------------------------------------------------------------------------
; Return available terms
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::names, $
    ADDITIVE = additive, MULTIPLICATIVE = multiplicative

    IF (KEYWORD_SET (multiplicative)) THEN BEGIN
    
       ;== Multiplicative terms
       
       ;RETURN, self.mulTerms->names ()
       RETURN, 0
    
    ENDIF ELSE BEGIN
    
       ;== Additive terms
       
       ;RETURN, self.addTerms->names ()
       RETURN, *self.termInfo.term_names
    
    ENDELSE
       
END


; ----------------------------------------------------------------------------
; Return number of parameters for each term
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::num_param_of_term

    RETURN, *self.termInfo.num_param_of_term
       
END


; ----------------------------------------------------------------------------
; Return number of available terms
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::count, $
    ADDITIVE = additive, MULTIPLICATIVE = multiplicative

    IF (KEYWORD_SET (multiplicative)) THEN BEGIN
    
       ;== Multiplicative terms
       
       ;RETURN, self.mulTerms->count ()
       RETURN, 0
    
    ENDIF ELSE BEGIN
    
       ;== Additive terms
       
       ;RETURN, self.addTerms->count ()
       RETURN, self.termInfo.num_terms_avail
    
    ENDELSE
       
END


; ----------------------------------------------------------------------------
; Return a copy of a term.  
; NOTE: Caller is responsible for memory management of the returned object.
; ----------------------------------------------------------------------------
FUNCTION Mfit_TermInfo::copy, name, DEFUALTS = defaults

    ;== Assume NAME is an additive term
    error = 0

    names = self->names()
    index = WHERE (STRTRIM(names, 2) EQ STRTRIM(name, 2), error)

    IF (error EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Unknown term: ' + STRING (name) + '.  Copy failed.'
       RETURN, 0
    ENDIF
    
   index = index[0]
  
    ;== Copy the term object
    
    obj = OBJ_NEW ('PhotonTerm')
               
    paramName     = (*self.termInfo.param_names)[index, *]
    units         = (*self.termInfo.param_units)[index, *]
    ;fixed         = (*self.termInfo.param_vary_store)[index, *]
    IF (KEYWORD_SET (defaults)) THEN BEGIN
        value     = (*self.paramDefaults)[index, *]
        fixed     = (*self.pvaryDefaults)[index, *]
    ENDIF ELSE BEGIN
        value     = (*self.termInfo.param_defaults)[index, *]
        fixed     = (*self.termInfo.param_vary_store)[index, *]
    ENDELSE
    rel_limits    = (*self.termInfo.rel_change_limit)[index, *]
    abs_limit     = (*self.termInfo.abs_change_limit)[index, *]
    allowed_sign  = (*self.termInfo.allowed_sign)[index, *]
    ;==TBD: get these into MFIT!
    fix_priority  = 0L
    rel_threshold = 0.0
    abs_threshold = 0.0

  RESTORE, !MFIT.script_file                                                                     ; AMG 
  IF (script) AND (N_ELEMENTS(vals) GT 1) THEN BEGIN                                             ;
      IF (SIZE(vals))[N_ELEMENTS(SIZE(vals))-2] EQ 10 THEN vals=*vals[numfits]                   ;
      value = vals[0,*]                                                                          ;
      fixedV = vals[1,*]                                                                         ;
      vIndex=WHERE(fixedV EQ 0)                                                                  ;
      fIndex=WHERE(fixedV EQ 1)                                                                  ;
      fixed[0,vIndex]='V'                                                                        ;
      IF fIndex[0] NE -1 THEN fixed[0,fIndex]='F'                                                ;
  ENDIF                                                                                          ;
        
    paramsArr = (params = { TERM_DATA })
    
    nparams = (*self.termInfo.num_param_of_term)[index]
	FOR j = 0, nparams - 1 DO BEGIN
	    params = { TERM_DATA, paramName[j], units[j], $
	                  (STRUPCASE (fixed[j]) EQ 'V') ? 0L : 1L, $
	                  value[j], rel_limits[j], abs_limit[j], $
	                  (STRUPCASE (allowed_sign[j]) EQ 'P') ? 1L : 0L, $
	                  fix_priority, $
	                  rel_threshold, abs_threshold }
	    paramsArr = [paramsArr, params]
	    
	ENDFOR
	
	
	IF (nparams GT 0) THEN paramsArr = paramsArr[1: *]

    obj->setName, name
    obj->setIndex, index + 1
    obj->setParams, paramsArr
                  
    RETURN, obj

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Mfit_TermInfo__define

    tmpl = { TERMINFO, $
                     
        phot_model_ver    : '',         $
        num_terms_avail   : 0L,         $
        num_add_terms     : 0L,         $
        nt_add_line       : 0L,         $

        num_param_of_term : PTR_NEW (), $ 
        term_names        : PTR_NEW (), $
        param_names       : PTR_NEW (), $
        param_units       : PTR_NEW (), $
        param_vary_store  : PTR_NEW (), $
        param_defaults    : PTR_NEW (), $
        rel_change_limit  : PTR_NEW (), $
        abs_change_limit  : PTR_NEW (), $
        allowed_sign      : PTR_NEW (), $
        undet_priority    : PTR_NEW (), $
        undet_rel_req     : PTR_NEW (), $
        undet_abs_req     : PTR_NEW ()  $
    
    }

    obj = { MFIT_TERMINFO, INHERITS SINGLETON, $
                     
        infoFilename      : '',           $
        termInfo          : { TERMINFO }, $ 
        paramDefaults     : PTR_NEW (),   $     ; Defaults from info file
        pvaryDefaults     : PTR_NEW (),   $     ; Default value: 'V' or 'F'
        termFactory       : OBJ_NEW (),   $     ; The photon termfactory
        MAX_TERMS         : 0L,           $
        MAX_PPT           : 0L            $
    }

END
