; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PhotonModel
;
; PURPOSE:
;
;     Interactively build a photon model, which consists of one more
;     more additive and/or multiplicative photon terms.
;
;     CURRENTLY ONLY A SINGLE PHOTON TERM IS SUPPORTED.
;
; CALLING SEQUENCE:
;
;     o = OBJ_NEW ('PhotonModel' [, /SELECT])
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     SELECT : Set this keyword to call the SELECT method when
;              the object is instantiated.
;
; INHERITS:
;
;     NONE
;
; RESTRICTIONS:
;
;     Currently will handle only a single photon additive term
;
; DEPENDENCIES:
;
;     NONE
;
; METHODS:
;     
;     select (PROCEDURE) - Choose photon model terms
;         Inputs : NONE
;        Outputs : NONE
;       Keywords : NONE
;
;     clear (PROCEDURE) - Clear the current model
;         Inputs : NONE
;        Outputs : NONE
;       Keywords : NONE
;
;     model (FUNCTION) - Return the current model
;         Inputs : NONE
;        Outputs : A PhotonTerm object of the current model. If only one
;                  photon term has been selected for the model, the return
;                  value will be a scalar object reference, otherwise an
;                  OBJARR is returned.
;                  NOTE: A copy of the model is return, not just a reference.
;                  The caller is responsible for memory management of the 
;                  returned object(s).
;       Keywords : NONE
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
FUNCTION PhotonModel::init, SELECT = select, TERM_INFO = term_info_ref

    self.termFactory = term_info_ref

    ;== Initialize the omitted terms array

    self.omittedTerms = PTR_NEW (!RM_PARAMS.photterms)
    
    ;== Initialize the parameter-settings choice to make it persistent
    
    self.choice = 0L
    self.fixchoice = 1L
    self.statchoice = 0L
    
    ;== Initialize storage for the user's previous selection

    self.selectedTerms = PTR_NEW ()

    ;== List for the photon model terms
    
    self.termList = OBJ_NEW ('List')
        
    IF (KEYWORD_SET (select)) THEN $
       self->select
        
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PhotonModel::cleanup

    PTR_FREE, self.paramID
    PTR_FREE, self.fixedID
    
;    !PHOTTERMS = *self.omittedTerms
    !RM_PARAMS.photterms = *self.omittedTerms
    PTR_FREE, self.omittedTerms
    PTR_FREE, self.selectedTerms

    names = self.termList->names ()
    n = self.termList->count ()    
    
    FOR i = 0, n - 1 DO BEGIN
        obj = self.termList->data (names[i])
        OBJ_DESTROY, obj
    ENDFOR        
    OBJ_DESTROY, self.termList

END    

; ----------------------------------------------------------------------------
PRO PhotonModel::invalidateSelection

    IF PTR_VALID (self.selectedTerms) THEN BEGIN
         PTR_FREE, self.selectedTerms
         self.selectedTerms = PTR_NEW ()
    ENDIF

END
   
; ----------------------------------------------------------------------------
; The following procedure forwards the event structure to the class handler,
; since one cannot currently (IDL v5.2) define a class method as an
; event handler. 
; ----------------------------------------------------------------------------
PRO PhotonModel_selectModel_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = state
    (*state).self->selectModel_event, event

END
; ----------------------------------------------------------------------------
PRO PhotonModel::selectModel_event, event

    
    WIDGET_CONTROL, event.top, GET_UVALUE = state
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF
            
                'Cancel': BEGIN

                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 
            
                'Omit': BEGIN
                    
                    ;== Take some of the photon models out of the list.
                    ;We use a system variable to make this persistent 
                    ;during this session, and a parameter (save) file 
                    ;so that it is persistent across sessions.
                    selected = WIDGET_INFO (self.termListID, /LIST_SELECT)
                    IF (selected[0] EQ -1 ) THEN RETURN

                    names = self.termFactory->names ()
                    nNames = N_ELEMENTS (names)    
                    omitArr = LONARR (nNames)            

                    f = '(I' + STRING (STRLEN (STRTRIM (nNames, 2))) + ')'    
                    prefix = STRING (STRTRIM (INDGEN (nNames) + 1, 2), FORMAT = f) + ' '
                    listElements = prefix + names
                    
                    nParams = self.termFactory->num_param_of_term ()
                    nonZeroParms = WHERE ((nParams GT 0) AND $
                                   (*self.omittedTerms EQ 0))
                    selectedElements  = listElements[nonZeroParms]
                    selectedElements = selectedElements[selected]       
                    nNames = N_ELEMENTS (selectedElements)       
                    nIdx = 0L         
                    FOR k = 0, nNames - 1 DO BEGIN
                         READS, selectedElements[k], nIdx
                         omitArr[nIdx - 1] = 1L
                    ENDFOR
                    
                    omitArr = (omitArr OR *self.omittedTerms)
                    PTR_FREE, self.omittedTerms
                    self.omittedTerms = PTR_NEW (omitArr)

                    nonZeroParms = WHERE ((nParams GT 0) AND $
	                           (*self.omittedTerms EQ 0))
                    names  = listElements[nonZeroParms]
	            
                    ;== Saved use selection is likely to be invalid
                    self->invalidateSelection

                    WIDGET_CONTROL, self.termListID, SET_VALUE = names
                    RETURN
                    END 
            
                'Restore': BEGIN
                    
                    ;== Allow the user to rebuild the full list of names.
                    names = self.termFactory->names ()
                    nNames = N_ELEMENTS (names)    
                    omitArr = LONARR (nNames)            
                    PTR_FREE, self.omittedTerms
                    self.omittedTerms = PTR_NEW (omitArr)
                    nParams = self.termFactory->num_param_of_term ()
                    nonZeroParms = WHERE ((nParams GT 0) AND $
	                           (*self.omittedTerms EQ 0))
                    f = '(I' + STRING (STRLEN (STRTRIM (nNames, 2))) + ')'    
                    prefix = STRING (STRTRIM (INDGEN (nNames) + 1, 2), FORMAT = f) + ' '
                    listElements = prefix + names
                    listElements  = listElements[nonZeroParms]

                    ;== Saved use selection is likely to be invalid
                    self->invalidateSelection

                    WIDGET_CONTROL, self.termListID, SET_VALUE = listElements
                    RETURN
                    END 
            
                'Accept': BEGIN
                
                    selected = WIDGET_INFO (self.termListID, /LIST_SELECT)
                    IF (selected[0] EQ -1 ) THEN RETURN
                    
                    ;== Eliminate models user has marked as 'omit'
                    names = self.termFactory->names ()
                    nParams = self.termFactory->num_param_of_term ()
                    nonZeroParms = WHERE ((nParams GT 0) AND $
	                           (*self.omittedTerms EQ 0))
                    names = names[nonZeroParms]
                    names = names[selected]                    
                    PTR_FREE, self.selectedTerms
                    self.selectedTerms = PTR_NEW (selected)   
                    
                    ;== Determine which fitting statistic to use, make the 
                    ;choice persistent.
                    WIDGET_CONTROL, self.statisticID, GET_VALUE = doStat
                    self.statchoice = doStat
                    
                    ;== Determine if undetermined parameters are to be fixed in batch fit, 
                    ;make the choice persistent.
                    WIDGET_CONTROL, self.fixUndetID, GET_VALUE = doFixUnd
                    self.fixchoice = doFixUnd
                    
                    ;== Determine if parameters are to be set, make the 
                    ;choice persistent.
                    WIDGET_CONTROL, self.useDefaultTermsID, GET_VALUE = doSet
                    self.choice = doSet
                    IF (doSet) THEN BEGIN

                       self->selectTerms, event.top, names
                    
                    ENDIF ELSE BEGIN

                       nNames = N_ELEMENTS (names)

                       FOR i = 0, nNames - 1 DO BEGIN

                           term = self.termFactory->copy (names[i])
                           self.termList->add, names[i], term

                       ENDFOR
                    
                    ENDELSE
                                          
                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 

                ELSE:
            
            ENDCASE 
            END
                
        ELSE: 
                            
    ENDCASE
    
END
            

;------------------------------------------------------------------------------
; Build model selection dialog
;------------------------------------------------------------------------------
PRO PhotonModel::select, FIT_STAT = fit_stat


RESTORE, !MFIT.script_file                                                                         ; AMG
IF script THEN BEGIN                                                                               ;
  IF (statistic NE 0) THEN self.statchoice=(fit_stat=statistic)                                    ;
  IF (SIZE(model))[N_ELEMENTS(SIZE(model))-2] EQ 10 THEN names=(*model[numfits])[*] ELSE $         ;
  names=model[*, numfits]                                                                          ;
  nNames=N_ELEMENTS(names)                                                                         ;
  FOR i = 0, nNames - 1 DO BEGIN                                                                   ;
      IF (SIZE(model))[N_ELEMENTS(SIZE(model))-2] EQ 10 THEN BEGIN                                 ;
        term = self.termFactory->copy ((*model[numfits])[i])                                       ;
        self.termList->add, (*model[numfits])[i], term                                             ;
      ENDIF ELSE BEGIN                                                                             ;
        term = self.termFactory->copy (model[i,numfits])                                           ;
        self.termList->add, model[i, numfits], term                                                ;
      ENDELSE                                                                                      ;
        term = self.termFactory->copy (names[i], /DEFUALTS)                                        ;
        name    = term->name ()                                                                    ;
        nParams = term->nparams ()                                                                 ;
        params  = term->params ()                                                                  ;
        term->setParams, params                                                                    ;
        self.termList->add, names[i], term                                                         ;
  ENDFOR                                                                                           ;
  RETURN                                                                                           ;
ENDIF                                                                                              ;


    IF (KEYWORD_SET (fit_stat)) THEN $
       self.statchoice = fit_stat
       
    topBase = WIDGET_BASE (TITLE = 'PhotonModel', /COLUMN, /BASE_ALIGN_CENTER)

    label = WIDGET_LABEL (topBase, VALUE = 'Select one or more photon model terms')

    names = self.termFactory->names ()
    nNames = N_ELEMENTS (names)
        
    f = '(I' + STRING (STRLEN (STRTRIM (nNames, 2))) + ')'    
    prefix = STRING (STRTRIM (INDGEN (nNames) + 1, 2), FORMAT = f) + ' '
    
    listElements = prefix + names
        
    nParams = self.termFactory->num_param_of_term ()
    nonZeroParms = WHERE ((nParams GT 0) AND $
	                           (*self.omittedTerms EQ 0))
    listElements  = listElements[nonZeroParms]
    nNames = N_ELEMENTS (listElements)
       
    self.termListID = WIDGET_LIST (topBase, VALUE = listElements, $
            YSIZE = MIN ([nNames, 15]), /MULTIPLE)
    ;== Use the saved values for the next run
    IF PTR_VALID (self.selectedTerms) THEN BEGIN
        WIDGET_CONTROL, self.termListID, SET_LIST_SELECT = *self.selectedTerms
    ENDIF
    
    label = WIDGET_LABEL (topBase, VALUE = 'Photon Model Parameters:')
    choose = ['Keep current', 'Set parameters']
    self.useDefaultTermsID = CW_BGROUP (topBase, choose, /ROW, $
        /EXCLUSIVE, SET_VALUE = self.choice)
    
    statlabel = WIDGET_LABEL (topBase, VALUE = 'Fitting Statistic:')
    statchoose = ['Chi^2', 'Likelihood', 'C-Stat']
    self.statisticID = CW_BGROUP (topBase, statchoose, /ROW, $
        /EXCLUSIVE, SET_VALUE = self.statchoice)
    
    label = WIDGET_LABEL (topBase, VALUE = 'Undetermined Values in Batch Fit:')
    choose = ['Leave Free', 'Automatically Fix']
    self.fixUndetID = CW_BGROUP (topBase, choose, /ROW, $
        /EXCLUSIVE, SET_VALUE = self.fixchoice)

    gridBase = WIDGET_BASE (topBase, /GRID, COLUMN = 4, SPACE = 10)    
    button = WIDGET_BUTTON (gridBase, VALUE = 'Accept')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Omit')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Restore')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Cancel')
    
    ;== Store the object reference on the display widget
    
    state = PTR_NEW ({ self: self })
    WIDGET_CONTROL, topBase, SET_UVALUE = state
    WIDGET_CONTROL, topBase, /REALIZE

    ;== Start event loop
    
    XMANAGER, OBJ_CLASS (self) + '_selectModel', topBase ; , /NO_BLOCK

END


; ----------------------------------------------------------------------------
; The following procedure forwards the event structure to the class handler,
; since one cannot currently (IDL v5.2) define a class method as an
; event handler. 
; ----------------------------------------------------------------------------
PRO PhotonModel_selectTerms_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = state
    (*state).self->selectTerms_event, event

END
; ----------------------------------------------------------------------------
PRO PhotonModel::selectTerms_event, event


    WIDGET_CONTROL, event.top, GET_UVALUE = state
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF
            
                'Cancel': BEGIN

                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 
            
                'Defaults': BEGIN

                    names  = (*state).names
                    nNames = N_ELEMENTS (names)

                    FOR i = 0, nNames - 1 DO BEGIN

                        term = self.termFactory->copy (names[i], /DEFUALTS)

                        name    = term->name () 
                        nParams = term->nparams ()        
                        params  = term->params ()

                        OBJ_DESTROY, term

                        FOR j = 0, nParams - 1 DO BEGIN

                            WIDGET_CONTROL, (*self.paramID)[i, j], $
                                SET_VALUE = STRING ( $
                                    params[j].value, F = '(g10.5)') ;f8.2

                            WIDGET_CONTROL, (*self.fixedID)[i, j], $
                                SET_VALUE = params[j].fixed

                        ENDFOR

                    ENDFOR
                    END
                      
                'Accept': BEGIN

                    names  = (*state).names
                    nNames = N_ELEMENTS (names)
                    FOR i = 0, nNames - 1 DO BEGIN

                        term = self.termFactory->copy (names[i])
                    
                        nparams = term->nparams ()
                        params  = term->params ()
                        
                        FOR j = 0, nparams - 1 DO BEGIN

                            WIDGET_CONTROL, (*self.paramID)[i, j], $
                                GET_VALUE = value
                            params[j].value = FLOAT (value)
                            
                            WIDGET_CONTROL, (*self.fixedID)[i, j], $
                                GET_VALUE = value
                            params[j].fixed = FIX (value)
                                
                        ENDFOR
                        
                        term->setParams, params
                        self.termList->add, names[i], term
                         
                    ENDFOR 
                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 

                ELSE:
            
            ENDCASE 
            END
        
        ELSE: 
                            
    ENDCASE
    
END


;------------------------------------------------------------------------------
; Build photon term parameter selection dialog
;------------------------------------------------------------------------------
PRO PhotonModel::selectTerms, leader, names

    topBase = WIDGET_BASE (TITLE = 'PhotonModel: Set Term Parameters', $
        /COLUMN, /MODAL, GROUP_LEADER = leader, /BASE_ALIGN_CENTER)

    label = WIDGET_LABEL (topBase, $
        VALUE = 'Set parameter values for photon model term')
                      
    nNames = N_ELEMENTS (names)

; TODO: Remove maxParams restriction
    maxParams = 20     

    PTR_FREE, self.paramID
    self.paramID = PTR_NEW (LONARR (nNames, maxParams))
    
    PTR_FREE, self.fixedID
    self.fixedID = PTR_NEW (LONARR (nNames, maxParams))
        
    FOR i = 0, nNames - 1 DO BEGIN

        term = self.termFactory->copy (names[i])
        
        name    = term->name () 
        nParams = term->nparams ()        
        params  = term->params ()
        
        OBJ_DESTROY, term

        ;label = WIDGET_LABEL (topBase, VALUE = ' ') ;Uses too much space!
        label = WIDGET_LABEL (topBase, VALUE = name)
        
        FOR j = 0, nParams - 1 DO BEGIN

            base  = WIDGET_BASE (topBase, COLUMN = 3, /GRID)
            label = WIDGET_LABEL (base, VALUE = params[j].name, /ALIGN_RIGHT)
            (*self.paramID)[i, j] = WIDGET_TEXT (base, /EDIT, $
                VALUE = STRING (params[j].value, F = '(g10.5)'), XSIZE = 8)  ;f8.2
          
            choose1 = ['Vary', 'Fixed']
            (*self.fixedID)[i, j] = CW_BGROUP (base, choose1, $
                /ROW, /EXCLUSIVE, SET_VALUE = params[j].fixed)

        ENDFOR
        

    ENDFOR

    gridBase = WIDGET_BASE (topBase, /GRID, COLUMN = 3, SPACE = 20)
    button = WIDGET_BUTTON (gridBase, VALUE = 'Accept')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Cancel')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Defaults')
    
    ;== Store the object reference on the display widget
    
    state = PTR_NEW ({ self: self, names: names })
    WIDGET_CONTROL, topBase, SET_UVALUE = state
    WIDGET_CONTROL, topBase, /REALIZE

    ;== Start event loop
    
    XMANAGER, OBJ_CLASS (self) + '_selectTerms', topBase  ; , /NO_BLOCK
    

END

    
; ----------------------------------------------------------------------------
; The following procedure forwards the event structure to the class handler,
; since one cannot currently (IDL v5.2) define a class method as an
; event handler. 
; ----------------------------------------------------------------------------
PRO PhotonModel_selectParams_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = state
    (*state).self->selectParams_event, event

END
; ----------------------------------------------------------------------------
PRO PhotonModel::selectParams_event, event


    WIDGET_CONTROL, event.top, GET_UVALUE = state
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF
            
                'Cancel': BEGIN

                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 
            
                'Accept': BEGIN

                    names  = (*state).names
                    nNames = N_ELEMENTS (names)
                    selection = (*state).selection
                    ranges = (*state).ranges
                    popup1 = (*state).popup1
                    displaySigma = WIDGET_INFO(popup1, /DROPLIST_SELECT)
                    popup2 = (*state).popup2
                    numInterest = WIDGET_INFO(popup2, /DROPLIST_SELECT)
                    
                    maxParams = 20     

                    gotSelection = INTARR (nNames, maxParams)
                    gotRange = FLTARR (nNames, maxParams, 2)
                    
                    FOR i = 0, nNames - 1 DO BEGIN

                        term = self.termFactory->copy (names[i])
                    
                        nparams = term->nparams ()
                        params  = term->params ()
                        
                        OBJ_DESTROY, term
                        FOR j = 0, nparams - 1 DO BEGIN
                        
                            ;== We need to check each ID, since we are not
                            ;creating the full widget sometimes...

                            IF (selection[i, j] NE 0l) THEN BEGIN
                                WIDGET_CONTROL, selection[i, j], $
                                    GET_VALUE = value, BAD_ID = bad
                                IF (bad EQ 0l) THEN gotSelection[i, j] = value
                            ENDIF
                            
                            IF (ranges[i, j, 0] NE 0l) THEN BEGIN
                                WIDGET_CONTROL, ranges[i, j, 0], $
                                    GET_VALUE = value1, BAD_ID = bad
                                IF (bad EQ 0l) THEN gotRange[i, j, 0] = value1
                            ENDIF

                            IF (ranges[i, j, 1] NE 0l) THEN BEGIN
                                WIDGET_CONTROL, ranges[i, j, 1], $
                                    GET_VALUE = value2, BAD_ID = bad
                                IF (bad EQ 0l) THEN gotRange[i, j, 1] = value2
                            ENDIF
                                
                                
                        ENDFOR
                    ENDFOR 
                        
                    ;== Neat way to pass data back from a widget:
                    *(*state).mySelect = gotSelection
                    *(*state).myRanges = gotRange
                    *(*state).displaySigma = displaySigma
                    *(*state).numInterest = numInterest
                         
                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 

                ELSE:
            
            ENDCASE 
            END
        
        ELSE: 
                            
    ENDCASE
    
END


;------------------------------------------------------------------------------
; Build photon term parameter selection dialog for 1D & 2D chisq calculation
;------------------------------------------------------------------------------
PRO PhotonModel::selectParams, myTerms, myParams, myErrors, selectionData, $
    rangeData, displaySigma, numInterest, CHI1D = chi1d, CHI2D = chi2d, $
    INTERVAL = interval, REPLOT = replot

    topBase = WIDGET_BASE (TITLE = 'PhotonModel: Select Parameters', $
        /COLUMN, /BASE_ALIGN_CENTER)

    label = WIDGET_LABEL (topBase, $
        VALUE = 'Set parameter values for Chisq mapping')
    
    IF (NOT OBJ_VALID (self.termList)) THEN RETURN
    
    myBase  = WIDGET_BASE (topBase, ROW = 2, /GRID)
    sigmaOrPC = ['Sigma Levels', 'Percent Levels']
    IF KEYWORD_SET(INTERVAL) THEN BEGIN
        choice_label = 'Choose Error Interval by:'
        param_label = 'Level of Sigma (or Percentile):' 
		splat = INDGEN (3) + 1
    ENDIF ELSE BEGIN
        choice_label = 'Choose to display:'
        param_label = 'Parameters of Interest:' 
		splat = INDGEN (10) + 1
    ENDELSE
    label = WIDGET_LABEL (myBase, VALUE = choice_label)
    popup1 = WIDGET_DROPLIST (myBase, VALUE = STRING (sigmaOrPC))
    label = WIDGET_LABEL (myBase, VALUE = param_label)
    IF KEYWORD_SET (chi2d) THEN splat = splat[1:*]
    popup2 = WIDGET_DROPLIST (myBase, VALUE = STRING (splat))
    
    
    names = self.termList->names ()
    
    nNames = N_ELEMENTS (names)
    termArr = INDGEN (N_ELEMENTS (myTerms))
    termInd = termArr [WHERE (myTerms)]
    termInd = termInd[1: *] - 1

    maxParams = 10     
    selection = LONARR (nNames, maxParams)
    ranges = LONARR (nNames, maxParams, 2)
    
    IF KEYWORD_SET (replot) THEN dummy = '' $ ;== Don't need the rest of the widget!
    ELSE BEGIN
        
    FOR i = 0, nNames - 1 DO BEGIN

        term = self.termFactory->copy (names[i])
        
        name    = term->name () 
        nParams = term->nparams ()        
        params  = term->params ()
        
        OBJ_DESTROY, term

        label = WIDGET_LABEL (topBase, VALUE = STRTRIM (name))
        
        base  = WIDGET_BASE (topBase, ROW = nParams, /GRID)

        FOR j = 0, nParams - 1 DO BEGIN
        
            myValue = myParams[termInd[i], j]
            myErrRange = myErrors[termInd[i], j] * 4.0

            IF NOT (params.fixed)[j] THEN BEGIN
                selection[i, j] = CW_BGROUP (base, STRTRIM(params[j].name, 2) + ':', $
                /ROW, /NONEXCLUSIVE, SET_VALUE = [0])
            ENDIF ELSE BEGIN
				label = WIDGET_LABEL (base, VALUE = STRTRIM(params[j].name, 2) + ':')
            ENDELSE
                
            label = WIDGET_LABEL (base, VALUE =  + STRTRIM (STRING (myValue), 2))
       
            ranges[i, j, 0] = WIDGET_TEXT (base, /EDIT, $
                VALUE = STRING (myValue - myErrRange, F = '(g10.4)'), XSIZE = 6)
                
            ranges[i, j, 1] = WIDGET_TEXT (base, /EDIT, $
                VALUE = STRING (myValue + myErrRange, F = '(g10.4)'), XSIZE = 6)
                    
        ENDFOR
        

    ENDFOR
    
    ENDELSE

    gridBase = WIDGET_BASE (topBase, /GRID, COLUMN = 2, SPACE = 20)
    button = WIDGET_BUTTON (gridBase, VALUE = 'Accept')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Cancel')
    
    ;== Need to pass a valid ptr to the widget; any one will do...
    k = 0.0
    l = 0
    mySelectPtr = PTR_NEW (k)
    myRangesPtr = PTR_NEW (k)
    myDisSigPtr = PTR_NEW (l)
    myNumIntPtr = PTR_NEW (l)

    ;== Store the object reference on the display widget
    state = PTR_NEW ({ self: self, names: names, selection: selection, $
            ranges: ranges, popup1: popup1, popup2: popup2, $
            mySelect: mySelectPtr, myRanges: myRangesPtr, $
            displaySigma: myDisSigPtr, numInterest: myNumIntPtr})
    WIDGET_CONTROL, topBase, SET_UVALUE = state
    WIDGET_CONTROL, topBase, /REALIZE

    ;== Start event loop
    
    XMANAGER, OBJ_CLASS (self) + '_selectParams', topBase  ; , /NO_BLOCK
    
    selectionData = *mySelectPtr
    rangeData     = *myRangesPtr
    displaySigma  = *myDisSigPtr
    numInterest   = *myNumIntPtr
    
    PTR_FREE, mySelectPtr
    PTR_FREE, myRangesPtr
    PTR_FREE, myDisSigPtr
    PTR_FREE, myNumIntPtr
    
END


; ----------------------------------------------------------------------------
; Return current term names
; ----------------------------------------------------------------------------
FUNCTION PhotonModel::names

    RETURN, self.termList->names ()
       
END


; ----------------------------------------------------------------------------
; Return number of terms in the model
; ----------------------------------------------------------------------------
FUNCTION PhotonModel::count

    RETURN, self.termList->count () 
            
END    


; ----------------------------------------------------------------------------
; Return chosen fit statistic
; ----------------------------------------------------------------------------
FUNCTION PhotonModel::statchoice

    RETURN, self.statchoice
            
END    


; ----------------------------------------------------------------------------
; Return chosen fit statistic
; ----------------------------------------------------------------------------
FUNCTION PhotonModel::fixUndetChoice

    RETURN, self.fixchoice
            
END    



; ----------------------------------------------------------------------------
; Add a term to the model
; ----------------------------------------------------------------------------
PRO PhotonModel::add, name, term

    IF (N_PARAMS () LT 2) THEN $
       term = self.termFactory->copy (name)    
    
    self.termList->add, name, term
        
END    


; ----------------------------------------------------------------------------
; Delete a term from the model
; ----------------------------------------------------------------------------
PRO PhotonModel::delete, name

    self.termList->delete, name
        
END    


; ----------------------------------------------------------------------------
; Clear the current model
; ----------------------------------------------------------------------------
PRO PhotonModel::clear

    names = self.termList->names ()
    n = self.termList->count ()
    
    FOR i = 0, n - 1 DO BEGIN
        obj = self.termList->data (names[i])
	; 12/10/2002 RDP: Sometimes we have already cleared!
        IF OBJ_VALID (obj) THEN OBJ_DESTROY, obj
    ENDFOR            
    OBJ_DESTROY, self.termList
    self.termList = OBJ_NEW ('List')
 
END    


; ----------------------------------------------------------------------------
; Setup the current model
; ----------------------------------------------------------------------------
PRO PhotonModel::setModel, names, model

    n = N_ELEMENTS (names)
    IF (n LT 1) THEN RETURN
       
    self->clear
    
    FOR i = 0, n - 1 DO BEGIN
        term = self.termFactory->copy (names [i])
        self.termList->add, names[i], term
	;OBJ_DESTROY, term
    ENDFOR
    
END


; ----------------------------------------------------------------------------
; Return the current model
; ----------------------------------------------------------------------------
FUNCTION PhotonModel::model

    names = self->names ()
    n = self->count ()
    IF (n LT 1) THEN $
       RETURN, OBJ_NEW ()
       
    IF (n EQ 1) THEN BEGIN
       model = OBJ_NEW ()
    ENDIF ELSE BEGIN
       model = OBJARR (n)
    ENDELSE
       
    FOR i = 0, n - 1 DO $
        model[i] = (self.termList->data (names[i]))->copy ()
    
    RETURN, model

END

; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PhotonModel__define

    obj = { PHOTONMODEL, $

        termListID        : 0L, $          ; List of possible photon terms
        useDefaultTermsID : 0L, $          ; Allow user to avoid setting terms
        choice            : 0L, $          ; Save the status of the setting
        fixUndetID        : 0L, $          ; Allow user to change undetermined values handling:
        fixchoice         : 0L, $          ; Fix or Float undetermined values in batch fitting
        statisticID       : 0L, $          ; Allow user to change fitting statistic
        statchoice        : 0L, $          ; Save the status of the fitting statistic
        omittedTerms      : PTR_NEW (), $  ; Terms the user has omitted
        selectedTerms     : PTR_NEW (), $  ; Terms selected previously
        paramID           : PTR_NEW (), $  ; Parameter text box IDs
        fixedID           : PTR_NEW (), $  ; Parameter fixed button IDs
        
        termFactory       : OBJ_NEW (), $  ; Create photon model terms        
        termList          : OBJ_NEW ()  $  ; Holds the user-selected model
        
    }


END
