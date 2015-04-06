; -----------------------------------------------------------------------------
; 
; Main entry point for the RMFIT spectral fitting package.
;
; -----------------------------------------------------------------------------
;
; Scripting Keywords
; /SCRIPT : must be enabled for scripting mode
;
; LCFILE : A row array of lightcurve files (required)
;
; RSPFILE : A row array of response files (required)
;
; LUFILE : A row array of lookup files same dimension as lcFile and rspFile (optional)
; 
; MODEL : An array of models, denoted by name.  A row of models indicates a combined fit, 
;         while a column of models indicates separate fits to be performed. If an array of 
;         combines fits is desired where the number of combined fits is not constant throughout
;         the array, each set of model fits must be stored in an element of a pointer array. (required)
;
; VALS : A 2-column array, where the first column is a list of guess values for a model, 
;        and the second column denotes the status of the value: fixed (1) or varying (0).
;        If fitting an array of models, storing a 2-column array for each model is required. (optional)
;
; STATISTIC : Set the fitting statistic.  Chi-Square (0), Likelihood (1), or C-Stat (2)  (optional)
;
; EINTERVAL : Set the energy interval for flux/fluence calculations (optional)
;
; ITER : This is the iteration of different sets of detectors being loaded.  If only
;        one set of detectors is to be loaded then there will only be one iteration and
;        ITER=0.  For multiple detector sets scripted through a FOR loop, ITER will be 
;        the iteration of the FOR loop.  (required)
;
; /VERBOSE : Turn on to show loaded detectors in PHA Display (optional)
;
; /TABLE : Turn on to parse the fit log into the spectral table (optional)
;
; O : Stores the RMFIT object reference so that multiple iterations of detectors can be fit (required)
;
;------------------------------------------------------------------------------------------







PRO RMFIT, SCRIPT=script, lcFile=lcFile, rspFile=rspFile, luFile=luFile, model=model, vals=vals, $
           statistic=statistic, eInterval=eInterval, iter=iter, VERBOSE=verbose, TABLE=table, o

    ;== Add the RMFIT directory tree to IDL's path
    
    CASE (STRUPCASE (!VERSION.OS_FAMILY)) OF
    
        'UNIX'    : pathDelimiter = ':'
        'WINDOWS' : pathDelimiter = ';'
        'VMS'     : pathDelimiter = ','
        'MACOS'   : pathDelimiter = ','        
        ELSE      : MESSAGE, 'Unsupported operating system.'
        
    ENDCASE
               
    ;== Start out by trying to find the parameter file in the current dir
    makeNewFile = 0
    CD, CURRENT = cur
    
    result = FINDFILE('my_parms.sav')
    IF (result[0] EQ '') THEN BEGIN
        makeNewFile = 1
    ENDIF ELSE BEGIN
        param_file = FILE_EXPAND_PATH(result[0])
    ENDELSE
    
    result2 = FINDFILE('my_script.sav')                                                           ; AMG
    IF (result2[0] EQ '') THEN BEGIN                                                              ;
      makeNewFile = 1                                                                             ;
    ENDIF ELSE BEGIN                                                                              ;
      script_file = FILE_EXPAND_PATH(result2[0])                                                  ;
    ENDELSE                                                                                       ;
    
    
    ;== We must be clever and find where our files are. This works *ONLY* 
    ;if the IDL_STARTUP.PRO file or IDL_PATH environment variable defines 
    ;!PATH to include our root directory.
    IF (FLOAT (!VERSION.RELEASE) GE 5.3) THEN BEGIN
        paths = CALL_FUNCTION ('STRSPLIT', !PATH, pathDelimiter, /EXTRACT)
    ENDIF ELSE BEGIN
	    paths = STR_SEP (!PATH, pathDelimiter)
    ENDELSE
    nPaths = N_ELEMENTS (paths)
    i = nPaths       ;Traverse in reverse order. User is likely to have 
                     ;added the correct path at the end...
    REPEAT BEGIN
        i = i - 1
        param_savefile = FILEPATH ('save_parms.sav', ROOT_DIR = paths[i])
        result = FINDFILE(param_savefile)
    ENDREP UNTIL ((result[0] NE '') OR (i EQ 0))
    
    IF (result[0] EQ '') THEN BEGIN
        msg = 'You must set the IDL !PATH variable to point to the'
        msg = [msg, 'rmfit directory before running this program.']
        junk = DIALOG_MESSAGE (/ERROR, msg)
        RETURN
    ENDIF
              
    c = paths[i]
    
    libdir = FILEPATH ('' , ROOT_DIR = c, SUB = 'lib')
    srcdir = FILEPATH ('' , ROOT_DIR = c, SUB = 'src')
    hlpdir = FILEPATH ('' , ROOT_DIR = c, SUB = 'hlp')
    lib    = EXPAND_PATH ('+' + libdir)
    src    = EXPAND_PATH ('+' + srcdir)
    hlp    = EXPAND_PATH (hlpdir)

    info_file = FILEPATH ('mfit_func.info', ROOT_DIR = c, SUB = ['mfit'])
    ;param_file = FILEPATH ('save_parms.sav', ROOT_DIR = c)
    
    ;== Now create the save file, if needed
    IF makeNewFile THEN BEGIN
        param_file = FILEPATH ('save_parms.sav', ROOT_DIR = c)
        RESTORE, /RELAXED_STRUCTURE_ASSIGNMENT, FILE = param_file
        script_file = FILEPATH ('save_parms.sav', ROOT_DIR = c)                                    ; AMG
        RESTORE, /RELAXED_STRUCTURE_ASSIGNMENT, FILE = script_file                                 ;
        CD, CURRENT = cur
        msg = 'RMFIT needs to create a parameter file to store your'
        msg = [msg, 'settings. It will be placed in the directory:']
        msg = [msg, cur]
        msg = [msg, 'You may move this file to any other place that']
        msg = [msg, 'you have write privilege; however, you should']
        msg = [msg, 'either always start RMFIT in the directory, or add a']
        msg = [msg, 'line to your IDL startup file that points to the']
        msg = [msg, 'directory containing the file, similar to the following:']
        msg = [msg, 'CD, ' + "'" + cur + "'"]
        junk = DIALOG_MESSAGE (/INFORMATION, msg, /CANCEL)
        IF (junk EQ 'Cancel') THEN Error = 1 ;MESSAGE, 'User Cancelled!'
        
        CATCH, Error
        IF Error NE 0 THEN BEGIN
            print, 'Error status: ', Error
            print, 'Error string: ', !ERR_STRING
            msg = "Can't write files to the current directory. RMFIT will" ;'
            msg = [msg, 'try to find a directory where you have write privilege.']
            junk = DIALOG_MESSAGE (/ERROR, msg)
            cur = DIALOG_PICKFILE (/DIRECTORY,  $
              TITLE = 'Select a Directory for writing')
            Error = 0
            ;CD, cur
        ENDIF
        
        phaNum=(numfits=0)                                                                         ; AMG
        IF KEYWORD_SET(SCRIPT) THEN BEGIN                                                          ;
          script=1                                                                                 ;
          lcFile=lcFile                                                                            ;
          rspFile=rspFile                                                                          ;
          IF KEYWORD_SET(luFile) THEN luFile=luFile ELSE luFile=0                                  ;
          model=model                                                                              ;
          IF KEYWORD_SET(vals) THEN vals=vals ELSE vals=0                                          ;
          IF KEYWORD_SET(statistic) THEN statistic=statistic ELSE statistic=0                      ;
          IF KEYWORD_SET(eInterval) THEN eInterval=eInterval ELSE eInterval=0                      ;
          iter=iter                                                                                ;
          scriptverbose=KEYWORD_SET(verbose)                                                       ;
          table=KEYWORD_SET(table)                                                                 ;
        ENDIF ELSE BEGIN                                                                           ;
          script=(vals=(statistic=(eInterval=(iter=(scriptVerbose=(table=0))))))                   ;
          lcFile=(rspFile=(luFile=(model='')))                                                     ;
        ENDELSE                                                                                    ;

        ;== As new parameters are added to the save file, this next line
        ;must be modified!
        param_file_temp = FILEPATH ('my_parms.sav', ROOT_DIR = cur)
        SAVE, FILE = param_file_temp, param_photterms, param_colors, param_flux_int, param_dur_int, param_last_path
        param_file = param_file_temp
        script_file_temp = FILEPATH ('my_script.sav', ROOT_DIR = cur)                             ; AMG
        SAVE, FILE = script_file_temp, script, lcFile, rspFile, luFile, phaNum, model, vals, $    ;
                     statistic, eInterval, numfits, iter, scriptVerbose, table                    ;
        script_file = script_file_temp                                                            ;
    ENDIF
    
    phaNum=(numfits=0)                                                                            ; AMG
    IF KEYWORD_SET(SCRIPT) THEN BEGIN                                                             ;
      scriptTemp=1                                                                                ;
      lcFileTemp=lcFile                                                                           ;
      rspFileTemp=rspFile                                                                         ;
      IF KEYWORD_SET(luFile) THEN luFileTemp=luFile ELSE luFileTemp=''                            ;
      modelTemp=model                                                                             ;
      IF KEYWORD_SET(vals) THEN valsTemp=vals ELSE valsTemp=0                                     ;
      IF KEYWORD_SET(statistic) THEN statTemp=statistic ELSE statTemp=0                           ;
      IF KEYWORD_SET(eInterval) THEN eIntervalTemp=eInterval ELSE eIntervalTemp=0                 ;
      iterTemp=iter                                                                               ;
      scriptVerboseTemp=KEYWORD_SET(Verbose)                                                      ;
      tableTemp=KEYWORD_SET(table)                                                                ;
    ENDIF ELSE BEGIN                                                                              ;
      scriptTemp=(valsTemp=(statTemp=(eIntervalTemp=(iterTemp=(scriptVerboseTemp=(tableTemp=0))))));
      lcFileTemp=(rspFileTemp=(luFileTemp=(modelTemp='')))                                        ;
    ENDELSE                                                                                       ;
                                                                                                  ;
    RESTORE, /RELAXED_STRUCTURE_ASSIGNMENT, FILE = script_file                                    ;
    script=scriptTemp                                                                             ;
    lcFile=lcFileTemp                                                                             ;
    rspFile=rspFileTemp                                                                           ;
    luFile=luFileTemp                                                                             ;
    model=modelTemp                                                                               ;
    vals=valsTemp                                                                                 ;
    statistic=statTemp                                                                            ;
    eInterval=eIntervalTemp                                                                       ;
    iter=iterTemp                                                                                 ;
    scriptVerbose=scriptVerboseTemp                                                               ;
    table=tableTemp                                                                               ;
    IF N_ELEMENTS(lcFileTemp) NE 0 THEN lcFile=lcFileTemp                                         ;
    SAVE, file=script_file, script, lcFile, rspFile, luFile, phaNum, model, vals, statistic, $    ;
               eInterval, numfits, iter, scriptVerbose, table                                     ;

    ;== Set system variables for MFIT.  Other MFIT parameters
    ;== can be added to the MFIT_SYSTEM structure as required.

    !PATH = lib + pathDelimiter + src + pathDelimiter + hlp + pathDelimiter + !PATH
   
    MFIT = { MFIT_SYSTEM, $
    
        PARAM_FILE : param_file, $
        SCRIPT_FILE : script_file, $
        INFO_FILE : info_file, $
        HELP_PATH : hlpdir     $
        
    }    
    READ_ONLY = 1
    DEFSYSV, '!MFIT', MFIT, READ_ONLY
    
   
    ;== Create an instance of RMFIT
    
    IF N_ELEMENTS(o) NE 0 THEN o->load, PARENT = parent, FILENAME = lcFile ELSE $                ; AMG
    o = OBJ_NEW ('rmfit')


    ;== If you are checking for memory leaks, you must execute the 
    ;== following line before looking at the heap variable usage.
    ;

;    OBJ_DESTROY, o

END    
