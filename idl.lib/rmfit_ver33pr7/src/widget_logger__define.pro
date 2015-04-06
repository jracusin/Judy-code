; ----------------------------------------------------------------------------
;+
; NAME:
;     Widget_Logger (OBJECT)
;
; PURPOSE:
;     An object that implements a logging widget.
;
; CALLING SEQUENCE:
;     myLogger = OBJ_NEW ('Widget_Logger' [, parentBase])
;
; INPUTS:
;
;     parentBase (OPTIONAL)
;         If this argument is supplied and is a valid WIDGET_BASE, then
;         this base will be the parent for the WIDGET_LOGGER.  This allows
;         the logger to be embedded into larger widget applications.  
;
;         In this case, the HIDE_BUTTON keyword will be ignored (the 
;         "Hide" button will be omitted from the logger widget), and the 
;         TITLE button, if supplied, will be a WIDGET_LABEL above the logger 
;         text area.
;
; KEYWORDS:
;
;     XSIZE
;         xsize of the log text area (characters, DEFAULT = 60)
;
;     YSIZE
;         ysize of the log text area (characters, DEFAUTL = 10)
;
;     MAP
;         Set this keyword to 0 to prohibit mapping of the widget when the 
;         logger object is instantiated, or 1 to map the widget (DEFAULT = 1)
;
;     TITLE
;         Logger widget window title (DEFAULT = 'WidgetLogger')
;
;     HIDE_BUTTON
;         Set this keyword to 0 to omit the "Hide" button, or 1 to 
;         include (DEFAULT = 1)
;
;     CLEAR_BUTTON
;         Set this keyword to 0 to omit the "Clear" button, or 1 to 
;         include (DEFAULT = 1)
;
;     GROUP_LEADER
;         The widget ID of an existing widget that serves as "group leader" 
;         for the Logger widget.  Widget application hierarchies are defined 
;         by group membership relationships between top-level widget bases. 
;         When a group leader is killed for any reason, all widgets in the 
;         group are also destroyed.
;
; METHODS:
;
;    append (PROCEDURE) - Append text to the logger widget
;
;         Inputs: text: STRING or STRARR of text to append to the logger
;                 If no input argument is specified, a blank line is appended
;        Outputs: NONE
;       Keywords: NONE
;
;    clear (PROCEDURE) - Clear the text area of the logger widget
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;    getText (FUNCTION) - Return the current text displayed in the logger
;
;         Inputs: NONE
;        Outputs: STRARR of text currently displayed
;       Keywords: NONE
;
;    hide (PROCEDURE) - Hide (MAP = 0) the widget
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;    setClearButton (PROCEDURE) - Change the label of the "Clear" button
;
;         Inputs: STRING specifying new label for the button
;        Outputs: NONE
;       Keywords: NONE
;
;    setHideButton (PROCEDURE) - Change the label of the "Hide" button
;
;         Inputs: STRING specifying new label for the button
;        Outputs: NONE
;       Keywords: NONE
;
;    setLineNumber (PROCEDURE) - Set the zero-based line number of the
;        line to be positioned on the topmost visible line in the text
;        widget's viewport. No horizontal scrolling is performed. Note
;        that this is a line number, not a character offset.
;
;         Inputs: INTEGER specifying the topmost visible line number
;        Outputs: NONE
;       Keywords: LAST : Set this keyword to set the last line of text as
;                        the topmost visible line.
;
;    setSize (PROCEDURE) - Set the XSIZE and/or YSIZE of the log text area
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: XSIZE : xsize of the log text area (characters)
;                 YSIZE : ysize of the log text area (characters)
;
;    show (PROCEDURE) - Show (MAP = 1) the widget
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;
;    getWidgetIDs (FUNCTION) - Return a structure containing the widget ids
;        of the logger components.  The following structure tags are returned:
;
;             top   : Top level base ID
;             text  : Text area widget ID
;             clear : Clear button widget ID
;             hide  : Hide button widget ID
;
;        DO NOT CHANGE THE UVALUES OF ANY OF THE LOGGER WIDGET COMPONENTS, AS
;        THEY ARE USED INTERNALLY BY THE OBJECT.  This method is not
;        recommended for use; manipulating the logger widget from outside
;        the object can compromise the internal object state.
;
;         Inputs: NONE
;        Outputs: STRUCTURE containing the logger widget component IDs
;       Keywords: NONE
;
; EXAMPLE:
;
;     ; Create a logger widget
;     ;
;     logger = OBJ_NEW ('Widget_Logger', MAP = 0, TITLE = 'myLogger')
;
;     ; Change the label of the 'Clear' button to 'Clear Log Text'
;     ;
;     logger->setClearButton, 'Clear Log Text'
;
;     ; Show the widget
;     ;
;     logger->show
;
;     ; Add some text
;     ;
;     logger->append, ['A line of text.', 'Another line of text.']
;
;     ; Clear the logger programatically
;     ;
;     logger->clear
;
;     ; Add a lot of text, then make the 300th line visible
;     ;
;     logger->append, 'Line number ' + STRTRIM (SINDGEN (500) + 1, 2)
;     logger->setLineNumber, 300
;
;     ; All done
;     ;
;     OBJ_DESTROY, logger
;
; MODIFICATION HISTORY:
;
;     RDP, 2001 July, added SAVE_BUTTON keyword    
;
;     RSM, 1999 December, WIDGET_LOGGER no longer needs to be the top of
;          a widget hierarchy.  It can be embedded into larger widget 
;          applications by supplying a valid WIDGET_BASE on instantiation.
; 
;     RSM, 1999 November, added GROUP_LEADER keyword
;
;     RSM, 1998 December, added CLEAR_BUTTON, HIDE_BUTTON keywords
;
;     Written, 1998 June, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION widget_logger::init, base, $
    XSIZE = xsize, YSIZE = ysize, MAP = map, TITLE = title, $
    HIDE_BUTTON = hide_button, CLEAR_BUTTON = clear_button, $
    SAVE_BUTTON = save_button, TABLE_BUTTON = table_button, $                                 ; AMG
    GROUP_LEADER = group_leader


    ; By default, the logger is mapped on creation
    ;
    doMap = 1
    IF (N_ELEMENTS (map) NE 0) THEN $
       doMap = map

    self->build, base, XSIZE = xsize, YSIZE = ysize, TITLE = title, $
        HIDE_BUTTON = hide_button, CLEAR_BUTTON = clear_button, $
        SAVE_BUTTON = save_button, TABLE_BUTTON = table_button, $                             ; AMG
        GROUP_LEADER = group_leader    

    IF (doMap) THEN $
       self->show

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO widget_logger::cleanup

    IF (WIDGET_INFO (self.topID, /VALID)) THEN $
       WIDGET_CONTROL, self.topID, /DESTROY

END


; ----------------------------------------------------------------------------
; Event handler
; ----------------------------------------------------------------------------
PRO widget_logger_EVENT, event

    topID = WIDGET_INFO (event.top, FIND_BY_UNAME = 'WIDGET_LOGGER_TOPID')
    IF (topID EQ 0) THEN $
       MESSAGE, 'Failed to find WIDGET_LOGGER id in widget hierarchy.'
       
    WIDGET_CONTROL, topID, GET_UVALUE = self

    self->setKeyboardEvents
    
    self->display_event, event

END

PRO widget_logger::display_event, event

    self->show

    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_KILL_REQUEST': BEGIN

            ;== Dismiss, not kill
            self->hide
            ;WIDGET_CONTROL, event.top, MAP = 0 
            END

        'WIDGET_BUTTON': BEGIN
        ; Widget button events only for this object

        WIDGET_CONTROL, event.id, GET_UVALUE = uvalue

        CASE (uvalue) OF

            'Clear' : self->clear
            'Hide'  : self->hide
            'Save'  : self->saveText
            'Table' : BEGIN                                                                     ; AMG
                          fitText=self->getText()                                               ;
                          IF N_ELEMENTS(fitText) LE 1 THEN RETURN
                          spectable=OBJ_NEW('SPECTABLE')                                        ;
                          self.tableObj=spectable                                               ;
                          spectable->GET_INFO, fitText, self                                    ;
                          self.tflag=1                                                          ;
                      END                                                                       ;

             ELSE: ;self->setStatus, 'Event not handled: ' + value, 2

            ENDCASE
            
        END ; WIDGET_BUTTON events
        

        'WIDGET_TEXT_CH': self->keyBindings, event.ch

        ELSE: ;self->setStatus, 'Event not handled.' , 2
                       
    ENDCASE

END

 
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO widget_logger::keyBindings, char
    
    ; COMPILE_OPT HIDDEN

    CASE (STRING (char)) OF

         'd' : self->hide
         'h' : self->hide
         'c' : self->clear
         ELSE:

    ENDCASE
    
END


;------------------------------------------------------------------------------
; Set up handler for keyboard events
;------------------------------------------------------------------------------
PRO widget_logger::setKeyboardEvents
    
    WIDGET_CONTROL, self.textID, /INPUT_FOCUS

END


; ----------------------------------------------------------------------------
; Build GUI
; ----------------------------------------------------------------------------
PRO widget_logger::build, parentBase, $
    XSIZE = xsize, YSIZE = ysize, TITLE = title, $
    HIDE_BUTTON = hide_button, CLEAR_BUTTON = clear_button, $
    SAVE_BUTTON = save_button, TABLE_BUTTON = table_button, $                                   ; AMG
    GROUP_LEADER = group_leader

       
    IF (N_ELEMENTS (title) EQ 0) THEN title = 'WidgetLogger'
    IF (N_ELEMENTS (xsize) EQ 0) THEN xsize = 60
    IF (N_ELEMENTS (ysize) EQ 0) THEN ysize = 10

    use_clear_button = 1
    use_hide_button  = 1
    use_save_button  = 1
    use_table_button = 1                                                                        ; AMG
    IF (N_ELEMENTS (clear_button) NE 0) THEN use_clear_button = clear_button
    IF (N_ELEMENTS (hide_button) NE 0)  THEN use_hide_button = hide_button
    IF (N_ELEMENTS (save_button) NE 0)  THEN use_save_button = save_button
    IF (N_ELEMENTS (table_button) NE 0) THEN use_table_button = table_button                    ; AMG
    

    haveParent = 0
    IF (N_ELEMENTS (parentBase) NE 0) THEN BEGIN
      
       IF (WIDGET_INFO (parentBase, /NAME) NE 'BASE') THEN BEGIN
       
          MESSAGE, /CONTINUE, 'Argument must be a valid WIDGET_BASE.'
       
       ENDIF ELSE BEGIN
       
          haveParent = 1
          use_hide_button = 0
    
       ENDELSE
       
    ENDIF


    IF (haveParent) THEN BEGIN
    
       IF (N_ELEMENTS (title) NE 0) THEN $
          w = WIDGET_LABEL (parentBase, VALUE = STRING (title))
    
       self.topID = WIDGET_BASE (parentBase, UNAME = 'WIDGET_LOGGER_TOPID', $
           /COLUMN, GROUP_LEADER = group_leader)
           
       self.textID = WIDGET_TEXT (parentBase, /SCROLL, $
           XSIZE = xsize, YSIZE = ysize, /ALL)   ;<== Support key bindings

       IF (use_clear_button) THEN BEGIN

          base = WIDGET_BASE (parentBase, ROW = 1, /ALIGN_CENTER)

          self.clearID = WIDGET_BUTTON (base, $
              VALUE = 'Clear', UVALUE = 'Clear', /DYNAMIC, $
              UNAME = 'WIDGET_LOGGER_CLEARID', $
              EVENT_PRO = OBJ_CLASS (self) + '_EVENT')

       ENDIF
       
       IF (use_save_button) THEN BEGIN

          base = WIDGET_BASE (parentBase, ROW = 1, /ALIGN_CENTER)

          self.saveID = WIDGET_BUTTON (base, $
              VALUE = 'Save', UVALUE = 'Save', /DYNAMIC, $
              EVENT_PRO = OBJ_CLASS (self) + '_EVENT')

       ENDIF
       
    ENDIF ELSE BEGIN 

       self.topID = WIDGET_BASE (TITLE = title, UNAME = 'WIDGET_LOGGER_TOPID', $
           /COLUMN, /BASE_ALIGN_CENTER, MAP = 0, GROUP_LEADER = group_leader, $
           /TLB_KILL_REQUEST_EVENTS)

       self.textID = WIDGET_TEXT (self.topID, /SCROLL, $
           XSIZE = xsize, YSIZE = ysize, /ALL)

       base = WIDGET_BASE (self.topID, ROW = 1, /GRID_LAYOUT, SPACE = 20)

       IF (use_clear_button) THEN $
          self.clearID = WIDGET_BUTTON (base, $
              VALUE = 'Clear', UVALUE = 'Clear', $
              UNAME = 'WIDGET_LOGGER_CLEARID', /DYNAMIC)
       IF (use_hide_button) THEN $
          self.hideID  = WIDGET_BUTTON (base, $
              VALUE = 'Hide', UVALUE = 'Hide', /DYNAMIC)
       IF (use_save_button) THEN $
          self.saveID  = WIDGET_BUTTON (base, $
              VALUE = 'Save', UVALUE = 'Save', /DYNAMIC)
       IF (use_table_button) THEN $                                                             ; AMG
          self.tableID = WIDGET_BUTTON (base, $                                                 ;
              VALUE = 'Table', UVALUE = 'Table', /DYNAMIC)                                      ;

    ENDELSE

    WIDGET_CONTROL, self.topID, SET_UVALUE = self
    WIDGET_CONTROL, self.topID, /REALIZE

    XMANAGER, OBJ_CLASS (self), self.topID, /NO_BLOCK


END


; ----------------------------------------------------------------------------
; Show the logger
; ----------------------------------------------------------------------------
PRO widget_logger::show

    WIDGET_CONTROL, self.topID, MAP = 1, SHOW = 1

END


; ----------------------------------------------------------------------------
; Hide the logger
; ----------------------------------------------------------------------------
PRO widget_logger::hide

    WIDGET_CONTROL, self.topID, MAP = 0

END


; ----------------------------------------------------------------------------
; Save the logger data
; ----------------------------------------------------------------------------
PRO widget_logger::saveText

    myText = self->getText ()
    
    filename = DIALOG_PICKFILE ( $
              TITLE = 'Write the fit results', $
              FILE = 'fitresults.txt', $             ; FILTER = '*.lu', 
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.topID, $
              /WRITE, /OVERWRITE_PROMPT)             ; <---Changed for Mac!
    IF (filename EQ '') THEN RETURN
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(filename, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(filename)) $
        THEN RETURN
    ; If the user accidently puts a space in front of the filename...
    retpos = STRPOS(filename, STRING(10B))
    IF (retpos NE 0) THEN BEGIN
     filename = STRMID(filename, retpos + 1)
    ENDIF
    
    !RM_PARAMS.lastPath = lastPath

    numb = N_ELEMENTS (myText)
    OPENW, FL, filename, ERROR = status, /GET_LUN
    FOR i = 0, numb - 1 DO PRINTF, FL, myText[i]
    CLOSE, fl
    FREE_LUN, FL

END


; ----------------------------------------------------------------------------
; Set the size of the logger
; ----------------------------------------------------------------------------
PRO widget_logger::setSize, XSIZE = xsize, YSIZE = ysize

    WIDGET_CONTROL, self.textID, XSIZE = xsize, YSIZE = ysize

END


; ----------------------------------------------------------------------------
; Set the topmost visible line number of the logger
; ----------------------------------------------------------------------------
PRO widget_logger::setLineNumber, lineNo, LAST = last

    IF (N_ELEMENTS (last) NE 0) THEN $
       lineNo = N_ELEMENTS (self->getText ())

    IF (N_ELEMENTS (lineNo) EQ 0) THEN $
       lineNo = N_ELEMENTS (self->getText ())

    WIDGET_CONTROL, self.textID, SET_TEXT_TOP_LINE = FIX (lineNo)

END


; ----------------------------------------------------------------------------
; Set the labels for the logger buttons
; ----------------------------------------------------------------------------
PRO widget_logger::setClearButton, newLabel, _EXTRA = extra

    IF (N_PARAMS () NE 1) THEN BEGIN
       MESSAGE, /CONTINUE, "Usage: setClearButton, 'label'"
       RETURN
    ENDIF

    IF (WIDGET_INFO (self.clearID, /VALID)) THEN $
       WIDGET_CONTROL, self.clearID, SET_VALUE = STRING (newLabel), _EXTRA = extra

END

PRO widget_logger::setHideButton, newLabel

    IF (N_PARAMS () NE 1) THEN BEGIN
       MESSAGE, /CONTINUE, "Usage: setHideButton, 'label'"
       RETURN
    ENDIF

    IF (WIDGET_INFO (self.hideID, /VALID)) THEN $
       WIDGET_CONTROL, self.hideID, SET_VALUE = STRING (newLabel)

END


; ----------------------------------------------------------------------------
; Clear the logger
; ----------------------------------------------------------------------------
PRO widget_logger::clear

    WIDGET_CONTROL, self.textID, SET_VALUE = ''

END


; ----------------------------------------------------------------------------
; Append to the logger
; ----------------------------------------------------------------------------
PRO widget_logger::append, text
    
    IF (N_ELEMENTS (text) NE 0) THEN BEGIN
    
       WIDGET_CONTROL, self.textID, UPDATE = 0
       WIDGET_CONTROL, self.textID, SET_VALUE = STRING (text), /APPEND
       WIDGET_CONTROL, self.textID, UPDATE = 1
       
       ; Routine to dynamically capture log output and pass to table
       IF self.tflag GE 1 THEN BEGIN                                                            ; AMG
           WIDGET_CONTROL, self.textID, GET_UVALUE=addtext                                      ;
           IF N_ELEMENTS(addtext) EQ 0 THEN addtext=''                                          ;
           addtext=[addtext, STRING(text)]                                                      ;
           WIDGET_CONTROL, self.textID, SET_UVALUE=addtext                                      ;
           FOR i=0, N_ELEMENTS(text)-1 DO BEGIN                                                 ;
              IF STRMATCH(STRING(text[i]), '*Energy Flux*') EQ 1 THEN self.tflag=2              ;
           ENDFOR                                                                               ;
                                                                                                ;
           IF self.tflag EQ 2 THEN BEGIN                                                        ;
               fittext=addtext                                                                  ;
               self.tflag=1                                                                     ;
               addtext=0 & dummy=TEMPORARY(addtext)                                             ;
               WIDGET_CONTROL, self.textID, SET_UVALUE=''                                       ;
               self.tableObj->GET_INFO, fittext, self                                           ;
           ENDIF                                                                                ;
       ENDIF                                                                                    ;
    
    ENDIF 

    RESTORE, !MFIT.script_file                                                                  ; AMG
      IF (script) AND (self.tflag NE 1) AND (table) THEN BEGIN                                  ;
        fitText=self->getText()                                                                 ;
        IF N_ELEMENTS(fitText) LE 1 THEN RETURN                                                 ;
        spectable=OBJ_NEW('SPECTABLE')                                                          ;
        self.tableObj=spectable                                                                 ;
        spectable->GET_INFO, fitText, self                                                      ;
        self.tflag=1                                                                            ;
      ENDIF                                                                                     ;

END


; ----------------------------------------------------------------------------
; Retrieve the current logger text
; ----------------------------------------------------------------------------
FUNCTION widget_logger::getText

    WIDGET_CONTROL, self.textID, GET_VALUE = currentText

    RETURN, currentText

END


; ----------------------------------------------------------------------------
; Retrieve the logger widget IDs
; ----------------------------------------------------------------------------
FUNCTION widget_logger::getWidgetIDs

    ids = { WIDGET_LOGGER_ID, $
        top   : self.topID,   $
        text  : self.textID,  $
        clear : self.clearID, $
        hide  : self.hideID,  $
        saveit: self.saveID,  $
        table : self.tableID  $                                                                ; AMG
    }

    RETURN, ids

END


; ----------------------------------------------------------------------------
; A widget text-logging object
; ----------------------------------------------------------------------------
PRO widget_logger__define

    obj = { WIDGET_LOGGER, $

        ; Widget IDs
	;
	topID   :        0L, $
	textID  :        0L, $
	clearID :        0L, $
	hideID  :        0L, $
	saveID  :        0L, $
  tableID :        0L, $                                                                        ; AMG
  tableObj: OBJ_NEW(), $                                                                        ;
  tflag   :        0L  $                                                                        ;
  
    }


END
