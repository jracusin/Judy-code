; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     StatusBar (OBJECT)
;
; PURPOSE:
;     This object creates a WIDGET_LABEL that contains a built-in timer.
;     It is intended to be used as a status line for a GUI.  The timer
;     feature can be used to show status information for a set time interval,
;     after which the status is cleared, or reset to the previous text.
;
; INPUTS:
;     parent : The widget ID of the parent of the StatusBar
;
; KEYWORDS:
;     Any valid WIDGET_LABEL keywords, except UVALUE.  The UVALUE field
;     is used internally by the object, and should not be modified.
;
; DEPENDENCIES:
;     NONE
;
; PUBLIC METHODS:
;     
;     text (FUNCTION) - Return the current text
;         Inputs: NONE
;        Outputs: STRING containing the current text
;       Keywords: NONE
;
;     setText (PROCEDURE) - Set the current text
;         Inputs: TEXT - STRING containing the current text
;                 INTERVAL - Optional FLOAT input specifying the number of
;                 seconds to show the text. 
;        Outputs: NONE 
;       Keywords: REVERT - If the optional INTERVAL input parameter is given,
;                 the default is to clear the text to a null string after
;                 the interval expires.  Set this keyword to revert to the
;                 previous text when the interval expires.  
;
;                 Note that changing the status text repeatedly before a set 
;                 interval expires may result in the reverted text not being 
;                 the STRING you intended.
;
;     clear (PROCEDURE) - Clear the current text.  This convenience method
;                 is identical to calling the setText method with the null
;                 string as input.
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;     id (FUNCTION) - Return the widget ID of the text label
;         Inputs: NONE
;        Outputs: The widget ID of the text label 
;       Keywords: NONE
;
;
; EXAMPLE:
;
;     base       = WIDGET_BASE (/COLUMN)
;     labelID    = WIDGET_LABEL  (base, VALUE = 'Demonstration of StatusBar')
;     activateID = WIDGET_BUTTON (base, VALUE = 'Activate StatusBar')
;     clearID    = WIDGET_BUTTON (base, VALUE = 'Clear StatusBar')
;     doneID     = WIDGET_BUTTON (base, VALUE = 'Quit')
;    
;     statusBar = OBJ_NEW ('StatusBar', base)
;
;     WIDGET_CONTROL, base, /REALIZE
;
;     finished = 0
;     WHILE (NOT finished) DO BEGIN
;
;           event = WIDGET_EVENT (base, /NOWAIT, /SAVE)
;           
;           CASE (event.id =) OF
;               
;             doneID     : finished = 1
;             activateID : statusBar->setText, 'Show text for 10 sec', 10
;             clearID    : statusBar->clear
;               
;           ENDCASE
;
;     ENDWHILE
;     
;     WIDGET_CONTROL, base, /DESTROY
;
; MODIFICATION HISTORY:
;
;     Written, 1999 September, Robert.Mallozzi@msfc.nasa.gov
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 



;------------------------------------------------------------------------------
; Constructor
;------------------------------------------------------------------------------
FUNCTION StatusBar::init, parent, _EXTRA = extra
    
    IF (N_PARAMS () LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.'
       RETURN, 0
    ENDIF
           
    IF (NOT WIDGET_INFO (parent, /VALID)) THEN BEGIN
       MESSAGE, /CONTINUE, 'Invalid parent widget ID.'
       RETURN, 0
    ENDIF
    
    self.text = ' '
    self.textID = WIDGET_LABEL (parent, VALUE = self.text, $
        /DYNAMIC, /ALIGN_LEFT, UVALUE = self, _EXTRA = extra)
    
    XMANAGER, 'StatusBar', self.textID, /JUST_REG
         
    RETURN, 1

END


;------------------------------------------------------------------------------
; Destructor
;------------------------------------------------------------------------------
PRO StatusBar::cleanup
    
END


;------------------------------------------------------------------------------
; Forward events to the object event handler
;------------------------------------------------------------------------------
PRO StatusBar_EVENT, event
    
    WIDGET_CONTROL, event.id, GET_UVALUE = self
    self->eventHandler, event

END

    
;------------------------------------------------------------------------------
; Event handler
;------------------------------------------------------------------------------
PRO StatusBar::eventHandler, event

    self.text = ((self.revert) ? self.lastText : ' ')    
    WIDGET_CONTROL, self.textID, SET_VALUE = self.text

END


;------------------------------------------------------------------------------
; Return the current text
;------------------------------------------------------------------------------
FUNCTION StatusBar::text
    
    IF (STRCOMPRESS (self.text, /REMOVE_ALL) EQ '') THEN $
       RETURN, ''
       
    RETURN, self.text

END


;------------------------------------------------------------------------------
; Set the current text
;------------------------------------------------------------------------------
PRO StatusBar::setText, text, interval, REVERT = revert

    self.revert   = KEYWORD_SET (revert)
    self.lastText = self.text
    
    self.text = text
    WIDGET_CONTROL, self.textID, SET_VALUE = self.text

    IF (N_ELEMENTS (interval) NE 0) THEN $
       WIDGET_CONTROL, self.textID, TIMER = FLOAT (interval)

END


;------------------------------------------------------------------------------
; Clear the current text
;------------------------------------------------------------------------------
PRO StatusBar::clear
    
    self.text = ' '
    WIDGET_CONTROL, self.textID, SET_VALUE = self.text

END

;------------------------------------------------------------------------------
; An object that implements a status bar
;------------------------------------------------------------------------------
PRO StatusBar__define

    obj = { STATUSBAR, $
   
        textID   : 0L, $        
        text     : '', $
        lastText : '', $
        revert   : 0   $ 
    }

END
