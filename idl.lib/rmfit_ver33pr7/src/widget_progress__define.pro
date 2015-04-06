; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;+
; NAME:
;     widget_progress
;
; PURPOSE:
;     An object that implements a progress bar widget.
;
; TYPE:
;     OBJECT
;
; CATEGORY:
;     Widgets
;
; CALLING SEQUENCE:
;     myProgress = OBJ_NEW ('widget_progress')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;
;     XSIZE        : (INTEGER) xsize of the widget (pixels, D = 300)
;     YSIZE        : (INTEGER) ysize of the widget (pixels, D = natural size)
;     TITLE        : (STRING)  Progress widget window title (D = 'Progress')
;     CANCEL_LABEL : (STRING)  Label for cancel button (D = 'Cancel')
;     MAP          : (INTEGER) Map widget when object is instantiated (D = 1) 
;     FGCOLOR      : (LONG) Index of the foreground color (D = !P.COLOR)
;     BGCOLOR      : (LONG) Index of the background color (D = !P.BACKGROUND)
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     None known.
;
; RESTRICTIONS:
;     None known.
;
; DEPENDENCIES:
;     NONE
;
; METHODS:
;
;    update (PROCEDURE) - Update the percent done bar
;
;         Inputs: fraction = FLOAT value of the fraction done (0.0 - 1.0)
;        Outputs: NONE
;       Keywords: NONE
;
;    cancel (FUNCTION) - Check if user clicked the "Cancel" button
;
;         Inputs: NONE
;        Outputs: 1 if "Cancel" was selected, else 0
;       Keywords: NONE
;
;    map (PROCEDURE) - Show (MAP = 1) the widget
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
; EXAMPLE:
;
;     ; Create a progress widget.  This will map the widget to the screen,
;     ; since the MAP keyword is not supplied, and the default is to
;     ; map the widget when the object is instantiated.
;     ;
;     progress = OBJ_NEW ('widget_progress', CANCEL_LABEL = 'Interrupt')
;
;     FOR i = 0, 20 DO BEGIN
;
;         fraction = i / 20.0
;
;         progress->update, fraction
;         IF (progress->cancel ()) THEN $
;            GOTO, done
;
;         ; Replace the WAIT command with the work you want performed
;         ;
;         WAIT, 2
;
;     ENDFOR
;
;     DONE:
;     OBJ_DESTROY, progress
;
;
; MODIFICATION HISTORY:
;     v1.0: Written, 1999 April, Robert.Mallozzi@msfc.nasa.gov
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;----------------------------------------------------------------------------
; Cleanup
;----------------------------------------------------------------------------
PRO widget_progress::cleanup

    IF (WIDGET_INFO (self.topID, /VALID)) THEN $
       WIDGET_CONTROL, self.topID, /DESTROY

END



;----------------------------------------------------------------------------
; Build the progress widget
;----------------------------------------------------------------------------
FUNCTION widget_progress::init, $
    TITLE = title, $
    XSIZE = xsize, YSIZE = ysize, $
    GROUP_LEADER = group_leader, $
    CANCEL_LABEL = cancel_label, MAP = map, $
    BGCOLOR = bgcolor, FGCOLOR = fgcolor


    IF (N_ELEMENTS (title) EQ 0) THEN $
       title = 'Progress'

    IF (N_ELEMENTS (xsize) EQ 0) THEN $
       xsize = 300
    
    IF (N_ELEMENTS (cancel_label) EQ 0) THEN $
       cancel_label = 'Cancel'

    IF (N_ELEMENTS (map) EQ 0) THEN $
       map = 1

    IF (N_ELEMENTS (bgColor) EQ 0) THEN $
       bgColor = !P.BACKGROUND
       
    IF (N_ELEMENTS (fgColor) EQ 0) THEN $
       fgColor = !P.COLOR
        
        
    IF (N_ELEMENTS (group_leader) NE 0) THEN BEGIN

       self.topID = WIDGET_BASE (/COLUMN, /FLOATING, TITLE = STRING (title), $
           GROUP_LEADER = group_leader, XPAD = 5, YPAD = 5, YSIZE = ysize)
    
    ENDIF ELSE BEGIN

       self.topID = WIDGET_BASE (/COLUMN, TITLE = STRING (title), $
           XPAD = 5, YPAD = 5, YSIZE = ysize)
    
    ENDELSE
    
    drawID = WIDGET_DRAW (self.topID, XSIZE = xsize, YSIZE = 15)
    self.labelID  = WIDGET_LABEL (self.topID, VALUE = ' 0% Complete')
    self.cancelID = WIDGET_BUTTON (self.topID, VALUE = cancel_label)

    self.fgColor = fgColor
    self.bgColor = bgColor
    

    ; Realize
    ;
    WIDGET_CONTROL, self.topID, /REALIZE, MAP = map
    
    ; Fill the progress bar
    ;
    WIDGET_CONTROL, drawID, GET_VALUE = win
    self.winID = win
    WSET, self.winID
    POLYFILL, [0, 1, 1, 0], [0, 0, 1, 1], /NORMAL, COLOR = bgColor          

    
    RETURN, 1

END ; init


;----------------------------------------------------------------------------
; Update the progress widget
;----------------------------------------------------------------------------
PRO widget_progress::update, fraction

    ; Clamp range to [0.0, 1.0]
    ;
    fraction = MAX ([0.0, fraction])
    fraction = MIN ([1.0, fraction])
    
    ; Draw updated bar
    ;
    WSET, self.winID
    POLYFILL, [0, fraction, fraction, 0], [0, 0, 1, 1], $
        /NORMAL, COLOR = self.fgColor          
    WIDGET_CONTROL, self.labelID, $
        SET_VALUE = STRCOMPRESS (FIX (fraction * 100)) + '%'

END

;----------------------------------------------------------------------------
; Check for "Cancel"
;----------------------------------------------------------------------------
FUNCTION widget_progress::cancel

    abort = 0
    
    quit = WIDGET_EVENT (self.cancelID, /NOWAIT, /SAVE)
    IF (quit.id EQ self.cancelID) THEN $
       abort = 1

    RETURN, abort
    
END


;----------------------------------------------------------------------------
; Map the progress widget
;----------------------------------------------------------------------------
PRO widget_progress::map

    WIDGET_CONTROL, self.topID, MAP = 1

END

;----------------------------------------------------------------------------
; Implementation of a widget progress object
;----------------------------------------------------------------------------

PRO widget_progress__define

    obj = { WIDGET_PROGRESS, $
        
        topID    : 0L, $
	labelID  : 0L, $
	cancelID : 0L, $
        winID    : 0L, $
                
        bgColor  : !P.BACKGROUND, $
        fgColor  : !P.COLOR $

    }


END ; define
