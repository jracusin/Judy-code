; ----------------------------------------------------------------------------
;+
; NAME:
;     WMenu (OBJECT)
;
; PURPOSE:
;     A widget object that encapsulates a pull-down menu.  This object
;     should not be instantiated directly.  Use a WMenuFactory object
;     to manage the creation and destruction of WMenu objects.
;
; CALLING SEQUENCE:
;     menu = OBJ_NEW ('WMenu', parent, VALUE = value)
;
; INPUTS:
;    parent
;        Widget ID of the menu's parent.  This ID must be a WIDGET_BASE. 
;
; KEYWORDS:
;
;    VALUE (REQUIRED)
;        The initial value setting of the top level button of the menu.
;        The value is the label for the menu button, which can be a string
;        or a bitmap.  If you specify a bitmap, you must also set the BITMAP 
;        keyword.
;
;        NOTE : If a bitmap is used for the button value, the BITMAP
;        keyword must be set.  This is slightly different from the
;        WIDGET_BUTTON function, which does not require the keyword if a
;        BYTARR bitmap is used.
;
;    UNAME (REQUIRED for bitmap buttons)
;        A STRING that can be used to identify the widget. You can
;        associate a name with each widget in a specific  hierarchy, and
;        then use that name to query the WMenu object to obtain the widget
;        ID.  The UNAME should be unique to the WMenu object.
;
;        NOTE : If you do not supply a UNAME, the contents of the VALUE
;        keyword will be used as the UNAME.  Note that for bitmap buttons,
;        this may not be possible; thus for bitmap buttons, you must use
;        this keyword or else an error occurs.
;
;    BITMAP
;        Set this keyword if the VALUE is a bitmap (filename or BYTARR).
;
;    EVENT_HANDLER
;        A STRING containing the name of a procedure to be called by the
;        WIDGET_EVENT function when an event arrives from the widget.
;        Individual menu buttons can optionally be assigned a different
;        handler using the EVENT_PRO or EVENT_FUNC keyword in the ADD method.
;
;    TEAROFF
;        Set this keyword to allow the menu to be "torn off" into its
;        own window.
;
;    WIDTH
;        Set this keyword to a integer specifying the number of '-' characters
;        to use for the tearoff menu item.  The default value is 15, and
;        the minimum value is 6.
;
;    <WIDGET_BUTTON keywords>
;        Any other IDL WIDGET_BUTTON keywords may be used.
;
; PUBLIC METHODS:
;
; ----------------------------------------------------------------------------
;    add (FUNCTION) : Add a button to a menu.
; ----------------------------------------------------------------------------
;
;    Inputs : 
;
;        PARENT (OPTIONAL)
;        A widget ID of the button's parent.  If the parent parameter
;        is omitted, the top level menu button is used as the new
;        button's parent.
;
;    Outputs:
;
;        The widget ID of the new button is returned.
;
;    Keywords:
;
;        VALUE (REQUIRED)
;        The initial value setting of the top level button of the menu.
;        The value is the label for the menu button, which can be a string
;        or a bitmap.  If you specify a bitmap, you must also set the BITMAP
;        keyword.
;
;        NOTE : If a bitmap is used for the button value, the BITMAP
;        keyword must be set.  This is slightly different from the
;        WIDGET_BUTTON function, which does not require the keyword if a
;        BYTARR bitmap is used.
;
;        UNAME (REQUIRED for bitmap buttons)
;        A STRING that can be used to identify the widget. You can
;        associate a name with each widget in a specific  hierarchy, and
;        then use that name to query the WMenu object to obtain the widget
;        ID.  The UNAME should be unique to the WMenu object.
;
;        NOTE : If you do not supply a UNAME, the contents of the VALUE
;        keyword will be used as the UNAME.  Note that for bitmap buttons,
;        this may not be possible; thus for bitmap buttons, you must use
;        this keyword or else an error occurs.
;
;        BITMAP
;        Set this keyword if the VALUE is a bitmap (filename or BYTARR).
;
;        <WIDGET_BUTTON keywords>
;        Any other IDL WIDGET_BUTTON keywords may be used.
;
;
; ----------------------------------------------------------------------------
;    remove (PRO) : Remove a button from a menu.
; ----------------------------------------------------------------------------
;    
;    Inputs
;    ------
;
;    ID
;        This scalar input parameter can be of type LONG, in which case
;        it is treated as the widget ID of the button, or of type STRING,
;        where it is treated as the current UNAME of the button.  If this
;        parameter is omitted, the entire menu is removed.
;
;        The option to use a STRING to denote a menu item is preferred
;        since in this case you need not store the menu item widget ID
;        in order to access the item.
;
;    Outputs
;    -------
;
;        NONE
;
;    Keywords
;    --------
;
;    ALL
;        Set this keyword to remove the entire menu.  You may also omit
;        the ID input parameter to remove the entire menu.
;
;
; ----------------------------------------------------------------------------
;    setSensitive (PROCEDURE) : Set the sensitivity of menu buttons.
; ----------------------------------------------------------------------------
;
;    Inputs 
;    ------
;                 
;    ID
;        This scalar input parameter can be of type LONG, in which case
;        it is treated as the widget ID of the button, or of type STRING,
;        where it is treated as the current UNAME of the button. If this
;        parameter is omitted, the entire menu sensitivity is set.
;
;        The option to use a STRING to denote a menu item is preferred
;        since in this case you need not store the menu item widget ID
;        in order to access the item.
;
;    STATE
;        The sensitivity state (0 or 1)
;
;    Outputs
;    -------
;
;        NONE
;
;    Keywords
;    --------
;
;        NONE
;
;
; ----------------------------------------------------------------------------
;    value (FUNCTION) : Return the value of a menu button.
; ----------------------------------------------------------------------------
;     
;    Inputs
;    ------
;
;    ID
;        The widget ID of the button.  If omitted, a STRARR of all
;        menu VALUEs is returned.
;
;    Outputs
;    -------
;
;        The current VALUE of the button(s) is returned.  Note that this
;        function cannot be used for bitmap buttons.
;
;    Keywords
;    --------
;
;        NONE
;
; 
; ----------------------------------------------------------------------------
;    setValue (PRO) : Set the value of a menu button.
; ----------------------------------------------------------------------------
;    
;    Inputs
;    ------
;
;    ID
;        This scalar input parameter can be of type LONG, in which case
;        it is treated as the widget ID of the button, or of type STRING,
;        where it is treated as the current UNAME of the button.
;
;        The option to use a STRING to denote a menu item is preferred
;        since in this case you need not store the menu item widget ID
;        in order to access the item.
;
;    VALUE
;        The new value setting of the widget.  The value is the label for 
;        the menu button, which can be a string or a bitmap.  If you specify 
;        a bitmap, you must also set the BITMAP keyword.
;
;        NOTE : If a bitmap is used for the button value, the BITMAP
;        keyword must be set.  This is slightly different from the
;        WIDGET_BUTTON function, which does not require the keyword if a
;        BYTARR bitmap is used.
;
;    Outputs
;    -------
;
;        NONE
;
;    Keywords
;    --------
;
;    UNAME (REQUIRED for bitmap buttons)
;        A STRING that can be used to identify the widget. You can
;        associate a name with each widget in a specific  hierarchy, and
;        then use that name to query the WMenu object to obtain the widget
;        ID.  The UNAME should be unique to the WMenu object.
;
;        NOTE : If you do not supply a UNAME, the contents of the VALUE
;        keyword will be used as the UNAME.  Note that for bitmap buttons,
;        this may not be possible; thus for bitmap buttons, you must use
;        this keyword or else an error occurs.
;
;    BITMAP
;        Set this keyword if the VALUE is a bitmap (filename or BYTARR).
;
;
; ----------------------------------------------------------------------------
;     uname (FUNCTION) : Return the UNAME of a menu button.
; ----------------------------------------------------------------------------
;
;    Inputs
;    ------
;
;    ID
;        The widget ID of the button.  If omitted, a STRARR of all
;        menu UNAMEs is returned.
;
;    Outputs
;    -------
;
;        The current UNAME of the button(s) is returned.
;
;    Keywords
;    --------
;
;        NONE
;
;
;    setTearoffWidth (PRO) : Set the width (in characters) of the tearoff
;        menu item.  The default value is 15, and the minimum value is 6.
;
;     
;
; MODIFICATION HISTORY:
;
;     Written, 2000 January, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION WMenu::init, parent, $
    VALUE = value, UNAME = uname, BITMAP = bitmap, $
    EVENT_HANDLER = event_handler, $
    TEAROFF = tearoff, WIDTH = width, $    
    _EXTRA = extra
    
    IF (N_PARAMS () LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.'
       RETURN, 0
    ENDIF

    IF (N_ELEMENTS (value) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: VALUE.'
       RETURN, 0
    ENDIF
       
    IF (NOT WIDGET_INFO (parent, /VALID)) OR $
       (WIDGET_INFO (parent, /NAME) NE 'BASE') THEN BEGIN
       MESSAGE, /CONTINUE, 'Input argument must be of type WIDGET_BASE.'
       RETURN, 0
    ENDIF

    self.TYPE_TEXT   = 1
    self.TYPE_BITMAP = 2
    
    self.HTYPE_PROC  = 1
    self.HTYPE_FUNC  = 2

    self.parentID = parent              
    self.handler  = (N_ELEMENTS (event_handler) NE 0) ? event_handler : ''
   
    self.topID = self->add (parent, $
        VALUE = value, UNAME = uname, BITMAP = bitmap, /MENU, _EXTRA = extra)
        
    IF (KEYWORD_SET (tearoff)) THEN BEGIN

;       IF (N_ELEMENTS (width) EQ 0) THEN width = 15
;       value = STRING (REPLICATE (BYTE ('-'), width > 6))      
       
       value = '----- TEAROFF'         
       self.tearoffItemID = self->add ( $
           self.topID, VALUE = value, $
           EVENT_PRO = 'WMENU_EVENTHANDLER', UVALUE = self)
    ENDIF
               
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO WMenu::cleanup

    IF (PTR_VALID (self.buttons)) THEN BEGIN
       
       nButtons = N_ELEMENTS (*self.buttons)
    
       FOR i = 0, nButtons - 1 DO $
           PTR_FREE, ((*self.buttons).value)[i]

       PTR_FREE, self.buttons
        
    ENDIF
    
END    


; ----------------------------------------------------------------------------
; Internal event handler
; ----------------------------------------------------------------------------
PRO WMENU_EVENTHANDLER, event
    
; COMPILE_OPT HIDDEN

    WIDGET_CONTROL, event.id, GET_UVALUE = self
    self->tearOffMenu
    
END


;------------------------------------------------------------------------------
; Tear off button menus
;------------------------------------------------------------------------------
PRO WMenu::tearOffMenu

; COMPILE_OPT HIDDEN

    ; Check if tearoff menu is already built
    
    IF (WIDGET_INFO (self.tearoffID, /VALID)) THEN BEGIN
       WIDGET_CONTROL, self.tearoffID, SHOW = 1
       RETURN
    ENDIF
                       
    ; Traverse the menu hierarchy

    children = self->findChildren (self.topID)
    IF (children[0] EQ 0) THEN RETURN
    
    WIDGET_CONTROL, children[0], GET_VALUE = value
    IF (STRMID (value, 0, 5) NE '-----') OR (N_ELEMENTS (children) EQ 1) THEN $
       RETURN

    children = children[1:*]

    idx    = WHERE ((*self.buttons).id EQ self.topID)
    button = (*self.buttons)[idx]
    title  = (button.type EQ self.TYPE_TEXT) ? *button.value : 'TearOff'
    self.tearoffID = WIDGET_BASE (/COLUMN, TITLE = title)

    self->tearOffMenuBuild, children, PARENT = self.tearoffID
    
    ; Check if the menu object's parent widget base has a UVALUE

    widget = self.topID
    REPEAT BEGIN

        parent = WIDGET_INFO (widget, /PARENT)
        tlb    = widget
        widget = parent

    ENDREP UNTIL (parent EQ 0)

    WIDGET_CONTROL, tlb, GET_UVALUE = uvalue

    IF (N_ELEMENTS (uvalue) NE 0) THEN $
       WIDGET_CONTROL, self.tearoffID, SET_UVALUE = uvalue

    
    ; Group leaders must be WIDGET_BASE so IDL iconifies correctly
    ;
    ; NOTE: a bug in IDL (v5.3) prevents the tearoff menu from being
    ; unmapped from the display even if the group leader is unmapped.
    
    WIDGET_CONTROL, self.tearoffID, GROUP_LEADER = tlb


    ; Climb the widget hierarchy searching for an event handler
    
    widget = self.topID
    parent = -1    
    WHILE (parent NE 0 AND self.handler EQ '') DO BEGIN
        
        parent = WIDGET_INFO (widget, /PARENT)
        widget = parent
        
        self.handler = WIDGET_INFO (parent, /EVENT_PRO)
        IF (self.handler EQ '') THEN $
           self.handler = WIDGET_INFO (parent, /EVENT_FUNC)
    
    ENDWHILE
        
    IF (self.handler EQ '') THEN BEGIN
       MESSAGE, /CONTINUE, 'Could not find event handler for the menu.'
       WIDGET_CONTROL, self.tearoffID, /DESTROY
       RETURN
    ENDIF

    
    ; Set minimum size of tearoff
       
    geo = WIDGET_INFO (self.tearoffID, /GEOMETRY)
    IF (geo.scr_xsize LT 100) THEN $
       WIDGET_CONTROL, self.tearoffID, SCR_XSIZE = 100
           
    WIDGET_CONTROL, self.tearoffID, /REALIZE
    XMANAGER, 'WMENU', self.tearoffID, EVENT_HANDLER = self.handler, /NO_BLOCK

END


;------------------------------------------------------------------------------
; Recursive routine to build the tearoff menu
;------------------------------------------------------------------------------
PRO WMenu::tearOffMenuBuild, ids, PARENT = parent
   
; COMPILE_OPT HIDDEN

    IF (NOT PTR_VALID (self.buttons)) THEN RETURN    
    buttons = *self.buttons

    FOR i = 0, N_ELEMENTS (ids) - 1 DO BEGIN

        idx = WHERE (ids[i] EQ buttons.id, found)
        IF (found NE 0) THEN BEGIN

           button = buttons[idx]

           IF (button.handler EQ '') THEN BEGIN
              handler = self.handler
              htype = self.HTYPE_PROC
           ENDIF ELSE BEGIN
              handler = button.handler
              htype = button.htype
           ENDELSE
           
           child = WIDGET_INFO (button.id, /CHILD)    

           IF (child NE 0) THEN BEGIN

              newID = WIDGET_BUTTON (parent, VALUE = *button.value, /MENU)

              children = self->findChildren (button.id)

              self->tearOffMenuBuild, children, PARENT = newID

           ENDIF ELSE BEGIN

              IF (htype EQ self.HTYPE_PROC) THEN BEGIN
              
                 newID = WIDGET_BUTTON (parent, VALUE = *button.value, $
                     EVENT_PRO = handler, $
                     BITMAP = button.type EQ self.TYPE_BITMAP)
              
              ENDIF ELSE BEGIN

                 newID = WIDGET_BUTTON (parent, VALUE = *button.value, $
                     EVENT_FUNC = handler, $
                     BITMAP = button.type EQ self.TYPE_BITMAP)
              
              ENDELSE

           ENDELSE

        ENDIF ; found

    ENDFOR

END


; ----------------------------------------------------------------------------
; Rebuild the tearoff menu
; ----------------------------------------------------------------------------
PRO WMenu::tearOffMenuRebuild
        
; COMPILE_OPT HIDDEN

    IF (WIDGET_INFO (self.tearoffID, /VALID)) THEN BEGIN
       WIDGET_CONTROL, self.tearoffID, /DESTROY
       self->tearOffMenu
    ENDIF

END


; ----------------------------------------------------------------------------
; Given a widget ID, return a LONARR containing the widget IDs of all children
; ----------------------------------------------------------------------------
FUNCTION WMenu::findChildren, parent

; COMPILE_OPT HIDDEN

    ; Find first child widget
    
    child = WIDGET_INFO (parent, /CHILD)    
    IF (child EQ 0) THEN RETURN, LONARR (1)

    ; Find the child widget's siblings
    
    children = (s = child)

    REPEAT BEGIN
        
        sibling = WIDGET_INFO (s, /SIBLING)
        IF (sibling NE 0) THEN BEGIN
        
           ; Set up to find the next sibling
           
           children = [children, sibling]             
           s = sibling
                              
        ENDIF

    ENDREP UNTIL (sibling EQ 0)

    RETURN, children
    
END


; ----------------------------------------------------------------------------
; Add a menu item
; ----------------------------------------------------------------------------
FUNCTION WMenu::add, parent, $
    VALUE = value, UNAME = uname, BITMAP = bitmap, $
    EVENT_PRO = event_pro, EVENT_FUNC = event_func, $
    _EXTRA = extra

    IF (N_PARAMS () LT 1) THEN parent = self.topID

    IF (N_ELEMENTS (value) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: VALUE.'
       RETURN, 0
    ENDIF

    IF (N_ELEMENTS (uname) EQ 0) AND (NOT KEYWORD_SET (bitmap)) THEN $
       uname = value
       
    IF (N_ELEMENTS (uname) EQ 0) AND (KEYWORD_SET (bitmap)) THEN BEGIN    
       MESSAGE, /CONTINUE, 'Missing keyword: UNAME.'
       RETURN, 0
    ENDIF

    ; Add the new menu item to the widget hierarchy
    
    IF (self.handler EQ '')       OR $
       (KEYWORD_SET (event_pro))  OR $
       (KEYWORD_SET (event_func)) THEN BEGIN
    
       id = WIDGET_BUTTON (parent, /DYNAMIC, $
           VALUE = value, UNAME = uname, BITMAP = bitmap, $
           EVENT_PRO = event_pro, EVENT_FUNC = event_func, $
           _EXTRA = extra)
       
    ENDIF ELSE BEGIN
    
       id = WIDGET_BUTTON (parent, /DYNAMIC, $
           VALUE = value, UNAME = uname, BITMAP = bitmap, $
           EVENT_PRO = self.handler, _EXTRA = extra)

    ENDELSE

    ; Store the new menu item
    
    button         = { WMENU_WIDGET }
    button.id      = id
    button.value   = PTR_NEW (value)
    button.type    = KEYWORD_SET (bitmap) ? self.TYPE_BITMAP : self.TYPE_TEXT
    button.handler = ''
    button.uname   = uname
    button.htype   = 0

    IF (N_ELEMENTS (event_pro) NE 0) THEN BEGIN
       button.handler = event_pro
       button.htype   = self.HTYPE_PROC
    ENDIF
       
    IF (N_ELEMENTS (event_func) NE 0) THEN BEGIN
       button.handler = event_func
       button.htype   = self.HTYPE_FUNC
    ENDIF
    
    IF (NOT PTR_VALID (self.buttons)) THEN BEGIN
    
       self.buttons = PTR_NEW (button)
       
    ENDIF ELSE BEGIN
    
       buttons = *self.buttons
       PTR_FREE, self.buttons
       self.buttons = PTR_NEW ([buttons, button])
       
    ENDELSE      
    
    RETURN, button.id
        
END    


; ----------------------------------------------------------------------------
; Remove a menu item
; ----------------------------------------------------------------------------
PRO WMenu::remove, id, ALL = all

    IF (NOT PTR_VALID (self.buttons)) THEN RETURN    
    
    buttons  = *self.buttons
    nButtons = N_ELEMENTS (*self.buttons)

    IF (KEYWORD_SET (all) OR N_PARAMS () EQ 0) THEN BEGIN
    
       ; Remove the entire menu
       
       FOR i = 0, nButtons - 1 DO $
           PTR_FREE, ((*self.buttons).value)[i]

       PTR_FREE, self.buttons
    
       IF (WIDGET_INFO (self.topID, /VALID)) THEN $
          WIDGET_CONTROL, self.topID, /DESTROY

       IF (WIDGET_INFO (self.tearoffID, /VALID)) THEN $
          WIDGET_CONTROL, self.tearoffID, /DESTROY

    ENDIF ELSE BEGIN

       ; Remove a single menu item
       
       ; Find item by UNAME or ID       
       type = SIZE (id, /TNAME)

       IF (type EQ 'LONG') THEN BEGIN

          idx1 = WHERE (buttons.id EQ id, found)
          idx2 = WHERE (buttons.id NE id)

       ENDIF ELSE BEGIN
          
          uname = STRING (id)              
          
          menuUnames = self->uname ()
          idx1 = WHERE (menuUnames EQ uname, found)
          idx2 = WHERE (menuUnames NE uname)
       
       ENDELSE
       IF (found EQ 0) THEN RETURN

       ; Remove the requested item, and all its children
       
       myButtons = buttons[idx1]
       FOR i = 0, found - 1 DO BEGIN
           
           button = myButtons[i]
           
           PTR_FREE, button.value
         ;  WIDGET_CONTROL, button.id, /DESTROY      ;button[i].id, 
                     
           children = self->findChildren (button.id)
           IF (children[0] NE 0) THEN BEGIN
           
              FOR j = 0, N_ELEMENTS (children) - 1 DO self->remove, children[j]
           
           ENDIF
                      
           WIDGET_CONTROL, button.id, /DESTROY      ;button[i].id, 
       
       ENDFOR

       buttons = buttons[idx2]
       PTR_FREE, self.buttons
       self.buttons = PTR_NEW (buttons)

       self->tearOffMenuRebuild
           
    ENDELSE
         
END


; ----------------------------------------------------------------------------
; Set button sensitivity
; If id is omitted, the entire menu sensitivity is set
; ----------------------------------------------------------------------------
PRO WMenu::setSensitive, id, state
 
    IF (NOT PTR_VALID (self.buttons)) THEN RETURN    

    buttons  = *self.buttons
    nButtons = N_ELEMENTS (buttons)
    
    IF (N_PARAMS () LT 2) THEN BEGIN
       
       ; Set sensitivity on the entire menu
       
       state = FIX (id)
       FOR i = 0, nButtons - 1 DO $
           WIDGET_CONTROL, buttons[i].id, SENSITIVE = state
        
    ENDIF ELSE BEGIN
       
       ; Set sensitivity on a single menu item

       ; Find item by VALUE or ID       
       type = SIZE (id, /TNAME)
       
       IF (type EQ 'LONG') THEN BEGIN
          
          FOR i = 0, N_ELEMENTS (id) - 1 DO $
              WIDGET_CONTROL, id[i], SENSITIVE = state[i]   
       
       ENDIF ELSE BEGIN
          
          uname = STRING (id)
          
          menuUnames = self->uname ()
          idx = WHERE (menuUnames EQ uname, found)
          IF (found NE 0) THEN BEGIN

             FOR j = 0, N_ELEMENTS (idx) - 1 DO $
                 WIDGET_CONTROL, buttons[idx[j]].id, SENSITIVE = state

          ENDIF
               
       ENDELSE    
    
    ENDELSE
    

END


; ----------------------------------------------------------------------------
; Set a button value
; ----------------------------------------------------------------------------
PRO WMenu::setValue, id, value, UNAME = uname

    IF (NOT PTR_VALID (self.buttons)) THEN RETURN    

    buttons  = *self.buttons
    nButtons = N_ELEMENTS (buttons)
    
    ; Find item by VALUE or ID       
    type = SIZE (id, /TNAME)

    IF (type EQ 'LONG') THEN BEGIN

       idx = WHERE (buttons.id EQ id)    
    
    ENDIF ELSE BEGIN
    
       uname = STRING (id)

       menuUnames = self->uname ()
       idx = WHERE (menuUnames EQ uname, found)
       IF (found NE 0) THEN $
          id = buttons[idx].id
        
    ENDELSE
    
    PTR_FREE, buttons[idx].value
    buttons[idx].value = PTR_NEW (value)
    
    PTR_FREE, self.buttons
    self.buttons = PTR_NEW (buttons)
    
    FOR i = 0, N_ELEMENTS (id) - 1 DO BEGIN
    
        WIDGET_CONTROL, id[i], SET_VALUE = value
    
        IF (N_ELEMENTS (uname) EQ 0) THEN uname = STRING (value)
        WIDGET_CONTROL, id[i], SET_UNAME = uname
        
    ENDFOR
    
    self->tearOffMenuRebuild
        
END


; ----------------------------------------------------------------------------
; Set the width (in characters) of the tearoff menu.  Minimum value is 6.
; ----------------------------------------------------------------------------
PRO WMenu::setTearoffWidth, width

    IF (N_ELEMENTS (width) EQ 0) THEN width = 6
    
    IF (WIDGET_INFO (self.tearoffItemID, /VALID)) THEN BEGIN

       value  = STRING (REPLICATE (BYTE ('-'), width > 6))      
       WIDGET_CONTROL, self.tearoffItemID, SET_VALUE = value

    ENDIF

END

; ----------------------------------------------------------------------------
; Return a STRING or STRARR of menu item values. 
; Bitmap values are return a null string.
; ----------------------------------------------------------------------------
FUNCTION WMenu::value, id

    buttons  = *self.buttons
    nButtons = N_ELEMENTS (buttons)

    IF (N_ELEMENTS (id) EQ 0) THEN BEGIN

       IF (NOT PTR_VALID (self.buttons)) THEN RETURN, ''    

       value = ''
       FOR i = 0, nButtons - 1 DO BEGIN

           IF (buttons[i].type EQ self.TYPE_BITMAP) THEN BEGIN

              value = [value, '']

           ENDIF ELSE BEGIN

              value = [value, *(buttons.value)[i]]

           ENDELSE

       
       ENDFOR
       
       IF (N_ELEMENTS (value) GT 1) THEN value = value[1:*]

    ENDIF ELSE BEGIN

       IF (NOT self->buttonExists (id, INDEX = index)) THEN RETURN, ''

       value = ''
       IF (buttons[index].type NE self.TYPE_BITMAP) THEN BEGIN
          
          WIDGET_CONTROL, id, GET_VALUE = value

       ENDIF
       
    ENDELSE

    RETURN, value
        
END


; ----------------------------------------------------------------------------
; Return a STRING or STRARR of menu item UNAMEs
; ----------------------------------------------------------------------------
FUNCTION WMenu::uname, id

   IF (N_ELEMENTS (id) EQ 0) THEN BEGIN
   
      IF (NOT PTR_VALID (self.buttons)) THEN RETURN, ''    

      buttons  = *self.buttons
      nButtons = N_ELEMENTS (buttons)

      uname = ''
      FOR i = 0, nButtons - 1 DO $
          uname = [uname, buttons[i].uname]

      IF (N_ELEMENTS (uname) GT 1) THEN uname = uname[1:*]

    ENDIF ELSE BEGIN

      idx = WHERE (buttons.id EQ id, found)
      IF (found EQ 0) THEN RETURN, ''
      
      uname = buttons[idx].uname

    ENDELSE
        
    RETURN, uname
        
END


; ----------------------------------------------------------------------------
; Verify that a button exists in the menu hierarchy
; ----------------------------------------------------------------------------
FUNCTION WMenu::buttonExists, id, INDEX = index

; COMPILE_OPT HIDDEN

    IF (NOT PTR_VALID (self.buttons)) THEN RETURN, 0    
    buttons = *self.buttons

    index = WHERE (buttons.id EQ id, found)
    
    RETURN, (found NE 0)

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO WMenu__define

    button = { WMENU_WIDGET, $
    
        id      : 0L,            $  
        type    : 0,             $  
        value   : PTR_NEW (),    $  
        uname   : '',            $
        handler : '',            $  
        htype   : 0              $  
    }
        
    obj = { WMENU, $
    
        TYPE_TEXT     : 0,         $
        TYPE_BITMAP   : 0,         $
        HTYPE_PROC    : 0,         $
        HTYPE_FUNC    : 0,         $
        handler       : '',        $
        parentID      : 0L,        $
        topID         : 0L,        $
        tearoffID     : 0L,        $
        tearoffItemID : 0L,        $
        buttons       : PTR_NEW () $

    }

END
