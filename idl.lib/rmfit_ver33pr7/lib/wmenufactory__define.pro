; ----------------------------------------------------------------------------
;+
; NAME:
;     WMenuFactory (OBJECT)
;
; PURPOSE:
;     Provide an interface for creating WMenu objects.  This class
;     handles instantiation and destruction of WMenu objects, relieving
;     the client from managing the those objects.
;
; CALLING SEQUENCE:
;     factory = OBJ_NEW ('WMenuFactory')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;
;    TEAROFF
;        Set this keyword to allow all menus constructed with the CREATE
;        method to be "torn off" into their own windows.  Setting this keyword
;        allows you omit having to set the TEAROFF keyword for each call
;        to the CREATE method.
;
; PUBLIC METHODS:
;
; ----------------------------------------------------------------------------
;    create (FUNCTION) : Create an instance of a WMenu object.
; ----------------------------------------------------------------------------
;
;    Inputs
;    ------
;
;    PARENT
;        A widget ID of the button's parent.  This ID must be a WIDGET_BASE. 
;
;    Outputs 
;    -------
;
;        The object reference of the new WMenu object.is returned.
;
;    Keywords
;    --------
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
;
;    TEAROFF
;        Set this keyword to allow the menu to be "torn off" into its
;        own window.
;
;    <WIDGET_BUTTON keywords>
;        Any other IDL WIDGET_BUTTON keywords may be used.
;
;
; ----------------------------------------------------------------------------
;    destroy (PRO) : Remove a WMenu object.  Note that by default, clients
;                    need not call the destroy method.  The WMenuFactory class
;                    is a container object that will destroy all WMenu objects
;                    when the factory object is destroyed.
; ----------------------------------------------------------------------------
;
;    Inputs
;    ------
;
;    REF
;        The object reference of the WMenu object to destroy.
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
;    reference (FUNCTION) : Return an object reference to a valid WMenu object.
; ----------------------------------------------------------------------------
;
;    Inputs
;    ------
;
;        NONE
;
;    Outputs 
;    -------
;
;        A WMenu object reference is returned.
;
;    Keywords
;    --------
;
;    UNAME (REQUIRED)
;        A STRING containing the UNAME of the WMenu object.
;
;
;
; EXAMPLE
;
;     base = WIDGET_BASE (/COLUMN, MBAR = menuBar)
;
;     ; Create a WMenu factory.  Make all menus created with the CREATE
;     ; method to be tear-off enabled.
;
;     menuFactory = OBJ_NEW ('WMenuFactory', /TEAROFF)
;
;     ; Create some WMenu objects.
;
;     menu1 = menuFactory->create (menuBar, VALUE = 'Menu 1')
;     id = menu1->add (VALUE = 'Item 1')
;     id = menu1->add (VALUE = 'Item 2')
;     id = menu1->add (VALUE = 'Item 3')
;
;     menu2 = menuFactory->create (menuBar, VALUE = 'Menu 2')
;     id = menu2->add (VALUE = 'Item 1')
;     id = menu2->add (VALUE = 'Item 2')
;     id = menu2->add (VALUE = 'Item 3')
;
;     menu3 = menuFactory->create (menuBar, VALUE = 'Menu 3', /HELP)
;     id = menu3->add (VALUE = 'Item 1')
;     id = menu3->add (VALUE = 'Item 2')
;
;        .
;        .
;        .
;    
;     ; Destroy the factory (and the WMenu objects it contains)
;     OBJ_DESTROY, menuFactory
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
FUNCTION WMenuFactory::init, TEAROFF = tearoff

    self.tearoff = KEYWORD_SET (tearoff)
               
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO WMenuFactory::cleanup

    FOR i = 0, self.count - 1 DO $
        OBJ_DESTROY, (*self.list)[i]
            
    PTR_FREE, self.list
        
END    


;------------------------------------------------------------------------------
; Create an instance of a WMenu object
;------------------------------------------------------------------------------
FUNCTION WMenuFactory::create, parent, VALUE = value, UNAME = uname, $
    BITMAP = bitmap, EVENT_HANDLER = event_handler, TEAROFF = tearoff, $
    _EXTRA = extra

    IF (self.tearoff) THEN tearoff = 1
    
    wMenu = OBJ_NEW ('WMenu', parent, $
        VALUE = value, UNAME = uname, BITMAP = bitmap, $
        EVENT_HANDLER = event_handler, TEAROFF = tearoff, $
        _EXTRA = extra)

    IF (OBJ_VALID (wMenu)) THEN BEGIN
    
       IF (self.count GT 0) THEN BEGIN

          list = *self.list
          PTR_FREE, self.list
          self.list = PTR_NEW ([list, wMenu])

       ENDIF ELSE BEGIN

          self.list = PTR_NEW (wMenu)

       ENDELSE

       self.count = self.count + 1
       
    ENDIF
       
    RETURN, wMenu
    
END


; ----------------------------------------------------------------------------
; Destroy an instance of a WMenu
; ----------------------------------------------------------------------------
PRO WMenuFactory::destroy, obj
        
    IF (OBJ_VALID (obj)) THEN BEGIN
        
       idx = WHERE (*self.list EQ obj, found)
       IF (found EQ 0) THEN RETURN

       list = *self.list
       idx = WHERE (*self.list NE obj)
       list = list[idx]
       PTR_FREE, self.list
       self.list = PTR_NEW (list)

       self.count = self.count - 1

       OBJ_DESTROY, obj

    ENDIF

END


; ----------------------------------------------------------------------------
; Return a reference to a valid WMenu object
; ----------------------------------------------------------------------------
FUNCTION WMenuFactory::reference, UNAME = uname
    
    null = OBJ_NEW ()
    
    IF (NOT PTR_VALID (self.list)) THEN RETURN, null

    IF (N_ELEMENTS (uname) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: UNAME.'
       RETURN, null
    ENDIF   
           
    nMenus = N_ELEMENTS (*self.list)
    
    FOR i = 0, nMenus - 1 DO BEGIN
    
        menu = (*self.list)[i]
        menuUnames = menu->uname ()
        
        idx = WHERE (menuUnames EQ uname, found)
        IF (found NE 0) THEN RETURN, menu
        
    ENDFOR
            
    RETURN, null
    
END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO WMenuFactory__define
        
    obj = { WMENUFACTORY, $
        
        tearoff : 0,         $
        count   : 0L,        $
        list    : PTR_NEW () $

    }

END