; ----------------------------------------------------------------------------
;+
; NAME:
;     PSCONFIG (FUNCTION)
;
; PURPOSE:
;     A widget to configure the IDL Postscript device.
;
;     PSCONFIG uses an internal object to do all the work, and also 
;     requires a custom object widget defined in rectange__define.pro
;
; CALLING SEQUENCE:
;     s = PSCONFIG ()
;
; INPUTS:
;     NONE
;    
; OUTPUTS:
;     s   A structure that can be passed to the DEVICE command via 
;         the _EXTRA keyword.
;      
; KEYWORDS:
;
;     DEFAULT
;         Set this keyword to a STRING containing one of the available
;         default page layouts.  The default is 'IDL Letter'.  The following
;         layouts are available:
;
;             'IDL Letter'
;             'Letter Portrait'
;             'Letter Portrait (3/4)'
;             'Letter Landscape'
;             'A4 Half Portrait'
;             'A4 Portrait'
;             'A4 Landscape'
;
;     INITIALIZE
;         Set this keyword to immediately return the default configuration
;         device structure without invoking the GUI.
;
;     CANCEL  
;         Set this keyword to a named variable that is set to 1 if the
;         "Cancel" button was selected, and 0 otherwise.  Note however
;         that you do not have to use this keyword to test the return 
;         value before calling the DEVICE procedure.  If "Cancel" is selected, 
;         the structure { NULL : 0B } is returned, which can be harmlessly 
;         passed to DEVICE.  See the example below.
;
;     TITLE
;         STRING containing the window title
;         (default = 'Configure Postscript Parameters')
;
;     PARENT
;         A widget ID specifying the parent of the PSCONFIG widget.
;
;     FGCOLOR
;         An RGB triple specifying the color of the plot region in the widget 
;         (default = [255, 255, 255])
;
;     BGCOLOR
;         An RGB triple specifying the color of the paper in the widget
;         (default = [100, 100, 100])
;
;     FILENAME
;         Set this keyword to a name of a psConfig configuration file
;         to read on startup (a sample file can be written from the GUI using
;         the Config->Save button).  Using this keyword does not alter the 
;         default settings that are restored when the "Defaults" button 
;         is clicked.
;
; EXAMPLE:
;
;     config = PSCONFIG (CANCEL = cancel)
;     
;     ; You don't need to worry if the user canceled the dialog.  
;     ; If so, PSCONFIG() will have no effect.
;     SET_PLOT, 'PS'
;     DEVICE, _EXTRA = config
;
;     ; So you could actually configure in a single command
;     SET_PLOT, 'PS'
;     DEVICE, _EXTRA = PSCONFIG ()
;
; MODIFICATION HISTORY:
;
;     RSM, 13 Mar 2000, v 1.02
;        Added "Letter Portrait (3/4)" default layout.  This layout leaves
;            room at the bottom of the page to add some text notes.
;
;     RSM, 22 Dec 1999, v 1.01
;        Added capability to load/store settings in a file.
;        Added keyword FILENAME to load settings from a file at startup.
;        Fixed error with setting Color and Bits Per Pixel simultaneously.
;        Fixed error when using the "Browse" button to set the filename.
;        Attempt to alter the file extension when the EPS button is activated.
;        Miscellaneous layout changes.
;        Added version number.
;
;     Written, Robert.Mallozzi@msfc.nasa.gov, 1999 December
;     Ideas from ps_form.pro by D. Fanning and cmps_form.pro by C. Markwardt.
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION PsConfig::init, FILENAME = filename, $
    DEFAULT = default, INITIALIZE = initialize, $
    PARENT = parent, TITLE = title, $
    FGCOLOR = fgcolor, BGCOLOR = bgcolor
        
    self.version = '1.01'    
    self.userDevice = !D.NAME           

    self->setDefaults, DEFAULT = default  
    IF (N_ELEMENTS (filename) NE 0) THEN $
       self->settings, /LOAD, FILENAME = filename
      
    IF (NOT KEYWORD_SET (initialize)) THEN BEGIN
      
       IF (N_ELEMENTS (fgcolor) EQ 0) THEN fgcolor = [255, 255, 255]
       IF (N_ELEMENTS (bgcolor) EQ 0) THEN bgcolor = [100, 100, 100]
       
       self->build, PARENT = parent, TITLE = title, $
           FGCOLOR = fgcolor, BGCOLOR = bgcolor
    
    ENDIF ELSE BEGIN
                         
       ; Convert to IDL landscape offsets
       
       IF (self.device.landscape) THEN BEGIN

          paperName = self.device.name
          s = REVERSE (self->paperSize (paperName))

          xPageSize = s[0]
          xoff = self.device.xoff
          yoff = self.device.yoff

          self->convertOffset, xPageSize, xoff, yoff, /TOIDL
          self->updateDevice, XOFF = xoff, YOFF = yoff

       ENDIF

    ENDELSE
    
    RETURN, 1
    
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PsConfig::cleanup

    PTR_FREE, self.layouts
    PTR_FREE, self.papers
    PTR_FREE, self.fonts

    SET_PLOT, self.userDevice
        
END    


; ----------------------------------------------------------------------------
; Set up default Postscript parameters
; ----------------------------------------------------------------------------
PRO PsConfig::setDefaults, DEFAULT = default 
    
    ; Postscript fonts
        
    PTR_FREE, self.fonts 

    self.fonts = PTR_NEW ([ $

        'AvantGarde-Book',              $	            
        'AvantGarde-BookOblique',       $	    
        'AvantGarde-Demi',              $	            
        'AvantGarde-DemiOblique',       $	    
        'Bookman-Demi',                 $	            
        'Bookman-DemiItalic',           $	    
        'Bookman-Light',                $ 	            
        'Bookman-LightItalic',          $	    
        'Courier',                      $	                    
        'Courier-Bold',                 $	                 
        'Courier-BoldOblique',          $	    
        'Courier-Oblique',              $	        
        'Helvetica',                    $	            
        'Helvetica-Bold',               $	            
        'Helvetica-BoldOblique',        $	    
        'Helvetica-Narrow',             $	    
        'Helvetica-Narrow-Bold',        $	    
        'Helvetica-Narrow-BoldOblique', $
        'Helvetica-Narrow-Oblique',     $
        'Helvetica-Oblique',            $	    
        'NewCenturySchlbk-Bold',        $	    
        'NewCenturySchlbk-BoldItalic',  $
        'NewCenturySchlbk-Italic',      $	    
        'NewCenturySchlbk-Roman',       $	
        'Palatino-Bold',                $   
        'Palatino-BoldItalic',          $     
        'Palatino-Italic',              $ 
        'Palatino-Roman',               $  
        'Symbol',                       $  
        'Times-Bold',                   $      
        'Times-BoldItalic',             $        
        'Times-Italic',                 $    
        'Times-Roman',                  $     
        'ZapfChancery-MediumItalic',    $  
        'ZapfDingbats'                  $ 
    ])
     
    ; Predefined paper formats.  
    ; More paper definitions are available below if needed - just add
    ; them to the self.papers definition, and they will then be available
    ; from the widget interface.
    
    PTR_FREE, self.papers
    
    self.papers = PTR_NEW ([ $

        ; US papers
        ;
        ;            Name       xsize  ysize  natural coordinates
        ;                        (in)   (in)  (cm = 0, inches = 1)
        
        { PSPAPER, 'Letter',     8.5,   11.0,   1 }, $
        { PSPAPER, 'Legal',      8.5,   14.0,   1 }, $
        { PSPAPER, 'Ledger',    11.0,   17.0,   1 }, $ 

        ; ISO Standard papers
        ;
        ;            Name       xsize  ysize  natural coordinates
        ;                        (mm)   (mm)  (cm = 0, inches = 1)
         
        { PSPAPER, '4A0',       1682,   2378,   0 }, $
        { PSPAPER, '2A0',       1189,   1682,   0 }, $
        { PSPAPER, 'A0',         841,   1189,   0 }, $
        { PSPAPER, 'A1',         594,    841,   0 }, $
        { PSPAPER, 'A2',         420,    594,   0 }, $
        { PSPAPER, 'A3',         297,    420,   0 }, $
        { PSPAPER, 'A4',         210,    297,   0 }, $
        { PSPAPER, 'A5',         148,    210,   0 }, $

        { PSPAPER, 'B0',        1000,   1414,   0 }, $
        { PSPAPER, 'B1',         707,   1000,   0 }, $
        { PSPAPER, 'B2',         500,    707,   0 }, $
        { PSPAPER, 'B3',         353,    500,   0 }, $
        { PSPAPER, 'B4',         250,    353,   0 }, $
        { PSPAPER, 'B5',         176,    250,   0 }, $

        { PSPAPER, 'C0',         917,   1297,   0 }, $
        { PSPAPER, 'C1',         648,    917,   0 }, $
        { PSPAPER, 'C2',         458,    648,   0 }, $
        { PSPAPER, 'C3',         324,    458,   0 }, $
        { PSPAPER, 'C4',         229,    324,   0 }, $
        { PSPAPER, 'C5',         162,    229,   0 }  $

        
        ; UNUSED PAPER DEFINITIONS
        
        ;{ PSPAPER, 'D',        17.0,    22.0,   1 }, $
        ;{ PSPAPER, 'F',        22.0,    34.0,   1 }, $
        ;{ PSPAPER, 'G',        34.0,    44.0,   1 }, $
        ;{ PSPAPER, 'H',        28.0,    40.0,   1 }, $
        ;{ PSPAPER, 'I',        11.0,    42.0,   1 }, $
        ;{ PSPAPER, 'J',        28.0,    48.0,   1 }, $
        ;{ PSPAPER, 'K',        34.0,    48.0,   1 }, $
        ;{ PSPAPER, 'L',        40.0,    48.0,   1 }, $
        ;{ PSPAPER, 'A6',        105,     148,   0 }, $
        ;{ PSPAPER, 'A7',         74,     105,   0 }, $
        ;{ PSPAPER, 'A8',         52,      74,   0 }, $
        ;{ PSPAPER, 'A9',         37,      52,   0 }, $
        ;{ PSPAPER, 'A10',        26,      37,   0 }, $
        ;{ PSPAPER, 'B6',        125,     176,   0 }, $
        ;{ PSPAPER, 'B7',         88,     125,   0 }, $
        ;{ PSPAPER, 'B8',         62,      88,   0 }, $
        ;{ PSPAPER, 'B9',         44,      62,   0 }, $
        ;{ PSPAPER, 'B10',        31,      44,   0 }, $
        ;{ PSPAPER, 'C6',         114,    162,   0 }, $
        ;{ PSPAPER, 'C7',          81,    114,   0 }, $
        ;{ PSPAPER, 'C8',          57,     81,   0 }, $
        ;{ PSPAPER, 'C9',          40,     57,   0 }, $
        ;{ PSPAPER, 'C10',         28,     40,   0 }, $
        
    ])
        
            
    ; Predefined page layouts.  Bounding box must be in cm.
    ; Offsets are always relative to lower left corner.
    ; See object definition at the bottom of this file for the
    ; format of the PSDEVICE structure.
        
    CD, CURRENT = dir
    file = FILEPATH ('idl.ps', ROOT_DIR = dir)

    PTR_FREE, self.layouts
    
    self.layouts = PTR_NEW ([ $
        
        { PSDEVICE, 'IDL Letter',            'Letter', file,   $
              17.78, 1.905, 12.70, 12.70,    0, 0, 4, 0, 0, 0, $
              'Helvetica', 12, 0 },                            $
               
        { PSDEVICE, 'Letter Portrait',       'Letter', file,   $
              17.78, 1.905, 24.13, 1.905,    0, 0, 4, 0, 0, 0, $
              'Helvetica', 12, 0 },                            $

        { PSDEVICE, 'Letter Portrait (3/4)', 'Letter', file,   $
              17.78, 1.905, 19.05, 6.985,    0, 0, 4, 0, 0, 0, $
              'Helvetica', 12, 0 },                            $

        { PSDEVICE, 'Letter Landscape',      'Letter', file,   $
              24.13, 1.905, 17.78, 1.905,    0, 0, 4, 0, 0, 1, $
              'Helvetica', 12, 0 },                            $
 
        { PSDEVICE, 'A4 Half Portrait',      'A4', file,       $
              17.19, 1.905, 12.945, 14.85,   0, 0, 4, 0, 0, 0, $
              'Helvetica', 12, 0 },                            $

        { PSDEVICE, 'A4 Portrait',           'A4', file,       $
              17.19, 1.905, 25.89, 1.905,    0, 0, 4, 0, 0, 0, $
              'Helvetica', 12, 0 },                            $
 
        { PSDEVICE, 'A4 Landscape',          'A4', file,       $
              25.89, 1.905, 17.19, 1.905,    0, 0, 4, 0, 0, 1, $
              'Helvetica', 12, 0 }                             $
    ])

    ; Set the default device  
    
    IF (N_ELEMENTS (default) EQ 0) THEN $
       default = 'IDL Letter'
    
    idx = WHERE (STRUPCASE ((*self.layouts).name) EQ STRUPCASE (default), cnt)
    IF (cnt EQ 0) THEN idx = 0
    
    self.device = (*self.layouts)[idx[0]]
        
END


; ----------------------------------------------------------------------------
; Update current Postscript device
; ----------------------------------------------------------------------------
PRO PsConfig::updateDevice, _EXTRA = data

    IF (N_ELEMENTS (data) GT 0) THEN $
       names = TAG_NAMES (data)

    d = self.device
           
    FOR i = 0, N_ELEMENTS (names) - 1 DO BEGIN
    
        CASE (STRUPCASE (names[i])) OF
        
            'NAME'           : d.name           = STRING (data.name)
            'PAPER'          : d.paper          = STRING (data.paper)
            'FILENAME'       : d.filename       = STRING (data.filename)
            'XSIZE'          : d.xsize          = FLOAT (data.xsize)
            'YSIZE'          : d.ysize          = FLOAT (data.ysize)
            'XOFF'           : d.xoff           = FLOAT (data.xoff)
            'YOFF'           : d.yoff           = FLOAT (data.yoff)        
            'INCHES'         : d.inches         = FIX (data.inches)
            'COLOR'          : d.color          = FIX (data.color)
            'BITS_PER_PIXEL' : d.bits_per_pixel = FIX (data.bits_per_pixel)
            'ENCAPSULATED'   : d.encapsulated   = FIX (data.encapsulated)
            'ISOLATIN1'      : d.isolatin1      = FIX (data.isolatin1)
            'LANDSCAPE'      : d.landscape      = FIX (data.landscape)
            'SET_FONT'       : d.set_font       = STRING (data.set_font)
            'FONT_SIZE'      : d.font_size      = FIX (data.font_size)
            'PREVIEW'        : d.preview        = FIX (data.preview)
                        
        ENDCASE
    
    ENDFOR   
    
    self.device = d

END


; ----------------------------------------------------------------------------
; Center page in x and/or y direction
; ----------------------------------------------------------------------------
PRO PsConfig::centerPage, value

    CASE (value) OF
    
        'X'    : self.PageBox->center, /X
        'Y'    : self.PageBox->center, /Y
        'XY'   : self.PageBox->center
        'Fill' : self.PageBox->center, /FILL

        ELSE :
            
    ENDCASE

    self->updatePage

END


; ----------------------------------------------------------------------------
; Event handler
; ----------------------------------------------------------------------------
PRO PsConfig_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->eventHandler, event
    
END

PRO PsConfig::eventHandler, event

    type = TAG_NAMES (event, /STRUCTURE)
 
    CASE (type) OF
    

        'WIDGET_BUTTON' : BEGIN
        
            WIDGET_CONTROL, event.id, GET_VALUE = value

            CASE (value) OF
            
                'Moving the Bounding Box'   : self->help, /MOVE
                'Resizing the Bounding Box' : self->help, /RESIZE
                'Setting Paper Sizes'       : self->help, /PAPER
                'About'                     : self->help, /ABOUT

                'Accept' : BEGIN

                    ; Convert to IDL landscape offsets
                    
                    IF (self.device.landscape) THEN BEGIN

                       paperName = self.device.name
                       s = REVERSE (self->paperSize (paperName))

                       xPageSize = s[0]
                       xoff = self.device.xoff
                       yoff = self.device.yoff

                       self->convertOffset, xPageSize, xoff, yoff, /TOIDL
                       self->updateDevice, XOFF = xoff, YOFF = yoff

                    ENDIF
                             
                    self.cancel = 0
                    OBJ_DESTROY, self.PageBox
                    WIDGET_CONTROL, event.top, /DESTROY
                    END

                'Defaults' : BEGIN
                    self->setDefaults
                    self->updateForm
                    self->drawPage
                    END

                'Cancel' : BEGIN
                    self.cancel = 1
                    OBJ_DESTROY, self.PageBox
                    WIDGET_CONTROL, event.top, /DESTROY
                    END

                '...' : self->selectFile
                
                'XY'   : self->centerPage, value
                'X'    : self->centerPage, value
                'Y'    : self->centerPage, value
                'Fill' : self->centerPage, value
                                            
                'Inches  '    : BEGIN
                    self.inches = event.select  
                    self->updateForm
                    END
                    
                'Centimeters' : BEGIN
                    self.inches = (event.select ? 0 : 1)
                    self->updateForm
                    END
                
                'Portrait'    : BEGIN                    
                    self.device.landscape = (event.select ? 0 : 1)
                    IF (event.select) THEN BEGIN
                       self.PageBox->center, /FILL
                       self->updateDevice, NAME = 'Custom' 
                       self->updatePage
                    ENDIF
                    END

                'Landscape'   : BEGIN
                    self.device.landscape = event.select
                    IF (event.select) THEN BEGIN
                       self.PageBox->center, /FILL
                       self->updateDevice, NAME = 'Custom' 
                       self->updatePage
                    ENDIF
                    END

                'Encapsulate (EPS)'  : BEGIN
                    self->updateDevice, ENCAPSULATED = FIX (event.select)
                    file = self.device.filename
                    
		    IF (FLOAT (!VERSION.RELEASE) GE 5.3) THEN BEGIN
		    
		       IF (event.select) THEN BEGIN
                	  pos = CALL_FUNCTION ( $
			      'STREGEX', file, '\.ps$', /FOLD_CASE)
                	  IF (pos NE -1) THEN $
                             file = STRMID (file, 0, pos) + '.eps'
                       ENDIF ELSE BEGIN
                	  pos = CALL_FUNCTION ( $
			      'STREGEX', file, '\.eps$', /FOLD_CASE)
                	  IF (pos NE -1) THEN $
                             file = STRMID (file, 0, pos) + '.ps'
                       ENDELSE
                    ENDIF
                    
                    self->updateDevice, FILENAME = file
                    self->updateForm
                    END
                         
                'ISOLatin1 Encoding' : $
                    self->updateDevice, ISOLATIN1 = FIX (event.select)

                'B & W' : BEGIN
                    self->updateDevice, COLOR = FIX (event.select ? 0 : 1)
                     END

                'Color' : BEGIN
                    self->updateDevice, COLOR = FIX (event.select)
                    END
		    
                    ;self->updateDevice, COLOR = FIX (event.select)
                       
                '1' : self->updateDevice, BITS_PER_PIXEL = 1
                '2' : self->updateDevice, BITS_PER_PIXEL = 2
                '4' : self->updateDevice, BITS_PER_PIXEL = 4
                '8' : self->updateDevice, BITS_PER_PIXEL = 8
                       
                'EPSI ' : self->updateDevice, PREVIEW = 1
                'EPSF ' : self->updateDevice, PREVIEW = 2

                'None' : IF (event.id EQ self.preview0ID) THEN $
                            self->updateDevice, PREVIEW = 0

                'Load' : BEGIN
                    self->settings, /LOAD, ERROR = error
                    IF (NOT error) THEN BEGIN
                       self->updateForm
                       self->drawPage
                    ENDIF
                    END

                'Save' : BEGIN
                    self->settings, /SAVE
                    END

                ELSE : 

            ENDCASE
            END
    

        'WIDGET_DROPLIST' : BEGIN
            
            CASE (event.id) OF
            
                self.layoutID : BEGIN
                
                    IF (event.index EQ 0) THEN $ ; Custom
                       RETURN        

                    idx = event.index - 1
                    self.device = (*self.layouts)[idx]
                    self->updateForm
                    self->drawPage
                    END

                self.paperListID : BEGIN

                    paper = ((*self.papers).name)[event.index]
                    self->updateDevice, NAME = 'Custom', PAPER = paper 
                    self->updateForm
                    self->drawPage
                    self.PageBox->center
                    END                       

                self.fontID : BEGIN
                   
                    fontName = (*self.fonts)[event.index]
                    self->updateDevice, SET_FONT = fontName
                    END   

                ELSE :
            
            ENDCASE
            END
                        

        'WIDGET_KBRD_FOCUS' : BEGIN
            IF (event.enter NE 0) THEN RETURN
            self->updateTextBox, event.id
            END

        'WIDGET_TEXT_CH' : self->updateTextBox, event.id
            
        ELSE : 
         
    ENDCASE

END


; ----------------------------------------------------------------------------
; This handler is called when text boxes are changed or lose focus
; ----------------------------------------------------------------------------
PRO PsConfig::updateTextBox, id

    CASE (id) OF

        self.fontSizeID : BEGIN

            WIDGET_CONTROL, self.fontSizeID, GET_VALUE = value
            self->updateDevice, FONT_SIZE = FIX (value)
            END

        self.fileID : BEGIN

            WIDGET_CONTROL, self.fileID, GET_VALUE = value
            self->updateDevice, FILENAME = STRING (value)
            END

        ELSE : BEGIN ; bounding box

            WIDGET_CONTROL, self.xsizeID, GET_VALUE = xsize
            WIDGET_CONTROL, self.ysizeID, GET_VALUE = ysize
            WIDGET_CONTROL, self.xoffID,  GET_VALUE = xoff
            WIDGET_CONTROL, self.yoffID,  GET_VALUE = yoff

            factor = 1.0
            IF (self.inches) THEN factor = 2.54

            self->updateDevice, NAME = 'Custom', $
                XSIZE = FLOAT (xsize) * factor,  $
                YSIZE = FLOAT (ysize) * factor,  $
                XOFF  = FLOAT (xoff) * factor,   $
                YOFF  = FLOAT (yoff) * factor                    

            self->drawPage
            self->updateForm
            END

    ENDCASE        

END


; ----------------------------------------------------------------------------
; This handler is called on a mouse release on the bounding box
; ----------------------------------------------------------------------------
PRO PsConfig::updatePage, x, y

    ; Compute the xsize, ysize of the bounding box
    
    s = self->paperSize ()
    IF (self.device.landscape) THEN s = REVERSE (s)

    vertices = self.PageBox->vertices ()
    xFrac = MAX (vertices[0, *]) - MIN (vertices[0, *])
    yFrac = MAX (vertices[1, *]) - MIN (vertices[1, *])

    xsize = s[0] * xFrac
    ysize = s[1] * yFrac
     
 
    ; Compute the xoff, yoff of the bounding box

    xoff = MIN (vertices[0, *]) * s[0]
    yoff = MIN (vertices[1, *]) * s[1]
            
    self->updateDevice, NAME = 'Custom', $
        XSIZE = xsize, YSIZE = ysize, XOFF = xoff, YOFF = yoff    

    self->updateForm
    self->drawPage

END


; ----------------------------------------------------------------------------
; Build PS configuration dialog
; ----------------------------------------------------------------------------
PRO PsConfig::build, PARENT = parent, TITLE = title, $
    FGCOLOR = fgcolor, BGCOLOR = bgcolor
                
                
    IF (N_ELEMENTS (title) EQ 0) THEN $
       title = 'Configure Postscript Parameters'

    IF (N_ELEMENTS (parent) NE 0) THEN $
       extra = { GROUP_LEADER : parent, MODAL : 1 }
    
    self.topID = WIDGET_BASE (TITLE = title, $
        /COLUMN, TLB_FRAME_ATTR = 1 + 8, /BASE_ALIGN_CENTER, _EXTRA = extra)

    tlb    = WIDGET_BASE (self.topID, /COL, /FRAME)
    tlbRow = WIDGET_BASE (tlb, /ROW)
   
    leftBase = WIDGET_BASE (tlbRow, /COLUMN)
    
    base = WIDGET_BASE (leftBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Paper')
    self.paperListID = WIDGET_DROPLIST (base, VALUE = (*self.papers).name)
    self.paperSizeID = WIDGET_LABEL (base, VALUE = ' ', /DYNAMIC)

    base = WIDGET_BASE (leftBase, COLUMN = 4, /GRID)

    w = WIDGET_LABEL (base, VALUE = 'XSize', /ALIGN_RIGHT)
    w = WIDGET_LABEL (base, VALUE = 'XOffset', /ALIGN_RIGHT)
    self.xsizeID = WIDGET_TEXT  (base, XSIZE = 6, $
        /EDIT, VALUE = '', /KBRD_FOCUS_EV)
    self.xoffID  = WIDGET_TEXT  (base, XSIZE = 6, $
        /EDIT, VALUE = '', /KBRD_FOCUS_EV)

    w = WIDGET_LABEL (base, VALUE = 'YSize', /ALIGN_RIGHT)
    w = WIDGET_LABEL (base, VALUE = 'YOffset', /ALIGN_RIGHT)
    self.ysizeID = WIDGET_TEXT  (base, XSIZE = 6, $
        /EDIT, VALUE = '', /KBRD_FOCUS_EV)  
    self.yoffID  = WIDGET_TEXT  (base, XSIZE = 6, $
        /EDIT, VALUE = '', /KBRD_FOCUS_EV)
  
    rowBase = WIDGET_BASE (leftBase, /ROW, /EXCLUSIVE)
    self.inchesID = WIDGET_BUTTON (rowBase, VALUE = 'Inches  ')
    self.cmID = WIDGET_BUTTON (rowBase, VALUE = 'Centimeters')
       
    rowBase = WIDGET_BASE (leftBase, /ROW, /EXCLUSIVE)
    self.portraitID = WIDGET_BUTTON (rowBase, VALUE = 'Portrait')
    self.landscapeID = WIDGET_BUTTON (rowBase, VALUE = 'Landscape')

    base = WIDGET_BASE (leftBase, /COLUMN, /NONEXCLUSIVE)
    self.epsID = WIDGET_BUTTON (base, VALUE = 'Encapsulate (EPS)')

    base = WIDGET_BASE (leftBase, /COLUMN, /NONEXCLUSIVE)
    self.isoLatinID = WIDGET_BUTTON (base, VALUE = 'ISOLatin1 Encoding')

    ;base = WIDGET_BASE (leftBase, /COLUMN, /NONEXCLUSIVE)
    ;self.colorID = WIDGET_BUTTON (base, VALUE = 'Color')

    base = WIDGET_BASE (leftBase, /ROW, /EXCLUSIVE)
    self.bwID    = WIDGET_BUTTON (base, VALUE = 'B & W')
    self.colorID = WIDGET_BUTTON (base, VALUE = 'Color')

    base = WIDGET_BASE (leftBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Bits Per Pixel')
    b = WIDGET_BASE (base, /ROW, /EXCLUSIVE)
    self.bpp1ID = WIDGET_BUTTON (b, VALUE = '1')
    self.bpp2ID = WIDGET_BUTTON (b, VALUE = '2')
    self.bpp4ID = WIDGET_BUTTON (b, VALUE = '4')
    self.bpp8ID = WIDGET_BUTTON (b, VALUE = '8')

    base = WIDGET_BASE (leftBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Preview')
    b = WIDGET_BASE (base, /ROW, /EXCLUSIVE)
    self.preview0ID = WIDGET_BUTTON (b, VALUE = 'None')
    self.preview1ID = WIDGET_BUTTON (b, VALUE = 'EPSI ')
    self.preview2ID = WIDGET_BUTTON (b, VALUE = 'EPSF ')


    rightBase = WIDGET_BASE (tlbRow, /COLUMN, /BASE_ALIGN_CENTER)

    ; Turn off updates to the widget to avoid flashing.  When the Rectangle
    ; object is instantiated, it invokes WIDGET_CONTROL to get the object
    ; reference for its draw widget.  This causes its parent widget hierarchy
    ; to be mapped to the screen...
    
    WIDGET_CONTROL, self.topID, UPDATE = 0

    self.PageBox = OBJ_NEW ('Rectangle', PARENT = rightBase, $
        XSIZE = 100, YSIZE = 100, $
        COLOR = fgcolor, BACKGROUND = bgcolor, $
        RELEASE = 'updatePage', OBJECT = self)

    base = WIDGET_BASE (rightBase, /GRID, COLUMN = 4)
    button = WIDGET_BUTTON (base, VALUE = 'X')
    button = WIDGET_BUTTON (base, VALUE = 'Y')
    button = WIDGET_BUTTON (base, VALUE = 'XY')
    button = WIDGET_BUTTON (base, VALUE = 'Fill')
    
    names = ['Custom', (*self.layouts).name]
    self.layoutID = WIDGET_DROPLIST (rightBase, VALUE = names, UVALUE = names)
           
    base = WIDGET_BASE (tlb, /ROW)
    w = WIDGET_LABEL (base, VALUE = '  Font')    
    self.fontID = WIDGET_DROPLIST (base, VALUE = *self.fonts)
    self.fontSizeID = WIDGET_TEXT (base, XSIZE = 3, $
        /EDIT, VALUE = '12', /KBRD_FOCUS_EV)
    w = WIDGET_LABEL (base, VALUE = 'pt')    

    base = WIDGET_BASE (tlb, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Filename')
    self.fileID = WIDGET_TEXT (base, /EDIT, VALUE = '', $
        XSIZE = 40, /KBRD_FOCUS_EV)
    w = WIDGET_BUTTON (base, VALUE = '...')

    base = WIDGET_BASE (self.topID, /ROW, /ALIGN_CENTER, SPACE = 20, /GRID)
    w = WIDGET_BUTTON (base, VALUE = 'Accept')
    w = WIDGET_BUTTON (base, VALUE = 'Defaults')
    w = WIDGET_BUTTON (base, VALUE = 'Cancel')
    w = WIDGET_BUTTON (base, VALUE = 'Config', /MENU)
        ww = WIDGET_BUTTON (w, VALUE = 'Load')
        ww = WIDGET_BUTTON (w, VALUE = 'Save')
    w = WIDGET_BUTTON (base, VALUE = 'Help', /MENU)
        ww = WIDGET_BUTTON (w, VALUE = 'Moving the Bounding Box')
        ww = WIDGET_BUTTON (w, VALUE = 'Resizing the Bounding Box')
        ww = WIDGET_BUTTON (w, VALUE = 'Setting Paper Sizes')
        ww = WIDGET_BUTTON (w, VALUE = 'About', /SEPARATOR)
        
    WIDGET_CONTROL, self.topID, /REALIZE
    self.PageBox->show

    WIDGET_CONTROL, self.topID, UPDATE = 1
    WIDGET_CONTROL, self.topID, SET_UVALUE = self

    self->updateForm
    self->drawPage

    XMANAGER, 'PsConfig', self.topID

END


; ----------------------------------------------------------------------------
; Update the dialog
; ----------------------------------------------------------------------------
PRO PsConfig::updateForm
        
    f = '(F6.2)'
    factor = 1.0
    IF (self.inches) THEN factor = 1.0 / 2.54
    
    xsize = STRTRIM (STRING (factor * self.device.xsize, FORMAT = f), 2)
    ysize = STRTRIM (STRING (factor * self.device.ysize, FORMAT = f), 2)
    xoff  = STRTRIM (STRING (factor * self.device.xoff,  FORMAT = f), 2)
    yoff  = STRTRIM (STRING (factor * self.device.yoff,  FORMAT = f), 2)

    WIDGET_CONTROL, self.xsizeID, SET_VALUE = xsize
    WIDGET_CONTROL, self.ysizeID, SET_VALUE = ysize
    WIDGET_CONTROL, self.xoffID,  SET_VALUE = xoff
    WIDGET_CONTROL, self.yoffID,  SET_VALUE = yoff

    WIDGET_CONTROL, self.inchesID, SET_BUTTON = self.inches EQ 1
    WIDGET_CONTROL, self.cmID,     SET_BUTTON = self.inches EQ 0

    WIDGET_CONTROL, self.portraitID,  SET_BUTTON = self.device.landscape EQ 0
    WIDGET_CONTROL, self.landscapeID, SET_BUTTON = self.device.landscape EQ 1

    WIDGET_CONTROL, self.epsID, SET_BUTTON = self.device.encapsulated EQ 1
    WIDGET_CONTROL, self.isoLatinID, SET_BUTTON = self.device.isolatin1 EQ 1

    WIDGET_CONTROL, self.bwID,    SET_BUTTON = self.device.color EQ 0
    WIDGET_CONTROL, self.colorID, SET_BUTTON = self.device.color EQ 1

    WIDGET_CONTROL, self.bpp1ID, SET_BUTTON = self.device.bits_per_pixel EQ 1
    WIDGET_CONTROL, self.bpp2ID, SET_BUTTON = self.device.bits_per_pixel EQ 2
    WIDGET_CONTROL, self.bpp4ID, SET_BUTTON = self.device.bits_per_pixel EQ 4
    WIDGET_CONTROL, self.bpp8ID, SET_BUTTON = self.device.bits_per_pixel EQ 8
    
    WIDGET_CONTROL, self.preview0ID, SET_BUTTON = self.device.preview EQ 0
    WIDGET_CONTROL, self.preview1ID, SET_BUTTON = self.device.preview EQ 1
    WIDGET_CONTROL, self.preview2ID, SET_BUTTON = self.device.preview EQ 2

    name = STRUPCASE (self.device.name)
    idx = (WHERE (STRUPCASE ((*self.layouts).name) EQ name, cnt))[0]
    IF (cnt EQ 0) THEN idx = -1
    WIDGET_CONTROL, self.layoutID, SET_DROPLIST_SELECT = idx + 1

    name = STRUPCASE ((*self.papers).name)
    idx = (WHERE (STRUPCASE (self.device.paper) EQ name, cnt))[0]
    IF (cnt EQ 0) THEN idx = 0
    WIDGET_CONTROL, self.paperListID, SET_DROPLIST_SELECT = idx  

    s = self->paperSize (NATURAL = natural)
    xsize = s[0] ; cm (natural = 0) or inches (natural = 1)
    ysize = s[1] ; cm (natural = 0) or inches (natural = 1)

    xp    = STRTRIM (STRING (xsize, FORMAT = '(F10.2)'), 2)
    yp    = STRTRIM (STRING (ysize, FORMAT = '(F10.2)'), 2)
    unit  = (natural EQ 1) ? 'in' : 'cm'
    label = STRING (xp, yp, unit, FORMAT = '(A0, " x ", A0, " ", A0)')
    WIDGET_CONTROL, self.paperSizeID, SET_VALUE = label
    
    ;name = STRUPCASE (self.device.set_font)
    ;idx = (WHERE (STRUPCASE (*self.fonts) EQ name, cnt))[0]
    ;IF (cnt EQ 0) THEN idx = 0
    ;WIDGET_CONTROL, self.fontID, SET_DROPLIST_SELECT = idx  

    ;WIDGET_CONTROL, self.fontSizeID, $
    ;    SET_VALUE = STRTRIM (self.device.font_size, 2)

    WIDGET_CONTROL, self.fileID, SET_VALUE = self.device.filename
    WIDGET_CONTROL, self.fileID, $
        SET_TEXT_SELECT = [STRLEN (self.device.filename), 0]

END


; ----------------------------------------------------------------------------
; Return current paper size.
;
; If NATURAL is set to a named variable, the paper size will be returned in 
; its natural coordinates (cm or inches), and NATURAL will be set to an 
; integer specifying the coordinates (0 = cm, 1 = inches).  Otherwise, cm 
; are returned.
; ----------------------------------------------------------------------------
FUNCTION PsConfig::paperSize, paperName, NATURAL = natural

    IF (N_ELEMENTS (paperName) EQ 0) THEN $
       paperName = self.device.paper

    idx = (WHERE ((*self.papers).name EQ paperName, cnt))[0]
    IF (cnt EQ 0) THEN idx = 0 ; Letter
    
    data = (*self.papers)[idx]

    IF (data.coords EQ 0) THEN BEGIN
    
       xsize = data.xsize / 10.0 ; mm->cm
       ysize = data.ysize / 10.0
    
    ENDIF ELSE BEGIN
    
       xsize = data.xsize * 2.54 ; inches->cm
       ysize = data.ysize * 2.54

    ENDELSE
    
    IF (ARG_PRESENT (natural)) THEN BEGIN
              
       IF (data.coords EQ 1) THEN BEGIN
       
          xsize = xsize / 2.54 ; cm->inches
          ysize = ysize / 2.54
          
       ENDIF
    
       natural = data.coords
       
    ENDIF

    RETURN, [xsize, ysize]
    
END


; ----------------------------------------------------------------------------
; Update the page widget.
;
; Set the size of the page drawing area on the screen based on the
; current paper size and orientation.  Try to make the maximum size of 
; the draw area to be 7 cm.
; ----------------------------------------------------------------------------
PRO PsConfig::drawPage

    ; Draw the page
    
    s = self->paperSize ()
    xsize = s[0] ; cm
    ysize = s[1] ; cm
        
    maxv = MAX ([xsize, ysize])
    factor = 7.0 / maxv
    
    IF (self.device.landscape) THEN BEGIN
       temp  = xsize
       xsize = ysize
       ysize = temp
    ENDIF
   
    xsize = xsize * factor
    ysize = ysize * factor
        
    self.PageBox->setSize, XSIZE = xsize, YSIZE = ysize, UNITS = 2 ; cm


    ; Draw the bounding box on the page
    
    bbxsize = self.device.xsize * factor ; cm
    bbysize = self.device.ysize * factor

    bbxoff  = self.device.xoff * factor  ; cm
    bbyoff  = self.device.yoff * factor

    ; Compute the vertices of the bounding box

    vertices = FLTARR (2, 4)

    xpt = bbxoff / FLOAT (xsize)
    ypt = bbyoff / FLOAT (ysize)
          
    vertices[0, 0] = xpt
    vertices[1, 0] = ypt
    
    vertices[0, 1] = xpt
    vertices[1, 1] = ypt + (bbysize / FLOAT (ysize))
    
    vertices[0, 2] = xpt + (bbxsize / FLOAT (xsize))
    vertices[1, 2] = ypt + (bbysize / FLOAT (ysize))
    
    vertices[0, 3] = xpt + (bbxsize / FLOAT (xsize))
    vertices[1, 3] = ypt
        
    self.PageBox->setVertices, vertices
    
END


; ----------------------------------------------------------------------------
; Convert between the strange IDL definition of YOFFSET and XOFFSET in
; landscape mode to a form where the XOFFSET and YOFFSET always refer to 
; the lower-left hand corner of the page.
; ----------------------------------------------------------------------------
PRO PsConfig::convertOffset, xPageSize, xoff, yoff, $
    TOIDL = toidl, FROMIDL = fromidl
       
    ixoff = xoff 
    iyoff = yoff

    IF (KEYWORD_SET (fromidl)) THEN BEGIN
       yoff = ixoff
       xoff = xPageSize - iyoff
    ENDIF

    IF (KEYWORD_SET (toidl)) THEN BEGIN
       xoff = iyoff
       yoff = xPageSize - ixoff
    ENDIF

END


; ----------------------------------------------------------------------------
; Show a help dialog
; ----------------------------------------------------------------------------
PRO PsConfig::help, $
    MOVE   = move,   $
    RESIZE = resize, $
    PAPER  = paper,  $
    ABOUT  = about

    IF (KEYWORD_SET (about)) THEN BEGIN

       s = [ $
           'PsConfig v' + self.version, $
           'Robert.Mallozzi@msfc.nasa.gov', $
           ' ', $
           'A tool to configure the IDL Postscript device.']
       
       t = 'About'
     
    ENDIF
       
    IF (KEYWORD_SET (move)) THEN BEGIN

       s = [ $
           'You can move the bounding box (BB) in several ways:', $
           ' ', $
           '   1. Use the buttons below the drawing area', $
           '      to center the BB on the page.', $
           '   2. Click with the left mouse button and drag in the', $
           '      center region of the BB to move it on the page.', $
           '   3. Double click with the left mouse button to', $
           '      center the BB on the page.', $
           '   4. Single click with the middle mouse button to', $
           '      center the BB in the x direction.', $
           '   5. Single click with the right mouse button to', $
           '      center the BB in the y direction.', $
           '   6. Double click with the right mouse button to', $
           '      make the BB fill the page.']
    
        t = 'Moving the Bounding Box'
    
    ENDIF
            
    IF (KEYWORD_SET (resize)) THEN BEGIN

       s = [ $
           'You can resize the bounding box (BB) by using the mouse.', $
           'Place the mouse near the edge of the BB, then left', $
           'click and drag to resize the box.  If you place the', $
           'mouse near a corner, resize will occur in both the x', $
           'and y directions.  If you place the mouse near an edge', $
           '(away from a corner), resize will occur in the x or y', $
           'direction only.', $
           ' ', $
           'You can also set the size by entering values directly', $
           'into the text boxes at the upper left.']

        t = 'Resizing the Bounding Box'
    
    ENDIF
        
    IF (KEYWORD_SET (paper)) THEN BEGIN

       s = [ $
           'You can select from several predefined layouts using', $
           'the droplist below the drawing area.', $
           ' ', $
           'You can set a custom page layout by selecting the paper', $
           'type from the Paper droplist, and then setting the bounding', $
           'box parameters using the mouse or the text entry boxes at', $
           'the upper left of the widget.']

        t = 'Setting the Paper Size'
    
    ENDIF
        
    d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.topID, s, TITLE = t)
    
END


; ----------------------------------------------------------------------------
; Allow user to select a filename for writing
; ----------------------------------------------------------------------------
PRO PsConfig::selectFile, parent, FILTER = filter

    IF (N_ELEMENTS (filter) EQ 0) THEN $
       filter = self.device.encapsulated ? '*.eps' : '*.ps'
    
    WIDGET_CONTROL, self.fileID, GET_VALUE = initFilename
    filename = DIALOG_PICKFILE (/WRITE, FILE = initFilename[0], $
        DIALOG_PARENT = self.topID, FILTER = filter, /OVERWRITE_PROMPT)
    
    ; If the user accidently puts a space in front of the filename...
    retpos = STRPOS(filename, STRING(10B))
    IF (retpos NE 0) THEN BEGIN
     filename = STRMID(filename, retpos + 1)
    ENDIF

    IF (filename NE '') OR $
       ((STRPOS(filename, '/', /REVERSE_SEARCH) + 1) NE STRLEN(filename)) THEN BEGIN
       
       WIDGET_CONTROL, self.fileID, SET_VALUE = filename
       WIDGET_CONTROL, self.fileID, SET_TEXT_SELECT = [STRLEN (filename), 0]
       
       self->updateDevice, FILENAME = filename
     
    ENDIF ELSE self->updateDevice, FILENAME = initFilename
     
END


; ----------------------------------------------------------------------------
; Return object data
; ----------------------------------------------------------------------------
FUNCTION PsConfig::config  &  RETURN, self.device  &  END
FUNCTION PsConfig::cancel  &  RETURN, self.cancel  &  END


; -----------------------------------------------------------------------------
; Load or save settings from a file
; -----------------------------------------------------------------------------
PRO PsConfig::settings, LOAD = load, SAVE = save, $
    FILENAME = filename, ERROR = error
    
    error = 0
    
    loading = KEYWORD_SET (load)
    saving  = KEYWORD_SET (save)
    IF (loading + saving EQ 0) THEN loading = 1
        

    IF (loading) THEN BEGIN
    
       IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN

          filename = DIALOG_PICKFILE (/READ, /MUST_EXIST, $
              FILTER = '*.cfg', DIALOG_PARENT = self.topID)

       ENDIF
       
       c = FINDFILE (filename, COUNT = found)
       IF ((found EQ 0) OR (filename EQ '')) THEN BEGIN ; FINDFILE is broken
          error = 1
          RETURN      	  
       ENDIF

       ; If the user accidently puts a space in front of the filename...
       retpos = STRPOS(filename, STRING(10B))
       IF (retpos NE 0) THEN BEGIN
          filename = STRMID(filename, retpos + 1)
       ENDIF

       OPENR, FL, filename, /GET_LUN
       
       ; Read the header
       line = ''
       READF, FL, line       
       IF (STRPOS (STRLOWCASE (line), 'psconfig') EQ -1) THEN BEGIN

          error = 1
          w = DIALOG_MESSAGE (/ERROR, TITLE = 'File Read Error', $
	      ['This file does not appear to be', $
	       'a valid psConfig settings file:', $
               file], DIALOG_PARENT = self.topID)
          
       ENDIF ELSE BEGIN

          WHILE (NOT EOF (FL)) DO BEGIN
          
              READF, FL, line
	      IF ((STRMID (STRTRIM (line, 2), 0, 1) NE ';') AND $
                  (STRCOMPRESS (line, /REMOVE_ALL) NE '')) $
                 THEN BEGIN

		 IF (FLOAT (!VERSION.RELEASE) GE 5.3) THEN BEGIN
		    tokens = CALL_FUNCTION ('STRSPLIT', line, '=', /EXTRACT)
		 ENDIF ELSE BEGIN
		    tokens = STR_SEP (line, '=')
		 ENDELSE

	         IF (N_ELEMENTS (tokens) GT 1) THEN BEGIN

		    key   = STRTRIM (tokens[0], 2)
		    value = STRTRIM (tokens[1], 2)

		    CASE (STRUPCASE (key)) OF

                       'PAPER'          : $
                           self->updateDevice, PAPER = STRING (value)
                       'FILENAME'       : $
                           self->updateDevice, FILENAME = STRING (value)
                       'XSIZE'          : $
                           self->updateDevice, XSIZE = FLOAT (value)
                       'XOFFSET'        : $
                           self->updateDevice, XOFF = FLOAT (value)
                       'YSIZE'          : $
                           self->updateDevice, YSIZE = FLOAT (value)
                       'YOFFSET'        : $
                           self->updateDevice, YOFF = FLOAT (value)
                       'INCHES'         : $
                           self->updateDevice, INCHES = FIX (value)
                       'COLOR'          : $
                           self->updateDevice, COLOR = FIX (value)
                       'BITS_PER_PIXEL' : $
                           self->updateDevice, BITS_PER_PIXEL = FIX (value)
                       'ENCAPSULATED'   : $
                           self->updateDevice, ENCAPSULATED = FIX (value)
                       'ISOLATIN1'      : $
                           self->updateDevice, ISOLATIN1 = FIX (value)
                       'LANDSCAPE'      : $
                           self->updateDevice, LANDSCAPE = FIX (value)
                       'FONT'           : $
                           self->updateDevice, SET_FONT = STRING (value)
                       'PREVIEW'        : $
                           self->updateDevice, PREVIEW = FIX (value)
		        ELSE            : $
                           MESSAGE, /CONTINUE, 'Unrecognized parameter: ' + key

		    ENDCASE

	         ENDIF

	      ENDIF
          
          ENDWHILE
          
          self->updateDevice, NAME = 'Custom'                       
       
       ENDELSE
       
    ENDIF ; load


    
    IF (saving) THEN BEGIN
    
       IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN
       
          filename = DIALOG_PICKFILE (/WRITE, /OVERWRITE_PROMPT, $
              FILTER = '*.cfg', DIALOG_PARENT = self.topID)
       
       ENDIF
        
       IF (filename EQ '') THEN BEGIN
          error = 1
          RETURN      	  
       ENDIF

       ; If the user accidently puts a space in front of the filename...
       retpos = STRPOS(filename, STRING(10B))
       IF (retpos NE 0) THEN BEGIN
          filename = STRMID(filename, retpos + 1)
       ENDIF

       OPENW, FL, filename, /GET_LUN
       
       ; Write the header
       PRINTF, FL, '; psConfig settings'
       PRINTF, FL, ';'
             
       PRINTF, FL, 'PAPER          = ', self.device.paper       
       PRINTF, FL, 'FILENAME       = ', self.device.filename       
       PRINTF, FL, 'XSIZE          = ', self.device.xsize          
       PRINTF, FL, 'XOFFSET        = ', self.device.xoff           
       PRINTF, FL, 'YSIZE          = ', self.device.ysize          
       PRINTF, FL, 'YOFFSET        = ', self.device.yoff           
       PRINTF, FL, 'INCHES         = ', self.device.inches         
       PRINTF, FL, 'COLOR          = ', self.device.color          
       PRINTF, FL, 'BITS_PER_PIXEL = ', self.device.bits_per_pixel 
       PRINTF, FL, 'ENCAPSULATED   = ', self.device.encapsulated   
       PRINTF, FL, 'ISOLATIN1      = ', self.device.isolatin1      
       PRINTF, FL, 'LANDSCAPE      = ', self.device.landscape      
       PRINTF, FL, 'FONT           = ', self.device.set_font       
       PRINTF, FL, 'PREVIEW        = ', self.device.preview        
       
    ENDIF ; save

    CLOSE, fl
    FREE_LUN, FL
    
END


; ----------------------------------------------------------------------------
; Definition of PsConfig object
; ----------------------------------------------------------------------------
PRO PsConfig__define


    ; Structure template for built-in paper sizes
    
    template = { PSPAPER,   $

        name           : '',   $ ; Paper name
        xsize          : 0.0,  $ ; xsize
        ysize          : 0.0,  $ ; ysize
        coords         : 0     $ ; natural coordinates (inches = 0, cm = 1)
    }    
    
    ; Structure template for the Postscript device parameters
    
    template = { PSDEVICE,   $

        name           : '',   $ ; Configuration name
        paper          : '',   $ ; Paper size        
        filename       : '',   $ ; The name of the output file
        xsize          : 0.0,  $ ; The x size of the plot   (cm)
        xoff           : 0.0,  $ ; The x offset of the plot (cm)
        ysize          : 0.0,  $ ; The y size of the plot   (cm)
        yoff           : 0.0,  $ ; The y offset of the plot (cm)
        inches         : 0,    $ ; Inches or centimeters
        color          : 0,    $ ; Color on or off
        bits_per_pixel : 0,    $ ; How many bits per image pixel
        encapsulated   : 0,    $ ; Encapsulated or regular PostScript
        isolatin1      : 0,    $ ; Encoding is not ISOLATIN1
        landscape      : 0,    $ ; Landscape or portrait mode
        set_font       : '',   $ ; Font string
        font_size      : 0,    $ ; Font size (points)
        preview        : 0     $ ; Include preview
    }


    obj = { PSCONFIG, $
        
        version        : '',           $
        
        userDevice     : !D.NAME,      $
        PageBox        : OBJ_NEW (),   $
        inches         : 0,            $
        cancel         : 0,            $

        topID          : 0L,           $
        paperListID    : 0L,           $
        paperSizeID    : 0L,           $
        fileID         : 0L,           $
        xsizeID        : 0L,           $
        xoffID         : 0L,           $
        ysizeID        : 0L,           $
        yoffID         : 0L,           $
        inchesID       : 0L,           $
        cmID           : 0L,           $
        portraitID     : 0L,           $
        landscapeID    : 0L,           $
        epsID          : 0L,           $
        isoLatinID     : 0L,           $

        bwID           : 0L,           $
        colorID        : 0L,           $

        bpp1ID         : 0L,           $
        bpp2ID         : 0L,           $
        bpp4ID         : 0L,           $
        bpp8ID         : 0L,           $
        
        preview0ID     : 0L,           $
        preview1ID     : 0L,           $
        preview2ID     : 0L,           $

        layoutID       : 0L,           $
        fontID         : 0L,           $
        fontSizeID     : 0L,           $

        papers         : PTR_NEW (),   $ ; List of available paper sizes
        layouts        : PTR_NEW (),   $ ; Set of predefined configurations
        fonts          : PTR_NEW (),   $ ; Available fonts 
        device         : { PSDEVICE }  $ ; Current PS device parameters
                            
    }


END


; -----------------------------------------------------------------------------
FUNCTION PSCONFIG, FILENAME = filename, $
    DEFAULT = default, INITIALIZE = initialize, PARENT = parent, $
    CANCEL = cancel, TITLE = title, $
    FGCOLOR = fgcolor, BGCOLOR = bgcolor
    
    dName = !D.NAME
;    IF (dName NE 'X' AND dName NE 'WIN' AND dName NE 'MAC') THEN BEGIN
;       os = BYTE (!VERSION.OS)
;       os = STRUPCASE (os[0:2])
;       IF (os EQ 'MAC' OR os EQ 'WIN') THEN BEGIN
;          SET_PLOT, os
;       ENDIF ELSE BEGIN
;          SET_PLOT, 'X'
;       ENDELSE
;    ENDIF
           
    o = OBJ_NEW ('PsConfig', FILENAME = filename, $
        DEFAULT = default, INITIALIZE = initialize, $
        PARENT = parent, TITLE = title, FGCOLOR = fgcolor, BGCOLOR = bgcolor)
    
    config = o->config ()
    cancel = o->cancel ()
    
    IF (cancel) THEN config = { NULL : 0 }
              
    OBJ_DESTROY, o
    
    SET_PLOT, dName

    RETURN, config
     
END    
