; -----------------------------------------------------------------------------
;+
; NAME:
;     CDF_EXTRACT (PROCEDURE)
;
; PURPOSE:
;     Extract a portion of a BATSE CDF file to a FITS file.
;
; CALLING SEQUENCE:
;     CDF_EXTRACT [, FILENAME = filename, NOGUI = nogui]
;
; INPUTS:
;     NONE 
;    
; OUTPUTS:
;     s   A structure that can be passed to the PLOT command via 
;         the _EXTRA keyword.
;      
; KEYWORDS:
;
;     FILENAME (OPTIONAL)
;         Set this keyword to a CDF filename to process, otherwise
;         a dialog box is used to select a file.
;
;     NOGUI
;         Set this keyword to extract CDF data in batch mode.
;
; MODIFICATION HISTORY:
;
;     Written, Robert.Mallozzi@msfc.nasa.gov, 2000 April
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION CDFExtract::init, PARENT = parent, $
    NOGUI = nogui, _EXTRA = extra
    
    ; INPUT = input, OUTPUT = output, $
    ; BEG_TIME = beg_time, END_TIME = end_time
    
    
    self.noGUI = KEYWORD_SET (nogui)

    self.Reader = OBJ_NEW ('CDFReader')    
    
    IF (NOT self.noGUI) THEN BEGIN 
    
       self->buildGUI, PARENT = parent
    
    ENDIF ELSE BEGIN
    
       self->autoExtract, _EXTRA = extra
       
    ENDELSE
             
    RETURN, 1
    
END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO CDFExtract::cleanup

    OBJ_DESTROY, self.Reader
    
END


; -----------------------------------------------------------------------------
; Build the widget
; -----------------------------------------------------------------------------
PRO CDFExtract::buildGUI, PARENT = parent
    
    title = 'CDF Extract'
       
    IF (N_ELEMENTS (parent) NE 0) THEN $
       extra = { GROUP_LEADER : parent, MODAL : 1, TLB_FRAME_ATTR : 9}

    self.topID = WIDGET_BASE (TITLE = title, /COLUMN, _EXTRA = extra)
    
    topBase = WIDGET_BASE (self.topID, /COLUMN, /FRAME)
            
            
    base = WIDGET_BASE (topBase, COL = 5, /GRID)
    
    w = WIDGET_LABEL (base, VALUE = 'Start Time', /ALIGN_RIGHT)
    self.begTimeID = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '')

    w = WIDGET_LABEL (base, VALUE = 'End Time', /ALIGN_RIGHT)
    self.endTimeID = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '')

    self.selectID = WIDGET_BUTTON (base, VALUE = 'Select')
     
    w = WIDGET_LABEL (topBase, VALUE = ' ')

    base = WIDGET_BASE (topBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = ' Input', /ALIGN_RIGHT)
    self.fileInID = WIDGET_TEXT (base, /EDIT, VALUE = '', $
        XSIZE = 40, /KBRD_FOCUS_EVENTS)
    w = WIDGET_BUTTON (base, VALUE = '...', UVALUE = 'input')

    base = WIDGET_BASE (topBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Output', /ALIGN_RIGHT)
    self.fileOutID = WIDGET_TEXT (base, /EDIT, VALUE = '', $
        XSIZE = 40, /KBRD_FOCUS_EVENTS)
    w = WIDGET_BUTTON (base, VALUE = '...', UVALUE = 'output')

    self.statusID = WIDGET_LABEL (topBase, VALUE = ' ', /DYNAMIC, /ALIGN_LEFT)
    
    base = WIDGET_BASE (self.topID, /ROW, /ALIGN_CENTER, SPACE = 20, /GRID)
    self.writeID = WIDGET_BUTTON (base, VALUE = 'Write FITS')
    w = WIDGET_BUTTON (base, VALUE = 'Dismiss')
    w = WIDGET_BUTTON (base, VALUE = 'Help')
       
    self->enableButtons, 0
    
    WIDGET_CONTROL, self.topID, /REALIZE
    WIDGET_CONTROL, self.topID, SET_UVALUE = self

    XMANAGER, 'CDFExtract', self.topID


END


; -----------------------------------------------------------------------------
; Event handler
; -----------------------------------------------------------------------------
PRO CDFExtract_EVENT, event
       
    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->eventHandler, event

END

PRO CDFExtract::eventHandler, event
        
    type = TAG_NAMES (event, /STRUCTURE)
    CASE (type) OF
            
        'WIDGET_BUTTON': BEGIN
       
            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF

                 'Dismiss'    : WIDGET_CONTROL, event.top, /DESTROY
                 'Help'       : self->help
                 'Write FITS' : self->write

                 '...' : BEGIN
                 
                     WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                     
                     filename = DIALOG_PICKFILE ( $
                         TITLE = 'Select a CDF File', $
                         FILTER = '*.fits', MUST_EXIST = uvalue EQ 'input', $
                         DIALOG_PARENT = self.topID)
                     IF (filename EQ '') THEN RETURN
                     
                     IF (uvalue EQ 'input') THEN BEGIN
                        WIDGET_CONTROL, self.fileInID, SET_VALUE = filename
                        self->read
                     ENDIF ELSE BEGIN
                        WIDGET_CONTROL, self.fileOutID, SET_VALUE = filename
                     ENDELSE   
                     END
                     
                 'Select' : BEGIN
                     
                     ;== Integrate over all channels
                     rate = TOTAL ((self.Reader->data()).ext2.counts, 2)

                     ;== Integrate over all detectors
                     rate = TOTAL (rate, 1)

                     ;== Compute seconds of day
                     sod = 86400.0d * $
                         ((self.Reader->data()).ext2.mid_time - $
                         LONG ((self.Reader->data()).ext2.mid_time))
 
                     xmin = MIN (sod,  MAX = xmax)
                     ymin = MIN (rate, MAX = ymax)
                                         
                     PLOT, sod, rate, $
                         PSYM   = 10, $
                         XTITLE = 'Seconds of Day', $
                         YTITLE = 'Rate (counts / second)', $
                         TITLE  = 'Select Interval to Extract', $
                         XRANGE = [xmin, xmax], XSTYLE = 1, $
                         YRANGE = [ymin, ymax]
                         
                     CURSOR, /DOWN, x1, y1
                     OPLOT, [x1, x1], !Y.CRANGE, LINESTYLE = 1
                      
                     CURSOR, /DOWN, x2, y2
                     OPLOT, [x2, x2], !Y.CRANGE, LINESTYLE = 1
                                          
                     x = [LONG (x1), LONG (x2)]
                     self.selectedTimes = x[SORT (x)] 
                     
                     ;== Find the indices of the selected times
                     
                     unused = MIN (ABS (sod - x1), idx1)
                     unused = MIN (ABS (sod - x2), idx2)                  
                     self.selectedIdx = [idx1, idx2]
                                         
                     s = STRING (x[0], FORMAT = '(I5)')
                     WIDGET_CONTROL, self.begTimeID, SET_VALUE = s
                     s = STRING (x[1], FORMAT = '(I5)')
                     WIDGET_CONTROL, self.endTimeID, SET_VALUE = s

                     WDELETE
                     END

            ENDCASE ; value
            END
        
        'WIDGET_TEXT_CH' : BEGIN
            END
        
        'WIDGET_KBRD_FOCUS' : BEGIN
            END
        
        ELSE: ; do nothing
            
    ENDCASE ; type
  
END


; ----------------------------------------------------------------------------
; Read CDF file
; ----------------------------------------------------------------------------
PRO CDFExtract::read

    WIDGET_CONTROL, /HOURGLASS
    WIDGET_CONTROL, self.statusID, SET_VALUE = 'Reading CDF file...'

    WIDGET_CONTROL, self.fileInID, GET_VALUE = filename
    self.Reader->setFilename, filename
    self.Reader->read, ERROR = error
    IF (error) THEN BEGIN 
       WIDGET_CONTROL, self.statusID, SET_VALUE = 'Error reading input file!'
       RETURN
    ENDIF   
       
    self->enableButtons, (error NE 1)

    WIDGET_CONTROL, self.statusID, SET_VALUE = ' '
        
END


; ----------------------------------------------------------------------------
; Write selected portion of CDF file to FITS
; ----------------------------------------------------------------------------
PRO CDFExtract::write

    WIDGET_CONTROL, self.fileOutID, GET_VALUE = filename
    filename = filename[0]
    
    IF (filename EQ '') THEN BEGIN
       w = DIALOG_MESSAGE (/ERROR, DIALOG_PARENT = self.topID, $
           'Please specify output filename')
       RETURN
    ENDIF

    WIDGET_CONTROL, self.begTimeID, GET_VALUE = begTime
    begTime = begTime[0]
    
    WIDGET_CONTROL, self.endTimeID, GET_VALUE = endTime
    endTime = endTime[0]

    IF (begTime EQ '' OR endTime EQ '') THEN BEGIN
       w = DIALOG_MESSAGE (/ERROR, DIALOG_PARENT = self.topID, $
           'Please specify start and end times')
       RETURN
    ENDIF
   
    self->writeData, filename, LONG (begTime), LONG (endTime)
    
END

PRO CDFExtract::writeData, filename, begTime, endTime

    headers = self.Reader->header()
    data    = self.Reader->data()

    ;== Extract selected time indices
    
    sod = 86400.0d * $
        ((self.Reader->data()).ext2.mid_time - $
        LONG ((self.Reader->data()).ext2.mid_time))

    idx = WHERE (sod GE begTime AND sod LE endTime, cnt)
    IF (cnt EQ 0) THEN $
       MESSAGE, 'No selected time indices found.'
    
stop

    table1 = data.ext1[idx] ; wrong    
    table2 = data.ext2[idx]    
    
    ; Update headers...
    
    MWRFITS, undefined, filename, headers.ext0, /CREATE
    MWRFITS, table1,    filename, headers.ext1
    MWRFITS, table2,    filename, headers.ext2
   
END


; ----------------------------------------------------------------------------
; Enable/disable buttons
; ----------------------------------------------------------------------------
PRO CDFExtract::enableButtons, state

    WIDGET_CONTROL, self.selectID, SENSITIVE = state
    WIDGET_CONTROL, self.writeID,  SENSITIVE = state
    
END


; ----------------------------------------------------------------------------
; Show help
; ----------------------------------------------------------------------------
PRO CDFExtract::help

    msg = [ $
    
        'Enter start, stop times of the portion of data', $
        'to extract.  Times must be entered in seconds of', $
        'day (0 - 86400 s)', $
        ' ', $
        'Use the Select button to choose an interval', $
        'interactively from the lightcurve.', $
        ' ', $
        'The output FITS file will contain rates for all', $
        'eight detectors in the approximate time range that', $
        'is specified.  The FITS file conforms to the BATSE', $
        'CDF FITS format.'  $
    ]     

    w = DIALOG_MESSAGE (msg, /INFORMATION, DIALOG_PARENT = self.topID)    

END


; ----------------------------------------------------------------------------
; Extract data non-interactively
; ----------------------------------------------------------------------------
PRO CDFExtract::autoExtract, _EXTRA = extra

    IF (N_ELEMENTS (extra) EQ 0) THEN BEGIN
       PRINT, 'Missing keywords: INPUT, OUTPUT, BEGTIME, ENDTIME'
       RETURN
    ENDIF
       
    tags = TAG_NAMES (extra)
    
    idx = WHERE (tags EQ 'INPUT', cnt)
    IF (cnt EQ 0) THEN BEGIN
       PRINT, 'Missing keyword: INPUT'
       RETURN
    ENDIF
       
    idx = WHERE (tags EQ 'OUTPUT', cnt)
    IF (cnt EQ 0) THEN BEGIN
       PRINT, 'Missing keyword: OUTPUT'
       RETURN
    ENDIF

    idx = WHERE (tags EQ 'BEGTIME', cnt)
    IF (cnt EQ 0) THEN BEGIN
       PRINT, 'Missing keyword: BEGTIME'
       RETURN
    ENDIF

    idx = WHERE (tags EQ 'ENDTIME', cnt)
    IF (cnt EQ 0) THEN BEGIN
       PRINT, 'Missing keyword: ENDTIME'
       RETURN
    ENDIF

    self.Reader->setFilename, extra.input
    self.Reader->read, ERROR = error
    IF (error) THEN BEGIN 
       MESSAGE, /CONTINUE, 'Error reading input file!'
       RETURN
    ENDIF
    
    self->writeData, extra.output, extra.begTime, extra.endTime

END


; -----------------------------------------------------------------------------
; Extract data from a CDF file
; -----------------------------------------------------------------------------
PRO CDFExtract__define

    
    obj = { CDFEXTRACT, $
        
        noGUI         : 0,          $

        selectedTimes : DBLARR(2),  $                
        selectedIdx   : LONARR(2),  $
        
        topID         : 0L,         $
        begTimeID     : 0L,         $
        endTimeID     : 0L,         $
        fileInID      : 0L,         $
        fileOutID     : 0L,         $
        selectID      : 0L,         $
        writeID       : 0L,         $
        statusID      : 0L,         $
        
        Reader        : OBJ_NEW ()  $
        
    }

END


; -----------------------------------------------------------------------------
PRO CDF_EXTRACT, PARENT = parent, NOGUI = nogui, _EXTRA = extra
           
    o = OBJ_NEW ('CDFExtract', PARENT = parent, NOGUI = nogui, _EXTRA = extra)
    OBJ_DESTROY, o
     
END    
