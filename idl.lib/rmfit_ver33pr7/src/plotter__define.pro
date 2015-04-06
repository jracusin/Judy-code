; ----------------------------------------------------------------------------
; Decorator class Object to enhance display of multiple plots.
; Manages the default and current plot range info for each separate plot 
; (by plot type).
;
; Decorates the DISPLAY object by adding several common routines. Plotting
; object must inherit PLOTTER and pass it a valid DISPLAY.
;
; INPUTS: Must have a valid DISPLAY object to manage
;
; Written by RDP 01/10/02
;
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION plotter::init, display, _EXTRA = extra    

    ;== Set the display
    
    IF OBJ_VALID (display) THEN BEGIN
    
        self.display = display
        
    ENDIF ELSE BEGIN

       MESSAGE, /CONTINUE, 'PLOTTER object needs a valid DISPLAY.'
    
    ENDELSE
    
    ;== We need a mechanism to store the range info for each separate plot type
    
    self.rangeList = OBJ_NEW ('List')
    
    ;== Manage the plot options as well:
    self.myOptions  = PTR_NEW ([ $
             'thick',       $
             'xThick',      $
             'yThick',      $
             'tickLen',     $
             'charSize',    $
             'charThick',   $
             'xTicks',      $
             'xMinor',      $
             'yTicks',      $
             'yMinor',      $
             'title',       $
             'xTitle',      $
             'yTitle',      $
             'subTitle',    $
             'xStyle',      $
             'yStyle',      $
             'lineStyle',   $
             'xGridStyle',  $
             'yGridStyle',  $
             'font',        $
             'psym',        $                 
             'symSize',     $
             'xMargin',     $
             'yMargin'      ])
             
    self.leftOptions  = PTR_NEW ([ $
             'thick',       $
             'xThick',      $
             'yThick',      $
             'tickLen',     $
             'charSize',    $
             'charThick',   $
             'xTicks',      $
             'xMinor',      $
             'title',       $
             'xTitle',      $
             'subTitle',    $
             'xStyle',      $
             'lineStyle',   $
             'xGridStyle',  $
             'font',        $
             'psym',        $                 
             'symSize',     $
             'xMargin',     $
             'yMargin'      ])
	     
    self.rightOptions  = PTR_NEW ([ $
             'thick',       $
             'xThick',      $
             'yThick',      $
             'tickLen',     $
             'charSize',    $
             'charThick',   $
             'xTicks',      $
             'xMinor',      $
             'xTitle',      $
             'xStyle',      $
             'yTicks',      $
             'yMinor',      $
             'yTitle',      $
             'lineStyle',   $
             'xGridStyle',  $
             'yGridStyle',  $
             'font',        $
             'psym',        $                 
             'symSize',     $
             'xMargin',     $
             'yMargin'      ])
             
    self.topOptions  = PTR_NEW ([ $
             'thick',       $
             'xThick',      $
             'yThick',      $
             'tickLen',     $
             'charSize',    $
             'charThick',   $
             'xTicks',      $
             'xMinor',      $
             'yTicks',      $
             'yMinor',      $
             'title',       $
             'yTitle',      $
             'xStyle',      $
             'lineStyle',   $
             'xGridStyle',  $
             'yGridStyle',  $
             'font',        $
             'psym',        $                 
             'symSize'])
             
    self.botOptions  = PTR_NEW ([ $
             'thick',       $
             'xThick',      $
             'yThick',      $
             'tickLen',     $
             'charSize',    $
             'charThick',   $
             'xTicks',      $
             'xMinor',      $
             'yTicks',      $
             'yMinor',      $
             'xTitle',      $
;             'yTitle',      $
             'subTitle',    $
             'xStyle',      $
             'lineStyle',   $
             'xGridStyle',  $
             'yGridStyle',  $
             'font',        $
             'psym',        $                 
             'symSize'      ])
             
    RETURN, 1
      
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO plotter::cleanup

    self.rangeList->clear
    OBJ_DESTROY, self.rangeList
    
    PTR_FREE, self.myOptions
    PTR_FREE, self.topOptions
    PTR_FREE, self.botOptions
    PTR_FREE, self.leftOptions
    PTR_FREE, self.rightOptions
    
END


;-----------------------------------------------------------------------------
; Accessor function to obtain the saved zoom ranges for each plot.
; Pass a unique string for each type of different plot. 
; ----------------------------------------------------------------------------
FUNCTION plotter::getRanges, thePlotType

    RETURN, self.rangeList->data (thePlotType)
    
END

;-----------------------------------------------------------------------------
; Finish saving the saved zoom ranges for each plot.
; Pass a unique string for each type of different plot. 
; ----------------------------------------------------------------------------
PRO plotter::finishRanges, thePlotType, XLOG = xlog, YLOG = ylog

    IF (SIZE (thePlotType, /TNAME) NE 'STRING') THEN RETURN ;== No harm done...

    theRange = self.rangeList->data (thePlotType)
    
    IF ((SIZE(theRange))[0] EQ 0) THEN BEGIN ;No PlotType -- add one
        theRange = { RANGE_INFO }
        self.rangeList->add, thePlotType, theRange
    ENDIF
    
    newRange = theRange
    
    IF KEYWORD_SET (xlog) THEN BEGIN
        newRange.currentRange[0, *] = xlog ? 10.^!x.cRange : !x.cRange 
    ENDIF ELSE BEGIN
        newRange.currentRange[0, *] = !x.cRange 
    ENDELSE

    IF KEYWORD_SET (ylog) THEN BEGIN
        newRange.currentRange[1, *] = ylog ? 10.^!y.cRange : !y.cRange 
    ENDIF ELSE BEGIN
        newRange.currentRange[1, *] = !y.cRange 
    ENDELSE

    self.display->setDefaultRange, newRange.defaultRange
    ;self.display->setCurrentRange, newRange.currentRange

    self.rangeList->delete, thePlotType
    self.rangeList->add, thePlotType, newRange

END

;-----------------------------------------------------------------------------
; Implement the defafult and saved zoom ranges for each plot.
; Pass a unique string for each type of different plot. 
; ----------------------------------------------------------------------------
PRO plotter::adjustRanges, xRange, yRange, thePlotType, $
    XLOG = xlog, YLOG = ylog

    IF (SIZE (thePlotType, /TNAME) NE 'STRING') THEN RETURN ;== No harm done...

    IF (NOT self.rangeList->inList (thePlotType)) THEN BEGIN
        theRange = { RANGE_INFO }
        theRange.haveDefaultRange = 1
        theRange.defaultRange[0, *] = xRange
        theRange.defaultRange[1, *] = yRange
        theRange.currentRange[0, *] = xRange
        theRange.currentRange[1, *] = yRange
        self.rangeList->add, thePlotType, theRange
    ENDIF ELSE BEGIN
        theRange = self.rangeList->data (thePlotType)
        xRange = TRANSPOSE (theRange.currentRange[0, *])
        yRange = TRANSPOSE (theRange.currentRange[1, *])
        
    ENDELSE
    
END


; ----------------------------------------------------------------------------
; Clear all the current plot range info
; ----------------------------------------------------------------------------
PRO plotter::clearRanges

    self.rangeList->clear
    
END


; ----------------------------------------------------------------------------
; Definition of plotter
; ----------------------------------------------------------------------------
PRO plotter__define

    tmpl = { RANGE_INFO,                  $
    
        currentRange     : FLTARR (2, 2), $
        defaultRange     : FLTARR (2, 2), $
        haveDefaultRange : 0              $
    }

    obj = { plotter, $
      
        myOptions        : PTR_NEW (),  $   ; Plot options to pass by _REF_EXTRA
        leftOptions      : PTR_NEW (),  $   ; Plot options for left axis on plot
        rightOptions     : PTR_NEW (),  $   ; Plot options for right axis on plot
        topOptions       : PTR_NEW (),  $   ; Plot options for top axis on plot
        botOptions       : PTR_NEW (),  $   ; Plot options for bottom axis on plot
        display          : OBJ_NEW (),  $   ; Display object handling the plots
        rangeList        : OBJ_NEW ()   $   ; List of x & y ranges for each plot type       

    }
    
END
