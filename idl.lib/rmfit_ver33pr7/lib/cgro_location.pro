; -----------------------------------------------------------------------------
; Plots CGRO location at a given trigger time.
;
; header = BFITS primary header (EXT 0)
; -----------------------------------------------------------------------------

PRO CGRO_LOCATION, TJD, header, $
    FGCOLOR = fgcolor, BGCOLOR = bgcolor, $
    HIGHLIGHT = highlight, DIALOG_PARENT = dialog_parent 


    IF (N_ELEMENTS (fgcolor) EQ 0) THEN $
       fgcolor = !P.COLOR
       
    IF (N_ELEMENTS (bgcolor) EQ 0) THEN $
       bgcolor = !P.BACKGROUND

    IF (N_ELEMENTS (highlight) EQ 0) THEN $
       highlight = !P.COLOR

    ; Spacecraft location (km), as stored in the BFITS header.
    
    SC_X_POS = SXPAR (header, 'SC-X-POS')
    SC_Y_POS = SXPAR (header, 'SC-Y-POS')
    SC_Z_POS = SXPAR (header, 'SC-Z-POS')

    ; Seconds of day
    
    SOD = SXPAR (header, 'TRIG-TIM')

    ; GRO position
    
    CALC_LONG_LAT, TJD, SOD, SC_X_POS, SC_Y_POS, SC_Z_POS, GRO_LON, GRO_LAT
    
    command = [ $
        'MAP_SET, 0, 0, /GRID, /AITOFF, /CONTINENTS, /NOBORDER, ' + $
        'COLOR = ' + STRING (fgcolor), $
        'CIRCLE, SYMSIZE = 1.3', $
        'OPLOT, [xData, xData], [yData, yData], PSYM = 8, ' + $
        'COLOR = ' + STRING (highlight), $
        'DRAW_SAA' $
    ]
                     
    cSave = !P.BACKGROUND
    !P.BACKGROUND = bgcolor

    xData = GRO_LON
    yData = GRO_LAT
    
    result = DIALOG_PLOT ( $
        WINTITLE = 'CGRO Location', $
        XDATA = xData, YDATA = yData, $
        XSIZE = 600, YSIZE = 400, $
        TITLE = SXPAR (header, 'OBJECT') + ': CGRO Location', $
        EXECUTE = command, /PS_OPTION, /GIF_OPTION, $
        DIALOG_PARENT = dialog_parent) 
    
    !P.BACKGROUND = cSave


END
