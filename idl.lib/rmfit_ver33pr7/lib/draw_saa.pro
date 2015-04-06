PRO DRAW_SAA, SHOW_BOXES = SHOW_BOXES


    IF (N_ELEMENTS (SHOW_BOXES) NE 0) THEN BEGIN

       ; Australia box
       ;
       PX = [105.0, 125.0, 125.0, 105.0, 105.0]
       PY = [-24.0, -24.0, -29.0, -29.0, -24.0]
       POLYFILL, PX, PY, /LINE_FILL, ORIENT = 45, SPACING = 0.1
       PLOTS, PX, PY

       ; North America box
       ;
       PX = [-115.0, -80.0, -80.0, -115.0, -115.0]
       PY = [29.0, 29.0, 22.0, 22.0, 29.0]
       POLYFILL, PX, PY, /LINE_FILL, ORIENT = 45, SPACING = 0.1
       PLOTS, PX, PY

       ; Small SAA box
       ;
       PX = [-100.0, -70.0, -70.0, -100.0, -100.0]
       PY = [-23.0, -23.0, -29.0, -29.0, -23.0]
       POLYFILL, PX, PY, /LINE_FILL, ORIENT = 45, SPACING = 0.1
       PLOTS, PX, PY

    ENDIF


    ; SAA
    ;
    PX = [276, 276, 306, 328, 354, 34, 34, 276]
    PY = [-30, -12, -1.5, 0, -13.5, -26, -30, -30] 

    ; Polygons crossing the zero boundry must be drawn in device coordinates
    ;
    devcrd = CONVERT_COORD(px, py, /DATA, /TO_DEVICE)
    PX_DC = REFORM(devcrd[0, *])
    PY_DC = REFORM(devcrd[1, *])

    PLOTS, PX_DC, PY_DC, /DEVICE
    POLYFILL, PX_DC, PY_DC, /LINE_FILL, ORIENT = 45, SPACING = 0.1, /DEVICE


END
