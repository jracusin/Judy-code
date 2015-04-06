;------------------------------------------------------------------------------
; Draw a curve, skipping over data gaps
;------------------------------------------------------------------------------

PRO DRAW_HISTORY, x, y, dy, TOLERANCE = tolerance, _EXTRA = extra

    haveDY = (N_ELEMENTS (dy) NE 0)

    n = SIZE (x)
    IF (n[0] EQ 1) THEN BEGIN
   
       OPLOT, x, [y, y], _EXTRA = extra 
      
       IF (haveDY) THEN BEGIN
          xx = 0.5 * (x[0, 0] + x[1, 0])
          xx = [xx, xx]
          yy = y[0] + [-dy[0], dy[0]]
          OPLOT, xx, yy, _EXTRA = extra 
       ENDIF
   
    ENDIF ELSE BEGIN

       IF (N_ELEMENTS (tolerance) NE 0) THEN BEGIN

          gap_tol = tolerance

       ENDIF ELSE BEGIN       

          gap_tol = MAX ([5.0E-5, 0.001 * MIN (ABS (x[1, *] - x[0, *])) ])

       ENDELSE

       dx = x[0, 1:*] - x[1, 0:n[2] - 2]
       gaps = WHERE (dx GT gap_tol)

       IF (gaps[0] EQ -1) THEN BEGIN
          n_gaps = 0
          gaps = [-1, n[2] - 1]
       ENDIF ELSE BEGIN
          n_gaps = N_ELEMENTS (gaps)
          gaps = [-1, gaps, n[2] - 1]
       ENDELSE

       FOR i = 0L, n_gaps DO BEGIN
          
          good_x = [TRANSPOSE(x[0, gaps[i] + 1:gaps[i + 1]]), x[1, gaps[i + 1]]]
          good_y = y[gaps[i] + 1:gaps[i + 1]]
          
          nx = N_ELEMENTS (good_x)
          ny = N_ELEMENTS (good_y)
 
          xx = FLTARR (2 * nx)
          yy = FLTARR (2 * ny)
          
          even = 2 * LINDGEN (nx)
          xx[even] = good_x
          xx[even + 1] = good_x
          
          even = 2 * LINDGEN (ny)
          yy[even] = good_y
          yy[even + 1] = good_y
          
          k = 2 * nx - 2
          OPLOT, xx[1:k], yy, _EXTRA = extra
             
       ENDFOR
       
       ; Overplot error bars for small range
       ;
       IF (haveDY) THEN BEGIN
       
          x_range = 0.005 * (!X.CRANGE[1] - !X.CRANGE[0])
          dx = x[1, *] - x[0, *]
       
          FOR i = 0L, n[2] - 1 DO BEGIN
         
              IF (dx[i] GT x_range) THEN BEGIN
         
                 xx = 0.5 * (x[0, i] + x[1, i])
                 xx = [xx, xx]
                 yy = y[i] + [-dy[i], dy[i]]
                 OPLOT, xx, yy, _EXTRA = extra 
         
              ENDIF
         
          ENDFOR

       ENDIF
    
    ENDELSE

END
