PRO PLOT_POINTS, X_DATA, Y_DATA, DY_DATA, COLOR = C

N = SIZE(X_DATA)
INDEX = N(2) - 1
IF N(0) NE 2 THEN BEGIN
   N = SIZE(Y_DATA)
   INDEX = N(1) - 1
   ENDIF
X_RANGE = 0.005*(!X.CRANGE(1) - !X.CRANGE(0))
DX = X_DATA(1, *) - X_DATA(0, *)
IF !Y.TYPE THEN BEGIN               ; Axis is log., so we do fancy stuff
   Y_RANGE = 10 ^ !Y.CRANGE
   USERSYM, [-0.5, 0.5, 0.5, -0.5], [0.5, 0.5, -0.5, -0.5], /FILL
   FOR I = 0, INDEX DO BEGIN
      YY = Y_DATA(I) 
      YY = [YY, YY]
      XX = [X_DATA(0, I), X_DATA(1, I)]
      XX2 = 0.5*(X_DATA(0, I) + X_DATA(1, I))
      XX2 = [XX2, XX2]
      IF (KEYWORD_SET(DY_DATA)) AND (DX(I) GT X_RANGE) THEN BEGIN
         IF Y_DATA(I) LT DY_DATA(I) THEN BEGIN ; Create upper limit mark
            YY = 2 * DY_DATA(I) 
            YY = [YY, YY]
            YY2 = [Y_RANGE(0), 2 * DY_DATA(I)]
            ENDIF $
         ELSE BEGIN                            ; Create error bar
            YY2 = Y_DATA(I) + [- DY_DATA(I), DY_DATA(I)]
            ENDELSE
         ENDIF $
      ELSE BEGIN                 ; No DY_DATA, just plot a point
         YY2 = Y_DATA(I) 
         YY2 = [YY2, YY2]
         ENDELSE
      IF (YY2(0) GT Y_RANGE(1)) OR (YY2(1) LT Y_RANGE(0)) THEN BEGIN
         YY = Y_DATA(I) 
         YY = [YY, YY]
         XX = 0.5*(X_DATA(0, I) + X_DATA(1, I))
         XX = [XX, XX]
         OPLOT, XX, YY, PSYM=8, COLOR = C   ; Make a square if the point is off-axis 
         ENDIF $
      ELSE BEGIN                 ; Plot the data point w. error bar
         OPLOT, XX, YY, COLOR = C
         OPLOT, XX2, YY2, COLOR = C
         ENDELSE
      ENDFOR
   ENDIF $
ELSE BEGIN                          ; Axis is linear - don't do tricks.
   A = FINDGEN(16) * (!PI * 2 / 16.0)
   USERSYM, COS(A), SIN(A), /FILL   ; Define a circle symbol for dy = 0
   FOR I = 0, INDEX DO BEGIN
      XX = [X_DATA(0, I), X_DATA(1, I)]
      YY = Y_DATA(I) 
      YY = [YY, YY]
      IF XX(0) EQ XX(1) THEN $
         OPLOT, XX, YY, PSYM = 8, COLOR = C $
      ELSE $
         OPLOT, XX, YY, COLOR = C
      ENDFOR
   IF KEYWORD_SET(DY_DATA) THEN BEGIN
      FOR I = 0, INDEX DO IF (DX(I) GT X_RANGE) OR (DX(I) EQ 0.0) THEN BEGIN
         XX = 0.5*(X_DATA(0, I) + X_DATA(1, I))
         XX = [XX, XX]
         IF DY_DATA(I) NE 0.0 THEN BEGIN
            YY = Y_DATA(I) + [- DY_DATA(I), DY_DATA(I)]
            OPLOT, XX, YY, COLOR = C
            ENDIF $
         ELSE BEGIN
            YY = Y_DATA(I)
            YY = [YY, YY]
            OPLOT, XX, YY, PSYM = 8, COLOR = C
            ENDELSE
         ENDIF
      ENDIF
   ENDELSE



END; PRO PLOT_POINTS
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
