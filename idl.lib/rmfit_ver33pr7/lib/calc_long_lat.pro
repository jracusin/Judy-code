; Calculates GRO position (DEGREES)
;  DATE = TJD
;  TIME = seconds of day
;  X, Y, Z = GRO position in km
;
PRO CALC_LONG_LAT, DATE, TIME, X, Y, Z, ELONG, ELAT

PI = ACOS(-1.0D)
	
; Calculate latitude
ELAT = ATAN(DOUBLE(Z) / SQRT(X*X + Y*Y))
ELAT = ELAT * (180 / PI)

; Calculate sidereal time and convert from seconds of day to degrees
GST     = TJD_TO_SIDEREAL(DATE, TIME)
GST_DEG = GST / 240.0D

; Calculate longitude
ANGL = ATAN(DOUBLE(Y) / X)
; Get the quadrant correct
IF ((X GT 0) AND (Y LT 0)) THEN ANGL = ANGL + 2*PI
IF ((X LT 0) AND (Y LT 0)) THEN ANGL = ANGL + PI
IF ((X LT 0) AND (Y GT 0)) THEN ANGL = ANGL + PI

ANGL = ANGL * (180.0D / PI)

ELONG = ANGL - GST_DEG

IF (ELONG LT 0.0) THEN ELONG = 360.0 + ELONG
IF (ELONG GE 360.0) THEN ELONG = ELONG MOD 360.0



END
