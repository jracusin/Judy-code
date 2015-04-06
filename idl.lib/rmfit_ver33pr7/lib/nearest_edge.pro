FUNCTION NEAREST_EDGE, THRESHOLDS, RANGE, INDICES

    N = SIZE (THRESHOLDS)

    ; Only one bin selected
    ;
    IF (N[0] EQ 1) THEN BEGIN
       INDICES = LONG([0, 1])
       RETURN, THRESHOLDS
    ENDIF

    ER = [ TRANSPOSE (THRESHOLDS[0, *]), THRESHOLDS[1, N[2] - 1] ]

    I0 = SEARCH (ER, RANGE[0])

    IF (I0 EQ -1) THEN I0 = 0 ELSE IF I0 EQ N[2] THEN I0 = N[2] - 1
    IF (THRESHOLDS[1, I0] LT RANGE[0]) AND (I0 LT N[2] - 1) THEN I0 = I0 + 1

    I1 = SEARCH (ER, RANGE[1])
    IF (I1 EQ -1) THEN I1 = 0 ELSE IF (I1 EQ N[2]) THEN I1 = N[2] - 1

    INDICES = LONG ([I0, I1])


    RETURN, [ THRESHOLDS[0, I0], THRESHOLDS[1, I1] ]


END
