;
; Search a monotonically increasing array for a target value.  The returned
; index corresponds to a value in the array that preceeds the target
; value.
; 
FUNCTION SEARCH, arr, target

    IF (target LT arr[0]) THEN RETURN, -1

    first = 0
    last = N_ELEMENTS (arr) - 1

SEARCHING:

    bullseye = first + ((last - first) + 1)/2
    IF (first EQ last) THEN RETURN, bullseye

    IF (target LT arr[bullseye]) THEN BEGIN
    
       last = bullseye - 1
    
    ENDIF $
    ELSE IF (bullseye EQ last) THEN BEGIN
    
       RETURN, bullseye
    
    ENDIF $
    ELSE BEGIN
    
       FIRST = bullseye
    
    ENDELSE

    GOTO, SEARCHING

    RETURN, bullseye

END
