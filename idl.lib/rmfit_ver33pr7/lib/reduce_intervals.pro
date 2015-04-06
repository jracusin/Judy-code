;
; Given an input array of indices that denote intervals into
; an array, reduce the input array to the smallest set of 
; indices that describe the selected intervals.  For example,
; if the input array describes three intervals by the indices
;
;              4 5 6  20 21 22 23 24  33 34 35 36 37 38
;              ^^^^^  ^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^
;   interval:  1      2               3
;
; then this routine would return an array of (2, 3) indices that 
; denote the start and stop indices of the three selected intervals:
;
;               4,  6
;              20, 24
;              33, 38
;
; -----------------------------------------------------------------------------

FUNCTION REDUCE_INTERVALS, input

    ;== Test case
    ;input = [4, 5, 6, 20, 21, 22, 23, 24, 33, 34, 35, 36, 37, 38]

    n = N_ELEMENTS (input)   
    IF (n EQ 1) THEN RETURN, [input, input]
        
    input  = input[SORT (input)]
    intBeg = -1
    intEnd = -1
         
    FOR i = 0L, n - 1 DO BEGIN
    
        IF (i EQ 0) THEN BEGIN
           
           intBeg = [intBeg, input[i]]
        
        ENDIF ELSE BEGIN
        
           IF (input[i] NE input[i - 1] + 1) THEN BEGIN
           
              intEnd = [intEnd, input[i - 1]]
              intBeg = [intBeg, input[i]]
        
           ENDIF
           
        ENDELSE
    
    ENDFOR

    nInt = N_ELEMENTS (intBeg)
    IF (N_ELEMENTS (intEnd) NE nInt) THEN $
       intEnd = [intEnd, input[n - 1]]

    intBeg = intBeg[1:*]
    intEnd = intEnd[1:*]
    nInt   = nInt - 1 

    output = INTARR (2, nInt)
    output[0, *] = intBeg
    output[1, *] = intEnd
    
    RETURN, output
    
END
