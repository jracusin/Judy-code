;------------------------------------------------------------------------------
; Return range of a line that spans the full plotted range
; NB: Make sure a PLOT has been done for the current window before using!
;------------------------------------------------------------------------------

FUNCTION axisRange, XAXIS = xaxis, YAXIS = yaxis, ZAXIS = zaxis

    IF (KEYWORD_SET (xaxis)) THEN $
       RETURN, (!X.CRANGE * (!X.TYPE EQ 0)) + $
            (10.0^(!X.CRANGE * (!X.TYPE EQ 1)) * (!X.TYPE EQ 1))

    IF (KEYWORD_SET (yaxis)) THEN $
       RETURN, (!Y.CRANGE * (!Y.TYPE EQ 0)) + $
            (10.0^(!Y.CRANGE * (!Y.TYPE EQ 1)) * (!Y.TYPE EQ 1))

    IF (KEYWORD_SET (zaxis)) THEN $
       RETURN, (!Z.CRANGE * (!Z.TYPE EQ 0)) + $
            (10.0^(!Z.CRANGE * (!Z.TYPE EQ 1)) * (!Z.TYPE EQ 1))

    RETURN, -1

END
