; ----------------------------------------------------------------------------
; Resolve overlapping SPAN intervals
; ----------------------------------------------------------------------------

FUNCTION RESOLVE_SELECTIONS, xData, coords, span


    ; Handle combining of overlapping selections
    ;
    x_span = coords[0, *]
    x_span = x_span[SORT (x_span)]
    test_span = [TRANSPOSE (x_span), TRANSPOSE (x_span)]

    COUNT_SPAN, xData, test_span, test_ind
    ; RDP 09/23/04: Yuki Kaneko found this error
    IF (test_ind[0] EQ -1) THEN RETURN, span

    n_temp = SIZE (span)
    n_regions = n_temp[1] - 1

    x_edge = NEAREST_EDGE (xData, x_span)

    IF (n_regions GT 0) THEN BEGIN

       temp_span = span[1:*, *]
       ind_arr = INTARR (n_regions)
       ind_arr[INDGEN (n_regions)] = 1

       FOR i = 1, n_regions DO BEGIN

           IF ((x_edge[0] LE span[i, 0]) AND $
              (x_edge[1] GE span[i, 1])) THEN BEGIN

              temp_span[i - 1, *] = TRANSPOSE (x_span)
              ind_arr[i - 1] = 0

           ENDIF

       ENDFOR

       FOR i = 1, n_regions DO BEGIN

           IF (((x_edge[0] GT span[i, 0]) AND $
              (x_edge[0] LT span[i, 1])) OR $
              ((x_edge[1] GT span[i, 0]) AND $
              (x_edge[1] LT span[i, 1]))) THEN BEGIN

              temp_span[i - 1, *] = TRANSPOSE (x_span)
              ind_arr[i - 1] = 0

           ENDIF

       ENDFOR

       temp_span = [temp_span, TRANSPOSE (x_span)]
       ind_arr = [ind_arr, 1]
       temp_ind = WHERE (ind_arr, cnt)

       IF (cnt GT 0) THEN $
          temp_span = temp_span[temp_ind, *]

       temp_span = temp_span[SORT (temp_span[*, 0]), *]

       t_span = TRANSPOSE ( $
           [MIN (temp_span[*, 0]), MAX (temp_span[*, 1])])

       newSpan = [t_span, temp_span]

    ENDIF ELSE BEGIN

       newSpan = TRANSPOSE ([[x_span], [x_span]])

    ENDELSE

    RETURN, newSpan

END
