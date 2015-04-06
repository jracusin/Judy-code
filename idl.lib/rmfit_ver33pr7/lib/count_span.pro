;
; Given an array of input bins, and a span, return the indices into 
; the input array that denote regions of the array containing the span
; intervals.  A span should contain midpoint values of the data bins, NOT
; the bin edges themselves.
;
PRO COUNT_SPAN, edges, a_span, an_ind, comb

    n      = SIZE (edges)
    ar     = LONARR(2)
    acomb  = ar
    an_arr = LONARR (n[2])
    an_arr (LINDGEN(n[2])) = 0
    an_ind = LINDGEN(n[2])

    the_regions = SIZE (a_span)
    IF (the_regions[1] - 1 GT 0) THEN BEGIN
       lower = 1L
       upper = the_regions[1] - 1
    ENDIF ELSE BEGIN
       lower = 0L
       upper = 0L
    ENDELSE

    FOR hh = lower, upper DO BEGIN

       er = NEAREST_EDGE (edges, a_span[hh, *], ar)

       ; Handle binned thresholds
       ;
       IF (N_ELEMENTS (comb) NE 0) THEN BEGIN          

          eb = NEAREST_EDGE (comb, a_span[hh, *], acomb)
      
;          IF (eb[0] LT er[0]) THEN BEGIN
             WHILE (eb[0] LT er[0]) DO BEGIN
                ar[0] = ar[0] - 1
                er[0] = edges[0, ar[0]]
             ENDWHILE
;          ENDIF
      
;          IF (eb[1] GT er[1]) THEN BEGIN
             WHILE (eb[1] GT er[1]) DO BEGIN
                ar[1] = ar[1] + 1
                er[1] = edges[1, ar[1]]
             ENDWHILE
;          ENDIF
      
       ENDIF
       
       ; RDP 09/23/04: Yuki Kaneko found this error
       IF (ar[1] LT ar[0]) THEN BEGIN
          an_ind = -1
          RETURN
       ENDIF ELSE BEGIN
          an_arr[ar[0]:ar[1]] = 1
       ENDELSE
   
    ENDFOR

    ; Test for no content selection
    ;
    idx = WHERE (an_arr, cnt)
    an_ind = (cnt GT 0) ? an_ind[idx] : -1
   

END
