; ----------------------------------------------------------------------------
; Compute net count rate (background subtracted) and errors (counts / s-energy)
; ----------------------------------------------------------------------------
PRO NETCRATE, $
    obsCrate, backCrate, backCsig, liveTime, chanWidth, $    ; IN
    netCrate, netCsig                                        ; OUT

    netCrate = (netCsig = obsCrate * 0.0)

    numDet = 1
    s = SIZE (obsCrate)
    IF (s[0] GT 1) THEN numDet = s[2]
       
    FOR i = 0, numDet - 1 DO BEGIN
    
        ; Live time masking
        ;
        idx = WHERE (liveTime[*, i] GT 0.0, cnt)
        IF (cnt GT 0) THEN BEGIN

           netCrate[idx, i] = obsCrate[idx, i] - backCrate[idx, i]

           factor = 1.0 / (ABS (liveTime[idx, i]) * chanWidth[idx, i])        
           netCsig[idx, i] = backCsig[idx, i]^2 + $
               factor * (obsCrate[idx, i] > factor)
           netCsig[idx, i] = SQRT (netCsig[idx, i])

        ENDIF

    ENDFOR
        
END
