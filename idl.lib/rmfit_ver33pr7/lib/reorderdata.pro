; ----------------------------------------------------------------------------
; Utility to take multiple sets of data and reorganize according to ascending
; order of energy. This is useful in plots or calculations that need a flat 
; dataset covering the entire useful energy range, especially photon and energy 
; flux calculations. 
;
; effArea should take care of any effective area mismatches in the spectral 
; fit data.
;
; Optionally set /WIDE to accomodate the expanded MFIT photon 
; model energy scale, which is 6 times the input scale
;
; Returns the number of spectral records found in dataArr
; ----------------------------------------------------------------------------
FUNCTION reorderData, numDet, numChan, energyArr, dataArr, fitChan, effArea, $
         fullEnergy, fullData, WIDE = wide
         
    result = 1
    scale = 1
    IF KEYWORD_SET (WIDE) THEN scale = 6
    
    nEnergy = N_ELEMENTS (energyArr[*, 0])
    result = N_ELEMENTS (dataArr[*, 0]) / nEnergy
    
    ;== Prepare the indices. Note the special handling if only one channel used
    idx1 = fitchan[0, 0] * scale
    idx2 = fitchan[1, 0] * scale + (fitchan[1, 0] EQ fitchan[0, 0] ? 0 : -1)
    
    ;== Start off initializing the arrays
    fullEnergy = energyArr[idx1: idx2, 0]
    fullData  = [0.]  ;dataArr[idx1: idx2, 0]
;    nChan = INTARR (numDet)
;    nChan[0] = idx2 - idx1 + 1
    
    ;== Now loop through detectors, if any
    FOR j = 1, numDet - 1 DO BEGIN
        idx1 = fitchan[0, j] * scale
        idx2 = fitchan[1, j] * scale + $
                   (fitchan[1, j] EQ fitchan[0, j] ? 0 : -1)
        fullEnergy = [fullEnergy, energyArr[idx1: idx2, j]]
;        nChan[j] = idx2 - idx1 + 1
    ENDFOR
    
    FOR i = 0, result - 1 DO BEGIN
        FOR j = 0, numDet - 1 DO BEGIN
            idx1 = i * numChan[j] + fitchan[0, j] * scale
            idx2 = i * numChan[j] + fitchan[1, j] * scale  + $
                   (fitchan[1, j] EQ fitchan[0, j] ? 0 : -1)
            fullData  = [fullData, dataArr[idx1: idx2, j] * $
                        (j EQ 0 ? 1. : effArea[i, j - 1])]
        ENDFOR
    ENDFOR
    
    fullData = fullData[1: *]      ;== Why do I *always* have to do this?
    kk = SORT (fullEnergy)
    fullEnergy = fullEnergy[kk]
    fullData = REFORM (fullData, N_ELEMENTS (kk), result)
    fullData = fullData[kk, *]
    
    RETURN, result
END