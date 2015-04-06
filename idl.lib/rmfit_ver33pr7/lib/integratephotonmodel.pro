; ----------------------------------------------------------------------------
; Utility to perform the Extended Simpson's rule integration in log10 space
; ----------------------------------------------------------------------------
FUNCTION integratePhotonModel, photEnergies, photModel, fluenceInt, ENERGY_INT = energy_int


    ;== Check the validity of the fluence interval:
    kk = WHERE ((photEnergies GE fluenceInt[0] AND photEnergies LE fluenceInt[1]), co)
    IF (co EQ 0) THEN RETURN, 0.0
    
    ;== We're going to intergrate in log space:
    energies = ALOG10 (photEnergies[kk])
    IF KEYWORD_SET (ENERGY_INT) THEN model = photModel[kk] * photEnergies[kk] ELSE $ ;energies ELSE $
       model = photModel[kk] ;* energies

    ;== Interpolate the model over a regular grid (in log space)
    j = N_ELEMENTS (kk)
    k = j + ((j MOD 2 EQ 0) ? 1 : 0)   ;== We need an odd number of grid points
    delta = (energies[j - 1] - energies[0]) / FLOAT (k - 1)
    regEnergies = FINDGEN (k) * delta + energies[0]
    regModel = INTERPOL (model, energies, regEnergies) * 2. / 3.

    ;== Do the odd-even thing with the integrand
    ll = WHERE ((INDGEN (k) MOD 2) EQ 1)
    regModel[ll] = regmodel[ll] * 2.   ;== 4/3, 2/3 pattern
    
    ;== Treat the endpoints of the array: 1/3, instead of 2/3
    regModel[0] = regModel[0] / 2.
    regModel[k - 1] = regModel[k - 1] / 2.

	;== Catch the odd out-of-bounds number:
	nans = WHERE (FINITE (regModel, /NAN), coNaN)
	IF (coNaN GT 0) THEN BEGIN ; bad array!
		regModel[nans] = 0.
	ENDIF
    ;== Integrate:
    RETURN, delta * TOTAL (regModel * 10.^regEnergies) * ALOG (10.) ;== Mistake:
    
END

