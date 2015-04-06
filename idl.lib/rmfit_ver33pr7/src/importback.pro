;------------------------------------------------------------------------------
; RDP-- Import backround model from another file
;------------------------------------------------------------------------------
PRO PhaDisplay::importBack

    myDetList = self.Fitter->getDetectorList()
    detnames  = myDetList->names ()
    detnum = myDetList->count ()
    bak_filelist = STRARR(detnum)
    FOR i = 0, detnum - 1 DO BEGIN
		delimpos = STRPOS(detnames[i], '/', /REVERSE_SEARCH)
		bak_filename = detnames[i]
		IF (delimpos NE 0) THEN BEGIN
			bak_filename = STRMID(bak_filename, delimpos + 1, STRLEN(detnames[i]))
		ENDIF
		bak_filelist[i] = bak_filename
	ENDFOR
	
	initSel = INTARR(detnum)
	bakSelect = dialog_checklist(bak_filelist, INITIAL = initSel, $
	            TITLE = 'Import Background from Dataset:')
	bakInd = WHERE(bakSelect, bcnt)
	IF bcnt EQ 0 THEN RETURN
	
	det = myDetList->data (detnames[bakInd])
	myDisp = det.display
	IF myDisp.Background->haveModel() EQ 0 THEN BEGIN
          self->setStatus, 'Background model is required for this function.', 10, /REVERT
		RETURN
	ENDIF

;	myBack = myDisp.background
	OBJ_DESTROY, self.background
	self.background = OBJ_NEW ('Background', myDisp.Reader)
	backSpan = myDisp.Background->backSpan ()
	self.background->setSpan, backSpan
	backOrder = myDisp.Background->order () 
	newESpan = myDisp.lightcurve->energySpan()
	;== Transfer the fitted data to the new background:
	self.background->setIntegrated, myDisp.Background->integrated () 
;    combThresholds = myDisp.Spectrum->combinedThresholds ()	
;    e = NEAREST_EDGE (combThresholds, newESpan[0, *], channels) 
;	newELu = myDisp.lightcurve->energyLookup()

    ;== Fit background model
	;success = self.background->fit (ORDER = backOrder, CHANNELS = channels)    
stop
	self->setEnergyLookup, newELu
	self->setEnergySpan, newESpan
    self->integrateAndCombine
	self.lightcurve->update
	self.spectrum->update
	self->plot
	;== It will usually be the case that we don't want to fit the data we drew the model from:
	self.Fitter->deleteDetector,detnames[bakInd]

END
