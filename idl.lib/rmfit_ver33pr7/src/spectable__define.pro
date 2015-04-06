; SpecTable

; An object to capture spectral fit data from RMFIT and
; display data in a table that can be adjusted and
; sorted according to one's needs.  The table can then 
; be printed out in a format that is suitable for the
; wiki, as well as a delimited format than can be opened
; in a spreadsheet program

; Written, April 2009, A. Goldstein

;------------------------------------------------------------------------------------------------
; Object Initiation
FUNCTION SPECTABLE::INIT

; Create pointers and set default values
self.newcols=PTR_NEW(/ALLOCATE_HEAP)
self.sortselect.table_col=PTR_NEW(/ALLOCATE_HEAP)
self.logger=PTR_NEW(/ALLOCATE_HEAP)
self.eacols=PTR_NEW(/ALLOCATE_HEAP)
self.numparams=N_ELEMENTS(self.showcols)
self.showcols[0]=1
self.showcols[2:4]=1
self.showcols[self.numparams-3:self.numparams-1]=1
self.eaflag=0

; Successful initiation
RETURN, 1

END

;------------------------------------------------------------------------------------------------
; Parse Logger Output
PRO SPECTABLE::GET_INFO, all_fits, logger

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Store object state information from the logging widget
IF PTR_VALID(self.logger) THEN *self.logger=logger

; Build table GUI
self->BUILD

; Load table structure
WIDGET_CONTROL, self.baseID, GET_UVALUE=struct

; Find start of fit
spec_marker='----'
spec_marker_index=WHERE(STRPOS(all_fits, spec_marker) NE -1)

; If table already exists, and user performs another fit, 
; append fit to table rather than launch a new table
IF self.append EQ 1 THEN BEGIN
  spec_marker_index=MAX(spec_marker_index)
  all_fits=all_fits[spec_marker_index:*]
  flag=1
ENDIF

numfits=N_ELEMENTS(spec_marker_index)

; Find batch fit and omit
batch_marker='==> Batch Fitting'
batch_marker_index=WHERE(STRPOS(all_fits, batch_marker) NE -1)

IF batch_marker_index[0] NE -1 THEN BEGIN
  batch_end_marker='Spectrum #'
  batch_end=WHERE(STRPOS(all_fits, batch_end_marker) NE -1)
  batch_end_index=MAX(batch_end)
  FOR i=0, N_ELEMENTS(batch_marker_index)-1 DO all_fits[batch_marker_index[i]:batch_end_index[i]]=''
ENDIF

; Find fit error, if exists
err_marker='*** ERROR: Nonlinear fit failed.'
err_marker_index=WHERE(STRPOS(all_fits, err_marker) NE -1)

; If fit error(s) exists, find start and end of fit error(s)
IF (err_marker_index[0] NE -1) THEN BEGIN
  err_end_marker='*** ERROR! : Failed to obtain an inverse for the covariance matrix!'
  err_end_index=WHERE(STRPOS(all_fits, err_end_marker) NE -1)
  err_start_index=LONARR(N_ELEMENTS(err_end_index))
  
  FOR i=0, N_ELEMENTS(err_marker_index)-1 DO BEGIN
    startlines=spec_marker_index[WHERE(spec_marker_index LT err_marker_index[i])]
    err_start_index[i]=MIN(ABS(err_marker_index[i]-startlines), err_start)
    err_start_index[i]=spec_marker_index[err_start]
  ENDFOR
  
  ; Convert all error lines to whitespace (rather than deleting lines so
  ; so we don't have to shift all the indices after the deletion)
  FOR i=0, N_ELEMENTS(err_marker_index)-1 DO all_fits[err_start_index[i]:err_end_index[i]]=''
  
ENDIF

err_marker2='*** ERROR: status ='
err_marker_index2=WHERE(STRPOS(all_fits, err_marker2) NE -1)
IF (err_marker_index2[0] NE -1) THEN BEGIN
  err_end_marker2='*** ERROR: status ='
  err_end_index2=WHERE(STRPOS(all_fits, err_end_marker2) NE -1) + 8
  err_start_index2=LONARR(N_ELEMENTS(err_end_index2))
  
  FOR i=0, N_ELEMENTS(err_marker_index2)-1 DO BEGIN
    startlines=spec_marker_index[WHERE(spec_marker_index LT err_marker_index2[i])]
    err_start_index2[i]=MIN(ABS(err_marker_index2[i]-startlines), err_start)
    err_start_index2[i]=spec_marker_index[err_start]
  ENDFOR
  FOR i=0, N_ELEMENTS(err_marker_index2)-1 DO all_fits[err_start_index2[i]:err_end_index2[i]]=''
ENDIF

; Replicate structure N times for N number of fits, unless just appending
; to current table, then copy one entry  
IF self.append EQ 1 THEN spec_arr=REPLICATE(struct[0], 1) ELSE spec_arr=REPLICATE(struct, numfits)

; Find end of fit
end_marker='Energy Flux ='
endline=WHERE(STRPOS(all_fits, end_marker) NE -1)+1
y=0

; Define table size
spec_info=STRARR(self.numparams-1, numfits)
rownum=LONARR(numfits)

; Here begins the parsing routine, which first finds the function
; being used and then parsing the information based on the function
FOR z=0, N_ELEMENTS(endline)-1 DO BEGIN
  full_text=all_fits[y:endline[z]-1]
  y=endline[z]

  ; Find where the function name lives in the file
  fit_marker='TERM:'
  fit_marker_index=WHERE(STRPOS(full_text, fit_marker) NE -1)
  IF fit_marker_index[0] NE -1 THEN fit_line=full_text[fit_marker_index] ELSE RETURN
  fxn=STRTRIM(STRMID(fit_line, 6))
  fxn1=fxn[0]
  IF N_ELEMENTS(fxn) GT 1 THEN fxn_next=fxn[1:*]

; Rename function
  CASE 1 OF
  
    STRMATCH(fxn1, 'Power Law'): fxn='PL'
    STRMATCH(fxn1, 'Broken*'): fxn='BPL'
    STRMATCH(fxn1, 'Power Law w.*'): fxn='PL2'
    STRMATCH(fxn1, 'Smooth*'): fxn='SBPL'
    STRMATCH(fxn1, 'Band*'): fxn='Band'
    STRMATCH(fxn1, 'Old Band*'): fxn='Band, Old'
    STRMATCH(fxn1, 'Comptonized*'): fxn='Comp'
    STRMATCH(fxn1, 'Old Compton*'): fxn='Comp, Old'
    STRMATCH(fxn1, 'Brainerd*'): fxn='Brain PL'
    STRMATCH(fxn1, 'Log Normal*'): fxn='Log Norm'
    STRMATCH(fxn1, 'Gauss (log10*'):fxn='GLogE'
    STRMATCH(fxn1, 'Gaussian (lo*'):fxn='GLogE'
    STRMATCH(fxn1, 'Sunyaev*'): fxn='Sunyaev'
    STRMATCH(fxn1, 'OTTB*'): fxn='OTTB'
    STRMATCH(fxn1, 'Black*'): fxn='BB'
    STRMATCH(fxn1, 'Y. Soong*'): fxn='Y.Soong'
    STRMATCH(fxn1, 'Tanaka*'):fxn='Tanaka'
    STRMATCH(fxn1, 'OTT Synch*'):fxn='OTT Sync'
    STRMATCH(fxn1, 'XSPEC T*'): fxn='XTherm. Brem.'
    STRMATCH(fxn1, 'Gaussian L*'): fxn='GLine'
    STRMATCH(fxn1, 'Interstellar*'): fxn='IS Abs.'
    STRMATCH(fxn1, 'Redshifted I*'): fxn='Red IS Abs.'
   
    ELSE: BEGIN 
            ; Not able to parse function info, so skip and continue
            ; to next fit
            PRINT, 'Not able to append '+fxn+' to table'
            numfits=numfits-1
            GOTO, ESCAPE
          END
  
  ENDCASE
  spec_arr[z].fxn=fxn
  spec_info[4,z]=fxn
  
  ; Rename any secondary function
  IF N_ELEMENTS(fxn_next) GT 0 THEN BEGIN
    fxn2=STRARR(N_ELEMENTS(fxn_next))
    FOR f=0, N_ELEMENTS(fxn2)-1 DO BEGIN
      CASE 1 OF 
      
        STRMATCH(fxn_next[f], 'Power Law*'): fxn2[f]='PL'
        STRMATCH(fxn_next[f], 'Broken*'): fxn2[f]='BPL'
        STRMATCH(fxn_next[f], 'Power Law w.*'): fxn2[f]='PL2'
        STRMATCH(fxn_next[f], 'Smooth*'): fxn2[f]='SBPL'
        STRMATCH(fxn_next[f], 'Band*'): fxn2[f]='Band'
        STRMATCH(fxn_next[f], 'Old Band*'): fxn2[f]='Band, Old'
        STRMATCH(fxn_next[f], 'Comptonized*'): fxn2[f]='Comp'
        STRMATCH(fxn_next[f], 'Old Compt*'): fxn2[f]='Comp, Old'
        STRMATCH(fxn_next[f], 'Brainerd*'): fxn2[f]='Brain PL'
        STRMATCH(fxn_next[f], 'Log Normal*'): fxn2[f]='Log Norm'
        STRMATCH(fxn_next[f], 'Gauss (log10*'):fxn2[f]='GLogE'
        STRMATCH(fxn_next[f], 'Gaussian (lo*'):fxn2[f]='GLogE'
        STRMATCH(fxn_next[f], 'Sunyaev*'): fxn2[f]='Sunyaev'
        STRMATCH(fxn_next[f], 'OTTB*'): fxn2[f]='OTTB'
        STRMATCH(fxn_next[f], 'Black*'): fxn2[f]='BB'
        STRMATCH(fxn_next[f], 'Y. Soong*'): fxn2[f]='Y.Soong'
        STRMATCH(fxn_next[f], 'Tanaka*'):fxn2[f]='Tanaka'
        STRMATCH(fxn_next[f], 'OTT Synch*'):fxn2[f]='OTT Sync'
        STRMATCH(fxn_next[f], 'XSPEC T*'): fxn2[f]='XTherm. Brem.'
        STRMATCH(fxn_next[f], 'Gaussian L*'): fxn2[f]='GLine'
        STRMATCH(fxn_next[f], 'Low*'): fxn2[f]='Lo Cut'
        STRMATCH(fxn_next[f], 'High*'): fxn2[f]='Hi Cut'
        STRMATCH(fxn_next[f], 'Multiplicative*'): fxn2[f]='Mult. PL'
        STRMATCH(fxn_next[f], 'Interstellar*'): fxn2[f]='IS Abs.'
        STRMATCH(fxn_next[f], 'Mult. B*'): fxn2[f]='Mult. BPL'
        STRMATCH(fxn_next[f], 'Redshifted I*'): fxn2[f]='Red IS Abs.'
        STRMATCH(fxn_next[f], 'Eff.*'): fxn2[f]='Eff. Area'
        ELSE: BEGIN 
        
                ; Not able to parse function info, so skip and continue
                ; to next fit
                PRINT, 'Not able to append secondary fxn ' + fxn2 + ' to fit'
                RETURN
              END
      ENDCASE
    ENDFOR
  ENDIF ELSE fxn2='0'
  
  ; Escape if not recognized
  IF N_ELEMENTS(spec_arr) EQ 0 THEN BEGIN
    ESCAPE: CONTINUE
  ENDIF 
  
  marker='==> Data file'
  marker_index=WHERE(STRPOS(full_text, marker) NE -1)
  IF marker_index[0] NE -1 THEN grb_line=full_text[marker_index]
  name=STRMID(grb_line, STRPOS(grb_line, 'bn', /REVERSE_SEARCH)+2, 9)
  spec_arr[z].name=name[0]
  spec_info[1,z]=name[0]

    marker='Amplitude' & t=5
    self->PARAM_EXTRACT, marker, full_text, amp, amp_err, errhi, t, spec_info, z
    ;spec_arr[z].amp=spec_info[t,z]

    marker1='Index  ' & marker2='pow law indx' & marker3='Index <'
    marker4='Index1 <' & marker5='alpha' & t=[12, 12, 13, 13, 10]
    marker=[marker1, marker2, marker3, marker4, marker5]
    self->PARAM_EXTRACT, marker, full_text, index, index_err, errhi, t, spec_info, z
    ;spec_arr[z].index=spec_info[t,z]
    
    marker1='Index > BE' & marker2='Index2 >' & marker3='Indx BE' & marker4='beta'
    t=[14, 14, 14, 11]
    marker=[marker1, marker2, marker3, marker4]
    self->PARAM_EXTRACT, marker, full_text, index2, index2_err, errhi, t, spec_info, z
    ;spec_arr[z].index2=spec_info[t,z]
    
    marker='Index > BE2' & t=15
    self->PARAM_EXTRACT, marker, full_text, index3, index3_err, errhi, t, spec_info, z
    ;spec_arr[z].index3=spec_info[t,z]
    
    marker1='Break E' & marker2='break energy' & marker3='1st Break' & marker4='E0'
    t=[7, 7, 8, 7]
    marker=[marker1, marker2, marker3, marker4]
    self->PARAM_EXTRACT, marker, full_text, indexb, indexb_err, errhi, t, spec_info, z
    ;spec_arr[z].indexb=spec_info[t,z]
    
    marker='2nd Break' & t=9
    self->PARAM_EXTRACT, marker, full_text, break2, break2_err, errhi, t, spec_info, z
    ;spec_arr[z].break2=spec_info[t,z]
 
    marker='  Epeak  ' & t=6
    self->PARAM_EXTRACT, marker, full_text, epeak, epeak_err, errhi, t, spec_info, z
    ;spec_arr[z].epeak=spec_info[t,z]

    marker='Electron E' & t=16
    self->PARAM_EXTRACT, marker, full_text, ee, ee_err, errhi, t, spec_info, z
    ;spec_arr[z].electron_e=spec_info[t,z]
    
    marker1='Optical Dpth' & marker2='opticaldepth' & marker3='tau1' & t=[17,17,17]
    marker=[marker1, marker2]
    self->PARAM_EXTRACT, marker, full_text, opdepth, opdepth_err, errhi, t, spec_info, z
    ;spec_arr[z].opdepth=spec_info[t,z]
    
    marker='tau2' & t=18
    self->PARAM_EXTRACT, marker, full_text, opdepth2, opdepth2_err, errhi, t, spec_info, z
    ;spec_arr[z].opdepth2=spec_info[t,z]
    
    marker='Geometry Fac' & t=19
    self->PARAM_EXTRACT, marker, full_text, geofac, geofac_err, errhi, t, spec_info, z
    ;spec_arr[z].geofac=spec_info[t,z]
 
    marker=' kT ' & t=20
    self->PARAM_EXTRACT, marker, full_text, kt, kt_err, errhi, t, spec_info, z
    ;spec_arr[z].kt=spec_info[t,z]
     
    marker='Cutoff E' & t=21
    self->PARAM_EXTRACT, marker, full_text, cut, cut_err, errhi, t, spec_info, z
    ;spec_arr[z].cut=spec_info[t,z]
    
    marker1='E folding' & marker2='E-folding' & t=[22,22]
    marker=[marker1, marker2]
    self->PARAM_EXTRACT, marker, full_text, efold, efold_err, errhi, t, spec_info, z
    ;spec_arr[z].efold=spec_info[t,z]

    marker='param ratio' & t=23
    self->PARAM_EXTRACT, marker, full_text, parrat, parrat_err, errhi, t, spec_info, z
    ;spec_arr[z].parrat=spec_info[t,z]
  
    marker='Cosmol. z' & t=24
    self->PARAM_EXTRACT, marker, full_text, redshift, redshift_err, errhi, t, spec_info, z
    ;spec_arr[z].redshift=spec_info[t,z]
  
    marker='Metalicity' & t=25
    self->PARAM_EXTRACT, marker, full_text, metal, metal_err, errhi, t, spec_info, z
    ;spec_arr[z].metal=spec_info[t,z]

    marker1=' mu ' & marker2='Centroid' & marker3='centroid' & marker4='E_L'
    marker=[marker1, marker2, marker3, marker4] & t=[26, 28, 28, 28]
    self->PARAM_EXTRACT, marker, full_text, mu, mu_err, errhi, t, spec_info, z
    ;spec_arr[z].cent=spec_info[t,z]
  
    marker1= ' sigma ' & marker2='log10 FWHM' & marker3='FWHM'
    marker=[marker1, marker2, marker3] & t=[27, 29, 29]
    self->PARAM_EXTRACT, marker, full_text, sigma, sigma_err, errhi, t, spec_info, z
    ;spec_arr[z].sigma=spec_info[t,z]
    
    marker='logFWHM slop' & t=30
    self->PARAM_EXTRACT, marker, full_text, fslope, fslope_err, errhi, t, spec_info, z
    ;spec_arr[z].fslope=spec_info[t,z]
  
    marker1='equiv width' & marker2=' W '
    marker=[marker1, marker2] & t=[31, 31]
    self->PARAM_EXTRACT, marker, full_text, width, width_err, errhi, t, spec_info, z
    ;spec_arr[z].width=spec_info[t,z]

    marker='E_C' & t=32
    self->PARAM_EXTRACT, marker, full_text, ec, ec_err, errhi, t, spec_info, z
    ;spec_arr[z].ec=spec_info[t,z]

    marker='nHe/nH' & t=33
    self->PARAM_EXTRACT, marker, full_text, nhe, nhe_err, errhi, t, spec_info,z
    ;spec_arr[z].nhe=spec_info[t,z]

    marker='H col dens.' & t=34
    self->PARAM_EXTRACT, marker, full_text, coldense, coldense_err, errhi, 34, spec_info, z
    ;spec_arr[z].coldense=spec_info[34,z]

  
  ; Find where chi-square, dof, and red. chi-square live in the file
  chisq_marker='==> CHISQ'
  chisq_marker_index=WHERE(STRPOS(full_text, chisq_marker) NE -1)
  IF chisq_marker_index[0] NE -1 THEN BEGIN
    chisq_line=STRSPLIT(full_text[chisq_marker_index], /EXTRACT)
    chisq=STRMID(chisq_line[3], 0, STRPOS(chisq_line[3], ','))
    dof=STRMID(chisq_line[6], 0, STRPOS(chisq_line[6], ','))
    chi=DECIMALS(chisq, 2) + '/' + dof
    redchi=DECIMALS(STRMID(chisq_line[10], 0, STRPOS(chisq_line[10], ',')), 2)
    spec_arr[z].chi=chi
    spec_arr[z].redchi=redchi
    spec_info[35,z]=chi
    spec_info[36,z]=redchi
  ENDIF ELSE BEGIN
    spec_arr[z].chi='-'
    spec_arr[z].redchi='-'
    spec_info[35,z]='-'
    spec_info[36,z]='-'
  ENDELSE
  
  ; Find where the log likelihood lives in the file
  likely_marker='==> -2 LOG'
  likely_marker_index=WHERE(STRPOS(full_text, likely_marker) NE -1)
  IF likely_marker_index[0] NE -1 THEN BEGIN
    likely_line=STRSPLIT(full_text[likely_marker_index], /EXTRACT)
    likely=STRMID(likely_line[5], 0, STRPOS(likely_line[5], ','))
    dof=likely_line[8]
    likely=DECIMALS(likely, 2) + '/' + dof
    spec_arr[z].likely=likely
    spec_info[37,z]=likely
  ENDIF ELSE BEGIN
    spec_arr[z].likely='-'
    spec_info[37,z]='-'
  ENDELSE
  
  ; Find where the castor statistic lives in the file
  cash_marker='==> Castor'
  cash_marker_index=WHERE(STRPOS(full_text, cash_marker) NE -1)
  IF cash_marker_index[0] NE -1 THEN BEGIN
    cash_line=STRSPLIT(full_text[cash_marker_index], /EXTRACT)
    cash=STRMID(cash_line[4], 0, STRPOS(cash_line[4], ','))
    dof=STRMID(cash_line[7], 0, STRPOS(cash_line[7], ','))
    cash=DECIMALS(cash, 2) + '/' + dof
    spec_arr[z].cash=cash
    spec_info[38,z]=cash
  ENDIF ELSE BEGIN
    spec_arr[z].cash='-'
    spec_info[38,z]='-'
  ENDELSE
  
  ; Find where the photon flux lives in the file
  phflux_marker='==> Photon'
  phflux_marker_index=WHERE(STRPOS(full_text, phflux_marker) NE -1)
  phflux_line=STRSPLIT(full_text[phflux_marker_index], /EXTRACT)
  phflux=phflux_line[4]
  phflux_err=phflux_line[6]
  spec_arr[z].phflux=self->JOIN(phflux, phflux_err, 2)
  spec_info[39,z]=self->JOIN(phflux, phflux_err, 2)

  ; Find where the energy flux lives in the file
  eflux_marker='==> Energy'
  eflux_marker_index=WHERE(STRPOS(full_text, eflux_marker) NE -1)
  eflux_line=STRSPLIT(full_text[eflux_marker_index], /EXTRACT)
  eflux=eflux_line[4]
  eflux_err=eflux_line[6]
  spec_arr[z].eflux=self->JOIN(eflux, eflux_err, 3)
  spec_info[40,z]=self->JOIN(eflux, eflux_err, 3)
  
  ; Define detector arrays
  det_marker='==> Data file'
  det_marker_index=WHERE(STRPOS(full_text, det_marker) NE -1)
  det_line=MAKE_ARRAY(1,N_ELEMENTS(det_marker_index), /STRING)
  det_file=MAKE_ARRAY(1,N_ELEMENTS(det_marker_index), /STRING)
  det=MAKE_ARRAY(N_ELEMENTS(det_marker_index), /STRING)
  
  ; Find where the detector names and data types live in the file
  FOR j=0,N_ELEMENTS(det_marker_index)-1 DO BEGIN
    det_line[j]=STRTRIM(full_text[det_marker_index[j]])
    det_file[j]=STRMID(det_line[j], STRPOS(det_line[j],'glg')+4)
    IF STRCMP(det_file[j], 'ctime_',6) EQ 1 THEN BEGIN
      spec_arr[z].data_type='CTIME'
      spec_info[2,z]='CTIME'
      det[j]=STRMID(det_file[j], 6,2)
    ENDIF
    IF STRCMP(det_file[j], 'cspec_',6) EQ 1 THEN BEGIN
      spec_arr[z].data_type='CSPEC'
      spec_info[2,z]='CSPEC'
      det[j]=STRMID(det_file[j], 6,2)
    ENDIF
    IF STRCMP(det_file[j], 'tte_', 4) EQ 1 THEN BEGIN
      spec_arr[z].data_type='TTE'
      spec_info[2,z]='TTE'
      det[j]=STRMID(det_file[j], 4,2)
    ENDIF
  ENDFOR
  olddet=det
  self->DET_COLLECT, det
  spec_arr[z].det=det
  spec_info[0,z]=det
  
  ; Find where the fit interval lives in the file
  dt_marker='==> Fit interval'
  dt_marker_index=WHERE(STRPOS(full_text, dt_marker) NE -1)
  dt_line=STRTRIM(full_text[dt_marker_index[0]])
  dt_line=STRMID(dt_line, STRPOS(dt_line, ':')+1, STRPOS(dt_line, 's,')-STRPOS(dt_line, ':')-1)
  time=STRTRIM(dt_line)
  spec_arr[z].time=time
  spec_info[3,z]=time
  
  ; If effective area is present, find where it lives
  effarea_marker='TERM: Eff.'
  effarea_marker_index=WHERE(STRPOS(full_text, effarea_marker) NE -1)
  IF effarea_marker_index[0] NE -1 THEN BEGIN
    effarea_start=effarea_marker_index+2
    effarea_end=phflux_marker_index-3
    numeffs=FIX((effarea_end-effarea_start)+1)
    effterms=STRARR(2,numeffs)
    FOR k=0, numeffs[0]-1 DO BEGIN
      effarea_line=full_text[effarea_start+k]
      effarea_info=STRSPLIT(effarea_line, /EXTRACT)
      effarea=effarea_info[4]
      effarea_err=effarea_info[6]
      effterms[0,k]=STRJOIN([olddet[1+k], olddet[0]], '/')
      effterms[1,k]=self->JOIN(effarea, effarea_err, 2)
    ENDFOR
    IF self.append NE 1 THEN PTR_FREE, spec_arr[z].effarea
    spec_arr[z].effarea=PTR_NEW(effterms)
    self.eaflag=1
  ENDIF ELSE BEGIN
    IF self.append NE 1 THEN PTR_FREE, spec_arr[z].effarea
    spec_arr[z].effarea=PTR_NEW()
  ENDELSE
  
  ; Reformat function name if secondary function exists
  IF fxn2[0] NE '0' THEN BEGIN
    fxn=[fxn,fxn2]
    fxn=STRJOIN(fxn, ' w/ ')
    spec_arr[z].fxn=fxn
    spec_info[4,z]=fxn
  ENDIF
  
  ; Set the rownumbers for indexing
  IF N_ELEMENTS(flag) EQ 0 THEN spec_arr[z].rownum=STRCOMPRESS(STRING(z), /REMOVE_ALL) ELSE $
      spec_arr.rownum=STRCOMPRESS(STRING(N_ELEMENTS(struct)+1), /REMOVE_ALL)
  
ENDFOR

; If appending to an existing table, store info in structure
IF N_ELEMENTS(flag) NE 0 THEN BEGIN
  WIDGET_CONTROL, self.baseID, GET_UVALUE=struct
  rownum=STRCOMPRESS(STRING(N_ELEMENTS(struct)), /REMOVE_ALL)
  spec_arr=[struct, spec_arr]
  WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr
  *self.newcols=[[*self.newcols], [spec_info, rownum]]
  self->RIGHT_COLUMNS, numfits
ENDIF ELSE BEGIN
  
  ; If creating table, store info and and set append flag 
  ; to true, so next run will append rather than create
  WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr
  FOR z=0, numfits-1 DO rownum[z]=spec_arr[z].rownum
  rownum=STRCOMPRESS(STRING([[-1,-1], rownum]), /REMOVE_ALL)
  *self.newcols=[[self.colhead],[spec_info]]
  *self.newcols=[*self.newcols, TRANSPOSE(rownum)]
  self->RIGHT_COLUMNS, numfits
  IF self.append LE 1 THEN self.append=1

ENDELSE

END

;------------------------------------------------------------------------------------------------
; Find Location of Parameters in File
PRO SPECTABLE::PARAM_EXTRACT, marker, full_text, param, param_err, errhi, table_index, spec_info, z
  
  ; Some valuable variables:
  name = ' '
  vary = ' '
  value = 0.
  pm    = ' '
  error = 0.
  mp    = ' '
  errhi = 0.
  units = ' '
                           
; Find where the parameter lives in the file
  FOR i=0, N_ELEMENTS(marker)-1 DO BEGIN
    marker_index=WHERE(STRPOS(full_text, marker[i]) NE -1)
    IF marker_index[0] LT 0 THEN BEGIN
      IF spec_info[table_index[i], z] EQ '' THEN spec_info[table_index[i], z]='-' 
      CONTINUE
    ENDIF
    line = full_text[marker_index]
    FOR j=0, N_ELEMENTS(line)-1 DO BEGIN
      READS, line[j], name, vary, param, pm, param_err, units, $
             FORMAT = '(A20, A6, G12.4, A5,G12.3, A)'
      vary_fix = STRTRIM(vary, 2) 
      IF vary_fix EQ 'INTRVL' THEN BEGIN
        READS, line[j], name, vary, param, pm, param_err, mp, errhi, units, $
               FORMAT = '(A20, A6, G12.4,A5,G12.3 ,A3,G12.3, A)'
        IF spec_info[table_index[i], z] EQ '-' THEN spec_info[table_index[i], z]=''
        spec_info[table_index[i], z]=spec_info[table_index[i], z]+self->JOIN(param, param_err, 2, ERR_HI=errhi)+'  '
      ENDIF ELSE BEGIN
        IF spec_info[table_index[i], z] EQ '-' THEN spec_info[table_index[i], z]=''
        spec_info[table_index[i], z]=spec_info[table_index[i], z]+self->JOIN(param, param_err, 2)+'  '
      ENDELSE
    ENDFOR  
  ENDFOR

RETURN

END

;------------------------------------------------------------------------------------------------
; Collect Detectors Into One String
PRO SPECTABLE::DET_COLLECT, dets

; Index NaI and BGO detectors
n_index=WHERE(STRMID(dets, 0, 1) EQ 'n')
b_index=WHERE(STRMID(dets, 0, 1) EQ 'b')

; Sort NaIs and join into one string
IF n_index[0] NE -1 THEN BEGIN
  ndets=dets(n_index)
  ndets=ndets[SORT(ndets)]
  ndets=STRJOIN(ndets, '+')
ENDIF

; Sort BGOs and join into one string
IF b_index[0] NE -1 THEN BEGIN
  bdets=dets(b_index)
  bdets=bdets[SORT(bdets)]
  bdets=STRJOIN(bdets, '+')
ENDIF

; If NaIs and BGOs both exist, join into one string
IF (n_index[0] NE -1) AND (b_index[0] EQ -1) THEN dets=ndets
IF (n_index EQ[0] -1) AND (b_index[0] NE -1) THEN dets=bdets
IF (n_index NE[0] -1) AND (b_index[0] NE -1) THEN dets=STRJOIN([ndets, bdets], '+')

RETURN

END

;------------------------------------------------------------------------------------------------
; Display Columns
PRO SPECTABLE::COLUMN_SHOW

; Find columns to be displayed
showindex=WHERE(self.showcols EQ 1)

; Make sure at least one column is displayed
IF showindex[0] NE -1 THEN BEGIN
  
  showcols=(*self.newcols)[showindex, *]
  
  ; Find column headers
  colheads=showcols[*,0]
  
  ; Find column widths
  colwidths=FIX(showcols[*,1])
  
  ; Find corresponding fit info for those columns
  info=showcols[*,2:*]
  
  numfits=N_ELEMENTS(info[0,*])
  numcols=N_ELEMENTS(colheads)-1

;  FOR i=0, numfits-1 DO BEGIN
;    index=WHERE(STRPOS(info[*,i], '  ') NE -1)
;    IF index[0] NE -1 THEN BEGIN
;      FOR j=0, N_ELEMENTS(index)-1 DO BEGIN
;        temp=STRSPLIT(info[index[j], i], '  ', /EXTRACT, /REGEX)
;        FOR k=0, N_ELEMENTS(temp)-1 DO temp[k]=temp[k]+STRING(13B)
;        info[index[j], i]=STRJOIN(temp, ' ')
;      ENDFOR
;    ENDIF
;  ENDFOR
  
  
  ; Update table with new column definitions
  struct=[[colheads], [STRCOMPRESS(STRING(colwidths), /REMOVE_ALL)], [info]]
  WIDGET_CONTROL, self.tableID, COLUMN_LABELS=colheads[0:*], COLUMN_WIDTHS=colwidths[0:*], $
                  TABLE_XSIZE=numcols, TABLE_YSIZE=numfits, SET_VALUE=info[0:*,*], ALIGNMENT=1

  ; Save new column definitions
  WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
  FOR i=0, N_ELEMENTS(spec_arr)-1 DO (spec_arr)[i].rownum=STRCOMPRESS(STRING(i), /REMOVE_ALL)
  WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr

  ; If effective area exists then create table
  IF self.eaflag EQ 1 THEN self->EFF_AREA

ENDIF

END

;------------------------------------------------------------------------------------------------
; Determine Correct Columns To Display
PRO SPECTABLE::RIGHT_COLUMNS, numfits

;self.showcols[5:self.numparams-4]=0

; Get stored structure
WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
totalnum=N_ELEMENTS(spec_arr)


IF KEYWORD_SET(numfits) EQ 0 THEN BEGIN
  numfits=totalnum
  fitflag=0
ENDIF ELSE fitflag=1
del_col = INTARR(numfits)
; Cycle through functions to determine which columns should
; be displayed
FOR i=0, numfits-1 DO BEGIN
  ; Temporarily split apart function name 
  IF (self.append EQ 1) AND (fitflag EQ 1) THEN fxn=STRSPLIT(spec_arr[totalnum+i-1].fxn, ' w/ ', /EXTRACT, /REGEX) $
     ELSE fxn=STRSPLIT(spec_arr[i].fxn, ' w/ ', /EXTRACT, /REGEX)
ENDFOR


flag=INTARR(26)                                  ; Number of models
lastfit=N_ELEMENTS((*self.newcols)[0,*])-1
FOR i=0, lastfit-2 DO BEGIN
  CASE (*self.newcols)[4, i+2]  OF
    'PL': flag[0]=1  
    'BPL':flag[1]=1
    'PL2':flag[2]=1
    'SBPL':flag[3]=1
    'Band':flag[4]=1
    'Band, Old':flag[5]=1
    'Comp':flag[6]=1
    'Comp, Old':flag[7]=1
    'Brain PL': flag[8]=1
    'Log Norm': flag[9]=1
    'GLogE': flag[10]=1
    'Sunyaev': flag[12]=1
    'OTTB': flag[13]=1
    'BB': flag[14]=1
    'Y.Soong': flag[15]=1
    'Tanaka': flag[16]=1
    'OTT Synch': flag[17]=1
    'XTherm. Brem.': flag[18]=1
    'GLine': flag[19]=1
    'Lo Cut': flag[20]=1
    'Hi Cut': flag[21]=1
    'Mult. PL': flag[22]=1
    'IS Abs.': flag[23]=1
    'Mult. BPL': flag[24]=1
    'Red. IS Abs.': flag[25]=1
    ELSE: a=0
  ENDCASE
ENDFOR

IF NOT (flag[0] XOR flag[6] XOR flag[7] XOR flag[15] XOR flag[22]) AND (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  (*self.newcols)[10,0]='Alpha'
ENDIF
IF (flag[0] OR flag[6] OR flag[7] OR flag[15] OR flag[22]) AND (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  (*self.newcols)[10,0]='Alpha/Index'
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[12,i+2] NE '-' THEN BEGIN
      (*self.newcols)[10,i+2]=(*self.newcols)[12,i+2]
      (*self.newcols)[12,i+2]='-'
    ENDIF
  ENDFOR
ENDIF
IF (flag[0] OR flag[6] OR flag[7] OR flag[15] OR flag[22]) AND NOT (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  (*self.newcols)[10,0]='Alpha'
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[10,i+2] NE '-' THEN BEGIN
      (*self.newcols)[12,i+2]=(*self.newcols)[10,i+2]
      (*self.newcols)[10,i+2]='-'
    ENDIF
  ENDFOR
ENDIF

IF NOT (flag[1] XOR flag[2] XOR flag[3] XOR flag[24]) AND (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  IF flag[0] THEN (*self.newcols)[10,0]='Alpha/Index' ELSE (*self.newcols)[10,0]='Alpha'
  (*self.newcols)[11,0]='Beta'
ENDIF
IF (flag[1] OR flag[2] OR flag[3] OR flag[24]) AND (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  (*self.newcols)[10,0]='Alpha/Index1'
  (*self.newcols)[11,0]='Beta/Index2'
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[13,i+2] NE '-' THEN BEGIN
      (*self.newcols)[10,i+2]=(*self.newcols)[13,i+2]
      (*self.newcols)[13,i+2]='-'
    ENDIF
    IF (*self.newcols)[14,i+2] NE '-' THEN BEGIN
      (*self.newcols)[11,i+2]=(*self.newcols)[14,i+2]
      (*self.newcols)[14,i+2]='-'
    ENDIF
  ENDFOR
ENDIF
IF (flag[1] OR flag[2] OR flag[3] OR flag[24]) AND NOT (flag[4] OR flag[5] OR flag[16]) THEN BEGIN
  (*self.newcols)[10,0]='Alpha'
  (*self.newcols)[11,0]='Beta'
  IF (flag[0] OR flag[6] OR flag[7] OR flag[15] OR flag[22]) AND (flag[1] OR flag[2] OR flag[3] OR flag[24]) THEN BEGIN
    FOR i=0, lastfit-2 DO BEGIN
      IF (*self.newcols)[12,i+2] NE '-' THEN BEGIN
        (*self.newcols)[13,i+2]=(*self.newcols)[12,i+2]
        (*self.newcols)[12,i+2]='-'
      ENDIF
    ENDFOR
  ENDIF
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[10,i+2] NE '-' THEN BEGIN
      (*self.newcols)[13,i+2]=(*self.newcols)[10,i+2]
      (*self.newcols)[10,i+2]='-'
    ENDIF
    IF (*self.newcols)[11,i+2] NE '-' THEN BEGIN
      (*self.newcols)[14,i+2]=(*self.newcols)[11,i+2]
      (*self.newcols)[11,i+2]='-'
    ENDIF
  ENDFOR
ENDIF

IF NOT (flag[2]) AND (flag[1] OR flag[3] OR flag[5] OR flag[15] OR flag[24]) THEN BEGIN
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[8,i+2] NE '-' THEN BEGIN
      (*self.newcols)[7,i+2]=(*self.newcols)[8,i+2]
      (*self.newcols)[8,i+2]='-'
    ENDIF
  ENDFOR
ENDIF
IF (flag[2]) AND (flag[1] OR flag[3] OR flag[5] OR flag[15] OR flag[24]) THEN BEGIN
  FOR i=0, lastfit-2 DO BEGIN
    IF (*self.newcols)[7,i+2] NE '-' THEN BEGIN
      (*self.newcols)[8,i+2]=(*self.newcols)[7,i+2]
      (*self.newcols)[7,i+2]='-'
    ENDIF
  ENDFOR
ENDIF


IF self.showcols[1] NE 1 THEN a=0 ELSE a=1
IF self.showcols[5] NE 1 THEN b=0 ELSE b=1
self.showcols[5:self.numparams-4]=0
FOR i=0, lastfit-2 DO self.showcols[WHERE((*self.newcols)[*,i+2] NE '-' AND (*self.newcols)[*,i+2] NE '')]=1
self.showcols[1]=a & self.showcols[5]=b

FOR i=0, numfits-1 DO BEGIN
    IF del_col[i] EQ 1 THEN BEGIN
		  IF i LT numfits-1 THEN BEGIN
			  IF i EQ 0 THEN BEGIN
				  spec_arr=spec_arr[i+1:*]
        ENDIF ELSE BEGIN
				IF self.append EQ 1 THEN spec_arr=[spec_arr[0:i-1], spec_arr[i+1:*]] 
			  ENDELSE
        IF self.append EQ 1 THEN  *self.newcols=[[(*self.newcols)[*,0:i+1]], [(*self.newcols)[*,i+3:*]]]
		  ENDIF ELSE BEGIN
			  IF i NE 0 THEN BEGIN
			    spec_arr=spec_arr[0:i-1]
          *self.newcols=(*self.newcols)[*,0:i+1]
        ENDIF ELSE BEGIN
          *self.newcols=(*self.newcols)[*, 0:N_ELEMENTS((*self.newcols)[0,*])-2]
          self.append=2
          RETURN
        ENDELSE
		  ENDELSE
    ENDIF	
	; Save structure
	WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr
  
ENDFOR


numfits=N_ELEMENTS(spec_arr)

; Determine which fitting statistic is used, and display
; corresponding columns
FOR i=0, numfits-1 DO BEGIN
  IF (spec_arr[i].chi NE '-') AND (spec_arr[i].chi NE '') THEN self.showcols[35:36]=1
  IF (spec_arr[i].likely NE '-') AND (spec_arr[i].likely NE '') THEN self.showcols[37]=1
  IF (spec_arr[i].cash NE '-') AND (spec_arr[i].cash NE '') THEN self.showcols[38]=1
ENDFOR
            
self->COLUMN_SHOW

RETURN

END

;------------------------------------------------------------------------------------------------
; Sort Based On Multiple Keys
PRO SPECTABLE::MULTISORT, order

; Find rank of sort keys and sort from most important
; to least important
cols=INDGEN(self.numparams-1)
order2=[[cols], [ABS(order)]]
order2_index=WHERE(order2[*,1] NE 0)
IF order2_index[0] NE -1 THEN order2=order2[order2_index, *] ELSE RETURN
order2=order2[SORT(order2[*, 1]), *]

order=order[WHERE(order NE 0)]
sign=LONARR(N_ELEMENTS(order))

; If sign of the rank is negative then sort in descending order
; If sign of the rank is positive then sort in ascending order 
FOR i=0, N_ELEMENTS(order)-1 DO BEGIN
  IF FINITE(SQRT(order[i]), /NAN) EQ 1 THEN BEGIN
    sign[i]=-1
  ENDIF ELSE BEGIN
    sign[i]=1
  ENDELSE
ENDFOR

; Organize sorting keys
oldrows=(*self.newcols)[*, 2:*]
numfits=N_ELEMENTS(oldrows[0,*])
numkeys=N_ELEMENTS(order2[*,0])
tempcols=STRARR(numkeys, numfits)

FOR i=0, numkeys-1 DO tempcols[i,*]=oldrows[order2[i],*]

; Up to 9 sorting keys can be used
FOR i=0, numkeys-1 DO BEGIN
  
  CASE i OF
    0: key1=tempcols(0,*)
    1: key2=tempcols(1,*)
    2: key3=tempcols(2,*)
    3: key4=tempcols(3,*)
    4: key5=tempcols(4,*)
    5: key6=tempcols(5,*)
    6: key7=tempcols(6,*)
    7: key8=tempcols(7,*)
    8: key9=tempcols(8,*)
    ELSE: RETURN
  ENDCASE

ENDFOR

; Send the sort keys and sort order to Craig Markwardt's sorting function
; The sorting function will return a long array of sorted row indices
CASE numkeys OF
  1: indices=MULTISORT(key1, ORDER=sign)
  2: indices=MULTISORT(key1, key2, ORDER=sign)
  3: indices=MULTISORT(key1, key2, key3, ORDER=sign)
  4: indices=MULTISORT(key1, key2, key3, key4, ORDER=sign)
  5: indices=MULTISORT(key1, key2, key3, key4, key5, ORDER=sign)
  6: indices=MULTISORT(key1, key2, key3, key4, key5, key6, ORDER=sign)
  7: indices=MULTISORT(key1, key2, key3, key4, key5, key6, key7, ORDER=sign)
  8: indices=MULTISORT(key1, key2, key3, key4, key5, key6, key7, key8, ORDER=sign)
  9: indices=MULTISORT(key1, key2, key3, key4, key5, key6, key7, key8, key9, ORDER=sign)
  ELSE: RETURN
ENDCASE

; Index rows with new indices
newrows=oldrows[*,indices]
*self.newcols=[[(*self.newcols)[*,0:1]], [newrows]]

; Reindex and store new row numbers
FOR i=0, N_ELEMENTS((*self.newcols)[0,*])-3 DO (*self.newcols)[self.numparams-1, i+2]=STRCOMPRESS(STRING(i), /REMOVE_ALL)

; Save structure
WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
spec_arr=spec_arr[indices]
WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr

self->COLUMN_SHOW

END

;------------------------------------------------------------------------------------------------
; Write Table
PRO SPECTABLE::MAKE_TABLE, type

; Include Effective Area?
IF self.eaflag EQ 1 THEN effq=DIALOG_MESSAGE('Do you want to include the Effective Area(s)?', $
                                                DIALOG_PARENT=self.baseID, /QUESTION)

CASE type OF 

  0: BEGIN
       ; Create header for wiki
       delimiter='||'
       pre="<:>'''"
       post="'''"
       rowdefine="||<:rowbgcolor="+STRING("""#8080FF""")+" tablewidth="+STRING(""" 115%""")+">"
       default_filename='spec2wiki.txt'     
     END

  1: BEGIN 
       ; Create header for confluence
       delimiter='||'
       pre='*'
       post='*'
       rowdefine=delimiter
       default_filename='spec2confluence.txt'
     END

  2: BEGIN  
       ; Create header for open spreadsheet
       delimiter=','
       pre=''
       post=''
       rowdefine=delimiter
       default_filename='spec2excel.txt'
     END

ENDCASE


IF type EQ 0 THEN num_col= "'''#" + post  ELSE num_col= pre + '#' + post

; Find columns to be displayed
showindex=WHERE(self.showcols EQ 1)
showcols=['#', (*self.newcols)[showindex, 0]]
header=pre + showcols[0:N_ELEMENTS(showcols)-2] + post

IF N_ELEMENTS(effq) GT 0 THEN ea_col=pre + *self.eacols + post

; Add effective area columns
IF N_ELEMENTS(effq) GT 0 THEN BEGIN
  IF effq EQ 'Yes' THEN header=[header, ea_col]
ENDIF

header=STRJOIN(header, delimiter)
header= rowdefine + header + delimiter

; Determine info corresponding to columns to be printed
WIDGET_CONTROL, self.tableID, GET_VALUE=spec_arr
numfits=N_ELEMENTS(spec_arr[0,*])
numcols=N_ELEMENTS(spec_arr[*,0])
IF N_ELEMENTS(spec_arr[0,*]) GT 1 THEN spec_arr=SHIFT(spec_arr, [1,0]) ELSE $
                                       spec_arr=SHIFT(spec_arr, 1)
; Add effective areas
IF N_ELEMENTS(effq) GT 0 THEN BEGIN
  IF effq EQ 'Yes' THEN BEGIN
    WIDGET_CONTROL, self.efftableID, GET_VALUE=effarea
    spec_arr=[spec_arr, effarea]
  ENDIF
ENDIF
      
; Create one string for each table row, adding delimiters
IF type EQ 1 THEN delimiter='|'
spec_table=STRARR(1, numfits)
FOR i=0, N_ELEMENTS(spec_arr[0,*])-1 DO spec_arr[0,i]=STRCOMPRESS(STRING(spec_arr[0,i]+1), /REMOVE_ALL)
FOR i=0, numfits-1 DO spec_table[0,i]=STRJOIN(spec_arr[*,i], delimiter)
FOR i=0, numfits-1 DO spec_table[0,i]=delimiter + spec_table[0,i] + delimiter

; Add special formatting for confluence
IF type EQ 1 THEN BEGIN
  FOR i=0, numfits-1 DO spec_table[0,i]=STRJOIN(STRSPLIT(spec_table[0,i], '+', /EXTRACT), '\+')
  FOR i=0, numfits-1 DO spec_table[0,i]=STRJOIN(STRSPLIT(spec_table[0,i], '-', /EXTRACT), '\-')
ENDIF

; Choose directory and filename
filename=DIALOG_PICKFILE(DEFAULT_EXTENSION='txt', PATH = !RM_PARAMS.lastPath, $
                         DIALOG_PARENT=self.baseID, FILE=default_filename, /WRITE)
IF filename EQ '' THEN RETURN

; Open file and write header to file
OPENW, lun, filename, /GET_LUN
PRINTF, lun, header, FORMAT='(a,$)'

; Write each row to file
FOR i=0, numfits-1 DO BEGIN
  PRINTF, lun, ''
  PRINTF, lun, spec_table[0,i], FORMAT='(a,$)'
ENDFOR

; Close file
CLOSE, lun
FREE_LUN, lun

; If running OS X, launch the file (only works for Mac OS X),
; otherwise file can be found in the selected folder
IF !VERSION.OS_NAME EQ 'Mac OS X' THEN SPAWN, "open " + filename

END

;------------------------------------------------------------------------------------------------
; Concatenate Value And Error
FUNCTION SPECTABLE::JOIN, val, err, dp, ERR_HI = err_hi

; If both value and error are in scientific notation, run
; subroutine to specially format resulting string
IF (STRPOS(val, 'E') NE -1) AND (STRPOS(err, 'E') NE -1) THEN BEGIN
  expon=STRMID(val, STRPOS(val, 'E')+1)
  expon2=STRMID(err, STRPOS(err, 'E')+1)
  valnew=STRMID(val, 0, STRPOS(val, 'E'))
  valnew=DECIMALS(valnew, dp)
  errnew=STRMID(err, 0, STRPOS(err, 'E'))
  IF ABS(LONG(expon2)) GT ABS(LONG(expon)) THEN BEGIN
    errnew=FLOAT(errnew)/10^(ABS(LONG(expon2))-ABS(LONG(expon)))
  ENDIF ELSE BEGIN
    errnew=FLOAT(errnew)*10^(ABS(LONG(expon))-ABS(LONG(expon2)))
  ENDELSE
  errnew=STRCOMPRESS(STRING(errnew), /REMOVE_ALL)
  errnew=DECIMALS(errnew, dp)
  
  IF KEYWORD_SET(ERR_HI) THEN BEGIN
	  expon2=STRMID(err_hi, STRPOS(err_hi, 'E')+1)
	  errhi=STRMID(err_hi, 0, STRPOS(err_hi, 'E'))
	  IF ABS(LONG(expon3)) GT ABS(LONG(expon)) THEN BEGIN
		  errhi=FLOAT(errhi)/10^(ABS(LONG(expon3))-ABS(LONG(expon)))
	  ENDIF ELSE BEGIN
		  errhi=FLOAT(errhi)*10^(ABS(LONG(expon))-ABS(LONG(expon3)))
	  ENDELSE
	  errhi=STRCOMPRESS(STRING(errhi), /REMOVE_ALL)
	  errhi=DECIMALS(err_hi, dp)
	  RETURN, "("+ valnew + " - " + errnew +  " + " + errhi + ")E"+expon
  ENDIF ELSE BEGIN
	  RETURN, "("+ valnew + ' +/- ' + errnew + ")E"+expon
  ENDELSE
  
ENDIF ELSE BEGIN

  ; Round value and error
;  IF STRPOS(val, 'E') NE -1 THEN SCI_NOT, val, dp ELSE $
;    val=DECIMALS(val, dp)
;  IF STRPOS(err, 'E') NE -1 THEN SCI_NOT, err, dp ELSE $
;    err=DECIMALS(err, dp)
IF STRPOS(val, 'E') NE -1 THEN BEGIN
  a=[val,err]
  SCI_NOT, a
ENDIF ELSE BEGIN
val=DECIMALS(val, dp)
err=DECIMALS(err, dp)
ENDELSE

  ; Return concatenated string
  IF KEYWORD_SET(ERR_HI) THEN BEGIN
	  IF STRPOS(err_hi, 'E') NE -1 THEN SCI_NOT, err_hi, dp ELSE $
		err_hi=DECIMALS(err_hi, dp)
	  RETURN, val + " (+" + err_hi +  "/-" + err + ')'
  ENDIF ELSE BEGIN
	  RETURN, val + ' +/- ' + err 
  ENDELSE
  
ENDELSE

END

;------------------------------------------------------------------------------------------------
; Resize Table Widget
PRO SPECTABLE::RESIZE, event

; Get new width and height of base widget
x=event.x
y=event.y

; Calculate difference in width and height between old and new size
dx=FLOAT(x)-FLOAT(self.xsize)
dy=FLOAT(y)-FLOAT(self.ysize)

; Change the width and height of table widget by that difference
WIDGET_CONTROL, self.tableID, SCR_XSIZE=self.cols+dx, SCR_YSIZE=self.rows+dy

; Store differences
self.cols=self.cols+dx
self.rows=self.rows+dy

; Store new base width and height
self.xsize=x
self.ysize=y

RETURN

END

;------------------------------------------------------------------------------------------------
; Entry For Main Event Handler
PRO SPECTABLE_EVENT, event

; Since the entry for the event handler must be outside the object
; we need to have a way of referencing the existing object,
; so we find the ID for our base widget where we have our object stored
rowID=WIDGET_INFO(event.top, FIND_BY_UNAME='rowbar')

; Then we can use that ID to restore our object definition
WIDGET_CONTROL, rowID, GET_UVALUE=self

; And then pass our event to the event handler inside the object
self->EVENT_HANDLER, event

END

;------------------------------------------------------------------------------------------------
; Main Event Handler
PRO SPECTABLE::EVENT_HANDLER, event

; Get event actions
WIDGET_CONTROL, event.top , GET_UVALUE=widgetIDs

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Handle events
CASE event.id OF
  
  ; Make table
  self.button1ID: BEGIN
            CASE event.index OF
              0: self->MAKE_TABLE, 0          ; wiki
              1: self->MAKE_TABLE, 1          ; confluence
              2: self->MAKE_TABLE, 2          ; spreadsheet
            ENDCASE
          END
          
  ; Delete row
  self.button2ID: BEGIN
               WIDGET_CONTROL, event.top, GET_UVALUE=struct
               
               ; If table is greater than one row, delete selected row
               IF (N_ELEMENTS(struct) GT 1) AND (self.table_row.row_info[2] NE '') THEN BEGIN
                 
                 ; Delete row from table
                 WIDGET_CONTROL, self.tableID, /DELETE_ROWS
                 no_space=self.table_row.row_info[WHERE(self.table_row.row_info NE '')]
                 row=no_space[N_ELEMENTS(no_space)-1]
                 
                 ; Find the corresponding row in the structure array
                 rownums=STRARR(N_ELEMENTS((*self.newcols)[0,*]))
                 FOR i=0, N_ELEMENTS((*self.newcols)[0,*])-1 DO $
                          rownums[i]=(*self.newcols)[self.numparams-1,i]
                 IF (rownums[N_ELEMENTS(rownums)-1] EQ 0) AND (N_ELEMENTS(rownums) GE 3) THEN $
                    rownums=rownums[0:N_ELEMENTS(rownums)-2]
                 delrow=WHERE(rownums EQ LONG(row))
                 
                 first=0
                 last=N_ELEMENTS((*self.newcols)[0,*])-1
                 
                 ; Delete row from structure array
                 CASE delrow OF
                   first: *self.newcols=[[(*self.newcols)[*,0:1]], [(*self.newcols)[*,3:*]]]
                   last: *self.newcols=(*self.newcols)[*,0:last-1]
                   ELSE: *self.newcols=[[(*self.newcols)[*,0:delrow-1]], [(*self.newcols)[*,delrow+1:last]]]
                 ENDCASE
                
                 ; As well as here
                 CASE LONG(row) OF
                   0: struct=struct[1:*]
                   N_ELEMENTS(struct)-1: struct=struct[0:N_ELEMENTS(struct)-2]
                   ELSE: struct=[struct[0:row-1], struct[row+1: N_ELEMENTS(struct)-1]]
                 ENDCASE
                                 
                 ; Reindex structure array
                 FOR n=0, N_ELEMENTS((*self.newcols)[0,*])-3 DO $
                    (*self.newcols)[self.numparams-1,n+2]=STRCOMPRESS(STRING(n), /REMOVE_ALL)
                 WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
                 FOR i=0, N_ELEMENTS(spec_arr)-1 DO (spec_arr)[i].rownum=STRCOMPRESS(STRING(i), /REMOVE_ALL)
                 FOR i=0, N_ELEMENTS(struct)-1 DO (struct)[i].rownum=STRCOMPRESS(STRING(i), /REMOVE_ALL)
                 
                 ; Save structure array
                 self.table_row.row_info=0
                 *(self.sortselect.table_col)=0
                 WIDGET_CONTROL, self.baseID, SET_UVALUE=spec_arr
                 WIDGET_CONTROL, event.top, SET_UVALUE=struct
                 
                 ; Reset row info
                 self.table_row.row_info=''
                 self.table_row.row_info=''
                 
                 self->RIGHT_COLUMNS
             
               ENDIF
             END
  
  ; Show/Hide columns
  self.button3ID: self->CHECKBOX, self.showcols
  
  ; Sort data
  self.button4ID: BEGIN
                  WIDGET_CONTROL, self.baseID, GET_UVALUE=struct
                  respectable=(struct[0]).self
                  WIDGET_SORT, self.showcols, respectable
                END
 
  ; Close table
  self.button5ID: self->EXIT, event
  
  ; Row selection event
  self.tableID: BEGIN
                   
                   ; Retrieve table's selection mode and selection
                   disjoint=WIDGET_INFO(self.tableID, /TABLE_DISJOINT_SELECTION)
                   selection=WIDGET_INFO(self.tableID, /TABLE_SELECT)
                   
                   ; Check to see whether selection exists
                   IF selection[0] NE -1 THEN has_select=1 ELSE has_select=0
                   
                   ;  If there is a  valid selection, get the value
                   WIDGET_CONTROL, self.tableID, GET_VALUE=spec_arr
                   numrows=N_ELEMENTS(spec_arr[0,*])
                   numcols=N_ELEMENTS(spec_arr[*,0])
                   ; Store selected row information
                   
                   IF event.type EQ 4 THEN BEGIN
                     IF (has_select) AND (event.sel_top LT numrows) AND (event.sel_bottom LT numrows) $
                        AND (event.sel_left LT numcols) AND (event.sel_right LT numcols) THEN BEGIN
                       WIDGET_CONTROL, self.tableID, GET_VALUE=table_row, /USE_TABLE_SELECT
                       IF N_ELEMENTS(table_row[0,*]) EQ 1 THEN self.table_row.row_info=table_row
                       ;self.table_row.row_info=table_row
                       has_select=0
                     ENDIF
                   ENDIF
                 END
  
  self.baseID: self->RESIZE, event
  
  ; In the event of some unforeseen event
  ElSE: PRINT, 'Oops!'

ENDCASE

END

;------------------------------------------------------------------------------------------------
; Exit And Destroy
PRO SPECTABLE::EXIT, event

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Dump information stored in pointers
dump=TEMPORARY(*self.newcols)
IF N_ELEMENTS(*self.sortselect.table_col) NE 0 THEN $
    dump2=TEMPORARY(*self.sortselect.table_col)

; Dump any effective area info 
WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
FOR i=0, N_ELEMENTS(spec_arr)-1 DO BEGIN
  IF PTR_VALID(spec_arr[i].effarea) THEN dummy=TEMPORARY(*spec_arr[i].effarea)
  PTR_FREE, spec_arr[i].effarea
ENDFOR

; Reset table flag in widget_logger, so it knows table 
; does not exist
(*self.logger).tflag=0
; Free pointers
PTR_FREE, self.newcols, self.sortselect.table_col, self.logger, self.eacols

; Destroy table
WIDGET_CONTROL, self.baseID, /DESTROY
OBJ_DESTROY, self
STRUCT_FREE, spec_arr

END

;------------------------------------------------------------------------------------------------
; Build Table Widget
PRO SPECTABLE::BUILD, base

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; If table does not already exist, then create
IF self.baseID EQ 0L THEN BEGIN

  ; Define default column names and widths
  columns=['Detectors','GRB Name','Data','Dt (s)','Model','Amplitude','Epeak','Break E', 'Break E1', $
           'Break E2','Alpha','Beta','Index','Index1','Index2', 'Index3', 'Electron E', 'Op. Depth',$
           'Op. Depth2','Geom. Factor', 'kT', 'Cutoff E', 'E Folding','Param. Ratio','Cosmol. z',$
           'Metalicity','Mu', 'Sigma','Centroid','FWHM','FWHM slope','Line Width','Electron Cool','nHe/NH',$
           'H Column Dens.','Chi2/DOF','Red. Chi2','Likely/DOF', 'C-Stat/DOF', 'Ph. Flux (10-1000 keV)', $
           'En. Flux (10-1000 keV)']
  widths=['60','80','50','90','70','100''100','100','100','100','100','100','100','100','100','100','100',$
          '100','100','100','100','100','100','100','100','100','100','100','100','100','100','100',$
          '100','100','100','100','80','60','100','100','140','140']
  self.colhead=[[columns], [widths]]

  ; Top-Level base widget
  base=WIDGET_BASE(TITLE='Spectral Analysis', /COLUMN, /BASE_ALIGN_CENTER, TLB_SIZE_EVENTS=1, UNAME='TLB')
  
  ; Table widget
  table=WIDGET_TABLE(base, ALIGNMENT=1, UVALUE='table event', XSIZE=12, X_SCROLL_SIZE=12,$
                     Y_SCROLL_SIZE=6, /RESIZEABLE_COLUMNS,/ALL_EVENTS)
  
  info=WIDGET_LABEL(base, VALUE='Select row to delete from table', /FRAME, /ALIGN_CENTER)

  ; Row base widget containing buttons
  bar=WIDGET_BASE(base, /ROW, UNAME='rowbar')
  maketext=WIDGET_LABEL(bar, VALUE='Export to:')
  makechoice=['Wiki', 'Confluence', 'Excel']
  droplist=WIDGET_COMBOBOX(bar, VALUE=makechoice, UVALUE='make')
  button2=WIDGET_BUTTON(bar, VALUE='DELETE ROW', UVALUE='del_row')
  button3=WIDGET_BUTTON(bar, VALUE='EDIT COLUMNS', UVALUE='edit_col')
  button4=WIDGET_BUTTON(bar, VALUE='SORT SELECT', UVALUE='sortselect')
  button5=WIDGET_BUTTON(bar, VALUE='CLOSE', UVALUE='close')

  ; Realize widgets
  WIDGET_CONTROL, base, /REALIZE

  ; Get size of base widget
  getsize=WIDGET_INFO(base, /GEOMETRY)
  self.xsize=getsize.scr_xsize+(2*getsize.margin)
  self.ysize=getsize.scr_ysize+(2*getsize.margin)
  
  ; Get size of table widget
  getsize=WIDGET_INFO(table, /GEOMETRY)
  self.cols=getsize.scr_xsize+(2*getsize.margin)
  self.rows=getsize.scr_ysize+(2*getsize.margin)
  
  ; Store IDs in object
  self.baseID=base
  self.tableID=table
  self.rowID=bar
  self.button1ID=droplist
  self.button2ID=button2
  self.button3ID=button3
  self.button4ID=button4
  self.button5ID=button5

  ; Create fit info structure
  struct={tableinfo, det:'',name:'', data_type:'', time:'', fxn:'', amp:'', epeak:'', index:'', alpha:'',$
          beta:'', chi:'', redchi:'', likely:'', cash:'', phflux:'', eflux:'', indexb:'', break1:'', $
          break2:'', index1:'', index2:'', index3:'', electron_e:'', opdepth:'',opdepth2:'',$
          geofac:'',kt:'', cut:'', efold:'',parrat:'',redshift:'',metal:'',mu:'',sigma:'',cent:'',$
          fwhm:'',fslope:'',width:'',ec:'',nhe:'',coldense:'',self:self, rownum:0L, effarea:PTR_NEW()}

  ; Save structures to widget bases
  WIDGET_CONTROL, bar, SET_UVALUE=self
  WIDGET_CONTROL, base, SET_UVALUE=struct

  XMANAGER, OBJ_CLASS(self), self.baseID, /NO_BLOCK

ENDIF

RETURN

END

;------------------------------------------------------------------------------------------------
; Entry For Checkbox Widget Event Handler
PRO CHECKBOX_EVENT, event

; Same as for main event handler, find ID of base widget
; where object definition is stored
baseID=WIDGET_INFO(event.top, FIND_BY_UNAME='BOXBASE')

; Restore object definition
WIDGET_CONTROL, baseID, GET_UVALUE=checks
self=checks.self

; Pass event to event handler inside object
self->CHECKBOX_HANDLER, event

END

;------------------------------------------------------------------------------------------------
; Checkbox Widget Event Handler
PRO SPECTABLE::CHECKBOX_HANDLER, event

; Get event actions       
WIDGET_CONTROL, event.top, GET_UVALUE=checks
WIDGET_CONTROL, event.id, GET_UVALUE=action

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

CASE action OF

  ; Accept selections
  'accept': BEGIN
              ; Save selections
              self.showcols=checks.selection
              
              ; Destroy object
              WIDGET_CONTROL, event.top, /DESTROY
              
              self->COLUMN_SHOW
              RETURN
            END
            
  ; Cancel selections
  'cancel': BEGIN
              ; Restore old selections
              self.showcols=checks.old_select
              
              ; Destroy object
              WIDGET_CONTROL, event.top, /DESTROY
              RETURN
            END
  
  'tabs': RETURN ; Do Nothing
            
  ; Selection event
  ELSE: BEGIN
          ; Find index of selection
          index=WHERE(checks.IDs EQ event.id)
          
          ; Translate index to column selection
          checks.selection[index]=event.select
          WIDGET_CONTROL, event.top, SET_UVALUE=checks
        END

ENDCASE

END

;------------------------------------------------------------------------------------------------
; Build Checkbox Widget
PRO SPECTABLE::CHECKBOX, selected

; Top-Level base and tabs
tlb=WIDGET_BASE(TITLE='Select Columns', /COLUMN, SCR_XSIZE=250, UNAME='BOXBASE')
infotext='Select/Deselect Columns to' + STRING(10B) + 'Show/Hide'
info=WIDGET_LABEL(tlb, VALUE=infotext, /FRAME)
tabs=WIDGET_TAB(tlb, UVALUE='tabs')
tab1=WIDGET_BASE(tabs, TITLE='General', COLUMN=2)
tab2=WIDGET_BASE(tabs, TITLE='GRB Models', COLUMN=2)
tab3=WIDGET_BASE(tabs, TITLE='Thermal Models', COLUMN=2)

; Set columms for tabs
col1=WIDGET_BASE(tab1, /COLUMN, /NONEXCLUSIVE)
col2=WIDGET_BASE(tab1, /COLUMN, /NONEXCLUSIVE)
col3=WIDGET_BASE(tab2, /COLUMN, /NONEXCLUSIVE)
col4=WIDGET_BASE(tab2, /COLUMN, /NONEXCLUSIVE)
col5=WIDGET_BASE(tab3, /COLUMN, /NONEXCLUSIVE)
col6=WIDGET_BASE(tab3, /COLUMN, /NONEXCLUSIVE)

; Checkbox buttons
det_button=WIDGET_BUTTON(col1, VALUE='Detectors', UVALUE='1')
name_button=WIDGET_BUTTON(col1, VALUE='GRB Name', UVALUE='1')
data_button=WIDGET_BUTTON(col1, VALUE='Data', UVALUE='1')
dt_button=WIDGET_BUTTON(col1, VALUE='Dt', UVALUE='1')
model_button=WIDGET_BUTTON(col1, VALUE='Model', UVALUE='1')
amp_button=WIDGET_BUTTON(col1, VALUE='Amplitude', UVALUE='1')
epeak_button=WIDGET_BUTTON(col3, VALUE='Epeak', UVALUE='1')
indexb_button=WIDGET_BUTTON(col3, VALUE='Break E (BPL/SBPL)', UVALUE='1')
ebreak1_button=WIDGET_BUTTON(col3, VALUE='Break E1', UVALUE='1')
ebreak2_button=WIDGET_BUTTON(col3, VALUE='Break E2', UVALUE='1')
alpha_button=WIDGET_BUTTON(col3, VALUE='Alpha', UVALUE='1')
beta_button=WIDGET_BUTTON(col4, VALUE='Beta', UVALUE='1')
index_button=WIDGET_BUTTON(col4, VALUE='Index (PL/Comp)', UVALUE='1')
index1_button=WIDGET_BUTTON(col4, VALUE='Index1', UVALUE='1')
index2_button=WIDGET_BUTTON(col4, VALUE='Index2', UVALUE='1')
index3_button=WIDGET_BUTTON(col4, VALUE='Index3', UVALUE='1')
ee_button=WIDGET_BUTTON(col5, VALUE='Electron E', UVALUE='1')
opdepth_button=WIDGET_BUTTON(col5, VALUE='Op. Depth', UVALUE='1')
opdepth2_button=WIDGET_BUTTON(col5, VALUE='Op. Depth2', UVALUE='1')
geofac_button=WIDGET_BUTTON(col5, VALUE='Geom. Fac.', UVALUE='1')
kt_button=WIDGET_BUTTON(col5, VALUE='kT', UVALUE='1')
cut_button=WIDGET_BUTTON(col5, VALUE='Cutoff E', UVALUE='1')
efold_button=WIDGET_BUTTON(col5, VALUE='E Folding', UVALUE='1')
parrat_button=WIDGET_BUTTON(col5, VALUE='Param. Ratio', UVALUE='1')
redshift_button=WIDGET_BUTTON(col5, VALUE='Cosmol. z', UVALUE='1')
metal_button=WIDGET_BUTTON(col5, VALUE='Metalicity', UVALUE='1')
mu_button=WIDGET_BUTTON(col6, VALUE='Mu', UVALUE='1')
sigma_button=WIDGET_BUTTON(col6, VALUE='Sigma', UVALUE='1')
cent_button=WIDGET_BUTTON(col6, VALUE='Centroid', UVALUE='1')
fwhm_button=WIDGET_BUTTON(col6, VALUE='FWHM', UVALUE='1')
fslope_button=WIDGET_BUTTON(col6, VALUE='FWHM Slope', UVALUE='1')
width_button=WIDGET_BUTTON(col6, VALUE='Line Width', UVALUE='1')
ec_button=WIDGET_BUTTON(col6, VALUE='Electron Cooling', UVALUE='1')
nhe_button=WIDGET_BUTTON(col6, VALUE='nHe/nH', UVALUE='1')
coldense_button=WIDGET_BUTTON(col6, VALUE='H Column Density', UVALUE='1')
chisq_button=WIDGET_BUTTON(col1, VALUE='Chi2/DOF', UVALUE='1')
redchi_button=WIDGET_BUTTON(col2, VALUE='Red. Chi2', UVALUE='1')
likely_button=WIDGET_BUTTON(col2, VALUE='Likely/DOF', UVALUE='1')
cash_button=WIDGET_BUTTON(col2, VALUE='C-Stat/DOF', UVALUE='1')
phflux_button=WIDGET_BUTTON(col2, VALUE='Ph. Flux', UVALUE='1')
enflux_button=WIDGET_BUTTON(col2, VALUE='En. Flux', UVALUE='1')

; Button bar
bar=WIDGET_BASE(tlb, /ROW, /BASE_ALIGN_CENTER)
accept_button=WIDGET_BUTTON(bar, VALUE='Accept', UVALUE='accept')
cancel_button=WIDGET_BUTTON(bar, VALUE='Cancel', UVALUE='cancel')

; Store button IDs
boxes=[det_button,name_button,data_button,dt_button,model_button,amp_button,epeak_button,indexb_button,$
       ebreak1_button,ebreak2_button,alpha_button,beta_button,index_button,index1_button,$
       index2_button,index3_button,ee_button,opdepth_button,opdepth2_button,geofac_button,$
       kt_button,cut_button,efold_button,parrat_button,redshift_button,metal_button,mu_button,$
       sigma_button,cent_button,fwhm_button,fslope_button,width_button,ec_button,nhe_button,$
       coldense_button,chisq_button,redchi_button,likely_button,cash_button,phflux_button,enflux_button]

; If the columns are currently shown in the table, set the checkbox ON
; otherwise the checkbox is OFF
FOR i=0, N_ELEMENTS(boxes)-1 DO BEGIN
  IF FIX(selected[i]) THEN WIDGET_CONTROL, boxes[i], SET_BUTTON=1
ENDFOR     

; Save structure
checks={checks, IDs:boxes, selection:selected, old_select:selected, self:self}

; Realize widget
WIDGET_CONTROL, tlb, /REALIZE

WIDGET_CONTROL, tlb, SET_UVALUE=checks

XMANAGER, 'CHECKBOX', tlb, /NO_BLOCK

END

;------------------------------------------------------------------------------------------------
; Entry For Effective Area Widget Event Handler
PRO EFF_AREA_EVENT, event

; Same as for main event handler, find ID of base widget
; where object definition is stored
effareaID=WIDGET_INFO(event.top, FIND_BY_UNAME='Eff. Area')

; Restore object definition
WIDGET_CONTROL, effareaID, GET_UVALUE=self
;self=checks.self

; Pass event to event handler inside object
self->EFF_AREA_HANDLER, event

END

;------------------------------------------------------------------------------------------------
; Effective Area Table Event Handler
PRO SPECTABLE::EFF_AREA_HANDLER, event

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Event Handler
CASE event.id OF 
  
  ; Close table 
  self.dismiss: self->EFF_AREA_DISMISS
  
  ; Resize table
  self.effareaID: BEGIN
          
          ; Get new base widget size
          x=event.x
          y=event.y
          
          ; Calculate difference in size from old to new
          dx=FLOAT(x)-FLOAT(self.effxsize)
          dy=FLOAT(y)-FLOAT(self.effysize)
          
          ; Change table size by that difference
          WIDGET_CONTROL, self.efftableID, SCR_XSIZE=self.effcols+dx, SCR_YSIZE=self.effrows+dy
          
          ; Store new table size
          self.effcols=self.effcols+dx
          self.effrows=self.effrows+dy
          
          ; Store new base size
          self.effxsize=x
          self.effysize=y
        
        END

ENDCASE

END

;------------------------------------------------------------------------------------------------
; Dismiss Effective Area Table
PRO SPECTABLE::EFF_AREA_DISMISS

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Free effective area pointers
self.eaflag=0
WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
FOR i=0, N_ELEMENTS(spec_arr)-1 DO BEGIN
  IF PTR_VALID(spec_arr[i].effarea) THEN dummy=TEMPORARY(*spec_arr[i].effarea)
  PTR_FREE, spec_arr[i].effarea
ENDFOR

; Destroy widget
WIDGET_CONTROL, self.effareaID, /DESTROY
self.effareaID=0
self.efftableID=0
self.eaflag=0

RETURN

END

;------------------------------------------------------------------------------------------------
; Build Effective Area Table
PRO SPECTABLE::EFF_AREA

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; If table doesn't already exist then create
IF self.effareaID EQ 0L THEN BEGIN

  ; Top-level base widget
  base=WIDGET_BASE(TITLE='Effective Area', /COLUMN, /BASE_ALIGN_CENTER, GROUP_LEADER=self.baseID, $
                   UNAME='Eff. Area', TLB_SIZE_EVENTS=1)
  
  ; Table widget
  table=WIDGET_TABLE(base, ALIGNMENT=1, XSIZE=3, Y_SCROLL_SIZE=6, COLUMN_WIDTHS=100)

  rowbar=WIDGET_BASE(base, /ROW, /BASE_ALIGN_CENTER)
  dismiss=WIDGET_BUTTON(rowbar, VALUE='Dismiss')

  ; Realize widget
  WIDGET_CONTROL, base, /REALIZE
  
  ; Store widget IDs
  self.effareaID=base
  self.efftableID=table
  self.dismiss=dismiss
  
  ; Get size of base widget
  getsize=WIDGET_INFO(base, /GEOMETRY)
  self.effxsize=getsize.scr_xsize+(2*getsize.margin)
  self.effysize=getsize.scr_ysize+(2*getsize.margin)
  
  ; Get size of table widget
  getsize=WIDGET_INFO(table, /GEOMETRY)
  self.effcols=getsize.scr_xsize+(2*getsize.margin)
  self.effrows=getsize.scr_ysize+(2*getsize.margin)
  
  WIDGET_CONTROL, base, SET_UVALUE=self
  XMANAGER, 'EFF_AREA', base, /NO_BLOCK

ENDIF

  ; See if each pointer in the array of structures is valid and count
  WIDGET_CONTROL, self.baseID, GET_UVALUE=spec_arr
  array=LONARR(2, N_ELEMENTS(spec_arr))
  FOR i=0, N_ELEMENTS(spec_arr)-1 DO BEGIN
    array[0,i]=i
    IF PTR_VALID(spec_arr[i].effarea) THEN array[1,i]=1 ELSE array[1,i]=0
  ENDFOR
  
  ; Find number of columns in each valid pointer
  effrows=WHERE(array[1,*] EQ 1, count )
  numeff=count
  IF numeff NE 0 THEN BEGIN
    eff_nums=LONARR(numeff)
    FOR i=0, numeff-1 DO eff_nums[i]=N_ELEMENTS((*spec_arr[effrows[i]].effarea)[0,*])
  
    ; Build array of effective areas corresponding to fits in main table
    oldcols=''
    FOR i=0, numeff-1 DO BEGIN
      columns=STRARR(eff_nums[i])
      numcols=eff_nums[i]
      index=LONARR(numcols)
      FOR j=0, numcols-1 DO columns[j]=(*spec_arr[effrows[i]].effarea)[0,j]
      FOR k=0, numcols-1 DO index[k]=WHERE(STRMATCH(oldcols, columns[k]) EQ 1)
      IF WHERE(index[0] EQ -1) NE -1 THEN columns=[oldcols, columns[WHERE(index EQ -1)]] ELSE $
                                        columns=oldcols
                                        oldcols=columns    
    ENDFOR
    
    ; Remember to fill blank lines, and blank effective area ratios
    columns=columns[1:*]
    numcols=N_ELEMENTS(columns)
    number=LONG(TOTAL(eff_nums)+(N_ELEMENTS(spec_arr)-N_ELEMENTS(eff_nums)))
    eff_entry=STRARR(3, number)
    k=0
    m=0
    FOR i=0, N_ELEMENTS(array[0,*])-1 DO BEGIN
      IF array[1,i] EQ 1 THEN BEGIN
        FOR j=0, eff_nums[m]-1 DO BEGIN
          index=WHERE(columns EQ (*spec_arr[effrows[m]].effarea)[0,j])
          eff_entry[*,k]=[STRCOMPRESS(STRING(i), /REMOVE_ALL), STRCOMPRESS(STRING(index), /REMOVE_ALL), $
                          (*spec_arr[effrows[m]].effarea)[1,j]]
          k=k+1
        ENDFOR
        m=m+1
      ENDIF ELSE BEGIN
        eff_entry[*,k]=[STRCOMPRESS(STRING(i), /REMOVE_ALL), 'ALL', '-']
        k=k+1
      ENDELSE   
    ENDFOR
  
    effarea=STRARR(numcols, MAX(eff_entry[0,*])+1)
  
    ; Fill blank lines
    FOR i=0, number-1 DO BEGIN
      IF eff_entry[1,i] NE 'ALL' THEN BEGIN
        effarea[eff_entry[1,i], eff_entry[0,i]]=eff_entry[2, i]
      ENDIF ELSE effarea[*, eff_entry[0,i]]='-'
    ENDFOR

    ; And here
    numrows=N_ELEMENTS(effarea[0,*])
    FOR i=0, numrows-1 DO BEGIN
      index=WHERE(effarea[*,i] EQ '')
      IF index[0] NE -1 THEN effarea[index, i]='-'
    ENDFOR
    *self.eacols=columns
    
    ; Update effective area table
    WIDGET_CONTROL, self.efftableID, COLUMN_LABELS=columns, COLUMN_WIDTHS=100, TABLE_XSIZE=numcols, $
                    TABLE_YSIZE=numrows, SET_VALUE=effarea
  ENDIF
  
END

;------------------------------------------------------------------------------------------------
; Define SpecTable
PRO SPECTABLE__DEFINE

; Object structure
obj={spectable, baseID:0L, tableID:0L, rowID:0L, button1ID:0L, button2ID:0L, button3ID:0L, button4ID:0L, $
     button5ID:0L, effareaID:0L, efftableID:0L, dismiss:0L, xsize:0L, ysize:0L, rows:0L, cols:0L, $
     effxsize:0L, effysize:0L, effrows:0L, effcols:0L, eaflag:0L, append:0L, colhead:STRARR(41,2), $
     sortselect:{sortselect, table_col:PTR_NEW(), pos:0L, order:0}, newcols:PTR_NEW(), showcols:INTARR(42), $
     eacols:PTR_NEW(), logger:PTR_NEW(), table_row:{table_row, row_info:STRARR(41), rownum:0L}, numparams:0}

RETURN

END

