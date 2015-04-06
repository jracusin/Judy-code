;+
; NAME:
;  READ_SPDIAG1D
;
;
; PURPOSE:
;  read in the spDiag file for a given plateid (mjd)
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, /silent
;
;
; INPUTS:
;  plateID: plate number in integer form.
;
;
; OPTIONAL INPUTS:
;  mjd: modified julian date
;  spec_vers: version of the spectra.  Default is to use !SDSS_SPEC_VERS
;
; KEYWORD PARAMETERS:
;  /silent: do not print out informative messages.
;
;
; OUTPUTS:
;  spDiag:  the contents of the spDiag file.
;
;
; RESTRICTIONS:
;  The user needs yanny_read.pro and the spectra data on disk.  !SDSS_SPEC_VERS
;  and !SDSS_SPEC_DIR must be defined (sdssidl_setup.pro and config files).
;
;
; PROCEDURE:
;  Use yanny_read to read the contents of the spDiag file.
;
;
; EXAMPLE:
;  read_spDiag1D, 1026, spDiag
;
;
; MODIFICATION HISTORY:
;  Creation: ??-??-2003.  Erin Sheldon UofChicago
;
;-


PRO read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, silent=silent

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: read_spdiag1d, plateID, spdiag, mjd=mjd, spec_vers=spec_vers, silent=silent'
      return
  ENDIF 

  delvarx, spdiag

  pstr = strn(plateID,length=4,padch='0')
  IF n_elements(spec_vers) EQ 0 THEN spec_vers = !SDSS_SPEC_VERS
  dir=!SDSS_SPEC_DIR+spec_vers+'/'+pstr+'/1d/'

  IF n_elements(mjd) EQ 0 THEN BEGIN  
      files = findfile(dir + 'spDiag1d-?????-'+pstr+'.par',$
                       count=count)

      IF count EQ 0 THEN BEGIN 
          IF NOT keyword_set(silent) THEN BEGIN
              print,'plate: '+pstr+' fiber: '+fstr+' does not exist'
          ENDIF 
          delvarx,spdiag
          return
      ENDIF 
      
      ;; get the latest
      file = files[count-1]
      
      tmp = str_sep(file, '-')
      mjdstr = tmp[1]
      mjd = long(mjdstr)

  ENDIF ELSE BEGIN 
      mjdstr = strn(mjd, length=5, padch='0')
      file = dir + 'spDiag1d-'+mjdstr+'-'+pstr+'.par'
  ENDELSE 

  yanny_read, file, pdata
  spdiag = *pdata
  ptr_free, pdata

END 
