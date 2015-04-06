;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    SDSSIDL_SETUP
;       
; PURPOSE:
;    Define system variables used by sdssidl procedures.
;
; CALLING SEQUENCE:
;    sdssidl_setup
;
; INPUTS: 
;    None.
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    /silent: If set then only print error messages.
;       
; OUTPUTS: 
;    None.
;
; OPTIONAL OUTPUTS:
;    None.
;
; CALLED ROUTINES:
;    DEFSYSV
; 
; PROCEDURE: 
;    Defines !SDSSIDL_DEF, which has a tag for each sdssidl system
;        variable. Tag will be 1 if defined or 0 if not defined.
;
;    System variables looked for in the config file (which must be set in the
;    shell variable SDSSIDL_CONFIG)
;         !SDSS_DATA_DIR: base directory for sloan data archives
;         !SDSS_SHAPECORR_DIR: base directory for shape corrected data archives
;         !SDSS_SPEC_DIR: base directory for spectro 1d outputs
;         !SDSS_VAGC_DIR: directory for blantons VAGC catalogs
;         !RADEC_SEARCH_DIR: base directory for Ra-Dec search files
;         !PHOTOZ_DIR: base directory for photoz files
;         !RUN_STATUS_FILE: file containing run status info
;         !GET_ATLAS_SOFILE: .so file for compiled atlas reader (called by
;                            GET_ATLAS.PRO)
;         !GET_ATLAS_ENTRY: the entry in the .so file
;         !GET_ATLAS53_SOFILE: same, but for photo 5.3 outputs
;         !GET_ATLAS53_ENTRY: same, but for photo 5.3 outputs
;         !htmLookupRADEC_SOFILE: .so file for htmlookupradec
;         !htmLookupRADEC_ENTRY: entry
;         !htmIntersectRADEC_SOFILE: .so file for intersect code
;         !htmIntersectRADEC_ENTRY: entry
;         !PIXEL_MASK_FILE_BOUND: the pixel bound mask
;         !PIXEL_MASK_FILE_COMBINED: pixel combined mask
;         !PIXEL_MASK_SOFILE: .so file for pixel mask code
;         !PIXEL_MASK_ENTRY: entry
;         !PIXEL_AREA_SOFILE: .so file for area code
;         !PIXEL_AREA_ENTRY: entry
;         !sphPoly_MASK_FILE: sphpoly mask
;         !sphPoly_MASK_SOFILE: .so file for sphpoly mask code
;         !sphPoly_MASK_ENTRY: entry
;   
;    Other System Variables Set:
;         !RUN_STATUS: Structure containing info on each run, read from the
;                      !RUN_STATUS_FILE 
;         !SEARCH_RUNS: Those on which FIELD_RANGE.PRO has been run, for ra/dec
;                       searching 
;         !HOSTNAME: the host name of the computer
;
; REVISION HISTORY:
;    Author: Erin Scott Sheldon  UofMich 11/18/99  Always changing.
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO sdssidl_setup, silent=silent

  On_error, 2

  IF NOT keyword_set(silent) THEN silent = 0

  defsysv, '!SDSSIDL_DEF', exist=defexist
  IF NOT defexist THEN BEGIN 
      sdssidl_def=create_struct('config_file_read',0b,$
                                'hostname_defined', 0b, $
                                'get_atlas_sofile_defined', 0b, $
                                'get_atlas_entry_defined', 0b, $
                                'get_atlas53_sofile_defined', 0b, $
                                'get_atlas53_entry_defined', 0b, $
                                'sdss_data_dir_defined', 0b, $
                                'sdss_shapecorr_dir_defined', 0b, $
                                'sdss_spec_dir_defined', 0b, $
                                'sdss_spec_vers_defined', 0b, $
                                'sdss_vagc_dir_defined', 0b, $
                                'sdss_vagc_vers_defined', 0b,$
                                'sdss_lss_dir_defined',0b,$
                                'sdss_lss_vers_defined',0b,$
                                'radec_search_dir_defined', 0b, $
                                'photoz_dir_defined', 0b, $
                                'search_runs_defined', 0b, $
                                'run_status_file_defined', 0b, $
                                'run_status_defined', 0b,$
                                'htmLookupRADEC_sofile_defined', 0b, $
                                'htmLookupRADEC_entry_defined', 0b, $
                                'htmIntersectRADEC_sofile_defined', 0b, $
                                'htmIntersectRADEC_entry_defined', 0b, $
                                'pixel_mask_file_bound_defined', 0b, $
                                'pixel_mask_file_combined_defined', 0b, $
                                'pixel_mask_sofile_defined', 0b, $
                                'pixel_mask_entry_defined', 0b, $
                                'pixel_area_sofile_defined', 0b, $
                                'pixel_area_entry_defined', 0b, $
                                'sphPoly_mask_file_defined', 0b, $
                                'sphPoly_mask_sofile_defined', 0b, $
                                'sphPoly_mask_entry_defined', 0b)

      defsysv, '!SDSSIDL_DEF', sdssidl_def
      IF NOT silent THEN message,'System structure !SDSSIDL_DEF defined.',/inf
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read config file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT !SDSSIDL_DEF.CONFIG_FILE_READ THEN BEGIN 
      spawn, 'echo $SDSSIDL_CONFIG', config_file
      IF config_file[0] EQ '' THEN BEGIN 
          message, "$SDSSIDL_CONFIG undefined"
      ENDIF ELSE BEGIN 
          config_file=config_file[0]
          print,'Reading config file: ',config_file
          parse_config, config_file, keywords, values
          !sdssidl_def.config_file_read = 1
      ENDELSE 
  ENDIF ELSE keywords=''

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define system dependent variables here if relevant. Leave undefined
  ;; if they aren't; also, don't redefine once defined
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT !SDSSIDL_DEF.HOSTNAME_DEFINED THEN BEGIN 
      spawn,'hostname',answer,/noshell
      hostname = str_sep(answer[0], '.')
      hostname=hostname[0]
      defsysv,'!HOSTNAME',hostname
      !sdssidl_def.hostname_defined = 1
  ENDIF 

  IF NOT !SDSSIDL_DEF.GET_ATLAS_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'get_atlas_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          get_atlas_sofile = values[w[0]]
          defsysv, '!GET_ATLAS_SOFILE', get_atlas_sofile
          !sdssidl_def.get_atlas_sofile_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.GET_ATLAS_ENTRY_DEFINED THEN BEGIN
      w=where(strlowcase(keywords) EQ 'get_atlas_entry',nw)
      IF nw NE 0 THEN BEGIN 
          get_atlas_entry = values[w[0]]
          defsysv, '!GET_ATLAS_ENTRY',get_atlas_entry
          !sdssidl_def.get_atlas_entry_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.GET_ATLAS53_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'get_atlas53_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          get_atlas53_sofile = values[w[0]]
          defsysv, '!GET_ATLAS53_SOFILE', get_atlas53_sofile
          !sdssidl_def.get_atlas53_sofile_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.GET_ATLAS53_ENTRY_DEFINED THEN BEGIN
      w=where(strlowcase(keywords) EQ 'get_atlas53_entry',nw)
      IF nw NE 0 THEN BEGIN 
          get_atlas53_entry = values[w[0]]
          defsysv, '!GET_ATLAS53_ENTRY',get_atlas53_entry
          !sdssidl_def.get_atlas53_entry_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.SDSS_DATA_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_data_dir',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_data_dir = values[w[0]]
          defsysv, '!SDSS_DATA_DIR', sdss_data_dir
          !sdssidl_def.sdss_data_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.SDSS_SHAPECORR_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_shapecorr_dir',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_shapecorr_dir = values[w[0]]
          defsysv, '!SDSS_SHAPECORR_DIR',sdss_shapecorr_dir
          !sdssidl_def.sdss_shapecorr_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.SDSS_SPEC_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_spec_dir',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_spec_dir = values[w[0]]
          defsysv, '!SDSS_SPEC_DIR',sdss_spec_dir
          !sdssidl_def.sdss_spec_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.SDSS_SPEC_VERS_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_spec_vers',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_spec_vers = values[w[0]]
          defsysv, '!SDSS_SPEC_VERS',sdss_spec_vers
          !sdssidl_def.sdss_spec_vers_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.SDSS_VAGC_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_vagc_dir',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_vagc_dir = values[w[0]]
          defsysv, '!SDSS_VAGC_DIR',sdss_vagc_dir
          !sdssidl_def.sdss_vagc_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.SDSS_VAGC_VERS_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_vagc_vers',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_vagc_vers = values[w[0]]
          defsysv, '!SDSS_VAGC_VERS',sdss_vagc_vers
          !sdssidl_def.sdss_vagc_vers_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.SDSS_LSS_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_lss_dir',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_lss_dir = values[w[0]]
          defsysv, '!SDSS_LSS_DIR',sdss_lss_dir
          !sdssidl_def.sdss_lss_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.SDSS_LSS_VERS_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sdss_lss_vers',nw)
      IF nw NE 0 THEN BEGIN 
          sdss_lss_vers = values[w[0]]
          defsysv, '!SDSS_LSS_VERS',sdss_lss_vers
          !sdssidl_def.sdss_lss_vers_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.RADEC_SEARCH_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'radec_search_dir',nw)
      IF nw NE 0 THEN BEGIN 
          radec_search_dir = values[w[0]]
          defsysv, '!RADEC_SEARCH_DIR', radec_search_dir
          !sdssidl_def.radec_search_dir_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PHOTOZ_DIR_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'photoz_dir',nw)
      IF nw NE 0 THEN BEGIN 
          photoz_dir = values[w[0]]
          defsysv, '!PHOTOZ_DIR', photoz_dir
          !sdssidl_def.photoz_dir_defined = 1
      ENDIF 
  ENDIF 

  ;; define the run_status_file name here. You need to run the procedure
  ;; good_reruns.pro in order to make the run_status_file

  IF NOT !SDSSIDL_DEF.RUN_STATUS_FILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'run_status_file',nw)
      IF nw NE 0 THEN BEGIN 
          run_status_file = values[w[0]]
          defsysv, '!RUN_STATUS_FILE', run_status_file
          !sdssidl_def.run_status_file_defined = 1
      ENDIF 
  ENDIF 

  ;; .so files and entries for htm searching
  IF NOT !SDSSIDL_DEF.htmLookupRADEC_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'htmlookupradec_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          htmlookupradec_sofile = values[w[0]]
          defsysv, '!htmLookupRADEC_SOFILE', htmlookupradec_sofile
          !sdssidl_def.htmlookupradec_sofile_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.htmLookupRADEC_ENTRY_DEFINED THEN BEGIN
      w=where(strlowcase(keywords) EQ 'htmlookupradec_entry',nw)
      IF nw NE 0 THEN BEGIN 
          htmlookupradec_entry = values[w[0]]
          defsysv, '!htmLookupRADEC_ENTRY',htmlookupradec_entry
          !sdssidl_def.htmlookupradec_entry_defined = 1
      ENDIF 
  ENDIF 

  IF NOT !SDSSIDL_DEF.htmIntersectRADEC_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'htmintersectradec_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          htmintersectradec_sofile = values[w[0]]
          defsysv, '!htmIntersectRADEC_SOFILE', htmintersectradec_sofile
          !sdssidl_def.htmintersectradec_sofile_defined = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.htmIntersectRADEC_ENTRY_DEFINED THEN BEGIN
      w=where(strlowcase(keywords) EQ 'htmintersectradec_entry',nw)
      IF nw NE 0 THEN BEGIN 
          htmintersectradec_entry = values[w[0]]
          defsysv, '!htmIntersectRADEC_ENTRY',htmintersectradec_entry
          !sdssidl_def.htmintersectradec_entry_defined = 1
      ENDIF 
  ENDIF 

  ;; pixel masks (for imaging data)
  IF NOT !SDSSIDL_DEF.PIXEL_MASK_FILE_BOUND_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_mask_file_bound',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_mask_file_bound = values[w[0]]
          defsysv, '!PIXEL_MASK_FILE_BOUND', pixel_mask_file_bound
          !SDSSIDL_DEF.PIXEL_MASK_FILE_BOUND_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PIXEL_MASK_FILE_COMBINED_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_mask_file_combined',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_mask_file_combined = values[w[0]]
          defsysv, '!PIXEL_MASK_FILE_COMBINED', pixel_mask_file_combined
          !SDSSIDL_DEF.PIXEL_MASK_FILE_COMBINED_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PIXEL_MASK_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_mask_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_mask_sofile = values[w[0]]
          defsysv, '!PIXEL_MASK_SOFILE', pixel_mask_sofile
          !SDSSIDL_DEF.PIXEL_MASK_SOFILE_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PIXEL_MASK_ENTRY_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_mask_entry',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_mask_entry = values[w[0]]
          defsysv, '!PIXEL_MASK_ENTRY', pixel_mask_entry
          !SDSSIDL_DEF.PIXEL_MASK_ENTRY_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PIXEL_AREA_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_area_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_area_sofile = values[w[0]]
          defsysv, '!PIXEL_AREA_SOFILE', pixel_area_sofile
          !SDSSIDL_DEF.PIXEL_AREA_SOFILE_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.PIXEL_AREA_ENTRY_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'pixel_area_entry',nw)
      IF nw NE 0 THEN BEGIN 
          pixel_area_entry = values[w[0]]
          defsysv, '!PIXEL_AREA_ENTRY', pixel_area_entry
          !SDSSIDL_DEF.PIXEL_AREA_ENTRY_DEFINED = 1
      ENDIF 
  ENDIF 

  ;; spherical polygon masks for spectra
  IF NOT !SDSSIDL_DEF.sphPoly_MASK_FILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sphpoly_mask_file',nw)
      IF nw NE 0 THEN BEGIN 
          sphpoly_mask_file = values[w[0]]
          defsysv, '!sphPoly_MASK_FILE', sphpoly_mask_file
          !SDSSIDL_DEF.sphPoly_MASK_FILE_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.sphPoly_MASK_SOFILE_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sphpoly_mask_sofile',nw)
      IF nw NE 0 THEN BEGIN 
          sphpoly_mask_sofile = values[w[0]]
          defsysv, '!sphPoly_MASK_SOFILE', sphpoly_mask_sofile
          !SDSSIDL_DEF.sphPoly_MASK_SOFILE_DEFINED = 1
      ENDIF 
  ENDIF 
  IF NOT !SDSSIDL_DEF.sphPoly_MASK_ENTRY_DEFINED THEN BEGIN 
      w=where(strlowcase(keywords) EQ 'sphpoly_mask_entry',nw)
      IF nw NE 0 THEN BEGIN 
          sphpoly_mask_entry = values[w[0]]
          defsysv, '!sphPoly_MASK_ENTRY', sphpoly_mask_entry
          !SDSSIDL_DEF.sphPoly_MASK_ENTRY_DEFINED = 1
      ENDIF 
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get search runs only once. Can't redefine system arrays.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF !sdssidl_def.radec_search_dir_defined THEN BEGIN ;directory must be defined
      IF NOT !sdssidl_def.search_runs_defined THEN BEGIN ;must be undefined
          files=findfile(!RADEC_SEARCH_DIR)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; get runs from file names
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          IF files[0] NE '' THEN BEGIN 
              nf = n_elements(files)
              search_runs=replicate(-1,nf)
              FOR i=0L, nf-1 DO BEGIN 
                  tmp=str_sep(files[i], '-')
                  IF n_elements(tmp) EQ 4 THEN search_runs[i] = long( tmp[2] )
              ENDFOR 
              wgood=where(search_runs NE -1, ngood)
              IF ngood NE 0 THEN BEGIN 
                  search_runs = search_runs[wgood]
                  search_runs = search_runs[rem_dup(search_runs)]
                  
                  defsysv, '!SEARCH_RUNS', search_runs
                  IF NOT silent THEN message,'!SEARCH_RUNS defined.',/inf
                  !sdssidl_def.search_runs_defined = 1
              ENDIF ELSE BEGIN
                  IF NOT silent THEN message,'No search runs found',/inf
              ENDELSE 
          ENDIF ELSE BEGIN
              IF NOT silent THEN message,'No search runs found',/inf
          ENDELSE 
      ENDIF 
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Define run status only once. Can't redefine system structs.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT !sdssidl_def.run_status_defined THEN BEGIN ;must be undefined
      IF !sdssidl_def.run_status_file_defined THEN BEGIN ;file must be defined
          IF fexist(!run_status_file) THEN BEGIN ;make sure file exists
              run_status = mrdfits(!run_status_file,1,status=status,/silent,$
                                  STRUCTYP='run_status')
              IF status LT 0 THEN BEGIN 
                  message,'Error reading SDSS run status file '+$
                          !run_status_file,/inf
              ENDIF ELSE BEGIN 
                  defsysv, '!RUN_STATUS', run_status
                  IF NOT silent THEN message,$
                    'System struct !RUN_STATUS has been defined.',/inf
                  !sdssidl_def.run_status_defined = 1
              ENDELSE 
          ENDIF ELSE BEGIN 
              IF NOT silent THEN message,'SDSS run status file '+$
                !run_status_file+' not found. Use procedure '+$
                'GOOD_RERUNS.PRO to make the run status file',/inf
          ENDELSE 
      ENDIF 
  ENDIF 

  IF NOT silent THEN message, 'SDSSIDL system variables have been added. '+$
    'Check !SDSSIDL_DEF structure to see what variable are defined.',/inf

  return 
END 
