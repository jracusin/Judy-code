;+
; NAME:
;  PARSE_CONFIG
;
;
; PURPOSE:
;  Parse simple 2 column "option value" config file. Each is read in as a 
;    string
;
; CALLING SEQUENCE:
;  parse_config, config_file, keywords, values
;
;
; INPUTS:
;  config_file: full path to file
;
; OUTPUTS:
;  keywords: the names of the config variables
;  values: values of config variables
;
; MODIFICATION HISTORY:
;  ??-??-2002  Erin Sheldon UofMichigan
;  02-Apr-2004: Allow comments, empty lines
;-

PRO parse_config, config_file, keywords, values

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: parse_config, config_file, keywords, values'
      return
  ENDIF 

  on_error, 2

  openr, lun, config_file, /get_lun

  nlines=numlines(config_file)

  line=' '
  delvarx, keywords
  delvarx, values
  FOR i=0L, nlines-1 DO BEGIN 
      readf, lun, line

      ;; First, only look for stuff before any comment mark #
      cp = strpos(line,'#')
      IF cp NE -1 THEN BEGIN 
          line = strmid(line,0,cp)
      ENDIF 

      ;; Remove leading and trailing white space
      line=strtrim(line)

      IF (line NE '') THEN BEGIN 

          ;; strsplit defaults to splitting on white space
          tmp = strsplit(line, /extract)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; After leading/trailing whitespace and comments
          ;; are removed, should only result in a key-value pair
          ;; else ignore
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          IF n_elements(tmp) EQ 2 THEN BEGIN 
              add_arrval, tmp[0], keywords
              add_arrval, tmp[1], values
          ENDIF 

      ENDIF 
  ENDFOR 

  free_lun, lun

END 
