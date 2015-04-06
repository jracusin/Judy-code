PRO find_radec_astrans, ra, dec, mrun, mcamcol, mfield, $
                        runstr=runstr, colstr=colstr, fstr=fstr, $
                        silent=silent, first=first

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;       FIND_RADEC_ASTRANS
;       
; PURPOSE:
;	Find the run, camcol and field that contain the input ra and dec.
;       More than one run may be returned if ra,dec is in overlap region
;       between the runs. Uses astrans file; more accurate than find_radec
;       without /astrans keyword, but slower
; 
; CALLING SEQUENCE:
;       find_radec_astrans, ra, dec [, run, camcol, field, 
;                   runstr=runstr, colstr=colstr, fstr=fstr, 
;                   /first, /silent]
;                 
; INPUTS: 
;       ra, dec: The positions in degrees.
;
; KEYWORD PARAMETERS:
;       /first: just find the first run that matches
;       /silent: don't print run, camcol, field
;       
; OUTPUTS: 
;       run, camcol, field
;
; OPTIONAL OUTPUTS:
;       runstr=runstr: run string with standard padded zeros
;       colstr=colstr: column string
;       fstr=fstr: field string with standard padded zeros.
;       
;
; CALLED ROUTINES:
; 
;       EQ2SURVEY
;       ETA2STRIPENUM
;       (SDSSIDL_SETUP)
;       READ_ASTRANS
;
;
; PROCEDURE: 
;
; REVISION HISTORY:
;	Created: 15-AUG-2002 Erin Sheldon UofMich
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: find_radec_astrans, ra, dec, run, camcol, field, runstr=runstr, colstr=colstr, fstr=fstr, /first, /silent'
     print,''
     print,'Use doc_library,"find_radec"  for more help.'  
     return
  ENDIF 

  IF NOT keyword_set(silent) THEN silent=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These will be system dependent variables.
  ;; The search directory and the searchable runs.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, mrun, mcamcol, mfield, runstr, colstr, fstr

  sdssidl_setup, /silent
  IF NOT !sdssidl_def.run_status_defined THEN BEGIN 
      message,'!RUN_STATUS structure must be defined'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; convert to survey coords
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  eq2survey, ra, dec, lambda, eta
  IF (lambda GT 90) OR (lambda LT -90) THEN southern=1 ELSE southern=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; see which stripe this is. May give one of the surrounding 
  ;; stripes if near edge in outer column, so
  ;; search this stripe and surrounding stripes (if possible)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  maxstripe = 45
  minstripe = 1
  stripe = eta2stripenum(eta, southern=southern, /silent)
  IF NOT keyword_set(southern) THEN stripe = stripe < maxstripe > minstripe

  IF stripe NE maxstripe THEN stripes = [stripe, stripe+1] ELSE stripes = stripe
  IF stripe NE minstripe THEN stripes = [stripe-1, stripes]
  n_search_stripe = n_elements(stripes)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; See which stripes we know about
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  make_runstatus_struct, rs
  rs.astrans_exist = 'Y'
  
  runstatus_select, rs, wst1
  IF wst1[0] EQ -1 THEN BEGIN 
      message,'No runs with asTrans found!!'
  ENDIF 

  make_runstatus_struct,fs
  fs.ASTRANS_EXIST = 'Y'

  FOR i=0L, n_search_stripe-1 DO BEGIN 

      ;; any runs in this stripe?
      wst = where( !RUN_STATUS[wst1].stripe EQ stripes[i], nmatch)
      IF nmatch NE 0 THEN BEGIN
          ind = wst1[wst]

          struns = !RUN_STATUS[ind].run
          streruns = !RUN_STATUS[ind].rerun

          ;; get unique runs
          rmd = rem_dup(struns)
          nuniq = n_elements(rmd)

          ;; find max rerun and use it
          FOR j=0L, nuniq-1 DO BEGIN 
              w=where( !RUN_STATUS.run EQ struns[rmd[j]] )

              runstatus_select, fs, wgood, input_index=w
              IF wgood[0] NE -1 THEN BEGIN 
                  maxrerun = max( long(!RUN_STATUS[wgood].rerun) )
                  trun = struns[rmd[j]]
                  add_arrval, trun, search_runs
                  add_arrval, maxrerun, search_reruns
              ENDIF 
          ENDFOR 

      ENDIF 

  ENDFOR 

  nfound = n_elements(search_runs)
  IF nfound EQ 0 THEN BEGIN 
      print,'No good stripes found for this ra and dec'
      mrun=-1
      mcamcol=-1
      mfield=-1
      return
  ENDIF 

  nfound = n_elements(search_runs)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Search
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  nrun = n_elements(search_runs)
  colmin=1
  colmax=6

  mincol = 0.0
  maxcol = 2048.

  FOR irun = 0L, nrun-1 DO BEGIN 
      trunstr = run2string(search_runs[irun])
      FOR camcol = colmin, colmax DO BEGIN 
          tcolstr = ntostr(camcol)

          ;; search in r-band
          clr = 2
          read_astrans, search_runs[irun], search_reruns[irun], camcol, clr, $
                        trans, node, inc, /silent
          
          nfields = n_elements(trans)
          fieldmin = trans[0].field
          fieldmax = trans[nfields-1].field

          FOR fi=0L, nfields-1 DO BEGIN 
              field = trans[fi].field
              radec2rowcol, trans, node, inc, field, ra, dec, row, col

              minrow = (field GT fieldmin)*64 ;don't include overlap region except
              maxrow = 1489 - (field LT fieldmax)*64 ;first and last frames

              ;; is it in this field?
              IF ( (row LT maxrow) AND (row GT minrow) AND $
                   (col LT maxcol) AND (col GT mincol) ) THEN BEGIN 

                  tfstr = field2string(field)
                  add_arrval, search_runs[irun], mrun
                  add_arrval, field, mfield
                  add_arrval, camcol, mcamcol
                  add_arrval, trunstr, runstr
                  add_arrval, tcolstr, colstr
                  add_arrval, tfstr, fstr
                  
                  IF NOT silent THEN BEGIN 
                      
                      print,'Run: ',trunstr,$
                            ' Camcol: ',tcolstr,$
                            ' Field: ',tfstr
                  ENDIF 
                  IF keyword_set(first) THEN return

              ENDIF 

          ENDFOR ;; fields

      ENDFOR ;; camcols
  ENDFOR ;; runs

  IF n_elements(mrun) EQ 0 THEN BEGIN
      print,'Coordinates Not Found'
      mrun=-1
      mcamcol=-1
      mfield=-1
  ENDIF 
  return 
END 
