PRO find_radec, ra, dec, mrun, mcamcol, mfield, $
                runstr=runstr, colstr=colstr, fstr=fstr, $
                silent=silent, first=first, astrans=astrans

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;       FIND_RADEC
;       
; PURPOSE:
;	Find the run, camcol and field that contain the input ra and dec.
;       More than one run may be returned if ra,dec is in overlap region
;       between the runs.
; 
; CALLING SEQUENCE:
;       find_radec, ra, dec [, run, camcol, field, 
;                   runstr=runstr, colstr=colstr, fstr=fstr, 
;                   /first, /silent, /astrans]
;                 
; INPUTS: 
;       ra, dec: The positions in degrees.
;
; KEYWORD PARAMETERS:
;       /first: just find the first run that matches
;       /silent: don't print run, camcol, field
;       /astrans: call find_radec_astrans to find run. Slower but more accurate
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
;       MATCH
;       MRDFITS
;       DELVARX
;       ADD_ARRVAL
;       NTOSTR
;
;
; PROCEDURE: 
;	Must have !RADEC_SEARCH_DIR and !SEARCH_RUNS, and !RUN_STATUS defined in
;       SDSSIDL_SETUP.PRO  The directory contains the files created
;       by FIELD_RANGE.PRO, which finds the lambda-eta range of each field.
;       !SEARCH_RUNS is all the runs which have been run through FIELD_RANGE.
;       These files contain structures for each field with the following
;       tags: field, etamin, etamax, lambdamin, lambdamax. Assumes that
;       fields are approximately square in lambda-eta.
;       !RUN_STATUS has all the runs we know about on the current machine.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofMich 10/15/99 
;       Converged to survey coords. 9-OCT-2000 E.S.S.
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: find_radec, ra, dec, run, camcol, field, runstr=runstr, colstr=colstr, fstr=fstr, /silent, /astrans'
     print,''
     print,'Use doc_library,"find_radec"  for more help.'  
     return
  ENDIF 

  ;; use astrans method?

  IF keyword_set(astrans) THEN BEGIN 
      find_radec_astrans, ra, dec, mrun, mcamcol, mfield, $
                          runstr=runstr, colstr=colstr, fstr=fstr, $
                          silent=silent, first=first
      return
  ENDIF 

  IF NOT keyword_set(silent) THEN silent=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These will be system dependent variables.
  ;; The search directory and the searchable runs.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, mrun, mcamcol, mfield, runstr, colstr, fstr

  sdssidl_setup, /silent
  IF NOT !sdssidl_def.radec_search_dir_defined THEN BEGIN 
      message,'!RADEC_SEARCH_DIR must be defined'
  ENDIF 
  IF NOT !sdssidl_def.search_runs_defined THEN BEGIN 
      message,'!SEARCH_RUNS array must be defined'
  ENDIF 
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
  rs.tsobj_exist = 'Y'
  
  runstatus_select, rs, wst1
  IF wst1[0] EQ -1 THEN BEGIN 
      message,'No good runs found!!'
  ENDIF 

  FOR i=0L, n_search_stripe-1 DO BEGIN 

      wst = where( !RUN_STATUS[wst1].stripe EQ stripes[i], nmatch)
      IF nmatch NE 0 THEN add_arrval, !RUN_STATUS[wst1[wst]].run, search_runs

  ENDFOR 

  nfound = n_elements(search_runs)

  IF nfound EQ 0 THEN BEGIN 
      print,'No good stripes found for this ra and dec'
      mrun=-1
      mcamcol=-1
      mfield=-1
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now make sure we have processed the runs in search_runs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  match, !SEARCH_RUNS, search_runs, mproc, msearch, /sort
  IF mproc[0] EQ -1 THEN BEGIN 
      print,'None of the runs in the stripes have been processed by field_range.pro'
      mrun=-1
      mcamcol=-1
      mfield=-1
      return
  ENDIF 
  search_runs = search_runs[msearch]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Search
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  nrun = n_elements(search_runs)
  colmin=1
  colmax=6

  FOR irun = 0L, nrun-1 DO BEGIN 
      trunstr = run2string(search_runs[irun])
      FOR camcol = colmin, colmax DO BEGIN 
          tcolstr = ntostr(camcol)

          file = !RADEC_SEARCH_DIR + 'lameta-range-' + $
                              trunstr + '-' + $
                              tcolstr + '.fit'
          
          openr, lun, file, /get_lun
          IF (irun EQ 0) AND (camcol EQ colmin) THEN BEGIN 
              t=mrdfits3(lun, 1, 0, /silent)
          ENDIF ELSE BEGIN
              t=mrdfits3(lun, 1, 0, /silent, /deja_vu)
          ENDELSE 
          free_lun, lun

          nw=0
          IF NOT keyword_set(southern) THEN BEGIN 
              w=where(eta LE t.etamax AND eta GE t.etamin AND $
                      lambda LE t.lambdamax and lambda GE t.lambdamin, nw)
          ENDIF ELSE BEGIN 
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; Southern Stripe
              ;; check for region where crosses lambda = -180, lambda=180
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
              w2=lindgen(n_elements(t))
              w1=where( (t.lambdamax GE 175) AND $
                        (t.lambdamin LE -175), n1) ;field crosses
                              
              IF n1 NE 0 THEN BEGIN
                  ;; must search that field separately
                  IF (eta LE t[w1].etamax) AND (eta GE t[w1].etamin) AND $
                     ( (lambda LE t[w1].lambdamin) OR (lambda GE t[w1].lambdamax) ) THEN BEGIN
                      w=w1
                      nw=1
                      delvarx, w1
                  ENDIF 
              ENDIF 
              IF (nw EQ 0) THEN BEGIN
                  IF n1 NE 0 THEN remove, w1, w2 ;regular fields
                  ;; search these fields normally
                  w=where(eta LE t[w2].etamax AND eta GE t[w2].etamin AND $
                          lambda LE t[w2].lambdamax and lambda GE t[w2].lambdamin, nw)
                  IF nw NE 0 THEN w=w2[w]
                  delvarx, w2
              ENDIF 

          ENDELSE 
          IF nw NE 0 THEN BEGIN 

              tfstr = field2string(t[w[0]].field)
              add_arrval, search_runs[irun], mrun
              add_arrval, t[w[0]].field, mfield
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
      ENDFOR 
  ENDFOR           

  IF n_elements(mrun) EQ 0 THEN BEGIN
      print,'Coordinates Not Found'
      mrun=-1
      mcamcol=-1
      mfield=-1
  ENDIF 
  return 
END 
