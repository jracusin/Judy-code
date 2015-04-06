;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GOOD_RERUNS
;       
; PURPOSE:
;    Find files and other info for the runs in the !SDSS_DATA_DIR and !SDSS_SHAPECORR_DIR
;
; CALLING SEQUENCE:
;    good_reruns [, /nocheckmask]
;
; INPUTS: 
;    NONE: gets its info from system variables defined in SDSSIDL_SETUP
;
; OPTIONAL INPUTS:
;    NONE:
;
;       
; OUTPUTS: 
;    Outputs !RUN_STATUS_FILE, a fits file containing info about
;    known runs.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    SDSSIDL_SETUP
;    ADD_ARRVAL
;    FETCH_RERUN
;    FETCH_DIR
;    FETCH_FILE_LIST
;    MWRFITS
;
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    14-NOV-2000 Erin Scott Sheldon UofMich
;    18-Nov-2002: Added bad2 flag checking: tsField files
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO good_reruns_getreruns, dir, runs, reruns

  delvarx, runs, reruns
  print
  print,'Finding Run Directories in: '+dir
  spawn, 'ls '+dir,answer
  IF answer[0] EQ '' THEN BEGIN
      print,'Nothing Found!'
      return
  ENDIF 
  
  ;; in case we are looking in corrected dir, remove leading "corr"
  nans=n_elements(answer)
  FOR i=0L, nans-1 DO BEGIN
      ;;tmp = (strsplit(answer[i], 'corr', /extract))[0]
      tmp = str_sep(answer[i],'corr')
      IF n_elements(tmp) EQ 1 THEN tmp = tmp[0] ELSE tmp = tmp[1]
      IF strnumber(tmp) THEN BEGIN
          add_arrval,long(tmp),truns
          add_arrval,answer[i], tdir
      ENDIF 
  ENDFOR 

  nrun = n_elements(truns)
  print
  print,'Found '+ntostr(nrun)+' Runs'
  ;; find reruns

  print
  print,'Finding Rerun Directories'
  FOR i=0L, nrun-1 DO BEGIN 
      tmprun = truns[i]
      runstr = ntostr(tmprun)
      ;;print,'Run = '+runstr

      tmpdir = dir+'/'+tdir[i]
      spawn, 'ls '+tmpdir,answer

      IF answer[0] NE '' THEN BEGIN 
          nans = n_elements(answer)
          FOR j=0L, nans-1 DO BEGIN 
              IF strnumber(answer[j]) THEN BEGIN
                  ;;print,'  Rerun = '+answer[j]
                  add_arrval, tmprun, runs
                  add_arrval, fix(answer[j]), reruns
              ENDIF 
          ENDFOR 
      ENDIF 
  ENDFOR 

  nrerun = n_elements(reruns)
  print
  print,'Found '+ntostr(nrerun)+' reruns'

END 

PRO good_reruns_convert_photov, photo_ver_s, photo_ver

  IF photo_ver_s EQ 'NOPHOTOZ' THEN BEGIN 
      photo_ver = -1.0
      return
  ENDIF 

  IF photo_ver_s EQ 'NOBAYES' THEN BEGIN 
      photo_ver = -1.0
      return
  ENDIF 

  t = str_sep(photo_ver_s, 'v')
  t = t[1]
  tt = str_sep(t, '_')
  photo_ver = float(tt[0]+'.'+tt[1]+tt[2])

END 

PRO good_reruns

  on_error, 2

  sdssidl_setup
  IF NOT !sdssidl_def.sdss_data_dir_defined THEN BEGIN 
      message,'!SDSS_DATA_DIR must be defined'
  ENDIF 
  IF NOT !sdssidl_def.run_status_file_defined THEN BEGIN 
      message,'!RUN_STATUS_FILE must be defined'
  ENDIF 
  
  good_reruns_getreruns, !SDSS_DATA_DIR, runs, reruns
  print
  good_reruns_getreruns, !SDSS_SHAPECORR_DIR, cruns, creruns
  print

  ten = ulong64(10)
  super = ulong64(runs)*ten^6 + ulong64(reruns)
  csuper = ulong64(cruns)*ten^6 + ulong64(creruns)
  match, super, csuper, mt, mtc, /sort

  IF mtc[0] NE -1 THEN BEGIN 
      nmtc = n_elements(mtc)
      IF nmtc NE n_elements(creruns) THEN BEGIN 
          remove, mtc, cruns, creruns, csuper
          runs = [runs, cruns]
          reruns = [reruns, creruns]

          super = [super, csuper]
      ENDIF 
  ENDIF 

  s=sort(super)
  runs = runs[s]
  reruns = reruns[s]

  print,' run  rerun'
  colprint, runs, reruns

  nrun = n_elements(runs)
  s=create_struct("stripe", -1, "strip", "?", "run", 0L, "rerun", -2, $
                  "tsobj_photo_v", -1.0, "fpatlas_photo_v", -1.0, $
                  "adatc_photo_v", -1.0, 'baye_ver', -1.0, 'phtz_ver', -1.0, $
                  "bad", 0L, "bad2", 0L)
  run_status = replicate(s, nrun)
  run_status.run = runs
  run_status.rerun = reruns

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check if directories are OK
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  print,'Checking for asTrans/tsObj/tsField/fpAtlas/psField/fpM/adatc files'
  print

  w=where(run_status.rerun GE 0, ngood)
  FOR i=0L, ngood-1 DO BEGIN 
      ind=w[i]

      print,"Run: "+ntostr(run_status[ind].run)+" Rerun: "+ntostr(run_status[ind].rerun)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; initialize bad, bad2 for this run/rerun
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      bad  = 0L
      bad2 = 0L

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; check for asTrans file
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      transdir = !SDSS_DATA_DIR+ $
        ntostr(run_status[ind].run)+'/'+$
        ntostr(run_status[ind].rerun)+'/astrom/'
      spawn,'ls '+transdir+' | grep asTrans | wc -w',nastrans
      nastrans = long(nastrans[0])
      IF nastrans EQ 0 THEN bad = bad + 2L^0
      FOR camcol=1,6 DO BEGIN 

          fetch_dir, run_status[ind].run, camcol, run_status[ind].rerun, $
                     dir, atldir, corrdir=corrdir

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; check for tsObj,fpAtlas,psField files
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;; need to get the files for tsObj and fpAtlas
          fetch_file_list, dir, files, fnums
          spawn,'ls '+atldir+' | grep fpAtlas',atlasfiles
          IF atlasfiles[0] EQ '' THEN natlas = 0 ELSE natlas=n_elements(atlasfiles)

          ;; only need to check # of these files
          spawn,'ls '+atldir+' | grep psField | wc -w',npsfield
          spawn,'ls '+atldir+' | grep fpM | wc -w',nmask
          spawn,'ls '+corrdir+' | grep adatc | wc -w',nadatc
          npsfield = long(npsfield[0])
          nmask    = long(nmask[0])
          nadatc   = long(nadatc[0])

          photoz_file = !photoz_dir+'tsObj_ascii_'+ntostr(run_status[ind].run)+$
            '_'+ntostr(run_status[ind].rerun)+'_'+ntostr(camcol)+'.dat'
          photoz_file_exist = fexist(photoz_file)

          ;; tsField check added recently: goes into bad2
          spawn,'ls '+dir+' | grep tsField | wc -w',ntsField
          ntsField = long(ntsField[0])

          IF (files[0] EQ '') THEN BEGIN 
              bad = bad + 2L^(camcol+0)
          ENDIF ELSE BEGIN
              IF run_status[ind].stripe EQ -1 THEN BEGIN
                  hdr=headfits(files[0])
                  run_status[ind].stripe = sxpar(hdr, 'stripe')
              ENDIF
              IF run_status[ind].strip EQ "?" THEN BEGIN
                  hdr=headfits(files[0])
                  run_status[ind].strip = ntostr(sxpar(hdr, 'strip'))
              ENDIF
              IF run_status[ind].tsobj_photo_v EQ -1 THEN BEGIN 
                  hdr=headfits(files[0])
                  ;; they changed header tags at one point
                  tphv = ntostr(sxpar(hdr,'phot_ver'))
                  IF tphv EQ '0' THEN BEGIN 
                      tphv = ntostr(sxpar(hdr,'photo_ve'))
                  ENDIF 
                  good_reruns_convert_photov, tphv, phv
                  run_status[ind].tsobj_photo_v = phv
              ENDIF 
          ENDELSE 
          IF natlas EQ 0 THEN BEGIN
              bad = bad + 2L^(camcol+6)
          ENDIF ELSE BEGIN 
              IF run_status[ind].fpatlas_photo_v EQ -1 THEN BEGIN 
                  hdr=headfits(atldir+atlasfiles[0])
                  tfphv = ntostr(sxpar(hdr, 'version'))
                  good_reruns_convert_photov, tfphv, fphv
                  run_status[ind].fpatlas_photo_v = fphv
              ENDIF 
          ENDELSE 
          IF npsfield EQ 0 THEN BEGIN
              bad = bad + 2L^(camcol+12)
          ENDIF 
          IF nmask EQ 0 THEN BEGIN
              bad = bad + 2L^(camcol+18)
          ENDIF 
          ;; 2L^30 is last available flag in bad
          IF nadatc EQ 0 THEN BEGIN
              bad = bad + 2L^(camcol+24)
          ENDIF ELSE BEGIN 
              fetch_file_list, corrdir, cfiles, cfnums, front='adatc'
              chdr = headfits(cfiles[0])

              tcphv = ntostr(sxpar(chdr,'phot_ver'))
              IF tcphv EQ '0' THEN BEGIN 
                  tcphv = ntostr(sxpar(chdr,'photo_ve'))
                  IF tcphv EQ '0' THEN stop
              ENDIF 

              good_reruns_convert_photov, tcphv, cphv
              run_status[ind].adatc_photo_v = cphv

              tphtzv = sxpar(chdr, 'PHTZ_VER')
              tbayev = sxpar(chdr, 'BAYE_VER')
              good_reruns_convert_photov, tphtzv, phtzv
              good_reruns_convert_photov, tbayev, bayev

              run_status[ind].baye_ver = bayev
              run_status[ind].phtz_ver = phtzv

              IF ((run_status[ind].strip EQ "?" ) OR $
                  (run_status[ind].stripe EQ -1) ) THEN BEGIN 

                  run_status[ind].stripe = sxpar(chdr, 'stripe')
                  run_status[ind].strip = ntostr(sxpar(chdr, 'strip'))
              ENDIF 
          ENDELSE 

          ;; note: 2L^0 not yet used in bad2!  This done in order to
          ;; mirror above with tsObj
          IF ntsField[0] EQ 0 THEN BEGIN 
              bad2 = bad2 + 2L^(camcol+0)
          ENDIF 

          IF NOT photoz_file_exist THEN BEGIN 
              bad2 = bad2 + 2L^(camcol+6)
          ENDIF 

      ENDFOR 

      run_status[ind].bad  = bad
      run_status[ind].bad2 = bad2

  ENDFOR 

  print
  print,'!RUN_STATUS_FILE: ',!RUN_STATUS_FILE
  print
  mwrfits, run_status, !RUN_STATUS_FILE, /create
  
  return
END 
