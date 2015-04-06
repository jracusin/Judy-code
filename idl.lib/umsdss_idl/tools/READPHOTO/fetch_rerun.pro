pro fetch_rerun,run,rerun

;
;+
; NAME:
;    FETCH_RERUN
;
; PURPOSE:
;    will get the newest rerun that exists on disk for run "run"
;    does this by CD-ing to the directory from 
;    '/usr/sdss/data01/imaging/'+string(run)+'/calibChunks/'
;    this directory is symbolically linked to the newest rerun 
;    directory. Then just spawn a pwd and look at the 
;    directory name
;
; CALLING SEQUENCE:
;    fetch_rerun, run, rerun
;
; INPUTS:
;    run: the run in integer/long form.
;
; OUTPUTS:
;    rerun: the rerun in integer form.
;
;    NOTES: 1) Need !SDSS_DATA_DIR to be defined (see SDSSIDL_SETUP.PRO)
;           2) If run directory does not exist, then -1 is returned.
;-
;

  if n_params() eq 0 then begin
      print,'-syntax fetch_rerun,run,rerun'
      return
      print,'Use doc_library,"fetch_rerun" for more info'
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set base directory here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  sdssidl_setup,/silent
  IF NOT !SDSSIDL_DEF.SDSS_DATA_DIR_DEFINED THEN BEGIN 
      message,'!SDSS_DATA_DIR must be defined'
  ENDIF 
  base = !SDSS_DATA_DIR

  s = base + string(run)+'/calibChunks/'
  s = strcompress(s,/rem)
  
  spawn,'ls -l '+s,answer
  IF answer[0] EQ ''THEN BEGIN
      print,'No directory for run ',run
      rerun = -1
      return
  ENDIF 
  
  spawn,'pwd',old_dir
  
  cd,s
  spawn,'pwd',wd
  wd=wd
  wd=wd(0)
  ss=str_sep(wd,'/')
  wss = where(ss EQ 'calibChunks')

  rerun=fix(ss(wss[0]-1))
  
  cd,old_dir[0]

  return
end

