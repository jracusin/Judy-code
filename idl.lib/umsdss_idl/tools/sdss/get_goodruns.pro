;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GET_GOODRUNS
;       
; PURPOSE:
;    Get all the "good" runs based on what files are available.  The default
;    is to return all runs for which the rerun and stripe is known.  
;    Further cuts can be applied based on availability of files.
;
; CALLING SEQUENCE:
;    get_goodruns, runs, reruns, stripes, strips, indices, ngood, $
;                    silent=silent, $
;                    /tsObj, /fpAtlas, $
;                    /psField, /fpM=fpM, $
;                    /tsField, /asTrans, /adatc, $
;                    /checkall=checkall, $
;                    help=help, $
;                    input_index = input_index
;
;
; INPUTS: 
;    None
;
; OPTIONAL INPUTS:
;    none
;
; KEYWORD PARAMETERS:
;    /tsObj: check for tsObj files
;    /fpAtlas: "" fpAtlas
;    /psField: "" psField
;    /fpM: "" fpM
;    /tsField: "" tsField
;    /asTrans: "" asTrans
;    /adatc: "" adatc
;
;    /checkall: check for all files
;
;    /silent: don't print out the good runs,reruns,etc
;    /help: print the syntax
;
; OUTPUTS: 
;    runs,reruns,stripes,strips,indices,ngood (in !run_status struct)
;
;
; CALLED ROUTINES:
;    SDSSIDL_SETUP
;    MAKE_RUNSTATUS_STRUCT
;    RUNSTATUS_SELECT
;
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Created: 25-Sep-2002  Erin Scott Sheldon UMich
;                                      
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO get_goodruns, runs, reruns, stripes, strips, indices, ngood, $
                  silent=silent, $
                  tsObj=tsObj, fpAtlas=fpAtlas, $
                  psField=psField, fpM=fpM, $
                  tsField=tsField, asTrans=asTrans, adatc=adatc, $
                  checkall=checkall, $
                  help=help, $
                  input_index = input_index

  delvarx,runs,reruns,stripes,strips

  IF keyword_set(help) THEN BEGIN
      print,'-Syntax:  get_goodruns, '+$
            'runs, reruns, stripes, strips, indices, ngood, $'
      print,'                        input_index=input_index, $'
      print,'                        /tsObj, /fpAtlas, /psField, /fpM, $'
      print,'                        /tsField, /asTrans, /adatc, $'
      print,'                        /checkall, $'
      print,'                        /silent, $'
      print,'                        /help'
      return
  ENDIF 

  sdssidl_setup,/silent

  IF NOT !sdssidl_def.run_status_defined THEN BEGIN 
      message,'!RUN_STATUS struct not defined in sdssidl_setup'
  ENDIF 

  runs=-1 & reruns = -1 & stripes = -1 & strips = ""

  ;; make sure ready to correct (psField can be checked separately
  ;; since it doesn't matter if its the same rerun)
  ;; or photo_version >= 5.3
  make_runstatus_struct,st

  st.rerun_exist = 'Y'
  st.stripe_exist = 'Y'

  IF keyword_set(checkall) THEN BEGIN 
      st.tsObj_exist = 'Y'
      st.fpAtlas_exist = 'Y'
      st.psField_exist = 'Y'
      st.fpM_exist = 'Y'
      st.tsField_exist = 'Y'
      st.asTrans_exist = 'Y'
      st.adatc_exist = 'Y'
  ENDIF ELSE BEGIN 
      IF keyword_set(tsObj) THEN st.tsObj_exist = 'Y'
      IF keyword_set(fpAtlas) THEN st.fpAtlas_exist = 'Y'
      IF keyword_set(psField) THEN st.psField_exist = 'Y'
      IF keyword_set(fpM) THEN st.fpM_exist = 'Y'
      IF keyword_set(tsField) THEN st.tsField_exist = 'Y'
      IF keyword_set(asTrans) THEN st.asTrans_exist = 'Y'
      IF keyword_set(adatc) THEN st.adatc_exist = 'Y'
  ENDELSE 

  runstatus_select, st, indices, input_index = input_index
  
  IF indices[0] EQ -1 THEN BEGIN 
      print,'No good runs/reruns found'
      indices=-1
      ngood = 0
      return
  ENDIF 

  ngood = n_elements(indices)

  runs = !RUN_STATUS[indices].run
  reruns = !RUN_STATUS[indices].rerun
  strips = !RUN_STATUS[indices].strip
  stripes = !RUN_STATUS[indices].stripe

  IF NOT keyword_set(silent) THEN BEGIN 
      print
      print,'        runs  reruns stripes strips'
      colprint,runs,reruns,stripes,'      '+strips
  ENDIF 


  return
END 
