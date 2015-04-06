
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    GET_GOODSTRIPE
;       
; PURPOSE:
;    A wrapper for get_goodruns that also checks agains the input stripe 
;    number.  
;   
; CALLING SEQUENCE:
;    get_goodstripe, stripe, runs, reruns, stripes, strips, indices, ngood, $
;                    silent=silent, $
;                    /tsObj, /fpAtlas, $
;                    /psField, /fpM=fpM, $
;                    /tsField, /asTrans, /adatc, $
;                    /checkall=checkall, $
;                    help=help, $
;                    input_index = input_index
;
; INPUTS: 
;    stripe: the SDSS stripe number in integer form
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
;    GET_GOODRUNS
;
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    25-Sep-2002  Erin Scott Sheldon UMich
;                                                            
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO get_goodstripe, stripe, runs, reruns, stripes, strips, indices, ngood, $
                    silent=silent, $
                    tsObj=tsObj, fpAtlas=fpAtlas, $
                    psField=psField, fpM=fpM, $
                    tsField=tsField, asTrans=asTrans, adatc=adatc, $
                    checkall=checkall, $
                    help=help, $
                    input_index = input_index

  IF n_params() LT 1 THEN BEGIN
      print,'-Syntax:  get_goodstripe, '+$
            'stripe, runs, reruns, stripes, strips, indices, ngood, $'
      print,'                        input_index=input_index, $'
      print,'                        /tsObj, /fpAtlas, /psField, /fpM, $'
      print,'                        /tsField, /asTrans, /adatc, $'
      print,'                        /checkall, $'
      print,'                        /silent, $'
      print,'                        /help'
      return
  ENDIF 


  delvarx,runs,reruns,stripes,strips

  get_goodruns, runs, reruns, stripes, strips, indices, ngood, $
                /silent, $
                tsObj=tsObj, fpAtlas=fpAtlas, $
                psField=psField, fpM=fpM, $
                tsField=tsField, asTrans=asTrans, adatc=adatc, $
                checkall=checkall, $
                input_index = input_index


  IF ngood EQ 0 THEN BEGIN 
      print
      print,'No good runs/reruns found'
      return
  ENDIF

  w=where(stripes EQ stripe, ngood)
  IF ngood EQ 0 THEN BEGIN 
      print
      print,'No good runs/reruns found'
      runs=-1
      reruns=-1
      strips=""
      return
  ENDIF 

  indices = indices[w]

  runs = runs[w]
  reruns = reruns[w]
  stripes = stripes[w]
  strips = strips[w]

  IF NOT keyword_set(silent) THEN BEGIN 
      print
      print,'        runs  reruns    strips'
      colprint,runs,reruns,'         '+strips
  ENDIF 

  return


END 
