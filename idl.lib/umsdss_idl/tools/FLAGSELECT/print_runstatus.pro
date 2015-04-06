
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    PRINT_RUNSTATUS
;       
; PURPOSE:
;    Decode and print the status (from !run_status) for the input run/rerun
;
; CALLING SEQUENCE:
;    print_runstatus, run, rerun
;
; INPUTS: 
;    run: photo run
;    rerun: photo rerun
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    print to screen
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    SDSSIDL_SETUP
;    NTOSTR
; 
; PROCEDURE: 
;    the .bad tag is a bitmask containing status flags.  Decode it
;    and print the results
;	
;
; REVISION HISTORY:
;    
;       Creation:
;          ??-??-2000: Erin Scott Sheldon UofChicago
;          18-Nov-2002: Added bad2 flag checking: tsField files
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO print_runstatus, run, rerun

  on_error, 2

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: print_runstatus, run, rerun'
      return
  ENDIF 

  sdssidl_setup,/silent
  w1=where( !RUN_STATUS.run EQ run,nw)
  
  IF nw EQ 0 THEN BEGIN 
      print,"Don't know anything about run ",ntostr(run)
      return
  ENDIF 

  nrerun = n_elements(rerun)
  IF nrerun NE 0 THEN BEGIN 
      FOR j=0L, nrerun-1 DO BEGIN 
          w2=where( !RUN_STATUS[w1].rerun EQ rerun,nw)
          IF nw EQ 0 THEN BEGIN 
              print,"Run ",ntostr(run),": No such rerun ",ntostr(rerun)
              return
          ENDIF 
      ENDFOR 
  ENDIF ELSE rerun = !RUN_STATUS[w1].rerun

  ;; does bad2 exist?
  IF tag_exist(!RUN_STATUS, 'bad2') THEN dobad2=1 ELSE dobad2=0

  nrerun = n_elements(rerun)
  FOR i=0L, nrerun-1 DO BEGIN 
 
      w2 = where( !RUN_STATUS[w1].rerun EQ rerun[i] )
      ind = w1[w2]
      bad = !RUN_STATUS[ind].BAD
      IF dobad2 THEN bad2 = !RUN_STATUS[ind].BAD2
      stripe = !RUN_STATUS[ind].STRIPE
      strip = !RUN_STATUS[ind].STRIP

      ;; check for astrans file
      IF (bad AND 2L^0) NE 0 THEN transstr = 'Missing' ELSE transstr = 'OK'
  
      ;; check for tsObj/fpatlas/psfield/fpM/adatc
      tsobjstr =   ['tsObj',     replicate('OK     ', 6)]
      atlasstr =   ['fpAtlas',   replicate('OK     ', 6)]
      psfieldstr = ['psField',   replicate('OK     ', 6)]
      fpMstr    =  ['fpM',       replicate('OK     ', 6)]
      adatcstr  =  ['adatc',     replicate('OK     ', 6)]

      tsfieldstr = ['tsField',   replicate('OK     ', 6)]
      photozstr =  ['Photoz',    replicate('OK     ', 6)]

      FOR camcol=1,6 DO BEGIN 
          
          IF ( bad AND 2L^(camcol+ 0) ) NE 0 THEN tsobjstr[camcol]   = 'Missing'
          IF ( bad AND 2L^(camcol+ 6) ) NE 0 THEN atlasstr[camcol]   = 'Missing'
          IF ( bad AND 2L^(camcol+12) ) NE 0 THEN psfieldstr[camcol] = 'Missing'
          IF ( bad AND 2L^(camcol+18) ) NE 0 THEN fpMstr[camcol]     = 'Missing'
          IF ( bad AND 2L^(camcol+24) ) NE 0 THEN adatcstr[camcol]   = 'Missing'

          IF dobad2 THEN BEGIN 
              IF ( bad2 AND 2L^(camcol+ 0) ) NE 0 THEN tsfieldstr[camcol] = 'Missing'
              IF ( bad2 AND 2L^(camcol+ 6) ) NE 0 THEN photozstr[camcol]  = 'Missing'
          ENDIF 
      ENDFOR 
      
      spacing = '   '
      print
      print,'* Run = '+ntostr(run),'  Rerun = '+ntostr(rerun[i]),$
        '  Stripe = '+ntostr(stripe),'  Strip = '+strip
      print
      print,'Filetype    col1      col2      col3      col4      col5      col6'
      print,'-------------------------------------------------------------------'
      print,tsobjstr[0]+'        '+tsobjstr[1]+spacing+tsobjstr[2]+spacing+tsobjstr[3]+spacing+tsobjstr[4]+spacing+tsobjstr[5]+spacing+tsobjstr[6]

      print,tsFieldstr[0]+'      '+tsFieldstr[1]+spacing+tsFieldstr[2]+spacing+tsFieldstr[3]+spacing+tsFieldstr[4]+spacing+tsFieldstr[5]+spacing+tsFieldstr[6]

      print,atlasstr[0]+'      '+atlasstr[1]+spacing+atlasstr[2]+spacing+atlasstr[3]+spacing+atlasstr[4]+spacing+atlasstr[5]+spacing+atlasstr[6]

      print,psfieldstr[0]+'      '+psfieldstr[1]+spacing+psfieldstr[2]+spacing+psfieldstr[3]+spacing+psfieldstr[4]+spacing+psfieldstr[5]+spacing+psfieldstr[6]

      print,fpMstr[0]+'          '+fpMstr[1]+spacing+fpMstr[2]+spacing+fpMstr[3]+spacing+fpMstr[4]+spacing+fpMstr[5]+spacing+fpMstr[6]

      print,photozstr[0]+'       '+photozstr[1]+spacing+photozstr[2]+spacing+photozstr[3]+spacing+photozstr[4]+spacing+photozstr[5]+spacing+photozstr[6]

      print,adatcstr[0]+'        '+adatcstr[1]+spacing+adatcstr[2]+spacing+adatcstr[3]+spacing+adatcstr[4]+spacing+adatcstr[5]+spacing+adatcstr[6]

      print,'Astrans      '+transstr
      
  ENDFOR 
  print
  return
END 
