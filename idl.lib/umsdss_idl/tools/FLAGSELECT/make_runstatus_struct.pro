;+
; NAME:
;       MAKE_RUNSTATUS_STRUCT
; PURPOSE:
;	Set up a parameter structure for sdss RUN STATUS selection
;
; CALLING SEQUENCE:
;      make_runstatus_struct, runstatus_struct
;
; INPUTS:
;       
; OUTPUTS:
;	runstatus_struct: the structure used for sdss object selection....
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: This sets up the structure for run status selection of 
;   sdss runs/reruns
;	
;
; REVISION HISTORY:
;  ??-??-2002: Erin Scott Sheldon UofChicago
;  18-Nov-2002: Added bad2 flag checking: tsField files
;	
;-


pro make_runstatus_struct, runstatus_struct

 on_error, 2

 if N_params() ne 1 then begin
        print,'Syntax - make_runstatus_struct, runstatus_struct'
        return
 endif

 runstatus_struct = { $
                      RERUN_EXIST:    'D', $
                      STRIPE_EXIST:   'D', $
                      $
                      ASTRANS_EXIST:  'D', $
                      TSOBJ_EXIST:    'D', $
                      FPATLAS_EXIST:  'D', $
                      PSFIELD_EXIST:  'D', $
                      FPM_EXIST:      'D', $
                      ADATC_EXIST:    'D', $
                      TSFIELD_EXIST:  'D', $ ; bad2 flags
                      PHOTOZ_EXIST:   'D', $
                      $
                      TSOBJ1_EXIST:   'D', $
                      TSOBJ2_EXIST:   'D', $
                      TSOBJ3_EXIST:   'D', $
                      TSOBJ4_EXIST:   'D', $
                      TSOBJ5_EXIST:   'D', $
                      TSOBJ6_EXIST:   'D', $
                      $
                      FPATLAS1_EXIST: 'D', $
                      FPATLAS2_EXIST: 'D', $
                      FPATLAS3_EXIST: 'D', $
                      FPATLAS4_EXIST: 'D', $
                      FPATLAS5_EXIST: 'D', $
                      FPATLAS6_EXIST: 'D', $
                      $
                      PSFIELD1_EXIST: 'D', $
                      PSFIELD2_EXIST: 'D', $
                      PSFIELD3_EXIST: 'D', $
                      PSFIELD4_EXIST: 'D', $
                      PSFIELD5_EXIST: 'D', $
                      PSFIELD6_EXIST: 'D', $
                      $
                      FPM1_EXIST: 'D', $
                      FPM2_EXIST: 'D', $
                      FPM3_EXIST: 'D', $
                      FPM4_EXIST: 'D', $
                      FPM5_EXIST: 'D', $
                      FPM6_EXIST: 'D', $
                      $
                      ADATC1_EXIST: 'D', $
                      ADATC2_EXIST: 'D', $
                      ADATC3_EXIST: 'D', $
                      ADATC4_EXIST: 'D', $
                      ADATC5_EXIST: 'D', $
                      ADATC6_EXIST: 'D', $
                      $
                      TSFIELD1_EXIST: 'D', $ ; bad2 flags
                      TSFIELD2_EXIST: 'D', $
                      TSFIELD3_EXIST: 'D', $
                      TSFIELD4_EXIST: 'D', $
                      TSFIELD5_EXIST: 'D', $
                      TSFIELD6_EXIST: 'D', $
                      $
                      PHOTOZ1_EXIST: 'D', $ 
                      PHOTOZ2_EXIST: 'D', $
                      PHOTOZ3_EXIST: 'D', $
                      PHOTOZ4_EXIST: 'D', $
                      PHOTOZ5_EXIST: 'D', $
                      PHOTOZ6_EXIST: 'D'  $
                    }

  return 
  end



