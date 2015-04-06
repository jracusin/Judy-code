;+
; NAME:
;       MAKE_STATUS_STRUCT
; PURPOSE:
;       Set up a parameter structure for sdss status selection
;
; CALLING SEQUENCE:
;      make_status_struct, status_struct 
;
; INPUTS:
;       
; OUTPUTS:
;       status_struct: the structure used for sdss object selection....
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: This sets up the structure for status selection of sdss objects
;       
;
; REVISION HISTORY:
;       Ryan Scranton		11/4/99
;-


pro make_status_struct, status_struct

 on_error, 2

 if N_params() ne 1 then begin
        print,'Syntax - make_status_struct, status_struct'
        return
 endif

 status_struct = { $
        SET: 'D', $
        GOOD: 'D', $
        DUPLICATE: 'D', $
        OK_RUN: 'D', $
        RESOLVED: 'D', $
        PSEGMENT: 'D', $
        FIELD: 'D', $
        SCANLINE: 'D', $
        STRIPE: 'D', $
        SECONDARY: 'D', $
        PRIMARY: 'D', $
        TARGET: 'D'}

  return 
  end

