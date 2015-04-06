;+
; NAME:
;       MAKE_FLAG_STRUCT
; PURPOSE:
;	Set up a structure for sdss flag selection.
;       Can select on either flags or flags2. 
;
; CALLING SEQUENCE:
;      make_flag_struct, flag_struct 
;
; INPUTS:
;       
; OUTPUTS:
;	flag_struct: the structure used for sdss object selection....
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: This sets up the structure for flag selection of sdss objects
;	
;
; REVISION HISTORY:
;	Tim McKay	UM	1/8/99
;       Philf Fischer           1/15/99
;       Erin Scott Sheldon UM 2/5/00 Added flags2
;-


pro make_flag_struct, flag_struct

  on_error, 2

 if N_params() ne 1 then begin
        print,'Syntax - make_flag_struct, flag_struct'
        return
 endif

 flag_struct = { $              ;Begin flags
                 CANONICAL_CENTER: 'D', $
                 BRIGHT: 'D', $
                 EDGE: 'D', $
                 BLENDED: 'D', $
                 CHILD: 'D', $
                 PEAKCENTER: 'D', $
                 NODEBLEND: 'D', $
                 NOPROFILE: 'D', $
                 NOPETRO: 'D', $
                 MANYPETRO: 'D', $
                 NOPETRO_BIG: 'D', $
                 DEBLEND_TOO_MANY_PEAKS: 'D', $
                 CR: 'D', $
                 MANYR50: 'D', $
                 MANYR90: 'D', $
                 BAD_RADIAL: 'D', $
                 INCOMPLETE_PROFILE: 'D', $
                 INTERP: 'D', $
                 SATUR: 'D', $
                 NOTCHECKED: 'D', $
                 SUBTRACTED: 'D', $
                 NOSTOKES: 'D', $
                 BADSKY: 'D', $
                 PETROFAINT: 'D', $
                 TOO_LARGE: 'D', $
                 DEBLENDED_AS_PSF: 'D', $
                 DEBLEND_PRUNED: 'D', $
                 ELLIPFAINT: 'D', $
                 BINNED1: 'D', $
                 BINNED2: 'D', $
                 BINNED4: 'D', $
                 MOVED: 'D', $
                 $              ;Begin flags2
                 DEBLENDED_AS_MOVING: 'D', $
                 NODEBLEND_MOVING   : 'D', $
                 TOO_FEW_DETECTIONS: 'D', $
                 BAD_MOVING_FIT    : 'D', $
                 STATIONARY       : 'D', $
                 PEAKS_TOO_CLOSE  : 'D', $
                 MEDIAN_CENTRE    : 'D', $
                 LOCAL_EDGE       : 'D', $
                 BAD_COUNTS_ERROR: 'D', $
                 BAD_MOVING_FIT_CHILD: 'D', $
                 DEBLEND_UNASSIGNED_FLUX: 'D', $
                 SATUR_CENTER    : 'D', $
                 INTERP_CENTER  : 'D', $
                 DEBLENDED_AT_EDGE: 'D', $
                 DEBLEND_NOPEAK   : 'D', $
                 PSF_FLUX_INTERP : 'D', $
                 TOO_FEW_GOOD_DETECTIONS: 'D', $
                 CENTER_OFF_AIMAGE   : 'D', $
                 DEBLEND_DEGENERATE: 'D', $
                 BRIGHTEST_GALAXY_CHILD  : 'D', $
                 CANONICAL_BAND : 'D', $
                 AMOMENT_FAINT : 'D', $
                 AMOMENT_UNWEIGHTED : 'D', $ ;same flag as faint. Now we find uw mom
                 AMOMENT_SHIFT : 'D', $      ;if faint
                 AMOMENT_MAXITER : 'D', $    
                 MAYBE_CR : 'D', $
                 MAYBE_EGHOST : 'D', $
                 NOTCHECKED_CENTER: 'D'}

  return 
  end



