;+
; NAME:
;   PHOTOID
;
;
; PURPOSE:
;   Create unique super id from run,rerun,camcol,field,id
;
;
; CATEGORY:
;   SDSS Specific routine.
;
;
; CALLING SEQUENCE:
;   superid = photoid(run,rerun,camcol,field,id)
;    OR
;   superid = photoid(struct)
;
;
; INPUTS:
;   run,rerun,camcol,field,id (may be arrays)
;    OR
;   struct: must contain the above (may be array of structs)
;
;
; OPTIONAL INPUTS:
;   NONE
;
;
; KEYWORD PARAMETERS:
;   NONE
;
;
; OUTPUTS:
;   A superid is returned
;
;
; OPTIONAL OUTPUTS:
;   NONE
;
;
; COMMON BLOCKS:
;   NONE
;
;
; SIDE EFFECTS:
;   NONE
;
;
; RESTRICTIONS:
;   NONE
;
;
; PROCEDURE:
;   super = id + field*10^6 + camcol*10^11 + rerun*ten^12 + run*ten^15
;
;
; MODIFICATION HISTORY:
;   Created ??-??-2002 Erin Sheldon UofMichigan
;
;-



FUNCTION photoid, run, rerun, camcol, field, id

  ten=ulong64(10)
  p1 = 0L
  p2 = 6L
  p3 = 11L
  p4 = 12L
  p5 = 15L

  np = n_params()
  IF n_params() EQ 1 THEN BEGIN 

      super = $
        ulong64(run.id)*ten^p1     + $
        ulong64(run.field)*ten^p2   + $
        ulong64(run.camcol)*ten^p3 + $
        ulong64(run.rerun)*ten^p4  + $
        ulong64(run.run)*ten^p5

  ENDIF ELSE IF np EQ 5 THEN BEGIN 

      nr   = n_elements(run)
      nrer = n_elements(rerun)
      nc   = n_elements(camcol)
      nf   = n_elements(field)
      nid  = n_elements(id)
      
      IF total([nr,nrer,nc,nf,nid]) NE 5*nr THEN BEGIN 
          print,'All arrays must be same size'
          return, -1
      ENDIF 
      
      super = $
        ulong64(id)*ten^p1     + $
        ulong64(field)*ten^p2   + $
        ulong64(camcol)*ten^p3 + $
        ulong64(rerun)*ten^p4  + $
        ulong64(run)*ten^p5

  ENDIF ELSE BEGIN 
      print,'Syntax: superid=photoid(run,rerun,camcol,field,id OR struct)'
      return,-1
  ENDELSE 

  return,super

END 
