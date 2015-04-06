pro atlas_name, run, camcol, field, fname

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    ATLAS_NAME
;
; PURPOSE: 
;    Subroutine of get_atlas.  Creates the name of atlas file.
;
; CALLING SEQUENCE:
;    atlas_name, run, camcol, field [, atlasname]
;
; INPUTS:  
;    run: sdss run number
;    camcol: camera column
;    field: field of the object
;
; OUTPUTS: 
;    atlasname
;
; REVISION HISTORY:
;    Erin Scott Sheldon  3/14/99
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 3 THEN BEGIN
      print,'-Syntax: atlas_name, run, camcol, field [, atlasname]'
      return
  ENDIF 

  rs = run2string(run)
  cs = strtrim(string(camcol),2)
  fs = field2string(field)
  fname='fpAtlas-'+rs+'-'+cs+'-'+fs+'.fit '
  fname = strtrim(fname)

return
end
