
;+
; NAME:
;  SDSS_MAGUSE
;
;
; PURPOSE:
;  Given a structure, look for magnitude arrays and return the tag index of the
;  first one found.  The default is to look for
;  COUNTS_MODEL, then PETROCOUNTS, FIBERCOUNTS, PSFCOUNTS. The user can tell it
;  to look for a specific tag as well using the magUse keyword.
;
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;  wmag = sdss_maguse(struct, magUse=magUse, /silent)
;
;
; INPUTS:
;  struct: A structure.
;
;
; OPTIONAL INPUTS:
;  magUse: the name of the magnitude to look for.
;
;
; KEYWORD PARAMETERS:
;  /silent: no informative messages.
;
;
; OUTPUTS:
;  the tag index of the matching magnitude array.
;
;
; EXAMPLE:
;  run=756
;  rerun=40
;  camcol=3
;  field=125
;  nFrames=20
;  read_tsobj, [run,rerun,camcol], struct, start=field, nFrames=nFrames
;  wmag = sdss_maguse(struct)
;
;
; MODIFICATION HISTORY:
;   Creation: ??-??-2003  Erin Sheldon UofChicago
;
;-


FUNCTION sdss_maguse, struct, maguse=maguse, silent=silent

  IF n_elements(struct) EQ 0 THEN BEGIN 
      print,'-Syntax: wmag = sdss_maguse(sdss_struct, maguse=maguse)'
      return,-1
  ENDIF 

  tags = tag_names(struct)

  IF n_elements(maguse) NE 0 THEN BEGIN 
      wmag = where(tags EQ strupcase(maguse), nmatch)
      IF nmatch NE 0 THEN BEGIN 
          return, wmag[0]
      ENDIF 
  ENDIF 

   wmag = where(tags EQ 'COUNTS_MODEL',nmod)
   IF nmod EQ 0 THEN BEGIN 
       wmag = where(tags EQ 'PETROCOUNTS', npet)
       IF npet EQ 0 THEN BEGIN 
           wmag = where(tags EQ 'FIBERCOUNTS',nfib)
           IF nfib EQ 0 THEN BEGIN 
               wmag = where(tags EQ 'PSFCOUNTS',npsf)
               IF (npsf EQ 0) AND (NOT keyword_set(silent)) THEN BEGIN 
                   print
                   print,'No magnitude arrays found. Not displaying colors.'
               ENDIF
           ENDIF
       ENDIF
   ENDIF
   
   return,wmag[0]

END 
