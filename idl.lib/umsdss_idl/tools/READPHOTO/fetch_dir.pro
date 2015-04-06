pro fetch_dir, run, camcol, rerun, dir, atldir, file, atlfile,$
               field=field, check=check, corrdir=corrdir,$
               corratldir=corratldir
;
;+
; NAME:
;    FETCH_DIR
;
; PURPOSE:
;    given run camcol and rerun it will retrun
;    the directory dir on fsgi03 where this tsObj file lives
;    and the directory atldir where the atlas images live
;    if given the field it will output the tsObj file as well
;    and the atlas file
;
; CALLING SEQUENCE:
;   fetch_dir, run, camcol, rerun [, dir, atldir, file, atlfile,
;              field=field, check=check, corrdir=corrdir]
;
; INPUTS:
;    run, camcol, rerun: in integer form
; 
; OUTPUTS:
;    none
;
; OPTIONAL INPUTS:
;    field: An integer field number.  If input, file and atlfile (tsObj and
;           fpAtlas) file names can be returned
;    check: If /check, then will make sure directory exists.  Returns '' if
;           not.
;
; OPTIONAL OUTPUTS:
;    dir, atldir: the tsObj directory and fpAtlas directory (objcs)
;    file, atlfile: filenames for input field number
;    corrdir: the directory holding corrected shape files. Requires
;       system variable !SDSS_SHAPECORR_DIR in SDSSIDL_SETUP.PRO
;    corratldir:  ''
;
; NOTES 
;    1) Variables !SDSS_DATA_DIR and !SDSS_SHAPECORR_DIR are set in
;       sdssidl_setup.  You must have that procedure to run this one.
;    2) If /check is set and a file or directory is not found, then
;       it is set to ''
;
; EXAMPLE:
;    IDL> fetch_dir,259,5,1,200,dir,file,field=203
;    IDL> print,dir
;    /usr/sdss/data01/imaging/259/1/calibChunks/5
;    IDL> print,file
;    /usr/sdss/data01/imaging/259/1/calibChunks/5/tsObj-000259-5-1-0203.fit
;-
;

  if n_params() eq 0 then begin
      print,'-syntax fetch_dir,run,camcol,rerun,dir,atldir,file,atlfile,field=field,check=check, corrdir=corrdir, corratldir=corratldir'
      return
      print,'Use doc_library,"fetch_dir" for more info'
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set base directory for your maching here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  sdssidl_setup,/silent
  IF NOT !SDSSIDL_DEF.SDSS_DATA_DIR_DEFINED THEN BEGIN 
      message,'!SDSS_DATA_DIR must be defined'
  ENDIF 
  base = !SDSS_DATA_DIR

  runs=strcompress(run,/rem)
  reruns=strcompress(rerun,/rem)
  camcols=strcompress(camcol,/rem)
  
  dir = base + runs+'/'+reruns+'/'+'calibChunks/'+camcols+'/'
  atldir = base + runs+'/'+reruns+'/'+'objcs/'+camcols+'/'
  
  IF !SDSSIDL_DEF.SDSS_SHAPECORR_DIR_DEFINED THEN BEGIN 
      corrbase = !SDSS_SHAPECORR_DIR
      corrdir = corrbase + 'corr'+ runs+'/'+reruns+'/'+'calibChunks/'+camcols+'/'
      corratldir = corrbase + 'corr'+ runs+'/'+reruns+'/'+'objcs/'+camcols+'/'
  ENDIF ELSE BEGIN
      message,'!SDSS_SHAPECORR_DIR not defined',/inf
      corrdir=''
      corratldir=''
  ENDELSE 
  IF keyword_set(check) THEN BEGIN
      ret=0
      spawn,'ls -ld '+dir,answer
      IF answer[0] EQ '' THEN BEGIN
          print,'tsObj Directory: '
          print,dir
          print,'does not exist'
          dir=''
          file=''
          ret=1
      ENDIF 
      spawn,'ls -ld '+atldir,answer
      IF answer[0] EQ '' THEN BEGIN
          print,'Atlas Directory: '
          print,atldir
          print,'does not exist'
          atldir=''
          atlfile=''
          ret=1
      ENDIF 

      spawn,'ls -ld '+corrdir,answer
      IF answer[0] EQ '' THEN BEGIN
          print,'corr Directory: '
          print,dir
          print,'does not exist'
          corrdir=''
      ENDIF 
      spawn,'ls -ld '+corratldir,answer
      IF answer[0] EQ '' THEN BEGIN
          print,'corr Atlas Directory: '
          print,atldir
          print,'does not exist'
          corratldir=''
      ENDIF 

      IF ret THEN return
  ENDIF 
  
  if n_elements(field) eq 0 then return
  
  file='tsObj-'
  atlfile='fpAtlas-'
  
  f=string(1000000+run)
  f=strmid(strcompress(f,/rem),1,6)
                                ;now looks like "000259"
  g=string(10000+field)
  g=strmid(strcompress(g,/rem),1,4)
                                ;now looks like "0203"
  
  
  file=file+f
  file=file+'-'+camcols+'-'+reruns+'-'+g+'.fit'
  file=dir+file
  
  atlfile=atlfile+f
  atlfile=atlfile+'-'+camcols+'-'+g+'.fit'
  atlfile=atldir+atlfile

  if keyword_set(check) then begin
      if exist(file) eq 0 then begin
          print
          print,'tsObj file:  '
          print,file
          print,'does not exist'
          file=''
      endif
      if exist(atlfile) eq 0 then begin
          print
          print,'Atlas file:  '
          print,atlfile
          print,'does not exist'
          atlfile=''
      endif
  endif
  
  return
end














