
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    READ_ASTRANS
;       
; PURPOSE:
;    Read from an asTrans astrometry transformation file for 
;    given run, camcol, and bandpass. Can be used to 
;    transform between (row,col), or CCD coordinates, and (mu,nu),
;    or great circle coords.
;
; CALLING SEQUENCE:
;    
;
; INPUTS: 
;    read_astrans, run, rerun, camcol, clr [, trans, node, inc]
;
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    NONE
;
; OPTIONAL OUTPUTS:
;    trans: The astrometry structure.
;    node: The node position of this stripe. 
;    inc: Inclination of this stripe. 
;       node and inc required by rowcol2munu.pro, and gc2eq.pro or gc2survey.pro
;
; CALLED ROUTINES:
;    FETCH_DIR
;    HEADFITS
;    MRDFITS
; 
; PROCEDURE: 
;    asTrans files conain info for each camcol/field/bandpass for a given
;    column.  The different camcol/bandpasses are in different 
;    extensions.  See http://www-sdss.fnal.gov:8000/dm/flatFiles/asTrans.html
;    for details.
;	
;
; REVISION HISTORY:
;    Created: 23-OCT-2000 Erin Scott Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO read_astrans, run, rerun, camcol, clr, trans, node, inc, silent=silent, indir=indir, transfile=transfile

  IF n_params() LT 3 THEN BEGIN 
      print,'-Syntax: read_astrans, run, rerun, camcol, clr [, trans, node, inc, silent=silent, indir=indir, transfile=transfile]'
      print,''
      print,'Use doc_library,"read_astrans"  for more help.'  
      return
  ENDIF 

  colors = ['u','g','r','i','z']
  delvarx,trans

  IF clr GT 4 OR clr LT 0 THEN BEGIN
      print,'Legal clr: [0,1,2,3,4]'
      return
  ENDIF 

  ;; get astrom directory
  defsysv, '!SDSS_DATA_DIR', exists=exists
  IF NOT exists THEN sdssidl_setup
  base = !SDSS_DATA_DIR
  
  IF n_elements(indir) NE 0 THEN BEGIN 
      astromdir = indir
  ENDIF ELSE BEGIN 
      astromdir = !SDSS_DATA_DIR+'/'+ntostr(run)+'/'+ntostr(rerun)+'/astrom/'
  ENDELSE 
  spawn,'ls -l '+astromdir,answer
  IF answer[0] EQ ''THEN BEGIN
      print,'No astrom directory for run '+ntostr(run)
      return
  ENDIF 

  runstr = run2string(run)
  IF n_elements(transfile) EQ 0 THEN $
    transfile = astromdir + 'asTrans-'+runstr+'.fit'

  hdr = headfits(transfile)

  IF datatype(hdr[0]) EQ 'INT' THEN BEGIN 
      print,'No astrans file!'
      return
  ENDIF 

  camcols = sxpar(hdr,'camcols')
  filters = sxpar(hdr,'filters')
  node = sxpar(hdr,'node')
  inc = sxpar(hdr,'incl')

  camarray = fix( str_sep(camcols, ' ') )
  filtarray = str_sep(filters, ' ')

  ncams = n_elements(camarray)
  nfils = n_elements(filtarray)
  
  wcam = where( camarray EQ camcol, nc)
  IF nc EQ 0 THEN BEGIN
      print,'Camcol ',camcol,' not processed'
      return
  ENDIF 
  wclr = where(filtarray EQ colors[clr], nclr)
  IF nclr EQ 0 THEN BEGIN 
      print,'Filter ',colors[clr],' not processed'
      return
  ENDIF 

  ext = wcam[0]*nfils + (wclr[0] + 1)
  trans = mrdfits(transfile, ext, ehdr,/silent)

  ccam=sxpar(ehdr, 'camcol',count=count)
  IF count EQ 0 THEN BEGIN
      print,'No camcol in header'
      return
  ENDIF 
  cfilt=sxpar(ehdr, 'filter',count=count)
  IF count EQ 0 THEN BEGIN
      print,'No filter in header'
      return
  ENDIF 
  IF NOT keyword_set(silent) THEN BEGIN 
      print
      print,'Camcols processed: ',camcols
      print,'Filters processed: ',filters
      print,'Reading extension: ',ext
      print
      print,'Got camcol = ',ntostr(ccam),'('+ntostr(camcol)+')'
      print,'    filter = ',ntostr(cfilt),'('+colors[clr]+')'
      print
  ENDIF 

  return
END 
