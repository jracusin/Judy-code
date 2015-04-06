;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    PRINT_FLAGS
; PURPOSE:
;    Prints flags for a single object....
;
; Inputs:  pstruct: a photo output structure (must have .flags tag...)
;	   index: index of the object of interest
;
; Outputs: Prints flags status
;
; Author:  Tim McKay
; Date: 1/7/99
; Erin Scott Sheldon  UM  2/5/00 added flags2, made easier to add flags
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro print_flags, pstruct, index, colorindex, objc=objc

  on_error, 2

  if n_params() LT 3 then begin
      print,'-syntax print_flags, pstruct, index, colorindex, objc=objc'
      print,' Only for use with a single object'
      return
  endif

  IF n_elements(index) GT 1 THEN BEGIN 
      print,'Can only do one object at a time'
      return
  ENDIF 

  numflags=32
  numflags2=21

  tags=tag_names(pstruct)
  w1=where(tags EQ 'FLAGS2', nw1)
  w2=where(tags EQ 'OBJC_FLAGS2', nw2)


  fstr = ['CANONICAL_CENTER', $
          'BRIGHT', $
          'EDGE', $
          'BLENDED ', $
          'CHILD', $
          'PEAKCENTER', $
          'NODEBLEND', $
          'NOPROFILE', $
          'NOPETRO', $
          'MANYPETRO', $
          'NOPETRO_BIG', $
          'DEBLEND_TOO_MANY_PEAKS', $
          'CR', $
          'MANYR50', $
          'MANYR90', $
          'BAD_RADIAL', $
          'INCOMPLETE_PROFILE', $
          'INTERP', $
          'SATUR', $
          'NOTCHECKED', $
          'SUBTRACTED', $
          'NOSTOKES', $
          'BADSKY', $
          'PETROFAINT ', $
          'TOO_LARGE', $
          'DEBLENDED_AS_PSF', $
          'DEBLEND_PRUNED', $
          'ELLIPFAINT', $
          'BINNED1', $
          'BINNED2', $
          'BINNED4 ', $
          'MOVED']

  f2str=  ['DEBLENDED_AS_MOVING', $ 
           'NODEBLEND_MOVING', $ 
           'TOO_FEW_DETECTIONS', $ 
           'BAD_MOVING_FIT', $
           'STATIONARY', $
           'PEAKS_TOO_CLOSE', $
           'MEDIAN_CENTRE', $
           'LOCAL_EDGE', $ 
           'BAD_COUNTS_ERROR', $ 
           'BAD_MOVING_FIT_CHILD', $ 
           'DEBLEND_UNASSIGNED_FLUX', $ 
           'SATUR_CENTER', $ 
           'INTERP_CENTER', $
           'DEBLENDED_AT_EDGE', $
           'DEBLEND_NOPEAK', $
           'PSF_FLUX_INTERP', $
           'TOO_FEW_GOOD_DETECTIONS', $
           'MEASURED', $ 
           'GROWN_MERGED', $ 
           'HAS_CENTER', $
           'MEASURE_BRIGHT']



  if keyword_set(objc) then BEGIN

      print,'Printing objc flags!'
      addstr=''
      f = long(pstruct(index).objc_flags)
      fstr = 'OBJC_'+fstr
      IF nw2 NE 0 THEN BEGIN 
          f2 = long(pstruct(index).objc_flags2)
          f2str = 'OBJC2_'+f2str
          doflags2=1
      ENDIF ELSE doflags2=0

  ENDIF ELSE BEGIN 
    
      addstr='['+ntostr(colorindex)+']'
      f=long(pstruct(index).flags(colorindex))
      fstr = 'OBJECT1_'+fstr
      IF nw1 NE 0 THEN BEGIN 
          f2str = 'OBJECT2_'+f2str
          f2=long(pstruct(index).flags2(colorindex))
          doflags2 = 1
      ENDIF ELSE doflags2=0
  ENDELSE 

  ;; flags

  print
  print,'flags'+addstr+' = ',ntostr(f)
  print,'------------------'
  for j=0,numflags-1 do begin
      h=long(2L^j)
      if ((f and h) ne 0) then begin
          print,fstr[j]+'  ',ntostr(j)
      endif
  endfor

  ;; flags2

  IF doflags2 THEN BEGIN 
      print
      print,'flags2'+addstr+' = ',ntostr(f2)
      print,'------------------'
      FOR j=0, numflags2-1 DO BEGIN 
          h=long(2L^j)
          if ((f2 and h) ne 0) then begin
              print,f2str[j]+'  ',ntostr(j)
          endif
      ENDFOR
  ENDIF 


return

end
