PRO ptime, t, num, tstr=tstr, savetime=savetime


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    sptime
;       
; PURPOSE: 
;    print a time in a hr min sec format. 
;	
; CALLING SEQUENCE: 
;    ptime, t [, num, tstr=tstr]
;      
;                 
; INPUTS: 
;    t: a time in seconds.
;
; OPTIONAL INPUTS: 
;    num: number of places to keep in seconds. Default is 4, 
;         which includes any decimal places.
;
; KEYWORD PARAMETERS:
;    /savetime: if this is set, then t is set to the current systime(1)
;               and returned without printing
;       
; OUTPUTS: 
;    prints the time.
;
; OPTIONAL OUTPUTS: 
;    tstr: a string containing the time in hr min sec format.
;
; CALLED ROUTINES:
;                NTOSTR
; 
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon UofM  7/8/99
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: ptime, t [, num, tstr=tstr, savetime=savetime]'
     print,''
     print,'Use doc_library,"ptime"  for more help.'  
     return
  ENDIF 

  IF keyword_set(savetime) THEN BEGIN
      t=systime(1)
      return
  ENDIF 

  IF n_elements(num) EQ 0 THEN num = 4
  tstr='Time: '
  IF t LE 60.0 THEN BEGIN
    tstr = tstr+ntostr(t,num)+' seconds'
    print,tstr
    return
  ENDIF 

  IF t LE 3600.0 THEN BEGIN
    min = long(t/60.0)
    sec = t-min*60.
    tstr = tstr+ntostr(min)+' min ' + ntostr(sec,num)+' sec'
    print,tstr
    return
  ENDIF 

  hr = long(t/3600.0)
  tmin = (t/3600.0 - hr)*60
  min = long(tmin)
  sec = (tmin-min)*60.
  
  tstr = tstr+ntostr(hr) + ' hours '+ntostr(min)+' min '+ntostr(sec,num)+' sec'
  print, tstr

  return
END
