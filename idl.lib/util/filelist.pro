; Copyright(c) 1992, CreaSo Creative Software Systems GmbH. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	FILELIST
;
; PURPOSE:
;    This routine finds the files or directories at the current directory level.
;    It must be called with either files or directories as a keyword.
;
; CALLING SEQUENCE:
;	FILELIST
;
; INPUTS:
;       None 
;
; KEYWORDS:
;       PATH        - path in which programm looks for
;       FILES       - List of found files 
;       DIRECTORIES - List of found directories 
;       FILTER      - List of filters
;       USEDPATH    - path in which rocedure looks for. It could be that 
;                      keyword path is "[-]" and than it could be useful to 
;                      know the used path
;       ERROR       -  if set, than error message will not be send variable will
;                      set as follows : 
;                          0 - normal successful completion
;                          1 - directory not found
;
; OUTPUT: 
;       List of found files.
;
; COMMON BLOCKS:
;	None
;
; SIDE EFFECTS:
;       No known side effects.
;
; RESTRICTIONS:
;	For VAX/VMS only.
;
; EXAMPLE:
;      filelist, path = "[here]",$
;                FILES       = files, $
;                DIRECTORIES = DIRECTORIES, $
;                FILTER = "*.c"
;
;      Looks for C source code in the directories [here]
;      returns the file list in files and the directory list in directories.
;
; MODIFICATION HISTORY:
;	July 1992, AH,	CreaSo		Created.
;-
pro filelist, path        = path, $
	      filter      = filter, $
	      files       = files, $
              usedpath    = usedpath,$
	      directories = directories, $
              message     = message, $
              error       = error

   ;Inialize variables

   error = 0
   files   = [""]
   directories = [""]

   ;a: Initialize filter value, if necessary

   if (n_elements(filter)) then                             $
      if (filter eq '*') or (filter eq '') then fil = "*.*" $
      else fil = strupcase(filter)                          $
   else                                                     $
      fil = '*.*'

   ;a: Initialize path value

   if (keyword_set(path)) then  $
      upath = strcompress(path) $
   else                         $
      cd, current=upath         

   ;a: Get current path

   cd, current=curpath

   if keyword_set(usedpath) then changedir, usedpath, message=message

   ;a: Change directory

   changedir, upath, error=error, message=message

   if error ne -1 then begin

      filescan, upath, path=usedpath, dir=dir, dev=dev, node=node, /full

      ;Look for files.
 
      results  = findfile (fil)
      if fil ne '*.*;*' and fil ne '*.*' and fil ne '*.dir' then $
         results = [results, findfile ("*.dir")]

      filled = where (results ne '', found)
      if found ne 0 then results = results(filled)

      numfound = n_elements (results)
      types    = bytarr(numfound)
 
      ;Mark dierectories
 
      for i = 0, numfound - 1 do begin
         results(i) = strmid (results(i), $
                         strpos(results(i), ']')+1, strlen(results(i)))
         found = strpos(results(i), ".DIR", 0)
         if (found EQ -1) then types(i) = 2 else types(i) = 1
      endfor
 
      fileindices = where(types EQ 2, found)
      if (found NE 0) then $
         files = results(fileindices)
      dirindices = where(types EQ 1, found)
      if (found NE 0) then $
         directories = results(dirindices)
   endif

   cd, curpath
   return	   

END
