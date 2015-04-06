pro which,proc_name,files=files,show=show,help=help, silent=silent
;+
; NAME:
;	WHICH
;
; PURPOSE:
;	Determine in which library/directory the procedure or function
;	specified is located in the !PATH.  This is useful for finding
;	out which library a certain procedure comes from, particularly
;	when there are duplicates.  This is similar to the unix
;	'which' command.
;
; CALLING SEQUENCE:
;    WHICH, [ proc_name ]          ;Find PROC_NAME in !PATH and display
;
; OPTIONAL INPUT:
;	proc_name - Character string giving the name of the IDL procedure or 
;		function.  Do not give an extension.   If omitted, 
;		the program will prompt for PROC_NAME.
;       /show: if set, will show the user the first file found
;       /help: print syntax
;
; OUTPUTS:
;	files: An array containing the filenames
;
; SIDE EFFECTS
;	None.
;
; PROCEDURE:
;	The system variable !PATH is parsed into individual libraries or 
;	directories.   Each library or directory is then searched for the
;	procedure name.  
;
; EXAMPLE:
;	Find out where the procedure CURVEFIT lives.
;
;	IDL> which, 'curvefit'
;
; RESTRICTIONS:
;	None.
;
; REVISION HISTORY:
;	29-MAY-94  Modified from getpro.pro by E. Deutsch
;	14-JUL-95  Fixed for IDL 4.0
;       03-DEC-2000 Added files and show keywords. Erin Scott Sheldon
;       21-JUN-2004 Use FILE_WHICH procedure for IDL > 5.3  for significant
;             speed increase. Fixed intrinsic procedure searching. E.S.S.
;-

  IF keyword_set(help) THEN BEGIN
      print,'-Syntax: which, proc_name, files=files, show=show, help=help'
      return
  ENDIF 

  On_error,2                    ;Return to caller on error

  COMMON which_block, funcnames, pronames

  ;; VMS or Unix operating system
  os = !VERSION.OS                     

  ;; Can use faster searching on 5.4
  IDLversion = float(!version.release)

  ;; Prompt for procedure name?
  if (n_params() eq 0) then begin 	     
      proc_name = ' ' 
      read,'Enter name of procedure to look for: ',proc_name     
  endif else zparcheck, 'which', proc_name, 1, 7, 0, 'Procedure name'

  ;; Don't want file extensions

  fdecomp, proc_name, disk, dir, name      
  name = strtrim( name, 2 )  

  ;; Set up separate copy commands for VMS and Unix

  if (os eq "vms") then begin   
      sep = ',' & dirsep = '' & name = strupcase(name)
  endif else begin
      sep = ':' & dirsep = '/'
  endelse   

  ;; Get current IDL path of directories
  
  temp = !PATH                     
  if (os eq "vms") then temp = strupcase(temp)
  

  ;; Loop over each directory in !PATH until procedure name found
  delvarx, files
  
  found=0
  while (temp ne '') do begin   
      dir = gettok( temp, sep)
      
      if strmid(dir,0,1) EQ '@' then begin ;Text Library?
          if (os ne "vms") then message, $
            '!path contains a invalid VMS directory specification',/cont $
          else begin
              libname = strmid( dir,1,strlen(dir)-1 ) ;Remove the "@" symbol
              spawn,'library/extract='+name+'/out='+name+'.pro '+$
                libname,out,count=i
              if (i eq 0) then begin ;Success?
                  message,name + '.PRO extracted from ' + libname,/INF
                  return
              endif
          endelse
      endif else begin          ;Directory

          IF IDLversion LT 5.4 THEN BEGIN 
              ;; Old way
              a = findfile(dir + dirsep + name+'.pro',COUNT=i)
              if (I ge 1) then begin ;Found by FINDFILE?
                  filename = dir+dirsep+name+'.pro'
                  add_arrval, filename, files
                  
                  IF NOT keyword_set(silent) THEN BEGIN 
                      if (found eq 0) then print,'Using: '+filename
                      if (found eq 1) then print,'Also in: '+filename
                  ENDIF 
                  found=1
              ENDIF
          ENDIF ELSE BEGIN 
              ;; New way: E.S.S.
              fileName = FILE_WHICH(dir, proc_name+'.pro')
              IF fileName NE '' THEN BEGIN 

                  IF NOT keyword_set(silent) THEN BEGIN 
                      if (found eq 0) then print,'Using: ',filename
                      if (found eq 1) then print,'Also in: ',filename
                  ENDIF 
                  found=1
                  add_arrval, filename, files

              ENDIF 
          ENDELSE 

      endelse
  endwhile
  
  IF (found EQ 1) THEN BEGIN
      IF keyword_set(show) THEN spawn,'more '+files[0]
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The search failed. Check for intrinsic IDL procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF n_elements(funcNames) EQ 0 THEN BEGIN 
      proNames  = routine_info(/system)
      funcNames = routine_info(/system,/func)
  ENDIF 

  uname = strupcase(name)
  funcTest = where (funcNames EQ uname, fcount)
  proTest  = where (proNames  EQ uname, pcount)
  
  IF (fcount EQ 0) AND (pcount EQ 0) THEN BEGIN

      ;; Not found
      IF NOT keyword_set(silent) THEN BEGIN 
          print, 'Procedure '+Name+' not found in a !PATH directory.'
          print, 'Check your spelling or search individual directories.'
      ENDIF 

  ENDIF ELSE begin 
      files = 'INTRINSIC'

      ;; Its either a built in function or procedure
      IF NOT keyword_set(silent) THEN BEGIN 

          IF fCount NE 0 THEN BEGIN 
              print, 'Function ' + Name + ' is an intrinsic IDL Function.'
              print, 'No path information available.'
          ENDIF ELSE BEGIN 
              print, 'Procedure ' + Name + ' is an intrinsic IDL procedure.'
              print, 'No path information available.'
          ENDELSE 
      ENDIF 

  ENDELSE 

  return

end 
  
