PRO run_find_errors,infile,ofile,bestfit=bestfit

  IF n_elements(infile) EQ 0 THEN infile='xspec_errors2.log'
  IF n_elements(ofile) EQ 0 THEN ofile='xspec_errors_output.txt'
print,infile
  openr,flun,infile,/get_lun
  openw,olun,ofile,/get_lun

  line=''
  lines=''
  noread=0
  i=0
  WHILE NOT eof(flun) DO BEGIN
      IF NOT noread THEN readf,flun,line
      noread=0
      lines=[lines,line]

      IF (strpos(line,'XSPEC') NE -1) THEN BEGIN 
          printf,olun,line
          print,line
      ENDIF 

      if (strpos(line,'    Chi-Squared      ') ne -1) then begin 
          openw,tlun,'tmp.txt',/get_lun
          WHILE (NOT eof(flun)) AND (strpos(line,'XSPEC') EQ -1) DO BEGIN 
              readf,flun,line
              IF (strpos(line,'XSPEC') EQ -1) THEN BEGIN 
                  IF line NE '' THEN printf,tlun,line
                  noread=1
              ENDIF 
          ENDWHILE 
          close,tlun
          free_lun,tlun
          IF n_elements(bestfit) NE 0 THEN bfit=bestfit[i] ELSE bfit=0.
          find_errors,'tmp.txt',bfit,err
          k=get_kbrd(10)
          printf,olun,bfit,err
          i=i+1
      ENDIF 
     
  ENDWHILE

  close,/all
  free_lun,flun
  free_lun,olun

  spawn,'rm tmp.txt'

  return
END 
