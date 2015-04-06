; -----------------------------------------------------------------------------
;+
; NAME:
;     File (OBJECT)
;
; PURPOSE:
;     An object for filename manipulation.  
;
; CALLING SEQUENCE:
;     file = OBJ_NEW ('File' [, filename, /INTERACTIVE])
;
; INPUTS:
;     filename (OPTIONAL): Scalar filename, or STRARR of filenames
;
; KEYWORDS:
;     INTERACTIVE
;         Select filename(s) interactively when the object is 
;         instantiated.  If this keyword is set, any valid 
;         DIALOG_PICKFILE keywords may also be used.
;
; PUBLIC METHODS:
;     
;     set (PROCEDURE) - Set the filename
;         Inputs: Scalar STRING or STRARR containing the filename(s)
;        Outputs: NONE
;       Keywords: INTERACTIVE: Select filename(s) interactively. 
;                     If this keyword is set, any valid 
;                     DIALOG_PICKFILE keywords may also be used.
;
;                 ERROR: Set to a named variable to return 1 if the file
;                     was successfully set, else return 0 
;
;     get (FUNCTION) - Retrieve the filename properties
;         Inputs: NONE
;        Outputs: STRARR containing the requested property (DEFAULT = full name)
;       Keywords: 
;
;           VERBOSE: Print a warning if object filename has not yet been set.
;
;         * NOT ALL OF THE FOLLOWING KEYWORDS APPLY FOR ALL OS.  For example, 
;           DEVICE and VERSION do not apply under UNIX, and are returned as 
;           null.
;
;           BASENAME  : Filename with leading path removed  
;
;           PATH      : Filename path
;                       UNIX    : path + basename is the full filename
;                       VMS     : device + path + basename is the full filename
;                       WINDOWS : device + path + basename is the full filename
;                       MACOS   : device + path + basename is the full filename
;
;           DIRECTORY : Filename directory (same as PATH)
;
;           ROOT      : Filename with leading path and file extension removed.  
;                       An extension consists of all characters after and 
;                       including the last "."
;
;           EXTENSION : Filename extension.  An extension consists of all 
;                       characters after and including the last "."
;
;           DEVICE    : Filename device
;
;           NODE      : VMS only: The node name
;
;           VERSION   : VMS only: Filename version.  A version consists of 
;                       all characters after and including the last ";"
;
;     haveFile (FUNCTION) - Return 1 if a filename has been set, else 0
;         Inputs: NONE
;        Outputs: 0 or 1
;       Keywords: NONE
;
; EXAMPLE:
;
;     ;== Create an instance of a file object
;     
;     file = OBJ_NEW ('File', '/path/to/file/root.ext')
;
;     ;== Obtain some file properties
;     
;     basename = file->get (/BASENAME)      
;     path     = file->get (/PATH)          
;     root     = file->get (/ROOT)          
;     ext      = file->get (/EXTENSION)
;     version  = file->get (/VERSION)    ; null for UNIX
;
;     ;== All done
;     
;     OBJ_DESTROY, file
;
; MODIFICATION HISTORY:
;
;     RDP, 2002 Nov
;         Fixed INTERACTIVE keyword handling in INIT method
;
;     RSM, 2000 Mar
;         Added haveFile() method
;         Removed private method checking (problematic for inheritance)
;
;     Written, 1999 Jan, Robert.Mallozzi@msfc.nasa.gov
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION File::init, name, INTERACTIVE = interactive, _EXTRA = extra
    
    self.haveFile = 0
    
    IF (N_ELEMENTS (name) NE 0) THEN BEGIN

       self.haveFile = 1
       self.filename = PTR_NEW (name)

       self->breakFile

    ENDIF    

    IF (KEYWORD_SET (interactive)) THEN BEGIN
    
       self->set, /INTERACTIVE, ERROR = error, _EXTRA = extra
       self.haveFile = (error EQ 0) ? 1 : 0
              
    ENDIF
    
    RETURN, 1

END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO File::cleanup

    PTR_FREE, self.filename
    PTR_FREE, self.path
    PTR_FREE, self.device
    PTR_FREE, self.directory
    PTR_FREE, self.name
    PTR_FREE, self.extension
    PTR_FREE, self.version      
    PTR_FREE, self.node

END


; -----------------------------------------------------------------------------
; PRIVATE: Break the filename into its components
; -----------------------------------------------------------------------------
PRO File::breakFile

    ; COMPILE_OPT HIDDEN

    IF (NOT self.haveFile) THEN $
       RETURN
       
    file = *self.filename    
    n = N_ELEMENTS (file)

    ;== Some components are not applicable for all operating systems.
    ;== For example, DEV and VER do not apply for UNIX, and are set to null.
     
    IF (n EQ 1) THEN BEGIN
    
       pat = ''       
       dev = ''       
       dir = ''
       nam = ''
       ext = ''
       ver = ''       
       nod = ''

    ENDIF ELSE BEGIN
    
       pat = STRARR (n)        
       dev = STRARR (n)        
       dir = STRARR (n)
       nam = STRARR (n)
       ext = STRARR (n)
       ver = STRARR (n)        
       nod = STRARR (n)
   
    ENDELSE

    CASE (STRUPCASE (!VERSION.OS_FAMILY)) OF
    
       
        'UNIX': BEGIN

            FOR i = 0, n - 1 DO BEGIN
                
                f = file[i]

                key = STRPOS (f, ':')               
                IF (key NE -1) THEN BEGIN
                   
                   nod[i] = STRMID (f, 0, key + 1)                    
                   f = STRMID (f, key + 1)
                
                ENDIF

                key = RSTRPOS (f, '/')
                IF (key EQ -1) THEN BEGIN
                
                   nam[i] = f
                                      
                ENDIF ELSE BEGIN
                   
                   dir[i] = STRMID (f, 0, key + 1)
                   nam[i] = STRMID (f, key + 1)
                
                ENDELSE

                ;== Name and extension
                
                key = RSTRPOS (nam[i], '.')
                IF (key NE -1) THEN BEGIN
                
                   ext[i] = STRMID (nam[i], key)
                   nam[i] = STRMID (nam[i], 0, key)
                                      
                ENDIF

                pat[i] = dev[i] + dir[i]
                
            ENDFOR

            END ; UNIX
        

        'VMS': BEGIN
        
            FOR i = 0, n - 1 DO BEGIN
                
                f = file[i]

                key = STRPOS (f, '::')               
                IF (key NE -1) THEN BEGIN
                   
                   nod[i] = STRMID (f, 0, key + 2)                    
                   f = STRMID (f, key + 2)
                
                ENDIF
                
                key = STRPOS (f, ':')
                IF (key NE -1) THEN BEGIN

                   dev[i] = STRMID (f, 0, key + 1)

                   temp = STRMID (f, key + 1)
                   key = RSTRPOS (temp, ']')
                   dir[i] = STRMID (temp, 0, key + 1)                    
                   nam[i] = STRMID (temp, key + 1)

                ENDIF ELSE BEGIN

                   key = RSTRPOS (f, ']')
                   IF (key NE -1) THEN BEGIN

                      dir[i] = STRMID (f, 0, key + 1)
                      nam[i] = STRMID (f, key + 1)

                   ENDIF ELSE BEGIN

                      nam[i] = f 

                   ENDELSE

                ENDELSE
                
                ;== Extension
                
                key = RSTRPOS (nam[i], '.')                
                IF (key NE -1) THEN BEGIN

                   ext[i] = STRMID (nam[i], key)
                   nam[i] = STRMID (nam[i], 0, key)

                ENDIF

                ;== Version
                
                key = RSTRPOS (ext[i], ';')                
                IF (key NE -1) THEN BEGIN

                   ver[i] = STRMID (ext[i], key)
                   ext[i] = STRMID (ext[i], 0, key)

                ENDIF

                pat[i] = dev[i] + dir[i]

            ENDFOR
            
            END ; VMS
    

        'WINDOWS': BEGIN
        
            FOR i = 0, n - 1 DO BEGIN
                
                f = file[i]

                key = STRPOS (f, ':')               
                IF (key NE -1) THEN BEGIN
                   
                   dev[i] = STRMID (f, 0, key + 1)                    
                   f = STRMID (f, key + 1)
                
                ENDIF
                
                key = RSTRPOS (f, '\')
                IF (key EQ -1) THEN BEGIN
                
                   nam[i] = f
                                      
                ENDIF ELSE BEGIN
                   
                   dir[i] = STRMID (f, 0, key + 1)
                   nam[i] = STRMID (f, key + 1)
                
                ENDELSE

                ;== Name and extension
                
                key = RSTRPOS (nam[i], '.')
                IF (key NE -1) THEN BEGIN
                
                   ext[i] = STRMID (nam[i], key)
                   nam[i] = STRMID (nam[i], 0, key)
                                      
                ENDIF ELSE BEGIN
                   
                   ext[i] = ''
                
                ENDELSE

                pat[i] = dev[i] + dir[i]
                
            ENDFOR

            END ; WINDOWS
    

        'MACOS': BEGIN
            
            FOR i = 0, n - 1 DO BEGIN
                
                f = file[i]

                key = STRPOS (f, '::')               
                IF (key NE -1) THEN BEGIN
                   
                   dev[i] = STRMID (f, 0, key + 2)                    
                   f = STRMID (f, key + 2)
                
                ENDIF
                
                key = RSTRPOS (f, ':')
                IF (key EQ -1) THEN BEGIN
                
                   nam[i] = f
                                      
                ENDIF ELSE BEGIN
                   
                   dir[i] = STRMID (f, 0, key + 1)
                   nam[i] = STRMID (f, key + 1)
                
                ENDELSE

                ;== Name and extension
                
                key = RSTRPOS (nam[i], '.')
                IF (key NE -1) THEN BEGIN
                
                   ext[i] = STRMID (nam[i], key)
                   nam[i] = STRMID (nam[i], 0, key)
                                      
                ENDIF ELSE BEGIN
                   
                   ext[i] = ''
                
                ENDELSE

                pat[i] = dev[i] + dir[i]
                
            ENDFOR

            END ; MACOS

    
        ELSE: MESSAGE, 'Unsupported OS: ' + !VERSION.OS_FAMILY

    ENDCASE
     
    PTR_FREE, self.path      & self.path      = PTR_NEW (pat)
    PTR_FREE, self.device    & self.device    = PTR_NEW (dev)
    PTR_FREE, self.directory & self.directory = PTR_NEW (dir)
    PTR_FREE, self.name      & self.name      = PTR_NEW (nam)
    PTR_FREE, self.extension & self.extension = PTR_NEW (ext)
    PTR_FREE, self.version   & self.version   = PTR_NEW (ver)
    PTR_FREE, self.node      & self.node      = PTR_NEW (nod)


END


; -----------------------------------------------------------------------------
; Retrieve the filename properties
; -----------------------------------------------------------------------------
FUNCTION File::get, $
    
    BASENAME  = basename,  $
    DEVICE    = device,    $
    DIRECTORY = directory, $
    PATH      = path,      $
    ROOT      = root,      $
    EXTENSION = extension, $
    VERSION   = version,   $
    NODE      = node,      $
    VERBOSE   = verbose

    IF (NOT self.haveFile) THEN BEGIN
       IF (KEYWORD_SET (verbose)) THEN $
          MESSAGE, /CONTINUE, 'Use SET method to set the filename.'
       RETURN, ''
    ENDIF

    CASE (1) OF
    
        KEYWORD_SET (basename): $
            RETURN, *self.name + *self.extension + *self.version

        KEYWORD_SET (path): $
            RETURN, *self.path

        KEYWORD_SET (root): $
            RETURN, *self.name

        KEYWORD_SET (extension): $
            RETURN, *self.extension

        KEYWORD_SET (device): $
            RETURN, *self.device
        
        KEYWORD_SET (directory): $
            RETURN, *self.directory

        KEYWORD_SET (version): $
            RETURN, *self.version

        KEYWORD_SET (node): $
            RETURN, *self.node

        ELSE : RETURN, *self.filename
    
    ENDCASE
    
END


; -----------------------------------------------------------------------------
; Set the filename data
; -----------------------------------------------------------------------------
PRO File::set, name, INTERACTIVE = interactive, ERROR = error, _EXTRA = extra
    
    error = 0

    IF ((N_PARAMS () LT 1) AND (NOT KEYWORD_SET (interactive))) THEN BEGIN

       MESSAGE, /CONTINUE, 'Missing parameter: NAME.'
       error = 1
       RETURN

    ENDIF
	
	; It could be that the user is calling the oject directly, !MFIT is not defined:
	script = 0
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		MFIT = { MFIT_SYSTEM, $
		
			PARAM_FILE : '', $
			SCRIPT_FILE : '', $
			INFO_FILE : '', $
			HELP_PATH : ''     $
			
		}    
		READ_ONLY = 1
		DEFSYSV, '!MFIT', MFIT, READ_ONLY
		CATCH, /CANCEL 
	ENDIF
    
    IF (!MFIT.script_file NE '') THEN RESTORE, !MFIT.script_file                                                                                             ; AMG

	
    IF script THEN name=rspFile[phaNum-1] ELSE BEGIN                                               ;
    IF (KEYWORD_SET (interactive)) THEN BEGIN

       name = DIALOG_PICKFILE (_EXTRA = extra)
       IF (name[0] EQ '') THEN BEGIN
          error = 1
          RETURN
       ENDIF
       ; User may have slipped up! Filename is only a path:
       IF ((STRPOS(name, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(name)) THEN BEGIN
          error = 1
          RETURN
       ENDIF
       ; If the user accidently puts a space in front of the filename...
       retpos = STRPOS(name, STRING(10B))
       IF (retpos NE 0) THEN BEGIN
          name = STRMID(name, retpos + 1)
       ENDIF

    ENDIF
  ENDELSE

    self.haveFile = 1
    
    PTR_FREE, self.filename
    self.filename = N_ELEMENTS (name) EQ 1 ? PTR_NEW (name[0]) : PTR_NEW (name)

    self->breakFile
    
END


; -----------------------------------------------------------------------------
; Query if filename has been set
; -----------------------------------------------------------------------------
FUNCTION File::haveFile & RETURN, self.haveFile & END


; -----------------------------------------------------------------------------
; Object that stores a filename
; -----------------------------------------------------------------------------
PRO File__define

    obj = { FILE, $

        haveFile  : 0,          $        

        filename  : PTR_NEW (), $        
        path      : PTR_NEW (), $         
        device    : PTR_NEW (), $
        directory : PTR_NEW (), $
        name      : PTR_NEW (), $
        extension : PTR_NEW (), $
        version   : PTR_NEW (), $        
        node      : PTR_NEW ()  $        

    }

END
