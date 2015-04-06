;+
; NAME:
;      READ_STRUCT
; PURPOSE:
;      Quickly read a numeric ASCII data file into an IDL structure.  
;
; EXPLANATION:
;      Columns of data may be separated by commas or spaces. This 
;      program is fast but is restricted to numerical data only.  Use READCOL 
;      if greater flexibility is desired.   Use READFMT to read a fixed-format 
;      ASCII file.
;
;      This is a hacked version of RDFLOAT. This program has the advantage
;      that it can read any number of columns, the numbers in each
;      column may be of different type, and no extra memory usage is required
;      in addition to the structure itself.  If the column types do not match
;      that in the structure tags, then they are converted.
;
; CALLING SEQUENCE:
;      READ_STRUCT, file, use_struct, outstruct,
;           SKIPLINE = ,ENDSKIP=, NUMLINE= , NLINES=, /silent]
;
; INPUTS:
;      file - Name of ASCII data file, scalar string.  In VMS, an extension of 
;              .DAT is assumed, if not supplied.
;      use_struct: a single struct which will be replicated and returned
;                  with the data. The order of tags should equal the order
;                  of the columns in the data file. This can include arrays as
;                  long as they match up with the columns. No strings are
;                  allowed.
;
; OPTIONAL INPUTS:
;      SKIPLINE - Integer scalar specifying number of lines to skip at the top
;              of file before reading.   Default is to start at the first line.
;      ENDSKIP - same but at the end of the file.
;      NUMLINE - Integer scalar specifying number of lines in the file to read.
;             Default is to read the entire file
;      NLINES - how many lines are actually in the file. Saves time since no
;      call to the numlines procedure is required.
;
; KEYWORD PARAMETERS:
;
;      /silent: if set, nothing is printed to STDOUT
;
; OUTPUTS:
;     outstruct: the output structure. It is an array of structures, each
;                element is identical to the input use_struct. the number
;                of array elements is the number of lined of data in the
;                file. 
;
;
; RESTRICTIONS:
;      Cannot be used to read strings.
;
; EXAMPLES:
;   ;; Read a 5 column file into a structure.
;          0      0.0749063     85.3804855     15.8031492
;          1      0.3044448     16.1201153     70.0963593
;          2      0.8440148     36.8116074     93.5149231
;          .......
;
;   IDL> use_struct = create_struct('a',0L,'b',0.0,'c',fltarr(2))
;   IDL> read_struct, 'test.dat', use_struct, outstruct
;   Reading 20 lines from file test.dat
;   IDL> help, outstruct, /str
;   ** Structure <8277fd4>, 3 tags, length=16, data length=16, refs=1:
;      A               LONG                 0
;      B               FLOAT         0.0749063
;      C               FLOAT     Array[2]
;
;
; CALLED FUNCTIONS:
;      NUMLINES()
;
; REVISION HISTORY:
;      Converted from RDFLOAT.PRO to read into a structure 
;                                   Erin Scott Sheldon 07-Mar-2001
;
;-

PRO read_struct, name, use_struct, outstruct,$
                 SKIPLINE = skipline, $
                 ENDSKIP = endskip, $
                 NUMLINE = numline, $
                 NLINES = nlines, $
                 silent=silent


  On_error,2                           ;Return to caller
  
  if N_params() lt 2 then begin
      print,'Syntax - read_struct, file, use_struct, outstruct [, '
      print,'                      SKIPLINE=, ENDSKIP=, '
      print,'                      NLINES=, NUMLINE =, /silent]'
      return
  endif
    
  ;; Get number of lines in file
  IF n_elements(nlines) EQ 0 THEN nlines = NUMLINES( name )
  if nlines LT 0 then return
  
  ;; Should we skip lines at the top?
  if not keyword_set( SKIPLINE ) then skipline = 0
  nlines = nlines - skipline

  ;; Skip lines at the end?
  IF n_elements(endskip) NE 0 THEN nlines = nlines-endskip

  ;; How many lines to actually read
  if keyword_set( NUMLINE ) then nlines = numline < nlines
    
  ;; open file and skip lines
  openr, lun, name, /get_lun
  temp = ' '
  if skipline GT 0 then $
    for i=0,skipline-1 do readf, lun, temp
  
  IF NOT keyword_set(silent) THEN $
    print,'Reading ',ntostr(nlines),' lines from file ',name
  
  ;; replicate the structure and read it in
  outstruct = replicate(use_struct, nlines)
  readf, lun, outstruct

  ;; close the fil
  free_lun, lun
  
  return
end
