; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;+
;
; NAME:
;     xvarexport
;
; PURPOSE:
;     This widget allows the export of user-selected variables 
;     to a IDL SAVE file.  It could be used, for example, in a widget 
;     program to allow users to save arbitrary data.
;
;     NOTE: it requires input data in a very specific format.  See the EXAMPLE.
;
; TYPE:
;     PROCEDURE     
;
; CATEGORY:
;     Widgets
;
; CALLING SEQUENCE:
;     XVAREXPORT, inputData
;
; INPUTS:
;
;     inputData : array of structures of input data that will be available
;          for saving.  The structures must be of the following form:
;
;          inputData = { name: '', data: PTR_NEW () }
;          
;          inputData.name : STRING denoting the name of the data variable 
;                           to store in the SAV file
;          inputData.data : POINTER to the data to store in the SAV file. 
;                           The first level pointer will be removed before
;                           saving.  For example, if you want to save the
;                           variable X = 10, inputData.name should be set
;                           to 'x', and inputData.data should be set to
;                           PTR_NEW (x).  When the SAV file is restored, the
;                           variable X = 10 will be restored.
;
; KEYWORDS:
;     
;     GROUP_LEADER  : The group leader for the XVAREXPORT widget.  See 
;                     WIDGET_BASE for an explanation of GROUP_LEADER.
;     VISIBLE       : Set this keyword to an integer to specify the number 
;                     of data items visible without scrolling.
;                     Default = MIN (number of items, 10)  
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     Starts XMANAGER if it is not already running
;
; RESTRICTIONS:
;     None known
;
; DEPENDENCIES:
;     NONE
;
; EXAMPLE:
;
;     x = 10
;     y = 20.0
;     z = PTR_NEW (INDGEN (10, /FLOAT))
;     
;     ; Set up the input structure required by XVAREXPORT
;     ; 
;     input = REPLICATE ({ name: '', data: PTR_NEW() }, 3) 
;     
;     input[0].name = 'x' & input[0].data = PTR_NEW (x)
;     input[1].name = 'y' & input[1].data = PTR_NEW (y)
;     input[2].name = 'z' & input[2].data = PTR_NEW (z)
; 
;     ; Note that the pointers allocated for the input structure will be
;     ; automatically freed by XVAREXPORT
;     ;
;     XVAREXPORT, input
;
;
; MODIFICATION HISTORY:
;
;     1.1 : RDP (02/23/01) Must use /WRITE for std file dialog on Mac OS
;     1.0 : Written, 1999 February, Robert.Mallozzi@msfc.nasa.gov
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 



;------------------------------------------------------------------------------
; Event handler
;------------------------------------------------------------------------------
PRO XVAREXPORT_EVENT, event


    WIDGET_CONTROL, event.top, GET_UVALUE = entry
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF
            
                'CANCEL': BEGIN
                    FOR i = 0, N_ELEMENTS (*entry) - 1 DO $
                        PTR_FREE, (*entry).data 
                    PTR_FREE, entry 
                    WIDGET_CONTROL, event.top, /DESTROY
                    END
            
                'Export': BEGIN

                    index = WHERE ((*entry).selected EQ 1, numSelected)
                    IF (numSelected EQ 0) THEN $
                       RETURN

                    ; Get SAV filename
                    ;                                           
                    file = DIALOG_PICKFILE ( $
                        FILE = 'varexport.sav', FILTER = '*.sav', $
                        TITLE = 'Select Export Filename', $
                        DIALOG_PARENT = event.top, /WRITE)
                    IF (file EQ '') THEN $
                       RETURN

                    s = FINDFILE (file, COUNT = found)
                    IF (found NE 0) THEN BEGIN
                       IF (DIALOG_MESSAGE (/QUESTION, ['File exists', file, $
                          'Ok to overwrite?'], DIALOG_PARENT = event.top) $
                          NE 'Yes') THEN $
                          RETURN
                    ENDIF      
                    
                    ; Copy data to local variables
                    ;
                    name = (*entry)[index].name
                    data = (*entry)[index].data
                   
                    FOR i = 0, numSelected - 1 DO BEGIN
                        command = name[i] + ' = *data[' + STRTRIM (i, 2) + ']' 
                        IF (NOT EXECUTE (command)) THEN $
                           MESSAGE, 'EXECUTE failed.'        
                    ENDFOR
                    
                    ; Build the command string
                    ; 
                    s = ''
                    FOR i = 0, numSelected - 1 DO BEGIN
                        IF (i LT numSelected - 1) THEN BEGIN
                           s = s + name[i] + ', '
                        ENDIF ELSE BEGIN
                           s = s + name[i]
                        ENDELSE
                    ENDFOR    
                    
                    ; Create the SAV file
                    ;
                    command = 'SAVE, FILENAME = "' + file + '", ' + s
                    IF (NOT EXECUTE (command)) THEN $
                       MESSAGE, 'EXECUTE failed.'        
       
                    ; Cleanup
                    ;
                    FOR i = 0, N_ELEMENTS (*entry) - 1 DO $
                        PTR_FREE, (*entry).data 
                    PTR_FREE, entry 
                    WIDGET_CONTROL, event.top, /DESTROY
                    END

            ENDCASE
            END
    
        'WIDGET_LIST': BEGIN
            (*entry).selected = (*entry).selected * 0
            index = WIDGET_INFO (event.id, /LIST_SELECT)
            (*entry)[index].selected = 1
            END
            
        ELSE: ; event not handled
    
    ENDCASE


END


;------------------------------------------------------------------------------
; GUI
;------------------------------------------------------------------------------
PRO XVAREXPORT, input, GROUP_LEADER = group_leader, VISIBLE = visible


    ; ----- Verify input
  
    IF (N_PARAMS() LT 1) THEN $
       MESSAGE, 'Incorrect number of arguments.'
    
    IF (STRUPCASE (SIZE (input, /TNAME)) NE 'STRUCT') THEN $
       MESSAGE, 'Input must be of type STRUCT.'
    
    IF (N_TAGS (input) NE 2) THEN $
       MESSAGE, 'Input STRUCT has incorrect number of tags.'

    tags = STRUPCASE (TAG_NAMES (input))
    IF ((tags[0] NE 'NAME') AND (tags[1] NE 'NAME')) THEN $
       MESSAGE, 'Input STRUCT has missing tag: NAME.'
    IF ((tags[0] NE 'DATA') AND (tags[1] NE 'DATA')) THEN $
       MESSAGE, 'Input STRUCT has missing tag: DATA.'
    

    ; ----- GUI code

    numRequests = N_ELEMENTS (input)

    numVisible = MIN ([10, numRequests])
    IF (N_ELEMENTS (visible) NE 0) THEN $
       numVisible = FIX (visible)

    entry = { $

        name : '', $
        data : PTR_NEW (), $

        buttonID : 0L, $    
        selected : 0   $
    
    }    
    entry = REPLICATE (entry,  numRequests)
    STRUCT_ASSIGN, input, entry
    
    ; Build the list strings
    ;
    sarr = ''
    FOR i = 0, numRequests - 1 DO BEGIN

        command = 'HELP, *input[' + STRTRIM (i, 2) + '].data, OUTPUT = s'
        IF (NOT EXECUTE (command)) THEN $
           MESSAGE, 'EXECUTE failed.'        
        
        s = STRCOMPRESS (s[0]) 
        sarr = [sarr, STRMID (s, STRPOS (s, ' '))]

    ENDFOR
    sarr = sarr[1:*]
    
    maxwidth = MAX (STRLEN (entry.name)) + 3                  
    
    FOR i = 0, numRequests - 1 DO BEGIN        
        pad = STRING (REPLICATE (32B, maxwidth - STRLEN (entry[i].name)))
        sarr[i] = entry[i].name + pad + sarr[i]
    ENDFOR

    topBase = WIDGET_BASE (/COLUMN, TITLE = 'XVarExport')
    
    w = WIDGET_LABEL (topBase, VALUE = 'Select variables for export')
    w = WIDGET_LIST (topBase, VALUE = sarr, /MULTIPLE, $
        YSIZE = numVisible, FONT = 'fixed')
    w = WIDGET_BUTTON (topBase, VALUE = 'Export')
    w = WIDGET_BUTTON (topBase, VALUE = 'CANCEL')

    WIDGET_CONTROL, topBase, SET_UVALUE = PTR_NEW (entry), /REALIZE
            
    ; Event loop
    ;
    XMANAGER, 'XVAREXPORT', topBase, /NO_BLOCK
 
    
END
