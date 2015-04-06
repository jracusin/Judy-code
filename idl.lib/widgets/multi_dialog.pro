;
;+
; NAME:
;
;       MULTI_DIALOG
;
; PURPOSE:
;
;       MULTI_DIALOG can easily display a popup widget dialog to get any kind
;		of user input or to give textual information.
;		Arrays, structures and arrays of structures are accepted as input.
;
;		It can make dialog widgets with input fields, with exclusive or
;		not exclusive check buttons, with file or directory fileds and browsing
;		or with text information and can manage dialogs with a large number of
;		input items by automatically adding NEXT/PREVIOUS buttons
;		into a multi-page layout.
;
;		It also automatically fits the widget size to both the iput items and
;		the title of the widget, and always keeps the widget within the screen area.
;
;		More, MULTI_DIALOG can manage multiple simultaneous dialogs, in a way
;		fully transparent to the user. In fact each time the funciton is called
;		in non-blocking mode, via the /NO_BLOCK keyword, a new dialog widget
;		is created while any already opened non-blocking dialog remains active
;		and can get user input.
;
;
; CATEGORY:
;
;       Widgets.
;
; CALLING SEQUENCE:
;
;		Result = MULTI_DIALOG(Title=Title, Label=Label, Value=Value, Return_index=Return_index, $
;				Field=Field, File=File, Directory=Directory, Check_button=Check_button, Message=Message, $
;				Exclusive=Exclusive, All_button=All_button, None_button=None_button, $
;				Rows=Rows, Columns=Columns, Max_Ysize=Max_Ysize, Max_Xsize=Max_Xsize, $
;				CENTER=CENTER, NO_FRAME=NO_FRAME, POS=POS, NO_BLOCK=NO_BLOCK, GROUP=GROUP)
;
;
; KEYWORD PARAMETERS:
;
;		TITLE: The title of the popup widget.
;
;       LABEL: The array of iput labels.
;
;       VALUE: The initial values of the iputs: 1-D array, structure or 1-D array of structures.
;				Values can be any type, but if /FIELD is set, the type BYTE is used as boolean type as follows:
;				0b -> FALSE, 1b to 255b -> TRUE, and is displayed as "NO" or "YES"; conversely,
;				0b or 1b is returned in the Result if the user writes "NO" or "YES".
;
;       RETURN_INDEX: If /CHECK_BUTTON is selected, set this keyword to have as Result the indexes of selected items.
;
;
;
;		One of the followin keyword MUST be set:
;
;		FIELD: Get user input, using CW_FIELD.
;
;		FILE: Get file selection user input by also oofering a BROWSE button for each item.
;
;		DIRECTORY: The same f FILE but only allow to select directories.
;
;		CHECK_BUTTON: Get item selection by toggle buttons; set the /EXCLUSIVE keyword to obtain an exclusive selection.
;
;		MESSAGE: 1-D string array to be displayed. Expecially useful with /NO_BLOCK to display multiple long information.
;
;
;
;		EXCLUSIVE: If /CHECK_BUTTON is selected, shows exclusive toggle buttons.
;
;		ALL_BUTTON: If /CHECK_BUTTON is selected but EXCLUSIVE is not, add an "ALL" button to select all the items at once.
;
;		NONE_BUTTON: If /CHECK_BUTTON is selected but EXCLUSIVE is not, add a "NONE" button to un-select all the items at once.
;
;
;
;		Multi_Dialog always fits the widget automatically and place it at the upper left corner of the screen,
;		but the user can control this by the following keywords:
;
;		ROWS: Number of item rows simultaneously displayed on the screen.
;				If the number of rows is high and the widget is to becaming
;				larger than the screen size, Multi_Dialog automatically place a scroll bar.
;				The number of rows can be less than the number of input items, in this case
;				a multi-page widget is created and the NEXT/PREVIOUS buttons are automatically added.
;
;		COLUMNS: Number of item columns simultaneously displayed on the screen. Default is 1 column.
;				If the number of columns is high and the widget is to becaming
;				larger than the screen size, Multi_Dialog automatically place a scroll bar.
;				The number of columns times the number of rows can be less than the number
;				of input items, in this case a multi-page widget is created and
;				the NEXT/PREVIOUS buttons are automatically added.
;
;		MAX_YSIZE: Set the maximum allowed vertical dimension of the widget, in units of screen size (default is 0.75).
;				If this size is not enough, a multi-page widget is created and the NEXT/PREVIOUS
;				buttons are automatically added
;
;		MAX_XSIZE: Set the maximum allowed horizontal dimension of the widget, in units of screen size (default is 0.35).
;				If this size is not enough, a multi-page widget is created and the NEXT/PREVIOUS
;				buttons are automatically added
;
;		CENTER: IF MESSAGE keyword is set, it cause the text to be center justified (default is left justification).
;
;		NO_FRAME: Avoid the default to draw a frame around the items box.
;
;		POS: Two element vector of the form [X position, Y position] to place the widget on the screen
;				from the upper left corner (pixel units).
;
;		NO_BLOCK: Avoid the default to block the execution until the dialog is closed.
;				Set this keyword if multiple simultaneous dialogs or long standing informational widgets are desired.
;
;		GROUP: Group leader keyword.
;
;
; OUTPUTS:
;
;       The function returns the user input in the same type of the VALUE keyword.
;		If MESSAGE is set, it returns -1.
;
;
;
; PROCEDURE:
;
;		The function keeps track of the data belonging to simultaneous dialogs by building
;		an internal structure which is referenced by a pointer vector, whose elements are
;		placed in the same order in which widgets are created. The event handler
;		dereferences the correct version of the structure depending on which widget
;		is involved by the current event.
;
;		Cause the widget_base function does not always fit its title and content adequately,
;		the title and one item is first drawn, not mapped, to measure widget geometry,
;		then the complete widget is created with the correct size.
;
;
; EXAMPLES:
;
;		- Allow the user to edit a structure:
;
;			param = {a: 0., b='hello', c='YES'}
;
;			tags = TAG_NAMES(param)
;
;			new_param = MULTI_DIALOG(/FIELD, VALUE=param, LABEL=tags, TITLE='ALGORITHMS PARAMETERS:')
;
;
;		- Allow the user to select one or more of 30 items, ordered in 5 columns:
;
;			fibers = intarr(30)
;
;			selected = MULTI_DIALOG(TITLE='Select the fibers:', VALUE=fibers, $
;					/CHECK_BUTTON, LABEL='Fiber ' + string(indgen(30)), $
;					/ALL_BUTTON, /NONE_BUTTON, COLUMNS=5)
;
;		- Creates and manage three long informational widgets simultaneously:
;
;			message1 = string(indgen(100))
;			message2 = string(findgen(1000))
;			message3 = 'Row: ' + string(indgen(500))
;
;			a = MULTI_DIALOG(MESSAGE=message1, ROWS=10, /NO_BLOCK)
;			b = MULTI_DIALOG(MESSAGE=message2, ROWS=15, /NO_BLOCK, POS=[350,0])
;			c = MULTI_DIALOG(MESSAGE=message3, ROWS=20, /NO_BLOCK, POS=[700,0])
;
;
; MODIFICATION HISTORY:
;
;       This routine is a modification of the Dialog routine by David L. Windt, Bell Labs, May 1997
;
;       Feb 2004 - 	Gianluca Li Causi, INAF - Rome Astronomical Observatory
;					licausi@mporzio.astro.it
;					http://www.mporzio.astro.it/~licausi/
;
;		Planned improvements:
;			- Output the pointer vector to the results if multiple dialog.
;			- Try to run when value is undefined.
;			- Automatic LABEL=TAG_NAMES(VALUE) when VALUE is structure and LABEL is not defined
;			- Update POS when user move the widget
;			- Forbid user resize and destroy of widgets
;			- String type POS keyword e.g. POS='bottom_right", etc.
;			- Add LIST and SLIDER input type
;			- Compact the source code
;
;-






FUNCTION ___String_to_Long, stringa

;Converts all the numeric characters in a string into a long variable..

IF strlen(stringa) EQ 0 THEN RETURN, 0

new_stringa = ''
FOR i = 0, strlen(stringa)-1 DO BEGIN
	char = strmid(stringa, i, 1)
	IF byte(char) GE 48 AND byte(char) LE 57 THEN new_stringa = new_stringa + char
ENDFOR


RETURN, long(new_stringa)

END





;***************************
;* EVENT HANDLER PROCEDURE *
;***************************
PRO Multi_Dialog_event,event

COMMON Multi_Dialog_data, Base, selection, fieldid, listid, browseid, check_button_id, Riquadro, dir_only
COMMON Multi_Dialog_browsing, browse_position, group_n_items, n_items, ntags, tipo, extra

COMMON Multi_Dialog_handling, vct_base, ptr_Multi_Handling, dialog_index

on_error,0


;*********************************
;Manage simultaneous MultiDialogs:
;*********************************
;Find the dialog index which corresponds to the event.top value;
;Dereference the corresponding Multi_Handling pointer;
;Reset the COMMON variables to the current Multi_Handling tags.

dialog_index = WHERE(event.top EQ vct_base)					;Index of the currently handled dialog
Multi_Handling = *(ptr_Multi_Handling[dialog_index[0]])		;Multi_Handling structure corresponding to the current dialog

Multi_Handling.Base = event.top

Base = Multi_Handling.Base
selection = Multi_Handling.selection
Riquadro = Multi_Handling.Riquadro
fieldid = Multi_Handling.fieldid
browseid = Multi_Handling.browseid
check_button_id = Multi_Handling.check_button_id
browse_position = Multi_Handling.browse_position
group_n_items = Multi_Handling.group_n_items
n_items = Multi_Handling.n_items
ntags = Multi_Handling.ntags
tipo = Multi_Handling.tipo
dir_only = Multi_Handling.dir_only
extra = Multi_Handling.extra
;*********************************



;Browsing positions:
primo_item = browse_position									;Position of the first visualized item
ultimo_item = (primo_item + group_n_items - 1) < (n_items -1)	;Position of the last visualized item
n_displayed_items = ultimo_item - primo_item + 1


;Get uvalue of this event and determine event type:
WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
tipo_evento = uvalue

pos = STRPOS(uvalue, 'browse')
IF pos[0] EQ 0 THEN tipo_evento = 'browse'			;serve perche' ogni browse button e' numerato con l'indice dell'item
pos = STRPOS(uvalue, 'check_button')
IF pos[0] EQ 0 THEN tipo_evento = 'check_button'	;serve perche' ogni browse button e' numerato con l'indice dell'item
pos = STRPOS(uvalue, 'field')
IF pos[0] EQ 0 THEN tipo_evento = 'field'			;serve perche' ogni browse button e' numerato con l'indice dell'item


;Automatic update of fields during digitation:
IF tipo_evento EQ 'field' THEN BEGIN
	FOR item = primo_item, ultimo_item DO BEGIN				;ciclo sui campi per ogni visualizzazione

       	elemento = FIX(item/ntags)
		tag = item MOD ntags

       	WIDGET_CONTROL, fieldid[item], GET_VALUE = valore		;legge il valore del campo

		IF tipo EQ 8 THEN selection[elemento].(tag) = valore $		;type structure
			ELSE $
			selection[elemento] = valore
	ENDFOR
ENDIF



;Event manager:
CASE tipo_evento OF

    'browse': BEGIN

		item = ___String_to_Long(uvalue)
       	elemento = FIX(item/ntags)
		tag = item MOD ntags

		;current value:
		IF tipo EQ 8 THEN old_valore = selection[elemento].(tag) $		;type structure
				ELSE $
				old_valore = selection[elemento]

		;new value:
		new_valore = DIALOG_PICKFILE(DIR=dir_only, file='*.*', $
				title='Select a file or a directory...', path=old_valore)

		;selection update:
		IF new_valore NE '' THEN BEGIN									;se il nome non e' nullo

			WIDGET_CONTROL, fieldid[item], SET_VALUE = new_valore

			IF dir_only EQ 0 AND FILE_TEST(new_valore) NE 0 OR dir_only EQ 1 THEN BEGIN		;se il file esiste o se e' una directory anche se non esiste

				IF tipo EQ 8 THEN selection[elemento].(tag) = new_valore $		;type stucture
					ELSE $
					selection[elemento] = new_valore

			ENDIF

		ENDIF


    	GOTO, fine
    END



	'All': BEGIN
		WIDGET_CONTROL, Riquadro, SET_BUTTON = 1
		FOR item = 0, n_items-1 DO BEGIN

			elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = item MOD ntags				;indice dell'elemento nell'oggetto

			IF tipo EQ 8 THEN selection[elemento].(tag) = 1 $				;type structure
                ELSE $
				selection[elemento] = 1
		ENDFOR
	END

	'None': BEGIN
		WIDGET_CONTROL, Riquadro, SET_BUTTON = 0
		FOR item = 0, n_items-1 DO BEGIN

			elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = item MOD ntags				;indice dell'elemento nell'oggetto

			IF tipo EQ 8 THEN selection[elemento].(tag) = 0 $				;type structure
                ELSE $
				selection[elemento] = 0
		ENDFOR
	END



	'Next': result = Multi_Dialog(_extra=extra, /next)


	'Previous': result = Multi_Dialog(_extra=extra, /previous)


	'Cancel': BEGIN
        ;IF n_elements(listindex) THEN selection=-1 $			;roba per 'list'
        ;	ELSE $
        ;	selection=event.value

		selection = extra.value		;no changes

		WIDGET_CONTROL, Base, /DESTROY

		IF extra.return_index EQ 1 THEN BEGIN

			IF tipo EQ 8 THEN BEGIN
				selected = intarr(n_items)
				FOR item = 0, n_items-1 DO BEGIN
					elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
					tag = item MOD ntags				;indice dell'elemento nell'oggetto
				 	selected[item] = selection[elemento].(tag) EQ 1
				ENDFOR
				selection = where(selected EQ 1)
			ENDIF ELSE selection = where(selection EQ 1)

		ENDIF

		GOTO, fine
	END


	'OK': BEGIN

        CASE 1 OF

            ;Field widget:
            KEYWORD_SET(fieldid): BEGIN

				FOR item = primo_item, ultimo_item DO BEGIN			;ciclo sui campi per ogni visualizzazione

					elemento = FIX(item/ntags)
					tag = item MOD ntags

		        	WIDGET_CONTROL, fieldid[item], GET_VALUE = valore

					IF tipo EQ 8 THEN BEGIN
						IF SIZE(selection[elemento].(tag), /TYPE) EQ 1 THEN valore = byte(WHERE(strupcase(valore[0]) EQ ['NO', 'YES']))	;type byte used ad type boolean
						selection[elemento].(tag) = valore
					ENDIF ELSE BEGIN
						IF tipo EQ 1 THEN valore = byte(WHERE(strupcase(valore[0]) EQ ['NO', 'YES']))
						selection[elemento] = valore
					ENDELSE
				ENDFOR

				WIDGET_CONTROL, Base, /DESTROY
				GOTO, fine
			END


            ;Check button widget:
            KEYWORD_SET(check_button_id): BEGIN

				WIDGET_CONTROL, Base, /DESTROY

				IF extra.return_index EQ 1 THEN BEGIN

					IF tipo EQ 8 THEN BEGIN
						selected = intarr(n_items)
						FOR item = 0, n_items-1 DO BEGIN
							elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
							tag = item MOD ntags				;indice dell'elemento nell'oggetto
						 	selected[item] = selection[elemento].(tag) EQ 1
						ENDFOR
						selection = where(selected EQ 1)
					ENDIF ELSE selection = where(selection EQ 1)

				ENDIF

				GOTO, fine

			END


            ;List widget:
            ;KEYWORD_SET(listid): BEGIN
            ;
            ;        id=widget_info(listid,/list_select)
            ;        if listindex then selection=id $
            ;        	else begin
    	    ;                if id ge 0 then selection=cho(id) $
    	    ;                	else $
    	    ;                	selection='Cancel'
	        ;            endelse
			;
			;	WIDGET_CONTROL, Base, /DESTROY
			;	GOTO, fine
			;END


            ELSE: selection=event.value

        ENDCASE


	END		;end of 'OK'


	'check_button': BEGIN

		item = ___String_to_Long(uvalue)
       	elemento = FIX(item/ntags)
		tag = item MOD ntags

		;Button selection:
		IF extra.exclusive EQ 1 THEN BEGIN			;if EXCLUSIVE then clear all items but the one selected
			IF tipo EQ 8 THEN BEGIN
				selection = selection * 0
				selection[elemento].(tag) = 1
			ENDIF ELSE BEGIN
				selection = selection * 0
				selection[elemento] = 1
			ENDELSE
		ENDIF ELSE BEGIN							;not exclusive:
			IF tipo EQ 8 THEN selection[elemento].(tag) = 1 - selection[elemento].(tag) $		;type structure
				ELSE $
				selection[elemento] = 1 - selection[elemento]
		ENDELSE

	END



    ;'list': begin
    ;    if listindex then selection=event.index else selection=cho(event.index)
    ;    ;; if user double-clicks, then we're done:
    ;    if event.clicks ne 2 then return
    ;end


	ELSE:

ENDCASE		;end case uvalue


fine:
Multi_Handling.Base = Base
Multi_Handling.selection = selection
Multi_Handling.Riquadro = Riquadro
Multi_Handling.fieldid = fieldid
Multi_Handling.browseid = browseid
Multi_Handling.check_button_id = check_button_id
Multi_Handling.browse_position = browse_position
Multi_Handling.group_n_items = group_n_items
Multi_Handling.n_items = n_items
Multi_Handling.ntags = ntags
Multi_Handling.tipo = tipo
Multi_Handling.dir_only = dir_only
Multi_Handling.extra = extra
ptr_Multi_Handling[dialog_index] = PTR_NEW(Multi_Handling)

END






;*****************
;* MAIN FUNCTION *
;*****************
FUNCTION Multi_Dialog, Label=Label, value=value, field=field, file=file, directory=directory, message=message, $
			check_button=check_button, exclusive=exclusive, all_button=all_button, none_button=none_button, $
            list=list, choices=choices, return_index=return_index, $
			rows=rows, columns=columns, MAX_YSIZE=MAX_YSIZE, MAX_XSIZE=MAX_XSIZE,$
			TITLE=Title, NO_FRAME=NO_FRAME, CENTER=CENTER, $
			POS=POS, GROUP=GROUP, NO_BLOCK=NO_BLOCK, $
			next=next, previous=previous

COMMON Multi_Dialog_data, Base, selection, fieldid, listid, browseid, check_button_id, Riquadro, dir_only
COMMON Multi_Dialog_browsing, browse_position, group_n_items, n_items, ntags, tipo, extra
COMMON Multi_Dialog_handling, vct_base, ptr_Multi_Handling, dialog_index

on_error, 2


;*********
;Defaults:
;*********
default_max_ysize = 0.75				;Default maximum Y-Size of the Widget (fraction of screen)
default_max_xsize = 0.35				;Default maximum X-Size of the Widget (fraction of screen)
default_position = [0,0]				;Default position of the upper-left corner of the widget on the screen (pixels)
default_max_items = 1000				;Default maximum items contemporary displayed


;**************
;Input Control:
;**************

IF NOT KEYWORD_SET(TITLE) THEN Title = ' '
IF Title EQ '' THEN Title = ' '

IF n_elements(Message) EQ 0 THEN Message = ''

IF n_elements(value) EQ 0 AND (n_elements(Field) NE 0 $
	OR n_elements(File) NE 0 OR n_elements(Directory) NE 0) THEN BEGIN
	IF n_elements(Label) NE 0 THEN value = strarr(n_elements(Label)) ELSE Message, 'VALUE keyword not defined!'
ENDIF

IF n_elements(value) EQ 0 AND n_elements(Check_button) NE 0 THEN BEGIN
	IF n_elements(Label) NE 0 THEN value = intarr(n_elements(Label)) ELSE Message, 'VALUE keyword not defined!'
ENDIF

IF n_elements(value) EQ 0 THEN value = 0

IF n_elements(File) EQ 0 THEN File = ''

IF n_elements(Directory) EQ 0 THEN Directory = ''

IF n_elements(Label) EQ 0 THEN Label = ''					;aggiungere label = tag_names per struttura <--
Label = string(Label)

IF n_elements(Field) EQ 0 THEN Field = 0

IF n_elements(List) EQ 0 THEN List = 0

IF n_elements(Choices) EQ 0 THEN Choices = 0

IF n_elements(Check_button) EQ 0 THEN Check_button = 0

IF n_elements(EXCLUSIVE) EQ 0 THEN EXCLUSIVE = 0

IF n_elements(All_button) EQ 0 THEN All_button = 0

IF n_elements(None_button) EQ 0 THEN None_button = 0

IF n_elements(RETURN_INDEX) EQ 0 THEN RETURN_INDEX=0

IF n_elements(NO_FRAME) EQ 0 THEN NO_FRAME=0

IF NOT KEYWORD_SET(MAX_YSIZE) THEN max_ysize = default_max_ysize

IF NOT KEYWORD_SET(MAX_XSIZE) THEN max_xsize = default_max_xsize

IF n_elements(CENTER) EQ 0 THEN CENTER = 0

IF n_elements(POS) EQ 0 THEN POS = default_position

IF NOT KEYWORD_SET(GROUP) THEN extra_GROUP = 0 ELSE extra_GROUP = GROUP

IF NOT KEYWORD_SET(rows) THEN rows = default_max_items

IF NOT KEYWORD_SET(columns) THEN columns = 1

IF n_elements(NO_BLOCK) EQ 0 THEN NO_BLOCK = 0

IF KEYWORD_SET(next) THEN browse_position = (browse_position + group_n_items) < (n_items -1)
IF KEYWORD_SET(previous) THEN browse_position = (browse_position - group_n_items) > 0
IF KEYWORD_SET(next) OR KEYWORD_SET(previous) THEN WIDGET_CONTROL, Base, /DESTROY $
	ELSE $
	browse_position = 0

IF KEYWORD_SET(file) THEN dir_only = 0 ELSE dir_only = 1		;File o directory


;;Set NO_BLOCK = 0 when user interaction is required:
;;IF KEYWORD_SET(Field) OR KEYWORD_SET(File) OR KEYWORD_SET(Directory) OR KEYWORD_SET(List) THEN NO_BLOCK = 0


;Defines the structure for the _EXTRA keyword to recall the widget from the Event Handler routine:
extra = {Title:Title, Label:Label, field:field, value:value, file:file, directory:directory, Message:Message, $
		check_button:check_button, exclusive:exclusive, all_button:all_button, none_button:none_button, $
		list:list, choices:choices, return_index:return_index, $
		rows:rows, columns:columns, MAX_YSIZE:MAX_YSIZE, MAX_XSIZE:MAX_XSIZE,$
		POS:POS, GROUP:extra_GROUP, NO_FRAME:NO_FRAME, CENTER:CENTER, NO_BLOCK:NO_BLOCK}



;**********
;Constants:
;**********
min_xsize = 80									;Minimum possible X-Size of a Widget with no space left for the Title (pixels)
min_ysize = 25									;Minimum possible Y-Size of a Widget with the only Title Bar (pixels)
scroll_control_xsize = 25						;X-Size of a vertical Scroll Bar (pixels)
scroll_control_ysize = 30						;X-Size of a vertical Scroll Bar (pixels)

buttons_x_size = 100							;X-Size of buttons (pixels)
buttons_y_size = 36								;Y-Size of buttons (pixels)
ypads = 5										;Y-Pad of widgets (pixels)
xpads = 5										;Y-Pad of widgets (pixels)


;Fattore tra le dimensioni del titolo e del testo dentro al widget:
IF !VERSION.OS_FAMILY EQ 'unix' THEN text_to_title_size_factor = 2.5 ELSE text_to_title_size_factor = 1.2

max_xsize = max_xsize < 1.
max_ysize = max_ysize < 1.
max_ysize = max_ysize > 0.2

screen_size = GET_SCREEN_SIZE()
xsize_max = screen_size[0] * max_xsize
ysize_max = screen_size[1] * max_ysize

ysize_max = ysize_max - min_ysize		         				;Maximum Y-Size of Widget without the Title Bar (pixels)
scroll_items = long(rows) * long(columns) < default_max_items


IF KEYWORD_SET(value) AND NOT KEYWORD_SET(next) AND NOT KEYWORD_SET(previous) THEN selection = value

browseid = 0L
fieldid = 0L
check_button_id = 0L
listid = 0L
;listindex = KEYWORD_SET(return_index)



;***************
;WIDGET CREATION
;***************

;Create the main BASE widget
Base = WIDGET_BASE (TITLE = Title, /COLUMN, MAP = 0, GROUP=GROUP)


;Temporary writes the Title to measure it:
id = WIDGET_LABEL (BASE, VALUE = Title)
geom = WIDGET_INFO (id, /GEOMETRY)
item_ysize = geom.SCR_YSIZE										;Y-Size of the text (pixels)
title_xsize = geom.SCR_XSIZE									;X-Size of the Title (pixels)
WIDGET_CONTROL, id, /DESTROY



;***************************************************************
;Temporary write an item to measure its size and then delete it:
;***************************************************************

CASE 1 OF				;(exclusive conditions)

    KEYWORD_SET(Message): BEGIN

		max_lenght = max(strlen(Message), index)
		max_string = Message[index]										;Longest string in Message
		id = WIDGET_LABEL (BASE, VALUE = max_string)
		geom = WIDGET_INFO (id, /GEOMETRY)
		item_ysize = geom.SCR_YSIZE										;Y-Size of the text (pixels)
		item_xsize = geom.SCR_XSIZE										;X-Size of the longest string (pixels)
		WIDGET_CONTROL, id, /DESTROY

	END


    KEYWORD_SET(field) OR KEYWORD_SET(file) OR KEYWORD_SET(directory): BEGIN

        sz = size(value)
        n_elementi = n_elements(value)
        tipo = size(value, /type)
        IF tipo EQ 8 THEN BEGIN					;type structure
        	ntags = N_TAGS(value)
        	n_items = n_elementi * ntags			;puo' essere anche un vettore di strutture
        ENDIF ELSE BEGIN
        	n_items = n_elementi
        	ntags = 1
        ENDELSE

		IF n_elements(Label) EQ 1 THEN IF Label EQ '' THEN Label = 'Variable ' + strtrim(string(lindgen(n_items)), 2)

		IF KEYWORD_SET(file) OR KEYWORD_SET(directory) THEN BEGIN
			basiid = WIDGET_BASE(Base, COLUMN=3)
 		ENDIF ELSE BEGIN
 			basiid = WIDGET_BASE(Base, COLUMN=2, /GRID)
 		ENDELSE


		max_Label = max(strlen(Label), index)
		max_Label = Label[index]
		label_size = strlen(max_Label)

		labelid = WIDGET_TEXT(basiid, xsize=label_size, $
						EDITABLE=0, VALUE=max_Label, FRAME=0, YOFFSET=ypads)

        IF tipo EQ 8 THEN BEGIN					;type structure
			sss = lonarr(n_items)
			FOR item = 0, n_items-1 DO BEGIN
				elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
				tag = item MOD ntags				;indice dell'elemento nell'oggetto
				sss[item] = strlen(string(value[elemento].(tag)))
			ENDFOR
			longest_value = max(sss, index)
			elemento = FIX(index/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = index MOD ntags				;indice dell'elemento nell'oggetto
			longest_value = string(value[elemento].(index))
        ENDIF ELSE BEGIN
			longest_value = max(strlen(string(value)), index)
			longest_value = string(value([index]))
        ENDELSE

		field_size = strlen(longest_value)
        fieldid = CW_FIELD(basiid, title='', value = longest_value, /string, XSIZE=field_size)

		IF KEYWORD_SET(file) OR KEYWORD_SET(directory) THEN $
			browseid = WIDGET_BUTTON(basiid, VALUE='Browse...', uvalue='browse')

		geom = WIDGET_INFO (basiid, /GEOMETRY)
		item_ysize = geom.SCR_YSIZE										;Y-Size of the item (pixels)
		item_xsize = geom.SCR_XSIZE										;X-Size of the item (pixels)
		WIDGET_CONTROL, basiid, /DESTROY

	END


    KEYWORD_SET(list):	BEGIN
	END


    KEYWORD_SET(check_button):	BEGIN

        sz = size(value)
        n_elementi = n_elements(value)
        tipo = size(value, /type)
        IF tipo EQ 8 THEN BEGIN					;type structure
        	ntags = N_TAGS(value)
        	n_items = n_elementi * ntags			;puo' essere anche un vettore di strutture
        ENDIF ELSE BEGIN
        	n_items = n_elementi
        	ntags = 1
        ENDELSE

		Riquadro = WIDGET_BASE (BASE, /COLUMN, EXCLUSIVE=EXCLUSIVE, NONEXCLUSIVE=1-EXCLUSIVE)

		IF n_elements(Label) EQ 1 THEN IF Label EQ '' THEN Label = 'Variable ' + strtrim(string(lindgen(n_items)), 2)

		max_Label = max(strlen(Label), index)
		max_Label = Label[index]
		label_size = strlen(max_Label)

		check_button_id = Widget_Button(Riquadro, VALUE=max_Label)

		geom = WIDGET_INFO (check_button_id, /GEOMETRY)
		check_button_size = geom.SCR_XSIZE								;X-Size of the item (pixels)

		geom = WIDGET_INFO (Riquadro, /GEOMETRY)
		item_ysize = geom.SCR_YSIZE										;Y-Size of the item (pixels)
		item_xsize = geom.SCR_XSIZE										;X-Size of the item (pixels)
		WIDGET_CONTROL, Riquadro, /DESTROY
	END

	ELSE: Message, 'One of the following keywor MUST be declared: "FIELD", "LIST", "MESSAGE", "FILE", "DIRECTORY" or "CHECK_BUTTON".'

ENDCASE


;if n_elements(value) eq 0 then value=0
;if n_elements(choices) gt 0 then list=1



;*****************
;Create the items:
;*****************

CASE 1 OF				;(exclusive conditions)

    KEYWORD_SET(Message):	BEGIN

		n_elementi = n_elements(Message)
       	n_items = n_elementi
       	ntags = 1

		;Build the widget bases for the text:
		group_n_items = scroll_items < n_items > 1					;Number of items contemporary shown in the scroll frame
		primo_item = browse_position									;Position of the first visualized item
		ultimo_item = (primo_item + group_n_items - 1) < (n_items -1)	;Position of the last visualized item
		n_displayed_items = ultimo_item - primo_item + 1

		items_base_ysize = 2 * ypads + item_ysize * group_n_items		;Y-Size of the text frame (pixels). This number MUST NOT overcome the Short Integer range
		scroll_y = items_base_ysize GT ysize_max

		IF group_n_items LT n_items THEN ysize_max = ysize_max - (buttons_y_size + 4 * ypads)	;Maximum Y-Size of Widget without the Buttons (pixels)

		IF scroll_y EQ 0 THEN BEGIN
			Riquadro = WIDGET_BASE (BASE, /COLUMN, ALIGN_CENTER=CENTER, BASE_ALIGN_CENTER=CENTER, FRAME=1-NO_FRAME, SCR_XSIZE=item_xsize+2*xpads)
		ENDIF ELSE BEGIN
			Riquadro = WIDGET_BASE (BASE, /COLUMN,  ALIGN_CENTER=CENTER, BASE_ALIGN_CENTER=CENTER, FRAME=1-NO_FRAME, /SCROLL, $
						SCR_YSIZE=ysize_max, Y_SCROLL_SIZE=items_base_ysize, SCR_XSIZE=item_xsize+2*xpads+scroll_control_xsize)
		ENDELSE


		;Write the items:
		FOR item = primo_item, ultimo_item DO $
			id = WIDGET_LABEL (Riquadro, VALUE = Message[item], ALIGN_CENTER=CENTER, ALIGN_RIGHT=1-CENTER, XSIZE=item_xsize)

	END




    KEYWORD_SET(field):	BEGIN

        sz = size(value)
        n_elementi = n_elements(value)
        tipo = size(value, /type)
        IF tipo EQ 8 THEN BEGIN					;type structure
        	ntags = N_TAGS(value)
        	n_items = n_elementi * ntags			;puo' essere anche un vettore di strutture
        ENDIF ELSE BEGIN
        	n_items = n_elementi
        	ntags = 1
        ENDELSE


		;Build the widget bases for the text:
		group_n_items = scroll_items < n_items > 1					;Number of items contemporary shown in the scroll frame
		primo_item = browse_position									;Position of the first visualized item
		ultimo_item = (primo_item + group_n_items - 1) < (n_items -1)	;Position of the last visualized item
		n_displayed_items = ultimo_item - primo_item + 1

		effective_columns = group_n_items < columns						;Effective columns required
		effective_rows = CEIL(group_n_items / float(effective_columns))	;Effective rows required

		items_base_ysize = (2 * ypads + item_ysize) * effective_rows				;Y-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range
		items_base_xsize = (2 * xpads + item_xsize) * effective_columns			;X-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range

		scroll_y = items_base_ysize GT ysize_max
		scroll_x = items_base_xsize GT xsize_max

		IF effective_rows LT n_items THEN ysize_max = ysize_max - (buttons_y_size + 4 * ypads)	;Maximum Y-Size of Widget without the Buttons (pixels)

		CASE 1 OF
			scroll_x EQ 0 AND scroll_y EQ 0: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_XSIZE=items_base_xsize)
			END
			scroll_x AND scroll_y: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						X_SCROLL_SIZE=items_base_xsize, SCR_YSIZE=ysize_max + 2*ypads, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize < xsize_max)
			END
			scroll_x: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_YSIZE=items_base_ysize + 2*ypads, $
						SCR_XSIZE=items_base_xsize < xsize_max, $
						X_SCROLL_SIZE=items_base_xsize, Y_SCROLL_SIZE=items_base_ysize)
			END
			scroll_y: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize, $
						SCR_YSIZE=ysize_max)
			END
		ENDCASE



		;Write the items:
        IF n_elements(fieldid) LT n_items THEN fieldid = lonarr(n_items)		;vettore degli id dei campi
        basiid = lonarr(n_items)			;vettore degli id delle basi

		FOR item = primo_item, ultimo_item DO BEGIN			;ciclo sui campi per ogni visualizzazione

			elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = item MOD ntags				;indice dell'elemento nell'oggetto

			IF tipo EQ 8 THEN valore = selection[elemento].(tag) $				;type structure
                ELSE $
				valore = selection[elemento]

	        tipo_valore = size(valore, /type)

			basiid[item] = WIDGET_BASE(Riquadro, COLUMN=2, /GRID)

			labelid = WIDGET_TEXT(basiid[item], xsize=label_size, $
						EDITABLE=0, VALUE=Label[item], FRAME=0, YOFFSET=ypads)

			IF tipo_valore EQ 1 THEN BEGIN				;type byte used as type boolean: 0b = 'NO', other values = 'YES'
				valore = (['NO', 'YES'])[valore NE 0]
				tipo_valore = 7
			ENDIF

	        fieldid[item] = CW_FIELD(basiid[item], title='',$
                       uvalue = 'field' + strtrim(string(item)), $
                       value = valore, $
                       /all_events, $
                       float = tipo_valore EQ 4, $
                       integer = tipo_valore EQ 2, $
                       long = tipo_valore EQ 3, $
                       string = tipo_valore EQ 7, $
                       XSIZE=field_size)

		ENDFOR

        buttons=['Cancel','OK']
    END





    KEYWORD_SET(file) OR KEYWORD_SET(directory):BEGIN

        sz = size(value)
        n_elementi = n_elements(value)
        tipo = size(value, /type)
        IF tipo EQ 8 THEN BEGIN					;type structure
        	ntags = N_TAGS(value)
        	n_items = n_elementi * ntags
        ENDIF ELSE BEGIN
        	n_items = n_elementi
        	ntags = 1
        ENDELSE


		;Build the widget bases for the text:
		group_n_items = scroll_items < n_items > 1						;Number of items contemporary shown in the scroll frame
		primo_item = browse_position									;Position of the first visualized item
		ultimo_item = (primo_item + group_n_items - 1) < (n_items -1)	;Position of the last visualized item
		n_displayed_items = ultimo_item - primo_item + 1


		effective_columns = 1											;Effective columns required
		effective_rows = CEIL(group_n_items / float(effective_columns))	;Effective rows required

		items_base_ysize = (2 * ypads + item_ysize) * effective_rows	;Y-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range
		items_base_xsize = (2 * xpads + item_xsize) * effective_columns	;X-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range

		scroll_y = items_base_ysize GT ysize_max
		scroll_x = items_base_xsize GT xsize_max

		IF effective_rows LT n_items THEN ysize_max = ysize_max - (buttons_y_size + 4 * ypads)	;Maximum Y-Size of Widget without the Buttons (pixels)

		CASE 1 OF
			scroll_x EQ 0 AND scroll_y EQ 0: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_XSIZE=items_base_xsize)
			END
			scroll_x AND scroll_y: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						X_SCROLL_SIZE=items_base_xsize, SCR_YSIZE=ysize_max + 2*ypads, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize < xsize_max)
			END
			scroll_x: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_YSIZE=items_base_ysize + 2*ypads, $
						SCR_XSIZE=items_base_xsize < xsize_max, $
						X_SCROLL_SIZE=items_base_xsize, Y_SCROLL_SIZE=items_base_ysize)
			END
			scroll_y: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize, $
						SCR_YSIZE=ysize_max)
			END
		ENDCASE



		;Write the items:
        IF n_elements(fieldid) LT n_items THEN fieldid = lonarr(n_items)	;vettore degli id dei campi
        basiid = lonarr(n_items)											;vettore degli id delle basi
        browseid = lonarr(n_items)											;vettore degli id dei tasti browse


		FOR item = primo_item, ultimo_item DO BEGIN			;ciclo sui campi per ogni visualizzazione

			elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = item MOD ntags				;indice dell'elemento nell'oggetto

			IF tipo EQ 8 THEN valore = string(selection[elemento].(tag)) $				;type structure
                ELSE $
				valore = string(selection[elemento])

			basiid[item] = WIDGET_BASE(Riquadro, COLUMN=3)

			labelid = WIDGET_TEXT(basiid[item], xsize=label_size, $
						EDITABLE=0, VALUE=Label[item], FRAME=0, YOFFSET=3)

	        fieldid[item] = CW_FIELD(basiid[item], title='',$
                       uvalue = 'field' + strtrim(string(item)), $
                       value = valore, $
                       /all_events, $
                       /string, xsize = field_size)

			browseid[item] = WIDGET_BUTTON(basiid[item], VALUE='Browse...', $
						uvalue='browse' + strtrim(string(item),2))

		ENDFOR

        buttons=['Cancel','OK']
    END




    KEYWORD_SET(check_button):	BEGIN

        sz = size(value)
        n_elementi = n_elements(value)
        tipo = size(value, /type)
        IF tipo EQ 8 THEN BEGIN						;type structure
        	ntags = N_TAGS(value)
        	n_items = n_elementi * ntags			;Value can be an array of structures
        ENDIF ELSE BEGIN
        	n_items = n_elementi
        	ntags = 1
        ENDELSE


		;Build the base for the items:
		group_n_items = scroll_items < n_items > 1						;Number of items contemporary shown in the scroll frame
		primo_item = browse_position									;Position of the first visualized item
		ultimo_item = (primo_item + group_n_items - 1) < (n_items -1)	;Position of the last visualized item
		n_displayed_items = ultimo_item - primo_item + 1

		effective_columns = group_n_items < columns						;Effective columns required
		effective_rows = CEIL(group_n_items / float(effective_columns))	;Effective rows required

		items_base_ysize = (2 * ypads + item_ysize) * effective_rows	;Y-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range
		items_base_xsize = (2 * xpads + item_xsize) * effective_columns	;X-Size of the items frame (pixels). This number MUST NOT overcome the Short Integer range

		scroll_y = items_base_ysize GT ysize_max
		scroll_x = items_base_xsize GT xsize_max

		IF effective_rows LT n_items THEN ysize_max = ysize_max - (buttons_y_size + 4 * ypads)	;Maximum Y-Size of Widget without the Buttons (pixels)

		CASE 1 OF
			scroll_x EQ 0 AND scroll_y EQ 0: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_XSIZE=items_base_xsize, $
						EXCLUSIVE=EXCLUSIVE, NONEXCLUSIVE=1-EXCLUSIVE)
			END
			scroll_x AND scroll_y: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						X_SCROLL_SIZE=items_base_xsize, SCR_YSIZE=ysize_max + 2*ypads, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize < xsize_max, $
						EXCLUSIVE=EXCLUSIVE, NONEXCLUSIVE=1-EXCLUSIVE)
			END
			scroll_x: BEGIN
				ysize_max = ysize_max - scroll_control_ysize
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns,  /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, SCR_YSIZE=items_base_ysize + 2*ypads, $
						SCR_XSIZE=items_base_xsize < xsize_max, $
						X_SCROLL_SIZE=items_base_xsize, Y_SCROLL_SIZE=items_base_ysize, $
						EXCLUSIVE=EXCLUSIVE, NONEXCLUSIVE=1-EXCLUSIVE)
			END
			scroll_y: BEGIN
				Riquadro = WIDGET_BASE (BASE, COLUMN=effective_columns, /ALIGN_CENTER, BASE_ALIGN_CENTER=CENTER, $
						FRAME=1-NO_FRAME, Y_SCROLL_SIZE=items_base_ysize, $
						SCR_XSIZE=items_base_xsize + scroll_control_xsize, $
						SCR_YSIZE=ysize_max, $
						EXCLUSIVE=EXCLUSIVE, NONEXCLUSIVE=1-EXCLUSIVE)
			END
		ENDCASE


		;Write the items:
        IF n_elements(check_button_id) LT n_items THEN check_button_id = lonarr(n_items)		;vettore degli id dei campi
        basiid = lonarr(n_items)			;vettore degli id delle basi

		FOR item = primo_item, ultimo_item DO BEGIN			;ciclo sui campi per ogni visualizzazione

			elemento = FIX(item/ntags)			;indice dell'oggetto (es. elemento del vettore di strutture)
			tag = item MOD ntags				;indice dell'elemento nell'oggetto

			IF tipo EQ 8 THEN valore = selection[elemento].(tag) $				;type structure
                ELSE $
				valore = selection[elemento]

	        tipo_valore = size(valore, /type)

			IF tipo_valore EQ 1 THEN BEGIN				;type byte used as type boolean: 0b = 'NO', other values = 'YES'
				valore = (['NO', 'YES'])[valore NE 0]
				tipo_valore = 7
			ENDIF

			check_button_id[item] = Widget_Button(Riquadro, $
						UVALUE='check_button' + strtrim(string(item)), $
						VALUE=Label[item], SCR_XSIZE=check_button_size)

			WIDGET_CONTROL, check_button_id[item], SET_BUTTON = valore

		ENDFOR

        buttons = ['Cancel','OK']
		IF KEYWORD_SET(ALL_BUTTON) AND NOT KEYWORD_SET(EXCLUSIVE) THEN buttons = ['All', buttons]
		IF KEYWORD_SET(NONE_BUTTON) AND NOT KEYWORD_SET(EXCLUSIVE) THEN buttons = [buttons, 'None']

    END





    ;KEYWORD_SET(list):	BEGIN
	;
    ;    if keyword_set(choices) eq 0 then $
    ;      message,'Must supply an array of choices for the list.'
    ;    cho=choices
    ;    if n_params() eq 0 then Label='Choose: '
    ;    label=widget_label(base,value=Label,frame=0)
    ;    listid=widget_list(base,value=choices, $
    ;                       ysize=n_elements(choices) < 10, $
    ;                       uvalue='list')
    ;    buttons=['Cancel','OK']
    ;    cho=choices             ; set common variable for event handler.
    ;END



ENDCASE




;*****************
;Make the Buttons:
;*****************
IF group_n_items LT n_items THEN BEGIN
	IF n_elements(buttons) NE 0 THEN buttons = ['Previous', buttons, 'Next'] ELSE buttons = ['Previous', 'Next']
END
IF KEYWORD_SET(buttons) NE 0 THEN BEGIN
	Base_Buttons = Widget_Base(Base, UNAME='Base_Buttons', /ALIGN_CENTER, /BASE_ALIGN_CENTER, SPACE=25, XPAD=xpads, YPAD=ypads, ROW=1)
	FOR b = 0, n_elements(buttons)-1 DO BEGIN
		CASE buttons[b] OF
			'Previous':	Previous_Button = 	Widget_Button(Base_Buttons, UNAME='Previous', UVALUE='Previous', XOFFSET=xpads, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=0, /ALIGN_CENTER, VALUE='<--- Previous')
			'Next':		Next_Button = 		Widget_Button(Base_Buttons, UNAME='Next', UVALUE='Next',  XOFFSET=233, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=0, /ALIGN_CENTER, VALUE='Next --->')
			'Cancel':	Cancel_Button = 	Widget_Button(Base_Buttons, UNAME='Cancel', UVALUE='Cancel',  XOFFSET=233, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=1, /ALIGN_CENTER, VALUE='Cancel')
			'OK':		OK_Button = 		Widget_Button(Base_Buttons, UNAME='OK', UVALUE='OK',  XOFFSET=233, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=1, /ALIGN_CENTER, VALUE='OK')
			'All':		All_Button = 		Widget_Button(Base_Buttons, UNAME='All', UVALUE='All',  XOFFSET=233, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=1, /ALIGN_CENTER, VALUE='All')
			'None':		None_Button = 		Widget_Button(Base_Buttons, UNAME='None', UVALUE='None',  XOFFSET=233, YOFFSET=ypads, SCR_XSIZE=buttons_x_size, SCR_YSIZE=buttons_y_size, SENSITIVE=1, /ALIGN_CENTER, VALUE='None')
			ELSE:
		ENDCASE
	ENDFOR
ENDIF


;Check the minimum and maximum size and fit both title and text
geom = WIDGET_INFO (Base, /GEOMETRY)
min_xsize = min_xsize + title_xsize * text_to_title_size_factor			;Minimum X-Size with the Title Bar (pixels)
xsize = (min_xsize > item_xsize > geom.scr_xsize) < screen_size[0]		;Effective X-Size with the Items (pixels)

;Resize the widget:
WIDGET_CONTROL, Base, SCR_XSIZE=xsize

;Realize the widget
WIDGET_CONTROL, Base, /REALIZE

;Set the position on the screen
WIDGET_CONTROL, Base, TLB_SET_XOFFSET = POS[0], TLB_SET_YOFFSET = POS[1]

;Map the widget hierarchy
WIDGET_CONTROL, Base, MAP = 1


;Set Browse Buttons sensisivity:
IF group_n_items LT n_items THEN BEGIN
	IF browse_position + group_n_items LT n_items THEN $
		WIDGET_CONTROL, Next_Button, SENSITIVE=1 $
		ELSE $
		WIDGET_CONTROL, Next_Button, SENSITIVE=0
	IF browse_position GT 0 THEN $
		WIDGET_CONTROL, Previous_Button, SENSITIVE=1 $
		ELSE $
		WIDGET_CONTROL, Previous_Button, SENSITIVE=0
ENDIF




;************************************************
;Updates simultaneous MultiDialogs handling data:
;************************************************
;Keep track of the dialogs by a vector with the value of Base for each managed dialog:
;es: vct_base = [105, 145] means that the first managed dialog have the index 105, the second 145, etc.
;Keep track of the COMMON data for each dialog by a vector with the pointers to each Multi_handling structure.

IF n_elements(selection) EQ 0 THEN selection = value
IF n_elements(tipo) EQ 0 THEN tipo = -1


IF n_elements(ptr_Multi_Handling) EQ 0 THEN BEGIN		;If this is the first occurrence of Multi_Dialog
	Multi_Handling = {Base:Base, selection:selection, Riquadro:Riquadro, $
			fieldid:fieldid, browseid:browseid, check_button_id:check_button_id, $
			browse_position:browse_position, group_n_items:group_n_items, $
			n_items:n_items, ntags:ntags, tipo:tipo, dir_only:dir_only, extra:extra}
	ptr_Multi_Handling = PTR_NEW(Multi_Handling)
	vct_base = Base
ENDIF ELSE BEGIN
	IF KEYWORD_SET(next) OR KEYWORD_SET(previous) THEN BEGIN
		Multi_Handling = *(ptr_Multi_Handling[dialog_index[0]])		;Multi_Handling structure corresponding to the current dialog
		Multi_Handling.Base = Base
		Multi_Handling.selection = selection
		Multi_Handling.Riquadro = Riquadro
		Multi_Handling.fieldid = fieldid
		Multi_Handling.browseid = browseid
		Multi_Handling.check_button_id = check_button_id
		Multi_Handling.browse_position = browse_position
		Multi_Handling.group_n_items = group_n_items
		Multi_Handling.n_items = n_items
		Multi_Handling.ntags = ntags
		Multi_Handling.tipo = tipo
		Multi_Handling.dir_only = dir_only
		Multi_Handling.extra = extra
		ptr_Multi_Handling[dialog_index] = PTR_NEW(Multi_Handling)
		vct_base[dialog_index] = Base								;updates the index of the current dialog base
	ENDIF ELSE BEGIN
	Multi_Handling = {Base:Base, selection:selection, Riquadro:Riquadro, $
			fieldid:fieldid, browseid:browseid, check_button_id:check_button_id, $
			browse_position:browse_position, group_n_items:group_n_items, $
			n_items:n_items, ntags:ntags, tipo:tipo, dir_only:dir_only, extra:extra}
		ptr_Multi_Handling = [ptr_Multi_Handling, PTR_NEW(Multi_Handling)]	;add the pointer to the current multi_handling stucture
		vct_base = [vct_base, Base]											;add the current base index
	ENDELSE
ENDELSE




;Manage the widget:
XMANAGER,'multi_dialog', Base, GROUP=GROUP, NO_BLOCK=NO_BLOCK

IF n_elements(selection) EQ 0 THEN selection = -1

RETURN, selection

END
