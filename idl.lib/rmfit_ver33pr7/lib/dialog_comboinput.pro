;
;	DIALOG_COMBOINPUT
;
;	AUTHOR : Vandiver Chaplin, UAH
;
;	VERSION: 2.0, 22 September 2009
;
;	Dialog routine allowing input from text widgets or combobox widgets.
;	An 'Observer' type pattern can be used with the BIND keywords.
;	A binding association goes one way, with the list value being sent to 
;	the associated text widgets (observers).  Using the PREPEND, APPEND keywords, or 
;	the MAPPING keyword, this value can be modified before appearing in a given oberver.
;	PREPEND and APPEND effect Observer behavior, so there must be one for each text
;	widget which is an observer (the default is '' for both).  MAPPING is proper to the list
;	in question.  If using a mapping, all observers recieve the mapped value.
;
;	Inputs are like DIALOG_INPUT with the following additions:
;
;	STATIC : String array with names of non-editable text widgets to make.
;	STATIN : The initial values.  Use BIND_TO_STATIC_N to make these observers of a list.
;
;	LISTNAMES : String array with the names of each combobox widget to make. These namese
;				appear in a label next to the widget.
;	LISTITEMS : 1-D String array with all of the items for above list.  This array is parsed
;				with the next keyword.
;	LISTSIZES :	Int array with the size of each list in LISTNAMES.
;
;	LISTEDIT :	Optional. Indeces of lists in LISTNAMES to make editable.
;
;	LISTSTART :	The start index for each list. Optional, default is 0.
;
;	MAPLIST : Structure specifying which lists to map, the mapping object, and 
;				whether the values should be considered as a complete set.
;				These three arrays should be the same size.
;				Repeated values in 'lists' will use the last map object.
;				{
;				lists: int array with the index of the list(s) to map
;				mappings: OBJ array where each element is a HASH object (part of rmfit).
;				completed: 0 or 1.  If 1, new pairs can only use existing values in the hash.
;				}
;				Each HASH object should have keys corresponding to entries in the list,
;				otherwise the mapping is useless.
;
;	BIND_LSTATS_N : The indeces of the lists in LISTNAMES to observe.
;	BIND_TO_STATIC_N : The indeces of the text widgets in STATIC to make observers of the
;						corresponding list in BIND_LSTATS_N.
;
;   BIND_TO_PROMPT_N : The indeces of the text widgets in PROMPT to make observers of the
;						corresponding list in BIND_LSTATS_N.
;
;	PREPEND : String array to alter the behavior of the corresponding observer N.
;	APPEND  : Same
;
;	HELP : String array with lines of text in each element.
;
;----------------------------------------------------------------------------------------


;-----------------------------------------------------------------------------------
; Proto-type functions
;-----------------------------------------------------------------------------------

PRO dialoginfo__define
  tmp = {dialoginfo, $
  		 boundCnt:0B,$
         boundIDs:PTR_NEW(), $
         pref:'', $
         suff:'', $
         index:0L, $
         mapping:{MAP,hash:OBJ_NEW(),complete:0b,osize:0},$ $
         calls:PTR_NEW() $
         }
END

PRO textslave__define
	tmp = {textslave, $
			govID: 0L, $
			route: '', $
			pref: '', $
			suff: '' $
			}
END

FUNCTION GETTEXT, id
	value=WIDGET_INFO(id, /COMBOBOX_GETTEXT)
	return,value
END

FUNCTION GETINDEX, id
	WIDGET_CONTROL, id , GET_UVALUE=info
	return,info.index
END

PRO CALL_LISTEVENTS, ev
	WIDGET_CONTROL, ev.ID , GET_UVALUE=info
	myEventCalls = *info.calls
	;print, 'EventCalls', myEventCalls
	FOR i=0, N_ELEMENTS(myEventCalls)-1 DO BEGIN
		IF myEventCalls[i] NE '' THEN $
		CALL_PROCEDURE, myEventCalls[i], ev
	ENDFOR
END

PRO ValueToPrompt, ev
	WIDGET_CONTROL, ev.ID, GET_UVALUE=info
	key = WIDGET_INFO(ev.id, /COMBOBOX_GETTEXT)
	print,'last index=',info.index
	print, 'new=',ev.INDEX
	IF OBJ_VALID(info.mapping.hash) THEN v = info.mapping.hash->get(key) ELSE v = key

	IF SIZE(v,/TYPE) EQ 10 && NOT PTR_VALID(v) THEN BEGIN
	
		WIDGET_CONTROL, ev.ID, GET_VALUE=list
		npairs = N_ELEMENTS(list)
		
		print, list
		IF ev.STR EQ '' THEN BEGIN
			IF info.index+1 GT info.mapping.osize THEN BEGIN
				info.mapping.hash->remove, list[info.index]
				info.index--
				print, "Deleting ",list[info.index]
				WIDGET_CONTROL, ev.ID, COMBOBOX_DELETEITEM = info.index+1,$
					SET_UVALUE = info, SET_COMBOBOX_SELECT = info.index,$
					SEND_EVENT={ID:0L,TOP:0L,HANDLER:0L,INDEX:info.index}
			END
			RETURN
		ENDIF
		
		val0 = info.mapping.hash->get(list[0])
		
		values = REPLICATE(val0, npairs)
		FOR k=1,npairs-1 DO values[k] = info.mapping.hash->get(list[k])
		
		current_val = info.mapping.hash->get( list[info.index] )
		
		IF info.mapping.complete THEN BEGIN
			values = values[UNIQ(values)]
			npairs = n_elements(values)
			
			start = WHERE(current_val EQ values, count)
			IF count EQ 0 THEN start = 0 ELSE start = start[0]
		
			result = DIALOG_COMBOINPUT( $
					DIALOG_PARENT = ev.TOP, $
					TITLE = 'New Key, Value Pair',$
					STATIC = ['Key   :','Value :'],$
					STATIN = [key,current_val],$
					LISTNAMES = ['New value:'],$
					LISTITEMS = [values],$
					LISTSIZES = [npairs],$
					LISTSTART = [start],$
					BIND_LSTATS_N = [0],$
					BIND_TO_STATIC_N = [1] $
					)
		END ELSE BEGIN
			start = WHERE(current_val EQ values, count)
			IF count EQ 0 THEN start = 0 ELSE start = start[0]
		
			result = DIALOG_COMBOINPUT( $
					DIALOG_PARENT = ev.TOP, $
					TITLE = 'New Key, Value Pair',$
					STATIC = ['Key   :','Value :'],$
					STATIN = [key,current_val],$
					LISTNAMES = ['New value:'],$
					LISTITEMS = [values],$
					LISTSIZES = [npairs],$
					LISTSTART = [start],$
					LISTEDIT = [0],$
					BIND_LSTATS_N = [0],$
					BIND_TO_STATIC_N = [1] $
					)
		END
		
		IF ((SIZE (result))[0] EQ 0) THEN RETURN
		
		text = result[1]
		
		nell = WIDGET_INFO(ev.ID,/COMBOBOX_NUMBER)
		
		info.mapping.hash->set, key, text
		v = text
		
		info.index = nell
		WIDGET_CONTROL, ev.ID, COMBOBOX_ADDITEM = key,$
						SET_UVALUE = info
		
	ENDIF
	
	BoundMessage, info.boundCnt, *info.boundIDs, v
END

PRO SelectionToPrompt, ev
	selection=WIDGET_INFO(ev.ID, /COMBOBOX_GETTEXT)
	WIDGET_CONTROL, ev.ID, GET_UVALUE=info	
	BoundMessage, info.boundCnt, *info.boundIDs, selection
END

PRO indextouval, ev
	WIDGET_CONTROL, ev.ID , GET_UVALUE=info
	IF ev.INDEX GE 0 THEN info.index = ev.INDEX
	;print, 'INDEX',info.index
	WIDGET_CONTROL, ev.ID , SET_UVALUE=info
END

PRO BoundMessage, count, wIDs, text
	FOR w=0, count-1 DO BEGIN
		WIDGET_CONTROL, wIDs[w], SET_VALUE=text
	ENDFOR
END

PRO slaveCATCH_VALUE, id, value
	WIDGET_CONTROL, id, GET_UVALUE=slave, PRO_SET_VALUE=''

	IF (slave.route NE '') THEN $
		val = CALL_FUNCTION(self.route, slave.govID) $
	ELSE BEGIN 
		val = slave.pref + value + slave.suff
	ENDELSE

	WIDGET_CONTROL, id, SET_VALUE=val
	WIDGET_CONTROL, id, PRO_SET_VALUE='slaveCATCH_VALUE'
END

PRO ShowHelp, ev, TITLE=title
	WIDGET_CONTROL,ev.ID,GET_UVALUE=help_text
	
	nlines = n_elements(help_text)
	
	base = WIDGET_BASE(GROUP_LEADER=ev.TOP, TITLE = title, /FLOATING)
	
	text = WIDGET_TEXT(base,VALUE = help_text, YSIZE=nlines)
	
	WIDGET_CONTROL,base,/REALIZE
END

FUNCTION LIST_GOVINIT, GOVERNOR=fWidgets, SLAVE=tWidgets, EVENTROUTE=route, $
	PREPEND = pre, APPEND = post, SCATCH=slavecatch
	
	govs = N_ELEMENTS(fWidgets)
	slaves = N_ELEMENTS(tWidgets)
	nrout = N_ELEMENTS(slavecatch)
	npref = N_ELEMENTS(pre)
	nsuff = N_ELEMENTS(post)
	IF govs EQ 0 THEN RETURN, 0
	IF N_ELEMENTS(route) EQ 0 THEN route = ''
	f_prev = 0
	s = 0
	FOR i=0, MAX([govs,slaves])-1 DO BEGIN
		IF i+1 GT govs THEN g = govs-1 ELSE g=i
		IF i+1 GT slaves THEN s = slaves-1 ELSE s=i
		WIDGET_CONTROL, fWidgets[g], GET_UVALUE = info
		IF (fWidgets[g] NE f_prev) THEN BEGIN
			IF SIZE(info,/TYPE) NE 8 THEN BEGIN
				info = {dialoginfo}
				info.calls = PTR_NEW([route])
			ENDIF ELSE BEGIN
				IF PTR_VALID(info.calls) THEN *info.calls = [*info.calls, route] $
				ELSE info.calls = PTR_NEW([route])
			ENDELSE
		ENDIF
		IF PTR_VALID(info.boundIDs) THEN BEGIN
			*info.boundIDs = [*info.boundIDs, tWidgets[s]]
			info.boundCnt++
		ENDIF ELSE BEGIN
			 info.boundIDs = PTR_NEW([tWidgets[s]])
			 info.boundCnt = 1
		ENDELSE
		WIDGET_CONTROL, fWidgets[g], $
			EVENT_PRO='CALL_LISTEVENTS', $
			SET_UVALUE = info
			
		;Fill the slave structure
		IF s+1 GT npref THEN pref = '' ELSE pref = pre[s]
		IF s+1 GT nsuff THEN suff = '' ELSE suff = post[s]
		
		WIDGET_CONTROL, tWidgets[s], GET_UVALUE = slave
		IF SIZE(slave,/TYPE) NE 8 THEN slave = {textslave}
		IF s+1 LE nrout THEN slave.route = slavecatch[s]
		slave.govID = fWidgets[g]
		slave.pref = pref
		slave.suff = suff
		WIDGET_CONTROL, tWidgets[s], $
			PRO_SET_VALUE='slaveCATCH_VALUE', $
			SET_UVALUE = slave
			
		f_prev = fWidgets[g]
	ENDFOR
	
	RETURN, 1
END

;-----------------------------------------------------------------------------------
; Main
; ****  DIALOG_COMBOINPUT ****
;
;-----------------------------------------------------------------------------------
	
FUNCTION DIALOG_COMBOINPUT, $
	PROMPT = prompt, INITIAL = initial, $
	STATIC = static, STATIN = stinit, $
	HELP = help_text, $
    TITLE = title, XSIZE = xsize, YSIZE = ysize, WIDTH = width, $
    NFIELDS = nfields, DIALOG_PARENT = dialog_parent, $
    OK = ok, CANCEL = cancel, $
    LISTITEMS = list, LISTNAMES = names, LISTSIZES = listsize, $
    LISTEDIT = edit, LISTSTART = liststart, $
    MAPLIST = listmapdef, $
    BIND_LISTS_N = bind, $
    BIND_TO_PROMPT_N = prbind, $
    BIND_LSTATS_N = bind2, $
    BIND_TO_STATIC_N = stbind, $
    PREPEND = pre, APPEND = post, RETURNLISTINDEX = return2
    
numFields = N_ELEMENTS (prompt)

IF (numFields GT 0) THEN BEGIN
	IF (N_ELEMENTS (initial) EQ 0) THEN BEGIN
	   value = REPLICATE ('', numFields)
	ENDIF ELSE BEGIN
	   IF (N_ELEMENTS (initial) NE numFields) THEN $
		  MESSAGE, 'INITIAL must be array of length NFIELDS.'
	   ; Trim non-string datatypes
	   ;
	   s = SIZE (initial)
	   IF (s[s[0]+1] NE 7) THEN BEGIN
		  value = STRTRIM (initial, 2)
	   ENDIF ELSE BEGIN
		  value = initial
	   ENDELSE   
	ENDELSE
ENDIF

IF (N_ELEMENTS (title) EQ 0) THEN title = 'dialog_input'
IF (N_ELEMENTS (ok) EQ 0) THEN ok = 'Ok'
IF (N_ELEMENTS (cancel) EQ 0) THEN cancel = 'Cancel'
HAVE_PARENT = N_ELEMENTS(dialog_parent) NE 0

;Start of WIDGET construction

; Dialogue BASE
IF (HAVE_PARENT) THEN BEGIN
   topBase = WIDGET_BASE (TITLE = title, /COLUMN, /BASE_ALIGN_CENTER, $
       /FLOATING, /MODAL, GROUP_LEADER = dialog_parent, $
       XSIZE = xsize, YSIZE = ysize)
ENDIF ELSE BEGIN
   topBase = WIDGET_BASE (TITLE = title, /COLUMN, /BASE_ALIGN_CENTER, MAP = 0, $
       XSIZE = xsize, YSIZE = ysize)   
ENDELSE

; TEXT entry widgets
IF (numFields GT 0) THEN BEGIN
   base = WIDGET_BASE (topBase, ROW = numFields, /GRID_LAYOUT)
   textID = LONARR (numFields)
   FOR i = 0,  N_ELEMENTS (prompt) - 1 DO BEGIN
       textID[i] = CW_FIELD (base, VALUE = value[i], XSIZE = width, $
           TITLE = prompt[i])
   ENDFOR 
ENDIF

; Static Value Display widgets
nstatic = N_ELEMENTS (static)
IF (nstatic GT 0) THEN BEGIN
	statID = LONARR(nstatic)
	IF (N_ELEMENTS(stinit) EQ 0) THEN stinit = REPLICATE('',nstatic)
	base = WIDGET_BASE (topBase, ROW = nstatic, /GRID_LAYOUT)
	FOR i = 0, nstatic - 1 DO BEGIN
		w = WIDGET_LABEL (base, VALUE = static[i])
		statID[i] = WIDGET_TEXT(base, VALUE = stinit[i], XSIZE = width)
	ENDFOR
ENDIF
		

; COMBOBOX widgets
IF (N_ELEMENTS(list) GT 0) THEN BEGIN
	nlists = N_ELEMENTS(listsize)
	IF nlists EQ 0 THEN BEGIN
		nlists = N_ELEMENTS(list)
		listsize = [nlists]
	ENDIF
	nstartpos = N_ELEMENTS(liststart)

	neditable = N_ELEMENTS(edit)
	nreturnsi = N_ELEMENTS(return2)
	bindings = N_ELEMENTS(bind)

	IF N_ELEMENTS(listmapdef) NE 0 THEN $
		nmappings = N_ELEMENTS(listmapdef.lists) $
	ELSE nmappings = 0
	
	IF nmappings EQ 0 THEN liststomap = [-1] ELSE liststomap = listmapdef.lists
	IF neditable EQ 0 THEN edit = [-1]
	IF nreturnsi EQ 0 THEN return2 = [-1]
	
	
	listbase = WIDGET_BASE(topBase,  ROW = nlists, /GRID_LAYOUT)
	;listname = WIDGET_BASE(listbase, /COLUMN, /BASE_ALIGN_LEFT)
	;listvals = WIDGET_BASE(listbase, /COLUMN, /BASE_ALIGN_RIGHT)
	j = 0
	k = 0
	r = 0
	m = 0
	listID = LONARR(nlists)
	
	FOR i = 0, nlists-1 DO BEGIN
		jsize = listsize[i]
		sublist = list[j:j+jsize-1]
		j += jsize
		
		w = WIDGET_LABEL(listbase, VALUE = names[i], /ALIGN_CENTER)
		
		IF edit[k] EQ i THEN BEGIN
			listID[i] = WIDGET_COMBOBOX(listbase, VALUE=sublist, /EDITABLE)
			IF k+1 LT neditable THEN k++
		ENDIF ELSE BEGIN
			listID[i] = WIDGET_COMBOBOX(listbase, VALUE=sublist)
		ENDELSE
		
		info = {dialoginfo}
		
		IF return2[r] EQ i THEN BEGIN
			IF PTR_VALID(info.calls) THEN *info.calls = ['indextouval', *info.calls] $
				ELSE info.calls = PTR_NEW(['indextouval'])
			WIDGET_CONTROL, listID[i], $
							$;FUNC_GET_VALUE='GETINDEX', $
							EVENT_PRO='CALL_LISTEVENTS'
			IF r+1 LT nreturnsi THEN r++
		ENDIF
		
		IF liststomap[m] EQ i THEN BEGIN
			info.mapping.hash = listmapdef.mappings[m]
			info.mapping.complete = listmapdef.completed[m]
			
			keys = info.mapping.hash->keys(count)
			keys = ''
			info.mapping.osize = count
			WIDGET_CONTROL, listID[i], $
							FUNC_GET_VALUE='', $
							EVENT_PRO='CALL_LISTEVENTS'
			IF m+1 LT nmappings THEN m++
		ENDIF		
		
		WIDGET_CONTROL, listID[i], SET_UVALUE = info
		
	ENDFOR
	
	IF (bindings GT 0 and N_ELEMENTS(prbinds) GT 0) THEN $
		stat = LIST_GOVINIT( $
			GOVERNOR = listID[bind], SLAVE = textID[prbind], $
			EVENTROUTE = ['ValueToPrompt','indextouval'] $
			)
	IF (N_ELEMENTS(statID) GT 0 and N_ELEMENTS(stbind) GT 0) THEN $
		stat = LIST_GOVINIT( $
			GOVERNOR = listID[bind2], SLAVE = statID[stbind], $
			EVENTROUTE = ['ValueToPrompt','indextouval'], PREPEND = pre, APPEND = post $
			)
ENDIF

; Ok, Cancel buttons
rowBase = WIDGET_BASE (topBase, /ROW, /GRID_LAYOUT)
ok = WIDGET_BUTTON (rowBase, VALUE = ok)
cancel = WIDGET_BUTTON (rowBase, VALUE = cancel)

IF N_ELEMENTS(help_text) NE 0 THEN $
	helpb = WIDGET_BUTTON(rowBase, Value = 'Help', $
			UVALUE = help_text) $
ELSE helpb = -99
    
    
; Map to screen
WIDGET_CONTROL, topBase, /REALIZE

; Place the dialog: window manager dependent
IF (NOT HAVE_PARENT) THEN BEGIN

   CURRENT_SCREEN = GET_SCREEN_SIZE()
   WIDGET_CONTROL, topBase, TLB_GET_SIZE = DIALOG_SIZE

   DIALOG_PT = [(CURRENT_SCREEN[0] / 2.0) - (DIALOG_SIZE[0] / 2.0), $ 
                (CURRENT_SCREEN[1] / 2.0) - (DIALOG_SIZE[1] / 2.0)] 

   WIDGET_CONTROL, topBase, $
                   TLB_SET_XOFFSET = DIALOG_PT[0], $
                   TLB_SET_YOFFSET = DIALOG_PT[1]
   WIDGET_CONTROL, topBase, MAP = 1

ENDIF

nfields = N_ELEMENTS(textID)
nlists = N_ELEMENTS(listID)

IF nlists GT 0 && nstartpos GT 0 THEN BEGIN
	FOR i=0,nstartpos-1 DO BEGIN
		list_wID = listID[i]
		startPos = liststart[i]
		;print,'startPos',startPOs
		WIDGET_CONTROL, list_wID, SET_COMBOBOX_SELECT = startPos, GET_UVALUE=info
		info.index = startPos
		mapped = OBJ_VALID(info.mapping.hash)
		WIDGET_CONTROL, list_wID, SET_UVALUE=info
		IF mapped THEN $
				WIDGET_CONTROL, list_wID,$
				SEND_EVENT={ID:0L,TOP:0L,HANDLER:0L,INDEX:startPos}
	ENDFOR
ENDIF



REDO:
value = STRARR(nfields + nstatic + nlists)

REPEAT BEGIN
	event = WIDGET_EVENT(topBase)
	IF event.ID eq helpb THEN ShowHelp,event, TITLE='HELP: '+title
ENDREP UNTIL event.ID eq ok OR event.ID eq cancel


IF event.ID eq cancel THEN BEGIN
	WIDGET_CONTROL, topBase, /DESTROY
	RETURN, ''
ENDIF

FOR i = 0, nfields - 1 DO BEGIN
	WIDGET_CONTROL, textID[i], GET_VALUE=v
	IF v eq '' THEN BEGIN
    		err=DIALOG_MESSAGE('There are empty fields', /ERROR, DIALOG_PARENT=topBase)
    		GOTO, REDO
    ENDIF
	value[i] = v
ENDFOR

FOR i = 0, nstatic - 1 DO BEGIN
	WIDGET_CONTROL, statID[i], GET_VALUE=v, GET_UVALUE=uv
	value[i+nfields] = v
	IF SIZE(uv,/TYPE) EQ 8 THEN HEAP_FREE, uv
ENDFOR

FOR i = 0, nlists - 1 DO BEGIN
	WIDGET_CONTROL, listID[i], GET_UVALUE=uv
	v = WIDGET_INFO(listID[i], /COMBOBOX_GETTEXT)
	IF v eq '' THEN BEGIN
    		err=DIALOG_MESSAGE('Warning: There were empty list fields', $
    					/ERROR, DIALOG_PARENT=topBase)
    ENDIF
	value[i+nfields+nstatic] = v
	
	IF SIZE(uv,/TYPE) EQ 8 THEN HEAP_FREE, uv, /PTR
ENDFOR

WIDGET_CONTROL, topBase, /DESTROY
RETURN, value

END