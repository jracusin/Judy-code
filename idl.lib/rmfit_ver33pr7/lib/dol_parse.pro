; DOL PARSER
 
; A program to capture DOL output, collect the data, and
; display in a table widget.  Information can then be 
; printed out in a format that suitable for the wiki.

; Written, April 2009, A. Goldstein 

;------------------------------------------------------------------------------------------------
; Event Handler
PRO DOL_PARSE_EVENT, event

COMMON table_select, table_row
COMMON baseid, base, table, i

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Retrieve structure stored in base widget
WIDGET_CONTROL, event.top, GET_UVALUE=dol_arr

; Event Handler
WIDGET_CONTROL, event.id, GET_UVALUE=uvalue
CASE event.id OF

  ; Make log
  (*dol_arr[0].IDs).makeID: BEGIN         
            file=DIALOG_PICKFILE(DEFAULT_EXTENSION='txt', PATH = !RM_PARAMS.lastPath, $
                                 FILE='dol2wiki.txt', DIALOG_PARENT=baseid, /WRITE)
            IF file EQ '' THEN RETURN
            
            ; Get structure and open file unit
            WIDGET_CONTROL, event.top, GET_UVALUE=dol_arr
            OPENW, lun, file, /GET_LUN
            header="||<:rowbgcolor="+STRING("""#8080FF""")+"> ||<:> '''Time bin'''||"+ $
                   "<:> '''Dt (sec)'''||<:> '''RA (deg)'''||<:> '''Dec (deg)'''||"+ $
                   "<:> '''Err (deg)'''||<:> '''LAT angle (deg)'''||<:> '''Chi^2^ with norm'''||"+ $
                   "<:> '''Soft.ratio'''||"
            
            ; Print header and formatted lines of structure
            PRINTF, lun, header, FORMAT='(a,$)'
            FOR j=0, N_ELEMENTS(dol_arr)-1 DO BEGIN
              k=STRCOMPRESS(STRING(j+1), /REMOVE_ALL)
              entry='||' + k + '||  ||' + (dol_arr[j]).time + '||' + (dol_arr[j]).ra + '||' + $
              (dol_arr[j]).dec + '||' + (dol_arr[j]).err + '||' + (dol_arr[j]).lat_ang + '||' + $
              (dol_arr[j]).chisq + '||' + (dol_arr[j]).soft +'||'
              PRINTF, lun, ''
              PRINTF, lun, entry, FORMAT='(a,$)'
            ENDFOR
            CLOSE, lun
            FREE_LUN, lun
            
            PRINT, 'Printed table to '+ file
            
            ; If running OS X, launch the wiki table (only works for Mac OS X),
            ; otherwise file can be found in the DOL parent folder
            IF !VERSION.OS_NAME EQ 'Mac OS X' THEN SPAWN, "open " + file
          END
          
  ; Delete row
  (*dol_arr[0].IDs).delID: BEGIN  
              ; If greater than one row, delete selected row
              WIDGET_CONTROL, event.top, GET_UVALUE=dol_arr
              IF N_STRUCT(table_row) NE 0 THEN BEGIN
                IF (N_ELEMENTS(dol_arr) GT 1) AND (N_ELEMENTS(table_row.i) EQ 1) THEN BEGIN
                  WIDGET_CONTROL, (dol_arr[0]).table, /DELETE_ROWS
                  first=0
                  last=N_ELEMENTS(dol_arr)-1
                  
                  ; Delete row from structure array
                  CASE table_row.i OF
                    first: dol_arr=dol_arr[1:*]
                    last: dol_arr=dol_arr[first:last-1]
                    ELSE: dol_arr=[dol_arr[first:table_row.i-1], dol_arr[table_row.i+1:last]]
                  ENDCASE
                  
                  ; Reindex structure array
                  FOR n=0, N_ELEMENTS(dol_arr)-1 DO dol_arr[n].i=n
                  i=i-1
                  
                  ; Save structure array
                  WIDGET_CONTROL, event.top, SET_UVALUE=dol_arr
                  WIDGET_CONTROL, (dol_arr[0]).table, SET_VALUE=dol_arr
                  
                ENDIF
                
              ENDIF 
            END
   
  ; Show more information         
  (*dol_arr[0].IDs).moreID: DOL_PARSE_MORE, event
  
  ; Exit
  (*dol_arr[0].IDs).closeID: DOL_PARSE_EXIT, event
  
  ; Row selection
  (*dol_arr[0].IDs).tableID: BEGIN
                   ; Retrieve table's selection mode and selection
                   disjoint=WIDGET_INFO((dol_arr[0]).table, /TABLE_DISJOINT_SELECTION)
                   selection=WIDGET_INFO((dol_arr[0]).table, /TABLE_SELECT)

                   ; Check to see whether selection exists
                   IF selection[0] NE -1 THEN has_select=1 ELSE has_select=0

                   ;  If there is a  valid selection, get the value
                   numrows=N_ELEMENTS(dol_arr)
                   
                   IF event.type EQ 4 THEN BEGIN
                   IF (has_select) AND (event.sel_top LE numrows-1) AND (event.sel_top NE -1) THEN $
                   WIDGET_CONTROL, (dol_arr[0]).table, GET_VALUE=table_row, /USE_TABLE_SELECT
                   ENDIF
                 END
  
  ; Resize event               
  (*dol_arr[0].IDs).baseID: BEGIN
                  
                  ; Get new size of base widget
                  x=event.x
                  y=event.y

                   ; Calculate difference in size between new and old
                  dx=FLOAT(x)-FLOAT((*dol_arr[0].IDs).xsize)
                  dy=FLOAT(y)-FLOAT((*dol_arr[0].IDs).ysize)

                  ; Change table size by that difference
                  WIDGET_CONTROL, (*dol_arr[0].IDs).tableID, SCR_XSIZE=(*dol_arr[0].IDs).cols+dx, $
                                  SCR_YSIZE=(*dol_arr[0].IDs).rows+dy
                  
                  ; Store new table size
                  (*dol_arr[0].IDs).cols=(*dol_arr[0].IDs).cols+dx
                  (*dol_arr[0].IDs).rows=(*dol_arr[0].IDs).rows+dy

                  ; Store new base size
                  (*dol_arr[0].IDs).xsize=x
                  (*dol_arr[0].IDs).ysize=y
                END
                
  ELSE: PRINT, 'Oops!'                      ; In the event there is some unforeseen event 

ENDCASE

END

;------------------------------------------------------------------------------------------------
; Parse DOL
PRO DOL_PARSE_GET, base_id

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  PRINT, 'MORE THAN LIKELY YOU HAVE AN OLD VERSION OF THE DOL OR YOU HAVE NOT COMPILED IT YET'
  RETURN
ENDIF

;file='~/Desktop/dol_output.txt'
; Read temp file
file='dol_output.txt'

; Read in dol output
OPENR, FL, file, /GET_LUN
line = ' '
full_file = ' '
num_lines = 0
WHILE NOT EOF(FL) DO BEGIN
  READF, FL, line
  full_file = [full_file, line]
  num_lines = num_lines + 1
ENDWHILE
CLOSE, fl
FREE_LUN, FL
full_file = full_file(1:*)

; Get stored structure
WIDGET_CONTROL, base_id, GET_UVALUE=dol_arr
numrows=N_ELEMENTS(dol_arr)
dol_out=dol_arr[numrows-1]

; Find where RA, Dec live in output
loc_marker=' Begin Standard output'
loc_index=WHERE(STRPOS(full_file, loc_marker) NE -1)
loc_line=full_file[loc_index]
locs=STRMID(loc_line, STRPOS(loc_line, ':')+1)
locs2=STRSPLIT(locs)
ra=STRCOMPRESS(STRMID(locs, locs2[0], locs2[1]-locs2[0]), /REMOVE_ALL)
dec=STRCOMPRESS(STRMID(locs, locs2[1], locs2[2]-locs2[1]), /REMOVE_ALL)
err=STRCOMPRESS(STRMID(locs, locs2[2]), /REMOVE_ALL)
dol_out.ra=ra
dol_out.dec=dec
dol_out.err=err

; Find where LAT angle lives in output
lat_marker=' J2000: dist from source to z-axis (this prog)'
lat_index=WHERE(STRPOS(full_file, lat_marker) NE -1)
lat_line=full_file[lat_index]
lat_ang=STRCOMPRESS(STRMID(lat_line, STRPOS(lat_line, ')')+1), /REMOVE_ALL)
lat_ang=DECIMALS(lat_ang, 2)
dol_out.lat_ang=lat_ang

; Find where chisq lives in output
chi_marker=' chi2 with norm'
chi_index=WHERE(STRPOS(full_file, chi_marker) NE -1)
chi_line=full_file[chi_index]
;chisq=STRCOMPRESS(STRMID(chi_line, STRPOS(chi_line, 'm')+1), /REMOVE_ALL)
chisq=STRSPLIT(chi_line, /EXTRACT)
chisq=chisq[3]
chisq=DECIMALS(chisq, 2)
dol_out.chisq=chisq

; Find where softness ratio lives in output
soft_marker=' data signif'
soft_index=WHERE(STRPOS(full_file, soft_marker) NE -1)
soft_line=full_file[soft_index]
soft=STRCOMPRESS(STRMID(soft_line, STRPOS(soft_line, 'ratio')+6), /REMOVE_ALL)
soft=DECIMALS(soft, 2)
dol_out.soft=soft

; Find where the geocenter azimuth/zenith live
geoloc_marker='geocenter az'
geoloc_index=WHERE(STRPOS(full_file, geoloc_marker) NE -1)
geoloc_line=full_file[geoloc_index]
geoloc=STRMID(geoloc_line, STRPOS(geoloc_line, 'zen')+4)
geoloc2=STRSPLIT(geoloc)
geo_az=STRCOMPRESS(STRMID(geoloc, geoloc2[0], geoloc2[1]-geoloc2[0]), /REMOVE_ALL)
geo_zen=STRCOMPRESS(STRMID(geoloc, geoloc2[1]), /REMOVE_ALL)
geo_az=DECIMALS(geo_az, 2)
geo_zen=DECIMALS(geo_zen, 2)
(*dol_out.extras).geo_az=geo_az
(*dol_out.extras).geo_zen=geo_zen

; Find where the source to geocenter angle lives
geosrc_marker='J2000: dist from source to geocenter (this prog)'
geosrc_index=WHERE(STRPOS(full_file, geosrc_marker) NE -1)
geosrc_line=full_file[geosrc_index]
geosrc=STRCOMPRESS(STRMID(geosrc_line, STRPOS(geosrc_line, ')')+1), /REMOVE_ALL)
geosrc=DECIMALS(geosrc, 2)
(*dol_out.extras).geosrc=geosrc

; Find where the source azimuth/zenith live
srcloc_marker='GRB Az, Zen (Fermi):'
srcloc_index=WHERE(STRPOS(full_file, srcloc_marker) NE -1)
srcloc_line=full_file[srcloc_index]
srcloc=STRMID(srcloc_line, STRPOS(srcloc_line, ':')+1)
srcloc2=STRSPLIT(srcloc)
src_az=STRCOMPRESS(STRMID(srcloc, srcloc2[0], srcloc2[1]-srcloc2[0]), /REMOVE_ALL)
src_zen=STRCOMPRESS(STRMID(srcloc, srcloc2[1]), /REMOVE_ALL)
(*dol_out.extras).src_az=src_az
(*dol_out.extras).src_zen=src_zen

; Find where the galactic coordinates live
galloc_marker='Galactic coords:'
galloc_index=WHERE(STRPOS(full_file, galloc_marker) NE -1)
galloc_line=full_file[galloc_index]
galloc=STRMID(galloc_line, STRPOS(galloc_line, ':')+1)
galloc2=STRSPLIT(galloc)
longl=STRCOMPRESS(STRMID(galloc, galloc2[0], galloc2[1]-galloc2[0]), /REMOVE_ALL)
latb=STRCOMPRESS(STRMID(galloc, galloc2[1]), /REMOVE_ALL)
(*dol_out.extras).longl=longl
(*dol_out.extras).latb=latb

; Find where the detector to Earth angles live
earth_ang_marker='Detector Earth angles:'
earth_ang_start=WHERE(STRPOS(full_file, earth_ang_marker) NE -1)+1
earthangs=STRARR(1,12)
FOR n=0, 11 DO BEGIN
  earth_ang_line=full_file[earth_ang_start + n]
  earth_ang=STRSPLIT(earth_ang_line)
  earth_ang2=STRCOMPRESS(STRMID(earth_ang_line, earth_ang[1], earth_ang[2]-earth_ang[1]), /REMOVE_ALL)
  earthangs[0,n]=DECIMALS(earth_ang2, 2)
ENDFOR
(*dol_out.extras).earthangs=earthangs

; Find where the detector to source angles live
src_ang_marker='Source angles (this prog):'
src_ang_start=WHERE(STRPOS(full_file, src_ang_marker) NE -1)+1
srcangs=STRARR(1,12)
FOR n=0, 11 DO BEGIN
  src_ang_line=full_file[src_ang_start + n]
  src_ang=STRSPLIT(src_ang_line)
  src_ang2=STRCOMPRESS(STRMID(src_ang_line, src_ang[1], src_ang[2]-src_ang[1]), /REMOVE_ALL)
  srcangs[0,n]=DECIMALS(src_ang2, 2)
ENDFOR
(*dol_out.extras).srcangs=srcangs

; Find where the detectors' azimuth/elevation live
detcoords_marker='Source angles (this prog):'
detcoords_start=WHERE(STRPOS(full_file, detcoords_marker) NE -1)+1
detcoords=STRARR(2,12)
FOR n=0,11 DO BEGIN
  detcoords_line=full_file[detcoords_start + n]
  az_el=STRSPLIT(detcoords_line)
  azimuth=STRCOMPRESS(STRMID(detcoords_line, az_el[2], az_el[3]-az_el[2]), /REMOVE_ALL)
  elevation=STRCOMPRESS(STRMID(detcoords_line, az_el[3]), /REMOVE_ALL)
  detcoords[*,n]=[DECIMALS(azimuth, 2), DECIMALS(elevation, 2)]
ENDFOR
(*dol_out.extras).detcoords=detcoords

; Find where the dead time for each detector lives
dead_marker='DEADTIME:'
dead_start=WHERE(STRPOS(full_file, dead_marker) NE -1)
dead=STRARR(1,12)
FOR n=0, 11 DO BEGIN
  dead_line=full_file[dead_start + n/2]
  detdead=STRSPLIT(dead_line)
  IF n EQ 0 THEN dead[0,n]=STRCOMPRESS(STRMID(dead_line, detdead[2], detdead[3]-detdead[2]), /REMOVE_ALL)
  IF n EQ 1 THEN dead[0,n]=STRCOMPRESS(STRMID(dead_line, detdead[4]), /REMOVE_ALL)
  
  IF (n GT 1) AND (n MOD 2 EQ 0) THEN $
    dead[0,n]=STRCOMPRESS(STRMID(dead_line, detdead[1], detdead[2]-detdead[1]), /REMOVE_ALL)

  IF (n GT 1) AND (n MOD 2 NE 0) THEN $
    dead[0,n]=STRCOMPRESS(STRMID(dead_line, detdead[3]), /REMOVE_ALL)
    
  dead[0,n]=DECIMALS(dead[0,n], 4)
ENDFOR
(*dol_out.extras).dead=dead
  
; Store structure in base widget and update table
dol_arr[numrows-1]=dol_out
WIDGET_CONTROL, base_id, SET_UVALUE=dol_arr
WIDGET_CONTROL, (dol_arr[0]).table, TABLE_YSIZE=numrows, SET_VALUE=dol_arr, ALIGNMENT=1

; Delete temp file
FILE_DELETE, file

END

;------------------------------------------------------------------------------------------------
; Build Extra Info Table
PRO DOL_PARSE_MORE, event

; Catch any possible errors
 CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

COMMON table_select, table_row

; If valid row is selected, then launch additional window
IF (N_STRUCT(table_row) NE 0) AND (N_ELEMENTS(table_row) EQ 1) THEN BEGIN

  ro=table_row.i
  WIDGET_CONTROL, event.top, GET_UVALUE=dol_arr
  COMMON baseid, base, table, i
  
  ; Define base widget
  title='LOCATION #'+STRING(ro)
  more=WIDGET_BASE(TITLE=title, /COLUMN, GROUP_LEADER=base, /BASE_ALIGN_CENTER)
  
  ; Define row1
  infobar=WIDGET_BASE(more, /ROW)
  text=WIDGET_LABEL(infobar, VALUE='Source Azimuth/Zenith: ')
  input=WIDGET_LABEL(infobar, VALUE=(*dol_arr[ro].extras).src_az + ', ' + (*dol_arr[ro].extras).src_zen)
  text=WIDGET_LABEL(infobar, VALUE='    Galactic LongL/LatB: ')
  input=WIDGET_LABEL(infobar, VALUE=(*dol_arr[ro].extras).longl + ', ' + (*dol_arr[ro].extras).latb)
  
  ; Define row2
  infobar2=WIDGET_BASE(more, /ROW)
  text=WIDGET_LABEL(infobar2, VALUE='Geo Azimuth/Zenith:    ')
  input=WIDGET_LABEL(infobar2, VALUE=(*dol_arr[ro].extras).geo_az + ', ' + (*dol_arr[ro].extras).geo_zen)
  text=WIDGET_LABEL(infobar2, VALUE='    Geo/Source Angle:   ')
  input=WIDGET_LABEL(infobar2, VALUE=(*dol_arr[ro].extras).geosrc)
  
  ; Define table
  columns2=['Det/Source Angles', 'Det/Earth Angles', 'Deadtime', 'Det. Azimuth', 'Det. Elevation']
  rows=['Det. 0', 'Det. 1', 'Det. 2', 'Det. 3', 'Det. 4', 'Det. 5', 'Det. 6', 'Det. 7', 'Det. 8', $
        'Det. 9', 'Det. A', 'Det. B']
  widths2=[110, 110, 80, 80, 90]
  info=[(*dol_arr[ro].extras).srcangs, (*dol_arr[ro].extras).earthangs, (*dol_arr[ro].extras).dead, $
        (*dol_arr[ro].extras).detcoords]
  table2=WIDGET_TABLE(more, VALUE=info, COLUMN_LABELS=columns2, ROW_LABELS=rows, $
                      COLUMN_WIDTHS=widths2, ALIGNMENT=1, XSIZE=5, YSIZE=12)
 
  ; Realize widget                   
  WIDGET_CONTROL, more, /REALIZE

ENDIF

END

;------------------------------------------------------------------------------------------------
; Close Table
PRO DOL_PARSE_EXIT, event

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Free pointers
WIDGET_CONTROL, event.top, GET_UVALUE=dol_arr
FOR n=0, N_ELEMENTS(dol_arr)-1 DO BEGIN
  IF PTR_VALID(dol_arr[n].IDs) THEN dummy=TEMPORARY(*dol_arr[n].IDs)
  PTR_FREE, dol_arr[n].IDs
ENDFOR

; Destroy the top level base
WIDGET_CONTROL, event.top, /DESTROY

; Call and destroy all variables in common blocks
COMMON baseid, base, table, i
COMMON table_select, table_row
base=0 & dummy=TEMPORARY(base)
table=0 & dummy=TEMPORARY(table)
i=0 & dummy=TEMPORARY(i)
table_row=0 & dummy=TEMPORARY(table_row)

RETURN

END

;------------------------------------------------------------------------------------------------

PRO DOL_PARSE_CLEANUP, id

; Get pointers and free them
WIDGET_CONTROL, id, GET_UVALUE=dol_arr

FOR n=0, N_ELEMENTS(dol_arr)-1 DO PTR_FREE, dol_arr[n].extras

END

;------------------------------------------------------------------------------------------------
; Build Table Widget
PRO DOL_PARSE, timerange

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

; Convert timerange from rmfit to string and concatenate to a single string
timerange=STRCOMPRESS(STRING(timerange), /REMOVE_ALL)
timerange[0]=DECIMALS(timerange[0], 3)
timerange[1]=DECIMALS(timerange[1], 3)
time=timerange[0] + ' : ' + timerange[1]

; Check if dol table already exists
COMMON baseid, base, table, i
IF (N_ELEMENTS(base) EQ 0) THEN BEGIN 

  ; If not, create widget hierarchy 
  columns=['Time Range (s)', 'RA (deg)', 'Dec (deg)', 'Err (deg)', 'LAT angle (deg)', 'Chi2 w/ norm', $
           'Soft. ratio']
  widths=[90, 60, 60, 60, 100, 80, 70]
  
  base=WIDGET_BASE(TITLE='DOL OUTPUT', /COLUMN, /BASE_ALIGN_CENTER, TLB_SIZE_EVENTS=1)
  table=WIDGET_TABLE(base, COLUMN_LABELS=columns, COLUMN_WIDTHS=widths, ALIGNMENT=1, $
                     UVALUE='table event', XSIZE=7, $;X_SCROLL_SIZE=7,$
                     Y_SCROLL_SIZE=6, /ALL_EVENTS)
  info=WIDGET_LABEL(base, VALUE='Select row to delete or show more info', /FRAME)
  bar=WIDGET_BASE(base, /ROW)
  button1=WIDGET_BUTTON(bar, VALUE='MAKE TABLE', UVALUE='make')
  button2=WIDGET_BUTTON(bar, VALUE='DELETE ROW', UVALUE='delete')
  button3=WIDGET_BUTTON(bar, VALUE='MORE INFO...', UVALUE='more')
  button4=WIDGET_BUTTON(bar, VALUE='CLOSE', UVALUE='close')

  ; Realize table widget
  WIDGET_CONTROL, base, /REALIZE

  ; Create structure
  i=0
  extras={geo_az:'', geo_zen:'', geosrc:'', src_az:'', src_zen:'', longl:'', latb:'', $
          earthangs:STRARR(1,12), srcangs:STRARR(1,12), detcoords:STRARR(2,12), dead:STRARR(1,12)}
  extra_ptr=PTR_NEW(extras)
  
  IDs={baseID:base, tableID:table, makeID:button1, delID:button2, moreID:button3, closeID:button4, $
       xsize:0L, ysize:0L, rows:0L, cols:0L}
  id_ptr=PTR_NEW(IDs) 
  
  dol_out={time:time, ra:'', dec:'', err:'', lat_ang:'', chisq:'', soft:'', $
           extras:extra_ptr, ids:id_ptr, table:table, i:i}
  dol_arr=dol_out
  
  ; Get base widget size
  getsize=WIDGET_INFO(base, /GEOMETRY)
  (*dol_arr[0].IDs).xsize=getsize.scr_xsize+(2*getsize.margin)
  (*dol_arr[0].IDs).ysize=getsize.scr_ysize+(2*getsize.margin)
  
  ; Get table size
  getsize=WIDGET_INFO(table, /GEOMETRY)
  (*dol_arr[0].IDs).cols=getsize.scr_xsize+(2*getsize.margin)
  (*dol_arr[0].IDs).rows=getsize.scr_ysize+(2*getsize.margin)
  
ENDIF ELSE BEGIN

  ;If exists dol table already exists, add another row to structure/table
  i=i+1
  extras={geo_az:'', geo_zen:'', geosrc:'', src_az:'', src_zen:'', longl:'', latb:'', $
          earthangs:STRARR(1,12), srcangs:STRARR(1,12), detcoords:STRARR(2,12), dead:STRARR(1,12)}
  extra_ptr=PTR_NEW(extras)        
  
  IDs={baseID:0L, tableID:0L, makeID:0L, delID:0L, moreID:0L, closeID:0L, $
       xsize:0L, ysize:0L, rows:0L, cols:0L}
  id_ptr=PTR_NEW(IDs) 
  
  dol_out={time:time, ra:'', dec:'', err:'', lat_ang:'', chisq:'', soft:'', $
           extras:extra_ptr, ids:id_ptr, table:table, i:i}
  
  WIDGET_CONTROL, base, GET_UVALUE=dol_arr
  *dol_out.ids=*(dol_arr[0]).ids
  dol_arr=[dol_arr,dol_out]

ENDELSE

; Store structure in base widget
WIDGET_CONTROL, base, SET_UVALUE=dol_arr

XMANAGER, 'DOL_PARSE', base, CLEANUP='dol_parse_cleanup', /NO_BLOCK

; Call to fill structures
DOL_PARSE_GET, base

END