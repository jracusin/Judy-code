; $Id: map_viewer.pro,v 1.2 1993/05/27 22:00:20 troy Exp $
;
;  NOTE: This version of map_viewer should ONLY be run under IDL4.  It will
;	not work under IDL5!
;
;	This program reads an Aitoff image of the sky in galactic coordinates
;		and allows the user to rotate it arbitrarily, convert it
;		to celestial or ecliptic coordinates, reproject it to
;		any one of 12 possible projections, and draw points or
;		lines on it.  It is based heavily on the V3.6 map_demo
;		procedure supplied with IDL.  It is entirely widget-driven.
;
;	File structure for points or lines to plot:
;		1) projection: 'ecliptic', 'galactic', or 'celestial'
;		2) pairs of coordinates in degrees
;	    For files containing lines ('*.lines'), pairs of -99.0, -99.0
;		will break the line segment (lift the 'pen').
;
;	Modified: 4/30/96 by jam to add zoom feature.
;		09/19/96 by DNB: added hist_equal function to make all
;			maps viewable.  This isn't ideal for everything,
;			but it works for everything.
;
;  To Do:
;
;	1) Improve histogram color mapping.  There are actually several 
;		problems here.  The first is that the readfits routine
;		doesn't return valid BLANK values, so the automatic
;		scaling includes the BLANK values, which corresponds to
;		a large fraction of the map for HEAO-1 maps and totally
;		messes up the color lookup table scaling.  I think that
;		this needs to be fixed by changing readfits to return
;		NAN values for blank pixels, since many of the IDL routines
;		can be set to ignore NAN values.
;	Todo: add options to Palette button to permit rescaling of data.
;
;   Bugs:
;	1) When palette is changed, lines and symbols cannot be displayed
;		until a new map is read in.  No idea why.
;
; ***********************************************************************
; ***********************************************************************
@map_viewer_subs
; ***********************************************************************
; ***********************************************************************




; ***********************************************************************
; ***********************************************************************
pro draw_map
; ***********************************************************************
; ***********************************************************************

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

@map_viewer_common

	projnum = [15, 10, 14, 6, 9, 8, 1, 2, 4, 7, 5, 3]
	iproj = projnum(numproj)  ; selects correct projection

	;if last_p eq iproj then return	;Nothing to do?
	last_p = iproj
	if iproj ne 7 then begin	; projection not satellite
		IF (XRegistered("map_viewer_circle")) THEN BEGIN
		  WIDGET_CONTROL, conn_two, SENSITIVE = 1
		  WIDGET_CONTROL, great_cir, SENSITIVE = 1
		ENDIF
		IF sat_base(0) NE 0 THEN BEGIN
			;Kill satellite base if active
			if (widget_info(sat_base(0),/valid)) then  $
				WIDGET_CONTROL, sat_base(0),/DESTROY
			sat_base(0) = 0
		ENDIF
	endif		


lon1 = lon0
lat1 = lat0
oldlat = lat1

;		Take care of special cases:
;
IF iproj EQ 3 THEN BEGIN	;conic?
	if (lat1 eq 0) and (lon1 eq 0) then lat1 = 45
	lat1 = (lat1 > (-85)) < 85	;Adjust ranges
	lon1 = (lon1 > (-85)) < 85
	if (lat1 * lon1) lt 0 then BEGIN
		WIDGET_CONTROL, l2_label, $
			SET_VALUE = [ 'Illegal values for Conic']
		return
		endif
	WIDGET_CONTROL, LAT_SLIDER, SET_VALUE = lat1	;Update the sliders
	WIDGET_CONTROL, LON_SLIDER, SET_VALUE = lon1
	WIDGET_CONTROL, l2_label, SET_VALUE = [' ']
ENDIF
			

IF iproj EQ 14 THEN lat1 = 0	;Sinusoidal?
		

;**********	Draw the map....

if iproj eq 14 then j = 11 $
else if iproj eq 15 then j = 12 else j = iproj	;Kludge


wset, map_window

IF iproj EQ 3 THEN BEGIN	;Conic
	if (lat1 lt 0) or (lon1 lt 0) THEN limit = [-80,-180,0,180] $
	else limit = [0,-180,80,180]
ENDIF ELSE limit = 0		;Let map_set make limits for others

widget_control, /HOURGLASS

map_set, lat1, lon1, rot0, PROJ = iproj, grid=0, $
	sat_p = sat_params, $
	title = 'File ' + filename + ': ' + projs(numproj) + ' projection, ' $
		+ coords + ' coordinates', limit = limit


a = widget_base(TITLE='Note', /COLUMN, XOFFSET=400, YOFFSET=300)
junk = widget_label(a, value='Warping sky data to maps can')
junk = widget_label(a, value='require a significant amount of time.')
widget_control, a, /REAL

tvscl, map_image(hist_equal(image), x0, y0, latmin=-90, latmax = 89.5, $
		/BILIN, /WHOLE), x0, y0	;Show it

widget_control, a, /DESTROY

;dummy=''
;read,'Ready to rescale image?',dummy
;h_eq_ct

; map_set, lat1, lon1, rot0, PROJ=iproj, limit = limit, $
; 	/noerase, grid=0, sat_p = sat_params

temp = !map.out
if !map.out(4) eq (-90) then !map.out(4) = -75	;Don't do polar regions
if !map.out(5) eq 90 then !map.out(5) = 75
map_grid, latdel = 30, londel = 30, color=3
!map.out = temp			;Restore limits
lat1 = oldlat

return
end


; ***********************************************************************
; ***********************************************************************
pro map_viewer_event, event
; ***********************************************************************
; ***********************************************************************

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

@map_viewer_common

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
s = size(eventval)

if (idebug ge 1) then begin
	print, 'Event size: ', s
	if (s(1) gt 0) then print, 'Event value: ', eventval
	print,'Event.id = ', event.id
	if (event.id eq drawable) then begin
		print,'Event.press = ', event.press
		print,'!x.s(1), !x.type = ', !x.s(1), !x.type	
		print,'!y.s(1), !y.type = ', !y.s(1), !y.type	
	endif
	print,''
endif

if (map_window ne 0) then wset, map_window
if event.id eq drawable then begin	;Update lat/lon text widget?
	if event.press eq 0 then return
;	if (!x.s(1) eq 0) or (!x.type ne 2) then return
	if (!x.s(1) eq 0) then return
	!x.type = 2	; set to 2 here because xloadct resets this to 0
	p = convert_coord(event.x, event.y, /device, /to_data) ;Get coords
	if (idebug ge 1) then begin
		print, ''
		print, 'x,y = ', event.x, event.y
		print, 'p = ', p
	endif
	p(0) = -p(0)       ; reverse longitude for astronomy
	if (p(0) lt 0.0) then p(0) = p(0) + 360.0
  set_ll:
	WIDGET_CONTROL, l2_label, SET_VALUE = ' '
	
	case coords of 
	    'galactic': begin
		WIDGET_CONTROL, ll_label, set_value = [ $
		    'Galactic Longitude = ' + string(p(0),format='(F6.1)'), $
		    'Galactic Latitude  = ' + string(p(1),format='(F6.1)')]
		endcase
	    'ecliptic': begin
		WIDGET_CONTROL, ll_label, set_value = [ $
		    'Ecliptic Longitude = ' + string(p(0),format='(F6.1)'), $
		    'Ecliptic Latitude  = ' + string(p(1),format='(F6.1)')]
		endcase
	    'celestial': begin
		WIDGET_CONTROL, ll_label, set_value = [ $
		    'RA = ' + string(p(0),format='(F6.1)') + ' degrees', $
		    'Dec  = ' + string(p(1),format='(F6.1)') + ' degrees']
		endcase
	endcase

	IF ABS(p(0)) gt 360 THEN RETURN	;Valid point?

	case cir.llflag of
		0: begin	; write map value
			case coords of
			    'celestial': begin
				ra = p(0)	; use epoch 2000 internally
				dec = p(1)
				precess, ra, dec, 2000.0, 1950.0   ; precess to 1950
							;   (required by euler)
				euler, ra, dec, glong, glat, 1	; convert to galactic
					end
	    		    'ecliptic': begin
				elong = p(0)
				elat = p(1)
				euler, elong, elat, glong, glat, 5 ; perform conversion
	    				end
			    'galactic': begin
				glong = p(0)
				glat = p(1)
					end
			endcase
			map_viewer_lbpix,glong,glat,xcenter,nx,ny,ix,iy
			if (idebug ge 1) then print, 'ix, iy = ', ix, iy
			value = data(ix,iy)
			if (idebug ge 1) then print, 'map value = ', value
			WIDGET_CONTROL, l2_label, $
				SET_VALUE = 'I = ' $
					+ string(value,format='(F6.1)') $
					+ ' ' + units
		   end
		1: begin	; Mark great circle - first point
			plots, -p(0), p(1), psym=4   ; show first point
			WIDGET_CONTROL, l2_label, SET_VALUE = $
			 	'Mark second point.'
			cir.ll = p(0:1)
			cir.llflag = 2
		   end
		2: begin	; 2nd point
			plots, -p(0), p(1), psym=4   ; show second point
			calc_distance, cir.ll, p(0:1)
			cir.llflag = 0
		   end
	endcase

	return
endif					;Cursor hit on map

if s(1) eq 7 then CASE eventval OF	;String value?
	"LAT_SLIDER":	begin
				lat0 = event.value
				draw_map
				return
			endcase
	"LON_SLIDER":	begin
				lon0 = -1*event.value
				if (lon0 gt -180) then lon0 = lon0 + 360
				draw_map
				return
			endcase
	"ROT_SLIDER":	begin
				rot0 = event.value
				draw_map
				return
			endcase
	"SALT"      :	begin
				sat_params(0) = 1.0 + event.value / 6371.
				draw_map
				return
			endcase
	"SALPHA"    :   begin
				sat_params(1) = event.value
				draw_map
				return
			endcase
	"SBETA"	    :   begin
				sat_params(2) = event.value
				draw_map
				return
			endcase
	"SDONE"	    :   begin
				WIDGET_CONTROL, sat_base(0),/DESTROY
				sat_base(0) = 0
			endcase
	"CROT_SLIDER" : begin
				cir.rot = event.value
				draw_map
				return
			endcase
	"CLON_SLIDER":	begin
				cir.lon0 = event.value
				if cir.lon0 lt 0 then cir.lon0 = 180+cir.lon0
				draw_map
				return
			endcase

	"MEASURE" : begin
			cir.llflag = 1		;Expecting 1st point
			message = widget_message(/information,  $
				dialog_parent=drawable, $
				'Mark two points by clicking mouse on map')
			WIDGET_CONTROL, l2_label, SET_VALUE = $
			  'Mark first point.'
			return
			endcase
	"RESET" : begin			;Set center / rotation to 0.
			lat0 = 0
			lon0 = 0
			rot0 = 0
			WIDGET_CONTROL, LAT_SLIDER, SET_VALUE = lat0
			WIDGET_CONTROL, LON_SLIDER, SET_VALUE = lon0
			WIDGET_CONTROL, ROT_SLIDER, SET_VALUE = rot0
			map_viewer_color
			draw_map
		endcase
	"EXIT" : begin 			;Adios!!  Restore prev color tbls
		IF N_ELEMENTS(r_curr) GT 0 THEN TVLCT, r_curr, g_curr, b_curr
		WIDGET_CONTROL, event.top, /DESTROY
		RETURN
		endcase
	"HELP" : begin			;Display help text
		XDisplayFile, $
			'/pkg/xray/idl_lib/planning/map_viewer_help.txt', $
			TITLE = "Map Viewer Help", $
			GROUP = event.top, $
			WIDTH = 72, HEIGHT = 24
		return
		endcase
	"SELECT_MAP": begin			;Show file input control panel
			select_map
			create_draw_window
			draw_map
			return
		endcase
	"PLOT_SYMBOLS": begin
			plot_symbols
			return
		endcase
	"PLOT_NUMBERS": begin
			plot_numbers
			return
		endcase
	"CONNECT_POINTS": begin
			plot_lines
			return
		endcase

	"PALETTE": begin
			if (idebug ge 1) then print,'!x.type = ', !x.type
			xloadct
			if (idebug ge 1) then print,'!x.type = ', !x.type
		endcase

	"ZOOM": begin
			mapzoom
		endcase
	"GAL": if (coords ne 'galactic') then begin
			coords = 'galactic'
			;message = widget_message(/information, $
			;	dialog_parent=projection_base, $
			;	'Changing to galactic coordinates: please select new map')
			select_map,/noheader,/default_map
			create_draw_window
			draw_map
			return
		endif else return

	"CEL": if (coords ne 'celestial') then begin
			coords = 'celestial'
			;message = widget_message(/information, $
			;	dialog_parent=projection_base, $
			;	'Changing to celestial coordinates: please select new map')
			select_map,/noheader,/default_map
			create_draw_window
			draw_map
			return
		endif else return

	"ECL": if (coords ne 'ecliptic') then begin
			coords = 'ecliptic'
			;message = widget_message(/information, $
			;	dialog_parent=projection_base, $
			;	'Changing to ecliptic coordinates: please select new map')
			select_map,/noheader,/default_map
			create_draw_window
			draw_map
			return
		endif else return



	ELSE: MESSAGE, "Event user value not found"

endcase else begin
	numproj = eventval

	create_draw_window

	if (numproj eq 9) then begin    ; satellite projection?
		if (idebug ge 1) then print,'Satellite projection info request: '
		IF (XRegistered("map_viewer_circle")) THEN BEGIN
		  WIDGET_CONTROL, conn_two, SENSITIVE = 0
		  WIDGET_CONTROL, great_cir, SENSITIVE = 0
		ENDIF
		slide_wid = 250
		sat_base = lonarr(5)
		sat_base(0) = WIDGET_BASE( $
			title='Satellite Projection Parameters',/COLUMN)
		sat_base(1) = WIDGET_SLIDER(sat_base(0), $
			XSIZE = slide_wid, MINIMUM = 100, MAXIMUM = 15000, $
			VALUE= (sat_params(0)-1) * 6371., $
			TITLE = 'Altitude (Km)', $
			UVALUE = "SALT")
		sat_base(2) = WIDGET_SLIDER(sat_base(0), $
			XSIZE = slide_wid, MINIMUM = -90, MAXIMUM = 90, $
			VALUE= sat_params(1), TITLE = 'Alpha (up)', $
			UVALUE = "SALPHA")
		sat_base(3) = WIDGET_SLIDER(sat_base(0), $
			XSIZE = slide_wid, MINIMUM = -180, MAXIMUM = 180, $
			VALUE= sat_params(2), TITLE = 'Beta (rotation)', $
			UVALUE = "SBETA")
		sat_base(4) = WIDGET_BUTTON(sat_base(0), /ALIGN_CENTER, $
			VALUE = "Done", UVALUE = "SDONE")
		WIDGET_CONTROL, sat_base(0), /REALIZE
		XManager, "map_viewer_satellite", sat_base(0), $
			EVENT_HANDLER = "MAP_VIEWER_EVENT", $
			GROUP_LEADER = event.top
	    endif
	draw_map
	xsize = old_xsize
	RETURN
endelse

END


PRO map_viewer4, GROUP = GROUP

@map_viewer_common

IF XRegistered("map_viewer") THEN RETURN		;only one instance

if n_elements(image) gt 1 then begin		;Save image
	image = image > 16b		;Bottom 16 colors are used for grids
	endif

projs = ["Aitoff", "Mollweide", "Sinusoidal", "Azimuthal", "Mercator", $
	"Cylindrical", "Stereographic", "Orthographic", "Equal Area", $
	"Satellite", "Gnomonic", "Conic" ]

idebug = 0
numproj = 0
last_p = -1
device, get_screen = xsize		;Size to machine
xsize = xsize(0) * 2 / 3
ysize = xsize * 4 / 5
max_xsize = xsize*1.2
sliderwidth = 200
xsize = 0
lat0 = 0
lon0 = 0
rot0 = 0
!p.multi=0
drawable = 0L
r_base = 0L
map_window = 0
old_xsize = 0
symbol = 2
coords = 'galactic'
image = 0

map_viewer_color		;Load our color tables

cir = { CIRCLE_PARAMS, base : 0L, lon0 : 0.0, rot : 0.0, color : 4, $
	crot_slider : 0L, clon_slider : 0L, ll : [0., 0.], llflag : 0 }

sat_params = [ 1.2, 0, 0]  ;Salt, salpha, sbeta
sat_base = lonarr(5)

map_viewerbase = WIDGET_BASE(title = "Map Viewer Controls", /ROW)
l_base = WIDGET_BASE(map_viewerbase, /COLUMN)	;Left side widgets
r_base = WIDGET_BASE(map_viewerbase, /COLUMN)	;Right side
drawable = WIDGET_DRAW(r_base, XSIZE = xsize, YSIZE = ysize, $
		RET=2, /BUTTON_EVENTS)


XPdMenu, [	'" Done "			EXIT',		$
		'" Reset "			RESET',		$
		'" Help "			HELP',		$
	 	'"Palette" 			PALETTE'],  l_base


XPdMenu, [	 '"Select Map File"		SELECT_MAP', $
		'"Plot points" {', $
			'"Plot symbols"         PLOT_SYMBOLS', $
			'"Plot numbers"		PLOT_NUMBERS',$
			'"Plot lines"    CONNECT_POINTS', $
		'}' $ 
	],  l_base

XPdMenu, [   '"Coord System" {', $
		'"Galactic"			GAL', $
		'"Celestial"			CEL', $
		'"Ecliptic"			ECL', $
		'}', $
		'"Measure"                      MEASURE',	$
		'"Zoom"				ZOOM'], l_base

projection_base = 0L			;Base for projection info
junk = WIDGET_BASE(l_base)
projection_base = WIDGET_BASE(junk, $
		uvalue=0L, /COLUMN)

p_base = WIDGET_BASE(projection_base, COLUMN=2, /FRAME, /EXCLUSIVE)
for i=0, n_elements(projs)-1 do begin
	junk = WIDGET_BUTTON(p_base, uvalue = i, VALUE=projs(i), /NO_REL)
	if (i eq numproj) then WIDGET_CONTROL,junk,set_button=1
endfor

junk = WIDGET_BASE(projection_base, /ROW)

lon_slider = WIDGET_SLIDER(projection_base, $
	XSIZE = sliderwidth, MINIMUM = 0, MAXIMUM = 360, VALUE = lon0, $
	TITLE = 'Center Longitude', uvalue = "LON_SLIDER")
lat_slider = WIDGET_SLIDER(projection_base, $
	XSIZE = sliderwidth, MINIMUM = -90, MAXIMUM = 90, VALUE = lat0, $
	TITLE = 'Center Latitude', uvalue = "LAT_SLIDER")
rot_slider = WIDGET_SLIDER(projection_base, $
	XSIZE = sliderwidth, MINIMUM = -90, MAXIMUM = 90, VALUE = rot0, $
	TITLE = 'Rotation', uvalue = "ROT_SLIDER")


; *****

ll_label = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=2, /FRAME, $
		VALUE = ['Click Mouse on map','for inverse transforms'])

l2_label = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=1, /FRAME, $
		VALUE = [' '])

select_map, /noheader
s = size(image)
if (s(0) eq 2) then begin
	WIDGET_CONTROL, map_viewerbase, /REALIZE
	WIDGET_CONTROL, drawable, get_value = map_window
	create_draw_window
	draw_map

	XManager, "map_viewer", map_viewerbase, $
		EVENT_HANDLER = map_viewer_events, $
		GROUP_LEADER = group
endif
image = 0		;Free some memory
end
