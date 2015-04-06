; $Id: map_viewer.pro,v 1.2 1993/05/27 22:00:20 troy Exp $
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
;		08/23/97 by DNB: updated to work correctly with IDL 5.
;			This required a number of fixes, which are 
;			documented in the code.  Of major import is
;			the reassignment of projection ID numbers,
;			since they have changed between IDL4 and IDL5.
;			See notes in draw_map.
;       07/27/04 by JLR: added plot Sun/Moon constraints to plot points
;       09/19/04 by JLR: added auto feature for use by SOT
;       01/21/05 by JLR: added feature to only look at slews of concern for 
;                        "Indiana Jones" maneuver
;       06/27/05 by JLR: added search for ephemeris in current directory 
;                        before searching on /bulk/vader3/
;       12/22/05 by JLR: added realslew option rather than slewfile
;       04/03/06 by JLR: changed ephemeris directory to /home/swift/stkproducts
;                         will only work on linux/mac
;       11/28/07 by JLR: added box to explain reason for flagging slew and 
;                         began flagging orbit plane slews
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
;	2) Add options to Palette button to permit rescaling of data.
;	3) Add coordinate grid overlays
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

  projnum = [13, 10, 11, 6, 9, 8, 1, 2, 4, 7, 5, 3]
  iproj = projnum(numproj)      ; selects correct projection
                                ; NOTE: This program uses an undocumented keyword
                                ; 	to map_set (PROJ) to set the projection type.
                                ;	The values of PROJ for several projections
                                ;	changed between IDL4 and IDL5, resulting in
                                ;	incorrect projections.  Check for this if
                                ;	IDL updates again in the future.  The values
                                ;	here were obtained from the file
                                ;	/usr/local/rsi/idl_5/lib/map_set.pro, and
                                ;	are given in code in lines 648-663.
                                ;	They are also given in subroutine map_proj_info,
                                ;	which contains all the info for setting up the
                                ;	recognized map projections.

  lon1 = lon0
  lat1 = lat0
  rot1 = rot0
  if ((lon1 eq last_l) and (lat1 eq last_b) and (rot1 eq last_r) and $
      (last_p eq iproj) and (filename eq last_file) and (coords eq last_coords) and $
      (projs(numproj) ne 'Satellite') and (oldgrids eq grids) and (doreset eq 0)) then return ;Nothing to do?

  if (projs(numproj) ne 'Satellite') then begin	; projection not satellite
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


  oldlat = lat1

;		Take care of special cases:
;
  IF (projs(numproj) eq 'Conic') THEN BEGIN	;conic?
                                ; if (lat1 eq 0) and (lon1 eq 0) then lat1 = 45
     lat1 = (lat1 > (-85)) < 85	;Adjust ranges
     lon1 = (lon1 > (-85)) < 85
     if (lat1 * lon1) lt 0 then BEGIN
        WIDGET_CONTROL, l2_label, $
                        SET_VALUE = [ 'Illegal values for Conic']
        return
     endif
     WIDGET_CONTROL, LAT_SLIDER, SET_VALUE = lat1 ;Update the sliders
     WIDGET_CONTROL, LON_SLIDER, SET_VALUE = lon1
     WIDGET_CONTROL, l2_label, SET_VALUE = [' ']
  ENDIF
  

  IF ((projs(numproj) eq 'Sinusoidal') and ((lat1 ne 0) or (rot0 ne 0))) $
  THEN begin                    ;Sinusoidal?
     lat0 = 0                   ; Set latitude to zero
     WIDGET_CONTROL, LAT_SLIDER, SET_VALUE = lat0 ;Update the sliders
     lat1 = lat0
     rot0 = 0                   ; Set rotation to zero
     WIDGET_CONTROL, ROT_SLIDER, SET_VALUE = rot0
     if ((lon1 eq last_l) and (lat1 eq last_b) and (rot1 eq last_r) and (last_p eq iproj) and (filename eq last_file) and (coords eq last_coords)) then return ;Nothing to do?
     last_b = lat1
     last_r = rot0
  endif	
  last_l = lon1
  last_b = lat1
  last_r = rot1
  last_p = iproj
  last_file = filename
  last_coords = coords
  

;**********	Draw the map....

  wset, map_window

  IF (projs(numproj) eq 'Conic') THEN BEGIN	;Conic
     if (lat1 lt 0) or (lon1 lt 0) THEN limit = [-80,-180,0,180] $
     else limit = [0,-180,80,180]
;ENDIF ELSE limit = [-90,-180,90,180]	;Changed 8/20/97 by DNB
  ENDIF ELSE limit = 0	

  widget_control, /HOURGLASS

  if (n_elements(limit) gt 1) then begin
     map_set, lat1, lon1, rot0, PROJ = iproj, grid=grids, $
              sat_p = sat_params, $
              title = 'File ' + filename + ': ' + projs(numproj) + ' projection, ' $
              + coords + ' coordinates', limit = limit
  endif else begin
     map_set, lat1, lon1, rot0, PROJ = iproj, grid=grids, $
              sat_p = sat_params, $
              title = 'File ' + filename + ': ' + projs(numproj) + ' projection, ' $
              + coords + ' coordinates'
  endelse

  a = widget_base(TITLE='Note', /COLUMN, XOFFSET=400, YOFFSET=300)
  junk = widget_label(a, value='Warping sky data to maps can')
  junk = widget_label(a, value='require a significant amount of time.')
  widget_control, a, /REAL

  if not keyword_set(auto) then begin 
     new_image = map_image(hist_equal(image), x0, y0, mi_xsize, mi_ysize, $
                           latmin=-90, latmax = 89.5, lonmin=-180.0, lonmax=180.0, $
                           /BILIN)

     colimg=bytscl(new_image,min=min(new_image),max=max(new_image))/255.*231.+12.
     colr=[[r_orig],[g_orig],[b_orig]]
     redimg=colr[colimg,0]
     grnimg=colr[colimg,1]
     bluimg=colr[colimg,2]
     xs=n_elements(new_image[*,0])
     ys=n_elements(new_image[0,*])
     redimg=reform(redimg,xs,ys)
     grnimg=reform(grnimg,xs,ys)
     bluimg=reform(bluimg,xs,ys)

     tvscl,[[[redimg]],[[grnimg]],[[bluimg]]],x0,y0,true=3
  endif else begin 
     new_image = map_image(image, x0, y0, mi_xsize, mi_ysize, $
                           latmin=-90, latmax = 89.5, lonmin=-180.0, lonmax=180.0, $
                           /BILIN)
     tvscl,new_image,x0,y0
  endelse 
;tvscl, new_image, x0, y0    ;Show it

; Draw grid lines
  if (grids eq 2) then begin
     map_grid,latdel=10,londel=10
     map_grid,latdel=30,londel=30, glinestyle=0
  endif
  if (grids eq 1) then map_grid,latdel=30,londel=30, glinestyle=1

  oldgrids = grids
  widget_control, a, /DESTROY

;dummy=''
;read,'Ready to rescale image?',dummy
;h_eq_ct

; map_set, lat1, lon1, rot0, PROJ=iproj, limit = limit, $
; 	/noerase, grid=0, sat_p = sat_params


  lat1 = oldlat
  new_image=0

;if keyword_set(auto) then plot_sunmoon;,auto=auto,noskip=noskip,year=year

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
        print,'!x.s, !x.type = ', !x.s, !x.type	
        print,'!y.s, !y.type = ', !y.s, !y.type	
     endif
     print,''
  endif

  if (map_window ne 0) then wset, map_window
  if event.id eq drawable then begin ;Update lat/lon text widget?
     if event.press eq 0 then return
     if (!x.s(1) eq 0) then return
;	!x.type = 2	; set to 2 here because xloadct resets this to 0
                                ; commented out 8/23/97 - V5 doesn't have this bug
     p = convert_coord(event.x, event.y, /device, /to_data) ;Get coords
     if (idebug ge 1) then begin
        print, ''
        print, 'x,y = ', event.x, event.y
        print, 'p = ', p
     endif
     p(0) = -p(0)               ; reverse longitude for astronomy
     if (p(0) lt 0.0) then p(0) = p(0) + 360.0
     set_ll:
     if not keyword_set(auto) then WIDGET_CONTROL, l2_label, SET_VALUE = ' '
     
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
           plots,-p[0],p[1],psym=2
        endcase
        'celestial': begin
           WIDGET_CONTROL, ll_label, set_value = [ $
                           'RA = ' + string(p(0),format='(F6.1)') + ' degrees', $
                           'Dec  = ' + string(p(1),format='(F6.1)') + ' degrees']
        endcase
     endcase

     IF ABS(p(0)) gt 360 THEN RETURN ;Valid point?

     case cir.llflag of
        0: begin                ; write map value
           case coords of
              'celestial': begin
                 ra = p(0)      ; use epoch 2000 internally
                 dec = p(1)
;				precess, ra, dec, 2000.0, 1950.0   ; precess to 1950
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
           if not auto then begin 
              WIDGET_CONTROL, l2_label, $
                              SET_VALUE = 'I = ' $
                              + string(value,format='(F6.1)') $
                              + ' ' + units
           endif 
        end
        1: begin                         ; Mark great circle - first point
           plots, -p(0), p(1), psym=4    ; show first point
           WIDGET_CONTROL, l2_label, SET_VALUE = $
                           'Mark second point.'
           cir.ll = p(0:1)
           cir.llflag = 2
        end
        2: begin                         ; 2nd point
           plots, -p(0), p(1), psym=4    ; show second point
           calc_distance, cir.ll, p(0:1)
           cir.llflag = 0
        end
     endcase

     return
  endif                         ;Cursor hit on map


  if s(1) eq 7 then CASE eventval OF ;String value?
     "LAT_SLIDER":	begin
        lat0 = event.value
        draw_map
        return
     endcase
     "LON_SLIDER":	begin
        lon0 = -1*event.value
        if (lon0 lt -180) then lon0 = lon0 + 360
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
        cir.llflag = 1          ;Expecting 1st point
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
        doreset=1
        draw_map
     endcase
     "EXIT" : begin 			;Adios!!  Restore prev color tbls
        IF N_ELEMENTS(r_curr) GT 0 THEN TVLCT, r_curr, g_curr, b_curr
        WIDGET_CONTROL, event.top, /DESTROY
        RETURN
     endcase
     "HELP" : begin             ;Display help text
        XDisplayFile, $
           '/bulk/pkg/xray/idl_lib/planning/map_viewer_help.txt', $
           TITLE = "Map Viewer Help", $
           GROUP = event.top, $
           WIDTH = 72, HEIGHT = 24
        return
     endcase
     "SELECT_MAP": begin        ;Show file input control panel
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
     "PLOT_SUNMOON": begin
        plot_sunmoon,slewchoice='n'
        return
     endcase
     
     "PALETTE": begin
        if (idebug ge 1) then print,'Before xloadct: !x.type = ', !x.type
        xloadct                 ;,group=map_viewerbase
        if (idebug ge 1) then print,'After xloadct: !x.type = ', !x.type
     endcase

     "GRIDON": begin
        grids = 1
        draw_map
        return
     endcase

     "GRIDMAX": begin
        grids = 2
        draw_map
        return
     endcase

     "GRIDOFF": begin
        grids = 0
        draw_map
        return
     endcase

     "ZOOM": begin
        mapzoom
     endcase
     "PREV": begin 
        if slewinfo eq -1 then print,'No more slews in PPST' else plot_sunmoon,slewchoice='-1'
     endcase
     "BADPREV": begin 
        badslew=1
        noskip=0
        while badslew and slewinfo gt 0 do begin 
           plot_sunmoon,slewchoice='-1',badslew=badslew
        endwhile
        noskip=1
        badslew=0
        if slewinfo le 0 then print,'No more slews in PPST'
     endcase
     "MAYBEBADPREV": begin 
        maybebadslew=1
        noskip=0
        while maybebadslew and slewinfo gt 0 do begin 
           plot_sunmoon,slewchoice='-1',maybebadslew=maybebadslew
        endwhile
        noskip=1
        maybebadslew=0
        if slewinfo le 0 then print,'No more slews in PPST'
     endcase
     "NEXT": begin 
        plot_sunmoon,slewchoice='1'
        if slewinfo+1 ge n_elements(linedata[0,*]) then print,'No more slews in PPST'
     endcase
     "BADNEXT": begin 
        badslew=1
        noskip=0
        while badslew and (slewinfo+1 lt n_elements(linedata[0,*])) do begin 
           plot_sunmoon,slewchoice='1',badslew=badslew
        endwhile
        if badslew eq 1 then tmp=dialog_message('No more bad slews!',dialog_parent=projection_base,/info)
        noskip=1
        if slewinfo ge n_elements(linedata[0,*]) then print,'No more slews in PPST'
        badslew=0
     endcase
     "MAYBEBADNEXT": begin 
        maybebadslew=1
        noskip=0
        while maybebadslew and (slewinfo+1 le n_elements(linedata[0,*])) do begin 
           plot_sunmoon,slewchoice='1',maybebadslew=maybebadslew
        endwhile
        if maybebadslew eq 1 then tmp=dialog_message('No more maybe bad slews!',dialog_parent=projection_base,/info)
        noskip=1
        if slewinfo ge n_elements(linedata[0,*]) then print,'No more slews in PPST'
        maybebadslew=0
     endcase
     "SLEW": begin
        WIDGET_CONTROL,setslew,get_value=value
        vpos=strpos(value,': ')
        if vpos ne -1 then value=strmid(value,vpos+1)
        plot_sunmoon,slewchoice=value
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

     if (numproj eq 9) then begin ; satellite projection?
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


PRO map_viewer, GROUP = GROUP, autos=autos, linefile=linefile, pointname=pointname,doskip=doskip,ppst=ppsts,realslew=realslew,onlyas=onlyas,safepoint=safepoint,check_momentum_off=check_momentum_off,onlymo=onlymo
  
@map_viewer_common
  if keyword_set(onlyas) then onlyantisun=1 else onlyantisun=0
  if keyword_set(safepoint) then sfpoint=1 else sfpoint=0
  if keyword_set(autos) then auto=1 else auto=0
  if keyword_set(check_momentum_off) then nomom=1 else nomom=0
  if keyword_set(onlymo) then onlymoon=1 else onlymoon=0
  if auto then begin 
     ppst=ppsts
     lfile=linefile
     pfile=pointname
     if n_elements(realslew) gt 0 then begin
        syear=strmid(ppst[0].begdate,0,4)
        startday=strmid(ppst[0].begdate,5,3)
        eyear=strmid(ppst[n_elements(ppst)-1].enddate,0,4)
        endday=strmid(ppst[n_elements(ppst)-1].enddate,5,3)
        if startday eq endday then endday=endday+1
        ydn2md,syear,startday,smonth,sday
        ydn2md,eyear,endday,emonth,eday
        s=' '
        spawn,'get_dtas.py '+ntostr(syear)+s+ntostr(smonth)+s+ntostr(sday)+s+ntostr(eyear)+s+ntostr(emonth)+s+ntostr(eday)
        readcol,'dtas_SACSRA.dat',scdate,scra,format='(a,d)',skip=2,delim=','
        readcol,'dtas_SACSDEC.dat',scdate,scdec,format='(a,d)',skip=2,delim=','
        readcol,'dtas_SACSROLL.dat',scdate,scroll,format='(a,d)',skip=2,delim=','
        nbuf=n_elements(scdate)
        if nbuf eq 0 then begin
           print,'DATA NOT COMPLETE' ;  CHILE is not a crystal ball!'
           return
        endif 
        sctime=lonarr(nbuf)
        for i=0L,nbuf-1 do sctime[i]=date2met(scdate[i])
;        spawn,'rm dtas*dat'
        compare_slew=1
     endif else compare_slew=0
  endif 
  doreset=0
  IF XRegistered("map_viewer") THEN RETURN ;only one instance

  if n_elements(image) gt 1 then begin ;Save image
     image = image > 16b               ;Bottom 16 colors are used for grids
  endif

  projs = ["Aitoff", "Mollweide", "Sinusoidal", "Azimuthal", "Mercator", $
           "Cylindrical", "Stereographic", "Orthographic", "Equal Area", $
           "Satellite", "Gnomonic", "Conic" ]

  idebug = 0
  numproj = 0
  last_p = -1
  device, get_screen = xsize    ;Size to machine
  xsize = xsize(0) * 2 / 3
  ysize = xsize * 4 / 5
  max_xsize = xsize*1.2
  sliderwidth = 200
  xsize = 0
  lat0 = 0
  lon0 = 0
  rot0 = 0
  last_l = 180.
  last_b = 45.
  last_r = 30.
  last_file = ''
  !p.multi=0
  drawable = 0L
  r_base = 0L
  color_box = 0L
  map_window = 0
  old_xsize = 0
  coords = 'galactic'
  if keyword_set(auto) then coords='ecliptic'
  last_coords = coords
  grids = 2
  oldgrids=0
  image = 0
  symbol = 4
  symsize = 1.5
  symcolor = 255
  slewinfo=-1
  setslew=0L
  value=''
  map_viewer_color              ;Load our color tables

  cir = { CIRCLE_PARAMS, base : 0L, lon0 : 0.0, rot : 0.0, color : 4, $
          crot_slider : 0L, clon_slider : 0L, ll : [0., 0.], llflag : 0 }

  sat_params = [ 1.2, 0, 0]     ;Salt, salpha, sbeta
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
                 '"Plot Sun/Moon"     PLOT_SUNMOON',$
                 '}' $ 
           ],  l_base

  XPdMenu, [   '"Coords" {', $
               '"Galactic"			GAL', $
               '"Celestial"			CEL', $
               '"Ecliptic"			ECL', $
               '}', $
               '"Grids" {', $
               '"30 degree Grid Lines"		GRIDON',$
               '"10 degree Grid Lines"		GRIDMAX',$
               '"Grid Lines Off"		GRIDOFF',$
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

  if not auto then begin 
     lon_slider = WIDGET_SLIDER(projection_base, $
                                XSIZE = sliderwidth, MINIMUM = 0, MAXIMUM = 360, VALUE = lon0, $
                                TITLE = 'Center Longitude', uvalue = "LON_SLIDER")
     lat_slider = WIDGET_SLIDER(projection_base, $
                                XSIZE = sliderwidth, MINIMUM = -90, MAXIMUM = 90, VALUE = lat0, $
                                TITLE = 'Center Latitude', uvalue = "LAT_SLIDER")
     rot_slider = WIDGET_SLIDER(projection_base, $
                                XSIZE = sliderwidth, MINIMUM = -90, MAXIMUM = 90, VALUE = rot0, $
                                TITLE = 'Rotation', uvalue = "ROT_SLIDER")
  endif 

; *****

  ll_label = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=2, /FRAME, $
                         VALUE = ['Click Mouse on map','for inverse transforms'])
  if not auto then begin 
     l2_label = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=1, /FRAME, $
                            VALUE = [' '])
  endif 

  if auto then begin 
     XPdMenu, [	'" Previous Slew "			PREV', $   
                '" Next Slew "			NEXT'],l_base

     setslew = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=1, /FRAME, $
                           UVALUE="SLEW",value='Slew +/- N(m): ',/editable)
     slewobjs = WIDGET_TEXT(l_base, XSIZE = 23, YSIZE=6, /FRAME, $
                            value=[' '])
     flagreason = WIDGET_TEXT(l_base,XSIZE = 23, YSIZE=6, /FRAME, $
                            value=[' '])
     XPdMenu, [	'" Previous Bad Slew "			BADPREV', $   
                '" Next Bad Slew "			BADNEXT'],l_base
;     XPdMenu, [	'" Previous Maybe Bad Slew "			MAYBEBADPREV', $   
;                '" Next Maybe Bad Slew "			MAYBEBADNEXT'],l_base
     
     
;   WIDGET_CONTROL,setslew,get_value=value
;   PATH='/bulk/pkg/xray/allsky_maps/'
;   FILENAME=PATH+'DIRBE-corrected_IRAS_100um.fits'
;     path='/bulk/pkg/xray/idl_lib/planning/'
     path='/Volumes/Apps_and_Docs/jracusin/idl.lib/swift_sot/'
     filename=path+'blank_map.fits'
     select_map, /noheader,/default_map
  endif else select_map,/noheader
  s = size(image)
  if (s(0) eq 2) then begin
     WIDGET_CONTROL, map_viewerbase, /REALIZE
     WIDGET_CONTROL, drawable, get_value = map_window
     create_draw_window
     draw_map
     if auto then begin 
        
        ypos=strpos(linefile,'PPST_')
        if ypos eq -1 then ypos=strpos(linefile,'AFST_')
        year=strmid(linefile,ypos+5,4)
        day=strmid(linefile,ypos+9,3)
        ephdir=''
        tmpeph=ephdir+'STK_EPH_*'
        if not exist(tmpeph) then begin 
           ephdir='/home/swift/stkproducts/'
           tmpeph=ephdir+'STK_EPH_'+year+day+'*'
           while not exist(tmpeph) do begin
              if day*1. gt 1 then day=ntostr(day-1) else begin
                 day='365' & year=ntostr(year-1)
              endelse 
              if day lt 100 then day='0'+day
              if day lt 10 then day='0'+day
              tmpeph=ephdir+'STK_EPH_'+year+day+'*'
           endwhile
        endif
        ephfile=findfile(tmpeph)
        neph=n_elements(ephfile)
        ephfile=ephfile[neph-1]
        
        epos=strpos(ephfile,'STK_EPH')
        ephdate1=strmid(ephfile,epos+8,7)
        ephdate2=strmid(ephfile,epos+16,7)
        date=year+day
        if date lt ephdate1 or date gt ephdate2 then begin
           ephdir='/home/swift/stkproducts/'
           tmpeph=ephdir+'STK_EPH_'+year+day+'*'
           while not exist(tmpeph) do begin
              day=ntostr(day-1)
              if day lt 100 then day='0'+day
              if day lt 10 then day='0'+day
              tmpeph=ephdir+'STK_EPH_'+year+day+'*'
           endwhile
           
           ephfile=findfile(tmpeph)
           neph=n_elements(ephfile)
           ephfile=ephfile[neph-1]
           
           epos=strpos(ephfile,'STK_EPH')
           ephdate1=strmid(ephfile,epos+8,7)
           ephdate2=strmid(ephfile,epos+16,7)
           date=year+day
           if date lt ephdate1 or date gt ephdate2 then begin
              print,'No valid ephemeris available'
              return
           endif 
        endif 
        
;       ephdir='/home/xrt/EPH/'
;       ephfile=ephdir+'STK_EPH_2004363_2005010_01.txtbak'
;       if year eq 2004 then ephfile=ephdir+'STK_EPH_2004363_2005010_01.txtbak2' else ephfile=ephdir+'STK_EPH_2004363_2005010_01.txt'
;       ephdir='/home/jracusin/EPH/'
;       if year eq 2004 then ephfile=ephdir+'STK_EPH_2004.txt' else ephfile=ephdir+'STK_EPH_2005.txt'

        print,'Using Ephemeris file: ',ephfile
        eph=readeph(ephfile)
        earth_dat=strarr(n_elements(eph),9)
        earth_dat[*,0]=eph.dy
        earth_dat[*,1]=eph.time
        earth_dat[*,2]=eph.x
        earth_dat[*,3]=eph.y
        earth_dat[*,4]=eph.z
        earth_dat[*,5]=eph.lat
        earth_dat[*,6]=eph.lon
        earth_dat[*,7]=eph.ra
        earth_dat[*,8]=eph.dec
        
;        openr,lun,ephfile,/get_lun
;        dummy=parse_str(lun)
;        earth_lnes=get_lines(ephfile)
;        earth_dat=strarr(earth_lnes,9)
;        ct=0.
        
;        while not eof(lun) do begin
;           line=parse_str(lun)
           
;           if (n_elements(line) eq 9) then begin
;              hr=strmid(line[1],0,2)
;              mi=strmid(line[1],3,2)
;              sec=strmid(line[1],6,2)
;;             fracday=float(hr)/24.+float(mi)/24./60.+float(sec)/24./3600.
;              earth_dat[ct,0]=strmid(line[0],0,3)
;;             earth_dat[ct,1]=fracday
;              earth_dat[ct,1:*]=line[1:*]
;              ct=ct+1.
;           endif 
;        endwhile 
;        free_lun,lun
        
       @map_viewer_common
       
       plot_lines
;       plot_numbers
;       if keyword_set(doskip) then noskip=0 else noskip=1
       
       noskip=1
       slewinfo=slewinfo+1
       plot_sunmoon,slewchoice='1'
       
;       plot_lines
;       plot_numbers
    endif 
     
     XManager, "map_viewer", map_viewerbase, $
               EVENT_HANDLER = map_viewer_events, $
               GROUP_LEADER = group ;,/no_block
  endif
  image = 0                     ;Free some memory
  return
end
