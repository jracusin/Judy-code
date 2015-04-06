; ***********************************************************************
; ***********************************************************************
pro map_viewer_color
; ***********************************************************************
; ***********************************************************************

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

r_orig = bytarr(256)
g_orig = bytarr(256)
b_orig = bytarr(256)

r=bytarr(247)
g=bytarr(247)
b=bytarr(247)

get_lun,luin
lut_file = '/bulk/pkg/xray/idl_lib/praxis/idl.color4.lut' 
openr,luin,lut_file

head=''
readf,luin,head

for i=0,246 do begin
	readf,luin,t1,t2,t3
	r(i)=t1
	g(i)=t2
	b(i)=t3
endfor
close,luin
free_lun,luin

r_orig(9:255) = r
g_orig(9:255) = g
b_orig(9:255) = b

r_curr = r_orig
g_curr = g_orig
b_curr = b_orig
tvlct,r_orig, g_orig, b_orig
end

; ***********************************************************************
; ***********************************************************************
pro map_viewer_xy,A,B,X,Y
; ***********************************************************************
; ***********************************************************************
;
;  SUBROUTINE: map_viewer_xy
;  AUTHOR: RICK BORKEN
;  DATE: DEC. 1975
;  MODIFIED: 6/2/82 BY DNB TO CORRECTLY HANDLE ALL LONGITUDE VALUES
;      BETWEEN -3600 DEGREES AND +3600 DEGREES.
;	10/18/95 by DNB for IDL
;  DESCRIPTION:
;      map_viewer_xy CALCULATES THE X,Y POSITION OF A POINT ON AN AITOFF
;      PROJECTION, GIVEN THE LATITUDE AND LONGITUDE.
;      THE PROJECTION IS 5.0 UNITS HIGH AND 10.0 UNITS WIDE,
;      CENTERED AT 0.,0.
;
;  USAGE:  SUBROUTINE map_viewer_xy(A,B,X,Y)
;      A = LATITUDE OF POINT IN DEGREES (-90. < A < 90.)
;      B = LONGITUDE OF POINT IN DEGREES (0. < B < 360.)
;      X,Y = LOCATION OF POINT ON PROJECTION IN IMAGE UNITS.
;              (-5. < X < 5.)  (-2.5 < Y < 2.5)
;
;  COMMON BLOCKS:
;      NONE
;
;  SUBROUTINES CALLED BY map_viewer_xy:
;      NONE
;
      RADIAN = 57.29578
      TWORAD = 114.59156
      PI = 3.1415926
      PIO2 = pi/2.0
      ALPHA=(-A+90.0)/RADIAN
      GLONG=(B+3600.) mod 360.
      over = where(glong gt 180.0, count)
      if (count gt 0) then GLONG(over) = GLONG(over)-360.0
      BETA=GLONG/TWORAD
      CBETA=COS(BETA)
      SBETA=SIN(BETA)
      CALPHA=COS(ALPHA)
      SALPHA=SIN(ALPHA)
      SC=-CALPHA
      SS=SALPHA*SBETA
      CAP=SALPHA*CBETA
      CAP = CAP < 1.0
      SAP=SQRT(1.0-CAP^2)
      nonzero = where(sap ge 0.001)
      CBP = FLTARR(n_elements(sap))
      if (nonzero(0) ge 0) then CBP(nonzero)=SC(nonzero)/SAP(nonzero)
      SBP = FLTARR(n_elements(sap))
      if (nonzero(0) ge 0) then SBP(nonzero)=SS(nonzero)/SAP(nonzero)
      R=SQRT(1.0-CAP)
      X=-5.0*R*SBP      ; The minus sign here converts to astronomical Aitoff
      Y=-(R*CBP)*5.0/2.0
      RETURN

      END

; ***********************************************************************
; ***********************************************************************
       pro map_viewer_lbpix,GLONG,GLAT,CENTER,NX,NY,IX,IY
; ***********************************************************************
; ***********************************************************************
;
;  SUBROUTINE: map_viewer_lbpix
;  AUTHOR:  DAVE BURROWS
;  MODIFIED:
;	12/4/95 by DNB to change so it will work with any Aitoff
;		projection.  Now reads NX, NY (size of Aitoff map).
;  MODIFIED: 4/1/82 BY S. HOLZ, ADDING MACC UTILITY ROUTINES
;      TO CHECK THE NUMBER OF PARAMETERS IN THE CALLING
;      STATEMENT, AND PRINTING AN ERROR MESSAGE AND WALK-BACK
;      IF THE NUMBER IS WRONG.
;  Modified: 2/25/85 by DNB to remove the calls added by S. Holz.
;  DATE:  OCT 16, 1981
;  DESCRIPTION:  THIS SUBROUTINE RETURNS THE PIXEL COORDINATES
;      OF THE MAP PIXEL WHICH CONTAINS THE GIVEN GALACTIC POSITION.
;
;  USAGE:  map_viewer_lbpix,GLONG,GLAT,CENTER,INTK,INTJ,IX,IY
;      GLONG = GALACTIC LONGITUDE IN DEGREES
;      GLAT = GALACTIC LATITUDE IN DEGREES
;      CENTER = CENTER LONGITUDE IN DEGREES
;      NX = number of map pixels in longitude axis
;      NY = number of map pixels in latitude axis
;      IX,IY = COORDINATES OF PIXEL WHICH CONTAINS THE SPECIFIED POINT.
;
;  COMMON BLOCKS:
;      NONE
;
;  SUBROUTINES CALLED BY map_viewer_lbpix
;      map_viewer_xy
;
;
       L = (720.0+GLONG-CENTER) mod 360.0
       B = GLAT
       map_viewer_xy,B,L,X,Y
;  Rescale projection to range of +/- 1.0 for X and Y
       X = X/5.
       Y = Y/2.5	; changed 12/4/95 by DNB
;  Calculate pixel number
;       IX = FIX((180*X + 181)/(2*INTK) + 1.0)
;       IY = FIX((180*Y + 91)/(2*INTJ) + 1.0)
	ix = fix(round( ((nx-1)*x + nx + 1.0)/2.0 ))
	iy = fix(round( ((ny-1)*y + ny + 1.0)/2.0 ))
;	print,x,y,ix,iy
       RETURN
       END

; ***********************************************************************
; ***********************************************************************
pro drawcirc, rot, long0, color
; ***********************************************************************
; ***********************************************************************
@map_viewer_common


;Draw a great circle with given rotation & offset

n = fix(theta)		;# of points
rota = rot * !DTOR		;Radians

t = findgen(n+1) * (theta * !DTOR/n)
sint = sin(t)
y = cos(t)
x = sint * sin(rota)
z = sint * cos(rota)
lat = asin(z) * !RADEG 
lon = atan(x,y) * !RADEG + long0
b = where(lon le (-180.), count)
if count ne 0 then lon(b) = lon(b) + 360.
b = where(lon gt 180., count)
if count ne 0 then lon(b) = lon(b)-360.

wset, map_window

plots,lon,lat, color = color, thick=2
return
end

; ***********************************************************************
; ***********************************************************************
pro drawarc, p1, p2, color, theta,ssize,lstyle  ; theta is angle of circle segment 
;						(in degrees)
; ***********************************************************************
; ***********************************************************************
@map_viewer_common

if (idebug ge 3) then begin
	print,'In DRAWARC: p1 =', p1
	print,'            p2 = ', p2
	print,'         color = ', color
	print,'         theta = ', theta
endif

;Draw a great circle between two input points


n = fix(theta)		;# of points (one per degree)

; Convert input directions to vectors
p1r = p1 * !dtor	;To radians
p2r = p2 * !dtor
lon = [p1r(0), p2r(0)]
lat = [p1r(1), p2r(1)]
x = cos(lat) * cos(lon)		;To xyz space
y = cos(lat)* sin(lon)
z = sin(lat)
u = [x(0), y(0), z(0)]		; 3-vectors
v = [x(1), y(1), z(1)]

if (idebug ge 3) then begin
	print,'  u = ', u
	print,'  v = ', v
endif

temp = crossp(u,v)
w = temp/norm(temp)
temp = crossp(w,u)
vv = temp/norm(temp)

;ds = findgen(n+1) * (theta * !DTOR/n)  ; Calculate rotations for all points
;uu = [cos(ds), sin(ds), fltarr(n+1)]
;help,lon

lon = fltarr(n+1)
lat = fltarr(n+1)
for i = 0,n do begin
	ds = i * (theta * !DTOR/n)
	uu = [cos(ds), sin(ds), 0.0]
	matrix = [[u], [vv], [w]]
	uuu = matrix # uu
; Convert back to lat, long
	lat(i) = asin(uuu(2)/norm(uuu))*!RADEG
	ccc = uuu(0)^2 + uuu(1)^2
	lon(i) = 0.0
	if (ccc ne 0.0) then begin
		if (idebug ge 5) then print,'uuu(0),ccc=',uuu(0),' ',ccc
		temp = acos(uuu(0)/sqrt(ccc))
		if (uuu(1) lt 0.0) then temp = -1.0*temp
		lon(i) = (temp*!RADEG + 720.0) mod 360.0
	endif
	if (idebug ge 3) then print,'  i, lon, lat = ', i, lon(i), lat(i)
endfor

wset, map_window
plotsym,0,/fill,1
ltest=size(lstyle)
;stop
if ltest[1] eq 2 then plots,lon,lat, color = color, thick=4,linestyle=lstyle $
else plots,lon,lat, color = color, thick=2,psym=8,symsize=ssize
return
end

; ***********************************************************************
; ***********************************************************************
pro cir_2p, p1, p2 ,ssize,lstyle  ;Connect two points, in the form of [lon, lat] with
		; a great circle arc
; ***********************************************************************
; ***********************************************************************

@map_viewer_common

p1r = p1 * !dtor	;To radians
p2r = p2 * !dtor

twopi = 2 * !pi
dlon = twopi + p2r(0) - p1r(0)			;delta longitude
while dlon gt !pi do dlon = dlon - twopi	;to -pi to +pi

; Great Circle Distance:
cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
;help,p1r(1),p2r(1),dlon
if (idebug ge 5) then print,'cosd = ', cosd
;help,cosd
if cosd gt 1.0d then cosd=1.
dst = acos(cosd)*180.0/!pi	;Angular separation in degrees

;lon = [p1r(0), p2r(0)]
;lat = [p1r(1), p2r(1)]
;x = cos(lat) * sin(lon)		;To xyz space
;y = cos(lat)* cos(lon)
;z = sin(lat)

;a = z(0) * y(1) - y(0) * z(1)	;Plane containing center of earth & 2 pnts
;b = z(1) * x(0) - x(1) * z(0)	; aX + bY = Z

;elon0 = -atan(b/a)		;Equator crossing
;rot = atan(tan(lat(1)) / sin(lon(1) - elon0))
;rot = 90 - rot * !radeg

;cir.lon0 = !RADEG * elon0
;cir.rot = rot

d = 'Ang. dist. = ' + STRING(dst, FORMAT='(f6.2)')+'  degrees' 
;WIDGET_CONTROL, ll_label, SET_VALUE = d
;WIDGET_CONTROL, cir.crot_slider, SET_VALUE = cir.rot
;WIDGET_CONTROL, cir.clon_slider, SET_VALUE = cir.lon0

if (idebug ge 3) then print,'p1, p2, dst =', p1, ' ', p2, dst
;help,dst

drawarc, p1, p2, cir.color, dst,ssize,lstyle

;symbol = ((symbol+1) mod 7) + 1
;wset, map_window
;plots, p1(0), p1(1), psym=symbol		;Mark the points
;plots, p2(0), p2(1), psym=symbol

end

; ***********************************************************************
; ***********************************************************************
pro calc_distance, p1, p2 
;Calculate distance in degrees between two points, in the form of [lon, lat]
; ***********************************************************************
; ***********************************************************************

@map_viewer_common

p1r = p1 * !dtor	;To radians
p2r = p2 * !dtor

twopi = 2 * !pi
dlon = twopi + p2r(0) - p1r(0)			;delta longitude
while dlon gt !pi do dlon = dlon - twopi	;to -pi to +pi

; Great Circle Distance:
cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
if (idebug ge 5) then print,'cosd = ', cosd
dst = acos(cosd)*180.0/!pi	;Angular separation in degrees

d = 'Ang. dist. = ' + STRING(dst, FORMAT='(f6.2)')+'  degrees' 
WIDGET_CONTROL, l2_label, SET_VALUE = d

return
end

; ***********************************************************************
; ***********************************************************************
pro select_map,NOHEADER=noheader,DEFAULT_MAP=default_map
; ***********************************************************************
; ***********************************************************************

@map_viewer_common

    true = 1
    false = 0
    warning = 0L

    fileOK = false
    repeat begin
	if not (keyword_set(default_map)) then $
	    Filename = pickfile(/READ, TITLE='Choose Aitoff map in FITS format.',  $
		PATH='/bulk/pkg/xray/allsky_maps', $
		FILTER='*.fits', GROUP=map_viewerbase, /NOCONFIRM)
	
;    if (filename eq ) then begin
;       filename='/bulk/pkg/xray/idl_lib/planning/blank_map.fits'
;    endif        
;return	; bail out
	if (filename ne '') then begin
		fileOK = true		; assume file is OK
	    ;; Read data, handling I/O errors.
		a = widget_base(TITLE='Message', /COLUMN)
		junk = widget_label(a, value='Reading FITS file...')
		widget_control, a, /REAL

		widget_control, /HOURGLASS
		data = readfits(Filename, Header)
		widget_control, a, /DESTROY
		if not (keyword_set(noheader)) then xdisplayfile, '', $
			TEXT=Header, TITLE=Filename, GROUP=map_viewerbase

	    ; Check for correct file format
		projection = sxpar(header, 'CTYPE1')
		if (!err lt 0) or (projection ne 'GLON-AIT') then begin
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
				'Invalid map projection')
			fileOK = false		; get new file
		endif
	    ; Get map size
		if (fileOK) then begin
		    nx = sxpar(header, 'NAXIS1')
		    if (!err lt 0) then begin
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
				'NAXIS1 (map size) not found in FITS header')
			fileOK = false		; get new file
		    endif
		endif
		if (fileOK) then begin
		    ny = sxpar(header, 'NAXIS2')
		    if (!err lt 0) then begin
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
				'NAXIS2 (map size) not found in FITS header')
			fileOK = false		; get new file
		    endif
		endif
	    ; Now get map center
		if (fileOK) then begin
		    xcenter = sxpar(header, 'CRVAL1')
		    if (!err lt 0) then begin
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
			   'CRVAL1 (center longitude) not found in FITS header')
			fileOK = false		; get new file
		    endif
		endif
		if (fileOK) then begin
		    ycenter = sxpar(header, 'CRVAL2')
		    if (!err lt 0) then begin
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
			    'CRVAL2 (center latitude) not found in FITS header')
			fileOK = false		; get new file
		    endif
		    if (abs(ycenter) gt 0.5) then begin 
			warning = widget_message(/error, $
				dialog_parent=projection_base, $
				'Map projection not centered on galactic plane')
			fileOK = false		; get new file
		    endif
		endif
		if (fileOK) then begin
		    units = sxpar(header, 'BUNIT')
		endif
	endif
    endrep until fileOK

    ; Convert from Aitoff in FITS format to internal format
	a = widget_base(TITLE='Message', /COLUMN)
	junk = widget_label(a, value='Converting from Aitoff map in galactic coordinates to internal format in '+coords+' coordinates ...')
	widget_control, a, /REAL
	glong=float(indgen(360))+180.0
	glong=reform(glong,360,1)
	glong=rebin(glong,360,181,/sample)
	glat=float(indgen(181)-90)
	glat = reform(glat,1,181)
	glat = rebin(glat,360,181,/sample)
; Convert glong, glat to ecliptic or celestial coords if necessary.
;	(Actually, if in ecliptic mode, take glong and glat as grid
;	of ecliptic coords and convert them to galactic coords.
;	Then call map_viewer_lbpix to get ix, iy of those coords as usual.
;	Similar procedure for celestial.
;
;	Conversions are performed using the "euler" routine in LibAstro
;
	if (coords eq 'celestial') then begin
		ra = glong		; use epoch 2000 internally
		dec = glat
		ra =  reform(ra,65160)
		dec = reform(dec,65160)
		precess, ra, dec, 2000.0, 1950.0   	; precess to 1950
							;   (required by euler)
		ra = reform(ra,360,181)	
		dec = reform(dec,360,181)
		euler, ra, dec, glong, glat, 1		; convert to galactic
		ra = 0					; free up memory
		dec = 0
	endif else $
	    if (coords eq 'ecliptic') then begin
		elong = glong
		elat = glat
		euler, elong, elat, glong, glat, 5	; perform conversion
		elong = 0
		elat = 0
	    endif

	map_viewer_lbpix,glong,glat,xcenter,nx,ny,ix,iy

	image = rotate(data(ix,iy),5)
	widget_control, a, /DESTROY

	; release memory
	; data=0
	glong = 0
	glat = 0
	ix = 0
	iy = 0

	return
end

; ***********************************************************************
; ***********************************************************************
pro plot_lines,DEFAULT_FILE=default_file,date
;	Read a file containing (long, lat) coordinate pairs and plot
;	them on the map, connecting points with line along great circle.
; ***********************************************************************
; ***********************************************************************

@map_viewer_common

;	if not (keyword_set(default_file)) then $
  if n_elements(lfile) eq 0 then $
     linename = pickfile(/READ, $
     TITLE='Select file containing (long, lat) pairs',  $
                         FILTER='*.lines', GROUP=map_viewerbase, /NOCONFIRM) $
  else linename=lfile
;	else linename = default_file

	if (linename eq '') then return

	;; Read data, handling I/O errors.

	get_lun, luin
	openr, luin, linename
	dummy = ''
	readf, luin, dummy
	filecoords = strlowcase(dummy)
	linedata = fltarr(10000)
	i = 0
	while (not EOF(luin)) do begin
		readf, luin, a, b,c
		linedata(i) = a 
        linedata(i+1) = b
        linedata[i+2]=c
		i = i + 3
	endwhile
	linedata = linedata(0:(i-1))
	if (idebug ge 2) then print,'Data array size: ', size(linedata)
	if (idebug gt 5) then print,linedata
	xdim = n_elements(linedata)/3
	linedata = reform(linedata, 3, xdim)
	if (idebug ge 2) then print,'Data array size: ', size(linedata)
	symbol = ((symbol+1) mod 7) + 1
	cir.color = 200;(cir.color+32) mod 256
    
	if (filecoords ne coords) then begin
	    select = 0
	    if ((filecoords eq 'celestial') and (coords eq 'galactic')) then $
		select = 1
	    if ((filecoords eq 'galactic') and (coords eq 'celestial')) then $
		select = 2
	    if ((filecoords eq 'celestial') and (coords eq 'ecliptic')) then $
		select = 3
	    if ((filecoords eq 'ecliptic') and (coords eq 'celestial')) then $
		select = 4
	    if ((filecoords eq 'ecliptic') and (coords eq 'galactic')) then $
		select = 5
	    if ((filecoords eq 'galactic') and (coords eq 'ecliptic')) then $
		select = 6
	    if (select eq 0) then return	; invalid input
	    valid = where(linedata(1,*) ne -99.0)
	    ilong = linedata(0,valid)
	    ilat = linedata(1,valid)

	    euler, ilong, ilat, olong, olat, select	; convert coords

	    linedata(0,valid) = olong
	    linedata(1,valid) = olat

	    ilong = 0
	    ilat = 0
	    olong = 0
	    olat = 0	; free memory
	endif

;	linedata(0,*) = -1.0*linedata(0,*)	; invert for display
    
    if n_elements(date) eq 2 then begin 
       wt=where(date[1] eq linedata[2,*])
       print,'PPST Time: ',linedata[2,wt] ;print slew start time from ppst
       print,'Date: ',date[1]
       cir_2p,linedata[0:1,wt-1],linedata[0:1,wt],0.5,0
    endif else begin 
       
       for i = 1, xdim-1 do begin
          if (idebug ge 3) then print,'PLOT_LINES: 1st point = ', $
             linedata(0:1,(i-1))
          if (idebug ge 3) then print,'PLOT_LINES: 2nd point = ', $
             linedata(0:1,i)
          if ((linedata(1,(i-1)) ne -99.0) and (linedata(1, i) ne -99.0)) then $
             cir_2p, linedata(0:1,(i-1)), linedata(0:1, i),2,0
       endfor
    endelse 

	free_lun, luin
;	linedata = 0	; free memory

	return
end

; ***********************************************************************
; ***********************************************************************
pro sym_event, symevent
;	Process symbol events
; ***********************************************************************
; ***********************************************************************

@map_viewer_common
widget_control, symevent.id, get_uvalue = eventval
s=size(eventval)

if (idebug ge 1) then begin
	print, 'Event size: ', s
	if (s(1) gt 0) then print, 'Event value: ', eventval
	print,'Event.id = ', symevent.id
	if (eventval ne "COLOR_SLIDER") then print,'Event.select = ', symevent.select
	if ((eventval eq "SYMBOL_SIZE")or(eventval eq "SYMBOL_TYPE")) then $
		print,'Event.value = ', symevent.value
	if (symevent.id eq drawable) then begin
		print,'Event.press = ', event.press
		print,'!x.s, !x.type = ', !x.s, !x.type	
		print,'!y.s, !y.type = ', !y.s, !y.type	
	endif
	print,''
;	stop
endif

case eventval of
	"SYMBOL_TYPE": begin
			if (symevent.select eq 1) then symbol = symevent.value + 1
		end
	"SYMBOL_SIZE": begin
			if (symevent.select eq 1) then begin
			case symevent.value of
				0: symsize = 1.0
				1: symsize = 1.5
				2: symsize = 2.0
				3: symsize = 2.5
				4: symsize = 3.0
			endcase
			endif
		end
	"COLOR_SLIDER": begin
		symcolor = symevent.value
		wset, color_box
		box=replicate(symcolor,90,15)
		tv,box
		end
	"APPLY": begin
		wset, map_window
		oplot,pointdata(*,0), pointdata(*,1),psym=symbol, $
			symsize=symsize, color=symcolor
	    end
	"DISMISS": begin
		widget_control,/destroy,symbase
	    end
		
endcase

end

; ***********************************************************************
; ***********************************************************************
pro plot_symbols
;	Read a file containing (long, lat) coordinate pairs and plot
;	them on the map.  Option of connecting lines along great circle.
; ***********************************************************************
; ***********************************************************************

@map_viewer_common
  
  if n_elements(pfile) eq 0 then $
     symbname = pickfile(/READ, $
                         TITLE='Select file containing (long, lat) pairs',  $
                         FILTER='*.pts', GROUP=map_viewerbase, /NOCONFIRM) $
  else symbname=pfile
  
  if (symbname eq '') then return

	;; Read data, handling I/O errors.

	get_lun, luin
	openr, luin, symbname
	dummy = ''
	readf, luin, dummy
	filecoords = strlowcase(dummy)
	pointdata = fltarr(3000)
	i = 0
	while (not EOF(luin)) do begin
		readf, luin, a,b
		pointdata(i) = a 
        pointdata(i+1) = b
		i = i + 2
	endwhile
	pointdata = pointdata(0:(i-1))
	if (idebug ge 2) then begin
		print,'Data array size: ', size(pointdata)
		help, pointdata
	endif
	if (idebug gt 5) then print, pointdata
	xdim = n_elements(pointdata)/2
	pointdata = reform(pointdata, 2, xdim)
	if (idebug ge 2) then begin
		print,'Data array size: ', size(pointdata)
		help, pointdata
	endif

if (idebug ge 5) then stop

	if (filecoords ne coords) then begin
	    select = 0
	    if ((filecoords eq 'celestial') and (coords eq 'galactic')) then $
		select = 1
	    if ((filecoords eq 'galactic') and (coords eq 'celestial')) then $
		select = 2
	    if ((filecoords eq 'celestial') and (coords eq 'ecliptic')) then $
		select = 3
	    if ((filecoords eq 'ecliptic') and (coords eq 'celestial')) then $
		select = 4
	    if ((filecoords eq 'ecliptic') and (coords eq 'galactic')) then $
		select = 5
	    if ((filecoords eq 'galactic') and (coords eq 'ecliptic')) then $
		select = 6
	    if (select eq 0) then return	; invalid input
	    ilong = pointdata(0,*)
	    ilat = pointdata(1,*)
if (idebug ge 5) then stop
	    euler, ilong, ilat, olong, olat, select	; convert coords
if (idebug ge 5) then stop
	    pointdata(0,*) = olong
	    pointdata(1,*) = olat
	if (idebug ge 2) then begin
		print,'Data array size: ', size(pointdata)
		help, pointdata
	endif
	    ilong = 0
	    ilat = 0
	    olong = 0
	    olat = 0	; free memory
	endif

;	pointdata(0,*) = -1.0*pointdata(0,*)	; invert for display

	pointdata = transpose(reform(pointdata, 2, xdim))
	if (idebug ge 2) then begin
		print,'Data array size: ', size(pointdata)
		help, pointdata
	endif
	if (idebug ge 2) then print,'Symbol color: ', !P.COLOR
	
; get symbol size, color, and style

	symbase=widget_base(title="Choose Symbol",/row)
	symlbase = widget_base(symbase,/column)
	symrbase = widget_base(symbase,/column)
	bgroup1 = cw_bgroup(symlbase, ['+', 'Asterisk', 'Dot', 'Diamond', $
		'Triangle', 'Square', 'X'], /column, label_top='Symbol Type', $
		set_value=symbol-1,uvalue="SYMBOL_TYPE",/exclusive, /frame)
	xpdmenu,['"Apply"        APPLY'],symlbase
	xpdmenu,['"Dismiss"        DISMISS'],symlbase

	bgroup2 = cw_bgroup(symrbase,['1', '1.5', '2', '2.5', '3'], /column, $
		label_top='Size', set_value=symsize,uvalue="SYMBOL_SIZE",$
		/exclusive, /frame)
	color_slider = widget_slider(symrbase,minimum=0,maximum=255,$
		value=symcolor, title='Color Index', uvalue="COLOR_SLIDER")
	color_widget = widget_draw(symrbase,xsize=90,ysize=15,ret=2)

	widget_control,/realize,symbase
	widget_control, color_widget, get_value = color_box
	wset, color_box
	box=replicate(symcolor,90,15)
	tv,box
	xmanager, "Choose Symbol", symbase, event_handler="SYM_EVENT"
	
	free_lun, luin

	return
end

; ***********************************************************************
; ***********************************************************************
pro plot_numbers, DEFAULT_FILE=default_file
;	Read a file containing (long, lat) coordinate pairs and plot
;	them on the map, labeling them with sequential numbers.  
; ***********************************************************************
; ***********************************************************************

@map_viewer_common

;	if not (keyword_set(default_file)) then $
    if n_elements(pfile) eq 0 then $
     numname = pickfile(/READ, $
                        TITLE='Select file containing (long, lat) pairs',  $
                        FILTER='*.pts', GROUP=map_viewerbase, /NOCONFIRM) $
    else numname=pfile
;	else numname = default_file

	if (numname eq '') then return

	;; Read data, handling I/O errors.

	get_lun, luin
	openr, luin, numname
	dummy = ''
	readf, luin, dummy
	filecoords = strlowcase(dummy)
	pointdata = fltarr(3000)
	i = 0
	while (not EOF(luin)) do begin
		readf, luin, a, b
		pointdata(i) = a 
		pointdata(i+1) = b
		i = i + 2
	endwhile
	num_pairs = i/2
	pointdata = pointdata(0:(i-1))		; Collapse data array to actual size
	if (idebug ge 2) then print,'Data array size: ', size(pointdata)
	if (idebug gt 5) then print,pointdata
	xdim = n_elements(pointdata)/2
	pointdata = reform(pointdata, 2, xdim)
	if (idebug ge 2) then print,'Data array size: ', size(pointdata)

if (idebug ge 5) then stop

	if (filecoords ne coords) then begin
	    select = 0
	    if ((filecoords eq 'celestial') and (coords eq 'galactic')) then $
		select = 1
	    if ((filecoords eq 'galactic') and (coords eq 'celestial')) then $
		select = 2
	    if ((filecoords eq 'celestial') and (coords eq 'ecliptic')) then $
		select = 3
	    if ((filecoords eq 'ecliptic') and (coords eq 'celestial')) then $
		select = 4
	    if ((filecoords eq 'ecliptic') and (coords eq 'galactic')) then $
		select = 5
	    if ((filecoords eq 'galactic') and (coords eq 'ecliptic')) then $
		select = 6
	    if (select eq 0) then return	; invalid input
	    ilong = pointdata(0,*)
	    ilat = pointdata(1,*)
if (idebug ge 5) then stop
	    euler, ilong, ilat, olong, olat, select	; convert coords
if (idebug ge 5) then stop
	    pointdata(0,*) = olong
	    pointdata(1,*) = olat
	    ilong = 0
	    ilat = 0
	    olong = 0
	    olat = 0	; free memory
	endif

;	pointdata(0,*) = -1.0*pointdata(0,*)	; invert for display

	pointdata = transpose(reform(pointdata, 2, xdim))
	if (idebug ge 2) then print,'Data array size: ', size(pointdata)
	if (idebug ge 2) then print,'Number data pairs: ', num_pairs
	if (idebug ge 2) then print,'Symbol color: ', !P.COLOR
	wset, map_window
	numbers = strtrim(string(indgen(num_pairs) + 1),2)
	xyouts,pointdata(*,0), pointdata(*,1),numbers, alignment=0.5,color=0,size=2,charthick=3
	
	free_lun, luin
;	pointdata = 0

	return
    end

;***************
;***************
function parse_str,lun
;function to parse string elements read from a file,
;defined by its logical unit number, lun,
;as a single line. Returns a string array with as
;many elements as are string elements in the input
;string when seperated at whitespace intervals of any length

data_str=''

readf,lun,data_str
data_str=strtrim(data_str,2)
data_str=strcompress(data_str)
data_arr=str_sep(data_str,' ')
return,data_arr
end
;***********
;***********

;************
;************
function get_lines,fname
;function to return the number of lines in a given
;file

ct=0.
openr,lun,fname,/get_lun
while not eof(lun) do begin
        line=parse_str(lun)
        ct=ct+1.
endwhile
free_lun,lun
return,ct
end
;***********
;***********

; ***********************************************************************
; ***********************************************************************
pro plot_sunmoon                ;,auto=auto,noskip=noskip,year=year
;	User enters the desired date, and the sun and moon are plotted 
;	appropriately.
; ***********************************************************************
; ***********************************************************************

@map_viewer_common
  
  jdnow=systime(/julian)
  daycnv,jdnow,yr,mn,day,hr
  dy = ymd2dn(yr,mn,day)
  initial=[yr,dy]
  if not keyword_set(auto) then begin 
     ;;getting user input for date
     date=dialog_input(title='Enter Date (default=today):',$
                       nfields=2,prompt=['yyyy','ddd'],xsize=200,$ 
                       dialog_parent=map_viewerbase,initial=initial)
     ydn2md,date[0],date[1],month,day
     year=date[0]
     
     partday=date[1] mod 1.
     hrs=floor(partday*24)
     mins=floor((partday*24-hrs)*60)
     secs=(((partday*24-hrs)*60)-mins)*60.
     if secs ge 30. then mins=mins+1

     if mins eq 60 then begin
        mins=0
        hrs=hrs+1
     endif
     if hrs eq 24 then begin
        hrs=0
        date[1]=date[1]+1.
     endif
     
     ydn2md,date[0],date[1],month,day
     jdcnv,date[0],month,day,partday*24.,jd
     
     ;;plotting sun/moon data for date
     sunpos,jd,sunra,sundec
     moonpos,jd,moonra,moondec
     cpointdata=fltarr(2,2)
     cpointdata[0,0]=sunra
     cpointdata[1,0]=sundec
     cpointdata[0,1]=moonra
     cpointdata[1,1]=moondec
     filecoords='celestial'
     select=0
     if ((filecoords eq 'celestial') and (coords eq 'galactic')) then $
        select = 1
     if ((filecoords eq 'celestial') and (coords eq 'ecliptic')) then $
        select = 3
     
     ilong = cpointdata(0,*)
     ilat = cpointdata(1,*)
     if (idebug ge 5) then stop
     if select ne 0 then $
        euler, ilong, ilat, olong, olat, select $ ; convert coords 
     else begin
        olong=ilong
        olat=ilat
     endelse 
     if (idebug ge 5) then stop
     cpointdata(0,*) = olong
     cpointdata(1,*) = olat
     
     xdim = n_elements(cpointdata)/2
     cpointdata = transpose(reform(cpointdata, 2, xdim))
     
     plotsym,0,/fill,1
     plots,cpointdata[0,0], cpointdata[0,1],psym=8,color=255,symsize=3 ;sun
     plots,cpointdata[1,0], cpointdata[1,1],psym=8,color=0,symsize=3 ;moon
     
     ;;sun
     cir.color = 255            ;(cir.color+32) mod 256
     ang = 2*!PI*findgen(49)/48. ;Get position every 5 deg
     psize=45.
     xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
     xarr=xarr+cpointdata[0,0]  &  yarr=yarr+cpointdata[0,1]
     xarr2=fltarr(49)
     yarr2=fltarr(49)
     for ct=0,48 do begin
        temp=ll_arc_distance([cpointdata[0,0],cpointdata[0,1]]*!pi/180.,45.*!pi/180.,ct*7.5*!pi/180.)
        xarr2(ct)=temp(0)*180./!pi
        yarr2(ct)=temp(1)*180./!pi
     endfor
     
;;    for i=1,48 do begin
;;       cir_2p,[xarr[i-1],yarr[i-1]],[xarr[i],yarr[i]],0.5
;;    endfor 
     for i=1,48 do begin
        cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]]
     endfor 
     
     ;;moon
     cir.color = 0
     ang = 2*!PI*findgen(49)/48. ;Get position every 5 deg
     psize=30.
     xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
     xarr=xarr+cpointdata[1,0]  &  yarr=yarr+cpointdata[1,1]
     xarr2=fltarr(49)
     yarr2=fltarr(49)
     for ct=0,48 do begin
        temp=ll_arc_distance([cpointdata[1,0],cpointdata[1,1]]*!pi/180.,30.*!pi/180.,ct*7.5*!pi/180.)
        xarr2(ct)=temp(0)*180./!pi
        yarr2(ct)=temp(1)*180./!pi
     endfor
;;    for i=1,48 do begin
;;       cir_2p,[xarr[i-1],yarr[i-1]],[xarr[i],yarr[i]],0.5
;;    endfor 
     for i=1,48 do begin
        cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]]
     endfor 
     
     return
  endif 
  
  
  ;;NOW ASSUMES AUTO RUN BY CHILE WITH PPST/EARTH CONSTRAINTS
  nslew=n_elements(linedata[0,*])
  
;THE LOOP CRITERIA HAS BEEN ALTERED TO ALLOW THE CONTRAINT CIRCLES TO
;BE 'NUDGED' FORWARD OR BACKWARD BY A USER INPUT NUMBER OF MINUTES.
;WHILE NUDGING, THE LAST PLOTTED SLEW PATH IS HELD STATIC BUT THE
;NUDGED ORBIT TIME IS MAINTAINED (or cumulative if you prefer) FROM NUDGE TO NUDGE (so consecutive
;nudges of 5m and -5m take you to the original constraint pattern, not
;-5m). ONCE A NEW SLEW PASS IS INPUT BY THE USER, THE CONSTRAINT
;PATTERN IS UPDATED TO THE NEW PASS TIME +/- 0 MINUTES, SO THAT NUDGES
;OF THE CONSTRAINTS ABOUT THE NEW PASSTIME BEGIN FROM 0, NOT FROM ANY
;NUDGE TIME ABOUT A PREVIOUS PASS.


  k='1'
  j=0
  while (k ne 'q') and (j lt nslew) do begin 

     if strmid(k,strlen(k)-1,1) ne 'm' then begin
        date=[year,linedata[2,j]]
        date2pass=date
     endif else begin
;        if strmid(k,strlen(k)-1,1) eq 's' then begin           
;        endif else $
        date[1]=date[1]+float(strmid(k,0,strlen(k)-1))/1440.
     endelse

     partday=date[1] mod 1.
     hrs=floor(partday*24)
     mins=floor((partday*24-hrs)*60)
     secs=(((partday*24-hrs)*60)-mins)*60.
     if secs ge 30. then mins=mins+1
;     if mins eq 60 then mins=59;NOT QUITE RIGHT BUT CLOSE AND MUCH EASIER THAN 
     if mins eq 60 then begin
        mins=0
        hrs=hrs+1
     endif
     if hrs eq 24 then begin
        hrs=0
        date[1]=date[1]+1.
     endif
;     if date[1] lt 180. then date[0]=2005
     if hrs lt 10 then hrs='0'+strtrim(string(hrs),2)
     if mins lt 10 then mins='0'+strtrim(string(mins),2)
     if secs lt 10 then secs='0'+strtrim(string(secs),2) ;not currently used
     partdaystr=strtrim(string(hrs),2)+':'+strtrim(string(mins),2)+':00.00'

     wed=where(fix((strmid(earth_dat[*,0],0,3)) eq fix(date[1])) and (earth_dat[*,1] eq partdaystr))
     elaz2,-1.*[earth_dat[wed,2],earth_dat[wed,3],earth_dat[wed,4]],dec,ra
     print,[earth_dat[wed,2],earth_dat[wed,3],earth_dat[wed,4]]
     earth_ra=ra
     earth_dec=dec

     print,date[0],fix(date[1]),fix((date[1] mod 1.)*24.),((date[1] mod 1.)*24.*60. mod 60.)
     
     
     ydn2md,date[0],date[1],month,day
     jdcnv,date[0],month,day,partday*24.,jd

     sunpos,jd,sunra,sundec
     moonpos,jd,moonra,moondec
     
     cpointdata=fltarr(2,3)
     cpointdata[0,0]=sunra
     cpointdata[1,0]=sundec
     cpointdata[0,1]=moonra
     cpointdata[1,1]=moondec
     cpointdata[0,2]=earth_ra
     cpointdata[1,2]=earth_dec
     num_pairs=3

     gcirc,1,sunra/15.,sundec,earth_ra/15.,earth_dec,consdist1
     gcirc,1,moonra/15.,moondec,earth_ra/15.,earth_dec,consdist2
     consdist1=consdist1/3600.  ;arcsecs to deg
     consdist2=consdist2/3600.  ;arcsecs to deg
     econ=67+28.
     mcon=30.
     scon=45.
     
;     noskip=0
     if not keyword_set(noskip) then noskip=0
     if ((consdist1 ge econ+scon-5. and consdist1 le econ+scon+15.) or (consdist2 ge econ+mcon-5. and consdist2 le econ+mcon+15.)) or (noskip eq 1) and (j gt 0) or (strmid(k,strlen(k)-1,1) eq 'm' or strmid(k,strlen(k)-1,1) eq 's') then begin 
        
        p1=linedata[0:1,j-1]
        p2=linedata[0:1,j]
        print,p1,p2
        
        p1r = p1 * !dtor        ;To radians
        p2r = p2 * !dtor
        twopi = 2 * !pi
        dlon = twopi + p2r(0) - p1r(0) ;delta longitude
        while dlon gt !pi do dlon = dlon - twopi ;to -pi to +pi
                                ; Great Circle Distance:
        cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
        if (idebug ge 5) then print,'cosd = ', cosd
        if cosd gt 1.0d then cosd=1.
        theta = acos(cosd)*180.0/!pi ;Angular separation in degrees
        
        n = fix(theta)          ;# of points (one per degree)

; Convert input directions to vectors

        lon = [p1r(0), p2r(0)]
        lat = [p1r(1), p2r(1)]
        x = cos(lat) * cos(lon) ;To xyz space
        y = cos(lat)* sin(lon)
        z = sin(lat)
        u = [x(0), y(0), z(0)]  ; 3-vectors
        v = [x(1), y(1), z(1)]
        
        if (idebug ge 3) then begin
           print,'  u = ', u
           print,'  v = ', v
        endif
        
        temp = crossp(u,v)
        w = temp/norm(temp)
        temp = crossp(w,u)
        vv = temp/norm(temp)
        
        lon = fltarr(n+1)
        lat = fltarr(n+1)
        for i = 0,n do begin
           ds = i * (theta * !DTOR/n)
           uu = [cos(ds), sin(ds), 0.0]
           matrix = [[u], [vv], [w]]
           uuu = matrix # uu
; Convert back to lat, long
           lat(i) = asin(uuu(2)/norm(uuu))*!RADEG
           ccc = uuu(0)^2 + uuu(1)^2
           lon(i) = 0.0
           if (ccc ne 0.0) then begin
              if (idebug ge 5) then print,'uuu(0),ccc=',uuu(0),' ',ccc
              temp = acos(uuu(0)/sqrt(ccc))
              if (uuu(1) lt 0.0) then temp = -1.0*temp
              lon(i) = (temp*!RADEG + 720.0) mod 360.0
           endif
           if (idebug ge 3) then print,'  i, lon, lat = ', i, lon(i), lat(i)
        endfor
        
                                ;get cpointdata into right units
        
        filecoords='celestial'
        select=0
        if ((filecoords eq 'celestial') and (coords eq 'galactic')) then $
           select = 1
        if ((filecoords eq 'celestial') and (coords eq 'ecliptic')) then $
           select = 3
;        if (select eq 0) then return ; invalid input
        
        ilong = cpointdata(0,*)
        ilat = cpointdata(1,*)
        if (idebug ge 5) then stop
        if select ne 0 then $
           euler, ilong, ilat, olong, olat, select $ ; convert coords 
        else begin
           olong=ilong
           olat=ilat
        endelse 
        if (idebug ge 5) then stop
        cpointdata(0,*) = olong
        cpointdata(1,*) = olat
        ilong = 0
        ilat = 0
        olong = 0
        olat = 0                ; free memory
;                                print,cpointdata
        
;        cpointdata(0,*) = -1.0*cpointdata(0,*) ; invert for display
;        if select eq 3 then begin 
        olon=lon
        olatd=lat
;        endif else euler,lon,lat,olon,olatd,select
;        euler,sunra,sundec,slon,slat,3
;        euler,moonra,moondec,mlon,mlat,3
        slon=cpointdata[0,0]
        slat=cpointdata[1,0]
        mlon=cpointdata[0,1]
        mlat=cpointdata[1,1]
        elon=cpointdata[0,2]
        elat=cpointdata[1,2]
        
;           cpointdata(0,*) = -1.0*cpointdata(0,*) ; invert for display
        xdim = n_elements(cpointdata)/2
        cpointdata = transpose(reform(cpointdata, 2, xdim))
        if (idebug ge 2) then print,'Data array size: ', size(cpointdata)
        if (idebug ge 2) then print,'Number data pairs: ', num_pairs
        if (idebug ge 2) then print,'Symbol color: ', !P.COLOR
        wset, map_window
        numbers = strtrim(string(indgen(num_pairs) + 1),2)
        
        
        wsuncon=where((((olon lt slon+scon+5.) and $
;                           (olon gt elon+econ-5.))$
        (olon gt slon-scon-20.))$
           or ((olon gt slon-scon-5.) $
;                                  and (olon lt elon-econ+5.))) $
        and (olon lt slon+scon+20.))) $
           and (consdist1 ge econ+scon-5. and $
                consdist1 le econ+scon+15.) $
           and (olatd ge -2 and olatd le 2),nws)
        wmooncon=where((((olon lt mlon+mcon+5.) and $
                         (olon gt mlon-mcon-20.)) $
;                             (olon gt elon+econ-5.))$
        or ((olon gt mlon-mcon-5.) and $
;                                    (olon lt elon-econ+5.)))$
        (olon lt mlon+mcon+20.))) $
           and (consdist2 ge econ+mcon-5. and $
                consdist2 le econ+mcon+15.) $
           and (olatd ge -2 and olatd le 2),nwm)

        if (nws gt 0 or nwm gt 0) or (noskip eq 1) or (strmid(k,strlen(k)-1,1) eq 'm') or strmid(k,strlen(k)-1,1) eq 's' then begin
           
           erase
           
           lat0 = 0
           lon0 = 0
           rot0 = 0
           WIDGET_CONTROL, LAT_SLIDER, SET_VALUE = lat0
           WIDGET_CONTROL, LON_SLIDER, SET_VALUE = lon0
           WIDGET_CONTROL, ROT_SLIDER, SET_VALUE = rot0
           map_viewer_color
           doreset=1
           draw_map
           
           plotsym,0,/fill,1

           ;earth
           cir.color = 128
           ang = 2*!PI*findgen(49)/48. ;Get position every 5 deg
           psize=67.+28.
           for psizes=1,psize do begin 
              xarr = psizes*cos(ang)  &  yarr = psizes*sin(ang)
              xarr=xarr+cpointdata[2,0]  &  yarr=yarr+cpointdata[2,1]
              xarr2=fltarr(49)
              yarr2=fltarr(49)
;    for psizes=1,psize do begin 
              for ct=0,48 do begin
                 temp=ll_arc_distance([cpointdata[2,0],cpointdata[2,1]]*!pi/180.,psizes*!pi/180.,ct*7.5*!pi/180.)
                 if temp(0) ne temp(0) then temp(0)=0. ;trick to check for NaNs and 0-fill
                 if temp(1) ne temp(1) then temp(1)=0.
                 xarr2(ct)=temp(0)*180./!pi
                 yarr2(ct)=temp(1)*180./!pi
;          print,temp(1)
              endfor
;      print,yarr2
;help,xarr2
;help,yarr2
;print,psizes
;print,cpointdata       
;;       for i=1,48 do begin
;;          cir_2p,[xarr[i-1],yarr[i-1]],[xarr[i],yarr[i]]
;;       endfor 
              for i=1,48 do begin
                 cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]],0.5
              endfor 
           endfor 
           
           plots,cpointdata[0,0], cpointdata[0,1],psym=8,color=255,symsize=3 ;sun
           plots,cpointdata[1,0], cpointdata[1,1],psym=8,color=0,symsize=3 ;moon
           plots,cpointdata[2,0], cpointdata[2,1],psym=8,color=128,symsize=1 ;earth
           
           
                                ;sun
           cir.color = 255      ;(cir.color+32) mod 256
           ang = 2*!PI*findgen(49)/48. ;Get position every 5 deg
           psize=45.
           xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
           xarr=xarr+cpointdata[0,0]  &  yarr=yarr+cpointdata[0,1]
           xarr2=fltarr(49)
           yarr2=fltarr(49)
           for ct=0,48 do begin
              temp=ll_arc_distance([cpointdata[0,0],cpointdata[0,1]]*!pi/180.,45.*!pi/180.,ct*7.5*!pi/180.)
              xarr2(ct)=temp(0)*180./!pi
              yarr2(ct)=temp(1)*180./!pi
           endfor
           
;;    for i=1,48 do begin
;;       cir_2p,[xarr[i-1],yarr[i-1]],[xarr[i],yarr[i]],0.5
;;    endfor 
           for i=1,48 do begin
              cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]]
           endfor 
           
                                ;moon
           cir.color = 0
           ang = 2*!PI*findgen(49)/48. ;Get position every 5 deg
           psize=30.
           xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
           xarr=xarr+cpointdata[1,0]  &  yarr=yarr+cpointdata[1,1]
           xarr2=fltarr(49)
           yarr2=fltarr(49)
           for ct=0,48 do begin
              temp=ll_arc_distance([cpointdata[1,0],cpointdata[1,1]]*!pi/180.,30.*!pi/180.,ct*7.5*!pi/180.)
              xarr2(ct)=temp(0)*180./!pi
              yarr2(ct)=temp(1)*180./!pi
           endfor
;;    for i=1,48 do begin
;;       cir_2p,[xarr[i-1],yarr[i-1]],[xarr[i],yarr[i]],0.5
;;    endfor 
           for i=1,48 do begin
              cir_2p,[xarr2[i-1],yarr2[i-1]],[xarr2[i],yarr2[i]]
           endfor 
           
;    pointdata = 0
           
           map_grid,latdel=10,londel=10
           map_grid,latdel=30,londel=30, glinestyle=0
           
           plot_lines,date2pass
           plot_numbers
           k=''
;stop
           read,k,prompt='Type: # to advance # slews, n to continue to next slew, #m to nudge constraints by # minutes, (or in doskip mode type #s to advance to slews in between slews of concern): '

        endif
        
     endif 

     if string(k) ne 'q' then begin
        if string(k) eq 'n' then begin
           j=j+1 
           k='1'
;       endif else begin
;           if abs(float(k)) ge 1 then j=j+k*1 else j=j+0
;       endelse
        endif else begin
           if string(k) eq 's' then stop
           if strmid(k,strlen(k)-1,1) ne 'm' then begin
              if strmid(k,strlen(k)-1,1) eq 's' then $
                 j=j+strmid(k,0,strlen(k)-1)*1 $
              else j=j+k*1 
           endif else j=j+0
        endelse
     endif
     
;stop
     
  endwhile
  print,'I want to quit'
  return
end

; ***********************************************************************
; ***********************************************************************
pro create_draw_window
; ***********************************************************************
; ***********************************************************************

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

@map_viewer_common

if (numproj le 5) then xsize = max_xsize else xsize = ysize*5.5/5.75
if (idebug ge 1) then print,'Entering create_draw_window: xsize = ', xsize
if (idebug ge 1) then print,'         System window size: xsize = ', !d.x_size

if (idebug ge 1) then print,'old_xsize = ', old_xsize
if (xsize ne old_xsize) then begin
	if (idebug ge 1) then print,'Resizing window...'
	WIDGET_CONTROL, drawable, xsize = xsize, ysize = ysize 
	if (idebug ge 1) then print,'Group Leader ID: ', map_viewerbase
endif
if (idebug ge 1) then print,'Done resizing, window size: xsize = ', xsize
if (idebug ge 1) then print,'        System window size: xsize = ', !d.x_size

old_xsize = xsize

return
end
