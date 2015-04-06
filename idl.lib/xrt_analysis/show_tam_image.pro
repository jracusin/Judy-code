; $Id: show_tam_image.pro 4.1 1994/08/26 21:46:59 patb Exp $

pro show_tam_image, pxmin, pxmax, pymin, pymax, pmin, pmax, $
		nokeep=nokeep, new_window=new_win

; name	: show_tam_image
; Inputs:
;	pxmin = minimum column for processing
;	pxmax = maximum column for processing
;	pymin = minimum row for processing
;	pymax = maximum row for processing
; 	pmin = minimum fraction to display (default is 0.01)
; 	pmax = maximum fraction to display (default is 0.99)
; Keywords:
;       NOKEEP: Destroy the zoom window after exiting the procedure.
;		By default, show_image keeps the window.
;
;   	NEW_WINDOW:	Normally, if SHOW_IMAGE is called with /KEEP and 
;		then called again, it will use the same window to display 
;		the zoomed image.  Calling SHOW_IMAGE with /NEW_WINDOW  
;		forces it to create a new window for this purpose.
; Common blocks:
;	ccd_frame_common:  image = input image to be displayed	
;

; author: Dave Burrows
; date	: 07/16/94
; purpose: display image on screen, using scaling similar to s.X.
;	This routine makes a histogram of the data values and scales
;	the display to cover from pmin to pmax of the pixels.
; mods	: 08/02/94 by DNB: added code to create new window for displays
;		(or use image window if it already exists).  Window is
;		scaled to show entire image.  Added new optional parameters.
;		Mods based on zoom.
;	02/27/96 by DNB: changed so only portion of image to be processed
;		is used to calculate histogram.
;
@pass1_common
@tam_common
common show_window, show_w

if n_elements(image) le 0 then return		; no input image
image_size = size(image)
if n_elements(pxmin) le 0 then pxmin = 0
if n_elements(pxmax) le 0 then pxmax = image_size(1)-1
if n_elements(pymin) le 0 then pymin = 0
if n_elements(pymax) le 0 then pymax = image_size(2)-1
if n_elements(pmin) le 0 then pmin = 0.01
if n_elements(pmax) le 0 then pmax = 0.99
if n_elements(show_w) eq 0 then show_w = -1		;No display window yet

old_w = !d.window
if keyword_set(new_win) then show_w = -1	;Don't use old window (if any)
;
;  If an old window is to be used, then make sure it still exists.  (Added by
;  William Thompson, 20 May 1993.)
;
if show_w ge 0 then begin
	device, window_state=win_state
	if not win_state(show_w) then show_w = -1
endif
;
;  Make sure the parameters xs and ys agree with the size of the window, in
;  case a window is being reused from a previous call to SHOW_IMAGE,/KEEP.  
;  (Added by William Thompson, 20 May 1993.)
;
IF show_w GE 0 THEN BEGIN
	OLD_WINDOW = !D.WINDOW
	WSET, show_w
	XS = !D.X_SIZE
	YS = !D.Y_SIZE
	WSET, OLD_WINDOW
ENDIF


pmin = pmin < 1.00 > 0.00
pmax = pmax < 1.00 > pmin

;Set process limits for calculation of histogram
xmin = pxmin
xmax = pxmax
ymin = pymin
ymax = pymax

his = histogram(image(xmin:xmax,ymin:ymax))
dmin = min(image(xmin:xmax,ymin:ymax))
;dmax = max(image(xmin:xmax,ymin:ymax))

npix=total(his)
s=size(his)

sum=0L
hmin=0L
hmax=npix

for i=0L, s(1)-1 do begin
	sum = sum + his(i)
	if (sum le pmin*npix) then hmin = i
	if (sum le pmax*npix) then hmax = i
endfor

; now figure out what the real values are and display the image

hmin = hmin + dmin
hmax = hmax + dmin

if (color_lut ge 0) then begin
	loadct,(color_lut mod 38)
endif else begin
	lut = ((abs(color_lut)-1) mod 4) + 1
	lut_file = '/bulk/pkg/xray/idl_lib/praxis/idl.color' $
		+ strcompress(string(lut),/remove_all) + '.lut'
	read_lut,lut_file
end

;; Since PASS1 uses the Penn State display widget programs, it must adhere
;; to the color table protocol used by those programs.  Thus we must
;; TV images using only the lower portion of the color table.
color_manager, NCOLORS=ncolors

; set up new image display window if needed
syz = size(image)
xsize = syz(1) < max_xsize
ysize = syz(2) < max_ysize
if show_w lt 0 then begin	;Make new window?
	window,/free,xsize=xsize,ysize=ysize,title='XRT TAM Image'
	show_w = !d.window	; save window number for future use
endif else begin
	wset,show_w		; reset to image display window
;	erase			;Erase it
endelse

if (window_mode eq 0) then tv,bytscl(image,min=hmin,max=hmax,top=ncolors-1) $
else tvscl, image

window_title = 'File: '+file_tam
xyouts,10,10,window_title,charsize=1.0,charthick=1.0,/device

IF KEYWORD_SET(NOKEEP) THEN BEGIN
        if show_w ge 0 then wdelete,show_w              ;Done with window
        show_w = -1
ENDIF
show_win = show_w	;Return index of zoom window to user
end
	
