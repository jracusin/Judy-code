pro mapzoom,xsize=xs,ysize=ys,fact=fact,interp=interp,continuous = cont, $
	pxsc
;+
; NAME:	
;	MAPZOOM
; PURPOSE:
;	Display part of an image (or graphics) from the current window
;	expanded in another window.
;	The cursor is used to mark the center of the zoom.
; CATEGORY:
;	Display.
; CALLING SEQUENCE:
;	lzoom, .... Keyword parameters.
; INPUTS:
;	All input parameters are keywords.
;	Fact = zoom expansion factor, default = 4.
;	Interp = 1 or set to interpolate, otherwise pixel replication is used.
;	xsize = X size of new window, if omitted, 512.
;	ysize = Y size of new window, default = 512.
;	Continuous = keyword param which obviates the need to press the
;		left mouse button.  The zoom window tracks the mouse.
;		Only works well on fast computers.
;
; OUTPUTS:
;	No explicit outputs.   A new window is created and destroyed when
;	the procedure is exited.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	A window is created / destroyed.
; RESTRICTIONS:
;	Only works with color systems.
; PROCEDURE:
;	Straightforward.
; MODIFICATION HISTORY:
;-
on_error,2              ;Return to caller if an error occurs
if n_elements(xs) le 0 then xs = 512
if n_elements(ys) le 0 then ys = 512
if n_elements(fact) le 0 then fact=4
if keyword_set(cont) then waitflg = 2 else waitflg = 3
ifact = fact
old_w = !d.window	;Save original window number
orig_ysiz = !d.y_vsize	;Save original window y-size
zoom_w = -1		;No zoom window yet
tvcrs,1			;enable cursor
ierase = 0		;erase zoom window flag
print,'Left button to zoom, right button for new zoom or exit.'
again:
	tvrdc,x,y,waitflg,/dev	;Wait for change
	case !err of
4:	goto, done
2:	print, 'Use the left button to zoom.'
else:	begin
	x0 = 0 > (x-xs/(ifact*2)) 	;left edge from center
	y0 = 0 > (y-ys/(ifact*2)) 	;bottom
	nx = xs/ifact			;Size of new image
	ny = ys/ifact
	nx = nx < (!d.x_vsize-x0)
	ny = ny < (!d.y_size-y0)
	x0 = x0 < (!d.x_vsize - nx)
	y0 = y0 < (!d.y_vsize - ny)
	a = tvrd(x0,y0,nx,ny)		;Read image
	if zoom_w lt 0 then begin	;Make new window?
		window,/free,xsize=xs,ysize=ys,title='Zoomed Image'
		zoom_w = !d.window
	endif else begin
		wset,zoom_w
		if ierase then erase		;Erase it?
		ierase = 0
	endelse
	xss = nx * ifact	;Make integer rebin factors
	yss = ny * ifact
	tv,rebin(a,xss,yss,sample=1-keyword_set(interp))
;	print, 'Left button to view coordinates, right buttom for new zoom or exit.'
	print, 'Right buttom for new zoom or exit.'
	wset,old_w
loop:
		wset,zoom_w			; Go to the zoomed window
		cursor,x,y,/dev		; selectively sample the
						;   mouse coordinates
		if (!err eq 4) then goto,another
						; If right button is
						; pushed, get another sample
						; from the original, or quit.
	
; Following code commented out because it doesn't work right in map_viewer
;		xposn=x0+x/ifact			; Coordinates
;     		yposn=y0 + y/ifact
;		xy = [xposn,yposn]
;		p=convert_coord(xposn,yposn,/device,/to_data)
;		x=string(p(0))
;		y=string(p(1))
;		print, 'The cursor position is:'
;		print, x, y
			goto,loop		; Get another pixel value

another:				; Get another section of original
					;   image
		print, ' '
		print, 'You may zoom another section of the original window'
		print, 'with the left mouse button,'
		print, 'or hit the right mouse button in the original window'
		print, 'to quit the zoom program.'
		print, ' '
		wset,zoom_w            ; Erase zoom window
		erase
		wset, old_w		;Set to original image
	endelse
endcase
goto,again

done:
if zoom_w ge 0 then wdelete,zoom_w		;Done with window

return
end



