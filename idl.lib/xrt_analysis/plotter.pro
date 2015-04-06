pro plotter, showplot, Param1, Param2, TITLE=Title, $
		XTITLE=Xtitle, YTITLE=Ytitle, SUBTITLE=subtitle, $
		XRANGE=xrange, YRANGE=yrange, PSYM=psym, $
		HISTOGRAM=Histogram, MIN=Min, BINSIZE=Binsize, $
		NOBLOCK=noblock,top_base=top_base

; $Id: plotter.pro 4.4 1994/10/16 14:29:20 burrows Exp $
; MODIFIED:
;   05/31/04 by JLR: added keyword noblock to allow option for widget to 
;                    not be modal (not block)
;   12/19/95 by PSB: Improved handling of histogram-related keywords and
;   clarified the comments about plotting histograms.
;
;  This is a plotting program for praxis.  Based on the value of
;	the showplot variable in the call line arguments, it either
;	does nothing, plots the input array and returns, or sends the
;	input array to Pat Broos' interactive plot viewer.
;
;  PARAMETERS:
;	showplot: 0: do nothing
;		  1: plot data in the current IDL window, return
;		  2: open interactive plot viewer widget
;	param1 = x data if 2 data arrays are passed, y data otherwise
;	param2 = y data if 2 data arrays are passed
;       TITLE, SUBTITLE, XTITLE, YTITLE, PSYM: same as IDL plot routine
;	XRANGE, YRANGE: only used when showplot = 1
;	HISTOGRAM, MIN, BINSIZE: see note below
;
;;; Histogram data can be plotted with a nice X axis and a bar chart look
;;; in two ways. 

;;; (1) If you *already* have calculated the X coordinates of the center of
;;; each bin, then pass those X coordinates as Param1, pass the histogram
;;; array as Param2, and set HISTOGRAM.  Do NOT use the MIN or BINSIZE keywords.

;;; (2) If you want *this* routine to calculate the X coordinates of the bins
;;; then pass only the histogram array as Param1, set HISTOGRAM, and pass
;;; the MIN and BINSIZE values that were used to compute the histogram array.
;==========================================================================

@common			; include PRAXIS common blocks

; Set defaults
if not keyword_set(title) then title=''
if not keyword_set(xtitle) then xtitle=''
if not keyword_set(ytitle) then ytitle=''
if not keyword_set(subtitle) then subtitle=''
if not keyword_set(psym) then psym=0
if     keyword_set(histogram) then psym=10
if not keyword_set(min) then min=0.0
if not keyword_set(binsize) then binsize=1.0

if (showplot eq 1) then begin
	old_window = !D.window
	if (n_elements(plot_window) eq 1) then begin
	    if (plot_window lt 0) then begin
		window,-plot_window,title='Plot Window'
		plot_window = !D.window		; save window number
	    endif else wset, plot_window
	endif else begin
	    window,/free,title='Plot Window'
	    plot_window = !D.window		; save window number
	endelse
endif

case n_params() of
	2: begin
	    y_data = Param1
	    num_points = n_elements(y_data)
	    x_data = lindgen(num_points)

	    if keyword_set(histogram) then begin
	      x_data = x_data * binsize + min + binsize/2.0
	      y_data = [0, y_data, 0]
	      x_data = [x_data(0)-binsize, x_data, x_data(num_points-1)+binsize]
	    endif

	    if not keyword_set(yrange) then begin
			ymin = min(param1)
			ymax = max(param1)
	    endif 

	   end
	3: begin
	   y_data = Param2
	   x_data = Param1

		if not keyword_set(yrange) then begin
			ymin = min(param2)
			ymax = max(param2)
		endif
	   end
	else: message,'Invalid number of parameters passed to PLOTTER'
endcase

case showplot of
   0: begin
      end

   1: begin
	if not keyword_set(xrange) then $
   		xrange = [min(x_data),max(x_data)]

	if not keyword_set(yrange) then begin
        	delta = ((ymax - ymin)*0.1/2.0) > 0.1
        	yrange = [ymin-delta, ymax+delta]
	endif

      	plot, x_data, y_data, TITLE=title, SUBTITLE=subtitle, PSYM=psym, $
	    XTITLE=xtitle, YTITLE=ytitle, XRANGE=xrange, YRANGE=yrange

      end

   2: begin
	if not keyword_set(xrange) then begin
   		xmin = min(x_data)
		xmax = max(x_data)
	endif else begin
		xmin = xrange(0)
		xmax = xrange(1)
	endelse

	num_points = n_elements(x_data)
	temp = where(x_data gt xmin, count)
	if (count le 0) then ixmin = 0 $
	    else ixmin = max([temp(0) - 1, 0])
	temp = where(x_data ge xmax, count)
	if (count le 0) then ixmax = num_points - 1 $
	    else ixmax = temp(0)

	if not keyword_set(yrange) then begin
        	delta = ((ymax - ymin)*0.1/2.0) > 0.1
        	ymin = min(y_data)-delta
		ymax = max(y_data)+delta	
	endif else begin
		ymin = yrange(0)
		ymax = yrange(1)
	endelse
	
	if keyword_set(noblock) then block=0 else block=1

     	function_1d, top_base, x_data(ixmin:ixmax), y_data, $
		title=title, subtitle=subtitle,$
		xtitle=xtitle, ytitle=ytitle, psym=psym, BLOCK=block
        xmanager
      end
endcase

if (showplot eq 1) then $
	if (old_window ge 0) then wset, old_window	; reset plot window

end
