pro piled_up_photodiode, buffer_pointer, error_code
; $Id: piled_up_photodiode.pro 1.0 2001/12/21 17:25:30 burrows Exp $

;name	: piled_up_photodiode
;author	: Dave Burrows
;date	: 01/17/95
;lang	: IDL
;
;purpose: This routine processes piled-up photodiode mode frames.  The output
;	file is a 1-D fits image file containing the PU-PD "lightcurve". 
;
;	It returns an error code which is 0 if no errors are found:
;	error_code	
;	    0	no errors
;	   81	EOF encountered before end of data frame
;
; Rev:
;	02/02/05 by DNB: modified to handle other sizes of input files 
;		(only affects output FITS image file)
;	05/13/03 by DNB: added checking of EOL and EOF flags
;	05/02/03 by DNB: changed pupd_image array to UINT (from FLT)
;	04/01/03 by DNB: Minor bug fix
;	03/27/03 by DNB: fixed bug in previous change
;	03/21/03 by DNB: added 2-D image output file
;	09/05/02 by DNB: based on ccd_raw_frame and low_rate_photodiode.
;	05/20/02 by DNB: changed scaling on plots
;	04/11/02 by DNB to add more error information when pixels are missing.
;	04/01/02 by DNB to fix histogram scaling.
;		Also added noise calculation.
;	01/26/02 by DNB to generate histogram of raw data frame.
;
; $Log: process_raw_ccd_frame.pro $
;
;

@pass1_common

npucol = 631	; number of "columns" in PU-PD mode

image = intarr(2,2)	; dummy array so we can make a FITS header

n_calls = n_calls + 1

error_code = 0
cmprssd = 0				; flag for compressed frame

ccd_frame = ccd_frame + 1

print,'Processing CCD Frame # ', strtrim(string(ccd_frame),2), $
		' in piled-up photodiode mode'
printf,lulog,format='(3a,i)', $
		'Processing CCD Frame #', strtrim(string(ccd_frame),2), $
		' in piled-up photodiode mode at buffer offset ', buffer_pointer

; Read the CCD header information, create header
process_pu_frame_hdr, buffer_pointer, error_code

iframe = iframe + 1
frame_count[iframe] = ccd_frame
;light_curve[iframe] = N_events
light_curve[iframe] = count_rate
read_mode[iframe] = xrt_mode

if (N_events lt 1) then return

pupd_image = uintarr(N_events)
pupd_lightcurve = fltarr(N_events,2)
pupd_lightcurve[*,0] = findgen(N_events)*pixel_time

for i=0L,N_events-1 do begin
    ; Get more data if this row is not contained entirely within the current
    ;	data block
    if (buffer_pointer ge data_buffer_size-npucol*2) then begin
	if (idebug ge 5) then begin
	    print,'PILED_UP_PHOTODIODE: buffer_pointer = ', buffer_pointer, $
			', data_buffer_size = ', data_buffer_size
	    printf,lulog,'PILED_UP_PHOTODIODE: buffer_pointer = ', buffer_pointer, $
			', data_buffer_size = ', data_buffer_size
	endif
	if (append_data_block(buffer_pointer) ne 0) then begin
		if (buffer_pointer ge data_buffer_size-npucol*2) then $
				error_code = 81
	endif
    endif

    pixel = extract_int(buffer_pointer)
    pupd_image[i] = fix((pixel and 'FFF0'X)/16)
    pupd_lightcurve[i,1] = pupd_image[i]
    flags = fix(pixel and '000F'X)
    if (flags ne 8) then begin
	flag_bits = bytarr(4)
	flag_bits[0] = (flags and '8'xb)/8
	flag_bits[1] = (flags and '4'xb)/4
	flag_bits[2] = (flags and '2'xb)/2
	flag_bits[3] = (flags and '1'xb)
	if ((i lt n_pixels-1) and (flag_bits[2] gt 0) and (idebug gt 5)) then begin
	    print_error,0,1,'PILED_UP_PHOTODIODE: invalid EOF flag at index ' $
		+ strtrim(string(i),2) + ' of frame ' $
		+ strtrim(string(ccd_frame),2) + ': ' $ 
		+ ' error is in "row" ' + strtrim(string(i/631),2) $
		+ ', "column" ' + strtrim(string(i mod 631),2)
	endif
	if ((i mod 631) ne 630) then begin
	    print_error,0,1,'PILED_UP_PHOTODIODE: data flags at index ' $
		+ string(i) + ' of frame ' + string(ccd_frame) + ': ' $ 
		+ string(flag_bits,format='(4I1)')
	endif
	if ((error_code eq 81) and (buffer_pointer ge data_buffer_size)) then $
			goto,cleanup
    endif
endfor

cleanup:


; Calculate noise level by fitting Gaussian to histogram function.
spectrum = histogram(pupd_image,MIN=0,MAX=4095)

	; Plot histogram of data
	case show_plot of
		1: print,'Plotting raw spectrum ...'
		2: print,'Calling interactive plotting widget to plot raw spectrum ...'
		else: temp=0
	endcase

	xrng = [0.0, 4095.0]
	p1plotter,show_plot,spectrum,title='Piled-Up Photodiode Histogram for CCD Frame ' + $
		strtrim(string(ccd_frame),2) + ' in file ' + filein,$
		xtitle='DN',ytitle='Counts/bin',/histogram, min=0.5, binsize=1, $
			xrange = xrng
	
	if ((show_plot eq 1) and (print_plots ge 1)) then begin		; send plot to printer
		set_plot,'ps'
		device,/landscape
		num_points = n_elements(spectrum)
		x_data = lindgen(num_points)
		binsize = 1
		minbin = 0.5
		x_data = x_data * binsize + minbin + binsize/2.0
		spectrum = [0, spectrum, 0]
		x_data = [x_data(0)-binsize, x_data, x_data(num_points-1)+binsize]
		psym = 10
		ymin = min(spectrum)
		ymax = max(spectrum)
	   	xrange = [min(x_data),max(x_data)]
		delta = ((ymax - ymin)*0.1/2.0) > 0.1
	        yrange = [ymin-delta, ymax+delta]
		yrange = [0.,100.]

 	     	plot, x_data, spectrum, TITLE='Piled-up Photodiode Histogram for CCD Frame ' + $
			strtrim(string(ccd_frame),2) + ' in file ' + filein, PSYM=psym, $
		    XTITLE='DN', YTITLE='Counts/bin', XRANGE=xrange, YRANGE=yrange
	
		device,/close
		spawn,'lpr -r idl.ps'
		set_plot,'X'
	endif

; Set up CCD spectrum
title = 'XRT Cumulative Pulse Height Spectrum'
xmin = 0
ymax = max(spectrum[50:*])
ixval = where(spectrum gt max([0.015*ymax,2]), count)
if (count gt 0) then begin
	sxval = size(ixval)
	xmax = 1.05*ixval(sxval(1)-1)
endif else xmax = n_elements(spectrum) - 1
ymin = 0
if (xmax gt 30) then ymax = max(spectrum[30:min([xmax,4095])])

; Plot diagnostics

if (iframe ge 1) and (show_plot gt 0) then begin
    old_window = !D.window
    if (n_elements(diagnostic_window) eq 0) then begin
	window,/free,title='CCD Processing Diagnostics'
	diagnostic_window = !D.window
    endif else wset, diagnostic_window

    !P.Multi = [0,1,3]
    ymaxlc = max(light_curve[0:iframe])
    lc = light_curve[0:iframe]>(ymaxlc/1.e6)
    yminlc = min(lc)
;    yminlc = ymaxlc/1.e6
    plot, frame_count[0:iframe], lc, $
	title = 'CCD Light Curve', xtitle='Frame #', $
	ytitle='cps', yrange=[yminlc, ymaxlc], /ylog
    plot, frame_count[0:iframe], read_mode, $
	title = 'CCD Readout Mode (1=Null, 2=Short Image, 3=Long Image, 4=PUPD, 5=LRPD, 6=WT, 7=PC, 8=Raw, 9=Bias', $
	xtitle='Frame #', ytitle='xrt_mode', yrange=[0,10]
    plot, spectrum, xrange=[xmin,xmax], yrange=[ymin,ymax], $
	xtitle='DN', ytitle='# Events', title=title
;    plot, frame_count[0:iframe], baseline[0:iframe], $
;	title = 'CCD1 Mean Baseline (corner pixels)', $
;	xtitle='Frame #', ytitle='DN'
;    plot, frame_count[0:iframe], readnoise[0:iframe], $
;	title = 'CCD1 Readnoise (corner pixels)', xtitle='Frame #', $
;	ytitle='DN (rms)'
endif
!P.Multi = 0	; reset to one plot per page

if (n_elements(old_window) ne 0) then $
	if (old_window(0) ne -1) then wset, old_window

; Calculate data statistics for header
med = median(pupd_lightcurve[*,1])
stats = moment(pupd_lightcurve[*,1])
mean = stats[0]
variance = stats[1]
stdev = sqrt(variance)

sxaddhist,' ', ccd_hdr
sxaddhist,'PASS1 PU-PD Lightcurve Statistics:', ccd_hdr
sxaddpar,ccd_hdr,'MEDIAN',med,'Median value of frame (DN)'
sxaddpar,ccd_hdr,'MEAN',mean,'Mean value of frame (DN)'
sxaddpar,ccd_hdr,'STDEV',stdev,'Standard Deviation of pixel values (DN)'

; Create a FITS file with primary header.
file_lightcurve= filebase + '.frame' + strtrim(string(ccd_frame),2) + '.pupd_lc.fits'
sxaddpar,ccd_hdr,'FILE',file_lightcurve,'Name of this file'

writefits, file_lightcurve, pupd_lightcurve, ccd_hdr
;spawn,'gzip '+file_lightcurve

; recast file as 631 x N image file if even number of rows

if (N_events mod 631UL eq 0) then begin
	file_image = filebase + '.frame' + strtrim(string(ccd_frame),2) + '.pupd.fits'
	number_of_rows = N_events/631UL
	pupd_image = reform(pupd_image,631,number_of_rows)
	writefits, file_image, pupd_image, ccd_hdr
;	spawn,'gzip '+file_image
endif

return
end
