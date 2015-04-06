pro bias_map, buffer_pointer, error_code
; $Id: bias_map.pro 1.0 2001/12/21 17:25:30 burrows Exp $

;name	: bias_map
;author	: Dave Burrows
;date	: 01/17/95
;lang	: IDL
;
;purpose: This routine processes XRT bias map frames.  
;
;	It returns an error code which is 0 if no errors are found:
;	error_code	
;	    0	no errors
;	   81	EOF encountered before end of data frame
;
; Rev:
;	01/16/04 by DNB: improved error condition handling when EOF found in bias frame
;	09/03/03 by DNB: fixed bug with FWHM printout (wasn't converting from stdev to fwhm)
;	06/10/03 by DNB: fixed major bug that was trashing following data
;		(buffer_pointer was being incremented incorrectly).  Also
;		eliminated intermediate scratch file - should speed up
;		processing somewhat.
;	06/04/03 by DNB: changed final close,luscratch to free_lun,luscratch
;	05/21/03 by DNB: fixed to re-use bias image window
;	05/13/03 by DNB: changed ccd_frame to bias_frame
;	03/27/03 by DNB: changed "spectrum" to "bias_spectrum" to avoid
;		contention with other routines over spectrum dimensions
;	09/10/02 by DNB: based on ccd_raw_frame.pro
;	05/20/02 by DNB: changed scaling on plots
;	04/11/02 by DNB to add more error information when pixels are missing.
;	04/01/02 by DNB to fix histogram scaling.
;		Also added noise calculation.
;	01/26/02 by DNB to generate histogram of raw data frame.
;;
;

@pass1_common
common show_bias, show_b
if n_elements(show_b) eq 0 then show_b = -1		;No display window yet

n_calls = n_calls + 1

error_code = 0
cmprssd = 0				; flag for compressed frame

print,'Processing XRT Bias Map'
printf,lulog,'Processing XRT Bias Map'

image = intarr(ncol,nrow)

bias_frame = bias_frame + 1

; Read the CCD header information, create header
process_bias_frame_hdr, buffer_pointer, error_code
if (error_code eq 91) then begin
	print_error,0,1,'Found EOF in bias frame header - no bias frame produced'
	return
endif

ix0 = (600-bias_ncol)/2
iy0 = (600-bias_nrow)/2
for j=0,bias_nrow-1 do begin
    ; Get more data if this row is not contained entirely within the current
    ;	data block
    if (buffer_pointer ge data_buffer_size-bias_ncol*2) then begin
	if (idebug ge 5) then begin
	    print,'BIAS_MAP: buffer_pointer = ', buffer_pointer, $
			', data_buffer_size = ', data_buffer_size
	    printf,lulog,'BIAS_MAP: buffer_pointer = ', buffer_pointer, $
			', data_buffer_size = ', data_buffer_size
	endif
	if (append_data_block(buffer_pointer) ne 0) then begin
		if (buffer_pointer ge data_buffer_size-bias_ncol*2) then begin
			error_code = 81
			print_error,0,1,'Found EOF in bias frame data - no bias frame produced'
			return
		endif	
	endif
    endif

    for i=0,bias_ncol-1 do begin
	image[i+ix0,j+iy0] = extract_int(buffer_pointer)
	if ((error_code eq 81) and (buffer_pointer ge data_buffer_size)) then $
			goto,cleanup
    endfor
endfor



cleanup:

; Show image on screen
;if (show_ccd_image eq 1) then pass1_show_image

;; Since PASS1 uses the Penn State display widget programs, it must adhere
;; to the color table protocol used by those programs.  Thus we must
;; TV images using only the lower portion of the color table.
color_manager, NCOLORS=ncolors

if (show_plot gt 0) then begin 
; set up new image display window if needed
syz = size(image)
xsize = syz(1) < max_xsize
ysize = syz(2) < max_ysize
old_window = !D.window
if (show_b lt 0) then begin	; Make new window?
	window,/free,xsize=xsize,ysize=ysize,title='XRT Bias Image'
	show_b = !d.window	; save window number for future use
endif else begin
	wset,show_b		; reset to image display window
;	erase			;Erase it
endelse

tvscl,image
if (n_elements(old_window) ne 0) then $
	if (old_window(0) ne -1) then wset, old_window
endif

; Calculate noise level by fitting Gaussian to histogram function.
bias_spectrum = histogram(image,MIN=-100.0,MAX=900.0)

;spec_min = min(image)
;temp = max(bias_spectrum,spec_max)
;baseline_max = min([(spec_min + (fix(spec_max)-spec_min)*3),4095])
;x = indgen(n_elements(bias_spectrum))
;x = x[spec_min:baseline_max]
x = indgen(1003) - 101.0 + 0.5

;y = bias_spectrum[spec_min:baseline_max]
y = [0,bias_spectrum,0]

if (n_elements(x) gt 3) then begin
	yfit = gaussfit(x,y,a,NTERMS=3)
	print,''
	print,'BIAS_MAP: Results from Gaussian fit to noise peak:'
	print,'BIAS_MAP:      Mean bias value:    ', a(1), ' DN'
	print,'BIAS_MAP:      Bias width (noise): ', a(2), ' DN (rms)'
	print,'BIAS_MAP:                                  =', a(2)*2.354, ' DN (FWHM)'
	print,''
	printf,lulog,''
	printf,lulog,'BIAS_MAP: Results from Gaussian fit to noise peak:'
	printf,lulog,'BIAS_MAP:      Mean bias value:    ', a(1), ' DN'
	printf,lulog,'BIAS_MAP:      Bias width (noise): ', a(2), ' DN (rms)'
	printf,lulog,'BIAS_MAP:                    (for gain=2.52 eV/DN)= ', a(2)*2.52/3.65, ' e- (rms)'
	printf,lulog,'BIAS_MAP:                                  =', a(2)*2.354, ' DN (FWHM)'
	printf,lulog,'BIAS_MAP:                    (for gain=2.52 eV/DN)= ', a(2)*2.354*2.52, ' eV (FWHM)'
	printf,lulog,''

	; Plot histogram of image
	case show_plot of
		1: print,'Plotting raw spectrum ...'
		2: print,'Calling interactive plotting widget to plot raw spectrum ...'
		else: temp=0
	endcase

;	xrng = [-100., 200.]
;	yrng = [0.0, 100.0]
	xrng = [min(x),max(x)]
	yrng = [0.1,max(y)]
	if (pix_gen) then begin
		xrng = [a(1)-30, a(1)+30]
		yrng = [0.,max(bias_spectrum)]
	endif
;	p1plotter,show_plot,x,y,title='Bias Map Histogram for CCD Frame ' + $
;		strtrim(string(bias_frame),2) + ' in file ' + filein,$
;		xtitle='DN',ytitle='Counts/bin',/histogram, min=0.5, binsize=1, $
;			xrange = xrng, yrange=yrng

	if (show_plot gt 0) then begin 
	old_window = !D.window
	if (n_elements(spectrum_window) eq 0) then begin
		window,/free,title='Bias Map Histogram'
		spectrum_window = !D.window
	endif else wset, spectrum_window
	if (show_plot gt 0) then begin 
		plot,x,y,title='Bias Map Histogram for CCD Frame ' + $
			strtrim(string(bias_frame),2) + ' in file ' + filein,$
			xtitle='DN',ytitle='Counts/bin', /ylog, psym=10, $
			xrange=xrng, yrange=yrng
	
	if (n_elements(old_window) ne 0) then $
		if (old_window(0) ne -1) then wset, old_window
	endif
	endif 

	if ((show_plot eq 1) and (print_plots ge 1)) then begin		; send plot to printer
		set_plot,'ps'
		device,/landscape
		num_points = n_elements(bias_spectrum)
		x_data = lindgen(num_points)
		binsize = 1
		minbin = 0.5
		x_data = x_data * binsize + minbin + binsize/2.0
		bias_spectrum = [0, bias_spectrum, 0]
		x_data = [x_data(0)-binsize, x_data, x_data(num_points-1)+binsize]
		psym = 10
		ymin = min(bias_spectrum)
		ymax = max(bias_spectrum)
	   	xrange = [min(x_data),max(x_data)]
		delta = ((ymax - ymin)*0.1/2.0) > 0.1
	        yrange = [ymin-delta, ymax+delta]
		yrange = [0.,100.]

 	     	plot, x_data, bias_spectrum, TITLE='Bias Map Histogram for CCD Frame ' + $
			strtrim(string(bias_frame),2) + ' in file ' + filein, PSYM=psym, $
		    XTITLE='DN', YTITLE='Counts/bin', XRANGE=xrange, YRANGE=yrange
	
		device,/close
		spawn,'lpr -r idl.ps'
		set_plot,'X'
	endif
endif

; Calculate image statistics for header
med = median(image)
stats = moment(image)
mean = stats[0]
variance = stats[1]
stdev = sqrt(variance)
	print,''
	print,'BIAS_MAP: Median value of bias map is ', med, ' DN'
	print,'BIAS_MAP: Mean value of bias map is   ', mean, ' DN'
	print,'BIAS_MAP: Standard Deviation of bias map =     ', stdev, ' DN (rms)'
	print,'BIAS_MAP:                                =     ', stdev*2.354, ' DN (FWHM)'
	print,''
	printf,lulog,''
	printf,lulog,'BIAS_MAP: Median value of bias map is ', med, ' DN'
	printf,lulog,'BIAS_MAP: Mean value of bias map is   ', mean, ' DN'
	printf,lulog,'BIAS_MAP: Standard Deviation of bias map =     ', stdev, ' DN (rms)'
	printf,lulog,'BIAS_MAP:                                =     ', stdev*2.354, ' DN (FWHM)'
	printf,lulog,''

LLD = med + 3.0*stdev
ULD = 4094
over_ULD = where(image[3:bias_ncol-5,*] gt ULD, N_pixels_over_ULD)
over_LLD = where(image[3:bias_ncol-5,*] gt LLD, N_pixels_over_LLD)
N_events = N_pixels_over_LLD - N_pixels_over_ULD

sxaddhist,' ',ccd_hdr
sxaddhist,'PASS1 PIXEL PROCESSING RESULTS:',ccd_hdr
sxaddpar,ccd_hdr,'MEDIAN',med,'Median value of image (DN)'
sxaddpar,ccd_hdr,'MEAN',mean,'Mean value of image (DN)'
sxaddpar,ccd_hdr,'STDEV',stdev,'Standard Deviation of pixel values (DN)'
sxaddpar,ccd_hdr,'ULD',ULD,'Upper Level Discriminator (DN)'
sxaddpar,ccd_hdr,'LLD',LLD,'Event threshold (Lower Level Discriminator)'
sxaddpar,ccd_hdr,'SPLIT',4096,'Split Event threshold (DN)'
sxaddpar,ccd_hdr,'N_GT_LLD',N_pixels_over_LLD,'Number of pixels over LLD'
sxaddpar,ccd_hdr,'N_GT_ULD',N_pixels_over_ULD,'Number of pixels over ULD'
sxaddpar,ccd_hdr,'N_EVENTS',N_events,'Number of pixels between LLD and ULD'

; Store old file name base
old_filebase = filebase

; Set file name
frame_type = '.bias_map_'

write_ccd_image

filebase = old_filebase

;buffer_pointer = buffer_pointer - 2 ; TBD: temporary fix for missing pixel

return
end
