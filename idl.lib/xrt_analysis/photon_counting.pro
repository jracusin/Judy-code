pro photon_counting, buffer_pointer, error_code
;
;name	: photon_counting
;author	: Dave Burrows
;date	: 02/09/02
;lang	: IDL

;purpose: This routine unpacks and processes photon-counting mode CCD frames.
;	The data are stored in two structures:
;	1) the CCD Frame Header info is put into a FITS header
;	2) the events are stored as standard FITS event files.
;
;	Output file includes quality flags for each photon, 
;		with values:
;		SG: single pixel event in PC mode
;		SP: split event in PC mode
;		TR: triple pixel event in PC mode
;		QU: quad-pixel event in PC mode
;		WT: windowed timing mode pixel
;		LR: low-rate photodiode timing mode pixel
;
;	It returns an error code which is 0 if no errors are found:
;		error_code	
;		    0	no errors
;		   95	invalid X or Y position - probably indicates bad data
;
;  Modifications:
;   07/16/04 by JLR: fixed bug with reform so would work with IDL6.0
;	06/02/04 by DNB: fixed error checking on xpos and ypos so that program
;		exits gracefully when it hits a bunch of zeroes caused by 
;		download synch problems at end of download.
;	01/14/04 by DNB: fixed bug in first FOR loop when # photons is very large
;	11/04/03 by DNB: added error checking for out-of-range positions.
;		These are typically caused by missing data frames, which 
;		get PASS1 confused.  Need to skip to next header.
;	08/18/03 by DNB: fixed label for event type.
;	04/11/03 by DNB: fixed bug in catalog file name.
;	04/01/03 by DNB: added debugging print statements
;	09/01/02 by DNB: changed definitions of RAWX, RAWY and DETX, DETY
;	08/09/02 by DNB: more modifications for XRT: changed pixel numbering
;		to run from 0-599 over image area
;	02/09/02 by DNB: modified for use with XRT data formats
;	05/27/96 by DNB: Three major modifications:
;		1) Added histogram of corner pixel values, plot the 
;			peak value and width of this histogram.
;		2) Subtract the histogram peak from all data values before
;			writing them out.
;		3) Added standard FITS event file output for compatibility
;			with ASCA and ACIS analysis software.
;		Also:
;		1) Removed roundoff in diagonal element calculation.
;	10/15/96 by DNB: added FOCX, FOCY coordinates, changed definition
;		of DETX, DETY to agree with ASCA, ACIS conventions.
;
;
;

@pass1_common

n_calls = n_calls + 1

error_code = 0
events_counter = 0
Num_single = 0U
Num_split = 0U
Num_triple = 0U
Num_quad = 0U
Num_cr = 0U

pixel = bytarr(16)

cmprssd = 0				; flag for compressed frame

image = intarr(ncol,nrow)			; create image array	

ccd_frame = ccd_frame + 1

print,'Processing CCD Frame # ', strtrim(string(ccd_frame),2), $
		' in photon-counting mode'
printf,lulog,format='(3a,i)', $
		'Processing CCD Frame #', strtrim(string(ccd_frame),2), $
		' in photon-counting mode at buffer offset ', buffer_pointer


process_pc_frame_hdr, buffer_pointer, error_code

; Create a FITS file with primary header.
file_events= filebase + '.frame' + strtrim(string(ccd_frame),2) + '.pc.events.fits'
primary_hdr = ccd_hdr[8:*]

mwrfits, primary_hdu, file_events, primary_hdr, /CREATE

iframe = iframe + 1
frame_count[iframe] = ccd_frame
;light_curve[iframe] = N_events
light_curve[iframe] = count_rate
read_mode[iframe] = xrt_mode

if (N_events lt 1) then return

; Define the data structure for X-ray events
THIS_EVENT = {EVENT, QUALITY_FLAG:'  ', X_POS:0, Y_POS:0, NEIGHBORHOOD:FLTARR(3,3)}

EVENTS = replicate({EVENT, QUALITY_FLAG:'  ', X_POS:0, Y_POS:0, $
		NEIGHBORHOOD:FLTARR(3,3)},N_events)

corners = fltarr(N_events)

; now read the CCD data and put it into the buffer array

if (n_calls lt 20) then begin
	print_debug,12,'PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC'
	print_debug,12,'*****************************************'
endif

for i = 0L, N_events-1 do begin
	; first check to see whether we are at the end of the data
	if (buffer_pointer gt (data_buffer_size-16)) then begin
	    result =  append_data_block(buffer_pointer) 
	    if (result ne 0) then begin
		if (result = 153) then begin	; found EOT
		    found_ID = -1
		    if (buffer_pointer le data_buffer_size - 4) then begin
			ID = extract_long(buffer_pointer) 
			buffer_pointer = buffer_pointer - 4
			goto, End_of_Data_Frame
		    endif else begin
			ID = '4E07'xl
			goto, End_of_Data_Frame
		    endelse
		endif else begin
		    found_ID = -2	; end of input file
		    if (buffer_pointer le data_buffer_size - 4) then begin
			ID = extract_long(buffer_pointer)
			buffer_pointer = buffer_pointer - 4 
		    endif else begin
			ID = 'FFFFFFFF'xl	
			goto, End_of_Data_Frame
		    endelse
		endelse
	    endif
	endif

	; if there are more than 16 bytes left in the file, unpack the
	;	next X-ray event.  

	pixel = data_buffer(buffer_pointer:buffer_pointer+15)
	buffer_pointer = buffer_pointer + 16	; increment buffer pointer
	
;	; First, get the Event Counter/ID
;	temp = bits(2,1) + bits(3,1)*2 + bits(4,1)*4 + bits(5,1)*8 $
;		+ bits(6,1)*16 + bits(7,1)*32
;	if (temp ne (events_counter+1) mod 64) then begin
;		print_error,0,1,'PHOTON_COUNTING: *** Error in event number ***'
;		print_error,0,1,'PHOTON_COUNTING:   expected event #' $
;			+ strtrim(string((events_counter+1) mod 64),2)
;		print_error,0,1,'PHOTON_COUNTING:   found event #' $
;			+ strtrim(string(temp),2)
;		stop, 'PHOTON_COUNTING: Fatal Error - cannot unpack CCD data'
;	endif
;	events_counter = temp

	events_counter = events_counter + 1

	if ((idebug ge 15) and (i le 20) and (n_calls le 10)) then begin
	    print,''
	    print,'    Event number: ', events_counter
	    print,format="('X-ray event, bytes 1-8:  ', 8z3.2)", $
			pixel[0:7]
	    print,format="('X-ray event, bytes 9-16: ', 8z3.2)", $
			pixel[8:15]
	    printf,lulog,''
	    printf,lulog,'    Event number: ', events_counter
	    printf,lulog,format="('          X-ray event: ', 16z3.2)", $
			pixel[0:15]
	endif

	; Decode X position (X positions count column 0 as the first active 
	;		column from the left (A3).)  The XRT has 5 guard pixels.
	;
	xpos = pixel[0]*4 + (pixel[1]/64)
	THIS_EVENT.X_POS = xpos ; 	from CUBIC: + 6
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then print,'          xpos, X: ', $
		xpos, ' / ', THIS_EVENT.X_POS
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then printf,lulog, '          xpos, X: ', $
		xpos, ' / ', THIS_EVENT.X_POS
	if ((xpos le 0) or (xpos ge ncol-2)) then begin
		print_error,0,1,'PHOTON_COUNTING: Invalid X position = ' $
			+ strtrim(string(xpos),2) $
			+ ' for event # ' + string(strtrim(events_counter),2) $
			+ ' of CCD frame ' + strtrim(string(ccd_frame),2)
		print_error,0,1,'PHOTON_COUNTING: Bailing out of this frame ' $
			+ '- rest of frame discarded, returning to PASS1'
		error_code = 95
		goto, process_frame
	endif


	; Decode Y position (Y positions count from 0)
	ypos = (pixel[1] and '3F'XB)*16 + (pixel[2]/16)
	THIS_EVENT.Y_POS = ypos ;	from CUBIC: + 1
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then print,'          ypos, Y: ', $
		ypos, ' / ', THIS_EVENT.Y_POS
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then printf,lulog,'          ypos, Y: ', $
		ypos, ' / ', THIS_EVENT.Y_POS
	if ((ypos le 0) or (ypos ge nrow-2)) then begin
		print_error,0,1,'PHOTON_COUNTING: Invalid Y position = ' $
			+ strtrim(string(ypos),2) $
			+ ' for event # ' + string(strtrim(events_counter),2) $
			+ ' of CCD frame ' + strtrim(string(ccd_frame),2)
		print_error,0,1,'PHOTON_COUNTING: Bailing out of this frame ' $
			+ '- rest of frame discarded, returning to PASS1'
		error_code = 95
		goto, process_frame
	endif

	; Clear event neighborhood
	THIS_EVENT.NEIGHBORHOOD = 0

	; Decode neighborhood
	this_event.neighborhood[0,0] = (pixel[2] and '0F'xb)*256 + pixel[3]
	this_event.neighborhood[1,0] = pixel[4]*16 + pixel[5]/16
	this_event.neighborhood[2,0] = (pixel[5] and '0F'xb)*256 + pixel[6]
	this_event.neighborhood[0,1] = pixel[7]*16 + pixel[8]/16
	this_event.neighborhood[1,1] = (pixel[8] and '0F'xb)*256 + pixel[9]
	this_event.neighborhood[2,1] = pixel[10]*16 + pixel[11]/16
	this_event.neighborhood[0,2] = (pixel[11] and '0F'xb)*256 + pixel[12]
	this_event.neighborhood[1,2] = pixel[13]*16 + pixel[14]/16
	this_event.neighborhood[2,2] = (pixel[14] and '0F'xb)*256 + pixel[15]

	; Now copy event into events array
	EVENTS[i] = THIS_EVENT

	; Subtract baseline_offset
	EVENTS[i].NEIGHBORHOOD = EVENTS[i].NEIGHBORHOOD - baseline_offset

	; Accumulate average diagonal corner values for histogram
	n_corners = 0
	corners[i] = 0.0
	if (events[i].neighborhood[0,0] lt split_threshold) then begin
		corners[i] = corners[i] + events[i].neighborhood[0,0]
		n_corners = n_corners + 1
	endif
	if (events[i].neighborhood[2,0] lt split_threshold) then begin
		corners[i] = corners[i] + events[i].neighborhood[2,0]
		n_corners = n_corners + 1
	endif
	if (events[i].neighborhood[0,2] lt split_threshold) then begin
		corners[i] = corners[i] + events[i].neighborhood[0,2]
		n_corners = n_corners + 1
	endif
	if (events[i].neighborhood[2,2] lt split_threshold) then begin
		corners[i] = corners[i] + events[i].neighborhood[2,2]
		n_corners = n_corners + 1
	endif
	
	if (n_corners gt 0) then corners[i] = corners[i]/n_corners

	; Get number of pixels above threshold
	dummy = where(events[i].neighborhood ge split_threshold, N_pix)

	case N_pix of
		0: begin
			EVENTS[i].QUALITY_FLAG = 'SG' 	; single event
			Num_single = Num_single + 1
		   end
		1: begin
			EVENTS[i].QUALITY_FLAG = 'SG' 	; single event
			Num_single = Num_single + 1
		   end
		2: begin
			EVENTS[i].QUALITY_FLAG = 'SP'	; singly split event
			Num_split = Num_split + 1
			; Accumulate split types for single splits
			for j=0,2 do begin
			    for k=0,2 do begin
				if (events[i].neighborhood[j,k] gt split_threshold) then  $
					split_type[j,k] = split_type[j,k] + 1
			    endfor
			endfor
		   end
		3: begin
			EVENTS[i].QUALITY_FLAG = 'TR'	; Triply split event
			Num_triple = Num_triple + 1
		   end
		4: begin
			EVENTS(I).QUALITY_FLAG = 'QU'	; Quad event
			Num_quad = Num_quad + 1
		   end
		else: begin
			EVENTS(I).QUALITY_FLAG = 'CR'	; Quad event
			Num_cr = Num_cr + 1			
		     end
	endcase
	

	if ((idebug ge 15) and (i le 20) and (n_calls le 10)) then begin
		print,'          X-ray neighborhood: raw and corrected'
		print,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,2], $
							EVENTS[i].NEIGHBORHOOD[0:2,2]
		print,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,1], $
							EVENTS[i].NEIGHBORHOOD[0:2,1]
		print,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,0],$
							EVENTS[i].NEIGHBORHOOD[0:2,0]
	    	printf,lulog,'     X-ray neighborhood: raw and corrected'
		printf,lulog,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,2],$
							EVENTS[i].NEIGHBORHOOD[0:2,2]
		printf,lulog,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,1],$
							EVENTS[i].NEIGHBORHOOD[0:2,1]
		printf,lulog,format='(10x,3i6,10x,3i6)', this_event.neighborhood[0:2,0],$
							EVENTS[i].NEIGHBORHOOD[0:2,0]
	endif
	
	
	; Now put the event into the image array
	image((EVENTS[i].X_POS-1):(EVENTS[i].X_POS+1),(EVENTS[i].Y_POS-1):(EVENTS[i].Y_POS+1)) $
	= image((EVENTS[i].X_POS-1):(EVENTS[i].X_POS+1),(EVENTS[i].Y_POS-1):(EVENTS[i].Y_POS+1)) $
			+ EVENTS[i].NEIGHBORHOOD
	
endfor
  
End_of_Data_Frame:


PROCESS_FRAME:
; Did the event counts come out right?

if (Num_single ne N_single) then begin
	print_error,0,verbosity,' '
	print_error,0,verbosity,'***** PHOTON_COUNTING: CCD Frame ' $
		+ strtrim(string(ccd_frame),2)
	print_error,0,verbosity, $
		'***** PHOTON_COUNTING: incorrect single event count *****'
	print_error,0,verbosity, '***** PHOTON_COUNTING: header reports ' $
		+ strtrim(string(N_single),2) + ' single events.'
	print_error,0,verbosity, '***** PHOTON_COUNTING: Found ' $
		+ strtrim(string(Num_single),2) + ' single events in file.'
endif

if (Num_split ne N_split) then begin
	print_error,0,verbosity,' '
	print_error,0,verbosity,'***** PHOTON_COUNTING: CCD Frame ' $
		+ strtrim(string(ccd_frame),2)
	print_error,0,verbosity,$
		'***** PHOTON_COUNTING: incorrect split event count *****'
	print_error,0,verbosity, '***** PHOTON_COUNTING: header reports ' $
		+ strtrim(string(N_split),2) + ' split events.'
	print_error,0,verbosity, '***** PHOTON_COUNTING: Found ' $
		+ strtrim(string(Num_split),2) + ' split events in file.'
endif

; Calculate baseline from diagonal pixels:

; 1) Accumulate data for cumulative histogram:
dummy = cumulative_hist
cumulative_hist = histogram(corners, min=-100.0, max=200.0, input=dummy)

;; 2) Fit histogram to Gaussian
;;	NOTE: GAUSSFIT actually does the wrong problem - it should be
;;		replaced by a function that does a nonlinear regression
;;		to differences of the integral Gaussian function in order
;;		to correctly fit the histogram results.  (See FORTRAN
;;		GAUSSFIT program).
;yfit = gaussfit(dn, corner_hist, a, estimates=[10.0,0.0,10.0],nterms=3)
;baseline[iframe] = A[1]
;readnoise[iframe] = A[2]
;print,' '
;print,'Corner Pixels: Baseline = ', a[1],' DN;   Readnoise = ', a[2], ' DN '
;print,''
;
;printf,lulog,' '
;printf,lulog,'Corner Pixels: Baseline = ', a[1],' DN;   Readnoise = ', a[2], ' DN '
;printf,lulog,''
;
;; 3) Generate plot
;old_window = !D.window
;if (n_elements(corner_window) eq 0) then begin
;	window,/free,title='Corner Pixel Histogram', ysize=300
;	corner_window = !D.window		; save window number
;endif else wset, corner_window
;
;title='CCD Corner Pixel Histogram for frame #' + strtrim(string(ccd_frame),2)
;plot, dn, corner_hist, title=title, psym=10
;oplot, dn, yfit
;ylabel1 = max(corner_hist)/5.0
;ylabel2 = max(corner_hist)/10.0
;xyouts,dn(10), ylabel1, 'Mean: ' + strtrim(string(a(1)),2) + ' DN'
;xyouts,dn(10), ylabel2, 'Sigma: ' + strtrim(string(a(2)),2) + ' DN'
;if (n_elements(old_window) ne 0) then $
;	if (old_window(0) ne -1) then wset, old_window
;
;
;; Subtract baseline from events
;if ((A(1) ge min(corners)) and (A(1) le max(corners))) then begin
;	case baseline_correction_mode of
;		0: events.neighborhood = events.neighborhood
;		1: EVENTS.NEIGHBORHOOD = EVENTS.NEIGHBORHOOD - A(1)
;	endcase
;endif

for i=0L,n_elements(EVENTS)-1 do begin
    ; Accumulate split charge
    split_charge = split_charge + events[i].neighborhood

    ; Accumulate event spectrum
    ival = round(total(EVENTS[i].NEIGHBORHOOD(where(EVENTS[i].NEIGHBORHOOD gt -10))))
    if ((ival gt 0)and(ival lt 4096)) then $
		spectrum[ival] = spectrum[ival] + 1
endfor


; Set up CCD spectrum
title = 'XRT Cumulative Single + Split Pulse Height Spectrum'
xmin = 0
ymax = max(spectrum[50:*])
ixval = where(spectrum gt max([0.015*ymax,2]), count)
if (count gt 0) then begin
	sxval = size(ixval)
	xmax = ixval(sxval(1)-1)
endif else xmax = n_elements(spectrum) - 1
ymin = 0
ymax = max(spectrum[30:min([xmax,4095])])

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
;    plot, frame_count(0:iframe), baseline(0:iframe), $
;	title = 'CCD1 Mean Baseline (corner pixels)', $
;	xtitle='Frame #', ytitle='DN'
;   plot, frame_count(0:iframe), readnoise[0:iframe], $
;	title = 'CCD1 Readnoise (corner pixels)', xtitle='Frame #', $
;	ytitle='DN (rms)'
endif

!P.Multi = 0	; reset to one plot per page
if (n_elements(old_window) ne 0) then $
	if (old_window(0) ne -1) then wset, old_window


if (n_calls lt 20) then begin
	print_debug,12,'PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC_PC'
	print_debug,12,'*****************************************'
endif

;; Write event list in FITS format.


;; Write the events to a FITS binary table.  
;; The CCD has a CHIPX/CHIPY coordinate system used in the flight software which
;;	includes guard pixels and overclocks (see Appendix A of XRT-PSU-028 for diagram)
;;	and a RAWX/RAWY coordinate pixel system which includes only the image array.  
;;	Both of these start with pixel (0,0).
;; The focal plane has a DETX/DETY coordinate system in pixel units, 
;; 	plus a FOCX/FOCY coordinate system giving the event positions in units of
;;	millimeters in the focal plane relative to the chip center.
;;	SEE XRT-PSU-037 for complete description of these coordinate systems.

bin_table = replicate( { TIME: 0D, $
			 CCDFrame: 0L, $
			 CCD_ID: 0B, $
			 CCDNODE: 0B, $
			 XRT_MODE: 0B, $
			 TYPE: '  ', $
			 CHIPX: 0, $
			 CHIPY: 0, $
			 RAWX: 0, $
			 RAWY: 0, $
			 DETX: 0, $
			 DETY: 0, $
		 	 FOCX: 0.0, $
			 FOCY: 0.0, $
			 PHAS:  fltarr(9) }, N_events )

bin_table.TIME   = (frame_start_time + frame_stop_time) / 2.0
bin_table.CCDFrame = ccd_frame
bin_table.CCD_ID = 1
bin_table.CCDNODE = amp
bin_table.XRT_MODE = xrt_mode
bin_table.TYPE = events.QUALITY_FLAG
bin_table.RAWX   = events.X_POS
bin_table.RAWY   = events.Y_POS
bin_table.CHIPX   = bin_table.RAWX + 6
bin_table.CHIPY   = bin_table.RAWY

case amp of
	1: begin	
		    bin_table.DETX   = bin_table.RAWX + 1
		    bin_table.DETY   = bin_table.RAWY + 1
	   end
	2: begin
		    bin_table.DETX   = 600 - bin_table.RAWX
		    bin_table.DETY   = bin_table.RAWY + 1
	   end
endcase

K = 0.0400  ; pixel scale, in mm per pixel, exactly!
A =  -300.5*K; (mm)	; pixel offset to pixel #1
B =  -300.5*K; (mm)	; pixel offset to pixel #1

bin_table.FOCX   = A + (K * bin_table.DETX) 
bin_table.FOCY   = B + (K * bin_table.DETY)

;;; The PHAS FITS column represents a 3x3 neighborhood around each event
;;; as a 9-element vector.  The pixels are stored in the order shown below
;;; to agree with the ASCA format, which was provided in a private
;;; communication with Ken Ebisawa.  I cannot find a document that says this.
;;;                  ^
;;;                | 6 7 8
;;;    RAWY=ccdRow | 4 0 5
;;;                | 1 2 3
;;;                |----------->
;;;                 RAWX=ccdColumn

neighborhood = events.NEIGHBORHOOD
events = 0
bin_table[*].PHAS(0) = reform( /OVERWRITE, neighborhood(1,1,*) )
bin_table[*].PHAS(1) = reform( /OVERWRITE, neighborhood(0,0,*) )
bin_table[*].PHAS(2) = reform( /OVERWRITE, neighborhood(1,0,*) )
bin_table[*].PHAS(3) = reform( /OVERWRITE, neighborhood(2,0,*) )
bin_table[*].PHAS(4) = reform( /OVERWRITE, neighborhood(0,1,*) )
bin_table[*].PHAS(5) = reform( /OVERWRITE, neighborhood(2,1,*) )
bin_table[*].PHAS(6) = reform( /OVERWRITE, neighborhood(0,2,*) )
bin_table[*].PHAS(7) = reform( /OVERWRITE, neighborhood(1,2,*) )
bin_table[*].PHAS(8) = reform( /OVERWRITE, neighborhood(2,2,*) )
neighborhood = 0

; Make events extension header
sxaddpar, events_hdr, 'EXTNAME',  'EVENTS'
sxaddpar, events_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'
sxaddpar, events_hdr, 'TUNIT1', 'Spacecraft time (s)'
sxaddpar, events_hdr, 'TUNIT2', 'CCD Frame Number'
sxaddpar, events_hdr, 'TUNIT3', 'CCD Number'
sxaddpar, events_hdr, 'TUNIT4', 'CCD Amp # (node)'
sxaddpar, events_hdr, 'TUNIT5', 'Readout Mode'
sxaddpar, events_hdr, 'TUNIT6', 'Event Type'
sxaddpar, events_hdr, 'TUNIT7', 'On-board readout pixels (0-634)'
sxaddpar, events_hdr, 'TUNIT8', 'On-board readout pixels (0-601)'
sxaddpar, events_hdr, 'TUNIT9', 'Image Area pixels (0-599)'
sxaddpar, events_hdr, 'TUNIT10', 'Image Area pixels (0-601)'
sxaddpar, events_hdr, 'TUNIT11', 'Focal plane pixels (1-600)'
sxaddpar, events_hdr, 'TUNIT12', 'Focal plane pixels (1-602)'
sxaddpar, events_hdr, 'TUNIT13', 'Focal plane coords (mm)'
sxaddpar, events_hdr, 'TUNIT14', 'Focal plane coords (mm)'
sxaddpar, events_hdr, 'TUNIT15', 'DN'
sxaddhist, ' ', events_hdr
sxaddhist, '********************************************', events_hdr
sxaddhist,'XRT CCD Frame processed by PASS1 '+version, $
			events_hdr
sxaddpar,events_hdr,'ORIGIN','PSU X-ray Astronomy', $
			'Data from Penn State X-ray Astronomy'
sxaddpar, events_hdr, 'TELESCOP', 'Swift', 'Swift Gamma-Ray Burst Explorer'
sxaddpar, events_hdr,'INSTRUME','XRT',$
			'XRT Instrument on Swift Gamma-ray Burst Explorer'
sxaddhist, ' ', events_hdr
sxaddpar, events_hdr, 'OBS_NUM', strtrim(string(observation_number),2), 'Swift Observation Number'
sxaddpar, events_hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
sxaddpar, events_hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
sxaddpar, events_hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
sxaddhist, ' ', events_hdr
sxaddpar, events_hdr, 'OBS_MODE', obs_mode
sxaddpar, events_hdr, 'DATAMODE', readout_mode, 'XRT CCD in Photon-Counting Mode'
sxaddpar, events_hdr, 'BSLNOFST', baseline_offset, 'Baseline_offset (DN) before correction to true DN'
sxaddhist, ' ', events_hdr
sxaddhist, ' ', events_hdr
sxaddpar, events_hdr, 'TIMEREF', 'LOCAL', 'No barycentric correction'
;sxaddpar, events_hdr, 'TIMESYS', '1968.xxxxxxxxxx', 'Uses ITOS epoch'
sxaddpar, events_hdr, 'TIMEUNIT', 's'
sxaddpar, events_hdr, 'CLOCKAPP', 'F', 'Mission time not corrected for clock drift'
sxaddpar, events_hdr, 'TASSIGN', 'SATELLITE', 'location of time assignment'
sxaddpar, events_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'
sxaddpar, events_hdr, 'MET_STRT', readout_start_time,'Satellite time of readout start'
sxaddpar, events_hdr, 'MET_STOP', readout_stop_time,'Satellite time of readout end'
sxaddpar, events_hdr, 'TSTART', frame_start_time,'Satellite time of exposure start'
sxaddpar, events_hdr, 'TSTOP', frame_stop_time,'Satellite time of exposure end'
sxaddpar, events_hdr, 'TELAPSE', frame_stop_time - frame_start_time, 'Elapsed time'
sxaddpar, events_hdr, 'ONTIME', exposure_time,'Total good time for this frame'
sxaddpar, events_hdr, 'EXPOSURE', exposure_time,'CCD Exposure time in seconds'
sxaddpar, events_hdr, 'LIVETIME', livetime, 'Livetime for this frame'

; Write events extension table to file
mwrfits, bin_table, file_events, events_hdr

bin_table = 0

;; Write an entry in the catalog file.
if (lupccatalog lt 0) then begin
  file_pc_catalog = filebase + '.pc.cat' 
	print,format='(a,a)','PHOTON_COUNTING: opening new timing catalog: ', file_pc_catalog
	printf,lulog,format='(a,a)','PHOTON_COUNTING: opening new timing catalog: ', file_pc_catalog
  openw, lupccatalog, file_pc_catalog, /GET_LUN
endif

printf, lupccatalog, file_events
printf, lulog, format='(2a)', 'PHOTON_COUNTING: done processing file ', file_events

; Accumulate image
snapshot_image = snapshot_image + image
accumulated_images = accumulated_images + 1

; write CCD image FITS file and display first 2 images
if ((ccd_frame - first_frame) le 2) then begin
	show_ccd_image = 0
	frame_type = '.pc_image_'
	write_ccd_image
endif


return

end
