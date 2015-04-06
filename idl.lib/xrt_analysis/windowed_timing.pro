pro windowed_timing, buffer_pointer, error_code
;
;name	: windowed_timing
;author	: Dave Burrows
;date	: 02/09/02
;lang	: IDL

;purpose: This routine unpacks and processes windowed_timing mode CCD frames.
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
;
;  Modifications:
;	04/10/05 by DNB: fixed bug for files with > 32,768 events (wrong 
;		variable type in for loop index).
;	08/18/03 by DNB: fixed label for event type.  Note that in WT mode
;		the event is not classified, so the event type is just WT
;		to indicate that it is not assigned.
;	05/21/03 by DNB: split WT and LRPD events catalogs
;	04/02/03 by DNB: bug fix in print statement
;	04/01/03 by DNB: added debugging print statements
;	09/02/02 by DNB: based on photon_counting.pro
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

pixel = bytarr(4)

cmprssd = 0				; flag for compressed frame

image = intarr(ncol,nrow)			; create image array	

ccd_frame = ccd_frame + 1

print,'Processing CCD Frame # ', strtrim(string(ccd_frame),2), $
		' in windowed timing mode'
printf,lulog,format='(3a,i)', $
		'Processing CCD Frame #', strtrim(string(ccd_frame),2), $
		' in windowed timing mode at buffer offset ', buffer_pointer


process_wt_frame_hdr, buffer_pointer, error_code

; Create a FITS file with primary header.
file_events= filebase + '.frame' + strtrim(string(ccd_frame),2) + '.wt.events.fits'
primary_hdr = ccd_hdr[8:*]

mwrfits, primary_hdu, file_events, primary_hdr, /CREATE

iframe = iframe + 1
frame_count[iframe] = ccd_frame
;light_curve[iframe] = N_events
light_curve[iframe] = count_rate
read_mode[iframe] = xrt_mode

if (N_events lt 1) then return

; Define the data structure for X-ray events
THIS_EVENT = {PIXEL, QUALITY_FLAG:'  ', X_POS:0, Y_POS:0, PIXEL:0.0}

EVENTS = replicate({PIXEL, QUALITY_FLAG:'  ', X_POS:0, Y_POS:0, $
		PIXEL:0.0},N_events)

corners = fltarr(N_events)

; now read the CCD data and put it into the buffer array

for i = 0L, long(N_events-1) do begin
	; first check to see whether we are at the end of the data
	if (buffer_pointer gt (data_buffer_size-4)) then begin
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

	; if there are more than 4 bytes left in the file, unpack the
	;	next X-ray event.  

	pixel = data_buffer(buffer_pointer:buffer_pointer+3)
	buffer_pointer = buffer_pointer + 4	; increment buffer pointer
	
;	; First, get the Event Counter/ID
;	temp = bits(2,1) + bits(3,1)*2 + bits(4,1)*4 + bits(5,1)*8 $
;		+ bits(6,1)*16 + bits(7,1)*32
;	if (temp ne (events_counter+1) mod 64) then begin
;		print_error,0,1,'WINDOWED_TIMING: *** Error in event number ***'
;		print_error,0,1,'WINDOWED_TIMING:   expected event #' $
;			+ strtrim(string((events_counter+1) mod 64),2)
;		print_error,0,1,'WINDOWED_TIMING:   found event #' $
;			+ strtrim(string(temp),2)
;		stop, 'WINDOWED_TIMING: Fatal Error - cannot unpack CCD data'
;	endif
;	events_counter = temp

	events_counter = events_counter + 1

	if ((idebug ge 15) and (i le 20) and (n_calls le 10)) then begin
	    print,''
	    print,'    Event number: ', events_counter
	    print,format="('X-ray event, bytes 1-4:  ', 4z3.2)", pixel
	    printf,lulog,''
	    printf,lulog,'    Event number: ', events_counter
	    printf,lulog,format="('          X-ray event: ', 4z3.2)", pixel
	endif

	; Decode X position (X positions count column 0 as the first active 
	;	(image area) column from the left (A3).)  
	;	The XRT has 5 guard pixels.
	;
	xpos = pixel[0]*4 + (pixel[1]/64)
	THIS_EVENT.X_POS = xpos
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then print,'          xpos, X: ', $
		xpos, ' / ', THIS_EVENT.X_POS
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then printf,lulog, '          xpos, X: ', $
		xpos, ' / ', THIS_EVENT.X_POS

	; Decode Y position (Y positions count from 0)
	ypos = (pixel[1] and '3F'XB)*16 + (pixel[2]/16)
	THIS_EVENT.Y_POS = ypos ;
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then print,'          ypos, Y: ', $
		ypos, ' / ', THIS_EVENT.Y_POS
	if (idebug ge 15 and i le 20 and (n_calls le 10)) then printf,lulog,'          ypos, Y: ', $
		ypos, ' / ', THIS_EVENT.Y_POS

	; Decode pixel value
	this_event.pixel = (pixel[2] and '0F'xb)*256 + pixel[3]

	; Now copy event into events array
	EVENTS[i] = THIS_EVENT

	; Subtract baseline_offset
	EVENTS[i].PIXEL = EVENTS[i].PIXEL ; TBD - baseline_offset

	EVENTS[i].QUALITY_FLAG = 'WT' 	; windowed timing
	

	if ((idebug ge 15) and (i le 20) and (n_calls le 10)) then begin
		print,'          X-ray pixel value: raw and corrected'
		print,format='(10x,i6,10x,i6)', this_event.pixel, $
					EVENTS[i].pixel

	    	printf,lulog,'     X-ray pixel value: raw and corrected'
		printf,lulog,format='(10x,i6,10x,i6)', this_event.pixel,$
							EVENTS[i].pixel
	endif
	
	
	; Now put the event into the image array
	image((EVENTS[i].X_POS),(EVENTS[i].Y_POS)) $
		= image((EVENTS[i].X_POS),(EVENTS[i].Y_POS)) $
			+ EVENTS[i].PIXEL	
endfor
  
End_of_Data_Frame:


PROCESS_FRAME:

; Accumulate spectrum
for i=0L,n_elements(EVENTS)-1 do begin
    ival = round(EVENTS[i].PIXEL)
    if (ival gt 0) then $
		spectrum[ival] = spectrum[ival] + 1
endfor

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
;    plot, frame_count(0:iframe), readnoise[0:iframe], $
;	title = 'CCD1 Readnoise (corner pixels)', xtitle='Frame #', $
;	ytitle='DN (rms)'
endif
!P.Multi = 0	; reset to one plot per page

if (n_elements(old_window) ne 0) then $
	if (old_window(0) ne -1) then wset, old_window

; Write out X-ray events to file
;get_lun,luevent
;file_events = mk_file_name((filebase + '.frame'), ccd_frame, 'phot')
;openw,luevent,file_events
;writeu,luevent,event_header
;for nevent=0,N_events-1 do writeu,luevent,events(nevent)
;free_lun,luevent

if (n_calls lt 20) then begin
	print_debug,12,'WT_WT_WT_WT_WT_WT_WT_WT_WT_WT_WT_WT_WT_WT'
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
			 OFFSET: 0L, $	; used in low rate mode
			 CHIPX: 0, $
			 CHIPY: 0, $
			 RAWX: 0, $
			 RAWY: 0, $
			 DETX: 0, $
			 DETY: 0, $
		 	 FOCX: 0.0, $
			 FOCY: 0.0, $
			 PHA:  0.0 }, N_events )

bin_table.CCDFrame = ccd_frame
bin_table.CCD_ID = 1
bin_table.CCDNODE = amp
bin_table.XRT_MODE = xrt_mode
bin_table.TYPE = events.QUALITY_FLAG
bin_table.RAWX   = events.X_POS
bin_table.RAWY   = events.Y_POS
bin_table.CHIPX   = bin_table.RAWX + 6
bin_table.CHIPY   = bin_table.RAWY
; for PASS1, assume source is centered on CCD
bin_table.TIME   = readout_start_time + (bin_table.CHIPY - 90.0)*row_time $
			- 2.5*15E-6 - 255*1.5E-6 - 100*6.55E-6

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

bin_table.PHA = events.PIXEL
events = 0

; Make events extension header
sxaddpar, events_hdr, 'EXTNAME',  'EVENTS'
sxaddpar, events_hdr, 'TUNIT1', 'Spacecraft time (s)'
sxaddpar, events_hdr, 'TUNIT2', 'CCD Frame Number'
sxaddpar, events_hdr, 'TUNIT3', 'CCD Number'
sxaddpar, events_hdr, 'TUNIT4', 'CCD Amp # (node)'
sxaddpar, events_hdr, 'TUNIT5', 'Readout Mode'
sxaddpar, events_hdr, 'TUNIT6', 'Event Type'
sxaddpar, events_hdr, 'TUNIT7', 'Pixel offset'
sxaddpar, events_hdr, 'TUNIT8', 'On-board readout pixels (0-634)'
sxaddpar, events_hdr, 'TUNIT9', 'On-board readout pixels (0-601)'
sxaddpar, events_hdr, 'TUNIT10', 'Image Area pixels (0-599)'
sxaddpar, events_hdr, 'TUNIT11', 'Image Area pixels (0-601)'
sxaddpar, events_hdr, 'TUNIT12', 'Detector focal plane pixels (1-600)'
sxaddpar, events_hdr, 'TUNIT13', 'Detector focal plane pixels (1-602)'
sxaddpar, events_hdr, 'TUNIT14', 'Focal plane coordinates (mm)'
sxaddpar, events_hdr, 'TUNIT15', 'Focal plane coordinates (mm)'
sxaddpar, events_hdr, 'TUNIT16', 'DN'
sxaddhist, ' ', events_hdr
sxaddhist, '*******************************************', events_hdr
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
sxaddpar, events_hdr, 'DATAMODE', readout_mode, 'XRT CCD in Windowed Timing Mode'
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
if (luwtcatalog lt 0) then begin
  file_wt_catalog = filebase + '.wt.cat' 
	print,format='(a,a)','WINDOWED_TIMING: opening new timing catalog: ', file_wt_catalog
	printf,lulog,format='(a,a)','WINDOWED_TIMING: opening new timing catalog: ', file_wt_catalog
  openw, luwtcatalog, file_wt_catalog, /GET_LUN
endif

printf, luwtcatalog, file_events
printf, lulog, format='(2a)', 'WINDOWED_TIMING: done processing file ', file_events

; Accumulate image
snapshot_image = snapshot_image + image
accumulated_images = accumulated_images + 1

; write CCD image FITS file and display first 2 images
if (idebug gt 9) then printf, lulog,'ccd_frame, first_frame = ', ccd_frame, first_frame
if ((ccd_frame - first_frame) lt 2) then begin
	show_ccd_image = 0
	frame_type = '.wt_image_'
	write_ccd_image
endif


return

end
