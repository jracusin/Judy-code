pro process_lr_frame_hdr, buffer_pointer, error_code
; $Id: process_lr_frame_hdr.pro 0.0 1995/12/21 17:25:30 burrows Exp $

;name	: process_lr_frame_hdr
;author	: Dave Burrows
;date	: 02/10/02
;lang	: IDL

;purpose: This routine processes the XRT CCD Frame Header for 
;	Low-Rate Photodiode mode.  It extracts
;	data from this header for the output FITS header and writes the
;	housekeeping data out to a temporary HK data file that is read
;	and processed at the end of the run.
;	It returns an error code which is 0 if no errors are found:
;	error_code	
;	    0	no errors
;	   91	PROCESS_LR_FRAME_HDR: EOF encountered before end of CCD header
;
;   Rev:
;	08/10/04 by DNB: minor changes in header formats
;	07/29/04 by DNB: fixed header format for Build 8.8 (added more bias info)
;	01/22/04 by DNB: changed size of header in buffer size check so it
;		doesn't bomb out when it gets to EOF.
;	04/08/03 by DNB: resynchronized frame numbers
;	03/27/03 by DNB: dropped synchronization between on-board and internal CCD frame number
;	09/04/02 by DNB: created from process_wt_frame_hdr.pro
;	09/03/02 by DNB: updated FITS keywords to match SDC version better.
;	08/12/02 by DNB: changed from photon-counting version to windowed
;		timing mode.
;	08/09/02 by DNB: massive changes to get routine to work correctly
;		for XRT.  Debugged using photon-counting data generated with
;		fake (simulated) CCD inputs.
;	04/07/02 by DNB: changed HK array to unsigned integer, deleted last
;		four time words.
;;
;

@pass1_common

common save_data,oldwaveform,n_frames	; saves this for next entry

; "declare" variable types
error_code = 0

if (n_elements(n_frames) eq 0) then n_frames = 0	; initialize
n_frames = n_frames + 1

; Get more data if this frame header is not contained entirely within the current
;	data block
if (buffer_pointer ge (data_buffer_size-218+20)) then begin
	print,''
	if (append_data_block(buffer_pointer) ne 0) then begin
		error_code = 91
	endif
endif

; Read CCD Frame number
frame_num = extract_long(buffer_pointer)

if (n_frames lt 20) then begin
	print_debug,12,'*****************************************'
	print_debug,12,'LR_LR_LR_LR_LR_LR_LR_LR_LR_LR_LR_LR_LR_LR'
	print_debug,12,'PROCESS_LR_FRAME_HDR: Processing Low-Rate Photodiode Mode Frame Hdr ' $
		+ 'for CCD Frame #' + strtrim(string(frame_num),2)
endif

if (ccd_frame eq 1) then begin
	first_frame = frame_num
endif 
if (idebug gt 9) then printf,lulog,'ccd_frame, frame_num, first_frame = ', ccd_frame, frame_num, first_frame
if (frame_num ne ccd_frame) then begin
    if (ccd_frame ne 1) then begin
	print_error,0,1,'PROCESS_LR_FRAME_HDR: ERROR in CCD Frame #: found '+ $
		strtrim(string(frame_num),2)+', expected '+ $
		strtrim(string(ccd_frame),2)
    endif
    ccd_frame = frame_num		; synchronize the counters
endif


; Get CCD header info and check for format errors
; TBD: Add error checking for following parameters - compare to disk file
;	with nominal values

observation_number = extract_long(buffer_pointer)
obs_segment = (observation_number and 'FF000000'xl) / (256L*256L*256L)
target_id = (observation_number and '00FFFFFF'xl)
seq_num = target_id*1000L + obs_segment

ra = extract_float(buffer_pointer)
dec = extract_float(buffer_pointer)
roll = extract_float(buffer_pointer)

if (n_frames lt 20) then print_debug,12,'     RA = ' + strtrim(string(ra),2) + ', Dec = ' $
	+ strtrim(string(dec),2) + ', Roll = ' + strtrim(string(roll),2)

acs_flags = bytarr(4)
acs_flag_word = extract_byte(buffer_pointer)
obs_mode = 'SLEW'
if ((acs_flag_word and '08'x) eq 8) then begin
	safe_mode = 'T' 
	acs_flags[3] = 1
endif else safe_mode = 'F'
if ((acs_flag_word and '04'x) eq 4) then begin
	in_saa = 'T' 
	acs_flags[2] = 1
endif else in_saa = 'F'
if ((acs_flag_word and '02'x) eq 2) then begin
	is_in_10_arcmin = 'T' 
	acs_flags[1] = 1
endif else is_in_10_arcmin = 'F'
if ((acs_flag_word and '01'x) eq 1) then begin
	is_settled = 'T' 
	acs_flags[0] = 1
	obs_mode = 'POINTING'
endif else is_settled = 'F'

if (n_frames lt 20) then print_debug,12,'     ACS Flags: IS_SETTLED = ' + is_settled $
	+ ', IS_IN_10_ARCMIN = ' + is_in_10_arcmin $
	+ ', IS_IN_SAA = ' + in_saa + ', SAFE_MODE = ' + safe_mode

xrt_state = extract_byte(buffer_pointer)
case xrt_state of
	'11'x: state = 'Auto'
	'22'x: state = 'Manual'
	'44'x: state = 'RED'
	else: state = 'Undefined'
endcase

xrt_mode = extract_byte(buffer_pointer)
case xrt_mode of
	1: readout_mode = 'Null'
	2: readout_mode = 'SHORTIMAGE'
	3: readout_mode = 'LONG IMAGE'
	4: readout_mode = 'PUPD'
	5: readout_mode = 'LRPD'
	6: readout_mode = 'WT'
	7: readout_mode = 'PC'
	8: readout_mode = 'RAW'
	9: readout_mode = 'BIAS'
	10: readout_mode = 'Stop'
	else: readout_mode = 'Undefined'
endcase

if (n_frames lt 20) then print_debug,12,'     XRT_STATE = ' + state + ',   XRT_MODE = ' + readout_mode

if (xrt_mode ne 5) then print_error,0,1,'PROCESS_LR_FRAME_HDR: wrong xrt_mode found for Low-Rate Photodiode mode: mode flag is ' + strtrim(string(xrt_mode),2)

waveform = fix(extract_byte(buffer_pointer))
if ((waveform lt 0) or (waveform gt 255)) then begin
	if (n_elements(oldwaveform) eq 0) then begin
		print,''
		print,'PROCESS_LR_FRAME_HDR: *** INVALID WAVEFORM NUMBER FOUND: ', waveform, ' ***'
		print,'waveform=1-127 for amp 1, waveform=128-255 for amp 2'
		read,'Enter waveform number for this CCD frame: ', waveform
		oldwaveform = waveform
	endif else waveform = oldwaveform
endif
if (waveform lt 128) then amp_num=1 else amp_num=2

count_rate = extract_float(buffer_pointer)

if (n_frames lt 20) then print_debug, 12, '     Count Rate = ' + string(count_rate)

tam_x1 = extract_float(buffer_pointer)
tam_y1 = extract_float(buffer_pointer)
tam_x2 = extract_float(buffer_pointer)
tam_y2 = extract_float(buffer_pointer)

if (n_frames lt 20) then print_debug, 15, '     TAM: ' + string(tam_x1) + string(tam_y1) $
	+ string(tam_x2) + string(tam_y2)

; Now, process HK data 
if (process_hk eq 0) then begin		; open HK file
	process_hk = 1
	get_lun,luhk
	file_hk = filebase + '.internal_temp_hk_file'
	openw,luhk,file_hk,/swap_if_little_endian
endif

hk_array = uintarr(34)
for i=0,27 do begin
	hk_array[i] = extract_int(buffer_pointer)
endfor
ccd_temp = RTD(hk_array[0], rtd_channel[hdr_hk_chan[0]])

hk_array[28] = xrt_state
hk_array[29] = xrt_mode
hk_array[30:33] = acs_flags


readout_start_sec = extract_long(buffer_pointer)
readout_start_subsec = extract_int(buffer_pointer)
readout_start_time = readout_start_sec + readout_start_subsec*20.0D-6

readout_stop_sec = extract_long(buffer_pointer)
readout_stop_subsec = extract_int(buffer_pointer)
readout_stop_time = readout_stop_sec + readout_stop_subsec*20.0D-6

if (n_frames lt 20) then print_debug,12,'     Readout time start & stop: ' $
	+ string(readout_start_time) + string(readout_stop_time)

if (readout_stop_time lt readout_start_time) then print_error,0,1, $
    'PROCESS_LR_FRAME_HDR: WARNING: Readout START time > Readout STOP time!'

; Calculate actual exposure time, and the frame start and stop times.
;	Times in header correspond to the METs at the end of the first and
;	last rows and therefore correspond to the readout time, not the exposure
;	time.  This code corrects this to retrieve the actual exposure time.
delta = readout_stop_time - readout_start_time
row_time = delta/601		; nrow = 602, times in rows 1 and 602
pixel_time = row_time/float(631)	; ncol = 631
xfer_time = 0.0
first_pixel = readout_start_time - 631*pixel_time - (5+300+602+301)*pixel_time


if (n_frames lt 20) then print_debug,15,'     Delta, Row_time, Pixel_time, xfer_time:   ' $
	+ string(delta) + string(row_time) + string(pixel_time) + string(xfer_time)

exposure_time = delta + row_time
livetime = exposure_time

if (n_frames lt 20) then print_debug, 15, '     Exposure time is ' $
		+ string(exposure_time)

if (xrt_mode eq last_mode) then $
	frame_start_time = readout_start_time - (5+1204+630)*pixel_time $
;	= time when first pixel of "frame" enters image area: it then is 
;		clocked down 1204 times and out 5 times to amp; then
;		630 other pixels are clocked out before the time stamp.
;	
else $
	frame_start_time = readout_start_time - 631*pixel_time
;	= time when first pixel of "frame" is read out

frame_stop_time = readout_stop_time - (5+631+601)*pixel_time
;	= time when last pixel of "frame" leaves image area
;	Note that "frame" times will overlap with these definitions
satellite_time = (frame_start_time + frame_stop_time)/2.0 ; use mid-point of CCD exposure

printf,lustats, format='(i10,'' LR '', 5f20.6,5f10.6)', $
	observation_number, last_stop_time, readout_start_time, readout_stop_time, $
	frame_start_time, frame_stop_time, delta, row_time, $
	xfer_time, exposure_time, livetime

last_stop_time = readout_stop_time
last_mode = xrt_mode

if (n_frames lt 20) then print_debug,15,'     Frame time start & stop:   ' $
	+ string(frame_start_time) + string(frame_stop_time)
if (n_frames lt 20) then print_debug,15,'     Satellite_time: ' + string(satellite_time)

nom_exp_sec = extract_int(buffer_pointer)
nom_exp_subsec = extract_int(buffer_pointer)
nom_exposure_time = nom_exp_sec + nom_exp_subsec*20.0D-6

if (n_frames lt 20) then print_debug,12,'     Nominal and actual exposure times: ' $
	+ string(nom_exposure_time) + ' (nominal) ,' $
	+ string(exposure_time) + ' (actual)'

N_events = 0

; Extract bias threshold
bias_threshold = extract_int(buffer_pointer)

event_threshold = extract_int(buffer_pointer)
N_pixels_over_LLD = extract_long(buffer_pointer)

ULD = extract_int(buffer_pointer)
N_pixels_over_ULD = extract_long(buffer_pointer)

amp = extract_byte(buffer_pointer)
if (amp ne amp_num) then begin
    print_error,0,1,$
	'PROCESS_LR_FRAME_HDR: Wrong Amp for this waveform number - waveform = ' $
	+ strtrim(string(waveform),2) + ', amp = ' + strtrim(string(fix(amp)),2)
endif

;baseline_offset = extract_int(buffer_pointer)
baseline_offset = 0

N_Hdr_Pix = extract_byte(buffer_pointer)

; 2 extra bytes for bias_level added to header in Build 8.8
bias_level = 0
if (bias_threshold gt 0) then bias_level = extract_int(buffer_pointer)

N_pixels = extract_long(buffer_pointer)
N_events = N_pixels		; needed for rest of PASS1 to work right

if (N_Hdr_Pix gt 0) then begin	; new LrPD format header - read bias data for Build > 8.4
	N_Bias_Pix = extract_long(buffer_pointer)
	Sum_Bias_Pix = extract_double(buffer_pointer)
	SOS_Bias_Pix = extract_double(buffer_pointer)
	Bias_Pixels = intarr(N_Hdr_Pix)
	for j=0,N_Hdr_Pix-1 do begin
		Bias_Pixels[j] = extract_int(buffer_pointer)
	endfor	
endif

print_debug,0,'  Frame #' + strtrim(string(frame_num),2) $
	+ ':  LRPD mode @ Satellite_time = ' + strtrim(string(satellite_time),2) $
	+ ',  N > LLD = ' + strtrim(string(N_events),2) $
	+ ',  Count Rate = ' + strtrim(string(count_rate),2)


if (n_frames lt 30) then begin
	print_debug,12,'     Event thresholds: Event = ' $
		+ strtrim(string(event_threshold),2) $
		+ ', ULD = ' + strtrim(string(ULD),2)

	print_debug,12,'     Pixel counts:'
	print_debug,12,'          # pixels over LLD = ' $
		+ strtrim(string(n_pixels_over_lld),2)
	print_debug,12,'          # pixels over ULD = ' $
		+ strtrim(string(n_pixels_over_uld),2)
	print_debug,12,'          N_pixels telemetered = ' $
		+ strtrim(string(n_pixels),2)
endif



; Print/write housekeeping data

if (n_frames lt 20) then begin
	print_debug,20,'   Housekeeping data at S/C time ' + string(satellite_time) + ':'
	if (idebug ge 20) then begin
		print,format='("             ",10(z6,1x))',hk_array
		printf,lulog,format='("             ",10(z6,1x))',hk_array
	endif
	if (idebug ge 25) then begin
		print,'PROCESS_LR_FRAME_HDR: HK = ', hk_array
		printf,lulog,'PROCESS_LR_FRAME_HDR: HK = ', hk_array
	endif
endif

num_hk_records = num_hk_records + 1

; write output data to file as unformatted binary data
writeu,luhk,satellite_time,hk_array


; Now, create the FITS file header

file_info = fstat(luin)
file_date = bin_date(systime(0))
date = string(file_date(2),file_date(1),format="(i2.2,'/',i2.2,'/')") $
		+ strmid(string(file_date(0),format="(i4.4)"),2,2)

mkhdr,ccd_hdr,image		; TBD: change so this uses empty image array
sxaddhist, ' ', ccd_hdr
sxaddhist,'*** SWIFT XRT *** CCD Frame processed by PASS1 '+version, $
			ccd_hdr
sxaddpar, ccd_hdr, 'ORIGIN','PSU X-ray Astronomy', $
			'Data from Penn State X-ray Astronomy'
sxaddpar, ccd_hdr, 'TELESCOP', 'Swift', 'Swift Gamma-Ray Burst Explorer'
sxaddpar, ccd_hdr, 'INSTRUME','XRT',$
			'XRT Instrument on Swift Gamma-ray Burst Explorer'
sxaddhist, ' ', ccd_hdr
sxaddpar, ccd_hdr, 'OBS_NUM', $
	strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
	'Swift Observation Number'
sxaddpar, ccd_hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
sxaddpar, ccd_hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
sxaddpar, ccd_hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
sxaddhist, ' ', ccd_hdr
sxaddpar, ccd_hdr, 'OBS_MODE', obs_mode
sxaddpar, ccd_hdr, 'DATAMODE', readout_mode, 'XRT Low-Rate Photodiode mode'
sxaddhist, ' ', ccd_hdr
sxaddpar, ccd_hdr, 'CREATOR',  'PASS1 (low_rate_photodiode), '+version

sxaddhist, ' ', ccd_hdr
sxaddhist, 'XRT Low-Rate Photodiode Mode CCD Frame', ccd_hdr
sxaddpar, ccd_hdr, 'FILEIN',file_info.name,'Name of input data file'
sxaddpar, ccd_hdr, 'DATE',date,'File creation date'
sxaddpar, ccd_hdr, 'FRAME',ccd_frame,'Sequential frame number in this file'
sxaddpar, ccd_hdr, 'CMPRSD','F','This frame was not taken in compressed format'
sxaddpar, ccd_hdr, 'RAW','F','This frame not collected in raw format'

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Frame Time Stamps:', ccd_hdr
sxaddpar, ccd_hdr, 'MET_STRT', readout_start_time, 'Satellite time of readout start'
sxaddpar, ccd_hdr, 'MET_STOP', readout_stop_time, 'Satellite time of readout end'
sxaddpar, ccd_hdr, 'TSTART', frame_start_time, 'Satellite time of exposure start'
sxaddpar, ccd_hdr, 'TSTOP', frame_stop_time, 'Satellite time of exposure end'
sxaddpar, ccd_hdr, 'TELAPSE', frame_stop_time - frame_start_time, 'Elapsed time'
sxaddpar, ccd_hdr, 'ONTIME', exposure_time,'Total good time for this frame'
sxaddpar, ccd_hdr, 'EXPOSURE', exposure_time,'CCD Exposure time in seconds'
sxaddpar, ccd_hdr, 'LIVETIME', livetime, 'Livetime for this frame'
sxaddpar, ccd_hdr, 'T0', first_pixel, 'S/C time of first pixel'
sxaddpar, ccd_hdr, 'DELTA_T', pixel_time, 'Time between samples'


sxaddhist, ' ', ccd_hdr
sxaddhist, 'Aspect Information:', ccd_hdr
sxaddpar, ccd_hdr, 'RA',ra,'Right Ascension (degrees)'
sxaddpar, ccd_hdr, 'DEC',dec,'Declination (degrees)'
sxaddpar, ccd_hdr, 'Roll',roll,'Roll angle (degrees)'
sxaddpar,ccd_hdr,'ACS_Flags', fix(acs_flag_word), $
	'ACS Flags: 0x'+strtrim(string(acs_flag_word,format='(z2.2)'),2)
sxaddpar, ccd_hdr, 'SAFEHOLD',safe_mode,'Is the S/C in Safe Hold?'
sxaddpar, ccd_hdr, 'SAA',in_saa,'Is the S/C in the SAA?'
sxaddpar, ccd_hdr, '10arcmin',is_in_10_arcmin,'Within 10 arcmin of target?'
sxaddpar, ccd_hdr, 'SETTLED',is_settled,'Settled on target?'
sxaddpar, ccd_hdr, 'TAM_X1',tam_x1,'TAM Window 1'
sxaddpar, ccd_hdr, 'TAM_Y1',tam_y1,'TAM Window 1'
sxaddpar, ccd_hdr, 'TAM_X2',tam_x2,'TAM Window 2'
sxaddpar, ccd_hdr, 'TAM_Y2',tam_y2,'TAM Window 2'

; Set up CCD frame parameters

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Parameters:', ccd_hdr

sxaddpar, ccd_hdr, 'CCD_FRAME',frame_num,'On-board CCD Frame Counter'
sxaddpar, ccd_hdr, 'CCD_NUM',1,'CCD Number'
sxaddpar, ccd_hdr, 'AMP',fix(amp),'CCD Amplifier (1 or 2)'
sxaddpar, ccd_hdr, 'WAVEFORM',waveform,'CCD Readout Waveform Number'
sxaddpar, ccd_hdr, 'StateFlg',fix(xrt_state), $
	'XRT State Flag: 0x'+strtrim(string(xrt_state,format='(z2.2)'),2)
sxaddpar, ccd_hdr, 'XRTSTATE', state, 'XRT State'
sxaddpar, ccd_hdr, 'XRT_Mode',fix(xrt_mode),'XRT Mode Flag'
sxaddpar, ccd_hdr, 'READMODE', readout_mode, 'XRT Readout Mode'

; set up header structure for normal CCD events
EVENT_HEADER = {HEADER, N_EVENTS:0L, IEVENT_THRESHOLD:0L, $
		ISPLIT_THRESHOLD:0L, EVENT_THRESHOLD:0.0, $
		SPLIT_THRESHOLD:0.0, EVENT_SIGMA:0.0, SPLIT_SIGMA:0.0, $
		READ_NOISE:0.0, XMIN:0, XMAX:0, YMIN:0, YMAX:0, CCD:0} 
	   	    ; Output file has 42 byte records for compatibility 
	   	    ;    w/ FORTRAN direct access file format with 
	   	    ;    fixed sized records (42 bytes per record needed for events)

EVENT_HEADER.N_EVENTS = N_events
EVENT_HEADER.IEVENT_THRESHOLD = event_threshold
EVENT_HEADER.EVENT_THRESHOLD = event_threshold
window_halfwidth = 300
window_halfheight = 300
EVENT_HEADER.XMIN = 0	; counting from 0
EVENT_HEADER.YMIN = 0   ; counting from 0
EVENT_HEADER.XMAX = 599
EVENT_HEADER.YMAX = 601
EVENT_HEADER.CCD = 1

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Processing Parameters: ', ccd_hdr
sxaddpar, ccd_hdr, 'WIN_DX',window_halfwidth,'Half-width of window in pixels'
sxaddpar, ccd_hdr, 'WIN_DY',window_halfheight,'Half-height of window in pixels'
sxaddpar, ccd_hdr, 'XMIN',     EVENT_HEADER.XMIN
sxaddpar, ccd_hdr, 'XMAX',     EVENT_HEADER.XMAX
sxaddpar, ccd_hdr, 'YMIN',     EVENT_HEADER.YMIN
sxaddpar, ccd_hdr, 'YMAX',     EVENT_HEADER.YMAX
sxaddpar, ccd_hdr, 'EVTHRESH',event_threshold,'Event threshold (Lower Level Discriminator)'

sxaddpar, ccd_hdr, 'BSLN_OFF',baseline_offset,'Baseline_offset (DN)'

sxaddpar, ccd_hdr, 'ULD',ULD,'Upper Level Discriminator (DN)'
sxaddpar, ccd_hdr, 'N_GT_LLD',N_pixels_over_LLD,'Number of pixels over LLD'
sxaddpar, ccd_hdr, 'N_GT_ULD',N_pixels_over_ULD,'Number of pixels over ULD'
sxaddpar, ccd_hdr, 'N_PIXELS',N_pixels, 'Number of pixels telemetered'

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Bias Data: ', ccd_hdr
sxaddpar, ccd_hdr, 'BIAS_TH', bias_threshold, 'Bias Calculation Threshold'
sxaddpar, ccd_hdr, 'BIAS_LVL', bias_level, 'Integer bias level subtracted from data'
if (N_Hdr_Pix gt 0) then begin	; new data format Build 8.7+
	sxaddpar, ccd_hdr, 'NBIASPIX', N_Bias_Pix, 'Number of Pixels used to calculate bias level'
	sxaddpar, ccd_hdr, 'Sum_BIAS', Sum_Bias_Pix, 'Sum of Bias Pixels (<= BIAS_TH)'
	sxaddpar, ccd_hdr, 'SOS_BIAS', SOS_Bias_Pix, 'Sum of Squares of Bias Pixels (<= BIAS_TH)'
	if (N_Bias_Pix gt 0) then sxaddpar, ccd_hdr, 'BIAS', Sum_Bias_Pix/N_Bias_Pix, 'Mean Bias Level (for pixels <= BIAS_TH)'
	if (N_Bias_Pix gt 1) then sxaddpar, ccd_hdr, 'RD_NOISE', $
		Sqrt((SOS_Bias_Pix - (Sum_Bias_Pix)^2/N_Bias_Pix)/(N_Bias_Pix-1)), $
		'Read Noise (Std Dev of pixels <= BIAS_TH)'
endif
sxaddpar, ccd_hdr, 'NHDRPIX',fix(N_Hdr_Pix),'Number of pixel values in frame header'
if (N_Hdr_Pix gt 0) then begin
	pix_sum = 0
	for j=0,N_Hdr_Pix-1 do begin
		sxaddpar, ccd_hdr, 'PIXEL'+strtrim(string(j),2), Bias_Pixels[j], 'Pixel value for bias calculation'
		pix_sum = pix_sum + bias_pixels[j]
	endfor
	mean_pix = float(pix_sum)/float(N_Hdr_Pix)
	sxaddpar, ccd_hdr, 'MEANPIX', mean_pix, 'Mean value of header pixels'
	median_pix = median(bias_pixels)
	sxaddpar, ccd_hdr, 'MEDIANPX', median_pix, 'Median value of header pixels'
endif

; Put HK values in header

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Housekeeping: ', ccd_hdr
sxaddpar, ccd_hdr, 'T_CCD',ccd_temp,'CCD Temperature (degrees C)'
; set up HK channels
ch = hdr_hk_chan
sxaddpar,ccd_hdr,'Vod1',hk_array[1]*hk_slope[ch[1]] + hk_intercept[ch[1]],$
	'Vod for Amp 1'
sxaddpar,ccd_hdr,'Vod2',hk_array[2]*hk_slope[ch[2]] + hk_intercept[ch[2]],$
	'Vod for Amp 2'
sxaddpar,ccd_hdr,'Vrd1',hk_array[3]*hk_slope[ch[3]] + hk_intercept[ch[3]],$
	'Amp 1 reset diode voltage'
sxaddpar,ccd_hdr,'Vrd2',hk_array[4]*hk_slope[ch[4]] + hk_intercept[ch[4]], $
	'Amp 2 reset diode voltage'
sxaddpar,ccd_hdr,'Vog1',hk_array[5]*hk_slope[ch[5]] + hk_intercept[ch[5]], $
	'Amp 1 output gate voltage'
sxaddpar,ccd_hdr,'Vog2',hk_array[6]*hk_slope[ch[6]] + hk_intercept[ch[6]], $
	'Amp 2 output gate voltage'
sxaddpar,ccd_hdr,'1Rp1',hk_array[7]*hk_slope[ch[7]] + hk_intercept[ch[7]], $
	'Amp 1 Serial clock phase 1 voltage'
sxaddpar,ccd_hdr,'1Rp2',hk_array[8]*hk_slope[ch[8]] + hk_intercept[ch[8]], $
	'Amp 1 Serial clock phase 2 voltage'
sxaddpar,ccd_hdr,'1Rp3',hk_array[9]*hk_slope[ch[9]] + hk_intercept[ch[9]], $
	'Amp 1 Serial clock phase 3 voltage'
sxaddpar,ccd_hdr,'1pR',hk_array[10]*hk_slope[ch[10]] + hk_intercept[ch[10]],$
	'Amp 1 Reset Clock voltage'
sxaddpar,ccd_hdr,'2pR',hk_array[11]*hk_slope[ch[11]] + hk_intercept[ch[11]], $
	'Amp 2 Reset Clock voltage'
sxaddpar,ccd_hdr,'2Rp1',hk_array[12]*hk_slope[ch[12]] + hk_intercept[ch[12]],$
	'Amp 2 Serial clock phase 1 voltage'
sxaddpar,ccd_hdr,'2Rp2',hk_array[13]*hk_slope[ch[13]] + hk_intercept[ch[13]],$
	'Amp 2 Serial clock phase 2 voltage'
sxaddpar,ccd_hdr,'2Rp3',hk_array[14]*hk_slope[ch[14]] + hk_intercept[ch[14]],$
	'Amp 2 Serial clock phase 3 voltage'
sxaddpar,ccd_hdr,'Vgr',hk_array[15]*hk_slope[ch[15]] + hk_intercept[ch[15]],$
	'Guard Ring voltage'
sxaddpar,ccd_hdr,'Vsub',hk_array[16]*hk_slope[ch[16]] + hk_intercept[ch[16]],$
	'Substrate voltage'
sxaddpar,ccd_hdr,'Vbackjun',hk_array[17]*hk_slope[ch[17]]+hk_intercept[ch[17]],$
	'Back Junction voltage'
sxaddpar,ccd_hdr,'Vid',hk_array[18]*hk_slope[ch[18]] + hk_intercept[ch[18]],$
	'Input Diode voltage'
sxaddpar,ccd_hdr,'Ip1',hk_array[19]*hk_slope[ch[19]] + hk_intercept[ch[19]],$
	'Image area phase 1 clock voltage'
sxaddpar,ccd_hdr,'Ip2',hk_array[20]*hk_slope[ch[20]] + hk_intercept[ch[20]],$
	'Image area phase 2 clock voltage'
sxaddpar,ccd_hdr,'Ip3',hk_array[21]*hk_slope[ch[21]] + hk_intercept[ch[21]],$
	'Image area phase 3 clock voltage'
sxaddpar,ccd_hdr,'Sp1',hk_array[22]*hk_slope[ch[22]] + hk_intercept[ch[22]],$
	'Store area phase 1 clock voltage'
sxaddpar,ccd_hdr,'Sp2',hk_array[23]*hk_slope[ch[23]] + hk_intercept[ch[23]],$
	'Store area phase 2 clock voltage'
sxaddpar,ccd_hdr,'Sp3',hk_array[24]*hk_slope[ch[24]] + hk_intercept[ch[24]],$
	'Store area phase 3 clock voltage'
sxaddpar,ccd_hdr,'pIG',hk_array[25]*hk_slope[ch[25]] + hk_intercept[ch[25]],$
	'Input Gate clock voltage'
sxaddpar,ccd_hdr,'V_BSLN_A',hk_array[26]*hk_slope[ch[26]]+hk_intercept[ch[26]],$
	'Signal Chain A Baseline voltage'
sxaddpar,ccd_hdr,'V_BSLN_B',hk_array[27]*hk_slope[ch[27]]+hk_intercept[ch[27]],$
	'Signal Chain B Baseline voltage'

; Reset the cmprssd flag
cmprssd = 0
return

end
