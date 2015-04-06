pro process_tam_image_hdr, buffer_pointer, error_code
; $Id: process_tam_image_hdr.pro 0.0 1995/12/21 17:25:30 burrows Exp $

;name	: process_tam_image_hdr
;author	: Dave Burrows
;date	: 02/10/02
;lang	: IDL

;purpose: This routine processes the XRT TAM Frame Header.  It extracts
;	data from this header for the output FITS header.
;	It returns an error code which is 0 if no errors are found:
;	error_code	
;	    0	no errors
;	   71	Packet is not LDP page 1
;	   72	Found redundant LDP page 1
;	   73	Packet is not a TAM Image Header
;	   91	PROCESS_TAM_IMAGE_HDR: EOF encountered before end of CCD header
;
;   Rev:
;	08/09/04 by DNB: fixed error in unpacking UTC factors (was treated as 
;			unsigned, should have been signed)
;	04/21/04 by DNB: saving first and last times
;	06/14/03 by DNB: fixed numerous logic errors needed to process data
;		file that doesn't begin with TAM header.
;	04/10/03 by DNB: commented out keywords not valid for full-frame TAM image
;	04/07/03 by DNB: based on process_image_frame_hdr
;	03/27/03 by DNB: dropped synchronization between on-board and internal CCD frame number
;	09/06/02 by DNB: based on windowed-timing mode version.
;	08/09/02 by DNB: massive changes to get routine to work correctly
;		for XRT.  Debugged using photon-counting data generated with
;		fake (simulated) CCD inputs.
;	04/07/02 by DNB: changed HK array to unsigned integer, deleted last
;		four time words.
;;
;

@pass1_common

@tam_common

; "declare" variable types
error_code = 0



; Read TAM LDP number
LDP_num = extract_int(buffer_pointer)

print_debug,0,'*****************************************'
print_debug,0,'TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM'
print_debug,0,'PROCESS_TAM_IMAGE_HDR: Processing TAM Image Hdr ' $
		+ 'for TAM LDP#' + strtrim(string(LDP_num),2)

if (tam_frame eq 1) then begin
	first_frame = LDP_num
endif 
if (idebug gt 9) then printf,lulog,'tam_frame, LDP_num, first_frame = ', tam_frame, LDP_num, first_frame
if (LDP_num ne tam_frame) then begin
    if (tam_frame ne 1) then begin
	print_error,0,1,'PROCESS_TAM_IMAGE_HDR: ERROR in TAM Frame #: found '+ $
		strtrim(string(LDP_num),2)+', expected '+ $
		strtrim(string(tam_frame),2)
    endif
    tam_frame = LDP_num		; synchronize the counters
endif

LDP_page = extract_int(buffer_pointer)
pages_in_file = extract_int(buffer_pointer)
page_count = page_count + 1UL
	
print, 'TAM_IMAGE: Product # = ', LDP_num,$
		'   Page # = ', LDP_page, '   Pages in file = ', $
				pages_in_file
printf,lulog, 'TAM_IMAGE: Product # = ', LDP_num,$
		'   Page # = ', LDP_page, '   Pages in file = ', $
				pages_in_file

if (LDP_page ne 1) then begin
	print_error,0,1,'PROCESS_TAM_IMAGE_HDR: LDP page number is not 1...'
	LDP_page_old = LDP_page
	LDP_num_old = LDP_num
	error_code = 71
	return
endif

if (LDP_num eq LDP_num_old) then begin	; redundant LDP header
	print_error,0,1,'PROCESS_TAM_IMAGE_HDR: found redundant LDP header -- skipping this packet'
	LDP_page_old = LDP_page
	LDP_num_old = LDP_num
	error_code = 72
	return
endif

LDP_num_old = LDP_num
LDP_page_old = LDP_page

; Get TAM header info and check for format errors
; TBD: Add error checking for following parameters - compare to disk file
;	with nominal values

observation_number = extract_long(buffer_pointer)
obs_segment = (observation_number and 'FF000000'xl) / (256L*256L*256L)
target_id = (observation_number and '00FFFFFF'xl)
seq_num = target_id*1000L + obs_segment
print,' '
print,format='(a,'' 0x'',z8.8, i10, i5, i15)', 'Obs Num, Target ID, Obs Seg, Seq Num: ', $
	observation_number, target_id, obs_segment, seq_num
printf,lulog,' '
printf,lulog,format='(a,'' 0x'',z8.8, i10, i5, i15)', 'Observation Number, Target ID, Obs Segment, Sequence Number: ', $
	observation_number, target_id, obs_segment, seq_num

readout_start_sec = extract_long(buffer_pointer)
readout_start_subsec = extract_int(buffer_pointer)
readout_start_time = readout_start_sec + readout_start_subsec*20.0D-6
tam_image_time = readout_start_time

if (tam_frame eq 1) then first_time = tam_image_time $
else last_time = tam_image_time

readout_utc_sec = long(extract_long(buffer_pointer))
readout_utc_subsec = extract_int(buffer_pointer)
readout_utc_time = readout_utc_sec + readout_utc_subsec*20.0D-6

if (tam_frame eq 1) then first_utc = readout_utc_time $
else last_utc = readout_utc_time

print,format='(a, f15.3,f15.6)', 'TAM Image time and UTC: ', readout_start_time, readout_utc_time

	if (idebug ge 35) then begin
		print,'TAM_IMAGE: Observation segment, Target ID = ', $
				obs_segment, target_id
		print,format='(a,''0x'',z8.8)', $
			'TAM_IMAGE: Data collection time seconds = ', $
				readout_start_sec
		print,format='(a,''0x'',z8.8)',$
			'TAM_IMAGE: Data collection time subsec  = ', $
				readout_start_subsec
		print,'TAM_IMAGE: Data collection time = ', readout_start_time, ' s'
		print,format='(a,''0x'',z8.8)', $
			'TAM_IMAGE: Data collection UTC seconds = ', readout_utc_sec
		print,format='(a,''0x'',z8.8)',$
			'TAM_IMAGE: Data collection UTC subsec  = ', readout_utc_subsec

		printf,lulog,'TAM_IMAGE: Observation segment, Target ID = ', $
				obs_segment, target_id
		printf,lulog,format='(a,''0x'',z8.8)', $
			'TAM_IMAGE: Data collection time seconds = ', $
				readout_start_sec
		printf,lulog,format='(a,''0x'',z8.8)',$
			'TAM_IMAGE: Data collection time subsec  = ', $
				readout_start_subsec
		printf,lulog,'TAM_IMAGE: Data collection time = ', readout_start_time, ' s'
		printf,lulog,format='(a,''0x'',z8.8)', $
			'TAM_IMAGE: Data collection UTC seconds = ', readout_utc_sec
		printf,lulog,format='(a,''0x'',z8.8)',$
			'TAM_IMAGE: Data collection UTC subsec  = ', readout_utc_subsec
	endif


tam_frame_header_id = extract_long(buffer_pointer)
if (tam_frame_header_id ne '9A052222'x) then begin
	print_error,0,1,'PROCESS_TAM_IMAGE_HDR: ERROR - found incorrect TAM header ID = 0x' + strtrim(string(tam_frame_header_id,format='(z8.8)'),2) + ' at buffer position ' + strtrim(string(buffer_pointer),2)
	print_error,0,1, 'PROCESS_TAM_IMAGE_HDR: packet is not a TAM Image Header - returning to calling program'
	error_code = 72
	return
endif

;ra = extract_float(buffer_pointer)
;dec = extract_float(buffer_pointer)
;roll = extract_float(buffer_pointer)
;
;if (n_frames lt 20) then print_debug,12,'     RA = ' + strtrim(string(ra),2) + ', Dec = ' $
;	+ strtrim(string(dec),2) + ', Roll = ' + strtrim(string(roll),2)

tam_report_mode = extract_int(buffer_pointer)
case tam_report_mode of
	0: begin
		print, 'TAM Auto State report mode: ', tam_report_mode, ' = Disabled (no TAM data)'
		printf,lulog, 'TAM Auto State report mode: ', tam_report_mode, ' = Disabled (no TAM data)'
	   end
	1: begin
		print, 'TAM Auto State report mode: ', tam_report_mode, ' = Ap_ID 0x48f only'
		printf,lulog, 'TAM Auto State report mode: ', tam_report_mode, ' = Ap_ID 0x48f only'
	   end
	2: begin
		print, 'TAM Auto State report mode: ', tam_report_mode, ' = Ap_ID 0x542 packet: TAM Windows report'
		printf,lulog, 'TAM Auto State report mode: ', tam_report_mode, ' = Ap_ID 0x542 packet: TAM Windows report'
	   end
	else: stop, 'PROCESS_TAM_IMAGE_HDR: Unrecoverable error - invalid TAM report mode'
endcase

tam_x1_window = extract_int(buffer_pointer)
tam_y1_window = extract_int(buffer_pointer)
tam_x2_window = extract_int(buffer_pointer)
tam_y2_window = extract_int(buffer_pointer)

;print,' '
;print, 'TAM Window 1 coordinates (lower-left corner): ', tam_x1_window, tam_y1_window
;print, 'TAM Window 2 coordinates (lower-left corner): ', tam_x2_window, tam_y2_window
printf,lulog, ' ' 
printf,lulog, 'TAM Window 1 coordinates (lower-left corner): ', tam_x1_window, tam_y1_window
printf,lulog, 'TAM Window 2 coordinates (lower-left corner): ', tam_x2_window, tam_y2_window

tam_x1_size = extract_int(buffer_pointer)
tam_y1_size = extract_int(buffer_pointer)
tam_x2_size = extract_int(buffer_pointer)
tam_y2_size = extract_int(buffer_pointer)

;print, ' '
;print, 'TAM Window 1 size: ', tam_x1_size, tam_y1_size
;print, 'TAM Window 2 size: ', tam_x2_size, tam_y2_size
printf,lulog, ' '
printf,lulog, 'TAM Window 1 size: ', tam_x1_size, tam_y1_size
printf,lulog, 'TAM Window 2 size: ', tam_x2_size, tam_y2_size

tam_lld = extract_int(buffer_pointer)
tam_uld = extract_int(buffer_pointer)
num_pix = extract_int(buffer_pointer)

;print,' ' 
;print,'TAM LLD, TAM ULD, NumPixForCentroid = ', tam_lld, tam_uld, num_pix
printf,lulog,' ' 
printf,lulog,'TAM LLD, TAM ULD, NumPixForCentroid = ', tam_lld, tam_uld, num_pix

tam_int_mode = extract_int(buffer_pointer)
int_lines = extract_int(buffer_pointer)
frame_delay = extract_int(buffer_pointer)

;print, 'TAM Integration Mode, Int. Lines, Frame Delay = ', tam_int_mode, int_lines, frame_delay
printf,lulog, 'TAM Integration Mode, Int. Lines, Frame Delay = ', tam_int_mode, int_lines, frame_delay

gain = extract_int(buffer_pointer)
gain_level = ['1X', '2X', '4X', '8X']
print,'TAM Gain Level: ', gain, ' = ', gain_level[gain]
printf,lulog, 'TAM Gain Level: ', gain, ' = ', gain_level[gain]

led = extract_int(buffer_pointer)
led_select = ['LEDs Off', 'LED1', 'LED2']
;print, 'LED Select:     ', led, ' = ', led_select[led]
printf,lulog, 'LED Select:     ', led, ' = ', led_select[led]

sigma_max = extract_float(buffer_pointer)
;print,'Sigma Max for centroid: ', sigma_max
printf,lulog,'Sigma Max for centroid: ', sigma_max

tam_x1_standby_window = extract_int(buffer_pointer)
tam_y1_standby_window = extract_int(buffer_pointer)
tam_x2_standby_window = extract_int(buffer_pointer)
tam_y2_standby_window = extract_int(buffer_pointer)
;print,' '
;print, 'TAM Standby Window 1 coordinates (lower-left corner): ', tam_x1_standby_window, tam_y1_standby_window
;print, 'TAM Standby Window 2 coordinates (lower-left corner): ', tam_x2_standby_window, tam_y2_standby_window
printf,lulog, ' ' 
printf,lulog, 'TAM Standby Window 1 coordinates (lower-left corner): ', tam_x1_standby_window, tam_y1_standby_window
printf,lulog, 'TAM Standby Window 2 coordinates (lower-left corner): ', tam_x2_standby_window, tam_y2_standby_window

tam_x1_standby_size = extract_int(buffer_pointer)
tam_y1_standby_size = extract_int(buffer_pointer)
tam_x2_standby_size = extract_int(buffer_pointer)
tam_y2_standby_size = extract_int(buffer_pointer)

;print, ' '
;print, 'TAM Standby Window 1 size: ', tam_x1_standby_size, tam_y1_standby_size
;print, 'TAM Standby Window 2 size: ', tam_x2_standby_size, tam_y2_standby_size
printf,lulog, ' '
printf,lulog, 'TAM Standby Window 1 size: ', tam_x1_standby_size, tam_y1_standby_size
printf,lulog, 'TAM Standby Window 2 size: ', tam_x2_standby_size, tam_y2_standby_size

addr = extract_long(buffer_pointer)
rs422_id = extract_long(buffer_pointer)
rs422_port = extract_long(buffer_pointer)
fill = extract_long(buffer_pointer)
tam_state = extract_long(buffer_pointer)
tam_error = extract_long(buffer_pointer)

printf,lulog,' '
printf,lulog,format='(a,6Z10.8)', 'TAM S/W parameters: ', addr, rs422_id, rs422_port, fill, tam_state, tam_error

tam_x1 = extract_float(buffer_pointer)
tam_y1 = extract_float(buffer_pointer)
tam_x2 = extract_float(buffer_pointer)
tam_y2 = extract_float(buffer_pointer)

tam_sequence_counter = extract_long(buffer_pointer)

tam_registers = uintarr(15)
for ijk=0,14 do tam_registers[ijk] = extract_int(buffer_pointer)

tam_sig1 = extract_float(buffer_pointer)
tam_sig2 = extract_float(buffer_pointer)

; write data to tam centroid file
printf,lutam,format='(3i10,i5,f20.6,6f10.3)',$
	tam_sequence_counter,tam_frame,target_id,obs_segment,$
		readout_start_time,$
		tam_x1,tam_y1,tam_sig1,$
		tam_x2,tam_y2,tam_sig2

print,' '
print,'TAM Centroid #', strtrim(string(tam_sequence_counter),2)
print,'TAM Window 1 centroid: ', tam_x1, tam_y1
print,'TAM Window 2 centroid: ', tam_x2, tam_y2
printf,lulog,' '
print,'TAM Centroid #', strtrim(string(tam_sequence_counter),2)
printf,lulog,'TAM Window 1 centroid, std dev: ', tam_x1, tam_y1, tam_sig1
printf,lulog,'TAM Window 2 centroid, std dev: ', tam_x2, tam_y2, tam_sig2

printf,lulog,' '
printf,lulog,'TAM Registers: '
printf,lulog,format='(15z6.4)',tam_registers

n_pixels = extract_long(buffer_pointer)
n_events = n_pixels

;****************************************************************************

; Now, create the FITS file header

file_info = fstat(luin)
file_date = bin_date(systime(0))
date = string(file_date(2),file_date(1),format="(i2.2,'/',i2.2,'/')") $
		+ strmid(string(file_date(0),format="(i4.4)"),2,2)

image=intarr(512,512)
mkhdr,tam_hdr,image		; TBD: change so this uses empty image array
sxaddhist, ' ', tam_hdr
sxaddhist,'*** SWIFT XRT *** TAM Full-Frame Image processed by PASS1 '+version, $
			tam_hdr
sxaddpar, tam_hdr, 'ORIGIN','PSU X-ray Astronomy', $
			'Data from Penn State X-ray Astronomy'
sxaddpar, tam_hdr, 'TELESCOP', 'Swift', 'Swift Gamma-Ray Burst Explorer'
sxaddpar, tam_hdr, 'INSTRUME','XRT',$
			'XRT Instrument on Swift Gamma-ray Burst Explorer'
sxaddhist, ' ', tam_hdr
sxaddpar, tam_hdr, 'OBS_NUM', $
	strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
	'Swift Observation Number'
sxaddpar, tam_hdr, 'TARGETID',target_id, 'Swift Target ID'
sxaddpar, tam_hdr, 'OBS_SEG', obs_segment, 'Swift Observation Segment for this target'
sxaddpar, tam_hdr, 'SEQ_NUM',  seq_num, 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
sxaddhist, ' ', tam_hdr
sxaddpar, tam_hdr, 'CREATOR',  'PASS1 (image), '+version

sxaddhist, ' ', tam_hdr
sxaddhist, 'TAM Image Frame', tam_hdr
sxaddpar, tam_hdr, 'FILEIN',file_info.name,'Name of input data file'
sxaddpar, tam_hdr, 'DATE',date,'File creation date'
sxaddpar, tam_hdr, 'FRAME',tam_frame,'Sequential frame number in this file'

sxaddhist, ' ', tam_hdr
sxaddhist, 'TAM Frame Time Stamp:', tam_hdr
sxaddpar, tam_hdr, 'TAM_TIME', readout_start_time,'Satellite time of readout start'
sxaddpar, tam_hdr, 'TAM_UTC', readout_utc_time,'UTC correction'

sxaddhist, ' ', tam_hdr
sxaddhist, 'Aspect Information:', tam_hdr
;sxaddpar, tam_hdr, 'RA', ra, 'Right Ascension (degrees)'
;sxaddpar, tam_hdr, 'DEC', dec, 'Declination (degrees)'
;sxaddpar, tam_hdr, 'Roll', roll, 'Roll angle (degrees)'
sxaddpar, tam_hdr, 'TAM_X1', tam_x1, 'TAM Window 1 X centroid'
sxaddpar, tam_hdr, 'TAM_Y1', tam_y1, 'TAM Window 1 Y centroid'
sxaddpar, tam_hdr, 'TAM_SIG1', tam_sig1, 'TAM Window 1 Standard Deviation (radial)'
sxaddpar, tam_hdr, 'TAM_X2', tam_x2, 'TAM Window 2 X centroid'
sxaddpar, tam_hdr, 'TAM_Y2', tam_y2, 'TAM Window 2 Y centroid'
sxaddpar, tam_hdr, 'TAM_SIG2', tam_sig2, 'TAM Window 2 Standard Deviation (radial)'


; Set up TAM frame parameters

sxaddhist, ' ', tam_hdr
sxaddhist, 'TAM Parameters:', tam_hdr
sxaddpar, tam_hdr, 'TAM_FRAME',LDP_num,'On-board TAM Frame Counter (LDP #)'
;sxaddpar, tam_hdr, 'TAM_SEQ', tam_sequence_counter, 'TAM Sequence Counter (valid updates)'
;sxaddpar, tam_hdr, 'TAMSTATE', tam_state, 'TAM State: -1=error, 0=off, 1=OK'
sxaddpar, tam_hdr, 'TAM_MODE', tam_int_mode, 'TAM Integration mode: 0=ILR, 1=FDR'
sxaddpar, tam_hdr, 'INTLINES', int_lines, 'TAM Integration Lines (ILR mode)'
sxaddpar, tam_hdr, 'FRMDELAY', frame_delay, 'TAM Frame Delay (FDR mode)'
;sxaddpar, tam_hdr, 'TAM_RPRT', tam_report_mode, 'TAM Report Mode in Auto State: 0=none, 1=0x48f, 2=0x542'
sxaddpar, tam_hdr, 'GAIN', gain, 'TAM gain: 0=1X, 1=2X, 2=4X, 3=8X'
sxaddpar, tam_hdr, 'LED_NUM', led, 'TAM LED: 0=none, 1=LED1, 2=LED2'
;sxaddpar, tam_hdr, 'TAM_LLD', tam_lld, 'TAM Lower Level Discrimination (centroid threshold)'
;sxaddpar, tam_hdr, 'TAM_ULD',tam_uld,'TAM Upper Level Discriminator (centroid threshold)'
;sxaddpar, tam_hdr, 'NUM_PIX',num_pix,'Number of pixels for valid centroid'
;sxaddpar, tam_hdr, 'SIG_MAX', sigma_max, 'TAM Max Sigma for valid centroid'
;sxaddpar, tam_hdr, 'X1WIN', tam_x1_window, 'TAM Window 1 Lower Left Corner X'
;sxaddpar, tam_hdr, 'Y1WIN', tam_y1_window, 'TAM Window 1 Lower Left Corner Y'
;sxaddpar, tam_hdr, 'WIN1XSYZ', tam_x1_size, 'TAM Window 1 X Size (pixels)'
;sxaddpar, tam_hdr, 'WIN1YSYZ', tam_y1_size, 'TAM Window 1 Y Size (pixels)'
;sxaddpar, tam_hdr, 'X2WIN', tam_x2_window, 'TAM Window 2 Lower Left Corner X'
;sxaddpar, tam_hdr, 'Y2WIN', tam_y2_window, 'TAM Window 2 Lower Left Corner Y'
;sxaddpar, tam_hdr, 'WIN2XSYZ', tam_x2_size, 'TAM Window 2 X Size (pixels)'
;sxaddpar, tam_hdr, 'WIN2YSYZ', tam_y2_size, 'TAM Window 2 Y Size (pixels)'
;sxaddpar, tam_hdr, 'X1STNDBY', tam_x1_standby_window, 'TAM Standby Window 1 Lower Left Corner X'
;sxaddpar, tam_hdr, 'Y1STNDBY', tam_y1_standby_window, 'TAM Standby Window 1 Lower Left Corner Y'
;sxaddpar, tam_hdr, 'SDB1XSYZ', tam_x1_standby_size, 'TAM Standby Window 1 X Size (pixels)'
;sxaddpar, tam_hdr, 'SDB1YSYZ', tam_y1_standby_size, 'TAM Standby Window 1 Y Size (pixels)'
sxaddpar, tam_hdr, 'ADDR', strtrim(string(addr,format='(''0x'',z8.8)'),2), 'TAM Static Structure Address'
sxaddpar, tam_hdr, 'RS422_ID', rs422_id, 'TAM RS422 File Descriptor'
sxaddpar, tam_hdr, 'RS422PRT', rs422_port, 'TAM RS422 Port'
sxaddpar, tam_hdr, 'TAM_ERR', strtrim(string(tam_error,format='(''0x'',z8.8)'),2), 'Last TAM Error'
for ijk=0,14 do begin
	sxaddpar, tam_hdr, 'REG'+strtrim(string(ijk),2), $
		strtrim(string(tam_registers[ijk],format='(''0x'',z8.8)'),2), $
		'Contents of TAM register '+strtrim(string(ijk),2)
endfor



; Reset the cmprssd flag
cmprssd = 0
return

end
