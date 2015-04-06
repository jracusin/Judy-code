pro modechange, filename, showplot, debug_level, DEBUG5=debug5, $
	NOINIT=noinit, noprint=noprint,quicklook=quicklook
; $Id: stripchart

;name	: modechange
;author	: Dave Burrows
;date	: 03/02/2004
;lang	: IDL

;purpose: This program reads an ITOS Mode Change file (Ap_ID 0x486 or
;		0x5E6) and creates a 2-D FITS file
;
; Rev:
;   07/06/04 by JLR: added quicklook keyword
;	05/28/04 by JLR: made keyword noprint work
;	03/10/04 by DNB: added printout to log file
;	04/01/04 by DNB: removed most prints to screen
;
;

@pass1_common

@pass1_path_init	; initialize paths for input files
version = 'V1.1'

if keyword_set(quicklook) then quick_look_mode=1 else quick_look_mode = 0

common save_data,oldwaveform,n_frames	; saves this for next entry

if not (keyword_set(noinit)) then begin
@pass1_init	; initialize common block variables.
endif

GET_LUN, luin		; Get Logical Unit Numbers for input and output files

if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
	filein = pickfile(title='Select XRT Mode Change data file: ', $
		filter='*modechange*', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
endif else begin
	filein = filename
endelse
filebase = filein

if (n_params() lt 2) then show_plot = 1 $
else show_plot = showplot

if (n_params() lt 3) then idebug = 0 $
else idebug = debug_level

if keyword_set(debug5) then idebug = 5

print_plots = 1
if keyword_set(noplots) then begin
	print_plots = 0
	show_plot = 1
	show_ccd_image = 1
	show_hk_image = 1
endif

if keyword_set(noprint) then print_plots=0

; Check that file exists

openr,luin,filein,error=file_error
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
;	read,'Enter name of XRT Mode Change input file: ', filein
	filein = pickfile(title='Select XRT Mode Change data file: ', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
	openr,luin,filein,error=file_error
endrep until (file_error eq 0)
close,luin

GET_LUN, lulog
openw,lulog,filein+'.modechange.log'

GET_LUN, luerr
openw,luerr,filein+'.modechange.err'


; "declare" variable types, initialize variables
error_code = 0
last_time = 0.0

header_id = 0U
seq_cntl = 0U
packet_length = 0U
time_sec = 0L
time_usec = 0U
product_num = 0U
page_num = 0U
checksum = 0U
ccsds_sec = 0L
old_sec = ccsds_sec
ccsds_subsec = 0L
old_subsec = ccsds_subsec
ccsds_id1 = '0C86'x	; RT HK version
ccsds_id2 = '0D36'x	; TDRSS version

byte1 = 0B
byte2 = 0B

; Open input file
get_lun,luin
openr,luin,filein,error=file_error,/swap_if_little_endian
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
	read,'Enter name of XRT Mode Change input file: ', file_in
	openr,luin,file_in,error=file_error,/swap_if_little_endian
endrep until (file_error eq 0)

; Read the file and process the data

print,' '
print,'Reading the XRT Mode Change file ...'
print,' '

printf,lulog, 'Mode Change message log for file ', filein
printf,lulog, '    processed with MODECHANGE ' + version + ' on ', runtime
printf,lulog, ' '

;print,format='(12x,a24,a20,4a8,8a5)', 'Data GMT', $
;	'Satellite Time (s)', 'RA (d)',$
;	'Dec (d)', 'Roll', 'cps ', 'State', ' Mode', ' Wvfm', $
;	'  ACS', ' Stld', '  10m', '  SAA', ' Safe'

printf,lulog,format='(12x,a24,a20,4a8,8a5)', 'Data GMT', $
	'Satellite Time (s)', 'RA (d)',$
	'Dec (d)', 'Roll', 'cps ', 'State', ' Mode', ' Wvfm', $
	' ACS', ' Stld', '  10m', '  SAA', ' Safe'


packet_num = 0
repeat begin
	packet_num = packet_num + 1
;
; Search for a valid CCSDS packet header ID
;	WARNING: this technique fails if there is an offset by an
;	odd number of bytes!  (This situation should never occur).
; This routine only processes Ap_IDs corresponding to HK packets.
; Other Ap_IDs are ignored.
;
	extra_bytes = -2
	while ((header_id ne ccsds_id1)and(header_id ne ccsds_id2)) do begin
		readu,luin,byte1,byte2
		header_id = byte1*256+byte2
		extra_bytes = extra_bytes + 2
	endwhile
	if (idebug ge 5) then print,extra_bytes,' extra bytes before packet ', $
		packet_num

	; Read rest of CCSDS header
	readu,luin,byte1,byte2
	seq_cntl = byte1*256+byte2
	control = seq_cntl and 'C000'x
	if (control ne 'C000'x) then stop, 'MODECHANGE: Error in file format: invalid control bits'
	sequence = seq_cntl and '3FFF'x
	readu,luin,byte1,byte2
	packet_length = byte1*256+byte2
	if (idebug ge 10) then begin
		print,'MODECHANGE: ---------------------------------
		print,'MODECHANGE: reading CCSDS packet #', sequence
		print,FORMAT='(A,3Z10,I10)',$
				'MODECHANGE: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'MODECHANGE: packet length = ', packet_length
		printf,lulog,'MODECHANGE: ---------------------------------
		printf,lulog,'MODECHANGE: reading CCSDS packet #', sequence
		printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
				'MODECHANGE: CCSDS ID, control, Seq = ', header_id,$
				control, sequence
		printf,lulog,'MODECHANGE: packet length = ', packet_length
	endif
	old_seq = sequence
	if (old_seq eq 16383) then old_seq = -1  ; sequence #s are mod 16384


	; Read the rest of the data into a buffer
	buffer_size = (packet_length+1)
	buffer = bytarr(buffer_size)
	readu,luin,buffer

	; Now unpack the secondary CCSDS header (time)
	ccsds_sec = buffer[0]*65536UL*256UL + buffer[1]*65536UL + buffer[2]*256UL + buffer[3]
	ccsds_subsec = buffer[4]*256UL + buffer[5]
	ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
	if (idebug ge 10) then begin
		print,'MODECHANGE: CCSDS time =  ', ccsds_time
		printf,lulog,'MODECHANGE: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; Calculate the checksum
	if (idebug ge 10) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 10) then begin
		print, 'MODECHANGE: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'MODECHANGE: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum

	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 10) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','MODECHANGE: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','MODECHANGE: Calculated checksum:', checksum
	endif


; Extract tertiary header info and check for format errors

	data_buffer = buffer
	buffer_pointer = 6
	observation_number = extract_long(buffer_pointer)

	XRT_sec = extract_long(buffer_pointer)
	XRT_subsec = extract_int(buffer_pointer)
	satellite_time = XRT_sec + XRT_subsec*20.0D-6
	if (idebug ge 5) then print,'Satellite time is ', satellite_time

	if (satellite_time lt last_time) then begin
		print_error,0,1, $
		    'MODECHANGE: WARNING: Satellite time decreased!'
		print_error,0,1, 'MODECHANGE:      Last time =  ' $
			+ string(last_time)
		print_error,0,1, 'MODECHANGE:      Current time=' $
			+ string(satellite_time)
	endif

	last_time = satellite_time

	UTC_sec = long(extract_long(buffer_pointer))
	UTC_subsec = extract_int(buffer_pointer) 
	UTC_delta = UTC_sec + UTC_subsec*20.0D-6

	ra = extract_float(buffer_pointer)
	dec = extract_float(buffer_pointer)
	roll = extract_float(buffer_pointer)

	acs_flags = bytarr(4)
	acs_flag_word = extract_byte(buffer_pointer)
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
	endif else is_settled = 'F'

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
		2: readout_mode = 'Short Image'
		3: readout_mode = 'Long Image'
		4: readout_mode = 'Piled-up Photodiode'
		5: readout_mode = 'Low Rate Photodiode'
		6: readout_mode = 'Windowed Timing'
		7: readout_mode = 'Photon-Counting'
		8: readout_mode = 'Raw Data'
		9: readout_mode = 'Bias Map'
		10: readout_mode = 'Stop'
		else: readout_mode = 'Undefined'
	endcase

	waveform = fix(extract_byte(buffer_pointer))
	if ((waveform lt 0)or(waveform gt 255)) then begin
		if (n_elements(oldwaveform) eq 0) then begin
			print,''
			print,'MODECHANGE: *** INVALID WAVEFORM NUMBER FOUND: ', waveform, ' ***'
			print,'waveform=1-127 for amp 1, waveform=128-255 for amp 2'
			read,'Enter waveform number for this CCD frame: ', waveform
			oldwaveform = waveform
		endif else waveform = oldwaveform
	endif
	if (waveform lt 128) then amp=1 else amp=2	

	count_rate = extract_float(buffer_pointer)

	; Now, process HK data 
	if (process_hk eq 0) then begin		; open HK file
		process_hk = 1
		get_lun,lumc
		file_hk = filebase + '.internal_temp_mc_file'
		openw,lumc,file_hk,/swap_if_little_endian
	endif
	hk_array = fltarr(12)

	hk_array[0] = ra
	hk_array[1] = dec
	hk_array[2] = roll
	hk_array[3] = count_rate
	hk_array[4] = xrt_state
	hk_array[5] = xrt_mode
	hk_array[6] = waveform
	hk_array[7] = acs_flag_word
	hk_array[8:11] = acs_flags

	if (idebug ge 10) then begin
		print,''
		print,'Mode Change data:'
		print,format='("MODECHANGE: ",10(z6,1x))',hk_array
	endif

	gmt = itos_time(satellite_time,UTC_delta)
;	print,format='(a,a24,f20.6,4f8.2,8i5)', 'MODECHANGE: ', $
;		gmt, satellite_time, hk_array[0:3], fix(hk_array[4:11])
	printf,lulog,format='(a,a24,f20.6,4f8.2,8i5)', 'MODECHANGE: ', $
		gmt, satellite_time, hk_array[0:3], fix(hk_array[4:11])
	num_hk_records = num_hk_records + 1

	; write output data to file as unformatted binary data
	writeu,lumc,satellite_time,hk_array

	; reset header_id to force new read
	header_id = 0

endrep until (EOF(luin))
close,luin

print,''
print,'*******************************************************************'
print,'*******************************************************************'
print,'Done processing ', packet_num, ' CCSDS packets'

if (process_hk ne 0) then begin
	close,lumc
	print,' '
	print,'Processing data and writing Mode Change FITS file...'
	print, ' '
	process_modechange_data
endif
close,/all

end
