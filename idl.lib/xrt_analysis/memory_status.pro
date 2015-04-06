pro memory_status, filename, showplot, debug_level, DEBUG5=debug5, $
	NOINIT=noinit, PRINTPLOTS=printplots

;name	: memory_status
;author	: Dave Burrows
;date	: 04/06/2004
;lang	: IDL

;purpose: This program reads an ITOS Memory Status file (Ap_ID 0x481 or
;		0x5E1) and creates an output text file.
;
; Rev:
;	03/10/04 by DNB: added printout to log file
;	04/01/04 by DNB: removed most prints to screen
;
;

@pass1_common

@pass1_path_init	; initialize paths for input files
version = 'V1.1'
quick_look_mode = 0

common save_data,oldwaveform,n_frames	; saves this for next entry

if not (keyword_set(noinit)) then begin
@pass1_init	; initialize common block variables.
endif

GET_LUN, luin		; Get Logical Unit Numbers for input and output files

if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
	filein = pickfile(title='Select XRT Memory Status data file: ', $
		filter='*memorystatus*', /must_exist)
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

; Check that file exists

openr,luin,filein,error=file_error
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
	filein = pickfile(title='Select XRT Memory Status data file: ', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
	openr,luin,filein,error=file_error
endrep until (file_error eq 0)
close,luin

GET_LUN, lulog
openw,lulog,filein+'.memory_status.log'

GET_LUN, luerr
openw,luerr,filein+'.memory_status.err'


; "declare" variable types, initialize variables
error_code = 0
last_time = 0.0
last_sbe = 0
last_mbe = 0
numerr = 0

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
ccsds_id1 = '0C81'x	; RT HK version
ccsds_id2 = '0D31'x	; TDRSS version

byte1 = 0B
byte2 = 0B
edac = intarr(11)
log_tcb = intarr(11)

; Open input file
get_lun,luin
openr,luin,filein,error=file_error,/swap_if_little_endian
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
	read,'Enter name of XRT Memory Status input file: ', file_in
	openr,luin,file_in,error=file_error,/swap_if_little_endian
endrep until (file_error eq 0)

; Read the file and process the data

print,' '
print,'Reading the XRT Memory Status file ...'
print,' '
print,'                       GMT                    S/C Time (s)   # Single    # Mult   bytes scrubbed'

printf,lulog, 'Memory Status message log for file ', filein
printf,lulog, '    processed with MEMORY_STATUS ' + version + ' on ', runtime
printf,lulog, ' '
printf,lulog,'                       GMT                    S/C Time (s)   # Single    # Mult   bytes scrubbed'


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
	if (control ne 'C000'x) then stop, 'MEMORY_STATUS: Error in file format: invalid control bits'
	sequence = seq_cntl and '3FFF'x
;	print,'Reading packet # ', packet_num
	printf,lulog,'Reading packet # ', packet_num, '     CCSDS packet #', sequence

	readu,luin,byte1,byte2
	packet_length = byte1*256+byte2
	if (idebug ge 10) then begin
		print,'MEMORY_STATUS: ---------------------------------
		print,'MEMORY_STATUS: reading CCSDS packet #', sequence
		print,FORMAT='(A,3Z10,I10)',$
				'MEMORY_STATUS: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'MEMORY_STATUS: packet length = ', packet_length
		printf,lulog,'MEMORY_STATUS: ---------------------------------
		printf,lulog,'MEMORY_STATUS: reading CCSDS packet #', sequence
		printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
				'MEMORY_STATUS: CCSDS ID, control, Seq = ', header_id,$
				control, sequence
		printf,lulog,'MEMORY_STATUS: packet length = ', packet_length
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
		print,'MEMORY_STATUS: CCSDS time =  ', ccsds_time
		printf,lulog,'MEMORY_STATUS: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; Calculate the checksum
	if (idebug ge 10) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 10) then begin
		print, 'MEMORY_STATUS: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'MEMORY_STATUS: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum

	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 10) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','MEMORY_STATUS: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','MEMORY_STATUS: Calculated checksum:', checksum
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
		    'MEMORY_STATUS: WARNING: Satellite time decreased!'
		print_error,0,1, 'MEMORY_STATUS:      Last time =  ' $
			+ string(last_time)
		print_error,0,1, 'MEMORY_STATUS:      Current time=' $
			+ string(satellite_time)
	endif

	last_time = satellite_time

	UTC_sec = long(extract_long(buffer_pointer))
	UTC_subsec = extract_int(buffer_pointer) 
	UTC_delta = UTC_sec + UTC_subsec*20.0D-6

	single_bit = extract_long(buffer_pointer)
	last_sb_error = extract_long(buffer_pointer)
	last2_sb_error = extract_long(buffer_pointer)

	multiple_bit = extract_long(buffer_pointer)
	last_mb_error = extract_long(buffer_pointer)
	last2_mb_error = extract_long(buffer_pointer)

	double_words = extract_long(buffer_pointer)

	for i=0,10 do begin
		edac[i] = extract_int(buffer_pointer)
	endfor
	for i=0,10 do begin
		log_tcb[i] = extract_int(buffer_pointer)
	endfor

	gmt = itos_time(satellite_time)
	bytes_read = double_words*8L

	print,format='(a,a24,f20.6,3i10)', 'MEMORY_STATUS: ', $
		gmt, satellite_time, single_bit, multiple_bit, bytes_read
	printf,lulog,format='(a,a24,f20.6,3i10)', 'MEMORY_STATUS: ', $
		gmt, satellite_time, single_bit, multiple_bit, bytes_read

;	if ((packet_num gt 1) and $
;	    ((single_bit ne last_sbe)or(multiple_bit ne last_mbe))) then $
	if ((single_bit ne 0) or (multiple_bit ne 0)) then begin
		printf,luerr,format='(a,a24,f20.6,3i10)', 'MEMORY_STATUS: ', $
			gmt, satellite_time, single_bit, multiple_bit, double_words
		numerr = numerr + 1
	endif

	last_sbe = single_bit
	last_mbe = multiple_bit

	; reset header_id to force new read
	header_id = 0

endrep until (EOF(luin))
close,luin

print,''
print,'*******************************************************************'
print,'*******************************************************************'
print,'Done processing ', packet_num, ' CCSDS packets'
if (numerr eq 0) then print, '        NO MEMORY ERRORS FOUND' $
else print, '        ***********', numerr, ' MEMORY ERRORS FOUND ***********'
print,''

close,/all

end
