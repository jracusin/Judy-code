pro append_tam_data_block, buffer_pointer, error_code

; This routine reads in a CCSDS packet containing TAM data, checks the
;	data format, and returns the data in the data_buffer array 
;	(in common)
;
; Revised:
;	04/01/04 by DNB: fixed debug print statement labels to give correct 
;		program name
;	04/21/03 by DNB: fixed bugs and added more useful diagnostics when
;		routine encounters incomplete CCSDS header at end of file.
;

@pass1_common
@tam_common

bytes_read = -2
byte1 = 0b
byte2 = 0b
error_code = 0
repeat begin
    	readu,luin,byte1,byte2
    	ccsds_hdr_id = byte1*256U + byte2
	bytes_read = bytes_read+2
endrep until ((ccsds_hdr_id eq '0d41'x) or (ccsds_hdr_id eq '0d42'x))
num_CCSDS_packets_read = num_CCSDS_packets_read + 1
page_count = page_count + 1
print,format='(a,$)','.'
if (bytes_read gt 0) then print_error,0,1,'FILE FORMAT ERROR: ' $
	+ strtrim(string(bytes_read),2) $
	+ ' bytes before valid CCSDS Header ID found'
 
; Read rest of CCSDS header
readu,luin,byte1,byte2
seq_cntl = byte1*256U+byte2
control = seq_cntl and 'C000'x
if (control ne 'C000'x) then stop, 'APPEND_TAM_DATA_BLOCK: Error in file format: invalid control bits'
sequence = seq_cntl and '3FFF'x
if (sequence ne old_seq+1) then begin
	print,' '
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: *******************************'
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: Error in CCSDS packet sequence'
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: Last packet number: ' $
			+ string(old_seq)
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: This packet number: ' $
			+ string(sequence)
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: *******************************'
; TBD	stop, 'APPEND_TAM_DATA_BLOCK: Error in CCSDS packet sequence'
endif
readu,luin,byte1,byte2
packet_length = byte1*256U+byte2
if (idebug ge 25) then begin
	print,'APPEND_TAM_DATA_BLOCK: ---------------------------------
	print,'APPEND_TAM_DATA_BLOCK: reading CCSDS packet #', sequence
	printf,lulog,'APPEND_TAM_DATA_BLOCK: ---------------------------------
	printf,lulog,'APPEND_TAM_DATA_BLOCK: reading CCSDS packet #', sequence
endif
if (idebug ge 30) then begin
	print,FORMAT='(A,3Z10,I10)',$
			'APPEND_TAM_DATA_BLOCK: CCSDS ID, seq_cntl, control, Seq = ', ccsds_hdr_id,$
			seq_cntl, control, sequence
	print,'APPEND_TAM_DATA_BLOCK: packet length = ', packet_length
	printf,lulog,FORMAT='(A,4(''  0x'',Z4.4))',$
			'APPEND_TAM_DATA_BLOCK: CCSDS ID, seq_cntl, control, Seq = ', $
			ccsds_hdr_id, seq_cntl, control, sequence
	printf,lulog,'APPEND_TAM_DATA_BLOCK: packet length = ', packet_length
endif
old_seq = sequence
if (old_seq eq 16383) then old_seq = -1  ; sequence #s are mod 16384

; Read the rest of the data into a buffer
data_buffer_size = (packet_length+1)
data_buffer = bytarr(data_buffer_size)
readu,luin,data_buffer
buffer_pointer = 0

; Now unpack the secondary CCSDS header (time)
ccsds_sec = extract_long(buffer_pointer)
ccsds_subsec = extract_int(buffer_pointer)

ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
tam_time = ccsds_time

if (idebug ge 30) then begin
	print,'APPEND_TAM_DATA_BLOCK: CCSDS seconds    =  ', ccsds_sec
	print,'APPEND_TAM_DATA_BLOCK: CCSDS subseconds =  ', ccsds_subsec
	print,'APPEND_TAM_DATA_BLOCK: CCSDS time =  ', ccsds_time
	printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
		'APPEND_TAM_DATA_BLOCK: CCSDS seconds    =  ', ccsds_sec, ccsds_sec
	printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
		'APPEND_TAM_DATA_BLOCK: CCSDS subseconds =  ', ccsds_subsec, ccsds_subsec
	printf,lulog,'APPEND_TAM_DATA_BLOCK: CCSDS time =  ', ccsds_time
endif
ccsds_time_old = ccsds_time
old_sec = ccsds_sec
old_subsec = ccsds_subsec

; Now check the LDP number and page number
	ldp_num = extract_int(buffer_pointer)
	ldp_page = extract_int(buffer_pointer)
	if (ldp_num ne ldp_num_old) then begin
		print_error,0,1,'APPEND_TAM_DATA_BLOCK: found unexpected LDP number: ' $
			+ 'expected ' + strtrim(string(ldp_num_old),2) $
			+ ', found ' + strtrim(string(ldp_num),2)
		stop,'Unrecoverable data format error...'
	endif
	if ((ldp_page ne ldp_page_old+1)and(ldp_page ne 1)) then begin
		print_error,0,1,'APPEND_TAM_DATA_BLOCK: found unexpected LDP page: ' $
			+ 'expected ' + strtrim(string(ldp_page_old+1),2) $
			+ ', found ' + strtrim(string(ldp_page),2)
		stop,'Unrecoverable data format error...'
	endif
	ldp_num_old = ldp_num
	ldp_page_old = ldp_page


; Calculate the checksum
if (idebug ge 30) then help,checksum
checksum = fix(total(data_buffer[6:data_buffer_size-3]),type=12) ; exclude checksum word
if (idebug ge 30) then begin
	print, 'APPEND_TAM_DATA_BLOCK: Total of all bytes outside CCSDS headers: ', checksum
	printf,lulog, 'APPEND_TAM_DATA_BLOCK: Total of all bytes outside CCSDS headers: ', checksum
endif
check = checksum
;checksum = byte(check,2)*256UL + byte(check,3) 	; extract lowest 2 bytes
; Now get checksum value from file
chksum = data_buffer[data_buffer_size-2]*256U + data_buffer[data_buffer_size-1]

if (idebug ge 30) then begin
	help,checksum
	help,chksum
	print,FORMAT='(A,Z10)','APPEND_TAM_DATA_BLOCK: Checksum in file is ', chksum
	print,FORMAT='(A,Z10)','APPEND_TAM_DATA_BLOCK: Calculated checksum:', checksum
endif

; Verify checksum
if (chksum ne checksum) then begin
    print_error,0,1,'APPEND_TAM_DATA_BLOCK: Checksum error in file '+filein
    print_error,0,1,'APPEND_TAM_DATA_BLOCK:	Checksum error in CCSDS header '+strtrim(string(sequence),2)
    print_error,0,1,'APPEND_TAM_DATA_BLOCK: Checksum in file: '+string(chksum)
    print_error,0,1,'APPEND_TAM_DATA_BLOCK: Calculated checksum: '+string(checksum)
    print_error,0,1,'APPEND_TAM_DATA_BLOCK: Error found in product #'+string(ldp_num)+' page #'+string(ldp_page)
    if (chksum eq 0) then begin
	print_error,0,1,'APPEND_TAM_DATA_BLOCK: Checksum in file is zero - may be incomplete CCSDS header'
	printf,lulog,format='(a,/,10(''0x'',z4.4,2x))','Buffer contents: ', $
		data_buffer
	error_code = 201
    endif else $
    	stop, 'APPEND_TAM_DATA_BLOCK: HALT: Checksum error in CCSDS header'
endif

return
end
