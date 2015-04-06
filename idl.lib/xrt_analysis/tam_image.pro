pro tam_image, header_id, buffer_pointer, error_code
;
;name	: tam_image
;author	: Dave Burrows
;date	: 04/07/02
;lang	: IDL

;purpose: This routine unpacks and processes TAM images (full-frame format).
;	The data are stored in on output in standard FITS image files.
;
;
;	It returns an error code which is 0 if no errors are found:
;		error_code	
;		    0	no errors
;
;  Modifications:
;	06/03/04 by DNB: deleted frame centroid stuff - not relevant
;	06/03/04 by DNB: fixed bug that resulted in printing error message
;		for every pixel in file if one gain error was found
;	05/27/04 by DNB: added support for binary table FITS file
;	06/14/03 by DNB: fixed numerous logic errors needed to process data
;		file that doesn't begin with TAM header.
;	05/12/03 by DNB: changed checks on gain values in pixels to count
;		number of errors
;	04/10/03 by DNB: modified to process TAM image data
;	09/02/02 by DNB: based on windowed_timing.pro
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

@tam_common

window_mode = 0

n_calls = n_calls + 1

error_code = 0

pixel = bytarr(4)

cmprssd = 0				; flag for compressed frame
	
tam_frame = tam_frame + 1

print,'Processing TAM Frame # ', strtrim(string(tam_frame),2)
printf,lulog,'Processing TAM Frame # ', strtrim(string(tam_frame),2), $
		' at buffer offset ', buffer_pointer

; Process the LDP header
seq_cntl = 0U
packet_length = 0U
time_sec = 0UL
time_usec = 0U
product_num = 0U
page_num = 0U
checksum = 0U
ccsds_sec = 0UL
old_sec = ccsds_sec
ccsds_subsec = 0UL
old_subsec = ccsds_subsec
byte1 = 0b
byte2 = 0b
page_count = page_count + 1
;repeat begin
	; Read rest of CCSDS header
	readu,luin,byte1,byte2
	num_CCSDS_packets_read = num_CCSDS_packets_read + 1
	seq_cntl = byte1*256U+byte2
	control = seq_cntl and 'C000'x
	if (control ne 'C000'x) then stop, 'TAM_IMAGE: Error in file format: invalid control bits'
	sequence = seq_cntl and '3FFF'x
	if (sequence ne old_seq+1) then begin
		print,' '
		print_error,0,1,'TAM_IMAGE: *******************************'
		print_error,0,1,'TAM_IMAGE: Error in CCSDS packet sequence'
		print_error,0,1,'TAM_IMAGE: Last packet number: ' $
			+ string(old_seq)
		print_error,0,1,'TAM_IMAGE: This packet number: ' $
			+ string(sequence)
		print_error,0,1,'TAM_IMAGE: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
		print_error,0,1,'TAM_IMAGE: *******************************'
; TBD		stop, 'TAM_IMAGE: Error in CCSDS packet sequence'
	endif
	readu,luin,byte1,byte2
	packet_length = byte1*256U+byte2
	if (idebug ge 25) then begin
		print,'TAM_IMAGE: ---------------------------------
		print,'TAM_IMAGE: reading CCSDS packet #', sequence
		printf,lulog,'TAM_IMAGE: ---------------------------------
		printf,lulog,'TAM_IMAGE: reading CCSDS packet #', sequence
	endif
	if (idebug ge 30) then begin
		print,FORMAT='(A,3Z10,I10)',$
				'TAM_IMAGE: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'TAM_IMAGE: packet length = ', packet_length
		printf,lulog,FORMAT='(A,4(''  0x'',Z4.4))',$
				'TAM_IMAGE: CCSDS ID, seq_cntl, control, Seq = ', $
				header_id, seq_cntl, control, sequence
		printf,lulog,'TAM_IMAGE: packet length = ', packet_length
	endif
	old_seq = sequence
	if (old_seq eq 16383) then old_seq = -1  ; sequence #s are mod 16384

	; Read the rest of the data into a buffer
	buffer_size = (packet_length+1)
	data_buffer = bytarr(buffer_size)
	buffer_pointer = 0
	readu,luin,data_buffer

	; Now unpack the secondary CCSDS header (time)
	ccsds_sec = extract_long(buffer_pointer)
	ccsds_subsec = extract_int(buffer_pointer)

	ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
	tam_time = ccsds_time

	if (idebug ge 30) then begin
		print,'TAM_IMAGE: CCSDS seconds    =  ', ccsds_sec
		print,'TAM_IMAGE: CCSDS subseconds =  ', ccsds_subsec
		print,'TAM_IMAGE: CCSDS time =  ', ccsds_time
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
			'TAM_IMAGE: CCSDS seconds    =  ', ccsds_sec, ccsds_sec
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
			'TAM_IMAGE: CCSDS subseconds =  ', ccsds_subsec, ccsds_subsec
		printf,lulog,'TAM_IMAGE: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec


	; Calculate the checksum
	if (idebug ge 30) then help,checksum
	checksum = fix(total(data_buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 30) then begin
		print, 'TAM_IMAGE: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'TAM_IMAGE: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum
	;checksum = byte(check,2)*256UL + byte(check,3) 	; extract lowest 2 bytes
	; Now get checksum value from file
	chksum = data_buffer[buffer_size-2]*256U + data_buffer[buffer_size-1]

	if (idebug ge 30) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','TAM_IMAGE: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','TAM_IMAGE: Calculated checksum:', checksum
	endif

	; Verify checksum
	if (chksum ne checksum) then begin
	    print_error,0,1,'TAM_IMAGE: Checksum error in file '+filein
	    print_error,0,1,'TAM_IMAGE:	Checksum error in CCSDS header '+strtrim(string(sequence),2)
	    print_error,0,1,'TAM_IMAGE: Checksum in file: '+string(chksum)
	    print_error,0,1,'TAM_IMAGE: Calculated checksum: '+string(checksum)
	    print_error,0,1,'TAM_IMAGE: Error found in product #'+string(product_num)+' page #'+string(page_num)
	    stop, 'TAM_IMAGE: HALT: Checksum error in first CCSDS header'
	endif


	process_tam_image_hdr, buffer_pointer, error_code

	if (error_code ne 0) then return
;endrep until (error_code eq 0)

;iframe = iframe + 1
;tamtime[iframe] = tam_image_time
;frame_count[iframe] = tam_frame
;targetid[iframe] = target_id
;obsseg[iframe] = obs_segment
;tamx1[iframe] = tam_x1
;tamy1[iframe] = tam_y1
;tamx2[iframe] = tam_x2
;tamy2[iframe] = tam_y2
;tamsig1[iframe] = tam_sig1
;tamsig2[iframe] = tam_sig2

if (N_events lt 1) then return
if (N_events ne 512UL*512UL) then stop, 'TAM_IMAGE: image size error'

events_counter = 0

; now read the TAM data and put it into the buffer array

data_buffer_size = n_elements(data_buffer)

gain_errors = 0
for iy = 0,511 do begin
    for ix = 0,511 do begin
	; first check to see whether we are at the end of the data
	if (buffer_pointer gt (data_buffer_size-4)) then begin	;ignore checksum
	    append_tam_data_block, buffer_pointer 
	endif

	; if there are more than 2 bytes left in the file, unpack the
	;	next X-ray event.  

	pixel = extract_int(buffer_pointer)	
	image[ix,iy] = uint(pixel and '03FF'X)
	flags = uint(pixel and 'FC00'X)/256
	window_num = flags/128
	first_in_window = (flags and '40'xb)/64
	first_in_line = (flags and '20'xb)/32
	aps_gain = (flags and '18'xb)/8
	if (first_in_line and (ix ne 0)) then print_error,0,1, $
		'TAM_IMAGE: found BOL flag in wrong location ' $
		+ 'in frame ' + strtrim(string(iframe),2) $
		+ ': iy = ' + string(iy) + ',  ix = ' + string(ix)
	if (first_in_window and ((ix ne 0)or(iy ne 0))) then print_error,0,1, $
		'TAM_IMAGE: found BOW flag in wrong location ' $
		+ 'in frame ' + strtrim(string(iframe),2) $
		+ ': iy = ' + string(iy) + ',  ix = ' + string(ix)
	if (aps_gain ne gain) then begin
	    gain_errors = gain_errors + 1
 	    if ((gain_errors ge 1)and(gain_errors le 10)) then begin
		print_error,0,1, $
		'TAM_IMAGE: gain in header (' $
		+ strtrim(string(gain),2) + ') <> gain in pixels (' $
		+ strtrim(string(aps_gain),2) + ') for frame ' $
		+ strtrim(string(tam_frame),2)
	    endif
	endif
    endfor
endfor
if (gain_errors gt 1) then print_error,0,1,$
	'TAM_IMAGE: ' + strtrim(string(gain_errors),2) $
	+ ' gain errors found in TAM image # ' + strtrim(string(tam_frame),2)
  
End_of_Data_Frame:

; Now read in the redundant LDP header

append_tam_data_block, buffer_pointer



PROCESS_FRAME:

print_debug,0,'TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM'
print_debug,0,'*******************************************'

; write TAM image FITS file
if (idebug gt 9) then printf, lulog,'tam_frame, first_frame = ', tam_frame, first_frame


write_tam_image


return

end
