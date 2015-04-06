pro strip_hdr, ccsds_id, file_in, file_out

; $Id: strip_hdr.pro 1.0 2001/11/05 17:25:30 burrows Exp $

;name	: strip_hdr
;author : Dave Burrows
;date	: 11/05/01
;lang	: IDL

;purpose: This routine removes CCSDS and LDP headers from input data
;	file of XRT data.  Headers are checked for correct format.
;	Routine terminates if errors are found.
;
;	Input:
;		ccsds_id = value of CCSDS header ID word (16 bits)
;		file_in = ASCII name of input data file
;	Output:
;		file_out = ASCII name of output data file (with CCSDS and LDP
;			headers removed).
;
; Rev:
;	08/18/04 by DNB: fixed another bug in kludge for build 8.7 processing: had to add code to
;			deal with case where LRPD frame has packet size 151 (no bias pixels).
;	08/18/04 by DNB: fixed bug that was changing byte1 and byte2 to integers.
;	08/18/04 by DNB: modified to patch up Build 8.8 telemetry so PASS1 can process it correctly.
;		The intent in Build 8.8 was that the bias_threshold would be a flag indicating whether
;			not a bias value was subtracted from the data, and whether or not this 
;			subtracted bias value was added into the header in two extra bytes.
;			The former was implemented in Build 8.8, but the latter was not: the two
;			extra bytes are always in the header in Build 8.8, no matter what the value
;			of the bias_threshold is.  PASS1 therefore cannot use the bias_threshold to 
;			determine whether the data format is from Build 8.7 and earlier, or Build 8.8
;			and later.  However, you can tell which format you have, based on the size
;			of the header.  This information is only available in strip_hdr, since it 
;			strips off the CCSDS header information.  Therefore, I have modified strip_hdr
;			to recognize CCSDS packets from LRPD frames made in Build 8.7 and earlier, 
;			and to insert the two bias_level bytes.  (This violates the 
;			overall design philosophy of strip_hdr, which is that it only knows about the
;			CCSDS packet structure, not the LDP structure, but it's necessary to process
;			the data correctly.)
;	04/21/04 by DNB: added variables to track first and last CCSDS packet times and
;				to implement UTC corrections in GMT times
;	12/17/03 by DNB: added observation number in hex to timeline print statement
;	12/16/03 by DNB: commented out one print statement (duplicates info in log file that
;		is in timeline file, clutters up log file, so deleted from log file).
;	06/04/03 by DNB: fixed bug caused by dropping incomplete CCSDS packets but then
;		dropping the next copy because the page number is wrong.
;	06/04/03 by DNB: modified logic for processing out-of-sequence CCSDS packets to 
;		deal with DITL data.  Data files contain multiple SSR downloads,
;		so they have incomplete CCSDS packet at end of one download, 
;		followed by repeated packets at beginning of next.  STRIP_HDR
;		now tries to ignore the incomplete packet and the repeated pages.
;	05/19/03 by DNB: changed frame times to T_start instead of CCSDS time.
;	05/15/03 by DNB: changed format of timeline output; added times
;	05/13/03 by DNB: small change in print format
;	04/10/03 by DNB: changed so it can successfully find packet ID
;		offset by odd number of bytes.
;	04/07/03 by DNB: minor changes to error messages
;	09/06/02 by DNB: modified so strip_hdr will work correctly with files
;		that have no snapshot header at the beginning.
;	08/08/02 by DNB: added code to permit PASS1 to process files with multiple
;		LDPs with the same number, which sometimes happens during testing if
;		instrument is rebooting in the middle of an ITOS archive.
;	03/25/02 by DNB: added some extra debug information, modified to accept
;		CCSDS format errors (i.e., if header ID is not found where
;		expected, program will try to continue)
;	03/13/02 by DNB: changed input to read file byte-by-byte so code will
;		work on both Sun workstations and PC.  Also had to change
;		calculation of checksum for same reason (changed from L to U).
;	01/26/02 by DNB: converted time calculations to double precision
;
; $Log: strip_hdr.pro $
;
;
;
; This program reads data from input file, one CCSDS record at a time,
;	and writes modified file to output file, with CCSDS and LDP headers
;	removed.  It assumes that data are in correct order, and halts if
;	an error is found.

@pass1_common

utc = 0.0d
last_read_time = 0.0D0
product_num_old = 0
page_num_old = 0
snapshot_num_old = 0

acs_flags = intarr(4)

; Set up code to permit processing of data sets that include instrument restarts
restart_count = 0
flag = ['','A','B','C','D','E','F','G','H','I','J','K','L','M','N','P', $
			'Q','R','S','T','U','V','W','X','Y','Z']

; Open files
get_lun,luin
openr,luin,file_in,error=file_error,/swap_if_little_endian
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
	read,'Enter name of XRT input file: ', file_in
	openr,luin,file_in,error=file_error,/swap_if_little_endian
endrep until (file_error eq 0)

header_id = 0U
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
last_delta = 0

num_CCSDS_packets_read = 0UL
page1_count = 0
page_count = 0UL

byte1 = 0B
byte2 = 0B

; Read the first CCSDS packet

;repeat begin
	; Search for a valid CCSDS packet header ID

	readu,luin,byte1,byte2
	header_id = byte1*256U + byte2
	while (header_id ne ccsds_id) do begin
		byte1 = byte2
		readu,luin,byte2
		header_id = byte1*256U+byte2
	endwhile

	; Read rest of CCSDS header
	readu,luin,byte1,byte2
	seq_cntl = byte1*256U+byte2
	control = seq_cntl and 'C000'x
	if (control ne 'C000'x) then stop, 'STRIP_HDR: Error in file format: invalid control bits'
	sequence = seq_cntl and '3FFF'x
	num_CCSDS_packets_read = num_CCSDS_packets_read + 1
	readu,luin,byte1,byte2
	packet_length = byte1*256U+byte2
	if (idebug ge 25) then begin
		print,'STRIP_HDR: ---------------------------------
		print,'STRIP_HDR: reading CCSDS packet #', sequence
		printf,lulog,'STRIP_HDR: ---------------------------------
		printf,lulog,'STRIP_HDR: reading CCSDS packet #', sequence
	endif
	if (idebug ge 30) then begin
		print,FORMAT='(A,3Z10,I10)',$
				'STRIP_HDR: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'STRIP_HDR: packet length = ', packet_length
		printf,lulog,FORMAT='(A,4(''  0x'',Z4.4))',$
				'STRIP_HDR: CCSDS ID, seq_cntl, control, Seq = ', $
				header_id, seq_cntl, control, sequence
		printf,lulog,'STRIP_HDR: packet length = ', packet_length
	endif
	old_seq = sequence
	if (old_seq eq 16383) then old_seq = -1  ; sequence #s are mod 16384


	; Read the rest of the data into a buffer
	buffer_size = (packet_length+1)
	buffer = bytarr(buffer_size)
	readu,luin,buffer

	; Now unpack the secondary CCSDS header (time)
	ccsds_sec = buffer[0]*65536UL*256UL + buffer[1]*65536UL $
		+ buffer[2]*256UL + buffer[3]
	ccsds_subsec = buffer[4]*256UL + buffer[5]

	ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
	first_time = ccsds_time

	if (idebug ge 30) then begin
		print,'STRIP_HDR: CCSDS seconds    =  ', ccsds_sec
		print,'STRIP_HDR: CCSDS subseconds =  ', ccsds_subsec
		print,'STRIP_HDR: CCSDS time =  ', ccsds_time
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i20)',$
			'STRIP_HDR: CCSDS seconds    =  ', ccsds_sec, ccsds_sec
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i20)',$
			'STRIP_HDR: CCSDS subseconds =  ', ccsds_subsec, ccsds_subsec
	endif
	if (idebug ge 20) then begin
		printf,lulog,'STRIP_HDR: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; now unpack the product number and page number
	product_num = 0
	page_num = 0
	product_num = buffer[6]*256U + buffer[7]
	page_num = buffer[8]*256U + buffer[9]

	if (idebug ge 25) and (idebug lt 30) then begin
		print,format='(A,3Z6,Z10,Z6,2I4)', 'First 16 bytes: ',$
			header_id, seq_cntl, packet_length, ccsds_sec, $
			ccsds_subsec, product_num, page_num
		printf,lulog,format='(A,3Z6,Z10,Z6,2I4)', 'First 16 bytes: ',$
			header_id, seq_cntl, packet_length, ccsds_sec, $
			ccsds_subsec, product_num, page_num
	endif

	; Calculate the checksum
	if (idebug ge 30) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 30) then begin
		print, 'STRIP_HDR: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'STRIP_HDR: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum
	;checksum = byte(check,2)*256UL + byte(check,3) 	; extract lowest 2 bytes
	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 30) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','STRIP_HDR: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','STRIP_HDR: Calculated checksum:', checksum
	endif

	; Verify checksum
	if (chksum ne checksum) then begin
	    print_error,0,1,'STRIP_HDR: Checksum error in file '+file_in
	    print_error,0,1,'STRIP_HDR:	Checksum error in CCSDS header '+strtrim(string(sequence),2)
	    print_error,0,1,'STRIP_HDR: Checksum in file: '+string(chksum)
	    print_error,0,1,'STRIP_HDR: Calculated checksum: '+string(checksum)
	    print_error,0,1,'STRIP_HDR: Error found in product #'+string(product_num)+' page #'+string(page_num)
	    stop, 'STRIP_HDR: HALT: Checksum error in first CCSDS header'
	endif

	product_num_old = product_num
	page_num_old = page_num

	; Check rest of first packet, which should be a Snapshot header record
	if (packet_length eq 41) then begin
		pages_in_file = buffer[10]*256U + buffer[11]
		page_count = page_count + 1UL
		if (idebug ge 30) then begin
			print, 'STRIP_HDR: Product # = ', product_num,$
			    '   Page # = ', page_num, '   Pages in file = ', $
					pages_in_file
			printf,lulog, 'STRIP_HDR: Product # = ', product_num,$
			    '   Page # = ', page_num, '   Pages in file = ', $
					pages_in_file
		endif
		if (page_num eq 1) then page1_count = 1


		obs_seg = buffer[12]
		target_id = buffer[13]*256UL*256UL + buffer[14]*256UL + buffer[15]
		data_collection_sec = buffer[16]*65536UL*256UL + buffer[17]*65536UL + $
			buffer[18]*256UL + buffer[19]
		data_collection_subsec = buffer[20]*256UL + buffer[21]
		data_collection_time = double(data_collection_sec) $
			+ double(data_collection_subsec)*20.0D-6
		utc_sec = long(buffer[22]*65536UL*256UL + buffer[23]*65536UL + buffer[24]*256UL + buffer[25])
		utc_subsec = buffer[26]*256UL + buffer[27]
		utc = double(utc_sec) + double(utc_subsec)*20.0D-6

		snapshot_header_id = buffer[28]*65536UL*256UL + buffer[29]*65536UL $
			+ buffer[30]*256UL + buffer[31]
		snapshot_num = buffer[32]*65536UL*256UL + buffer[33]*65536UL $
			+ buffer[34]*256UL + buffer[35]
		eot_marker = buffer[36]*65536UL*256UL + buffer[37]*65536UL $
			+ buffer[38]*256UL + buffer[39]
		gmt = itos_time(data_collection_time, utc)
		printf,luccsds,format='(a,i5,a,i5,a,i7,i5,a,f20.6,a,a)', 'Snapshot ', snapshot_num, $
			'     Total pages =', pages_in_file, $
			'     Target: ', target_id, obs_seg, $
			'     Time = ', data_collection_time, ' = ', gmt
		printf,luccsds,format='(2i5,f20.6,2i5,''     0x'',z8.8)',sequence, $
			packet_length, ccsds_time, product_num, page_num, snapshot_header_id

	
		if (idebug ge 35) then begin
			print,'STRIP_HDR: Observation segment, Target ID = ', $
				obs_seg, target_id
			print,format='(a,''0x'',z8.8)', $
				'STRIP_HDR: Data collection time seconds = ', $
				data_collection_sec
			print,format='(a,''0x'',z8.8)',$
				'STRIP_HDR: Data collection time subsec  = ', $
				data_collection_subsec
			print,'STRIP_HDR: Data collection time = ', data_collection_time, ' s'
			print,format='(a,''0x'',z8.8)', $
				'STRIP_HDR: Data collection UTC seconds = ', utc_sec
			print,format='(a,''0x'',z8.8)',$
				'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
			print,format='(a,''0x'',z8.8)','STRIP_HDR: Snapshot Header ID = ', snapshot_header_id
			print,'STRIP_HDR: Snapshot count = ', snapshot_num
			print,format='(a,''0x'',z8.8)','STRIP_HDR: EOT marker = ', eot_marker
			print,'STRIP_HDR: repeat end logic = ', ((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))

			printf,lulog,'STRIP_HDR: Observation segment, Target ID = ', $
				obs_seg, target_id
			printf,lulog,format='(a,''0x'',z8.8)', $
				'STRIP_HDR: Data collection time seconds = ', $
				data_collection_sec
			printf,lulog,format='(a,''0x'',z8.8)',$
				'STRIP_HDR: Data collection time subsec  = ', $
				data_collection_subsec
			printf,lulog,'STRIP_HDR: Data collection time = ', data_collection_time, ' s'
			printf,lulog,format='(a,''0x'',z8.8)', $
				'STRIP_HDR: Data collection UTC seconds = ', utc_sec
			printf,lulog,format='(a,''0x'',z8.8)',$
				'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
			printf,lulog,format='(a,''0x'',z8.8)','STRIP_HDR: Snapshot Header ID = ', snapshot_header_id
			printf,lulog,'STRIP_HDR: Snapshot count = ', snapshot_num
			printf,lulog,format='(a,''0x'',z8.8)','STRIP_HDR: EOT marker = ', eot_marker
			printf,lulog,'STRIP_HDR: repeat end logic = ', ((snapshot_header_ID eq 	'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))
		endif

		if (snapshot_header_ID eq 'FEC07B92'xul) then begin
			print,'STRIP_HDR: found LDP header for Snapshot #', snapshot_num
			printf,lulog,'STRIP_HDR: found LDP header for Snapshot #', snapshot_num
			snapshot_num_old = snapshot_num
			printf, lutimeline, '*****************************************************************************'
			printf, lutimeline, '*****************************************************************************'
;
; Convert time to GMT
			gmt = itos_time(data_collection_time,utc)
			printf,lutimeline, format='(4a, 5x, 7a)',$
				'First LDP: Time = ', $
				string(data_collection_time, format='(f14.3)'), $
				' = ', gmt

			printf, lutimeline, format='(5a,i5,a,i6,a,i4,a,i,a,z8.8)', $
				'     Product # = ', strtrim(string(product_num),2), $
				'   Page # = ', strtrim(string(page_num),2), $
				'     Snapshot #: ', snapshot_num, $
				'   Target ID: ', target_id, $
				'   Obs Seg: ', obs_seg, $
				'   Obs Number: ', obs_seg*256UL*256UL*256UL+target_id, $
				' = 0x', obs_seg*256UL*256UL*256UL+target_id

		endif else begin
			print,' '
			print,FORMAT='(A,A,Z10)',$
				'STRIP_HDR: Error in Snapshot ID word; should be "FEC07B92", ',$
				'found ', snapshot_header_ID
;			print,'STRIP_HDR: Checking next packet for valid Snapshot ID ...'
			print,'Processing CCSDS frames anyway - check format carefully!'
			print,' '
			printf,lulog,' '
			printf,lulog,FORMAT='(A,A,Z10)',$
				'STRIP_HDR: Error in Snapshot ID word; should be "FEC07B92", ',$
				'found ', snapshot_header_ID
		;	printf,lulog,'STRIP_HDR: Checking next packet for valid Snapshot ID ...'
			printf,lulog,'Processing CCSDS frames anyway - check format carefully!'
			printf,lulog,' '
			snapshot_num_old = product_num
			;stop,'STRIP_HDR: Invalid input file format: does not begin with valid LDP header'
		endelse
	endif else begin
		print_error,0,1,'STRIP_HDR: File does not begin with LDP Header; packet_length = ' $
			+ strtrim(string(packet_length),2)
		printf,luccsds,format='(2i5,f20.6,2i5)',sequence, $
			packet_length, ccsds_time, product_num, page_num
	endelse
;	header_id = 0 ; reset this to force program to read it again
;endrep until ((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))


; Open output file
file_out = file_in + '_LDP' + strtrim(string(product_num),2) + '.sci'
get_lun,luout
openw,luout,file_out,/swap_if_little_endian

;writeu,luout,buffer[28:buffer_size-3]  ; delete headers and checksum

if (page_num eq 1) then writeu,luout,buffer[28:buffer_size-3] $
else begin

	; adjust buffer if this is an LRPD header using old FSW
	if (packet_length eq 211) then begin
		; this could be a LRPD frame header from build 8.7 or earlier
		if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
			(buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)) $	; LRPD
		then begin
			if (idebug ge 10) then begin
				printf,lulog,'got LRPD frame header'
				printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
				printf,lulog,' '
				printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
			endif
			buffer = [buffer[0:145],0b,0b,buffer[146:*]]
			buffer_size = buffer_size+2

			if (idebug ge 10) then begin
				printf,lulog,' '
				printf,lulog,' Here is the adjusted buffer: '
				printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
			endif
		endif
	endif

	; adjust buffer if this is an LRPD or PUPD header using old FSW
	if (packet_length eq 151) then begin
		; this could be an LRPD or PUPD frame header from build 8.7 or earlier
		if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
			(((buffer[12] eq '81'x) and (buffer[13] eq '9b'x)) $	; PUPD
			or ((buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)))) $ ; LRPD
		then begin
			if (idebug ge 10) then begin
				printf,lulog,'got PUPD frame header'
				printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
				printf,lulog,' '
				printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
			endif
			buffer = [buffer[0:145],0b,0b,buffer[146:*]]
			buffer_size = buffer_size+2

			if (idebug ge 10) then begin
				printf,lulog,' '
				printf,lulog,' Here is the adjusted buffer: '
				printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
			endif
		endif
	endif

	writeu,luout,buffer[10:buffer_size-3]
endelse

packet_num = 1
; read and process remaining LDP records until end of this file
repeat begin
	; Read next CCSDS header
	checksum = 0U
	readu,luin,byte1,byte2
	header_id = byte1*256U+byte2
	extra_bytes = 0UL
	while ((header_id ne ccsds_id)and(not EOF(luin))and(extra_bytes lt 100000)) do begin
		readu,luin,byte1,byte2
		header_id = byte1*256U+byte2
		extra_bytes = extra_bytes + 2UL
	endwhile
	if (header_id ne ccsds_id) then begin
		print,FORMAT='(/,A,Z10,A,I10)','STRIP_HDR: Found CCSDS ID = ', header_id, ' in CCSDS packet # ', packet_num
		print,FORMAT='(A,Z10)','STRIP_HDR: Expected CCSDS ID = ', ccsds_id
		printf,luerr,FORMAT='(/,A,Z10,A,I10)','STRIP_HDR: Found CCSDS ID = ', header_id, ' in CCSDS packet # ', packet_num
		printf,luerr,FORMAT='(A,Z10)','STRIP_HDR: Expected CCSDS ID = ', ccsds_id
		printf,lulog,FORMAT='(/,A,Z10,A,I10)','STRIP_HDR: Found CCSDS ID = ', header_id, ' in CCSDS packet # ', packet_num
		printf,lulog,FORMAT='(A,Z10)','STRIP_HDR: Expected CCSDS ID = ', ccsds_id
		;stop, 'STRIP_HDR: Error in APID'
		printf,lulog,'Terminating STRIP_HDR and processing data in PASS1: extra_bytes = ', extra_bytes
		goto, no_more_data
	endif
	readu,luin,byte1,byte2
	seq_cntl = byte1*256U+byte2
	readu,luin,byte1,byte2
	packet_length = byte1*256U+byte2
	packet_num = packet_num + 1
	control = seq_cntl and 'C000'x
	sequence = seq_cntl and '3FFF'x
	num_CCSDS_packets_read = num_CCSDS_packets_read + 1
	if (control ne 'C000'x) then begin 
		print,' '
		print_error,0,1,'STRIP_HDR1: in packet# ' + string(packet_num) $
			+ ', seq_cntl, control, sequence: ' + string(seq_cntl,format='(z4.4)') $
			+ ' ' + string(control,format='(z4.4)') + ' ' + string(sequence,format='(z4.4)')
		print_error,0,1,'STRIP_HDR1: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
		stop, 'STRIP_HDR1: Error in file format: invalid control: '
	endif
	if (idebug ge 25) then begin
		print,' '
		print,'STRIP_HDR2: ---------------------------------
		print,'STRIP_HDR2: reading CCSDS packet #', sequence
		printf,lulog,'STRIP_HDR2: ---------------------------------
		printf,lulog,'STRIP_HDR2: reading CCSDS packet #', sequence
	endif
	if (idebug ge 30) then begin
		print,FORMAT='(A,Z10,Z10,I10)',$
			'STRIP_HDR2: CCSDS ID, seq_cntl, control, Seq = ', $
			header_id, seq_cntl, control, sequence
		print,'STRIP_HDR2: packet length = ', packet_length
		printf,lulog,FORMAT='(A,4(''  0x'',Z4.4))',$
				'STRIP_HDR: CCSDS ID, seq_cntl, control, Seq = ', $
				header_id, seq_cntl, control, sequence
		printf,lulog,'STRIP_HDR2: packet length = ', packet_length
	endif
	if (sequence ne old_seq+1) then begin
		print,' '
		print_error,0,1,'STRIP_HDR3: *******************************'
		print_error,0,1,'STRIP_HDR3: Error in CCSDS packet sequence'
		print_error,0,1,'STRIP_HDR3: Last packet number: ' $
			+ string(old_seq)
		print_error,0,1,'STRIP_HDR3: This packet number: ' $
			+ string(sequence)
		print_error,0,1,'STRIP_HDR3: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
		print_error,0,1,'STRIP_HDR3: *******************************'
; TBD		stop, 'STRIP_HDR3: Error in CCSDS packet sequence'
	endif
	if (extra_bytes gt 0) then begin
		print_error,0,1,'STRIP_HDR4 WARNING: Found '+string(extra_bytes)+' extra bytes before start of CCSDS header #'+string(sequence)
		print_error,0,1,'STRIP_HDR4 WARNING: Possible data corruption'
		print_error,0,1,'STRIP_HDR4: This packet number: ' $
			+ strtrim(string(sequence),2)
		print_error,0,1,'STRIP_HDR4: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
	endif
	old_seq = sequence
	if (old_seq eq 16383) then old_seq = -1

	; Read the rest of the data into a buffer
	buffer_size = (packet_length+1)
	buffer = bytarr(buffer_size)
	readu,luin,buffer

	; Now unpack the secondary CCSDS header (time)
	ccsds_sec = 0UL
	ccsds_subsec = 0UL
	ccsds_sec = buffer[0]*65536UL*256UL + buffer[1]*65536UL + buffer[2]*256UL + buffer[3]
	ccsds_subsec = buffer[4]*256UL + buffer[5]
	ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
	last_time = ccsds_time

;
; Convert time to GMT
;
	
	gmt = itos_time(ccsds_time,utc)
	
	if (idebug ge 30) then begin
		print,'STRIP_HDR: CCSDS seconds    =  ', ccsds_sec
		print,'STRIP_HDR: CCSDS subseconds =  ', ccsds_subsec
		print,'STRIP_HDR: CCSDS time =  ', ccsds_time
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
			'STRIP_HDR: CCSDS seconds    =  ', ccsds_sec, ccsds_sec
		printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
			'STRIP_HDR: CCSDS subseconds =  ', ccsds_subsec, ccsds_subsec
	endif

;	del_time = ccsds_sec - old_sec + (ccsds_subsec - old_subsec)*20.0D-6
	del_time = ccsds_time - ccsds_time_old
	if (idebug ge 20) then begin
		printf,lulog,format='(a,f20.6,a,f20.6)','STRIP_HDR: CCSDS time =  ', ccsds_time, '  Delta = ', del_time
	endif
	if (del_time lt 0.0) then begin
		print,' '
		print_error,0,1,'STRIP_HDR5: Error in CCSDS time sequence in CCSDS packet '+strtrim(string(sequence),2)
		print,FORMAT='(A,2Z10)','STRIP_HDR5: Last packet time', $
			old_sec, old_subsec
		print,FORMAT='(A,2Z10)','STRIP_HDR5: This packet time', $
			ccsds_sec, ccsds_subsec
		printf,lulog,FORMAT='(A,2Z10)','STRIP_HDR5: Last packet time', $
			old_sec, old_subsec
		printf,lulog,FORMAT='(A,2Z10)','STRIP_HDR5: This packet time', $
			ccsds_sec, ccsds_subsec
		printf,luerr,FORMAT='(A,2Z10)','STRIP_HDR5: Last packet time', $
			old_sec, old_subsec
		printf,luerr,FORMAT='(A,2Z10)','STRIP_HDR5: This packet time', $
			ccsds_sec, ccsds_subsec
		print_error,0,1,'STRIP_HDR5: del_time = ' + string(del_time)
		print_error,0,1,'STRIP_HDR5: This packet number: ' $
			+ string(sequence)
		print_error,0,1,'STRIP_HDR5: cumulative CCSDS packet #' $
			+ strtrim(string(num_CCSDS_packets_read),2) $
			+ ', cumulative LDP page #' + strtrim(string(page_count),2)
; TBD		stop, 'STRIP_HDR5: Error in CCSDS time sequence'
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; now unpack the product number and page number
	product_num = 0U
	page_num = 0U
	product_num = buffer[6]*256U + buffer[7]
	page_num = buffer[8]*256U + buffer[9]
	page_count = page_count + 1UL

	if (idebug ge 25) and (idebug lt 30) then begin
		print,format='(A,3Z6,Z10,Z6,2I4)', 'First 16 bytes: ',$
			header_id, seq_cntl, packet_length, ccsds_sec, $
			ccsds_subsec, product_num, page_num
		printf,lulog,format='(A,3Z6,Z10,Z6,2I4)', 'First 16 bytes: ',$
			header_id, seq_cntl, packet_length, ccsds_sec, $
			ccsds_subsec, product_num, page_num
	endif

	if (idebug ge 30) then begin
		print, 'STRIP_HDR6: Product # = ', product_num,$
			'   Page # = ', page_num
		printf,lulog, 'STRIP_HDR6: Product # = ', product_num,$
			'   Page # = ', page_num
	endif
	if (product_num ne product_num_old) then begin
		print,' '
		if (page_num eq 1) then begin
			page1_count = 1
			; Check for valid LDP header ID
			pages_in_file = buffer[10]*256U + buffer[11]

			; Check rest of this packet, which should be a Snapshot header record
			obs_seg = buffer[12]
			target_id = buffer[13]*256UL*256UL + buffer[14]*256UL + buffer[15]
			data_collection_sec = buffer[16]*65536UL*256UL $
				+ buffer[17]*65536UL + buffer[18]*256UL + buffer[19]
			data_collection_subsec = buffer[20]*256UL + buffer[21]
			data_collection_time = double(data_collection_sec) $
				+ double(data_collection_subsec)*20.0D-6
			utc_sec = long(buffer[22]*65536UL*256UL + buffer[23]*65536UL $
				+ buffer[24]*256UL + buffer[25])
			utc_subsec = buffer[26]*256UL + buffer[27]
			utc = double(utc_sec) + double(utc_subsec)*20.0D-6

			
			snapshot_header_ID = 0UL
			snapshot_num = 0UL
			eot_marker = 0UL
			snapshot_header_ID = (buffer[28]*256UL + buffer[29])*65536UL + buffer[30]*256UL + buffer[31]
			snapshot_num = (buffer[32]*256UL + buffer[33])*65536UL $
				+ buffer[34]*256UL + buffer[35]
			eot_marker = (buffer[36]*256UL + buffer[37])*65536UL + buffer[38]*256UL + buffer[39]
			printf, lutimeline, '*****************************************************************************'
			printf, lutimeline, '*****************************************************************************'
;
; Convert time to GMT
;
			gmt = itos_time(data_collection_time,utc)	

			printf,lutimeline, format='(4a, 5x, 7a)', $
				'New LDP: Time = ', $
				string(data_collection_time, format='(f14.3)'), $
				' = ', gmt

			printf, lutimeline, format='(5a,i5,a,i6,a,i4,a,i,a,z8.8)', $
				'     Product # = ', strtrim(string(product_num),2), $
				'   Page # = ', strtrim(string(page_num),2), $
				'     Snapshot #: ', snapshot_num, $
				'   Target ID: ', target_id, $
				'   Obs Seg: ', obs_seg, $
				'   Obs Number: ', obs_seg*256UL*256UL*256UL+target_id, $
				' = 0x', obs_seg*256UL*256UL*256UL+target_id

			printf, lutimeline, ' '
			printf, lutimeline, format='(2a6,2a8,a19,a26,a25,2x,a8,4x,a,2x,a17)', $
				'Frame#', 'Type', 'cps', 'State', $
				'Readout Start (s)', $
				'(YYYY-DDD-HH:MM:SS.SSSSS)', $
				'   Delta t (s)   d-d (us)', $
				'CCD Temp', 'ACS Flags', $
 				'CCSDS Time (s)'
			printf, lutimeline, format='(100x,a)', '(Safe, SAA, 10'', Settled)'

		printf,luccsds,format='(a,i5,a,i5,a,i7,i5,a,f20.6,a,a)', 'Snapshot ', snapshot_num, $
			'     Total pages =', pages_in_file, $
			'     Target: ', target_id, obs_seg, $
			'     Time = ', data_collection_time, ' = ', gmt
		printf,luccsds,format='(2i5,f20.6,2i5,''     0x'',z8.8)',sequence, $
			packet_length, ccsds_time, product_num, page_num, snapshot_header_id

			if (idebug ge 35) then begin
				print, 'STRIP_HDR: Pages in file = ', pages_in_file
				print,'STRIP_HDR: Observation segment, Target ID = ', $
						obs_seg, target_id
				print,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection time seconds = ', $
						data_collection_sec
				print,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection time subsec  = ', $
						data_collection_subsec
				print,'STRIP_HDR: Data collection time = ', data_collection_time, ' s'
				print,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection UTC seconds = ', utc_sec
				print,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
				print,FORMAT='(A,Z10)','STRIP_HDR7: snapshot_header_ID = ', snapshot_header_ID
				print,FORMAT='(A,Z10)','STRIP_HDR7: snapshot_num = ', snapshot_num
				print,FORMAT='(A,Z10)','STRIP_HDR7: eot_marker = ', eot_marker
				print,'STRIP_HDR7: repeat end logic = ', $
					((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))

				printf,lulog, 'STRIP_HDR: Pages in file = ', pages_in_file
				printf,lulog,'STRIP_HDR: Observation segment, Target ID = ', $
						obs_seg, target_id
				printf,lulog,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection time seconds = ', $
						data_collection_sec
				printf,lulog,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection time subsec  = ', $
						data_collection_subsec
				printf,lulog,'STRIP_HDR: Data collection time = ', data_collection_time, ' s'
				printf,lulog,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection UTC seconds = ', utc_sec
				printf,lulog,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
				printf,lulog,FORMAT='(A,Z10)','STRIP_HDR7: snapshot_header_ID = ', snapshot_header_ID
				printf,lulog,FORMAT='(A,Z10)','STRIP_HDR7: snapshot_num = ', snapshot_num
				printf,lulog,FORMAT='(A,Z10)','STRIP_HDR7: eot_marker = ', eot_marker
				printf,lulog,'STRIP_HDR7: repeat end logic = ', $
					((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))
			endif

			if (snapshot_header_ID ne 'FEC07B92'xul) then begin
				print,FORMAT='(A,A,Z10)',$
					'STRIP_HDR8: Error in Snapshot ID word; should be "FEC07B92", ',$
						'found ', snapshot_header_ID
				printf,lulog,FORMAT='(A,A,Z10)',$
					'STRIP_HDR8: Error in Snapshot ID word; should be "FEC07B92", ',$
					'found ', snapshot_header_ID
				printf,luerr,FORMAT='(A,A,Z10)',$
					'STRIP_HDR8: Error in Snapshot ID word; should be "FEC07B92", ',$
					'found ', snapshot_header_ID
				print_error,0,1,'STRIP_HDR8: This packet number: ' $
					+ string(sequence)
				print_error,0,1,'STRIP_HDR8: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
				stop,'STRIP_HDR8: Invalid input file format: LDP header not found in expected location'
			endif
			if (snapshot_num ne snapshot_num_old+1) then begin
			    print,FORMAT='(A,Z8.8,A,Z8.8)',$
					'STRIP_HDR8a: Error in Snapshot Number; should be ',$
					snapshot_num_old, ', found ', snapshot_num
			    printf,lulog,FORMAT='(A,Z8.8,A,Z8.8)',$
					'STRIP_HDR8a: Error in Snapshot Number; should be ',$
					snapshot_num_old, ', found ', snapshot_num
			    print_error,0,1,'STRIP_HDR8a: This packet number: ' $
					+ strtrim(string(sequence),2)
			    print_error,0,1,'STRIP_HDR8a: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
			    if (snapshot_num eq 1) then begin
				; Assume that the instrument was switched off and on again and 
				;      try to retrieve this snapshot
				restart_count = restart_count + 1
			    endif ;$
;			    else $
;				stop,'STRIP_HDR14: Invalid input file format: snapshot does not begin with valid LDP header'
				
			endif
			snapshot_num_old = snapshot_num

			print,' '
			print,'STRIP_HDR9: Starting new LDP: Product # = ', product_num,$
				'   Page # = ', page_num
			printf,lulog,' '
			printf,lulog,'STRIP_HDR9: Starting new LDP: Product # = ', product_num,$
				'   Page # = ', page_num
			product_num_old = product_num

			; Close output file
			close,luout
			; Open new output file
			file2 = file_in + '_LDP' + strtrim(string(product_num),2) + flag[restart_count] + '.sci'
			openw,luout,file2,/swap_if_little_endian
			file_out = [file_out,file2]

			; adjust buffer if this is an LRPD header using old FSW
			if (packet_length eq 211) then begin
				; this could be a LRPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)) $	; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got LRPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

			; adjust buffer if this is an LRPD or PUPD header using old FSW
			if (packet_length eq 151) then begin
				; this could be an LRPD or PUPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(((buffer[12] eq '81'x) and (buffer[13] eq '9b'x)) $	; PUPD
					or ((buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)))) $ ; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got PUPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

			writeu,luout,buffer[28:buffer_size-3]  ; delete headers and checksum
		endif else begin
			print_error,0,1,'STRIP_HDR10: New LDP does not begin with page 1'
			print_error,0,1,'STRIP_HDR10: Product # = ' + strtrim(string(product_num),2)
			print_error,0,1,'STRIP_HDR10: Page # = ' + strtrim(string(page_num),2)
			print_error,0,1,'STRIP_HDR10: This packet number: ' $
					+ string(sequence)
			print_error,0,1,'STRIP_HDR10: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
			if ((product_num eq 0)and(page_num eq 0)) then begin
				print_error,0,1,'STRIP_HDR10: skipping apparently incomplete CCSDS packet'
				goto, loop_end
			endif
			printf,luccsds,format='(2i5,f20.6,2i5)',sequence, $
				packet_length, ccsds_time, product_num, page_num

;			print_error,0,1,'STRIP_HDR10: Returning to calling program ...'
;			free_lun,luin
;			free_lun,luout
;			return
		endelse
	endif else $
		if (page_num eq 1) then begin		; this should be the 2nd copy of page 1
			page1_count = page1_count + 1
			; Check for valid LDP header ID
			pages_in_file = buffer[10]*256U + buffer[11]
			if (idebug ge 3) then begin
				print, 'STRIP_HDR: Number of pages in this LDP should be ', pages_in_file
				printf,lulog, 'STRIP_HDR: Number of pages in this LDP should be ', pages_in_file
			endif

			if (page_num_old ne pages_in_file) then begin ; this may be an LDP roll-over
				print_error,0,1,$
					'STRIP_HDR11: Error in number of pages; should be ' $
					+strtrim(string(pages_in_file),2) + ' found ' + strtrim(string(page_num_old),2)
				print_error,0,1,$
					'STRIP_HDR11: '+strtrim(string(page1_count),2) $
					+ ' pages numbered page 1 found in LDP product #' $
					+ strtrim(string(product_num),2)
				print_error,0,1,$
					'STRIP_HDR11: Error occurred in CCSDS packet #' $
					+ strtrim(string(sequence),2)
				print_error,0,1,'STRIP_HDR11: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
				goto,process_packet
; TBD				stop,'STRIP_HDR11: Error in page numbers'
			endif
			; Calculate other header info for diagnostics
			obs_seg = buffer[12]
			target_id = buffer[13]*256UL*256UL + buffer[14]*256UL + buffer[15]
			data_collection_sec = buffer[16]*65536UL*256UL $
				+ buffer[17]*65536UL + buffer[18]*256UL + buffer[19]
			data_collection_subsec = buffer[20]*256UL + buffer[21]
			data_collection_time = double(data_collection_sec) $
				+ double(data_collection_subsec)*20.0D-6
			utc_sec = long(buffer[22]*65536UL*256UL + buffer[23]*65536UL $
				+ buffer[24]*256UL + buffer[25])
			utc_subsec = buffer[26]*256UL + buffer[27]
			utc = double(utc_sec) + double(utc_subsec)*20.0D-6

			snapshot_header_id = buffer[28]*65536UL*256UL $
				+ buffer[29]*65536UL + buffer[30]*256UL + buffer[31]
			snapshot_num = buffer[32]*65536UL*256UL + buffer[33]*65536UL $
				+ buffer[34]*256UL + buffer[35]
			eot_marker = buffer[36]*65536UL*256UL + buffer[37]*65536UL $
				+ buffer[38]*256UL + buffer[39]

		printf,luccsds,format='(a,i5,a,i5,a,i7,i5,a,f20.6,a,a)', 'Snapshot ', snapshot_num, $
			'     Total pages =', pages_in_file, $
			'     Target: ', target_id, obs_seg, $
			'     Time = ', data_collection_time, ' = ', gmt
		printf,luccsds,format='(2i5,f20.6,2i5,''     0x'',z8.8)',sequence, $
			packet_length, ccsds_time, product_num, page_num, snapshot_header_id


	
			if (idebug ge 35) then begin
				print,'STRIP_HDR: Observation segment, Target ID = ', $
						obs_seg, target_id
				print,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection time seconds = ', $
						data_collection_sec
				print,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection time subsec  = ', $
						data_collection_subsec
				print,'STRIP_HDR: Data collection time = ', $
						data_collection_time, ' s'
				print,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection UTC seconds = ', utc_sec
				print,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
				print,format='(a,''0x'',z8.8)','STRIP_HDR: Snapshot Header ID = ', snapshot_header_id
				print,'STRIP_HDR: Snapshot count = ', snapshot_num
				print,format='(a,''0x'',z8.8)','STRIP_HDR: EOT marker = ', eot_marker
				print,'STRIP_HDR: repeat end logic = ', $
					((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))

				printf,lulog,'STRIP_HDR: Observation segment, Target ID = ', $
						obs_seg, target_id
				printf,lulog,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection time seconds = ', $
						data_collection_sec
				printf,lulog,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection time subsec  = ', $
						data_collection_subsec
				printf,lulog,'STRIP_HDR: Data collection time = ', $
						data_collection_time, ' s'
				printf,lulog,format='(a,''0x'',z8.8)', $
					'STRIP_HDR: Data collection UTC seconds = ', utc_sec
				printf,lulog,format='(a,''0x'',z8.8)',$
					'STRIP_HDR: Data collection UTC subsec  = ', utc_subsec
				printf,lulog,format='(a,''0x'',z8.8)','STRIP_HDR: Snapshot Header ID = ', snapshot_header_id
				printf,lulog,'STRIP_HDR: Snapshot count = ', snapshot_num
				printf,lulog,format='(a,''0x'',z8.8)','STRIP_HDR: EOT marker = ', eot_marker
				printf,lulog,'STRIP_HDR: repeat end logic = ', $
					((snapshot_header_ID eq 'FEC07B92'xul) and (eot_marker ne '4E074E07'xul))
			endif

			if (snapshot_header_ID ne 'FEC07B92'xul) then begin
				print,FORMAT='(A,A,Z10)',$
					'STRIP_HDR13: Error in Snapshot ID word; should be "FEC07B92", ',$
						'found ', snapshot_header_ID
				printf,lulog,FORMAT='(A,A,Z10)',$
					'STRIP_HDR13: Error in Snapshot ID word; should be "FEC07B92", ',$
					'found ', snapshot_header_ID
				printf,luerr,FORMAT='(A,A,Z10)',$
					'STRIP_HDR13: Error in Snapshot ID word; should be "FEC07B92", ',$
					'found ', snapshot_header_ID
				print_error,0,1,'STRIP_HDR13: This packet number: ' $
					+ strtrim(string(sequence),2)
				print_error,0,1,'STRIP_HDR13: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
				stop,'STRIP_HDR13: Invalid input file format: does not begin with valid LDP header'
			endif
			if (snapshot_num ne snapshot_num_old) then begin
				print,FORMAT='(A,Z8.8,A,Z8.8)',$
					'STRIP_HDR14: Error in Snapshot Number; should be ',$
					snapshot_num_old, ', found ', snapshot_num
				printf,lulog,FORMAT='(A,Z8.8,A,Z8.8)',$
					'STRIP_HDR14: Error in Snapshot Number; should be ',$
					snapshot_num_old, ', found ', snapshot_num
				print_error,0,1,'STRIP_HDR14: This packet number: ' $
					+ strtrim(string(sequence),2)
				print_error,0,1,'STRIP_HDR14: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
;				stop,'STRIP_HDR14: Invalid input file format: does not begin with valid LDP header'
				
			endif
			snapshot_num_old = snapshot_num
		endif $
		else $
			printf,luccsds,format='(2i5,f20.6,2i5)',sequence, $
				packet_length, ccsds_time, product_num, page_num


process_packet:

	if ((page_num gt 1) and (page_num ne page_num_old+1)) then begin
		print_error,0,1, $
			'     STRIP_HDR15: Last page number = '+string(page_num_old)
		print_error,0,1, $
			'     STRIP_HDR15: Current page # =   '+string(page_num)
		print_error,0,1,'     STRIP_HDR15: This packet number: ' $
					+ strtrim(string(sequence),2)
		print_error,0,1,'     STRIP_HDR15: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
		if (page_num lt page_num_old+1) then begin
			print_error,0,1,'STRIP_HDR15: skipping out-of-order CCSDS packet # ' $
				+ strtrim(string(sequence),2)
			goto, loop_end
		endif
;		stop, '     STRIP_HDR15: Error in page numbers'
	endif
	page_num_old = page_num

	; Calculate the checksum
	if (idebug ge 30) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	check = checksum
	;checksum = byte(check,2)*256UL + byte(check,3) 	; extract lowest 2 bytes 
	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	; Verify the checksum
	if (chksum ne checksum) then begin
	    print,' '
	    print_error,0,1,'STRIP_HDR16: Checksum error in file ' + file_in
	    print_error,0,1,'STRIP_HDR16:	Checksum error in CCSDS header '+strtrim(string(sequence),2)
	    print_error,0,1,'STRIP_HDR16: Checksum in file: ' + string(chksum)
	    print_error,0,1,'STRIP_HDR16: Calculated checksum: ' + string(checksum)
	    print_error,0,1,'STRIP_HDR16: Error found in product #' $
		+ string(product_num) + ', page #' + string(page_num)
	    print_error,0,1,'STRIP_HDR16: WARNING!! POSSIBLE DATA CORRUPTION'	
	    print_error,0,1,'STRIP_HDR16: This packet number: ' $
					+ strtrim(string(sequence),2)
	    print_error,0,1,'STRIP_HDR16: cumulative CCSDS packet #' $
					+ strtrim(string(num_CCSDS_packets_read),2) $
					+ ', cumulative LDP page #' + strtrim(string(page_count),2)
	    if (chksum ne 0) then begin
		; Write the LDP data to output file, deleting headers and checksum
		if (page_num eq 1) then begin
			writeu,luout,buffer[28:buffer_size-3]
		endif $
		else begin

			; adjust buffer if this is an LRPD header using old FSW
			if (packet_length eq 211) then begin
				; this could be a LRPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)) $	; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got LRPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

			; adjust buffer if this is an LRPD or PUPD header using old FSW
			if (packet_length eq 151) then begin
				; this could be an LRPD or PUPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(((buffer[12] eq '81'x) and (buffer[13] eq '9b'x)) $	; PUPD
					or ((buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)))) $ ; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got PUPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

			writeu,luout,buffer[10:buffer_size-3]
		endelse

	; Note: do not write the data out if chksum=0, since this probably means that the last CCSDS packet is incomplete.
	    endif else begin
		print_error,0,1,'STRIP_HDR16A: **** Dropping incomplete CCSDS packet ****'
		page_num_old = page_num_old - 1		; page_num_old already set, so decrement it now
	    endelse
	endif else begin
		; Write the LDP data to output file, deleting headers and checksum
		if (page_num eq 1) then writeu,luout,buffer[28:buffer_size-3] $
		else begin
			; adjust buffer if this is an LRPD header using old FSW
			if (packet_length eq 211) then begin
				; this could be a LRPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)) $	; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got LRPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

			; adjust buffer if this is an LRPD or PUPD header using old FSW
			if (packet_length eq 151) then begin
				; this could be an LRPD or PUPD frame header from build 8.7 or earlier
				if ((buffer[10] eq '80'x) and (buffer[11] eq '73'x) and $
					(((buffer[12] eq '81'x) and (buffer[13] eq '9b'x)) $	; PUPD
					or ((buffer[12] eq 'b9'x) and (buffer[13] eq '18'x)))) $ ; LRPD
				then begin
					if (idebug ge 10) then begin
						printf,lulog,'got PUPD frame header'
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
						printf,lulog,' '
						printf,lulog,' here is the amp and Npix:', buffer[144], buffer[145]
					endif
					buffer = [buffer[0:145],0b,0b,buffer[146:*]]
					buffer_size = buffer_size+2

					if (idebug ge 10) then begin
						printf,lulog,' '
						printf,lulog,' Here is the adjusted buffer: '
						printf,lulog,format='(10(z2.2,z2.2,2x))',buffer
					endif
				endif
			endif

		    writeu,luout,buffer[10:buffer_size-3]
		    if ((n_elements(buffer) ge 22) and (buffer[10] eq '80'x) $
					and (buffer[11] eq '73'x)) then begin
			frame_id = buffer[10]*256L*256L*256L + buffer[11]*256L*256L $
				+ buffer[12]*256L + buffer[13]
			case frame_id of
				'807353e0'x: type = 'IM'
				'8073819B'x: type = 'PUPD'
				'8073b918'x: type = 'LRPD'
				'8073f0aa'x: type = ' WT '
				'8073ab6f'x: type = ' PC '
				'80730f0f'x: type = ' RAW'
				'8073f0f0'x: type = 'BIAS'
			else: type = '    '
			endcase
			ccd_num = buffer[14]*256L*256L*256L + buffer[15]*256L*256L $
				+ buffer[16]*256L + buffer[17]
			target_id = buffer[19]*256L*256L + buffer[20]*256L + buffer[21]
			obs_seg = buffer[18]

			b1 = buffer[38]
			b2 = buffer[39]
			b3 = buffer[40]
			b4 = buffer[41]
			sign = (b1 and 128)/128		; get sign
			exponent = (((b2 and 128)/128) or ((b1 and 127)*2)) - 127
			mantissa = b4 + b3*256L + (b2 and 127)*256L*256L
			value = 2.0^exponent
			for i=22,0,-1 do begin
			    bit = (mantissa and 2L^i)/2L^i
			    value = value + bit*2.0^(i-23+exponent)
			endfor
			if (sign) then value = -1.0*value
			count_rate = value
			acs_flg = buffer[34]
			acs_flags[0] = (acs_flg and '8'xb)/8
			acs_flags[1] = (acs_flg and '4'xb)/4
			acs_flags[2] = (acs_flg and '2'xb)/2
			acs_flags[3] = (acs_flg and '1'xb)
			xrt_state = buffer[35]
			case xrt_state of
				'11'x: state = 'Auto'
				'22'x: state = 'Manual'
				'44'x: state = 'RED'
				else: state = 'Undefined'
			endcase
			xrt_mode = buffer[36]
			ccd_temp = buffer[58]*256 + buffer[59]
			readout_start_sec = buffer[114]*256UL*256UL*256UL $
				+ buffer[115]*256UL*256UL + buffer[116]*256UL + buffer[117]
			readout_start_subsec = buffer[118]*256UL + buffer[119]
			readout_start_time = double(readout_start_sec) + double(readout_start_subsec)*20.0D-6
			delta_readout_time = readout_start_time - last_read_time
			last_read_time = readout_start_time
			delta_delta = fix((delta_readout_time - last_delta)*1.0E6)
			last_delta = delta_readout_time
;
; Convert time to GMT
;
	
			gmt = itos_time(readout_start_time,utc)
	
;			print,'STRIP_HDR16B: s, subsec, time, date = ', readout_start_sec, $
;					readout_start_subsec, string(readout_start_time,format='(f20.6)'), $
;					'     ', gmt

			printf,lutimeline,format='(i6,a6,f8.2,2x,a6,f17.6,a28,2x,f12.6,1x,i9,2x,f8.1,5x,4i2,f20.6)', $
				ccd_num, type, count_rate, state, $
				readout_start_time, gmt, delta_readout_time, $
				delta_delta, rtd(ccd_temp,3), acs_flags, ccsds_time
;			printf,lulog,format='(a,z8,a6,i3,i6,2i4, f8.2, z6.4)', $
;				'CCD Frame Header, Type, LDP#, Target ID, Obs Seg, Frame#, Count Rate, CCD Temp: ', $
;				frame_id, type, product_num, target_id, obs_seg, ccd_num, count_rate, ccd_temp

		    endif
		endelse
	endelse

loop_end:
	if (idebug gt 99) then print,format='(z10)',buffer[buffer_size-3]
	if ((page_num mod 10) eq 0) then print,format='(a,$)','.'
endrep until (EOF(luin))

no_more_data:
if (idebug ge 30) then begin
	print,'STRIP_HDR: ---------------------------------
endif
free_lun,luin
free_lun,luout
printf,lulog,'STRIP_HDR17: Returning to PASS1 at cumulative CCSDS packet #' $
	+ strtrim(string(num_CCSDS_packets_read),2) $
	+ ', cumulative LDP page #' + strtrim(string(page_count),2)
printf,lutimeline,'STRIP_HDR: Returning to PASS1 at cumulative CCSDS packet #' $
	+ strtrim(string(num_CCSDS_packets_read),2) $
	+ ', cumulative LDP page #' + strtrim(string(page_count),2)
return
end


