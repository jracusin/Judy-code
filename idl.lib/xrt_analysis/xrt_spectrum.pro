pro xrt_spectrum, filename, showplot, debug_level, DEBUG5=debug5, $
	NOINIT=noinit, PRINTPLOTS=printplots,quicklook=quicklook
; $Id: xrt_spectrum

;name	: xrt_spectrum
;author	: Dave Burrows
;date	: 02/03/03
;lang	: IDL

;purpose: This program reads an XRT Spectrum file archived by ITOS
;	and writes the spectrum out as a 1-D FITS spectrum file.
;
;
; Rev:
;   08/18/04 by JLR: added quicklook keyword and made output ps plot
;	07/29/04 by DNB: added bias_level to support Build 8.8 software
;	02/12/04 by DNB: added support for waveform number and termination condition
;	10/24/03 by DNB: added FITS keywords and comments to output file.
;	10/21/03 by DNB: fixed major bugs in program, including incorrect time conversions, 
;		incorrect spectrum array size, incorrect packet treatment
;
; $Log: process_ccd_frame_hdr.pro $
;
;
;

@pass1_common

@pass1_path_init	; initialize paths for input files
version = 'V1.0'

common save_data,oldwaveform,n_frames	; saves this for next entry

if not (keyword_set(noinit)) then begin
@pass1_init	; initialize common block variables.
endif

GET_LUN, luin		; Get Logical Unit Numbers for input and output files

if (n_params() eq 0) then begin
;	read,'Enter name of XRT Spectrum file: ', filein
	filein = pickfile(title='Select XRT Spectrum file: ', $
		filter='*spectrum*', /must_exist)
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
	show_ccd_spectrum = 1
	show_hk_spectrum = 1
endif

; Check that file exists

openr,luin,filein,error=file_error
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
;	read,'Enter name of XRT Spectrum file: ', filein
	filein = pickfile(title='Select XRT Spectrum file: ', $
		filter='*spectrum*', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
	openr,luin,filein,error=file_error
endrep until (file_error eq 0)
close,luin

GET_LUN, lulog
openw,lulog, filein+'.spectrum.log'
GET_LUN, luerr
openw,luerr, filein+'.spectrum.err'

; Set up housekeeping arrays
load_hk_tables



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
ccsds_id = '0CE1'x	; TDRSS packet

byte1 = 0B
byte2 = 0B

; Open input file
get_lun,luin
openr,luin,filein,error=file_error,/swap_if_little_endian
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
	read,'Enter name of XRT input file: ', file_in
	openr,luin,file_in,error=file_error,/swap_if_little_endian
endrep until (file_error eq 0)

; Read the file and process the data

packet_count = 0
packet_num = 0
spectrum = intarr(1024)
nchan = -1
old_packet_num = 0
spectrum_num = 0

repeat begin
	packet_count = packet_count + 1
	print,''
	print,'*************************************************'
	print,'CCSDS packet ', packet_count
	printf,lulog,''
	printf,lulog,'*************************************************'
	printf,lulog,'CCSDS packet ', packet_count
;
; Search for a valid CCSDS packet header ID
;	WARNING: this technique fails if there is an offset by an
;	odd number of bytes!  (This situation should never occur).
; This routine only processes Ap_IDs corresponding to HK packets.
; Other Ap_IDs are ignored.
;
	extra_bytes = -2
	while (header_id ne ccsds_id) do begin
		readu,luin,byte1,byte2
		header_id = byte1*256+byte2
		extra_bytes = extra_bytes + 2
	endwhile
	if (idebug ge 5) then print,extra_bytes,' extra bytes before packet ', $
		packet_count

	packet_num = ((packet_count-1) mod 3) + 1

	; Read rest of CCSDS header
	readu,luin,byte1,byte2
	seq_cntl = byte1*256+byte2
	control = (seq_cntl and 'C000'x)/'4000'x

	if (control ne 3) then begin
		print, 'SPECTRUM: Error in file format: invalid control bits =', control
		stop, 'SPECTRUM: Fatal error in control bits'
	endif

	sequence = seq_cntl and '3FFF'x

	readu,luin,byte1,byte2
	packet_length = byte1*256+byte2
	if (idebug ge 10) then begin
		print,'SPECTRUM: ---------------------------------
		print,'SPECTRUM: reading CCSDS packet #', sequence
		print,FORMAT='(A,3Z10,I10)',$
				'SPECTRUM: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'SPECTRUM: packet length = ', packet_length
		printf,lulog,'SPECTRUM: ---------------------------------
		printf,lulog,'SPECTRUM: reading CCSDS packet #', sequence
		printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
				'SPECTRUM: CCSDS ID, control, Seq = ', header_id,$
				control, sequence
		printf,lulog,'SPECTRUM: packet length = ', packet_length
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
		print,'SPECTRUM: CCSDS time =  ', ccsds_time
		printf,lulog,'SPECTRUM: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; Calculate the checksum
	if (idebug ge 10) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 10) then begin
		print, 'SPECTRUM: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'SPECTRUM: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum

	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 10) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','SPECTRUM: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','SPECTRUM: Calculated checksum:', checksum
	endif


; Extract tertiary header info and check for format errors

	data_buffer = buffer
	buffer_pointer = 6
	observation_number = extract_long(buffer_pointer)
	obs_segment = (observation_number and 'FF000000'xl) / (256L*256L*256L)
	target_id = (observation_number and '00FFFFFF'xl)
	seq_num = target_id*1000L + obs_segment

	XRT_sec = extract_long(buffer_pointer)
	XRT_subsec = extract_int(buffer_pointer)
	spec_start_time = double(XRT_sec) + double(XRT_subsec)*20.0D-6
	if (idebug ge 5) then print,'Satellite time is ', spec_start_time

	if (spec_start_time lt last_time) then begin
		print_error,0,1, $
		    'SPECTRUM: WARNING: Satellite time decreased!'
		print_error,0,1, 'SPECTRUM:      Last time =  ' $
			+ string(last_time)
		print_error,0,1, 'SPECTRUM:      Current time=' $
			+ string(spec_start_time)
	endif

	last_time = spec_start_time

	UTC_sec = long(extract_long(buffer_pointer))
	UTC_subsec = extract_int(buffer_pointer)
	UTC_delta = double(UTC_sec) + double(UTC_subsec)*20.0D-6

	packet_number = extract_int(buffer_pointer)
	if (packet_number ne packet_num) then begin
		print_error,0,1,$
			'SPECTRUM: ERROR in packet numbers: expected ' $
			+ string(packet_num) + ', found ' + string(packet_number)
		stop, 'SPECTRUM: fatal error'
	endif
	if (packet_number ne old_packet_num + 1) then begin
		print_error,0,1,$
			'SPECTRUM: packet number does not increment correctly: ' $
				+ 'old packet number = ' + string(old_packet_num) $
				+ '; this packet number = ' + string(packet_num)
		stop, 'SPECTRUM: fatal error'
	endif
	old_packet_num = packet_number mod 3

	case packet_number of
		1: begin
			ra = extract_float(buffer_pointer)
			dec = extract_float(buffer_pointer)
			print,'RA, Dec = ', ra, dec
			printf,lulog,'RA, Dec = ', ra, dec

			spec_stop_sec = extract_long(buffer_pointer)
			spec_stop_subsec = extract_int(buffer_pointer)
			spec_stop_time = double(spec_stop_sec) + double(spec_stop_subsec)*20.0D-6
			print,'Spectrum start time = ', spec_start_time
			printf,lulog,format='(a,f20.6)', $
				'Spectrum start time = ', spec_start_time
			print,'Spectrum stop time  = ', spec_stop_time
			printf,lulog,format='(a,f20.6)', $
				'Spectrum stop time  = ', spec_stop_time

			for i=0,449 do begin
				channel = extract_int(buffer_pointer)
				nchan = nchan + 1
				spectrum(nchan) = channel
				if (idebug gt 10) then begin
					print,nchan,channel,spectrum(nchan)
					printf,lulog,nchan,channel,spectrum(nchan)
				endif
			endfor
			livetime = extract_float(buffer_pointer)
			print,'Livetime = ', livetime
			printf,lulog,'Livetime = ', livetime
			term = extract_int(buffer_pointer)
			pd = (term and '0080'x)/128
			wt = (term and '0040'x)/64
			termination = (term and '000F'x)
			case termination of
				0: cond = 'manual'
				1: cond = 'abort - timed out'
				2: cond = 'abort - end of snapshot'
				3: cond = 'abort - entered SAA'
				4: cond = 'LR mode ended normally'
				5: cond = 'WT mode ended normally'
			endcase
			print,'Termination code: ', term, ' (', cond, ')'
			printf,lulog,format='(''Termination code = '', z8.8)', term
			if (pd eq 1) then printf,lulog,'     Spectrum contains LRPD mode data'
			if (wt eq 1) then printf,lulog,'     Spectrum contains WT mode data'
			printf,lulog,'     Termination condition: ',cond

			mode = extract_byte(buffer_pointer)
			waveform = extract_byte(buffer_pointer)
			bias_level = extract_int(buffer_pointer)
			print,'Mode, waveform, bias_level = ', mode, waveform, bias_level
			printf,lulog,'Mode, waveform, bias_level = ', mode, waveform, bias_level
		    end
		2: begin
			for i=0,449 do begin
				channel = extract_int(buffer_pointer)
				nchan = nchan + 1
				spectrum(nchan) = channel
				if (idebug gt 10) then begin
					print,nchan,channel,spectrum(nchan)
					printf,lulog,nchan,channel,spectrum(nchan)
				endif
			endfor
		    end
		3: begin
			for i=0,123 do begin
				channel = extract_int(buffer_pointer)
				nchan = nchan + 1
				spectrum(nchan) = channel
				if (idebug gt 10) then begin
					print,nchan,channel,spectrum(nchan)
					printf,lulog,nchan,channel,spectrum(nchan)
				endif
			endfor
			if (idebug gt 5) then print,spectrum
            if not keyword_set(quicklook) then $
               plot,spectrum,xtitle='Channel #' ;,xrange=[0.,1000.]
            
            set_plot,'ps'
            device,filename=filebase+'.spectrum.ps', /landscape
            plot,spectrum,xtitle='Channel #' ;,xrange=[0.,1000.]
            device,/close
            
			spectrum_num = spectrum_num + 1
;			case spectrum_num of
;				1: begin
;					filenm = filein + '.tdrss_spectrum_1.fits'
;					spectrum1 = spectrum
;					writefits,filenm,spectrum
;				   end
;				2: begin
;					filenm = filein + '.tdrss_spectrum_2.fits'
;					spectrum2 = spectrum - spectrum1
;					writefits,filenm,spectrum2
;				   end
;			else: begin
;				print_error,0,1,'SPECTRUM: WARNING - too many spectra in input file'
				filenm = filein + '.tdrss_spectrum_' + strtrim(string(spectrum_num),2) + '.fits'

				mkhdr,hdr,spectrum
				file_date = bin_date(systime(0))
				date = string(file_date(2),file_date(1),format="(i2.2,'/',i2.2,'/')") $
					+ strmid(string(file_date(0),format="(i4.4)"),2,2)
				sxaddpar,hdr,'DATE',date,'File creation date'
				sxaddpar,hdr,'ORIGIN','PSU X-ray Astronomy', $
					'Data from Penn State X-ray Astronomy'
				sxaddpar,hdr,'INSTRUME','XRT','Swift X-ray Telescope'
				sxaddpar,hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
				sxaddhist,' ',hdr
				sxaddhist,'XRT Converted Housekeeping Data File created by STRIPCHART '+version, hdr
				sxaddpar, hdr, 'OBS_NUM', $
					strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
					'Swift Observation Number'
				sxaddpar, hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
				sxaddpar, hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
				sxaddpar, hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
				sxaddpar, hdr, 'TSTART', spec_start_time, 'Satellite time of exposure start'
				sxaddpar, hdr, 'TSTOP', spec_stop_time, 'Satellite time of exposure end'
				sxaddpar, hdr, 'RA',ra,'Right Ascension (degrees)'
				sxaddpar, hdr, 'DEC',dec,'Declination (degrees)'
				sxaddpar, hdr, 'LIVETIME', livetime, 'Livetime for this frame'
				if (pd eq 1) then sxaddpar, hdr, 'PD_MODE', pd,'Spectrum contains LRPD mode data' $
				else sxaddpar, hdr, 'PD_MODE', pd, 'Spectrum contains NO LRPD mode data'
				if (wt eq 1) then sxaddpar, hdr, 'WT_MODE', wt,'Spectrum contains WT mode data' $
				else sxaddpar, hdr, 'WT_MODE', wt, 'Spectrum contains NO WT mode data'
				sxaddpar, hdr, 'BIAS_LVL', bias_level, 'Bias Level subtracted from data'

				writefits,filenm,spectrum,hdr
;				endelse
;			endcase
			nchan = -1
			spectrum = intarr(1024)
			print,''
			print,'**************************************************'
			print,''
			printf,lulog,''
			printf,lulog,'***********************************************'
			printf,lulog,''
		    end
	endcase

	; reset header_id to force new read
	header_id = 0

endrep until (EOF(luin))

print,''
print,'*******************************************************************'
print,'*******************************************************************'
print,'Done processing ', packet_count, ' CCSDS packets'

close,/all

end
