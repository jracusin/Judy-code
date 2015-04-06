pro plc, filename, showplot, debug_level, DEBUG5=debug5, $
	NOINIT=noinit, PRINTPLOTS=printplots,quicklook=quicklook
; $Id: xrt_plc

;name	: plc
;author	: Dave Burrows
;date	: 02/03/03
;lang	: IDL

;purpose: This program reads an XRT Prompt LightCurve file archived by ITOS
;	and writes the plc out as a 1-D FITS file and as a 
;	binary table.
;
;
; Rev:
;   08/18/04 by JLR: added quicklook keyword and output ps plot
;	02/12/04 by DNB: added support for termination conditions
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
;	read,'Enter name of XRT Prompt Light Curve file: ', filein
	filein = pickfile(title='Select XRT Prompt Light Curve file: ', $
		filter='*lightcurve*', /must_exist)
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
	show_ccd_plc = 1
	show_hk_plc = 1
endif

; Check that file exists

openr,luin,filein,error=file_error
if (file_error ne 0) then repeat begin
	print,'Error opening input file.  Please try another file name.'
;	read,'Enter name of XRT Prompt Light Curve file: ', filein
	filein = pickfile(title='Select XRT Prompt Light Curve file: ', $
		filter='*lightcurve*', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
	openr,luin,filein,error=file_error
endrep until (file_error eq 0)
close,luin

GET_LUN, lulog
openw,lulog, filein+'.plc.log'
GET_LUN, luerr
openw,luerr, filein+'.plc.err'

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
ccsds_id = '0CE5'x	; TDRSS packet

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
nchan = -1
old_packet_num = 0
plc_num = 0
carry_flag = 0

repeat begin
	plc = fltarr(100)
	xerror = plc
	yerror = plc
	low = plc
	high = plc
	plc_mode = bytarr(100)
	plc_time = dblarr(100)
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
		print, 'PLC: Error in file format: invalid control bits =', control
		stop, 'PLC: Fatal error in control bits'
	endif

	sequence = seq_cntl and '3FFF'x

	readu,luin,byte1,byte2
	packet_length = byte1*256+byte2
	if (idebug ge 10) then begin
		print,'PLC: ---------------------------------
		print,'PLC: reading CCSDS packet #', sequence
		print,FORMAT='(A,3Z10,I10)',$
				'PLC: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
		print,'PLC: packet length = ', packet_length
		printf,lulog,'PLC: ---------------------------------
		printf,lulog,'PLC: reading CCSDS packet #', sequence
		printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
				'PLC: CCSDS ID, control, Seq = ', header_id,$
				control, sequence
		printf,lulog,'PLC: packet length = ', packet_length
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
		print,'PLC: CCSDS time =  ', ccsds_time
		printf,lulog,'PLC: CCSDS time =  ', ccsds_time
	endif
	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; Calculate the checksum
	if (idebug ge 10) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 10) then begin
		print, 'PLC: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'PLC: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum

	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 10) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','PLC: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','PLC: Calculated checksum:', checksum
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
	plc_start_time = double(XRT_sec) + double(XRT_subsec)*20.0D-6
	if (idebug ge 5) then print,'Satellite time is ', plc_start_time

	if (plc_start_time lt last_time) then begin
		print_error,0,1, $
		    'PLC: WARNING: Satellite time decreased!'
		print_error,0,1, 'PLC:      Last time =  ' $
			+ string(last_time)
		print_error,0,1, 'PLC:      Current time=' $
			+ string(plc_start_time)
	endif

	last_time = plc_start_time

	UTC_sec = long(extract_long(buffer_pointer))
	UTC_subsec = extract_int(buffer_pointer)
	UTC_delta = double(UTC_sec) + double(UTC_subsec)*20.0D-6

	packet_number = extract_int(buffer_pointer)
	if (packet_number ne 1) then begin
		print_error,0,1,$
			'PLC: ERROR in packet number: expected 1' $
			+ ', found ' + string(packet_number)
		stop, 'PLC: fatal error'
	endif

	case packet_number of
		1: begin
			ra = extract_float(buffer_pointer)
			dec = extract_float(buffer_pointer)
			print,'RA, Dec = ', ra, dec
			printf,lulog,'RA, Dec = ', ra, dec

			plc_stop_sec = extract_long(buffer_pointer)
			plc_stop_subsec = extract_int(buffer_pointer)
			plc_stop_time = double(plc_stop_sec) + double(plc_stop_subsec)*20.0D-6
			print,'PLC start time = ', plc_start_time
			printf,lulog,format='(a,f20.6)', $
				'PLC start time = ', plc_start_time
			print,'PLC stop time  = ', plc_stop_time
			printf,lulog,format='(a,f20.6)', $
				'PLC stop time  = ', plc_stop_time

			last_time = -1
			for i=0,99 do begin
				data = extract_byte(buffer_pointer)
				plc_mode[i] = data
				case plc_mode[i] of
					0: time = 0.0
					2: time = 0.1
					3: time = 2.5
					4: time = 8.2623
					5: time = 8.2623
					6: time = 0.7210
					7: time = 2.5073
					else: stop, 'Found invalid mode'
				endcase
				coarse_time = extract_int(buffer_pointer)
				if (coarse_time lt last_time) then carry_flag = carry_flag + 1
				last_time = coarse_time
				fine_time = extract_int(buffer_pointer)
				plc_time[i] = carry_flag*double(65536) $
					+ double(coarse_time) + double(fine_time)*20.0D-6 
				plc_time[i] = plc_time[i] - time/2.0   ; correct to middle of frame
				if (i eq 0) then begin
					plc_time0 = plc_time[i] - time/2.0
					plc_start_time = plc_start_time - time
					time0 = time
				endif
				count_rate = extract_float(buffer_pointer)
				plc[i] = count_rate
				if (idebug gt 10) then begin
					print,nchan,channel,plc(nchan)
					printf,lulog,nchan,channel,plc(nchan)
				endif
				if (time gt 0) then begin
					counts = plc[i]*time
					xerror[i] = time/2.0
					yerror[i] = sqrt(counts)/time
				endif
			endfor

			; Adjust the times to correct S/C time
			; This is necessary because plc_time is mod 65536,
			;	but plc_start_time is correct
			plc_time = plc_start_time + plc_time - plc_time0
			
			livetime = extract_float(buffer_pointer)
			print,'Livetime = ', livetime
			printf,lulog,'Livetime = ', livetime

			n_bins = extract_int(buffer_pointer)
			print,'Light curve has ', n_bins, ' bins of valid data'

			term = extract_int(buffer_pointer)
			buffer_pointer = buffer_pointer + 4	; skip spare bytes
			case term of 
				0: cond = 'Normal: 100 bins of data'
				1: cond = 'Terminated by timeout: < 100 bins'
				2: cond = 'Terminated by end of snapshot: < 100 bins'
				3: cond = 'Terminated by SAA entrance: < 100 bins'
			endcase
			print,'Termination condition: ', term, ' (', cond, ')'
			printf,lulog,'Termination condition: ', term, ' (', cond, ')'


            if (idebug gt 5) then print,plc
            
            if not keyword_set(quicklook) then begin 
               plot,plc,xtitle='Time bin #', ytitle='Count Rate (cps)' ;,xrange=[0.,1000.]
               low = plc - yerror
               high = plc + yerror
               errplot,low,high
            endif 
            
            set_plot,'ps'
            device,filename=filebase+'.plc.ps', /landscape
            plot,plc,xtitle='Time bin #', ytitle='Count Rate (cps)' ;,xrange=[0.,1000.]
            low = plc - yerror
            high = plc + yerror
            errplot,low,high
            device,/close
            
            plc_num = plc_num + 1
				
			filenm = filein + '.tdrss_plc_' + strtrim(string(plc_num),2) + '.fits'

			mkhdr,hdr,plc
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
			sxaddpar, hdr, 'TSTART', plc_start_time, 'Satellite time of exposure start'
			sxaddpar, hdr, 'TSTOP', plc_stop_time, 'Satellite time of exposure end'
			sxaddpar, hdr, 'RA',ra,'Right Ascension (degrees)'
			sxaddpar, hdr, 'DEC',dec,'Declination (degrees)'
			sxaddpar, hdr, 'LIVETIME', livetime, 'Livetime for this frame'
			sxaddpar, hdr, 'NBINS', n_bins, 'Number of valid lightcurve bins'

			writefits,filenm,plc,hdr

; write out binary table file
			bin_table = replicate(  {TIME: 0D, $
						MODE: 0, $
						RATE: 0.0, $
						XERROR: 0.0, $
						YERROR: 0.0 }, n_bins)
			bin_table.TIME = plc_time[0:n_bins-1]
			bin_table.MODE = plc_mode[0:n_bins-1]
			bin_table.RATE = plc[0:n_bins-1]
			bin_table.XERROR = xerror[0:n_bins-1]
			bin_table.YERROR = yerror[0:n_bins-1]

			sxaddpar, events_hdr, 'EXTNAME',  'EVENTS'
			sxaddpar, events_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'
			sxaddpar, events_hdr, 'TUNIT1', 'Spacecraft time (s)'
			sxaddpar, events_hdr, 'TUNIT2', 'Readout Mode'
			sxaddpar, events_hdr, 'TUNIT3', 'cps'
			sxaddpar, events_hdr, 'TUNIT4', 'cps'
			sxaddpar, events_hdr, 'TUNIT5', 'cps'
			sxaddhist, ' ', events_hdr
			sxaddhist, '********************************************', events_hdr

			sxaddpar,events_hdr,'DATE',date,'File creation date'
			sxaddpar,events_hdr,'ORIGIN','PSU X-ray Astronomy', $
				'Data from Penn State X-ray Astronomy'
			sxaddpar,events_hdr,'INSTRUME','XRT','Swift X-ray Telescope'
			sxaddpar,events_hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
			sxaddhist,' ',events_hdr
			sxaddhist,'XRT Converted Housekeeping Data File created by STRIPCHART '+version, events_hdr
			sxaddpar, events_hdr, 'OBS_NUM', $
				strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
					'Swift Observation Number'
			sxaddpar, events_hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
			sxaddpar, events_hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
			sxaddpar, events_hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
			sxaddpar, events_hdr, 'TSTART', plc_start_time, 'Satellite time of exposure start'
			sxaddpar, events_hdr, 'TSTOP', plc_stop_time, 'Satellite time of exposure end'
			sxaddpar, events_hdr, 'RA',ra,'Right Ascension (degrees)'
			sxaddpar, events_hdr, 'DEC',dec,'Declination (degrees)'
			sxaddpar, events_hdr, 'LIVETIME', livetime, 'Livetime for this frame'
			sxaddpar, events_hdr, 'NBINS', n_bins, 'Number of valid lightcurve bins'

			mwrfits, bin_table, filenm, events_hdr

			nchan = -1
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
