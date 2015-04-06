pro postage_stamp, filename, showplot, debug_level, DEBUG5=debug5, $
	NOINIT=noinit, PRINTPLOTS=printplots,quicklook=quicklook
; $Id: postage_stamp

;name	: postage_stamp
;author	: Dave Burrows
;date	: 02/03/03
;lang	: IDL

;purpose: This program reads an XRT Postage Stamp file archived by ITOS
;	and writes the Postage Stamp image out as a FITS image file.
;
;
; Rev:
;   12/12/04 by JLR: added more significant digits to SC RA/DEC
;   08/18/04 by JLR: added quicklook keyword
;	03/08/04 by DNB: fixed time format, added time to file name
;	02/12/04 by DNB: added support for readout mode, waveform
;	12/16/03 by DNB: added observation number in hex to print statements
;	10/24/03 by DNB: added keywords for all header data
;	10/21/03 by DNB: fixed major bugs in program, including incorrect time conversions, 
;		incorrect packet treatment
;
; $Log: process_ccd_frame_hdr.pro $
;
;
;

@pass1_common

@pass1_path_init	; initialize paths for input files
version = 'V2.1'

common save_data,oldwaveform,n_frames	; saves this for next entry

if not (keyword_set(noinit)) then begin
@pass1_init	; initialize common block variables.
endif

get_lun, luin		; Get Logical Unit Number for input file
if (n_params() eq 0) then begin
;	read,'Enter name of XRT Postage Stamp file: ', filein
	filein = pickfile(title='Select XRT Postage Stamp file: ', $
		filter='*postage*', /must_exist)
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
;	read,'Enter name of XRT Postage Stamp file: ', filein
	filein = pickfile(title='Select XRT Postage Stamp file: ', $
		filter='*postage*', /must_exist)
	path_pos = strpos(filein, '/', /reverse_search)
	filein = strmid(filein,path_pos+1)
	openr,luin,filein,error=file_error
endrep until (file_error eq 0)
close,luin

get_lun, lulog
openw,lulog, filein+'.postage_stamp.log'

get_lun, luerr
openw,luerr, filein+'.postage_stamp.err'



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
ccsds_id = '0CE2'x	; TDRSS packet

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
image = intarr(51*51)
npix = -1
old_packet_num = 0
image_num = 0

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
		print, 'POSTAGE_STAMP: Error in file format: invalid control bits =', control
		stop, 'POSTAGE_STAMP: Fatal error in control bits'
	endif

	sequence = seq_cntl and '3FFF'x

	readu,luin,byte1,byte2
	packet_length = byte1*256+byte2

	print,'POSTAGE_STAMP: ---------------------------------
	print,'POSTAGE_STAMP: reading CCSDS packet #', sequence
	print,FORMAT='(A,3Z10,I10)',$
				'POSTAGE_STAMP: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
				seq_cntl, control, sequence
	print,'POSTAGE_STAMP: packet length = ', packet_length
	printf,lulog,'POSTAGE_STAMP: ---------------------------------
	printf,lulog,'POSTAGE_STAMP: reading CCSDS packet #', sequence
	printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
				'POSTAGE_STAMP: CCSDS ID, control, Seq = ', header_id,$
				control, sequence
	printf,lulog,'POSTAGE_STAMP: packet length = ', packet_length

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
		print,FORMAT='(A,Z10,Z10,F20.6)','POSTAGE_STAMP: CCSDS time = ',$
			ccsds_sec, ccsds_subsec, ccsds_time
		printf,lulog,FORMAT='(A,Z10,Z10,F20.6)','POSTAGE_STAMP: CCSDS time = ',$
			ccsds_sec, ccsds_subsec, ccsds_time
	endif

	print,format='(a,f20.6)','POSTAGE_STAMP: CCSDS time =  ', ccsds_time
	printf,lulog,format='(a,f20.6)','POSTAGE_STAMP: CCSDS time =  ', ccsds_time

	ccsds_time_old = ccsds_time
	old_sec = ccsds_sec
	old_subsec = ccsds_subsec

	; Calculate the checksum
	if (idebug ge 10) then help,checksum
	checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
	if (idebug ge 10) then begin
		print, 'POSTAGE_STAMP: Total of all bytes outside CCSDS headers: ', checksum
		printf,lulog, 'POSTAGE_STAMP: Total of all bytes outside CCSDS headers: ', checksum
	endif
	check = checksum

	; Now get checksum value from file
	chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

	if (idebug ge 10) then begin
		help,checksum
		help,chksum
		print,FORMAT='(A,Z10)','POSTAGE_STAMP: Checksum in file is ', chksum
		print,FORMAT='(A,Z10)','POSTAGE_STAMP: Calculated checksum:', checksum
	endif


; Extract tertiary header info and check for format errors

	data_buffer = buffer
	buffer_pointer = 6
	observation_number = extract_long(buffer_pointer)
	obs_segment = (observation_number and 'FF000000'xl) / (256L*256L*256L)
	target_id = (observation_number and '00FFFFFF'xl)
	seq_num = target_id*1000L + obs_segment
	print,format='(a,i10,a,z8.8,i6,i4,i10)','Obs Num, Target ID, Obs Seg, Seq #: ', $
		observation_number, ' = 0x', observation_number, $
		target_id, obs_segment, seq_num
	printf,lulog,format='(a,i10,a,z8.8,i6,i4,i10)','Obs Num, Target ID, Obs Seg, Seq #: ', $
		observation_number, ' = 0x', observation_number, $
		target_id, obs_segment, seq_num

	XRT_sec = extract_long(buffer_pointer)
	XRT_subsec = extract_int(buffer_pointer)
	frame_start_time = double(XRT_sec) + double(XRT_subsec)*20.0D-6
	if (idebug ge 10) then begin
		print,FORMAT='(A,Z10,Z10,F20.6)','POSTAGE_STAMP: Data collection time = ',$
		XRT_sec, XRT_subsec, frame_start_time
		printf,lulog,FORMAT='(A,Z10,Z10,F20.6)','POSTAGE_STAMP: Data collection time = ',$
		XRT_sec, XRT_subsec, frame_start_time
	endif
	print,format='(a,f20.6)','Satellite time of data collection is ', frame_start_time
	printf,lulog,format='(a,f20.6)','Satellite time of data collection is ', frame_start_time
	deltat = ccsds_time - frame_start_time
	print,'     ', deltat, ' seconds between data collection and SCUI'
	printf,lulog,'     ', deltat, ' seconds between data collection and SCUI'

	if (frame_start_time lt last_time) then begin
		print_error,0,1, $
		    'POSTAGE_STAMP: WARNING: Satellite time decreased!'
		print_error,0,1, 'POSTAGE_STAMP:      Last time =  ' $
			+ string(last_time)
		print_error,0,1, 'POSTAGE_STAMP:      Current time=' $
			+ string(frame_start_time)
	endif

	last_time = frame_start_time

	UTC_sec = long(extract_long(buffer_pointer))
	UTC_subsec = extract_int(buffer_pointer)
	UTC_delta = double(UTC_sec) + double(UTC_subsec)*20.0D-6

	packet_number = extract_int(buffer_pointer)
	if (packet_number ne packet_num) then begin
		print_error,0,1,$
			'POSTAGE_STAMP: ERROR in packet numbers: expected ' $
			+ string(packet_num) + ', found ' + string(packet_number)
		stop, 'POSTAGE_STAMP: fatal error'
	endif
	if (packet_number ne old_packet_num + 1) then begin
		print_error,0,1,$
			'POSTAGE_STAMP: packet number does not increment correctly: ' $
				+ 'old packet number = ' + string(old_packet_num) $
				+ '; this packet number = ' + string(packet_num)
		stop, 'POSTAGE_STAMP: fatal error'
	endif
	old_packet_num = packet_number mod 3

	case packet_number of
		1: begin
			image_num = image_num + 1
			if (image_num gt 1) then print, 'POSTAGE STAMP: multiple images in input file'
			print,'Image #', image_num, ' in input file:'
			printf,lulog,'Image #', image_num, ' in input file:'
			ccd_frame = extract_long(buffer_pointer)
			print,'CCD frame number is ', ccd_frame
			printf,lulog,'CCD frame number is ', ccd_frame
			old_ccd_frame = ccd_frame
			ra = extract_float(buffer_pointer)
			dec = extract_float(buffer_pointer)
			roll = extract_float(buffer_pointer)
			print,'RA, Dec, Roll of image = ', ra, dec, roll
			printf,lulog,'RA, Dec, Roll of image = ', ra, dec, roll

			rawy = extract_int(buffer_pointer)
			rawx = extract_int(buffer_pointer)
			print,'RAWX, RAWY = ', rawx, rawy
			printf,lulog,'RAWX, RAWY = ', rawx, rawy

			gain = extract_byte(buffer_pointer)
			print,'Gain for this image: ', gain
			printf,lulog,'Gain for this image: ', gain

			for i=0,866 do begin
				pixel = extract_byte(buffer_pointer)
				npix = npix + 1
				image[npix] = fix(pixel)*gain
				if (idebug gt 10) then begin
					print,npix,pixel,image[npix]
					printf,lulog,npix,pixel,image[npix]
				endif
			endfor
			centroid_y = extract_float(buffer_pointer)
			centroid_x = extract_float(buffer_pointer)
			print,'Centroid (X,Y) = (', centroid_x, centroid_y, ')'
			printf,lulog,'Centroid (X,Y) = (', centroid_x, centroid_y, ')'
			sigma = extract_float(buffer_pointer)
			sigma_max = extract_float(buffer_pointer)
			events_in_centroid = extract_long(buffer_pointer)
			min_events = extract_long(buffer_pointer)
			num_iter = extract_int(buffer_pointer)
			max_iter = extract_int(buffer_pointer)
			dconverge = extract_float(buffer_pointer)
			max_dconv = extract_float(buffer_pointer)
			mode = extract_byte(buffer_pointer)
			waveform = extract_byte(buffer_pointer)
			print,'Sigma, sig_max = ', sigma, sigma_max
			print,'N_events, min_events = ', events_in_centroid, min_events
			print,'Num_iter, max_iter = ', num_iter, max_iter
			print,'dconverge, max_dconv = ', dconverge, max_dconv
			print,'Readout mode and waveform: ', mode, waveform
			printf,lulog,'Sigma, sig_max = ', sigma, sigma_max
			printf,lulog,'N_events, min_events = ', events_in_centroid, min_events
			printf,lulog,'Num_iter, max_iter = ', num_iter, max_iter
			printf,lulog,'dconverge, max_dconv = ', dconverge, max_dconv
			printf,lulog,'Readout mode and waveform: ', mode, waveform
		    end
		2: begin
			ccd_frame = extract_long(buffer_pointer)
			print,'CCD frame number is ', ccd_frame
			printf,lulog,'CCD frame number is ', ccd_frame
			if (ccd_frame ne old_ccd_frame) then begin
				print_error,0,1,'POSTAGE_STAMP: Error in ccd frame for packet 2: ' $
					+ strtrim(string(ccd_frame),2)
			endif
			for i=0,917 do begin
				pixel = extract_byte(buffer_pointer)
				npix = npix + 1
				image(npix) = fix(pixel)*gain
				if (idebug gt 10) then begin
					print,npix,pixel,image(npix)
					printf,lulog,npix,pixel,image(npix)
				endif
			endfor
			flux_factor = extract_float(buffer_pointer)
			print,'Flux factor = ', flux_factor
			printf,lulog,'Flux factor = ', flux_factor
		    end
		3: begin
			ccd_frame = extract_long(buffer_pointer)
			print,'CCD frame number is ', ccd_frame
			printf,lulog,'CCD frame number is ', ccd_frame
			if (ccd_frame ne old_ccd_frame) then begin
				print_error,0,1,'POSTAGE_STAMP: Error in ccd frame for packet 3: ' $
					+ strtrim(string(ccd_frame),2)
			endif
			for i=0,815 do begin
				pixel = extract_byte(buffer_pointer)
				npix = npix + 1
				image(npix) = fix(pixel)*gain
				if (idebug gt 10) then begin
					print,npix,pixel,image(npix)
					printf,lulog,npix,pixel,image(npix)
				endif
			endfor
			
			boresight_column = extract_float(buffer_pointer)
			boresight_row = extract_float(buffer_pointer)
			boresight_roll = extract_float(buffer_pointer)
			print,'Boresight column, row, and roll = ', boresight_column, boresight_row, boresight_roll
			printf,lulog,'Boresight column, row, and roll = ', boresight_column, boresight_row, boresight_roll
	
			platescale = extract_float(buffer_pointer)
			print,'Platescale = ', platescale,' arcsec/pixel'
			printf,lulog,'Platescale = ', platescale, ' arcsec/pixel'

			sc_y = extract_float(buffer_pointer)
			sc_z = extract_float(buffer_pointer)
			sc_roll = extract_float(buffer_pointer)
			print,'S/C Y, Z, Roll of boresight: ', sc_y, sc_z, sc_roll
			printf,lulog,'S/C Y, Z, Roll of boresight: ', sc_y, sc_z, sc_roll
			if (events_in_centroid gt 0) then begin
			
				tam_refx1 = extract_float(buffer_pointer)
				tam_refy1 = extract_float(buffer_pointer)
				tam_refx2 = extract_float(buffer_pointer)
				tam_refy2 = extract_float(buffer_pointer)
				print,'TAM centroids: X1, Y1, X2, Y2 = ', tam_refx1, tam_refy1, tam_refx2, tam_refy2
				printf,lulog,'TAM ref windows: X1, Y1, X2, Y2 = ', tam_refx1, tam_refy1, tam_refx2, tam_refy2

				tam_thetat = extract_float(buffer_pointer)
				tam_optical_gain = extract_float(buffer_pointer)
				print,'TAM Theta_t = ', tam_thetat
				print,'TAM optical gain = ', tam_optical_gain
				printf,lulog,'TAM Theta_t = ', tam_thetat
				printf,lulog,'TAM optical gain = ', tam_optical_gain

				tam_pri_scale = extract_float(buffer_pointer)
				tam_st_scale = extract_float(buffer_pointer)
				print,'TAM primary window plate scale = ', tam_pri_scale
				print,'TAM ST window plate scale = ', tam_st_scale
				printf,lulog,'TAM primary window plate scale = ', tam_pri_scale
				printf,lulog,'TAM ST window plate scale = ', tam_st_scale

				tam_sequence_counter = extract_long(buffer_pointer)
				print,''
				print,'TAM image sequence counter: ', tam_sequence_counter
				printf,lulog,''
				printf,lulog,'TAM image sequence counter: ', tam_sequence_counter

				tam_init = extract_byte(buffer_pointer)
				tam_corr_en = extract_byte(buffer_pointer)
				print,'Has TAM centroid been calculated?  A: ', tam_init
				print,'Is TAM correction enabled?  A: ', tam_corr_en
				printf,lulog,'Has TAM centroid been calculated?  A: ', tam_init
				printf,lulog,'Is TAM correction enabled?  A: ', tam_corr_en

				nominal_exp_time = extract_float(buffer_pointer)
				print,'Nominal exposure time = ', nominal_exp_time
				printf,lulog,'Nominal exposure time = ', nominal_exp_time

				sc_ra = extract_float(buffer_pointer)
                sc_dec = extract_float(buffer_pointer)
                sc_ra=double(sc_ra)
                sc_dec=double(sc_dec)
				print,'S/C RA and Dec: ', sc_ra, sc_dec
				printf,lulog,'S/C RA and Dec: ', sc_ra, sc_dec
			
				gy = extract_float(buffer_pointer)
				gz = extract_float(buffer_pointer)
				print,'Intermediate results: Gy, Gz = ', gy, gz
				printf,lulog,'Intermediate results: Gy, Gz = ', gy, gz

				grb_ra = extract_double(buffer_pointer)
				grb_dec = extract_double(buffer_pointer)
				print,'GRB RA, Dec = ', grb_ra, grb_dec
				printf,lulog,'GRB RA, Dec = ', grb_ra, grb_dec

				window_halfwidth = extract_int(buffer_pointer)
				print,'Window Half-Width = ', window_halfwidth
				printf,lulog,'Window Half-Width = ', window_halfwidth

				; skip spare bytes
				buffer_pointer = buffer_pointer + 2
			endif

			image = reform(image,51,51)
			if not keyword_set(quicklook) then tvscl,rebin(image,255,255,/sample)

			if (idebug gt 5) then print,image

			filenm = filein + '.' $
				+ strtrim(string(XRT_sec),2) + '.postage_stamp.fits' 

			mkhdr,hdr,image
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
			sxaddpar,hdr, 'OBS_NUM', $
					strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
					'Swift Observation Number'
			sxaddpar,hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
			sxaddpar,hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
			sxaddpar,hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
			sxaddpar,hdr, 'TSTART', frame_start_time, 'Satellite time of exposure start'
			sxaddpar,hdr,'CCD_FRAME',ccd_frame,'On-board CCD Frame Counter'
			sxaddpar,hdr, 'RA',ra,'Right Ascension of image center (degrees)'
			sxaddpar,hdr, 'DEC',dec,'Declination of image center (degrees)'
			sxaddpar,hdr, 'Roll',roll,'Roll angle of image (degrees)'
			sxaddpar,hdr, 'RAWX', rawx, 'Column number of image center in CCD pixels'
			sxaddpar,hdr, 'RAWY', rawy, 'Column number of image center in CCD pixels'
			sxaddpar,hdr,'GAIN',gain,'Inverse gain (image=raw_data*gain)'
			sxaddpar,hdr,'CNTDR_X',centroid_x,'Centroid position in RAWX coords'
			sxaddpar,hdr,'CNTDR_Y',centroid_y,'Centroid position in RAWY coords'
			sxaddpar,hdr,'SIGMA',sigma,'Standard deviation of centroid'
			sxaddpar,hdr,'MAX_SIG',sigma_max,'Max. Std. Dev. for success'
			sxaddpar,hdr,'N_EVENTS',events_in_centroid,'# events in centroid window'
			sxaddpar,hdr,'MIN_EVNT',min_events,'Min # events for success'
			sxaddpar,hdr,'N_ITER',num_iter,'Number of iterations'
			sxaddpar,hdr,'MAX_ITER',max_iter,'Max iterations for success'
			sxaddpar,hdr,'DCONV',dconverge,'Actual convergence distance'
			sxaddpar,hdr,'MAX_CONV',max_dconv,'Max convergence distance for success'
			sxaddpar,hdr,'HWIDTH',window_halfwidth,'Centroid window half-width'
			sxaddpar,hdr,'K_FLUX',flux_factor,'Approximate source flux is: (DN-BIAS)*K_FLUX'
			sxaddpar,hdr,'BORE_COL',boresight_column,'Boresight column in RAWX'
			sxaddpar,hdr,'BORE_ROW',boresight_row,'Boresight row in RAWY'
			sxaddpar,hdr,'BORE_ROL',boresight_roll,'Boresight roll angle in degrees'
			sxaddpar,hdr,'PLATE',platescale,'CCD plate Scale in arcsec/pixel'
			sxaddpar,hdr,'SY',sc_y,'S/C to XRT boresight correction in arcseconds'
			sxaddpar,hdr,'SZ',sc_z,'S/C to XRT boresight correction in arcseconds'
			sxaddpar,hdr,'S_ROLL',sc_roll,'S/C to XRT boresight roll correction in degrees'
			if (events_in_centroid gt 0) then begin
				sxaddpar,hdr,'TAMREFX1',tam_refx1,'TAM Window 1 Reference X position'
				sxaddpar,hdr,'TAMREFY1',tam_refy1,'TAM Window 1 Reference Y position'
				sxaddpar,hdr,'TAMREFX2',tam_refx2,'TAM Window 2 Reference X position'
				sxaddpar,hdr,'TAMREFY2',tam_refy2,'TAM Window 2 Reference Y position'
				sxaddpar,hdr,'TAM_THT',tam_thetat,'TAM angle theta_t (degrees)'
				sxaddpar,hdr,'TAM_GAIN',tam_optical_gain,'TAM optical gain'
				sxaddpar,hdr,'TAM_PSCL',tam_pri_scale,'TAM Window 1 plate scale'
				sxaddpar,hdr,'TAM_SSCL',tam_st_scale,'TAM Window 2 plate scale'
				sxaddpar,hdr,'TAM_SEQ',tam_sequence_counter,'TAM image sequence counter'
				sxaddpar,hdr,'TAM_INIT',tam_init,'BOOLEAN: Is TAM Initialized?'
				sxaddpar,hdr,'TAM_CORR',tam_corr_en,'BOOLEAN: Is TAM Correction Enabled?'
				sxaddpar,hdr,'T_NOM',nominal_exp_time,'Nominal exposure time (for flux estimate)'
				sxaddpar,hdr,'SC_RA',sc_ra,'Spacecraft RA'
				sxaddpar,hdr,'SC_DEC',sc_dec,'Spacecraft Declination'
				sxaddpar,hdr,'GY',gy,'GRB Y position in XRT coordinates'
				sxaddpar,hdr,'GZ',gz,'GRB Z position in XRT coordinates'
				sxaddpar,hdr,'GRB_RA',grb_ra,'GRB Right Ascension, high precision'
				sxaddpar,hdr,'GRB_DEC',grb_dec,'GRB Declination, high precision'
			endif
			writefits,filenm,image,hdr
			npix = -1
			print,''
			print,'Image written to file ', filenm
			printf,lulog,''
			printf,lulog,'Image written to file ', filenm
			print,''
			print,'**************************************************'
			printf,lulog,''
			printf,lulog,'***********************************************'
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
