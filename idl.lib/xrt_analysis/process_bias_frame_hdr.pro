pro process_bias_frame_hdr, buffer_pointer, error_code
; $Id: process_bias_frame_hdr.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: process_bias_frame_hdr
;author	: Dave Burrows
;date	: 01/24/95
;lang	: IDL

;purpose: This routine processes the Swift XRT Bias Map Frame Header.
;	It extracts data from this header for the output FITS header and creates
;	a FITS header structure.
;	It returns an error code which is 0 if no errors are found:
;	error_code	
;	    0	no errors
;	   91	PROCESS_BIAS_FRAME_HDR: EOF encountered before end of CCD header
;
; TBD: Missing items (vs CUBIC):
;	livetime
;	pixel sample
;
; Rev:
;	02/13/04 by DNB: support added recently for variable size bias map
;		telemetry windows.  This broke data processing for old format
;		files.  Had to modify to allow both to process correctly.
;	06/10/03 by DNB: fixed error in spare bytes at end of header
;	06/02/03 by DNB: fixed to check for amplifier number errors
;	05/13/03 by DNB: fixed errors in bias frame numbering
;	03/27/03 by DNB: removed erroneous error message
;	08/09/02 by DNB: fixed error in first_frame logic
;	04/28/02 by DNB: changed to read correct header format.
;	04/10/02 by DNB: added warning for fake TAM data
;	04/07/02 by DNB: deleted last four time words from HK array
;	04/02/02 by DNB: changed assignment of waveform numbers to amp
;	04/02/02 by DNB: corrected some errors in calculation of frame start
;		and stop times.
;	02/10/02 by DNB: changed size of hk_array to allow separate words for
;		ACS states, since they are independent
;	02/08/02 by DNB: corrected header HK conversions, deleted offset keyword
;
; $Log: process_ccd_frame_hdr.pro $
;
;	09/03/96 by DNB: Added waveform number
;
;

@pass1_common

common save_data,oldwaveform,n_frames	; saves this for next entry

; "declare" variable types
error_code = 0

; Get more data if this frame header is not contained entirely within the current
;	data block
if (buffer_pointer ge (data_buffer_size-158-22)) then begin
	print,''
	if (append_data_block(buffer_pointer) ne 0) then begin
		error_code = 91
	endif
endif

; Read CCD Frame number
bias_frame_num = extract_long(buffer_pointer)

if (bias_frame eq 1) then begin
	first_bias_frame = bias_frame_num
endif 
if (bias_frame_num ne bias_frame) then begin
    if (bias_frame ne 1) then begin
	print_error,0,1,'PROCESS_BIAS_FRAME_HDR: ERROR in Bias Frame #: found '+ $
		strtrim(string(bias_frame_num),2)+', expected '+ $
		strtrim(string(bias_frame),2)
    endif
    bias_frame = bias_frame_num		; synchronize the counters
endif

print,'Processing CCD Bias Frame #', bias_frame_num
printf,lulog,'Processing CCD Bias Frame #', bias_frame_num

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

; NOTE: following lines removed because mode in Bias Map header is usually 1, not 9
if (xrt_mode ne 9) then print_error,0,1,'PROCESS_BIAS_FRAME_HDR: wrong xrt_mode found for Bias Map mode: mode flag is ' $
	+ strtrim(string(xrt_mode,format='(i)'),2)

waveform = fix(extract_byte(buffer_pointer))
if ((waveform lt 0)or(waveform gt 255)) then begin
	if (n_elements(oldwaveform) eq 0) then begin
		print,''
		print,'PROCESS_BIAS_FRAME_HDR: *** INVALID WAVEFORM NUMBER FOUND: ', waveform, ' ***'
		print,'waveform=1-127 for amp 1, waveform=128-255 for amp 2'
		read,'Enter waveform number for this CCD frame: ', waveform
		oldwaveform = waveform
	endif else waveform = oldwaveform
endif
if (waveform lt 128) then amp_num=1 else amp_num=2

if (idebug gt 3) then begin
	print,'ACS flags = ', acs_flags
	print,'XRT state = ', state
	print,'Waveform = ', waveform
endif

;count_rate = extract_float(buffer_pointer)
;print,'Count rate is ', count_rate
buffer_pointer = buffer_pointer + 4

tam_x1 = extract_float(buffer_pointer)
tam_y1 = extract_float(buffer_pointer)
tam_x2 = extract_float(buffer_pointer)
tam_y2 = extract_float(buffer_pointer)

if ((tam_x1 eq 1.0)and(tam_y1 eq 2.0)and(tam_x2 eq 3.0)and(tam_y2 eq 4.0)) then $
	print_error,0,1,'PROCESS_BIAS_FRAME_HDR: WARNING: found fake TAM data'

;; Now, process HK data
;if (process_hk eq 0) then begin		; open HK file
;	process_hk = 1
;	get_lun,luhk
;	file_hk = filebase + '.internal_temp_hk_file'
;	openw,luhk,file_hk,/swap_if_little_endian
;endif

hk_array = uintarr(34)
for i=0,27 do begin
	hk_array[i] = extract_int(buffer_pointer)
endfor
ccd_temp = RTD(hk_array[0], rtd_channel[hdr_hk_chan[0]])

hk_array[28] = xrt_state
hk_array[29] = xrt_mode
hk_array[30:33] = acs_flags

;buffer_pointer = buffer_pointer - 2	; TBD: delete this line when header format is fixed
readout_start_sec = extract_long(buffer_pointer)
readout_start_subsec = extract_int(buffer_pointer)
readout_start_time = readout_start_sec + readout_start_subsec*20.0D-6

readout_stop_sec = extract_long(buffer_pointer)
readout_stop_subsec = extract_int(buffer_pointer)
readout_stop_time = readout_stop_sec + readout_stop_subsec*20.0D-6

if (readout_stop_time lt readout_start_time) then print_error,0,1, $
	'PROCESS_BIAS_FRAME_HDR: WARNING: Frame START time > Frame STOP time in CCD frame ' + strtrim(string(bias_frame_num),2) + '!!'

satellite_time = (readout_start_time + readout_stop_time)/2.0

printf,lustats, format='(i10,'' IM '', 20x, 2f20.6)', $
	observation_number, readout_start_time, readout_stop_time

num_bias_frames = extract_long(buffer_pointer)

bias_ncol = 600
bias_nrow = 600
image = intarr(bias_ncol,bias_nrow)

amp = extract_byte(buffer_pointer)
if (amp ne amp_num) then begin
    print_error,0,1,$
	'PROCESS_BIAS_FRAME_HDR: Wrong Amp for this waveform number - waveform = ' $
	+ strtrim(string(waveform),2) + ', amp = ' + strtrim(string(fix(amp)),2)
endif

;
; Extract bias map window size.  If size is zero, assume this data file is in
; in old format (before window was implemented) and set size to 600x600
;
windowHalfwidth = extract_int(buffer_pointer)
if (windowHalfwidth gt 0) then bias_ncol = 2*windowHalfwidth
windowHalfheight = extract_int(buffer_pointer)
if (windowHalfheight gt 0) then bias_nrow = 2*windowHalfheight
Npix = extract_long(buffer_pointer)
if (Npix eq 0) then Npix = ulong(bias_ncol)*ulong(bias_nrow)
if (Npix ne ulong(bias_ncol)*ulong(bias_nrow)) then begin
	print, ' '
	print, 'PROCESS_BIAS_FRAME_HDR: bias_nrow, bias_ncol, Npix = ', bias_nrow, bias_ncol, Npix
	stop, 'PROCESS_BIAS_FRAME_HDR: error in number of bias pixels'
endif

; skip over the spare words
buffer_pointer = buffer_pointer + 1

if (idebug ge 10) then begin
	print,''
	print,'Housekeeping data: at time ', satellite_time
	print,format='("PROCESS_BIAS_FRAME_HDR: ",10(z6,1x))',hk_array
endif


if (idebug ge 15) then print,'PROCESS_BIAS_FRAME_HDR: HK = ', hk_array
;num_hk_records = num_hk_records + 1
;
;; write output data to file as unformatted binary data
;writeu,luhk,satellite_time,hk_array


; Now, create the FITS file header

file_info = fstat(luin)
file_date = bin_date(systime(0))
date = string(file_date(2),file_date(1),format="(i2.2,'/',i2.2,'/')") $
		+ strmid(string(file_date(0),format="(i4.4)"),2,2)

mkhdr,ccd_hdr,image		; TBD: change so this uses empty image array
sxaddhist, ' ', ccd_hdr
sxaddhist,'*** SWIFT XRT *** CCD Bias Frame processed by PASS1 '+version, $
			ccd_hdr
sxaddpar,ccd_hdr,'ORIGIN','PSU X-ray Astronomy', $
			'Data from Penn State X-ray Astronomy'
sxaddpar, ccd_hdr, 'TELESCOP', 'Swift', 'Swift Gamma-Ray Burst Explorer'
sxaddpar,ccd_hdr,'INSTRUME','XRT',$
			'XRT Instrument on Swift GRB Explorer'
sxaddhist, ' ', ccd_hdr
sxaddpar, ccd_hdr, 'OBS_NUM', $
	strtrim(string(observation_number,format='(''0x'',z8.8)'),2), $
	'Swift Observation Number'
sxaddpar, ccd_hdr, 'TARGETID', target_id, 'Swift Target ID'
sxaddpar, ccd_hdr, 'OBS_SEG', obs_segment, 'Swift Observation Segment for this target'
sxaddpar, ccd_hdr, 'SEQ_NUM',  seq_num, 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
sxaddhist, ' ', ccd_hdr
sxaddpar, ccd_hdr, 'OBS_MODE', obs_mode
sxaddpar, ccd_hdr, 'DATAMODE', readout_mode,'XRT CCD Bias Map'
sxaddpar, ccd_hdr, 'CREATOR',  'PASS1 (bias_map), '+version

sxaddhist, ' ', ccd_hdr
sxaddhist, 'XRT Bias Map', ccd_hdr
sxaddpar, ccd_hdr,'FILE',file_info.name,'Name of input data file'
sxaddpar, ccd_hdr,'DATE',date,'File creation date'
sxaddpar, ccd_hdr,'FRAME',bias_frame,'Bias frame number for this file'
sxaddpar, ccd_hdr,'CMPRSD','F','This frame was not taken in compressed format'
sxaddpar, ccd_hdr,'RAW','T','This frame collected in raw format'

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Frame Time Stamps:', ccd_hdr
sxaddpar, ccd_hdr, 'MET_STRT', readout_start_time, 'Satellite time of readout start'
sxaddpar, ccd_hdr, 'MET_STOP', readout_stop_time, 'Satellite time of readout end'
;sxaddpar, ccd_hdr, 'TSTART', frame_start_time, 'Satellite time of exposure start'
;sxaddpar, ccd_hdr, 'TSTOP', frame_stop_time, 'Satellite time of exposure end'
;sxaddpar, ccd_hdr, 'TELAPSE', frame_stop_time - frame_start_time, 'Elapsed time'
;sxaddpar, ccd_hdr, 'ONTIME', exposure_time,'Total good time for this frame'
;sxaddpar, ccd_hdr, 'EXPOSURE', exposure_time,'CCD Exposure time in seconds'
;sxaddpar, ccd_hdr, 'LIVETIME', livetime, 'Livetime for this frame'

sxaddhist, ' ', ccd_hdr
sxaddhist,'Aspect Information:',ccd_hdr
sxaddpar,ccd_hdr,'RA',ra,'Right Ascension (degrees)'
sxaddpar,ccd_hdr,'DEC',dec,'Declination (degrees)'
sxaddpar,ccd_hdr,'Roll',roll,'Roll angle (degrees)'
sxaddpar,ccd_hdr,'ACS_Flags', fix(acs_flag_word), $
	'ACS Flags: 0x'+strtrim(string(acs_flag_word,format='(z2.2)'),2)
sxaddpar,ccd_hdr,'SAFEHOLD',safe_mode,'Is the S/C in Safe Hold?'
sxaddpar,ccd_hdr,'SAA',in_saa,'Is the S/C in the SAA?'
sxaddpar,ccd_hdr,'10arcmin',is_in_10_arcmin,'Within 10 arcmin of target?'
sxaddpar,ccd_hdr,'SETTLED',is_settled,'Settled on target?'
sxaddpar,ccd_hdr,'TAM_X1',tam_x1,'TAM Window 1'
sxaddpar,ccd_hdr,'TAM_Y1',tam_y1,'TAM Window 1'
sxaddpar,ccd_hdr,'TAM_X2',tam_x2,'TAM Window 2'
sxaddpar,ccd_hdr,'TAM_Y2',tam_y2,'TAM Window 2'

; Set up CCD frame parameters

sxaddhist,' ', ccd_hdr
sxaddhist,'CCD Parameters',ccd_hdr

sxaddpar,ccd_hdr,'CCD_FRAME',bias_frame_num,'On-board CCD Frame Counter'
sxaddpar,ccd_hdr,'CCD_NUM',1,'CCD Number'
sxaddpar,ccd_hdr,'AMP',fix(amp),'CCD Amplifier (1 or 2)'
sxaddpar,ccd_hdr,'WAVEFORM',waveform,'CCD Readout Waveform Number'
sxaddpar, ccd_hdr, 'StateFlg',fix(xrt_state), $
	'XRT State Flag: 0x'+strtrim(string(xrt_state,format='(z2.2)'),2)
sxaddpar, ccd_hdr, 'XRTSTATE', state, 'XRT State'
sxaddpar,ccd_hdr,'XRT_Mode',fix(xrt_mode),'XRT Mode Flag'
sxaddpar,ccd_hdr,'READMODE', readout_mode, 'XRT Readout Mode'
sxaddpar,ccd_hdr,'WIDTH', bias_ncol, 'Bias Map T/M Window Width'
sxaddpar,ccd_hdr,'HEIGHT', bias_nrow, 'Bias Map T/M Window Height'
sxaddpar,ccd_hdr,'Npix', Npix, 'Number of pixels in Bias Map Window'

;;;sxaddpar,ccd_hdr,'LLD',event_threshold,'Event threshold (Lower Level Discriminator)'
;sxaddpar,ccd_hdr,'SPLIT',split_threshold,'Split Event threshold (DN)'
;;;sxaddpar,ccd_hdr,'ULD',ULD,'Upper Level Discriminator (DN)'
;;;sxaddpar,ccd_hdr,'N_EVENTS',N_events,'Number of valid X-ray events in frame'
;sxaddpar,ccd_hdr,'N_SINGLE',N_single,'Number of single pixel X-ray events in frame'
;sxaddpar,ccd_hdr,'N_SPLIT',N_split,'Number of singly split X-ray events in frame'
;sxaddpar,ccd_hdr,'N_MULT', N_multiples,'Number of vetoed multiple events'
;;;sxaddpar,ccd_hdr,'N_GT_LLD',N_pixels_over_LLD,'Number of pixels over LLD'
;;;sxaddpar,ccd_hdr,'N_GT_ULD',N_pixels_over_ULD,'Number of pixels over ULD'
;sxaddpar,ccd_hdr,'ROW_CNTR',row_counter,'CCD Processing Row Counter'
;sxaddpar,ccd_hdr,'MN_BSLN',mean_row_baseline,'Mean Row Baseline level'

; Put HK values in header

sxaddhist, ' ', ccd_hdr
sxaddhist, 'CCD Housekeeping: ', ccd_hdr
sxaddpar,ccd_hdr,'T_CCD',ccd_temp,'CCD Temperature (degrees C)'
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

return

end
