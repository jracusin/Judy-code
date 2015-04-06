pro comandstats, filename, showplot, debug_level, DEBUG5=debug5, $
                  NOINIT=noinit, noplots=noplots, noprint=noprint, $
                  quicklook=quicklook
; $Id: comandstats

;name	: commandstats
;author	: Judy Racusin
;date	: 08/19/04
;lang	: IDL

;purpose: This program reads comandstats.0.  Modified from stripchart.
;
;
;
; Rev:
;
;
;

@pass1_common

@pass1_path_init                ; initialize paths for input files
  version = 'V2.0'
  if keyword_set(quicklook) then quick_look_mode=1 else quick_look_mode = 0

  common save_data,oldwaveform,n_frames	; saves this for next entry

  if not (keyword_set(noinit)) then begin
@pass1_init                     ; initialize common block variables.
  endif

  GET_LUN, luin           ; Get Logical Unit Numbers for input and output files

  if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select XRT data file: ', $
                       filter='*com*andstats*', /must_exist)
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
  no_plot=0
  if keyword_set(noplots) then begin
     print_plots = 0
     show_plot = 1
     show_ccd_image = 1
     show_hk_image = 1
     no_plot=1
  endif
  
  if keyword_set(noprint) then print_plots=0

; Check that file exists

  openr,luin,filein,error=file_error
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select XRT data file: ', /must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
     openr,luin,filein,error=file_error
  endrep until (file_error eq 0)
  close,luin

  GET_LUN, lulog
  openw,lulog,filein+'.comandstats.log'

  GET_LUN, luerr
  openw,luerr,filein+'.comandstats.err'
  
  GET_LUN,luout
  openw,luout,filein+'.comandstats.txt'
  printf,luout,'        SCTIME        SCUI Low Priority Buffer Depth'
  

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
  ccsds_id1 = '0C88'x           ; RT HK version
  ccsds_id2 = '0D38'x           ; TDRSS version

  byte1 = 0B
  byte2 = 0B
  
  if (show_plot gt 0) or (print_plots gt 0) then process_cs=1 else process_cs=0
  
; Open input file
  get_lun,luin
  openr,luin,filein,error=file_error,/swap_if_little_endian
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
     read,'Enter name of XRT input file: ', file_in
     openr,luin,file_in,error=file_error,/swap_if_little_endian
  endrep until (file_error eq 0)

; Read the file and process the data

  print,' '
  print,'Reading the COMANDSTATS telemetry file ...'
  print,' '

  packet_num = 0
  repeat begin
     packet_num = packet_num + 1
;
; Search for a valid CCSDS packet header ID
;	WARNING: this technique fails if there is an offset by an
;	odd number of bytes!  (This situation should never occur).
; This routine only processes Ap_IDs corresponding to COMANDSTATS packets.
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
     if (control ne 'C000'x) then stop, 'COMANDSTATS: Error in file format: invalid control bits'
     sequence = seq_cntl and '3FFF'x
     readu,luin,byte1,byte2
     packet_length = byte1*256+byte2
     if (idebug ge 10) then begin
        print,'COMANDSTATS: ---------------------------------'
        print,'COMANDSTATS: reading CCSDS packet #', sequence
        print,FORMAT='(A,3Z10,I10)',$
           'COMANDSTATS: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
           seq_cntl, control, sequence
        print,'COMANDSTATS: packet length = ', packet_length
        printf,lulog,'COMANDSTATS: ---------------------------------'
        printf,lulog,'COMANDSTATS: reading CCSDS packet #', sequence
        printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
           'COMANDSTATS: CCSDS ID, control, Seq = ', header_id,$
           control, sequence
        printf,lulog,'COMANDSTATS: packet length = ', packet_length
     endif
     old_seq = sequence
     if (old_seq eq 16383) then old_seq = -1 ; sequence #s are mod 16384


                                ; Read the rest of the data into a buffer
     buffer_size = (packet_length+1)
     buffer = bytarr(buffer_size)
     readu,luin,buffer

                                ; Now unpack the secondary CCSDS header (time)
     ccsds_sec = buffer[0]*65536UL*256UL + buffer[1]*65536UL + buffer[2]*256UL + buffer[3]
     ccsds_subsec = buffer[4]*256UL + buffer[5]
     ccsds_time = double(ccsds_sec) + double(ccsds_subsec)*20.0D-6
     if (idebug ge 10) then begin
        print,'COMANDSTATS: CCSDS time =  ', ccsds_time
        printf,lulog,'COMANDSTATS: CCSDS time =  ', ccsds_time
     endif
     ccsds_time_old = ccsds_time
     old_sec = ccsds_sec
     old_subsec = ccsds_subsec

                                ; Calculate the checksum
     if (idebug ge 10) then help,checksum
     checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
     if (idebug ge 10) then begin
        print, 'COMANDSTATS: Total of all bytes outside CCSDS headers: ', checksum
        printf,lulog, 'COMANDSTATS: Total of all bytes outside CCSDS headers: ', checksum
     endif
     check = checksum

                                ; Now get checksum value from file
     chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

     if (idebug ge 10) then begin
        help,checksum
        help,chksum
        print,FORMAT='(A,Z10)','COMANDSTATS: Checksum in file is ', chksum
        print,FORMAT='(A,Z10)','COMANDSTATS: Calculated checksum:', checksum
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
           'COMANDSTATS: WARNING: Satellite time decreased!'
        print_error,0,1, 'COMANDSTATS:      Last time =  ' $
           + string(last_time,format='(f20.6)')
        print_error,0,1, 'COMANDSTATS:      Current time=' $
           + string(satellite_time,format='(f20.6)')
     endif

     if (packet_num eq 1) then first_time = satellite_time $
     else last_time = satellite_time

     UTC_sec = long(extract_long(buffer_pointer))
     UTC_subsec = extract_int(buffer_pointer) 
     UTC_delta = UTC_sec + UTC_subsec*20.0D-6

     if (packet_num eq 1) then first_utc = UTC_delta $
     else last_utc = UTC_delta
     
     ccmcomrec=extract_int(buffer_pointer) ;2
     ccmcomexc=extract_int(buffer_pointer) ;2
     ccmcomrej=extract_int(buffer_pointer) ;2
     ccmlastrej=extract_int(buffer_pointer) ;2
     ppcomstat=extract_double(buffer_pointer) ;8 
     thccomstat=extract_double(buffer_pointer) ;8
     bhccomstat=extract_double(buffer_pointer) ;8 
     dcccomstat=extract_double(buffer_pointer) ;8
     erpcomstat=extract_double(buffer_pointer) ;8
     tamcomstat=extract_double(buffer_pointer) ;8
     teccomstat=extract_double(buffer_pointer) ;8
     xrtcomrec=extract_long(buffer_pointer) ;4
     xrtcomexc=extract_long(buffer_pointer) ;4
     xrtcomrej=extract_long(buffer_pointer) ;4
     xrtlastrej=extract_double(buffer_pointer) ;8
     sccomrec=extract_long(buffer_pointer) ;4
     sccomexc=extract_long(buffer_pointer) ;4
     sccomrej=extract_long(buffer_pointer) ;4
     sclastrej=extract_double(buffer_pointer) ;8
     fomcomrec=extract_long(buffer_pointer) ;4
     fomcomexc=extract_long(buffer_pointer) ;4
     fomcomrej=extract_long(buffer_pointer) ;4
     fomlastrej=extract_double(buffer_pointer) ;8
     sitimetonerec=extract_long(buffer_pointer) ;4
     sitimetoneexc=extract_long(buffer_pointer) ;4
     sitimetonerej=extract_long(buffer_pointer) ;4
     last_sclk_seconds=extract_long(buffer_pointer) ;4
     lower_apid=extract_int(buffer_pointer) ;2 
     upper_apid=extract_int(buffer_pointer) ;2
     red_apid=extract_int(buffer_pointer) ;2 
     red_func=extract_byte(buffer_pointer) ;1
     xrt_state=extract_byte(buffer_pointer) ;1
     scui_poll=extract_long(buffer_pointer) ;4
     scui_buffer=extract_long(buffer_pointer) ;4
     scui_low_buf_size=extract_long(buffer_pointer) ;4
     scui_high_buf_size=extract_long(buffer_pointer) ;4
     scui_low_buf_dep=extract_long(buffer_pointer) ;4
     scui_high_buf_dep=extract_long(buffer_pointer) ;4
     scui_bytes_sent=extract_long(buffer_pointer) ;4
     scui_stpdu_sent=extract_long(buffer_pointer) ;4
     scuicomrec=extract_int(buffer_pointer) ;2
     scuicomref=extract_int(buffer_pointer) ;2
     for z=0,3 do spares=extract_long(buffer_pointer) ;16
     
     
     printf,luout,satellite_time,scui_low_buf_dep
     
;	ra = extract_float(buffer_pointer)
;	dec = extract_float(buffer_pointer)
;	roll = extract_float(buffer_pointer)

; 	acs_flags = bytarr(4)
; 	acs_flag_word = extract_byte(buffer_pointer)
; 	if ((acs_flag_word and '08'x) eq 8) then begin
; 		safe_mode = 'T' 
; 		acs_flags[3] = 1
; 	endif else safe_mode = 'F'
; 	if ((acs_flag_word and '04'x) eq 4) then begin
; 		in_saa = 'T' 
; 		acs_flags[2] = 1
; 	endif else in_saa = 'F'
; 	if ((acs_flag_word and '02'x) eq 2) then begin
; 		is_in_10_arcmin = 'T' 
; 		acs_flags[1] = 1
; 	endif else is_in_10_arcmin = 'F'
; 	if ((acs_flag_word and '01'x) eq 1) then begin
; 		is_settled = 'T' 
; 		acs_flags[0] = 1
; 	endif else is_settled = 'F'

; 	xrt_state = extract_byte(buffer_pointer)
; 	case xrt_state of
; 		'11'x: state = 'Auto'
; 		'22'x: state = 'Manual'
; 		'44'x: state = 'RED'
; 		else: state = 'Undefined'
; 	endcase

; 	xrt_mode = extract_byte(buffer_pointer)
; 	case xrt_mode of
; 		1: readout_mode = 'Null'
; 		2: readout_mode = 'Short Image'
; 		3: readout_mode = 'Long Image'
; 		4: readout_mode = 'Piled-up Photodiode'
; 		5: readout_mode = 'Low Rate Photodiode'
; 		6: readout_mode = 'Windowed Timing'
; 		7: readout_mode = 'Photon-Counting'
; 		8: readout_mode = 'Raw Data'
; 		9: readout_mode = 'Bias Map'
; 		10: readout_mode = 'Stop'
; 		else: readout_mode = 'Undefined'
; 	endcase

; 	waveform = fix(extract_byte(buffer_pointer))
; 	if ((waveform lt 0)or(waveform gt 255)) then begin
; 		if (n_elements(oldwaveform) eq 0) then begin
; 			print,''
; 			print,'COMANDSTATS: *** INVALID WAVEFORM NUMBER FOUND: ', waveform, ' ***'
; 			print,'waveform=1-127 for amp 1, waveform=128-255 for amp 2'
; 			read,'Enter waveform number for this CCD frame: ', waveform
; 			oldwaveform = waveform
; 		endif else waveform = oldwaveform
; 	endif
; 	if (waveform lt 128) then amp=1 else amp=2	

; 	count_rate = extract_float(buffer_pointer)

 	; Now, process CS data 
 	if (process_cs eq 0) then begin		; open HK file
 		process_cs = 1
 		get_lun,lucs
 		file_cs = filebase + '.internal_temp_sc_file'
 		openw,lucs,file_cs,/swap_if_little_endian
 	endif

; 	hk_array = uintarr(120)
; 	for i=0,59 do begin
; 		j = i*2
; 		byte1 = extract_byte(buffer_pointer)
; 		byte2 = extract_byte(buffer_pointer)
; 		byte3 = extract_byte(buffer_pointer)
; 		hk_array[j] = byte1*16 + byte2/16
; 		hk_array[j+1] = (byte2 and '0F'xb)*256 + byte3
; 	endfor

; ; The last 7 HK words are spare.  Overwrite them them with the following data
; 	hk_array[113] = count_rate
; 	hk_array[114] = xrt_state
; 	hk_array[115] = xrt_mode
; 	hk_array[116:119] = acs_flags

;	ccd_temp = RTD(hk_array[32], rtd_channel[32])

;	if (idebug ge 10) then begin
;		print,''
;		print,'Housekeeping data:'
;		print,format='("COMANDSTATS: ",10(z6,1x))',hk_array
;	endif

;	if (idebug ge 15) then print,'COMANDSTATS: HK = ', hk_array
;	num_hk_records = num_hk_records + 1

;	; write output data to file as unformatted binary data
;	writeu,luhk,satellite_time,hk_array

                                ; reset header_id to force new read
    header_id = 0
;    print,scui_low_buf_dep
    
  endrep until (EOF(luin))
  close,luin

  print,''
  print,'*******************************************************************'
  print,'*******************************************************************'
  print,'Done processing ', packet_num, ' CCSDS packets'

  printf,lulog,' '
  printf,lulog,'*******************************************************************'
  printf,lulog,'*******************************************************************'
  printf,lulog,'Done processing ', packet_num, ' CCSDS packets'
  printf,lulog,format='(a,f20.6,a,a)','First CCSDS packet time: ', first_time, $
     ' = ', itos_time(first_time,first_utc)
  printf,lulog,format='(a,f20.6,a,a)',' Last CCSDS packet time: ', last_time, $
     ' = ', itos_time(last_time,last_utc)
  printf,lulog,format='(a,f20.6)',' Delta (seconds) = ', last_time-first_time
  printf,lulog,' '

  if (process_cs ne 0) then begin
     close,luout
     free_lun,luout
     print,' '
     print,'Processing COMANDSTATS data and writing COMANDSTATS PS file...'
     print, ' '
     process_comandstats_data,filein
  endif

  printf,lulog,' '
  printf,lulog,'*******************************************************************'
  printf,lulog,'*******************************************************************'
  printf,lulog,'Done processing ', packet_num, ' CCSDS packets'
  printf,lulog,format='(a,f20.6,a,a)','First CCSDS packet time: ', first_time, $
     ' = ', itos_time(first_time,first_utc)
  printf,lulog,format='(a,f20.6,a,a)',' Last CCSDS packet time: ', last_time, $
     ' = ', itos_time(last_time,last_utc)
  printf,lulog,format='(a,f20.6)',' Delta (seconds) = ', last_time-first_time


  close,/all
;stop
end
