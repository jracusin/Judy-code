pro xrt_startup, filename, showplot, debug_level, DEBUG5=debug5, $
                 NOINIT=noinit, PRINTPLOTS=printplots

;name	: xrt_startup
;author	: Judy Racusin
;date	: 05/25/2005
;lang	: IDL

;purpose: This program reads an ITOS XRT Startup file (Ap_ID 0x480)
;		     and creates an output text file.
;
; Rev:
;	05/25/05 by JLR: Created xrt_startup based on memory_status.pro
;
;

@pass1_common

@pass1_path_init                ; initialize paths for input files
  version = 'V1.1'
  quick_look_mode = 0

  common save_data,oldwaveform,n_frames	; saves this for next entry

  if not (keyword_set(noinit)) then begin
@pass1_init                     ; initialize common block variables.
  endif

  GET_LUN, luin           ; Get Logical Unit Numbers for input and output files

  if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select XRT Startup data file: ', $
                       filter='*startup*.0', /must_exist)
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
     filein = pickfile(title='Select XRT Startup data file: ', /must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
     openr,luin,filein,error=file_error
  endrep until (file_error eq 0)
  close,luin

  GET_LUN, lulog
  openw,lulog,filein+'.log'

;  GET_LUN, luerr
;  openw,luerr,filein+'.err'


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
  ccsds_id1 = '0C80'x           ; RT HK version
  ccsds_id2 = '0D30'x           ; TDRSS version

  byte1 = 0B
  byte2 = 0B

; Open input file
  get_lun,luin
  openr,luin,filein,error=file_error,/swap_if_little_endian
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
     read,'Enter name of XRT Startup input file: ', file_in
     openr,luin,file_in,error=file_error,/swap_if_little_endian
  endrep until (file_error eq 0)

; Read the file and process the data

  print,' '
  print,'Reading the XRT Startup file ...'
  printf,lulog, 'XRT Startup message log for file ', filein
  printf,lulog, '    processed with XRT_STARTUP ' + version + ' on ', runtime
  
  packet_num = 0
  repeat begin
     packet_num = packet_num + 1
     
     print,' '
     print,'                                           S/C Time (s)   Boot Conf Index   Boot Count'

     printf,lulog, ' '
     printf,lulog,'                                           S/C Time (s)   Boot Conf Index     Boot Count'
     
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
     if (control ne 'C000'x) then stop, 'XRT_STARTUP: Error in file format: invalid control bits'
     sequence = seq_cntl and '3FFF'x
;	print,'Reading packet # ', packet_num
     printf,lulog,'Reading packet # ', packet_num, '     CCSDS packet #', sequence

     readu,luin,byte1,byte2
     packet_length = byte1*256+byte2
     if (idebug ge 10) then begin
        print,'XRT_STARTUP: ---------------------------------'
        print,'XRT_STARTUP: reading CCSDS packet #', sequence
        print,FORMAT='(A,3Z10,I10)',$
           'XRT_STARTUP: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
           seq_cntl, control, sequence
        print,'XRT_STARTUP: packet length = ', packet_length
        printf,lulog,'XRT_STARTUP: ---------------------------------'
        printf,lulog,'XRT_STARTUP: reading CCSDS packet #', sequence
        printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
           'XRT_STARTUP: CCSDS ID, control, Seq = ', header_id,$
           control, sequence
        printf,lulog,'XRT_STARTUP: packet length = ', packet_length
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
        print,'XRT_STARTUP: CCSDS time =  ', ccsds_time
        printf,lulog,'XRT_STARTUP: CCSDS time =  ', ccsds_time
     endif
     ccsds_time_old = ccsds_time
     old_sec = ccsds_sec
     old_subsec = ccsds_subsec

                                ; Calculate the checksum
     if (idebug ge 10) then help,checksum
     checksum = fix(total(buffer[6:buffer_size-3]),type=12) ; exclude checksum word
     if (idebug ge 10) then begin
        print, 'XRT_STARTUP: Total of all bytes outside CCSDS headers: ', checksum
        printf,lulog, 'XRT_STARTUP: Total of all bytes outside CCSDS headers: ', checksum
     endif
     check = checksum

                                ; Now get checksum value from file
     chksum = buffer[buffer_size-2]*256U + buffer[buffer_size-1]

     if (idebug ge 10) then begin
        help,checksum
        help,chksum
        print,FORMAT='(A,Z10)','XRT_STARTUP: Checksum in file is ', chksum
        print,FORMAT='(A,Z10)','XRT_STARTUP: Calculated checksum:', checksum
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
           'XRT_STARTUP: WARNING: Satellite time decreased!'
        print_error,0,1, 'XRT_STARTUP:      Last time =  ' $
           + string(last_time)
        print_error,0,1, 'XRT_STARTUP:      Current time=' $
           + string(satellite_time)
     endif

     last_time = satellite_time

     UTC_sec = long(extract_long(buffer_pointer))
     UTC_subsec = extract_int(buffer_pointer) 
     UTC_delta = UTC_sec + UTC_subsec*20.0D-6

     bootconf=extract_long(buffer_pointer) ;4
     bootcount=extract_long(buffer_pointer) ;4
     letaskid=extract_long(buffer_pointer) ;4
     levectornum=extract_long(buffer_pointer) ;4
     lestack=extract_long(buffer_pointer) ;4
     levectoroff=extract_long(buffer_pointer) ;4
     leerrno=extract_long(buffer_pointer) ;4
     ledar=extract_long(buffer_pointer) ;4
     ledsisr=extract_long(buffer_pointer) ;4
     lefpscr=extract_long(buffer_pointer) ;4
     leeim0=extract_long(buffer_pointer) ;4
     leeim1=extract_long(buffer_pointer) ;4
     skipbf=extract_long(buffer_pointer) ;4
     clms=extract_long(buffer_pointer) ;4
     cpubussp=extract_long(buffer_pointer) ;4
     bc0sta=extract_long(buffer_pointer) ;4
     bcoenda=extract_long(buffer_pointer) ;4
     bcocpa=extract_long(buffer_pointer) ;4
     bceea=extract_long(buffer_pointer) ;4
     checksumbc0s=extract_long(buffer_pointer) ;4
     checksumbc0c=extract_long(buffer_pointer) ;4
     bc1sta=extract_long(buffer_pointer) ;4
     bc1enda=extract_long(buffer_pointer) ;4
     bc1cpa=extract_long(buffer_pointer) ;4
     bc1ea=extract_long(buffer_pointer) ;4
     checksumbc1s=extract_long(buffer_pointer) ;4
     checksumbc1c=extract_long(buffer_pointer) ;4
     checksumscas=extract_long(buffer_pointer) ;4
     checksumscac=extract_long(buffer_pointer) ;4
     checksumacas=extract_long(buffer_pointer) ;4
     checksumacac=extract_long(buffer_pointer) ;4
     checksumeefss=extract_long(buffer_pointer) ;4
     checksumeefsc=extract_long(buffer_pointer) ;4
     summarybit=extract_long(buffer_pointer) ;4
     drambit=bytarr(64)
     for i=0,63 do drambit[i]=extract_byte(buffer_pointer) ;64    
     
     gmt = itos_time(satellite_time)


     print,format='(a,a24,f20.6,i10,a,i10)', 'XRT_STARTUP: ', $
        gmt, satellite_time, bootconf, '   ',bootcount
     print
     print,'Checksums:      Stored     Calculated          Delta'
     print,'      BC0    ',checksumbc0s,checksumbc0c,checksumbc0s-checksumbc0c
     print,'      BC1    ',checksumbc1s,checksumbc1c,checksumbc1s-checksumbc1c
     print,'      SCA    ',checksumscas,checksumscac,checksumscas-checksumscac
     print,'      ACA    ',checksumacas,checksumacac,checksumacas-checksumacac
     print,'      EEFS   ',checksumeefss,checksumeefsc,checksumeefss-checksumeefsc
     print,' '
     print,'Summary of BIT results ',summarybit
     print,'DRAM BIT results ',drambit
     print,'_______________________________________________________________________________________________'
     
     printf,lulog,format='(a,a24,f20.6,i10,a,i10)', 'XRT_STARTUP: ', $
        gmt, satellite_time, bootconf, '     ',bootcount
     printf,lulog,''
     printf,lulog,'Checksums:      Stored     Calculated          Delta'
     printf,lulog,'      BC0    ',checksumbc0s,checksumbc0c,checksumbc0s-checksumbc0c
     printf,lulog,'      BC1    ',checksumbc1s,checksumbc1c,checksumbc1s-checksumbc1c
     printf,lulog,'      SCA    ',checksumscas,checksumscac,checksumscas-checksumscac
     printf,lulog,'      ACA    ',checksumacas,checksumacac,checksumacas-checksumacac
     printf,lulog,'      EEFS   ',checksumeefss,checksumeefsc,checksumeefss-checksumeefsc
     printf,lulog,' '
     printf,lulog,'Summary of BIT results ',summarybit
     printf,lulog,'DRAM BIT results ',drambit
     printf,lulog,'_______________________________________________________________________________________________'
                                ; reset header_id to force new read
     header_id = 0

  endrep until (EOF(luin))
  close,luin

  print,''
  print,'*******************************************************************'
  print,'*******************************************************************'
  print,'Done processing ', packet_num, ' CCSDS packets'
  if (numerr eq 0) then print, '        NO STARTUP ERRORS FOUND' $
  else print, '        ***********', numerr, ' STARTUP ERRORS FOUND ***********'
  print,''

  close,/all

end
