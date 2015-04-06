pro memory_dump, filename,debug_level
  
  @pass1_common
  @pass1_init
  version = 'V1.1'
  
  if (n_params() lt 2) then idebug = 0 $
  else idebug = debug_level
  
  GET_LUN, luin           ; Get Logical Unit Numbers for input and output files

  if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select XRT Memory Dump data file: ', $
                       filter='*d47.0', /must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
  endif else begin
     filein = filename
  endelse
  filebase = filein
  
  openr,luin,filein,error=file_error
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
     filein = pickfile(title='Select XRT Memory Dump data file: ', /must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
     openr,luin,filein,error=file_error
  endrep until (file_error eq 0)
  close,luin
  
  openw,lulog,filein+'.log',/get_lun
  
  header_id = 0U
  seq_cntl = 0U
  packet_length = 0U
  time_sec = 0L
  time_usec = 0U
  
  ccsds_id1 = '0D47'x           ; RT HK version
  ccsds_id2 = '0547'x           ; TDRSS version
  
  byte1 = 0B
  byte2 = 0B
  
; Open input file
  get_lun,luin
  openr,luin,filein,error=file_error,/swap_if_little_endian
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
     read,'Enter name of XRT Memory Dump input file: ', file_in
     openr,luin,file_in,error=file_error,/swap_if_little_endian
  endrep until (file_error eq 0)
  
  ; Read the file and process the data

  print,' '
  print,'Reading the XRT Memory Dump file ...'
  printf,lulog, 'XRT Memory Dump message log for file ', filein
  printf,lulog, '    processed with memory_dump ' + version + ' on ', runtime
  
  packet_num = 0
  repeat begin
     packet_num = packet_num + 1
     
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
     if (control ne 'C000'x) then stop, 'MEMORY_DUMP: Error in file format: invalid control bits'
     sequence = seq_cntl and '3FFF'x
;	print,'Reading packet # ', packet_num
     printf,lulog,'Reading packet # ', packet_num, '     CCSDS packet #', sequence

     readu,luin,byte1,byte2
     packet_length = byte1*256+byte2
     if (idebug ge 10) then begin
        print,'MEMORY_DUMP: ---------------------------------'
        print,'MEMORY_DUMP: reading CCSDS packet #', sequence
        print,FORMAT='(A,3Z10,I10)',$
           'MEMORY_DUMP: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
           seq_cntl, control, sequence
        print,'MEMORY_DUMP: packet length = ', packet_length
        printf,lulog,'MEMORY_DUMP: ---------------------------------'
        printf,lulog,'MEMORY_DUMP: reading CCSDS packet #', sequence
        printf,lulog,FORMAT='(A,Z10,Z10,I10)',$
           'MEMORY_DUMP: CCSDS ID, control, Seq = ', header_id,$
           control, sequence
        printf,lulog,'MEMORY_DUMP: packet length = ', packet_length
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
     data_buffer = buffer
     buffer_pointer = 6
     
     startadd=extract_long(buffer_pointer) ;4
     bytestrans=extract_int(buffer_pointer) ;2
     bytesreq=extract_int(buffer_pointer) ;2
     datawidth=extract_byte(buffer_pointer) ;1
     typeid=extract_byte(buffer_pointer) ;1
     blocknum=extract_byte(buffer_pointer) ;1
     datadump=bytarr(bytestrans)
     datadump=extract_byte(buffer_pointer,bytestrans) ;1
     checksum=extract_int(buffer_pointer) ;2
;     print,packet_num,bytestrans;datadump
     header_id = 0
     
  endrep until (EOF(luin))
  close,luin
  free_lun,luin
  
  print,''
  print,'*******************************************************************'
  print,'*******************************************************************'
  print,'Done processing ', packet_num, ' CCSDS packets'
  if (numerr eq 0) then print, '        NO STARTUP ERRORS FOUND' $
  else print, '        ***********', numerr, ' STARTUP ERRORS FOUND ***********'
  print,''

  close,/all

end
     
     
