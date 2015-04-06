pro tam_windows, header_id, buffer_pointer, error_code
;
;name	: tam_windows
;author	: Dave Burrows
;date	: 04/07/02
;lang	: IDL

;purpose: This routine unpacks and processes TAM images in two-window format.
;	The data are stored in on output in standard FITS image files.
;
;
;	It returns an error code which is 0 if no errors are found:
;		error_code	
;		    0	no errors
;
;  Modifications:
;	05/31/04 by A. Moretti: added ground calculation of TAM centroids, 
;		comparison with flight values.
;	05/27/04 by DNB: added support for binary table FITS file
;	04/01/04 by DNB: added error checking for end of file
;	06/14/03 by DNB: fixed numerous logic errors needed to process data
;		file that doesn't begin with TAM header.
;	05/16/03 by DNB: fixed problem with TAM window FITS headers
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

  window_mode = 1

  n_calls = n_calls + 1

  error_code = 0

  pixel = bytarr(4)

  cmprssd = 0                   ; flag for compressed frame
  
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
  if (control ne 'C000'x) then stop, 'TAM_WINDOWS: Error in file format: invalid control bits'
  sequence = seq_cntl and '3FFF'x
  if (sequence ne old_seq+1) then begin
     print,' '
     print_error,0,1,'TAM_WINDOWS: *******************************'
     print_error,0,1,'TAM_WINDOWS: Error in CCSDS packet sequence'
     print_error,0,1,'TAM_WINDOWS: Last packet number: ' $
        + string(old_seq)
     print_error,0,1,'TAM_WINDOWS: This packet number: ' $
        + string(sequence)
     print_error,0,1,'TAM_WINDOWS: cumulative CCSDS packet #' $
        + strtrim(string(num_CCSDS_packets_read),2) $
        + ', cumulative LDP page #' + strtrim(string(page_count),2)
     print_error,0,1,'TAM_WINDOWS: *******************************'
; TBD		stop, 'TAM_WINDOWS: Error in CCSDS packet sequence'
  endif
  readu,luin,byte1,byte2
  packet_length = byte1*256U+byte2
  if (idebug ge 25) then begin
     print,'TAM_WINDOWS: ---------------------------------
     print,'TAM_WINDOWS: reading CCSDS packet #', sequence
     printf,lulog,'TAM_WINDOWS: ---------------------------------
     printf,lulog,'TAM_WINDOWS: reading CCSDS packet #', sequence
  endif
  if (idebug ge 30) then begin
     print,FORMAT='(A,3Z10,I10)',$
        'TAM_WINDOWS: CCSDS ID, seq_cntl, control, Seq = ', header_id,$
        seq_cntl, control, sequence
     print,'TAM_WINDOWS: packet length = ', packet_length
     printf,lulog,FORMAT='(A,4(''  0x'',Z4.4))',$
        'TAM_WINDOWS: CCSDS ID, seq_cntl, control, Seq = ', $
        header_id, seq_cntl, control, sequence
     printf,lulog,'TAM_WINDOWS: packet length = ', packet_length
  endif
  old_seq = sequence
  if (old_seq eq 16383) then old_seq = -1 ; sequence #s are mod 16384

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
;	if (tam_frame eq 1) then first_time = tam_time $
;	else last_time = tam_time

  if (idebug ge 30) then begin
     print,'TAM_WINDOWS: CCSDS seconds    =  ', ccsds_sec
     print,'TAM_WINDOWS: CCSDS subseconds =  ', ccsds_subsec
     print,'TAM_WINDOWS: CCSDS time =  ', ccsds_time
     printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
        'TAM_WINDOWS: CCSDS seconds    =  ', ccsds_sec, ccsds_sec
     printf,lulog,format='(a,''0x'',z8.8,'' = '',i6)',$
        'TAM_WINDOWS: CCSDS subseconds =  ', ccsds_subsec, ccsds_subsec
     printf,lulog,'TAM_WINDOWS: CCSDS time =  ', ccsds_time
  endif
  ccsds_time_old = ccsds_time
  old_sec = ccsds_sec
  old_subsec = ccsds_subsec


                                ; Calculate the checksum
  if (idebug ge 30) then help,checksum
  checksum = fix(total(data_buffer[6:buffer_size-3]),type=12) ; exclude checksum word
  if (idebug ge 30) then begin
     print, 'TAM_WINDOWS: Total of all bytes outside CCSDS headers: ', checksum
     printf,lulog, 'TAM_WINDOWS: Total of all bytes outside CCSDS headers: ', checksum
  endif
  check = checksum
                                ;checksum = byte(check,2)*256UL + byte(check,3) 	; extract lowest 2 bytes
                                ; Now get checksum value from file
  chksum = data_buffer[buffer_size-2]*256U + data_buffer[buffer_size-1]

  if (idebug ge 30) then begin
     help,checksum
     help,chksum
     print,FORMAT='(A,Z10)','TAM_WINDOWS: Checksum in file is ', chksum
     print,FORMAT='(A,Z10)','TAM_WINDOWS: Calculated checksum:', checksum
  endif

                                ; Verify checksum
  if (chksum ne checksum) then begin
     print_error,0,1,'TAM_WINDOWS: Checksum error in file '+file_in
     print_error,0,1,'TAM_WINDOWS:	Checksum error in CCSDS header '+strtrim(string(sequence),2)
     print_error,0,1,'TAM_WINDOWS: Checksum in file: '+string(chksum)
     print_error,0,1,'TAM_WINDOWS: Calculated checksum: '+string(checksum)
     print_error,0,1,'TAM_WINDOWS: Error found in product #'+string(product_num)+' page #'+string(page_num)
     stop, 'TAM_WINDOWS: HALT: Checksum error in first CCSDS header'
  endif


  process_tam_win_hdr, buffer_pointer, error_code

  if (error_code ne 0) then return
;endrep until (error_code eq 0)

  iframe = iframe + 1
  tamtime[iframe] = tam_image_time
  frame_count[iframe] = tam_frame
  targetid[iframe] = target_id
  obsseg[iframe] = obs_segment
  tamx1[iframe] = tam_x1
  tamy1[iframe] = tam_y1
  tamx2[iframe] = tam_x2
  tamy2[iframe] = tam_y2
  tamsig1[iframe] = tam_sig1
  tamsig2[iframe] = tam_sig2

  if (N_events lt 1) then return
  if (N_events ne (tam_x1_size*tam_y1_size + tam_x2_size*tam_y2_size)) then begin
     print_error,0,1,'TAM_WINDOWS: Window 1 size (X,Y) = ' $
        + string(tam_x1_size, tam_y1_size, format='(2i5)')
     print_error,0,1,'TAM_WINDOWS: Window 2 size (X,Y) = ' $
        + string(tam_x2_size, tam_y2_size, format='(2i5)')
     print_error,0,1,'TAM_WINDOWS: total window size is ' $
        + strtrim(string(tam_x1_size*tam_y1_size + tam_x2_size*tam_y2_size),2)
     print_error,0,1,'TAM_WINDOWS: N_pixels is ' + strtrim(string(N_pixels),2)

     stop, 'TAM_WINDOWS: image size error'
  endif

  events_counter = 0

; now read the TAM data and put it into the buffer array

  data_buffer_size = n_elements(data_buffer)

  tam_win1 = uintarr(tam_x1_size, tam_y1_size)
  tam_win2 = uintarr(tam_x2_size, tam_y2_size)

  gain_errors = 0
  if (idebug gt 5) then print, 'TAM_WINDOWS: Processing first TAM window'
  for iy = 0,tam_y1_size-1 do begin
     for ix = 0,tam_x1_size-1 do begin
                                ; first check to see whether we are at the end of the data
        if (buffer_pointer gt (data_buffer_size-4)) then begin ;ignore checksum
           append_tam_data_block, buffer_pointer, error_code 
        endif
        if (error_code eq 201) then return

                                ; if there are more than 2 bytes left in the file, unpack the
                                ;	next X-ray event.  

        pixel = extract_int(buffer_pointer)
        tam_win1[ix,iy] = uint(pixel and '03FF'X)
        flags = uint(pixel and 'FC00'X)/256
        window_num = flags/128
        first_in_window = (flags and '40'xb)/64
        first_in_line = (flags and '20'xb)/32
        aps_gain = (flags and '18'xb)/8
        if (window_num ne 0) then print_error,0,1, $
           'TAM_WINDOWS: data are for wrong window = ' + strtrim(string(window_num),2)
        if (first_in_line and (ix ne 0)) then print_error,0,1, $
           'TAM_WINDOWS: found BOL flag in wrong location ' $
           + 'in frame ' + strtrim(string(iframe),2) $
           + ': iy = ' + string(iy) + ',  ix = ' + string(ix)
        if (first_in_window and ((ix ne 0)or(iy ne 0))) then print_error,0,1, $
           'TAM_WINDOWS: found BOW flag in wrong location ' $
           + 'in frame ' + strtrim(string(iframe),2) $
           + ': iy = ' + string(iy) + ',  ix = ' + string(ix)
        if (aps_gain ne gain) then gain_errors = gain_errors + 1
        if (gain_errors eq 1) then print_error,0,1, $
           'TAM_WINDOWS: gain in header (' $
           + strtrim(string(gain),2) + ' <> gain in pixels ' $
           + strtrim(stsring(aps_gain),2) + ' for frame ' $
           + strtrim(string(tam_frame),2)
        image[tam_x1_window+ix,tam_y1_window+iy] = tam_win1[ix,iy] 
     endfor
  endfor

  if (idebug gt 5) then print, 'TAM_WINDOWS: Processing second TAM window'  
  for iy = 0,tam_y2_size-1 do begin
     for ix = 0,tam_x2_size-1 do begin
                                ; first check to see whether we are at the end of the data
        if (buffer_pointer gt (data_buffer_size-4)) then begin ;ignore checksum
           append_tam_data_block, buffer_pointer, error_code 
        endif
        if (error_code eq 201) then return

                                ; if there are more than 2 bytes left in the file, unpack the
                                ;	next X-ray event.  

        pixel = extract_int(buffer_pointer)
        tam_win2[ix,iy] = uint(pixel and '03FF'X)
        flags = uint(pixel and 'FC00'X)/256
        window_num = flags/128
        first_in_window = (flags and '40'xb)/64
        first_in_line = (flags and '20'xb)/32
        aps_gain = (flags and '18'xb)/8
        if (window_num ne 1) then print_error,0,1, $
           'TAM_WINDOWS: data are for wrong window = ' + strtrim(string(window_num),2)
        if (first_in_line and (ix ne 0)) then print_error,0,1, $
           'TAM_WINDOWS: found BOL flag in wrong location ' $
           + 'in frame ' + strtrim(string(iframe),2) $
           + ': iy = ' + string(iy) + ',  ix = ' + string(ix)
        if (first_in_window and ((ix ne 0)or(iy ne 0))) then print_error,0,1, $
           'TAM_WINDOWS: found BOW flag in wrong location ' $
           + 'in frame ' + strtrim(string(iframe),2) $
           + ': iy = ' + string(iy) + ',  ix = ' + string(ix)
        if (aps_gain ne gain) then gain_errors = gain_errors + 1
        if (gain_errors eq 1) then print_error,0,1, $
           'TAM_WINDOWS: gain in header (' $
           + strtrim(string(gain),2) + ' <> gain in pixels ' $
           + strtrim(stsring(aps_gain),2) + ' for frame ' $
           + strtrim(string(iframe),2)
        image[tam_x2_window+ix,tam_y2_window+iy] = tam_win2[ix,iy] 
     endfor
  endfor
  if (gain_errors gt 1) then print_error,0,1,$
     'TAM_WINDOWS: ' + strtrim(string(gain_errors),2) $
     + ' gain errors found in TAM image # ', + strtrim(string(tam_frame),2)
  
  
End_of_Data_Frame:

; Now read in the redundant LDP header

  append_tam_data_block, buffer_pointer



PROCESS_FRAME:


; Plot diagnostics

  if (iframe ge 1) then begin
     if (show_plot gt 0) then begin 
        old_window = !D.window
        if (n_elements(diagnostic_window) eq 0) then begin
           window,/free,title='TAM Processing Diagnostics'
           diagnostic_window = !D.window
        endif else wset, diagnostic_window
        !P.Multi = [0,1,6]
        plot, frame_count[0:iframe], tamx1[0:iframe], $
           yrange=[min(tamx1[0:iframe]),max(tamx1[0:iframe])], $
           title = 'TAM Window 1 Centroid X', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamy1[0:iframe], $
           yrange=[min(tamy1[0:iframe]),max(tamy1[0:iframe])], $
           title = 'TAM Window 1 Centroid Y', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamsig1[0:iframe], $
           title = 'TAM Window 1 Std Dev', xtitle='Frame #', ytitle = 'Pixels'
        plot, frame_count[0:iframe], tamx2[0:iframe], $
           yrange=[min(tamx2[0:iframe]),max(tamx2[0:iframe])], $
           title = 'TAM Window 2 Centroid X', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamy2[0:iframe], $
           yrange=[min(tamy2[0:iframe]),max(tamy2[0:iframe])], $
           title = 'TAM Window 2 Centroid Y', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamsig2[0:iframe], $
           title = 'TAM Window 2 Std Dev', xtitle='Frame #', ytitle = 'Pixels'
     endif 
  endif
  !P.Multi = 0                  ; reset to one plot per page

  if (show_plot gt 0) then if (n_elements(old_window) ne 0) then $
     if (old_window(0) ne -1) then wset, old_window

  print_debug,0,'TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM_TAM'
  print_debug,0,'*******************************************'

; write TAM image FITS files
  if (idebug gt 9) then printf, lulog,'tam_frame, first_frame = ', tam_frame, first_frame

  print,format='(4a,f20.6)','Writing FITS file for TAM Window 1, ', $
     'frame #', strtrim(string(tam_frame)), $
     ' at S/C time ', tam_image_time ;, ': ', N_events, ' pixels'
  printf,lulog,format='(4a,f20.6)','Writing FITS file for TAM Window 1, ', $
     'frame #', strtrim(string(tam_frame)), $
     ' at S/C time ', tam_image_time ;, ': ', N_events, ' pixels'

  file_win1 = filebase + '.win1_frame' + strtrim(string(tam_frame),2) + '.fits'

  tam_win1_hdr = tam_hdr
  sxaddpar, tam_win1_hdr, 'NAXIS1', tam_x1_size
  sxaddpar, tam_win1_hdr, 'NAXIS2', tam_y1_size
  writefits, file_win1, tam_win1, tam_win1_hdr


  print,format='(4a,f20.6)','Writing FITS file for TAM Window 2, ', $
     'frame #', strtrim(string(tam_frame)), $
     ' at S/C time ', tam_image_time   
  printf,lulog,format='(4a,f20.6)','Writing FITS file for TAM Window 2, ', $
     'frame #', strtrim(string(tam_frame)), $
     ' at S/C time ', tam_image_time   

  file_win2 = filebase + '.win2_frame' + strtrim(string(tam_frame),2) + '.fits'

  tam_win2_hdr = tam_hdr
  sxaddpar, tam_win2_hdr, 'NAXIS1', tam_x2_size
  sxaddpar, tam_win2_hdr, 'NAXIS2', tam_y2_size
  writefits, file_win2, tam_win2, tam_win2_hdr

  cntrd,tam_win1,25.,25.,idx1,idy1,3
  cntrd,tam_win2,25.,25.,idx2,idy2,3
  print,idx1,idy1
  print,idx2,idy2

  x1=sxpar(tam_win2_hdr,'X1WIN')
  y1=sxpar(tam_win2_hdr,'y1WIN')
  x2=sxpar(tam_win2_hdr,'X2WIN')
  y2=sxpar(tam_win2_hdr,'y2WIN')

  calcx1[iframe]=idx1+x1 & calcy1[iframe]=idy1+y1
  calcx2[iframe]=idx2+x2 & calcy2[iframe]=idy2+y2

  diffx1 = calcx1[iframe]-tamx1[iframe]
  diffy1 = calcy1[iframe]-tamy1[iframe]
  diffx2 = calcx2[iframe]-tamx2[iframe]
  diffy2 = calcy2[iframe]-tamy2[iframe]

  printf,lulog,format='("Calc. centroid, diff (win1): ",4f10.3)', $
     calcx1[iframe], calcy1[iframe], diffx1, diffy1
  printf,lulog,format='("Calc. centroid, diff (win2): ",4f10.3)', $
     calcx2[iframe], calcy2[iframe], diffx2, diffy2


  write_tam_image


  return

end
