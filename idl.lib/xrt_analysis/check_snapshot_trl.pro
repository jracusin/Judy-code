pro check_snapshot_trl, buffer_pointer, error_code
; $Id: check_snapshot_trl.pro 3.4 1995/12/21 17:25:30 burrows Exp $

;name	: check_snapshot_trl
;author	: Dave Burrows
;date	: 01/24/02
;lang	: IDL

;purpose: This routine checks the XRT Snapshot Trailer report and verifies the
;	data format.  It generates an output file containing the Snapshot
;	Trailer info (ASCII).
;	It returns an error code which is 0 if no errors are found:
;		error_code	
;	    0	no errors
;	   91	CHECK_SNAPSHOT: EOF encountered before end of CCD header
;
; Rev:
;	05/30/04 by DNB: changed output file name to .trl (from .hdr),
;		increased number of printed decimal places to 3
;	01/15/04 by DNB: reset start_pointer to zero when appending data
;	12/17/03 by DNB: fixed error in bounds checking logic
;	12/17/03 by DNB: fixed error in bounds checking (trailer is 5004
;		bytes, not 5000 as programmed).
;	10/30/03 by DNB: moved out-of-range error statements until after
;		limits are printed to simplify output format
;	06/05/03 by DNB: fixed error in calculation of HK std deviations;
;		fixed errors in calculation of TAM centroid std deviations
;		(checked result against Excel spreadsheet to verify it is 
;		now correct).
;	04/24/03 by DNB: fixed errors in calculation of HK std deviations
;	04/23/03 by DNB: fixed errors in decoding snapshot trailer info for
;		bias row and histogram
;	04/01/02 by DNB: added call to append_data_block if buffer doesn't contain
;		entire Snapshot Trailer.
;	09/13/02 by DNB: writing bias row out as FITS file.
;	08/12/02 by DNB: changed to read HK sums and sum**2 as floats
;	04/07/02 by DNB: changed print format for 3 HK values
;	02/21/02 by DNB: modified print formats, added error checking
;	02/08/02 by DNB: added check for presence of Snapshot Trailer
;
; $Log: check_snapshot_trl.pro $
;
;

@pass1_common

;@initialize_ahkindex	; used on CUBIC, not in XRT

utc = 0.0D

print,'Processing Snapshot Trailer ...'
printf,lulog,'Processing Snapshot Trailer at buffer offset ', $
	strtrim(string(buffer_pointer),2), $
	' with buffer size = ', strtrim(string(data_buffer_size),2)
found_snapshot_trailer = 1

; "declare" variable types
error_code = 0
start_pointer = buffer_pointer

file_sci_hdr = filebase + '.trl'

get_lun,lushdr
openw,lushdr,file_sci_hdr
printf,lushdr,'Swift XRT Science Data Snapshot Trailer from file ', filein
printf,lushdr,'    written at ', runtime

; Get more data if snapshot trailer is not contained entirely within the current
;	data block

if ((buffer_pointer+5004) ge data_buffer_size) then begin
	print,''
	if (append_data_block(buffer_pointer) ne 0) then begin
		error_code = 91
		print_error,0,1,'CHECK_SNAPSHOT_TRL: EOF encountered before end of Snapshot Trailer'
		; extend data buffer to prevent addressing errors
		temp = data_buffer
		data_buffer = bytarr(5004+buffer_pointer+10)
		data_buffer[0:data_buffer_size-1] = temp
		data_buffer_size = n_elements(data_buffer)
	endif
	start_pointer = 0L	; reset this since append_data_block discards previously-processed data
endif

snapshot_number = extract_long(buffer_pointer)		; Snapshot counter
print_snapshot_trailer,0,1,'Processing Science Data Snapshot Trailer #' $
	+ strtrim(string(snapshot_number),2)

temp = extract_long(buffer_pointer)
obs_segment = (temp and 'FF000000'xl) / (256L*256L*256L)
target_id = temp and '00FFFFFF'xl
seq_num = target_id*1000L + obs_segment
print_snapshot_trailer,0,1,'Target ID:           ' + string(target_id)
print_snapshot_trailer,0,0,'Observation Segment: ' + string(obs_segment)
print_snapshot_trailer,0,0,'Sequence Number:     ' + string(seq_num)

seconds = extract_long(buffer_pointer)		; start time (s)
subseconds = extract_int(buffer_pointer)        ; start time (subsec)
snapshot_begin = double(seconds) + double(subseconds)*20.0E-6
print_snapshot_trailer,0,1,'Snapshot Start Time:       ' + string(snapshot_begin)

utc_seconds = long(extract_long(buffer_pointer))
utc_subsec = extract_int(buffer_pointer)
utc = double(utc_seconds) + double(utc_subsec)*20.0E-6
print_snapshot_trailer,0,0,'UTC Factor for start time: ' + string(utc)

gmt = itos_time(snapshot_begin,utc)
print_snapshot_trailer,0,0,'GMT: ' + gmt

seconds = extract_long(buffer_pointer)		; end time (s)
subseconds = extract_int(buffer_pointer)        ; end time (subsec)
snapshot_end = double(seconds) + double(subseconds)*20.0E-6
print_snapshot_trailer,0,1,'Snapshot End   Time:       ' + string(snapshot_end)

utc_seconds = long(extract_long(buffer_pointer))
utc_subsec = extract_int(buffer_pointer)
utc = double(utc_seconds) + double(utc_subsec)*20.0E-6
print_snapshot_trailer,0,0,'UTC Factor for end time  : ' + string(utc)

gmt = itos_time(snapshot_end,utc)
print_snapshot_trailer,0,0,'GMT: ' + gmt

ra = extract_float(buffer_pointer)
deghms,ra,rah,ram,ras
dec = extract_float(buffer_pointer)
degdms,dec,dd,dm,ds
roll = extract_float(buffer_pointer)
print_snapshot_trailer,0,1,'Right Ascension: ' + string(ra) $	; Aspect info
	+ ' = ' + strtrim(string(rah),2) $
	+ ':'   + strtrim(string(ram),2) $
	+ ':'   + strtrim(string(ras),2)
print_snapshot_trailer,0,0,'Declination:     ' + string(dec) $	; Aspect info
	+ ' = ' + strtrim(string(dd),2) $
	+ ':'   + strtrim(string(dm),2) $
	+ ':'   + strtrim(string(ds),2)
print_snapshot_trailer,0,0,'Roll Angle:      ' + string(roll)	; Aspect info


set_point = extract_int(buffer_pointer)	; CCD Temperature Set Point
if (set_point eq 'edbb'x) then $
	print_snapshot_trailer,0,1,'WARNING: File contains fake CCD temp. set pt.' $
else print_snapshot_trailer,0,1,'CCD Temperature Set Pt:        '+string(fix(set_point))

temp = extract_byte(buffer_pointer)		; spare byte
temp = extract_byte(buffer_pointer)		; spare byte

print,''
print,'*** See .hdr file for other system parameters, including HK summaries ***'
print,''

;print_snapshot_trailer,8,1,'       CCD1 DAC settings:                 CCD2 DAC settings'
;dac1=bytarr(32)
;for i=0,31 do dac1(i) = extract_byte(buffer_pointer)
;dac2=bytarr(32)
;for i=0,31 do dac2(i) = extract_byte(buffer_pointer)
;for i=0,3 do printf,lushdr, $
;	format="(8i4,5x,8i4)",dac1((i*8):(i*8+7)),dac2((i*8):(i*8+7))
;for i=0,3 do printf,lulog, $
;	format="(8i4,5x,8i4)",dac1((i*8):(i*8+7)),dac2((i*8):(i*8+7))
;if (idebug ge 8) then begin
;	for i=0,3 do print, $
;		format="(8i4,5x,8i4)",dac1((i*8):(i*8+7)),dac2((i*8):(i*8+7))
;endif
;

printf,lushdr,' '
printf,lushdr,'                  Analog HK Maximum Values '
if (idebug ge 8) then begin
	print,' '
	print,'                  Analog HK Maximum Values '
endif
ahk_max=uintarr(128)
for i=0,127 do ahk_max(i) = extract_int(buffer_pointer)	; read data
for i=0,7 do printf,lushdr, $				; print data
	format="(16i6)",ahk_max((i*16):(i*16+15))
if (idebug ge 8) then for i=0,7 do print, $	; print data
	format="(16i5)",ahk_max((i*16):(i*16+15))

printf,lushdr,' '
printf,lushdr,'                  Analog HK Minimum Values '
if (idebug ge 8) then begin
	print,' '
	print,'                  Analog HK Minimum Values '
endif
ahk_min=uintarr(128)
for i=0,127 do ahk_min(i) = extract_int(buffer_pointer)	; read data
for i=0,7 do printf,lushdr, $				; print data
	format="(16i6)",ahk_min((i*16):(i*16+15))
if (idebug ge 8) then for i=0,7 do print, $	; print data
	format="(16i5)",ahk_min((i*16):(i*16+15))

printf,lushdr,' '
printf,lushdr,'                  Analog HK Sums '
if (idebug ge 8) then begin
	print,' '
	print,'                  Analog HK Sums '
endif
ahk=fltarr(128)
for i=0,127 do ahk(i) = extract_float(buffer_pointer)	; read data
for i=0,15 do printf,lushdr, $				; print data
	format="(8f12.0)",ahk((i*8):(i*8+7))
if (idebug ge 8) then for i=0,15 do print, $	; print data
	format="(8f12.0)",ahk((i*8):(i*8+7))

printf,lushdr,' '
printf,lushdr,'                  Analog HK Sum**2 '
if (idebug ge 8) then begin
	print,' '
	print,'                  Analog HK Sum**2 '
endif
ahk2=fltarr(128)
for i=0,127 do ahk2(i) = extract_float(buffer_pointer)	; read data
for i=0,15 do printf,lushdr, $				; print data
	format="(8f14.0)",ahk2((i*8):(i*8+7))
if (idebug ge 8) then for i=0,15 do print, $	; print data
	format="(8f14.0)",ahk2((i*8):(i*8+7))

ahknum = extract_long(buffer_pointer)


;  Calculate standard deviations
stdev = fltarr(128)
stdev = float(sqrt((ahk2 - (double(ahk)*double(ahk))/float(ahknum))/float(ahknum-1)))


;  Calculate averages
;for i=0,127 do ahk(i) = fix(round(ahk(i)/ahknum))	; calculate averages
ahk = ahk/ahknum	; calculate averages

print_snapshot_trailer,0,1,'Analog HK Mean Values for' + string(ahknum) + ' samples'
for i=0,7 do printf,lushdr, $				; print averages
	format="(16i6)",fix(round(ahk((i*16):(i*16+15))))
printf,lulog,'                (see file ', file_sci_hdr, ' for raw HK summaries)'
if (idebug ge 3) then for i=0,7 do print, $	; print averages
	format="(16i5)",ahk((i*16):(i*16+15))

; Print standard deviations
print_snapshot_trailer,0,1,'Analog HK Standard Deviations for' + string(ahknum) + ' samples'
for i=0,7 do begin
   ahktmp=ahk[(i*16):(i*16+15)] 
   if n_elements(ahktmp) gt 1 then stmp=stdev(ahktmp) else stmp=0.
   printf,lushdr,format="(16f6.1)",stmp
endfor 

; Now print HK means in physical units

print_snapshot_trailer,8,1,'Analog HK in Physical Units:         Name     Min    Mean+/-Std Dev   Max'
for i=0,127 do begin
	dbglim = 8
	if (rtd_channel[i] gt 0) then begin
			min_hk = RTD(ahk_min(i),rtd_channel[i])
			mean_hk = RTD(ahk(i),rtd_channel[i])
			high = RTD((mean_hk+stdev(i)),rtd_channel[i])
			low = RTD((mean_hk-stdev(i)),rtd_channel[i])
			sigma = abs((high-low)/2.0)
			max_hk = RTD(ahk_max(i),rtd_channel[i])
		   endif $
	else begin
			min_hk = ahk_min(i)*hk_slope(i) + hk_intercept(i)
			mean_hk = ahk(i)*hk_slope(i) + hk_intercept(i)
			sigma = abs(stdev(i)*hk_slope(i))
			max_hk = ahk_max(i)*hk_slope(i) + hk_intercept(i)
        endelse

	hk_string = string(format='(a8,a6,3x,a24,2f9.3,''+/-'',f6.3,f9.3,1x,a)', $
		strtrim('HK'+strtrim(string(hk_chan(i)),2)), +strtrim(mux_chan(i)), $
		hk_name(i), min_hk, mean_hk, sigma, max_hk, hk_units(i))
	if (min_hk lt hk_low(i)) then begin
		hk_string = hk_string + '   *** Min is out-of-range ***   '
		dbglim = 0
	endif
	if (max_hk gt hk_high(i)) then begin
		hk_string = hk_string + '   *** Max is out-of-range ***   '
		dbglim = 0
	endif
	print_snapshot_trailer,dbglim,0,hk_string
endfor

for i=0,127 do begin
	if (rtd_channel[i] gt 0) then begin
			min_hk = RTD(ahk_min(i),rtd_channel[i])
			mean_hk = RTD(ahk(i),rtd_channel[i])
			high = RTD((mean_hk+stdev(i)),rtd_channel[i])
			low = RTD((mean_hk-stdev(i)),rtd_channel[i])
			sigma = abs((high-low)/2.0)
			max_hk = RTD(ahk_max(i),rtd_channel[i])
		   endif $
	else begin
			min_hk = ahk_min(i)*hk_slope(i) + hk_intercept(i)
			mean_hk = ahk(i)*hk_slope(i) + hk_intercept(i)
			sigma = abs(stdev(i)*hk_slope(i))
			max_hk = ahk_max(i)*hk_slope(i) + hk_intercept(i)
        endelse

	if (min_hk lt hk_low(i)) then begin
		print_error, 0, 1, 'CHECK_SNAPSHOT_TRL: *** HK channel ' $
			+ strtrim(string(i),2) + ' = ' + hk_name[i]$
			+ ':  Min (' + strtrim(string(min_hk),2) $
			+ ') is below lower limit of '$
			+ strtrim(string(hk_low(i)),2) + ' ***   '
	endif
	if (max_hk gt hk_high(i)) then begin
		print_error, 0, 1, 'CHECK_SNAPSHOT_TRL: *** HK channel ' $
			+ strtrim(string(i),2) + ' = ' + hk_name[i]$
			+ ':  Max (' + strtrim(string(max_hk),2) $
			+ ') is above upper limit of '$
			+ strtrim(string(hk_high(i)),2) + ' ***   '
	endif
endfor


; print the critical ones to the screen
print,''
i=32		; CCD temperature
physical_hk = RTD(ahk(i), rtd_channel[i])
hk_string = string(format='(a8,a6,3x,a24,f8.2,1x,a)', $
		strtrim('HK'+strtrim(string(hk_chan(i)),2)), +strtrim(mux_chan(i)), $
		hk_name(i), physical_hk, hk_units(i))
print,hk_string
ccd_temp = physical_hk		; set ccd_temp in common block

i=67		; Cold Finger Temp
physical_hk = RTD(ahk(i), rtd_channel[i])
hk_string = string(format='(a8,a6,3x,a24,f8.2,1x,a)', $
		strtrim('HK'+strtrim(string(hk_chan(i)),2)), +strtrim(mux_chan(i)), $
		hk_name(i), physical_hk, hk_units(i))
print,hk_string

i=122		; Pressure
physical_hk = ahk(i)*hk_slope(i) + hk_intercept(i)
hk_string = string(format='(a8,a6,3x,a24,f8.2,1x,a)', $
		strtrim('HK'+strtrim(string(hk_chan(i)),2)), +strtrim(mux_chan(i)), $
		hk_name(i), physical_hk, hk_units(i))
print,hk_string
	

;print_snapshot_trailer,8,1,'                  CCD Baseline '
;temp1=uintarr(602)
;for i=0,601 do temp1(i) = extract_int(buffer_pointer)	
;for i=0,39 do printf,lushdr, $				
;	format="(16i5)",temp1((i*16):(i*16+15))
;if (idebug ge 8) then begin
;	for i=0,7 do print, $		; print data
;		format="(16i5)",temp1((i*16):(i*16+15))
;endif
;if ((basic_test_mode eq 1) or (quick_look_mode eq 1) or (print_plots eq 0) $
;	and (show_plot lt 2)) then !P.MULTI = [0,2,3]	; put plots on one page
;case show_plot of
;	1: print,'Plotting CCD Baseline ...'
;	2: print,'Calling interactive plotting widget to plot CCD Baseline ...'
;	else: temp=0
;endcase
;p1plotter,show_plot,temp1,title='CCD Baseline for file '+filebase,$
;	xtitle='Row #',ytitle='DN',/histogram, min=0.5, binsize=1

!P.MULTI = [0,1,2]
for ijk=0,1 do begin
	for i=0,299 do bias_row[i,ijk] = extract_int(buffer_pointer)
	br_uld[ijk] = extract_int(buffer_pointer)
	br_rml[ijk] = extract_int(buffer_pointer)
	br_col_offset[ijk] = extract_int(buffer_pointer)
	br_length[ijk] = extract_int(buffer_pointer)
	br_amp[ijk] = extract_int(buffer_pointer)
endfor

current_br_bias_row = extract_int(buffer_pointer)
current_wt_bias_row = extract_int(buffer_pointer)
wt_event_limit = extract_int(buffer_pointer)
wt_col_offset = extract_int(buffer_pointer)
wt_ncolumns = extract_int(buffer_pointer)

; write the bias row to disk as FITS file
write_bias_row

; skip spare bytes
buffer_pointer = buffer_pointer + 50


!P.MULTI = 0

; *************** Process histogram *******************
print_snapshot_trailer,8,1,'***** Processing histogram *****'
snapshot_spec=ulonarr(1024)
for i=0,1023 do snapshot_spec(i) = extract_int(buffer_pointer)
if (max(snapshot_spec) gt 0) then $
	write_snapshot_spec,snapshot_spec

if (quick_look_mode and print_plots) then begin
	!P.MULTI(0) = 0
	print,'CHECK_SNAPSHOT_TRL: Sending plots to the printer'
	set_plot,'ps'
	device, /landscape

	p1plotter,show_plot,bias_row,title='CCD Bias Row for file '+filebase,$
		xtitle='Column #',ytitle='DN',/histogram, min=0.5, binsize=1

	p1plotter,show_plot,snapshot_spec,title='Accumulated Raw CCD Histogram (Snapshot Trailer) for file '+filebase,$
		xtitle='DN',ytitle='Counts/bin',/histogram, min=0.5, binsize=1

	XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
		'XRT Spectral Plots from Snapshot Trailer of file ' + filein
	XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime

	device,/close
	if (email_address ne '') then $
		spawn,'/usr/bin/mail ' + email_address + ' < idl.ps'
	spawn,'lpr -r idl.ps'
	set_plot,'X'
endif

!P.MULTI = 0		; reset plots to 1 per page

;CHECK_ERROR_FLAGS, buffer_pointer, error_code

print_snapshot_trailer,8,1,'                         Frame Number   Start Time     '

frame_num = extract_long(buffer_pointer)		; First CCD frame number
frame_start_s = extract_long(buffer_pointer)		; First CCD start time
frame_start_ss = extract_int(buffer_pointer)		; First CCD start subsec
frame_start = double(frame_start_s) + double(frame_start_ss*20.0E-6)
print_snapshot_trailer,8,0,'First CCD Frame info: '+string(frame_num)+string(frame_start)

frame_num = extract_long(buffer_pointer)		; Last CCD frame number
frame_start_s = extract_long(buffer_pointer)		; First CCD start time
frame_start_ss = extract_int(buffer_pointer)		; First CCD start subsec
frame_start = double(frame_start_s) + double(frame_start_ss*20.0E-6)
print_snapshot_trailer,8,2,'Last CCD Frame info:  '+string(frame_num)+string(frame_start)


; Read the TAM data
TAM_X1 = extract_float(buffer_pointer)
TAM_Y1 = extract_float(buffer_pointer)
TAM_X12 = extract_float(buffer_pointer)
TAM_Y12 = extract_float(buffer_pointer)

TAM_X2 = extract_float(buffer_pointer)
TAM_Y2 = extract_float(buffer_pointer)
TAM_X22 = extract_float(buffer_pointer)
TAM_Y22 = extract_float(buffer_pointer)

Num_TAM_smpl = extract_long(buffer_pointer)
print_snapshot_trailer,0,1,strtrim(string(Num_TAM_smpl),2) $
	+ ' TAM samples in this snapshot'
print_snapshot_trailer,0,0,'TAM X1 = ' + strtrim(string(TAM_X1,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM Y1 = ' + strtrim(string(TAM_Y1,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM X2 = ' + strtrim(string(TAM_X2,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM Y2 = ' + strtrim(string(TAM_Y2,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM X1**2 = ' + strtrim(string(TAM_X12,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM Y1**2 = ' + strtrim(string(TAM_Y12,format='(f20.6)'),2)
print_snapshot_trailer,0,0,'TAM X2**2 = ' + strtrim(string(TAM_X22,format='(f20.6)'),2)
print_snapshot_trailer,0,2,'TAM Y2**2 = ' + strtrim(string(TAM_Y22,format='(f20.6)'),2)

case Num_TAM_smpl of
	'deadbeef'xL: $
		print_snapshot_trailer,0,2,'CHECK_SNAPSHOT_TRL: WARNING - found fake TAM data'
	0: print_snapshot_trailer,0,1,'*** No TAM samples in this snapshot ***'
    else: begin
	mean_TAM_X1 = TAM_X1 / Num_TAM_smpl
	if (Num_TAM_smpl gt 1) then $
		sig_TAM_X1 = float(sqrt((TAM_X12 - (double(TAM_X1)*double(TAM_X1))/float(Num_TAM_smpl)) / float(Num_TAM_smpl - 1))) $
	else sig_TAM_X1 = 0.0
	print_snapshot_trailer,0,1,'TAM Window 1 X = ' + string(mean_TAM_X1) + $
		' +/- ' + string(sig_TAM_X1)

	mean_TAM_Y1 = TAM_Y1 / Num_TAM_smpl
	if (Num_TAM_smpl gt 1) then $
		sig_TAM_Y1 = float(sqrt((TAM_Y12 - (double(TAM_Y1)*double(TAM_Y1))/float(Num_TAM_smpl)) / float(Num_TAM_smpl - 1))) $
	else sig_TAM_Y1 = 0.0
	print_snapshot_trailer,0,0,'TAM Window 1 Y = ' + string(mean_TAM_Y1) + $
		' +/- ' + string(sig_TAM_Y1)

	mean_TAM_X2 = TAM_X2 / Num_TAM_smpl
	if (Num_TAM_smpl gt 1) then $
		sig_TAM_X2 = float(sqrt((TAM_X22 - (double(TAM_X2)*double(TAM_X2))/float(Num_TAM_smpl)) / float(Num_TAM_smpl - 1))) $
	else sig_TAM_X2 = 0.0
	print_snapshot_trailer,0,0,'TAM Window 2 X = ' + string(mean_TAM_X2) + $
		' +/- ' + string(sig_TAM_X2)

	mean_TAM_Y2 = TAM_Y2 / Num_TAM_smpl
	if (Num_TAM_smpl gt 1) then $
		sig_TAM_Y2 = float(sqrt((TAM_Y22 - (double(TAM_Y2)*double(TAM_Y2))/float(Num_TAM_smpl)) / float(Num_TAM_smpl - 1))) $
	else sig_TAM_Y2 = 0.0
	print_snapshot_trailer,0,0,'TAM Window 2 Y = ' + string(mean_TAM_Y2) + $
		' +/- ' + string(sig_TAM_Y2)
    endelse
endcase

X_bore = extract_float(buffer_pointer)
Y_bore = extract_float(buffer_pointer)
if ((X_bore eq 128.0) and (Y_bore eq 256.0)) then $
	print_snapshot_trailer,0,1,'CHECK_SNAPSHOT_TRL: WARNING - found fake boresight corrections' $
else begin
	print_snapshot_trailer,0,1,'X Boresight Correction = ' + string(X_bore)
	print_snapshot_trailer,0,0,'Y Boresight Correction = ' + string(Y_bore)
endelse

; Now, read the padding until we find the EOH ID word
padding = 0L

repeat begin
	if (idebug gt 10) then $
		print, format='(3i, 4z6.4)', padding, $
			buffer_pointer, data_buffer_size, $
			data_buffer[buffer_pointer:buffer_pointer+3]
	Science_Data_EOH_ID = extract_long(buffer_pointer)
	buffer_pointer = buffer_pointer - 3
	padding = padding + 1
endrep until (Science_Data_EOH_ID eq 'ED94037F'XUL) or (buffer_pointer eq data_buffer_size - 3)
padding = padding - 1
if (buffer_pointer eq (data_buffer_size - 3)) then begin
	printf,lushdr, 'ERROR: no EOH word found in data buffer'
	print_error, 0,1, 'CHECK_SNAPSHOT_TRL: No EOH word found in data buffer'
endif else buffer_pointer = buffer_pointer+3

end_pointer = buffer_pointer

if (padding eq 16) then $
	print_snapshot_trailer,8,1,'CHECK_SNAPSHOT_TRL: found correct # spare bytes at end of Snapshot Trailer' $
else begin
	printf,lushdr,'ERROR: Found '+string(padding)+$
		' bytes of padding at end of Snapshot Trailer (instead of 16)'
	print_error,8,1,'CHECK_SNAPSHOT_TRL: Found '+string(padding)+$
		' bytes of padding at end of Snapshot Trailer (instead of 16)'
endelse

SD_Header_Size = end_pointer - start_pointer

print_snapshot_trailer,8,1,'Snapshot Trailer size = '+string(SD_Header_size)+' bytes (should be 5000)'
if (SD_Header_size ne 5000) then print_error,8,1,$
	'CHECK_SNAPSHOT_TRL: incorrect Snapshot Trailer size = '$
		+string(SD_Header_size)+' bytes (should be 5000)'

close,lushdr
free_lun,lushdr


; check Science Data Header end marker
if (Science_Data_EOH_ID eq 'ED94037F'XL) then begin
	print,''
	print,'CHECK_SNAPSHOT_TRL: Found valid Snapshot Trailer EOF'
	printf,lulog,''
	printf,lulog,'CHECK_SNAPSHOT_TRL: Found valid Snapshot Trailer EOF'
	return
endif

print_error,0,1,'CHECK_SNAPSHOT_TRL: Invalid Science Data Header End ID: ' $
	+ strtrim(string(Science_Data_EOH_ID,format='(z8.8)'),2)
close,/all
stop,'*** CHECK_SNAPSHOT_TRL: Terminating with fatal error ***'

end
