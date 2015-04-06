; $Id: set_proc_info.pro 4.2 1994/08/31 20:09:55 burrows Exp $
pro set_proc_info
;
;Name: SET_PROC_INFO
;Author: Dave Burrows
;Date: 07/06/94
;modified: 07/14/94 by DNB to add bad columns
;	08/17/94 by DNB to add new CCD parameters
;	08/21/94 by DNB to add more CCD parameters
;	03/06/03 by DNB to add XRT defaults
;
@common

prefix=''
suffix=''
num=0
low=0

print,''
print,'Select CCD processing parameters:'
print,'    1) CCID-7 defaults'
;print,'    2) CCID-10 defaults (not available)'
;print,'    3) Loral defaults (not available)'
print,'    2) XRT defaults for Raw Mode (635x602)'
print,'    3) XRT defaults for other modes (631x602)'
print,'    4) CUBIC EEV defaults'
print,'    5) ROCKET EEV defaults'
print,'    6) CCID17 defaults'
print,''
print,'    7) Specific chip defaults'
print,'    8) Other'
select=''
read,'Enter selection: ',select
case select of
'1': ccid7_defaults
;'2': ccid10_defaults
;'3': loral_defaults
'2': xrt_raw_def
'3': xrt_pupd_def
'4': eev_cubic_def
'5': eev_rket_def
'6': ccid17_def
'7': read_chip_def
else: chip_defaults
endcase


ok = ''
repeat begin 
	print,''
	print,'Processing parameters selected: '
	print,'CCD type:        ', ccd_type
	print,'CCD S/N or ID:      ', ccd_name
	print,'Camera name:     ', camera_name
	print,'Amplifier:', amp_num
	print,'Amp location:       ', amp_location
	case frame_mode of
		'T': print,'Frame mode:      Frame Transfer'
		'F': print,'Frame mode:      Full-frame'
		else: message,'Error in frame_mode: must be T or F'
	endcase 
	print,'Exposure time:', exp_time, ' seconds'
	print,'Readout rate: ', readout_rate, ' kilopixels per second'
	case serial_forward of
		'T': print,'Serial clocks:   Forward'
		'F': print,'Serial clocks:   Reversed'
		else: message,'Error in serial_forward: must be T or F'
	endcase 
	case parallel_forward of
		'T': print,'Parallel clocks: Forward'
		'F': print,'Parallel clocks: Reversed'
		else: message,'Error in parallel_forward: must be T or F'
	endcase 
	case precharge_high of
		'T': print,'Reset clocks:    Precharge held high'
		'F': print,'Reset clocks:    Normal'
		else: message,'Error in precharge_high: must be T or F'
	endcase 
	print,'Photon source:   ', source
	print,'Comments:        ', comment
	print,''
	print,'Number of columns:            ', n_columns
	print,'Number of rows:               ', n_rows
	print,'Number of guard pixels:       ', guard_pixels
	print,'Number of overclocked pixels: ', overclocks
	print,'Number of guard rows:         ', guard_rows
	print,'Number of overclocked rows:   ', overclocked_rows
	print,'Left border:   ', left_border, ' columns'
	print,'Right border:  ', right_border, ' columns'
	print,'Top border:    ', top_border, ' rows'
	print,'Bottom border: ', bottom_border, ' rows'

	process_xmin = guard_pixels+left_border
	process_xmax = n_columns - overclocks - right_border - 1
	process_ymin = guard_rows + bottom_border
	process_ymax = n_rows - overclocked_rows - top_border - 1
	print,'X range: ', process_xmin+1, ' to ', process_xmax+1
	print,'Y range: ', process_ymin+1, ' to ', process_ymax+1
	print,''
	if (nbad_col gt 0) then begin
		print,'CCD #1 has ', nbad_col, ' bad columns.'
		print,'    In this image, they occur in columns:'
		if (nbad_col le 20) then print,'     ',bad_col(0:nbad_col-1)+1
	endif
	if (nbad_col2 gt 0) then begin
		print,'CCD #2 has ', nbad_col2, ' bad columns.'
		print,'    In this image, they occur in columns:'
		if (nbad_col2 le 20) then print,'     ',bad_col2(0:nbad_col-1)+1
	endif
	if (cubic2 eq 'y') then begin
                print,'This is CUBIC 2-CCD data'
        endif else begin
                print,'This is single CCD data'
        endelse

	print,''
	read,'Is this OK? ', ok
	ok = strupcase(strmid(ok,0,1))
	if (strlowcase(ok) ne 'y') then chip_defaults
endrep until (ok eq 'Y')

print,''
if (nbad_col lt 0) then begin
    read,'How many BLOCKS of bad columns does CCD #1 have? ',nblock
    nbad_col = 0
    bad_col = intarr(n_columns)
    if (nblock gt 0) then begin
	print,''
	print,'This program can use column numbers measured from either side'
	print,'   of the CCD, but it needs to know which you are entering.'
	print,'   If you are entering column numbers determined from THIS'
	print,'   image (in saoimage, for example), type "y" to next question:'
	answer = ''
	while (answer ne 'Y') and (answer ne 'N') do begin
	    read,'Are the bad columns counted FROM THIS AMPLIFIER? (Y/N): ', $
			answer
	    answer = strupcase(strmid(answer,0,1))
	endwhile
	    for i=1,nblock do begin
		read,'Enter first and last bad column (counting from 1): ', $
				col1, col2
		for j=col1,col2 do begin
			case answer of
				'Y': bad_col(nbad_col) = j-1
				'N': bad_col(nbad_col) = n_columns-overclocks-j
				else: message,'Invalid value of answer'
			endcase
			nbad_col = nbad_col + 1
		endfor
	    endfor
	endif
endif
print,''
if cubic2 eq 'y' then begin
if (nbad_col2 lt 0) then begin
    read,'How many BLOCKS of bad columns does CCD #2 have? ',nblock
    nbad_col2 = 0
    bad_col2 = intarr(n_columns)
    if (nblock gt 0) then begin
	print,''
	print,'This program can use column numbers measured from either side'
	print,'   of the CCD, but it needs to know which you are entering.'
	print,'   If you are entering column numbers determined from THIS'
	print,'   image (in saoimage, for example), type "y" to next question:'
	answer = ''
	while (answer ne 'Y') and (answer ne 'N') do begin
	    read,'Are the bad columns counted FROM THIS AMPLIFIER? (Y/N): ', $
			answer
	    answer = strupcase(strmid(answer,0,1))
	endwhile
	    for i=1,nblock do begin
		read,'Enter first and last bad column (counting from 1): ', $
				col1, col2
		for j=col1,col2 do begin
			case answer of
				'Y': bad_col2(nbad_col2) = j-1
				'N': bad_col2(nbad_col2) = n_columns-overclocks-j
				else: message,'Invalid value of answer'
			endcase
			nbad_col2 = nbad_col2 + 1
		endfor
	    endfor
	endif
endif
endif


print,' '
print,'Enter threshold used to exclude X-ray events '
print,'       from meanframe and readnoise calculations'
read,'   Enter number of sigma for threshold: ', thresh_sig

print,''
read,'Enter threshold used to identify X-ray events in sigma: ', event_sig

print,''
read,'Enter split event threshold in sigma: ', split_sig

end
