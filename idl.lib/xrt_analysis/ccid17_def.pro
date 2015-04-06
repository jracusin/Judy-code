; $Id: eev_cubic_def.pro 4.2 1994/10/13 02:16:11 burrows Exp burrows $
pro ccid17_def
; 
; This procedure sets the default processing parameters for an EEV ROCKET CCD
;
; Modified: 
;	08/15/94 by Dave Burrows: changed defaults to use CUBIC flight
;		defaults of 832 x 512 images.
;	08/21/94 by DNB: added camera_name, amp_location.
;	03/17/95 by jam to make 1024 X 832 for the rocket calibration and flight data
;	11/10/95 by jam to add ccid17 defaults settings
@common

temp=''
ccd_num = 0
ccd_type = ''
ccd_type = 'ccid17'
type:
read,'Enter CCD ID (i.e., CCID17A): ', ccd_name
ccd_name = strupcase(ccd_name)
case ccd_name of
	'CCID17A': begin
			ccd_name = 'CCID17A = ccid17-118-1'
			ccd_num = 0
		  end
	'CCID17B': begin
			ccd_name = 'Not Available'
			ccd_num = 1
		  end
	else: begin
		print,'Invalid response.  Try again ...'
		goto,type
	      end
endcase
read,'Enter the name of your CCD camera: ', camera_name
camera_name = strupcase(camera_name)
read_amp_num:
read,'Enter amplifier number (1-4): ', amp_num
case 1 of
    (0 le ccd_num) and (ccd_num le 5): $
	case amp_num of
		1: amp_location = 'L'
		2: amp_location = 'R'
		3: amp_location = 'L'
		4: amp_location = 'R'
		else: begin print,'Invalid amplifier number' $
			& goto,read_amp_num & end
   	 endcase
    (6 le ccd_num) and (ccd_num le 9): $
	case amp_num of
		1: amp_location = 'R'
		2: amp_location = 'L'
		3: amp_location = 'L'
		4: amp_location = 'R'
		else: begin print,'Invalid amplifier number' $
			& goto,read_amp_num & end
   	 endcase
endcase
frame_mode = 'T'
read,'Enter the exposure time (not including readout time) in seconds: ',$
	exp_time
read,'Enter the readout rate in kilopixels per second: ', readout_rate
serial_forward = ''
while (serial_forward ne 'T' and serial_forward ne 'F') do begin
	read,'Are the serial clocks running forward? ', temp
	serial_forward = strupcase(strmid(temp,0,1))
	if (idebug ge 2) then print,'  ccid17_def: temp, serial_forward: ', $
		temp, ' ', serial_forward
	if (serial_forward eq 'Y') then serial_forward = 'T'
	if (serial_forward eq 'N') then serial_forward = 'F'
	if (idebug ge 2) then print,'  ccid17_def: temp, serial_forward: ', $
		temp, ' ', serial_forward
endwhile
parallel_forward = ''
while (parallel_forward ne 'T' and parallel_forward ne 'F') do begin
	read,'Are the parallel clocks running forward? ', temp
	parallel_forward = strupcase(strmid(temp,0,1))
	if (parallel_forward eq 'Y') then parallel_forward = 'T'
	if (parallel_forward eq 'N') then parallel_forward = 'F'
endwhile
precharge_high = ''
while (precharge_high ne 'T' and precharge_high ne 'F') do begin
	read,'Is the precharge set high? ', temp
	precharge_high = strupcase(strmid(temp,0,1))
	if (precharge_high eq 'Y') then precharge_high = 'T'
	if (precharge_high eq 'N') then precharge_high = 'F'
endwhile
read,'Enter photon source (i.e., Fe-55, Al-K, Cu-L, ...): ', source
read,'Enter any further descriptive comment: ', comment
n_columns = 550
n_rows = 1026 
guard_pixels=7  ; includes 1 garbage pixel plus 8 true guard pixels plus 8 aluminized columns
overclocks=31	; pixels used for baseline correction
guard_rows=2
overclocked_rows=0
left_border=0
right_border=0
top_border=0
bottom_border=0
nbad_col = -1

end
