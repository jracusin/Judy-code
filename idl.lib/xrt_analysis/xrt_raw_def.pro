; $Id: eev_cubic_def.pro 4.2 1994/10/13 02:16:11 burrows Exp burrows $
pro xrt_raw_def
;
; This procedure sets the default processing parameters for the XRT flight CCD in raw image mode
;
; Modified: 
;	08/15/94 by Dave Burrows: changed defaults to use CUBIC flight
;		defaults of 832 x 512 images.
;	08/21/94 by DNB: added camera_name, amp_location.
;	03/17/95 by jam to make 1024 X 832 for the rocket calibration and flight data
;	03/06/03 by DNB: copied from rocket version
;   07/09/04 by JLR: updated parameters to current versions
;
@common

temp=''
ccd_num = 0
ccd_type = 'CCD-22FM'


ccd_name = 'ID'
ccd_num = 0

camera_name = 'FPCA+XEP'

read_amp_num:
	read,'Enter amplifier number (1-2): ', amp_num
	case amp_num of
		1: amp_location = 'L'
		2: amp_location = 'R'
		else: begin print,'Invalid amplifier number' $
			& goto,read_amp_num & end
   	 endcase

frame_mode = 'T'

exp_time = 0.0
readout_rate = 150.0

serial_forward = ''
while (serial_forward ne 'T' and serial_forward ne 'F') do begin
	read,'Are the serial clocks running forward? ', temp
	serial_forward = strupcase(strmid(temp,0,1))
	if (idebug ge 2) then print,'  EEV_CUBIC_DEF: temp, serial_forward: ', $
		temp, ' ', serial_forward
	if (serial_forward eq 'Y') then serial_forward = 'T'
	if (serial_forward eq 'N') then serial_forward = 'F'
	if (idebug ge 2) then print,'  XRT_RAW_DEF: temp, serial_forward: ', $
		temp, ' ', serial_forward
endwhile

parallel_forward = ''
while (parallel_forward ne 'T' and parallel_forward ne 'F') do begin
	read,'Are the parallel clocks running forward? ', temp
	parallel_forward = strupcase(strmid(temp,0,1))
	if (parallel_forward eq 'Y') then parallel_forward = 'T'
	if (parallel_forward eq 'N') then parallel_forward = 'F'
endwhile

precharge_high = 'F'

read,'Enter photon source (i.e., Fe-55, Al-K, Cu-L, ...): ', source
read,'Enter any further descriptive comment: ', comment
n_columns = 635
n_rows = 602 
guard_pixels=5  ; includes 1 garbage pixel plus 8 true guard pixels plus 8 aluminized columns
overclocks=20	; pixels used for baseline correction
guard_rows=2
overclocked_rows=2
left_border=10
right_border=10
top_border=10
bottom_border=10
nbad_col = -1

end
