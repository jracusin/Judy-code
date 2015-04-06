; $Id: pass1_init.pro 1.0 2002/01/18 17:25:30 burrows $
; $Log: pass1_init.pro $
;

filein = ' '
filebase = ' '
process_hk = 0
error_count = 0L
buffer_pointer = 0L
block_number = 0
num_hk_records = 0
end_of_data = 0		; flag used to signal EOT found
ccd_frame = 0
bias_frame = 0
show_hk_image = 2
show_ccd_image = 1
basic_test_mode = 0
verbosity = 1
color_lut = 0   ; used to be -3
max_xsize=640
max_ysize=602
baseline_correction_mode = 0
ccsds_hdr_id = '0D40'x
xrt_mode = 0
snapshot_number = 0
frame_type = ''

runtime = systime(0)

n_calls = 0

first_time = 0.0D
last_stop_time = 0.0
first_utc = 0.0D
last_utc = 0.0D
xfer_time = 0.0
last_mode = -1

light_curve = fltarr(20000)
read_mode = light_curve
single_events = light_curve
baseline = light_curve
readnoise = light_curve
frame_count = light_curve
iframe = - 1

split_type = uintarr(3,3) ; contains # of single splits in each location
split_charge = fltarr(3,3)

cumulative_hist = intarr(301)
cumulative_dn = indgen(301) - 100.0 + 0.5

spectrum = fltarr(4096)

; set up CCD size
nrow = 602
ncol = 635

accumulated_images = 0
snapshot_image = fltarr(ncol, nrow)

bias_row = uintarr(300,2)
br_uld = intarr(2)
br_rml = intarr(2)
br_col_offset = intarr(2)
br_length = intarr(2)
br_amp = intarr(2)

luwtcatalog = -1
lupccatalog = -1
lupdcatalog = -1
