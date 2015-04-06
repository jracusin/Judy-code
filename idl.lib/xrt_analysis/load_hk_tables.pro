pro load_hk_tables
; $Id: load_hk_tables.pro 3.2 1995/12/21 17:25:30 burrows Exp $
;
;name	: load_hk_tables
;author	: Dave Burrows
;date	: 01/27/95
;lang	: IDL

;purpose: This routine reads housekeeping data from a file and loads it
;	into tables used by PASS1 to convert from DN to physical units.
;
;  Rev:
;	05/27/04 by JLR: added link to pass1_path_init to make load_hk_tables work
;			 by itself or inside stripchart
;	02/21/02 by DNB: changed hdr_hk_chan dimension to 28
;	02/10/02 by DNB: reading in 'header_hk_channels.dat' file
;	02/03/02 by DNB: added comment line to top of HK file, changed
;		this routine to read that line and print it in the output log.
;	02/08/02 by DNB: added variable to track RTDs
;
; $Log: load_hk_tables.pro $
;

@pass1_common
@pass1_path_init

hk_chan = intarr(128)
a = 0
mux_chan = strarr(128)
b = ' '
hk_name = strarr(128)
c = ' '
rtd_channel = bytarr(128)
r = 0
hk_min = fltarr(128)
d = 0
hk_max = fltarr(128)
e = 0
hk_units = strarr(128)
f = ' '
hk_slope = fltarr(128)
g = 0.0
hk_intercept = fltarr(128)
h = 0.0
hdr_hk_chan = bytarr(28)

get_lun,lu_hk_tables
openr,lu_hk_tables,hk_table_path
dummy = ''
readf,lu_hk_tables,dummy
if (n_elements(lulog) ne 0) then $
	printf,lulog,format='(a,a)','LOAD_HK_TABLES: reading HK cal file: ', dummy

for i=0,127 do begin
	readf,lu_hk_tables,format='(i4,1x,a8,a24,i2,2f8.2,a12,2f16.1)',a,b,c,r,d,e,f,g,h

	hk_chan[i] = a
	mux_chan[i] = strtrim(b,2)
	hk_name[i] = strtrim(c,2)
	rtd_channel[i] = r
	hk_min[i] = d
	hk_max[i] = e
	hk_units[i] = strtrim(f,2)
	hk_slope[i] = g
	hk_intercept[i] = h
endfor

; Set up the HK channels for CCD frame headers
close,lu_hk_tables
openr,lu_hk_tables,hk_table_path2
readf,lu_hk_tables,dummy
readf,lu_hk_tables,dummy
if (n_elements(lulog) ne 0) then $
	printf,lulog,format='(a,a)','LOAD_HK_TABLES: reading HK channels for CCD headers'

for i=0,27 do begin
	readf,lu_hk_tables,format='(2i3)',a,d
	hdr_hk_chan[i] = d
endfor
close, lu_hk_tables
free_lun, lu_hk_tables


; Now load the low and high limit arrays
hk_low = fltarr(128)
hk_high = fltarr(128)

get_lun,lu_hk_range
openr,lu_hk_range,hk_range_path

dummy=''
a = 0.0
b = 0.0
readf, lu_hk_range, dummy
for i=0,127 do begin
	readf,lu_hk_range,j,a,b
	if ((i) ne j) then stop
	hk_low(i) = a
	hk_high(i) = b
endfor
close, lu_hk_range
free_lun, lu_hk_range

return
end

