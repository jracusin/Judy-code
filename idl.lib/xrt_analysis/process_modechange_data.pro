pro process_modechange_data
; $Id: process_modechange_data.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: process_modechange_data
;author	: Dave Burrows
;date	: 03/02/2004
;lang	: IDL
;
;purpose: This routine processes XRT Mode Change data and generates
;	an output FITS image file.
;
; Rev: 
;
;

@pass1_common

error_code = 0

if (n_elements(idebug) eq 0) then begin	;  initialize variables if necessary
	@pass1_path_init
	idebug = 0
	print_plots = 1
	show_hk_image = 2
	num_hk_records = 10000
	version = ''
	basic_test_mode = 0
	quick_look_mode = 0
	print_plots = 1
	runtime = systime(0)
endif

if (idebug ge 3) then begin
	print,'Processing Mode Change data'
endif

get_lun,lumc
if (n_elements(file_hk) eq 0) then begin	; undefined file name
	file_hk = PICKFILE(/READ, FILTER='*.internal_temp_mc_file', /NOCONFIRM)
	filebase = strmid(file_hk, 0, strlen(file_hk)-21)
	filein = filebase + '.modechange.fits'
endif
if (file_hk eq '') then begin	; empty file name
	file_hk = PICKFILE(/READ, FILTER='*.internal_temp_mc_file', /NOCONFIRM)
	filebase = strmid(file_hk, 0, strlen(file_hk)-21)
	filein = filebase + '.modechange.fits'
endif
openr,lumc,file_hk,/delete,/swap_if_little_endian

satellite_time = 0.0D0
hk_array = fltarr(12)
data = dblarr(13,num_hk_records)

; read data from file as unformatted binary data
for i=0,num_hk_records-1 do begin
	if (eof(lumc)) then begin
		data = data[*,0:i-1]	; truncate data array
		goto, Process_Data
	endif
	readu,lumc,satellite_time,hk_array
	if (i eq 0) then begin
		time0 = satellite_time
	endif
	data[0,i] = satellite_time
	data[1:12,i] = hk_array[0:11]
endfor

Process_Data:


mkhdr,header,data
file_date = bin_date(systime(0))
date = string(file_date(0),format="(i4.4)") $
	+ string(file_date(1),file_date(2),format="('-',i2.2,'-',i2.2)")
sxaddpar,header,'DATE',date,'File creation date'
sxaddpar,header,'ORIGIN','PSU X-ray Astronomy', $
		'Data from Penn State X-ray Astronomy'
sxaddpar,header,'INSTRUME','XRT','Swift X-ray Telescope'
sxaddpar,header,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
sxaddhist,' ',header
sxaddhist,'XRT Mode Change Data File created by MODECHANGE '+version, header
sxaddhist,'Column 1 is the satellite time, columns 2-13 are ',header
sxaddhist,'     the Mode Change message data.',header	
sxaddhist,'     Column 2 is the RA', header
sxaddhist,'     Column 3 is the Declination', header
sxaddhist,'     Column 4 is the Roll angle', header
sxaddhist,'     Column 5: XRT Count Rate (cps)', header
sxaddhist,'     Column 6: XRT State Flag      ', header
sxaddhist,'     Column 7: XRT Mode Flag       ', header
sxaddhist,'     Column 8: XRT CCD Waveform    ', header
sxaddhist,'     Column 9: ACS flag word       ', header
sxaddhist,'     Column 10: ACS Is_Settled Flag ', header
sxaddhist,'     Column 11: ACS In_10_Arcmin Flag', header
sxaddhist,'     Column 12: ACS In_SAA Flag     ', header
sxaddhist,'     Column 13: ACS Safe_Mode Flag  ', header
sxaddhist,' ', header
sxaddpar,header,'TIME0',time0,'Initial Satellite Time'
sxaddhist,' ', header

hk_fits_file = filebase + '.fits'
writefits,hk_fits_file,data,header


; Now write these out as FITS tables

print,' '
print,'Writing out binary table file for Mode Change data'
print,' '


; Write out the FITS file primary header
hk_fits_table_file = filebase + '.table.fits'
mwrfits,undefined,hk_fits_table_file,header[5:*],/create

; Make table extension header
fxbhmake,table_hdr,num_hk_records,'XRT_MC','Swift XRT Mode Change data file',/date
sxaddpar,table_hdr,'DATE',date,'File creation date'
sxaddpar,table_hdr,'ORIGIN','PSU X-ray Astronomy', $
		'Data from Penn State X-ray Astronomy'
sxaddpar,table_hdr,'INSTRUME','XRT','Swift X-ray Telescope'
sxaddpar,table_hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
sxaddhist,' ',table_hdr
sxaddhist,'XRT Mode Change Data File created by MODECHANGE '+version, table_hdr
sxaddhist,'Column 1 is the satellite time, columns 2-13 are ',table_hdr
sxaddhist,'     the Mode Change message data.',table_hdr	
sxaddhist,'     Column 2 is the RA', table_hdr
sxaddhist,'     Column 3 is the Declination', table_hdr
sxaddhist,'     Column 4 is the Roll angle', table_hdr
sxaddhist,'     Column 5: XRT Count Rate (cps)', table_hdr
sxaddhist,'     Column 6: XRT State Flag      ', table_hdr
sxaddhist,'     Column 7: XRT Mode Flag       ', table_hdr
sxaddhist,'     Column 8: XRT CCD Waveform    ', table_hdr
sxaddhist,'     Column 9: ACS flag word       ', table_hdr
sxaddhist,'     Column 10: ACS Is_Settled Flag ', table_hdr
sxaddhist,'     Column 11: ACS In_10_Arcmin Flag', table_hdr
sxaddhist,'     Column 12: ACS In_SAA Flag     ', table_hdr
sxaddhist,'     Column 13: ACS Safe_Mode Flag  ', table_hdr
sxaddhist,' ', table_hdr
sxaddpar, table_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'


fxbaddcol,1, table_hdr, data[0,0], 'S/C Time', TUNIT='seconds'
fxbaddcol,2, table_hdr, data[1,0], 'RA', TUNIT='degrees'
fxbaddcol,3, table_hdr, data[2,0], 'Dec', TUNIT='degrees'
fxbaddcol,4, table_hdr, data[3,0], 'Roll', TUNIT='degrees'
fxbaddcol,5, table_hdr, data[4,0], 'Count Rate', TUNIT='cps'
fxbaddcol,6, table_hdr, data[5,0], 'XRT State Flag'
fxbaddcol,7, table_hdr, data[6,0], 'XRT Mode Flag' 
fxbaddcol,8, table_hdr, data[7,0], 'XRT Waveform'
fxbaddcol,9, table_hdr, data[8,0], 'ACS Flag Word'
fxbaddcol,10, table_hdr, data[9,0], 'ACS IS_Settled'
fxbaddcol,11,table_hdr, data[10,0], 'ACS IN_10_ARCMIN'
fxbaddcol,12,table_hdr,data[11,0], 'ACS IN_SAA'
fxbaddcol,13,table_hdr,data[12,0], 'ACS SAFE_HOLD'

fxbcreate, luout, hk_fits_table_file, table_hdr, extension

for row=1,num_hk_records do begin
	fxbwrite,luout,data[0,row-1],1,row
	for col=2,12 do begin
		fxbwrite,luout,data[col-1,row-1],col,row
	endfor
endfor
fxbfinish,luout

;************************************************************************

print,''
   
; display image using Pat Broos' routines
if (num_hk_records gt 1) then begin
	if (basic_test_mode eq 1) then begin
		basic_func_test_plot
	endif else begin
;	    if (quick_look_mode eq 1) then begin
;			quick_look_plot
;	    endif else begin
		xmin = 0
		xmax = n_elements(data[0,*])
;		if (print_plots) then 
		modechange_hk_plots,xmin,xmax
			; send plots to printer
;	    endelse
	endelse
endif else begin
	print,'Only 1 HK record, so I can''t display it as an image'
endelse

file_hk = ''
return
end
