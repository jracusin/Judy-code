pro process_stripchart_data
; $Id: process_stripchart_data.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: process_stripchart_data
;author	: Dave Burrows
;date	: 01/16/95
;lang	: IDL
;
;purpose: This routine processes XRT housekeeping data from the TDRSS or
;	RT HK packets.  Data
;	stored in the temporary disk file are read back in and
;	stored in a FITS file.  The data array is displayed as a 2-D
;	image for interactive examination and data extraction.
;
; Modified:
;	10/30/03 by DNB: massive changes to generate binary FITS files
;	10/29/03 by DNB: changed output file to double precision
;	04/07/02 by DNB: Dropped last 2 time words.  Changed time in 
;		physical unit array to actual satellite time.
;	04/02/02 by DNB: corrected error in satellite times (was reading as
;		long instead of floating point).
;	02/21/02 by DNB: added two new HK values for baseline voltages - 
;		increased array sizes by one (other was spare)
;	02/10/02 by DNB: modified to support the XRT housekeeping.  XRT
;		does not keep all HK values in frame headers, so this
;		routine only processes a subset of the HK data.  Changed width
;		of HK file to 37 words.
;	01/29/95 by DNB: in addition to storing the raw HK data, this
;		routine now converts it to physical units and stores
;		these in a separate FITS file.
;	02/16/95 by DNB: added support for new stripchart mode with
;		extra data at end of record containing BL words and time.
;	03/30/95 by DNB: added no_plot to suppress print output.
;	09/03/96 by DNB: correct some errors in the BL2 bit documentation,
;		added stand-alone support.
;
; $Log: process_hk_data.pro $
;
;

@pass1_common

error_code = 0

if (n_elements(idebug) eq 0) then begin	;  initialize variables if necessary
	@pass1_path_init
	load_hk_tables
	idebug = 0
	print_plots = 1
	no_plot=0
	show_hk_image = 2
	num_hk_records = 10000
	version = ''
	basic_test_mode = 0
;	quick_look_mode = 0
	runtime = systime(0)
endif
get_lun, lu_sc_tables
openr, lu_sc_tables, hk_table_path3
dummy = ''
readf,lu_sc_tables,dummy
readf,lu_sc_tables,dummy
stripchart_hk_chan = intarr(113)
for i=0,112 do begin
	readf,lu_sc_tables,format='(2i4)',a,d
	stripchart_hk_chan[i] = d
endfor
free_lun, lu_sc_tables


if (idebug ge 3) then begin
	print,'Processing housekeeping data'
endif

get_lun,luhk
if (n_elements(file_hk) eq 0) then begin	; undefined file name
	file_hk = PICKFILE(/READ, FILTER='*.internal_temp_sc_file', /NOCONFIRM)
	filebase = strmid(file_hk, 0, strlen(file_hk)-21)
	filein = filebase + '.stripchart.fits'
endif
if (file_hk eq '') then begin	; empty file name
	file_hk = PICKFILE(/READ, FILTER='*.internal_temp_sc_file', /NOCONFIRM)
	filebase = strmid(file_hk, 0, strlen(file_hk)-21)
	filein = filebase + '.stripchart.fits'
endif
openr,luhk,file_hk,/delete,/swap_if_little_endian

satellite_time = 0.0D0
hk_array = uintarr(120)
data = uintarr(121,num_hk_records)
physical_units = dblarr(121,num_hk_records)

phys_mins = fltarr(121)
phys_maxs = phys_mins
phys_mean = phys_mins
phys_median = phys_mins
phys_stdev = phys_mins

; read data from file as unformatted binary data
for i=0,num_hk_records-1 do begin
	if (eof(luhk)) then begin
		data = data[*,0:i-1]	; truncate data array
		physical_units = physical_units[*,0:i-1]
		goto, Process_Data
	endif
	readu,luhk,satellite_time,hk_array
	if (i eq 0) then begin
		time0 = satellite_time
	endif
	data[0,i] = fix(satellite_time-time0)
	data[1:120,i] = hk_array[0:119]
	physical_units[0,i] = satellite_time
	for j=1,113 do begin
		chan = stripchart_hk_chan[j-1]
		if (rtd_channel[chan] gt 0) then $
			physical_units[j,i] = RTD(hk_array[j-1], rtd_channel[chan]) $
		else $
			physical_units[j,i] = hk_array[j-1]*hk_slope[chan] $
				+ hk_intercept[chan]
	endfor
	physical_units[114:*,i] = hk_array[113:*]
endfor

Process_Data:

; First, calculate statistics

for i=1,120 do begin
	phys_mins[i] = min(physical_units[i,*])
	phys_maxs[i] = max(physical_units[i,*])
	phys_median[i] = median(physical_units[i,*])
	moments = moment(physical_units[i,*])
	phys_mean[i] = moments[0]
	phys_stdev[i] = sqrt(moments[1])
endfor


; Now print HK means in physical units

printf,lulog,'Analog HK in Physical Units:         Name      Min     Mean+/-Std Dev    Max'
for i=0,112 do begin
    	chan = stripchart_hk_chan[i]
	
	hk_string = string(format='(a8,a6,3x,a24,2f9.3,''+/-'',f6.3,f9.3,1x,a)', $
		strtrim('HK'+strtrim(string(hk_chan[chan]),2)), $
		strtrim(mux_chan[chan]), hk_name[chan], phys_mins[i+1], phys_mean[i+1], $
		phys_stdev[i+1], phys_maxs[i+1], hk_units[chan])
	if (phys_mins[i+1] lt hk_low[chan]) then begin
		hk_string = hk_string + '   *** Min is out-of-range ***   '
	endif
	if (phys_maxs[i+1] gt hk_high[chan]) then begin
		hk_string = hk_string + '   *** Max is out-of-range ***   '
	endif
	printf,lulog,hk_string
endfor

for i=0,112 do begin
	chan = stripchart_hk_chan[i]
	if (phys_mins[i+1] lt hk_low[chan]) then begin
		print_error, 0, 1, 'PROCESS_STRIPCHART_DATA: *** HK channel ' $
			+ strtrim(string(chan),2) + ' = ' + hk_name[chan]$
			+ ':  Min (' + strtrim(string(phys_mins[i+1]),2) $
			+ ') is below lower limit of '$
			+ strtrim(string(hk_low[chan]),2) + ' ***   '
	endif
	if (phys_maxs[i+1] gt hk_high[chan]) then begin
		print_error, 0, 1, 'PROCESS_STRIPCHART_DATA: *** HK channel ' $
			+ strtrim(string(chan),2) + ' = ' + hk_name[chan]$
			+ ':  Max (' + strtrim(string(phys_maxs[i+1]),2) $
			+ ') is above upper limit of '$
			+ strtrim(string(hk_high[chan]),2) + ' ***   '
	endif
endfor




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
sxaddhist,'XRT Raw Housekeeping Data File created by STRIPCHART '+version, header
sxaddhist,'Column 1 is the relative time, columns 2-114 are ',header
sxaddhist,'     the Raw HK channel data.',header	
sxaddhist,'     Column 115 contains the XRT count rate', header
sxaddhist,'     Columns 116 - 121 contain the Status Flags', header
sxaddhist,'     in order: XRT State Flag, XRT Mode, ACS_Settled,',header
sxaddhist,'     ACS_in_10_arcmin, In_SAA, and Safe_Mode', header
sxaddhist,' ', header
sxaddpar,header,'TIME0',time0,'Initial Satellite Time'
sxaddhist,' ', header
sxaddhist,'HK Channel Names:',header
for i=0,112 do begin
    chan = stripchart_hk_chan[i]
    case strtrim(hk_units[chan],2) of
	'V': quantity = 'V'
	'A': quantity = 'I'
	'C': quantity = 'T'
	'Celsius': quantity = 'T'
	'degrees': quantity = 'theta'
	'Torr': quantity = 'P'
	'Bar': quantity = 'P'
	else: quantity = ' '
    endcase
    sxaddhist,'Word' + strtrim(string(i+1),2) + ': HK' + $
		strtrim(string(hk_chan[chan],format='(i3.3)'),2) + ': ' $
	    	+ hk_name[chan] + quantity + ' = ' $
		+ strtrim(string(hk_slope[chan]),2) $
	    	+ ' x DN + ' + strtrim(string(hk_intercept[chan]),2) + ' ' $
		+ hk_units[chan], header 
endfor
sxaddhist,'Word114: XRT Count Rate (cps)', header
sxaddhist,'Word115: XRT State Flag      ', header
sxaddhist,'Word116: XRT Mode Flag       ', header
sxaddhist,'Word117: ACS Is_Settled Flag ', header
sxaddhist,'Word118: ACS In_10_Arcmin Flag', header
sxaddhist,'Word119: ACS In_SAA Flag     ', header
sxaddhist,'Word120: ACS Safe_Mode Flag  ', header

raw_hk_fits_file = filebase + '.raw_stripchart.fits'
writefits,raw_hk_fits_file,data,header

; now write out physical units
mkhdr,hdr2,physical_units

sxaddpar,hdr2,'DATE',date,'File creation date'
sxaddpar,hdr2,'ORIGIN','PSU X-ray Astronomy', $
		'Data from Penn State X-ray Astronomy'
sxaddpar,hdr2,'INSTRUME','XRT','Swift X-ray Telescope'
sxaddpar,hdr2,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
sxaddhist,' ',hdr2
sxaddhist,'XRT Converted Housekeeping Data File created by STRIPCHART '+version, hdr2
sxaddhist,'Column 1 is the relative time, columns 2-114 are ',hdr2
sxaddhist,'     the HK data converted to physical units.',hdr2
sxaddhist,'     Column 115 contains the XRT count rate', hdr2
sxaddhist,'     Columns 116 - 121 contain the Status Flags', hdr2
sxaddhist,'     in order: XRT State Flag, XRT Mode, ACS_Settled,',hdr2
sxaddhist,'     ACS_in_10_arcmin, In_SAA, and Safe_mode', hdr2
sxaddhist,' ', hdr2
sxaddpar,hdr2,'TIME0',time0,'Initial Satellite Time'
sxaddhist,' ', hdr2
sxaddhist,'HK Channel Names:',hdr2
for i=0,112 do begin
    chan = stripchart_hk_chan[i]
    case strtrim(hk_units(i),2) of
	'V': quantity = 'V'
	'A': quantity = 'I'
	'C': quantity = 'T'
	'Celsius': quantity = 'T'
	'degrees': quantity = 'theta'
	'Torr': quantity = 'P'
	'Bar': quantity = 'P'
	else: quantity = ' '
    endcase
    sxaddhist,'Word' + strtrim(string(i+1),2) + ': HK' + $
	strtrim(string(hk_chan[chan],format='(i3.3)'),2) + ': ' $
	+ hk_name[chan] + quantity + ' = ' + $
	strtrim(string(hk_slope[chan]),2) $
	+ ' x DN + ' + strtrim(string(hk_intercept[chan]),2) + ' ' + $
	hk_units[chan], hdr2 
endfor
sxaddhist,'Word114: XRT Count Rate (cps)', hdr2
sxaddhist,'Word115: XRT State Flag      ', hdr2
sxaddhist,'Word116: XRT Mode Flag      ', hdr2
sxaddhist,'Word117: ACS Is_Settled Flag ', hdr2
sxaddhist,'Word118: ACS In_10_Arcmin Flag', hdr2
sxaddhist,'Word119: ACS In_SAA Flag     ', hdr2
sxaddhist,'Word120: ACS Safe_Mode Flag  ', hdr2

hk_fits_file = filebase + '.stripchart.fits'
writefits,hk_fits_file,physical_units,hdr2

; Now write these out as FITS tables

;print,' '
;print,'Writing out binary table file for raw HK data'
;print,' '
;
;; Write out the FITS file primary header
;raw_hk_fits_table_file = filebase + '.raw_stripchart_table.fits'
;mwrfits,undefined,raw_hk_fits_table_file,header[5:*],/create
;
;; Make table extension header
;fxbhmake,table_hdr,num_hk_records,'RAW_HK','Swift XRT Raw HK data file',/date
;sxaddpar,table_hdr,'DATE',date,'File creation date'
;sxaddpar,table_hdr,'ORIGIN','PSU X-ray Astronomy', $
;		'Data from Penn State X-ray Astronomy'
;sxaddpar,table_hdr,'INSTRUME','XRT','Swift X-ray Telescope'
;sxaddpar,table_hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
;sxaddhist,' ',table_hdr
;sxaddhist,'XRT Raw Housekeeping Data File created by STRIPCHART '+version, table_hdr
;sxaddhist,'Column 1 is the spacecraft time, columns 2-114 are ',table_hdr
;sxaddhist,'     the Raw HK channel data.',table_hdr	
;sxaddhist,'     Column 115 contains the XRT count rate', table_hdr
;sxaddhist,'     Columns 116 - 121 contain the Status Flags', table_hdr
;sxaddhist,'     in order: XRT State Flag, XRT Mode, ACS_Settled,',table_hdr
;sxaddhist,'     ACS_in_10_arcmin, In_SAA, and Safe_Mode', table_hdr
;sxaddhist,' ', table_hdr
;sxaddpar,table_hdr,'TIME0',time0,'Initial Satellite Time'
;sxaddhist,' ', table_hdr
;sxaddhist,'HK Channel Names:',table_hdr
;for i=0,112 do begin
;    chan = stripchart_hk_chan[i]
;    case strtrim(hk_units[chan],2) of
;	'V': quantity = 'V'
;	'A': quantity = 'I'
;	'C': quantity = 'T'
;	'Celsius': quantity = 'T'
;	'degrees': quantity = 'theta'
;	'Torr': quantity = 'P'
;	'Bar': quantity = 'P'
;	else: quantity = ' '
;    endcase
;    sxaddhist,'Column ' + strtrim(string(i+2),2) + ': HK' + $
;		strtrim(string(hk_chan[chan],format='(i3.3)'),2) + ': ' $
;	    	+ hk_name[chan] + quantity + ' = ' $
;		+ strtrim(string(hk_slope[chan]),2) $
;	    	+ ' x DN + ' + strtrim(string(hk_intercept[chan]),2) + ' ' $
;		+ hk_units[chan], table_hdr 
;endfor
;sxaddhist,'Column 115: XRT Count Rate (cps)', table_hdr
;sxaddhist,'Column 116: XRT State Flag      ', table_hdr
;sxaddhist,'Column 117: XRT Mode Flag       ', table_hdr
;sxaddhist,'Column 118: ACS Is_Settled Flag ', table_hdr
;sxaddhist,'Column 119: ACS In_10_Arcmin Flag', table_hdr
;sxaddhist,'Column 120: ACS In_SAA Flag     ', table_hdr
;sxaddhist,'Column 121: ACS Safe_Mode Flag  ', table_hdr
;sxaddpar, table_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'
;
;
;fxbaddcol,1, table_hdr, physical_units[0,0], 'SC time','Spacecraft clock time',$
;	TUNIT='s'
;for i=0,112 do begin
;    chan = stripchart_hk_chan[i,0]
;    fxbaddcol,i+2, table_hdr,fix(data[i,0]), hk_name[chan], $
;		'Word' + strtrim(string(i+1),2) + ': HK' + $
;		strtrim(string(hk_chan[chan],format='(i3.3)'),2) + ': ' $
;	    	+ hk_name[chan] + quantity + ' = ' $
;		+ strtrim(string(hk_slope[chan]),2) $
;	    	+ ' x DN + ' + strtrim(string(hk_intercept[chan]),2) + ' ' $
;		+ hk_units[chan], $
;		TUNIT='DN'
;endfor
;fxbaddcol,115, table_hdr, physical_units[114,0], 'Count Rate', TUNIT='cps'
;fxbaddcol,116, table_hdr, physical_units[115,0], 'XRT State Flag'
;fxbaddcol,117, table_hdr, physical_units[116,0], 'XRT Mode Flag' 
;fxbaddcol,118, table_hdr, physical_units[117,0], 'ACS IS_Settled'
;fxbaddcol,119,table_hdr,physical_units[118,0],'ACS IN_10_ARCMIN'
;fxbaddcol,120,table_hdr,physical_units[119,0],'ACS IN_SAA'
;fxbaddcol,121,table_hdr,physical_units[120,0],'ACS SAFE_HOLD'
;
;
;fxbcreate, luout, raw_hk_fits_table_file, table_hdr, extension
;
;for row=1,num_hk_records do begin
;	fxbwrite,luout,physical_units[0,row-1],1,row
;	for col=2,121 do begin
;		fxbwrite,luout,fix(data[col-2,row-1]),col,row
;	endfor
;endfor
;fxbfinish,luout

; repeat for physical units ******************************************

print,' '
print,'Writing out binary FITS file for HK in physical units'
print, ' '

; Write out the FITS file primary header
hk_fits_table_file = filebase + '.stripchart_table.fits'
mwrfits,undefined,hk_fits_table_file,header[5:*],/create

; Make table extension header
fxbhmake,table_hdr,num_hk_records,'XRT_HK','Swift XRT HK data file',/date
sxaddpar,table_hdr,'DATE',date,'File creation date'
sxaddpar,table_hdr,'ORIGIN','PSU X-ray Astronomy', $
		'Data from Penn State X-ray Astronomy'
sxaddpar,table_hdr,'INSTRUME','XRT','Swift X-ray Telescope'
sxaddpar,table_hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
sxaddhist,' ',table_hdr
sxaddhist,'XRT Housekeeping Data File created by STRIPCHART '+version, table_hdr
sxaddhist,'Column 1 is the spacecraft time, columns 2-114 are ',table_hdr
sxaddhist,'     the HK channel data in physical units.',table_hdr	
sxaddhist,'     Column 115 contains the XRT count rate', table_hdr
sxaddhist,'     Columns 116 - 121 contain the Status Flags', table_hdr
sxaddhist,'     in order: XRT State Flag, XRT Mode, ACS_Settled,',table_hdr
sxaddhist,'     ACS_in_10_arcmin, In_SAA, and Safe_Mode', table_hdr
sxaddhist,' ', table_hdr
sxaddpar,table_hdr,'TIME0',time0,'Initial Satellite Time'
sxaddhist,' ', table_hdr
sxaddhist,'HK Channel Names:',table_hdr
for i=0,112 do begin
    chan = stripchart_hk_chan[i]
    case strtrim(hk_units[chan],2) of
	'V': quantity = 'V'
	'A': quantity = 'I'
	'C': quantity = 'T'
	'Celsius': quantity = 'T'
	'degrees': quantity = 'theta'
	'Torr': quantity = 'P'
	'Bar': quantity = 'P'
	else: quantity = ' '
    endcase
    sxaddhist,'Column ' + strtrim(string(i+2),2) + ': HK' + $
		strtrim(string(hk_chan[chan],format='(i3.3)'),2) + ': ' $
	    	+ hk_name[chan] + quantity + ' = ' $
		+ strtrim(string(hk_slope[chan]),2) $
	    	+ ' x DN + ' + strtrim(string(hk_intercept[chan]),2) + ' ' $
		+ hk_units[chan], table_hdr 
endfor
sxaddhist,'Column 115: XRT Count Rate (cps)', table_hdr
sxaddhist,'Column 116: XRT State Flag      ', table_hdr
sxaddhist,'Column 117: XRT Mode Flag       ', table_hdr
sxaddhist,'Column 118: ACS Is_Settled Flag ', table_hdr
sxaddhist,'Column 119: ACS In_10_Arcmin Flag', table_hdr
sxaddhist,'Column 120: ACS In_SAA Flag     ', table_hdr
sxaddhist,'Column 121: ACS Safe_Mode Flag  ', table_hdr
sxaddpar, table_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'


fxbaddcol,1, table_hdr, physical_units[0,0], 'SC time','Spacecraft clock time',$
	TUNIT='s'
for i=0,112 do begin
    chan = stripchart_hk_chan[i,0]
    fxbaddcol,i+2, table_hdr,physical_units[i+1,0], hk_name[chan], $
		TUNIT=hk_units[chan]
endfor
fxbaddcol,115, table_hdr, physical_units[114,0], 'Count Rate', TUNIT='cps'
fxbaddcol,116, table_hdr, physical_units[115,0], 'XRT State Flag'
fxbaddcol,117, table_hdr, physical_units[116,0], 'XRT Mode Flag' 
fxbaddcol,118, table_hdr, physical_units[117,0], 'ACS IS_Settled'
fxbaddcol,119,table_hdr,physical_units[118,0],'ACS IN_10_ARCMIN'
fxbaddcol,120,table_hdr,physical_units[119,0],'ACS IN_SAA'
fxbaddcol,121,table_hdr,physical_units[120,0],'ACS SAFE_HOLD'

print,'Done creating binary FITS file header ...'

fxbcreate, luout, hk_fits_table_file, table_hdr, extension

print,'Writing ', num_hk_records, ' rows of data to binary FITS file'
print,format='(80(''X''))'
TBD = num_hk_records/80
for row=1,num_hk_records do begin
	if ((row mod TBD) eq 0) then print,format='(a1,$)', '.'
	fxbwrite,luout,physical_units[0,row-1],1,row
	for col=2,121 do begin
		fxbwrite,luout,physical_units[col-1,row-1],col,row
	endfor
endfor
fxbfinish,luout
print,'Finished writing out binary FITS table...'

;************************************************************************

print,''
   
; display image using Pat Broos' routines
if (num_hk_records gt 1) then begin
	if (basic_test_mode eq 1) then begin
		basic_func_test_plot
	endif else begin
;	    if (quick_look_mode eq 1) then begin
;			quick_look_plot
;    endif else begin
		xmin = 0
		xmax = n_elements(physical_units[0,*])
		if (no_plot eq 0) then stripchart_hk_plots,xmin,xmax
			; makes plots, sends to printer if noprint not set
		if (show_hk_image ge 2) and (quick_look_mode ne 1) then begin
			print,''
			print,'*** Displaying stripchart HK data as 2D image ***'
			print,'      The column number gives the HK channel.'
			print,'      A vertical cut through the image displays'
			print,'      a given HK channel versus time '
			dummy = stripchart_hk_viewer(float(physical_units[*,xmin:xmax]),$
					title='XRT Housekeeping Data')
			xmanager
;			xdisplayfile,'',text=hdr2,group=dummy,$
;					title='XRT Housekeeping Data Header'
		endif
     endelse 
;  endelse 
endif else begin
	print,'Only 1 HK record, so I can''t display it as an image'
endelse

file_hk = ''
return
end
