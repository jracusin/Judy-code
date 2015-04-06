pro write_ccd_image
; $Id: write_ccd_image.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: write_ccd_image
;author	: Dave Burrows
;date	: 01/17/95
;lang	: IDL
;
;purpose: This routine write XRT CCD images (raw or compressed)
;	out to disk as 2D FITS files (compressed) and displays their image.
;
;
; $Log: write_ccd_image.pro $
;  Rev:
;	09/30/03 by DNB: last fix was sloppy and wouldn't compile.  Fixed
;		the problem with the if-then-else block and corrected
;		print statements to log file.
;	09/03/03 by DNB: fixed bug in print statement for bias maps
;	06/10/03 by DNB: added support for bias map frame numbering
;
;

@pass1_common

error_code = 0

if (frame_type eq '.bias_map_') then begin 
	print,'Writing FITS file for CCD', $
		' bias map #', strtrim(string(bias_frame)), $
		' at S/C time ', readout_stop_time
	printf,lulog,'Writing FITS file for CCD', $
		' bias map #', strtrim(string(bias_frame)), $
		' at S/C time ', readout_stop_time
	file_ccd = filebase + frame_type + strtrim(string(bias_frame),2) + '.fits'
endif else begin
	print,'Writing FITS file for CCD', $
		' image #', strtrim(string(ccd_frame)), $
		' at S/C time ', satellite_time   ;, ': ', N_events, ' valid X-ray events'
	printf,lulog,'Writing FITS file for CCD', $
		' image #', strtrim(string(ccd_frame)), $
		' at S/C time ', satellite_time   ;, ': ', N_events, ' valid X-ray events'
	file_ccd = filebase + frame_type + strtrim(string(ccd_frame),2) + '.fits'
endelse
writefits,file_ccd,image,ccd_hdr

print,'Compressing FITS file...'
printf,lulog,'Compressing FITS file...'
spawn,'gzip '+file_ccd

print,''
printf,lulog,''

if ((show_ccd_image gt 0) and ((ccd_temp lt -20.0) or pix_gen)) then begin
	print,''
	print,'*** Displaying CCD image ***'
   
	case show_ccd_image of
	    1: begin
		; display image using show_image routine
		pass1_show_image
	       end
	    2: begin
		; display image using Pat Broos' routines
		title='XRT CCD Image'
		dummy = pass1_ccd_viewer(image,title=title,$
				    XTITLE='CCD Column',YTITLE='CCD Row',$
				    INDEX_TO_DATA_OFFSET=[1,1])

		xdisplayfile,'',text=ccd_hdr,group=dummy,title='XRT CCD Frame Header'
		xmanager
	       end
	endcase
endif

; get rid of extra storage
image = 0

return
end
