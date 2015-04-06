pro write_tam_image
; $Id: write_tam_image.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: write_tam_image
;author	: Dave Burrows
;date	: 04/07/03
;lang	: IDL
;
;purpose: This routine write XRT TAM images (full-frame)
;	out to disk as 2D FITS files (compressed) and displays their image.
;
; Rev: 
;	05/12/03 by DNB: shortened output file name
;
; $Log: write_ccd_image.pro $
;
;

@pass1_common

@tam_common

error_code = 0

print,'Writing FITS file for TAM', $
	' image #', strtrim(string(tam_frame)), $
	' at S/C time ', tam_image_time   ;, ': ', N_events, ' pixels'
printf,lulog,'Writing FITS file for TAM', $
	' image #', strtrim(string(tam_frame)), $
	' at S/C time ', tam_image_time   ;, ': ', N_events, ' pixels'

file_tam = filebase + '.frame' + strtrim(string(tam_frame),2) + '.fits'
writefits,file_tam,image,tam_hdr

print,'Compressing FITS file...'
printf,lulog,'Compressing FITS file...'
spawn,'gzip '+file_tam

print,''
printf,lulog,''

print,''
print,'*** Displaying TAM image ***'
   
if (show_plot gt 0) then show_tam_image


return
end
