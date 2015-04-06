pro start_new_data_block, buffer_pointer, error_code

; $Id: start_new_data_block.pro 3.1 1995/12/21 17:25:30 burrows Exp $

;name	: start_new_data_block
;author : Dave Burrows
;date	: 01/11/95
;lang	: IDL
;
;purpose: This routine reads a 1 Mbit block of data and checks for EOT.
;	On return, the error_code variable
;	contains the error code passed by CHECK_SYNCH_WORDS.
;
;	Error Codes:
;	   11	START_NEW_DATA_BLOCK: at EOF - no new data to read
;	   12	START_NEW_DATA_BLOCK: found EOF - incomplete data block
;	   13	START_NEW_DATA_BLOCK: error in CCSDS header
;
;
; $Log: start_new_data_block.pro $
;

@pass1_common

; "declare" variable types
error_code = 0
bytes_left = 0

; Check for EOF:
;if (end_of_data eq 0) then begin
	file_status = fstat(luin)
	bytes_left = file_status.size - file_status.cur_ptr
	if (idebug gt 20) then begin
	    print,'START_NEW_DATA_BLOCK: file has ', bytes_left, ' bytes left'
	endif
;endif

print,' '
print,'*************************************************************'
print,format="(' Reading 1 Mbit data block #', i3.3,' from file ', a)", $
		block_number+1, filein

printf,lulog,' '
printf,lulog,'*************************************************************'
printf,lulog,format="(' Reading 1 Mbit data block #', i3.3,' from file ', a)", $
		block_number+1, filein


if (bytes_left eq 0) then begin
	print,'START_NEW_DATA_BLOCK: At EOF'
	printf,lulog,'START_NEW_DATA_BLOCK: At EOF'
	error_code = 11
	data_buffer = bytarr(512L*256L)
	data_buffer_size = 0
	return
endif else begin
	if (bytes_left lt 131072) then begin
; Note: for XRT, data are not organized in 1 Mbit blocks, so it is normal to 
;	find an incomplete block at the end of the file.
		print,'START_NEW_DATA_BLOCK: ***** Reading last data block *****'
		printf,lulog,'START_NEW_DATA_BLOCK: ***** LAST BLOCK contains ' + $
			strtrim(string(bytes_left),2) + ' bytes *****'
		error_code = 12
		end_of_data = 1 	; flag that we have reached the EOF
	endif
endelse

if (error_code eq 12) then $
	data_buffer = bytarr(bytes_left) $
else $
	data_buffer = bytarr(512L*256L)		; create data buffer array
data_buffer_size = n_elements(data_buffer)
readu,luin,data_buffer			; read 1 Mbit block of data
block_number = block_number + 1

; Check for EOT in last block
if (error_code eq 12) then begin
	temp = bytarr(131072)
	temp(0:bytes_left-1) = data_buffer
	if ((temp(bytes_left-2) ne '4E'xb) $
		or (temp(bytes_left-1) ne '07'xb)) then begin
; add 2 EOT words to end of data array if they weren't already there
			temp(bytes_left) = '4E'xb
			temp(bytes_left+1) = '07'xb
			temp(bytes_left+2) = '4E'xb
			temp(bytes_left+3) = '07'xb
			bytes_left = bytes_left + 4
	endif
	data_buffer = temp[0:bytes_left-1]
endif

buffer_pointer = 0
data_buffer_size = n_elements(data_buffer)

; TBD: CHECK_EOT, buffer_pointer, error_code

return
end

