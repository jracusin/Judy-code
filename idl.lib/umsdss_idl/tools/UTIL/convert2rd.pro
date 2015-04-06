
pro convert2rd,x,y,ra,dec,rac=rac,decc=decc

;+
; NAME:
;    CONVERT2RD
;
; CALLING SEQUENCE:
;    convert2rd,x,y,ra,dec,rac=rac,decc=decc
;
; PURPOSE:
;    Convert from x-y to ra and dec
;
;-
if n_params() eq 0 then begin
  	print, 'syntax- convert2rd,x,y,ra,dec,rac=rac,decc=decc'
  	return
endif

astr_struct,astr
astr.crval=[double(rac),double(decc)]
xy2rd,x,y,astr,ra,dec
return
end
