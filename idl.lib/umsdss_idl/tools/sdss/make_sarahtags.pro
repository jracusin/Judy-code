pro make_sarahtags,taglist

if n_params() eq 0 then begin
	print,'-syntax make_sarahtags, taglist'
	return
endif

taglist = ['PARENT','NCHILD','ID','OBJC_TYPE','OBJC_FLAGS','OBJC_FLAGS2',$
	'OBJC_ROWC','OBJC_ROWCERR','OBJC_COLC','OBJC_COLCERR',$
	'ROWC','COLC','FIBERCOUNTS','FIBERCOUNTSERR','PETROCOUNTS','PETROCOUNTSERR','PETRORAD',$
	'PETRORADERR','Q','QERR','U','UERR','FLAGS','FLAGS2',$
	'RA','DEC','PRIMTARGET','SECTARGET']

return
end
