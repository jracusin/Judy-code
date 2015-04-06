pro make_default_tags,taglist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    MAKE_DEFAULT_TAGS
;       
; PURPOSE:
;    Produces a list of photo tags.  Can be sent to READ_PHOTO_COL or
;    RD_TSOBJ
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() eq 0 then begin
	print,'-syntax make_default_tags, taglist'
	return
endif

taglist = ['ID',              $
           'PARENT',          $
           'NCHILD',          $
           'OBJC_TYPE',       $
           'TYPE',            $
           'FLAGS',           $
           'FLAGS2',          $
           'OBJC_FLAGS',      $
           'OBJC_FLAGS2',     $
           'STATUS',          $
           'RA',              $
           'DEC',             $
           'PRIMTARGET',      $
           'SECTARGET',       $
           'OBJC_ROWC',       $
           'OBJC_ROWCERR',    $
           'OBJC_COLC',       $
           'OBJC_COLCERR',    $
           'ROWC',            $
           'COLC',            $
           'FIBERCOUNTS',     $
           'FIBERCOUNTSERR',  $
           'PETROCOUNTS',     $
           'PETROCOUNTSERR',  $
           'COUNTS_MODEL',    $
           'COUNTS_MODELERR', $
           'STAR_L',          $
           'EXP_L',           $
           'DEV_L',           $
           'PETRORAD',        $
           'PETRORADERR',     $
           'PETROR50',        $
           'PETROR50ERR',     $
           'PETROR90',        $
           'PETROR90ERR',     $
           'REDDENING',       $
           'Q',               $
           'QERR',            $
           'U',               $
           'UERR']

return
end
