; -----------------------------------------------------------------------------
;+
; NAME:
;     DIALOG_DISMISS (PROCEDURE)
;
; PURPOSE:
;     A generic event handler that can be used to destroy any top level widget.
;
; CALLING SEQUENCE:
;     XMANAGER, '_unused', topLevelBaseID, $
;         EVENT_HANDLER = 'DIALOG_DISMISS', /NO_BLOCK
;
;     Note the the required name parameter ('_unused') is stored in a
;     COMMON block by XMANAGER, so you should use a name that is unlikely
;     to clash with other event handler names that are in use.
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; OUTPUTS:
;     NONE
;
; DEPENDENCIES:
;     NONE
;
; MODIFICATION HISTORY:
;
;     Written, 2000 March, Robert.Mallozzi@msfc.nasa.gov
;
;-
; -----------------------------------------------------------------------------

PRO DIALOG_DISMISS, event

    IF (WIDGET_INFO (event.top, /VALID)) THEN $
       WIDGET_CONTROL, event.top, /DESTROY

END
