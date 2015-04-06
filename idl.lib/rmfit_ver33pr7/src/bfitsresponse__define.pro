; ----------------------------------------------------------------------------
;+
; NAME:
;
;     BfitsResponse (OBJECT)
;
; PURPOSE:
;
;     This object encapsulates the BATSE detector response function.
;     The response data are stored in FITS format (BFITS) files.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('BfitsResponse' [, filename])
;
; INPUTS:
;
;     filename (OPTIONAL) : STRING denoting the response file to read.  If
;         this parameter is given, the file will be read when the object
;         is instantiated.
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     Response
;
; DEPENDENCIES:
;
;     file__define.pro
;     response__define.pro
;
; METHODS:
;     
;     read (PROCEDURE) - Read a response file
;         Inputs: FILENAME : STRING denoting the response data filename
;        Outputs: NONE 
;       Keywords: ERROR : Set to a named variable to return 0 if the read
;                 was successful, or 1 if the read failed.
;
;     response (FUNCTION) - Return response data
;         Inputs: NONE
;        Outputs: The response data
;       Keywords: NONE
;
;     display (PROCEDURE) - Plot response contours
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: Any valid PLOT or CONTOUR keywords
;
; MODIFICATION HISTORY:
;
;     1.0 : Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION BfitsResponse::init, filename, READ = read

    status = self->response::init ()

    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
      
       self.File->set, filename   
       
       self->read, ERROR = error          
       IF (error) THEN BEGIN
          MESSAGE, /CONT, 'Failed to read response file: ' + STRING (filename)
          RETURN, 0
       ENDIF
        
    ENDIF
    
    RETURN, status

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO BfitsResponse::cleanup

    PTR_FREE, self.chan_energy 
    PTR_FREE, self.chan_width  
    PTR_FREE, self.chan_edges

    PTR_FREE, self.phot_energy
    PTR_FREE, self.phot_width     
    PTR_FREE, self.phot_edges

    self->response::cleanup

END    
    

; ----------------------------------------------------------------------------
; Read a BATSE DRM FITS file
; ----------------------------------------------------------------------------
PRO BfitsResponse::read, DATAREADER = dataReader, ERROR = error

    error = 0
    
    filename = self.File->get ()
        
    IF (filename[0] EQ '') THEN BEGIN
       MESSAGE, /CONTINUE, 'No response filename is set.'
       error = 1
       RETURN
    ENDIF


    MESSAGE, /RESET_ERROR_STATE    
    ON_IOERROR, READ_ERROR

    zenAngle = 0
    ok = OBJ_VALID (dataReader)
    
    ; For BATSE data, we get the detector zenith angle thusly:
    IF ok THEN $
    zenAngle = dataReader->getZenAngle()
    
    ;== Read data
    
    ok = self->read_compressed (filename, $
        num_ebins, num_chan, e_edges, c_edges, $
        resp_matrix, scat_matrix, mat_type, zenAngle)

    IF (NOT ok) THEN BEGIN
       ;MESSAGE, /CONTINUE, 'Failed to read compressed DRM.'
       error = 1
       RETURN
    ENDIF   


    ;== Single detector
    
    drm_dim1 = num_ebins - 1L
    drm_dim2 = num_chan - 1L
    drm_dim3 = 0L                      ; TODO: Individual multiple detectors

    IF (mat_type EQ 2) THEN BEGIN
       IF (DIALOG_MESSAGE (/QUESTION, $
          'Include Earth Scattering Matrix?') EQ 'Yes') THEN $
          resp_matrix = resp_matrix + scat_matrix
    ENDIF

    PTR_FREE, self.chan_energy
    self.chan_energy = PTR_NEW ( $
        (c_edges[1:num_chan] + c_edges[0:num_chan - 1]) / 2.0)
    
    PTR_FREE, self.chan_width           
    self.chan_width = PTR_NEW ( $
        c_edges[1:num_chan] - c_edges[0:num_chan - 1])
    
; TODO ???
;    idx = WHERE (*self.chan_width LE 0.0, cnt)
;    IF (cnt NE 0) THEN $
;       (*self.chan_width)[idx] = 0.01
    
    PTR_FREE, self.phot_energy
    self.phot_energy = PTR_NEW ( $
        (e_edges[0:num_ebins - 1] + e_edges[1:num_ebins]) / 2.0)
    
    PTR_FREE, self.phot_width
    self.phot_width  = PTR_NEW ( $
        e_edges[1: num_ebins] - e_edges[0: num_ebins - 1])

    PTR_FREE, self.response
    self.response = PTR_NEW (resp_matrix)
        
    PTR_FREE, self.chan_edges
    self.chan_edges = PTR_NEW (c_edges)
    
    PTR_FREE, self.phot_edges
    self.phot_edges = PTR_NEW (e_edges)

    ;== Read FITS headers and data tables

    header0 = HEADFITS (filename, EXTEN = 0)
    header1 = HEADFITS (filename, EXTEN = 1)
    ;header2 = HEADFITS (filename, EXTEN = 2)

    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0  : header0, $
        ext1  : header1  $
        ;ext2  : header2  $

    })    

READ_ERROR:    

    IF (!ERROR_STATE.CODE LT 0) THEN BEGIN
       error = 1
       MESSAGE, /CONTINUE, !ERROR_STATE.MSG
    ENDIF

END


; ----------------------------------------------------------------------------
; Read a BATSE DRM stored in compressed format
; ----------------------------------------------------------------------------
FUNCTION BfitsResponse::read_compressed, filename, $
    num_ebins, num_chan, e_edges, c_edges, resp_matrix, $
    scat_matrix, mat_type, zenAngle


    data0 = MRDFITS (filename, 0, header0, /SILENT)
    
    IF (SXPAR (header0, 'FILETYPE') NE 'BATSE_DRM') THEN BEGIN
       ;MESSAGE, /CONT, 'Unknown DRM file type: ' + SXPAR (header0, 'FILETYPE')
       RETURN, 0
    ENDIF
   
    num_ebins = SXPAR (header0, 'N_E_BINS')
    num_chan  = SXPAR (header0, 'N_E_CHAN')
    det_mode  = STRTRIM (SXPAR (header0, 'DET_MODE'), 2)
    
    data1 = MRDFITS (filename, 1, header1, /SILENT)
    num_drm  = SXPAR (header1, 'NAXIS2')
    mat_type = data1.mat_type
    
    resp_matrix = FLTARR (num_ebins, num_chan)

; TODO: mat_type is a vector if multiple DRMs are used
; TODO: For now, assume all types are the same as the 0th element
    mat_type = mat_type[0]
    
    IF (mat_type EQ 2) THEN $
       scat_matrix = FLTARR (num_ebins, num_chan)

    e_edges = FLTARR (num_ebins + 1)
    c_edges = FLTARR (num_chan + 1)
    n_zeros = INTARR (num_chan)

    FOR j = 1, num_drm DO BEGIN

        e_edges = TEMPORARY (e_edges) + data1[j-1].pht_edge
        c_edges = TEMPORARY (c_edges) + data1[j-1].e_edges
       
        n_zeros = data1[j-1].n_zeros
       
        CASE (mat_type) OF

            0: compressed = data1[j-1].drm_dir

            ;1: compressed = data1[j-1].drm_sca
            1: compressed = data1[j-1].drm_sct

            2: BEGIN
               compressed = data1[j-1].drm_dir
               ;scattered  = data1[j-1].drm_sca
               scattered  = data1[j-1].drm_sct
               END

            3: compressed = data1[j-1].drm_sum

            ELSE: MESSAGE, 'Unknown matrix type: ' + STRTRIM (mat_type, 2)

        ENDCASE

       upper = num_ebins - 1
       index = 0
       
       FOR i = 0, num_chan - 1 DO BEGIN
           
           lower = n_zeros[i] - 1
           
           resp_matrix[lower: upper, i] = resp_matrix[lower: upper, i] + $
               compressed[index: index + (upper - lower)]
           
           index = index + num_ebins - lower
       
       ENDFOR

       IF (mat_type EQ 2) THEN BEGIN
          
          index = 0
          
          FOR i = 0, num_chan - 1 DO BEGIN
          
              lower = n_zeros[i] - 1
              
              scat_matrix[lower: upper, i] = scat_matrix[lower: upper, i] + $
                  scattered[index: index + (upper - lower)]
              
              index = index + num_ebins - lower
          
          ENDFOR
       
       ENDIF

    ENDFOR

    e_edges = e_edges / num_drm
    c_edges = c_edges / num_drm
      
    ;== MP paper correction
    
    IF (STRUPCASE (det_mode) EQ 'SD') THEN BEGIN

       ; MESSAGE, /CONTINUE, 'SD millipore paper correction added.'

       e_mid = (e_edges[0: num_ebins - 1] + e_edges[1: num_ebins]) / 2.0
       trans = EXP (-244.5 / COS (zenAngle * !DTOR) * e_mid ^ (-2.95))
       FOR i = 0, num_chan - 1 DO $
           resp_matrix[*, i] = resp_matrix[*, i] * trans

    ENDIF

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Return response data
; ----------------------------------------------------------------------------
FUNCTION BfitsResponse::response

    resp = { $

        chan_energy : *self.chan_energy, $ 
        chan_width  : *self.chan_width,  $  
        chan_edges  : *self.chan_edges,  $  
        phot_energy : *self.phot_energy, $
        phot_width  : *self.phot_width,  $ 
        phot_edges  : *self.phot_edges,  $ 
        drm         : *self.response     $

    }
    
    RETURN, resp
    
END

;== Not Needed!
; ----------------------------------------------------------------------------
; Display response data
; ----------------------------------------------------------------------------
;PRO BfitsResponse::display, plotColor, xr, yr, COLORIZE = colorize, FLIP = flip, $
;                   _REF_EXTRA = extra
;
;    num_ebin = (SIZE (*self.response))[1]
;    num_chan = (SIZE (*self.response))[2]
;
;    ch_width = (*self.chan_width)[0: num_chan - 2]
;    eb_width = (*self.phot_width)[0: num_ebin - 2]
;
;    resp = (*self.response)[0: num_ebin - 2, 0: num_chan - 2]
;    FOR i = 0, num_ebin - 2 DO $
;        resp[i, *] = resp[i, *] / ch_width
;
;    ln_resp  = ALOG (resp + 0.01)
;    
;    ch_e     = (*self.chan_edges)[0: num_chan - 2] + 0.5 * ch_width
;    eb_e     = (*self.phot_edges)[0: num_ebin - 2] + 0.5 * eb_width
;    
;    XTITLE = 'Photon Energy (keV)'
;    YTITLE = 'Channel Energy (keV)'
;
;    IF (FLIP EQ 1) THEN BEGIN
;        ln_resp = TRANSPOSE (ln_resp)
;        tempChan = ch_e
;        ch_e = eb_e
;        eb_e = tempChan
;        tempTitle = XTITLE
;        XTITLE = YTITLE
;        YTITLE = tempTitle
;        END
;    xr       = [MIN (eb_e), MAX (eb_e)]
;    yr       = [MIN (ch_e), MAX (ch_e)]
;    ln_ch_e  = ALOG10 (ch_e)
;    ln_eb_e  = ALOG10 (eb_e)
;
;    IF colorize THEN BEGIN
;        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 255, $ 
;            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
;            C_COLORS = INDGEN(255), /FILL, $
;            XRANGE = xr, YRANGE = yr, $
;            _EXTRA = [*self.PlotOptions, 'XRANGE', 'YRANGE']
;            
;        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 10, $ 
;            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
;            COLOR = plotColor, /NOERASE, $
;            XRANGE = xr, YRANGE = yr, $
;            _EXTRA = [extra, 'XRANGE', 'YRANGE']
;    ENDIF ELSE BEGIN
;        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 10, $ 
;            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
;            COLOR = plotColor, $
;            XRANGE = xr, YRANGE = yr, $
;            _EXTRA = [extra, 'XRANGE', 'YRANGE']
;    ENDELSE
;
;    PLOT, xr, yr, /XLOG, /YLOG, /NOERASE, /NODATA, $
;        TITLE  = 'DRM Contour Map', $
;        XTITLE = xTitle, YTITLE = yTitle, $
;        XRANGE = xr, YRANGE = yr, $
;        XSTYLE = 1, YSTYLE = 1, _EXTRA = [extra, 'XRANGE', 'YRANGE']
;
;END
;
;
; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO BfitsResponse__define

    obj = { BFITSRESPONSE, INHERITS RESPONSE, $

        chan_energy : PTR_NEW (), $ 
        chan_width  : PTR_NEW (), $
        chan_edges  : PTR_NEW (), $
         
        phot_energy : PTR_NEW (), $
        phot_width  : PTR_NEW (), $
        phot_edges  : PTR_NEW ()  $

    }

END
