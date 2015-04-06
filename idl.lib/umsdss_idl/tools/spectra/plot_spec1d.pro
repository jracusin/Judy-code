;+
; NAME:
;  PLOT_SPEC1D
;
;
; PURPOSE:
;  Plot an SDSS spectra.  The redshift, spectral classification, spectro id,
;  and photo object id are also placed on the plot. Lines are labeled if that
;  information is available.
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;    By id: '
;       -Syntax: plot_spec1d, plateid, fiberid, $'
;            /noprompt, $'
;            /nolegend, $'
;            /nolines, $
;            mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $'
;            errclr=errclr, $'
;            leg_charsize=leg_charsize, $
;            label_charsize=label_charsize
;            _extra=_extra'
;    From structure: '
;       -Syntax: plot_spec1d, struct, $'
;            /nolegend, $'
;            /nolines, $
;            mjd=mjd, nsmooth=nsmooth, $'
;            errclr=errclr, $'
;            leg_charsize=leg_charsize, $
;            label_charsize=label_charsize
;            _extra=_extra'
;
;
; INPUTS:
;   Either 
;      *plateid, fiberid: identifier for SDSS plate number and the fiber number
;                         of the object in the plate. These can be arrays.
;   Or
;      *struct: The spectra structure for this object.
;   PLOT_SPEC1D will figure out what the user wants based on the inputs.
;
; KEYWORD PARAMETERS:
;   /noprompt: if plateid and fiberid sent as arrays, then the user will be
;              prompted to move to the next/previous object in 'X' device. This
;              behaviour can be turned off with /noprompt.
;   /nolegend: don't plot the legend, with the type, redshift, and ra/dec 
;   /nolines: don't plot and label the lines
;
; OPTIONAL INPUTS:
;   mjd=mjd: if plateid/fiberid are entered, then PLOT_SPEC1D will find the
;            latest mjd unless this input is given.  Must be the same size as
;            the plateid and fiberid inputs.
;   spec_vers: a string containing the spectro rerun id.  e.g. '1d_23'.  The
;              default is to read this from the !SDSS_SPEC_VERS variable.
;   nsmooth=nsmooth: boxcar smooth over this many adjacent pixels
;   errclr=errclr: color for the error curve. Default is green in X,Z and
;                  blue in PS
;   nsmooth=nsmooth: Number of neighbors to use in a boxcar average smoothing.
;   errclr=errclr: color for the noise curve.
;   leg_charsize=leg_charsize: the charsize for the legend
;   label_charsize=label_charsize: the charsize for the line labels.
;
;   _extra=_extra: plotting keywords
;
; OUTPUTS:
;   A plot is made on the screen.
;
; RESTRICTIONS:
;  You need the Goddard idl astronomy procedures and the Umich SDSS idl
;  libraries.  You need the !SDSS_SPEC_DIR variable set (see sdssidl_setup.pro)
;
;
; EXAMPLE:
;  plot_spec1d,550,125,nsmooth=10
;  plates=[550,655]
;  fibers=[125,36]
;  plot_spec1d,plates,fibers,nsmooth=10
;
;
; MODIFICATION HISTORY:
;  Creation: 15-Aug-2003: Erin Sheldon UofChicago
;
;-


PRO plot_spec1d_syntax

  print,'By id: '
  print,'   -Syntax: plot_spec1d, plateid, fiberid, $'
  print,'        /noprompt, $'
  print,'        /nolegend, $'
  print,'        /nolines,  $'
  print,'        mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $'
  print,'        errclr=errclr, $'
  print,'        leg_charsize=leg_charsize, $'
  print,'        label_charsize=label_charsize, $'
  print,'        _extra=_extra'
  print,'From structure: '
  print,'   -Syntax: plot_spec1d, struct, $'
  print,'        /nolegend, $'
  print,'        /nolines,  $'
  print,'        mjd=mjd, nsmooth=nsmooth, $'
  print,'        errclr=errclr, $'
  print,'        leg_charsize=leg_charsize, $'
  print,'        label_charsize=label_charsize, $'
  print,'        _extra=_extra'

END 

PRO plot_spec1d, plateid, fiberid, $
                 noprompt=noprompt, $
                 nolegend=nolegend, $
                 nolines=nolines, $
                 mjd=mjd, spec_vers=spec_vers, nsmooth=nsmooth, $
                 yrange=yrange, xrange=xrange, $
                 xstyle=xstyle, ystyle=ystyle, $
                 position=position, xminor=xminor, thick=thick,$
                 charsize=charsize, $
                 leg_charsize=leg_charsize, $
                 label_charsize=label_charsize, $
                 errclr=errclr, $
                 _extra=_extra

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  On_error,2

  np = n_params()
  IF np LT 1 THEN BEGIN 
      plot_spec1d_syntax
      return
  ENDIF 

  IF !d.name NE 'X' THEN noprompt=1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If the struct is input, then go ahead and plot.  Othersize, loop over the
  ;; input id's
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  dt = datatype(plateid) 
  IF dt NE 'STC' THEN BEGIN 
      IF n_params() LT 2 THEN BEGIN 
          plot_spec1d_syntax
          return
      ENDIF 

      ;; More parameter checking
      nplate = n_elements(plateid)
      nfiber = n_elements(fiberid)
      nmjd = n_elements(mjd)
      IF nplate NE nfiber THEN $
        message,'plateid must be same size as fiberid'
      IF (nmjd NE 0) AND (nmjd NE nfiber) THEN $
        message,'mjd must be same size as fiberid'

      IF n_elements(xrange) NE 0 THEN sxrange=1 ELSE sxrange=0
      IF n_elements(yrange) NE 0 THEN syrange=1 ELSE syrange=0
      IF n_elements(position) NE 0 THEN spos=1 ELSE spos=0

      ;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Loop over the ids
      ;;;;;;;;;;;;;;;;;;;;;;;;;

      i=0L
      iold = -1
      WHILE i LT nplate DO BEGIN 

          IF i NE iold THEN BEGIN 

              IF n_elements(mjd) NE 0 THEN tmjd = mjd[i]

              ;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; call read_spec1d
              ;;;;;;;;;;;;;;;;;;;;;;;;;

              read_spec1d, plateid[i], fiberid[i], /plot, $
                           mjd=tmjd, spec_vers=spec_vers, nsmooth=nsmooth, $
                           yrange=yrange, xrange=xrange, $
                           xstyle=xstyle, ystyle=ystyle, $
                           position=position, xminor=xminor, thick=thick,$
                           charsize=charsize, $
                           leg_charsize=leg_charsize, $
                           label_charsize=label_charsize, $
                           errclr=errclr, $
                           nolegend=nolegend, $
                           nolines=nolines, $
                           _extra=_extra

              IF nplate GT 1 THEN BEGIN 
                  ;; these will erroneously be saved between plots if we don't 
                  ;; delete them
                  IF NOT sxrange THEN delvarx,xrange
                  IF NOT syrange THEN delvarx,yrange
                  IF NOT spos    THEN delvarx,position
                  delvarx,tmjd
              ENDIF 

          ENDIF 
          iold = i

          ;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; prompt the user
          ;;;;;;;;;;;;;;;;;;;;;;;;;

          IF (nplate NE 1) AND (NOT keyword_set(noprompt)) THEN BEGIN 
              print,'(any key): next  (p): previous  (q): quit'
              key=get_kbrd(1)
              
              CASE strlowcase(key) OF
                  'p': i = (i-1) > 0
                  'q': return
                  ELSE: i = i+1 
              ENDCASE 
              
          ENDIF ELSE i=i+1

      ENDWHILE 

      return
  ENDIF 

  struct = plateid

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Should we smooth the spectra?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(nsmooth) NE 0 THEN BEGIN 
      nsmoothstr = 'Nsmooth: '+ntostr(long(nsmooth))
      spec = smooth(struct.spec, nsmooth)
      noise = smooth(struct.spec_err, nsmooth)
  ENDIF ELSE BEGIN 
      nsmoothstr = ''
      spec = struct.spec
      noise = struct.spec_err
  ENDELSE 

  ;; this is dave's program: spec_type.pro
  objtype = spec_type(struct.spec_cln)      

  ;;;;;;;;;;;;;;;;;;;
  ;; object id
  ;;;;;;;;;;;;;;;;;;;

  objid = strcompress(struct.objid)
  
  obj=str_sep(objid,' ')
  obj=obj(1:*)
  obj[0] = run2string(obj[0])
  obj[3] = field2string(obj[3])
  obj=strjoin(obj,'-')

  ;;;;;;;;;;;;;;;;;;;
  ;; Redshift string
  ;;;;;;;;;;;;;;;;;;;

  IF rnd(struct.z,4) LT 0 THEN znum = 7 ELSE znum = 6
  zstr = ntostr(struct.z,znum,/round)
  zerr_str = ntostr(struct.z_err,6,/round)

  zstr = 'z = '+zstr + !csym.plusminus+zerr_str

;  spid=strcompress(strn(struct.plateid,len=4,padchar='0')+'-'+$
;                   strn(struct.mjd)+'-'+$
;                   strn(struct.fiberid))

  spid=$
    strn(struct.mjd,     len=5, padchar='0')+'-'+$
    strn(struct.plateid, len=4, padchar='0')+'-'+$
    strn(struct.fiberid, len=3, padchar='0')


  angstrom = !csym.angstrom

  ;;;;;;;;;;;;;;;;;;;
  ;; RA/DEC string
  ;;;;;;;;;;;;;;;;;;;

  rnum = 7
  IF struct.raobj GE 10 THEN rnum = 8
  IF struct.raobj GE 100 THEN rnum = 9
  
  dnum = 7
  IF struct.decobj GE 10 THEN dnum = 8
  IF struct.decobj LT 0 THEN dnum = 8
  IF struct.decobj LE -10 THEN dum = 9

  rdstr = 'RA: '+ntostr(struct.raobj,rnum,/round)+$
    '  DEC: '+ntostr(struct.decobj,dnum,/round)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set up the titles
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  tit='specID: '+spid+'   ObjID: '+obj;+'  '+rdstr
  xtit = 'Wavelength ['+angstrom+']'

  minus = !csym.minus
  ytit = 'F!D'+!csym.lambda+'!N [10!U'+minus+'17!N erg cm!U'+minus+$
    '2!N s!U'+minus+'1!N '+angstrom+'!U'+minus+'1!N]'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Plot formating. There are defaults, but otherwise
  ;; it is left to the user
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF !d.name EQ 'X' THEN BEGIN 
      ;; if we don't do this, then the aspect program fails
      ;; to properly format 
      IF !d.window EQ -1 THEN window
  ENDIF 

  IF n_elements(yrange) EQ 0 THEN BEGIN 
      yrange=[-2,max(spec)*1.1]
  ENDIF
  IF n_elements(xrange) EQ 0 THEN BEGIN 
      xrange = [min(struct.lambda), max(struct.lambda)]
  ENDIF 
  IF n_elements(xstyle) EQ 0 THEN xstyle=1
  IF n_elements(ystyle) EQ 0 THEN ystyle=1+2
  IF n_elements(position) EQ 0 THEN position = aspect(1./!gratio)
  IF n_elements(xminor) EQ 0 THEN xminor=5
  IF n_elements(thick) EQ 0 THEN thick=1
  IF n_elements(charsize) EQ 0 THEN BEGIN
      charsize = !p.charsize < 1.0
  ENDIF 
  IF n_elements(errclr) EQ 0 THEN BEGIN 
      IF !d.name EQ 'PS' THEN BEGIN 
          errclr = !blue
      ENDIF ELSE BEGIN 
          errclr = !green
      ENDELSE 
  ENDIF 

  IF n_elements(leg_charsize) EQ 0 THEN BEGIN 
      IF !d.name EQ 'Z' THEN leg_charsize = 1.0 ELSE leg_charsize = 1.0
  ENDIF 

  plot, struct.lambda, spec, yrange=yrange, xrange=xrange, $
        xstyle=xstyle, ystyle=ystyle, $
        title=tit,xtitle=xtit,ytitle=ytit, xminor=xminor, position=position,$
        thick=thick, charsize=charsize, $
        _extra=_extra
  oplot, struct.lambda, noise, color=errclr, thick=thick

  IF NOT keyword_set(nolegend) THEN BEGIN 
      ;;legend,[objtype,zstr],/bottom,box=0,charsize=leg_charsize
      legend,objtype+'  '+zstr,/bottom,box=0,charsize=leg_charsize
      legend,rdstr,/bottom,/right,charsize=leg_charsize,box=0
  ENDIF 

  ;; Do the lines
  IF tag_exist(struct, 'lines_redshift') AND NOT keyword_set(nolines) THEN BEGIN 

      IF datatype(struct.lines_redshift) EQ 'STC' THEN BEGIN 

          reg_color = !p.color
          qso_em_color = !red
          both_em_color = !green
          IF !d.name EQ 'PS' THEN BEGIN 
              IF n_elements(label_charsize) EQ 0 THEN BEGIN 
                  label_charsize=0.7*charsize
              ENDIF 
              shift = 40 
              gal_em_color = !blue

              line_thick = 3
          ENDIF ELSE BEGIN 
              shift = 25
              IF n_elements(label_charsize) EQ 0 THEN BEGIN 
                  label_charsize = charsize
              ENDIF 
              IF !d.n_colors GT 255 THEN gal_em_color = !DodgerBlue $
              ELSE gal_em_color = !blue

              line_thick = 1
          ENDELSE 

          lines = struct.lines_redshift.restwave
          wave = struct.lines_redshift.wave
          
          names = specline_name(lines,$
                                type=type,pos=ypos)

          nline = n_elements(lines)

          color = lonarr(nline)
          w=where(type EQ 'reg',nw)
          IF nw NE 0 THEN color[w] = reg_color
          w=where(type EQ 'qso_em',nw)
          IF nw NE 0 THEN color[w] = qso_em_color
          w=where(type EQ 'gal_em',nw)
          IF nw NE 0 THEN color[w] = gal_em_color
          w=where(type EQ 'both_em',nw)
          IF nw NE 0 THEN color[w] = both_em_color
          
          FOR i=0L, nline-1 DO BEGIN 
              IF names[i] NE '' THEN BEGIN 
                  
                  ;; convert position to data coordinates
                  ;; x is plotted in the observed frame

                  x = wave[i]
                  y = !y.crange[0] + (!y.crange[1]-!y.crange[0])*ypos[i]
                  
                  oplot, [x, x], [0, 1.e6], line=1,color=color[i], $
                    thick=line_thick
                  xyouts, x-shift, y, names[i],charsize=label_charsize
              ENDIF 
          ENDFOR 
      ENDIF 

  ENDIF 
  
END 
