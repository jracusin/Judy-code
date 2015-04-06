
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    RGBFCHART
;       
; PURPOSE:
;    Create a color finding chart for the input ra/dec position
;    Calls FIND_RADEC_FCHART to create an r-band fchart, then constructs
;    the g and i fcharts.  Then RGBVIEW is called to create and display
;    the color image.  Various images can be written or the postscript
;    device can be opened before calling RGBFCHART
;
; CALLING SEQUENCE:
; THERE ARE TOO WAYS TO CALL RGBFCHART:
;    rgbfchart, ra, dec, $ OR
;               run, rerun, camcol, field, id, 
;               $                ; How big is the image?
;               radius=radius, $
;               radarcsec=radarcsec,$
;               radarcmin=radarcmin,$
;               $
;               runuse=runuse, $ ; sometimes don't want to use first run found
;               maxsize=maxsize, $ ; how large are default atlas images
;               $                ; for drawing the circles
;               circra=circra,$ ; additional positions to be circled
;               circdec=circdec,$
;               circ_rad=circ_rad,$
;               circ_color=circ_color,$
;               linestyle=linestyle,$
;               nocirc=nocirc, $
;               $                ; display parameters for rgbview
;               nonlinearity=nonlinearity, $
;               alpha=alpha, $
;               low_cut=low_cut, $
;               sdss=sdss, $
;               addu=addu,$
;               prompt=prompt, $
;               $                ; For writing images
;               jpegfile=jpegfile, $
;               pngfile=pngfile, $
;               expand=expand, $
;               jpegfchart=jpegfchart, $
;               pngfchart=pngfchart,$
;               $                ; These can be returned
;               fg=fg, fr=fr, fi=fi, $
;               struct=struct, $; struct for fields used
;               photoid=photoid, $ ; index of closest object
;               useind=useind, $
;               imtot=imtot, $
;               $                ; Style for X or Z display
;               addtitle=addtitle, title=title, xtitle=xtitle, $
;               nodisplay=nodisplay, $
;               rmap=rmap, gmap=gmap, bmap=bmap, $ ; 8-bit color maps
;               astrans=astrans,$ ; Use the astrans to find radec?
;               status=status, $ ; exit status
;
;         FOR COMPATABILITY ONLY
;               saturation=saturation,contrast=contrast, gamma=gamma,$
;               giffile=giffile, giffchart=giffchart, clip=clip
;
; INPUTS: 
;    ra/dec: coordinates in degrees.  If an array, then the search is done
;            on the first.  The other objects may be circled if /nocirc
;            is not set.  Additional positions to be circled may also be sent
;            through the circra=circra and circdec=circdec keywords
;
; OPTIONAL INPUTS:
;    radius:  Half length of the square finding chart in pixels.
;    radarcsec:  radius in arcseconds (takes precedence over radius)
;    radarcmin:  radius in arcminutes (takes precedence over radarcsec)
;
;    runuse: find_radec may return more than one run. Set this value
;            to an integer to choose which run.
;    maxsize:  maximum size for atlas images; sent to atlas images.  If 
;                  your images are being clipped off, you should increase
;                  maxsize. default = [500,500]
;
;    circ_rad:  array of radii used to make circle around objects in 
;               the list.  Default is size of finding chart/10.0 for the
;               first and 1/20. for the rest
;    circ_color:
;    linestyle:
;
;    nonlinearity: scaled image = asihn(nonlinearity*alpha*(im-sky))/nonlin
;    alpha:
;    low_cut: Lowest value in image to use
;
;    jpegffchart:  Name of jpeg file to write a finding chart. 
;                  If sent write_jpeg will be called and the image will be 
;                  written from the X-window.
;    pngfchart: png finding chart file.
;    jpegfile: File to write full-res jpeg to.
;    pngfile:  same for png
;    expand: factor by which to expand image for jpegfile, pngfile
;    addtitle: Additional title to tack on
;    title: Title
;    xtitle: xtitle
;
; KEYWORD PARAMETERS:
;    /clip: Use SIGMA_CLIP program to figure out mean and stdev of image. If
;           not set, then standard values for SDSS images are used (sky of
;           finding charts is always 1000.)
;    /nocirc: Don't circle ra/dec position
;    /sdss:   Use SDSS parameters to rescale counts to energy.
;    /addu:  add the u-band to the g image
;    /prompt: Use a prompt in rgbview to interactively change display
;             parameters.
;    /nodisplay: Don't display
;    /astrans: use astrans method to find radec
;
; OPTIONAL OUTPUTS:
;    fg, fr, fi: g,r,i finding charts
;    struct: the PHOTO structure read in to make finding chart
;    photoid: index into struct of nearest object to ra/dec
;    imtot: the bytescaled total rgb image
;    rmap=rmap, gmap=gmap, bmap=bmap: 
;          If on 8-bit display, one can set these keywords to 
;          a  named variable which will be set to the color map
;    status: if status ne 0 then something went wrong
;
; CALLED ROUTINES:
;    FIND_RADEC_FCHART (calls many programs)
;    FCHART
;    RGBVIEW
;    CIRC_RADEC
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Created ??-??-2001.  Documentation added 3-Jul-2002
;              Erin S. Sheldon UofMich
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO rgbfchart_circ, fr, ra, dec, struct, photoid, objx, objy, $
                    circ_rad=circ_rad, circ_color=circ_color, $
                    linestyle=linestyle, circleobj=circleobj, $
                    order=order

  nn = n_elements(ra)
  imsize = size(fr)
  ny = imsize[2]
  ss = imsize[1]

  IF n_elements(circ_rad) NE 0 THEN BEGIN 
      ncircrad = n_elements(circ_rad)
      IF ncircrad NE nn THEN message,'circ_rad must be same size as ra/dec'$
      ELSE circ_radius = circ_rad
  ENDIF ELSE circ_radius = [ss/10.0, replicate(ss/20.0,nn)]
  
  nc=n_elements(circ_color)
  IF nc NE 0 THEN BEGIN 
      IF nc NE nn THEN message,'color must be same size as ra/dec' $
      ELSE clruse = circ_color
  ENDIF ELSE  clruse=replicate(!white ,nn)
  
  nl=n_elements(linestyle)
  IF nl NE 0 THEN BEGIN 
      IF nl NE nn THEN message,'linestyle must be same size as ra/dec' $
      ELSE linest=linestyle
  ENDIF ELSE linest=replicate(0, nn)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; User input obj id.  This means the first ra/dec is
  ;; actually this one, so we shouldn't circle it twice
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(circleobj) THEN BEGIN 
      
      circx = (objx-0.5)
      
      IF keyword_set(order) THEN BEGIN 
;          circy = (ny-1) - circy
          circy = (ny-1) - objy - 0.5
      ENDIF ELSE BEGIN 
          circy = (objy-0.5)
      ENDELSE 

      ;; We shifted the axes by 0.5 for display purposes
      tvcircle, circ_radius[0], circx, circy, /data, $
        color=clruse[0],linestyle=linestyle[0]
      beg = 1L
      
  ENDIF ELSE beg = 0L
  
  FOR ic=beg, nn-1 DO BEGIN 
      
      circ_radec, $
        struct, photoid, objx, objy, ra[ic],dec[ic], circ_radius[ic],$
        clr=2,$
        color=clruse[ic], linestyle=linest[ic], order=order, ny=ny, $
        xshift = -0.5, yshift = -0.5
      
  ENDFOR 


END 

PRO rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
                    axiscolor=axiscolor

  axis,xaxis=0,xticks=4,xtickn=xlabels, xtitle=xtitle
  axis,xaxis=1,xticks=4,xtickn=[' ',' ',' ',' ',' ']
  axis,yaxis=0,yticks=4,ytickn=ylabels, ytitle=ytitle
  axis,yaxis=1,yticks=4,ytickn=[' ',' ',' ',' ',' ']

END 

PRO rgbfchart_display, imtot, xlabels, ylabels, xtitle, ytitle, title,$
                       color_im=color_im, rmap=rmap, gmap=gmap, bmap=bmap,$
                       order=order

  fi = reform( imtot[2,*,*] )
  implot_setup, fi, xsize, ysize, px, py, xrng, yrng, /center
  
  IF !d.name EQ 'PS' THEN BEGIN 

      device,/color
      tvlct,indgen(256),indgen(256),indgen(256)
      
      pos = [px[0], py[0], px[1], py[1]]
      tv, imtot,true=1,px[0],py[0], xsize=xsize, ysize=ysize, /device,$
        order=order

  ENDIF ELSE BEGIN 

      IF !d.n_colors LE 255 THEN BEGIN ;; 8-bit display
          tv, congrid(color_im, xsize, ysize), px[0],py[0],$
            order=order
          tvlct, rmap, gmap, bmap
          pos = [px[0], py[0], px[1], py[1]]
      ENDIF ELSE BEGIN 
          tv, congrid(imtot, 3, xsize, ysize),true=1,px[0],py[0],$
            order=order
          pos = [px[0], py[0], px[1], py[1]]
      ENDELSE 

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Establish the data coordinates and 
  ;; place title
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  plot, [0,0], [0,0], xstyle=5, ystyle=5, $
    title=title, $
    xrange=xrng, yrange=yrng, position=pos,$
    /noerase, /device, /nodata

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Put labels on the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
    axiscolor=axiscolor


END 

PRO rgbfchart_zbuff, im, xlabels, ylabels, xtitle, ytitle, title, order=order

  implot_setup, im, xsize, ysize, px, py, xrng, yrng, /center
  
  tv, congrid(im, xsize, ysize),px[0],py[0], order=order
  pos = [px[0], py[0], px[1], py[1]]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Establish the data coordinates and 
  ;; place title
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  plot, [0,0], [0,0], xstyle=5, ystyle=5, $
    title=title, $
    xrange=xrng, yrange=yrng, position=pos,$
    /noerase, /device, /nodata

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Put labels on the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
    axiscolor=axiscolor

END 


PRO rgbfchart, ra, dec, camcol, field, id, $ ; ra,dec can actually be run/rerun
               $                ; How big is the image?
               radius=radius, $
               radarcsec=radarcsec,$
               radarcmin=radarcmin,$
               $
               runuse=runuse,   $ ; sometimes don't want to use first run found
               maxsize=maxsize, $ ; how large are default atlas images
               $                ; Extra positions for circling
               circra=circra,$
               circdec=circdec,$
               $                ; for drawing the circles
               circ_rad=circ_rad,$
               circ_color=circ_color,$
               linestyle=linestyle,$
               nocirc=nocirc, $
               $
               directions=directions, $
               $                ; display parameters for rgbview
               nonlinearity=nonlinearity, $
               alpha=alpha, $
               low_cut=low_cut, $
               sdss=sdss, $
               addu=addu,$
               prompt=prompt, $
               $                ; For writing images
               jpegfile=jpegfile, $
               pngfile=pngfile, $
               expand=expand, $
               jpegfchart=jpegfchart, $
               pngfchart=pngfchart,$
               zresolution=zresolution, $
               $                ; These can be returned
               fg=fg, fr=fr, fi=fi, $
               struct=struct, $ ; struct for fields used
               photoid=photoid, $ ; index of closest object
               objx=objx, objy=objy, $
               useind=useind, $
               imtot=imtot, $
               $                ; Style for X or Z display
               addtitle=addtitle, title=title, xtitle=xtitle, $
               order=order, $
               nodisplay=nodisplay, $
               rmap=rmap, gmap=gmap, bmap=bmap, $ ; 8-bit color maps
               astrans=astrans,$ ; Use the astrans to find radec?
               status=status, $ ; exit status
               maguse=maguse,$
               $                ; For compatability only
               saturation=saturation,contrast=contrast, gamma=gamma,$
               giffile=giffile, giffchart=giffchart, clip=clip

  status = 1
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: '
      print,'    rgbfchart, ra, dec --OR-- run,rerun,camcol,field,id, $';
      print,'               $       ; How big is the image?'
      print,'               radius=radius, $'
      print,'               radarcsec=radarcsec,$'
      print,'               radarcmin=radarcmin,$'
      print,'               $'
      print,'               runuse=runuse,   $ ; sometimes dont want to use first run found'
      print,'               maxsize=maxsize, $ ; how large are default atlas images'
      print,'               $                ; for drawing the circles'
      print,'               circ_rad=circ_rad,$'
      print,'               circ_color=circ_color,$'
      print,'               linestyle=linestyle,$'
      print,'               nocirc=nocirc, $'
      print,'               $                ; display parameters for rgbview'
      print,'               nonlinearity=nonlinearity, $'
      print,'               alpha=alpha, $'
      print,'               low_cut=low_cut, $'
      print,'               sdss=sdss, $'
      print,'               addu=addu,$'
      print,'               prompt=prompt, $'
      print,'               $                ; For writing images'
      print,'               jpegfile=jpegfile, $'
      print,'               pngfile=pngfile, $'
      print,'               expand=expand, $'
      print,'               jpegfchart=jpegfchart, $'
      print,'               pngfchart=pngfchart,$'
      print,'               $                ; These can be returned'
      print,'               fg=fg, fr=fr, fi=fi, $'
      print,'               struct=struct, $'
      print,'               useind=useind, $'
      print,'               imtot=imtot, $'
      print,'               $                ; Style for X or Z display'
      print,'               addtitle=addtitle, title=title, xtitle=xtitle, $'
      print,'               nodisplay=nodisplay, $'
      print,'               rmap=rmap, gmap=gmap, bmap=bmap, $ ; 8-bit color maps'
      print,'               astrans=astrans,$ ; Use the astrans to find radec?'
      print,'               status=status  ; exit status'
               
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  colors=['u','g','r','i','z'] 
  ;; time the result
  time = systime(1)

  ;; PS device?
  IF !d.name EQ 'PS' THEN BEGIN 
      !p.thick=1
      !x.thick=1
      !y.thick=1
      !p.charsize=1
      !p.charthick=1
  ENDIF 

  ;; true color display?
  IF !d.n_colors GT 256 THEN true_color = 1 ELSE true_color = 0

  IF n_elements(radarcsec) NE 0 THEN radius = radarcsec/0.4    ;pixels
  IF n_elements(radarcmin) NE 0 THEN radius = radarcmin*60.0/0.4    ;pixels
  IF n_elements(radius) EQ 0 THEN radius = 300 ; pixels

  ncircra = n_elements(circra)
  ncircdec = n_elements(circdec)
  
  IF ncircra NE ncircdec THEN BEGIN 
      message,'inputs circra and circdec must be equal length'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Will we write a jpeg or png file of the finding chart? 
  ;; Note this is different than just a jpeg or png write without
  ;; annotation
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(jpegfchart) NE 0 OR n_elements(pngfchart) NE 0 THEN BEGIN 
      write_fchart_image = 1
  ENDIF ELSE write_fchart_image=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If no display or not true color display, we will need to use
  ;; the stacked z buffer method to write the true-color finding
  ;; chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (  ((NOT true_color) AND write_fchart_image) OR $
        (keyword_set(nodisplay) AND write_fchart_image) ) THEN BEGIN
      stack_zbuff = 1
  ENDIF ELSE stack_zbuff = 0
  IF n_elements(z_resolution) EQ 0 THEN BEGIN 
      zres = [775,670] 
  ENDIF ELSE zres=z_resolution

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make r-band finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() EQ 5 THEN BEGIN 
      
      circleobj=1 
      
      run = ra
      rerun = dec
      read_tsobj, [run,rerun,camcol], struct, start=field-1, nf=3

      IF n_elements(struct) EQ 0 THEN return

      photoid = where(struct.field EQ field AND struct.id EQ id, nw)
      IF nw EQ 0 THEN BEGIN 
          print,'No such id in this run/rerun/camcol/field'

          delvarx, struct, photoid, objx, objy
          return
      ENDIF 
      
      IF NOT keyword_set(nocirc) THEN print,'Circling Objx,Objy'

      clr = 2
      fchart, struct, photoid, radius, clr, fr, maxsize=maxsize, $
        objx=objx, objy=objy

      ra2circle = struct[photoid].ra
      dec2circle = struct[photoid].dec

  ENDIF ELSE BEGIN 
      circleobj=0
      
      find_radec_fchart, ra[0], dec[0], fr, struct, useind=useind, $
        radius=radius, maxsize=maxsize, $
        clr=2, photoid=photoid, runuse=runuse, $
        objx=objx, objy=objy,/nocirc, /nodisplay, astrans=astrans

      IF n_elements(fr) EQ 0 THEN BEGIN 
          delvarx, struct, photoid, objx, objy
          return
      ENDIF 

      IF NOT keyword_set(nocirc) THEN print,'Circling RA,DEC'

      ra2circle = ra
      dec2circle = dec

  ENDELSE 

  IF ncircra NE 0 THEN BEGIN 
      ra2circle = [ra2circle, circra]
      dec2circle = [dec2circle, circdec]
  ENDIF 

  frsize = size(fr)
  nx = frsize[1]
  ny = frsize[2]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now make g and i finding charts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  clr = 1
  fchart, struct, photoid, radius, clr, fg, maxsize=maxsize, $
    /shift2r

  clr = 3
  fchart, struct, photoid, radius, clr, fi, maxsize=maxsize, $
    /shift2r

  ;; should we add in the u band?
  IF keyword_set(addu) THEN BEGIN 
      clr=0
      fchart, struct, photoid, radius, clr, fu, maxsize=maxsize
      fg = (fg + fu)
  ENDIF 

  IF keyword_set(directions) THEN BEGIN 
      angle = angle_rowcol2radec(struct[photoid].run, $
                                 struct[photoid].rerun, $
                                 struct[photoid].camcol, $
                                 struct[photoid].field, 2)
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parameters for the annotation of the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  tags = tag_names(struct[photoid])
  runstr = run2string(struct[photoid].run)

  fieldstr = field2string(struct[photoid].field)
  camcolstr = strtrim(string(struct[photoid].camcol),2)
  idstr = strtrim(string(struct[photoid].id), 2)

  ;; Set up titles
  wrer = where(tags EQ 'RERUN', nwrer)
  IF nwrer NE 0 THEN BEGIN
      rerunstr='-'+strtrim(string(struct[photoid].(wrer[0])), 2)
  ENDIF ELSE BEGIN
      rerunstr = ''
  ENDELSE 

  IF n_elements(addtitle) EQ 0 THEN addtitle=''
  tmptitle=runstr+rerunstr+'-'+camcolstr+'-'+fieldstr+'-'+idstr
  IF n_elements(title) EQ 0 THEN title=tmptitle

  IF (NOT keyword_set(hideradec) ) THEN BEGIN 
      IF n_params() EQ 5 THEN BEGIN 
          radecstr, struct[photoid].ra, struct[photoid].dec, rastr, decstr
          title = title + '  ' +rastr+' : ' + decstr
      ENDIF ELSE BEGIN 
          radecstr, ra[0], dec[0], rastr, decstr
          title = title + '  ' +rastr+' : ' + decstr
      ENDELSE 
  ENDIF 

  title = title+'   '+addtitle

  ytitle='Offset (arcminutes)'

  wmag = sdss_maguse(struct, maguse=maguse, silent=silent)

  IF n_elements(xtitle) EQ 0 AND wmag NE -1 THEN BEGIN 

      c=struct[photoid].(wmag)
      xtitle=''
      FOR kk=0, 4 DO BEGIN
          IF (xtitle NE '') THEN xtitle = xtitle+'  '
          mag = strmid( strtrim(string(c[kk]),2), 0, 5)
          xtitle = xtitle + colors[kk]+'='+mag
      ENDFOR

      addtit = ''
      FOR kk=0,3 DO BEGIN
          diff = strmid(strtrim(string(c[kk] - c[kk+1]),2), 0, 5)
          addtit = addtit + '  '+colors[kk]+'-'+colors[kk+1]+'='+diff
      ENDFOR
      xtitle = xtitle + addtit  

  ENDIF 

  ss = size(fr)
  sx = ss[1]*.4/60.0
  sy = ss[2]*.4/60.0

  ;;;; make x and y tick labels
  xlabels = [ntostr(-2*sx/4.0, 5), $
             ntostr(-sx/4.0, 5)  , $
             '0.0', $
             ntostr(sx/4.0,4), $
             ntostr(2*sx/4.0,4)]
  ylabels = xlabels

  
  isky = 0.
  rsky = 0.
  gsky = 0.

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; This will write to the device that is open with no frame
  ;; unless /nodisplay
  ;; Also to jpeg or png files with no annotation
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbview, fi, fr, fg, $
    order=order, $
    nonlinearity=nonlinearity, $
    alpha=alpha, $
    imtot=imtot, $
    rsky=isky, gsky=rsky, bsky=gsky, $
    /noframe, prompt=prompt, low_cut=low_cut, $
    rmap=rmap, gmap=gmap, bmap=bmap, expand=expand, $
    color_im=color_im,jpegfile=jpegfile, pngfile=pngfile, sdss=sdss, $
    /nodisplay

  ;; Under certain circumstances, we need to do a stacked zbuffer
  ;; to write the finding chart: no display or PS or 8-bit display, etc.
  IF stack_zbuff THEN BEGIN 

      devold = !d.name
      setupplot, 'Z'
      device, set_resolution=zres

      tf = reform(imtot[0,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, order=order

      IF NOT keyword_set(nocirc) THEN BEGIN 
          rgbfchart_circ, $
            fr, ra2circle, dec2circle, struct, photoid, objx, objy, $
            circ_rad=circ_rad, circ_color=circ_color, $
            linestyle=linestyle, circleobj = circleobj, order=order
      ENDIF 
      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 

      tmp = tvrd()
      tsz = size(tmp,/dim)
      imtot2 = bytarr(3, tsz[0], tsz[1])
      imtot2[0,*,*] = tmp

      tf = reform(imtot[1,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, order=order

      IF NOT keyword_set(nocirc) THEN BEGIN 
          rgbfchart_circ, $
            fr, ra2circle, dec2circle, struct, photoid, objx, objy, $
            circ_rad=circ_rad, circ_color=circ_color, $
            linestyle=linestyle, circleobj = circleobj, order=order
      ENDIF 
      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 


      tmp = tvrd()
      imtot2[1,*,*] = tmp

      tf = reform(imtot[2,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, order=order

      IF NOT keyword_set(nocirc) THEN BEGIN 
          rgbfchart_circ, $
            fr, ra2circle, dec2circle, struct, photoid, objx, objy, $
            circ_rad=circ_rad, circ_color=circ_color, $
            linestyle=linestyle, circleobj = circleobj, order=order
      ENDIF 
      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 

      tmp = tvrd()
      imtot2[2,*,*] = tmp

      IF n_elements(jpegfchart) NE 0 THEN BEGIN 
          print,'Writing JPEG fchart: ',jpegfchart
          write_jpeg, jpegfchart, imtot2, true=1, quality=75
      ENDIF 
      IF n_elements(pngfchart) NE 0 THEN BEGIN 
          print,'Writing PNG fchart: ',pngfchart
          IF float(!version.release) LT 5.4 THEN BEGIN 
                  
              ;; In old version png is written out flipped
              imtot2[0,*,*] = rotate(rotate(reform(imtot2[0,*,*]),1),4)
              imtot2[1,*,*] = rotate(rotate(reform(imtot2[1,*,*]),1),4)
              imtot2[2,*,*] = rotate(rotate(reform(imtot2[2,*,*]),1),4)

              write_png, pngfchart, imtot2
              
          ENDIF ELSE BEGIN 
              write_png, pngfchart, imtot2
          ENDELSE 
      ENDIF 
      setupplot, devold

  ENDIF 

  IF NOT keyword_set(nodisplay) THEN BEGIN 

      rgbfchart_display, imtot, xlabels, ylabels, xtitle, ytitle, title,$
        order=order
        
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Circling points on the finding chart
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF NOT keyword_set(nocirc) THEN BEGIN 
          IF n_params() EQ 5 THEN circleobj=1 ELSE circleobj=0
          rgbfchart_circ, $
            fr, ra2circle, dec2circle, struct, photoid, objx, objy, $
            circ_rad=circ_rad, circ_color=circ_color, $
            linestyle=linestyle, circleobj = circleobj, $
            order=order

      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Put North-East lines on finding chart
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF keyword_set(directions) THEN BEGIN 
          angle = $
            angle_rowcol2radec(struct[photoid].run, $
                               struct[photoid].rerun, $
                               struct[photoid].camcol, $
                               struct[photoid].field, 2)
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; output images of the finding chart
      ;; These will be read from the display
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(giffchart) NE 0 THEN BEGIN 
          print,'Writing GIF no longer supported in IDL'
      ENDIF 
      IF n_elements(jpegfchart) NE 0 AND NOT stack_zbuff THEN BEGIN 
          IF true_color THEN BEGIN 
              print,'Writing JPEG fchart: ',jpegfchart
              write_jpeg, jpegfchart, tvrd(true=1), true=1, quality=75
          ENDIF ELSE BEGIN 
              print,'Cannot write JPEG from 8-bit display: try PNG format'
          ENDELSE 
      ENDIF 
      IF n_elements(pngfchart) NE 0 AND NOT stack_zbuff THEN BEGIN 
          print,'Writing PNG fchart: ',pngfchart

          IF true_color THEN BEGIN 

              IF float(!version.release) LT 5.4 THEN BEGIN 
                  tmp = tvrd(true=1)
                  
                  ;; In old version png is written out flipped
                  tmp[0,*,*] = rotate(rotate(reform(tmp[0,*,*]),1),4)
                  tmp[1,*,*] = rotate(rotate(reform(tmp[1,*,*]),1),4)
                  tmp[2,*,*] = rotate(rotate(reform(tmp[2,*,*]),1),4)
                  
                  write_png, pngfchart, tmp
                  
              ENDIF ELSE BEGIN 
                  write_png, pngfchart, tvrd(true=1)
              ENDELSE 
          ENDIF ELSE BEGIN 
              IF float(!version.release) LT 5.4 THEN BEGIN 
                  tmp = tvrd()
                  tmp = rotate( rotate(tmp,1), 4)
                  write_png, pngfchart, tmp, rmap, gmap, bmap
              ENDIF ELSE BEGIN 
                  write_png, pngfchart, tvrd(), rmap, gmap, bmap
              ENDELSE 
          ENDELSE 
      ENDIF 
  ENDIF 

  ptime, systime(1)-time
  status=0

  return
END 
