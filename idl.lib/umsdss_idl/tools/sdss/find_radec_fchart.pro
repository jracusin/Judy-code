;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;       FIND_RADEC_FCHART
;
; PURPOSE:
;	Create a finding chart for the input coordinates.
;
; CALLING SEQUENCE:
;       find_radec_fchart, ra, dec, fchart, pstruct, $
;                      clr=clr, $
;                      radius=radius, radarcsec=radarcsec, radarcmin=radarcmin, $
;                      maxsize=maxsize, tol=tol, $
;                      nonoise=nonoise, $
;                      nocirc=nocirc, $
;                      circ_rad=circ_rad,$
;                      circ_color=circ_color,$
;                      nodisplay=nodisplay, psfile=psfile, $
;                      silent=silent, $
;                      photoid=photoid, $
;                      objx=objx, objy=objy, $
;                      linestyle=linestyle,$
;                      _extra=extra, runuse=runuse, astrans=astrans
;
; INPUTS: 
;       ra, dec:   position of interest in degrees. If is an array, then
;                  search is based on first element only.
;
; OPTIONAL INPUTS:
;       clr:       bandpass of images from which to create finding chart.
;                  Must be an integer [r,g,u,i,z] -> [0,1,2,3,4]
;                  Default is red (2)
;       radius:    Half length of the square finding chart in pixels.
;       radarcsec: radius in arcseconds (takes precedence over radius)
;       radarcmin: radius in arcminutes (takes precedence over radarcsec)
;       maxsize:   maximum size for atlas images; sent to atlas images.  If 
;                  your images are being clipped off, you should increase
;                  maxsize. default = [500,500]
;       tol:       Tolerance for finding nearby object in arcseconds.  
;                  Default is 100.
;       cird_rad:  array of radii used to make circle around objects in 
;                    the list.  Default is size of finding chart/10.0 for the
;                    first and 1/20. for the rest
;         circ_color: colors to use for circles. Default is all white or
;                     black depending on device
;       psfile: make ps file with this name
;       linestyle: linestyles for circles. Default is all 0 (solid line)
;       runuse: find_radec may return more than one run. Set this value
;            to an integer to choose which run.
;       _extra:    Extra plotting keywords.
;
; KEYWORD PARAMETERS:
;       /nodisplay: don't display, just make fchart and return
;       /nonoise:   Set for no noise in fchart.
;       /nocirc: don't draw circles
;       /astrans: use direct astrans method
;
; OPTIONAL OUTPUTS: 
;       fchart:    The finding chart
;       pstruct:   A photo structure containing all objects in the frame 
;                  of ra,dec as well as the two frames before and after (if 
;                  its the first or last frame then it uses next two or 
;                  previous two.
;       altdir: atlas directory used to make fchart
;       photoid:     Returns Id of nearest object.
;       objx,objy: x,y positon of object used to make fchart
;
; CALLED ROUTINES:
;
;       FIND_RADEC 
;       TSOBJ_NAME
;            RUN_DIR
;       READ_PHOTO_COL
;       FCHART_CIRC_RADEC
;            CIRC_RADEC
;       (SAO)
;
; PROCEDURE: 
;	
;	Use find_radec to get the run, camcol, and field of an object that
;       is nearby to ra,dec and then read in the nearby objects.  Then create 
;       a finding chart and circle the ra,dec postion.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofMich 10/15/99  
;       Added radarcsec and radarcmin keyword parameters. 02-Jul-2002
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO find_radec_fchart, ra, dec, fchart, pstruct, useind=useind, $
                       clr=clr, $
                       shift2r=shift2r, $
                       $
                       radius=radius, $
                       radarcsec=radarcsec, $
                       radarcmin=radarcmin, $
                       $
                       tol=tol, $
                       runuse=runuse, astrans=astrans, $
                       photoid=photoid, $
                       $
                       maxsize=maxsize, $
                       $
                       nonoise=nonoise, $
                       $
                       directions=directions, $
                       circra=circra, $
                       circdec=circdec,$
                       circ_rad=circ_rad,$
                       circ_color=circ_color,$
                       linestyle=linestyle,$
                       circobj=circobj, $
                       nocirc=nocirc, $
                       $
                       nodisplay=nodisplay, $
                       order=order, $
                       psfile=psfile, $
                       silent=silent, $
                       objx=objx, objy=objy, $
                       _extra=extra


  IF N_params() EQ 0 THEN BEGIN 
      print,'-Syntax: find_radec_fchartra, dec, fchart, pstruct, $'
      print,'          useind=useind, $'
      print,'          clr=clr, $'
      print,'          shift2r=shift2r, $'
      print,'          $'
      print,'          radius=radius, $'
      print,'          radarcsec=radarcsec, $'
      print,'          radarcmin=radarcmin, $'
      print,'          $'
      print,'          tol=tol, $'
      print,'          runuse=runuse, astrans=astrans, $'
      print,'          photoid=photoid, $'
      print,'          $'
      print,'          maxsize=maxsize, $'
      print,'          $'
      print,'          nonoise=nonoise, $'
      print,'          $'
      print,'          directions=directions, $'
      print,'          circra=circra, $'
      print,'          circdec=circdec,$'
      print,'          circ_rad=circ_rad,$'
      print,'          circ_color=circ_color,$'
      print,'          linestyle=linestyle,$'
      print,'          circobj=circobj, $'
      print,'          nocirc=nocirc, $'
      print,'          $'
      print,'          nodisplay=nodisplay, $'
      print,'          order=order, $'
      print,'          psfile=psfile, $'
      print,'          silent=silent, $'
      print,'          objx=objx, objy=objy, $'
      print,'          _extra=extra'
      print,''
      print,'Use doc_library,"find_radec_fchart"  for more help.'  
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check input parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(clr) EQ 0 THEN clr=2

  IF n_elements(radius) EQ 0 THEN radius = 300
  IF n_elements(radarcsec) NE 0 THEN radius = radarcsec/0.4 ;pixels
  IF n_elements(radarcmin) NE 0 THEN radius = radarcmin*60.0/0.4 ;pixels
  IF n_elements(tol) EQ 0 THEN tolerance = 100/3600. ELSE tolerance=tol*3600.
  IF n_elements(maxsize) EQ 0 THEN maxsize = [500,500]

  nra = n_elements(ra)
  ndec = n_elements(dec)
  IF (nra NE ndec) THEN BEGIN
    print,'ra and dec should be same size'
    return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Additional ra/dec to circle?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ra2circle = ra
  dec2circle = dec

  ncircra = n_elements(circra)
  ncircdec = n_elements(circdec)
  
  IF ncircra NE ncircdec THEN BEGIN 
      message,'inputs circra and circdec must be equal length'
  ENDIF 

  IF ncircra NE 0 THEN BEGIN 
      ra2circle = [ra2circle, circra]
      dec2circle = [dec2circle, circdec]

      nra = nra + ncircra
      ndec = ndec + ncircdec
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Look for the ra/dec
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, fchart
  find_radec, ra[0], dec[0], run, camcol, field, silent=silent, astrans=astrans
  IF run[0] EQ -1 THEN return

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Look for runs with tsObj and fpAtlas files 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  make_runstatus_struct,rs
  rs.tsObj_exist = 'Y'
  rs.fpAtlas_exist = 'Y'
  runstatus_select, rs, wr
  IF wr[0] EQ -1 THEN BEGIN
      print,'  * No runs found with tsObj/fpAtlas'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now see if any of the runs containing ra/dec have both
  ;; tsObj and fpAtlas
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(runuse) EQ 0 THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; get a run that we can use
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      match, [!RUN_STATUS[wr].run], [run], mst, mrun
      IF mst[0] EQ -1 THEN BEGIN
          print,'  * No matching runs found with tsObj/fpAtlas'
          return
      ENDIF 
      wr = wr[mst[0]]
      runuse = mrun[0]
  ENDIF ELSE BEGIN 
      wr2=where(!RUN_STATUS[wr].run EQ run[runuse],nwr)
      IF nwr EQ 0 THEN BEGIN
          print,'  * Input runuse not matched after run_status cuts'
          return
      ENDIF 
      wr = wr[wr2]
  ENDELSE 

  wr2 = where(!RUN_STATUS[wr].rerun EQ max(!RUN_STATUS[wr].rerun),nwr)  
  wr=wr[wr2]
  rerun = !RUN_STATUS[wr].rerun


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read in the fields surrounding matching field
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  colid = [ run[runuse], rerun, camcol[runuse] ]
  read_tsobj, colid, pstruct, start=field[runuse]-1, nframes=3

  ;; anything found?
  IF n_elements(pstruct) EQ 0 THEN return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make the finding chart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Find nearest object which we will use to make finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT silent THEN print
  IF (n_elements(photoid) EQ 0) THEN BEGIN 
      IF NOT silent THEN print,'Getting photoid of closest object'
      IF NOT keyword_set(tol) THEN tol = 500/3600. ;100 arcseconds
      allow = 1
      close_match_radec, ra[0], dec[0], pstruct.ra, pstruct.dec, $
        match1, photoid, tol, allow, /silent
  ENDIF

  IF photoid[0] EQ -1 THEN BEGIN
      print,'No matches found'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make fchart for photoid and display it
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF n_elements(psfile) NE 0 THEN BEGIN
      begplot,name=psfile
  ENDIF 

  IF NOT silent THEN print,'Making the fchart'
  fchart, pstruct, photoid, radius, clr, fchart, maxsize=maxsize, $
    shift2r=shift2r, $
    objx=objx, objy=objy, silent=silent, nonoise=nonoise, $
    useind=useind

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; No need to continue of no display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(nodisplay) THEN return 

  IF !d.name EQ 'PS' THEN BEGIN 
      pcharold = !p.charsize
      !p.charsize=1.0
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Display the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  display_fchart, fchart, pstruct[photoid],objx,objy,clr,/nocirc,$
    silent=silent, nodisplay=nodisplay, order=order, directions=directions,$
    _extra=extra

  IF !d.name EQ 'PS' THEN !p.charsize=pcharold

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; No need to continue of not circling
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(nocirc) THEN return

  s=size(fchart)
  ss = min([s[1],s[2]])

  IF n_elements(circ_rad) NE 0 THEN BEGIN 
      ncircrad = n_elements(circ_rad)
      IF ncircrad NE nra THEN $
        message,'circ_rad must be same size as all ra/dec to circle'$
      ELSE circ_radius = circ_rad
  ENDIF ELSE BEGIN 
      IF nra EQ 1 THEN BEGIN 
          circ_radius = ss/10.0 
      ENDIF ELSE BEGIN 
          circ_radius = [ss/10.0, replicate(ss/20.0,nra-1)]
      ENDELSE 
  ENDELSE 

  IF NOT silent THEN BEGIN 
    print,'' 
    print,strtrim(string(nra),2)+' positions to be circled'
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; circle the ra's and dec's with different colors, sizes, lines
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(pos_angle) NE 0 THEN BEGIN 

      IF n_elements(rmax) NE nra THEN rmax_send=replicate(rmax,nra) $
      ELSE rmax_send=rmax
      IF n_elements(rmin) NE nra THEN rmin_send=replicate(rmin,nra) $
      ELSE rmin_send=rmin
      IF n_elements(pos_ang) NE nra THEN pos_ang_send=replicate(pos_ang,nra) $
      ELSE pos_ang_send=pos_ang

  ENDIF 

  nc=n_elements(circ_color)
  IF nc NE 0 THEN BEGIN 
      IF nc NE nra THEN $
        message,'circ_color array must be same size as ra/dec arrays' $
      ELSE clruse = circ_color
  ENDIF ELSE  BEGIN
      simpctable
      IF !d.name EQ 'X' THEN clruse=replicate(!white ,nra) $
      ELSE clruse=replicate(!p.color,nra)
  ENDELSE 

  nl=n_elements(linestyle)
  IF nl NE 0 THEN BEGIN 
      IF nl NE nra THEN $
        message,'linestyle must be same size as ra/dec arrays' $
      ELSE linest=linestyle
  ENDIF ELSE linest=replicate(0, nra)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Place the circles
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  FOR i=0, nra-1 DO BEGIN

      IF n_elements(pos_ang_send) NE 0 THEN possend=pos_ang_send[i]
      IF n_elements(rmax_send) NE 0 THEN rmxsnd=rmax_send[i]
      IF n_elements(rmin_send) NE 0 THEN rmnsnd=rmin_send[i]

      circ_radec, $
        pstruct, photoid, $
        objx, objy, ra2circle[i], dec2circle[i], circ_radius[i],$
        color=clruse[i], $
        clr=clr,silent=silent, linestyle=linest[i], $
        pos_ang=possend, rmax=rmxsnd, rmin=rmnsnd, order=order, ny=s[2]
      
  ENDFOR

  IF n_elements(psfile) NE 0 THEN BEGIN
      endplot,/noprint
      !p.charsize = pcharold
  ENDIF 
  IF NOT silent THEN print


return

  fchart_circ_radec, pstruct, ra, dec, fchart, $
    useind=useind, maxsize=maxsize, $
    shift2r=shift2r,$
    clr=clr, radius=radius, tol=tolerance, $
    nonoise=nonoise, $
    nodisplay=nodisplay, psfile=psfile, $
    circ_rad=circ_rad,$
    circ_color=circ_color,$
    linestyle=linestyle,$
    silent=silent, $
    photoid=objid, $
    objx=objx, objy=objy, $
    nocirc=nocirc, $
    _extra=extra

  photoid = objid

  return 
END 
