FUNCTION angle_rowcol2radec, run, rerun, camcol, field, clr, $
                             transfile=transfile

  ;; for 756, increasing row -> increasing ra
  ;;          increasing col -> increasing dec
  ;; ra increases to the  East
  ;; dec increases to the North

  ;; find rotation between row, col and ra/dec
  read_astrans, run, rerun, camcol, clr, trans, node, inc, $
    transfile=transfile, /silent

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; astrometry structure
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  radeg = 180./!dpi
  val = .4d/3600.
  astr={cd: double(identity(2)),$
        cdelt: [val,val], $
        crpix: dblarr(2), $
        crval: [0., 0.]}
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define set of pixels to rotate
  ;; vertical is increasing row here
  ;; horizontal is increasing column
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  lrow = 1489-1
  lcol = 2048-1
  nnr=200.
  vert=dblarr(2,nnr)
  horz=dblarr(2,nnr)
  ccol = 1023.5
  crow = 744.0
  FOR i=0L, nnr-1 DO BEGIN
      vert[0,i] = ccol          ;columns
      vert[1,i] = lrow-i        ;rows
;      IF i EQ 10 THEN vert[1,i] = 1000
      horz[0,i] = lcol - i      ;columns
      horz[1,i] = crow          ;rows
  ENDFOR 
  
  ;; convert center (row,col) to (mu,nu) great circle coords
  rowcol2munu, trans, field, crow, ccol, cmu, cnu

  gc2eq, cmu, cnu, node, inc, cra, cdec

  ;; same for the vertical/horizontal points
  rowcol2munu, trans, field, vert[1,*], vert[0,*], vmu, vnu
  rowcol2munu, trans, field, horz[1,*], horz[0,*], hmu, hnu
  gc2eq, vmu, vnu, node, inc, vra, vdec
  gc2eq, hmu, hnu, node, inc, hra, hdec

  ;; tangent project ra/dec
  astr.crval = [cra, cdec]
  rd2xy,cra,cdec,astr,cxx,cyy
  rd2xy, vra, vdec, astr, vxx, vyy
  rd2xy, hra, hdec, astr, hxx, hyy

  ;; angle of points in (lambda,eta)
  radecaang = atan(vyy, vxx)
  radecbang = atan(hyy, hxx)

  mvert = mean(!dpi/2. - radecaang)
  mhorz = mean(0. - radecbang)
  angle = mean( [!dpi/2. - radecaang, 0. - radecbang] )

  ;; That is from between ra/dec and row/col
  ;; minus sign for rotation to ra/dec

  angle = -angle

  return,angle

  print,mvert, mhorz, angle, angle*180./!pi

  xx = [vxx, cxx, hxx]
  yy = [vyy, cyy, hyy]
  s = sort(xx)
  xx = xx[s]
  yy = yy[s]

  xtitle = 'DEC (tangent projected)'
  ytitle = 'RA (tangent projected)'
  plot, [vxx, cxx, hxx], [vyy, cyy, hyy], /iso, xtitle=xtitle, ytitle=ytitle
  s=sort(vxx)
  oplot, vxx[s], vyy[s], color=!red
  oplot, [cxx], [cyy], psym=8, color=!cyan
  s=sort(hxx)
  oplot, hxx[s], hyy[s], color=!green

  key=prompt_kbrd()
  

  ;; Now go to the pixel coordinate system and draw 
  ;; an arrow for east and north (ra, dec)

  ;; The arrow should be in the upper right and be 1/10 the
  ;; smaller side of the data area

  plot, [0], /nodata, xrange=[0,2048], yrange=[0,1450], /iso

  plot_ew_arrows, angle, fracsize=fracsize, offset_frac=offset_frac


return, angle

  frac = 0.1
  offset_frac = 0.5
  
  xsize = abs(!x.crange[1]-!x.crange[0])
  ysize = abs(!y.crange[1]-!y.crange[0])

  arrow_size = min([xsize, ysize])*frac

  origin = fltarr(2)
  origin[0] = (!x.crange[1] - arrow_size*(1.+offset_frac))
  origin[1] = (!y.crange[1] - arrow_size*(1.+offset_frac))
;stop
  ;; The arrows
;  origin = [1500, 1000]

;  length = 300
  ;; unrotated
  x = [ 0.0, 0.0, 1.0]*arrow_size
  y = [ 1.0, 0.0, 0.0]*arrow_size

  ;; rotate
  xp =  cos(-angle)*x + sin(-angle)*y
  yp = -sin(-angle)*x + cos(-angle)*y

  ;; translate
  xp = xp+origin[0]
  yp = yp+origin[1]


  
  oplot, xp, yp
  xyouts, xp[0]-arrow_size/7., yp[0], 'E'
  xyouts, xp[2], yp[2]-arrow_size/7., 'N'


END 
