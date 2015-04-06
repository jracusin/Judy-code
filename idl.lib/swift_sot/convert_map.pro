pro convert_map,map,glong,glat
  
;  nx=n_elements(map[*,0])
;  ny=n_elements(map[0,*])
;  n=[nx,ny]
;  nx=nx*10.
;  ny=ny*10.
;  glong=findgen(nx)/nx*360.
;  glong=reform(glong,nx,1)
;  glong=rebin(glong,nx,ny,/sample)
;  glat=findgen(ny)/ny*180.-90
;  glat = reform(glat,1,ny)
;  glat = rebin(glat,nx,ny,/sample)
  
;  tmp=glong
;  glong[0:nx/2.-1,*]=tmp[nx/2.:*,*]
;  glong[nx/2.:*,*]=tmp[0:nx/2.-1,*]
  
;  ra = glong                    ; use epoch 2000 internally
;  dec = glat
;  ra =  reform(ra,65160)
;  dec = reform(dec,65160)
;  precess, ra, dec, 2000.0, 1950.0 ; precess to 1950
                                ;   (required by euler)
;  ra = reform(ra,360,181)	
;  dec = reform(dec,360,181)
  
;  euler, ra, dec, glong, glat, 1 ; convert to galactic
;  ra = 0                        ; free up memory
;  dec = 0
  
;  aitoff,glong,glat,x,y
;  glong=x
;  glat=y
  
  xcenter=180.
;  nx=sxpar(hdr,'NAXIS1')
;  ny=sxpar(hdr,'NAXIS2')
;  map_viewer_lbpix,glong,glat,xcenter,nx,ny,ix,iy
  
;  image = rotate(map(ix,iy),5)

  
;  wcssph2xy,glong,glat,x,y,21,ctype=['GLON-CAR','GLAT-CAR']
;  map_viewer_lbpix,glong,glat,xcenter,n[0],n[1],ix,iy
;  image = rotate(map(ix,iy),5)
  

;  !p.multi=[0,1,2]
;  rdis,map
;  rdis,image
;  tv,image
;  !p.multi=0
  
;  sxaddpar,hdr, name,value
;  new_image = map_image(hist_equal(image), x0, y0, mi_xsize, mi_ysize, $
;		latmin=-90, latmax = 89.5, lonmin=-180.0, lonmax=180.0, $
;                        /BILIN)
  
;  sxaddpar,hdr,'CRVAL1',0.
;  sxaddpar,hdr,'CRVAL2',0.
;  sxaddpar,hdr,'CRVAL2',0.
;  sxaddpar,hdr,'CTYPE1','GLON-AIT' 
;  sxaddpar,hdr,'CTYPE2','GLAT-AIT'
;  sxaddpar,hdr,'BUNIT','XRT Temp'
  
;  stop
  
  
  ;;;how to convert map - THIS WORKS!!!!
  ;;need map centered on 0,0
  map=mrdfits('~/thermal_map.fits',0,hdr) ;in galactic coords - center(180,0)
  im=map
  nx=n_elements(im[*,0])
  nx2=nx/2.
  im[0:nx2-1,*]=map[nx2:*,*]
  im[nx2:*,*]=map[0:nx2-1,*]
  map_set,/ait
  im2=map_image(im,/bil)
  sxaddpar,hdr,'CRVAL1',0.
  sxaddpar,hdr,'CRVAL2',0.
  sxaddpar,hdr,'CRVAL2',0.
  sxaddpar,hdr,'CTYPE1','GLON-AIT' 
  sxaddpar,hdr,'CTYPE2','GLAT-AIT'
  sxaddpar,hdr,'BUNIT','XRT Temp'
  im3=rotate(im2,5)
  mwrfits,im3,'~/thermal_map3.fits',hdr3,/create
  
  
  return
end 
