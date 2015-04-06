;;; $Id: tara_smooth.pro 3507 2009-08-07 17:54:51Z psb6 $

;;; This script is closely tied to the workflow shown in the recipe tara_smooth.txt.

;;; The parameter 'scene_name' is a scalar string specifying the first part of the filenames of the images you want to smooth.

;;; The parameter 'significance' is a scalar or vector number specifying the desired SNR of the smoothed flux image. 
;;; The significance computations assume there is no error on the background map!!

;;; By default, adaptive kernel smoothing with a Gaussian kernel is used.  Specify /TOPHAT to use a tophat kernel.

;;; Specify /WVT_BINNING to use the WVT Binning algorithm (Diehl 2006).
;     KEEPFIXED: [Input] Vector (size 2 x n_fixed) containing x and y image coordinates (probably 0-based) of the bin generators 
;                that you want to keep fixed in their position, e.g. KEEPFIXED=[ [100,100], [200,400], [500,300] ]. 
;                The binning algorithm will move all other bins around as usual. 
;                Example use: Keep one bin fixed on the center of a galaxy.

;;; For example,
;;;    tara_smooth, 'iarray', [5,10,15], /TOPHAT
;;; will smooth images named "*/iarray.diffuse.img" to a SNR of 15 using a tophat kernel.

;;; Optionally, you can specify a glob pattern for the bands (directories) you want to smooth.
;;;   BAND_NAME='{hard_band,soft_band}'
;;; The default is
;;;   BAND_NAME='*'

PRO tara_smooth, scene_name, significance, BAND_NAME=band_name, PLOT_ONLY=plot_only, $
             TOPHAT=tophat, MAX_RADIUS=max_radius, $
             WVT_BINNING=wvt_binning, KEEPFIXED=keepfixed

creator_string = "tara_smooth, version"+strmid("$Date: 2009-08-07 13:54:51 -0400 (Fri, 07 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

if ~keyword_set(band_name) then band_name = '*'

; Choose a smoothing kernel, 1 for Gaussian kernal, 0 for tophat.
gaussian_not_tophat =  ~keyword_set(tophat)

; Find the observed data images to be smoothed.
mask_fn     = scene_name+'_mask.img'
emap_fn     = scene_name+'.diffuse.emap'
obs_img_fn  = file_search(band_name+'/'+scene_name+'.diffuse.img', COUNT=num_obs)
bkg_fn      = file_dirname(obs_img_fn)+'/'+scene_name+'.bkg.img'
signal_fn   = file_dirname(obs_img_fn)+'/'+scene_name+'.signal'

print, 'Smoothing: '
forprint, obs_img_fn, bkg_fn, replicate(emap_fn, num_obs), F="(%'(%s - %s) / %s')"

; Read the field mask (=0 off the field of view).
!ERROR_STATE.CODE = 0
field_mask   = (readfits(mask_fn) GT 0)
if (!ERROR_STATE.CODE LT 0) then begin
  print, 'ERROR: could not read '+scene_name+'_mask.img'
  retall
endif

; Read the masked emap (=0 near point sources).
!ERROR_STATE.CODE = 0
obs_emap    = readfits(emap_fn)
if (!ERROR_STATE.CODE LT 0) then begin
  print, 'ERROR: could not read '+scene_name+'.diffuse.emap'
  retall
endif


if keyword_set(wvt_binning) then begin
  ; Since we're passing the WVT Binning algorithm a flux image (not separate data and emap images), we MUST distinguish pixels that have zero flux because no counts happened to be observed there from pixels that have zero flux because no observation was made there (off-field, or masked regions).
  ; All unobserved pixels must be zero in the field_mask map---we can NOT "smooth over" small holes!!  
  
  ; We should however exclude pixels where the emap is very small because the "signal" image (flux) will be very noisy there.
  typical_emap_value = median(obs_emap[where(obs_emap GT 0)])
  emap_threshold = 0.01 * typical_emap_value
  print, emap_threshold, F='(%"Ignoring pixels where exposure map is < %0.2g")'
  ind = where(obs_emap LT emap_threshold, count)
  if (count GT 0) then begin
    field_mask[ind] = 0
  endif
  
  suffix = ''
endif else begin
  ; The inputs to the adaptive smoothing code are able to represent two separate concepts:
  ; 1. pixels where there has been no observation (emap == 0)
  ; 2. pixels where we do NOT wish to compute an answer (field_mask == 0), i.e. the pixels that should be null in the output image.

  ; Extrapolate over any small gaps in the field mask between pointings.
  field_mask = field_mask OR (smooth(float(field_mask), 5, /EDGE) GT 0.01)
  
  ; Decide how you want to handle the holes in the data.  
  ; OPTION I: retain the holes.
  ;  field_mask = field_mask AND (obs_emap GT 0)
  ;  suffix     = '_nofill'
  
  ; OPTION II: extrapolate over the small holes in the emap remaining from source masking.
  field_mask = field_mask AND (smooth(float((obs_emap GT 0)), 5, /EDGE) GT 0.01)
  suffix     = '_fill5'
  
  ; OPTION I: extrapolate over all the holes.
  ; field_mask = field_mask 
  ; suffix     = '_filled'
  
  ;tvscl, field_mask
endelse ; adaptive smoothing
 

; Make directories to hold the output, such as sig11/gauss/
if keyword_set(wvt_binning) then begin
  smooth_dirs = string(significance, F='(%"sig%3.3d/wvt/")') 
endif else begin
  smooth_dirs = string(significance, F='(%"sig%3.3d/")') + (gaussian_not_tophat ? 'gauss/' : 'tophat/')
endelse
fixed_radius_map = 0

;---------------------------------------------------------------------
; OPTIONAL
; If you wish to smooth all images using the same set of kernels that were 
; used in an previous run, then read in that kernel radius map.  For example
;
;fixed_radius_map = readfits('sig19/gauss/500:7000_filled.radius')
;---------------------------------------------------------------------

for ii=0,num_obs-1 do begin
  fdecomp, obs_img_fn[ii], disk, path, name, qual
  
  out_dirs = path+smooth_dirs
  file_mkdir, out_dirs
  
  !ERROR_STATE.CODE = 0
  obs_img = readfits(obs_img_fn[ii], obsdata_header)
  if (!ERROR_STATE.CODE LT 0) then begin
    print, 'ERROR: could not read '+obs_img_fn[ii]
    retall
  endif
  !ERROR_STATE.CODE = 0
  bkg_img = readfits(bkg_fn[ii])
  if (!ERROR_STATE.CODE LT 0) then begin
    print, 'ERROR: could not read '+obs_img_fn[ii]
    retall
  endif
  
  ; Sometimes data appears in pixels with zero exposure, presumably due to
  ; differences between the reprojection of event data and the reprojection
  ; of masked exposure map images.
  ind = where(obs_emap LE 0 AND obs_img GT 0, count)
  if (count GT 0) then begin
    print, total(obs_img[ind]), F='(%"WARNING: %d observed events removed from pixels with zero exposure.")'
    obs_img   [ind] = 0
  endif

  ind = where(obs_emap LE 0 AND bkg_img GT 0, count)
  if (count GT 0) then begin
    print, total(bkg_img[ind]), F='(%"WARNING: %d scaled stowed events removed from pixels with zero exposure.")'
    bkg_img[ind] = 0
  endif
  

  if keyword_set(wvt_binning) then begin
    ; Run the WVT Binning tool.
    extast, obsdata_header, img2wcs_astr
    
    for jj=0,n_elements(smooth_dirs)-1 do begin
      perform_smoothing = ~keyword_set(plot_only)
    
      region_fn = out_dirs[jj] + name + suffix + ".wvtbin.reg"
      out_fn = out_dirs[jj] + name + suffix + ['.flux', '.signif', '.binnumber']
      print
      if file_test(out_fn[0]) then begin
        print, out_fn[0], F='(%"%s already exists; skipping this smoothing run.")'
        perform_smoothing = 0
      endif
      
      if perform_smoothing then begin
        print, 'Building ' + out_fn[0]
        help,obs_img,bkg_img,obs_emap,field_mask
        
        ; On April 3, 2006 Steven Diehl recommended using the MAX_AREA input to limit the size of bins in regions with little signal.
        ; This is also discussed in the manual, S6.1.8.
        max_area = n_elements(obs_img) * 0.05
    
;       ;; IF YOU DESIRE A CONSTANT NUMBER OF NET COUNTS IN EACH TESSELATE, THEN DO IT THIS WAY.
;       ; The noise estimate below, sqrt(obs_img), assumes there is no error on the background map!!
;       signal =      obs_img-bkg_img
;       noise  = sqrt(obs_img)
;       wvt_image, signal, noise, significance[jj], CTSIMAGE=obs_img, MASK=field_mask, MAX_AREA=max_area, binned_data, xnode, ynode, weight, BINNUMBER=binnumber, SNBIN=snbin, PLOTIT=2
;         
;       ; On February 20, 2006 Steven Diehl described the use of wvt_applybinning to "co-smooth" the exposure map.
;       binned_emap	= wvt_applybinning(binnumber, obs_emap)
;       
;       flux_map = binned_data/binned_emap
              
        ;; IF YOU DESIRE A CONSTANT FLUX IN EACH TESSELATE, THEN DO IT THIS WAY.
        ; The noise estimate below, sqrt(obs_img), assumes there is no error on the background map!!
        signal =     (obs_img-bkg_img)/obs_emap
        noise  = sqrt(obs_img)        /obs_emap
        
        ; Make sure there is no signal in masked pixels!
        ind = where(field_mask LE 0, count)
        if (count GT 0) then begin
          signal[ind] = 0
          noise [ind] = 0
        endif
        
        
        fxaddpar, obsdata_header, 'CREATOR' , creator_string
        fxaddpar, obsdata_header, 'TARGETSN', significance[jj]
        writefits, signal_fn[ii], signal,  obsdata_header
        wvt_image, signal, noise, significance[jj], CTSIMAGE=obs_img, MASK=field_mask, MAX_AREA=max_area, KEEPFIXED=keepfixed, binned_data, xnode, ynode, weight, BINNUMBER=binnumber, SNBIN=snbin, PLOTIT=2

        ; Create a significance map by applying the bin significance values (vector SNBIN) to the appropriate image pixes, as described by the BINNUMBER map.
        snr_map  = make_array(DIMENSION=size(obs_img, /DIMENSION), /FLOAT)
        nbins    = n_elements(snbin)
        bin_area = HISTOGRAM(binnumber, REVERSE_INDICES=r, min=1, max=nbins)
        
        save, FILE=out_dirs[jj] + name + suffix + '.sav'

        print, 'Building WVT bin region files ...'
        ; Open a master region file that will contain all the tesselate regions.
        openw,  unit1, region_fn, /GET_LUN
        printf, unit1, "# Region file format: DS9 version 3.0"
        printf, unit1, "J2000"
        
        if keyword_set(keepfixed) then begin
          ; Make regions marking any bin generators specified by the caller.
          !TEXTUNIT = unit1  
          ; The xy2ad routine expects 0-based pixel coordinates.
          xy2ad, keepfixed[0,*], keepfixed[1,*], img2wcs_astr, ra_pt, dec_pt
          forprint, TEXTOUT=5, /NoCOM, ra_pt, dec_pt, F='(%"cross point %10.6f %10.6f # tag={bin generator} color=red")'
        endif
        
        for kk=0L, nbins-1 do begin
          if (bin_area[kk] GT 0) then begin
            ind = r[r[kk]:r[kk+1]-1]
          
            snr_map[ind] = snbin[kk]
            
            ; Create a ds9 region file that outlines this bin.
            this_binnumber = kk+1
            contour, binnumber EQ this_binnumber, /CLOSED, LEVELS=[1],PATH_XY=xy, PATH_INFO=info, /PATH_DATA_COORDS
            
            if (n_elements(info) GT 0) then begin
              ; Extract the 0-based pixel coordinates of the contour polygon.
              ind = info[0].offset + indgen(info[0].N)
              polygon_x = float(round(reform(xy[0, ind])))
              polygon_y = float(round(reform(xy[1, ind])))
              
              ; Enlarge the polygons so that they touch each other.
              ; The best way to do this seems to be to move horizontal segments up/down and vertical segments left/right by 0.5 pixel.
              for this=0,n_elements(polygon_x)-1 do begin
                next = (this+1) MOD n_elements(polygon_x)
                if (polygon_x[this] EQ polygon_x[next]) then begin
                  ; Vertical segment
                  polygon_x[[this,next]] += (binnumber[2+polygon_x[this], polygon_y[this]] NE this_binnumber) ? 0.5 : (-0.5)
                endif
                
                if (polygon_y[this] EQ polygon_y[next]) then begin
                  ; Horizontal segment
                  polygon_y[[this,next]] += (binnumber[polygon_x[this], 2+polygon_y[this]] NE this_binnumber) ? 0.5 : (-0.5)
                endif
              endfor
            endif else begin
              ; The contour routine could not deal with this bin (e.g. it's very small).
              print, this_binnumber, F='(%"WARNING! Contour algorithm failed on bin %d; using bounding box, which may not be perfect.")'
              index_to_point, where(binnumber EQ this_binnumber), X, Y, size(binnumber)
              
              polygon_x = [min(X)-0.5, max(X)+0.5, max(X)+0.5, min(X)-0.5]
              polygon_y = [min(Y)-0.5, min(Y)-0.5, max(Y)+0.5, max(Y)+0.5]
            endelse
            
            ; The xy2ad routine expects 0-based pixel coordinates.
            xy2ad, polygon_x, polygon_y, img2wcs_astr, polygon_ra, polygon_dec
            
            polygon = dblarr(2,n_elements(polygon_ra))
            polygon[0,*] = polygon_ra
            polygon[1,*] = polygon_dec
            region = 'polygon(' + strcompress(strjoin(string(polygon,F='(F10.6)'),","), /REMOVE) + ')'
            
            ; Write this region both to its own file, and to the master region file.
            openw,  unit2, out_dirs[jj] + name + suffix + string(this_binnumber,F='(%".wvtbin%3.3d.reg")' ), /GET_LUN
            printf, unit2, "# Region file format: DS9 version 3.0"
            printf, unit2, "J2000"
            printf, unit1, region, this_binnumber, this_binnumber, F='(%"%s # move=0 text={%3.3d} tag={wvtbin%3.3d}")' 
            printf, unit2, region, this_binnumber, this_binnumber, F='(%"%s # move=0 text={%3.3d} tag={wvtbin%3.3d}")' 
            free_lun, unit2
          endif
        endfor ;kk
        free_lun, unit1
      
        ; Save flux, error, & radius images.
        flux_map = binned_data
        writefits, out_fn[0], flux_map,  obsdata_header
        writefits, out_fn[1],  snr_map,  obsdata_header
        writefits, out_fn[2], binnumber, obsdata_header
        
        bt = replicate({xnode:xnode[0], ynode:ynode[0], weight:weight[0], snbin:snbin[0]}, nbins)
        bt.xnode  = xnode
        bt.ynode  = ynode
        bt.weight = weight
        bt.snbin  = snbin
        fxbhmake, theader, nbins, 'WVT_BINS', 'outputs from wvt_image.pro', /DATE, /INIT
        fxaddpar, theader, 'CREATOR' , creator_string
        fxaddpar, theader, 'TARGETSN', significance[jj]
        fxaddpar, theader, 'TUNIT1', "none", 'location of the WVT bin generators, 0-based pixel index'
        fxaddpar, theader, 'TUNIT2', "none", 'location of the WVT bin generators, 0-based pixel index'
        fxaddpar, theader, 'TUNIT3', "none", 'weight of the WVT bin generators'
        fxaddpar, theader, 'TUNIT4', "none", 'actual bin SNR'
        mwrfits, bt, out_fn[2], theader
        
      endif ;perform_smoothing
      
      cmd = string(out_dirs[jj] + name, bkg_fn[ii], obs_img_fn[ii], signal_fn[ii], out_fn[2], region_fn, scene_name+'.diffuse.emap', out_fn[0], out_fn[1], F="(%'ds9 -geometry 1400x1000 -title %s -scale mode 99.0 %s -linear %s -log %s -linear %s -region %s  %s %s -log %s -linear -zoom to fit -match frames wcs &')")
      print, cmd, F='(%"\n  %s")'
      spawn, cmd
    endfor ;jj
  endif else begin
    ; Run our adaptive smoothing tool.
    
    if ~keyword_set(max_radius) then begin
      ; We arbitrarily set the maximum kernel to cover a certain fraction of the scene, 
      if gaussian_not_tophat then max_area = n_elements(obs_img) * 0.064 $
      else                        max_area = n_elements(obs_img) * 0.03
      
      max_radius = ceil(sqrt(max_area/!PI))
    endif
    
    ; We generate no more than 50 smoothing scales to speed things along.
    max_num_kernels = 50

    for jj=0,n_elements(smooth_dirs)-1 do begin
      perform_smoothing = ~keyword_set(plot_only)
    
      out_fn = out_dirs[jj] + name + suffix + ['.flux', '.signif', '.radius']
      print
      if file_test(out_fn[0]) then begin
        print, out_fn[0], F='(%"%s already exists; skipping this smoothing run.")'
        perform_smoothing = 0
      endif
      
      if perform_smoothing then begin
        print, 'Building ' + out_fn[0]
        help,obs_img,bkg_img,obs_emap,field_mask,max_radius,fixed_radius_map
    
        adaptive_density_2d, obs_img, significance[jj], MAX_RADIUS=max_radius, MAX_NUM_KERNELS=50, FIXED_RADIUS_MAP=fixed_radius_map, EMAP=obs_emap, BACKGROUND_MAP=bkg_img, FIELD_MASK=field_mask, GAUSS=gaussian_not_tophat, flux_map, error_map, radius_map
        
        ; Make sure MAX_RADIUS was big enough.
        largest_radius = max(radius_map) 
        if (NOT keyword_set(fixed_radius_map)) AND (largest_radius EQ max_radius) then begin
          print
          print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          print, 'WARNING! MAX_RADIUS is not large enough.'
        endif
    
        ; Save flux, error, & radius images.
        fxaddpar, obsdata_header, 'CREATOR' , creator_string
        fxaddpar, obsdata_header, 'TARGETSN', significance[jj]
        writefits, out_fn[0], flux_map,   obsdata_header
        writefits, out_fn[1], flux_map/error_map,  obsdata_header
        writefits, out_fn[2], radius_map, obsdata_header
      endif ;perform_smoothing
      
      cmd = string(out_dirs[jj] + name, bkg_fn[ii], obs_img_fn[ii], out_fn[2], scene_name+'.diffuse.emap', out_fn[0], out_fn[1], F="(%'ds9 -geometry 1400x1000 -title %s -scale mode 99.0 %s -linear %s -log %s -linear %s %s -log %s -linear -zoom to fit -match frames wcs &')")
      print, cmd, F='(%"\n  %s")'
      spawn, cmd
    endfor ;jj
  endelse ; adaptive smoothing
endfor ;ii

return
end

