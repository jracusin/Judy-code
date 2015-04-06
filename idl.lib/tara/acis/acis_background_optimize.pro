;;  acis_background_optimize, 'fullfield_4.emap', TARGET_AREA=xxx, evtfn, ENERGY_RANGE=[200, 7000], 'full_band/fullfield_4.gti', 'full_band/fullfield_4.img'      

;;; TARGET_AREA is area of source aperture in units of skypix^2.

;;; Note that dmmerge (2006) has a bug which causes the GTI tables to be wrong( see Ticket 8214).

@acis_extract

PRO acis_background_optimize, emap_filename, event_filename, TARGET_AREA=target_area, PROB_NO_SOURCE_THRESHOLD=prob_no_source_threshold, ENERGY_RANGE=energy_range, outdir

;COMMON acis_background_optimize, id1, id2, id3

if NOT keyword_set(energy_range) then energy_range=[0.5,8.0]


;; Setup a temp directory.
file_mkdir, outdir
run_command, /INIT, PARAM_DIR=outdir+'/param'

temp_image_fn    = outdir + '/temp.img'
temp_events_fn1  = outdir + '/temp1.evt'
flux_fn          = outdir + '/flux.img'
background_evt_fn= outdir + '/background.evt'
bkg_lc_fn        = outdir + '/background.lc'
bkg_emap_fn      = outdir + '/background.emap'
bkg_flux_fn      = outdir + '/background_flux.img'


run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes', 'punlearn get_sky_limits']                            


;; Filter the event list by energy; make an image matching the emap scene.
run_command, /QUIET, string(emap_filename, F="(%'get_sky_limits %s verbose=0 precision=2')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', binspec

cmd = string( event_filename, 1000*energy_range, binspec, temp_image_fn, $
    		   F="(%'dmcopy ""%s[energy=%d:%d][bin %s]"" %s')")
run_command, cmd
                    
  
data_img = readfits(temp_image_fn)
emap_img = readfits(emap_filename, emap_header)
writefits, flux_fn, data_img/emap_img, emap_header


; DESIGN ISSUES:
; 
; HOW SHOULD LOW EXPOSURE PIXELS BE HANDLED?  CLEARLY THEY ARE IGNORED IN THE MEANCLIP/MASKING COMPUTATIONS.  
; I THINK THERE IS NO NEED TO ACTUALLY MASK THEM.  THE FINAL PRODUCT FROM ALL OF THIS IS A BACKGROUND REGION FOR THE OBSERVATION (DATA AND EMAP) ON WHICH WE WILL COMPUTE A LIGHT CURVE.  
; WE WILL THEN CONVERT THE LIGHTCURVE TO FLUX UNITS BY DIVIDING BY THE SUM OF THE UNMASKED EMAP (INCLUDING LOW EXPOSURE REGIONS).  THEN WE WILL ESTIMATE THE BACKGROUND COUNT RATE IN SOME TARGET EXTRACTION REGION (WITH AREA SUPPLIED AND EMAP VALUE ASSUMED TO BE SOMETHING TYPICAL).
; 

;; Now we will attempt to mask the astrophysical sources by examining the flux image to find bright pixels.
;; We'll iteratively rebin the image until there are no zero-flux pixels left, since the zero-flux pixels make
;; it hard to estimate a background level in the meanclip.pro call below.
done = 0
rebin_factor = 1
Ncol = (size(emap_img, /DIM))[0]
Nrow = (size(emap_img, /DIM))[1]
mask_img = bytarr(Ncol,Nrow)

repeat begin
  ; Trim the arrays to have even dimensions and rebin by two.
  Ncol_rebin = Ncol / rebin_factor
  Nrow_rebin = Nrow / rebin_factor
  col_max    = (Ncol_rebin*rebin_factor) -1
  row_max    = (Nrow_rebin*rebin_factor) -1

  data_img_binned = frebin(data_img[0:col_max,0:row_max], Ncol_rebin, Nrow_rebin, /TOTAL)
  emap_img_binned = frebin(emap_img[0:col_max,0:row_max], Ncol_rebin, Nrow_rebin, /TOTAL)
      
    
                                 
  ; Compute flux image and ignore pixels with low exposure.
  flux_img_binned = data_img_binned / emap_img_binned
  
  mask_ind = where(emap_img_binned LT max(emap_img_binned)/20., num_mask )
  
  if (num_mask GT 0) then begin
    print, 'Masking ', num_mask*rebin_factor, ' pixels for low exposure.'
    flux_img_binned[mask_ind] = !VALUES.F_NAN
  end


  ; Find the unmasked pixels and identify bright outliers.
  flux_sample = flux_img_binned[ where(finite(flux_img_binned) AND (flux_img_binned GT 0), num_samples) ]

  meanclip, flux_sample, mean_flux, sigma_flux   
  max_flux = (mean_flux + 3*sigma_flux)
  help, mean_flux, sigma_flux, max_flux  
  dataset_1d, id1, flux_sample, DATASET_NAME=string(rebin_factor,F='(%"rebin=%d")')
  
  bad_ind = where( flux_img_binned GT max_flux, count)
  print, 'Masking ', count*rebin_factor, ' pixels with flux > ', max_flux
  dimspec = size(flux_img_binned)
  for ii=0L,count-1 do begin
    ; Convert bad_ind to a 2-D index pair.
    index_to_point, bad_ind[ii], X, Y, dimspec 
    
    ; Mask the corresponding region in the unbinned arrays.
    xmin = x * rebin_factor
    xmax = xmin + rebin_factor - 1
    ymin = y * rebin_factor
    ymax = ymin + rebin_factor - 1
    data_img[xmin:xmax,ymin:ymax] = 0
    emap_img[xmin:xmax,ymin:ymax] = 0
    mask_img[xmin:xmax,ymin:ymax] = 1B
  endfor
  
  
  ; Rebin and loop if there are any more zero-flux pixels.
  dum = where(flux_img_binned EQ 0, num_zeros)
  
  if (num_zeros GT 0) then begin
    print, rebin_factor, rebin_factor, num_zeros, F="(%'\nRebinning to %dX%d and iterating because %d flux samples are zero.')"
    rebin_factor = rebin_factor * 2
endif else done=1                                            

endrep until done


;; Write the masked images and spawn ds9.
mask_ind = where( mask_img, count )
if (count GT 0) then begin
  data_img[mask_ind] = !VALUES.F_NAN
  emap_img[mask_ind] = !VALUES.F_NAN
endif

writefits, bkg_flux_fn, data_img/emap_img, emap_header
writefits, bkg_emap_fn,          emap_img, emap_header

cmd = string(flux_fn, bkg_emap_fn, bkg_flux_fn, F="(%'ds9 -prefs nancolor red %s %s %s &')")
run_command, cmd

  
;; Apply the masking recorded by the emap (NOT the flux image) and the energy filter to the event list.
cmd = string(event_filename, bkg_emap_fn, temp_events_fn1, F="(%'dmimgpick ""%s[cols time,sky]"" %s %s method=closest clobber=yes')")
run_command, cmd

cmd = string(temp_events_fn1, background_evt_fn, F="(%'dmcopy ""%s[#3>1]"" %s clobber=yes')")
run_command, cmd
 

;;  Make a light curve; sort the bins by the RATE column.
bt        = mrdfits(background_evt_fn, 1)
gti_table = mrdfits(background_evt_fn, 2)

time = bt.time
time = time[sort(time)]





;cmd = string(background_evt_fn, bkg_lc_fn, F="(%'lightcurve infile=""%s[EVENTS]"" bkgfile=NONE outfile=%s nbins=0 binlength=0 counts_per_bin=400 clobber=yes')")
;run_command, cmd



; The question is whether lightcurve will make one huge bin covering the period between obsids or whether it will
; choke!
; 
; Also, dmmerge does NOT merge the GTI tables!  I tried to fix this using fmodhead in
; /Volumes/Bulk/townsley/NGC3576/data  but now temp_obsid_4496.evt makes dmlist segfault!  .
; 

;; Scale the light curve to be in units of expected background counts in the specified target aperture.
;;                             
; Find size of image pixels in units of skypixels.
pixel_size = sxpar(emap_header, 'CDELT1P')




;; Find the limiting flux for each possible cumulative exposure and plot.

  ; Find the smallest (integer) number of detected counts, c, for which 
  ; Pr(C>c; B) < prob_no_source_threshold

;; Choose a threshold background RATE.  Plot light curve and theshold.
;; We'll have to deal with the integer quantization noise from above.

;; Build a GTI table that applies that threshold.
;; NOTE we must work around a bug in dmgti!

;; Filter the event list by energy and GTI; make an image matching the emap scene.

;; 


return
end

  
