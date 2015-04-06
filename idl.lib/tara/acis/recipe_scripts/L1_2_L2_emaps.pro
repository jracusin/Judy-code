;;; Routine to execute the parts of the L1->L2 recipe related to building emaps.
;;; $Id: L1_2_L2_emaps.pro 3508 2009-08-07 17:55:20Z psb6 $

;;;  setenv ARDLIB_FILE `basename $PWD`_ardlib.par
;;;  L1_2_L2_emaps

@acis_extract_tools

PRO L1_2_L2_emaps

;IT IS VITAL THAT YOU SCALE THE STOWED DATA WITH RESPECT TO WHATEVER OBSERVATION DATA YOU WILL BE USING FOR DIFFUSE ANALYSIS.
;IN THIS RECIPE, THAT IS THE FILE acis.validation.evt2, NOT acis.spectral.evt2!  For VFAINT data, the 9-12 keV band is about 1.7 times ;brighter in acis.spectral.evt2 than in acis.validation.evt2!!


obsdata_filename = 'acis.validation.evt2'
stowed_filename  = 'acis.stowed.evt2'

bt_obs    = mrdfits(obsdata_filename, 1, obs_header)
bt_stowed = mrdfits(stowed_filename , 1, stowed_header)
  
; Scaling of stowed data uses the energy range 9-12 keV (Hickox & Markevitch, 2006, Section 4.1.3).
emax = max(bt_obs.energy) < max(bt_stowed.energy) < 12000
emin = 9000

   obs_ccd_hist = histogram(MIN=0,MAX=9,BIN=1, (   bt_obs.ccd_id)[where((   bt_obs.energy GE emin) AND (   bt_obs.energy LE emax))])
stowed_ccd_hist = histogram(MIN=0,MAX=9,BIN=1, (bt_stowed.ccd_id)[where((bt_stowed.energy GE emin) AND (bt_stowed.energy LE emax))])

ccd_scaling = stowed_ccd_hist / float(obs_ccd_hist)

iarray_scaling = mean(ccd_scaling[0:3])
dataset_1d, id, (   bt_obs.energy)[where(   bt_obs.ccd_id LE 3)], DATASET='I-array observed', XTITLE='energy (eV)'
dataset_1d, id, (bt_stowed.energy)[where(bt_stowed.ccd_id LE 3)], DATASET='I-array stowed (scaled)', NORM_ABSC=[0,15000], NORM_VAL=[iarray_scaling,iarray_scaling],COLOR='red',LINE=0

for ii=0,9 do begin
  if ~finite(ccd_scaling[ii]) then continue
  
  ; Record the effective LIVETIMEs of the stowed data for each CCD.
  keyname = string(ii, F="(%'LIVTIME%d')")
  livtime = sxpar( obs_header, keyname )
  cmd = string(stowed_filename, keyname, ccd_scaling[ii] * livtime, F="(%'dmhedit %s filelist=none operation=add key=%s  value=%0.1f')")
  print, cmd
  spawn, cmd
      
  ; Display a map of the fractional mismatch between the stowed and observed data, for each CCD.
  observ_image_spec = string(obsdata_filename, ii, emin, emax, F="(%'%s[ccd_id=%d,energy=%d:%d][bin chip=::128][opt type=r4]')")
  stowed_image_spec = string( stowed_filename, ii, emin, emax, F="(%'%s[ccd_id=%d,energy=%d:%d][bin chip=::128][opt type=r4]')")
  outfile           = string(ii, F="(%'stowed_error_significance_ccd%d.img')")
  
  cmd = string(observ_image_spec+","+stowed_image_spec, outfile, ccd_scaling[ii], $
               F="(%'dmimgcalc infile=""%s"" infile2=none outfile=%s operation=""imgout=(img1-(img2/%0.3f))/sqrt(img1)"" verbose=1 clob+')")          
  print, cmd
  spawn, cmd
  spawn, "ds9 -linear "+outfile+" -zoom to fit &"
endfor

print, emin/1000., emax/1000., obsdata_filename, F="(%'Stowed data is scaled as shown below so the energy range %0.1f:%0.1f keV matches %s:')"
forprint, indgen(10), ccd_scaling, F="(%'  CCD%d  %0.2f')"


; Create emaps for the observation.
; Scale each CCD in that emap to record the desired normalization of the stowed data.
ae_make_emap, obsdata_filename, ['fullfield_1','iarray_1','sarray_1'], CCD_LIST=['012367','0123','67'], ARDLIB_FILENAME=getenv('ARDLIB_FILE'), ASPECT_FN='acis.astrometric.asol1', CCD_SCALING=ccd_scaling, SCALED_PREFIX='stowed.'

spawn, 'ds9 -bin factor 8 -log '+obsdata_filename+' fullfield_1.emap '+stowed_filename+' stowed.fullfield_1.emap &'
return
end

