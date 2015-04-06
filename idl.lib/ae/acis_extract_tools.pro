;;; $Id: acis_extract_tools.pro 3517 2009-08-12 14:32:52Z psb6 $
;;; Accessory tools for ACIS Extract. 
;;; Patrick Broos, Penn State University, 2004

@acis_extract

;;; ==========================================================================
;;; This program will "poke" a FITS keyword into the source.stats file (obsname omitted)
;;; or obs.stats file (obsname supplied) for each source in the srclist.
;;; KEYWORD is a scalar string; VALUE and COMMENT are either scalar string or 
;;; string vectors whos length matches the catalog.

PRO ae_poke_source_property, catalog_or_srclist, obsname, KEYWORD=keyword, VALUE=value_p, COMMENT=comment_p, $
                  EXTRACTION_NAME=extraction_name, MERGE_NAME=merge_name

src_stats_basename       = 'source.stats'
obs_stats_basename       = 'obs.stats'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'


if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)

if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                else merge_subdir = ''
if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)


case n_elements(value_p) of
  1: value = replicate(value_p,num_sources)
  num_sources: value = value_p
  else: message, 'Length of VALUE parameter must be either 1 or '+string(num_sources)
endcase

case n_elements(comment_p) of
  1: comment = replicate(comment_p,num_sources)
  num_sources: comment = comment_p
  else: message, 'Length of COMMENT parameter must be either 1 or '+string(num_sources)
endcase

source_not_observed = bytarr(num_sources)

for ii = 0, num_sources-1 do begin
  if keyword_set(obsname) then begin
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn  = obsdir    + obs_stats_basename
  endif else begin
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii]
    stats_fn  = sourcedir + src_stats_basename
  endelse
  
  stats = headfits(stats_fn, ERRMSG=error)
  
  if keyword_set(error) then begin
    source_not_observed[ii] = 1
    continue
  endif
    
  fxaddpar, stats, keyword, value[ii], comment[ii]
  writefits, stats_fn, 0, stats
endfor ;ii

ind = where(source_not_observed, count)
if (count GT 0) then print, file_basename(stats_fn), count, F="(%'WARNING! Could not read %s for %d sources.')"
return
end


;;;; ==========================================================================
;;;; This tool fits two diffuse sources simultaneously, with one serving as an 
;;;; observation of the astrophysical (sky) background.
;;;; Each source is assume to have its own background spectrum representing the
;;;; local instrumental background (see diffuse recipe).
;;;;
;;;; The calibration (ARFs) for the diffuse sources is assumed to have been put 
;;;; onto a "per arcsec^2" basis by AE.
;;;;
;;;; Only chi^2 fitting is supported.
;
;PRO ae_fit_diffuse, object_name, sky_name, CHANNEL_RANGE=channel_range, $
;                  MODEL_FILENAME=model_filename, MODEL_CHANGES_FILENAME=model_changes_filename, $
;                  SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
;                  INTERACTIVE=interactive
;                  
;creator_string = "ae_fit_diffuse, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
;print, creator_string
;print, systime()
;
;;; Create a randomly named scratch directory that will not conflict with another instance of AE.
;repeat begin
;  session_name = string(random()*1E4, F='(I4.4)')
;  tempdir = 'AE' + session_name +'.noindex/'
;  tempdir = filepath(tempdir, /TMP)
;endrep until (~file_test(tempdir))
;file_mkdir, tempdir
;print, 'Using temporary directory: ', tempdir
;
;
;result = routine_info( 'acis_extract', /SOURCE )
;fdecomp, result.PATH, disk, codedir
;
;if NOT keyword_set(model_filename) then begin
;  print, 'ERROR:Parameter MODEL_FILENAME not supplied!'
;  GOTO, FAILURE
;endif
;
;; SNR_RANGE[1] is the user's goal for defining groups; SNR_RANGE[0] is the lower limit allowed before we abort the grouping attempt
;if (n_elements(snr_range) EQ 0) then $
;  snr_range = [1,3]
;if (n_elements(snr_range) NE 2) then begin
;  print, 'ERROR: keyword SNR_RANGE should be a 2-element vector giving the range of SNR allowed for each spectral group, e.g. [2.5,5].'
;  GOTO, FAILURE      
;endif
;
;if (snr_range[1] LT 0) then begin
;  print, 'ERROR: minimum SNR value (SNR_RANGE[1]) must be positive'
;  GOTO, FAILURE
;endif
;
;if (n_elements(num_groups_range) EQ 0) then $
;  num_groups_range = [10,250]
;if (n_elements(num_groups_range) NE 2) then begin
;  print, 'ERROR: keyword NUM_GROUPS_RANGE should be a 2-element vector specifying how many spectral groups are desired, e.g. [10,250].'
;  GOTO, FAILURE      
;endif
;
;  
;run_command, /INIT, PARAM_DIR=tempdir
;
;
;;; Create directory for this pair of sources.
;sourcename = object_name+'_AND_'+sky_name
;sourcedir = sourcename+'/'
;file_mkdir, sourcedir
;
;;; ------------------------------------------------------------------------
;;; Create symlinks to needed object files; group object spectrum
;suffix = ['.pi', '_bkg.pi', '.arf', '.rmf']
;fn = object_name+suffix
;file_delete, sourcedir+fn, /ALLOW_NONEXISTENT
;file_link, '../'+object_name+'/'+fn, sourcedir
;
;obj_src_spectrum_fn     = sourcedir+fn[0]
;obj_bkg_spectrum_fn     = sourcedir+fn[1]
;obj_grouped_spectrum_fn = ''
;
;ae_group_spectrum, obj_src_spectrum_fn, obj_bkg_spectrum_fn, obj_grouped_spectrum_fn, $
;                   CHANNEL_RANGE=channel_range, $
;                   SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
;                   CREATOR_STRING=creator_string, $
;                   this_snr_goal, grp_name, channel_starting_group, num_groups, inband_counts
;
;obj_ignore_spec = string(num_groups, F='(%"1,%d")')
;
;;; ------------------------------------------------------------------------
;;; Create symlinks to needed object files; group object spectrum
;fn = sky_name+suffix
;file_delete, sourcedir+fn, /ALLOW_NONEXISTENT
;file_link, '../'+sky_name+'/'+fn, sourcedir
;
;sky_src_spectrum_fn     = sourcedir+fn[0]
;sky_bkg_spectrum_fn     = sourcedir+fn[1]
;sky_grouped_spectrum_fn = ''
;
;ae_group_spectrum, sky_src_spectrum_fn, sky_bkg_spectrum_fn, sky_grouped_spectrum_fn, $
;                   CHANNEL_RANGE=channel_range, $
;                   SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
;                   CREATOR_STRING=creator_string, $
;                   this_snr_goal, junk, channel_starting_group, num_groups
;
;  
;sky_ignore_spec = string(num_groups, F='(%"1,%d")')
;
;
;;; ------------------------------------------------------------------------
;;; Choose energy bins for cplinear model of sky spectrum.
;;  We ask AE's grouping algorithm to create exactly 11 groups in order to get the 10 vertices that cplinear needs.
;ae_group_spectrum, sky_src_spectrum_fn, sky_bkg_spectrum_fn, '/dev/null', $
;                       CHANNEL_RANGE=channel_range, $
;                       SNR_RANGE=[0,this_snr_goal], NUM_GROUPS_RANGE=[11,11], $
;                       CREATOR_STRING=creator_string, $
;                       this_snr_goal, junk, channel_starting_group, num_groups 
;
;if (num_groups NE 11) then begin
;  print, 'ERROR: grouping algorithm was not able to choose channels for the cplinear sky model.'
;  retall
;endif
;
;; Use RMF & ARF to figure out the energy for each spectral channel.
;ae_channel_energy_and_arf, sourcedir+fn[3], sourcedir+fn[2], $
;  channel_number, channel_lowenergy, channel_highenergy, channel_midenergy
;
;energy_starting_group  = interpol(channel_midenergy, channel_number, channel_starting_group)
;
;
;;; ------------------------------------------------------------------------
;;; Build a name for the model using the basename of MODEL_FILENAME and if supplied
;;; appending the basename of MODEL_CHANGES_FILENAME.
;modelsubdir              = 'spectral_models/'
;
;fdecomp, model_filename, disk, dir, base_model_name
;
;model_name = base_model_name
;
;if keyword_set(model_changes_filename) then begin
;  fdecomp, model_changes_filename, disk, dir, model_changes_name, model_changes_qual
;
;  ; Look for a local file in the source directory to override the specified MODEL_CHANGES_FILENAME.
;  custom_model_name = strjoin([model_name,model_changes_name], '_')
;  fit_custom_fn     = sourcedir + modelsubdir + custom_model_name + '.' + model_changes_qual[0]
;  
;  if NOT file_test(fit_custom_fn) then begin
;    ; No local override found, so prepare to use the specified files.
;    if (n_elements(model_changes_filename) EQ 1) then begin
;      fit_custom_fn = model_changes_filename 
;    endif else begin
;      ; Concatenate all the MODEL_CHANGES files specified.
;      fit_custom_fn = tempdir + 'model_changes.xcm'
;      cmd = string(strjoin(model_changes_filename, ' '), fit_custom_fn, F='(%"cat %s >! %s")')
;      run_command, /UNIX, cmd, /QUIET
;    endelse
;  endif
;  
;  if file_test(fit_custom_fn) then begin
;    model_name = custom_model_name
;    print, 'CUSTOMIZATIONS to xcm script:'
;    run_command, /UNIX, 'cat '+fit_custom_fn
;    print
;  endif else fit_custom_fn = ''
;endif else fit_custom_fn = ''
;
;  
;;; ------------------------------------------------------------------------
;;; Build the fitting script.
;file_mkdir, sourcedir + modelsubdir
;fit_result_root         = grp_name + '_' + model_name
;fit_xcm_fn             = modelsubdir +fit_result_root + '.xcm'
;
;openw,  xcm_unit, sourcedir + fit_xcm_fn, /GET_LUN
;printf, xcm_unit, file_basename(obj_grouped_spectrum_fn), F='(%"set obj_spectrum_filename       \"%s\"")'
;printf, xcm_unit, file_basename(sky_grouped_spectrum_fn), F='(%"set sky_spectrum_filename       \"%s\"")'
;printf, xcm_unit, obj_ignore_spec,                    F='(%"set obj_ignore_spec             \"%s\"")'
;printf, xcm_unit, sky_ignore_spec,                    F='(%"set sky_ignore_spec             \"%s\"")'
;printf, xcm_unit, fit_result_root,                    F='(%"set model_name              \"%s\"")'
;printf, xcm_unit, 0,                                  F='(%"set c_stat_flag               %d")'
;printf, xcm_unit, inband_counts,                      F='(%"set src_cnts                  %d")'
;printf, xcm_unit, strjoin(string(energy_starting_group[1:10],F='(%"%4.2f")'),' '), $
;                                                      F='(%"set cplinear_energies        {%s}")'
;printf, xcm_unit, codedir+'xspec_scripts',            F='(%"set model_directory         \"%s\"")'
;printf, xcm_unit, keyword_set(interactive),           F='(%"set interactive_flag          %d")'
;free_lun, xcm_unit
;
;; Append MODEL_FILENAME to XSPEC script prefix using "sed" to insert any user-supplied customizations to the model.
;if (fit_custom_fn EQ '') then begin
;  cmd = string(model_filename, sourcedir + fit_xcm_fn, F="(%'cat %s >>! %s')")
;endif else begin
;  cmd = string(fit_custom_fn, model_filename, sourcedir + fit_xcm_fn, F="(%'sed -e ""/AE CUSTOMIZATIONS/r %s"" %s >>! %s')")
;endelse
;run_command, /UNIX, cmd
;
;;; ------------------------------------------------------------------------
;;; Perform the fit.
;ae_perform_fit, sourcedir, fit_result_root, INTERACTIVE=keyword_set(interactive)
;
;FAILURE: 
;return
;end



;;; ==========================================================================
;;; Routine that decides when a parameter confidence interval reported by the AE fitting scripts should be used and when it should be ignored.
;;; 
;;; The param_value, param_min, param_max inputs are the best-fit parameter value and the soft limits given to XSPEC.

;;; The lower_confidence_limit and upper_confidence_limit inputs are the confidence interval returned by XSPEC.
;;; Values in these vectors will be set to NaN when this routine determines that they should be ignored.

;;; The errstatus input is the set of status flags returned by XSPEC's "\nERROR" command.

;;; The structure returned by this function contains all the inputs, plus an xspec_anom_flags string vector that is a set of flags indicating various anomalies that were found:

;;;   f: the parameter was frozen
;;;   r: the parameter value falls outside the soft limits declared in the fitting script

;;;   s: error estimation was skipped (either by script or by XSPEC because reduced chi-squared was too high

;;;   n: non-monotonic warning from XSPEC
;;;   l: search for lower limit failed, or ran into hard limit
;;;   u: search for upper limit failed, or ran into hard limit
;;;   m: "minimization may have run into problem"

;;;   L: the lower confidence limit falls outside the soft limits declared in the fitting script
;;;   U: the upper confidence limit falls outside the soft limits declared in the fitting script
;;;   o: the parameter value falls outside its reprorted confidence interval

;;; The structure also contains a scalar named "bisector" that can be used to classify the confidence intervals into three categories:
;;;   1. Intervals that are smaller than bisector.
;;;   2. Intervals that are larger than bisector.
;;;   3. Intervals that are consistent with bisector, i.e. intervals that contain bisector.
;;; The bisector value is chosen to produce nearly equal numbers of sources in categories 1 and 2.


FUNCTION validate_xspec_confidence_interval, parameter_name, label, param_min, param_max, $
      param_value, lower_confidence_limit, upper_confidence_limit, errstatus, param_was_frozen, $
      HIDE_ERRORS_FLAG=hide_errors_flag, PLOT=plot, VERBOSE=verbose

; Defintions of flags returned by "tclout error" command in XSPEC, numbered left-to-right:
;  1      new minimum found
;  2      non-monotonicity detected
;  3      minimization may have run into problem
;  4      hit hard lower limit
;  5      hit hard upper limit
;  6      parameter was frozen
;  7      search failed in -ve direction
;  8      search failed in +ve direction
;  9      reduced chi-squared too high

seq = indgen(n_elements(param_value))

null_flag = replicate('.',n_elements(errstatus))
bad_ordering_flag  = null_flag
range_flag         = null_flag
skipped_flag       = null_flag
frozen_flag        = null_flag
lowerlimit_flag    = null_flag
upperlimit_flag    = null_flag
non_monotonic      = null_flag
lower_search       = null_flag
upper_search       = null_flag
bad_minimize       = null_flag

; We declare below the flag values that are valid reasons to ignore (set to NaN) the lower and upper confidence limits.
; We don't know how to interpret the "minimization may have run into problem" flag from XSPEC, and choose to ignore it.
; We choose to ignore the vague 'n' flags from XSPEC, since the error command seems to continue its search after warnings about non-monotonicity.  2009 April 21
bad_lerror_flaglist = '[osfLlc]'
bad_uerror_flaglist = '[osfUuc]'

; List of flag values (from validate_xspec_confidence_interval) that should invalidate the fit itself.
;bad_fit_flaglist    = '[ro]'
bad_fit_flaglist    = '[o]'



; If the "\nERROR" command was run on a frozen parameter then a flag in errstatus will indicate that.
; If the "\nERROR" command was skipped, then we can still infer that the parameter was frozen by examining the "frozen flag" saved by the fitting script.
ind = where(strmatch(errstatus, '?????T*') OR param_was_frozen, count)
if (count GT 0) then begin
  frozen_flag     [ind] = 'f'
  param_was_frozen[ind] = 1
endif

; Report when the best-fit parameter violated its soft limits.
ind = where(((param_value LT param_min) OR (param_value GT param_max)), count)
if (count GT 0) then begin
  range_flag[ind] = 'r'
  print, count, parameter_name, F='(%"\nThese %d sources have %s out of range:")'
  forprint, seq, label, param_value, SUBSET=ind, F='(%"%5d %s %f")', TEXTOUT=2

endif

; AE writes the string "skipped" when the error command is not attempted.
ind = where(strmatch(errstatus, 'skipped*'), count)
if (count GT 0) then skipped_flag[ind] = 's'

; XSPEC can skip an error estimation if reduced chi^2 is too large.
ind = where(strmatch(errstatus, '????????T*'), count)
if (count GT 0) then skipped_flag[ind] = 's'

ind = where(strmatch(errstatus, '?T*'), count)
if (count GT 0) then non_monotonic[ind] = 'n'


; Discard confidence limits that we have so far deemed to be unreliable, and THEN do some tests that involve those limits.
anom_flags = frozen_flag+range_flag+skipped_flag+non_monotonic

ind = where( stregex(/BOOL, anom_flags, bad_uerror_flaglist), count )
if (count GT 0) then upper_confidence_limit[ind] = !VALUES.F_NAN

ind = where( stregex(/BOOL, anom_flags, bad_lerror_flaglist), count )
if (count GT 0) then lower_confidence_limit[ind] = !VALUES.F_NAN


; Confidence limits of zero are taken to be failures of the error estimation procedure.
ind = where(strmatch(errstatus, '???T*' ) OR strmatch(errstatus, '??????T*' ) OR (lower_confidence_limit EQ 0), count)
if (count GT 0) then lower_search[ind] = 'l'

ind = where(strmatch(errstatus, '????T*') OR strmatch(errstatus, '???????T*') OR (upper_confidence_limit EQ 0), count)
if (count GT 0) then upper_search[ind] = 'u'

ind = where(strmatch(errstatus, '??T*'), count)
if (count GT 0) then bad_minimize[ind] = 'm'


; Discard confidence limits that we have so far deemed to be unreliable, and THEN do some tests that involve those limits.
anom_flags += lower_search+upper_search+bad_minimize

ind = where( stregex(/BOOL, anom_flags, bad_uerror_flaglist), count )
if (count GT 0) then upper_confidence_limit[ind] = !VALUES.F_NAN

ind = where( stregex(/BOOL, anom_flags, bad_lerror_flaglist), count )
if (count GT 0) then lower_confidence_limit[ind] = !VALUES.F_NAN


; Check for confidence intervals that violate the parameter ranges.
ind = where( lower_confidence_limit LT param_min, count)
if (count GT 0) then lowerlimit_flag[ind] = 'L'

ind = where( upper_confidence_limit GT param_max, count)
if (count GT 0) then upperlimit_flag[ind] = 'U'


; Discard confidence limits that we have so far deemed to be unreliable, and THEN do some tests that involve those limits.
anom_flags += lowerlimit_flag+upperlimit_flag

ind = where( stregex(/BOOL, anom_flags, bad_uerror_flaglist), count )
if (count GT 0) then upper_confidence_limit[ind] = !VALUES.F_NAN

ind = where( stregex(/BOOL, anom_flags, bad_lerror_flaglist), count )
if (count GT 0) then lower_confidence_limit[ind] = !VALUES.F_NAN


; Look for a best-fit parameter value that lies outside its confidence interval.
ind = where((upper_confidence_limit LT param_value) OR (lower_confidence_limit GT param_value), count)
if (count GT 0) then bad_ordering_flag[ind] = 'o'


; Perform one last cleaning of the confidence limits that we have deemed to be unreliable.
anom_flags += bad_ordering_flag

ind = where( stregex(/BOOL, anom_flags, bad_uerror_flaglist), count )
if (count GT 0) then upper_confidence_limit[ind] = !VALUES.F_NAN

ind = where( stregex(/BOOL, anom_flags, bad_lerror_flaglist), count )
if (count GT 0) then lower_confidence_limit[ind] = !VALUES.F_NAN


; Invalidate the fit result itself if certain anomalies were found.
ind = where(~param_was_frozen AND stregex(/BOOL, anom_flags, bad_fit_flaglist), count)
if (count GT 0) then begin
 param_value[ind] = !VALUES.F_NAN
endif 


; Hide errors when requested by caller.
if keyword_set(hide_errors_flag) then begin
  ind = where(hide_errors_flag, count)
  if (count GT 0) then begin
   anom_flags[ind] = 'hidden  '
   upper_confidence_limit[ind] = !VALUES.F_NAN 
   lower_confidence_limit[ind] = !VALUES.F_NAN 
  endif 
endif

; Mark the frozen cases.
ind = where( param_was_frozen, count )
if (count GT 0) then anom_flags[ind] = 'frozen    '
  
  
; Make some plots.
if keyword_set(plot) then begin
  dataset_1d,  id2,      param_value           , DATASET='best-fit'   , color='blue'  , BINSIZE=0.1   , XTIT=parameter_name
  dataset_1d,  id2,      upper_confidence_limit, DATASET='upper limit', color='red'   , BINSIZE=0.1 
  dataset_1d,  id2,      lower_confidence_limit, DATASET='lower-limit', color='yellow', BINSIZE=0.1
  function_1d, id1, seq, param_value           , DATASET='best-fit'   , color='blue'  , LINE=6, PSYM=2, YTIT=parameter_name
  function_1d, id1, seq, upper_confidence_limit, DATASET='upper-limit', color='red'   , LINE=6, PSYM=1, YTIT=parameter_name
  function_1d, id1, seq, lower_confidence_limit, DATASET='lower-limit', color='yellow', LINE=6, PSYM=1
endif

; Show sources with anomalies.
ind = where( stregex(/BOOL, anom_flags, '[a-zA-Z]' ), count )
if keyword_set(verbose) && (count GT 0) then begin
  print, count, parameter_name, F='(%"\nThese %d sources have one of the following anomalies on parameter %s:\n")'
  print, '  Reasons uncertainty on fit parameter is not reported:'
  print, '    o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
  print, '    s: error computation skipped' 
  print, '    f: parameter was frozen' 
  print, '    L: lowerlim outside specified range (soft limits in XSPEC)' 
  print, '    U: upperlim outside specified range (soft limits in XSPEC)' 
  print, '    l: tclout error "hit hard lower limit" OR "search failed in -ve direction"' 
  print, '    u: tclout error "hit hard upper limit" OR "search failed in +ve direction"' 
  print, '    c: tclout error "reduced chi-squared too high"' 
  print     
  print, '  Reasons fit parameter itself is not reported:'
  print, '    o: improper confidence interval, upperlim < best_val OR lowerlim > best_val' 
  print     
  print, '  Other anomalies:'
  print, '    r: parameter outside range specified in fitting script' 
  print, '    n: tclout error "non-monotonicity detected"' 
  print, '    m: tclout error "minimization may have run into problem"' 
  print

  forprint, seq, label, anom_flags, SUBSET=ind, F='(%"%5d %s:  %s")', TEXTOUT=2
endif

; Find a parameter value that produces equal numbers of confidence intervals that are above and below that value.
lind = where(finite(lower_confidence_limit), lcount)
uind = where(finite(upper_confidence_limit), ucount)
if (lcount EQ 0) && (ucount EQ 0) then begin
  bisector = !VALUES.F_NAN
endif else begin
  lower = lower_confidence_limit[lind]
  upper = upper_confidence_limit[uind]
  lower = lower[reverse(sort(lower))]
  upper = upper[        sort(upper)]
  
  lind = 0L ; index into vector "lower" defining upper limit on bisector
  uind = 0L ; index into vector "upper" defining lower limit on bisector
  ldone = 0B
  udone = 0B
  repeat begin
    ; Advance lind (reduce the bisector upper limit), but undo if we fall off the end of the array or the bisector range becomes improper.
    lind++
    if (lind GE lcount) || (lower[lind] LT upper[uind]) then begin
      lind--
      ldone = 1
    endif
    
    ; Advance uind (increase the bisector lower limit), but undo if we fall off the end of the array or the bisector range becomes improper.
    uind++
    if (uind GE ucount) || (lower[uind] LT upper[uind]) then begin
      uind--
      udone = 1
    endif
  endrep until (ldone && udone)
  
  bisector = mean([lower[uind],upper[uind]])
endelse ; search for bisector


;; ------------------------------------------------------------------------
; Build parameter strings.
errl = (param_value - lower_confidence_limit)
erru = (upper_confidence_limit - param_value)
par_str      = string( param_value,  F='(%" %0.1f")')
par_errl_str = string(errl, F='(%"_{-%0.1f}")')
par_erru_str = string(erru, F='(%"^{+%0.1f}")')

ind = where(strlen(par_str) EQ 4, count)
if (count GT 0) then begin
 par_str[ind] = '{\phn}' + par_str[ind]
endif

ind = where(param_was_frozen, count)
if (count GT 0) then begin
 par_str[ind] = par_str[ind] + '*'
endif

ind = where((errl < erru) LT 0.1, count)
if (count GT 0) then begin
 par_errl_str[ind] = string(errl[ind], F='(%"_{-%0.2f}")')
 par_erru_str[ind] = string(erru[ind], F='(%"^{+%0.2f}")')
endif

ind = where(~finite(errl) AND ~finite(erru), count)
if (count GT 0) then begin
 par_errl_str[ind] = ''
 par_erru_str[ind] = ''
endif

ind = where(~finite(errl) AND finite(erru), count)
if (count GT 0) then begin
 par_errl_str[ind] = '_{\cdots}'
endif

ind = where(~finite(erru) AND finite(errl), count)
if (count GT 0) then begin
 par_erru_str[ind] = '^{\cdots}'
endif
param_range = '$'+par_str+par_errl_str+par_erru_str+'$'

; Although the table generator would replace any 'NaN' strings with '\nodata', that mechanism won't work for these table cells
; because they are in LaTeX's math mode, and $\nodata$ is not legal syntax.
; Thus, we must explicitly change any null cells to '\nodata'.
ind = where(~finite(param_value), count)
if (count GT 0) then begin
 param_range[ind] = '\nodata'
endif

; Pad tails of strings with blanks to make them all the same length.
str_length = strlen(param_range)
pad_length = max(str_length) - str_length
pad = strmid('                          ', 0, pad_length)
param_range += pad

;; We need to return components of param_range so that we can build the entries for the emission measure from the NORM results.
return, { $
parameter_name        :parameter_name        ,$
label                 :label                 ,$
param_min             :param_min             ,$
param_max             :param_max             ,$
param_value           :param_value           ,$
lower_confidence_limit:lower_confidence_limit,$  
upper_confidence_limit:upper_confidence_limit,$  
errstatus             :errstatus             ,$
param_was_frozen      :param_was_frozen      ,$
anom_flags            :anom_flags            ,$
bisector              :bisector              ,$
par_str               :par_str               ,$
par_errl_str          :par_errl_str          ,$
par_erru_str          :par_erru_str          ,$
param_range           :param_range            $
}

end  ; validate_xspec_confidence_interval


;;; ==========================================================================
;;; hmsfr_tables
;;; ==========================================================================
;;; One of the table columns computed in this code is an "Effective Exposure" time for each source, 
;;; which is intended to convey the "depth" to which that source was observed in terms of some 
;;; reference configuration of the observatory (position on the detector and epoch of calibration).
;;; Authors are free to choose any reference configuration desired; a convenient choice is the PIMMS effective area
;;; (http://asc.harvard.edu/cgi-bin/build_viewer.cgi?ea) for a specific Chandra proposal cycle.

;;; Recall that for an exposure map computed at the monoenergy E
;;;   emap(x,y) = ARF(x,y,E) * EXPOSURE
;;;
;;; Thus, we choose to express the actual emap value for each source as a product of some reference effective area 
;;; and an effective exposure time:
;;;   emap(x,y) = ARF(PIMMS,E)           * effective_exposure
;;;             = nominal_effective_area * effective_exposure
;;;
;;; Thus, the "nominal_effective_area" parameter to this tool should be set to the Chandra-ACIS effective area, 
;;; at the energy for which your exposure map was built, for your desired reference configuration.
;;; For example, if your exposure map was built for 1.0 keV then you might set nominal_effective_area to the EA reported
;;; by PIMMS for a specific Chandra proposal cycle.

;;; The parameter "distance" must be in units of pc.
;;;
;;; The parameter SKY_OFFSET=[deltaX,deltaY] is the optional astrometric offset you wish to apply
;;; to all source positions, in units of CIAO sky pixels.  The RA & x axes have opposite signs
;;; so if you want to increase RA of the sources you supply a negative deltaX value.  
;;; The DEC and y axes have the same signs.
;;;
;;; SRC_SIGNIF_MIN can be supplied as a 2-vector of thresholds against the SRC_SIGNIF column. 
;;; The first value is used to omit sources from the spectroscopy tables.  
;;; The second value is used to suppress reporting of errors on fit parameters

PRO hmsfr_tables, summary_table_filename, template_filename, nominal_effective_area, distance, $
                  NO_SORT=no_sort, SORTED_TABLE_FILENAME=sorted_table_filename, $
                  SKY_OFFSET=sky_offset, QUIET=quiet, $
                  SRC_SIGNIF_MIN=src_signif_min, NET_COUNTS_MIN=net_counts_min

help, nominal_effective_area, distance
creator_string = "hmsfr_tables, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string

;; Check for common environment errors.
quiet = !QUIET  &  !QUIET = 1
catch, error_code
if (error_code NE 0) then begin
  print, 'ERROR: the IDL Astronomy Users Library is not in your IDL path.'
  retall
endif else resolve_routine, 'astrolib'
catch, /CANCEL
astrolib
; Make sure forprint calls do not block for user input.
!TEXTOUT=2
!QUIET = quiet
  
;; When log Nh is > this threshold we omit reporting soft fluxes.
logNh_threshold = 22.5

if (n_elements(src_signif_min) EQ 0) then src_signif_min=[0,0]
if (n_elements(src_signif_min) EQ 1) then src_signif_min=[src_signif_min,0]

if (n_elements(net_counts_min) EQ 0) then net_counts_min=0

;; ------------------------------------------------------------------------
;; Read summary table and check source names.
bt = mrdfits( summary_table_filename, 1, theader )
num_sources = n_elements(bt)
seq = 1+indgen(num_sources)

null_col = replicate('\nodata', num_sources)
null_val = replicate(!VALUES.F_NAN, num_sources)


;; Verify catalog is sorted by RA.
sort_ind = keyword_set(no_sort) ? indgen(num_sources) : bsort(bt.RA)
bt       = bt[sort_ind]
dum = where(sort_ind NE indgen(num_sources), count)
if (count GT 0) then print, 'WARNING!  This catalog was not sorted by RA!'


if keyword_set(sky_offset) then begin
  ; THE ADJUSTMENT OF POSITIONS AND NAMES MUST OCCUR AFTER THE SORTING ABOVE
  ; SINCE THE SORTED TABLE IS WRITTEN OVER THE INPUT FILENAME.

  ; Create astrometry conversion between sky & celestial coordinates, then
  ; adjust all the RA & DEC positions as specified & Rename the sources.
  make_astr, astr, CRVAL=[median(bt.RA),median(bt.DEC)], CRPIX=[1,1], DELT=[-0.000136667, 0.000136667]
  ad2xy, bt.RA, bt.DEC, astr, x, y
  xy2ad, x+sky_offset[0], y+sky_offset[1], astr, RA_FINAL, DEC_FINAL
  
  precision  = 1
  OBJECT_FINAL = strcompress(/REMOVE_ALL, adstring(RA_FINAL,DEC_FINAL,precision,/TRUNCATE))
  print, 'SKY_OFFSET=', sky_offset

  forprint, seq, bt.OBJECT, OBJECT_FINAL, F='(%"%4d %s -> %s")'
endif else begin
  RA_FINAL     = bt.RA
  DEC_FINAL    = bt.DEC
  OBJECT_FINAL = bt.OBJECT
endelse


; Use Latex math mode in object name.
CXOU = OBJECT_FINAL
for ii=0,num_sources-1 do begin
  name = strjoin( strsplit(CXOU[ii], /EXTRACT, '+'), '$+$')
  name = strjoin( strsplit(name,     /EXTRACT, '-'), '$-$')
  CXOU[ii] = name
endfor


;; Save the sorted table with extra columns for use in generating other tables, e.g. the counterparts table.
old_bt = bt
bt=replicate(create_struct( old_bt[0], 'RA_FINAL',0D,'DEC_FINAL',0D, 'OBJECT_FINAL','', 'CXOU',''), num_sources)
copy_struct,old_bt,bt
old_bt = 0
bt.RA_FINAL     = RA_FINAL
bt.DEC_FINAL    = DEC_FINAL
bt.OBJECT_FINAL = OBJECT_FINAL
bt.CXOU         = CXOU

if keyword_set(sorted_table_filename) then begin
  writefits,   sorted_table_filename, 0, headfits(summary_table_filename)
  mwrfits, bt, sorted_table_filename, theader
endif else print, 'Use SORTED_TABLE_FILENAME to specify an output file to hold sorted/offset table.' 

;; ------------------------------------------------------------------------
;; Handle some "flag" values from old AE runs that signify "no data".
ind = where(bt.PROB_KS EQ -1, count)
if (count GT 0) then bt[ind].PROB_KS =!VALUES.F_NAN


;; ------------------------------------------------------------------------
;; Verify that the expected photometry energy bands are in the expected place.
;; The AE default energy bands have been set to use the bands desired for HMSFRs
;; 0.5:8;  0.5:2, 2-8;  0.5:1.7, 1.7:2.8  2.8:8

band_full = 0
if max(abs([bt.ENERG_LO[band_full] - 0.5, bt.ENERG_HI[band_full] - 8.0])) GT 0.01 then begin
  print, band_full, bt[0].ENERG_LO[band_full], bt[0].ENERG_HI[band_full], F='(%"\nWARNING: Full Band (#%d) is %0.1f:%0.1f keV")'
endif

band_2000_8000 = 2
if max(abs([bt.ENERG_LO[band_2000_8000] - 2.0, bt.ENERG_HI[band_2000_8000] - 8.0])) GT 0.01 then begin
  print, band_2000_8000, bt[0].ENERG_LO[band_2000_8000], bt[0].ENERG_HI[band_2000_8000], F='(%"\nWARNING: Hard Band (#%d) is %0.1f:%0.1f keV")'
endif
stop
goto, SKIP_HRS

band_500_2000 = 1
if max(abs([bt.ENERG_LO[band_500_2000] - 0.5, bt.ENERG_HI[band_500_2000] - 2.0])) GT 0.01 then begin
  print, band_500_2000, F='(%"\nERROR: Band %d is not 0.5:2.0 keV")'
  return
endif

band_500_1700 = 3
if max(abs([bt.ENERG_LO[band_500_1700] - 0.5, bt.ENERG_HI[band_500_1700] - 1.7])) GT 0.01 then begin
  print, band_500_1700, F='(%"\nERROR: Band %d is not 0.5:1.7 keV")'
  return
endif

band_1700_2800 = 4
if max(abs([bt.ENERG_LO[band_1700_2800] - 1.7, bt.ENERG_HI[band_1700_2800] - 2.8])) GT 0.01 then begin
  print, band_1700_2800, F='(%"\nERROR: Band %d is not 1.7:2.8 keV")'
  return
endif

band_2800_8000 = 5
if max(abs([bt.ENERG_LO[band_2800_8000] - 2.8, bt.ENERG_HI[band_2800_8000] - 8.0])) GT 0.01 then begin
  print, band_2800_8000, F='(%"\nERROR: Band %d is not 2.8:8.0 keV")'
  return
endif


;; ------------------------------------------------------------------------
;; Here we use NET_CTS entries in pairs of photometry table rows to make HR = (hard_cnts - soft_cnts)/(hard_cnts + soft_cnts)
;; Compute hardness upper and lower 1-sigma errors using equation 1.31 in 
; "A Practical Guide to Data Analysis for Physical Science Students", L. Lyons, 1991.
hard_band = [band_2000_8000, band_1700_2800, band_2800_8000]
soft_band = [band_500_2000 , band_500_1700 , band_1700_2800]

for ii = 0, n_elements(hard_band)-1 do begin
  print, bt[0].ENERG_LO[soft_band[ii]], bt[0].ENERG_HI[soft_band[ii]], $
         bt[0].ENERG_LO[hard_band[ii]], bt[0].ENERG_HI[hard_band[ii]], $
         F='(%"Hardness Ratio using bands %3.1f:%3.1f and %3.1f:%3.1f")'

  ; Recall that NET_CNTS can be negative.  
  ; To ensure that HRs are bounded by [-1,1] we choose to clip such NET_CNTS entries at zero
  ; and choose to set their lower errors to zero.  
  ; We will later do similar clipping at zero when we offset NET_CNTS downward by NET_CNTS_SIGMA_LOW
  ; during Lyon's error propagation.
  
  hard_cnts = float(bt.NET_CNTS[hard_band[ii]]) > 0
  soft_cnts = float(bt.NET_CNTS[soft_band[ii]]) > 0
  
  hard_sigma_up   = bt.NET_CNTS_SIGMA_UP[hard_band[ii]]
  soft_sigma_up   = bt.NET_CNTS_SIGMA_UP[soft_band[ii]]

  hard_sigma_low  = bt.NET_CNTS_SIGMA_LOW[hard_band[ii]]
  soft_sigma_low  = bt.NET_CNTS_SIGMA_LOW[soft_band[ii]]
  
  ; Compute some metrics & flags to identify various conditions where the HRs and/or their errors are not reliable.
  is_undefined       = ((hard_cnts - hard_sigma_low) LE 0) AND ((soft_cnts - soft_sigma_low) LE 0)
  is_very_hard       = ((hard_cnts - hard_sigma_low) GT 0) AND ((soft_cnts - soft_sigma_low) LE 0)
  is_very_soft       = ((hard_cnts - hard_sigma_low) LE 0) AND ((soft_cnts - soft_sigma_low) GT 0)

  ; The Nx5 arrays "hard" and "soft" below will simplify the 5 evaluations of our function ((h-s)/(h+s)) needed  
  ; to do the Lyons calculations for upper and lower sigmas.
  ; 0th entry is nominal HR 
  ; 1st entry is where hard fluxuates upward,   HR fluxuates upward
  ; 2nd entry is where soft fluxuates upward,   HR fluxuates downward
  ; 3rd entry is where hard fluxuates downward, HR fluxuates downward
  ; 4rd entry is where soft fluxuates downward, HR fluxuates upward
  
  hard_cnts_more = hard_cnts + hard_sigma_up
  soft_cnts_more = soft_cnts + soft_sigma_up

  ; To retain range of HR we clip downward fluxuations in NET_CNTS at zero.
  hard_cnts_less = (hard_cnts - hard_sigma_low) > 0
  soft_cnts_less = (soft_cnts - soft_sigma_low) > 0
  
  hard = [[hard_cnts], [hard_cnts_more], [hard_cnts     ], [hard_cnts_less], [hard_cnts     ]]
  soft = [[soft_cnts], [soft_cnts     ], [soft_cnts_more], [soft_cnts     ], [soft_cnts_less]]
  
  ratios = (hard - soft) / (hard + soft)
  
  hr           = ratios[*,0]

  ; There's a questions here!  Lyon's book seems to say that upper sigma for f is computed using terms (1&2) where the 
  ; input parameters both fluxuate up, and lower sigma for f uses terms (3&4) where parameters fluxuate down.
  ; However it seems more reasonable to estimate the upper limit on f using the terms where f fluxuates upward (1&4)
  ; and estimate the lower limit on f using terms where f fluxuates downward (2&3).
  ; Talk to Niel and Eric about this!!!
  
  hr_sigma_up  = sqrt( (ratios[*,1] - hr)^2 + $
                       (ratios[*,2] - hr)^2 )

  hr_sigma_low = sqrt( (ratios[*,3] - hr)^2 + $
                       (ratios[*,4] - hr)^2 )


  ;; Insert !VALUES.F_NAN any place we want \nodata in the table.
  
  ; Handle ratios whose confidence interval includes +-1 (because confidence interval of hard or soft includes 0).
  ind = where(is_very_hard,count)
  if (count GT 0) then begin
    print, count, ' sources are very hard.'
    hr_sigma_up [ind] = !VALUES.F_NAN
  endif
  
  ind = where(is_very_soft,count)
  if (count GT 0) then begin
    print, count, ' sources are very soft.'
    hr_sigma_low[ind] = !VALUES.F_NAN
  endif

  ; Handle cases where we don't want to report the HR at all.
  ind = where(is_undefined,count)
  if (count GT 0) then begin
    print, count, ' sources have unreliable HR values.'
    hr          [ind] = !VALUES.F_NAN
    hr_sigma_up [ind] = !VALUES.F_NAN
    hr_sigma_low[ind] = !VALUES.F_NAN
  endif

  
  ; Save the results in named variables.
  case ii of
   0: begin
      hr1           = hr
      hr1_sigma_up  = hr_sigma_up
      hr1_sigma_low = hr_sigma_low
  is_undefined1       = is_undefined
  is_very_hard1       = is_very_hard
  is_very_soft1       = is_very_soft 
      end
   1: begin
      hr2           = hr
      hr2_sigma_up  = hr_sigma_up
      hr2_sigma_low = hr_sigma_low
  is_undefined2       = is_undefined
  is_very_hard2       = is_very_hard
  is_very_soft2       = is_very_soft 
      end
   2: begin
      hr3           = hr
      hr3_sigma_up  = hr_sigma_up
      hr3_sigma_low = hr_sigma_low
  is_undefined3       = is_undefined
  is_very_hard3       = is_very_hard
  is_very_soft3       = is_very_soft 
      end
  endcase
endfor ; ii

;; COUP SPECIAL CODE:
save, FILE='hr.sav', seq, OBJECT_FINAL, $
      is_undefined1, is_very_hard1, is_very_soft1, hr1, hr1_sigma_up, hr1_sigma_low, $
      is_undefined2, is_very_hard2, is_very_soft2, hr2, hr2_sigma_up, hr2_sigma_low, $
      is_undefined3, is_very_hard3, is_very_soft3, hr3, hr3_sigma_up, hr3_sigma_low

SKIP_HRS:

;; ------------------------------------------------------------------------
;; Verify the spectral model is as expected.
;; ???????? Look at bt.MODEL

null_flag = replicate('.',num_sources)

;; ------------------------------------------------------------------------
;; Compute columns for spectral tables.
nh_offset     = 22
cm_per_parsec = 3.086D18
dscale        = 4D*!PI*(distance * cm_per_parsec)^2
em_offset     = alog10(1E14 * dscale)
lx_offset     = alog10(dscale)
help, distance, dscale, lx_offset
         
; Hide errors when the source significance is small (since we don't trust them much)
hide_errors_flag = bt.SRC_SIGNIF[band_full] LT src_signif_min[1]
         
;; Examine NH results.
if (total(strmatch(tag_names(bt),'NH')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
  NH = validate_xspec_confidence_interval( 'NH', bt.LABEL, nh_offset + alog10(bt.NH0_MIN), nh_offset + alog10(bt.NH0_MAX), nh_offset + alog10(bt.NH), nh_offset + alog10(bt.NH_ERRL), nh_offset + alog10(bt.NH_ERRU), bt.NH_ERRST, bt.NH_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
endif                  


;; Examine KT results.
if (total(strmatch(tag_names(bt),'KT')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
  KT = validate_xspec_confidence_interval( 'KT', bt.LABEL, bt.KT0_MIN, bt.KT0_MAX, bt.KT, bt.KT_ERRL, bt.KT_ERRU, bt.KT_ERRST, bt.KT_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
endif
  
  
;; Examine Photon Index results.
if (total(strmatch(tag_names(bt),'PH')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
  PH = validate_xspec_confidence_interval( 'PH', bt.LABEL, bt.PH0_MIN, bt.PH0_MAX, bt.PH, bt.PH_ERRL, bt.PH_ERRU, bt.PH_ERRST, bt.PH_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
endif
  

;; Examine NORM results.
if (total(strmatch(tag_names(bt),'NORM')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
  NORM = validate_xspec_confidence_interval( 'NORM', bt.LABEL, alog10(1E-20), alog10(1E10), alog10(bt.NORM), alog10(bt.NORMERRL), alog10(bt.NORMERRU), bt.NORMERRS, bt.NORM_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
  
  ; Convert NORM to emission measure for thermal models.
  par_str      = string(  em_offset + NORM.param_value,  F='(%" %0.1f")')
  em_range = '$'+par_str+NORM.par_errl_str+NORM.par_erru_str+'$'

  ; Although the table generator would replace any 'NaN' strings with '\nodata', that mechanism won't work for these table cells
  ; because they are in LaTeX's math mode, and $\nodata$ is not legal syntax.
  ; Thus, we must explicitly change any null cells to '\nodata'.
  ind = where(~finite(NORM.param_value), count)
  if (count GT 0) then begin
    em_range[ind] = '\nodata'
  endif
  
  ; Pad tails of strings with blanks to make them all the same length.
  str_length = strlen(em_range)
  pad_length = max(str_length) - str_length
  pad = strmid('                          ', 0, pad_length)
  em_range += pad
endif

  


abundance_min =  0
abundance_max = 10
abundance_range = strarr(num_sources)

;; Examine KT results.
if (total(strmatch(tag_names(bt),'O_ERRS')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
    O = validate_xspec_confidence_interval( 'O', bt.LABEL, abundance_min, abundance_max, bt.O, bt.O_ERRL, bt.O_ERRU, bt.O_ERRS, bt.O_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
    abundance_range += ' & '+O.param_range
endif
  
  
;; Examine KT results.
if (total(strmatch(tag_names(bt),'NE_ERRS')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
    ; Variables and structure tags cannot have the name "NE" in IDL, thus "_NE" is used instead.
    _NE = validate_xspec_confidence_interval( 'NE', bt.LABEL, abundance_min, abundance_max, bt._NE, bt.NE_ERRL, bt.NE_ERRU, bt.NE_ERRS, bt.NE_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
    abundance_range += ' & '+_NE.param_range
endif
  
  
;; Examine KT results.
if (total(strmatch(tag_names(bt),'MG_ERRS')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
    MG = validate_xspec_confidence_interval( 'MG', bt.LABEL, abundance_min, abundance_max, bt.MG, bt.MG_ERRL, bt.MG_ERRU, bt.MG_ERRS, bt.MG_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
    abundance_range += ' & '+MG.param_range
endif
  
  
;; Examine KT results.
if (total(strmatch(tag_names(bt),'SI_ERRS')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
    SI = validate_xspec_confidence_interval( 'SI', bt.LABEL, abundance_min, abundance_max, bt.SI, bt.SI_ERRL, bt.SI_ERRU, bt.SI_ERRS, bt.SI_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
    abundance_range += ' & '+SI.param_range
endif
  
  
;; Examine KT results.
if (total(strmatch(tag_names(bt),'FE_ERRS')) GT 0) then begin
  ; Validate the parameter confidence interval produced by the fitting script.
  ; Confidence limits that should be ignored are set to NaN by the routine validate_xspec_confidence_interval.
    FE = validate_xspec_confidence_interval( 'FE', bt.LABEL, abundance_min, abundance_max, bt.FE, bt.FE_ERRL, bt.FE_ERRU, bt.FE_ERRS, bt.FE_FZ, HIDE_ERRORS_FLAG=hide_errors_flag, /PLOT, /VERBOSE )
    abundance_range += ' & '+FE.param_range
endif
  
  

CATALOG_NAME   = strtrim(bt.CATALOG_NAME,2)
MODEL          = strtrim(bt.MODEL,2)
net_counts     = round(  bt.NET_CNTS            [band_full] )
median_e       =         bt.ENERG_PCT50_OBSERVED[band_full]
SRC_SIGNIF     =         bt.SRC_SIGNIF          [band_full]
PROB_NO_SOURCE =         bt.PROB_NO_SOURCE      [band_full]



;; ------------------------------------------------------------------------
;; Process the template.
tempfilename = 'temp.tex'

openw, demo_unit, 'demo.tex', /GET_LUN
openr, in_unit,  template_filename, /GET_LUN

table_name = ''
line = ''
while NOT eof(in_unit) do begin
  readf, in_unit, line
  
  if (table_name EQ '') then begin
    ; Processing template outside of a table.
    if strmatch(line, 'TEMPLATE*') then begin
      ; Open a new table (name parsed from TEMPLATE line) 
      table_name = (strsplit(line,/EXTRACT))[1]

      print
      print, 'opening table ', table_name
      openw, table_unit, tempfilename, /GET_LUN
      
      ;get_date, date, /TIMETAG
      comment = "% Written by "+creator_string + systime()
      printf, table_unit, comment
      
      ; If table_name has any "_"s they must be escaped for LaTeX.
      printf, demo_unit, table_name, F='(%"\\input{%s}")'

    endif else printf, demo_unit, line

  endif else begin
    ; Processing a table
    if strmatch(line, 'END*') then begin
      ; End of the table found.
      print, 'closing table ', table_name
      free_lun, table_unit
      
      ; Convert "NaN" to \nodata command.
      spawn, string(tempfilename, table_name+'.tex', F='(%"sed ''s/   NaN /\\\\nodata/g'' %s >! %s")')

      table_name = ''
      continue
    endif

    ; Copy line to table file.
    printf, table_unit, line
    
    if strmatch(line, '\\startdata*') then begin
      ; Parse data format spec from template .
      fmt = ''
      while 1 do begin
        readf, in_unit, line
        if strmatch(line, '\\enddata*') then break
        fmt = fmt + line
      endwhile
      print, 'FORMAT= '+fmt
      
      ; Now write out the data.
      case table_name of
       ;--------------------------------------------------------------------------------------------
       'src_properties': begin
                  
         prob_no_source_string = string(alog10(PROB_NO_SOURCE), F='(%"%4.1f")')
         ind = where(PROB_NO_SOURCE LT 1E-5, count)
         if (count GT 0) then begin
           prob_no_source_string[ind] = "$<$-5"
         endif
         
         
         off_chip = null_flag
         ind = where(bt.FRACEXPO LT 0.9, count)
         if (count GT 0) then off_chip[ind] = 'g'
         
         streak = null_flag
         if (total(strmatch(tag_names(bt),'WARNFRAC')) GT 0) then begin
           ind = where(bt.WARNFRAC GT 0.1, count)
           if (count GT 0) then off_chip[ind] = 's'
         endif else print, 'WARNING!  The "streak" flag cannot be computed.'
                  
         anom_flags = off_chip+null_flag+null_flag+streak
         
         
         ; Convert PROB_KS in three-level flag.
         ; Omit KS result when off_chip flag set.
         var_flag = replicate('a',num_sources)
         
         ind = where(bt.PROB_KS LT 0.05, count)
         if (count GT 0) then var_flag[ind] = 'b'
         
         ind = where(bt.PROB_KS LT 0.005, count)
         if (count GT 0) then var_flag[ind] = 'c'
         
         ind = where(~finite(bt.PROB_KS) OR (off_chip NE null_flag), count)
         if (count GT 0) then var_flag[ind] = '\nodata'

         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq, bt.CXOU, bt.RA_FINAL, bt.DEC_FINAL, bt.ERR_DATA, bt.THETA, $
         
         bt.NET_CNTS[band_full], (bt.NET_CNTS_SIGMA_UP[band_full] + bt.NET_CNTS_SIGMA_LOW[band_full])/2, $
         
         (bt.BKG_CNTS[band_full] / bt.BACKSCAL[band_full]), bt.NET_CNTS[band_2000_8000] > 0, bt.PSF_FRAC, $
         
         SRC_SIGNIF, prob_no_source_string, anom_flags, var_flag, (bt.EMAP_TOT/nominal_effective_area)/1000., $
         
         median_e, F=fmt

         ;; Save some important columns for the observer's convenience.

         save, seq, CATALOG_NAME, OBJECT_FINAL, MODEL, RA_FINAL, DEC_FINAL, net_counts, SRC_SIGNIF, PROB_NO_SOURCE, median_e,  FILE='src_properties.sav'
         end
         

       ;--------------------------------------------------------------------------------------------
       'thermal_spectroscopy': begin

         ; Determine which sources belong in this table.  We process both tbabs_vapec and tbabs_2vapec models.
         in_this_table = (bt.SRC_SIGNIF[band_full] GE src_signif_min[0]) AND $
                         (bt.NET_CNTS  [band_full] GE net_counts_min) AND $
                          (strmatch(bt.MODEL, '*apec*'))
         
         ind = where( in_this_table, thermal_count )
         if (thermal_count EQ 0) || ~keyword_set(KT) then begin
           print, 'NO sources in thermal_spectroscopy table'
           goto, END_thermal_spectroscopy
         endif


         ; Work will full-length vectors so user can find index for specific source in plotting tools.  
         ; Sources not in this table with have NaN values.
         NH_val     = NH.param_value
         KT_val     = KT.param_value
         F0P5_2 = alog10(bt.F0P5_2)
         F2_8   = alog10(bt.F2_8)
         FC2_8  = alog10(bt.FC2_8)
         F0P5_8 = alog10(bt.F0P5_8)
         FC0P5_8= alog10(bt.FC0P5_8)
         
         ind = where(in_this_table EQ 0, count)
         if (count GT 0) then begin
           NH_val     [ind] = !VALUES.F_NAN
           KT_val     [ind] = !VALUES.F_NAN
           F0P5_2 [ind] = !VALUES.F_NAN
           F2_8   [ind] = !VALUES.F_NAN
           FC2_8  [ind] = !VALUES.F_NAN
           F0P5_8 [ind] = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         ;; Suppress flux columns when any parameter has no fit result.
         invalid_fit = in_this_table AND ((finite(NH.param_value) AND finite(KT.param_value) AND finite(NORM.param_value)) EQ 0)
         ind = where(invalid_fit, count)
         if (count GT 0) then begin
           print, count, thermal_count, F='(%"\nThese %d (out of %d total) thermal sources have no good estimate for at least one fit parameter:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           F2_8[ind]    = !VALUES.F_NAN
           FC2_8[ind]   = !VALUES.F_NAN
           F0P5_8[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         ;; Suppress soft flux columns when Nh is large.
         invalid_flux = in_this_table AND (NH.param_value GT logNh_threshold)
         ind = where(invalid_flux, count)
         if (count GT 0) then begin
           print, count, thermal_count, logNh_threshold, F='(%"\nThese %d (out of %d total) thermal sources have log Nh > %f:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         

         notes = null_col
         ind = where( strmatch(bt.MODEL, '*2*apec*'), count )
         if (count GT 0) then notes[ind] = '2T'
         
         ;; Write out table.
         ind = where( in_this_table, count )

         
         L0P5_2  = lx_offset+(F0P5_2)
         L2_8    = lx_offset+(F2_8)
         LC2_8   = lx_offset+(FC2_8)
         L0P5_8  = lx_offset+(F0P5_8)
         LC0P5_8 = lx_offset+(FC0P5_8)
         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq[ind], CXOU[ind], $
         (bt[ind]).NET_CNTS[band_full], (bt[ind]).SRC_SIGNIF[band_full], $
         NH.param_range[ind],  KT.param_range[ind], em_range[ind], abundance_range[ind], $
 
         (L0P5_2[ind]), (L2_8[ind]), (LC2_8[ind]), (L0P5_8[ind]), (LC0P5_8[ind]), notes[ind], F=fmt

         print

         ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
         !TEXTUNIT = 0

         if keyword_set(sky_offset) then begin
           print, ' seq    old name              new name            NH            kT         Norm            NC Emed model'
           forprint, seq[ind], bt[ind].OBJECT, bt[ind].OBJECT_FINAL, NH.anom_flags[ind], KT.anom_flags[ind], NORM.anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s -> %s  %s  %s  %s  %4d %3.1f %s")'

         endif else begin
           print, ' seq         name            NH          kT         Norm      NC  Emed      model'
           forprint, seq[ind],                      bt[ind].OBJECT_FINAL, NH.anom_flags[ind], KT.anom_flags[ind], NORM.anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s  %s  %s  %s  %4d %3.1f %s")'
         endelse
         
         if NOT keyword_set(quiet) then begin
           dsn = 'thermal model'

           dataset_1d,id1, NH_val,      XTIT='log Nh', DATASET=dsn
           dataset_1d,id2, KT_val,      XTIT='kT'
           dataset_1d,id3, LC0P5_8, XTIT='log Lc[0.5:8]', DATASET=dsn
           dataset_1d,id4, F0P5_8,  XTIT='log F[0.5:8]', DATASET=dsn

           dataset_2d,id5, NH_val, KT_val, XTIT='log Nh', YTIT='kT', PSYM=1, NAN=[0,-1]
           
           dataset_2d,id6, alog10(net_counts), NH_val,      XTIT='log NET_CNTS', YTIT='log Nh', PSYM=1, NAN=[1,0], DATASET=dsn
           dataset_2d,id7, alog10(net_counts), KT_val,      XTIT='log NET_CNTS', YTIT='kT', PSYM=1, NAN=[1,-5]
           dataset_2d,id8, alog10(net_counts), LC0P5_8, XTIT='log NET_CNTS', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[1,20], DATASET=dsn
           dataset_2d,id9, alog10(net_counts), F0P5_8,  XTIT='log NET_CNTS', YTIT='log F[0.5:8]', PSYM=1, NAN=[1,-20], DATASET=dsn

           dataset_2d,id10, NH_val, LC0P5_8,         XTIT='log Nh', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[0,20], DATASET=dsn
           dataset_2d,id11, KT_val, LC0P5_8,         XTIT='kT', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[-5,20]

           chi_sqr      = null_val
           cstat       = null_val
           if (total(strmatch(tag_names(bt),'CHI_SQR')) GT 0) then begin
             chi_sqr[ind] = bt[ind].CHI_SQR
             dataset_2d,id12, net_counts, chi_sqr, XTIT='NET_CNTS', YTIT='Reduced chi^2', PSYM=1, DATASET=dsn, NAN=[0,0]
           endif

           if (total(strmatch(tag_names(bt),'CSTAT')) GT 0) then begin
             cstat[ind] = bt[ind].CSTAT
           endif
           
           save, seq, NH_val, KT_val, $
             F0P5_2, F2_8, FC2_8, F0P5_8, FC0P5_8, $
             L0P5_2, L2_8, LC2_8, L0P5_8, LC0P5_8, chi_sqr, cstat, FILE='thermal_spectroscopy.sav'
         endif

END_thermal_spectroscopy:         
         end ;thermal_spectroscopy

         
       ;--------------------------------------------------------------------------------------------
       'powerlaw_spectroscopy': begin

         ; Determine which sources belong in this table.
         in_this_table = (bt.SRC_SIGNIF[band_full] GE src_signif_min[0]) AND $
                         (bt.NET_CNTS  [band_full] GE net_counts_min) AND $
                          strmatch(bt.MODEL, '*tbabs_pow*')
         
         ind = where( in_this_table, powerlaw_count )
         if (powerlaw_count EQ 0) || ~keyword_set(PH) then begin
           print, 'NO sources in powerlaw_spectroscopy table'
           goto, END_powerlaw_spectroscopy
         endif


         ; Work will full-length vectors so user can find index for specific source in plotting tools.  
         ; Sources not in this table with have NaN values.
         NH_val     = NH.param_value
         PH_val     = PH.param_value
         F0P5_2 = alog10(bt.F0P5_2)
         F2_8   = alog10(bt.F2_8)
         FC2_8  = alog10(bt.FC2_8)
         F0P5_8 = alog10(bt.F0P5_8)
         FC0P5_8= alog10(bt.FC0P5_8)
         
         ind = where(in_this_table EQ 0, count)
         if (count GT 0) then begin
           NH_val     [ind] = !VALUES.F_NAN
           PH_val     [ind] = !VALUES.F_NAN
           F0P5_2 [ind] = !VALUES.F_NAN
           F2_8   [ind] = !VALUES.F_NAN
           FC2_8  [ind] = !VALUES.F_NAN
           F0P5_8 [ind] = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         ;; Suppress flux columns when any parameter has no fit result.
         invalid_fit = in_this_table AND ((finite(NH.param_value) AND finite(PH.param_value) AND finite(NORM.param_value)) EQ 0)
         ind = where(invalid_fit, count)
         if (count GT 0) then begin
           print, count, powerlaw_count, F='(%"\nThese %d (out of %d total) powerlaw sources have no good estimate for at least one fit parameter:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, bt[ind].OBJECT
           
           F0P5_2[ind]  = !VALUES.F_NAN
           F2_8[ind]    = !VALUES.F_NAN
           FC2_8[ind]   = !VALUES.F_NAN
           F0P5_8[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         ;; Suppress soft flux columns when Nh is large.
         invalid_flux = in_this_table AND (NH.param_value GT logNh_threshold)
         ind = where(invalid_flux, count)
         if (count GT 0) then begin
           print, count, thermal_count, logNh_threshold, F='(%"\nThese %d (out of %d total) powerlaw sources have log Nh > %f:")'

           ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
           !TEXTUNIT = 0
           forprint, 1+ind, bt[ind].OBJECT, F='(%"%4d %s")'
           
           F0P5_2[ind]  = !VALUES.F_NAN
           FC0P5_8[ind] = !VALUES.F_NAN
         endif
         
         
         notes = null_col
         
         
         ;; Write out table.
         ind = where( in_this_table, count )

         
         L0P5_2  = lx_offset+(F0P5_2)
         L2_8    = lx_offset+(F2_8)
         LC2_8   = lx_offset+(FC2_8)
         L0P5_8  = lx_offset+(F0P5_8)
         LC0P5_8 = lx_offset+(FC0P5_8)
         
         !TEXTUNIT = table_unit
         forprint, TEXTOUT=5, /NoCOMMENT, seq[ind], CXOU[ind], $
         (bt[ind]).NET_CNTS[band_full], (bt[ind]).SRC_SIGNIF[band_full], $
         NH.param_range[ind],  PH.param_range[ind], NORM.param_range[ind], $
 
         (L0P5_2[ind]), (L2_8[ind]), (LC2_8[ind]), (L0P5_8[ind]), (LC0P5_8[ind]), notes[ind], F=fmt

         print


         ; We have to "give up" !TEXTUNIT to prevent the forprint below from closing table_unit.
         !TEXTUNIT = 0

         if keyword_set(sky_offset) then begin
           print, ' seq    old name              new name            NH          gamma   Norm            NC Emed model'
           forprint, seq[ind], bt[ind].OBJECT, bt[ind].OBJECT_FINAL, NH.anom_flags[ind], PH.anom_flags[ind], NORM.anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s -> %s  %s  %s  %s  %4d %3.1f %s")'

         endif else begin
           print, ' seq         name          NH        gamma        Norm      NC  Emed      model'
           forprint, seq[ind],                      bt[ind].OBJECT_FINAL, NH.anom_flags[ind], PH.anom_flags[ind], NORM.anom_flags[ind], net_counts[ind], median_e[ind], bt[ind].MODEL, F='(%"%4d %s  %s  %s  %s  %4d %3.1f %s")'
         endelse
         
         if NOT keyword_set(quiet) then begin
           ; Plot will full-length vectors so user can find index for specific source.
           dsn = 'powerlaw model'

           dataset_1d,id1, NH_val,      XTIT='log Nh', DATASET=dsn
           dataset_1d,id102, PH_val,    XTIT='photon index'
           dataset_1d,id3, LC0P5_8, XTIT='log Lc[0.5:8]', DATASET=dsn
           dataset_1d,id4, F0P5_8,  XTIT='log F[0.5:8]', DATASET=dsn

           dataset_2d,id105, NH_val, PH_val, XTIT='log Nh', YTIT='photon index', PSYM=1, NAN=[0,-1]
           
           dataset_2d,id6, net_counts, NH_val,      XTIT='NET_CNTS', YTIT='log Nh', PSYM=1, NAN=[1,0], DATASET=dsn
           dataset_2d,id107, net_counts, PH_val,    XTIT='NET_CNTS', YTIT='photon index', PSYM=1, NAN=[-1,0]
           dataset_2d,id8, net_counts, LC0P5_8, XTIT='NET_CNTS', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[1,20], DATASET=dsn
           dataset_2d,id9, net_counts, F0P5_8,  XTIT='NET_CNTS', YTIT='log F[0.5:8]', PSYM=1, NAN=[1,-20], DATASET=dsn

           dataset_2d,id10, NH_val, LC0P5_8,         XTIT='log Nh', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[0,20], DATASET=dsn
           dataset_2d,id111, PH_val, LC0P5_8,        XTIT='photon index', YTIT='log Lc[0.5:8]', PSYM=1, NAN=[-1,0]

           chi_sqr      = null_val
           cstat       = null_val
           if (total(strmatch(tag_names(bt),'CHI_SQR')) GT 0) then begin
             chi_sqr[ind] = bt[ind].CHI_SQR
             dataset_2d,id12, net_counts, chi_sqr, XTIT='NET_CNTS', YTIT='Reduced chi^2', PSYM=1, DATASET=dsn, NAN=[0,0]
           endif
           
           if (total(strmatch(tag_names(bt),'CSTAT')) GT 0) then begin
             cstat[ind] = bt[ind].CSTAT
           endif
           
           save, seq, NH_val, PH_val, $
             F0P5_2, F2_8, FC2_8, F0P5_8, FC0P5_8, $
             L0P5_2, L2_8, LC2_8, L0P5_8, LC0P5_8, chi_sqr, cstat, FILE='powerlaw_spectroscopy.sav'
         endif

END_powerlaw_spectroscopy:         
         end ;powerlaw_spectroscopy
 
 
        else: print, 'No definition found for a table named '+table_name
      endcase
      printf, table_unit, line  ;This is the \enddata line.
    endif ; data section 
    
  endelse ; processing a table
endwhile

free_lun, demo_unit, in_unit
print
print, 'To test tables run:'
print, '  latex demo; dvips -o demo.ps demo; gv --orientation=landscape --scale=2 demo.ps&'
return
end ; hmsfr_tables




;;; ==========================================================================
;;; ae_make_movie
;;; ==========================================================================
; Find Floating underflow; run with /VERBOSE & check flatness of flux & energy; get MPEG license and try MPEG output; energy legend; look at color of overlapping sources; 

; See if /MOTION_VEC reduces file size.

;;; .run acis_extract_tools
;;; ae_make_movie,'theta.cat','1874'
;;; ae_make_movie, SCENE_TEMPLATE='obs1874/data/central_1.emap', $
;;;                SCENE_PARAM_FILE='scene.txt, JPEG_BASENAME='theta'
;;; OR
;;; ae_make_movie,'theta.cat','1874'
;;; ae_make_movie, SCENE_TEMPLATE='obs1874/data/central_1.emap', NUM_FRAMES=100, JPEG_BASENAME='theta'

;;; SCENE_PARAM_FILE is 4 column ASCII describing the scene in each frame:
;;;   time_fraction: [0:1] time tag of frame as fraction in interval [TSTART:TSTOP]
;;;   deltaX, deltaY: panning offset in arcseconds
;;;   deltaScale:     multiplicative adjustment to CDELT (degrees/pixel) (e.g. 1.01 means zoom out 1%)

PRO ae_make_movie, catalog_or_srclist, obsname, MIN_COVERAGE=min_coverage, $
                   EXTRACTION_NAME=extraction_name, MERGE_NAME=merge_name, $
  
                   SCENE_TEMPLATE=scene_template, SCENE_PARAM_FILE=scene_param_file, NUM_FRAMES=num_frames, $
                   MPEG_FILENAME=mpeg_filename, JPEG_BASENAME=jpeg_basename, $
                   FWHM=fwhm, SATURATION=saturation, INVERT=invert, $
                   SHOW_FRAMES=show_frames, VERBOSE=verbose

COMMON ae_make_movie, num_sources, sources

creator_string = "ae_make_movie, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()
print, 'http://www.astro.psu.edu/xray/docs/TARA/ae_users_guide.html'
print, 'patb@astro.psu.edu'
print
print, 'Note the following limitations to this method:'
print, '* Light curves and median energies are NOT background subtracted.'
print, '* Sources on multiple CCDs are skipped.'
print, '* Multiple observations are not supported.'
print, '* Bad time intervals are interpolated over.'
print

if (n_elements(min_coverage) NE 1) then min_coverage=0.5
if NOT keyword_set(saturation)   then saturation=0.25

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,3000)

if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                else merge_subdir = ''
if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)


type = size(obsname,/TNAME)
if (type NE 'UNDEFINED') AND (type NE 'STRING') then begin
  print, 'parameter "obsname" must be a string'
  return
endif

src_stats_basename       = 'source.stats'
lc_smooth_basename       = 'source.smooth_lc'


color_manager

;; =============================================================================
;; READ ALL THE SOURCE INFORMATION INTO MEMORY.

if keyword_set(catalog_or_srclist) then begin
  
  if keyword_set(sources) then begin
    ptr_free, sources.TIME, sources.COUNT_RATE, sources.MEDIAN_ENERGY, sources.psf_img
    dum = temporary(sources)
  endif
 
  ;; Input catalog should be an ascii file with source names, e.g. output of 
  ;; prior call with /CHOOSE_REGIONS.
  readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

  ; Trim whitespace and remove blank lines.
  sourcename = strtrim(sourcename,2)
  ind = where(sourcename NE '', num_sources)
  
  if (num_sources EQ 0) then begin
    print, 'ERROR: no entries read from source list ', catalog_or_srclist
    retall
  endif
  
  sourcename = sourcename[ind]
  print, num_sources, F='(%"\n%d sources found in catalog.\n")'
  
  st = {NAME:'', ra:0D, dec:0D, $
        TIME:ptr_new(), COUNT_RATE:ptr_new(), MEDIAN_ENERGY:ptr_new(), $
        nominal_rate:0.0, nominal_energy:0.0, min_energy:0.0, max_energy:0.0, xl:0, yl:0, psf_img:ptr_new()}
  sources = replicate(st, num_sources)


  for ii = 0, num_sources-1 do begin
    sources[ii].NAME = sourcename[ii]

    ;; Construct filenames.
    sourcedir = sourcename[ii] + '/'                 + merge_subdir[ii]
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    fit_stats_fn        = sourcedir + src_stats_basename
    lc_smooth_fn    = obsdir + lc_smooth_basename

    if (NOT file_test(fit_stats_fn)) then begin
      print, 'Source ', sourcename[ii], ' not observed.'
      continue
    endif
    
    stats = headfits(fit_stats_fn, ERRMSG=error)
    if (NOT keyword_set(error)) then begin
      sources[ii].ra  = sxpar(stats, 'RA')
      sources[ii].dec = sxpar(stats, 'DEC')
    endif else print, 'WARNING! Could not read '+fit_stats_fn

    ; Read smoothed LC & median energy time series.
    if (NOT file_test(lc_smooth_fn)) then begin
      print, 'Source ', sourcename[ii], ' skipped; no ', lc_smooth_basename, ' found.'
      continue
    endif
    
    pheader = headfits(lc_smooth_fn)
    t = mrdfits(lc_smooth_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + lc_smooth_fn
    
    tmin = min(t.time, MAX=tmax)
    coverage = (tmax-tmin) / (sxpar(pheader, 'TSTOP')-sxpar(pheader, 'TSTART'))
    if (coverage LT min_coverage) then begin
      print, sourcename[ii], coverage, F='(%"Source %s skipped; LC coverage is only %4.2f")'
      continue
    endif
    
    if (n_elements(t) EQ 1) then t = [t[0],t[0]]

    sources[ii].TIME          = ptr_new(t.TIME, /NO_COPY)
    sources[ii].MEDIAN_ENERGY = ptr_new(t.MEDIAN_ENERGY / 1000., /NO_COPY)
    ; Some backward compatibility logic here:
    sources[ii].COUNT_RATE    = ptr_new( tag_exist(t, 'COUNT_RATE') ? t.COUNT_RATE : t.RATE, /NO_COPY)
    
    sources[ii].nominal_rate   = median(*(sources[ii].COUNT_RATE))
    sources[ii].nominal_energy = median(*(sources[ii].MEDIAN_ENERGY))
    sources[ii].min_energy     = min   (*(sources[ii].MEDIAN_ENERGY), MAX=max_energy)
    sources[ii].max_energy     = max_energy
  endfor ;ii

  print, fix(total(ptr_valid(sources.TIME))), min_coverage, F='(%"%d sources with >=%4.2f coverage in lc_smooth_basename.")'

  if NOT keyword_set(scene_template) then return
endif ;keyword_set(catalog_or_srclist)




;; =============================================================================
;; BUILD FRAME IMAGES
!EXCEPT=1

ptr_free, sources.psf_img

;; -----------------------------------------------------------------------------
;; Determine which sources are visible in the scene.
refhd = headfits(scene_template)
extast, refhd, refastr
scene_xsize = sxpar(refhd, 'NAXIS1')
scene_ysize = sxpar(refhd, 'NAXIS2')

scene_xcenter = (scene_xsize-1)/2.0D
scene_ycenter = (scene_ysize-1)/2.0D


if keyword_set(show_frames) then begin
  window, /FREE, XSIZE=scene_xsize<1024, YSIZE=scene_ysize<1024
  show_frames_win = !D.WINDOW
endif


ad2xy, sources.ra, sources.dec, refastr, xindex, yindex 

vis_index = where( ((xindex<yindex) GT 0) AND (xindex LT (scene_xsize-1)) $
                                          AND (yindex LT (scene_ysize-1)) $
                                          AND ptr_valid(sources.TIME), num_vis )

print, num_vis, F='(%"%d sources used for brightness & energy scaling")'
if (num_vis EQ 0) then begin
  return
endif


vis_sources = sources[vis_index]
xindex      = xindex [vis_index]
yindex      = yindex [vis_index]



if NOT keyword_set(fwhm) then begin
  ; Choose a PSF FWHM that's a fixed # of pixels.
  fwhm = 2.0 
endif

psf_exponent = 1.0
help, fwhm, psf_exponent

;; -----------------------------------------------------------------------------
;; Determine time tags of frames.
if keyword_set(scene_param_file) then begin
  readcol, scene_param_file, time_fractions, deltaX_arcsec, deltaY_arcsec, deltaScale, F='F,F,F,F'
  num_frames = n_elements(time_fractions)
  
  if (min(deltaScale) LE 0) then message, 'deltaScale must be positive'
  
endif else if keyword_set(num_frames) then begin
  time_fractions = (0.5 + findgen(num_frames))/num_frames
  deltaX_arcsec  = replicate(0,num_frames)
  deltaY_arcsec  = replicate(0,num_frames)
  deltaScale     = replicate(1,num_frames)
endif else begin
  print, 'You must supply either TIME_TAGS or NUM_FRAMES to specify the desired frame time tags.'
  return
endelse

; Convert time fractions to time tags.
time_fractions = 0 > time_fractions < 1
tstart = sxpar(refhd, 'TSTART')
tstop  = sxpar(refhd, 'TSTOP')
time_tags = tstart + time_fractions * (tstop-tstart)


if keyword_set(jpeg_basename) then begin
  files_to_remove = findfile( jpeg_basename + '.*.jpg', COUNT=count)
  if (count GT 0) then begin
    print, 'removing: ',files_to_remove
    file_delete, files_to_remove, /ALLOW_NONEXISTENT
  endif
  
  frame_num = lindgen(num_frames)
  jpeg_filenames = jpeg_basename + string(frame_num, F='(%".%4.4d.jpg")')
  forprint, frame_num, time_tags, TEXTOUT=jpeg_basename+'.lis', COMMENT='frame#    time'
endif


;; -----------------------------------------------------------------------------
;; Scale the brightness so the smallest nominal_rate has a brightness of 20%.
;; Choose a brightness rescaling table so that PSF peak pixels of the visible sources
;; at their nominal_rate fluxes would form a flat distribution over [min_level..1].
;; We'll add extra points to this table to extend it's flux range because we may 
;; need to EXTRAPOLATE the scaling 
;; curve beyond the range of the sources' nominal fluxes.  For example when the
;; brightest source flares we need to extrapolate beyond the nominal range.
;; This is a kind of histogram equalization scaling.
;; Only sources with positive nominal_rate fluxes are considered.
min_level = 0.2
max_level = 1.1

ind = where( vis_sources.nominal_rate GT 0, num_samples )
if (num_samples EQ 0) then begin
  print, 'All sources have zero nominal_rate fluxes; cannot continue.'
  return
endif

temp                 = vis_sources[ind].nominal_rate
nominal_rate_samples = temp[sort(temp)]

if (1) then begin
scaled_rate_samples  = (min_level + (max_level-min_level) * findgen(num_samples) / float(num_samples-1)) 
endif else begin
endelse

lin_params = linfit( nominal_rate_samples, scaled_rate_samples )

large_val = 10*max(nominal_rate_samples)
nominal_rate_samples = [0,nominal_rate_samples,large_val]
scaled_rate_samples  = [0,scaled_rate_samples, lin_params[0] + lin_params[1]*large_val]

if keyword_set(verbose) then begin
  function_1d, id2, nominal_rate_samples, scaled_rate_samples,  LINE=6, PSYM=1, XTIT='nominal rate', YTIT='central pixel value'
endif


;; -----------------------------------------------------------------------------
;; Construct an energy scaling table that makes good use of the color spectrum.
;; We'll use a table which results in a nearly flat distribution of scaled nominal energies
;; in the range [0..1].
;; We'll add extra points to this table to extend it's energy range because we may 
;; need to EXTRAPOLATE the scaling 
;; curve beyond the range of the sources' nominal energies.  For example
;; if the source with the hardest nominal energy gets even harder during a flare then
;; we'll need to scale an energy value outside the range of energies used to construct
;; the scaling curve.
;; This is a kind of histogram equalization scaling.
nominal_energy_samples = (vis_sources.nominal_energy)[sort( vis_sources.nominal_energy )]

scaled_energy_samples  = findgen(num_vis) / float(num_vis-1)

lin_params = linfit( nominal_energy_samples, scaled_energy_samples )

large_val = 10
nominal_energy_samples = [0,nominal_energy_samples,large_val]
scaled_energy_samples  = [0,scaled_energy_samples, lin_params[0] + lin_params[1]*large_val]

;; Determine max & min scaled energies that are possible.
linterp, nominal_energy_samples, scaled_energy_samples, min(vis_sources.min_energy), low_scaled_energy
linterp, nominal_energy_samples, scaled_energy_samples, max(vis_sources.max_energy), high_scaled_energy

if keyword_set(verbose) then begin
  function_1d, id4, nominal_energy_samples, scaled_energy_samples,  LINE=6, PSYM=1, XTIT='nominal energy', YTIT='scaled energy'
  help, low_scaled_energy, high_scaled_energy
endif


;; -----------------------------------------------------------------------------
;; Figure out an appropriate way to normalize PSF to 1.
wing_scale = 1
npix     = [100,100]
centroid = [50,50]
psf_raw = (  psf_gaussian(NPIX=npix, FWHM=fwhm,            CENTROID=centroid) + $
           2*psf_gaussian(NPIX=npix, FWHM=fwhm*wing_scale, CENTROID=centroid))
psf_peak = max( psf_raw )
help, psf_peak

if keyword_set(mpeg_filename) then $
  mpegID = mpeg_open( [scene_xsize,scene_ysize], MOTION_VEC_LENGTH=1, QUALITY=100 )

for frame_num = 0, num_frames-1 do begin

  ;; -----------------------------------------------------------------------------
  ;; Move scene center by the specified offset (in arcseconds) and adjust zoom.
  ; Convert specified offset for this frame from arcseconds to pixels.
  cdelt = abs(refastr.CDELT[0])
  deltaX_pix = (deltaX_arcsec[frame_num] / 3600.) / cdelt
  deltaY_pix = (deltaY_arcsec[frame_num] / 3600.) / cdelt
  
  ; Find RA,DEC at current scene center
  xy2ad, scene_xcenter, scene_ycenter, refastr, scene_ra_center, scene_dec_center
  
  ; Redefine astrometry so that RA,DEC is offset as specified from the scene center.
  ; One is added to new scene center because FITS CRPIX values are 1-based.
  refastr.CRPIX = 1 + [scene_xcenter+deltaX_pix, scene_ycenter+deltaY_pix]
  refastr.CRVAL =     [scene_ra_center, scene_dec_center]
  
  ; Adjust zoom as specified.
print, refastr.CRVAL
;print, deltaScale[frame_num]
  refastr.CDELT = refastr.CDELT * deltaScale[frame_num]


  ;; -----------------------------------------------------------------------------
  ;; Compute positions of sources & find ones visible.
  ad2xy, sources.ra, sources.dec, refastr, xindex, yindex 

  vis_index = where( ((xindex<yindex) GT 0) AND (xindex LT (scene_xsize-1)) $
                                            AND (yindex LT (scene_ysize-1)) $
                                            AND ptr_valid(sources.TIME), num_vis )

  if (num_vis NE 0) then begin
    vis_sources = sources[vis_index]
    xindex      = xindex [vis_index]
    yindex      = yindex [vis_index]
    rates_this_frame    = fltarr(num_vis)
    energies_this_frame = fltarr(num_vis)
  endif else begin
    rates_this_frame    = fltarr(2)
    energies_this_frame = fltarr(2)
  endelse

  scene_brightness   = fltarr(scene_xsize,scene_ysize)
  scene_energy_sum   = fltarr(scene_xsize,scene_ysize)
  
  print, frame_num, deltaX_pix, deltaY_pix, num_vis, F='(%"Frame %d; offset=(%5.2f,%5.2f); %d visible sources")' 

  ;; -----------------------------------------------------------------------------
  ;; Build frame of the scene at the specified time tag.
  for ii = 0, num_vis-1 do begin
    ; Sample the COUNT_RATE and MEDIAN_ENERGY time series at the requested time tags 
    ; and send through the scaling tables constructed earlier.
    ; It's vital to use linterp so that no linear extrapolation will be done. ???

    linterp, *(vis_sources[ii].TIME), *(vis_sources[ii].COUNT_RATE),    time_tags[frame_num], rate
    linterp, *(vis_sources[ii].TIME), *(vis_sources[ii].MEDIAN_ENERGY), time_tags[frame_num], median_energy
    
    linterp, nominal_rate_samples,   scaled_rate_samples,   rate,          scaled_rate
    linterp, nominal_energy_samples, scaled_energy_samples, median_energy, scaled_energy
    rates_this_frame[ii]    = scaled_rate
    energies_this_frame[ii] = scaled_energy

    
    ; Generate a PSF image and add to the scene.  Add weighted energy image to running sum.
    ; There are some floating underflow exceptions we have to mask.
    xl = floor(xindex[ii] - fwhm*wing_scale) > 0
    yl = floor(yindex[ii] - fwhm*wing_scale) > 0
  
    xh = ceil (xindex[ii] + fwhm*wing_scale) < (scene_xsize-1)
    yh = ceil (yindex[ii] + fwhm*wing_scale) < (scene_ysize-1)
    
    save_except = !EXCEPT & !EXCEPT = 0
;    psf_img = (scaled_rate) * $
;             ( psf_gaussian(NPIX=[1+xh-xl,1+yh-yl], FWHM=fwhm, $
;                            CENTROID=[xindex[ii]]-xl,yindex[ii]-yl]) / psf_peak )^psf_exponent

    npix     = [1+xh-xl,1+yh-yl]
    centroid = [xindex[ii]-xl,yindex[ii]-yl]
    psf_raw = (  psf_gaussian(NPIX=npix, FWHM=fwhm,            CENTROID=centroid) + $
               2*psf_gaussian(NPIX=npix, FWHM=fwhm*wing_scale, CENTROID=centroid)) 
    
    psf_img = (scaled_rate/psf_peak) * psf_raw
;print, max(psf_img)                        
    scene_brightness[xl,yl] = scene_brightness[xl:xh,yl:yh] + psf_img
    scene_energy_sum[xl,yl] = scene_energy_sum[xl:xh,yl:yh] + psf_img * scaled_energy

    error = check_math(MASK=32)    ;Clear floating underflow
    !EXCEPT = save_except

    
    if (ii GT 0) AND ((ii MOD 100) EQ 0) then print, ii, ' sources processed'
  endfor ;ii

  ; Apply normalization step in the computation of weighted energy image.
  scene_energy = scene_energy_sum / scene_brightness
  ind = where( finite( scene_energy, /NAN ), count ) 
  if (count GT 0) then scene_energy[ind] = 0

  if keyword_set(verbose) then begin
    dataset_1d, id1, rates_this_frame
    dataset_1d, id3, energies_this_frame
    s='' & read,s
  endif

  ;; -----------------------------------------------------------------------------
  ;; Construct color image using the HSV model.
  
  ; Scale scene_energy to a normalized hue image
  hue_norm = (scene_energy - low_scaled_energy) / ((high_scaled_energy - low_scaled_energy)>1e-8)

  tara_hsv2rgb,  0.0 > hue_norm < 1.0, saturation, scene_brightness > 0, 0, $
                red_data, grn_data, blu_data

  ; This is the place we would introduce a user-supplied background for the scene, e.g. 
  ; red, green, blue planes from a diffuse emission analysis.
  ; Add those into the point source planes, e.g. 
  ; red_data = red_data +  red_bkg * (0.3 / max(red_bkg))
    
  if keyword_set(show_frames) then begin
    wset, show_frames_win
    color_manager, /X_TRUE
  endif
  
  ; Its IMPORTANT to specify HIGH_VALUE below so that all frames are scaled the same!
  ; A HIGH_VALUE < 1 will "clip" the bright stars.
  rgb_scale, red_data, grn_data, blu_data, $
             LOW_VALUE =0, LOW_NORM=0, $
             HIGH_VALUE=0.8, HIGH_NORM=1, INVERT=keyword_set(invert), $
             red_channel, grn_channel, blu_channel, DO_PLOT=keyword_set(show_frames)

  ; Add graphic showing time_fractions.
  bar_height = ceil(0.05*scene_ysize)
  xx = round( (scene_xsize-1) * time_fractions[frame_num] ) 
  red_channel[xx, 0:bar_height] = 1
  grn_channel[xx, 0:bar_height] = 1
  blu_channel[xx, 0:bar_height] = 1
  
  
  ;; -----------------------------------------------------------------------------
  ;; Save color image.
  img = bytarr(3,scene_xsize,scene_ysize)
         
  num_levels = 256       
  img[0,*,*] = floor( num_levels * red_channel ) < (num_levels-1)
  img[1,*,*] = floor( num_levels * grn_channel ) < (num_levels-1)
  img[2,*,*] = floor( num_levels * blu_channel ) < (num_levels-1)

  if keyword_set(mpeg_filename) then $
    mpeg_put, mpegID, FRAME=frame_num, IMAGE=img
  
  if keyword_set(jpeg_basename) then $
    write_jpeg, jpeg_filenames[frame_num], img, TRUE=1, QUALITY=100
endfor; frame_num

if keyword_set(mpeg_filename) then begin
  print, 'Writing ', mpeg_filename, ' ...'
  mpeg_save,  mpegID,  FILENAME=file_expand_path(mpeg_filename)
  mpeg_close, mpegID
endif
        
return
end


;; =============================================================================
;; =============================================================================
PRO test_psfs

img=psf_gaussian(NPIX=200, FWHM=fwhm, CENTROID=100,NDIM=1)
x=indgen(200)
function_1d,id,x,img

; We'd like stars to get "larger" when they flare, but am having a hard time finding a PSF that gives the right look.
; Perhaps FWHM should vary with flux!  That would slow down code.

; This one seems disk-like in movie.
limg = alog(img) > (-exp(1))
function_1d,id,x,limg

; This one seems fuzzy is movie.
function_1d,id,x,img^0.25
return
end


;; =============================================================================
;; =============================================================================
PRO test_colors
show_frames=1

scene_xsize = 300
scene_ysize = 300
color_manager

if keyword_set(show_frames) then begin
  window, /FREE, XSIZE=scene_xsize<1024, YSIZE=scene_ysize<1024
  show_frames_win = !D.WINDOW
endif

ima=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[100,100])
imb=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[200,100])
imc=psf_gaussian(NPIX=[scene_xsize,scene_ysize],FWHM=90,CENTROI=[150,200])
scene_brightness=ima+imb+imc
scene_energy_sum=ima+2*imb+4*imc

scene_energy = scene_energy_sum / scene_brightness
ind = where( finite( scene_energy, /NAN ), count ) 
if (count GT 0) then scene_energy[ind] = 0

hue_norm = scene_energy / max(scene_energy)

;function_2d,id,scene_energy

  saturation = 0.5

  tara_hsv2rgb, hue_norm, saturation, scene_brightness > 0, 0, $
                red_data, grn_data, blu_data

  if keyword_set(show_frames) then begin
    wset, show_frames_win
    color_manager, /X_TRUE
  endif

  
  rgb_scale, red_data, grn_data, blu_data, $
             LOW_VALUE =0,  LOW_NORM=0, $
             HIGH_VALUE=0.8, HIGH_NORM=1, $
             red_channel, grn_channel, blu_channel, DO_PLOT=keyword_set(show_frames)

stop
function_2d,id,hue_norm,DATA='hue_norm'
function_2d,id,hue_img,DATA='hue_img'
function_2d,id,red_data,DATA='red_data'
function_2d,id,grn_data,DATA='grn_data'
function_2d,id,blu_data,DATA='blu_data'

return
end
  
;hue_norm=0.5 + 0.2*findgen(200)/199.
;value_data=replicate(1,200)
;make_2d,hue_norm,value_data

;; =============================================================================
;; =============================================================================
PRO make_scene_params, num_frames, filename

openw, unit, filename, /GET_LUN
!TEXTUNIT = unit

; One cycle on full field.
zero = replicate(0,num_frames)
one  = replicate(1,num_frames)
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), zero, zero, one, /NoCOMMENT

; One cycle while linear zooming.
; For f3.reg
xoffset =   8.86 ;arcsec
yoffset = -78.7 ; arcsec
zoom    =  4.69 

deltaX     = replicate(xoffset/float(num_frames)      , num_frames)
deltaY     = replicate(yoffset/float(num_frames)      , num_frames)
deltaScale = replicate(exp( -alog(zoom) / num_frames ), num_frames)
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), deltaX, deltaY, deltaScale, /NoCOMMENT

; One cycle on zoomed field.
forprint, TEXTOUT=5, findgen(num_frames)/(num_frames-1), zero, zero, one, /NoCOMMENT

free_lun, unit

return
end



;; =============================================================================
;; =============================================================================
PRO plot_spectrum, sourcename

base = sourcename +'/' + sourcename
src = mrdfits(base + '.pi',     1, src_header)
bkg = mrdfits(base + '_bkg.pi', 1, bkg_header)

energy = 14.45/1000.*(1+indgen(n_elements(src)))

backscal = sxpar(src_header, 'BACKSCAL') / bkg.BACKSCAL
spectrum = src.counts - backscal * bkg.counts

plot, energy, spectrum[0:547], PSYM=10

ind = where(src.counts NE 0)
function_1d, id, energy[ind], (src.counts)[ind],             LINE=6, PSYM=4, DATASET='source'
ind = where(bkg.counts NE 0)
function_1d, id, energy[ind], -backscal * (bkg.counts)[ind], LINE=6, PSYM=1, DATASET='background'

return
end



;;; ==========================================================================
;;; Set default spectral model preference.
;;; Use this tool to establish a default spectral model preference (in the FIT
;;; keyword BEST_MDL in the primary HDU of source.spectra) prior to running the
;;; interactive tool ae_spectra_viewer
;;; The parameter 'hduname' is a regular expression understood by strmatch().
;;; The parameter 'hduname' can be a scalar or a vector.
;;; ==========================================================================

PRO ae_default_model_preference, catalog_or_srclist, hduname, FORCE=force, $
                  MERGE_NAME=merge_name

fit_stats_basename       = 'source.spectra'
src_stats_basename       = 'source.stats'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

if (n_elements(hduname) EQ 1) then hduname = replicate(hduname, num_sources)

if (n_elements(hduname) NE num_sources) then begin
  print, 'ERROR: # of sources in catalog does not match length of hduname vector.'
  retall
endif

if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                else merge_subdir = ''
if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)


repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  temproot = 'AE' + session_name +'.noindex/'
  temproot = filepath(temproot, /TMP)
endrep until (NOT file_test(temproot))
file_mkdir, temproot
tempdir = temproot

run_command, /INIT, PARAM_DIR=tempdir

for ii = 0, num_sources-1 do begin
  print, sourcename[ii], F='(%"\nSource: %s")'
  
  ;; Find the HDUs in source.spectra.
  sourcedir = sourcename[ii] + '/' + merge_subdir[ii] 
  fit_stats_fn  = sourcedir + fit_stats_basename
  fits_open, fit_stats_fn, fcb, /NO_ABORT, MESSAGE=open_error
    
  if keyword_set(open_error) then begin
    print, 'No spectral models found.'
    continue
  end
  
  fits_close, fcb
  
  ; If an existing preference is found, respect it.
  preference = sxpar(fcb.HMAIN, 'BEST_MDL', COUNT=count)
  if (count GT 0) then begin
    if keyword_set(force) then begin
      print, 'Overriding existing preference for ', preference
    endif else begin
      print, 'Respecting existing preference for ', preference
      continue
    endelse
  endif

  ; Find the last HDU matching the specified model name spec.
  ind = where( strmatch(fcb.EXTNAME, hduname[ii], /FOLD_CASE), count )
  
  if (count GT 1) then begin
    ind = ind[count-1]
    print, count, hduname[ii], fcb.EXTNAME[ind],  F='(%"  %d spectral models match %s; using the most recent: %s )")'
    count = 1
  endif
      
  if (count EQ 0) then begin
    print, hduname[ii], F='(%"WARNING! Cannot find any spectral model matching ''%s''")'
    continue
  endif
  
  ;; Write the preference.
  cmd = string(fit_stats_fn, fcb.EXTNAME[ind], $
                 F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=BEST_MDL value=""%s"" comment=""default preferred model""')")

  run_command, cmd
  
  ; Delete any "best model" symlink we find, and remake it.
  link_name = sourcedir + 'spectral_models/best_model'
  if file_test(link_name, /DANGLING_SYMLINK) OR file_test(link_name, /SYMLINK) then file_delete, link_name
  if file_test(link_name) then begin
    print, 'WARNING: existing regular file '+link_name+' has not been changed.'
  endif else begin
    file_link, fcb.EXTNAME[ind], link_name
  endelse
   
  
endfor ;ii

CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(temproot) then begin
  list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, temproot
endif
return
end



;; =============================================================================
;; spectra_viewer tool
;;
;; Example: ae_spectra_viewer, 'tables/xspec.collated', KEY=['NH','KT','PH','F0P5_8']
;; =============================================================================

;; =============================================================================
;; Procedure to handle GUI updates when a source is selected.
PRO ae_spectra_viewer_select_source, st, source_index

fit_stats_basename       = 'source.spectra'
src_stats_basename       = 'source.stats'

; Range check the source number.
num_sources  = n_elements((*st).sourcename)
source_index = 0 > source_index < (num_sources-1)

(*st).current_source_index = source_index


; Now we can display information about the newly-selected source.
this_sourcename   = (*st).sourcename  [source_index]
this_source_label = (*st).source_label[source_index]

widget_control, (*st).source_name_id,   SET_VALUE=this_sourcename
widget_control, (*st).source_label_id,  SET_VALUE=this_source_label    
widget_control, (*st).source_number_id, SET_VALUE=1+source_index
if ((*st).rank_id GT 0) then $
  widget_control, (*st).rank_id, SET_VALUE=1+(*st).current_rank

print, this_sourcename, this_source_label, F='(%"\n----------------------------\n%s (%s)")'
this_note = (*st).notes[source_index]
if keyword_set(this_note) then print, this_note


num_models  = (*st).num_models

num_columns = n_elements((*st).keylist)  

; Clear the table     
if (num_models GT 0) then begin
  ; Unfortunately we cannot get the table to tell us how many rows it has, not even by GET_VALUE., so we have to keep track ourselves.
;  widget_control, (*st).model_table, /DELETE_ROWS, USE_TABLE_SELECT=[0,0,num_columns-1,(*st).num_models-1]
  all_rows = replicate(-1,2,(*st).num_models)
  all_rows[1,*] = indgen((*st).num_models)
  widget_control, (*st).model_table, /DELETE_ROWS, USE_TABLE_SELECT=all_rows

  num_models = 0
endif


;--------------------------------------------------------------------------
;; Find the HDUs in source.spectra.
sourcedir = this_sourcename + '/' + (*st).merge_subdir[source_index] 
fit_stats_fn  = sourcedir + fit_stats_basename
fits_open, fit_stats_fn, fcb, /NO_ABORT, MESSAGE=open_error
  
model_names = ''
selected_model_index = -1

if keyword_set(open_error) then begin
endif else begin
  num_models = fcb.NEXTEND
  if (fcb.NEXTEND GT 0) then begin
    model_names = strtrim(fcb.EXTNAME[1:*], 2)
    
    ; SORT the model names to be convenient for our standard spectral modeling recipe (recipe.txt).
    name_template = ['*_A*', '*_B*', '*_C*', '*_D*', '*_E*', '*_std1*', '*_std2*', '*kT_max*', '*_pow*']
    tail_index = 0L
    for jj = 0,n_elements(name_template)-1 do begin
      ind = where(strmatch(model_names, name_template[jj], /FOLD_CASE), count)
      
      ; Process each match.
      for kk=0,count-1 do begin
        this_index = ind[kk]
        
        ; Only process matches found in the unsorted tail.
        if (this_index LT tail_index) then continue
      
        ; Swap the matched model with the first model in the unsorted tail.
        temp                    = model_names[this_index]
        model_names[this_index] = model_names[tail_index]
        model_names[tail_index] = temp
        tail_index++
      endfor ;kk
    endfor ;jj
    
    
;      case widget_info( (*st).plot_name_list, /DROPLIST_SELECT ) of
;        0: name = '/ldata.ps'
;        1: name = '/icounts.ps'
;      endcase
    
    plot_files = strarr(num_models,2)
    plot_files[*,0] = sourcedir + 'spectral_models/' + model_names + '/ldata.ps'
    plot_files[*,1] = sourcedir + 'spectral_models/' + model_names + '/icounts.ps'
    
    for ii=0,n_elements(plot_files)-1 do begin
      if NOT file_test(plot_files[ii]) then plot_files[ii] = (*st).no_model_file
    endfor
  endif
endelse

num_gv_pairs  = n_elements((*(*st).gv_pids)) / 2  

num_gv_pairs_needed = (num_models > num_gv_pairs)

;--------------------------------------------------------------------------
;; Refresh the existing gv displays, and create more if needed.

;; NOTE that the gv manual says that you can send it a signal
;;    kill -HUP
;; to reload, rather than using the --watch option.  But it doesn't work on our X11 environment.
;;
;; I also find that neither symlinks nor hard links will reliably induce gv to redraw.
;; Thus, we're forced to COPY the PostScript files we want to display.

if (num_gv_pairs_needed GT 0) then begin
  ;; Display the plots.

  gv_files= strarr(num_gv_pairs_needed,2)
  gv_files[*,0] = (*st).tempdir + string(indgen(num_gv_pairs_needed), F='(%"%d_g")')                
  gv_files[*,1] = (*st).tempdir + string(indgen(num_gv_pairs_needed), F='(%"%d_c")')                
  
  ; Link up the files we need to display to the gv processes and
  ; link the null file to any extra gv processes.
  for ii=0,num_gv_pairs_needed-1 do begin
    if (ii LT num_models) then begin
      file_copy, /OVERWRITE,   plot_files[ii,0],  gv_files[ii,0] 
      file_copy, /OVERWRITE,   plot_files[ii,1],  gv_files[ii,1] 
    endif else begin
      file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,0]
      file_copy, /OVERWRITE, (*st).no_model_file, gv_files[ii,1]
    endelse
  endfor

  ; Do we need more gv sessions?
  if (num_gv_pairs_needed GT num_gv_pairs) then begin
    gv_logfile = (*st).tempdir + 'gv.log'
    gv_pids = strarr(num_gv_pairs_needed,2)
    if (num_gv_pairs GT 0) then gv_pids[0,0] = *(*st).gv_pids $
                           else file_delete, gv_logfile, /ALLOW_NONEXISTENT
    
    ; See these URLs for the X11 color chart:
    ; http://en.wikipedia.org/wiki/X11_color_names
    ; http://www.mcfedries.com/Books/cightml/x11color.htm
    ; http://www.febooti.com/products/iezoom/online-help/html-color-names-x11-color-chart.html
    ; See <X11root>/lib/X11/rgb.txt for the colors known to a given machine.
    
;      color = ['Cornsilk','Tan','SandyBrown','Goldenrod','Peru','DarkGoldenrod','Chocolate','Sienna','Brown','DarkSalmon','Salmon','IndianRed']
    color = ['Cornsilk','Goldenrod','Chocolate','DarkSalmon', 'Tan','Peru','Sienna','Salmon', 'SandyBrown','DarkGoldenrod','Brown','IndianRed']
    color = [color,color]
    
    seq = num_gv_pairs + indgen(num_gv_pairs_needed-num_gv_pairs)
    row = seq/3
    col = seq MOD 3
    sort_ind = reverse(sort(-100*row+col))
    dx = 400
    dy = 400

    fmt = "(%'gv --watch -xrm ""GV*background:%s"" -geometry +%d+%d --ad=%s %s >>&! %s &')" 
      
    for jj=0,n_elements(seq)-1 do begin
      ind = sort_ind[jj]
      ii = seq[ind]
      xpos =     2*dx*col[ind]
      ypos = 400+  dy*row[ind]

      cmd = string(color[ii], xpos+dx, ypos, (*st).gv_resource_file, gv_files[ii,1], gv_logfile, F=fmt)
      run_command, cmd, result, /UNIX
      gv_pids[ii,1] =  (strsplit(result[0], /EXTRACT))[1] 
      wait, 0.5

      cmd = string(color[ii], xpos   , ypos, (*st).gv_resource_file, gv_files[ii,0], gv_logfile, F=fmt)
      run_command, cmd, result, /UNIX
      gv_pids[ii,0] =  (strsplit(result[0], /EXTRACT))[1] 
      wait, 0.5
    endfor
    
    num_gv_pairs = num_gv_pairs_needed
    
    *(*st).gv_pids = gv_pids
  endif ; creating new gv processes
endif ; (num_gv_pairs_needed GT 0)


;--------------------------------------------------------------------------
;; Redraw the model table.
if (num_models EQ 0) then begin
  print, 'No fit results available for ', this_sourcename
  
endif else begin
  ;--------------------------------------------------------------------------
  ;; Populate the table of fit parameters.
  widget_control, (*st).model_table, INSERT_ROWS=num_models
  table       = strarr(num_columns, num_models)
;    table_color = strarr(num_columns, num_models)
  CSTAT       = fltarr(num_models)
  
  ; For each model, find each FITS keyword and format into a cell.
  ; It is vital to reference the extention by name rather than by number because we have re-ordered model_names!
  for ii=0,num_models-1 do begin
    fits_read, fcb, dummy, model_header, /NO_PDU, /HEADER_ONLY, EXTNAME=model_names[ii], /NO_ABORT, MESSAGE=error 
    
    if keyword_set(error) then message, 'BUG!'
    
    CSTAT  [ii] = sxpar(model_header,'CSTAT')
    
    for jj=0,num_columns-1 do begin
     keyname = (*st).keylist[jj]
     
     format = (jj EQ 0) ? '(A)' : '(g10.2)'
     if (keyname EQ 'CSTAT') then format = '(g10.4)'
     
     value =  sxpar(model_header, keyname, COUNT=count) 
     ; FITS keywords in these headers that happen to be IDL reserved words (e.g. "NE") have '_' prepended by the collation stage.
     if (count EQ 0) then begin
       keyname = (stregex(keyname, '^_(.+)', /SUB, /EXT))[1]
       value =  sxpar(model_header, keyname, COUNT=count) 
     endif
     
     
     if (count EQ 1) then begin
       table[jj,ii] = string(value, F=format)
       
       if (keyname EQ 'FC0P5_8') then begin                 
         if (alog10(value/sxpar(model_header,'F0P5_8')) GT (*st).flux_correction_limit) then $
           widget_control, (*st).model_table, BACKGROUND_COLOR=[255,100,100], USE_TABLE_SELECT=[[jj,ii],[jj,ii]]
       endif
       
       if (keyname EQ 'FCH8') then begin                 
         if (alog10(value/sxpar(model_header,'FH8')) GT (*st).flux_correction_limit) then $
           widget_control, (*st).model_table, BACKGROUND_COLOR=[255,100,100], USE_TABLE_SELECT=[[jj,ii],[jj,ii]]
       endif
       
       if (keyname EQ 'FCH7') then begin                 
         if (alog10(value/sxpar(model_header,'FH7')) GT (*st).flux_correction_limit) then $
           widget_control, (*st).model_table, BACKGROUND_COLOR=[255,100,100], USE_TABLE_SELECT=[[jj,ii],[jj,ii]]
       endif
     endif ; (count EQ 1)
    endfor
  endfor
  
  ind = where((*st).keylist EQ 'CSTAT', count)
  if (count GT 0) then table[ind,*] = string(CSTAT-min(CSTAT), F='(g10.4)')
  
  widget_control, (*st).model_table, SET_VALUE=table
  
  
  ;; Identify any model selection previously made and stored in the primary HDU..
   selected_model_name = strtrim(sxpar(fcb.HMAIN, 'BEST_MDL'), 2)
   is_provisional      = sxpar(fcb.HMAIN, 'PROVISNL', COUNT=0)
   if (count EQ 0) then is_provisional=0
   
   selected_model_index = where(model_names EQ selected_model_name, count)
   
  if (count EQ 1) then begin
    table_select = [[0,selected_model_index],[num_columns-1,selected_model_index]]
  endif else begin
    table_select = [[-1,-1],[-1,-1]]
  endelse

  widget_control,  (*st).model_table,    SET_TABLE_SELECT=table_select     
  widget_control,  (*st).provisional_id, SET_VALUE=is_provisional
endelse ; num_models GT 0
  

; Save the models found so the event handler can see them next time.
*(*st).model_names = model_names
 (*st).num_models  = num_models    


if NOT keyword_set(open_error) then fits_close, fcb


;; Show the source statistics.
widget_control, (*st).src_signif_id, SET_VALUE=string(F='(%"%0.1f")', (*st).src_signif[source_index])
widget_control, (*st).ag_frac_id   , SET_VALUE=string(F='(%"%4.2f")', (*st).ag_frac   [source_index])

text = 'no evidence'
prob_ks = (*st).PROB_KS[source_index]
if        (prob_ks LT 0.05)  then text = 'possible'
if        (prob_ks LT 0.005) then text = 'definite'
if ~finite(prob_ks) then text = '...'
widget_control, (*st).prob_ks_id   , SET_VALUE='variability: '+text


src_stats_fn = sourcedir + src_stats_basename                                          
src_stats    = headfits(src_stats_fn)
sxdelpar, src_stats, ['SIMPLE','BITPIX','NAXIS','EXTEND','DATE','CREATOR','RA_ML','DEC_ML','QUANTML','QUANTCOR','RA_CORR','DEC_CORR','RA_DATA','DEC_DATA','ERR_DATA','RA_PSF','DEC_PSF','END']

if (widget_info( (*st).stats_header_id, /VALID_ID )) then $
  widget_control, (*st).stats_header_id, SET_VALUE=src_stats
  
return ; ae_spectra_viewer_select_source
end                                                                                                                                    


;; =============================================================================
;;; Widget Event Handler Procedure
PRO ae_spectra_viewer_event, event

widget_control, /HOURGLASS
fit_stats_basename       = 'source.spectra'
src_stats_basename       = 'source.stats'


;; Get the state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=st

num_sources  = n_elements((*st).sourcename)

source_index = 0 > (*st).current_source_index < (num_sources-1)

num_models  = (*st).num_models

sourcedir = (*st).sourcename[source_index] + '/' + (*st).merge_subdir[source_index] 
fit_stats_fn  = sourcedir + fit_stats_basename

    
;; Process the event.
new_src_flag = 0
select_model_flag = 0

DestroyFlag = 0
case event.ID of

;--------------------------------------------------------------------------
  (*st).model_table: $
   begin
  if (Event.TYPE EQ 4) then begin
    row_num = Event.SEL_TOP
    if (row_num EQ Event.SEL_BOTTOM) AND (row_num GE 0) AND (row_num LT num_models) then begin
      print, 'Selected model ', (*(*st).model_names)[row_num]
      select_model_flag = 1
      
      cmd = string(fit_stats_fn, (*(*st).model_names)[row_num], $
                 F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=BEST_MDL value=""%s"" comment=""preferred model""')")

      if ~keyword_set((*st).read_only) then run_command, cmd
      
      ; Delete any "best model" symlink we find, and remake it.
      link_name = sourcedir + 'spectral_models/best_model'
      if file_test(link_name, /DANGLING_SYMLINK) OR file_test(link_name, /SYMLINK) then file_delete, link_name
      if file_test(link_name) then begin
        print, 'WARNING: existing file'+link_name+' has not been changed.'
      endif else begin
        file_link, (*(*st).model_names)[row_num], link_name
      endelse
      
      table_select = [[0,row_num], [n_elements((*st).keylist)-1,row_num]]
      widget_control,  (*st).model_table, SET_TABLE_SELECT=table_select     
    endif
  endif
  end

  
;--------------------------------------------------------------------------
  (*st).provisional_id: $
   begin
   is_provisional = Event.select
   cmd = string(fit_stats_fn, is_provisional ? 'T' : 'F', $
             F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=PROVISNL value=""%s"" datatype=boolean comment=""BEST_MDL is provisional""')")

   if ~keyword_set((*st).read_only) then run_command, cmd
   end
  
   
;--------------------------------------------------------------------------
;  (*st).plot_name_list: $
;   begin
;   end

   
   
   
;--------------------------------------------------------------------------
  (*st).source_label_id: $
   begin
   widget_control, (*st).source_label_id, GET_VALUE=source_label                                       
   ind = where(strlowcase(strtrim(source_label[0],2)) EQ strlowcase((*st).source_label), count)
   if (count GE 1) then begin
     source_index = ind[0]
     ae_spectra_viewer_select_source, st, source_index
   endif else print, 'Cannot find that source label.'
   end
   
;--------------------------------------------------------------------------
  (*st).source_number_id: $
   begin
   widget_control, (*st).source_number_id, GET_VALUE=source_number                                       
   ae_spectra_viewer_select_source, st, source_number-1
   end

;--------------------------------------------------------------------------
  (*st).prev_button: $
   begin
   source_index-- 
   source_index >= 0 
   ae_spectra_viewer_select_source, st, source_index
   end

;--------------------------------------------------------------------------
  (*st).next_button: $
   begin
   source_index++ 
   source_index <= (num_sources-1)                                                                                                
   ae_spectra_viewer_select_source, st, source_index
   end

;--------------------------------------------------------------------------
  (*st).rank_id: $
   begin
   widget_control, (*st).rank_id, GET_VALUE=rank                                       
   rank = 0 > --rank < max((*st).presentation_rank)
   ind = where((*st).presentation_rank EQ rank, count)
   if (count NE 1) then message, 'BUG!'
   (*st).current_rank = rank
   ae_spectra_viewer_select_source, st, ind[0]
   end

;--------------------------------------------------------------------------
  (*st).prev_rank_button: $
   begin
   if ((*st).current_rank LE 0) then begin
     print, 'You are at the first source in '+(*st).srclist_fn
   endif else begin
     (*st).current_rank--
     ind = where((*st).presentation_rank EQ (*st).current_rank, count)
     if (count NE 1) then message, 'BUG!'
     ae_spectra_viewer_select_source, st, ind[0]
   endelse
   end

;--------------------------------------------------------------------------
  (*st).next_rank_button: $
   begin
   if ((*st).current_rank GE max((*st).presentation_rank)) then begin
     print, 'You are at the last source in '+(*st).srclist_fn
   endif else begin
     (*st).current_rank++
     ind = where((*st).presentation_rank EQ (*st).current_rank, count)
     if (count NE 1) then message, 'BUG!'
     ae_spectra_viewer_select_source, st, ind[0]
   endelse
   end

;--------------------------------------------------------------------------
  (*st).done_button: DestroyFlag = 1

;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in ae_spectra_viewer'
       help,/ST,Event
       end
endcase
  
       
; DONE button
if (DestroyFlag) then begin
  ; Kill the gv processes.
  if (n_elements(*(*st).gv_pids) GT 0) then begin
    run_command, /UNIX, 'kill -TERM ' + strjoin('-' + *(*st).gv_pids,' '), STATUS=status
  endif
  
  
  ; Remove the temp_dir.
  files = file_search((*st).tempdir+'/*', /MATCH_INITIAL_DOT, COUNT=count)
  if (count GT 0) then file_delete, files, /ALLOW_NONEXISTENT
  file_delete, (*st).tempdir, /ALLOW_NONEXISTENT

  widget_control, top_base, /DESTROY
endif


return
end


;==========================================================================
;;; Widget Creation Routine
;==========================================================================
PRO ae_spectra_viewer, collated_filename, SRCLIST_FILENAME=srclist_fn, MERGE_NAME=merge_name, $
                       KEYLIST=keylist_p, NOTES=notes, BLOCK=block,  READ_ONLY=read_only

;; Read the source properties of all the sources.
bt = mrdfits(collated_filename, 1, theader, /SILENT, STATUS=status)
if (status NE 0) then begin
  print, 'ERROR reading ', collated_filename
  retall      
endif

num_sources = n_elements(bt)
print, num_sources, F='(%"\n%d sources found in collated table.\n")'


if keyword_set(keylist_p) then keylist = keylist_p $
 else keylist = ['NH','KT','KT2','PH','F0P5_2','F2_8','FC2_8','F0P5_8','FC0P5_8',$
                                      'FH2'   ,'F28' ,'FC28' ,'FH8'   ,'FCH8'   ,$
                                               'F27' ,'FC27' ,'FH7'   ,'FCH7'   ,$
                                      'CSTAT','CHI_SQR']
keylist = strtrim(keylist,2)
for ii=0, n_elements(keylist)-1 do $
  if (total(strmatch(tag_names(bt), keylist[ii], /FOLD_CASE)) EQ 0) then keylist[ii] = ''

keylist = keylist[where(keylist)]

column_labels = ['model',keylist]
flux_correction_limit = '0.5'


;; Look up the source properties of all the sources.
sourcename      = strtrim(bt.CATALOG_NAME   ,2)
label           = strtrim(bt.LABEL          ,2) 
if (total(strmatch(tag_names(bt), 'AG_FRAC', /FOLD_CASE)) EQ 0) then $
       ag_frac = fltarr(num_sources) $
  else ag_frac =          bt.AG_FRAC
prob_ks         =         bt.PROB_KS
band_full           = 0  
print, 'Using the energy band ', bt[0].ENERG_LO[band_full], bt[0].ENERG_HI[band_full]
src_signif          = bt.SRC_SIGNIF          [band_full]

;; Accept any NOTES supplied by the caller.
case n_elements(notes) of
  0: notes = strarr(num_sources)
  1: notes = replicate(notes, num_sources)
  num_sources: 
  else: begin
        print, 'ERROR: length of NOTES input not equal to number of sources'
        retall
        end
endcase


;; Handle any explicit SRCLIST_FILENAME supplied.
if keyword_set(srclist_fn) then begin
  readcol, srclist_fn, subset_sourcename, FORMAT='A', COMMENT=';'
  
  ; Trim whitespace and ignore blank lines.
  subset_sourcename = strtrim(subset_sourcename,2)

  presentation_rank = replicate(-1L, num_sources)
  rank = 0L
  for ii=0L, n_elements(subset_sourcename)-1 do begin
    if (subset_sourcename[ii] EQ '') then continue
    
    ; Parse lines with semicolons into source names and notes.
    ind = strpos(subset_sourcename[ii],';')
    if (ind NE -1) then begin
      this_note             = strtrim(strmid(subset_sourcename[ii],ind+1) ,2)
      subset_sourcename[ii] = strtrim(strmid(subset_sourcename[ii],0,ind) ,2)
    endif else this_note = ''
    
    ; Look for this sourcename in the full list.
    ind = where(sourcename EQ subset_sourcename[ii], count)
    if (count GT 0) then begin
      ; Mark this source as being in the subset, and record any note that was parsed.
      presentation_rank[ind[0]]  = rank++
      notes            [ind[0]] += this_note
    endif else print, subset_sourcename[ii], F='(%"Source %s is missing from collated table!")'
    
    if (count GT 1) then print, subset_sourcename[ii],  F='(%"WARNING: source %s appears multiple times in table %s")'
  endfor 
  show_rank_navigation = 1
endif else begin
  srclist_fn  = collated_filename
  presentation_rank = lindgen(num_sources)
  show_rank_navigation = 0
endelse

;; Configure to display the "first" source (rank EQ 0).
ind = where(presentation_rank EQ 0, count)
if (count NE 1) then message, 'BUG!'
initial_source_number = ind[0]



; MERGE_NAME is taken from AE keyword parameter if available, or from collated table otherwise.
dum = where(tag_names(bt) EQ 'MERGE_NAME', count)
if keyword_set(merge_name) then begin
  merge_subdir = merge_name    + '/'
endif else if (count EQ 1) then begin
  merge_subdir = strtrim(bt.MERGE_NAME,2) + '/'
endif else begin
  merge_subdir = ''
endelse

if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)


;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
help, tempdir

run_command, /INIT, PARAM_DIR=tempdir

; Identify a gv resources file we can use.
result = routine_info( 'ae_spectra_viewer', /SOURCE )
fdecomp, result.PATH, disk, codedir

gv_resource_file = "gv_resources.txt"
if (NOT file_test(gv_resource_file)) then begin
  gv_resource_file = codedir + gv_resource_file

  if (NOT file_test(gv_resource_file)) then begin
    print, 'Cannot find '+gv_resource_file
    retall
  endif
endif

; Identify a PostScript file to display when a gv session is not needed.
; Copy it to the tempdir for speed.
no_model_file       = "no_model.eps"
local_no_model_file = tempdir + no_model_file

if (NOT file_test(no_model_file)) then begin
  no_model_file = codedir + no_model_file

  if (NOT file_test(no_model_file)) then begin
    print, 'Cannot find '+no_model_file
    retall
  endif
endif
file_copy, /OVERWRITE, no_model_file, local_no_model_file



top_base = widget_base(TITLE='ACIS Extract Spectra Viewer', /BASE_ALIGN_CENTER, /COLUMN, $
                        /SPACE, /XPAD, /YPAD)

 button_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER)
 
   base = widget_base( button_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER, FRAME=1)
     source_number_id = cw_field( base, /INTEGER, /RETURN_EVENTS, XSIZE=4, VALUE=initial_source_number,  TITLE='Seq # in '+file_basename(collated_filename))                
     prev_button = widget_button( base, VALUE='Previous' )
     next_button = widget_button( base, VALUE='  Next  ' )
   
   plot_name_list = 0L ;widget_droplist( button_base, VALUE=['grouped spectra','cumulative spectra'])
   provisional_id   = cw_bgroup( button_base, ['provisional'], /NONEXCLUSIVE, SET_VALUE=0 )
   
   source_label_id  = cw_field( button_base, /STRING , /RETURN_EVENTS, XSIZE=15, VALUE='', TITLE='Source Label/Name:')                
   source_name_id   = cw_field( button_base, /STRING, /NOEDIT, XSIZE=18, TITLE='')
   ;widget_label( button_base, /DYNAMIC_RESIZE, VALUE=' ' )                                

   rank_id          = 0L
   prev_rank_button = 0L
   next_rank_button = 0L
 
   if show_rank_navigation then begin
     base = widget_base( button_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER, FRAME=1)
       rank_id = cw_field( base, /INTEGER, /RETURN_EVENTS, XSIZE=4, VALUE=1,  TITLE='Seq # in '+file_basename(srclist_fn))                
       prev_rank_button = widget_button( base, VALUE='Previous' )
       next_rank_button = widget_button( base, VALUE='  Next  ' )
   endif

   done_button = widget_button( button_base, VALUE='Quit' )

 source_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER )
 
   status_field_id= widget_label(source_base, /DYNAMIC_RESIZE, VALUE='Flux is RED when correction > '+flux_correction_limit+' dex.' )                                
   src_signif_id  =     cw_field(source_base, /STRING, /NOEDIT, XSIZE=4, TITLE='SRC_SIGNIF')
   ag_frac_id     =     cw_field(source_base, /STRING, /NOEDIT, XSIZE=4, TITLE='AG_FRAC')
   prob_ks_id     = widget_label(source_base, /DYNAMIC_RESIZE)

num_models = 10

column_widths = [300,replicate(100,n_elements(keylist))] ; units are pixels
device, get_screen_size=screen_size
scr_xsize = (100 + total(/INT, column_widths)) < (screen_size[0] - 100)
model_table = widget_table( top_base, ALIGNMENT=2, /ALL_EVENTS, COLUMN_LABELS=column_labels, COLUMN_WIDTHS=column_widths, /RESIZEABLE_COLUMNS, /SCROLL, SCR_XSIZE=scr_xsize,  XSIZE=n_elements(column_labels), Y_SCROLL_SIZE=num_models, /DISJOINT_SELECTION  )


header_base = widget_base(TITLE='Fits Header', GROUP=top_base, XOFFSET=0, YOFFSET=330, /COLUMN, /SPACE, /XPAD, /YPAD)

  stats_header_id = widget_text( header_base, /SCROLL, XSIZE=80, YSIZE=20 )

state = { read_only:keyword_set(read_only), $
  
          source_label_id:source_label_id, source_name_id:source_name_id, $
          source_number_id:source_number_id, prev_button:prev_button, next_button:next_button, $
          rank_id:rank_id, prev_rank_button:prev_rank_button, next_rank_button:next_rank_button, $
          status_field_id:status_field_id, $
          model_table:model_table, stats_header_id:stats_header_id, $
          done_button:done_button, src_signif_id:src_signif_id, ag_frac_id:ag_frac_id, prob_ks_id:prob_ks_id, $ 
        
          plot_name_list:plot_name_list, provisional_id:provisional_id, $
          
          flux_correction_limit:float(flux_correction_limit), $
          
          model_names:ptr_new(/ALLOC), num_models:num_models, $
          
          keylist:['EXTNAME',keylist], $
          current_rank:0L, current_source_index:-1L, $
          sourcename:sourcename, source_label:label, src_signif:src_signif, ag_frac:ag_frac, prob_ks:prob_ks, $
          merge_subdir:merge_subdir, $
          notes:notes, srclist_fn:srclist_fn, presentation_rank:presentation_rank, $
          
          gv_resource_file:gv_resource_file, no_model_file:local_no_model_file, tempdir:tempdir, gv_pids:ptr_new(/ALLOC) }

; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, header_base, /REALIZE
widget_control, top_base,    /REALIZE

event={ID:source_number_id, TOP:top_base, HANDLER:top_base}
ae_spectra_viewer_event, event

;dum = ae_spectra_viewer_event({ID:choose_button, TOP:top_base, HANDLER:top_base})

 
  
; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
; widget to behave when called by either widget applications or normal
; programs (which block on tty reads).
xmanager, 'ae_spectra_viewer', top_base, EVENT_HANDLER='ae_spectra_viewer_event', $
          JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0)
return
end



;==========================================================================
; Source reviewer tool.
; Useful for reviewing counterparts.
;==========================================================================

;At startup:
;- User responsible for loading images and region files.
;- Region file in frame 1 must have tags for each source label, to be used to highlight the selected source.
;- Read persistent storage (e.g. ASCII list of source names or labels) to identify approved sources; mark each one:
;  "xpaset frame 1"
;  "xpaset regions group foo_bar property include no"
;
;
;When a source is selected by ds9 mouse click:
;- Match frames to the one receiving the click.
;  "xpaset match frames wcs"
;- Compute distance from click to all sources (gcirc.pro) to identify closest source.
;
;When a source is selected by widget navigation controls:
;- Pan first frame to source position and match all frames.
;  "xpaset frame 1"
;  "xpaset match frames wcs"
;
;When a source is selected by any method:
;- Un-highlight the previously selected source.
;- Highlight the polygon belonging to the selected source.
;  "xpaset frame 1"
;  "xpaset regions select none"
;  "xpaset regions select group foo"
;  "xpaset regions width 3"
;  We manage the the "selected source" ourselves, rather than relying on the ds9 ability to select a region file because:
;  1. user can select muliple regions, violating the idea of the "current" source.
;  2. ...
;  
; 
;When a source is "committed"/"approved":
;- Mark the polygon belonging to the selected source.
;  "xpaset frame 1"
;  "xpaset regions group foo_bar property include no"
;- Record in some persistent storage that source has been approved.
;
;
;NOTES:
;- Use WCS for all coordinates in code and for all region files.
;- Composite catalog can provide all X-ray and IR properties we need to display.
;- Population plots will have to come from elsewhere.
;
;
;
;Two Modes of Tool
;* ds9 mouse/key input mode
;
;Widget has button to enter this mode.  Widget event handler for this button includes a loop that uses "imexam any" access point to get both mouse clicks and keystrokes from ds9.  Clicks select sources.  Meaningful keystrokes include:
;
;'a' to approve/commit the selected source.
;'q' to break out and resume widget processing (e.g. to annotate a source)
;
;
;* widget processing mode
;
;Widget includes button to enter ds9 mode, navigation controls, and annotation fields/flags.  Navigation controls include "next un-approved source".
;
;

;; =============================================================================
FUNCTION ae_source_viewer_print_source, cat, source_index, SKIP_PLOTS=skip_plots, INIT=init

COMMON ae_source_viewer, id1, id1b, id2, id2b
      
dum = check_math()

band_full = 0
large_phot_error = 0.1  ; mag

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

FLUX2     =             cat.ACIS.FLUX2             [band_full]
FLUX2_hi  = FLUX2*(1 + (cat.ACIS.NET_CNTS_SIGMA_UP [band_full] / cat.ACIS.NET_CNTS[band_full]))
;FLUX2_lo  = FLUX2*(1 - (cat.ACIS.NET_CNTS_SIGMA_LOW[band_full] / cat.ACIS.NET_CNTS[band_full]))

E_median =        cat.ACIS.ENERG_PCT50_OBSERVED[band_full]


if keyword_set(init) then begin
  id1=0 & id1b=0 & id2=0 & id2b=0
  
  ; Make population plots for Jiang data.
  is_match = (cat.IR.TYPE EQ successful_primary_type) AND (cat.IR.JIANG.TYPE EQ successful_primary_type)
  
  H_good = is_match AND (cat.IR.JIANG.EH        LT large_phot_error)
  K_good = is_match AND (cat.IR.JIANG.EK        LT large_phot_error)
  
  ind = where(K_good)
  function_1d, id1, (cat.IR.JIANG.K_CIT)  [ind], alog10(FLUX2[ind]),                PSYM=3, LINE=6, COLOR='green', DATASET='Jiang', XTIT='K',        YTIT='log NET_CNTS/MEAN_ARF/EXPOSURE', PLOT_WINDOW_OPTIONS='SET_XMARGIN=[6,0], SET_YMARGIN=[3,2], SHOW_DATE=0'
                                                                                                                                                     
; dataset_2d, id1b, (cat.IR.JIANG.K_CIT)  [ind], alog10(FLUX2[ind]),                PSYM=3,         COLOR='green', DATASET='Jiang', XTIT='K',        YTIT='log NET_CNTS/MEAN_ARF/EXPOSURE'

  ae_ds9_to_ciao_regionfile, 'Jiang_flux_K_96p7.reg', '/dev/null', POLYGON_X=polygon_x, POLYGON_Y=polygon_y
  
  function_1d, id1, polygon_x, polygon_y, LINE=0, COLOR='green', DATASET='96.7%'
; dataset_2d, id1b, polygon_x, polygon_y,         COLOR='green', DATASET='96.7%'
  
  
  ind = where(H_good AND K_good)
  function_1d, id2, E_median[ind], (cat.IR.JIANG.H_CIT - cat.IR.JIANG.K_CIT)[ind],  PSYM=3, LINE=6, COLOR='green', DATASET='Jiang', XTIT='E_median', YTIT='H-K',                            PLOT_WINDOW_OPTIONS='SET_XMARGIN=[6,0], SET_YMARGIN=[3,2], SHOW_DATE=0'
  
; dataset_2d, id2b, E_median[ind], (cat.IR.JIANG.H_CIT - cat.IR.JIANG.K_CIT)[ind],  PSYM=3,         COLOR='green', DATASET='Jiang', XTIT='E_median', YTIT='H-K'

  ae_ds9_to_ciao_regionfile, 'Jiang_color_absorption_96p7.reg', '/dev/null', POLYGON_X=polygon_x, POLYGON_Y=polygon_y
  
  function_1d, id2, polygon_x, polygon_y, LINE=0, COLOR='green', DATASET='96.7%'
; dataset_2d, id2b, polygon_x, polygon_y,         COLOR='green', DATASET='96.7%'
  
  
  ; Make population plots for 2MASS data.
  is_match = (cat.IR.TYPE EQ successful_primary_type) AND (cat.IR.TWOMASS.TYPE EQ successful_primary_type)

  H_good = is_match AND (cat.IR.TWOMASS.H_CMSIG LT large_phot_error) AND ~strmatch(cat.IR.TWOMASS.PH_QUAL, '?U?') AND ~strmatch(cat.IR.TWOMASS.PH_QUAL, '?E?')
  K_good = is_match AND (cat.IR.TWOMASS.K_CMSIG LT large_phot_error) AND ~strmatch(cat.IR.TWOMASS.PH_QUAL, '??U') AND ~strmatch(cat.IR.TWOMASS.PH_QUAL, '??E')

  ind = where(K_good)
  function_1d, id1, (cat.IR.TWOMASS.K_CIT)[ind], alog10(FLUX2[ind]), PSYM=3, LINE=6, COLOR='blue', DATASET='2MASS'

;  dataset_2d, id1b, (cat.IR.TWOMASS.K_CIT)[ind], alog10(FLUX2[ind]), PSYM=3,         COLOR='blue', DATASET='2MASS'
  
  ae_ds9_to_ciao_regionfile, '2MASS_flux_K_96p8.reg', '/dev/null', POLYGON_X=polygon_x, POLYGON_Y=polygon_y
  
  function_1d, id1, polygon_x, polygon_y, LINE=0, COLOR='blue', DATASET='96.8%'
; dataset_2d, id1b, polygon_x, polygon_y,         COLOR='blue', DATASET='96.8%'


  ind = where(H_good AND K_good)
  function_1d, id2, E_median[ind], (cat.IR.TWOMASS.H_CIT - cat.IR.TWOMASS.K_CIT)[ind],  PSYM=3, LINE=6, COLOR='blue', DATASET='2MASS'

;  dataset_2d,  id2b, E_median[ind], (cat.IR.TWOMASS.H_CIT - cat.IR.TWOMASS.K_CIT)[ind],  PSYM=3,         COLOR='blue', DATASET='2MASS'

  ae_ds9_to_ciao_regionfile, '2MASS_color_absorption_96p5.reg', '/dev/null', POLYGON_X=polygon_x, POLYGON_Y=polygon_y
  
  function_1d, id2, polygon_x, polygon_y, LINE=0, COLOR='blue', DATASET='96.5%'
; dataset_2d, id2b, polygon_x, polygon_y,         COLOR='blue', DATASET='96.5%'

  if keyword_set(skip_plots) then begin
    widget_control, id1 , /DESTROY, BAD_ID=bad_id
    widget_control, id1b, /DESTROY, BAD_ID=bad_id
    widget_control, id2 , /DESTROY, BAD_ID=bad_id
    widget_control, id2b, /DESTROY, BAD_ID=bad_id
  endif
  return, 0
endif

source = cat[source_index]

if max(abs([source.ACIS.ENERG_LO[band_full] - 0.5, source.ACIS.ENERG_HI[band_full] - 8.0])) GT 0.01 then begin
  return, string(band_full, F='(%"\nERROR: Band %d is not 0.5:8.0 keV")')
endif



; Show the selected sources on the scatter plots with no screening.
if (source.IR.TYPE EQ successful_primary_type) then begin
  ; Jiang data                            
  H_good = (source.IR.JIANG.TYPE EQ successful_primary_type)
  K_good = (source.IR.JIANG.TYPE EQ successful_primary_type)

  K_CIT  = K_good           ? source.IR.JIANG.K_CIT                         : !VALUES.F_NAN
  HK_CIT = H_good && K_good ? source.IR.JIANG.H_CIT - source.IR.JIANG.K_CIT : !VALUES.F_NAN
  
  H_error  = source.IR.JIANG.EH
  K_error  = source.IR.JIANG.EK

  ; 2MASS data.
  H_good = (source.IR.TWOMASS.TYPE EQ successful_primary_type) AND ~strmatch(source.IR.TWOMASS.PH_QUAL, '?U?') AND ~strmatch(source.IR.TWOMASS.PH_QUAL, '?E?')
  K_good = (source.IR.TWOMASS.TYPE EQ successful_primary_type) AND ~strmatch(source.IR.TWOMASS.PH_QUAL, '??U') AND ~strmatch(source.IR.TWOMASS.PH_QUAL, '??E')
  
  K_CIT  = [  K_CIT, K_good           ? source.IR.TWOMASS.K_CIT                           : !VALUES.F_NAN ]
  HK_CIT = [ HK_CIT, H_good && K_good ? source.IR.TWOMASS.H_CIT - source.IR.TWOMASS.K_CIT : !VALUES.F_NAN ]

  H_error  = [H_error, source.IR.TWOMASS.H_CMSIG]
  K_error  = [K_error, source.IR.TWOMASS.K_CMSIG]
  
  HK_error = sqrt(H_error^2 + K_error^2)
  
endif else begin
  K_CIT    = replicate(!VALUES.F_NAN,2)
  HK_CIT   = replicate(!VALUES.F_NAN,2)
  K_error  = replicate(!VALUES.F_NAN,2)
  HK_error = replicate(!VALUES.F_NAN,2)
endelse

if ~keyword_set(skip_plots) then begin
  s = [source_index,source_index]
  function_1d, id1, K_CIT,     PSYM=4, LINE=6, COLOR='red', DATASET='Selected', NSKIP_ERRORS=1, $
            X_ERROR=K_error, $
                                 alog10(FLUX2[s]), $
    ERROR=(alog10(FLUX2_hi[s]) - alog10(FLUX2[s]))
  
  function_1d, id2, E_median[s], X_ERROR=[0,0], HK_CIT, ERROR=HK_error, PSYM=4, LINE=6, COLOR='red', DATASET='Selected', NSKIP_ERRORS=1
endif



; Build a report summarizing source properties.
acis_report = strarr(80)
line = 0

if (source.ACIS07.TYPE EQ successful_primary_type) then begin
  acis_report[line++] = string(source.ACIS07.ID, F="(%'ACIS ID in Broos07 is %d')")
              line++
endif

N=1+source.IR.NUM_SM
if (N GT 1) then begin
  acis_report[line++] = string(N,F="(%'ACIS position is consistent with %d IR sources.')")
              line++
endif

;acis_report[line++] = string(source.ACIS.POSNTYPE,F="(%'ACIS position comes from   : %s.')")
;       line++

acis_report[line++] = string(source.ACIS.THETA,    F="(%'off-axis angle (arcmin)    : %0.1f')")

text = 'no evidence'
if (source.ACIS.PROB_KS LT 0.05)  then text = 'possible'
if (source.ACIS.PROB_KS LT 0.005) then text = 'definite'
if ~finite(source.ACIS.PROB_KS) then text = '...'

acis_report[line++] = 'variability                : '+text
            line++

PROB_NO_SOURCE = source.ACIS.PROB_NO_SOURCE[band_full]
acis_report[line++] = string(source.ACIS.ENERG_LO[band_full],source.ACIS.ENERG_HI[band_full], $
                        (PROB_NO_SOURCE LT 1E-5) ? "<0.00001" : string(PROB_NO_SOURCE, F='(%"%0.5f")'), $
                        F="(%'P_B           (%0.1f:%d keV)  : %s')")
        
acis_report[line++] = string(source.ACIS.ENERG_LO[band_full],source.ACIS.ENERG_HI[band_full],source.ACIS.NET_CNTS[band_full], F="(%'net counts    (%0.1f:%d keV)  : %d')")

acis_report[line++] = string(source.ACIS.ENERG_LO[band_full],source.ACIS.ENERG_HI[band_full],source.ACIS.ENERG_PCT50_OBSERVED[band_full], F="(%'median energy (%0.1f:%d keV)  : %0.1f')")
            line++

acis_report = acis_report[0:line]



ir_report   = strarr(80)
line = 0

; Here, we summarize IR info for *either* a successful or failed Primary mMatch between ACIS and IR.  
; But below we decide how to present the info based on the type of match.
if (source.IR.TYPE EQ successful_primary_type) || (source.IR.TYPE EQ failed_primary_type)  then begin
  ; However, within the IR catalog, we are interested only in successful Primary Matches to published catalogs.
  if (source.IR.JIANG.TYPE EQ successful_primary_type) then begin
    J_CIT = source.IR.JIANG.J_CIT 
    H_CIT = source.IR.JIANG.H_CIT 
    K_CIT = source.IR.JIANG.K_CIT 
    JHval = string(J_CIT - H_CIT, F="(%'%4.1f')")
    HKval = string(H_CIT - K_CIT, F="(%'%4.1f')")
 
    Jval = string(' ', $
                           J_CIT, $
                         ((source.IR.JIANG.EJ GT large_phot_error) || ~finite(source.IR.JIANG.EJ)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if ~finite(J_CIT) then begin
      Jval  = '  ... '
      JHval = ' ...'
    endif
    
    Hval = string(' ', $
                           H_CIT, $
                         ((source.IR.JIANG.EH GT large_phot_error) || ~finite(source.IR.JIANG.EH)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if ~finite(H_CIT) then begin
      Hval  = '  ... '
      JHval = ' ...'
      HKval = ' ...'
    endif
  
    Kval = string(' ', $
                           K_CIT, $
                         ((source.IR.JIANG.EK GT large_phot_error) || ~finite(source.IR.JIANG.EK)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if ~finite(K_CIT) then begin
      Kval  = '  ... '
      HKval = ' ...'
    endif
 
    Jiang_id   = string(source.IR.JIANG.ID, '',         F="(%'%16s %3s')")
    Jiang_vals = string(Jval, Hval, Kval, JHval, HKval, F="(%'%s %s %s %s %s')")
    Jiang_vals = repstr(Jiang_vals,'NaN','...')
  endif else begin
    Jiang_id   = ''
    Jiang_vals = ''
  endelse
  
  
  ; However, within the IR catalog, we are interested only in successful Primary Matches to published catalogs.
  if (source.IR.TWOMASS.TYPE EQ successful_primary_type) then begin
    J_CIT = source.IR.TWOMASS.J_CIT 
    H_CIT = source.IR.TWOMASS.H_CIT 
    K_CIT = source.IR.TWOMASS.K_CIT 
    JHval = string(J_CIT - H_CIT, F="(%'%4.1f')")
    HKval = string(H_CIT - K_CIT, F="(%'%4.1f')")
     
    Jval = string(strmatch(source.IR.TWOMASS.PH_QUAL, 'U??')                                                     ? '>' : ' ', $
                           J_CIT, $
                         ((source.IR.TWOMASS.J_CMSIG GT large_phot_error) || ~finite(source.IR.TWOMASS.J_CMSIG)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if strmatch(source.IR.TWOMASS.PH_QUAL, 'E??') || ~finite(J_CIT) then begin
      Jval  = '  ... '
      JHval = ' ...'
    endif
    
    Hval = string(strmatch(source.IR.TWOMASS.PH_QUAL, '?U?')                                                     ? '>' : ' ', $
                           H_CIT, $
                         ((source.IR.TWOMASS.H_CMSIG GT large_phot_error) || ~finite(source.IR.TWOMASS.H_CMSIG)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if strmatch(source.IR.TWOMASS.PH_QUAL, '?E?') || ~finite(H_CIT) then begin
      Hval  = '  ... '
      JHval = ' ...'
      HKval = ' ...'
    endif

    Kval = string(strmatch(source.IR.TWOMASS.PH_QUAL, '??U')                                                     ? '>' : ' ', $
                           K_CIT, $
                         ((source.IR.TWOMASS.K_CMSIG GT large_phot_error) || ~finite(source.IR.TWOMASS.K_CMSIG)) ? ':' : ' ', $
                  F="(%'%s%4.1f%s')")
    if strmatch(source.IR.TWOMASS.PH_QUAL, '??E') || ~finite(K_CIT) then begin
      Kval  = '  ... '
      HKval = ' ...'
    endif
    
    Twomass_id = string(source.IR.TWOMASS.DESIGNATION, source.IR.TWOMASS.CC_FLG, F="(%'%16s %3s')")
    Twomass_vals = string(Jval, Hval, Kval, JHval, HKval,                        F="(%'%s %s %s %s %s')")
    Twomass_vals = repstr(Twomass_vals,'NaN','...')
  endif else begin
    Twomass_id   = ''
    Twomass_vals = ''
  endelse

  
  ; However, within the IR catalog, we are interested only in successful Primary Matches to published catalogs.
  if (source.IR.GLIMPSE.TYPE EQ successful_primary_type) then begin
    MAG3_6 = string(source.IR.GLIMPSE.MAG3_6, F="(%'%5.1f')")
    MAG4_5 = string(source.IR.GLIMPSE.MAG4_5, F="(%'%5.1f')")
    MAG5_8 = string(source.IR.GLIMPSE.MAG5_8, F="(%'%5.1f')")
    MAG8_0 = string(source.IR.GLIMPSE.MAG8_0, F="(%'%5.1f')")
                              
    Glimpse_id   = string(source.IR.GLIMPSE.DESIGNATION, F="(%'%s')")
    Glimpse_vals = string(MAG3_6,MAG4_5,MAG5_8,MAG8_0,   F="(%'%s %s %s %s')")
    Glimpse_vals = repstr(Glimpse_vals,'NaN','...')
  endif else begin
    Glimpse_id   = ''
    Glimpse_vals = ''
  endelse

  ir_report[line++] = '   J      H      K    J-H  H-K    3.6   4.5   5.8   8.0        NIR ID             MIR ID'
  ir_report[line++] = ' ------------------------------  ---------------------- -------------------- --------------------'

  
  ; Display Jiang first if available.
  if (source.IR.JIANG.TYPE EQ successful_primary_type) then begin
    ir_report[line++] = string(  Jiang_vals, Glimpse_vals,   Jiang_id, Glimpse_id, F="(%'%30s  %23s %20s %s')")
    ir_report[line++] = string(Twomass_vals, '',             Twomass_id,           F="(%'%30s  %23s %20s')")
  endif else begin
    ir_report[line++] = string(Twomass_vals, Glimpse_vals, Twomass_id, Glimpse_id, F="(%'%30s  %23s %20s %s')")
    ir_report[line++] = string(  Jiang_vals, '',             Jiang_id,             F="(%'%30s  %23s %20s')")
  endelse
  
endif else begin
 ir_report[line++] = 'NO IR MATCH.'
endelse

       line++
ir_report = ir_report[0:line]


legend   = strarr(80)
line = 0
legend[line++] = 'Region Legend:'   
legend[line++] = '  ACIS        :diamond  green (match), cyan (no match), red (failed match)' 
legend[line++] = '  IR          :+        green (match), cyan (no match), magenta (secondary match)'
legend[line++] = '    GLIMPSE   :box      blue' 
legend[line++] = '    Jiang     :circle   blue'  
legend[line++] = '    2MASS     :X        blue' 
       line++
legend[line++] = 'Around each ACIS source a graphic depicts the domain over which'
legend[line++] = 'an IR source with zero position error could match the ACIS source.'  
       line++
legend[line++] = '- For unmatched ACIS sources (cyan diamond) a large cyan circle'
legend[line++] = '  depicts this domain.'  
       line++
legend[line++] = '- For matched ACIS sources (green or red diamond) the line region'
legend[line++] = '  depicting the match extends from the ACIS source through the'
legend[line++] = '  IR source out to the distance where the match would have failed'
legend[line++] = '  if the IR position error was zero.' 
legend = legend[0:line]

dum = check_math()

case source.IR.TYPE of
  successful_primary_type: $
    begin
    return, [acis_report, ir_report, legend]
    end
  failed_primary_type    : $
    begin
    print
    print, 'IR Properties of Failed Match:'
    print, ir_report
    return, [acis_report, 'POTENTIAL IR MATCH ASSIGNED TO ANOTHER ACIS SOURCE.', '','', legend]
    end
  else                   : $
    begin
    return, [acis_report, ir_report, legend]
    end
endcase



end


;; =============================================================================
;; Procedure to handle GUI updates when a source is selected.
PRO ae_source_viewer_select_source, st, source_index, PAN_TO_SOURCE=pan_to_source

; Range check the source number.
num_sources = n_elements((*st).sourcename)
source_index = 0 > source_index < (num_sources-1)

; If the cat has been modified, now is a good time to save it to disk.
if ~keyword_set((*st).read_only) && ((*st).pending_save GT 0) then begin
  ; Write the modified catalog structure to disk.
  if (((*st).save_counter++ MOD 100) EQ 0) then begin
    print, 'Backing up ', (*st).catalog_fn
    ; Periodically save a backup copy of the catalog structure.
    for ii=9,1,-1 do begin
      backup_fn       = string((*st).catalog_fn,ii,  F='(%"%s-%d")')
      older_backup_fn = string((*st).catalog_fn,ii+1,F='(%"%s-%d")')
      if file_test(backup_fn) then file_move, backup_fn, older_backup_fn, /OVERWRITE
    endfor
    
    if file_test((*st).catalog_fn) then file_move, (*st).catalog_fn, backup_fn, /OVERWRITE
  endif

  cat = (*st).cat
  save, cat, FILE=(*st).catalog_fn
  print, 'Saved ', (*st).catalog_fn
  (*st).pending_save = 0
endif


run_command, /QUIET, string((*st).my_ds9,             F='(%"xpaset -p %s frame first")')

if ((*st).current_source_index GE 0) then begin
  ; Un-highlight the currently-selected source.
   this_source_label = (*st).source_label[(*st).current_source_index]
   run_command, /QUIET, string((*st).my_ds9,                    F='(%"xpaset -p %s regions select none")')
   run_command, /QUIET, string((*st).my_ds9, this_source_label, F='(%"xpaset -p %s regions select group %s")')
   run_command, /QUIET, string((*st).my_ds9,                    F='(%"xpaset -p %s regions width 1")')
endif

(*st).current_source_index = source_index

if keyword_set(pan_to_source) then begin
  run_command, /QUIET, string((*st).my_ds9, (*st).RA[source_index], (*st).DEC[source_index], $
                                            F='(%"xpaset -p %s pan to %10.6f %10.6f wcs fk5 degrees")')
  ;After panning, we may have falled off of the image ds9 built from the event data; we must force ds9 to re-compute that image.
  run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s bin function sum")')
  run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s match frames wcs")')
endif

; Now we can highlight the newly-selected source and update the GUI
this_sourcename   = (*st).sourcename  [source_index]
this_source_label = (*st).source_label[source_index]

run_command, /QUIET, string((*st).my_ds9,                    F='(%"xpaset -p %s regions select none")')
run_command, /QUIET, string((*st).my_ds9, this_source_label, F='(%"xpaset -p %s regions select group %s")')
run_command, /QUIET, string((*st).my_ds9,                    F='(%"xpaset -p %s regions width 3")')
       
       
; If necessary, add the annotation to the list of options.
annotation_string = strtrim((*st).cat[source_index].UNRELIABLE_ANNOTATION,2)
if (annotation_string EQ '') then annotation_string = ' '

widget_control, (*st).unreliable_annotation_id, GET_VALUE=annotation_list
ind = where(annotation_list EQ annotation_string, count)
if (count EQ 0) then annotation_list = [annotation_list, annotation_string]

; Display the annotations. 
ind = where(annotation_list EQ annotation_string, count)
widget_control, (*st).unreliable_annotation_id, SET_VALUE=annotation_list, SET_COMBOBOX_SELECT=ind, SENSITIVE=((*st).cat[source_index].REVIEW_STATE EQ 'unreliable')


; If necessary, add the annotation to the list of options.
annotation_string = strtrim((*st).cat[source_index].PROVISIONAL_ANNOTATION,2)
if (annotation_string EQ '') then annotation_string = ' '

widget_control, (*st).provisional_annotation_id, GET_VALUE=annotation_list
ind = where(annotation_list EQ annotation_string, count)
if (count EQ 0) then annotation_list = [annotation_list, annotation_string]

; Display the annotations. 
ind = where(annotation_list EQ annotation_string, count)
widget_control, (*st).provisional_annotation_id, SET_VALUE=annotation_list, SET_COMBOBOX_SELECT=ind, SENSITIVE=((*st).cat[source_index].REVIEW_STATE EQ 'provisional')

ind = where(strmatch((*st).review_state_names, (*st).cat[source_index].REVIEW_STATE), count)
if (count EQ 0) then ind = 0
widget_control, (*st).review_state_id,  SET_VALUE=ind


widget_control, (*st).flag_group_id, SET_VALUE=[(*st).cat[source_index].uncatalogued_NIR,$
                                                (*st).cat[source_index].uncatalogued_MIR          ,$
                                                (*st).cat[source_index].nebular_contamination   ]

widget_control, (*st).source_name_id,   SET_VALUE=this_sourcename
widget_control, (*st).source_label_id,  SET_VALUE=this_source_label    
widget_control, (*st).source_number_id, SET_VALUE=1+source_index

;; Read any existing analysis notes file.  
;; We cannot use readcol because it won't read comment lines with spaces.
notes_fn = (*st).cat[source_index].ACIS.CATALOG_NAME + '/notes.txt'

if file_test(notes_fn) then begin
  Nlines = file_lines(notes_fn) 
endif else Nlines = 0

if (Nlines GT 0) then begin
  notes = strarr(Nlines)
  openr, unit, notes_fn, /GET_LUN
  readf, unit, notes
  free_lun, unit
endif else begin
  notes = strarr(1)
endelse
widget_control, (*st).analysis_notes_id, SET_VALUE=notes


;; Read any existing footnotes file.  
;; We cannot use readcol because it won't read comment lines with spaces.
notes_fn = (*st).cat[source_index].ACIS.CATALOG_NAME + '/footnotes.txt'

if file_test(notes_fn) then begin
  Nlines = file_lines(notes_fn) 
endif else Nlines = 0

if (Nlines GT 0) then begin
  notes = strarr(Nlines)
  openr, unit, notes_fn, /GET_LUN
  readf, unit, notes
  free_lun, unit
endif else begin
  notes = strarr(1)
endelse
widget_control, (*st).footnotes_id, SET_VALUE=notes



print, this_sourcename, this_source_label, F='(%"\n----------------------------\n%s (%s)")'
temp = (*st).notes[source_index]
if keyword_set(temp) then print, temp

;; Show the source report
if (widget_info( (*st).report_id, /VALID_ID )) then $
  widget_control, (*st).report_id, SET_VALUE=ae_source_viewer_print_source( (*st).cat, source_index, SKIP_PLOTS=(*st).skip_plots )
  
return
end                                                                                                                                    


;; =============================================================================
;;; Widget Event Handler Procedure
PRO ae_source_viewer_event, event

widget_control, /HOURGLASS


;; Get the state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=st                                                                                                 


num_sources  = n_elements((*st).sourcename)

source_index = 0 > (*st).current_source_index < (num_sources-1)

    
;; Process the event.
DestroyFlag = 0
case event.ID of

;--------------------------------------------------------------------------
  (*st).review_state_id: $
   begin
   (*st).cat[source_index].REVIEW_STATE = Event.value
   (*st).pending_save++
   widget_control, (*st).unreliable_annotation_id,  SENSITIVE=(Event.value EQ 'unreliable')
   widget_control, (*st).provisional_annotation_id, SENSITIVE=(Event.value EQ 'provisional')
   
   run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s frame first")')
   run_command, /QUIET, string((*st).my_ds9, (*st).source_label[source_index], (Event.value NE 'unreviewed') ? "no" : "yes", F='(%"xpaset -p %s regions group ''%s'' property include %s")')
   end
  
;--------------------------------------------------------------------------
  (*st).unreliable_annotation_id: $
   begin
   ; Save the observer's string.
   input_string = strtrim(Event.STR,2)
   (*st).cat[source_index].UNRELIABLE_ANNOTATION = input_string
   (*st).pending_save++
   
   ; And incorporate it into the list of options.
   widget_control, (*st).unreliable_annotation_id, GET_VALUE=annotation_list
   if (input_string NE '') && (total(/INT, annotation_list EQ input_string) EQ 0) then begin
     annotation_list = [annotation_list, input_string]
     widget_control, (*st).unreliable_annotation_id, SET_VALUE=annotation_list, SET_COMBOBOX_SELECT=n_elements(annotation_list)-1
   endif
   end
   
;--------------------------------------------------------------------------
  (*st).provisional_annotation_id: $
   begin
   ; Save the observer's string.
   input_string = strtrim(Event.STR,2)
   (*st).cat[source_index].PROVISIONAL_ANNOTATION = input_string
   (*st).pending_save++
   
   ; And incorporate it into the list of options.
   widget_control, (*st).provisional_annotation_id, GET_VALUE=annotation_list
   if (input_string NE '') && (total(/INT, annotation_list EQ input_string) EQ 0) then begin
     annotation_list = [annotation_list, input_string]
     widget_control, (*st).provisional_annotation_id, SET_VALUE=annotation_list
     widget_control, (*st).provisional_annotation_id, SET_COMBOBOX_SELECT=n_elements(annotation_list)-1
   endif
   end
   

;--------------------------------------------------------------------------
  (*st).flag_group_id: $
   begin
   widget_control, (*st).flag_group_id, GET_VALUE=value
   (*st).cat[source_index].uncatalogued_NIR        = value[0]
   (*st).cat[source_index].uncatalogued_MIR        = value[1]
   (*st).cat[source_index].nebular_contamination   = value[2]
   (*st).pending_save++
   end
   

;--------------------------------------------------------------------------
  (*st).mouse_select_button: $
   begin
   print, F="(%'\n\n  Left-click selects source.')" 

   ; Ask ds9 for a mouse click or keystroke and parse the result
   run_command, /QUIET, string((*st).my_ds9, F='(%"xpaget %s imexam any coordinate wcs fk5 degrees")'), result
   tokens = stregex(result,'(.+) +([0-9]+\.[0-9]+) +(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
   keystroke = strtrim(tokens[1],2)
   ra_click  = double(tokens[2])
   dec_click = double(tokens[3])
   click_frame = 1
   if (strlen(keystroke) GT 1) then keystroke=''
   
   ;--------------------------------------------------------------------------
   ; Select the source nearest to the mouse click or keystroke.
   ; An event off the image returns zeros for coordinates.
   deghour = 24D/360
   gcirc, 1, (*st).RA*deghour, (*st).DEC,  ra_click*deghour, dec_click,  distance
   
   far_from_source = (ra_click EQ 0) || (dec_click EQ 0) || (min(distance, source_index) GT 10.0)
   
   ; Match frames to the one receiving the click.
   ;run_command, /QUIET, string((*st).my_ds9, click_frame, F='(%"xpaset -p %s frame %d")')
   ;run_command, /QUIET, string((*st).my_ds9,              F='(%"xpaset -p %s match frames wcs")')

   ; Display current status of selected source.
   if ~far_from_source then ae_source_viewer_select_source, st, source_index
   
   ; Flush any widget events that may have arrived while in ds9 mode.
   widget_control, top_base, /CLEAR_EVENTS
   end


;--------------------------------------------------------------------------
  (*st).approve_multiple_button: $
   begin
   print, F="(%'\nInteractively select and approve sources in ds9:\n  Left-click selects source.\n  Key ""a"" selects source and sets approved flag.\n  Key ""m"" matches all frames to the selected one.')"
   print, F="(%'  To interact with IDL widget press ""q"" or left-click more than 10 arcsec from any source.')"
   
   finished = 0
   repeat begin
     update_widget = 0
     ; Ask ds9 for a mouse click or keystroke and parse the result
     run_command, /QUIET, string((*st).my_ds9, F='(%"xpaget %s imexam any coordinate wcs fk5 degrees")'), result
     tokens = stregex(result,'(.+) +([0-9]+\.[0-9]+) +(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
     keystroke = strtrim(tokens[1],2)
     ra_click  = double(tokens[2])
     dec_click = double(tokens[3])
     click_frame = 1
     if (strlen(keystroke) GT 1) then keystroke=''
     
 ;help,  keystroke, ra_click, dec_click    
      
     ; An event off the image returns zeros for coordinates.
     ; If event is mouse click then exit this mode.
     if (keystroke EQ '') && (ra_click*dec_click EQ 0) then break
      
     ;--------------------------------------------------------------------------
     ; Select the source nearest to the mouse click or keystroke.
     deghour = 24D/360
     gcirc, 1, (*st).RA*deghour, (*st).DEC,  ra_click*deghour, dec_click,  distance
     
     far_from_source = (min(distance, source_index) GT 10.0)
     
     ; Match frames to the one receiving the click.
     ;run_command, /QUIET, string((*st).my_ds9, click_frame, F='(%"xpaset -p %s frame %d")')
     ;run_command, /QUIET, string((*st).my_ds9,              F='(%"xpaset -p %s match frames wcs")')
  
     ;--------------------------------------------------------------------------
     ; Respond to Keystroke
     case keystroke of
      '':  begin
           if far_from_source then begin
             finished = 1
             break
           endif
           update_widget = 1
           end
           
      'a': begin
           if far_from_source then begin
             print, 'Place cursor closer to a source.'
             break
           endif
           ; Set REVIEW_STATE to "approved" for the currently-selected source.
           (*st).cat[source_index].REVIEW_STATE = 'approved'
           (*st).pending_save++
           run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s frame first")')
           run_command, /QUIET, string((*st).my_ds9, (*st).source_label[source_index], "no", F='(%"xpaset -p %s regions group ''%s'' property include %s")')
           update_widget = 1
           end
           
      'm': begin
           run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s match frames wcs")')
           end

      'q': begin
           ; Exit the ds9 Mode and return to widget event processing.
           finished = 1
           end
           
      else:
     endcase
     
     ; Display current status of selected source.
     if update_widget then ae_source_viewer_select_source, st, source_index

   endrep until finished
   
   ; Flush any widget events that may have arrived while in ds9 mode.
   widget_control, top_base, /CLEAR_EVENTS
    
   end

   
   
;--------------------------------------------------------------------------
  (*st).match_button: run_command, /QUIET, string((*st).my_ds9, F='(%"xpaset -p %s match frames wcs")')
   
   
;--------------------------------------------------------------------------
  (*st).source_number_id: $
   begin
   widget_control, (*st).source_number_id, GET_VALUE=source_number                                       
   ae_source_viewer_select_source, st, source_number-1, /PAN_TO_SOURCE
   end

;--------------------------------------------------------------------------
  (*st).source_label_id: $
   begin
   widget_control, (*st).source_label_id, GET_VALUE=source_label                                       
   ind = where(strlowcase(strtrim(source_label[0],2)) EQ strlowcase((*st).source_label), count)
   if (count GE 1) then begin
     source_index = ind[0]
     ae_source_viewer_select_source, st, source_index, /PAN_TO_SOURCE
   endif else print, 'Cannot find that source label.'
   end
   
;--------------------------------------------------------------------------
  (*st).prev_button: $
   begin
   source_index-- 
   source_index >= 0 
   ae_source_viewer_select_source, st, source_index, /PAN_TO_SOURCE
   end

;--------------------------------------------------------------------------
  (*st).next_button: $
   begin
   source_index++ 
   source_index <= (num_sources-1)                                                                                                
   ae_source_viewer_select_source, st, source_index, /PAN_TO_SOURCE
   end

;--------------------------------------------------------------------------
  (*st).prev_rank_button: $
   begin
   if ((*st).current_rank LE 0) then begin
     print, 'You are at the first source in '+(*st).srclist_fn
   endif else begin
     (*st).current_rank--
     ind = where((*st).presentation_rank EQ (*st).current_rank, count)
     if (count NE 1) then message, 'BUG!'
     ae_source_viewer_select_source, st, ind[0], /PAN_TO_SOURCE
   endelse
   end

;--------------------------------------------------------------------------
  (*st).next_rank_button: $
   begin
   if ((*st).current_rank GE max((*st).presentation_rank)) then begin
     print, 'You are at the last source in '+(*st).srclist_fn
   endif else begin
     (*st).current_rank++
     ind = where((*st).presentation_rank EQ (*st).current_rank, count)
     if (count NE 1) then message, 'BUG!'
     ae_source_viewer_select_source, st, ind[0], /PAN_TO_SOURCE
   endelse
   end

;--------------------------------------------------------------------------
  (*st).analysis_notes_id: $
   begin
   ; When the text widget is losing keyboard focus we must save its contents.
   if (tag_names(Event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') && (Event.ENTER EQ 0) then begin
     ;help, /st, event
     notes_fn = (*st).cat[source_index].ACIS.CATALOG_NAME + '/notes.txt'
     widget_control, (*st).analysis_notes_id, GET_VALUE=notes
     
     notes = strtrim(notes,0)
     
     ; If the file exists, then we must write it so the user is able to remove all the text.
     if file_test(notes_fn) ||(total(/INT, strlen(notes)) GT 0) then begin
       ;; Maintain several backups of the notes file.
       for ii=6,1,-1 do begin
         backup_fn       = string(notes_fn,ii,  F='(%"%s-%d")')
         older_backup_fn = string(notes_fn,ii+1,F='(%"%s-%d")')
         if file_test(backup_fn) then file_move, backup_fn, older_backup_fn, /OVERWRITE
       endfor
      
       if file_test(notes_fn) then file_move, notes_fn, backup_fn, /OVERWRITE
       
       ; Save the modified notes.
       print, 'Saving analysis notes to ', notes_fn
       forprint, TEXTOUT=notes_fn, notes, /NoCOMMENT, /SILENT
     endif
   endif
   end
 
   
;--------------------------------------------------------------------------
  (*st).footnotes_id: $
   begin
   ; When the text widget is losing keyboard focus we must save its contents.
   if (tag_names(Event, /STRUCTURE_NAME) EQ 'WIDGET_KBRD_FOCUS') && (Event.ENTER EQ 0) then begin
     ;help, /st, event
     notes_fn = (*st).cat[source_index].ACIS.CATALOG_NAME + '/footnotes.txt'
     widget_control, (*st).footnotes_id, GET_VALUE=notes
     
     notes = strtrim(notes,0)
     
     ; If the file exists, then we must write it so the user is able to remove all the text.
     if file_test(notes_fn) || (total(/INT, strlen(notes)) GT 0) then begin
       ;; Maintain several backups of the notes file.
       for ii=6,1,-1 do begin
         backup_fn       = string(notes_fn,ii,  F='(%"%s-%d")')
         older_backup_fn = string(notes_fn,ii+1,F='(%"%s-%d")')
         if file_test(backup_fn) then file_move, backup_fn, older_backup_fn, /OVERWRITE
       endfor
      
       if file_test(notes_fn) then file_move, notes_fn, backup_fn, /OVERWRITE
       
       ; Save the modified notes.
       print, 'Saving footnotes to ', notes_fn
       forprint, TEXTOUT=notes_fn, notes, /NoCOMMENT, /SILENT
     endif
   endif
   end
   
   
;--------------------------------------------------------------------------
  (*st).done_button: $
   begin
   DestroyFlag = 1
   ; Call ae_source_viewer_select_source one more time so it can save the modified catalog to disk.
   (*st).pending_save = 1000
   ae_source_viewer_select_source, st, source_index
   end
   
;--------------------------------------------------------------------------
 else: begin
       print, 'unknown event in ae_source_viewer'
       help,/ST,Event
       end
endcase
  




; DONE button
if (DestroyFlag) then begin
  ; Remove the temp_dir.
  files = file_search((*st).tempdir+'/*', /MATCH_INITIAL_DOT, COUNT=count)
  if (count GT 0) then file_delete, files, /ALLOW_NONEXISTENT
  file_delete, (*st).tempdir, /ALLOW_NONEXISTENT

  widget_control, top_base, /DESTROY
endif


return
end


;==========================================================================
;;; Widget Creation Routine

;;; Before calling this tool, create a ds9 session named 'ae_source_viewer' that shows the data and regions that you wish to see, e.g.
;;;   ds9 -title ae_source_viewer -tile iarray.source_search.evt -region polygons.reg -rgb -rgb lock colorbar yes  -red tj2.fits -green th2.fits -blue tk2.fits -region ir_cat.reg &
;;; The first frame must have regions that are tagged by the AE source label; this tool uses those tags to highlight the selected source.

;;; catalog_fn:
;;;   The name of an IDL savefile that holds an array of structures named "cat" that represents our catalog.
;;;   Annotations entered by the observer via this tool are stored in this data structure.
 ;==========================================================================
PRO ae_source_viewer, catalog_fn, SRCLIST_FILENAME=srclist_fn, DS9_TITLE=ds9_title, $
                      NOTES=notes, SKIP_PLOTS=skip_plots, BLOCK=block, READ_ONLY=read_only

resolve_routine, 'match_xy', /COMPILE_FULL_FILE

if ~keyword_set(ds9_title) then ds9_title = 'ae_source_viewer'
if ~keyword_set(catalog_fn) then catalog_fn = 'cat.sav'

;; Restore catalog.
if ~file_test(catalog_fn) then begin
  print, 'ERROR: cannot find file ', catalog_fn
  retall
endif

cat = ''
dum = temporary(cat)
restore, catalog_fn
if (n_elements(cat) EQ 0) then begin
  print, 'ERROR: savefile ', catalog_fn, ' must contain a structure array named "cat".'
  retall
endif

num_sources = n_elements(cat)

sourcename  = strtrim(cat.ACIS.CATALOG_NAME,2)
label       = strtrim(cat.ACIS.LABEL       ,2)
RA          = cat.ACIS.RA
DEC         = cat.ACIS.DEC


case n_elements(notes) of
  0: notes = strarr(num_sources)
  1: notes = replicate(notes, num_sources)
  num_sources: 
  else: begin
        print, 'ERROR: length of NOTES input not equal to number of sources'
        retall
        end
endcase

; Add annotation tags if necessary.
review_state_names  = ['unreviewed','approved','provisional'         ,'unreliable']
review_state_labels = ['unreviewed','approved','review again because','unreliable because']

if (total(strmatch(tag_names(cat),'REVIEW_STATE')) EQ 0) then begin
  temp  = replicate(create_struct('REVIEW_STATE','', 'UNRELIABLE_ANNOTATION','', 'PROVISIONAL_ANNOTATION','', $
                                  'uncatalogued_NIR',0B, 'uncatalogued_MIR',0B, 'nebular_contamination',0B,  $
                                  cat[0]), num_sources)
  struct_assign, cat, temp
  cat = temporary(temp)
  cat.REVIEW_STATE           = review_state_names[0]
  cat.UNRELIABLE_ANNOTATION  = ''
  cat.PROVISIONAL_ANNOTATION = ''
endif


;; Handle any explicit SRCLIST_FILENAME supplied.
if keyword_set(srclist_fn) then begin
  readcol, srclist_fn, subset_sourcename, FORMAT='A', COMMENT=';'
  
  ; Trim whitespace and ignore blank lines.
  subset_sourcename = strtrim(subset_sourcename,2)

  presentation_rank = replicate(-1L, num_sources)
  rank = 0L
  for ii=0L, n_elements(subset_sourcename)-1 do begin
    if (subset_sourcename[ii] EQ '') then continue
    
    ; Parse lines with semicolons into source names and notes.
    ind = strpos(subset_sourcename[ii],';')
    if (ind NE -1) then begin
      this_note             = strtrim(strmid(subset_sourcename[ii],ind+1) ,2)
      subset_sourcename[ii] = strtrim(strmid(subset_sourcename[ii],0,ind) ,2)
    endif else this_note = ''
    
    ; Look for this sourcename in the full list.
    ind = where(sourcename EQ subset_sourcename[ii], count)
    if (count GT 0) then begin
      ; Mark this source as being in the subset, and record any note that was parsed.
      presentation_rank[ind[0]]  = rank++
      notes            [ind[0]] += this_note
    endif else print, subset_sourcename[ii], F='(%"Source %s is missing from collated table!")'
    
    if (count GT 1) then print, subset_sourcename[ii],  F='(%"WARNING: source %s appears multiple times in table %s")'
  endfor  
  show_rank_navigation = 1
endif else begin
  srclist_fn  = catalog_fn
  presentation_rank = lindgen(num_sources)
  show_rank_navigation = 0
endelse

widget_control, DEFAULT_FONT='micro'

dum = ae_source_viewer_print_source(cat, SKIP_PLOTS=keyword_set(skip_plots), /INIT)


;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
help, tempdir

run_command, /INIT, PARAM_DIR=tempdir


; Wait for observer's ds9 session to register with XPA.
; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
my_ds9 = "DS9:"+repchr(ds9_title,' ','_')
repeat begin
  run_command, string(my_ds9, F='(%"xpaaccess ''%s''")'), result, /IGNORE_STATUS, /QUIET
  if (result[0] EQ 'yes') then break
  print, 'waiting for ds9 to come up...'
  wait,3
endrep until (0)

; Mark all the sources already reviewed.
print, 'Marking all the sources that have already been reviewed ...'
run_command, /QUIET, string(my_ds9, F='(%"xpaset -p %s frame first")')

for ii=0,num_sources-1 do begin
  if (cat[ii].REVIEW_STATE NE 'unreviewed') then begin
    run_command, /QUIET, string(my_ds9, label[ii], F='(%"xpaset -p %s regions group ''%s'' property include no")')
    print, label[ii], cat[ii].REVIEW_STATE, F='(%"Source %s is %s.")'
  endif
endfor


  
;widget_control, DEFAULT_FONT='-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1'                    
widget_control, DEFAULT_FONT='-*-fixed-bold-*-*-*-*-140-*-*-*-*-*-*'



top_base = widget_base(TITLE='ACIS Extract Source Viewer', /BASE_ALIGN_CENTER, /COLUMN, $
                        /SPACE, /XPAD, /YPAD)

; source_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER )
 button_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER)
 
   base = widget_base( button_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER, FRAME=1)
     source_number_id = cw_field( base, /INTEGER, /RETURN_EVENTS, XSIZE=4, VALUE=initial_source_number,  TITLE='Seq # in '+file_basename(catalog_fn))                
     prev_button = widget_button( base, VALUE='Previous' )
     next_button = widget_button( base, VALUE='  Next  ' )
   
   source_label_id  = cw_field( button_base, /STRING , /RETURN_EVENTS, XSIZE=15, VALUE='', TITLE='Source Label/Name:')                
   source_name_id   = cw_field( button_base, /STRING, /NOEDIT, XSIZE=18, TITLE='')

   rank_id          = 0L
   prev_rank_button = 0L
   next_rank_button = 0L
 
   if show_rank_navigation then begin
     base = widget_base( button_base, /ROW, /SPACE, /XPAD, /YPAD, /BASE_ALIGN_CENTER, FRAME=1)
       rank_id = cw_field( base, /INTEGER, /RETURN_EVENTS, XSIZE=4, VALUE=1,  TITLE='Seq # in '+file_basename(srclist_fn))                
       prev_rank_button = widget_button( base, VALUE='Previous' )
       next_rank_button = widget_button( base, VALUE='  Next  ' )
   endif
   
   mouse_select_button     = widget_button( button_base, VALUE='Mouse select' )
   done_button = widget_button( button_base, VALUE='  Quit  ' )

 review_base = widget_base( top_base, /ROW, SPACE=0, /XPAD, /YPAD, /BASE_ALIGN_BOTTOM, FRAME=2)
;   dum              = widget_label( annotation_base, VALUE='Proposed counterparts are:' )
   review_state_id  = cw_bgroup( review_base, review_state_labels, BUTTON_UVALUE=review_state_names, /EXCLUSIVE, /NO_RELEASE, /COLUMN, LABEL_TOP='Counterpart Match Status:' )
   
   
   annotation_base = widget_base( review_base, /COLUMN, SPACE=0, /XPAD, /YPAD, FRAME=2)
     provisional_annotation_id = widget_combobox(annotation_base, /EDITABLE, /DYNAMIC_RESIZE, VALUE=[' '] );, SCR_XSIZE=5, UNITS=1  )
     
     unreliable_annotation_id  = widget_combobox(annotation_base, /EDITABLE, /DYNAMIC_RESIZE, VALUE=[' ', '2 IR srcs may match 1 ACIS src', '2 ACIS srcs may match 1 IR src', 'Confusion -- multiple sources', 'Confusion -- nebular emission', 'Match has large offset'] );, SCR_XSIZE=5, UNITS=1  )

 flag_group_id  = cw_bgroup( top_base, ['Uncatalogued NIR counterpart', 'Uncatalogued MIR counterpart', 'Strong IR nebular emission'], /NONEXCLUSIVE, /COLUMN )
 
     
 button_base = widget_base( top_base, /ROW, SPACE=0, /XPAD, /YPAD, /BASE_ALIGN_BOTTOM )
   approve_multiple_button = widget_button( button_base, VALUE='Approve multiple sources' )
   match_button            = widget_button( button_base, VALUE='Match frames' )
 
 text_base = widget_base( top_base, /ROW, SPACE=0, /XPAD, /YPAD, /BASE_ALIGN_BOTTOM )
   base = widget_base( text_base, /COLUMN, SPACE=0, /XPAD, /YPAD )
   temp = widget_label( base, VALUE='Analysis notes:' )    
   analysis_notes_id = widget_text( base, /SCROLL, XSIZE=28, YSIZE=4, /EDITABLE, /WRAP, /KBRD_FOCUS_EVENTS, /IGNORE_ACCEL )
  
   base = widget_base( text_base, /COLUMN, SPACE=0, /XPAD, /YPAD )
   temp = widget_label( base, VALUE='Footnotes:' )    
   footnotes_id      = widget_text( base, /SCROLL, XSIZE=28, YSIZE=4, /EDITABLE, /WRAP, /KBRD_FOCUS_EVENTS, /IGNORE_ACCEL )
  
;misc_id   = cw_bgroup( top_base, [''], /NONEXCLUSIVE, SET_VALUE=0 )


report_base = widget_base(TITLE='Source Properties', GROUP=top_base, XOFFSET=0, YOFFSET=600, /COLUMN, /SPACE, /XPAD, /YPAD)

  report_id = widget_text( report_base, /SCROLL, XSIZE=60, YSIZE=15 )

state = { skip_plots:keyword_set(skip_plots), read_only:keyword_set(read_only), $
  
          review_state_names:review_state_names, review_state_id:review_state_id, $
          unreliable_annotation_id:unreliable_annotation_id,  provisional_annotation_id:provisional_annotation_id, $
          flag_group_id:flag_group_id, $
          analysis_notes_id:analysis_notes_id, footnotes_id:footnotes_id, $
          source_number_id:source_number_id, source_label_id:source_label_id, source_name_id:source_name_id, $
          prev_button:prev_button, next_button:next_button, $
          prev_rank_button:prev_rank_button, next_rank_button:next_rank_button, $
          mouse_select_button:mouse_select_button, $
          report_id:report_id, $
          approve_multiple_button:approve_multiple_button, match_button:match_button, done_button:done_button, $
          
          current_rank:-1L, current_source_index:-1L, sourcename:sourcename, source_label:label, RA:RA, DEC:DEC, $
          notes:notes, srclist_fn:srclist_fn, presentation_rank:presentation_rank, my_ds9:my_ds9, $
          
          catalog_fn:catalog_fn, pending_save:0, save_counter:0L, cat:cat, $
          
          tempdir:tempdir}

; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, report_base, /REALIZE
widget_control, top_base,    /REALIZE

event={ID:source_number_id, TOP:top_base, HANDLER:top_base}
ae_source_viewer_event, event

  
; The fancy combination of JUST_REG and NO_BLOCK is necessary to get the
; widget to behave when called by either widget applications or normal
; programs (which block on tty reads).
xmanager, 'ae_source_viewer', top_base, EVENT_HANDLER='ae_source_viewer_event', $
          JUST_REG=keyword_set(block), NO_BLOCK=(keyword_set(block) EQ 0)
return
end



;==========================================================================
;;; Dim source simulator.
;==========================================================================
PRO ae_dim_source_sim, obsdir, num_events


obs_stats = headfits(obsdir+'/obs.stats')

src_radius = sxpar(obs_stats, 'SRC_RAD')

bt=mrdfits(obsdir+'/source.evt', 1, theader)

dim = floor(sqrt(n_elements(bt)/num_events))
region_file = string(obsdir, dim, dim, F='(%"%s/grid%dX%d.reg")') 
event_file  = string(obsdir, dim, dim, F='(%"%s/grid%dX%d.evt")')

x0=median(bt.X)
y0=median(bt.Y)
xoffset= findgen(dim)*3*src_radius
yoffset= findgen(dim)*3*src_radius
make_2d,xoffset,yoffset
help, xoffset,yoffset
xoffset = reform(xoffset,n_elements(xoffset))
yoffset = reform(yoffset,n_elements(xoffset))
;info,xoffset
;info,yoffset

openw,  region2_unit, region_file, /GET_LUN
printf, region2_unit, "# Region file format: DS9 version 3.0"
!TEXTUNIT = region2_unit
forprint, TEXTOUT=5, x0+xoffset, y0+yoffset, replicate(src_radius, n_elements(xoffset)), F='(%"circle(%f,%f,%f)")', /NoCOMMENT
free_lun, region2_unit

num_rows = num_events*dim^2

xoffset = rebin(xoffset, num_rows, /SAMPLE)
yoffset = rebin(yoffset, num_rows, /SAMPLE)

grid = bt[0:num_rows-1]
grid.X = grid.X + xoffset
grid.Y = grid.Y + yoffset

fxaddpar, theader, 'TLMAX18', max(grid.x)+100
fxaddpar, theader, 'TLMAX19', max(grid.y)+100
mwrfits, [bt,grid], event_file, theader, /CREATE

spawn, string(event_file, region_file, F='(%"ds9 %s -region %s &")')
print, 'THETA=',sxpar(obs_stats,'THETA')
return
end



;==========================================================================
;;; Dim source simulator.
;==========================================================================
PRO ae_chart_interface, sourcename, obsname, ASPECT_FN=aspect_fn, SKIP_DOWNLOAD=skip_download, $
                        S_AIMPOINT=s_aimpoint, PIPELINE_RANDOMIZATION=pipeline_randomization

creator_string = "ae_chart_interface, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

; AE puts the 1.5 keV PSF first in the file.
psf_energy    = [1.49670, 0.2770, 4.510, 6.40, 8.60]

; These densities generate ~1E5 events (except at 8.6 keV).
chart_density = ( [1.19471,1.19085,2.36809,4.13947,15.7376] ) < 10
                     
src_stats_basename       = 'source.stats'
psf_basename             = 'source.psf'
obs_stats_basename       = 'obs.stats'
env_events_basename      = 'neighborhood.evt'
src_events_basename      = 'source.evt'


;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
help, tempdir

temp_events_fn       = tempdir + 'temp.evt'
temp_region_fn       = tempdir + 'temp.reg'
temp_image_fn        = tempdir + 'temp.img'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=tempdir

;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn get_sky_limits', 'punlearn dmcopy', 'pset dmcopy clobber=yes']

sourcedir       = sourcename + '/'  
obsdir          = sourcename + '/' + obsname + '/'
chartdir        = obsdir + 'chart'
file_mkdir, chartdir

src_stats_fn    = sourcedir + src_stats_basename
psf_fn          = obsdir +            psf_basename
chart_psf_fn    = obsdir + 'chart_' + psf_basename
stats_fn        = obsdir + obs_stats_basename
env_events_fn   = obsdir + env_events_basename
src_events_fn   = obsdir + src_events_basename

src_stats    = headfits(src_stats_fn, ERRMSG=error)
obs_stats    = headfits(stats_fn, ERRMSG=error)
event_header = headfits(src_events_fn, EXT=1)

;; =====================================================================
;; Calculate information needed to create the PSF.
ra  = sxpar(src_stats,   'RA')
dec = sxpar(src_stats,   'DEC')

;; ------------------------------------------------------------------------
energy_list = string(psf_energy,F='(%"%6.4f")')
print, 'Submit a single ChaRT run that includes multiple sources at the coordinates shown above using these energies and ray densities:  '
forprint, energy_list, chart_density  
for ii=0,n_elements(energy_list)-1 do file_copy, '/dev/null', energy_list[ii], /OVERWRITE


if NOT keyword_set(skip_download) then begin
  for ii=0,0 do begin

    tarball_filename = ''
    read, 'When the ChaRT run is completed, enter the URL to the ChaRT tarball: ', tarball_filename
    
    run_command, /UNIX, string(tempdir+'tarball', tarball_filename, F='(%"wget -O %s %s")')
    
    run_command, /UNIX, string(tempdir+'tarball', chartdir, F='(%"gtar -xzvf %s -C %s ")')
  endfor ;ii
endif  

ray_dirname = chartdir
; read, 'Enter name of directory containing ChaRT ray files: ', ray_dirname

ray_filename = file_search(ray_dirname, '*fits', COUNT=count)


;; =====================================================================
;; Process each ray file in the same order as listed in psf_energy.
temp = ray_filename
for ii=0,count-1 do begin
  ;; Figure out the energy of the rays
  cmd = string(temp[ii], F="(%'dmkeypar %s rt_kev echo+')")
  run_command, cmd, result
  
  dum = min( abs(psf_energy - float(result[0])), imin )
  ray_filename[ii] = temp[imin]
endfor ;ii

forprint, ray_filename, psf_energy

psf_header = headfits(psf_fn)

ae_make_psf, EVENT_FILE=env_events_fn, ASPECT_FN=aspect_fn, ASPECT_BLUR=sxpar(psf_header, 'ASP_BLUR'), TEMP_DIR=tempdir, S_AIMPOINT=keyword_set(s_aimpoint), PIPELINE_RANDOMIZATION=keyword_set(pipeline_randomization)

;; Convert ChaRT rays into an AE PSF file.
;; Match pixel size and image dimensions of existing PSF. 
skypixel_per_psfpixel = sxpar(psf_header, 'CDELT1P')
ae_make_psf, chart_psf_fn, skypixel_per_psfpixel, sxpar(psf_header, 'NAXIS1')*skypixel_per_psfpixel, psf_energy, 0, ra, dec, $
             EMAP_VAL=sxpar(obs_stats,'EMAP_MED'), SAOSACFile=ray_filename

  
;; =====================================================================
;; Plot a radial profile for the first AE PSF and first ChaRT PSF.
;; Both must be normalized by the appropriate PSF total.
src_radius  = sxpar(obs_stats, 'SRC_RAD') * 2
src_radius  = 20
energy_range = [1.0,2.0]
ae_radial_profile, ASPECT_FN=aspect_fn, RA=ra, DEC=dec, EVENTS_FN=env_events_fn, SRC_RADIUS=src_radius, ENERGY_RANGE=energy_range,$
                   PSF_FN=chart_psf_fn, TEMPDIR=tempdir, /PLOT, WIDGET_IDS=plot_ids, PSF_NAME='ChaRT PSF', SOURCENAME=sourcename, $
                   ks_psf, R_MEDIAN, EE_AT_RM
ae_radial_profile, ASPECT_FN=aspect_fn, RA=ra, DEC=dec, EVENTS_FN=env_events_fn, SRC_RADIUS=src_radius, ENERGY_RANGE=energy_range,$
                   PSF_FN=psf_fn, TEMPDIR=tempdir, /PLOT, WIDGET_IDS=plot_ids, PSF_NAME='Template PSF', SOURCENAME=sourcename, $
                   ks_psf, R_MEDIAN, EE_AT_RM

;for ii=0,1 do begin
;  psf_img = readfits((ii EQ 0) ? chart_psf_fn : psf_fn, psf_header, /SILENT)
;  print, 'Plotting radial profile at ', sxpar(psf_header,'ENERGY'), ' keV'
;  
;  ; Make an array that has the distances (in units of sky pixels) from each PSF pixel to the source.
;  ; For speed, work with only pixels where the PSF is not zero.
;  skypixel_per_psfpixel = sxpar(psf_header, 'CDELT1P')
;  extast, psf_header, psf2wcs_astr
;  ad2xy, ra, dec, psf2wcs_astr, xind_catalog, yind_catalog
;  
;  dist_circle, psf_distance, size(psf_img, /DIM), xind_catalog, yind_catalog
;  psf_distance  = psf_distance * skypixel_per_psfpixel
;  
;  ; Sort by distance to be ready to form a 1-D model.
;  sort_ind      = sort(psf_distance)
;  psf_distance  = psf_distance [sort_ind]
;  psf_img       = psf_img      [sort_ind]
;
;  ; Form the cumulative distribution function: psf_distn(psf_distance).
;  ; This is the 1-D model of the composite PSF.
;  psf_distn = total(psf_img, /NAN, /DOUBLE, /CUMULATIVE) / sxpar(psf_header,'PSF_TOTL')
;  
;  if (ii EQ 0) then begin
;    chart_psf_distance = psf_distance
;    chart_psf_distn    = psf_distn
;  endif
;endfor
;
;function_1d, id2,       psf_distance,       psf_distn, DATASET='PSF Library', XTIT='distance [skypix]', YTIT='enclosed fraction'
;function_1d, id2, chart_psf_distance, chart_psf_distn, DATASET='ChaRT'


CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif
return
end ; ae_chart_interface




;==========================================================================
;;; Convert source labels to source names.

;;; The parameter "labels" can either be a string array, or an integer.
;;; If an integer is supplied, then the tool prompts for you to enter the labels.
;==========================================================================
PRO ae_label2name, labels, collated_filename, catalog_names, indexes

collated_table = mrdfits(collated_filename,1)

all_labels = strlowcase(strtrim(collated_table.LABEL,2))

if (size(/TNAME, labels) EQ 'STRING') then begin
  num_labels = n_elements(labels)
endif else begin
  num_labels = labels
  labels = strarr(num_labels)
  read, labels
endelse

indexes = replicate(-1L,num_labels)
catalog_names  = replicate('XXXX', num_labels)

for ii=0,num_labels-1 do begin
  this_label = strlowcase(strtrim(labels[ii],2))
  ind = where(all_labels EQ this_label, count)
  case count of
    0: print, 'WARNING: label '+labels[ii]+' not found'
    1: begin
       indexes      [ii] =                               ind
       catalog_names[ii] = (collated_table.CATALOG_NAME)[ind]
       end
    else: begin
          print, this_label, F="(%'ERROR: duplicate label %s found in catalog:')"
          forprint, collated_table.CATALOG_NAME, all_labels, SUBSET=ind, F='(%"  %s == %s")'
          end
  endcase
endfor

forprint, SUBSET=where(indexes NE -1), catalog_names, labels, F="(%'%s (%s)')"
return
end



;==========================================================================
;; Routine to create a set of fake sources that "tile" a field of view, to
;; be used with ae_recon_detect.

;; NEIGHBORHOOD_SIZE is the same keyword option used in AE to specify the
;; width and height of the source neighborhood, in arcseconds.
;;
;; The result is a region file tiles.reg showing the tiles.  This can be 
;; hand edited in ds9 to reposition the tiles as desired.
;==========================================================================

PRO ae_neighborhood_tile, emap_filename, NEIGHBORHOOD_SIZE=neighborhood_size, STYLE=style

if ~keyword_set(neighborhood_size) then neighborhood_size = 50 ; arcsec

if ~keyword_set(style) then style = 1

emap = readfits(emap_filename, emap_header)
extast, emap_header, emap2wcs_astr
emap_xdim = (size(emap, /DIM))[0]
emap_ydim = (size(emap, /DIM))[1]
arcsec_per_imgpixel = emap2wcs_astr.CDELT[1] * 3600

tile_radius_arcsec = neighborhood_size/2.0 
if (style EQ 1) then begin
  x_oddrow_offset_arcsec = neighborhood_size/2.0
  x_pitch_arcsec         = neighborhood_size
  y_pitch_arcsec         = neighborhood_size/2.0
endif else begin
  x_oddrow_offset_arcsec = 0
  x_pitch_arcsec         = neighborhood_size - 10
  y_pitch_arcsec         = neighborhood_size - 10
endelse

x_pitch         = round(x_pitch_arcsec         / arcsec_per_imgpixel)
y_pitch         = round(y_pitch_arcsec         / arcsec_per_imgpixel)
x_oddrow_offset = round(x_oddrow_offset_arcsec / arcsec_per_imgpixel)
tile_radius     = round(tile_radius_arcsec     / arcsec_per_imgpixel)

openw,  region_unit, 'tiles.reg', /GET_LUN
printf, region_unit, "# Region file format: DS9 version 3.0"
printf, region_unit, "J2000"

num_tiles = 0L
is_odd_row = 0
x_grid = tile_radius
y_grid = tile_radius
while 1 do begin
  x_center = x_grid
  y_center = y_grid
  color = 'green'

  skip = 0
  for kk=0,1 do begin
    ; Reposition tile that's falling off the edge of the emap.
    while ((x_center-tile_radius) LT            0 ) do x_center++
    while ((y_center-tile_radius) LT            0 ) do y_center++
    while ((x_center+tile_radius) GT (emap_xdim-1)) do x_center--
    while ((y_center+tile_radius) GT (emap_ydim-1)) do y_center--
    
    ; Extract portion of emap falling inside the tile.
    xlow     = (x_center-tile_radius) > 0
    ylow     = (y_center-tile_radius) > 0
    xhigh    = (x_center+tile_radius) < (emap_xdim-1)
    yhigh    = (y_center+tile_radius) < (emap_ydim-1)
    
    tile_emap = emap[xlow:xhigh, ylow:yhigh]
    
    frac_off_field = total(/INT, tile_emap EQ 0) / float(n_elements(tile_emap))
    
    ; Skip tile that's mostly off-field.
    if (frac_off_field GT 0.90) then begin
      skip = 1
      break
    endif
    
    ; Reposition tile that's partially off-field
    if (frac_off_field GT 0.10) then begin
      x_offset = indgen(1+xhigh-xlow)
      y_offset = indgen(1+yhigh-ylow)
      make_2d, x_offset, y_offset
    
      x_center = xlow + round(total(tile_emap*x_offset) / total(tile_emap))
      y_center = ylow + round(total(tile_emap*y_offset) / total(tile_emap))
      color = 'magenta'
    endif else begin
      break
    endelse
  endfor ; kk
  
  if ~skip then begin  
    ; Write tile region to file.
    xy2ad, x_center, y_center, emap2wcs_astr, ra, dec
    num_tiles++
    printf, region_unit, ra, dec, [2,2]*tile_radius_arcsec, color, F="(%'box(%10.6f,%10.6f,%5.1f"",%5.1f"") # tag={fov} color={%s}')"
  endif

  ; Advance to the next tile center.
  x_grid += x_pitch
  if (x_grid GT (emap_xdim-1)) then begin
    ; Start a new row.
    x_grid = tile_radius
    is_odd_row = ~is_odd_row
    if is_odd_row then x_grid += x_oddrow_offset
    
    y_grid += y_pitch
    if (y_grid GT (emap_ydim-1)) then break
  endif
endwhile



free_lun, region_unit

print, num_tiles, ' tiles written to tiles.reg.'

spawn, string(emap_filename, F="(%'ds9 -log %s -region tiles.reg &')")
return
end


PRO ae_rm_duplicate_tiles, region_file
      readcol, region_file, lines, F='(A)', DELIM='@'
      
      num_lines = n_elements(lines)
      good = replicate(1B,num_lines)

      result = stregex(lines,'box[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
      x = double(reform(result[1,*]))
      y = double(reform(result[2,*]))
      
      ; Look for non-zero duplicates.
      for ii=0,num_lines-1 do begin
        if (x[ii] EQ 0) then continue
        
        sqr_dist = (x-x[ii])^2 + (y-y[ii])^2
        ind = where(sqr_dist LT 1^2, count)
        if (count EQ 1) then continue
        
        ind=ind[1:*]
        x[ind]    = 0
        y[ind]    = 0
        good[ind] = 0
      endfor

      forprint, TEXTOUT='/tmp/temp.reg', SUBSET=where(good), lines, /NOCOMMENT
      print, 'wrote cleaned regions to /tmp/temp.reg'
      spawn, 'wc /tmp/temp.reg '+region_file
      
      
      lines = lines[where(good, count)] + string(1+indgen(count), F='(%" text={%d}")')
      forprint, TEXTOUT='/tmp/temp.reg', lines, /NOCOMMENT
return
end



;==========================================================================
;;; Find source candidates in AE reconstructions.

;;; Example:
;;;    .r acis_extract_tools
;;;   ae_recon_detect, 'all.srclist', 3, '../../iarray.source_search.evt',  '../../mosaic.emap' 
;;;
;;; The hdunumber parameter is a zero-based HDU number (passed to readfits.pro) that
;;; selects which image in neighborhood.img is searched.
;==========================================================================
PRO ae_recon_detect, catalog_or_srclist, hdunumber, match_events_fn, emap_filename, FLOOR_CNTS=floor_cnts

creator_string = "ae_recon_detect, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

; Choose a photometry aperture of 5x5 image pixels (whose size varies with theta).
cell_size     = 5
cell_halfsize = 1 + floor(cell_size/2)
help, cell_size


if (n_elements(floor_cnts) EQ 0) then floor_cnts=2.5
help, floor_cnts

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
help, tempdir

temp_events_fn       = tempdir + 'temp.evt'
temp_region_fn       = tempdir + 'temp.reg'
temp_image_fn        = tempdir + 'temp.img'

;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


;; =====================================================================
;; Initialize stuff.
run_command, /INIT, PARAM_DIR=tempdir

;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes']


env_image_basename       = 'neighborhood.img'
psf_basename             = 'source.psf'
 
arcsec_per_skypixel = 0.492 ; (0.492 arcsec/skypix)

resolve_routine, 'match_xy', /COMPILE_FULL_FILE

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from event data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

if keyword_set(emap_filename) then begin
  emap    = readfits(emap_filename, emap_hdr)
  extast, emap_hdr, emap_astr

  bkg_map = emap
  bkg_map[*]=0
endif

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

bkg_flux_a  = fltarr(num_sources)
src_area_a  = fltarr(num_sources)
cell_area_a = fltarr(num_sources)
theta_a     = fltarr(num_sources)

openw,  region1_unit, 'recon_fov.reg', /GET_LUN
printf, region1_unit, "# Region file format: DS9 version 3.0"

for ii = 0, num_sources-1 do begin
  sourcedir = sourcename[ii] + '/' 
  composite_img_fn = sourcedir + env_image_basename
  composite_psf_fn = sourcedir + psf_basename

  stats       = headfits(/SILENT, sourcedir+'source.stats')
  TILE_NUMBER = strtrim(sxpar(stats,'LABEL'),2)
  TILE_NAME   = 'recon_'+TILE_NUMBER
  print, sourcename[ii], TILE_NAME, F='(%"\n-------\nSource: %s (%s)")'

  if NOT file_test(composite_img_fn) then begin
    print, env_image_basename, F='(%"\n-------\nWARNING!  Could not find %s")'
    printf, region1_unit, sxpar(stats,'RA'), sxpar(stats,'DEC'), TILE_NUMBER, F="(%'J2000;boxcircle point %10.6f %10.6f # tag={missing tile} text={%s} color={red}')"
    continue
  endif
  
  composite_img     = readfits(/SILENT, composite_img_fn, hood_hdr)
  maxlik_img        = readfits(/SILENT, composite_img_fn, maxlik_hdr, exten_no=hdunumber)
  composite_psf_hdr = headfits(/SILENT, composite_psf_fn)
  
  print, sxpar(maxlik_hdr, 'ML_ITERS'), ' iterations used in reconstruction'
  
  extast, maxlik_hdr, maxlik_astr
  arcsec_per_maxlikpixel = maxlik_astr.CDELT[1] * 3600
  xdim = (size(maxlik_img, /DIM))[0]
  ydim = (size(maxlik_img, /DIM))[1]
  
  ; We'll use the kernel_footprint utility to look up 1-D indexes into the images of each photometry kernel
  ; we need to evaluate, taking array edges into account.
  if keyword_set(kf_id) && ptr_valid(kf_id) then kernel_footprint, kf_id, /DESTROY

  kernel = replicate(1,cell_size,cell_size)
  kernel_footprint, kf_id, /CREATE, IMAGE=maxlik_img, KERNELS=kernel

  ;; Create a graphic depicting the field of view of this reconstruction.
  printf, region1_unit, sxpar(stats,'RA'), sxpar(stats,'DEC'), [xdim,ydim]*arcsec_per_maxlikpixel, TILE_NUMBER, F="(%'J2000;box %10.6f %10.6f %4.1f"" %4.1f"" # tag={fov} text={%s}')"


  ;; ------------------------------------------------------------------------
  ;; Estimate a uniform background FLUX (counts/skypix^2/s/cm^2) in the neighborhood.
  
  ;; We coarsely rebin the image to push most of the PSF into ~1 pixel, but we
  ;; ensure that the rebinned image retains a reasonable number (100) of pixels.
  radius50 = sxpar(composite_psf_hdr, 'RADIUS50')
  if NOT finite(radius50) then begin
    print, 'Keyword RADIUS50 not found; using on-axis value of 0.43.'
    radius50 = 0.43
  endif

  num_across          = radius50 / (arcsec_per_maxlikpixel)
  
  for rebin_factor= (1 > ceil(num_across * 4.)), 2, -1 do begin
    proposed_xdim = floor((size(composite_img, /DIM))[0] / rebin_factor)
    proposed_ydim = floor((size(composite_img, /DIM))[1] / rebin_factor) 
    if (proposed_xdim*proposed_ydim GE 100) then break
  endfor
  
  print, rebin_factor*arcsec_per_maxlikpixel, F='(%"\nEstimating background in composite data image rebinned to %4.1f arcsec/pixel.")'
  
   ; We choose to crop the data image to avoid incomplete pixels in the rebinned image.
  cmd = string(composite_img_fn, proposed_xdim*rebin_factor,rebin_factor, proposed_ydim*rebin_factor,rebin_factor, temp_image_fn, F="(%'dmcopy ""%s[bin #1=1:%d:%d,#2=1:%d:%d]"" %s')")
  run_command, /QUIET, cmd      
  bigpixel_image = readfits(temp_image_fn, bigpixel_hdr, /SILENT)
  
  ; Resample emap onto bigpixel image.
  hastrom, emap, emap_hdr, bigpixel_emap, dum_hdr, bigpixel_hdr, MISSING=0 

  ; Sometimes we get an edge where bigpixel_image has power but bigpixel_emap is zero.
  bigpixel_image *= (bigpixel_emap GT 0)
  
  ; Estimate a bkg in units of cts/bigpix^2/s/cm^2
  estimate_poisson_background, bigpixel_image, EMAP=bigpixel_emap, bkg_per_bigpix, SIGNIFICANCE=0.99, /VERBOSE

  ; Convert to units of cts/skypix^2/s/cm^2.
  extast, bigpixel_hdr, bigpixel_astr 
  arcsec_per_bigpixel = bigpixel_astr.CDELT[1] * 3600
  bkg_flux = bkg_per_bigpix * (arcsec_per_skypixel / arcsec_per_bigpixel)^2

  if keyword_set(emap_filename) then begin
    ; Resample the background tile onto the bkg_map scene.
    bkg_tile = replicate(1, xdim, ydim)
    hastrom, bkg_tile, hood_hdr, emap_hdr, MISSING=0
    ind = where(bkg_tile GT 0, count)
    if (count GT 0) then bkg_map[ind] = bkg_flux
  endif
 
 
  ;; ------------------------------------------------------------------------
  ;; Estimate the size of AE's nominal extraction polygon in this tile.
  THETA     = sxpar(stats,'THETA')
  src_area  = sxpar(stats, 'SRC_AREA')
  if (src_area EQ 0) then src_area  = !PI * sxpar(stats, 'SRC_RAD')^2 
  if (src_area EQ 0) then begin
    print, 'ERROR:  SRC_AREA and SRC_RAD are zero.'
    retall
  endif
  
  cell_area = (cell_size*arcsec_per_maxlikpixel/arcsec_per_skypixel)^2
  theta_a    [ii] = THETA
  src_area_a [ii] = src_area
  cell_area_a[ii] = cell_area
  bkg_flux_a [ii] = bkg_flux
  
  print, bkg_flux, F='(%"background flux = %0.2g (events/skypixel^2/s/cm^2) ")'

;  print, 'SRC_AREA   = ', src_area,       '        (skypixel^2)'   
;  print, 'CELL AREA  = ', cell_area,      '        (skypixel^2)' 

  save, bkg_flux, src_area, cell_area,FILE=sourcedir+'recon_detect.sav'
  
  ;; ------------------------------------------------------------------------
  ; Resample emap onto maxlik image.
  hastrom, emap, emap_hdr, maxlik_emap, dum_hdr, maxlik_hdr, MISSING=0 

  


  ;; ------------------------------------------------------------------------
  ;; Search for sources.  Because we employ a mechanism to impose a minimum
  ;; separation between sources, we consider candidate source positions ordered
  ;; by decreasing FLUX.
  ;; For the sort that deterines the order of processing, it's important to 
  ;; compute SINGLE-pixel fluxes (maxlik_img/maxlik_emap) rather
  ;; than 5x5 cell fluxes because odd things can occur,
  ;; e.g. a very bright pixel in the recon might not participate in a centroid
  ;; because it fell in the 5x5 ring of a high-flux CELL that was mistakenly 
  ;; considered too early.
  Nentries = 0
  ;; Build a catalog structure suitable for match_xy.
  cat_entry = {source_recon, ID:0L, X:0.0, Y:0.0, X_ERR:0.0, Y_ERR:0.0, RA:0D, DEC:0D, TILE_NAME:'', TILE_NUMBER:0L, X_CELL:0, Y_CELL:0, CELL_COUNTS:0.0, SRC_CNTS:0L, BKG_EXPECTED_IN_APERTURE:0.0, BKG_EXPECTED_OUTSIDE_CELL:0.0, Pb:0.0, THETA:0.0, CATALOG_NAME:'reconstruction' }
  
  cat = replicate(cat_entry, 2000)
  
  flux_map = maxlik_img/maxlik_emap
  good_ind = where(finite(flux_map))
  sort_ind = good_ind[reverse(sort(flux_map[good_ind]))]
  
  accepted_island_map = bytarr(xdim,ydim)
  
  ;; ------------------------------------------------------------------------ 
  ;; Even though our photometry cell may be 5x5, our 
  ;; definition of a local maximum always involves a 3x3 island in the recon 
  ;; image, NOT in any sort of smoothed photometry image.
  ll_neighbor_ind = [0,1,2,3]
  ur_neighbor_ind = [5,6,7,8]
  central_ind     = 4
  
  for jj = 0L, n_elements(sort_ind)-1 do begin
    ;Find the X,Y position of the cell, in the 0-based image coordinate system.
    temp = array_indices(maxlik_img, sort_ind[jj])
    xindex = temp[0]
    yindex = temp[1]
    
    ; We will NOT test for a local maximum if the detection island falls off the edge of the image.
    if ((xindex LT 1) OR (xindex GT xdim-1-1) OR $
        (yindex LT 1) OR (yindex GT ydim-1-1)) then continue
  
    ; Extract 3x3 detection island and check for a local maximum
    localmax_island = maxlik_img[xindex-1:xindex+1, yindex-1:yindex+1]

    local_max_found = (localmax_island[central_ind] GE max(localmax_island[ll_neighbor_ind])) AND $
                      (localmax_island[central_ind] GT max(localmax_island[ur_neighbor_ind]))

    if (NOT local_max_found) then continue
  
    ; Do not accept a 3x3 island that overlaps a previously accepted island.
    if (total(/INT, accepted_island_map[xindex-1:xindex+1, yindex-1:yindex+1]) GT 0) then begin
      print, 'Rejected a local max too close to an accepted local max.'
      continue
    endif

    ;; ------------------------------------------------------------------------
    ; Look up the exposure map value at this position.
    this_exposure = maxlik_emap[xindex, yindex]          
    
    ;; Estimate the background (counts) in AE's extraction aperture.
    ;; The aperture size (skypix^2) is estimated by src_area
    ;; The background (events/skypixel^2) is estimated by bkg_flux*this_exposure.
    bkg_per_skypix = bkg_flux * this_exposure
   
    bkg_expected_in_aperture  = 0 > (bkg_per_skypix * src_area)
    bkg_expected_outside_cell = 0 > (bkg_per_skypix * (src_area - cell_area))

    ;; ------------------------------------------------------------------------  
    ;; Perform photometry on a cell in the maxlik image at this location.
    
    ; We use the kernel_footprint utility to return the 1-D indexes of the cell, 
    ; with the central pixel listed first, dealing with array edges.
    kernel_footprint, kf_id, xindex, yindex, 0, pixel_ind
    maxlik_pixels = maxlik_img[pixel_ind]
    if (maxlik_pixels[0] NE maxlik_img[xindex, yindex]) then message, 'BUG in kernel_footprint utility!'
    
    ; For photometry we add up the central pixel (first in list), plus others in cell that:
    ; (a) Are less than central pixel.
    ; (b) Are not in accepted_island_map.
    good_flag     = (maxlik_pixels LT maxlik_pixels[0]) AND (accepted_island_map[pixel_ind] EQ 0)
    good_flag[0]  = 1

    recon_photometry = total(maxlik_pixels * good_flag)
    
    ; We impose an arbitrary lower limit on our photometry estimate to avoid very weak sources that would
    ; probably be rejected in our recipe later anyway.
    if (recon_photometry LT floor_cnts) then continue
    
    
    ;; ------------------------------------------------------------------------  
    ;; Estimate the SRC_CNTS (total counts in aperture) value that AE would find for a source at this location.
    
    ;; Presumably, reconstructions will push SOME of the background falling in the aperture into the source peak
    ;; that we're processing, and push SOME of the background in to random little peaks outside our cell.
    ;; Thus we expect that the recon photometry will fall between SRC_CNTS and NET_CNTS, but we don't know
    ;; in detail its relationship to SRC_CNTS
           
    ; This a simple law that assumes the recon photometry estimates SRC_CNTS.
    SRC_CNTS = round(recon_photometry)

    ; To get good agreement with detection by eye and with wavdetect, we find it necessary to put
    ; in an explicit ad hoc adjustment that boosts SRC_CNTS off-axis.
    ; On-axis we add nothing; at 10' off-axis we add 50% of bkg_expected_outside_cell.
    ;SRC_CNTS = round(counts_in_cell[xindex,yindex] + (bkg_expected_outside_cell * THETA*(0.5/10)))  

    ; This is a flat law, boosting by bkg_expected_outside_cell everywhere.
    ;SRC_CNTS = round(counts_in_cell[xindex,yindex] + bkg_expected_outside_cell) 

    
    ;; ------------------------------------------------------------------------  
    ;; Estimate AE's Pb statistic and compare to a threshold.
    ; We use a Pb threshold more liberal (larger than) that to be used for pruning the catalog.
    Pb_threshold   = 0.02	
    PROB_NO_SOURCE_poisson  = (1 - poisson_distribution(bkg_expected_in_aperture, SRC_CNTS - 1)) > 0

    if (PROB_NO_SOURCE_poisson GT Pb_threshold) then continue
      
    ; Estimate position as the centroid of some set of recon pixels,
    ; NOT as any computation done on the photometry (smoothed) image.
    ; We choose to centroid on the 3x3 island, even though our photometry cell is 5x5, to avoid
    ; severe perturbation of the centroid when the outer ring has a very bright pixel associated
    ; with another source.
    ; This is admittedly confusing: we let the outer ring help the source be significant but do
    ; not let it vote on the centroid.
    offset = [-1,0,1]
    
    make_2d, offset, offset, x_offset, y_offset
    centroid_island       = localmax_island
    
    cat_entry.X           = xindex + total(centroid_island*x_offset) / total(centroid_island)
    cat_entry.Y           = yindex + total(centroid_island*y_offset) / total(centroid_island)
    cat_entry.X_CELL      = xindex
    cat_entry.Y_CELL      = yindex
    cat_entry.CELL_COUNTS = recon_photometry
    cat_entry.SRC_CNTS    = SRC_CNTS
    cat_entry.bkg_expected_in_aperture  = bkg_expected_in_aperture
    cat_entry.bkg_expected_outside_cell = bkg_expected_outside_cell
    cat_entry.Pb          = PROB_NO_SOURCE_poisson
    cat_entry.THETA       = THETA
    
    cat[Nentries] = cat_entry
    Nentries = Nentries + 1
    accepted_island_map[xindex-1:xindex+1, yindex-1:yindex+1] = 1
  endfor ;jj
      
  if (Nentries EQ 0) then begin
    print, 'No sources detected.'
    continue
  endif
      
  cat     = cat[0:Nentries-1]
 
  ;Assign arbitrary position errors of 0.5", with a very weak dependence on distance from the 
  ;field center so that the merging process will favor sources closer to the PSF position.
  distance = sqrt( (cat.X-xdim/2.)^2 + (cat.Y-ydim/2.)^2 ) * arcsec_per_maxlikpixel ;arcseconds
  cat.X_ERR = (0.5 + distance/1E4) / arcsec_per_skypixel 
  cat.Y_ERR = cat.X_ERR
  
 ; Convert from image to celestial coordinates.
  xy2ad, cat.X, cat.Y, maxlik_astr, ra, dec
  cat.RA = ra
  cat.DEC=dec
  print
  print, '(celestial)              (centroid)   (cell,cell) counts_in_cell'
  forprint, ra, dec, cat.X, cat.Y, cat.x_cell, cat.y_cell, cat.cell_counts, F='(%"%10.6f %10.6f  %6.1f %6.1f  %3d %3d %7.1f")'

   ; Convert celestial to the ACIS tangent plane.
  ad2xy, ra, dec, event2wcs_astr, x, y
  ; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
  cat.X  = X+1
  cat.Y  = Y+1

  ; Create a region file in celestial coordinates.
  openw,  region2_unit, sourcedir+'recon.reg', /GET_LUN
  printf, region2_unit, "# Region file format: DS9 version 3.0"
  !TEXTUNIT = region2_unit
  forprint, TEXTOUT=5, /NoCOM, RA, DEC, F='(%"J2000;cross   point %10.6f %10.6f # tag={recon}")'
  free_lun, region2_unit

  
  cat.ID = 1+indgen(Nentries)
  cat.TILE_NAME   = TILE_NAME
  cat.TILE_NUMBER = TILE_NUMBER
  
; help, cat, /st
  print, Nentries, ' sources detected in '+TILE_NAME
  save, cat, bkg_flux, src_area, cell_area,FILE=sourcedir+'recon_detect.sav'
  
  ;; Merge this catalog with the previous ones.
  st = 0.99 ;significance threshold   
  if (n_elements(union_cat) EQ 0) then begin
    union_cat = cat 
  endif 
  
  match_xy, /QUIET, match_struct, union_cat, 'union_cat', /INIT
  match_xy, /QUIET, match_struct,       cat, TILE_NAME, st, UNION_CAT=union_cat
endfor ;ii
free_lun, region1_unit

; In the tile merging above, we faked the position errors in such a way that matches between tiles would retain the detection closest to a tile center.
; Here, we're going to try to estimate realistic position errors (based on off-axis angle) so that later merges of pointing catalogs will retain the observation of the source that has the lowest position error.
; We will estimate the position as follows:
;  - Esimate R50 (radius of circle enclosing 50% PSF power) from THETA.
;  - Assume that the PSF is Gaussian, and that R50 ~= sigma.
;  - Estimate how many counts the source produced via CELL_COUNTS.
;  - Compute the position error as the standard sigma/sqrt(N).

; In Dec 2007 I used MARX simulations at 1.5 keV with the readout streak disabled to measure PSF fractions at 1.5 keV as a function of off-axis angle.  
; These polynomial curves were fit to those measurements. The off-axis angle off_angle is in arcminutes.
off_angle = union_cat.THETA
radius50 = (0.85 -0.25*off_angle + 0.10*off_angle^2)     ; skypix
union_cat.X_ERR = radius50 / sqrt(union_cat.CELL_COUNTS) ;skypix
union_cat.Y_ERR = union_cat.X_ERR 

save, union_cat, theta_a, src_area_a, cell_area_a, bkg_flux_a, FILE='union.sav'

; Create region file.
catalog_ds9_interface, union_cat, 'union.reg', /WRITE_REGFILE

print
print, n_elements(union_cat), ' sources detected.'


if keyword_set(emap_filename) then begin
  writefits, 'bkg_map.img', bkg_map, emap_hdr
  spawn, 'ds9 bkg_map.img -region recon_fov.reg -zoom to fit &'
endif

;spawn, string(match_events_fn, union_regfile, F='(%"ds9 -log %s -region %s -region recon_fov.reg &")')
return
end


;==========================================================================

;;; Perform extractions within specific time ranges.
;;; time_filter should be a string array of CIAO time specifications, e.g. "tstart:tstop".
;;; extraction_name should be a string array of names for the extractions.
;;;
;==========================================================================
PRO ae_timerange_extract, sourcename, obsname, time_filter, extraction_name

resolve_routine, 'ae_recipe', /COMPILE_FULL_FILE

repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir

srclist_fn = tempdir + 'temp.cat'

src_stats_basename       = 'source.stats'
obs_stats_basename       = 'obs.stats'
num_sources = n_elements(time_filter)

;; Look up the existing extraction information.
    sourcedir = sourcename + '/' 
new_sourcedir = sourcename + '/' + extraction_name + '/'
    obsdir    = sourcename + '/' + obsname + '/' 
new_obsdir    = sourcename + '/' + obsname + '/' + extraction_name + '/'

src_stats_fn = sourcedir + src_stats_basename
obs_stats_fn = obsdir    + obs_stats_basename

obs_stats = headfits(obs_stats_fn)   

;; Make a srclist with multiple instances of the sourcename.
forprint, TEXTOUT=srclist_fn, replicate(sourcename,num_sources), /NoCOMMENT

;; Make sub-extraction directories; copy stats files; link extraction regions .
file_mkdir, new_sourcedir, new_obsdir

file_copy, /OVERWRITE, replicate(sourcedir + src_stats_basename,num_sources), new_sourcedir
file_copy, /OVERWRITE, replicate(obs_stats_fn,                  num_sources), new_obsdir

file_delete, /ALLOW_NONEXISTENT,                    new_obsdir + 'extract.reg'
file_link, replicate('../extract.reg',num_sources), new_obsdir + 'extract.reg'

  print, F='(%"\nae_timerange_extract: ============================================================")'  
  print,        'ae_timerange_extract: Extracting '+sourcename+' segments: ', extraction_name
  print,   F='(%"ae_timerange_extract: ============================================================\n")'  

ae_standard_extraction, obsname, SRCLIST=srclist_fn, EXTRACTION_NAME=extraction_name, TIME_FILTER=time_filter

; 
; In principle, we could run these for every obsid; the EXPOSURE would be zero for some obsids. 
; We'd have to check carefully whether that would be propagated correctely to the ARF/RMF weighting, and other things.
; For now we require the user to avoid null extractions.
; 
; Then we run a series of MERGE and FIT calls, one for each EXTRACTION_NAME value we have.
; Those results can be collated as is done in the pileup recipe.

acis_extract, srclist_fn, EXTRACTION_NAME=extraction_name, MERGE_NAME=extraction_name, /MERGE

acis_extract, srclist_fn, MERGE_NAME=extraction_name, /FIT_SPECTRA, CHANNEL_RANGE=[35,548], /CSTAT, MODEL_FILENAME='xspec_scripts/thermal/tbabs_vapec.xcm'


return
end


;==========================================================================
;;; Assign a supplied ds9 region expressed in celestial coordinates as the 
;;; extraction region for a source.
;;;
;;; If the optional parameter "obsname" is omitted, the supplied region becomes
;;; the extraction region for all observations of the source.
;;;
;;; ds9 is used to convert celestial coordinates in the supplied region to the
;;; sky coordinates required for extraction regions.
;==========================================================================

PRO ae_set_region, sourcename, celestial_region_fn, obsname, EXTRACTION_NAME=extraction_name

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
;; Some tools that use these temp filenames (e.g addarf and addrmf) are executed from a directory
;; other than cwd, and thus require that the path to tempdir is absolute, not relative.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
run_command, /INIT, PARAM_DIR=tempdir

obs_stats_basename       = 'obs.stats'
src_region_basename      = 'extract.reg'
env_events_basename      = 'neighborhood.evt'

;; We can NOT search for "extract.reg" because /MERGE could make a file
;; sourcename/extraction_name/extract.reg which would be misinterpreted here as
;; an observation!  We must instead search for "obs.stats" which appears only in observation
;; directories.
pattern = sourcename + '/*/' + extraction_subdir + obs_stats_basename
obs_stats_fn = file_search( pattern, COUNT=num_obs )

if keyword_set(obsname) then begin
  num_obs_found = num_obs
  pattern = sourcename + '/' + obsname + '/' + extraction_subdir + obs_stats_basename
  obs_stats_fn = file_search( pattern, COUNT=num_obs )
  if (num_obs_found GT num_obs) then print, strjoin(obsname,' '), num_obs_found-num_obs, F='(%"WARNING!  Obsname %s was specified; ignoring %d other observations.\n")'
endif 

if (num_obs EQ 0) then begin
  print, 'No extractions found.'
  return
endif

obs_dir = strarr(num_obs)
for jj = 0, num_obs-1 do begin
  fdecomp, obs_stats_fn[jj], disk, dir
  obs_dir[jj] = dir
endfor

env_events_fn = obs_dir + env_events_basename
region_fn     = obs_dir + src_region_basename

print, 'Spawning ds9 to perform coordinate conversions ...'
ae_send_to_ds9, my_ds9, NAME='ae_set_region_'+session_name
for ii=0,num_obs-1 do begin
  ;; Load observation data into ds9.
  ae_send_to_ds9, my_ds9, env_events_fn[ii], celestial_region_fn

  ;; Load region file into ds9 and resave in PHYSICAL coordinates.
  cmd1 = string(my_ds9,                          F='(%"xpaset -p %s regions system physical")')
  cmd2 = string(my_ds9, region_fn[ii],           F='(%"xpaset -p %s regions save %s")')
  run_command, [cmd1,cmd2], /QUIET
endfor

return
end


;==========================================================================
;;; Make some useful plots from the collated AE data products.
;==========================================================================

PRO ae_summarize_catalog, collatefile, bt

COMMON ae_summarize_catalog, id0, id1, id2, id3, id4, id5, id6, id7, id8, id9, id10, id11, id12

  if ~keyword_set(collatefile) then collatefile = 'all.collated'

  arcsec_per_skypixel  = 0.492 

  bt=mrdfits(collatefile, 1) 
  num_sources = n_elements(bt)
  band_full           = 0  
  print, 'Using the energy band ', bt[0].ENERG_LO[band_full], bt[0].ENERG_HI[band_full]

  BACKGRND            = bt.BACKGRND/1E-9
  SRC_CNTS            = bt.SRC_CNTS            [band_full]
  NET_CNTS            = bt.NET_CNTS            [band_full]
  flux2               = bt.FLUX2               [band_full]
  ENERG_PCT50_OBSERVED= bt.ENERG_PCT50_OBSERVED[band_full]
  SRC_SIGNIF          = bt.SRC_SIGNIF          [band_full]
  PROB_KS             = bt.PROB_KS
  MERGE_KS            = bt.MERGE_KS
  PSF_FRAC         = bt.PSF_FRAC
  off_angle        = bt.THETA
  radius50         = (0.85 -0.25*off_angle + 0.10*off_angle^2)  * arcsec_per_skypixel  ; arcseconds
  distance_src2src = bt.DISTANCE_SRC2SRC                        * arcsec_per_skypixel  ; arcseconds
  crowding         = distance_src2src / radius50
  
  print, total(/INT, finite(PROB_KS)),  F='(%"PROB_KS  is available for %d sources.")'
  print, total(/INT, finite(MERGE_KS)), F='(%"MERGE_KS is available for %d sources.")'
  
  dataset_1d, id0, SRC_CNTS        , XTIT='# of extracted counts, 0.5-8 keV'
  dataset_1d, id1, NET_CNTS        , XTIT='# of net counts, 0.5-8 keV'
  dataset_1d, id2, alog10(flux2)   , XTIT='log flux2 (photon/cm^2/s)'
  dataset_1d, id3, PSF_FRAC        , XTIT='PSF Fraction @1.5keV'
  dataset_1d, id4, distance_src2src, XTIT='distance (arcsec) to nearest neighbor' 
  dataset_1d, id5, crowding        , XTIT='crowding metric (distance/R50) to nearest neighbor' 
  dataset_2d, id6, off_angle, distance_src2src, PSYM=1, XTIT='off-axis angle (arcmin)', YTIT='distance (arcsec) to nearest neighbor'
  dataset_2d, id7, PSF_FRAC, BACKGRND, PSYM=1, XTIT='PSF Fraction @1.5keV', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
  dataset_1d, id8, BACKGRND,  XTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
  dataset_1d, id9, PROB_KS,   XTIT='smallest single-ObsId KS probability'
  dataset_1d,id10, MERGE_KS,  XTIT='multi-ObsId KS probability'
  dataset_1d,id11, ENERG_PCT50_OBSERVED,   XTIT='median energy (ENERG_PCT50_OBSERVED), 0.5-8.0 keV'
  dataset_1d,id12, SRC_SIGNIF,   XTIT='signal-to-noise ratio'
  
return
end


;==========================================================================
;;; Perform a very simple scan of source lightcurves for cosmic ray afterglows.

;;;
;;; As of May 2008 the CXC's tool acis_run_hotpix misses short-duration afterglow incidents.
;;; The sort of scan on extracted data we're doing here is NOT as good as one done on raw data, but it's useful.

;;; Inputs are:
;;;   catalog filename, MERGE_NAME as in many other tools.
;;;   BAND_NUM: 0-based index of the energy band (in source.photometry) on which statistics should be computed.
;;;   MAX_FRAME_SEPARATION, MAX_OFFSET: parameters defining separation of events in time and space that suggests an afterglow origin.

;;; Outputs are:
;;;   sourcename
;;;   Pb_revised (the PROB_NO_SOURCE in specified band recalculated if the suspected AG events were ignored)
;;;   fraction_inband_suspect_events (fraction of counts in specified band suspected to be AG events)

;;; See recipe.txt for example usage.

;==========================================================================
PRO ae_afterglow_report, catalog_or_srclist, MERGE_NAME=merge_name, BAND_NUM=band_num, $ ; input parameters
      MAX_FRAME_SEPARATION=max_frame_separation, MAX_OFFSET=max_offset, $                ; optional inputs
      sourcename, Pb_revised, num_inband_suspect_events, fraction_inband_suspect_events  ; output parameters

if (n_elements(max_frame_separation) EQ 0) then max_frame_separation=3
if (n_elements(max_offset)           EQ 0) then max_offset          =1
if (n_elements(band_num)             EQ 0) then band_num            =0

src_events_basename      = 'source.evt'
src_stats_basename       = 'source.stats'
src_photometry_basename  = 'source.photometry'

readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', catalog_or_srclist
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                else merge_subdir = ''
if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)
                              
Pb_revised                     = fltarr(num_sources)
fraction_inband_suspect_events = fltarr(num_sources)
num_inband_suspect_events = lonarr(num_sources)

ag_frame = lonarr(1E4)
ag_frame_index = 0L

print, max_frame_separation, max_offset, F='(%"Scanning for pairs of events separated by up to %d exposures and offset by up to %d CCD pixels  ...")'

energ_lo = -1 
energ_hi = -1
for ii = 0, num_sources-1 do begin
  sourcedir   = sourcename[ii] + '/' + merge_subdir[ii] 
  merged_src_events_fn = sourcedir + src_events_basename
  photometry_fn        = sourcedir + src_photometry_basename
                           
  ; Read the composite extracted event list.
  bt = mrdfits(merged_src_events_fn, 1, src_events_hdr, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + merged_src_events_fn
                 
  if (sxpar(src_events_hdr, 'NAXIS2') LT 2) then begin
    print, 'WARNING: source '+sourcename[ii]+' has too few events to analyze.'
    continue
  endif

  frame_length =       sxpar(src_events_hdr, 'TIMEDEL') 

  ; Sort events by time.
  bt = bt[sort(bt.TIME)]
  
  chipx = bt.CHIPX    
  chipy = bt.CHIPY
  energy= bt.ENERGY
  time  = bt.TIME
  time -= time[0] 
  
  ; Between consecutive events compute time intervals in units of CCD frames, CHIPX/CHIPY offsets, and differences in event energies.
  frame  = round(time / frame_length)
  dframe = frame [1:*] - frame
  dx     = chipx [1:*] - chipx
  dy     = chipy [1:*] - chipy
  de     = energy[1:*] - energy

  ; Identify *pairs* of events that are suspect, i.e. "close" in time and space.
  bad_ind = where((dframe LE max_frame_separation) AND (abs(dx) LE max_offset) AND (abs(dy) LE max_offset), num_bad_pairs)

  ; Count how many individual events are suspect.
  num_events            = n_elements(bt)
  flagged_by_AE         = bytarr(num_events)
  if (num_bad_pairs GT 0) then begin
    flagged_by_AE[bad_ind  ] = 1
    flagged_by_AE[bad_ind+1] = 1
  endif
  
  ; Identify events flagged as afterglow by the CXC's aggressive algorithm acis_detect_afterglow (stored in STATUS bits 16-19) which was run on the data by our L1->L2 recipe.
  ; Code adapted from that in compare_event_lists.pro.
  status_word = swap_endian(ulong(bt.STATUS,0,num_events), /SWAP_IF_LITTLE_ENDIAN)
  mask = ishft('1'XUL,16) OR $
         ishft('1'XUL,17) OR $
         ishft('1'XUL,18) OR $
         ishft('1'XUL,19) 

  ag_bits  = long(ishft(mask AND status_word, -16))
  
  flagged_by_acis_detect_afterglow = (ag_bits NE 0)

  ; Build strings reporting the event energy, annotated with an * for events flagged by acis_detect_afterglow tool.
  energy_label = string(energy, F='(%"%5d")')
  annotation   = replicate(' ', num_events)
  ind = where(flagged_by_acis_detect_afterglow, count)
  if (count GT 0) then annotation[ind] = '*'
  energy_label += annotation
  
  ; Look up photometry in the specified energy band.
  bt = mrdfits(photometry_fn, 1, src_events_hdr, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + photometry_fn
  
  if (energ_lo EQ -1) then begin
    energ_lo = bt[band_num].ENERG_LO
    energ_hi = bt[band_num].ENERG_HI
  endif else begin
    if (energ_lo NE bt[band_num].ENERG_LO) then message, 'ERROR: energy band for '+sourcedir+' does not match the previous sources! '
    if (energ_hi NE bt[band_num].ENERG_HI) then message, 'ERROR: energy band for '+sourcedir+' does not match the previous sources! '
  endelse
  
  SRC_CNTS = bt[band_num].SRC_CNTS
  BKG_CNTS = bt[band_num].BKG_CNTS 
  BACKSCAL = bt[band_num].BACKSCAL
  
  is_inband           = (1000*energ_lo LE energy) AND (energy LE 1000*energ_hi)
      
  ; Sanity check the photometry against the event data.
  SRC_CNTS_local = total(/INT, is_inband)
  diff = abs(SRC_CNTS - SRC_CNTS_local) 
  src_cnts_inconsistency = diff / float(SRC_CNTS > SRC_CNTS_local)
  if (diff GT 1) && (src_cnts_inconsistency GT 0.01) then $
    print, photometry_fn, merged_src_events_fn, 100*src_cnts_inconsistency, F="(%'WARNING! SRC_CNTS in %s and energy filter applied to %s differ by > %0.1f%%.')"
  
  num_inband_suspect_events[ii] = total(/INT, is_inband AND (flagged_by_AE OR flagged_by_acis_detect_afterglow))
   
  fraction_inband_suspect_events[ii] = num_inband_suspect_events[ii]/(SRC_CNTS > 1.0)



  ; Write afterglow statistics to source.stats file.
  stats_fn  = sourcedir + src_stats_basename
  stats = headfits(stats_fn, ERRMSG=error)
  
  if keyword_set(error) then begin
    print, 'WARNING! Could not read '+stats_fn
    continue
  endif
    
  fxaddpar, stats, 'AG_FRAC', fraction_inband_suspect_events[ii], string(energ_lo, energ_hi, F="(%'suspected afterglow fraction, %5.3f:%6.3f keV')")

  writefits, stats_fn, 0, stats
  
  
  if (num_inband_suspect_events[ii] EQ 0) then continue
  
  
  ; Compute the expected number of time intervals less than the threshold,
  ; assuming constant Poisson arrival, using the exponential distribution.
  ;exposure           = float(sxpar(src_events_hdr, 'EXPOSURE'))
  ;lambda             = SRC_CNTS / exposure                   ; Poisson mean, counts/second
  ;interval_threshold = (max_frame_separation + 1) * frame_length  ; seconds
  ;frac_expected = 1 - exp(-lambda * interval_threshold)        ; integral of exponential distn, 0:threshold
  scaled_bkg = BKG_CNTS / float(BACKSCAL)
  SRC_CNTS_revised = (SRC_CNTS - num_inband_suspect_events[ii]) > 0
  Pb_revised[ii] = binomial(SRC_CNTS_revised, SRC_CNTS_revised + BKG_CNTS, 1D/(1D + BACKSCAL) , /DOUBLE) > 0

  print, sourcedir, strtrim(sxpar(stats,'LABEL'),2), num_inband_suspect_events[ii], (100.0*fraction_inband_suspect_events[ii]), SRC_CNTS, (100.0*num_inband_suspect_events[ii])/scaled_bkg, Pb_revised[ii], F='(%"\n%s (%10s): %d suspected in-band afterglow events (%0.1f%% of %d SRC_CNTS, %0.1f%% of background); P_b if suspects were removed: %0.5f")'
  
  if (num_inband_suspect_events[ii] GT 20) then begin
    print, 'More than 20 suspect events; skipped printing!'      
    continue
  endif
  
  print, 'dfrm    dx    dy        energy    frame MOD 10000'
  if (num_bad_pairs GT 0) then begin
    forprint, SUBSET=bad_ind, dframe, dx,dy, energy_label[0:num_events-2],energy_label[1:*], frame[0:num_events-2] MOD 10000L, F='(%"%4d %5d %5d    %6s->%6s        %4d")'
  endif
  
  ind = where(flagged_by_acis_detect_afterglow AND ~flagged_by_AE, count)
  if (count GT 0) then begin
    print, F='(%"These additional afterglow events were identified only by acis_detect_afterglow:")'
    forprint, SUBSET=ind, energy_label, frame MOD 1000, F='(%"                            %6s        %4d")'
  endif
  
  if (num_bad_pairs GT 0) then begin
    ag_frame[ag_frame_index] = frame[bad_ind]
    ag_frame_index          += num_bad_pairs
  endif
endfor ;ii


print, 'Energy Range used for reported statistics was ', energ_lo, energ_hi

;dataset_1d, id, ag_frame[0:ag_frame_index-1], XTIT='Frame #'
return
end


;==========================================================================
;;; Script to build exposure maps for specified sets of CCDs, binned by 1 skypixel

;;; CCD_LIST is a scalar or vector string specifying the CCDs that should be 
;;; included in the emap, e.g. ['012367','0123'].  
;;; If not supplied then DETNAM is used to define the CCD list.

;;; scene_name is a scalar or vector string (same number of elements as CCD_LIST)
;;; specifying the basename of the exposure map(s) to be created.

;;; Aspect histograms and instrument maps will be written to a subdirectory "asphist/".

;==========================================================================
PRO ae_make_emap, obsdata_filename, scene_name, CCD_LIST=ccd_list, ARDLIB_FILENAME=ardlib_fn, $
                  ASPECT_FN=aspect_fn, PBKFILE=pbk_fn, MASKFILE=mask_fn, $
                  MONOENERGY=monoenergy, SPECTRUM_FN=spectrum_fn, ONLYTIME=onlytime, $
                  MATCHFILE=matchfile, $
                  REUSE_ASPHIST=reuse_asphist, REUSE_INSTMAP=reuse_instmap, $
                  CCD_SCALING=ccd_scaling, SCALED_PREFIX=scaled_prefix

creator_string = "ae_make_emap, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()
exit_code = 0

;if ~keyword_set(ardlib_fn)   then ardlib_fn  = 'ardlib.par'
if ~keyword_set(aspect_fn)   then aspect_fn  = 'acis.astrometric.asol1'
if ~keyword_set(pbk_fn)      then pbk_fn     = 'acis.pbk0'
if ~keyword_set(mask_fn)     then mask_fn    = 'acis.msk1'
if ~keyword_set(monoenergy)  then monoenergy = 1.0
if ~keyword_set(spectrum_fn) then spectrum_fn= 'NONE'

;; ------------------------------------------------------------------------
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir

run_command, /INIT, PARAM_DIR=tempdir


;; ------------------------------------------------------------------------
;; Deal with ardlib.par.

;; Copy observer-supplied ardlib.par to private PFILES directory.
if ~keyword_set(ardlib_fn) then begin
  print, 'ERROR: you must supply the path to your observation-specific ardlib.par file via ARDLIB_FILENAME'
  GOTO, FAILURE
endif

if ~file_test(ardlib_fn) then begin
  print, ardlib_fn, F='(%"\nERROR: ARDLIB_FILENAME %s not found.")'
  GOTO, FAILURE
endif

file_copy, ardlib_fn, tempdir + 'ardlib.par'

;; Remind the user about what's in ardlib.
run_command, 'paccess ardlib r', ardlib_path, /QUIET
print, ardlib_path, F='(%"Examining important parameters in %s")'

run_command, 'pdump ardlib', ardlib_contents, /QUIET
print, F='(%"\nThe bad pixel list parameters below should correspond to your observation:")'
forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_ACIS?_BADPIX*'))]

print, 'PAUSING FOR 10 SECONDS SO YOU CAN REVIEW ARDLIB INFORMATION ABOVE:'
if  keyword_set(reuse_asphist)       then print, F='(%"\nWARNING! Using previously computed aspect histograms if available.")'
if  keyword_set(reuse_instmap)       then print, F='(%"\nWARNING! Using previously computed instrument maps if available.\n")'
wait, 10


;; ------------------------------------------------------------------------
; Determine which CCDs need to be processed.
theader = headfits(obsdata_filename, EXT=1, ERRMSG=error )
if (keyword_set(error)) then begin
  print, 'ERROR reading ' + obsdata_filename
  GOTO, FAILURE
endif

if n_elements(ccd_list) EQ 0 then begin
  ; Look up what CCDs are present in observation.
  detnam   = strtrim(sxpar(theader, 'DETNAM'),2)
  ccd_list = strmid(detnam,5)
endif

num_scenes = n_elements(scene_name)

case num_scenes of
  0: begin
     print, 'ERROR: parameter scene_name must be supplied.'
     GOTO, FAILURE
     end
     
  n_elements(ccd_list): 
     
  else: $
     begin
     print, 'ERROR: lengths of scene_name and CCD_LIST must match.'
     GOTO, FAILURE
     end
endcase 

; Figure out the list of CCDs used in any of the scenes.
flag = bytarr(10)
for jj=0,num_scenes-1 do $
  for ii=0,strlen(ccd_list[jj])-1 do flag[fix(strmid(ccd_list[jj],ii,1))] = 1
active_ccds = where(flag, num_active_ccds)
print, active_ccds, F='(%"Active CCDs are: %d %d %d %d %d %d %d %d ")'

if keyword_set(ccd_scaling) then begin
  if (n_elements(ccd_scaling) LT (1+max(active_ccds))) then begin
    print, 'ERROR: parameter CCD_SCALING must be a 10-element vector (one scaling for each CCD).'
    GOTO, FAILURE
  endif
endif else ccd_scaling = replicate(1.0,10)

if ~keyword_set(scaled_prefix) then scaled_prefix = 'scaled'


;; ------------------------------------------------------------------------
;; Create aspect histograms and instmaps for all active CCDs.
asphist_dir = 'asphist'
file_mkdir, asphist_dir

run_command, /QUIET, ['punlearn asphist','punlearn mkinstmap','punlearn mkexpmap','punlearn dmimgcalc','punlearn dmhedit']

base       =  asphist_dir + string(indgen(10), F='(%"/ccd%d.")')
asphist_fn = base + 'asphist'
instmap_fn = base + 'instmap'
emap_fn    = base + 'emap'

for ii=0,num_active_ccds-1 do begin
  ccd_id = active_ccds[ii]
  
  ;; ------------------------------------------------------------------------
  ;; Create an aspect histogram file.
  if ~keyword_set(reuse_asphist) || ~file_test(asphist_fn[ccd_id]) then begin
    run_command, string(aspect_fn, obsdata_filename, ccd_id, asphist_fn[ccd_id], $
                        F="(%'asphist infile=%s evtfile=""%s[ccd_id=%d]"" outfile=%s dtffile="""" clob+')")  
  endif

  bt = mrdfits(asphist_fn[ccd_id], 1, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + asphist_fn[ccd_id]

  duration = total(bt.duration)
  
  keyname  = string(ccd_id, F='(%"EXPOSUR%d")')
  exposure = sxpar(theader, keyname)
  
  if (abs(duration-exposure)/duration GT 0.01) then begin
    print, duration, asphist_fn[ccd_id], keyname, exposure, $
           F='(%"WARNING: Sum of DURATION column (%d) in %s does not match value of keyword %s (%d)!")'
    print, 'It is likely that your aspect histogram was not computed correctly!!!!!'
    wait, 1
  endif

  ;; ------------------------------------------------------------------------
  ;; Create instrument map.
  if ~keyword_set(reuse_instmap) || ~file_test(instmap_fn[ccd_id]) then begin
    if keyword_set(onlytime) then begin 
      run_command, string(asphist_fn[ccd_id], pbk_fn, instmap_fn[ccd_id], ccd_id, monoenergy, mask_fn, $
                          F="(%'mkinstmap obsfile=""%s[asphist]"" pbkfile=%s outfile=%s detsubsys=""ACIS-%d;IDEAL"" mirror=""HRMA;AREA=1"" dafile=NONE pixelgrid=""1:1024:#1024,1:1024:#1024"" spectrumfile=NONE monoenergy=%f maskfile=%s grating=NONE verbose=0 clob+')")  
    endif else begin
      run_command, string(asphist_fn[ccd_id], pbk_fn, instmap_fn[ccd_id], ccd_id, spectrum_fn, monoenergy, mask_fn, $
                          F="(%'mkinstmap obsfile=""%s[asphist]"" pbkfile=%s outfile=%s detsubsys=""ACIS-%d"" pixelgrid=""1:1024:#1024,1:1024:#1024"" spectrumfile=%s monoenergy=%f maskfile=%s grating=NONE verbose=0 clob+')")  
    endelse 
  endif
endfor ;ii


;; ------------------------------------------------------------------------
;; Create single-CCD and merged exposure maps for each scene.
;; Use of normalize=no produces a map with units of s*cm^2.
for jj = 0, num_scenes-1 do begin
  ; Figure out the list of CCDs used in THIS scene.
  flag = bytarr(10)
  for ii=0,strlen(ccd_list[jj])-1 do flag[fix(strmid(ccd_list[jj],ii,1))] = 1
  active_ccds = where(flag, num_active_ccds)

  ;; ------------------------------------------------------------------------
  if keyword_set(matchfile) then begin
    run_command, string(matchfile, F="(%'get_sky_limits %s verbose=0 precision=2')")

    xygrid = ")get_sky_limits.xygrid"
  endif else begin
    ;; Determine span of these CCDs on the sky.
    ;; We want to pad this by about 20 pixels on all sides, to avoid significant cropping of the emap for a shallow observation.
    ccd_spec = strcompress(strjoin(active_ccds,','),/REMOVE_ALL)
  
    run_command, string(obsdata_filename, ccd_spec, $
                        F="(%'dmstat ""%s[ccd_id=%s][cols x,y]"" median=no sigma=no verbose=0')")  
   
    run_command, /QUIET, 'pget dmstat out_min out_max', result
    xymin = floor(float(strsplit(result[0],',', /EXTRACT))) - 20
    xymax = ceil (float(strsplit(result[1],',', /EXTRACT))) + 20
    
    num_x_pixels = xymax[0] - xymin[0]
    num_y_pixels = xymax[1] - xymin[1]
    
    xygrid = string(xymin[0], xymax[0], num_x_pixels, xymin[1], xymax[1], num_y_pixels, F="(%'%d:%d:#%d,%d:%d:#%d')")
  endelse
  
  ;; ------------------------------------------------------------------------
  ;; Create each exposure map on the desired scene.
  for ii=0,num_active_ccds-1 do begin
    ccd_id = active_ccds[ii]
    
    run_command, string(instmap_fn[ccd_id], emap_fn[ccd_id], asphist_fn[ccd_id], xygrid, $
                        F="(%'mkexpmap instmapfile=%s outfile=%s asphistfile=%s xygrid=""%s""  normalize=no useavgaspect=no verbose=0 clob+')")
  endfor ;ii

  ;; ------------------------------------------------------------------------
  ;; Now combine the single-obsid exposure maps..
  outfile = scene_name[jj] + (keyword_set(onlytime) ? '.tmap' : '.emap')
  
  normal_formula = strjoin(                                                     string(1+indgen(num_active_ccds),F="(%'img%d')" ), '+')
  scaled_formula = strjoin(   string(ccd_scaling[active_ccds],F="(%'(%0.2f*')")+string(1+indgen(num_active_ccds),F="(%'img%d)')"), '+')
  infile_stack   = strjoin(              emap_fn[active_ccds],",")
  
  run_command,   string(infile_stack, outfile, normal_formula, $
                      F="(%'dmimgcalc infile=""%s"" infile2=none outfile=%s operation=""imgout=(float)(%s)"" verbose=1 clob+')")          
         
  if keyword_set(ccd_scaling) then $
    run_command, string(infile_stack, scaled_prefix+outfile, scaled_formula, $
                      F="(%'dmimgcalc infile=""%s"" infile2=none outfile=%s operation=""imgout=(float)(%s)"" verbose=1 clob+')")          
         

  ;; Report the pixel size of the emap.
  emap_header = headfits(outfile)
  
  print, outfile, sxpar(emap_header,'CDELT1P'), sxpar(emap_header,'CDELT2P'), F="(%'\nThe pixels in exposure map %s are %0.5f X %0.5f sky pixels.')"
endfor ;jj


CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif

if (exit_code EQ 0) then return $
else begin
  print, 'ae_make_emap: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end



;==========================================================================
;;; Tool to create lightcurves suitable for identifying background flares.

;;; Images are made for each CCD in the specified set of CCDs (e.g. CCD_LIST='01236').

;;; Bright pixels (corresponding to sources, which might flare themselves) are masked.

;;; Single-CCD lightcurves are made for the unmasked regions.

;;; Single-CCD lightcurves, and an average lightcurve are plotted.

;==========================================================================
PRO ae_show_flares, obsdata_filename,  CCD_LIST=ccd_list, CCDS_TO_SUM=ccds_to_sum, BINSIZE=binsize, TEMPLATE_FN=template_fn

creator_string = "ae_show_flares, version"+strmid("$Date: 2009-08-12 10:32:52 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()
exit_code = 0

if ~keyword_set(binsize)   then binsize  = 100


;; ------------------------------------------------------------------------
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
temp_events1_fn   = tempdir + 'temp.evt'
temp_events2_fn   = tempdir + 'temp.evt'

run_command, /INIT, PARAM_DIR=tempdir

base          = tempdir + string(indgen(10), F='(%"ccd%d.")')
obs_event_fn  = base + 'evt'
mask_fn       = base + 'mask'
bkg_event_fn  = base + 'bkg'
lightcurve_fn = string(indgen(10), F='(%"ccd%d.lc")')

ae_send_to_ds9, my_ds9, NAME='ae_show_flares_'+session_name, OPTION_STRING='-linear -bin factor 8'

;; ------------------------------------------------------------------------
; Parse the list of CCDs.
flag = bytarr(10)
for ii=0,strlen(ccd_list)-1 do flag[fix(strmid(ccd_list,ii,1))] = 1
active_ccds = where(flag, num_active_ccds)

;; ------------------------------------------------------------------------
;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes', 'punlearn dmimgpick', 'pset dmimgpick clobber=yes', 'punlearn dmextract', 'pset dmextract clobber=yes', 'punlearn dmmerge', 'pset dmmerge clobber=yes']


;; Process each CCD.
for ii=0,num_active_ccds-1 do begin
  ccd_id = active_ccds[ii]
    
  run_command, string(obsdata_filename, ccd_id, obs_event_fn[ccd_id], F="(%'dmcopy ""%s[ccd_id=%d]"" %s')")  
 
  ;; ------------------------------------------------------------------------
  ;; Determine span of the CCD on the sky.
  run_command, string(obs_event_fn[ccd_id], F="(%'dmstat ""%s[cols x,y]"" median=no sigma=no verbose=0')")  
 
  run_command, /QUIET, 'pget dmstat out_min out_max', result
  xymin = floor(float(strsplit(result[0],',', /EXTRACT))) 
  xymax = ceil (float(strsplit(result[1],',', /EXTRACT))) 
  
  ;; ------------------------------------------------------------------------
  ;; Create a data image for this CCD, binned 10x10 to try to get an off-axis star to fall in a few pixels.
  run_command, string(obs_event_fn[ccd_id], xymin[0], xymax[0], 10, xymin[1], xymax[1], 10, mask_fn[ccd_id], F="(%'dmcopy ""%s[bin x=%d:%d:%d,y=%d:%d:%d]"" %s')")  
  
  mask = readfits(mask_fn[ccd_id], header)
  
  ;; ------------------------------------------------------------------------
  ;; Estimate the mean of the image, ignore the off-field pixels.  .
  ind_onfield = where( smooth(float(mask),3) GT 0, count_onfield )
  estimate_poisson_background, mask[ind_onfield], pix_mean, SIGNIFICANCE=0.99, /VERBOSE

  pix_sigma = sqrt(pix_mean)
  
  ;; Mask the bright pixels (corresponding to sources, which might flare). 
  ind = where(mask GT (pix_mean + 3*pix_sigma), count)
  if (count GT 0) then mask[ind] = -1
  writefits, mask_fn[ccd_id], mask, header
  print, (100.0*count)/count_onfield, ccd_id, F='(%"The brightest %4.1f%% of the pixels on CCD %d have been masked.")'
  
  ;; ------------------------------------------------------------------------
  ;; Filter the event list with the mask  
  cmd1 = string(obs_event_fn[ccd_id], mask_fn[ccd_id], temp_events1_fn, $
                F="(%'dmimgpick ""%s[cols time,sky,ccd_id]"" %s %s method=closest')")

  cmd2 = string(temp_events1_fn, bkg_event_fn[ccd_id], F="(%'dmcopy ""%s[#4>0]"" %s')")
  run_command, [cmd1,cmd2]

  ;; ------------------------------------------------------------------------
  ;; Create light curve using either the supplied template, or using the first CCD's lightcurve as a template.
  if keyword_set(template_fn) then begin 
    binspec = string(template_fn, F='(%"[bin time=grid(%s[cols time_min,time_max])]")')
  endif else begin
    binspec = string(binsize, F="(%'[bin time=::%d]')")
    template_fn = lightcurve_fn[ccd_id]
  endelse
  
  run_command, string(bkg_event_fn[ccd_id], binspec, lightcurve_fn[ccd_id], F="(%'dmextract infile=""%s%s"" bkg=none outfile=%s opt=ltc1')")
endfor ;ii


;; ------------------------------------------------------------------------
;; Display the masked data.
run_command, string(strjoin(bkg_event_fn[active_ccds],','), temp_events1_fn, F="(%'dmmerge ""%s"" columnList="""" outfile=%s')")

ae_send_to_ds9, my_ds9, temp_events1_fn

;; ------------------------------------------------------------------------
;; Create an average light curve across the specified CCDs.
if keyword_set(ccds_to_sum) then begin
  average_lightcurve_fn = 'ccd'+ccds_to_sum+'.lc'
  
  ; Parse the list of CCDs to average.
  flag = bytarr(10)
  for ii=0,strlen(ccds_to_sum)-1 do flag[fix(strmid(ccds_to_sum,ii,1))] = 1
  averaged_ccds = where(flag, num_averaged_ccds)
  
  
  run_command, string(strjoin(bkg_event_fn[averaged_ccds],','), temp_events2_fn, F="(%'dmmerge ""%s"" columnList="""" outfile=%s')")
  
  run_command, string(temp_events2_fn, binspec, average_lightcurve_fn, F="(%'dmextract infile=""%s%s"" bkg=none outfile=%s opt=ltc1')")
endif

;; ------------------------------------------------------------------------
;; Plot the light curves.
print  
for ii=0,num_active_ccds-1 do begin
  ccd_id = active_ccds[ii]
 
  bt = mrdfits(lightcurve_fn[ccd_id], 1, /SILENT)
  print, lightcurve_fn[ccd_id], median(100*bt.COUNT_RATE_ERR/bt.COUNT_RATE), F='(%"LC %s has ~%d%% errors.")'
  function_1d, id1, PSYM=1, LINESTYLE=6, DATASET=lightcurve_fn[ccd_id], (binsize/1000.)*bt.TIME_BIN, bt.COUNT_RATE, ERROR=bt.COUNT_RATE_ERR, YTIT='counts/s', XTIT='time (ks)'
  dataset_1d,  id2,         LINESTYLE=0, DATASET=lightcurve_fn[ccd_id],                              bt.COUNT_RATE,                          XTIT='counts/s'
endfor ;ii

if keyword_set(ccds_to_sum) then begin
  bt = mrdfits(average_lightcurve_fn, 1, /SILENT)
  print, average_lightcurve_fn, median(100*bt.COUNT_RATE_ERR/bt.COUNT_RATE), F='(%"LC %s has ~%d%% errors.")'
                                                                                 
  function_1d, id1, COLOR='blue', PSYM=1, LINESTYLE=6, DATASET=average_lightcurve_fn, (binsize/1000.)*bt.TIME_BIN, bt.COUNT_RATE, ERROR=bt.COUNT_RATE_ERR    
  dataset_1d,  id2, COLOR='blue',         LINESTYLE=0, DATASET=average_lightcurve_fn,                              bt.COUNT_RATE
endif

wait, 5
CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif

if (exit_code EQ 0) then return $
else begin
  print, 'ae_make_emap: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end


;==========================================================================
;;; Split a source list into segments to support processing on multiple CPUs.
;==========================================================================
PRO ae_split_srclist, num_segments, segment_prefix, SRCLIST_FILENAME=srclist_fn

if ~keyword_set(srclist_fn) then srclist_fn = 'all.srclist'

readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ERROR: no entries read from source list ', srclist_fn
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\n%d sources found in catalog.\n")'

segment_fn = segment_prefix + string(indgen(num_segments),F='(%".%2.2d.srclist")')

file_delete, /NOEXPAND_PATH, /ALLOW_NONEXISTENT, segment_fn

spawn, 'rm -i '+segment_prefix+'.??.srclist'

sources_per_segment = ceil(num_sources/float(num_segments))
segment_start = 0L
for ii=0L, num_segments-1 do begin
  segment_end = (segment_start+sources_per_segment-1) < (num_sources-1)
  forprint, TEXTOUT=segment_fn[ii], /NoComm, sourcename[segment_start:segment_end]
  print, 'Wrote '+segment_fn[ii]
  segment_start = segment_end + 1
  if (segment_start GE num_sources) then break
endfor

print, F='(%"\n Use the csh prefixes below when you start your IDL sessions so each will know which segment to process.")'
forprint, TEXTOUT=2, indgen(num_segments), F="(%'; setenv SEGMENT "".%2.2d"" ;')"

return
end


PRO acis_extract_tools
return
end

