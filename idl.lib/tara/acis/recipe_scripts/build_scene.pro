;;; This is an automated version of the Townsley ACIS analysis recipe image_construction.txt.

;;; $Id: build_scene.pro 3509 2009-08-07 17:55:49Z psb6 $

;;; The input template_name must be a string scalar containing any ONE of these template names:  
;;;   'fullfield','sarray','iarray','central_1', 'core_0.5'.

;;; The optional input /CREATE_TEMPLATE allows you to define a template 
;;; using a set of co-aligned observations of the same pointing, but then later to 
;; create images with that existing template using a wider set of observations.

;;; If MERGED_EVENTFILE is supplied then the reprojected event lists are combined and saved to the specified file.
;;; MERGED_COLUMNLIST can be used to supply a column filter that's applied to the merged event list.
;;; The merged event list is NOT spatially filtered by the scene.
;;; An energy filter for the merged event list can be supplied via MERGED_FILTERSPEC.

@acis_extract

PRO build_scene, template_name, SUFFIX=suffix, PATTERN=event_file_pattern, OBS_EVENT_FN=obs_event_fn, CREATE_TEMPLATE=create_template, $
  
  CREATE_IMAGE=create_image, IMAGE_FILTER_SPEC=image_filter_spec, IMAGE_NAME=image_name, SUM_RATES_NOT_COUNTS=sum_rates_not_counts, $
  
  MERGED_EVENTFILE=merged_eventfile, MERGED_COLUMNLIST=merged_columnlist, MERGED_FILTERSPEC=merged_filterspec, $
  
  EMAP_BASENAME=emap_basename_p, RESOLUTION=resolution, SHOW=show

creator_string = "build_scene, version"+strmid("$Date: 2009-08-07 13:55:49 -0400 (Fri, 07 Aug 2009) $", 6, 11)
print, creator_string
print, systime()
exit_code = 0


tangentplane_reference_fn = 'tangentplane_reference.evt'

;; ------------------------------------------------------------------------
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
temp_text_fn     = tempdir + 'temp.txt'

run_command, /INIT, PARAM_DIR=tempdir

run_command, /QUIET, ['pset dmcopy clobber=yes','pset reproject_events clobber=yes','pset dmimgcalc lookupTab=none clobber=yes','pset reproject_image clobber=yes']
 
if ~keyword_set(suffix)             then suffix=''
if ~keyword_set(event_file_pattern) then event_file_pattern = 'obsid*/acis.validation.evt2'

if (n_elements(image_filter_spec) EQ 0) then image_filter_spec = ['energy=500:7000', 'energy=2000:7000', 'energy= 500:2000']
if (n_elements(image_name)        EQ 0) then image_name        = [      'full_band',        'hard_band',        'soft_band']

if (n_elements(image_filter_spec) NE n_elements(image_name)) then begin
  print, 'ERROR: IMAGE_FILTER_SPEC and IMAGE_NAME must have the same number of elements.'
  GOTO, FAILURE
endif

template_fn     = template_name+       '_template.img'
merged_image_fn = template_name+suffix+'.img'
merged_emap_fn  = template_name+suffix+'.emap'

case template_name of
 'fullfield': $
    begin
    reproject_filter = ''
    regrid_filter    = ''
    emap_basename= 'fullfield_1.emap'
    end
 'sarray': $
    begin
    ; We want the sarray template to be defined using all the reprojected CCDs (same as the fullfield template).
    ; We'll filter again later during the image generation.
    if keyword_set(create_template) then begin
      reproject_filter = ''
      regrid_filter    = 'ccd_id>3,'
    endif else begin
      reproject_filter= 'ccd_id>3,'
      regrid_filter     = ''
    endelse
    emap_basename= 'sarray_1.emap'
    end
 'iarray' : $
    begin
    reproject_filter = 'ccd_id=0:3'
    regrid_filter    = ''
    emap_basename= 'iarray_1.emap'
    end
 'central_1'  : $
    begin
    reproject_filter = 'ccd_id=0:3'
    regrid_filter    = ''
    emap_basename= 'iarray_1.emap'
    end
 'core_0.5'   : $
    begin
    reproject_filter = 'ccd_id=0:3'
    regrid_filter    = ''
    emap_basename= 'iarray_1.emap'
    end
 else: begin
       print, 'ERROR! Ignoring unknown template named ', template_name
       retall
       end
endcase

if keyword_set(emap_basename_p) then emap_basename = emap_basename_p

if keyword_set(obs_event_fn) then begin
  num_obs = n_elements(obs_event_fn)
endif else begin
  ;; Find the event data and emaps we have to work with.
  obs_event_fn = file_search(event_file_pattern, COUNT=num_obs)
  if (num_obs EQ 0) then begin
    print, 'NO event data found!'
    GOTO, FAILURE
  endif
endelse

obs_emap_fn = file_dirname(obs_event_fn)+'/'+emap_basename


reproj_obs_event_fn = tempdir + string(indgen(num_obs), F='(%"reproj%d.evt")')
reproj_obs_emap_fn  = tempdir + string(indgen(num_obs), F='(%"reproj%d.emap")')

if ~file_test(tangentplane_reference_fn) then begin
  ;; Use the longest observation as the root tangent plane.
  exposure = fltarr(num_obs)
  for ii=0, num_obs-1 do begin
    header = headfits(obs_event_fn[ii], EXT=1)
    exposure[ii] = sxpar(header,'EXPOSURE')
  endfor ; ii
  dum = max(exposure, imax)
  
  file_delete, /QUIET, tangentplane_reference_fn
  file_link, obs_event_fn[imax], tangentplane_reference_fn
endif 

;; Reproject event data for the desired set of CCDs onto the root tangent plane.
;; Even the root observation is passed through the reproject command in order to get CCD filtering.
this_filter = keyword_set(reproject_filter) ? '['+reproject_filter+']' : ''
for ii=0, num_obs-1 do begin
  ; Reproject the data onto the reference tangent plane.
  ; We use random=0 because random=-1 produced an error message for stowed files, which don't have a TIME column.
  run_command, string(obs_event_fn[ii], this_filter, reproj_obs_event_fn[ii], tangentplane_reference_fn, F="(%'reproject_events ""%s%s"" %s match=%s random=0 aspect=none')")
endfor ; ii


if keyword_set(merged_eventfile) then begin
  column_spec = keyword_set(merged_columnlist) ? '[cols '+merged_columnlist+']' : ''
  filter_spec = keyword_set(merged_filterspec) ? '['     +merged_filterspec+']' : ''

  ; There's a bug in dmmerge in CIAO 3.3, 3.4 that messes up GTI tables. 
  ; We follow the instructions on the dmmerge bugs page (http://cxc.harvard.edu/ciao/bugs/dmmerge.html) 
  ; to ensure that the GTIs are going to be combined: append [subspace -expno] to each file specification.
  infile_stack = reproj_obs_event_fn+filter_spec+column_spec+'[subspace -expno,-sky]'
  print, F="(%'\nMerging these data:')"
  forprint, infile_stack
  forprint, infile_stack, TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
  
  run_command, string( temp_text_fn, merged_eventfile, F="(%'dmmerge ""@-%s"" %s clob+')")                   
endif


if keyword_set(create_template) then begin
  xymin =  !values.F_INFINITY 
  xymax = -!values.F_INFINITY 
  
  if (template_name EQ 'fullfield') || (template_name EQ 'sarray') || (template_name EQ 'iarray') then begin
    ;; Determine span of the data on the tangent plane.
    for ii=0, num_obs-1 do begin
      run_command, string(reproj_obs_event_fn[ii], F="(%'dmstat ""%s[cols x,y]"" median=no sigma=no verbose=0')")  
  
      run_command, /QUIET, 'pget dmstat out_min out_max', result
      xymin <= float(strsplit(result[0],',', /EXTRACT)) 
      xymax >= float(strsplit(result[1],',', /EXTRACT)) 
    endfor ; ii
    cen_x = round(mean([xymax[0],xymin[0]]))
    cen_y = round(mean([xymax[1],xymin[1]]))
    ; We pad the scene's field of view by a little bit, and make it square.
    half_dim = 20 + ceil( ((xymax[0]-xymin[0]) > (xymax[1]-xymin[1]))/2 )
  endif
  
  case template_name of
   'fullfield': $
      begin
      cmd = string(tangentplane_reference_fn, cen_x-half_dim,cen_x+half_dim, cen_y-half_dim,cen_y+half_dim, template_fn, F="(%'dmcopy ""%s[bin x=%d:%d:#1024,y=%d:%d:#1024]"" %s')")
      end
   'sarray': $
      begin
      ; We want the sarray template to be defined using all the CCDs (same as the fullfield template). 
      cmd = string(tangentplane_reference_fn, cen_x-half_dim,cen_x+half_dim, cen_y-half_dim,cen_y+half_dim, template_fn, F="(%'dmcopy ""%s[bin x=%d:%d:#1024,y=%d:%d:#1024]"" %s')")
      end
   'iarray' : $
      begin
      cmd = string(tangentplane_reference_fn, cen_x-half_dim,cen_x+half_dim, cen_y-half_dim,cen_y+half_dim, template_fn, F="(%'dmcopy ""%s[bin x=%d:%d:#1024,y=%d:%d:#1024]"" %s')")
      end
      
   'central_1'  : $
      begin
      ; The two central scenes are centered on the aimpoint.
      cen_x = 4096
      cen_y = 4096
      cmd = string(tangentplane_reference_fn, cen_x-512,cen_x+512, cen_y-512,cen_y+512, template_fn, F="(%'dmcopy ""%s[bin x=%d:%d:#1024,y=%d:%d:#1024]"" %s')")
      end
      
   'core_0.5'   : $
      begin
      ; The two central scenes are centered on the aimpoint.
      cen_x = 4096
      cen_y = 4096
      cmd = string(tangentplane_reference_fn, cen_x-256,cen_x+256, cen_y-256,cen_y+256, template_fn, F="(%'dmcopy ""%s[bin x=%d:%d:#1024,y=%d:%d:#1024]""  %s')")
      end
      
   else: begin
         print, 'ERROR! Ignoring unknown template name ', template_name
         GOTO, FAILURE
         end
  endcase
 run_command, cmd
endif ; keyword_set(create_template)

if ~keyword_set(create_image) then GOTO, CLEANUP


;; Reproject the exposure maps.
num_emaps = total(/INT, file_test(obs_emap_fn))
if (num_emaps NE num_obs) then begin
  print, 'ERROR: some of these emaps are missing: ', obs_emap_fn
  GOTO, FAILURE
endif

if ~keyword_set(resolution) then resolution = 1

; Note that the exposure map is a 2-D FUNCTION representing a physical quantity (typically, effective area
; multiplied by exposure time, in units of sec*cm^2*counts/photon), not a 2-D HISTOGRAM that is counting something
; with "per pixel" units. Thus, when rebinning we choose method=average so that the scale of the exposure map is
; unchanged.
print, F="(%'\nReprojecting the emaps:')"
;forprint, obs_emap_fn
for ii=0, num_obs-1 do begin
  run_command, string(obs_emap_fn[ii], template_fn, reproj_obs_emap_fn[ii], resolution, $
                       F="(%'reproject_image  infile=%s  matchfile=%s  outfile=%s  method=average resolution=%d')")
                     
  if keyword_set(sum_rates_not_counts) then begin
    ; We later want to scale the images of the event data by these emaps (dmimgcalc call below).
    ; To avoid NaN results we need to set the emap's zeros to a large number.
    emap = readfits(reproj_obs_emap_fn[ii], emap_header)
    ind = where(emap LE 0, count)
    if (count GT 0) then emap[ind] = !VALUES.F_INFINITY
    writefits, reproj_obs_emap_fn[ii], emap, emap_header
  endif
endfor


;; Find the binning that matches the template.
header = headfits(template_fn, ERRMSG=error )
if keyword_set(error) then begin
  print, 'ERROR reading ' + template_fn
  GOTO, FAILURE
endif
template_xdim = sxpar(header, 'NAXIS1')
template_ydim = sxpar(header, 'NAXIS2')

run_command, string(template_fn, F="(%'get_sky_limits %s verbose=0 precision=2')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', bin_spec
bin_spec = '[bin '+bin_spec[0]+']'


;; Filter each reprojected eventlist, bin into an image, and sum.
terms = string(1+indgen(2*num_obs),F="(%'img%d')")

for ii=0, n_elements(image_filter_spec)-1 do begin
  file_mkdir, image_name[ii]
  this_filter_spec = '['+regrid_filter+image_filter_spec[ii]+']' 
  
  infile_stack = reproj_obs_event_fn+this_filter_spec+bin_spec
  formula = strjoin(terms[0:num_obs-1], '+')
  
  if keyword_set(sum_rates_not_counts) then begin
    infile_stack = [infile_stack, reproj_obs_emap_fn]
    formula = strjoin( '('+terms[0:num_obs-1]+'/'+terms[num_obs:2*num_obs-1]+')', '+')
  endif
  
  print, F="(%'\nCombining these data:')"
  forprint, terms, infile_stack, F='(%"%s = %s")'
  forprint,        infile_stack, F='(%"%s")', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
  
  run_command, string(temp_text_fn, image_name[ii]+'/'+merged_image_fn, formula, $
                      F="(%'dmimgcalc infile=""@-%s"" infile2=none outfile=%s operation=""imgout=(%s)"" verbose=1 clob+')")          
endfor ; ii
review_list = image_name+'/'+merged_image_fn



if ~keyword_set(sum_rates_not_counts) then begin
  ;; Sum the exposure maps.
  print, F="(%'\nSumming the reprojected emaps:')"
  forprint, terms, reproj_obs_emap_fn, F='(%"%s = %s")'
  forprint,        reproj_obs_emap_fn, F='(%"%s")', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
  
  formula = strjoin(terms[0:num_obs-1], '+')

  run_command, string(temp_text_fn, merged_emap_fn, formula, $
                      F="(%'dmimgcalc infile=""@-%s"" infile2=none outfile=%s operation=""imgout=(%s)"" verbose=1 clob+')")          
  review_list = [review_list,merged_emap_fn]
endif


cmd = string(strjoin(review_list,' '), F="(%'ds9 %s &')")
if keyword_set(show) then begin
  print, 'Spawning ',cmd
  cd, CURRENT=cwd  &  cwd=file_basename(cwd)
  ae_send_to_ds9, my_ds9, NAME='build_scene.'+session_name+'@'+cwd, OPTION_STRING=''
  ae_send_to_ds9, my_ds9, review_list
endif else print, cmd, F="(%'\n Review images with:\n  %s')"


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
  print, 'build_scene: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end



