;;; This is an automated version of the Townsley ACIS analysis recipe image_construction.txt.

;;; $Id: smooth_scene.pro 3240 2008-09-15 16:21:26Z patb $

;;; The input template_name must be a string scalar containing any ONE of these template names:  
;;;   'fullfield','sarray','iarray','central_1', 'core_0.5', 'iarray.project'.

;;; This tool should be run from a "pointing" directory, i.e. one with composite emaps, and one or more sub-directories holding composite images to smooth.
;;; Those directories can be specified by IMAGE_NAME---the default is ['full_band','hard_band','soft_band']

;;; This tool should be multi-thread safe since we use a private PFILES dir and csmooth does not seem to use any scratch files.

@acis_extract

PRO smooth_scene, template_name, IMAGE_NAME=image_name, SHOW=show

creator_string = "smooth_scene, version"+strmid("$Date: 2008-09-15 12:21:26 -0400 (Mon, 15 Sep 2008) $", 6, 11)
print, creator_string
print, systime()
exit_code = 0


;; ------------------------------------------------------------------------
;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (~file_test(tempdir))
file_mkdir, tempdir
print, 'Using temporary directory: ', tempdir
param_dir        = tempdir + 'param/'
temp_text_fn     = tempdir + 'temp.txt'

run_command, /INIT, PARAM_DIR=param_dir

run_command, /QUIET, ['pset csmooth clobber=yes','pset dmimgcalc lookupTab=none clobber=yes']

if (n_elements(image_name) EQ 0) then begin
  image_name = [      'full_band',        'hard_band',        'soft_band']
endif

merged_image_fn = template_name+'.img'
merged_emap_fn  = template_name+'.emap'

if ~file_test(merged_emap_fn) then begin
  print, 'ERROR: cannot find ' + merged_emap_fn
  retall
endif
emap = readfits(merged_emap_fn, emap_header)

; For speed, choose a maximum smoothing scale that is based on the longest image dimension.
; See discussion of bug at http://cxc.harvard.edu/ciao4.0/bugs/csmooth.html
sclmax = (sxpar(emap_header,'NAXIS1') > sxpar(emap_header,'NAXIS2')) / 15

if ~stregex(/BOOLEAN, template_name, 'central_1|core_0.5')  then begin
  ;; Create a field mask from the emap.
  mask_fn         = template_name+'.mask'
  mask = (emap GT 0)
  writefits, mask_fn, mask, emap_header
endif


;; Filter each reprojected eventlist and bin into an image.
kernel_fn       = merged_image_fn+'.kernel'
smooth_image_fn = merged_image_fn+'.smooth.norm'
for ii=0, n_elements(image_name)-1 do begin
  ; Smooth the observed data.
  run_command, DIRECTORY=image_name[ii],$
    string(merged_image_fn, merged_image_fn, merged_image_fn, kernel_fn, sclmax, $
           F="(%'csmooth infile=%s outfile=%s.smooth outsigfile=%s.sig outsclfile=%s sclmode=compute sigmin=2.25 sigmax=3.25 sclmin=INDEF sclmax=%d sclmap="""" conmeth=fft conkerneltype=gauss verbose=2')")                   

  ; Co-smooth the emap using the same kernels.
  run_command, $
    string(merged_emap_fn, merged_emap_fn, merged_emap_fn, merged_emap_fn, image_name[ii], kernel_fn, $
           F="(%'csmooth infile=%s outfile=%s.smooth outsigfile=%s.sig outsclfile=%s.kernel sclmode=user sclmap=%s/%s sigmin=0 sigmax=0  sclmin=INDEF sclmax=INDEF conmeth=fft conkerneltype=gauss verbose=2')")                   

  ; Divide smoothed image by smoothed exposure map.
  if keyword_set(mask_fn) then begin
    cmd = string(merged_image_fn, mask_fn, merged_emap_fn, smooth_image_fn, $
           F="(%'dmimgcalc infile=""%s.smooth,../%s,../%s.smooth"" infile2=none outfile=%s operation=""imgout=(img1*img2)/img3"" verbose=1')")                   
  endif else begin
    cmd = string(merged_image_fn,          merged_emap_fn, smooth_image_fn, $
           F="(%'dmimgcalc infile=""%s.smooth,../%s.smooth""       infile2=none outfile=%s operation=""imgout=img1/img2"" verbose=1')")                   
  endelse
  
  run_command, DIRECTORY=image_name[ii], cmd
endfor ; ii

review_list = [image_name+'/'+smooth_image_fn]
cmd = string(strjoin(review_list,' '), F="(%'ds9 -log -bin factor 8 %s &')")
if keyword_set(show) then begin
  print, 'Spawning ',cmd
  cd, CURRENT=cwd  &  cwd=file_basename(cwd)
  ae_send_to_ds9, my_ds9, NAME='smooth_scene.'+session_name+'@'+cwd, OPTION_STRING='-log -bin factor 8'
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
  print, 'smooth_scene: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
return
end


