;;; $Id: acis_extract.pro 3519 2009-08-12 15:00:14Z psb6 $
;;; A tool for extraction of ACIS point sources.
;;; Patrick Broos & Leisa Townsley, Penn State University, 2002
;;; patb@astro.psu.edu   townsley@astro.psu.edu

;;; See extensive manual at http://www.astro.psu.edu/xray/docs/TARA/ae_users_guide.html

;;; This software is offered freely to the ACIS observer community under these 
;;; conditions:
;;; 1. You will not remove any of the header comments in the original file.
;;; 2. If you alter this file in minor ways to accomodate your local computing
;;;    environment, you will note that fact and your name in additional header
;;;    comments.
;;; 3. If you alter this file in ways that change its functionality you will note 
;;;    that fact and your name in additional header comments, AND YOU WILL CHANGE
;;;    THE NAME OF THE FILE/ROUTINE.  You are of course welcome to send us 
;;;    changes that you've found helpful and we will consider rolling them 
;;;    into acis_extract itself.

;;; Stage names to search with:
;;;    construct_regions show_regions extract_events check_positions new_catalog extract_spectra arf_correction_filename extract_backgrounds merge_observations fit_spectra cartoon_template                              
                  

; =============================================================================
PRO run_command, DIRECTORY=directory, command, result, STATUS=status, IGNORE_STATUS=ignore_status, $
                 HEASOFT=heasoft, UNIX=unix, SAS=sas, QUIET=quiet, INTERACTIVE=interactive, $
                 INIT=init, PARAM_DIR=param_dir

COMMON run_command, ciao_env, heasoft_env, sas_env

on_error, 2 

status = 0

;; ------------------------------------------------------------------------
;; Initialize command spawning machinery.
if keyword_set(init) then begin  
  ;; Set the variable FAST_START within the IDL environment.  
  ;; It will be inherited by all shells spawned; if the user has configured 
  ;; .cshrc correctly, the setup of the HEASOFT & CIAO packages will be skipped,
  ;; saving considerable time.
  ;; Shells spawned by run_command explicitly set up the package required.
  setenv, 'FAST_START=1'

  spawn, 'hostname', hostname, /NOSHELL
  print, hostname, F='(%"hostname is %s\n")'

  ;; Check for common environment errors.
;  print, 'Looking for AstroLib ...'
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

;  print, 'Looking for TARA ...'
  catch, error_code
  if (error_code NE 0) then begin
    print, 'ERROR: the TARA package is not in your IDL path.'
    retall
  endif else resolve_routine, 'function_1d'
  catch, /CANCEL
  !QUIET = quiet

  library_path = (!VERSION.OS EQ 'darwin') ? 'DYLD_LIBRARY_PATH' : 'LD_LIBRARY_PATH'

  ;; Save the initial values of $PATH, $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH
  baseline_path              = getenv('PATH')
  baseline_ld_library_path   = getenv('LD_LIBRARY_PATH')
  baseline_dyld_library_path = getenv('DYLD_LIBRARY_PATH')
  
  ;; ------------------------------------------------------------------------
  ;; Configure CIAO, save environment.
;  print, 'Configuring CIAO ...'
  spawn, 'which ciao', env, COUNT=count, EXIT_STATUS=exit_status
  if (exit_status NE 0) then begin
    print, 'ERROR: alias "ciao" is not defined.'
    retall
  endif
  ; Remove $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH so spawned shell will start fresh.
  setenv, 'LD_LIBRARY_PATH='  
  setenv, 'DYLD_LIBRARY_PATH='
  ; We unsetenv DISPLAY to speed up 'ciao' and to prevent failure when $DISPLAY is
  ; not valid (e.g. as happens when 'screen' is used to manage processes).
  spawn, 'unsetenv DISPLAY; ciao; env', env
  env_ind = where(strmatch(env, '*=*'), env_count, COMPLEMENT=print_ind, NCOMPLEMENT=print_count)
  if (env_count EQ 0) then begin
    print, 'ERROR: shell command "env" seems to have failed:'
    forprint, env
    retall
  endif
  for ii=0, print_count-1 do begin
    line = env[print_ind[ii]]
    if strmatch(line, 'Warning: X DISPLAY variable is not set.*') then continue
    if strmatch(line, '*GUI applications*') then continue
    if strmatch(line, 'CIAO configuration is complete*') then continue
    print, line
  endfor ;ii
  
  ; Rather than hard-coding the environment variables that CIAO and HEASOFT use, we simply 
  ; import the entire environment into the IDL process.
  ; Long values in the environment are skipped, since some users have .cshrc files that
  ; cause some variables to grow without bound.
  for ii=0, env_count-1 do begin
    line = env[env_ind[ii]]
    if (strlen(line) LT 1000) then setenv, line
  endfor
  ciao_env = 'setenv PATH '+getenv('PATH')+'; setenv '+library_path+' '+getenv(library_path)+'; '

  ; Restore baseline values of $PATH, $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH.
  setenv, 'PATH='             +baseline_path
  setenv, 'LD_LIBRARY_PATH='  +baseline_ld_library_path
  setenv, 'DYLD_LIBRARY_PATH='+baseline_dyld_library_path
  
  ; Verify the CIAO version is adequate.
  cmd = 'cat '+getenv('ASCDS_INSTALL')+'/VERSION'
  spawn, cmd, result
  ciao_version = float((stregex(result[0],'CIAO ([0-9]+.[0-9]*)',/SUB,/EXT))[1])
  if (ciao_version LT 4.0) then begin
    print
    print, 'ERROR: AE requires CIAO version 4.0 or later.'
    retall
  endif

  ;; ------------------------------------------------------------------------
  ;;Configure HEASOFT, save environment.
;  print, 'Configuring HEASOFT ...'
  spawn, 'which heasoft', env, COUNT=count, EXIT_STATUS=exit_status
  if (exit_status NE 0) then begin
    print, 'ERROR: alias "heasoft" is not defined.'
    retall
  endif
  ; Remove $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH so spawned shell will start fresh.
  setenv, 'LD_LIBRARY_PATH='  
  setenv, 'DYLD_LIBRARY_PATH='
  spawn, 'unsetenv DISPLAY; heasoft; env', env
  env_ind = where(strmatch(env, '*=*'), env_count, COMPLEMENT=print_ind, NCOMPLEMENT=print_count)
  if (env_count EQ 0) then begin
    print, 'ERROR: shell command "env" seems to have failed:'
    forprint, env
    retall
  endif
  if (print_count GT 0) then forprint, env, SUBSET=print_ind

  ; Rather than hard-coding the environment variables that CIAO and HEASOFT use, we simply 
  ; import the entire environment into the IDL process.
  ; Long values in the environment are skipped, since some users have .cshrc files that
  ; cause some variables to grow without bound.
  for ii=0, env_count-1 do begin
    line = env[env_ind[ii]]
    if (strlen(line) LT 1000) then setenv, line
  endfor
  heasoft_env = 'setenv PATH '+getenv('PATH')+'; setenv '+library_path+' '+getenv(library_path)+'; '

  ;; Restore baseline values of $PATH, $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH.
  setenv, 'PATH='             +baseline_path
  setenv, 'LD_LIBRARY_PATH='  +baseline_ld_library_path
  setenv, 'DYLD_LIBRARY_PATH='+baseline_dyld_library_path


  ;; ------------------------------------------------------------------------
  if keyword_set(sas) then begin  
    ;;Configure SAS, save environment.
  ;  print, 'Configuring SAS ...'
    spawn, 'which loadsas', env, COUNT=count, EXIT_STATUS=exit_status
    if (exit_status NE 0) then begin
      print, 'ERROR: alias "loadsas" is not defined.'
      retall
    endif
    ; Remove $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH so spawned shell will start fresh.
    setenv, 'LD_LIBRARY_PATH='  
    setenv, 'DYLD_LIBRARY_PATH='
    spawn, 'unsetenv DISPLAY; loadsas; env', env
    env_ind = where(strmatch(env, '*=*'), env_count, COMPLEMENT=print_ind, NCOMPLEMENT=print_count)
    if (env_count EQ 0) then begin
      print, 'ERROR: shell command "env" seems to have failed:'
      forprint, env
      retall
    endif
    if (print_count GT 0) then forprint, env, SUBSET=print_ind
  
    ; Rather than hard-coding the environment variables that CIAO and HEASOFT use, we simply 
    ; import the entire environment into the IDL process.
    ; Long values in the environment are skipped, since some users have .cshrc files that
    ; cause some variables to grow without bound.
    for ii=0, env_count-1 do begin
      line = env[env_ind[ii]]
      if (strlen(line) LT 1000) then setenv, line
    endfor
    sas_env = 'setenv PATH '+getenv('PATH')+'; setenv '+library_path+' '+getenv(library_path)+'; '
  
    ;; Restore baseline values of $PATH, $LD_LIBRARY_PATH, and $DYLD_LIBRARY_PATH.
    setenv, 'PATH='             +baseline_path
    setenv, 'LD_LIBRARY_PATH='  +baseline_ld_library_path
    setenv, 'DYLD_LIBRARY_PATH='+baseline_dyld_library_path
  endif

  
  ;; Make a private PFILES directory and point to it and the CIAO/HEASOFT system directories.
  ;; The PFILES path must be absolute, not relative, so tools can execute from anywhere.
  if ~keyword_set(param_dir) then begin
    print, 'ERROR: PARAM_DIR must specify a writable directory to store CIAO and FTOOLS parameter files.'
    retall
  endif
  
  for ii=0,n_elements(param_dir)-1 do if ~file_test(param_dir[ii], /DIRECTORY, /WRITE) then message, 'Writable parameter directory '+param_dir[ii]+' not found.'
  
  setenv, string(strjoin(param_dir,':'), getenv("ASCDS_INSTALL"), getenv("ASCDS_INSTALL"), getenv("LHEASOFT"), F='(%"PFILES=%s;%s/contrib/param:%s/param:%s/syspfiles")')

  print, F='(%"\nEnvironment set up for HEASOFT & CIAO with parameter paths:")'
  print, break_path(repchr(getenv("PFILES"), ';', ':'), /NOCURRENT), F='(2x,A)'
  print
  
  ; Take steps to prevent $SHLVL from growing without bound.
  setenv, 'SHLVL=1'
  
  ; Measure the spawning overhead.
  t0=systime(1) & run_command, 'exit', /QUIET 
  run_time = systime(1)-t0
  if (run_time GT 1) then print, run_time, F='(%"WARNING! Spawning a shell is taking %4.1f seconds.  \nUse the FAST_START environment variable to skip slow commands in .cshrc")'
  return
endif


;; ------------------------------------------------------------------------
;; Spawn the specified commands.

prefix = ciao_env
if keyword_set(heasoft) then prefix = heasoft_env
if keyword_set(sas)     then prefix = sas_env
if keyword_set(unix)    then prefix = ''

if ~keyword_set(quiet) then begin
  if keyword_set(directory) then print, directory, F='(%"\nSpawning (in %s):")' $
                            else print,            F='(%"\nSpawning:")'
  forprint, command, F="(%'  %s')"
  print
endif

cmd = strjoin(command,"; ")
result_count = 0

if keyword_set(directory) then pushd, directory

; When the caller wants the user to interact with the spawned process, then we can NOT ask spawn to trap stdout/stderr.
cmd = prefix+cmd
if keyword_set(interactive) then $
  spawn, cmd, EXIT_STATUS=exit_status $
else $
  spawn, cmd, EXIT_STATUS=exit_status, result, COUNT=result_count, /STDERR

if keyword_set(directory) then popd

if (exit_status EQ 0) || keyword_set(ignore_status) then begin
  ; Nominal spawn
  if (result_count GT 0) && ~keyword_set(quiet) then begin
    ; Show output text.
    forprint, result, F="(%'  > %s')"
    print
  endif

endif else begin
  ; Failed spawn. 
  status = exit_status
  
  ; If we didn't print the commands before, then do it now.
  if keyword_set(quiet) then begin
    if keyword_set(directory) then print, directory, F='(%"\nSpawning (in %s):")' $
                              else print,            F='(%"\nSpawning:")'
    forprint, command, F="(%'  %s')"
    print
  endif
  
  ; Show output text.
  if (result_count GT 0) then forprint, result, F="(%'  > %s')"

  print, exit_status, F='(%"\nERROR: spawned process failed with exit status %d!\n")'
  
  if keyword_set(prefix) then print, prefix, F="(%'Environment used for spawn was: %s\n')"
  
  if arg_present(status) then return
  
  message, /NONAME, 'Investigate the failure, e.g. by trying the failing command manually in a shell.  If you can fix the problem and want AE to re-try the command then type ".continue"; if you have run the command manually and want AE to skip over it then type .skip and then .continue; if you want to abort the AE run type "retall".'
endelse

return
end


; =============================================================================
;; Convert a region file from ds9 to CIAO format, and reduce size of polygon
;; entries as required to get file size under the CIAO 32000 character limit.
;;
;; The coordinates of the first polygon are returned in POLYGON_X, POLYGON_Y.
;; Return region_edited='T' if any line starts with "physical", suggesting
;; that it was written by a ds9 session rather than by AE.

;; The standard AE extraction region file contains both a polygon for the source region
;;  and a circle for the mask region, plus header lines related to ds9.


;; We want only the polygon line, and we must strip out any leading phrase
;; "physical;" and any trailing phrase "#..." since they seem to sometimes
;; (but not always) confuse CIAO.

PRO ae_ds9_to_ciao_regionfile, ds9_filename, ciao_filename, APPEND=append, $
        IGNORE_BACKGROUND_TAG=ignore_background_tag, MAX_VERTEXES=max_vertexes, $
        POLYGON_X=first_polygon_x, POLYGON_Y=first_polygon_y, REGION_EDITED=region_edited

max_file_length = 32000
num_lines = 0
lines     = strarr(100)
first_polygon_x = 0
first_polygon_y = 0
region_edited = 'F'

if NOT keyword_set(max_vertexes) then max_vertexes = 100

;; Parse ds9 regionfile.
openr, region_unit, ds9_filename, /GET_LUN

    line = ''
    while not eof(region_unit) do begin
      readf, region_unit, line
      
      ;; Unless /APPEND specified, 
      ;; we MUST keep the FIRST line of the file since it specifies the format via the ds9 version number.
      ;; CIAO filtering seems to use the ds9 version number when interpreting the file.
      ;; Let's just keep all the comment lines since they don't seem to bother CIAO.
      if keyword_set(append) AND (strmid(line,0,1) EQ '#') then continue
      
      
      ;; Try to determine if ds9 wrote the file.
      ;; Skip globals, and entries with "background" tag if desired.
      if                                        (strpos(line,'global')     NE -1) then begin
        region_edited = 'T'
        continue
      endif
      
      if keyword_set(ignore_background_tag) AND (strpos(line,'background') NE -1) then continue
      
      ;; Strip off trailing comments
      comment_start = strpos(line,'#')
      if (comment_start GT 0) then line = strmid(line,0,comment_start)
      
      ;; Strip off leading "physical;" tags present in ds9 version 3.
      if (strmid(line,0,9) EQ 'physical;') then line = strmid(line,9)
      
      ;; Save line.
      lines[num_lines] = line
      num_lines = num_lines + 1
    endwhile
free_lun, region_unit

if (num_lines EQ 0) then begin
  print, 'ERROR: '+ds9_filename+' contains no acceptable regions!'
  run_command, 'cat '+ds9_filename, /UNIX
  retall
endif

;; Write CIAO regionfile.
resample_all_polygons = 0
for pass=1,10 do begin
  openw, region_unit, ciao_filename, /GET_LUN, APPEND=keyword_set(append)


  for ii=0,num_lines-1 do begin
    line = lines[ii]
  
    first = strpos(line, 'polygon(')
    if (first EQ -1) then begin
      ; Non-polygons are written as-is.
      printf, region_unit, line
      
    endif else begin
      ; Parse the polygon.
      ; Check for leading '-' or '+'.
      sign_str = strmid(line, 0, first)
      case 1 of
        (strpos(sign_str,'+') NE -1): sign='+'
        (strpos(sign_str,'-') NE -1): sign='-'
        else:                         sign=''
      endcase

      first   = first + 8
      last    = strpos(line, ')', first)
      coords  = strmid(line, first, (last-first))
      numbers = float(strsplit(coords, ',', /EXTRACT))
      ind       = 2*indgen(n_elements(numbers)/2)
      polygon_x = numbers[ind]
      polygon_y = numbers[ind+1]
      
      ; Save the first polygon's coordinates for the caller.
      if NOT keyword_set(first_polygon_x) then begin
        first_polygon_x = polygon_x
        first_polygon_y = polygon_y
      endif

      ; If necessary reduce the number of vertices to reduce file size.
      num_current_points    = n_elements(polygon_x)
      resample_this_polygon = resample_all_polygons
      
      while (resample_this_polygon OR (num_current_points GE 2*max_vertexes) ) AND (num_current_points GE 8) do begin
        num_new_points     = ceil(num_current_points/2.0)
        
       ;print, 'resampling polygon ...'
        first_pt  = 2*indgen(num_new_points)
        second_pt = (first_pt+1) < (num_current_points-1)
        
        polygon_x = (polygon_x[first_pt] + polygon_x[second_pt])/2.0
        polygon_y = (polygon_y[first_pt] + polygon_y[second_pt])/2.0
      
        num_current_points = n_elements(polygon_x)
        resample_this_polygon = 0
      endwhile
      
      ; Polygons are printed with restricted precision to limit file size.
      polygon = fltarr(2,n_elements(polygon_x))
      polygon[0,*] = polygon_x
      polygon[1,*] = polygon_y

      src_region = sign + 'polygon(' + strcompress(strjoin(string(polygon,F='(F8.2)'),","), /REMOVE) + ')'
      printf, region_unit, src_region
      lines[ii] = src_region
    endelse ; is a polygon
  endfor ;ii
  
  filesize = (fstat(region_unit)).cur_ptr
  free_lun, region_unit
  
  if keyword_set(append) OR (filesize LT max_file_length) then break
  resample_all_polygons = 1  
  print, 'Output file is too long; resampling polygons'
endfor ;pass

if (NOT keyword_set(append)) AND (filesize GT max_file_length) then begin
  print
  print, 'ERROR: could not reduce size of region file sufficiently'
  forprint, lines[0:num_lines-1]
  stop
endif 

return
end


; =============================================================================
;; Display a tiled set of data files in ds9, with optional region files for each.

;; First call creates ds9 session using string NAME to name the session, e.g.
;;   ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name
;; If the string OPTION_STRING is supplied, it is passed to ds9, e.g.
;;   OPTION_STRING='-log -bin factor 8'

;; Subsequent calls display one or more data files and optional region files.

;; 
;; If a 2-vector PAN_TO_COORDS is supplied, ds9 will pan to the specified celestial coordinates.

PRO ae_send_to_ds9, my_ds9, NAME=name, OPTION_STRING=option_string, $
      data_fn, region_fn, DESIRED_ZOOM=desired_zoom, MATCH_PHYSICAL=match_physical, $
      PAN_TO_COORDS=pan_to_coords

if ~keyword_set(option_string) then option_string = '-log'

if keyword_set(name) then begin
  name = repchr(name,' ','_')
  run_command, string(name, option_string, F='(%"ds9 -xpa local -tile -title ''%s'' -bin buffersize 8192 %s &")')
  my_ds9 = "DS9:"+name
  
  ; Wait for ds9 to register with XPA.
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  repeat begin
    run_command, string(my_ds9, F='(%"xpaaccess ''%s''")'), result, /IGNORE_STATUS, /QUIET
    if (result[0] EQ 'yes') then break
    print, 'waiting for ds9 to come up...'
    wait,3
  endrep until (0)
  return
endif


if keyword_set(pan_to_coords) then begin
  run_command, string(my_ds9, pan_to_coords, F='(%"xpaset -p ''%s'' pan to %10.6f %10.6f wcs fk5 degrees")'), /QUIET
  return
endif


; Delete existing frames.
count  = n_elements(data_fn)
cmd    = strarr(2+3*count+6)
cmd[0] = string(my_ds9,                              F='(%"xpaset -p ''%s'' frame delete all")')
cmd[1] = string(my_ds9,                              F='(%"xpaset -p ''%s'' update off")')

if (count GT 0) then begin
  ; We create all the frames first in case adding a frame causes lots of computations on
  ; the existing frames.
  for jj = 0, count-1 do $
    cmd[2+jj] = string(my_ds9, 1+jj,                 F='(%"xpaset -p ''%s'' frame %s")')
endif
 
; Load data into each frame.
for jj = 0, count-1 do begin
  cmd[2+count+2*jj  ] = string(my_ds9, 1+jj,         F='(%"xpaset -p ''%s'' frame %s")')
  cmd[2+count+2*jj+1] = string(my_ds9, data_fn[jj],  F='(%"xpaset -p ''%s'' file  ''%s'' ")')
endfor
    
; Zoom the first frame as requested.
if (count GT 0) then $
  cmd[2+3*count  ] = string(my_ds9,                  F='(%"xpaset -p ''%s'' frame 1")')

if keyword_set(desired_zoom) then begin
  cmd[2+3*count+1]   = string(my_ds9, desired_zoom,  F='(%"xpaset -p ''%s'' zoom to %f")')
endif else begin
  cmd[2+3*count+1] = string(my_ds9,                  F='(%"xpaset -p ''%s'' zoom to fit")')
endelse

; Align frames if there's more than one.
if (count GT 0) then begin
  coordsys = keyword_set(match_physical) ? 'physical' : 'wcs'
  cmd[2+3*count+3] = string(my_ds9, coordsys,        F='(%"xpaset -p ''%s'' match frames %s")')
endif

cmd[2+3*count+4] = string(my_ds9,                    F='(%"xpaset -p ''%s'' update on")')

run_command, cmd, /QUIET


;; While the user starts looking at images, overlay regions.
count = n_elements(region_fn) < count
if (count GT 0) then begin
  cmd   = strarr(2*count + 1)
  for jj = 0, count-1 do begin
    if (region_fn[jj] NE '') then begin
      cmd[2*jj  ] = string(my_ds9, 1+jj,             F='(%"xpaset -p ''%s'' frame %d")')
      
      cmd[2*jj+1] = string(my_ds9, region_fn[jj],    F='(%"xpaset -p ''%s'' regions load %s")')
    endif
  endfor

  ;cmd[2*count] = string(my_ds9,                      F='(%"xpaset -p ''%s'' frame 1")')

  run_command, cmd, /QUIET
endif 

return
end

                                                    
; =============================================================================
; Read a source's ARF and RMF files.
; Return the energies of the PI channel boundaries and mid-points.
; Resample several column in the ARF table at the mid-points of the channels:
;   channel_base     = CIAO_ARF     (channel_midenergy)
;   channel_psf_frac = PSF_fraction (channel_midenergy)
;   channel_specresp = effective_ARF(channel_midenergy)

PRO ae_channel_energy_and_arf, rmf_fn, arf_fn, $
        channel_number, channel_lowenergy, channel_highenergy, channel_midenergy, channel_specresp, channel_psf_frac, channel_base

  ; Read the EBOUNDS table in the RMF which defines energy range (keV) for each channel.
  ; Channel numbers start at 1, not zero!
  fits_open, rmf_fn, fcb, /NO_ABORT, MESSAGE=error
  if keyword_set(error) then message, 'ERROR reading ' + rmf_fn
  
  fits_read,  fcb, table, header, /NO_PDU, EXTNAME='EBOUNDS'
  fits_close, fcb
  
  tbinfo, header, tb_str
  channel_number    = tbget(tb_str, table, 'CHANNEL')
  channel_lowenergy = tbget(tb_str, table, 'E_MIN')
  channel_highenergy= tbget(tb_str, table, 'E_MAX')
  channel_midenergy = 0.5 * (channel_lowenergy + channel_highenergy)
  
  ; Free the pointers allocated by tbinfo.
  ptr_free, tb_str.TSCAL, tb_str.TZERO
    
  if keyword_set(arf_fn) then begin
    ; Read the SPECRESP table in the ARF and look up an ARF value corresponding to each channel.
    ; We use linterp to truncate to the end points rather than extrapolating; the range of
    ; the RMF's EBOUNDS table may exceed that of the ARF..
    arf_table = mrdfits(arf_fn, 1, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + arf_fn

    linterp, 0.5*(arf_table.ENERG_LO + arf_table.ENERG_HI), arf_table.SPECRESP, channel_midenergy, channel_specresp
    
    if (total(strmatch(tag_names(arf_table), 'PSF_FRAC')) GT 0) then $
    linterp, 0.5*(arf_table.ENERG_LO + arf_table.ENERG_HI), arf_table.PSF_FRAC, channel_midenergy, channel_psf_frac
    
    if (total(strmatch(tag_names(arf_table), 'BASE')) GT 0) then $
    linterp, 0.5*(arf_table.ENERG_LO + arf_table.ENERG_HI), arf_table.BASE, channel_midenergy, channel_base
  endif else begin
    channel_specresp=0
    channel_psf_frac=0
    channel_base    =0
  endelse
return
end


; =============================================================================
; Radial profile analysis tool.

;;; ASPECT_FN is an aspect file for the observation; required by dmcoords.
; 
;;; Other parameters have defaults that are set up to run this tool in the AE extraction dir for a specific obsid.
; =============================================================================
PRO ae_radial_profile, ASPECT_FN=aspect_fn, RA=ra, DEC=dec, EVENTS_FN=env_events_fn, SRC_RADIUS=src_radius, ENERGY_RANGE=energy_range,$
                       PSF_FN=psf_fn, TEMPDIR=tempdir, $
                       PLOT=make_plot, WIDGET_IDS=plot_ids, PSF_NAME=psf_name, SOURCENAME=sourcename, $
                       ks_psf, R_MEDIAN, EE_AT_RM, VERBOSE=verbose

COMMON run_command


if ~keyword_set(ra) || ~keyword_set(dec) || ~keyword_set(sourcename) then begin
  src_stats = headfits('../source.stats')
  if ~keyword_set(ra)         then ra         = sxpar(src_stats, 'RA')
  if ~keyword_set(dec)        then dec        = sxpar(src_stats, 'DEC')
  if ~keyword_set(sourcename) then sourcename = sxpar(src_stats, 'OBJECT')
endif

if ~keyword_set(src_radius) then begin
  obs_stats = headfits('obs.stats')
  if ~keyword_set(src_radius) then src_radius = sxpar(obs_stats, 'SRC_RAD')
endif

if ~keyword_set(env_events_fn) then env_events_fn = 'neighborhood.evt'
if ~keyword_set(energy_range)  then energy_range = [1,2]
if ~keyword_set(psf_fn)        then psf_fn = 'source.psf'
if ~keyword_set(psf_name)      then psf_name = 'PSF'
if ~keyword_set(verbose)       then verbose = 0

if (n_elements(make_plot) EQ 0) then make_plot=1

if ~keyword_set(tempdir) then begin
  repeat begin
    session_name = string(random()*1E4, F='(I4.4)')
    temproot = 'AE' + session_name +'.noindex/'
    temproot = filepath(temproot, /TMP)
  endrep until (NOT file_test(temproot))
  file_mkdir, temproot
  print, 'Using temporary directory: ', temproot
  tempdir = temproot
endif
if ~keyword_set(ciao_env) then run_command, /INIT, PARAM_DIR=tempdir


inband_events_fn = tempdir + 'temp.inband.evt'
temp_image_fn    = tempdir + 'temp.img'

    ;; Convert RA,DEC to the (x,y) system of the composite event list.
    cmd = string(env_events_fn, aspect_fn, ra, dec,  F="(%'dmcoords %s asolfile=%s opt=cel celfmt=deg ra=%10.6f dec=%10.6f')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords x y', result
    xpos_catalog = float(result[0])
    ypos_catalog = float(result[1])
    

    ;; Extract inband event data in a circular region at the catalog position, reposition the region to the
    ;; mean of the data, and extract again..
    analysis_region = string(xpos_catalog, ypos_catalog, src_radius, F='(%"circle(%f,%f,%f)")')
    cmd = string(env_events_fn, 1000*energy_range, analysis_region, inband_events_fn, $
                 F="(%'dmcopy ""%s[energy=%6.1f:%7.1f,sky=%s]"" %s clobber+')")
    run_command, cmd

    inband_events = mrdfits(inband_events_fn, 1, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + inband_events_fn
    
    xpos_data = mean(inband_events.X)
    ypos_data = mean(inband_events.Y)
    
    analysis_region = string(xpos_data, ypos_data, src_radius, F='(%"circle(%f,%f,%f)")')
    cmd = string(env_events_fn, 1000*energy_range, analysis_region, inband_events_fn, $
                 F="(%'dmcopy ""%s[energy=%6.1f:%7.1f,sky=%s]"" %s clobber+')")
    run_command, cmd

    inband_events = mrdfits(inband_events_fn, 1, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + inband_events_fn
    
    inband_src_counts = n_elements(inband_events)

    ;; Examine the spatial distributions of these event data and of the PSF.
    ks_psf   = !VALUES.F_NAN
    EE_AT_RM = !VALUES.F_NAN
    R_MEDIAN = !VALUES.F_NAN
    if (inband_src_counts LT 4) then begin
      ; We need at least 4 counts for KS probability to be meaningful.
      print, 'Radial profile analysis skipped -- too few in-band counts.'
      
    endif else begin
      ; Compute and sort the distances (in units of skypix) from the events to the source position.
      event_distance = sqrt((inband_events.X-xpos_data)^2 + (inband_events.Y-ypos_data)^2)
      event_distance = event_distance[sort(event_distance)]
      
      ; Read astrometry information about the PSF image.
      full_psf      = readfits(psf_fn, psf_header, /SILENT)

      psf_energy = sxpar(psf_header,'ENERGY')

      crvalP = [sxpar(psf_header, 'CRVAL1P'), sxpar(psf_header, 'CRVAL2P')]
      crpixP = [sxpar(psf_header, 'CRPIX1P'), sxpar(psf_header, 'CRPIX2P')]
      cdeltP = [sxpar(psf_header, 'CDELT1P'), sxpar(psf_header, 'CDELT2P')]
      skypixel_per_psfpixel = cdeltP[0]

      extast, psf_header, psf2wcs_astr

      ;; Extract the PSF in a circular region at the catalog position, reposition the region to the
      ;; centroid of the PSF, and extract again..

      ; Earlier we are _supposed_ to have used the SAME observation's sky system for
      ; both the merged data and the composite PSF, thus we should be able to apply
      ; analysis_region to both.
      ; We use [opt full] to avoid the confusion of having a non-standard PSF FITS 
      ; header and astrometry structure running around.
      analysis_region = string(xpos_catalog, ypos_catalog, src_radius, F='(%"circle(%f,%f,%f)")')
  
      cmd = string(psf_fn, analysis_region, temp_image_fn, $
                 F="(%'dmcopy ""%s[sky=%s][opt full]"" %s clobber+')")
      run_command, cmd
      
      truncated_psf = readfits(temp_image_fn, /SILENT)
  
      ;; Find PSF centroid in image coordinates then convert to sky coordinates.
      ;; We cannot use xy2ad.pro for such conversions to PHYSICAL (sky) system.
      ;; We normalize the PSF image before doing the CENTROID calculation.
      truncated_psf /= total( truncated_psf, /NAN, /DOUBLE )
      
      xdim = (size(truncated_psf, /DIM))[0]
      ydim = (size(truncated_psf, /DIM))[1]

      ind = lindgen(xdim,ydim)
      xpos_psf = total( truncated_psf * (ind mod xdim), /DOUB ) 
      ypos_psf = total( truncated_psf * (ind  /  xdim), /DOUB ) 
  
      xpos_psf = (xpos_psf+1-crpixP[0])*cdeltP[0] + crvalP[0]                            
      ypos_psf = (ypos_psf+1-crpixP[1])*cdeltP[1] + crvalP[1]
      
      analysis_region = string(xpos_psf, ypos_psf, src_radius, F='(%"circle(%f,%f,%f)")')
  
      cmd = string(psf_fn, analysis_region, temp_image_fn, $
                 F="(%'dmcopy ""%s[sky=%s][opt full]"" %s clobber+')")
      run_command, cmd
      
      truncated_psf = readfits(temp_image_fn, /SILENT)
      truncated_psf /= total( truncated_psf, /NAN, /DOUBLE )
           
      xdim = (size(truncated_psf, /DIM))[0]
      ydim = (size(truncated_psf, /DIM))[1]

      if (total([xdim,ydim] - size(full_psf, /DIM)) GT 0) then $
        message, 'ERROR: dmcopy changed the dimensions of the PSF array!'
      
      ; Make an array that has the distances (in units of image pixels) from each PSF pixel to the source.
      ad2xy, ra, dec, psf2wcs_astr, xind_catalog, yind_catalog
      dist_circle, psf_distance, [xdim,ydim], xind_catalog, yind_catalog
      
      ind = where(truncated_psf GT 0, count)
      if (count LT 3) then begin
        print, 'Radial profile analysis skipped -- too few PSF pixels in aperture.'
      endif else begin
        ; Retain only the non-zero pixels; convert the distances to units of skypix.
        truncated_psf_1d = truncated_psf[ind]
        psf_distance_1d  = psf_distance [ind] * skypixel_per_psfpixel
        
        ; Sort by distance to be ready to form a 1-D model.
        sort_ind      = sort(psf_distance_1d)
        psf_distance_1d  = psf_distance_1d [sort_ind]
        truncated_psf_1d = truncated_psf_1d[sort_ind]
    
        ; Form the cumulative distribution function: psf_distn(psf_distance_1d).
        ; This is the 1-D model of the composite PSF.
        psf_distn = total(truncated_psf_1d, /NAN, /DOUBLE, /CUMULATIVE) / total(truncated_psf_1d, /NAN, /DOUBLE)
        
        ; Evaluate the model at the distances corresponding to the event data.
        ; We MUST make sure not to extrapolate in a way that produces values outide of [0,1]
        ; in the common case where the range of the event distances exceeds the range of the
        ; PSF pixel distances.
        ; We could avoid extrapolation completely by using linterp.pro, however extrapolation
        ; from psf_distance_1d[0] to zero gives a better PSF model than clipping to psf_distn[0].
        psf_distn_samples = 0 > interpol([0,psf_distn], [0,psf_distance_1d], event_distance) < 1
        
        ; Now we have samples of the PSF model, psf_distn_samples, evaluated
        ; at each distance (event_distance) where the observed cumulative distribution
        ; takes a setp of 1/inband_src_counts.
        ; Evaluate the KS distance and probablity.
        ; These lines derived from ksone.pro in AstroLib.
        cum_distn_before_step = (  findgen(inband_src_counts)) / inband_src_counts
        cum_distn_after_step  = (1+findgen(inband_src_counts)) / inband_src_counts
  
        ks_distance = max( abs( psf_distn_samples - cum_distn_before_step ) ) > $
                      max( abs( psf_distn_samples - cum_distn_after_step  ) )
        
        prob_ks, ks_distance, inband_src_counts, ks_psf
        
;        ; Calculate the 50% encircled energy radius for the truncated PSF.
;        dum = min(abs(psf_distn - 0.5), imin)
;        R50_PSF = psf_distance_1d[imin]
        
        ; Calculate the 50% encircled energy radius for the extracted data.
        R_MEDIAN = median(event_distance, /EVEN)
        
        ; Find the PSF fraction enclosed by this radius.
        EE_AT_RM = 0 > interpol([0,psf_distn], [0,psf_distance_1d], R_MEDIAN) < 1

        if keyword_set(make_plot) then begin
          if (n_elements(plot_ids) EQ 5) then begin
            id0 = plot_ids[0]
            id1 = plot_ids[1]
            id2 = plot_ids[2]
            id3 = plot_ids[3]
            id4 = plot_ids[4]
          endif
          
          temp_x = fltarr(2*inband_src_counts)
          temp_y = fltarr(2*inband_src_counts)
          ind    = 2*indgen(inband_src_counts)
          temp_x[ind  ] = event_distance
          temp_x[ind+1] = event_distance
          temp_y[ind  ] = cum_distn_before_step
          temp_y[ind+1] = cum_distn_after_step
          function_1d,id0, psf_distance_1d, psf_distn, DATASET=psf_name+' in aperture', TIT=sourcename, XTIT='distance [skypix]', YTIT='encircled fraction'
          function_1d,id0, temp_x,       temp_y,    DATASET='events in aperture', LINE=1, COLOR='red'
          

         
          ; Find the binning that matches the PSF.
          run_command, string(psf_fn, F="(%'get_sky_limits %s verbose=0 precision=2')")
          run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec
        
          ; Change from "xmin:xmax:#bins" syntax to "xmin:xmax:delx" syntax to ensure that 
          ; the image has square pixels to keep CIAO happy.
          tokens = strsplit(filterspec, '=:#,', /EXTRACT)
          xmin  = float(tokens[1])
          xmax  = float(tokens[2])
          xbins = float(tokens[3])
          ymin  = float(tokens[5])
          ymax  = float(tokens[6])
          binsize = (xmax-xmin)/xbins
          filterspec = string(xmin,xmax,binsize,ymin,ymax,binsize, F='(%"x=%7.2f:%7.2f:%7.4f,y=%7.2f:%7.2f:%7.4f")')
          
          ; Bin up the extracted events into an image and normalize.
          cmd = string(inband_events_fn, filterspec, temp_image_fn, F="(%'dmcopy ""%s[bin %s]"" %s clobber+')")
          run_command, cmd
          truncated_img      = readfits(temp_image_fn, /SILENT)
          norm               = total( truncated_img, /NAN, /DOUBLE )
                      
          xcoord = (indgen((size(truncated_img, /DIM))[0])+1-crpixP[0])*cdeltP[0] + crvalP[0]
          ycoord = (indgen((size(truncated_img, /DIM))[1])+1-crpixP[1])*cdeltP[1] + crvalP[1]
            
         ;s                 = 1./max(total(truncated_psf,2))
          s=1
          function_1d,id1, xcoord, s*total(truncated_psf,2),      PSYM=4, LINE=6, TITLE=sourcename, DATASET=psf_name+' in aperture', XTIT='sky X', YTIT='fraction'
          function_1d,id1, xcoord, s*total(truncated_img,2)/norm, PSYM=1, LINE=6, TITLE=sourcename, DATASET='events in aperture', COLOR='red', ERROR=s*(1 + sqrt(total(truncated_img,2)+0.75))/norm
                                                                                                                            
         ;s                 = 1./max(total(truncated_psf,1))
          s=1
          function_1d,id2, ycoord, s*total(truncated_psf,1),      PSYM=4, LINE=6, TITLE=sourcename, DATASET=psf_name+' in aperture', XTIT='sky Y', YTIT='fraction'
          function_1d,id2, ycoord, s*total(truncated_img,1)/norm, PSYM=1, LINE=6, TITLE=sourcename, DATASET='events in aperture', COLOR='red', ERROR=s*(1 + sqrt(total(truncated_img,1)+0.75))/norm
                                                              
          dataset_1d,id3, inband_events.X, DENSITY_TITLE=sourcename, DATASET='events in aperture', XTITLE='sky X'
          dataset_1d,id4, inband_events.Y, DENSITY_TITLE=sourcename, DATASET='events in aperture', XTITLE='sky Y'
          plot_ids = [id0,id1,id2,id3,id4]

          print
          print, xpos_catalog, ypos_catalog, F='(%"catalog position: %0.2f %0.2f")'
          print, xpos_psf,     ypos_psf,     F='(%"PSF centroid    : %0.2f %0.2f")'
          print, xpos_data,    ypos_data,    F='(%"mean data       : %0.2f %0.2f")'
          help, ks_psf, ks_distance, inband_src_counts, R_MEDIAN, EE_AT_RM
          
          print, psf_energy, energy_range, F='(%"PSF monoenergy is %0.2f keV; event energy range is [%0.2f:%0.2f] keV")'
         endif      
      endelse
    
      if (psf_energy LT energy_range[0]) || (psf_energy GT energy_range[1]) then print, 'WARNING!  PSF monoenergy is outside the event energy range!!'
    
    endelse
    file_delete, inband_events_fn, temp_image_fn, /ALLOW_NONEXISTENT

    if keyword_set(temproot) && file_test(temproot) then begin
      list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
      if (count GT 0) then file_delete, list
      file_delete, temproot
    endif
    return
    end
    
    
; =============================================================================
;;; Routine to apply AE's grouping algorithm (see manual).
;;;
;;; To have the algorithm ignore background set bkg_spectrum_fn=''.
;;; If grouped_spectrum_fn is null a grouped filename will be constructed and returned.
;;; See manual for meaning of CHANNEL_RANGE, SNR_RANGE, NUM_GROUPS_RANGE.
;;; The parameters this_snr_goal, grp_name, group_codes, num_groups, inband_src_counts
;;; return information about the grouping that AE's /FIT_SPECTRA stage needs.
PRO ae_group_spectrum, src_spectrum_fn, bkg_spectrum_fn, grouped_spectrum_fn, $
                       CHANNEL_RANGE=channel_range, $
                       SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                       CREATOR_STRING=creator_string, $
                       this_snr_goal, grp_name, channel_starting_group, num_groups, inband_src_counts, inband_scaled_bkg_counts 

if NOT keyword_set(creator_string) then creator_string = "ae_group_spectrum, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)

if (n_elements(channel_range) NE 2) then channel_range=[35,548]
min_channel = fix(channel_range[0])
max_channel = fix(channel_range[1])
 
; SNR_RANGE[1] is the user's goal for defining groups; SNR_RANGE[0] is the lower limit allowed before we abort the grouping attempt
if (n_elements(snr_range) EQ 0) then $
  snr_range = [1,3]
if (n_elements(snr_range) NE 2) then begin
  print, 'ERROR: keyword SNR_RANGE should be a 2-element vector giving the range of SNR allowed for each spectral group, e.g. [2.5,5].'
  return      
endif

if (snr_range[1] LT 0) then begin
  print, 'ERROR: minimum SNR value (SNR_RANGE[1]) must be positive'
  return
endif

if (n_elements(num_groups_range) EQ 0) then $
  num_groups_range = [10,250]
if (n_elements(num_groups_range) NE 2) then begin
  print, 'ERROR: keyword NUM_GROUPS_RANGE should be a 2-element vector specifying how many spectral groups are desired, e.g. [10,250].'
  return    
endif


;; ------------------------------------------------------------------------
;; Read the source & background spectra.
pheader   = headfits(src_spectrum_fn)
bin_table = mrdfits( src_spectrum_fn, 1, src_theader, /SILENT, STATUS=status)
if (status NE 0) then message, 'ERROR reading ' + src_spectrum_fn

src_channels        = bin_table.CHANNEL 
src_observed_counts = bin_table.COUNTS 
num_channels        = n_elements(src_channels)

if keyword_set(bkg_spectrum_fn) then begin
  ; This code does not handle every valid OGIP configuration.
  src_areascal        = float(sxpar(src_theader, 'AREASCAL'))
  if (src_areascal NE 1) then message, 'ERROR: expected AREASCAL=1 in src spectrum.'
  
  src_backscal        = float(sxpar(src_theader, 'BACKSCAL'))
  if (src_backscal EQ 0) then message, 'ERROR: BACKSCAL keyword missing from src spectrum.'
  
  bin_table = mrdfits( bkg_spectrum_fn, 1, bkg_theader, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + bkg_spectrum_fn

  bkg_observed_counts = bin_table.COUNTS
  
  bkg_areascal        = float(sxpar(bkg_theader, 'AREASCAL', COUNT=count))
  if (count EQ 0) then $
    bkg_areascal      = bin_table.AREASCAL
  
  bkg_backscal        = float(sxpar(bkg_theader, 'BACKSCAL', COUNT=count))
  if (count EQ 0) then $
    bkg_backscal      = bin_table.BACKSCAL
  
  if (bkg_areascal GT 0) && (bkg_backscal GT 0) then begin  
    bkg_counts_in_src_region = (src_backscal/bkg_backscal) * (bkg_observed_counts / bkg_areascal)
  endif else begin
    if (total(bkg_observed_counts) EQ 0) then begin
      ; This looks like an empty background spectrum.
      bkg_counts_in_src_region = replicate(0.0, n_elements(bkg_observed_counts))
    endif else message, 'ERROR: '+bkg_spectrum_fn+'has data but either BACKSCAL or AREASCAL are missing!'
  endelse
endif else begin
  bkg_observed_counts      = 0
  bkg_counts_in_src_region = 0
endelse

group_bins_to_snr, src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, $
                   GROUP_WITHOUT_BACKGROUND=(keyword_set(bkg_spectrum_fn) EQ 0), $
                   START_INDEX=value_locate(src_channels, min_channel), STOP_INDEX=value_locate(src_channels, max_channel), $
                   SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                   this_snr_goal, group_codes

grp_name = strcompress(string(this_snr_goal, F='(%"grp%8.1f")'),/REMOVE_ALL)

channel_starting_group = src_channels[where(group_codes EQ 1, num_groups)]

fdecomp, src_spectrum_fn, disk, item_path, item_name, item_qual

if (NOT keyword_set(grouped_spectrum_fn)) then $
  grouped_spectrum_fn = item_path + item_name + '_' + grp_name + (('' EQ item_qual) ? '' : ('.' +item_qual))


;; ------------------------------------------------------------------------
;; Write the grouped source spectrum.
if (grouped_spectrum_fn NE '/dev/null') then begin
  fxaddpar, pheader, 'CREATOR', creator_string
  fxaddpar, pheader, "FNFITS", file_basename(grouped_spectrum_fn)
  
  writefits, grouped_spectrum_fn, 0, pheader
  
  row = { CHANNEL: 0, COUNTS: 0L, GROUPING:0 }
  bin_table = replicate(row, num_channels)
  bin_table.CHANNEL = src_channels 
  bin_table.COUNTS  = src_observed_counts
  bin_table.GROUPING= group_codes
  fxaddpar, src_theader, 'CREATOR',  creator_string
  fxaddpar, src_theader, 'SNR_GOAL', this_snr_goal
  fxaddpar, src_theader, 'NUMGRPS', num_groups
  sxdelpar, src_theader, 'GROUPING'
  mwrfits, bin_table, grouped_spectrum_fn, src_theader
  print, num_groups, this_snr_goal, file_basename(grouped_spectrum_fn), F='(%"Spectrum with %d groups (SNR=%0.1f) written to %s")'
endif

ind = where((src_channels GE min_channel) AND (src_channels LE max_channel))
inband_src_counts        = total(/INT,      src_observed_counts[ind])
inband_scaled_bkg_counts = total(      bkg_counts_in_src_region[ind])
return
end ; ae_group_spectrum



; =============================================================================
;;; Using an XSPEC script constructed by AE, call XSPEC to perform a fit and manage the data products produced.
;;;
;;; * sourcedir is the source extraction directory, e.g. '181948.97-161633.2'
;;;
;;; * fit_result_root is a name for this fitting session---a combination of the 
;;;   grouping scheme and fitting script name(s), e.g. 'nogrp_tbabs_vapec_A'.
;;;   The fitting script is expected to be at the path 
;;;     'spectral_models/' + fit_result_root + '.xcm'
;;;
;;; * By default, the call to XSPEC arranges for the XSPEC screen output to appear in the IDL window, in case 
;;;   your script uses the "interactive" option.  If you are sure it does not, then you can specify
;;;   INTERACTIVE=0 to hide the XSPEC screen output.
;;;
;;; * FIT_TIMEOUT sets a CPU time limit (in seconds) on XSPEC.

PRO ae_perform_fit, sourcedir, fit_result_root, INTERACTIVE=interactive, TEMPDIR=tempdir, FIT_TIMEOUT=fit_timeout


COMMON run_command


if ~keyword_set(tempdir) then begin
  repeat begin
    session_name = string(random()*1E4, F='(I4.4)')
    temproot = 'AE' + session_name +'.noindex/'
    temproot = filepath(temproot, /TMP)
  endrep until (NOT file_test(temproot))
  file_mkdir, temproot
  print, 'Using temporary directory: ', temproot
  tempdir = temproot
endif
if ~keyword_set(ciao_env) then run_command, /INIT, PARAM_DIR=tempdir


if (strmid(sourcedir,0,/REVERSE) NE '/') then sourcedir += '/'

if (n_elements(interactive) EQ 0) then interactive = 1
if ~keyword_set(fit_timeout)      then fit_timeout = 600

;; These two directory names must be identically defined in the AE code.
modelsubdir             = 'spectral_models/'
fit_stats_basename      = 'source.spectra'

;; These file names must be identically defined in the fitting scripts.
latex_result_fn         = 'summary.ps'
latex_figure_file       = 'ldata.ps'

; Locate the LaTex template file.
latex_template_basename = 'xspec_template'
result = routine_info( 'acis_extract', /SOURCE )
fdecomp, result.PATH, disk, codedir
latex_template_fn       = codedir + latex_template_basename + '.tex'

;; Remove all the files that XSPEC should make, both in the directory where 
;; XSPEC is run and in the directory where AE moves them.
fit_xcm_fn     =             modelsubdir + fit_result_root + '.xcm'
fit_result_dir = sourcedir + modelsubdir + fit_result_root + '/'
file_mkdir, fit_result_dir

output_file_list = ['icounts.ps', 'ldata.ps', 'xspec_run.log', 'model.xcm','model_before_errors.xcm','model.xspecsav', 'model.fits', 'xspec.log', 'summary.ps']
file_delete, sourcedir     +output_file_list, /ALLOW_NONEXISTENT
file_delete, fit_result_dir+output_file_list, /ALLOW_NONEXISTENT

;; Remove the HDU in source.spectra that XSPEC will be writing to.
stats_fn  = sourcedir + fit_stats_basename 
if file_test(stats_fn) then begin
  fits_open, stats_fn, fcb, /NO_ABORT, MESSAGE=error
  if keyword_set(error) then message, 'ERROR opening '+stats_fn
  fits_close, fcb
  
  ; Find the HDU matching the specified model name.
  ind = where( strmatch(fcb.EXTNAME, fit_result_root, /FOLD_CASE), count )
  
  if (count GT 1) then message, 'ERROR: '+stats_fn+' has multiple HDUs named '+fit_result_root
  if (count EQ 1) then begin
    cmd = string(stats_fn, fit_result_root, F="(%'fdelhdu ""%s[%s]"" N Y')")
    run_command, /HEASOFT, cmd
  endif
endif ;file_test(stats_fn)


;; Run xspec.  We pass "tclexit" to stdin to get xspec to die if it has an
;; error that brings up the xspec prompt.
run_log = fit_result_dir+'xspec_run.log'
if keyword_set(interactive) then begin
  run_command, /HEASOFT, DIRECTORY=sourcedir, STATUS=status, /INTERACTIVE, string(fit_xcm_fn, F='(%"xspec - %s |& tee ./xspec_run.log")')
endif else begin
  cmd = string(fit_timeout, fit_xcm_fn, F='(%"limit cputime %d; echo tclexit 99 | xspec - %s >&! ./xspec_run.log")')
  if strmatch(getenv("SHELL"), "*csh") then cmd = 'set time=(0 "CPU seconds: %U + %S  elapsed time: %E"); '+cmd
  run_command, /HEASOFT, DIRECTORY=sourcedir, STATUS=status, cmd
endelse

; "Exit codes in the range 129-255 represent jobs terminated by Unix
; "signals". Each type of signal has a number, and what's reported as the job
; exit code is the signal number plus 128. Signals can arise from within the
; process itself (as for SEGV, see below) or be sent to the process by some
; external agent (such as the batch control system, or your using the "bkill"
; command). 
if keyword_set(status) then begin      
  case status of
    ; This is an exit code from the AE fitting scripts.
    97 : print,                     'ERROR:!  XSPEC script was unable to recognize your computing platform (operating system plus processor hardware) and thus did not know where to find the shared library containing the "cplinear" model compiled for your platform .  You may have to add an entry to the "switch" statment in the fitting script, and build the cplinear library yourself (a simple process shown in the AE manual).'
    ; This is an exit code from the AE fitting scripts.
    98 : print, run_log,              F='(%"WARNING!  Examine XSPEC session log %s; XSPEC script detected an error.")'
    ; This is from the tclexit command in the pipe above.
    99 : print, run_log,              F='(%"WARNING!  Examine XSPEC session log %s; XSPEC script execution aborted.")'
    ; signal SIGXCPU (BSD)
    152: print, run_log, fit_timeout, F='(%"WARNING!  Examine XSPEC session log %s; XSPEC process killed after consuming %d CPU seconds.")'
    ; ???
    158: print, run_log, fit_timeout, F='(%"WARNING!  Examine XSPEC session log %s; XSPEC process killed after consuming %d CPU seconds.")'
    
    else:print, run_log, status,      F='(%"WARNING!  Examine XSPEC session log %s; XSPEC process failed for unknown reason (exit code %d).")'
  endcase
endif else begin
  ; Save the model.
  if (NOT file_test(sourcedir+'model.fits')) then begin
    print, run_log, F='(%"WARNING!  Examine XSPEC session log %s; no model saved by XSPEC.")'
  endif else if file_test(stats_fn) then begin
    cmd = string(sourcedir+'model.fits', stats_fn, F="(%'fappend ""%s[1]"" %s history=no')")
    run_command, /HEASOFT, cmd
  endif else file_copy, sourcedir+'model.fits', stats_fn

  ; Use LaTeX to make a summary document.
  if file_test(sourcedir+latex_figure_file) then begin
    ;; Run latex
    cmd = [string(latex_template_fn, F='(%"latex %s >&! ./latex_run.log")'), $
           string(latex_template_basename, latex_result_fn, F='(%"dvips %s -q -o %s")')]
    run_command, /UNIX, DIRECTORY=sourcedir, cmd
    
    file_delete, sourcedir + ['latex_run.log','xspec_template.aux', 'xspec_template.dvi', 'xspec_template.log'], /ALLOW_NONEXISTENT
  endif else begin
    print, run_log, F='(%"WARNING!  Examine XSPEC session log %s; no plot produced by XSPEC.")'
  endelse
endelse ; XSPEC status ok

;; Move all output files to a subdirectory.
ind = where(file_test(sourcedir+output_file_list), count)
if (count GT 0) then $
  file_move, /OVERWRITE, sourcedir+output_file_list[ind], fit_result_dir 

if keyword_set(temproot) && file_test(temproot) then begin
  list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, temproot
endif

return
end






; =============================================================================
; Tool to make PSF images using MARX
; =============================================================================
;Are these PSF headers handled properly in /MERGE?

;;;
;;; Call once to initialize:
;;;  ae_make_psf, EVENT_FILE='source.evt', ASPECT_FN='obs.asol'
;;;
;;; Required keywords:
;;; EVENT_FILE is any event file that can provide MARX with the geometry of the observation.
;;;
;;; ASPECT_FN is an aspect file for the observation; will be provided to MARX for dither and is used by dmcoords.
;;;
;;; Optional keywords:
;;; ASPECT_BLUR is expected PSF blur due to aspect reconstruction errors (arcsec).
;;;
;;; TEMP_DIR is path to a temporary directory, including trailing slash.  Default is /tmp/.
;;;
;;; S_AIMPOINT should be set if the aimpoint of the observation is S3.
;;;
;;; PIPELINE_RANDOMIZATION should be set if the event positions include pipeline randomization.

;;; 
;;; Call again and again to create PSFs:
;;;  ae_make_psf, psf_fn, skypixel_per_psfpixel, footprint, psf_energy, desired_psf_counts, ra, dec

;;; skypixel_per_psfpixel should be carefully chosen so that an odd number of PSF pixels equals one ACIS pixel 
;;; in order to get accurate boxcar smoothing below.
;;;
;;; footprint (skypixels) is desired dimension of square PSF image..
;;;
;;; psf_energy (keV) and desired_psf_counts can be vectors.
;;;
;;; ra, dec is celestial position of source.
;;;
;;; X_CAT,Y_CAT (skypixels) is position of source in SKY system.  Will be computed if not passed.
;;;
;;; OFF_ANGLE is off-axis angle in arcmin.   Will be computed if not passed.
;;;
;;; CHIP_ID is CCD_ID mostly under the source.  Will be computed if not passed.

;;; EMAP_VAL is the level of the emap at the source position, assumed to be s*cm^2 at 1.0 keV.  
;;; Typical on-axis value will be used if not passed.


PRO ae_make_psf, EVENT_FILE=obsdata_filename_p, ASPECT_FN=aspect_fn_p, ASPECT_BLUR=aspect_blur_p, TEMP_DIR=tempdir, $
                 S_AIMPOINT=s_aimpoint_p, PIPELINE_RANDOMIZATION=pipeline_randomization_p, $
                 
                 psf_fn, skypixel_per_psfpixel, footprint, psf_energy, desired_psf_counts, ra, dec, $
                 X_CAT=x_cat, Y_CAT=y_cat, OFF_ANGLE=off_angle, CHIP_ID=chip_id, EMAP_VAL=emap_val, SAOSACFile=SAOSACFile
                  

COMMON run_command
COMMON ae_make_psf, marx_parameter_file, obsdata_filename, exposure, gratingtype, EFFICIENCY_TABLE, aspect_fn, aspect_blur, s_aimpoint, pipeline_randomization, sim_x_offset, sim_y_offset, sim_z_offset

creator_string = "ae_make_psf, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)



arcsec_per_ACISpixel = 0.492 
arcsec_per_skypixel  = 0.492 


; If not passed, create a scratch directory.
tempdir_supplied_by_caller = keyword_set(tempdir)
if ~tempdir_supplied_by_caller then begin
  repeat begin
    session_name = string(random()*1E4, F='(I4.4)')
    temproot = 'ae_make_psf' + session_name +'.noindex/'
    temproot = filepath(temproot, /TMP)
  endrep until (NOT file_test(temproot))
  file_mkdir, temproot
  print, 'Using temporary directory: ', temproot
  tempdir = temproot
endif
    
if keyword_set(obsdata_filename_p) then begin
    ; Initialize the run_command system if necessary.
    if ~keyword_set(ciao_env) then run_command, /INIT, PARAM_DIR=tempdir

    run_command, /QUIET, 'which marx', STATUS=status
    if keyword_set(status) then begin      
      print, 'ERROR: The MARX tools do not seem to be in your unix path!'
      retall
    endif
    
    print, creator_string + ', configuring MARX ...'

    obsdata_filename = obsdata_filename_p
    if keyword_set(aspect_fn_p) then aspect_fn = aspect_fn_p $
                                else message, 'ERROR: must supply aspect file via ASPECT_FN.'

    obsdata_header = headfits(obsdata_filename, EXT=1, ERRMSG=error )
    if (keyword_set(error)) then message, 'ERROR reading ' + obsdata_filename

    pipeline_randomization = keyword_set(pipeline_randomization_p)
    if ~keyword_set(aspect_blur_p) then aspect_blur_p = 0.07   ;arcseconds 
    aspect_blur            = aspect_blur_p
    
    rand_sky = sxpar(obsdata_header,'RAND_SKY')
    case rand_sky of
      0.0 : if  pipeline_randomization then print, 'WARNING! PIPELINE_RANDOMIZATION parameter and RAND_SKY FITS keyword are not consistent!'
      0.5 : if ~pipeline_randomization then print, 'WARNING! PIPELINE_RANDOMIZATION parameter and RAND_SKY FITS keyword are not consistent!'
      else: print, 'WARNING!  The event data are missing the RAND_SKY FITS keyword!'
    endcase
    
    exposure    = sxpar(obsdata_header,'EXPOSURE')
    gratingtype = strtrim(sxpar(obsdata_header,'GRATING'),2)
    if ((strmatch(gratingtype,'HETG') || strmatch(gratingtype,'LETG')) && (n_elements(s_aimpoint_p) EQ 0)) then s_aimpoint_p=1
    s_aimpoint = keyword_set(s_aimpoint_p)
    
    ;; ------------------------------------------------------------------------
    ;; Account for SIM offsets used in the observation, 
    ;; This algorithm was developed by Peter Maksym (pmaksym@northwestern.edu) in 2008.
    
    ;;Get base SIM X,Y,Z offsets from event file, calculate offset from MARX detinfo values
    SIM_X = float(sxpar(obsdata_header, 'SIM_X'))
    SIM_Y = float(sxpar(obsdata_header, 'SIM_Y'))
    SIM_Z = float(sxpar(obsdata_header, 'SIM_Z'))
    
    detector_type = (keyword_set(s_aimpoint) ? 'ACIS-S' : 'ACIS-I')
    run_command, 'detinfo '+detector_type+' | grep STF-STT', line, /QUIET
    reads, (stregex(line,'.*\((.*)\)',/sub,/ext))[1], nom_x_offset, nom_y_offset, nom_z_offset  
    
    sim_x_offset = SIM_X - nom_x_offset
    sim_y_offset = SIM_Y - nom_y_offset
    sim_z_offset = SIM_Z - nom_z_offset 

    print, sim_x_offset, sim_y_offset, sim_z_offset, F='(%"Your SIM position was offset from the MARX default by (%0.4f, %0.4f, %0.4f) mm.")'
    
    

    ;; We can NOT use DitherModel=NONE because that puts the source at the wrong sky coordinates (bug in MARX).
    ;;
    ;; To get correct falloff of the PSF at the chip edges we must have MARX dither with the observation's aspect file.
    ;; The important effect seems to be non-zero values of dy and dz in the aspect file (fid light motion?).
    ;;
    ;; From email conversations with John Davis, it appears that marx2fits does NOT blur the PSF when 
    ;; DitherModel=INTERNAL and DitherBlur=0, i.e. SKY coordinates are NOT computed from quantized CHIP coordinates, 
    ;; and no noise is added to the SKY coordinates.
    ;;
    ;; The best model of ACIS pixelization would be to dither the source, compute quantized CHIP coordinates, 
    ;; compute an aspect solution, and use APE to compute SKY coordinates from the noisy CHIP coordinates.
    ;; Here, however, we are convolving with a boxcar in image space.  The downside is that this boxcar is rotated
    ;; with respect to the CHIP coordinates where the uniform noise is actually occurring.
    ;; The upside is that smoothing in image space leads to a smoother PSF image more suited to contouring.
    ;;
    ;; For the purposes of image reconstruction, it would be good to let MARX include the ACIS readout streak, since
    ;; a real point source has that feature.  However, for the purpose of aperture correction we must NOT simulate the
    ;; streak because the mission has already accounted for the streak via the DTCOR mechanism.
    ;; NOTE that the choice of simulating the streak or not has an effect on the energy dependence of the 
    ;; aperture correction, via the CROPFRAC calculation!  It is not obvious what assumptions about the streak are
    ;; built into the HMRA and ACIS QE calibrations!

    ; Look in several places for marx.par, which has moved between MARX versions
    marx_data_dir = getenv('MARX_DATA_DIR')
    if (marx_data_dir EQ '') then begin
      print, 'ERROR: MARX is not configured ($MARX_DATA_DIR is not defined).'
      retall                          
    endif
    
    marx_parameter_file = tempdir + 'marx.par'
    parfile_template    = marx_data_dir + ['/../par/marx.par','/../../share/marx/pfiles/marx.par','/../pfiles/marx.par','/../../marx.par']
    for ii = 0, n_elements(parfile_template)-1 do begin
      if file_test(parfile_template[ii]) then begin
        file_copy, /OVERWRITE, parfile_template[ii], marx_parameter_file, /VERBOSE
        break
      endif
    endfor                                                      
    if (ii EQ n_elements(parfile_template)) then begin
      print, 'ERROR: cannot find marx.par in '+marx_data_dir
      retall
    endif
  
    marx_cmd   = strarr(13)
    marx_cmd[ 0] = 'pset marx Verbose=yes'
    marx_cmd[ 1] = 'pset marx DitherModel=FILE'
    marx_cmd[ 2] = 'pset marx DitherFile='+aspect_fn
    marx_cmd[ 3] = 'pset marx SourceType=POINT'
    marx_cmd[ 4] = 'pset marx SpectrumType=FLAT'
    marx_cmd[ 5] = 'pset marx TStart='   +string(sxpar(obsdata_header,'TSTART')   , F='(%"%0.1f")')
    marx_cmd[ 6] = 'pset marx RA_Nom='   +string(sxpar(obsdata_header,'RA_NOM')   , F='(%"%0.6f")')
    marx_cmd[ 7] = 'pset marx Dec_Nom='  +string(sxpar(obsdata_header,'DEC_NOM')  , F='(%"%0.6f")')
    marx_cmd[ 8] = 'pset marx Roll_Nom=' +string(sxpar(obsdata_header,'ROLL_NOM') , F='(%"%0.6f")')
    marx_cmd[ 9] = 'pset marx GratingType=' +gratingtype                                         
    marx_cmd[10] = 'pset marx ExposureTime=' +string(exposure , F='(%"%d")')
    marx_cmd[11] = 'pset marx DetIdeal=yes'
    marx_cmd[12] = 'pset marx ACIS_Frame_Transfer_Time=0'

    run_command, marx_cmd
                      
    ; These MARX efficiency values come from simulating an on-axis ACIS-I source.
    EFFICIENCY_TABLE = {EFFICIENCY_TABLE, energy:0.0, QE:0.0}
    EFFICIENCY_TABLE = [{EFFICIENCY_TABLE,0.2770 , 0.362},$
                        {EFFICIENCY_TABLE,1.0    , 0.346},$ 
                        {EFFICIENCY_TABLE,1.49670, 0.350},$ 
                        {EFFICIENCY_TABLE,4.510  , 0.176},$ 
                        {EFFICIENCY_TABLE,6.40   , 0.104},$ 
                        {EFFICIENCY_TABLE,8.60   , 0.029}]
    return
endif


;; ==================================================================================
;; Perform MARX simulations to build PSF images.
temp_image_fn        = tempdir + 'temp.img'
marxdir              = tempdir + 'marx'
marx_events_filename = tempdir + 'marx.evt' 

if ((n_elements(x_cat) EQ 0) || (n_elements(y_cat) EQ 0) || (n_elements(off_angle) EQ 0) || (n_elements(chip_id) EQ 0)) then begin
  ;; Convert RA,DEC to the (x,y) system of this obsid.
  cmd = string(obsdata_filename, aspect_fn, ra, dec,  F="(%'dmcoords %s asolfile=%s opt=cel celfmt=deg ra=%10.6f dec=%10.6f')")
  run_command, cmd
  
  run_command, 'pget dmcoords x y theta chip_id', result
  x_cat        = float(result[0])
  y_cat        = float(result[1])
  off_angle    = float(result[2])  ; arcmin
  chip_id      = fix  (result[3])
endif

if ~keyword_set(emap_val) then emap_val = exposure * 361.0

;; In Dec 2007 I used MARX simulations at 1.5 keV with the readout streak disabled 
;; to measure PSF fractions at 1.5 keV as a function of off-axis angle.  
;; These polynomial curves were fit to those measurements.
;; The off-axis angle off_angle is in arcminutes.
radius50 = (0.85 -0.25*off_angle + 0.10*off_angle^2) * arcsec_per_skypixel  ; arcseconds

; Remove any existing psf_fn before trying to write to it, since it might be a symlink!
file_delete, psf_fn, /ALLOW_NONEXISTENT

num_energies  = n_elements(psf_energy)
crop_fraction = fltarr(num_energies)
num_counts    = fltarr(num_energies)

for jj=0, num_energies-1 do begin     
    ;; Implement MARX workaround for sources on CCDs that are not associated with the aimpoint instrument.
    ;;Values in mm taken from MARX FAQ May 10, 2008  
    if (~keyword_set(s_aimpoint) && (chip_id GT 3)) then begin
        ; S-array CCD with ACIS-I
        detector_type = string('ACIS-S')
        x_offset = sim_x_offset - 0.0990
        y_offset = sim_y_offset
        z_offset = sim_z_offset - 43.4590
    endif else if (keyword_set(s_aimpoint) && (chip_id LT 4)) then begin
        ; I-array CCD with ACIS-S
        detector_type = string('ACIS-I')
        x_offset = sim_x_offset + 0.0990
        y_offset = sim_y_offset
        z_offset = sim_z_offset + 43.4590
    endif else begin
        ; CCD is on the aimpoint detector, i.e. nominal MARX simulation.
        detector_type = (keyword_set(s_aimpoint) ? 'ACIS-S' : 'ACIS-I')
        x_offset = sim_x_offset
        y_offset = sim_y_offset
        z_offset = sim_z_offset
    endelse


  ; We must have a calibration table to estimate the emap value at the requested energy.
  ; The 2.2 factor below is required because emap_val is for HRMA+ACIS but the flux we're giving MARX is for HRMA alone (DetIdeal=yes).
  linterp, EFFICIENCY_TABLE.energy, EFFICIENCY_TABLE.QE, psf_energy[jj], QE
  
  this_emap = (emap_val*2.2) * (QE / EFFICIENCY_TABLE[1].QE)

  file_mkdir, marxdir
  
  if keyword_set(SAOSACFile) then begin
    cmd1 = string(marx_parameter_file, marxdir, ra, dec, detector_type, x_offset, y_offset, z_offset, SAOSACFile[jj], marxdir, $
            F='(%"marx @@%s OutputDir==%s SourceRA=%0.6f SourceDEC=%0.6f DetectorType=%s DetOffsetX=%0.4f DetOffsetY=%0.4f DetOffsetZ=%0.4f DitherModel=INTERNAL DitherBlur=0 SourceType=SAOSAC SAOSAC_Color_Rays=no SAOSACFile=%s ExposureTime=0 >! %s/marx.log")' )
  endif else begin
    flux = desired_psf_counts[jj] / this_emap
    
    cmd1 = string(marx_parameter_file, marxdir, ra, dec, detector_type, x_offset, y_offset, z_offset, psf_energy[jj], psf_energy[jj], flux, marxdir, $
            F='(%"marx @@%s OutputDir=%s SourceRA=%0.6f SourceDEC=%0.6f DetectorType=%s DetOffsetX=%0.4f DetOffsetY=%0.4f DetOffsetZ=%0.4f MinEnergy=%0.4f MaxEnergy=%0.4f DitherBlur=0 SourceFlux=%0.4f >! %s/marx.log")' )
  endelse
  
  cmd2 = string(marxdir, marx_events_filename, marxdir, F='(%"marx2fits %s %s >>! %s/marx.log")' )
  run_command, [cmd1,cmd2]
  
  ; Devise a normalization for the cropped PSF image by counting the total events detected (in 0th order when gratings are used).
  if (strmatch(gratingtype,'HETG') || strmatch(gratingtype,'LETG')) then begin
    bt = mrdfits(marx_events_filename, 1, sim_header)
    num_counts[jj] = total(/INT, bt.order EQ 0)
  endif else begin
    sim_header = headfits(marx_events_filename, EXT=1, ERRMSG=error )
    if (keyword_set(error)) then message, 'ERROR reading ' + marx_events_filename
    
    num_counts[jj] = sxpar(sim_header, 'NAXIS2')
  endelse
  
  ; Use the syntax to "xmin:xmax:delx" syntax to ensure that the image has square pixels to keep CIAO happy.
  ; Place the source position at the center of the central pixel in the PSF image.
  psf_half_dim = ceil(0.5*footprint/skypixel_per_psfpixel)
  
  xmin = x_cat - skypixel_per_psfpixel * (  psf_half_dim + 0.5)
  ymin = y_cat - skypixel_per_psfpixel * (  psf_half_dim + 0.5)
  xmax = xmin  + skypixel_per_psfpixel * (2*psf_half_dim + 0.999)
  ymax = ymin  + skypixel_per_psfpixel * (2*psf_half_dim + 0.999)
  
  filterspec = string(xmin,xmax,skypixel_per_psfpixel,ymin,ymax,skypixel_per_psfpixel, F='(%"x=%0.4f:%0.4f:%0.6f,y=%0.4f:%0.4f:%0.6f")')
  
  cmd = string(marx_events_filename, filterspec, temp_image_fn, F="(%'dmcopy ""%s[bin %s][opt type=i4]"" %s clobber+')")
  run_command, cmd

  ;; Read the PSF image and the keywords specifying the transformation between pixel indexes
  ;; and physical coordinates (x,y).      
  psf_img = float(readfits(temp_image_fn, psf_header, /SILENT))
  arcsec_per_PSFpixel = 3600 * sxpar(psf_header, 'CDELT2')
  
  ; Compute the crop fraction on the raw data, before smoothing.
  crop_fraction[jj] = (num_counts[jj] - total(/INTEGER, psf_img)) / num_counts[jj]
  

  ;; ------------------------------------------------------------------------
  ;; Improve the accuracy of the HRMA PSF MARX made by smoothing  
  ;; to simulate aspect errors & ACIS pixel quantization.
  ;; See AE manual for derivation of sigma values.
  
  ; First we convolve with a square kernel of size 1x1 ACIS pixel to model the quantization of event positions by the ACIS pixel grid.
  ; As I recall, we require the kernel to have odd dimensions so that the astrometry of the 
  ; smoothed PSF will not be shifted.
  PSFpixel_per_ACISpixel = arcsec_per_ACISpixel / arcsec_per_PSFpixel
  kernel_half_dim = 0 > round( (PSFpixel_per_ACISpixel - 1)/2. )
  kernel_dim      = 1 + 2*kernel_half_dim
  print, replicate(kernel_dim,2), replicate(kernel_dim * arcsec_per_PSFpixel / arcsec_per_ACISpixel,2), F='(%"\nsmoothing PSF with box kernel (%dx%d PSF pixels = %4.2fx%4.2f ACIS pixels)\n")'
  
  psf_img = smooth( psf_img, kernel_dim, /EDGE_TRUNCATE )

  
  ; Second we convolve with a Gaussian kernl to mode aspect reconstruction errors.
  ; As I recall, we require the kernel to have odd dimensions so that the astrometry of the 
  ; smoothed PSF will not be shifted.
  sigma       = aspect_blur / arcsec_per_PSFpixel
  print, sigma, aspect_blur, F='(%"smoothing PSF with Gaussian kernel (sigma =%4.1f PSF pixels = %4.2f arcsec)\n")'
  
  kernel  = psf_gaussian( NPIXEL=(1 + 2*(ceil(3*sigma))), $
                        FWHM=(2*sqrt(2* aLog(2)))*sigma, /NORMALIZE )  ; FWHM=2.355*sigma
  
  psf_img = convol( psf_img, kernel, /CENTER, /EDGE_TRUNCATE )
  
  
  ; Third, if desired, we convolve with the square kernel again to model pipeline randomization.
  if pipeline_randomization then begin
    print, replicate(kernel_dim,2), replicate(kernel_dim * arcsec_per_PSFpixel / arcsec_per_ACISpixel,2), F='(%"smoothing PSF with box kernel (%dx%d PSF pixels = %4.2fx%4.2f ACIS pixels)\n")'
  
    psf_img = smooth( psf_img, kernel_dim, /EDGE_TRUNCATE )
  endif
          
          
  ;; ------------------------------------------------------------------------
  ;; Sum up all the light in the PSF within the aperture used for the HRMA calibration.
  ;; If possible, correct for light lost by cropping.
  psf_total = total(psf_img, /DOUBLE) / (1-crop_fraction[jj])

  
  get_date, date_today, /TIMETAG
  fxaddpar, psf_header, 'POSNDATE', date_today, 'date RA,DEC were changed'
  fxaddpar, psf_header, 'RA',       ra,  'source position', F='(F10.6)'
  fxaddpar, psf_header, 'DEC',      dec, 'source position', F='(F10.6)'
  fxaddpar, psf_header, 'X_CAT',    x_cat, 'source position (sky coordinates), from catalog'
  fxaddpar, psf_header, 'Y_CAT',    y_cat, 'source position (sky coordinates), from catalog'
  fxaddpar, psf_header, 'ENERGY',   psf_energy   [jj], '[keV] mono-energy of the PSF'
  fxaddpar, psf_header, 'SUMRCTS',  num_counts   [jj], 'number of counts simulated'
  fxaddpar, psf_header, 'CROPFRAC', crop_fraction[jj], 'fraction of the PSF cropped'
  fxaddpar, psf_header, 'PSF_TOTL', psf_total, 'normalization of this image'
  fxaddpar, psf_header, 'RADIUS50', radius50, 'radius (arcsec) enclosing ~50% PSF'
  fxaddpar, psf_header, 'GRATING',  gratingtype, 'Grating'
  fxaddpar, psf_header, 'ASP_BLUR', aspect_blur, '[arcsec] aspect blur applied'

  ;; ------------------------------------------------------------------------
  ;; Save to file.
  hduname = string(1+jj, F="(%'PSF%d')")
  fxaddpar, psf_header, 'HDUNAME', hduname
  fxaddpar, psf_header, 'CREATOR', creator_string
  
  if (jj EQ 0) then begin
    writefits, psf_fn, psf_img, psf_header 
  endif else begin
    sxdelpar, psf_header, ['SIMPLE','EXTEND']
    fxaddpar, psf_header, 'XTENSION', 'IMAGE   ', BEFORE='BITPIX'
    fxaddpar, psf_header, 'EXTNAME', hduname
    mwrfits, psf_img, psf_fn, psf_header
  endelse
endfor ;jj

print, F='(%"\n  energy  counts (x1000)  crop_fraction")'
forprint, psf_energy, round(num_counts/1000.), crop_fraction, F='(%"  %6.3f  %13d       %0.3f")'

; Save a copy of marx.par to the same directory holding the PSF images.
file_copy, /OVERWRITE, marxdir+'/marx.par', file_dirname(psf_fn)

; Clean up scratch directory if not managed by caller.
if ~tempdir_supplied_by_caller && file_test(temproot) then begin
  list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, temproot
endif
return
end


; =============================================================================
;; Photometry Computation

;; This routine hides the confusing 2-D data structure that we used in the MERGE stage to store extraction information
;; so that we can easily sum SRC_CNTS, BKG_CNTS, NET_CNTS across an energy band or across obsids.
;; This code is awkwardly structured, making it difficult and perhaps dangerous to re-use outside the context of AE's MERGE stage!

;; The table of extraction quantities should be loaded FIRST, e.g.
;;   ae_photometry, OBS_DATA=obs_data, SRC_CHANNELS=src_channels
;; followed by loading the respond files, e.g.
;;   ae_photometry, RMF_FN=merged_rmf_fn, ARF_FN=merged_arf_fn 
;; Then, you can compute photometry values, e.g.
;;         ae_photometry, eband_lo[jj], eband_hi[jj], photometry


; =============================================================================
PRO ae_photometry, OBS_DATA=obs_data, RMF_FN=rmf_fn, ARF_FN=arf_fn, $
                   SRC_CHANNELS=src_channels_param, SRC_CNTS_spectrum=SRC_CNTS_spectrum, BKG_CNTS_spectrum=BKG_CNTS_spectrum, $
                   energ_lo, energ_hi, photometry, x_distribution_variance, y_distribution_variance

;; The routine stores and uses several static data structures.         
COMMON ae_photometry, num_obs, $
  psf_x_var, bkg_x_var, psf_y_var, bkg_y_var, $
  src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, net_counts, net_flux, $  ; 2-D arrays
  channel_number, channel_lowenergy, channel_midenergy, channel_highenergy, channel_specresp,$ ; 1-D vectors from ARF
  src_channels, $                                                                              ; 1-D vectors from PI spectra
  total_bkg_exposurearea, total_src_exposurearea, total_exposure                               ; scalars

;; ------------------------------------------------------------------------
; Start using a new set of extractions.
if keyword_set(obs_data) then begin
    src_channels = src_channels_param

    num_obs = n_elements(obs_data)
    
    psf_x_var = obs_data.psf_x_var
    bkg_x_var = obs_data.bkg_x_var
    psf_y_var = obs_data.psf_y_var
    bkg_y_var = obs_data.bkg_y_var
    
    ;; We're going to need to do photometry for each obsid later (to get position errors), 
    ;; so we'll construct 2-D source and background spectra in vars src_observed_counts, bkg_observed_counts.
    
    ; These photometry arrays must be 2-D, even if we have only one obsid, because subsequent code uses the construct total(xxxx,2) to sum quantities over all obsids.  The extra row must be zero-filled.
    src_observed_counts   = obs_data.src_observed_counts
    bkg_observed_counts   = obs_data.bkg_observed_counts
    if (size(/N_DIM,src_observed_counts) EQ 1) then begin
      src_observed_counts = [[src_observed_counts],[fltarr(n_elements(src_observed_counts))]]
      bkg_observed_counts = [[bkg_observed_counts],[fltarr(n_elements(bkg_observed_counts))]]
    endif
    
    bkg_counts_in_src_region = make_array(/FLOAT, DIMENSION=size(bkg_observed_counts, /DIM))
    
    total_bkg_exposurearea = total(obs_data.bkg_backscal)
    total_src_exposurearea = total(obs_data.src_backscal)
    
    for jj = 0, num_obs-1 do begin
      ; -----------------------------------------------------------------------
      ; Compute the real-valued spectrum for the background that should
      ; be within the source aperture.  NOTE that the AE convention is
      ; that the background scaling information is carried solely in the
      ; BACKSCAL keywords (not in EXPOSURE keywords), which are integrals
      ; of the emaps over the extraction regions.  (See also comments in 
      ; source extraction code.) 
      ; That integration is how the geometric area of the region comes into play.
      ; The emaps are expected to represent both effective area and integration 
      ; time variations across the field -- the EXPOSURE keywords in the 
      ; spectra are not relevant.  
      
      ; Thus, AT THE ENERGY FOR WHICH THE EMAP WAS COMPUTED we can estimate 
      ; the real-valued background that should be within the source aperture
      ; by scaling the observed counts in the background spectrum by the ratio
      ; of these "sensitivity" (emap) integrals:
      
      area_exposure_ratio = float(obs_data[jj].src_backscal)/obs_data[jj].bkg_backscal
      if ~finite(area_exposure_ratio) then area_exposure_ratio = 0.0
      
      ; If the source and background regions have ARFs with the same SHAPE,
      ; i.e. they are near each other, then src_backscal & bkg_backscal have the same
      ; dependence on energy, and area_exposure_ratio is independent of energy.
      ; In other words if we repeated the exercise with emaps made at a different  
      ; energy we'd get the same area_exposure_ratio value.
      ; 
      ; However, if the source and background regions have differently shaped ARFs 
      ; then src_backscal & bkg_backscal would vary differently with energy and
      ; thus area_exposure_ratio is a function of energy.
      ; We can NOT simply use the ratio of the src & bkg ARFs as the area_exposure_ratio
      ; because that accounts only for sensitivity differences between the regions; the
      ; geometric area information is found only in (src_backscal/bkg_backscal).
      ; Recalling that (src_backscal/bkg_backscal) IS the correct scaling for the energy
      ; at which the emap was computed, we see that the appropriate energy-dependence to
      ; put on area_exposure_ratio is the ratio of the two ARFs, scaled to 1.0 at the
      ; emap's energy.  The user must supply that in the keyword EMAP_ENERGY.
      
      ; OBVIOUSLY USING A BACKGROUND ARF IN THIS WAY CAN CORRECTLY SCALE ONLY THE X-RAY
      ; BACKGROUND, NOT THE INSTRUMENTAL BACKGROUND, SO THE BACKGROUND SUBTRACTION MAY
      ; STILL HAVE PROBLEMS.
      ;
      ; A better approach is probably to use stowed event data to produce an instrumental 
      ; background spectrum for each extraction (object and sky regions), and then in the
      ; spectral analysis you can explicitly model the astrophysical (sky) X-ray background
      ; and simultaneously fit both extractions.  See the AE manual and the diffuse recipe.
      
      if file_test(obs_data[jj].bkg_arf_fn) then begin
        ; Interpolate each ARF onto the energy grid of the SOURCE spectrum, 
        ; and form a correction to area_exposure_ratio that is 1.0 at EMAP_ENERGY. 
        ; "src_channel_midenergy" & "dum4" should be indentical since the source
        ; RMF is used in both calls.
                     
        ae_channel_energy_and_arf, obs_data[jj].rmf_fn, obs_data[jj].arf_fn, $
                dum1, dum2, dum3, src_channel_midenergy, src_channel_specresp

        ae_channel_energy_and_arf, obs_data[jj].rmf_fn, obs_data[jj].bkg_arf_fn, $
                dum1, dum2, dum3, dum4, bkg_channel_specresp
        
        correction_for_energy = (src_channel_specresp / interpol(src_channel_specresp, src_channel_midenergy, emap_energy)) / $
                                (bkg_channel_specresp / interpol(bkg_channel_specresp, src_channel_midenergy, emap_energy))
        
;function_1d, id1, src_channel_midenergy, src_channel_specresp
;function_1d, id1, src_channel_midenergy, bkg_channel_specresp
        if (jj EQ 0) then begin
          name = string(sourcename[ii], obs_data[jj].obsid, area_exposure_ratio, F='(%"%s, obs%d, BACKSCAL ratio=%10.2g")')
          function_1d, id2, src_channel_midenergy, correction_for_energy, DATASET=name, XTIT='Energy (keV)', YTIT='Correction to background spectrum'
        endif

        area_exposure_ratio = area_exposure_ratio * correction_for_energy
      endif      
      
      ; Finally, apply our background scaling to the observed background spectrum. 
      bkg_counts_in_src_region[0,jj] = bkg_observed_counts[*,jj] * area_exposure_ratio

    endfor ; jj, looping over obsids

    ; Compute 2-D net counts array.  Since each bin is only 1 PI channel wide
    ; many bins will be negative (i.e. there will be background data there but no
    ; source event).
    net_counts               = src_observed_counts - bkg_counts_in_src_region
    
    total_exposure    = total(obs_data.src_exposure)
    
    
    ;; The calling protocol is that obs_data must be loaded first, then the responses.
    ;; So, as defensive programming, set the ARF values and  quantites derived from the ARF to zero.
    channel_specresp = replicate(0., 1 > n_elements(channel_number))    
    net_flux         = replicate(0., 1 > n_elements(channel_number),2)
endif ; keyword_set(obs_data)


;; ------------------------------------------------------------------------
;; Start using a new set of response files.
if keyword_set(rmf_fn) then begin
    ae_channel_energy_and_arf, rmf_fn, arf_fn, $
        channel_number, channel_lowenergy, channel_highenergy, channel_midenergy, channel_specresp

    ; If an ARF filename was passed, then make sure there are no zeros because we are dividing by the ARF later to compute "net_flux" array.
    if keyword_set(arf_fn) then begin
      ind = where(channel_specresp EQ 0, count)
      if (count GT 0) then begin
        print, 'WARNING: found ARF values equal to zero in '+arf_fn
        channel_specresp[ind] = 1E-10
      endif
      
      ; Compute 2-D net flux array; see algorithms section of manual.  Since each bin is only 1 PI channel wide
      ; many bins will be negative (e.g. there will be background data but zero source events).
      make_2d, channel_specresp, intarr(num_obs>2), channel_specresp_2D, dummy
      
      net_flux = net_counts / channel_specresp_2D / total_exposure
      
      ; WARNING!  The net_flux vector is very noisy at the ends, where the ARF is tiny.
      ; Thus, all the statistics derived from it (FLUX1, ENERG_MEAN_INCIDENT, ENERG_PCT25_INCIDENT,
      ; ENERG_PCT50_INCIDENT, ENERG_PCT75_INCIDENT) have low SYSTEMATIC error (because we
      ; "follow" the shape of the ARF), but have high RANDOM error (because a single src or bkg
      ; count where the ARF is tiny has a large effect on the statistic). 
      
    endif else begin
      ; No ARF filename was passed, so make sure we discard any ARF values and derived quantites from some previous call!!
      channel_specresp = replicate(0., n_elements(channel_number))    
      net_flux         = replicate(0., n_elements(channel_number),2)
    endelse
endif ; keyword_set(rmf_fn)
    


;; ------------------------------------------------------------------------
if arg_present (SRC_CNTS_spectrum) then begin
  SRC_CNTS_spectrum = total(src_observed_counts,2)
  BKG_CNTS_spectrum = total(bkg_observed_counts,2)
  src_channels_param= src_channels
endif


;; ------------------------------------------------------------------------
if arg_present(photometry) then begin
  f_nan      = !VALUES.F_NAN
  photometry = {ENERG_LO:0.0, ENERG_HI:0.0, $
                CHAN_LO: 0,   CHAN_HI: 0, $
                MEAN_ARF:0.0, SRC_CNTS:0L, $
                BKG_CNTS:0L,  BACKSCAL:0.0, $
                NET_CNTS:0.0, NET_CNTS_SIGMA_UP:0.0, NET_CNTS_SIGMA_LOW:0.0, $
                SRC_SIGNIF:0.0, PROB_NO_SOURCE:0.0, $
                FLUX1:   0.0, FLUX2:     0.0, $
                ENERG_MEAN_OBSERVED: f_nan, ENERG_MEAN_INCIDENT: f_nan, $
                ENERG_PCT25_OBSERVED:f_nan, ENERG_PCT25_INCIDENT:f_nan, $
                ENERG_PCT50_OBSERVED:f_nan, ENERG_PCT50_INCIDENT:f_nan, $
                ENERG_PCT75_OBSERVED:f_nan, ENERG_PCT75_INCIDENT:f_nan }

  photometry.ENERG_LO = energ_lo
  photometry.ENERG_HI = energ_hi
  
  ; When the caller supplies identical energ_lo and energ_hi values, then it is simply trying to look up an ARF values at a specific energy.
  ; This has to be coded specially:
  if (energ_lo EQ energ_hi) then begin
    photometry.MEAN_ARF = interpol(channel_specresp, channel_midenergy, energ_lo)
    return
  endif
  
    
  ; Look up PI channel numbers that bracket [eband_lo,eband_hi] ranges.
  ; A channel is included only if its mid-energy is in the range, i.e. if >=50% of it is in the energy range.
  ind =         (where(channel_midenergy GE energ_lo))[0]
  photometry.CHAN_LO = channel_number[ind]
  if (ind EQ 0) then $
    print, energ_lo, energ_hi, F='(%"ERROR! Your spectra do not span the photometry band %0.1f:%0.1f keV!")'
  
  ind = (reverse(where(channel_midenergy LE energ_hi)))[0]
  photometry.CHAN_HI = channel_number[ind] > photometry.CHAN_LO
  if (ind EQ (n_elements(channel_midenergy)-1) ) then $
    print, energ_lo, energ_hi, F='(%"ERROR! Your spectra do not span the photometry band %0.1f:%0.1f keV!")'
  
  
  ; Find the first subscript indexes of the arrays src_observed_counts, bkg_observed_counts,
  ; net_counts, net_flux that correspond to this band's channel range.
  ; The variable "src_channels" (from the CHANNEL column in spectrum file) usually starts at 1
  band_index = where((photometry.CHAN_LO  LE src_channels)   AND $
                     (src_channels        LE photometry.CHAN_HI), band_num_channels)
  
  ; Compute the mean ARF value in the energy band.
  ; Since the ARF is not evenly sampled we must integrate the ARF & divide by Erange.
  photometry.MEAN_ARF = total(channel_specresp[band_index] * (channel_highenergy[band_index] - channel_lowenergy[band_index]), /D)$
                                                     / total((channel_highenergy[band_index] - channel_lowenergy[band_index]), /D)
  
  ; Tally the SRC counts, BKG counts, and NET counts for this band in each obsid, producing vectors with num_obs elements.
  ; When there's only one obsid, src_observed_counts and net_counts are still 2-D---the extra row is all zeros.
  ; To prevent SRC_CNTS_single_obs and BKG_CNTS_single_obs from having extra elements in this case, we trim them below.
  SRC_CNTS_single_obs = (total(src_observed_counts[band_index,*],1,/D))[0:num_obs-1]
  BKG_CNTS_single_obs = (total(bkg_observed_counts[band_index,*],1,/D))[0:num_obs-1]
  NET_CNTS_single_obs = (total(         net_counts[band_index,*],1,/D))[0:num_obs-1]

  ; Sum those single-obsid photometry values to get merged scalar SRC_CNTS and BKG_CNTS.
  photometry.SRC_CNTS = round(total(SRC_CNTS_single_obs,/D))
  photometry.BKG_CNTS = round(total(BKG_CNTS_single_obs,/D))
                                              
  ; Estimate the (real-valued, scalar) number of background counts falling in the source region for this band
  ; by summing over the energy band and summing over the obsids.
  band_scaled_bkg = total(bkg_counts_in_src_region[band_index,*],/D)
  
  ; Compute the BACKSCAL value which, when used to normalize BKG_CNTS 
  ; in the NET_CNTS equation below, will produce the "band_scaled_bkg" 
  ; estimated above.
  ; For bands with zero background counts, BACKSCAL would be NaN.  Not knowing what else
  ; to do in such cases, we use the scaling one would get by combining all the src regions
  ; and combining all the background regions.
  photometry.BACKSCAL = (band_scaled_bkg GT 0) ? photometry.BKG_CNTS / band_scaled_bkg $
                                               : total_bkg_exposurearea/total_src_exposurearea
  
  ; Compute the significance of the observed SRC_CNTS as a disproof of the "null hypothesis" which is 
  ; that there is no source, i.e. that all the observed counts are background.  
  ; We use equation A7 from Weisskopf 2006 (astro-ph/0609585):
  if ~finite(photometry.BACKSCAL) then begin
    message, 'ERROR: BACKSCAL is not finite.'
  endif
  photometry.PROB_NO_SOURCE = binomial(photometry.SRC_CNTS, $
                                       photometry.SRC_CNTS + photometry.BKG_CNTS, $
                                       1D/(1D + photometry.BACKSCAL) , /DOUBLE) > 0
                                         
  
  ; Extract the channel numbers and channel energies that are in this energy band.
  ; Integrate the net counts and flux over the obsids (subscript #2), and extract the elements that are in this energy band.
  band_channels          = src_channels     [band_index]
  band_channel_midenergy = channel_midenergy[band_index]
  
  band_net_counts        =  total(net_counts[band_index,*],2)
  band_net_flux          =  total(net_flux  [band_index,*],2)
  
  ; Sum across the energy band to get scalar NET_CNTS and FLUX1 photometry.
  photometry.NET_CNTS = total(band_net_counts,/D)
  photometry.FLUX1    = total(band_net_flux,/D)
  
  ; Compute the more accurate, but more noisy FLUX2 estimate.
  photometry.FLUX2    = photometry.NET_CNTS / photometry.MEAN_ARF / total_exposure
  
  ; ------------------------------------------------------------------------
  ; Characterize the shape of the merged spectrum, within this energy band, in various ways.
  
  ; These are simple "center of masses" for net_counts or net_flux.
  if (photometry.NET_CNTS GT 0) then begin
    mean_channel = total(band_channels * band_net_counts, /D) / photometry.NET_CNTS
    photometry.ENERG_MEAN_OBSERVED = interpol(channel_midenergy, channel_number, mean_channel )
  endif
  
  if (photometry.FLUX1 GT 0) then begin
    mean_channel = total(band_channels * band_net_flux, /D) / photometry.FLUX1
    photometry.ENERG_MEAN_INCIDENT = interpol(channel_midenergy, channel_number, mean_channel )
  endif
  
  ; These are percentiles of net_counts or net_flux, i.e. the energy below which a
  ; specified fraction of the net_counts or net_flux falls.
  ; The 50% percentiles might be called "median" net_counts or net_flux.
  ;
  ; NOTE that the cumulative distributions of net_counts or net_flux as you move across
  ; the band are NOT necessarily monotonic (a count in src region makes
  ; it jump up, while a count in bkg region makes it jump down).
  ; The function histogram_percentile() is responsible for making a sensible estimate
  ; when there are multiple "crossings" of the specified percentile.  It returns a
  ; real-valued 0-based index of the histogram passed to it which we convert to an energy.
  if (band_num_channels GT 4) then begin
    ramp = indgen(band_num_channels)
    ind = histogram_percentile(band_net_counts, 0.25, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT25_OBSERVED = $
                              interpol(band_channel_midenergy, ramp, ind )
    
    ind = histogram_percentile(band_net_flux,   0.25, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT25_INCIDENT = $
                              interpol(band_channel_midenergy, ramp, ind )
    
    ind = histogram_percentile(band_net_counts, 0.50, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT50_OBSERVED = $
                              interpol(band_channel_midenergy, ramp, ind )
    
    ind = histogram_percentile(band_net_flux,   0.50, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT50_INCIDENT = $
                              interpol(band_channel_midenergy, ramp, ind )
    
    ind = histogram_percentile(band_net_counts, 0.75, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT75_OBSERVED = $
                              interpol(band_channel_midenergy, ramp, ind )
    
    ind = histogram_percentile(band_net_flux,   0.75, ERROR=error)
    if (error EQ 0) then photometry.ENERG_PCT75_INCIDENT = $
                              interpol(band_channel_midenergy, ramp, ind )
  endif
  
  ; ------------------------------------------------------------------------
  ; Compute uncertainies on some photometry estimates.
  
  ; We compute Gehrels upper (equation 7) & lower (equation 12) 84% limits (equivalent 
  ; to Gaussian 1-sigma limits) on SRC_CNTS and BKG_CNTS.    
  ; Then we propagate those 1-sigma upper & lowerlimits through the equation 
  ;   NET_CNTS = SRC_CNTS - BKG_CNTS/BACKSCAL
  ; using equation 1.31 in "A Practical Guide to Data Analysis for Physical Science Students",
  ; L. Lyons, 1991 to get upper and lower 1-sigma errors on NET_CNTS.
  
  src_cnts = photometry.SRC_CNTS
  bkg_cnts = photometry.BKG_CNTS
  
  src_cnts_limit_up = src_cnts + 1 + sqrt(src_cnts + 0.75)
  bkg_cnts_limit_up = bkg_cnts + 1 + sqrt(bkg_cnts + 0.75)
  
  ; These lower limits become NaN when ???_cnts = 0.  
  ; Post-processing programs (e.g. hardness ratio calculators) should use this flag to 
  ; decide what to do, e.g. to set lower limit on NET_CNTS to zero, or skip calculating
  ; things derived from NET_CNTS.
  src_cnts_limit_low = src_cnts * ( 1 - 1/(9.0*src_cnts) - 1/(3.0*sqrt(src_cnts)) )^3.0 
  bkg_cnts_limit_low = bkg_cnts * ( 1 - 1/(9.0*bkg_cnts) - 1/(3.0*sqrt(bkg_cnts)) )^3.0 

  photometry.NET_CNTS_SIGMA_UP  = sqrt( ( src_cnts_limit_up  - src_cnts)^2 + $
                                        ((bkg_cnts_limit_up  - bkg_cnts)/photometry.BACKSCAL)^2 )

  photometry.NET_CNTS_SIGMA_LOW = sqrt( ( src_cnts_limit_low - src_cnts)^2 + $
                                        ((bkg_cnts_limit_low - bkg_cnts)/photometry.BACKSCAL)^2 )
             
  photometry.SRC_SIGNIF = photometry.NET_CNTS / photometry.NET_CNTS_SIGMA_UP

  
  
  ;; ------------------------------------------------------------------------
  ;; Estimate the variance of the parent distribution for the merged extraced counts.
  ;; This is a weighted average of the variances of the PSFs and flat backgrounds, both clipped by the apertures.
  ;; See comments in MERGE stage.
  
  ; Assign PSF and flat background weights such that they sum to SRC_CNTS_single_obs.
  ; Then normalize the weights to sum to 1.0.
  psf_weight =  NET_CNTS_single_obs               > 0
  bkg_weight = (SRC_CNTS_single_obs - psf_weight) > 0

  weight  = [psf_weight, bkg_weight]
  
  weight /= total(weight)

  if (n_elements(weight)    NE 2*num_obs) then message, 'BUG IN AE!!!'
  if (n_elements(psf_x_var) NE   num_obs) then message, 'BUG IN AE!!!'
  
  ; Sum the weighted variances and get a standard deviation for the event distribution.
  x_distribution_variance =  total(weight * [psf_x_var, bkg_x_var]) 
  y_distribution_variance =  total(weight * [psf_y_var, bkg_y_var]) 
endif ; arg_present(photometry)

return
end




; =============================================================================
; ACIS EXTRACT MAIN PROGRAM
; =============================================================================
PRO acis_extract, catalog_or_srclist, obsname, obsdata_filename_p, $
                  SOURCE_NOT_OBSERVED=source_not_observed, $
                  EXTRACTION_NAME=extraction_name, MERGE_NAME=merge_name, $
                                  
                  CONSTRUCT_REGIONS=construct_regions, ASPECT_FN=aspect_fn, S_AIMPOINT=s_aimpoint, $
                  MASK_FRACTION=mask_fraction, MASK_MULTIPLIER=mask_multiplier, $
                  REGION_ONLY=region_only, $
                  PIPELINE_RANDOMIZATION=pipeline_randomization, ASPECT_BLUR=aspect_blur, $
                  DIFFUSE=diffuse, $
                  QUERY_ONLY=query_only, $
                  
                  SHOW_REGIONS=show_regions, SRCLIST_FILENAME=srclist_filename, INDEX_FILE=index_file, $
                  REGION_FILE=region_file, OMIT_BKG_REGIONS=omit_bkg_regions, DISPLAY_FILE=display_file, $
                                  
                  EXTRACT_EVENTS=extract_events, ONLY_EDITED=only_edited, $
                  WARNING_REGION_FILENAME=warning_region_filename, NEIGHBORHOOD_SIZE=neighborhood_size,$
                  TIME_FILTER=time_filter, $
                  
                  CHECK_POSITIONS=check_positions, MAXLIKELIHOOD_ITERATIONS=maxlikelihood_iterations, $
                  SKIP_RECONSTRUCTION=skip_reconstruction, SKIP_CORRELATION=skip_correlation, $
                  THETA_RANGE=theta_range,$
                  
                  NEW_CATALOG=new_catalog, $

                  EXTRACT_SPECTRA=extract_spectra, $
                  EMAP_FILENAME=emap_filename_p, ENERGY_RANGE=energy_range, DETCHANS=DETCHANS, $
                  ASPHIST_DIR=asphist_dir, ARDLIB_FILENAME=ardlib_filename, $
                  PBKFILE=pbkfile, MSKFILE=mskfile, REUSE_NEIGHBORHOOD=reuse_neighborhood, $
                  GENERIC_RMF_FN=generic_rmf_fn, USE_MKRMF=use_mkrmf, $
                  
                  ARF_CORRECTION_FILENAME=arf_correction_filename, $
                  
                  EXTRACT_BACKGROUNDS=extract_backgrounds, REUSE_BACKGROUND=reuse_background, $
                  MIN_NUM_CTS=min_num_cts, TWEAK_BACKSCAL=tweak_backscal, $
                  
                  TIMING=timing, $
                  
                  MERGE_OBSERVATIONS=merge_observations, EBAND_LO=eband_lo, EBAND_HI=eband_hi, $
                  EMAP_ENERGY=emap_energy, OVERLAP_LIMIT=overlap_limit, EMAP_UNIFORMITY_LIMIT=emap_uniformity_limit, $
                  SKIP_PSF=skip_psf_p, SKIP_NEIGHBORHOOD=skip_neighborhood_p, SKIP_SPECTRA=skip_spectra_p, SKIP_TIMING=skip_timing_p, $
                  MERGE_FOR_PB=merge_for_pb, MERGE_FOR_POSITION=merge_for_position, $
                  
                  FIT_SPECTRA=fit_spectra, CHANNEL_RANGE=channel_range, CSTAT=cstat, $
                  MODEL_FILENAME=model_filename, MODEL_CHANGES_FILENAME=model_changes_filename, $
                  SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                  GROUP_WITHOUT_BACKGROUND=group_without_background, INTERACTIVE=interactive, $
                  FIT_TIMEOUT=fit_timeout, $
                  
                  COLLATED_FILENAME=collated_filename, HDUNAME=hduname, SINGLE_OBSID=single_obsid, $
                  REGION_TAG=region_tag, MATCH_EXISTING=match_existing, LABEL_FILENAME=label_filename, $
                                  
                  PLOT=plot, CARTOON_TEMPLATE=cartoon_template, VERBOSE=verbose

creator_string = "acis_extract, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, F='(%"\nACIS Extract: ============================================================")'  
print, creator_string
print, systime()
print, 'http://www.astro.psu.edu/xray/docs/TARA/ae_users_guide.html'
print, 'Join the mail list to receive announcements: https://phoenix.astro.psu.edu/mailman/listinfo/acis-extract'
print, 'Contact: patb@astro.psu.edu'
print, F='(%"ACIS Extract: ============================================================\n")'  
exit_code = 0

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
;; Some tools that use these temp filenames (e.g addarf and addrmf) are executed from a directory
;; other than cwd, and thus require that the path to tempdir is absolute, not relative.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  temproot = 'AE' + session_name +'.noindex/'
  temproot = filepath(temproot, /TMP)
endrep until (NOT file_test(temproot))
file_mkdir, temproot
print, 'Using temporary directory: ', temproot

cache_dir        = temproot+ 'cache/'
tempdir          = temproot+ 'tmp/'
file_mkdir, cache_dir
file_mkdir, tempdir

temp_bkgimg_fn   = cache_dir + 'temp.bkg.img'
inband_events_fn = tempdir + 'temp.inband.evt'
temp_events_fn   = tempdir + 'temp.evt'
temp_region_fn   = tempdir + 'temp.reg'
temp_image_fn    = tempdir + 'temp.img'
temp_text_fn     = tempdir + 'temp.txt'
temp_wgt_fn      = tempdir + 'temp.wgt'
temp_rmf_fn      = tempdir + 'temp.rmf'

;; We assume that access to /tmp/ will often be faster than access to the event and emap data passed,
;; so let's start by copying those files to a cache.
dmmerge_header_lookup_fn = cache_dir + 'dmmerge_header_lookup.txt'

if keyword_set(obsdata_filename_p) then begin
  fdecomp, obsdata_filename_p, disk, item_path, item_name, item_qual
  obsdata_filename = cache_dir+ item_name+'.'+item_qual
  file_copy, obsdata_filename_p, obsdata_filename
endif

if keyword_set(emap_filename_p) then begin
  fdecomp, emap_filename_p, disk, item_path, item_name, item_qual
  emap_filename = cache_dir+ item_name+'.'+item_qual
  file_copy, emap_filename_p, emap_filename
endif


;; Save any existing PFILES environment variable.
inherited_pfiles = getenv("PFILES")


if NOT keyword_set(mask_fraction)   then mask_fraction  =0.99
if NOT keyword_set(mask_multiplier) then mask_multiplier=1.1
if NOT keyword_set(maxlikelihood_iterations) then maxlikelihood_iterations = 400
if (n_elements(verbose) EQ 0)       then verbose=1


if NOT keyword_set(energy_range) then energy_range=[0.5,8.0]

if NOT (keyword_set(eband_lo) AND keyword_set(eband_hi)) then begin
  eband_lo = [0.5,  0.5, 2.0,  0.5, 1.7, 2.8,  0.5, 1.5, 2.5,  0.5, 1.0, 2.0, 4.0, 6.0,  0.5, 2.0]
  eband_hi = [8.0,  2.0, 8.0,  1.7, 2.8, 8.0,  1.5, 2.5, 8.0,  1.0, 2.0, 4.0, 6.0, 8.0,  7.0, 7.0]
endif

if n_elements(overlap_limit        ) EQ 0 then overlap_limit         = 0.10

if n_elements(emap_uniformity_limit) EQ 0 then emap_uniformity_limit = 0.50

if NOT keyword_set(neighborhood_size) then neighborhood_size = 50 ; arcsec

if NOT keyword_set(asphist_dir)   then asphist_dir = './asphist'

if (n_elements(theta_range) NE 2) then theta_range = [0,100.]

type = size(obsname,/TNAME)
dim  = size(obsname,/DIMEN)
case dim of
  0   : fail = 0
  1   : fail = (type NE 'STRING')
  else: fail = (type NE 'STRING') || ~keyword_set(merge_observations)
endcase
if fail then begin
  print, 'ERROR: parameter "obsname" must be a scalar string'
  GOTO, FAILURE
endif

 
arcsec_per_ACISpixel = 0.492 
arcsec_per_skypixel  = 0.492 
nominal_psf_energy = 1.49670

max_polygon_elements = 100


; See these URLs for the X11 color chart:
; http://en.wikipedia.org/wiki/X11_color_names
; http://www.mcfedries.com/Books/cightml/x11color.htm
; http://www.febooti.com/products/iezoom/online-help/html-color-names-x11-color-chart.html
; See <X11root>/lib/X11/rgb.txt for the colors known to a given machine.
region_colors = ['red','green','cyan','magenta','yellow','DodgerBlue','SlateBlue','Cornsilk','Goldenrod','Chocolate','DarkSalmon', 'Tan','Peru','Sienna','Salmon', 'SandyBrown','DarkGoldenrod','Brown','IndianRed']

src_stats_basename       = 'source.stats'
src_photometry_basename  = 'source.photometry'
src_image_basename       = 'source.img'
obs_stats_basename       = 'obs.stats'
obs_frac_basename        = 'obs.psffrac'
env_events_basename      = 'neighborhood.evt'
env_image_basename       = 'neighborhood.img'
src_region_basename      = 'extract.reg'
bkg_region_basename      = 'background.reg'
bkg_pixels_region_basename = 'background_pixels.reg'
bkg_emap_basename        = 'background.emap'
src_emap_basename        = 'source.emap'
evt_region_basename      = 'evt.reg'
src_events_basename      = 'source.evt'
bkg_events_basename      = 'background.evt'
src_spectrum_basename    = 'source.pi'
bkg_spectrum_basename    = 'background.pi'
bkg_arf_basename         = 'background.arf'
rmf_basename             = 'source.rmf'
arf_basename             = 'source.arf'
psf_basename             = 'source.psf'
lc_binned_basename       = 'source.binned_lc'
lc_smooth_basename       = 'source.lc'
fit_stats_basename       = 'source.spectra'
modelsubdir              = 'spectral_models/'
event_plot_basename      = 'source.evt.ps'



;; Input catalog should be an ascii file with source names. 
if keyword_set(catalog_or_srclist) then begin
  if ~file_test(catalog_or_srclist) then begin
    print, 'ERROR: cannot find file ', catalog_or_srclist
    GOTO, FAILURE
  endif
  
  readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'
  
  ; Trim whitespace and remove blank lines.
  sourcename = strtrim(sourcename,2)
  ind = where(sourcename NE '', num_sources)
  
  if (num_sources EQ 0) then begin
    print, 'ERROR: no entries read from source list ', catalog_or_srclist
    GOTO, FAILURE
  endif
  
  sourcename = sourcename[ind]
  print, num_sources, F='(%"\n%d sources found in catalog.\n")'
  
  if (num_sources GT 1) then begin
    ;; Look for duplicate source names.
    sort_index   = sort(sourcename)
    sorted_names = sourcename[sort_index]
    
    uniq_index = uniq(sorted_names)
    num_uniq   = n_elements(uniq_index)
    if (num_uniq LT num_sources) then begin
      print, F='(%"\n=============================================================================")'
      print, 'WARNING: The catalog contains duplicate source names:'
    
      next = 0
      for ii=0L,num_uniq-1 do begin
        this = uniq_index[ii]
        if (this GT next) then begin
          print
          for jj=next,this do begin
            duplicate_index = sort_index[jj]
            print, sourcename[duplicate_index], F='(%"  %s")'
          endfor
        endif ; (this GT next)
        
        next = this+1
      endfor
      
      print, '============================================================================='
    endif ;duplicates
  endif

endif else num_sources = 1

;; If the SOURCE_NOT_OBSERVED supplied is a vector then we consider it an input parameter;
;; otherwise we initialize SOURCE_NOT_OBSERVED
if (size(/N_DIM, source_not_observed) NE 1) then source_not_observed = replicate(0B,num_sources)

if (n_elements(source_not_observed) NE num_sources) then begin
  print, 'ERROR: the vector SOURCE_NOT_OBSERVED and the catalog must have the same number of elements.'
  help, num_sources, sourcename, source_not_observed, /TRACEBACK
  GOTO, FAILURE
endif

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)

if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                else merge_subdir = ''
if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)

  
;; =============================================================================
if keyword_set(new_catalog) then begin
;; =============================================================================
  ;; Re-create 5-column catalog for the sources in the supplied source list.
  ;; For regions that were edited use the actual PSF fraction; for unedited regions
  ;; use the target PSF fraction.  For unobserved sources used the median of the
  ;; target PSF fractions.
  print, 'Reading source information ...'

  ra                   = replicate(!VALUES.D_NAN,num_sources)
  dec                  = replicate(!VALUES.D_NAN,num_sources)
  target_psf_fraction  = replicate(!VALUES.F_INFINITY,num_sources)
  region_edited        = bytarr(num_sources)
  psf_fraction         = replicate(!VALUES.F_INFINITY,num_sources)
  psf_energy           = fltarr(num_sources)

  ;; Read summary information from unnamed stats file for each source.
  for ii = 0, num_sources-1 do begin
    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    unnamed_src_stats_fn = sourcename[ii] + '/' + src_stats_basename
    stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra[ii]  = sxpar(stats, 'RA')
      dec[ii] = sxpar(stats, 'DEC')
      
      ; If existing object name and directory name don't match, warn the observer.
      if  (sourcename[ii] NE strtrim(sxpar(stats, 'OBJECT'),2)) then $
        print, 'WARNING!  Source catalog/directory name does not match OBJECT keyword.'
    endif else print, 'WARNING! Could not read ', unnamed_src_stats_fn


    if keyword_set(obsname) then begin
      ;; Look for PSF fraction in the single observation specified.
      obsdir   = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
      stats_fn = obsdir + obs_stats_basename
      obsid_count_i = 1
    
    endif else begin
      ;; Look for PSF fractions in all observations.
      stats_fn = file_search( sourcename[ii] + '/*/' + extraction_subdir[ii] + obs_stats_basename, COUNT=obsid_count_i )
      if (obsid_count_i EQ 0) then begin
        print, '  Not present in any observation'
        continue
      endif
    endelse

    for jj = 0, obsid_count_i-1 do begin
      stats = headfits(stats_fn[jj], ERRMSG=error)
      
      if (NOT keyword_set(error)) then begin
        target_psf_fraction [ii] = target_psf_fraction [ii]   < sxpar(stats, 'FRACSPEC')
        
        if (psf_energy[ii] EQ 0) then psf_energy[ii] = sxpar(stats, 'PSF_ENGY')
        
        if (sxpar(stats, 'REG_EDIT')) then begin
          region_edited     [ii] = 1
          
          this_psf_fraction = sxpar(stats, 'PSF_FRAC')
          if (this_psf_fraction LT psf_fraction[ii]) then begin
            psf_fraction      [ii] = this_psf_fraction
            psf_energy        [ii] = sxpar(stats, 'PSF_ENGY')
          endif
        endif ;REG_EDIT true
      endif ;no error
    endfor ;jj
  endfor ;ii
  
  ; Start with the smallest FRACSPEC found in all observations.
  new_psf_fraction = target_psf_fraction
  
  ; For regions hand edited, use the smallest actual PSF fraction.
  ind = where(region_edited, count)
  if (count GT 0) then begin
    new_psf_fraction[ind] = psf_fraction[ind]
    print, count, ' extraction regions edited; using actual PSF fractions'
    forprint, sourcename[ind], psf_fraction[ind]
  endif
  
  ; For unobserved sources, use the median PSF fraction of the observed ones.  
  ind = where(new_psf_fraction EQ 0, count)
  if (count GT 0) then begin
    non_null_frac = target_psf_fraction[where(target_psf_fraction NE 0)]
    new_psf_fraction[ind] = median(non_null_frac)
  endif
  
  forprint, TEXTOUT=new_catalog, sourcename, ra, dec, new_psf_fraction, psf_energy, $
            F='(A,1x,F10.6,1x,F10.6,1x,F5.3,1x,F7.5)', /NoCOMMENT
  print, '============================================================================='
  print, 'Wrote catalog ', new_catalog
  print, '============================================================================='
  GOTO, CLEANUP
endif


;; =============================================================================
if keyword_set(collated_filename) && ~keyword_set(show_regions) then begin
;; =============================================================================
  
  fdecomp, collated_filename, disk, item_path, item_name, item_qual
  if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual

  case n_elements(hduname) of
   0: hduname = strarr(num_sources)
   1: hduname = replicate(hduname, num_sources)
   else: begin
         temp       = hduname
         hduname    = strarr(num_sources)
         hduname[0] = temp
         end
  endcase
  
  dum = where(hduname EQ '', count)
  if (count GT 0) && (verbose GT 0) then print, count, F='(%"WARNING!  No HDUNAME specified for %d sources; will use result from the last fit performed.")'

  ;; If directed, use the existing table as a template.
  if keyword_set(match_existing) then begin
    status = 0
    case size(/TNAME, match_existing) of
      'STRUCT' : template_row = match_existing[0] 
      'STRING' : template_row = (mrdfits(match_existing   , 1, theader, /SILENT, STATUS=status))[0]
      else     : template_row = (mrdfits(collated_filename, 1, theader, /SILENT, STATUS=status))[0]
    endcase                         
    
    if (status EQ 0) then begin
      col_names = tag_names(template_row)
      num_cols  = n_elements(col_names)
      
      ; Create a structure matching the existing table but with all fields nulled.
      struct_assign, {foobar:0}, template_row 
      bin_table = replicate(template_row, num_sources)
    endif
  endif
    
  if keyword_set(bin_table) then begin
    start_pass = 2
  endif else begin
    start_pass = 1

    col_names    =      strarr(500)
    col_comments =      strarr(500)
    col_types    = replicate(4,500)  ; Default is FLOAT (IDL type 4)
    col_elements = replicate(1,500)  ; Default is scalar
    
    ;; Define a column for the source name in the catalog.
    col_names   [0] = 'catalog_name'
    col_comments[0] = 'source name in catalog'
    col_types   [0] = 7  ; STRING type
    num_cols        = 1
  
    ;; Define a column LABEL, the source label that will appear in region files.
    col_names   [1] = 'LABEL'
    col_comments[1] = 'source lable'
    col_types   [1] = 7  ; STRING type
    num_cols        = 2
  
    ;; Define a column MODEL, the XSPEC model name (an HDUNAME in source.spectra).
    col_names   [2] = 'MODEL'
    col_comments[2] = 'XSPEC model name'
    col_types   [2] = 7  ; STRING type
    num_cols        = 3
  
    ;; Define a column NMODELS, the number of XSPEC models available (HDUs in source.spectra).
    col_names   [3] = 'NMODELS'
    col_comments[3] = '# of spectral models'
    col_types   [3] = 2  ; INTEGER type
    num_cols        = 4
  
    ;; Define a column PROVISNL, a boolean flag indicating whether MODEL is provisional.
    col_names   [4] = 'PROVISNL'
    col_comments[4] = '# of spectral models'
    col_types   [4] = 2  ; INTEGER type
    num_cols        = 5
  
    ;; Define a column OBSDIR, the pathname of the single-observation extraction collated.
    col_names   [5] = 'OBSDIR'
    col_comments[5] = 'extraction directory'
    col_types   [5] = 7  ; STRING type
    num_cols        = 6
  
    ;; Define a column OBSDIR, the pathname of the single-observation extraction collated.
    col_names   [6] = 'MERGE_NAME'
    col_comments[6] = 'merge directory'
    col_types   [6] = 7  ; STRING type
    num_cols        = 7 
  endelse
   
    
  ;; In "pass 1" below we figure out what column names, types, and dimensions are 
  ;; needed in the output binary table, then we make a blank structure to hold that.
  ;; In "pass 2" we actually populate that structure with data from the source files.
 for pass=start_pass,2 do begin
  case pass of
   1: if (verbose GT 0) then print, 'Scanning results to define column names in output file...'
   2: begin
      if (verbose GT 0) then print, 'Reading AE results...'
  
      ;; Initialize FLOAT and DOUBLE columns to NaN so missing data is flagged.
      for jj = 0, num_cols-1 do begin
        case size(bin_table[0].(jj),/TYPE) of
          4: bin_table.(jj) = !VALUES.F_NAN
          5: bin_table.(jj) = !VALUES.D_NAN
          else:
        endcase
      endfor
  
      ;; Initialize columns we explicitly created.
      bin_table.CATALOG_NAME = sourcename
      bin_table.LABEL        = strtrim(1+indgen(num_sources),2)
      bin_table.MODEL        = "no fit"
      bin_table.NMODELS      = 0
      bin_table.PROVISNL     = 0
      bin_table.MERGE_NAME   = keyword_set(merge_name) ? merge_name : ''
      end
  endcase
  
  primary_obs_dir = strarr(num_sources)
  display_obs_dir = strarr(num_sources)
  primary_obsname = strarr(num_sources)
  display_obsname = strarr(num_sources)
  
  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be not extracted.
    if source_not_observed[ii] then continue

    sourcedir = sourcename[ii] + '/' + merge_subdir[ii] 

    ;; ------------------------------------------------------------------------
    ;; Scan the unnamed and named source.stats files for keywords.
    
    unnamed_src_stats_fn = sourcename[ii] + '/' + src_stats_basename
    src_stats_fn         = sourcedir            + src_stats_basename

    stats = headfits(src_stats_fn, ERRMSG=error)

    if keyword_set(error) then begin
      if (pass EQ 2) then print, sourcename[ii] + ' was not observed.'
      continue
    endif 
    
    
    ; If this is a named extraction, then look for an unnamed source.stats file to get fundamental source properties.
    if keyword_set(merge_subdir[ii]) && file_test(unnamed_src_stats_fn) then begin
      unnamed_stats = headfits(unnamed_src_stats_fn)
      comment = ''
      fxaddpar, stats, 'OBJECT'  , sxpar(unnamed_stats, 'OBJECT'  , COMMENT=comment), comment
      fxaddpar, stats, 'LABEL'   , sxpar(unnamed_stats, 'LABEL'   , COMMENT=comment), comment
      fxaddpar, stats, 'PROVENAN', sxpar(unnamed_stats, 'PROVENAN', COMMENT=comment), comment
      fxaddpar, stats, 'POSNDATE', sxpar(unnamed_stats, 'POSNDATE', COMMENT=comment), comment
      fxaddpar, stats, 'POSNTYPE', sxpar(unnamed_stats, 'POSNTYPE', COMMENT=comment), comment
      fxaddpar, stats, 'RA'      , sxpar(unnamed_stats, 'RA'      , COMMENT=comment), comment, F='(F10.6)'
      fxaddpar, stats, 'DEC'     , sxpar(unnamed_stats, 'DEC'     , COMMENT=comment), comment, F='(F10.6)'
    endif
    
    src_label = string(sourcename[ii], strtrim(sxpar(stats,'LABEL'),2), F='(%"Source %s (%s)")')
    
    fxhclean, stats
    sxdelpar, stats, ['DATE','COMMENT','HISTORY','EXTVER','CREATOR']
    
    ;; ------------------------------------------------------------------------
    ;; Figure out which observations should contribute extraction properties and regions.
    
    ; If the observer specified an obsname then use it, otherwise look in 
    ; the source.stats file for the deepest observation (PRIM_OBS) and the most crowded observation (WORSTOBS).
    if keyword_set(obsname) then begin
      primary_obsname[*] = obsname
      display_obsname[*] = obsname
      primary_count = 1
    endif else begin
      primary_obsname[ii] = strcompress(sxpar(stats, 'PRIM_OBS', COUNT=primary_count), /REMOVE_ALL)
    endelse

    
    if (primary_count EQ 1) then begin
      ; The source was merged.  
      if keyword_set(obsname) then begin
        ; Display the specified observation.
        display_obsname[ii] = obsname
      endif else begin
        ; If there were any crowded extractions in the merge, then show the polygon from the most crowded one.
        ; Otherwise, show the polygon from the Primary Observation.
        if (sxpar(stats, 'OVRLP_HI') GT 0) then begin
          temp = strcompress(sxpar(stats, 'WORSTOBS', COUNT=keyword_count), /REMOVE_ALL)
          display_obsname[ii] = (keyword_count EQ 1) ? temp : primary_obsname[ii]
        endif else begin
          display_obsname[ii] = primary_obsname[ii]
        endelse
      endelse
      
      ; Build the path to the primary observation, using any extraction name supplied
      primary_obs_dir[ii]   = sourcename[ii] + '/' + primary_obsname[ii] + '/' + extraction_subdir[ii]
      
      obs_stats_fn = primary_obs_dir[ii] + obs_stats_basename
      obs_stats    = headfits(obs_stats_fn, ERRMSG=error)
    
      if (NOT keyword_set(error)) then begin
        fxhclean, obs_stats
        sxdelpar, obs_stats, ['DATE','COMMENT','HISTORY','EXTVER','CREATOR']
        
        ; There can be duplicate keywords in the source header (stats) and the observation header (obs_stats).
        ; Precedence is governed by the /SINGLE_OBSID option.
        kywds = strcompress(strmid(obs_stats,0,8), /REMOVE_ALL)
        
        for jj=0,n_elements(kywds)-1 do begin
          kywd = kywds[jj]
          
          if (kywd EQ 'END') then break
          
          dum = sxpar(stats, kywd, COUNT=count)
          if ((count EQ 0) OR keyword_set(single_obsid)) then begin
            ; There's no keyword name conflict between obs.stats and source.stats, 
            ; or the observer specified that obs.stats takes precedence, so
            ; use the data from obs.stats.
            val = sxpar(obs_stats, kywd, COMMENT=comment)
            sxaddpar, stats, kywd, val, comment
          endif
        endfor ;jj
      endif else begin
        if (pass EQ 2) AND (verbose GT 0) then print,  'WARNING! '+src_label+' not extracted in observation '+primary_obsname[ii]
        ; Mark this source as unobserved so we can skip some code later.
        primary_obs_dir[ii] = ''
      endelse
    endif else begin
      ; The source was NOT merged.  
      if keyword_set(obsname) then begin
        ; Display the specified observation.
        display_obsname[ii] = obsname
      endif else begin
        ; or display region file from the "best observation" (if known).
        temp = strcompress(sxpar(stats, 'BESTOBS', COUNT=keyword_count), /REMOVE_ALL)
        display_obsname[ii] = (keyword_count EQ 1) ? temp : primary_obsname[ii]
      endelse
      
      if (pass EQ 2) AND (verbose GT 0) then print, 'WARNING! '+src_label+': no "obsname" parameter supplied, and cannot find kywd PRIM_OBS in '+src_stats_basename
    endelse
    
    ; Build the path to the display observation, using any extraction name supplied
    display_obs_dir[ii]   = sourcename[ii] + '/' + display_obsname[ii] + '/' + extraction_subdir[ii]

    
    ;; ------------------------------------------------------------------------
    ; Extract all the keywords we have gathered from source.stats and obs.stats.
    kywds = strcompress(strmid(stats,0,8), /REMOVE_ALL)
    
    for jj=0,n_elements(kywds)-1 do begin
      kywd = kywds[jj]
      
      if (kywd EQ 'END') then break
      
      val               = sxpar(stats, kywd, COMMENT=comment)
      this_type         = size(val,/TYPE)
      this_num_elements = size(val,/N_ELEMENTS)

      name     = repchr(kywd, '-', '_')
      this_col = (where(name EQ col_names, count))[0]        

      case pass of 
       1: begin
          ; We're defining the output file columns.
          ; Have we seen this column name before?
          if (count EQ 0) then begin
            ; Add a new column.
            col_names   [num_cols] = name
            col_types   [num_cols] = this_type
            col_elements[num_cols] = this_num_elements
            col_comments[num_cols] = comment
            num_cols = num_cols + 1
          endif else begin
            ; Make sure column type can represent this source's value.
            if (col_types[this_col] EQ 7) OR (this_type EQ 7) then begin 
              ;STRING (type 7) can represent anything
              col_types[this_col] = 7
            endif else begin
              ; DOUBLE (type 5) is "highest" numerical value we allow.
              col_types[this_col] = (col_types[this_col] > this_type) < 5
            endelse
            
            col_elements[this_col] = col_elements[this_col] > this_num_elements
            col_comments[this_col] = comment
          endelse
          end
          
       2: begin
          ; If MATCH_EXISTING was used to define the table columns, then there may not be one for the keyword in hand.
          if (count GT 0) then begin
            ; Extract the data.
            ; Without the [0] construct on the left side IDL will replicate a scalar on the right
            ; to fill all the elements of a vector on the left.
            temp    = bin_table[ii].(this_col)
            temp[0] = val
            bin_table[ii].(this_col) = temp
          endif
          end
      endcase
    endfor ;jj


    ;; When we're interested in single-obsid data, skip the photometry and fitting data products.
    if keyword_set(single_obsid) || (item_path EQ '/dev/') then GOTO, COLLATE_LOOPEND
    
    
    ;; ------------------------------------------------------------------------
    ;; Scan all the source.photometry files for binary table column names.
    
    photometry_fn  = sourcedir + src_photometry_basename
    if file_test(photometry_fn) then begin
      photometry_table  = mrdfits(photometry_fn, 1, photometry_hdr, /SILENT, STATUS=status)
    endif else status = 1
    
    if (status EQ 0) then begin
      flux_row          = photometry_table[0]
      this_num_elements = n_elements(photometry_table)
      tag_names = tag_names(photometry_table)
      for jj=0,n_elements(tag_names)-1 do begin
        val       = flux_row.(jj)
        
        comment   = sxpar(photometry_hdr, string(1+jj,F='(%"TUNIT%d")'), COMMENT=extra_comment)
        if (extra_comment NE '') then comment = comment + ', ' + extra_comment
        
        this_type = size(val,/TYPE)

        name     = tag_names[jj]
        this_col = where(name EQ col_names, count)
        
        case pass of 
         1: begin
            ; We're defining the output file columns.
            ; Have we seen this column name before?
            if (count EQ 0) then begin
              ; Add a new column.
              col_names   [num_cols] = name
              col_types   [num_cols] = this_type
              col_elements[num_cols] = this_num_elements
              col_comments[num_cols] = comment
              num_cols = num_cols + 1
            endif else begin
              ; Make sure column type can represent this source's value.
              if (col_types[this_col] EQ 7) OR (this_type EQ 7) then begin 
                ;STRING (type 7) can represent anything
                col_types[this_col] = 7
              endif else begin
                ; DOUBLE (type 5) is "highest" numerical value we allow.
                col_types[this_col] = (col_types[this_col] > this_type) < 5
              endelse
              
              col_elements[this_col] = col_elements[this_col] > this_num_elements
              col_comments[this_col] = comment
            endelse

            end
            
         2: begin
            ; If MATCH_EXISTING was used to define the table columns, then there may not be one for the keyword in hand.
            if (count GT 0) then begin
              ; Extract the data.
              ; Without the [0] construct on the left side IDL will replicate a scalar on the right
              ; to fill all the elements of a vector on the left.
              temp    = bin_table[ii].(this_col)
              temp[0] = photometry_table.(jj)
              bin_table[ii].(this_col) = temp
            endif
            end
        endcase
      endfor ;jj
    endif ;status


    ;; ------------------------------------------------------------------------
    ; Find the desired HDU in spectral fit results file.
    stats_fn  = sourcedir + fit_stats_basename    
    fits_open, stats_fn, fcb, /NO_ABORT, MESSAGE=error

    if keyword_set(error) then begin
;     if (pass EQ 2) AND (verbose GT 0) then print, 'WARNING! '+src_label+': No fit results available.'
      continue
    endif

    this_hduname = hduname[ii]
    
    if (this_hduname EQ 'BEST_MDL') then begin
      ; Look for preferences stored in Primary HDU.
      this_hduname = sxpar(fcb.HMAIN, 'BEST_MDL', COUNT=count)
      if (count EQ 1) then begin
        if (pass EQ 2) then bin_table[ii].PROVISNL = sxpar(fcb.HMAIN, 'PROVISNL')
      endif else begin
        count = 0
        this_hduname = ''
        if (pass EQ 2) then print, 'WARNING! '+src_label+': FITS keyword BEST_MDL was not found. '
      endelse
    endif
    
    if (this_hduname EQ '') then begin
      ; Use the last HDU in the file.
      ind   = n_elements(fcb.EXTNAME) - 1
      count = 1
      if (pass EQ 2) AND (verbose GT 0) then print, src_label, fcb.EXTNAME[ind], F='(%"%s: using the most recent spectral model: %s")'
    endif else begin
      ; Find the last HDU matching the specified model name spec.
      ind = where( strmatch(fcb.EXTNAME, this_hduname, /FOLD_CASE), count )
      
      if (count GT 1) then begin
        ind = ind[count-1]
        if (pass EQ 2) AND (verbose GT 0) then print, src_label, count, this_hduname, fcb.EXTNAME[ind],  F='(%"%s: %d spectral models match %s; using the most recent: %s )")'
        count = 1
      endif
    endelse
    
    if (count EQ 0) then begin
      if (pass EQ 2) AND (verbose GT 0) then print, src_label, this_hduname, F='(%"WARNING! %s: Cannot find any spectral model matching ''%s''")'
    endif else begin
      fits_read, fcb, dummy, stats, /NO_PDU, /HEADER_ONLY, EXTEN_NO=ind[0], /NO_ABORT, MESSAGE=error

      if (keyword_set(error)) then begin 
        if (pass EQ 2) then print, 'WARNING! Error reading '+stats_fn
      endif else begin
        if (pass EQ 2) then begin
          bin_table[ii].MODEL   = fcb.EXTNAME[ind]
          bin_table[ii].NMODELS = fcb.NEXTEND
        endif
        
        fxhclean, stats
        sxdelpar, stats, ['DATE','COMMENT','HISTORY','EXTVER','CREATOR','EXTNAME']
          
        kywds = strcompress(strmid(stats,0,8), /REMOVE_ALL)
          
        for jj=0,n_elements(kywds)-1 do begin
          kywd = kywds[jj]
          
          if (kywd EQ 'END') then break
          
          val               = sxpar(stats, kywd, COMMENT=comment)
          this_type         = size(val,/TYPE)
          this_num_elements = size(val,/N_ELEMENTS)
  
          name     = repchr(kywd, '-', '_')
          this_col = where(name EQ col_names, count)        
  
          case pass of 
           1: begin
              ; We're defining the output file columns.
              ; Have we seen this column name before?
              if (count EQ 0) then begin
                ; Add a new column.
                col_names   [num_cols] = name
                col_types   [num_cols] = this_type
                col_elements[num_cols] = this_num_elements
                col_comments[num_cols] = comment
                num_cols = num_cols + 1
              endif else begin
                ; Make sure column type can represent this source's value.
                if (col_types[this_col] EQ 7) OR (this_type EQ 7) then begin 
                  ;STRING (type 7) can represent anything
                  col_types[this_col] = 7
                endif else begin
                  ; DOUBLE (type 5) is "highest" numerical value we allow.
                  col_types[this_col] = (col_types[this_col] > this_type) < 5
                endelse
                
                col_elements[this_col] = col_elements[this_col] > this_num_elements
                col_comments[this_col] = comment
              endelse
              end
              
           2: begin
            ; If MATCH_EXISTING was used to define the table columns, then there may not be one for the keyword in hand.
            if (count GT 0) then begin
                ; Extract the data.
                ; Without the [0] construct on the left side IDL will replicate a scalar on the right
                ; to fill all the elements of a vector on the left.
                temp    = bin_table[ii].(this_col)
                temp[0] = val
                bin_table[ii].(this_col) = temp
              endif
              end
          endcase
        endfor ;jj
      endelse ;(no error)
    endelse ;(count GT 0)

    fits_close, fcb

COLLATE_LOOPEND:    
  endfor ;ii (loop over sources)


  ;; ------------------------------------------------------------------------
  ;; Compute distances between source locations.
  if (item_path NE '/dev/') then begin
    case pass of
     1: begin
        ;; Add the distance_src2src column computed here.
        col_names   [num_cols] = 'distance_src2src'
        col_comments[num_cols] = 'distance between sources (sky pixels)'
        num_cols = num_cols + 1

        ;; Add the distance_reg2reg column computed here.
        col_names   [num_cols] = 'distance_reg2reg'
        col_comments[num_cols] = '~distance between source regions (sky pixels)'
        num_cols = num_cols + 1

        ;; Add the neighbor column computed here.
        col_names   [num_cols] = 'neighbor'
        col_comments[num_cols] = '0-based index of nearest neighbor'
        col_types   [num_cols] = 3 ; LONG type
        num_cols = num_cols + 1
        end
      
     2: begin
        dum = where(col_names EQ 'X_CAT', count)
        if (count GT 0) then begin
          if (verbose GT 0) then print, 'computing distances between sources'
          ;; For each source region, find which source has a region most overlapping.
          ;; Note the the "neighbor" relationship is NOT symmetric, e.g. A's most overlapping neighbor may be B, and B's most overlapping neighbor may be C!
          src_radius       = bin_table.SRC_RAD
          distance_src2src = fltarr(num_sources)
          distance_reg2reg = fltarr(num_sources)
          neighbor         = lonarr(num_sources)
          deghour          = 24D/360
          ra_hrs           = bin_table.RA*deghour
          dec_degrees      = bin_table.DEC
          for ii = 0L, num_sources-1 do begin
            ; Compute distances between this source and neighbors in units of skypix.
            ; Earlier AE versions avoided a loop in the distance computation, but for large catalogs the IDL process exceeded its 4GB memory limit.
            gcirc, 1, ra_hrs, dec_degrees,  ra_hrs[ii], dec_degrees[ii],  distance_to_neighbors ; distance in arcseconds
            distance_to_neighbors    /= arcsec_per_skypixel                                     ; convert to skypix
            distance_to_neighbors[ii] = !VALUES.F_NAN                                           ; ignore yourself
          
            distance_src2src[ii] = min(/NAN, distance_to_neighbors)
            distance_reg2reg[ii] = min(/NAN, distance_to_neighbors - src_radius - src_radius[ii], ind)
            neighbor        [ii] = ind
          endfor   
          
          bin_table.distance_src2src = distance_src2src
          bin_table.distance_reg2reg = distance_reg2reg
          bin_table.neighbor         = neighbor
        endif ;'X_CAT' available
        end
    endcase
  endif ; (item_path NE '/dev/')
    
      
  ;; ------------------------------------------------------------------------
  ;; Construct structure array.
  if (pass EQ 1) then begin      
    ;; Construct the empty output binary table.
    format_codes = ['B','B','I','J','F','D','C','A']
    repeat_count = string(col_elements[0:num_cols-1], F='(I0)')
    ind = where(repeat_count EQ '1', count)
    if (count GT 0) then repeat_count[ind] = ''
    
    tag_descript = strjoin(repeat_count + format_codes[col_types], ',')
    
    ; The create_struct routine writes a temp file; to avoid conflict we run it from our temp dir.
    pushd, tempdir
    create_struct, bin_table, '', col_names[0:num_cols-1], tag_descript, DIMEN=num_sources
    popd
  endif ;(pass EQ 1)
 endfor ;pass loop

 bin_table.OBSDIR       = primary_obs_dir


  ;; ------------------------------------------------------------------------
  ;; Write an empty primary HDU.
  fxhmake, pheader, /EXTEND, /DATE, /INITIALIZE
  fxaddpar, pheader, 'CREATOR', creator_string  

  fxaddpar, pheader, "FNFITS", item_name
  
  if (item_path NE '/dev/') then writefits, collated_filename, 0, pheader

  
  ;; ------------------------------------------------------------------------
  ;; Now finally we can write out this giant structure array.
  if NOT keyword_set(match_existing) then begin
    fxbhmake, theader, num_sources, 'EXTRACTION RESULTS', /DATE
    
    ; Binary table columns can't have "comments", so we'll store those in TUNIT* keywords.
    for jj = 0, num_cols-1 do begin
      fxaddpar, theader, string(1+jj,F='(%"TUNIT%d")'), col_comments[jj]
    endfor
  endif

  get_date, date_today, /TIMETAG
  fxaddpar, theader, 'DATE', date_today
  fxaddpar, theader, 'CREATOR', creator_string  
  
  if (item_path NE '/dev/') then mwrfits, bin_table, collated_filename, theader
  
  dum = where(col_names EQ 'OBJECT', count)
  if (count GT 0) then begin
    object = strtrim(bin_table.OBJECT,2)
    ind = where((object NE '') AND (object NE '0') AND (sourcename NE object), count)
    if (count GT 0) then begin
      print, 'WARNING!  The following catalog source names (directory names) do not match the OBJECT keyword.'
      forprint, sourcename[ind], bin_table[ind].OBJECT, F='A,4x,A'
    endif
  endif

  
  ;; ------------------------------------------------------------------------
  ;; Write a table of source names and labels.
  LABEL = strtrim(bin_table.LABEL,2)
  if keyword_set(label_filename) then begin
    forprint, TEXTOUT=label_filename, /NoCOMMENT, sourcename, LABEL, SUBSET=sort(LABEL), F='(%"%s (%s)")'
  endif

  if (num_sources GT 1) then begin
    ;; Look for duplicate source labels.
    sort_index    = sort(LABEL)
    sorted_labels = LABEL[sort_index]
    
    uniq_index = uniq(sorted_labels)
    num_uniq   = n_elements(uniq_index)
    if (num_uniq LT num_sources) then begin
      print, F='(%"\n=============================================================================")'
      print, 'WARNING: The catalog contains duplicate source labels:'
    
      next = 0
      for ii=0L,num_uniq-1 do begin
        this = uniq_index[ii]
        if (this GT next) then begin
          print
          for jj=next,this do begin
            duplicate_index = sort_index[jj]
            print, sourcename[duplicate_index], LABEL[duplicate_index], F='(%"%s (%s)")'
          endfor
        endif ; (this GT next)
        
        next = this+1
      endfor
      
      print, '============================================================================='
    endif ;duplicates
  endif
  
  
  ;; ------------------------------------------------------------------------
  ;; Collate ds9 regions across the catalog.
  if keyword_set(region_file) then begin
    case n_elements(region_tag) of
      0:           user_tag_string = strarr(num_sources)
      1:           user_tag_string = 'tag={'+replicate(region_tag,num_sources)+'}'
      num_sources: user_tag_string = 'tag={'+region_tag+'}'
      else: begin
            print, 'ERROR: parameter USER_TAG_STRING must be a scaler or a vector as long as the catalog'
            GOTO, FAILURE
            end
    endcase
    
    if (total(strmatch(col_names,'PROVENAN')) EQ 1) then begin
      user_tag_string = 'tag={PROVENAN:'+strtrim(bin_table.PROVENAN,2)+ '} ' + user_tag_string
    endif
    
    if (verbose GT 0) then print, 'Building region file ...'
    color = replicate('green', num_sources)
    openw,  region1_unit, region_file, /GET_LUN
    printf, region1_unit, "# Region file format: DS9 version 3.0"
    !TEXTUNIT = region1_unit
  
    printf, region1_unit, F='(%"# Catalog (crosses), Data Mean (diamonds), Correlation Peak (circles), and Reconstruction Peak (boxes) positions")'
    
   
    ; Catalog positions.
    ind = where(finite(bin_table.RA) AND finite(bin_table.DEC), count)
    if (count GT 0) then $
      forprint, TEXTOUT=5, /NoCOM, SUBSET=ind, bin_table.RA, bin_table.DEC, LABEL, user_tag_string, color, F='(%"J2000;cross   point %10.6f %10.6f # text={%s} tag={cat} %s color=%s")'
    
    dec_offset = 1 / 3600.   ; 1 arcsec
    ra_offset  = dec_offset / cos(bin_table.DEC*!DTOR)
    
    ; Labels.
    ;forprint, TEXTOUT=5, /NoCOM, bin_table.RA+ra_offset, bin_table.DEC-dec_offset, LABEL, user_tag_string, color, F='(%"J2000;text %10.6f %10.6f # text={%s} tag={label} %s color=%s")'
      
    ; PSF fractions
    if (total(strmatch(col_names,'PSF_FRAC')) EQ 1) then begin
      ind = where(finite(bin_table.PSF_FRAC), count)
      if (count GT 0) then $
        forprint, TEXTOUT=5, /NoCOM, SUBSET=ind, bin_table.RA+ra_offset, bin_table.DEC, 100*bin_table.PSF_FRAC, user_tag_string, color, F='(%"J2000;text %10.6f %10.6f # text={%d%%} tag={fraction} %s color=%s")' 
    endif
 
    ; Data positions.
    if (total(strmatch(col_names,'RA_DATA') OR strmatch(col_names,'DEC_DATA')) EQ 2) then begin
      ind = where(finite(bin_table.RA_DATA) AND finite(bin_table.DEC_DATA), count)
      if (count GT 0) then $
        forprint, TEXTOUT=5, /NoCOM, SUBSET=ind, bin_table.RA_DATA, bin_table.DEC_DATA, user_tag_string, color, F='(%"J2000;diamond point %10.6f %10.6f # tag={data} %s color=%s")'
    endif
    
    ; Correlation positions.
    if (total(strmatch(col_names,'RA_CORR') OR strmatch(col_names,'DEC_CORR')) EQ 2) then begin
      ind = where(finite(bin_table.RA_CORR) AND finite(bin_table.DEC_CORR), count)
      if (count GT 0) then $
        forprint, TEXTOUT=5, /NoCOM, SUBSET=ind, bin_table.RA_CORR, bin_table.DEC_CORR, user_tag_string, color, F='(%"J2000;circle  point %10.6f %10.6f # tag={corr} %s color=%s")'
    endif
    
    ; ML positions.
    if (total(strmatch(col_names,'RA_ML') OR strmatch(col_names,'DEC_ML')) EQ 2) then begin
      ind = where(finite(bin_table.RA_ML) AND finite(bin_table.DEC_ML), count)
      if (count GT 0) then $
        forprint, TEXTOUT=5, /NoCOM, SUBSET=ind, bin_table.RA_ML, bin_table.DEC_ML, user_tag_string, color,     F='(%"J2000;box     point %10.6f %10.6f # tag={ml} %s color=%s")'
    endif
    
    
    ; Polygons
    for ii = 0, num_sources-1 do begin
      
      src_events_fn = display_obs_dir[ii] + src_events_basename
      region_fn     = display_obs_dir[ii] + src_region_basename
      
      if ~file_test(src_events_fn) || ~file_test(region_fn) then continue
      
      ; Build astrometic structure from event data header.
      theader = headfits( src_events_fn, EXT=1, ERRMSG=error )
      if (keyword_set(error)) then message, 'ERROR reading ' + src_events_fn
      
      fxbfind, theader, 'TTYPE', dum1, TTYPE, dum2, 'null'
      fxbfind, theader, 'TCTYP', dum1, TCTYP, dum2, 'null'
      fxbfind, theader, 'TCRVL', dum1, TCRVL, dum2, 0.0D
      fxbfind, theader, 'TCRPX', dum1, TCRPX, dum2, 0.0D
      fxbfind, theader, 'TCDLT', dum1, TCDLT, dum2, 0.0D
      colnames = strlowcase( strtrim(TTYPE,2) )
      x_ind    = where(strlowcase(colnames) EQ 'x')
      y_ind    = where(strlowcase(colnames) EQ 'y')
      make_astr, event2wcs_astr, DELTA=TCDLT[[x_ind,y_ind]], CTYPE=TCTYP[[x_ind,y_ind]], $
                                 CRPIX=TCRPX[[x_ind,y_ind]], CRVAL=TCRVL[[x_ind,y_ind]]
  
      ;; Convert polygon to WCS & write to the region file.      
      ae_ds9_to_ciao_regionfile, region_fn, '/dev/null', /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y
      
      if (n_elements(polygon_x) GT 1) then begin
        xy2ad, polygon_x-1, polygon_y-1, event2wcs_astr, polygon_ra, polygon_dec
        
        polygon = dblarr(2,n_elements(polygon_ra))
        polygon[0,*] = polygon_ra
        polygon[1,*] = polygon_dec
    
        src_region = 'polygon(' + strcompress(strjoin(string(polygon,F='(F10.6)'),","), /REMOVE) + ')'
        
        printf, region1_unit, src_region, display_obsname[ii], user_tag_string[ii], color[ii], F='(%"J2000;%s # move=0 edit=0 tag={obs%s} %s color=%s")' 
      endif
    endfor ; ii loop
    
    free_lun, region1_unit
  endif

  if (verbose GT 0) then begin
    print, '============================================================================='
    if (item_path NE '/dev/') then begin
      print, 'Wrote source properties to ', collated_filename
      print, 'Column names are: ', tag_names(bin_table)
    endif
    if keyword_set(region_file) then print, 'Wrote source regions to ', region_file
    print, '============================================================================='
  endif


  GOTO, CLEANUP
endif ;keyword_set(collated_filename)


; We initialize run_command here so the COLLATE stage will run faster.
; The tempdir must appear before the cache_dir in PFILES so that we can whack parameter files in tempdir and re-create fresh ones for each sources.
run_command, /INIT, PARAM_DIR=[tempdir,cache_dir]



;; =============================================================================
if keyword_set(plot) AND keyword_set(construct_regions) then begin
;; =============================================================================

  show_psfs = 1
  if show_psfs AND (num_sources LE 25) then begin
    ;; Display PSF images.
    
    obsdir    = sourcename + '/' + obsname + '/' + extraction_subdir
    psf_fn    = sourcename + '/' + obsname + '/' + psf_basename
    image_fn  = tempdir + sourcename
    region_fn = obsdir  + src_region_basename
    
    hdu = 0
    while 1 do begin
      print, 'Enter 0 to skip ds9 display of PSF images, OR'
      read, 'Enter desired HDU [1..5] in PSF file = {1.5, 0.277, 4.51, 6.4, 8.6} keV : ', hdu
      if (hdu LT 1) OR (hdu GT 5) then break
    
      ; Copy each PSF to a temp file named after the source so ds9 will show the name.
      infile  = psf_fn + string(hdu,F='(%"[PSF%d]")')
      outfile = strjoin(image_fn, ' ')
      cmd = string(strjoin(infile, ' '), outfile, F="(%'dmcopy ""%s"" ""%s""')")
      run_command, cmd

      
      ; Fake WCS so images can be aligned in ds9
      for ii = 0, num_sources-1 do begin
        header = headfits(image_fn[ii])
        fxaddpar, header, 'CRVAL1', 0
        fxaddpar, header, 'CRVAL2', 0

        fxaddpar, header, 'CRPIX1', sxpar(header, 'NAXIS1')/2.
        fxaddpar, header, 'CRPIX2', sxpar(header, 'NAXIS2')/2.
        
        modfits, image_fn[ii], 0, header
      endfor
      
      if NOT keyword_set(my_ds9) then ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name
    
      ae_send_to_ds9, my_ds9, image_fn, region_fn
    endwhile
  endif
  
  f_nan = replicate(!VALUES.F_NAN,num_sources)

  xpos_catalog    = f_nan
  ypos_catalog    = f_nan
  off_angle       = f_nan
  psf2cat_offset  = f_nan
  target_psf_fraction= f_nan
  psf_fraction    = f_nan
  src_radius      = f_nan
  pgn_area        = f_nan
  cropfrac277     = f_nan
  cropfrac8       = f_nan
  skypixel_per_psfpixel = f_nan
  
  ;; Read summary information from stats file for each source.
  
  print, 'reading source information'
  for ii = 0, num_sources-1 do begin
    sourcedir = sourcename[ii] + '/'                 + merge_subdir[ii] 
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    psf_fn    = sourcename[ii] + '/' + obsname + '/' + psf_basename
    src_stats_fn = sourcedir + src_stats_basename
    obs_stats_fn =    obsdir + obs_stats_basename

    
    header = headfits(psf_fn, ERRMSG=error, EXT=0)
    if (NOT keyword_set(error)) then begin
      skypixel_per_psfpixel[ii] = sxpar(header, 'CDELT1P')
    endif

    header = headfits(psf_fn, ERRMSG=error, EXT=1)
    if (NOT keyword_set(error)) then cropfrac277[ii] = sxpar(header, 'CROPFRAC')

    header = headfits(psf_fn, ERRMSG=error, EXT=4)
    if (NOT keyword_set(error)) then cropfrac8  [ii] = sxpar(header, 'CROPFRAC')

    
    ; Label the source with the LABEL property if available, or sequence number otherwise.
    src_stats = headfits(src_stats_fn, ERRMSG=error)
    if (NOT keyword_set(error)) then begin
      label  = sxpar(src_stats, 'LABEL', COUNT=count)
    endif else count=0
    
    if (count EQ 0) then label = string(1+ii)


    obs_stats    = headfits(obs_stats_fn, ERRMSG=error)
    if (NOT keyword_set(error)) then begin
      xpos_catalog   [ii] = sxpar(obs_stats, 'X_CAT')
      ypos_catalog   [ii] = sxpar(obs_stats, 'Y_CAT')
      off_angle      [ii] = sxpar(obs_stats, 'THETA')
      psf2cat_offset [ii] = sxpar(obs_stats, 'PSF2CAT')
      target_psf_fraction[ii] = sxpar(obs_stats, 'FRACSPEC')
      psf_fraction   [ii] = sxpar(obs_stats, 'PSF_FRAC')
      src_radius     [ii] = sxpar(obs_stats, 'SRC_RAD')
      pgn_area       [ii] = sxpar(obs_stats, 'PGN_AREA')
    endif
  endfor ;ii
  
  ;; Compute distances between source locations.
  print, 'computing distances between sources'
  make_2d, xpos_catalog, xpos_catalog, xpos_i, xpos_j
  make_2d, ypos_catalog, ypos_catalog, ypos_i, ypos_j
  
  distance_sqr = (xpos_i-xpos_j)^2. + (ypos_i-ypos_j)^2.
  src_num = indgen(num_sources)
  distance_sqr[src_num,src_num] = 1E10
  
  ;; For each source, find nearest neighbor.
  distance_src2src = fltarr(num_sources)
  neighbor = lonarr(num_sources)
  for ii = 0, num_sources-1 do begin
    distance_src2src[ii] = sqrt(min(distance_sqr[*,ii],ind,/NAN))
    neighbor[ii] = ind
  endfor   
  
  ;; Estimate distances between source extraction regions.
  distance_reg2reg = distance_src2src - src_radius - src_radius[neighbor]

  ;; Show summary information.
  tit = 'Proposed Extraction Regions for dataset "' + obsname + '"'
  dataset_2d, id1, NAN=[0,0], PSY=1, off_angle, pgn_area,   TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='Area of Source Region (sky pixels^2) via polyfillv (IDL)'

  dataset_2d, id6, NAN=[0,0], PSY=1, off_angle, psf2cat_offset, TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='PSF/Catalog Offset (sky pixels)'
  
  
  sn = indgen(num_sources)
  function_1d, id5, LI=6,PSY=4, sn, psf_fraction, DATASET='Actual', TITLE=tit, XTIT='Source # (0-based)', YTIT='PSF Fraction' 
  function_1d, id5, LI=6,PSY=1, sn, target_psf_fraction, DATASET='Requested'

  function_1d, id7, LI=6,PSY=1, sn, distance_reg2reg, TITLE=tit, XTIT='Source # (0-based)', YTIT='~Distance Between Source Regions (sky pixels)'

  x=intarr(num_sources)  &  y=x
  ind = where(distance_reg2reg LT 2, count)
  if (count GT 0) then begin
    x[ind] = sn[ind]
    y[ind] = neighbor[ind]
    dataset_2d, id8, NAN=[0,0], PSY=1, x, y, TITLE='Pairs of Crowded Sources', XTIT='Source # (0-based)', YTIT='Source # (0-based)'
  endif

  tit = 'Summary of PSF Images'
  dataset_2d, id2, NAN=[0,0], PSY=1, off_angle, cropfrac277, DATASET='277 eV', TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='CROPFRAC'
      
  dataset_2d, id2, NAN=[0,0], PSY=6, off_angle, cropfrac8  , DATASET='8 keV'

  dataset_2d, id3, NAN=[0,0], PSY=1, off_angle, skypixel_per_psfpixel, TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='PSF pixel size (sky pixels)'

  
  savefile = 'construct_regions_'+obsname+'.sav'
  save, sourcename, xpos_catalog, ypos_catalog, off_angle, psf2cat_offset, target_psf_fraction, psf_fraction, src_radius, pgn_area, distance_src2src, distance_reg2reg, neighbor, FILE=savefile

  print, '============================================================================='
  print, 'IDL vectors saved in ', savefile
  print, '============================================================================='
  GOTO, CLEANUP
endif


;; =============================================================================
if keyword_set(plot) AND keyword_set(check_positions) then begin
;; =============================================================================

  f_nan = replicate(!VALUES.F_NAN,num_sources)
  d_nan = replicate(!VALUES.D_NAN,num_sources)

  ra             = d_nan
  dec            = d_nan
  ra_ml          = d_nan
  dec_ml         = d_nan
  ra_corr        = d_nan
  dec_corr       = d_nan
  ra_data        = d_nan
  dec_data       = d_nan
  quantization_corr= f_nan
  src_counts     = f_nan
  src_radius     = f_nan
  off_angle      = f_nan
    
  ;; Read summary information from stats file for each source.
  print, 'reading source information'
  for ii = 0, num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii]
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra        [ii] = sxpar(stats, 'RA')
      dec       [ii] = sxpar(stats, 'DEC')
      quantization_corr[ii] = sxpar(stats, 'QUANTCOR')
      ra_ml     [ii] = sxpar(stats, 'RA_ML')
      dec_ml    [ii] = sxpar(stats, 'DEC_ML')
      ra_corr   [ii] = sxpar(stats, 'RA_CORR')
      dec_corr  [ii] = sxpar(stats, 'DEC_CORR')
      ra_data   [ii] = sxpar(stats, 'RA_DATA')
      dec_data  [ii] = sxpar(stats, 'DEC_DATA')

      off_angle [ii] = sxpar(stats, 'THETA')
      src_counts[ii] = sxpar(stats, 'SRC_CNTS')
      src_radius[ii] = sxpar(stats, 'SRC_RAD')
    endif else print, 'WARNING! Could not read '+stats_fn
  endfor  
  
  ;; Compute distance between catalog, correlation, & data positions.
  deghour = 24D/360
  gcirc, 1, ra*deghour,      dec,      ra_corr*deghour, dec_corr, cat2corr_offset
  gcirc, 1, ra*deghour,      dec,      ra_data*deghour, dec_data, cat2data_offset
  gcirc, 1, ra_data*deghour, dec_data, ra_corr*deghour, dec_corr, corr2data_offset
  
  ; The gcirc routine doesn't propagate NaN's, so we have to deal with them explicitly.
  ; We also have to detect missing (zero) coordinates.
  ind = where((finite(ra) AND (ra GT 0)) EQ 0, count)
  if (count GT 0) then begin
    cat2corr_offset [ind] = !VALUES.F_NAN
    cat2data_offset [ind] = !VALUES.F_NAN
  endif
  ind = where((finite(ra_data) AND (ra_data GT 0)) EQ 0, count)
  if (count GT 0) then begin
    corr2data_offset[ind] = !VALUES.F_NAN
    cat2data_offset [ind] = !VALUES.F_NAN
  endif
  ind = where((finite(ra_corr) AND (ra_corr GT 0)) EQ 0, count)
  if (count GT 0) then begin
    cat2corr_offset [ind] = !VALUES.F_NAN
    corr2data_offset[ind] = !VALUES.F_NAN
  endif
  
  ; Mark sources where no DATA position available (SRC_CNTS==0).
  ind = where(src_counts EQ 0, count)
  if (count GT 0) then begin
    cat2corr_offset [ind]=-1
    cat2data_offset [ind]=-1
    corr2data_offset[ind]=-1
  endif
      
  ;; Compute sky positions for an arbitrary sky (x,y) system with 
  ;; 0.5" pixels centered on the mean source position.
  make_astr, composite_astr, DELTA=[-0.000136667D,0.000136667D],  $
                             CRPIX=[0D,0D], CRVAL=[mean(ra,  /DOUBLE),mean(dec, /DOUBLE)]

  ad2xy, ra,      dec,      composite_astr, xpos_catalog, ypos_catalog
  ad2xy, ra_corr, dec_corr, composite_astr, xpos_corr,    ypos_corr
  
  ;; Show summary information.
  color_manager
  ;; ------------------------------------------------------------------------
  ; It's important to make the partvelvec plot have unity aspect.
  ; Estimate the plot region size in device units.
    xrange = minmax(xpos_catalog)
    yrange = minmax(ypos_catalog)
    wset,0
    xlen_est = !D.X_SIZE - !D.X_CH_SIZE * total( !X.margin )
    ylen_est = !D.Y_SIZE - !D.Y_CH_SIZE * total( !Y.margin )
  
    ; Enlarge the axis ranges to center desired region and have 1-1 aspect.
    pixel_size = max( [(xrange[1] - xrange[0]) / xlen_est, $
                       (yrange[1] - yrange[0]) / ylen_est] )
                
    xrange = ((xrange[0]+xrange[1]) / 2.) + $
                        pixel_size * xlen_est * [-0.5,0.5]
    
    yrange = ((yrange[0]+yrange[1]) / 2.) + $
                        pixel_size * ylen_est * [-0.5,0.5]

    window,xsize=1000,ysize=800
    partvelvec, xpos_corr-xpos_catalog, ypos_corr-ypos_catalog, xpos_catalog, ypos_catalog, TITLE='Vector (length magnified) from Catalog to Correlation Position', XRANGE=xrange, YRANGE=yrange, XSTYLE=1, YSTYLE=1, LENGTH=0.12


  ;; ------------------------------------------------------------------------
  tit = 'Comparison of Position Estimates'
;  dataset_2d, id1, NAN=[0,0], off_angle, cat2corr_offset,  PSYM=1, DATASET_NAME='Catalog to Correlation Peak', TITLE=tit, XTIT='Average Off-axis Angle (arcmin)', YTIT='Offset (arcseconds)'
;  dataset_2d, id1, NAN=[0,0], off_angle, cat2data_offset,  PSYM=6, DATASET_NAME='Catalog to Data Mean'
;  dataset_2d, id1, NAN=[0,0], off_angle, corr2data_offset, PSYM=4, DATASET_NAME='Data Mean to Correlation Peak'

  count_bins = [0,20,200,1E10] < max(src_counts)
  colors     = ['blue', 'red', 'green', 'white']
  for ii=0,n_elements(count_bins)-2 do begin
    mask = (count_bins[ii]+1 LE src_counts) AND (src_counts LE count_bins[ii+1])
    if (total(mask) GT 0) then begin
      name = string(count_bins[ii]+1, count_bins[ii+1], F='(%"%d <= source_counts <= %d")')
      
      dataset_2d, id2, NAN=[0,0], PSY=1, COLOR=colors[ii],   off_angle*mask, cat2corr_offset* mask, DATASET=name+', Catalog to Correlation Peak', TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='Offset (arcseconds)'
      
      dataset_2d, id2, NAN=[0,0], PSY=6, COLOR=colors[ii], off_angle*mask, cat2data_offset* mask, DATASET=name+', Catalog to Data Mean'
      
      dataset_2d, id2, NAN=[0,0], PSY=4, COLOR=colors[ii], off_angle*mask, corr2data_offset*mask, DATASET=name+', Data Mean to Correlation Peak'
    endif
  endfor
  
  dataset_1d, id9, BINSIZE=0.1, cat2corr_offset,  DATASET='Catalog to Correlation Peak', DENSITY_TITLE=tit, XTIT='Offset (arcseconds)'
  dataset_1d, id9, BINSIZE=0.1, cat2data_offset,  DATASET='Catalog to Data Mean'
  dataset_1d, id9, BINSIZE=0.1, corr2data_offset, DATASET='Data Mean to Correlation Peak'
  
  sn = indgen(num_sources)
  function_1d, id7, LI=6,PSY=1, COLOR='white', sn, cat2corr_offset,  DATASET='Catalog to Correlation Peak', TITLE=tit, XTIT='Source # (0-based)', YTIT='Offset (arcseconds)'
  function_1d, id7, LI=6,PSY=6, COLOR='white', sn, cat2data_offset,  DATASET='Catalog to Data Mean'
  function_1d, id7, LI=6,PSY=4, COLOR='white', sn, corr2data_offset, DATASET='Data Mean to Correlation Peak'
    
  ;; ------------------------------------------------------------------------
  save, sourcename, ra, dec, ra_ml, dec_ml, ra_corr, dec_corr, ra_data, dec_data, cat2corr_offset, cat2data_offset, corr2data_offset,  quantization_corr, off_angle, src_counts, src_radius, FILE='check_positions.sav'
  
  print, '============================================================================='
  print, 'IDL vectors saved in "check_positions.sav".'
  print, '============================================================================='
  GOTO, CLEANUP
endif


;; =============================================================================
if keyword_set(plot) AND $
   (keyword_set(extract_events) OR keyword_set(extract_spectra)) then begin
;; =============================================================================

  f_nan = replicate(!VALUES.F_NAN,num_sources)

  ra             = dblarr(num_sources)
  dec            = dblarr(num_sources)
  xpos_catalog   = f_nan
  ypos_catalog   = f_nan
  
  
  xpos_data      = f_nan
  ypos_data      = f_nan
  er_xpos_data   = f_nan
  er_ypos_data   = f_nan
  cat2data_offset= f_nan
  
  region_edited  = bytarr(num_sources)
  psf_fraction         = f_nan
  psf_fraction_min     = f_nan
  psf_fraction_max     = f_nan
  primary_ccd_fraction = f_nan
  arf_mean       = f_nan
  num_emap_pixels= f_nan
  mean_exposure  = f_nan
  median_exposure= f_nan
  min_exposure   = f_nan
  max_exposure   = f_nan
  pgn_area       = f_nan
  src_area       = f_nan
  src_counts     = f_nan
  probks         = f_nan
  off_angle      = f_nan
  warnfrac       = f_nan
  print, 'reading source information'

  ;; Read summary information from stats file for each source.
  for ii = 0, num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii] 
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra             [ii] = sxpar(stats, 'RA')
      dec            [ii] = sxpar(stats, 'DEC')
    endif else print, 'WARNING! Could not read '+stats_fn

    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn  = obsdir + obs_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      xpos_catalog        [ii] = sxpar(stats, 'X_CAT')
      ypos_catalog        [ii] = sxpar(stats, 'Y_CAT')

      xpos_data           [ii] = sxpar(stats, 'X_DATA')
      ypos_data           [ii] = sxpar(stats, 'Y_DATA')
      er_xpos_data        [ii] = sxpar(stats, 'EX_DATA')
      er_ypos_data        [ii] = sxpar(stats, 'EY_DATA')
      cat2data_offset     [ii] = sxpar(stats, 'CAT2DATA')

      region_edited       [ii] = sxpar(stats, 'REG_EDIT')
      psf_fraction        [ii] = sxpar(stats, 'PSF_FRAC')
      primary_ccd_fraction[ii] = sxpar(stats, 'CCD_FRAC')
      arf_mean            [ii] = sxpar(stats, 'MEAN_ARF')
      num_emap_pixels     [ii] = sxpar(stats, 'EMAP_NUM')
      mean_exposure       [ii] = sxpar(stats, 'EMAP_AVG')
      median_exposure     [ii] = sxpar(stats, 'EMAP_MED')
      min_exposure        [ii] = sxpar(stats, 'EMAP_MIN')
      max_exposure        [ii] = sxpar(stats, 'EMAP_MAX')
      pgn_area            [ii] = sxpar(stats, 'PGN_AREA')
      src_area            [ii] = sxpar(stats, 'SRC_AREA')
      src_counts          [ii] = sxpar(stats, 'SRC_CNTS')
      probks              [ii] = sxpar(stats, 'PROB_KS')
      off_angle           [ii] = sxpar(stats, 'THETA')
      warnfrac            [ii] = sxpar(stats, 'WARNFRAC')

      psf_frac_fn = obsdir + obs_frac_basename
      t = mrdfits(psf_frac_fn, 1, /SILENT, STATUS=status)
      
      if (status EQ 0) then begin
        psf_fraction_min    [ii] = min(t.fraction)
        psf_fraction_max    [ii] = max(t.fraction)
      endif 
    endif


  endfor
  

  ;; Show summary information.
  tit = 'Source Extraction Regions for dataset "' + obsname + '"'
  sn = indgen(num_sources)
  function_1d, id2, LI=6,PSY=1, sn, region_edited,     TITLE=tit, XTIT='Source # (0-based)', YTIT='Source Region Edited in ds9 (T/F)'
  function_1d, id3, LI=6,PSY=1, sn, pgn_area,          TITLE=tit, XTIT='Source # (0-based)', YTIT='Area of Source Region (sky pixels^2)', DATASET='via polyfillv (IDL)'

  dataset_1d, id4, BINSIZE=1, src_counts, DENSITY_TITLE=tit,      XTIT='In-band Counts in Source Region'

; dataset_2d, id5, NAN=[0,0], PSY=1, src_counts, cat2data_offset, TITLE=tit, XTIT='In-band Counts in Source Region', YTIT='Catalog/Data Offset (sky pixels)'

  count_bins = [0,20,200,1E10] < max(src_counts)
  colors     = ['blue', 'red', 'green', 'white']
  for ii=0,n_elements(count_bins)-2 do begin
    mask = (count_bins[ii]+1 LE src_counts) AND (src_counts LE count_bins[ii+1])
    if (total(mask) GT 0) then begin
      name = string(count_bins[ii]+1, count_bins[ii+1], F='(%"%d <= source_counts <= %d")')
      
      dataset_2d, id6, NAN=[0,0], PSY=6, COLOR=colors[ii], off_angle*mask, cat2data_offset*mask, DATASET=name, TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='Catalog/Data Offset (sky pixels)'
    endif
  endfor

  function_1d, id9, LI=6,PSY=6, sn, psf_fraction_max, COLOR='red',   DATASET='maximum',  TITLE=tit, XTIT='Source # (0-based)', YTIT='PSF Fraction'
  function_1d, id9, LI=6,PSY=1, sn, psf_fraction,     COLOR='white', DATASET='@ specified energy'
  function_1d, id9, LI=6,PSY=5, sn, psf_fraction_min, COLOR='blue',  DATASET='minimum'

  savefile = 'extract_events_'+obsname+'.sav'
  save, sourcename, xpos_catalog, ypos_catalog, xpos_data, ypos_data, er_xpos_data, er_ypos_data, cat2data_offset, region_edited, psf_fraction, pgn_area, src_counts, off_angle, warnfrac, FILE=savefile

  print, '============================================================================='
  print, 'IDL vectors saved in ', savefile
  print, '============================================================================='

  ;; If /EXTRACT_SPECTRA not specified, then skip everything else.
  if NOT keyword_set(extract_spectra) then GOTO, CLEANUP


  ;; Show more summary information.
  ind = where(finite(primary_ccd_fraction) AND (primary_ccd_fraction NE 1) AND (primary_ccd_fraction NE 0), count)
  if (count GT 0) then begin
    print & print, 'Light curves could not be computed for these sources because they contain data from multiple CCDs:'
    forprint, sourcename[ind]
  endif

  dataset_3d, id7, xpos_catalog, ypos_catalog, arf_mean, STAT_CODE=0, TITLE='Map of mean ARF value', XTIT='X (sky coordinates)', YTIT='Y (sky coordinates)', ZTIT='mean ARF value'

  function_1d, id8, LI=6,PSY=1, sn, arf_mean,              TITLE=tit, XTIT='Source # (0-based)', YTIT='Mean ARF value (cm^2 counts/photon)'

  function_1d, id10, LI=6,PSY=1, sn, primary_ccd_fraction, TITLE=tit, XTIT='Source # (0-based)', YTIT='Fraction of Source Counts on Primary CCD'
  
  function_1d, id3, LI=6,PSY=4, sn, src_area, DATASET='via dmextract'

  function_1d, id12, LI=6,PSY=1, sn,  probks,  TITLE=tit, XTIT='Source # (0-based)',                YTIT='KS significance wrt uniform LC model'
  dataset_2d, id13, NAN=[0,0], PSY=1, src_counts, probks, TITLE=tit, XTIT='In-band Counts in Source Region', YTIT='KS significance wrt uniform LC model'

  function_1d, id11, LI=6,PSY=1, sn,  warnfrac, TITLE=tit,XTIT='Source # (0-based)',                YTIT='Fraction of Src Events in WARNING_REGION'
  

  savefile = 'extract_spectra_'+obsname+'.sav'
  save, sourcename, xpos_catalog, ypos_catalog, region_edited, psf_fraction, primary_ccd_fraction, arf_mean, num_emap_pixels, mean_exposure, median_exposure, min_exposure, max_exposure, pgn_area, src_area, src_counts, probks, cat2data_offset, off_angle, warnfrac, FILE=savefile

  print, '============================================================================='
  print, 'IDL vectors saved in ', savefile
  print, '============================================================================='
  GOTO, CLEANUP
endif


;; =============================================================================
if keyword_set(plot) AND keyword_set(extract_backgrounds) then begin
;; =============================================================================

  f_nan = replicate(!VALUES.F_NAN,num_sources)

  xpos_catalog    = f_nan
  ypos_catalog    = f_nan
  bkg_counts  = f_nan
  bkg_radius  = f_nan
  background  = f_nan
  src_counts  = f_nan
  off_angle   = f_nan
  
  src_backscal= f_nan
  bkg_backscal= f_nan

  ;; Read summary information from stats file for each source.
  for ii = 0, num_sources-1 do begin
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn        = obsdir + obs_stats_basename
    src_spectrum_fn = obsdir + src_spectrum_basename
    bkg_spectrum_fn = obsdir + bkg_spectrum_basename

    stats = headfits(stats_fn, ERRMSG=error)    
    if (NOT keyword_set(error)) then begin
      xpos_catalog        [ii] = sxpar(stats, 'X_CAT')
      ypos_catalog        [ii] = sxpar(stats, 'Y_CAT')
      bkg_radius          [ii] = sxpar(stats, 'BKG_RAD')
      background          [ii] = sxpar(stats, 'BACKGRND')
      bkg_counts          [ii] = sxpar(stats, 'BKG_CNTS')
      src_counts          [ii] = sxpar(stats, 'SRC_CNTS')
      off_angle           [ii] = sxpar(stats, 'THETA')
    endif

    header = headfits(src_spectrum_fn, ERRMSG=error, EXT=1)    
    if (NOT keyword_set(error)) then begin
      src_backscal        [ii] = sxpar(header, 'BACKSCAL')
    endif

    header = headfits(bkg_spectrum_fn, ERRMSG=error, EXT=1)    
    if (NOT keyword_set(error)) then begin
      bkg_backscal        [ii] = sxpar(header, 'BACKSCAL')
    endif
  endfor
  
  
  if keyword_set(region_file) then begin
    ; Make a region file showing the background regions.
    openw,  region2_unit, region_file, /GET_LUN
    printf, region2_unit, "# Region file format: DS9 version 3.0"
    !TEXTUNIT = region2_unit
    forprint, TEXTOUT=5, xpos_catalog, ypos_catalog, bkg_radius, F='(%"circle(%0.1f,%0.1f,%0.1f) # tag={background}")', /NoCOMMENT
    free_lun, region2_unit
  endif

  
  ;; Show summary information.
  tit = 'Actual Background Extraction Regions for dataset "' + obsname + '"'
  sn = indgen(num_sources)
  function_1d, id3, LI=6,PSY=1, sn, bkg_counts, TITLE=tit, XTIT='Source # (0-based)', YTIT='In-band Counts in Background Region'
  function_1d, id7, LI=6,PSY=1, sn, background/1E-9, TITLE=tit, XTIT='Source # (0-based)', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
  function_1d, id5, LI=6,PSY=1, sn, bkg_radius,   TITLE=tit, XTIT='Source # (0-based)', YTIT='Radius of Background Region (sky pixels)'

  bkg_scaling = src_backscal/bkg_backscal
  function_1d, id2, LI=6,PSY=1, sn, bkg_scaling,TITLE=tit, XTIT='Source # (0-based)', YTIT='src_BACKSCAL/bkg_BACKSCAL'

  dataset_2d, id4, NAN=[0,0], PSY=1, src_counts, background/1E-9, TITLE=tit, XTIT='In-band Counts in Source Region', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'

  dataset_2d, id6, NAN=[0,0], PSY=1, off_angle, background/1E-9, TITLE=tit, XTIT='Off-axis Angle (arcmin)', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'

  savefile = 'extract_backgrounds_'+obsname+'.sav'
  save, sourcename, bkg_counts, bkg_radius, background, src_counts, off_angle, bkg_scaling, FILE=savefile

  print, '============================================================================='
  print, 'IDL vectors saved in ', savefile
  print, '============================================================================='
  GOTO, CLEANUP
endif

;; =============================================================================
if keyword_set(plot) AND keyword_set(merge_observations) then begin
;; =============================================================================

  f_nan = replicate(!VALUES.F_NAN,num_sources)
  d_nan = replicate(!VALUES.D_NAN,num_sources)

  ra             = d_nan
  dec            = d_nan
  off_angle      = f_nan
  arf_mean       = f_nan
  obsid_count    = f_nan
  background     = f_nan
  src_counts     = f_nan
  net_counts     = f_nan
  src_signif     = f_nan
  flux1          = f_nan
  flux2          = f_nan
  scal_max       = f_nan
  scal_min       = f_nan
  probks         = f_nan
  
  cartoon_acis   = 0
  cartoon_sky    = 0
  psf_missing    = replicate(1B,num_sources)
  if keyword_set(cartoon_template) then begin
    refhd = headfits(cartoon_template)
    extast, refhd, refastr
    cartoon_xsize = sxpar(refhd, 'NAXIS1')
    cartoon_ysize = sxpar(refhd, 'NAXIS2')
  endif

  ;; Read summary information from stats file for each source.
  print, 'reading source information'
  for ii = 0, num_sources-1 do begin
    sourcedir        = sourcename[ii] + '/' + merge_subdir[ii] 
    stats_fn         = sourcedir + src_stats_basename
    photometry_fn    = sourcedir + src_photometry_basename
    composite_psf_fn = sourcedir + psf_basename

    stats = headfits(stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra             [ii] = sxpar(stats, 'RA')
      dec            [ii] = sxpar(stats, 'DEC')
      off_angle      [ii] = sxpar(stats, 'THETA')
      obsid_count    [ii] = sxpar(stats, 'NUM_OBS')
      background     [ii] = sxpar(stats, 'BACKGRND')
      src_counts     [ii] = sxpar(stats, 'SRC_CNTS')
      scal_max       [ii] = sxpar(stats, 'SCAL_MAX')
      scal_min       [ii] = sxpar(stats, 'SCAL_MIN')
      probks         [ii] = sxpar(stats, 'PROB_KS')
    endif else print, 'WARNING! Could not read '+stats_fn

    t = mrdfits(photometry_fn, 1, /SILENT, STATUS=status)
    
    if (status EQ 0) then begin
      arf_mean       [ii] = t[0].MEAN_ARF
      net_counts     [ii] = t[0].NET_CNTS
      src_signif     [ii] = t[0].SRC_SIGNIF
      flux1          [ii] = t[0].FLUX1
      flux2          [ii] = t[0].FLUX2
    endif else print, 'WARNING! Could not read '+photometry_fn

    if keyword_set(cartoon_template) then begin
      if (flux2[ii] LE 0) then begin
        print, 'FLUX2[0] not positive -- source not added to cartoon'
        continue
      endif
      
      if NOT finite(flux2[ii]) then continue
      
      ;; Use ACIS PSF to build cartoon.
      if file_test(composite_psf_fn) then begin
        psf_missing[ii] = 0
        psf_img = readfits(composite_psf_fn, psf_header, /SILENT)
        
        ; Set all the NaN values to zero to keep future computations happy.
        ind = where(finite(psf_img) EQ 0, count)
        if (count GT 0) then psf_img[ind] = 0

        ; We have to be careful with the normalization of the PSF image.
        ; The PSF on disk has is implicitly scaled using PSF_TOTL to sum to <=1 
        ; due to cropping, i.e. total(psf_img)/PSF_TOTL <=1.
        ; Regridding with hastrom does NOT preserve the power -- we must renormalize so
        ; the power is back to the value in the original PSF.
        ; We have to normalize PSF _before_ resampling to scene grid because
        ; the PSF may fall partially off the scene.
          
        psf_total = sxpar(psf_header, 'PSF_TOTL')
        if (psf_total EQ 0) then begin
          print, "WARNING: obsolete PSFs in "+psf_fn
          power = 1.0
        endif else begin
          power = total(psf_img, /DOUBLE) / psf_total
        endelse
        
        fxaddpar, psf_header, 'EQUINOX', 2000.0      
        hastrom, psf_img, psf_header, refhd, MISSING=0  

        normalization = flux2[ii] * (power/total(psf_img, /DOUBLE))
        
        if finite(normalization) then begin        
          cartoon_acis = cartoon_acis + psf_img * normalization
        endif else print, 'WARNING: PSF image is empty'
      endif
      
      ;; Use constant PSF to build cartoon.
      ; We can not use /NORMALIZE in psf_gaussian() because the PSF may
      ; fall partially off the scene.
      ad2xy, ra[ii], dec[ii], refastr, xindex, yindex 
      psf_img = psf_gaussian(NPIX=[cartoon_xsize,cartoon_ysize], FWHM=3, CENTROID=[xindex,yindex])
        
      cartoon_sky = cartoon_sky + psf_img * flux2[ii]
    endif ;keyword_set(cartoon_template)
  endfor
  
  ;; Compute sky positions for an arbitrary sky (x,y) system with 
  ;; 0.5" pixels centered on the mean source position.
  make_astr, composite_astr, DELTA=[-0.000136667D,0.000136667D],  $
                             CRPIX=[0D,0D], CRVAL=[mean(ra,  /DOUBLE),mean(dec, /DOUBLE)]

  ad2xy, ra, dec, composite_astr, xpos_catalog, ypos_catalog
  
  ;; Construct cartoon image.
  if keyword_set(cartoon_template) then begin
    cartoon_acis_fn = 'cartoon_acis.img'
    cartoon_sky_fn  = 'cartoon_sky.img'

    print, '============================================================================='
    num_psf_missing = total(psf_missing)
    if (num_psf_missing EQ num_sources) then begin
      print, 'WARNING: construction of ',cartoon_acis_fn,' skipped because no CHECK_POSITIONS results found.'
    endif else begin
      if (num_psf_missing GT 0) then begin
        print, num_psf_missing, cartoon_acis_fn, F='(%"WARNING: %d sources omitted from %s because no CHECK_POSITIONS results found.")'
      endif else print, 'Wrote ' + cartoon_acis_fn

      fxaddpar, psf_header, 'CREATOR', creator_string
      writefits, cartoon_acis_fn, cartoon_acis/max(cartoon_acis), psf_header
      run_command, 'ds9 -log '+cartoon_acis_fn+' &'
    endelse

    fxaddpar, refhd, 'CREATOR', creator_string
    writefits, cartoon_sky_fn, cartoon_sky/max(cartoon_sky), refhd
    run_command, 'ds9 -log '+cartoon_sky_fn+' &'
    print, 'Wrote ' + cartoon_sky_fn
    print, '============================================================================='
  endif

  
  
  ;; Show summary information.
  tit = 'Composite Extractions'
  sn = indgen(num_sources)
  function_1d, id5, LI=6,PSY=1, sn, net_counts,  TITLE=tit, XTIT='Source # (0-based)', YTIT='Net Counts' 
  function_1d, id3, LI=6,PSY=1, sn, src_signif,  TITLE=tit, XTIT='Source # (0-based)', YTIT='Source Significance' 
  function_1d, id6, LI=6,PSY=1, sn, obsid_count, TITLE=tit, XTIT='Source # (0-based)', YTIT='# Observations of Source' 
  function_1d, id7, LI=6,PSY=1, sn, arf_mean,    TITLE=tit, XTIT='Source # (0-based)', YTIT='Mean ARF value (cm^2 counts/photon)'

  function_1d, id4, LI=6,PSY=1, sn, scal_max,    TITLE=tit, XTIT='Source # (0-based)', YTIT='BACKSCAL in background spectrum', DATASET='maximum'
  function_1d, id4, LI=6,PSY=4, sn, scal_min, DATASET='minimum'
  
  dataset_1d,  id8, arf_mean, BINSIZE=1, DENSITY_TITLE=tit, XTIT='Mean ARF value (cm^2 counts/photon)'
  
  dataset_3d, id1, xpos_catalog, ypos_catalog, arf_mean, STAT_CODE=0, TITLE='Map of mean ARF value', XTIT='composite X (0.5" pixels)', YTIT='composite Y (0.5" pixels)', ZTIT='mean ARF value'

  function_1d, id10, LI=6,PSY=1, sn,   probks, TITLE=tit, XTIT='Source # (0-based)',                         YTIT='KS significance between src & bkg spectra'
  dataset_2d, id11, NAN=[0,0], PSY=1, net_counts, probks, TITLE=tit, XTIT='Net Counts', YTIT='KS significance between src & bkg spectra'

  dataset_2d, id12, NAN=[0,0], PSY=1, net_counts, background/1E-9, TITLE=tit, XTIT='Net Counts', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'
  dataset_2d, id13, NAN=[0,0], PSY=1, src_signif, background/1E-9, TITLE=tit, XTIT='Source Significance', YTIT='Background Intensity (X10-9 photons/pixel^2/cm^2/sec)'

  dataset_2d, id14, NAN=[0,0], PSY=1, off_angle, net_counts,  TITLE=tit, XTIT='Average Off-axis Angle (arcmin)', YTIT='Net Counts'
  dataset_2d, id15, NAN=[0,0], PSY=1, off_angle, src_signif,  TITLE=tit, XTIT='Average Off-axis Angle (arcmin)', YTIT='Source Significance'

  dataset_2d, id16, NAN=[0,0], PSY=1, flux1, flux2,  TITLE=tit, XTIT='Flux1 (photon/cm^2/s) -- see manual', YTIT='Flux2 (photon/cm^2/s) -- see manual'

  dataset_2d, id17, NAN=[0,0], PSY=1, net_counts, src_signif,  TITLE=tit, XTIT='Net Counts', YTIT='Source Significance'

  save, sourcename, xpos_catalog, ypos_catalog, off_angle, arf_mean, obsid_count, background, src_counts, net_counts, src_signif, flux1, flux2, scal_max, scal_min, probks, FILE='merge_observations.sav'

  print, '============================================================================='
  print, 'IDL vectors saved in merge_observations.sav'
  print, '============================================================================='
  GOTO, CLEANUP
endif



;; =============================================================================
if keyword_set(construct_regions) AND keyword_set(diffuse) then begin
;; =============================================================================

  ;; Read a 2-column catalog.
  readcol, catalog_or_srclist, sourcename, catalog_region_fn, FORMAT='A,A', COMMENT=';'
  
  ; Trim whitespace and remove blank lines.
  sourcename = strtrim(sourcename,2)
  ind = where(sourcename NE '', num_sources)
  
  if (num_sources EQ 0) then begin
    print, 'ERROR: no entries read from source list ', catalog_or_srclist
    GOTO, FAILURE
  endif
  
  sourcename          = sourcename         [ind]
  catalog_region_fn   = catalog_region_fn  [ind]
  print, num_sources, F='(%"\n%d sources found in catalog.\n")'
  

  ;; Load observation data into ds9.
  print, 'Spawning ds9 to perform coordinate conversions ...'
  ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name, OPTION_STRING='-bin factor 8'
  ae_send_to_ds9, my_ds9, obsdata_filename
  print, 'Configure binning & zoom as desired, then press RETURN ...'
  cmd = ''
  read, '? ', cmd

  ;; Process each source.
  for ii = 0, num_sources-1 do begin
    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, 'pset dmcopy clobber=yes'
    
    print, sourcename[ii], F='(%"\nSource: %s")'
    
    ;; Construct directory & filenames for source.
    sourcedir = sourcename[ii] 
    
    unnamed_src_stats_fn = sourcename[ii] + '/' + src_stats_basename
   
    file_mkdir, sourcedir

    fxhmake,  unnamed_src_stats, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, unnamed_src_stats, 'CREATOR', creator_string
    fxaddpar, unnamed_src_stats, 'OBJECT', sourcename[ii], 'source name'
    fxaddpar, unnamed_src_stats, 'LABEL',  sourcename[ii], 'source name'
    fxaddpar, unnamed_src_stats, 'DIFFUSE', 'T', 'diffuse source'
    fxaddpar, unnamed_src_stats, 'RA',  !VALUES.F_NAN, 'diffuse source has no position'
    fxaddpar, unnamed_src_stats, 'DEC', !VALUES.F_NAN, 'diffuse source has no position'
    fxaddpar, unnamed_src_stats, 'REGIONFN', catalog_region_fn[ii], 'region file defining the source'
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
    
    ;; Construct filenames for specific obsid.
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn  = obsdir + obs_stats_basename
    region_fn = obsdir + src_region_basename
    file_mkdir, obsdir
    
    ;; Load region file into ds9 and resave in PHYSICAL coordinates.
    cmd = strarr(4)
    cmd[0] = string(my_ds9,                          F='(%"xpaset -p ''%s'' regions delete all")')
    cmd[1] = string(my_ds9, catalog_region_fn[ii],   F='(%"xpaset -p ''%s'' regions load %s")')
    cmd[2] = string(my_ds9,                          F='(%"xpaset -p ''%s'' regions system physical")')
    cmd[3] = string(my_ds9, region_fn,               F='(%"xpaset -p ''%s'' regions save %s")')
    run_command, cmd, /QUIET
    
    ;; Apply region file to observation data and determine X/Y ranges.
    ae_ds9_to_ciao_regionfile, region_fn, temp_region_fn, /IGNORE_BACKGROUND_TAG
    
    cmd = string(obsdata_filename, temp_region_fn, temp_events_fn, $
                 F="(%'dmcopy ""%s[sky=region(%s)][cols x,y]"" %s')")
    run_command, cmd
    
    wideband_events = mrdfits(temp_events_fn, 1, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + temp_events_fn

    if NOT keyword_set(wideband_events) then begin
      print, F='(%"\nSource is not in field of view.")'
      continue
    endif

    
    min_x = min (wideband_events.x, MAX=max_x)
    min_y = min (wideband_events.y, MAX=max_y)
    
    mask_radius = ((max_x-min_x) > (max_y-min_y)) / 2

    fxhmake,  stats, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'OBSNAME',  obsname, 'observation identifier'
    fxaddpar, stats, 'DIFFUSE', 'T', 'diffuse source'
    fxaddpar, stats, 'MSK_RAD',  mask_radius,  'mask radius (sky pixels)'
    fxaddpar, stats, 'X_CAT',    (min_x+max_x)/2, 'source position (sky coordinates)'
    fxaddpar, stats, 'Y_CAT',    (min_y+max_y)/2, 'source position (sky coordinates)'
    writefits, stats_fn, 0, stats
  endfor
  run_command, string(my_ds9, F='(%"xpaset -p ''%s'' exit")'), /QUIET
endif ;keyword_set(construct_regions) AND keyword_set(diffuse)


  
;; =============================================================================
if keyword_set(construct_regions) AND NOT keyword_set(diffuse) then begin
;; =============================================================================

  if ~keyword_set(aspect_blur) then aspect_blur = 0.07   ;arcseconds 
   
  ae_make_psf_not_configured = 1
  
  
  ;; Read the 5-column catalog.
  readcol, catalog_or_srclist, sourcename, ra, dec, target_psf_fraction, fiducial_psf_energy, FORMAT='A,D,D,F,D', COMMENT=';'
  
  ; Trim whitespace and remove blank lines.
  sourcename = strtrim(sourcename,2)
  ind = where(sourcename NE '', num_sources)
  
  if (num_sources EQ 0) then begin
    print, 'ERROR: no entries read from source list ', catalog_or_srclist
    GOTO, FAILURE
  endif
  
  sourcename          = sourcename         [ind]
  ra                  = ra                 [ind]
  dec                 = dec                [ind]
  target_psf_fraction = target_psf_fraction[ind]
  fiducial_psf_energy = fiducial_psf_energy[ind]
  print, num_sources, F='(%"\n%d sources found in catalog.\n")'
  
  
  ;; Read the exposure map & setup array index to sky coordinate conversion.
  emap = readfits(emap_filename, emap_header, /SILENT)
  extast, emap_header, emap2wcs_astr
  
  ind = where(finite(emap) AND (emap GT 0))
  emap_global_median= median(emap[ind])
  emap_threshold = emap_global_median/10.
  emap_xdim = (size(emap, /DIM))[0]
  emap_ydim = (size(emap, /DIM))[1]

  ;; In order to efficiently skip obviously off-field sources, without having to spawn dmcoords below, 
  ;; we will try to parse the approximate source position from the source name, 
  ;; and flag the source if it falls outside the emap array.
  result = stregex(sourcename, '([0-9]{2})([0-9]{2})([0-9]{2}\.[0-9]+)([+-][0-9]{2})([0-9]{2})([0-9]{2}\.[0-9]+)',/SUB,/EXT)
  
  ra_approx  = 15 * tenv( result[1,*],  result[2,*],  result[3,*])
  dec_approx =      tenv( result[4,*],  result[5,*],  result[6,*])
                                                                          
  ad2xy, ra_approx, dec_approx, emap2wcs_astr, xindex, yindex 
    
  source_not_observed = (reform(result[0,*]) NE '') AND ((xindex LT 0) OR (xindex GE emap_xdim) OR $
                                                         (yindex LT 0) OR (yindex GE emap_ydim)) 
      
      
;window,0,XSIZE=256,YSIZE=256  

  ;; ------------------------------------------------------------------------
  mask_radii = fltarr(num_sources)
  
  ;; Process each source.
  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be off-field.
    if source_not_observed[ii] then continue
    
    ;; Construct directory & filenames for source.
    sourcedir = sourcename[ii] 
    
    unnamed_src_stats_fn = sourcename[ii] + '/' + src_stats_basename
  
    
    ;; ------------------------------------------------------------------------
    ;; Construct OR update the UNNAMED source.stats FITS file where the coordinates are stored,.
    ;; We must be very careful here!
    ;;
    ;; * In the AE recipe, ae_source_manager writes RA/DEC to this file, then ae_make_catalog
    ;;   is called many times using a catalog with RA=0,DEC=0 as a flag to indicate that we
    ;;   should read the coordinates from source.stats.
    ;;   In this case we specifically DO NOT WRITE the RA/DEC values to the stats file 
    ;;   in order to avoid rounding errors from repeated conversion during FITS I/O.
    ;;
    ;; * In general, we want the observer to be able to populate source.stats with useful
    ;;   keywords (e.g. PROVENAN) and have them not be destroyed here.
    ;;
    ;; * However, it's good programming practice to whack all the keywords that AE wrote
    ;;   from a previous extraction since we're changing the source fundamentals here
    ;;   (position and PSF fraction).
    
    ; Look for an existing unnamed version of source.stats file, which is where coordinates are stored.
    if file_test(unnamed_src_stats_fn) then begin
        ; We try a few times because file access can collide with another AE process that's writing the file.
      for kk=1,3 do begin
        unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
        if ~keyword_set(error) then break
        wait, kk*0.5
      endfor ;kk
    endif else begin
      error = 1
    endelse
    
    
    if keyword_set(error) then begin
      if (ra[ii] EQ 0) AND (dec[ii] EQ 0) then begin
        print, 'ERROR: coordinates in catalog are (0,0) but cannot find an existing file '+unnamed_src_stats_fn
        GOTO, FAILURE
      endif

      ; Create a new FITS header to hold the fundamental source property keywords.
      file_mkdir, sourcedir  
      fxhmake, unnamed_src_stats, /INITIALIZE, /EXTEND, /DATE
      get_date, date_today, /TIMETAG
      fxaddpar, unnamed_src_stats, 'CREATOR' , creator_string
      fxaddpar, unnamed_src_stats, 'OBJECT'  , sourcename[ii], 'source name'
      fxaddpar, unnamed_src_stats, 'POSNDATE', date_today, 'date RA,DEC were changed'
      fxaddpar, unnamed_src_stats, 'RA'      ,     ra[ii], 'source position', F='(F10.6)'
      fxaddpar, unnamed_src_stats, 'DEC'     ,    dec[ii], 'source position', F='(F10.6)'
      fxaddpar, unnamed_src_stats, 'POSNTYPE',  'unknown', 'type of source position'
      fxaddpar, unnamed_src_stats, 'PROVENAN',  'unknown', 'source provenance'
      fxaddpar, unnamed_src_stats, 'BKSCL_LO',        20.0, 'smallest BACKSCAL allowed'
      fxaddpar, unnamed_src_stats, 'BKSCL_GL',        30.0, 'target   BACKSCAL'
      fxaddpar, unnamed_src_stats, 'BKSCL_HI',        40.0, 'largest  BACKSCAL allowed'

      ; Create the unnamed version of source.stats to store these fundamental source property keywords.
      writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
;     print, 'Creating new file source.stats.'
    endif else begin
      ; Then we either read OR write the RA/DEC keywords, depending on how AE is being called.
      if (ra[ii] EQ 0) AND (dec[ii] EQ 0) then begin
        ; This branch is taken when ae_make_catalog is the caller.
        ra [ii] = sxpar(unnamed_src_stats, 'RA' , COUNT= ra_available)
        dec[ii] = sxpar(unnamed_src_stats, 'DEC', COUNT=dec_available)
        if ~ra_available || ~dec_available then message, 'ERROR: RA & DEC are missing from '+unnamed_src_stats_fn
;       print, 'Coordinates taken from the existing file source.stats.'                                     
      endif else begin
        ; This branch is taken when there is an existing unnamed_src_stats_fn and 
        ; the observer has supplied a normal catalog with coordinates.
        get_date, date_today, /TIMETAG
        fxaddpar, unnamed_src_stats, 'POSNDATE', date_today, 'date RA,DEC were changed'
        fxaddpar, unnamed_src_stats, 'RA'      ,     ra[ii], 'source position', F='(F10.6)'
        fxaddpar, unnamed_src_stats, 'DEC'     ,    dec[ii], 'source position', F='(F10.6)'
;       print, 'Coordinates saved to existing file source.stats.'

        ; We should not have to change the OBJECT property, but we go ahead and do so to ensure
        ; that it matches the sourcename.
        fxaddpar, unnamed_src_stats, 'CREATOR', creator_string
        fxaddpar, unnamed_src_stats,  'OBJECT', sourcename[ii], 'source name'
        
        ; Overwrite the fundamental source property keywords to the unnamed version of source.stats.
        writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
      endelse
    endelse ;Existing source.stats file found.
    
    
    
    ;; ------------------------------------------------------------------------
    ;; Construct filenames for specific obsid.
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    psf_fn    = sourcename[ii] + '/' + obsname + '/' + psf_basename
    obs_stats_fn  = obsdir + obs_stats_basename
    region_fn     = obsdir + src_region_basename
    
    ;; ------------------------------------------------------------------------
    ;; Convert RA,DEC to the (x,y) system of this obsid.
    ;; Skip sources that fall off the field of view.
    cmd = string(obsdata_filename, aspect_fn, ra[ii], dec[ii],  F="(%'dmcoords %s asolfile=%s opt=cel celfmt=deg ra=%10.6f dec=%10.6f')")
    run_command, cmd, /QUIET

    run_command, /QUIET, 'pget dmcoords x y theta detx dety chip_id', result
    xpos_catalog = float(result[0])
    ypos_catalog = float(result[1])
    off_angle    = float(result[2])  ; arcmin
    detx         = float(result[3])
    dety         = float(result[4])
    chip_id      = fix  (result[5])

    
    ;; Find the source position in the exposure map and determine if that
    ;; position was observed.
    ad2xy, ra[ii], dec[ii], emap2wcs_astr, xindex, yindex 
    xindex = round(xindex)
    yindex = round(yindex)
    
    in_field = 0
    
    if ((xindex GE 0) AND (xindex LT emap_xdim) AND $
        (yindex GE 0) AND (yindex LT emap_ydim)) then begin
      emap_val_i = emap[xindex,yindex]
      in_field   = emap_val_i GE emap_threshold
    endif
        
    if ~in_field then begin    
      source_not_observed[ii] = 1
      if file_test(obsdir) then begin
        print, F='(%"\n===================================================================")'
        print, sourcename[ii], strtrim(sxpar(unnamed_src_stats,'LABEL'),2), obsdir, F='(%"ERROR: Source %s (%s) is not in field of view, but extraction %s exists!")'
        print, F='(%"===================================================================")'
      endif
      continue
    endif

    if keyword_set(query_only) then continue

    
    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    
    ;; ------------------------------------------------------------------------
    ;; We are going to process this source, so start the messages and clean out the source.stats properties.
    print, F='(%"\n===================================================================")'
    print, sourcename[ii], strtrim(sxpar(unnamed_src_stats,'LABEL'),2), xpos_catalog, ypos_catalog, F='(%"\nSource %s (%s) at sky=(%d,%d):")'

    if (off_angle GT 50) then begin
      print, 'ERROR: dmcoords computed crazy theta value for source: ', off_angle
      GOTO, FAILURE
    endif


    ; Now that we're sure we are about to change this obsid's extraction we clean out source properties from other stages to avoid confusion.
    ; Doing this earlier would allow an obsid that didn't observe the source to trash source.stats.
    ; The alternate approach, retaining only what we need, would prevent the observers from storing useful keywords in source.stats.
    sxdelpar, unnamed_src_stats, ['NUM_OBS', 'EMAP_TOT', 'FRACEXPO', 'WARNFRAC', 'OBSNAME', 'PRIM_OBS', 'SRC_CNTS', 'THETA', 'SRC_RAD', 'PSF_FRAC', 'MSK_RAD', 'RA_DATA', 'DEC_DATA', 'RA_ML', 'DEC_ML', 'QUANTCOR', 'RA_CORR', 'DEC_CORR', 'BACKGRND', 'SCAL_MAX', 'SCAL_MIN', 'KS_SPECT', 'EXPOSURE', 'EFFAREA', 'PROB_KS', 'ERR_DATA', 'KS_PSF' ]
      
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
    file_mkdir, obsdir
    
    ;; ------------------------------------------------------------------------
    ;; Use a set of PSFs computed earlier if possible.
    
    ; Look for PSF with desired fiducial_psf_energy value & current format standard.
    psf_found = 0
    fits_open, psf_fn, fcb, /NO_ABORT, MESSAGE=error
    if NOT keyword_set(error) then begin
      for extension =0, fcb.NEXTEND do begin
        fits_read, fcb, dum, header, /HEADER_ONLY, /NO_PDU, EXTEN_NO=extension

        psf_found = (abs(fiducial_psf_energy[ii] - sxpar(header, 'ENERGY')) LT 0.001) AND $
                    (sxpar(header, 'PSF_TOTL') NE 0) AND (sxpar(header, 'RADIUS50') NE 0)

        if psf_found then break
      endfor
      fits_close, fcb
    
      ; Unless /REGION_ONLY we require PSFs at multiple energies.
      if (NOT keyword_set(region_only)) AND (fcb.NEXTEND EQ 0) then psf_found = 0
    endif
    
    ; See if the existing PSF in near enough to the source position. 
    if psf_found then begin
      psf_found = 0
      
      ; Look for PSF position recorded by ae_make_psf.
      xpos_old = sxpar(header, 'X_CAT', COUNT=x_count)
      ypos_old = sxpar(header, 'Y_CAT', COUNT=y_count)
      
      ; Or look for mkpsf parameters in the HISTORY kywds.
      if (x_count EQ 0) || (y_count EQ 0) then begin
        x_result = stregex(header,':x=([0-9]+.[0-9]*)',/SUB,/EXT)
        y_result = stregex(header,':y=([0-9]+.[0-9]*)',/SUB,/EXT)
        x_ind = where(x_result[1,*],x_count)
        y_ind = where(y_result[1,*],y_count)
        if (x_count EQ 1) && (y_count EQ 1) then begin
          xpos_old = float(x_result[1,x_ind[0]])
          ypos_old = float(y_result[1,y_ind[0]])  
        endif
      endif
      
      if (x_count EQ 1) && (y_count EQ 1) then begin
        psf_found = (abs(xpos_catalog - xpos_old) LE 0.1) AND $
                    (abs(ypos_catalog - ypos_old) LE 0.1)
      endif
    endif

    if psf_found then begin
      psf_img = readfits(psf_fn, psf_header, EXT=extension, /SILENT)
      print, 'Using existing PSF images ...'
      GOTO, PSF_IN_HAND
    endif

    
    ;; In Dec 2007 I used MARX simulations at 1.5 keV with the readout streak disabled 
    ;; to measure PSF fractions at 1.5 keV as a function of off-axis angle.  
    ;; These polynomial curves were fit to those measurements.
    ;; The off-axis angle off_angle is in arcminutes.
    radius50 = (0.85 -0.25*off_angle + 0.10*off_angle^2) * arcsec_per_skypixel  ; arcseconds
    radius95 = (2.45 -0.32*off_angle + 0.24*off_angle^2) * arcsec_per_skypixel  ; arcseconds
    radius98 = (5.51 -0.87*off_angle + 0.31*off_angle^2) * arcsec_per_skypixel  ; arcseconds
 
    ;; We arbitrarily choose PSF pixel sizes to meet the goal of having at least  
    ;; min_num_across pixels across the 50% PSF fraction radius.
    min_num_across    = 10
    
     
    ;; ------------------------------------------------------------------------
    ;; Generate PSFs via MARX.
    if ae_make_psf_not_configured then begin
      ; In this setup call, pass cache_dir as TEMP_DIR so that marx.par will be preserved when tempdir is whacked. 
      ae_make_psf, TEMP_DIR=cache_dir, EVENT_FILE=obsdata_filename, ASPECT_FN=aspect_fn, ASPECT_BLUR=aspect_blur, S_AIMPOINT=keyword_set(s_aimpoint), PIPELINE_RANDOMIZATION=keyword_set(pipeline_randomization)
      ae_make_psf_not_configured = 0
    endif

    if keyword_set(region_only) then begin
      ; If /REGION_ONLY then only make one PSF image.
      psf_energy = fiducial_psf_energy[ii]
    endif else begin
      ; Otherwise, put the fiducial PSF first in the file.
      psf_energy = [0.2770, 1.49670, 4.510, 6.40, 8.60]
      fid_ind = where(abs(psf_energy - fiducial_psf_energy[ii])/fiducial_psf_energy[ii] LT 0.05, fid_count, COMPLEM=other_ind)

      if (fid_count EQ 1) then begin
        ; Specified energy is close enough to replace a standard one.
        psf_energy = [fiducial_psf_energy[ii], psf_energy[other_ind]]
      endif else begin
        psf_energy = [fiducial_psf_energy[ii], psf_energy]
      endelse
    endelse
  
    ; We pick an arbitrarily Poisson noise level, lower for the fiducial PSF that will be contoured.
    desired_psf_counts = [2E5,1E5,1E5,1E5,1E5]
    
    ; Choose a pixel size that's convenient for boxcar smoothing and that samples the core well.
    binspa_candidates = 1.0 / [  3,5,7,9,11,13]  
   
    for jj=0,n_elements(binspa_candidates)-1 do begin
      skypixel_per_psfpixel = binspa_candidates[jj]
      
      num_across = radius50 / (arcsec_per_skypixel * skypixel_per_psfpixel)
      
      if (num_across GE min_num_across) then break
    endfor
    
    ; In this call MARX will be run, so pass tempdir as TEMP_DIR.
    ae_make_psf, TEMP_DIR=tempdir, psf_fn, skypixel_per_psfpixel, 2*radius98/arcsec_per_skypixel, psf_energy, desired_psf_counts, $
                 ra[ii], dec[ii], X_CAT=xpos_catalog, Y_CAT=ypos_catalog, OFF_ANGLE=off_angle, CHIP_ID=chip_id, EMAP_VAL=emap_val_i
                 
  
    psf_img = readfits(psf_fn, psf_header, EXT=0, /SILENT)
    

PSF_IN_HAND:
    ;; ------------------------------------------------------------------------
    ;; Set all the NaN values to zero to keep total,max,contour, etc. routines happy.
    ind = where(finite(psf_img) EQ 0, count)
    if (count GT 0) then psf_img[ind] = 0
    
    ;; ------------------------------------------------------------------------
    ;; Get some astrometry information about PSF image.
    xdim = (size(psf_img, /DIM))[0]
    ydim = (size(psf_img, /DIM))[1]
        
    ; We cannot use xy2ad.pro for such conversions to PHYSICAL (sky) system.
    crvalP = [sxpar(psf_header, 'CRVAL1P'), sxpar(psf_header, 'CRVAL2P')]
    crpixP = [sxpar(psf_header, 'CRPIX1P'), sxpar(psf_header, 'CRPIX2P')]
    cdeltP = [sxpar(psf_header, 'CDELT1P'), sxpar(psf_header, 'CDELT2P')]


    ;; ------------------------------------------------------------------------
    ;; Find offset between PSF centroid and requested position.
    ;; This is a CENTROID calculation, NOT a PSF FRACTION -- the denominator must
    ;; be the sum of the (cropped) psf_img array.
    denominator = total( psf_img, /DOUBLE )
    ind = lindgen(xdim,ydim)
    xpos_psf = total( psf_img * (ind mod xdim), /DOUB ) / denominator
    ypos_psf = total( psf_img * (ind  /  xdim), /DOUB ) / denominator

    xpos_psf = (xpos_psf+1-crpixP[0])*cdeltP[0] + crvalP[0]
    ypos_psf = (ypos_psf+1-crpixP[1])*cdeltP[1] + crvalP[1]

    psf2cat_offset_i = sqrt((xpos_psf-xpos_catalog)^2 + (ypos_psf-ypos_catalog)^2)
    

    ;; ------------------------------------------------------------------------
    ;; Find a contour of the psf_img which encloses the desired PSF fraction.
    ;; Try to make a good initial guess for the contour by assuming the PSF is a cone.
    ;; Initial value for step corresponds to a change in fraction of 0.05
    peak = max(psf_img)
    contour_level = peak *          (1 - (target_psf_fraction[ii])^0.33)
    initial_step  = peak * 0.05 * 0.33 * (target_psf_fraction[ii])^(-0.66)
    step          = 0
    
    psf_total = sxpar(psf_header, 'PSF_TOTL')
    indent = ''
    done = 0
    loop_count = 0
    repeat begin
      loop_count = loop_count + 1
      
      ; Compute a single contour polygon.
      contour, psf_img, /CLOSED, LEVELS=[contour_level], $
               PATH_XY=xy, PATH_INFO=info, /PATH_DATA_COORDS
      ind = info[0].offset + indgen(info[0].N)
      polygon_x = float(reform(xy[0, ind]))
      polygon_y = float(reform(xy[1, ind]))
      
      
      ; Overly complex polygons don't seem useful, so let's reduce the vertex
      ; count when required.
      ; In fact, sharp features in polygon would be undesirable when the source
      ; position is not correct since they would not line up with the data.
      num_points = n_elements(polygon_x)
      while (num_points GT max_polygon_elements) do begin
        num_to_discard = floor((num_points/2) < (num_points-max_polygon_elements))
        
;       print, indent, num_to_discard, F='(%"%sAveraging %3d pairs of polygon vertexes ...")'
      
        discard = bytarr(num_points)
        for kk=0, 2*(num_to_discard-1), 2 do begin
          ;; Average vertexes kk and kk+1, and discard vertex kk+1.
          polygon_x[kk] = (polygon_x[kk] + polygon_x[kk+1])/2.0
          polygon_y[kk] = (polygon_y[kk] + polygon_y[kk+1])/2.0
          
          discard[kk+1] = 1
        endfor
        
        ind = where(discard EQ 0)
        polygon_x = polygon_x[ind]
        polygon_y = polygon_y[ind]        
        
        num_points = n_elements(polygon_x)
      endwhile
     
      ; Find the enclosed pixels & compute the PSF fraction.
      ind = polyfillv( polygon_x, polygon_y, xdim, ydim )
      
      psf_fraction_i = total(psf_img[ind], /DOUBLE) / psf_total

;tvscl, psf_img < contour_level
;plots,polygon_x,polygon_y,/dev,color=0
      
      ; Choose next search step.
      pf_error = target_psf_fraction[ii] - psf_fraction_i
      if (abs(pf_error) LT 0.01) then done = 1

      ; Initialize step based on sign of pf_error.
      if (step EQ 0) then step = initial_step * ((pf_error GT 0) ? -1 : 1)
      
      ; If the PSF fraction is too small and we're stepping up, then reverse.
      if ((pf_error GT 0) AND (step GT 0)) then step = -step/2.

      ; If the PSF fraction is too large and we're stepping down, then reverse.
      if ((pf_error LT 0) AND (step LT 0)) then step = -step/2.
      
      ; Stop if we're down to taking tiny steps.
      if (abs(step/contour_level) LT 0.01) then done = 1

      ; Stop if we've taken an excessive number of steps.
      if (loop_count GT 20) then begin
        print, 'WARNING, contour search aborted!!!'
        done = 1
      endif

      ; Take a step, but avoid a non-positive level.
;print, psf_fraction_i, contour_level, step
      contour_level = (contour_level + step) > (contour_level/2.)
      contour_level<= peak*0.95
      indent = indent + ' '
    endrep until (done)
    
    print, psf_fraction_i, loop_count, F='(%"Found PSF fraction %4.2f after trying %d contours.")' 
    
    ;; Compute the area of the enclosed pixels.
    src_area_i = n_elements(ind) * (cdeltP[0])^2

    
    ;; ------------------------------------------------------------------------
    ;; Construct a source extraction region corresponding to the contour.

    ;; Convert the polygon from array index to physical (x,y) coordinates, 
    ;; keeping in mind the 1-based FITS convention vs the 0-based IDL array index convention.
    polygon_x = (polygon_x+1-crpixP[0])*cdeltP[0] + crvalP[0]
    polygon_y = (polygon_y+1-crpixP[1])*cdeltP[1] + crvalP[1]
    
    polygon_radii  = sqrt((polygon_x-xpos_catalog)^2 + (polygon_y-ypos_catalog)^2)
    src_radius_max = max(polygon_radii, MIN=src_radius_min)

    ;; Construct ds9 polygon region specification.
    polygon = fltarr(2,n_elements(polygon_x))
    polygon[0,*] = polygon_x
    polygon[1,*] = polygon_y

    src_region = 'polygon(' + strcompress(strjoin(string(polygon,F='(F8.2)'),","), /REMOVE) + ')'

    ;; ------------------------------------------------------------------------
    ;; Find a circular mask region which excludes almost all of the PSF.
    
    ;; Convert the source position to a real-valued quantity in the PSF image array index coordinate system.
    xind_catalog = (crpixP[0] + (xpos_catalog-crvalP[0])/cdeltP[0]) - 1
    yind_catalog = (crpixP[1] + (ypos_catalog-crvalP[1])/cdeltP[1]) - 1

    ;; Make an array that has the distances (in PSF pixel units) from each pixel to the source.
    dist_circle, distance, [xdim,ydim], xind_catalog, yind_catalog
    
    ;; Start with the radius that encloses the source region.
    ;; Search for a radius (in PSF pixel units) that encloses mask_fraction of the light.
    for mask_radius = src_radius_max/cdeltP[0],xdim/2 do begin
      ind = where(distance LE mask_radius)
      this_mask_fraction = (total(psf_img[ind], /DOUBLE) / psf_total)
      
      if (verbose GT 1) then $
        print, 100*this_mask_fraction, mask_radius*cdeltP[0], F='(%"%5.1f%% @ mask_radius=%5.1f")'
      if (this_mask_fraction GT mask_fraction) then break
    endfor
   
    ;; Convert the mask radius from units of PSF pixels to sky pixels.
    mask_radius = mask_multiplier * (mask_radius*cdeltP[0])
    if (verbose GT 1) then print, mask_radius, F='(%"mask_radius=%5.1f skypix")'

    mask_radii[ii] = mask_radius
    mask_region = string(xpos_catalog, ypos_catalog, mask_radius, F='(%"circle(%f,%f,%f)")')
    
    ;; ------------------------------------------------------------------------
    ;; To later assess pileup, estimate the PSF fraction contained in a 3x3 sky pixel cell.
    cell_radius = 1.5 ;skypix
    ind = where(distance LE cell_radius/cdeltP[0], count)
    cell_frac = ((2*cell_radius/cdeltP[0])^2 / count) * (total(psf_img[ind], /DOUBLE) / psf_total)

    ;; ------------------------------------------------------------------------
    ;; Write the source region to a file.
    openw,  region_unit, region_fn, /GET_LUN
    printf, region_unit, "# Region file format: DS9 version 3.0"
    printf, region_unit, src_region, obsname, F='(%"%s # move=0 tag={%s}")' 
    
    ;; ------------------------------------------------------------------------
    ;; Add mask region to the file.
    printf, region_unit, mask_region + " # color=red background"
    free_lun, region_unit
        

    ;; ------------------------------------------------------------------------
    ;; Save information for summary plots.
    fxhmake,  stats, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, stats, 'CREATOR', creator_string
    fxaddpar, stats, 'OBSNAME',  obsname, 'observation identifier'
    fxaddpar, stats, 'EMAP_AVG', emap_val_i, 'exposure map value nearest source'
    fxaddpar, stats, 'THETA',    off_angle, 'off-axis angle (arcmin)'
    fxaddpar, stats, 'PSF2CAT',  psf2cat_offset_i,  'PSF to catalog offset (sky pixels)'
    fxaddpar, stats, 'FRACSPEC', target_psf_fraction[ii], 'target PSF fraction'
    fxaddpar, stats, 'PSF_FRAC', psf_fraction_i, string(fiducial_psf_energy[ii], F='(%"PSF fraction via polyfillv @%6.4f keV")')
    fxaddpar, stats, 'PSF_ENGY', fiducial_psf_energy[ii], 'fiducial PSF energy (keV)'
    fxaddpar, stats, 'MSK_RAD',  mask_radius,  'mask radius (sky pixels)'
    fxaddpar, stats, 'PGN_AREA',      src_area_i,      'src polygon area, via polyfillv (skypix^2)'
    fxaddpar, stats, 'SRC_RAD',  sqrt(src_area_i/!PI), 'sqrt(PGN_AREA/!PI) (skypix)'
    fxaddpar, stats, 'REG_EDIT', 'F', 'T=region edited in ds9'
    fxaddpar, stats, 'CELLFRAC', cell_frac, 'PSF fraction in 3x3 skypix cell'
    fxaddpar, stats, 'X_CAT',    xpos_catalog, 'source position (sky coordinates), from catalog'
    fxaddpar, stats, 'Y_CAT',    ypos_catalog, 'source position (sky coordinates), from catalog'
    fxaddpar, stats, 'X_PSF',    xpos_psf, 'PSF position (sky coordinates), centroid of image'
    fxaddpar, stats, 'Y_PSF',    ypos_psf, 'PSF position (sky coordinates), centroid of image'
    writefits, obs_stats_fn, 0, stats

  endfor ;ii
  
  ;; Check for mask radii (units of sky pixels) that are small with respect to the emap pixel size.
  emap_pixel_size = sxpar(emap_header,'CDELT1P')  ; units are sky pixels
  span = mask_radii / emap_pixel_size
  ind = where( (span NE 0) AND (span LT 4), count )
  if (count GT 0) then begin
    print
    print, 'WARNING!  The following sources have mask radii that are small with respect to the emap pixel size.'
    print, 'Once the mask is pixelized, the source masking may be inadequate.'
    forprint, sourcename[ind], mask_radii[ind]
  endif ;ii
  
  count = total(source_not_observed, /INT)
  if (count GT 0) then print, count, F="(%'\nWARNING!  %d sources were not observed.')"

endif ;keyword_set(construct_regions)



;; =============================================================================
if keyword_set(show_regions) && keyword_set(collated_filename) then begin
;; =============================================================================
  ;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
  run_command, /QUIET, 'pset dmcopy clobber=yes'

  deghour = 24D/360
  
  ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name

  pan_only = keyword_set(display_file) AND keyword_set(region_file)  

      
  ;; Read the source properties of all the sources.
  bt = mrdfits(collated_filename, 1, theader, /SILENT, STATUS=status)
  if (status NE 0) then begin
    print, 'ERROR reading ', collated_filename
    GOTO, FAILURE      
  endif
  
  num_sources = n_elements(bt)

  ;; Look up the source properties of all the sources.
  sourcename      = strtrim(bt.CATALOG_NAME   ,2)
  ra              =         bt.RA                 
  dec             =         bt.DEC                
  label           = strtrim(bt.LABEL          ,2) 
  posntype        = strtrim(bt.POSNTYPE       ,2) 


  if pan_only then begin
    ae_send_to_ds9, my_ds9, display_file, region_file
    point_ra = 0
    merge_available = 0
    recon_available = 0
  endif else begin
    if keyword_set(region_file) then begin
      ; Count any observer-supplied "point" regions.
      print, 'Reading ', region_file
      readcol, region_file, lines, F='(A)', DELIM='@'
      ind = where( (strmid(lines,0,1) NE '#') AND strmatch(lines, '*point*', /FOLD), num_external_points )
      print, region_file, num_external_points, F='(%"%s contains %d point regions")'
    endif else num_external_points = 0
    
    ; Make arrays to hold region strings & coordinates for observer-supplied and catalog positions.
    point_region = strarr(num_external_points+num_sources)
    point_ra     = dblarr(num_external_points+num_sources)
    point_dec    = dblarr(num_external_points+num_sources)
  
    if (num_external_points GT 0) then begin
      ; Parse external "point" regions for RA & DEC values.
      ; This regular expression should work for negative DEC, and space or comma separation, and with or without ().
      lines  = lines[ind]
      result = stregex(lines,'point[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
      point_ra    [0] = double(reform(result[1,*]))
      point_dec   [0] = double(reform(result[2,*]))
      point_region[0] = lines
    endif 
      
    ; Append "point" regions that mark all the sources in the catalog.
    point_ra    [num_external_points] = ra
    point_dec   [num_external_points] = dec
    for ii = 0, num_sources-1 do begin
      point_region[num_external_points+ii] = string(ra[ii], dec[ii], label[ii], F='("J2000;point(",F10.6,",",F10.6,") # point=cross width=2 text={",A,"}")')
    endfor
  endelse
 
  
  ; Use a subset of the sources if requested.
  if keyword_set(srclist_filename) then begin
    readcol, srclist_filename, sourcenames_to_show, FORMAT='A', COMMENT=';'
    
    ; Trim whitespace and remove blank lines.
    sourcenames_to_show = strtrim(sourcenames_to_show,2)
    ind = where(sourcenames_to_show NE '', num_to_show)
    
    if (num_to_show EQ 0) then begin
      print, 'ERROR: no entries read from source list ', srclist_filename
      GOTO, FAILURE
    endif
    
    sourcenames_to_show = sourcenames_to_show[ind]
    print, num_to_show, srclist_filename, F='(%"\n%d sources found in %s\n")'
    
    ; Create an array specifying the indexes into bt corresponding to the sources listed in sourcenames_to_show.
    presentation_list = replicate(-1L, num_to_show)
    
    for ii = 0,num_to_show-1 do begin
      ind = where(sourcename EQ sourcenames_to_show[ii], count)
      
      if (count GT 0) then begin
        presentation_list[ii] = ind[0]
      endif else print, sourcenames_to_show[ii], collated_filename, F='(%"WARNING: source %s is missing from table %s")'
      
      if (count GT 1) then print, sourcenames_to_show[ii], collated_filename, F='(%"WARNING: source %s appears multiple times in table %s")'
    endfor ;ii
    
    ind = where(presentation_list NE -1, count)
    if (count EQ 0) then begin
      print, 'Zero sources listed in ', srclist_filename, ' were found in ', collated_filename
      GOTO, FAILURE      
    endif
    presentation_list = presentation_list[ind]
    
    
  endif else if keyword_set(index_file) then begin
    ; Defined a subset and/or ordering of the catalog using supplied ASCII 1-based index file.
    readcol, index_file, source_num, FORMAT='L', COMMENT=';'
    num_to_show = n_elements(source_num)
    
    ; source_num is 1-based but indexing is 0-based.
    presentation_list  = 0 > (source_num-1) < (num_sources-1)
  endif else presentation_list = lindgen(num_sources)
    
  
  ; Because our "source list" was read here (via FITS table) rather than at the top of AE (via catalog_or_srclist)
  ; we need to deal with some loose ends now.
  ; MERGE_NAME is taken from AE keyword parameter if available, or from collated table otherwise.
  if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)
  
  dum = where(tag_names(bt) EQ 'MERGE_NAME', count)
  if keyword_set(merge_name) then begin
    merge_subdir = merge_name    + '/'
  endif else if (count EQ 1) then begin
    merge_subdir = strtrim(bt.MERGE_NAME,2) + '/'
  endif else begin
    merge_subdir = ''
  endelse
  
  if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)

  
  ; Convert RAs to hours for gcirc below.
        ra_hrs =       ra * deghour
  point_ra_hrs = point_ra * deghour
  
  energy_range_label = string(energy_range, F='(%"[%0.2f:%0.2f] keV ")')
  desired_zoom = 4
  
  presentation_index = 0L
  ii = presentation_list[presentation_index]
  while 1 do begin
    ; Report position of current source in presentation_list.
    sequence_number = (where(presentation_list EQ ii, count))[0]
    sequence_msg    = (count EQ 0) ? '' : string(1+sequence_number, n_elements(presentation_list), F="(%'#%d of %d')")
  
    print, F='(%"\n===================================================================")'
    print, sourcename[ii], label[ii], posntype[ii], sequence_msg, F='(%"\nSource %s (%s) (%s) %s")'
    
    ;; We can NOT search for "neighborhood.evt" because /MERGE could make a file
    ;; sourcename/extraction_name/neighborhood.evt which would be misinterpreted here as
    ;; an observation!  We must instead search for "obs.stats" which appears only in observation
    ;; directories, and then see which of those observations has a "neighborhood.evt".
    
    ; If the observer has specified a list of observations then look for them, otherwise look for any.
    pattern_base = sourcename[ii] + '/' + (keyword_set(obsname) ? obsname : '*') + '/'
    obs_stats_fn   = file_search( pattern_base + extraction_subdir[ii] + obs_stats_basename, COUNT=num_obs )
    
    if (num_obs EQ 0) then begin
      print, '  Not present in any observation'
      goto, COMMAND
    endif
    
    obs_dir = strarr(num_obs)
    for jj = 0, num_obs-1 do begin
      fdecomp, obs_stats_fn[jj], disk, dir
      obs_dir[jj] = dir
    endfor
    

    env_events_fn = obs_dir + env_events_basename
    good_obs  = where(file_test(env_events_fn), num_obs)
    if (num_obs EQ 0) then begin
      print, 'ERROR: none of these neighborhoods could be found:'
      forprint, env_events_fn
      GOTO, FAILURE
    endif
    obs_dir       = obs_dir      [good_obs]
    env_events_fn = env_events_fn[good_obs]

    
    ;; ------------------------------------------------------------------------
    ;; Build names for the files we'll need to access.
    sourcedir            = sourcename[ii] + '/' + merge_subdir[ii] 
    src_stats_fn         = sourcedir            + src_stats_basename
    merged_env_events_fn = sourcedir + env_events_basename
    composite_img_fn     = sourcedir + env_image_basename
    merged_region_fn     = sourcedir + src_region_basename
    region_fn            = obs_dir             + src_region_basename
    display_data_fn      = tempdir + string(indgen(num_obs), F='(%"frame%d_")') + env_events_basename
    edited_region_fn     = tempdir + string(indgen(num_obs), F='(%"frame%d_")') + src_region_basename
    bkg_pixels_region_fn = obs_dir + bkg_pixels_region_basename

    ; Look up in source.stats which extractions were merged.
    src_stats = headfits(src_stats_fn, ERRMSG=error)
    
    if keyword_set(error) then begin
      print, 'ERROR: File '+src_stats_fn+' is missing!'
      goto, COMMAND    
    endif
    
   obsnames_merged = strtrim(sxpar(src_stats,'OBSNAME'), 2) 
    
    if pan_only then begin
      ae_send_to_ds9, my_ds9, PAN_TO_COORDS=[ra[ii], dec[ii]]
      goto, SHOW_STATS
    endif
    
    
    ;; ------------------------------------------------------------------------
    ;; Display all the neighborhoods, & reconstruction if available, as quickly as possible.

    ; If available the merged neighborhood will be shown in a ds9 frame.
    dum = headfits(merged_env_events_fn, ERRMSG=error, EXT=1)
    merge_available = (NOT keyword_set(error))
    
    ; If available the composite reconstructed image will be shown in the last ds9 frame.
    dum = headfits(composite_img_fn, ERRMSG=error, EXT=1)
    recon_available = (NOT keyword_set(error))
    
    
    
    ; Identify annotation point regions that are nearby.
    gcirc, 1, ra_hrs[ii], dec[ii], point_ra_hrs, point_dec, distance
    ind = where( distance LT 30, num_nearby_points )
    if (num_nearby_points GT 0) then nearby_point_region = point_region[ind]

    ; We construct a temporary region file for each observation which is passed to
    ; and optionally modified by ds9.  The temporary region file contains:
    ; 1. source extraction region file for the observation
    ; 2. point regions marking sources in the whole catalog 
    ; 3. an optional user-supplied REGION_FILENAME 
    ; 4. the background region built by  ae_better_backgrounds, if available
    ; Parts 2&3 are in point_region string array constructed above.
    cmd = strarr(num_obs)
    for jj = 0, num_obs-1 do begin
      ; Copy source extraction region files.
      file_copy, region_fn[jj], edited_region_fn[jj], /OVERWRITE
      file_chmod,               edited_region_fn[jj], /U_WRITE
      
      ; Append "point" regions that are nearby.
      if (num_nearby_points GT 0) then begin
        openw, unit, edited_region_fn[jj], /GET_LUN, /APPEND
        printf, unit, nearby_point_region
        free_lun, unit
      endif
      
      if (NOT keyword_set(omit_bkg_regions)) AND file_test(bkg_pixels_region_fn[jj]) then $
        run_command, /UNIX, /QUIET, string(bkg_pixels_region_fn[jj], edited_region_fn[jj], F='(%"cat %s >>! %s")')
      
      ; Extract in-band neighborhood event list.
      cmd[jj] = string(env_events_fn[jj], 1000*energy_range, display_data_fn[jj], $
                   F="(%'dmcopy ""%s[energy=%6.1f:%7.1f]"" %s')")
    endfor ;jj
    run_command, cmd, /QUIET

            
    if file_test(merged_region_fn) then begin
      ; Copy composite polygons region file to a temporary region file.
      file_copy , merged_region_fn, temp_region_fn, /OVERWRITE
      file_chmod,                   temp_region_fn, /U_WRITE
    endif else file_delete, /ALLOW_NONEXISTENT, temp_region_fn
      
    ; Append "point" regions that are nearby.
    if (num_nearby_points GT 0) then begin
      openw, unit, temp_region_fn, /GET_LUN, /APPEND
      printf, unit, nearby_point_region
      free_lun, unit
    endif

    display_reg_fn = edited_region_fn
        
    if merge_available then begin
      ; Extract in-band neighborhood event list.
      cmd = string(merged_env_events_fn, 1000*energy_range, temp_events_fn, $
                   F="(%'dmcopy ""%s[energy=%6.1f:%7.1f]"" %s')")
      run_command, cmd, /QUIET
                 
      display_data_fn = [display_data_fn ,temp_events_fn]
      display_reg_fn  = [display_reg_fn  ,temp_region_fn]
    endif
    
    if recon_available then begin
      display_data_fn = [display_data_fn ,string(composite_img_fn, F="(%'""%s[1]""')")]
      display_reg_fn  = [display_reg_fn  ,temp_region_fn]
    endif
    
    ae_send_to_ds9, my_ds9, display_data_fn, display_reg_fn, DESIRED_ZOOM=desired_zoom
    
    ; Make sure the region coordinate system stays PHYSICAL.
    ; The reconstructed image has no PHYSICAL system and if it's loaded in ds9 the
    ; region system is unfortunately changed to IMAGE for all the frames.
    ; run_command, string(my_ds9, F='(%"xpaset -p ''%s'' regions system physical")'), /QUIET


    ;; ------------------------------------------------------------------------
    ;; While the user starts looking at images, do other work.
SHOW_STATS:    
    ; Display info to observer.
    obsname_status     = strarr(num_obs)
    obsnames_extracted = strarr(num_obs)
    emap_val   = fltarr(num_obs)
    off_angle  = fltarr(num_obs)
    src_counts = fltarr(num_obs)
    bkg_counts = fltarr(num_obs)
    backscal   = fltarr(num_obs)
    psf_frac   = fltarr(num_obs)
    VOTE_LO    = strarr(num_obs)
    VOTE_HI    = strarr(num_obs)
    
    for jj = 0, num_obs-1 do begin
      fdecomp, env_events_fn[jj], disk, dir
      stats_fn  = dir + obs_stats_basename
            
      ; Read this observation's statistics.
      stats = headfits(stats_fn)
      obsnames_extracted[jj] = strtrim(sxpar(stats, 'OBSNAME'),2)
      obsname_status    [jj] = total(/INT, strmatch(strsplit(/EXTRACT,obsnames_merged,','), obsnames_extracted[jj])) GT 0  ? '' : 'pruned'                     
      
      emap_val  [jj] = sxpar(stats, 'EMAP_AVG')
      off_angle [jj] = sxpar(stats, 'THETA')
      src_counts[jj] = sxpar(stats, 'SRC_CNTS')
      bkg_counts[jj] = sxpar(stats, 'BKG_CNTS')
      backscal  [jj] = sxpar(stats, 'BACKSCAL')
      psf_frac  [jj] = sxpar(stats, 'PSF_FRAC')
      temp           = sxpar(stats, 'VOTE_LO')
      VOTE_LO   [jj] = (temp LT sxpar(stats, 'BKSCL_HI')) ? string(round(temp),F='(%"%d")') : ''
      temp           = sxpar(stats, 'VOTE_HI')
      VOTE_HI   [jj] = (temp GT sxpar(stats, 'BKSCL_LO')) ? string(round(temp),F='(%"%d")') : ''
    endfor
    
    
    emap_val = emap_val / (total(emap_val) > 1)
    print
    print, energy_range_label, F="(%'     |        |%18s|')"
    print, '     |exposure|counts in aperture|      background      | off-axis  |'
    print, 'frame|   %    | total        bkg |counts scaling  vote  | angle ('') | psf_frac | ObsId'
    forprint, 1+indgen(num_obs), 100*emap_val, src_counts, bkg_counts/backscal, bkg_counts, round(backscal), VOTE_LO, VOTE_HI, off_angle, 100*psf_frac, obsnames_extracted, obsname_status, F='(%"%4d |%7d |%6d %10.1f |%5d %5d [%3s:%3s] | %9.1f | %8d | %s %s")'

    frame_number = num_obs
    if merge_available then print, ++frame_number, merge_subdir[ii]+env_events_basename, F='(%"%5d | merged data   : %s")'
    if recon_available then print, ++frame_number, merge_subdir[ii]+env_image_basename , F='(%"%5d | reconstruction: %s")'
    
    label_lowercase = strlowcase(label)
    
    ;; ------------------------------------------------------------------------
    ;; Wait for command.
COMMAND:
;    print, F='(%"\nTHE SOURCE MASK REGION is marked with the ds9 property ''background'' \n\rto distinguish it from the SOURCE EXTRACTION REGION.")'
    print, F='(%"\n\rCommands\n\r  n,+,'''' : next source\n\r   p,b,- : previous source\n\r       f : First source\n\r       l : Last source\n\r     xxx : source label")'
    if (sequence_number GE 0) then print, F='(%"       r : rejoin sequence after the current source")'
    print, F='(%"       s : save regions (extraction & mask) in seleted frame\n\r       g : click in ds9 to show coordinates\n\r       q : quit")'
    print, 'DO NOT save edited regions via ds9 menu!  Use "s" command instead!'
    
    cmd = ''
    read, '? ', cmd
    
    cmd = strlowcase(cmd)
    
    ;; Execute specified command.
    label_entered = 0
    case 1 of
      (cmd EQ 's'): $
        ; Save the regions in the selected frame.
        ; Make sure the region coordinate system remains PHYSICAL.
        ; The reconstructed image has no PHYSICAL system and if it's loaded in ds9 the
        ; region system is unfortunately changed to IMAGE for all the frames.
        begin
        run_command, string(my_ds9, F='(%"xpaget ''%s'' frame")'), current_frame, /QUIET
        current_frame = fix(current_frame)

        if (current_frame GT num_obs) then begin
          print, 'No observation in current frame.'
        endif else begin
          jj = current_frame-1
          cmd = strarr(3)
          cmd[0] = string(my_ds9,                       F='(%"xpaset -p ''%s'' regions system physical")')
          cmd[1] = string(my_ds9, edited_region_fn[jj], F='(%"xpaset -p ''%s'' regions save %s")')
          cmd[2] = string(edited_region_fn[jj], region_fn[jj], F="(%'grep -v point %s >! %s')")
          run_command, cmd, /QUIET
          print, 'Saved region file ', region_fn[jj]
        endelse
        goto, COMMAND
        end

      (cmd EQ 'g'): $
        begin
         print, F="(%'\n\n  Left-click in ds9 to print coordinates.')" 
      
         ; Ask ds9 for a mouse click or keystroke and parse the result
         run_command, /QUIET, string(my_ds9, F='(%"xpaget %s imexam any coordinate wcs fk5 degrees")'), result
         tokens = stregex(result,'(.+) +([0-9]+\.[0-9]+) +(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
         keystroke = strtrim(tokens[1],2)
         ra_click  = double(tokens[2])
         dec_click = double(tokens[3])
         print, ra_click, dec_click, F='(%"%10.6fD  %10.6fD")'
        goto, COMMAND
        end
        
      (cmd EQ 'n') OR (cmd EQ '+') OR (cmd EQ '')  : presentation_index++
      (cmd EQ 'p') OR (cmd EQ 'b') OR (cmd EQ '-') : presentation_index = 0 > --presentation_index
      (cmd EQ 'f')                                 : presentation_index = 0
      (cmd EQ 'l')                                 : presentation_index = n_elements(presentation_list) - 1
      (cmd EQ 'q')                                 : ii = num_sources

      (cmd EQ 'r'): $
        ; Jump the presentation_index to the current source.
        begin
        if (sequence_number GE 0) then presentation_index = 1+sequence_number
        end
      
      else: $ ; User typed a source label.
        begin
        label_entered = 1
        ind = where(cmd EQ label_lowercase, count)
        if (count GE 1) then ii = ind[0]
        end
    endcase
    
    if (presentation_index GE n_elements(presentation_list)) || (ii GE num_sources) then break
    if ~label_entered then ii = presentation_list[presentation_index]

    
    if NOT pan_only then begin
      ; Record the zoom setting the user left in the currently-selected frame.
      run_command, string(my_ds9, F='(%"xpaget ''%s'' zoom")'), /QUIET, result
      desired_zoom = float(result[0])
    endif
  endwhile
  
  run_command, string(my_ds9, F='(%"xpaset -p ''%s'' exit")'), /QUIET
endif ;keyword_set(show_regions)



;; =============================================================================
if keyword_set(extract_events) OR keyword_set(extract_spectra) then begin
;; =============================================================================
  obsdata_header = headfits(obsdata_filename, EXT=1, ERRMSG=error )
  if (keyword_set(error)) then message, 'ERROR reading ' + obsdata_filename
  
  nominal_exposure = sxpar(obsdata_header, 'EXPOSURE')

  if ~keyword_set(DETCHANS) then DETCHANS = 1024

  rmf_dir = ''
  if keyword_set(extract_spectra) then begin
    dafile  = 'NONE'
    if keyword_set(pbkfile) then begin
      if (pbkfile NE 'NONE') then begin
        if NOT file_test(pbkfile) then begin
          print, 'ERROR: the supplied PBKFILE file ', pbkfile, ' is not found!'
          GOTO, FAILURE
        endif
        dafile  = 'CALDB'
      endif
    endif else begin
      print, 'WARNING: PBKFILE parameter omitted; Dead Area correction will be skipped.'
      pbkfile = 'NONE'
    endelse
    
    if keyword_set(mskfile) then begin
      if (mskfile NE 'NONE') then begin
        if NOT file_test(mskfile) then begin
          print, 'ERROR: the supplied MSKFILE file ', mskfile, ' is not found!'
          GOTO, FAILURE
        endif
      endif
    endif else begin
      print, 'WARNING: MSKFILE parameter omitted; .'
      mskfile = 'NONE'
    endelse
    
    ;; Copy observer-supplied ardlib.par to private PFILES directory. 
    if NOT keyword_set(ardlib_filename) then begin
      print, 'ERROR: you must supply ARDLIB_FILENAME in /EXTRACT_SPECTRA stage.'
      GOTO, FAILURE
    endif

    if NOT file_test(ardlib_filename) then begin
      print, ardlib_filename, F='(%"ERROR: ARDLIB_FILENAME %s not found.")'
      GOTO, FAILURE
    endif
    
    file_copy, ardlib_filename, cache_dir + 'ardlib.par'
    
    ;; Figure out if this is a gratings observation.
    grating_spec = strtrim(sxpar(obsdata_header, 'GRATING'),2)
    case grating_spec of
      'NONE':
      'HETG':
      'LETG': print, 'WARNING: the PSF library is not correct for LETG data; various AE calculations (especially PSF fraction) will be incorrect!'
      else  : begin
              print, 'ERROR: the GRATING keyword in '+obsdata_filename+'has an invalid value.'
              GOTO, FAILURE
              end
    endcase
    
    ;; Sanity-check the aspect histograms supplied.
    for ccd_id = 0,9 do begin
      asphist_fn = asphist_dir + string(ccd_id, F='(%"/ccd%d.asphist")')
      keyname    = string(ccd_id, F='(%"EXPOSUR%d")')
      exposure   = sxpar(obsdata_header, keyname)
      
      if (exposure GT 0) && file_test(asphist_fn) then begin
        print, 'Verifying total exposure in ', asphist_fn
        bt = mrdfits(asphist_fn, 1, /SILENT, STATUS=status)
        if (status NE 0) then message, 'ERROR reading ' + asphist_fn
  
        if (n_elements(bt) EQ 1) then begin
          print, 'WARNING!  The aspect histogram ', asphist_fn, ' was empty.'
          wait, 1
          continue
        endif
        
        duration = total(bt.duration)
        
        if (abs(duration-exposure)/duration GT 0.01) then begin
          print, duration, asphist_fn, keyname, exposure, $
                 F='(%"WARNING!  Sum of DURATION column (%d) in %s does not match \n\rvalue of keyword %s (%d)!")'
          print, 'It is likely that your aspect histogram was not computed correctly!!!!!'
          wait, 1
        endif
      endif
    endfor

    ;; Remind the user about what's in ardlib.
    run_command, 'paccess ardlib r', ardlib_path, /QUIET
    print, ardlib_path, F='(%"\n \nExamining important parameters in %s")'
    
    run_command, 'pdump ardlib', ardlib_contents, /QUIET
    print, F='(%"\nThe bad pixel list parameters below should correspond to your observation:")'
    forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_ACIS?_BADPIX*'))]
    
    ; Determine whether CALDB or Townsley CTI correction is specified.
    print

    forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_RMF_FILE*'))]
    run_command, 'pget -abort ardlib AXAF_RMF_FILE', rmf_dir, /QUIET
    rmf_dir = strtrim(rmf_dir,2)

    if (n_elements(rmf_dir) GT 1) then begin
      print, 'ERROR: pget command unexpectedly returned multiple lines:'
      print, rmf_dir
      GOTO, FAILURE
    endif
    
    if (rmf_dir EQ 'CALDB') then begin
      print, F='(%"\nThe AXAF_RMF_FILE parameter claims your data are CTI corrected by CALDB.\nThe QEU files below should also refer to the CALDB.")'
      forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_ACIS?_QEU*'))]
 
      
    endif else if file_test(rmf_dir, /DIRECTORY) then begin
      print, F='(%"\nThe AXAF_RMF_FILE parameter claims your data are CTI corrected by Townsley et al.\nThe QEU files below should also refer to the Townsley et al. calibration library.")'
      forprint, ardlib_contents[where(strmatch(ardlib_contents,'AXAF_ACIS?_QEU*'))]
      
       ; The source.rmf for many sources will turn out to be a sym link to the Townsley RMF library.  To make the extraction directories portable we want these sym links to reference a sym link to the library which can be easily changed later.  To make life easy on the user we will manage this library sym link in the extraction directory.
       ; Save the path supplied by the user, and strip off any trailing slash and any double slash since file_readlink() does not return them.
       rmf_dir_supplied = repstr(rmf_dir,'//','/')
       if (strmid(rmf_dir_supplied,0,/REVERSE) EQ '/') then $
               rmf_dir_supplied = strmid(rmf_dir_supplied,0,strlen(rmf_dir_supplied)-1)
       
       for ii = 1,10 do begin
         rmf_dir = string(ii, F='(%"RMF_LIBRARY%d")')
         
         ; If there's an existing sym link by this name then read it and see if it points to rmf_dir_supplied.
         if file_test(rmf_dir, /SYMLINK) then begin
           if (file_readlink(rmf_dir) EQ rmf_dir_supplied) then break
           ; Not the link we need; try another name.
           continue
         endif
        
         ; If there's some other type of object with this name then try another name.
         if file_test(rmf_dir) OR file_test(rmf_dir,/DANGLING) then continue
         
         ; The name is free, so make the symlink we desire.
         file_link, rmf_dir_supplied, rmf_dir, /VERBOSE
         break
       endfor
       
       if (file_readlink(rmf_dir) NE rmf_dir_supplied) then begin
         print, 'ERROR: could not create symbolic link ', rmf_dir, ' to ', rmf_dir_supplied
         GOTO, FAILURE
       endif
       
       ; For convenience later make sure rmf_dir ends in slash.
       rmf_dir = rmf_dir + '/'

    endif else begin
      print, 'ERROR: the AXAF_RMF_FILE parameter value "',rmf_dir,'" must be CALDB or the path to the Townsley et al. calibration library.'
      GOTO, FAILURE
    endelse    
;    wait, 10
    

    ; CIAO 4.1 requires the CTI_APP keyword in the event file.
    CTI_APP  = sxpar(obsdata_header, 'CTI_APP' , COUNT=CTI_APP_available)
    if ~CTI_APP_available then begin
      print, 'ERROR: CIAO (v4.1 and above) requires the CTI_APP keyword in the event file!  See http://cxc.harvard.edu/ciao4.1/why/cti.html'
      GOTO, FAILURE
    endif
    

    ; Figure out whether to use mkrmf or mkacisrmf or the Townsley RMF library.
    if keyword_set(use_mkrmf) then begin
      print, 'WARNING!  You have specified that RMFs are generated by the tool mkrmf!  '
      rmf_tool = 'mkrmf'

    endif else if (rmf_dir EQ 'CALDB') then begin
      ; With modern data, we let CALDB match up the GAINFILE and phase 2 response files as shown on the ahelp page for mkacisrmf.
      gainfile             = 'CALDB'
      phase2_response_file = 'CALDB'
      rmf_tool = 'mkacisrmf'
      
     endif else begin
      rmf_tool = 'Townsley'
      ; Make spectra with 685 channels to match RMFs from CTI corrector.
      DETCHANS = 685
    endelse

    
;    ; Determine if CIAO is configured to deal with OBF contamination 
;    ; (ardlib says CALDB and the Cal Database actually contains the magic file).
;    run_command, 'pget -abort ardlib AXAF_ACIS0_CONTAM_FILE', contam_param, /QUIET
;    contam_file = "$CALDB/data/chandra/acis/contam/acisD1999-08-13contamN0004.fits"
;    contam_enabled = ((contam_param EQ 'CALDB') AND file_test(contam_file)) OR file_test(contam_param)
;    if contam_enabled then begin
;      print, F='(%"\nWARNING! Correction for ACIS contamination appears to be enabled in CIAO/CALDB.")'
;      forprint, ardlib_contents[where(strmatch(ardlib_contents,'*AXAF_ACIS?_CONTAM_FILE*'))]
;      print, 'DO NOT run the ARF_CORRECTION_FILENAME stage to correct for contamination.'
;    endif else begin
;      print, F='(%"\nCorrection for ACIS contamination appears to be disabled in CIAO/CALDB.")'
;      print, 'You should update CIAO/CALDB or run the ARF_CORRECTION_FILENAME stage with your own contamination correction.'
;    endelse
    
    print
    print, 'PAUSING FOR 10 SECONDS SO YOU CAN REVIEW ARDLIB INFORMATION ABOVE:'
    wait, 10
  endif  ;keyword_set(extract_spectra)
  
  
  energy0 = fltarr(num_sources)
  energy1 = fltarr(num_sources)
  
  case n_elements(time_filter) of
    0:           time_filter_spec = replicate(                      '',num_sources)
    1:           time_filter_spec = replicate('[time='+time_filter+']',num_sources)
    num_sources: time_filter_spec =           '[time='+time_filter+']'
    else: begin                               
          print, 'ERROR: parameter TIME_FILTER must be a scaler or a vector as long as the catalog'
          GOTO, FAILURE
          end
  endcase


  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be off-field.
    if source_not_observed[ii] then continue

    ;; Construct filenames.
    obsdir      = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    psf_fn      = sourcename[ii] + '/' + obsname + '/' + psf_basename
    
    rmf_dir_from_source = '../../' + rmf_dir
    if keyword_set(merge_subdir[ii]) then rmf_dir_from_source = '../' + rmf_dir_from_source
     
    stats_fn        = obsdir + obs_stats_basename
    psf_frac_fn     = obsdir + obs_frac_basename
    env_events_fn   = obsdir + env_events_basename
    region_fn       = obsdir + src_region_basename
    src_emap_fn     = obsdir + src_emap_basename
    src_events_fn   = obsdir + src_events_basename
    src_spectrum_fn = obsdir + src_spectrum_basename
    rmf_fn          = obsdir + rmf_basename
    mkrmf_log_fn    = obsdir + "mkrmf.log"
    mkrmf_wgt_fn    = obsdir + "mkrmf.wgt"
    arf_fn          = obsdir + arf_basename
    event_plot_fn   = obsdir + event_plot_basename
    
    if (NOT file_test(stats_fn)) then begin
;     print, F='(%"\n===================================================================")'
;     print, 'Source: ', sourcename[ii]
;     print, 'EXTRACTION SKIPPED: source not observed.'
      source_not_observed[ii] = 1
      continue
    endif

    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, ['pset dmcopy clobber=yes', 'pset dmextract clobber=yes', 'pset dmimgpick clobber=yes', 'pset mkarf clobber=yes', 'pset mkwarf clobber=yes', 'pset mkrmf clobber=yes verbose=2', 'pset mkacisrmf clobber=yes verbose=2', 'pset addrmf clobber=yes']
    
    print, F='(%"\n===================================================================")'
    print, 'Source: ', sourcename[ii]
    stats = headfits(stats_fn)
    xpos_catalog = sxpar(stats, 'X_CAT')
    ypos_catalog = sxpar(stats, 'Y_CAT')
    is_diffuse   = sxpar(stats, 'DIFFUSE')


    ;; ------------------------------------------------------------------------
    ;; Find & format the extraction region specifications in the region file.
    
    ae_ds9_to_ciao_regionfile, region_fn, temp_region_fn, /IGNORE_BACKGROUND_TAG, REGION_EDITED=region_edited
    
    if (keyword_set(only_edited) AND (region_edited EQ 'F')) then continue


    ;; ------------------------------------------------------------------------
    ;; Construct neighborhood event list. 
    build_neighborhood = 1
    if keyword_set(reuse_neighborhood) then begin
      build_neighborhood = NOT file_test(env_events_fn)
    endif
    
    if build_neighborhood then begin
      image_radius = round((neighborhood_size/2.0/arcsec_per_skypixel) > $  ; sky pixels
                            1.5*sxpar(stats, 'MSK_RAD')                   ) ; sky pixels
      image_dim    = (2*image_radius) + 1                     ; sky pixels
      cx = xpos_catalog - image_dim/2.0   
      cy = ypos_catalog - image_dim/2.0   
      
      cmd = string(obsdata_filename, cx,cx+image_dim, $
                                     cy,cy+image_dim, time_filter_spec[ii], env_events_fn, $
                   F="(%'dmcopy ""%s[x=%d:%d, y=%d:%d]%s"" %s')")
      run_command, cmd
    endif
    
    ; We have to set TDMIN & TDMAX to get ds9 to produce a nice default binning of the data.
    neighborhood_events = mrdfits(env_events_fn, 1, env_header, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + env_events_fn

    fxbfind, env_header, 'TTYPE', dum1, TTYPE, dum2, 'null'
    colnames = strlowcase( strtrim(TTYPE,2) )
    x_colnum = 1+where(strlowcase(colnames) EQ 'x')
    y_colnum = 1+where(strlowcase(colnames) EQ 'y')
    
    if build_neighborhood then begin
      openw, unit, temp_text_fn, /GET_LUN
      printf, unit, x_colnum, cx, x_colnum, cx+image_dim, $
                    y_colnum, cy, y_colnum, cy+image_dim, $
                    F='(%"#add\nTDMIN%d=%d\nTDMAX%d=%d\nTDMIN%d=%d\nTDMAX%d=%d")'
      free_lun, unit
      
      cmd = string(env_events_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
      run_command, cmd, /QUIET
     endif 
    
    ; The dmextract call done later will compute BACKSCAL as a fraction of the "area" of its input file, 
    ; which is env_events_fn (neighborhood.evt).  We want to calculate this area now (in skypix^2) so that
    ; we can later rescale the BACKSCAL value produced by dmextract to get an aperture area in physical units.
    ; CIAO represents the area of neighborhood.evt by the TLMIN and TLMAX keywords; we read them here, rather 
    ; than hard-coding their usual values, because early versions of AE changed TLMIN/TLMAX (to make ds9 happy).
    fxbfind, env_header, 'TLMIN', dum1, TLMIN, dum2, 0.0
    fxbfind, env_header, 'TLMAX', dum1, TLMAX, dum2, 0.0
    neighborhood_area = ( (TLMAX[x_colnum-1]-TLMIN[x_colnum-1])*(TLMAX[y_colnum-1]-TLMIN[y_colnum-1]) )[0]
    
    ;; ------------------------------------------------------------------------
    ;; Compute PSF fractions using the PSF images made earlier by dividing the 
    ;; light falling inside the source region by the total light.  
    f_nan = !VALUES.F_NAN

    fits_open, psf_fn, fcb, /NO_ABORT, MESSAGE=error
    if keyword_set(error) then begin
      ; A PSF is not available for diffuse sources.  For point sources it might be
      ; the case that a reasonable PSF is not available. 
      psf_fraction = replicate({energy:0.0D, fraction:0.0,     x_sdev:f_nan,     y_sdev:f_nan, $
                                                           bkg_x_sdev:f_nan, bkg_y_sdev:f_nan}, 2)
      psf_fraction.energy   = [0,10]
      psf_fraction.fraction = [1,1]
      fiducial_psf_energy   = f_nan
      if (NOT is_diffuse) then print, 'WARNING: no PSFs found; using PSF Fraction of 1.0'

    endif else begin
      ; PSF is available.
      ; If /REGION_ONLY then compute only 1 PSF fraction to save time.
      num_psf_images = keyword_set(region_only) ? 1 : 1 + fcb.NEXTEND
      psf_fraction = replicate({energy:0.0D, fraction:0.0,     x_sdev:f_nan,     y_sdev:f_nan, $
                                                           bkg_x_sdev:f_nan, bkg_y_sdev:f_nan}, num_psf_images)
    
      for jj =0, num_psf_images-1 do begin
        fits_read, fcb, dum, psf_header, /HEADER_ONLY, /NO_PDU, EXTEN_NO=jj
        psf_fraction[jj].energy = sxpar(psf_header, 'ENERGY')
        psf_total               = sxpar(psf_header, 'PSF_TOTL')
        if (psf_total EQ 0) then begin
          print, "WARNING: obsolete PSFs in "+psf_fn+" may have incorrect scaling.  AE versions 3.6 or higher can make better PSFs, but REMEMBER THAT THE /CONSTRUCT_REGIONS STAGE WILL OVERWRITE THE SOURCE'S EXTRACTION REGION FILE!"
          fits_read, fcb, psf_img, EXTEN_NO=jj
          psf_total = total(psf_img, /DOUBLE)
          help, psf_total
        endif
        
        cmd = string(psf_fn, 1+jj, temp_region_fn, temp_image_fn, $
                   F="(%'dmcopy ""%s[PSF%d][sky=region(%s)][opt update=no]"" %s')")
        run_command, cmd
        
        psf_img = readfits(temp_image_fn, psf_header, /SILENT)
        ind = where(finite(psf_img) EQ 0, num_nan)
        if (num_nan GT 0) then begin
          ind = where(psf_img GT 0, num_pos)
          print, 100*num_pos/float(num_nan+num_pos), $
            F='(%"WARNING: PSF model covers only %5.2f%% of extraction region; PSF fraction is underestimated")'
        endif
        
        ; PSF Fraction is power in region divided by total power to infinity.
        psf_total_in_region       = total(psf_img, /DOUBLE, /NAN)
        psf_fraction[jj].fraction = psf_total_in_region / psf_total
        
        
        ; It would be great to save the PSF truncated by the extraction region, 
        ; plus a flat background image truncated, so that later we could use them both 
        ; to build a 2-D parent distribution for the events extracted from multiple obsids  .
        ; However that would take a lot of space!  So, below we save 1-D descriptions of 
        ; these distributions.
        ;
        ; Compute the standard deviation of the PSF inside the extraction region for use later in position error estimates.  
        ; First let's compute the CENTROID of the psf_img array inside the extraction region, expressed in the array index coordinate system, by computing  marginal sums of psf_img weighted by the array indexes.  
        ; The denominator is NOT psf_total as it was above for the PSF fraction.
        xdim = (size(psf_img, /DIM))[0]
        ydim = (size(psf_img, /DIM))[1]
        x_ind = lindgen(xdim,ydim) mod xdim
        y_ind = lindgen(xdim,ydim)  /  xdim
        
        xpos_psf = total( psf_img * x_ind, /DOUB, /NAN ) / psf_total_in_region
        ypos_psf = total( psf_img * y_ind, /DOUB, /NAN ) / psf_total_in_region
        
        ; Then we compute the variances of this distribution along the two array index axes.
        x_variance = total( psf_img * (x_ind-xpos_psf)^2, /DOUB, /NAN ) / psf_total_in_region
        y_variance = total( psf_img * (y_ind-ypos_psf)^2, /DOUB, /NAN ) / psf_total_in_region

        ; Finally compute standard deviations and convert to units of sky pixels
        psf_fraction[jj].x_sdev = sqrt(x_variance) * sxpar(psf_header, 'CDELT1P')
        psf_fraction[jj].y_sdev = sqrt(y_variance) * sxpar(psf_header, 'CDELT2P')

        ; Compute the single-axis variances of a flat background truncated by the extraction region.
        ; Compute standard deviations and convert to units of sky pixels
        psf_footprint  = finite(psf_img) AND (psf_img GT 0)
        bkg_x_variance = total( psf_footprint * (x_ind-xpos_psf)^2, /DOUB, /NAN ) / total(psf_footprint)
        bkg_y_variance = total( psf_footprint * (y_ind-ypos_psf)^2, /DOUB, /NAN ) / total(psf_footprint)
  
        psf_fraction[jj].bkg_x_sdev = sqrt(bkg_x_variance) * sxpar(psf_header, 'CDELT1P')
        psf_fraction[jj].bkg_y_sdev = sqrt(bkg_y_variance) * sxpar(psf_header, 'CDELT2P')
      endfor ;jj
      fits_close, fcb
      
      ;; Sort psf_fraction by energy so its ready for interpolation later.
      psf_fraction = psf_fraction[ sort(psf_fraction.energy) ]
      
      fiducial_psf_energy = sxpar(stats, 'PSF_ENGY')
      
    endelse ;  PSF  available
    
    
    ;; Find the PSF that's at the fiducial energy.
    ind = where(abs(psf_fraction.energy - fiducial_psf_energy) LT 0.001, count)
    if (count EQ 0) then ind=[0]
    fiducial_psf_fraction = psf_fraction[ind[0]]
    

    fxhmake,  pheader, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, pheader, 'CREATOR', creator_string
    writefits, psf_frac_fn, 0, pheader

    theader = 0   &   dum=temporary(theader)
    fxaddpar, theader, 'EXTNAME', 'PSF_FRACTION'
    mwrfits, psf_fraction, psf_frac_fn, theader
    
    fxaddpar, stats, 'REG_EDIT', region_edited, 'T=region edited in ds9'
    fxaddpar, stats, 'PSF_FRAC', fiducial_psf_fraction.fraction, string(fiducial_psf_fraction.energy, F='(%"PSF fraction via DM @%6.4f keV")')

    
    ;; ------------------------------------------------------------------------
    ;; Find the set of emap pixels that fall within the region, and compute various 
    ;; statistics on those emap values.
    ;; For point sources we'll later use mean_exposure; for diffuse sources we'll 
    ;; later use the integral of the emap.

    cmd = string(emap_filename, temp_region_fn, src_emap_fn, $
                 F="(%'dmcopy ""%s[sky=region(%s)][opt null=-1]"" %s')")
    run_command, cmd
    
    emap = readfits(src_emap_fn, emap_header, /SILENT)
    ; We're including the emap=0 pixels in our emap statistics.
    ind  = where(emap NE -1, num_emap_pixels)

    if (num_emap_pixels GT 0) then begin
      emap_pixels = emap[ind]
    endif else begin
      ; No pixel centers fall in extraction polygon, so use closest emap value.
      emap_pixels = [sxpar(stats, 'EMAP_AVG')]
      print, 'No emap pixel centers are in source region; using closest emap value EMAP_AVG=', emap_pixels
    endelse
    
    ; If a time filter was supplied we must scale down the emap by the exposure fraction.
    if keyword_set(time_filter_spec[ii]) then begin
      exposure_fraction = sxpar(env_header, 'EXPOSURE') / nominal_exposure
      help, exposure_fraction
      emap_pixels = temporary(emap_pixels) * exposure_fraction
    endif
    
    mean_exposure     = mean  (emap_pixels, /DOUBLE)
    median_exposure   = median(emap_pixels, /EVEN)
    min_exposure      = min   (emap_pixels, MAX=max_exposure)
    ; Ignore any negative emap pixels.
    integral_exposure = total (emap_pixels>0, /DOUBLE)

    ;; ------------------------------------------------------------------------
    ;; Extract the total-band source event list.
    ;;
    ;; For point sources (small regions where edges are complex with respect to 
    ;; the pixelization of the emap) we use the exact region.
    ;;
    ;; For diffuse sources (large regions with unimportant edges) we use the set
    ;; of emap pixels above to define the actual source region to apply to the event list.
    ;; The region boundary is made to be "pixelized".
    ;; 
    ;; We extract from neighborhood events instead of full event list to save 
    ;; significant time.
    if (NOT is_diffuse) then begin
      ; Point Source
      cmd = string(env_events_fn, temp_region_fn, src_events_fn, $
                   F="(%'dmcopy ""%s[sky=region(%s)]"" %s')")
      run_command, cmd

    endif else begin
      ;; Diffuse Source
      ;; BELOW WE REQUIRE EMAP VALUE TO BE >1, INSTEAD OF >0, BECAUSE CIAO 3.0.1 HAS A BUG THAT 
      ;; CAUSES ZERO VALUES TO PASS THE >0 TEST!
      
      cmd1 = string(env_events_fn, src_emap_fn, temp_events_fn, $
                   F="(%'dmimgpick ""%s[cols time,ccd_id,chip,det,sky,pi,energy]"" %s %s method=closest')")

      cmd2 = string(temp_events_fn, src_events_fn, F="(%'dmcopy ""%s[#8>1]"" %s')")
      run_command, [cmd1,cmd2]
    endelse
    
    wideband_events = mrdfits(src_events_fn, 1, src_events_hdr, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + src_events_fn

    wideband_src_counts = sxpar(src_events_hdr, 'NAXIS2')


    ;; ------------------------------------------------------------------------
    ;; Determine if any source events fall in the "warning region" supplied.
    if keyword_set(warning_region_filename) then begin
      cmd = string(src_events_fn, warning_region_filename, temp_events_fn, $
                 F="(%'dmcopy ""%s[sky=region(%s)]"" %s')")
      run_command, cmd

      warning_events = mrdfits(temp_events_fn, 1, /SILENT, STATUS=status)
      if (status NE 0) then message, 'ERROR reading ' + temp_events_fn

      num_warning_events = sxpar(headfits(temp_events_fn, EXT=1), 'NAXIS2')
      fxaddpar, stats, 'WARNFRAC', float(num_warning_events)/(1>wideband_src_counts), 'fraction of events in warning region'
    endif else sxdelpar, stats, ['WARNFRAC']
    
   
    ;; ------------------------------------------------------------------------
    ;; Extract in-band source event list to compute statistics.
    ;; We do the energy filtering in IDL rather than CIAO for speed.
    if (wideband_src_counts EQ 0) then begin
      inband_src_counts = 0
    endif else begin
      inband_ind = where((1000*energy_range[0] LE wideband_events.energy) AND (wideband_events.energy LE 1000*energy_range[1]), inband_src_counts)
    endelse

    if (inband_src_counts EQ 0) then begin
      ;; There are no in-band data so we skip various statistics.
      print, 'WARNING: no in-band data found in source region.'
      
      sxdelpar, stats, ['X_DATA','Y_DATA','EX_DATA','EY_DATA','CAT2DATA','MEDIAN_E']
      fxaddpar, stats, 'SRC_CNTS', 0, string(energy_range, F="(%'source counts, %5.3f:%6.3f keV, in extraction region')")

      ;; We'd like to create a fake event at the source position in the structure inband_events 
      ;; because it's used often in the code below (e.g. to figure out for which CCDs we need ARFs).
      cmd = string(obsdata_filename, keyword_set(aspect_fn) ? aspect_fn : '', xpos_catalog, ypos_catalog,  F="(%'dmcoords %s asolfile=%s opt=sky x=%6.1f y=%6.1f')")
      run_command, /QUIET, cmd

      run_command, /QUIET, 'pget dmcoords chip_id chipx chipy', result
      chip_id = fix(result[0])
      chipx   = fix(result[1])
      chipy   = fix(result[2])

      if (chipx LT 1) OR (chipx GT 1024) OR (chipy LT 1) OR (chipy GT 1024) then begin
        ;; But ... the dmcoords tool can sometimes return silly results, such as
        ;;   chip_id=3, chipx=-1899, chipy=2028
        ;; So, if that happens we'll use values from the nearest event.
        dum = min( (neighborhood_events.x - xpos_catalog)^2 + (neighborhood_events.y - ypos_catalog)^2, ind )
        chip_id = neighborhood_events[ind].ccd_id
        chipx   = neighborhood_events[ind].chipx
        chipy   = neighborhood_events[ind].chipy  
        print, 'dmcoords produced silly result; using nearest event'
      endif

      inband_events = {x:xpos_catalog, y:ypos_catalog, energy:1.0, ccd_id:chip_id, chipx:chipx, chipy:chipy }

      ;; We should NOT skip the rest of the extraction!  We need the empty spectrum to
      ;; carry forward the information about this observation's exposure, even if no events
      ;; were detected.

    endif else begin
      ;; Normal case: some in-band events found.
      inband_events = wideband_events[inband_ind]

      ; Compute mean of data, and the "standard error of the mean" by dividing by sqrt(N).
      ; That formula is asymptotically correct as N gets large.  I don't know what to do for small N.
      ; We do NOT use the Student's T distribution for this calculation.  The Student's T is applied in the case where the variance of the parent distribution is (poorly) estimated from the N data points. In our case we accurately compute the variance from the PSF which is assumed to be the parent distribution.
      xpos_data_i = mean(inband_events.x, /DOUBLE)
      ypos_data_i = mean(inband_events.y, /DOUBLE)
      
      er_xpos_data_i = fiducial_psf_fraction.x_sdev / SQRT(inband_src_counts)
      er_ypos_data_i = fiducial_psf_fraction.y_sdev / SQRT(inband_src_counts)

      
      median_energy_i   = median([inband_events.energy], /EVEN)
      cat2data_offset_i = sqrt((xpos_catalog-xpos_data_i)^2     + (ypos_catalog-ypos_data_i)^2)
            
      fxaddpar, stats, 'X_DATA',   xpos_data_i, 'source position (sky coordinates), mean of data'
      fxaddpar, stats, 'Y_DATA',   ypos_data_i, 'source position (sky coordinates), mean of data'
      fxaddpar, stats, 'EX_DATA',  er_xpos_data_i, '1-sigma error on X_DATA'
      fxaddpar, stats, 'EY_DATA',  er_ypos_data_i, '1-sigma error on Y_DATA'
      fxaddpar, stats, 'CAT2DATA', cat2data_offset_i, 'catalog to data offset (sky pixels)'
     ;fxaddpar, stats, 'MEDIAN_E', median_energy_i,   string(energy_range, F="(%'median energy, %5.3f:%6.3f keV, in extraction region')")
      fxaddpar, stats, 'SRC_CNTS', inband_src_counts, string(energy_range, F="(%'source counts, %5.3f:%6.3f keV, in extraction region')")
  
  
      ;; Save lowest two in-band energies for Garmire NH estimation.
      ind = sort(inband_events.energy)
      energy0[ii] = inband_events[ind[0]].energy
      if (inband_src_counts GT 1) then energy1[ii] = inband_events[ind[1]].energy

    endelse ; inband_src_counts > 0

    fxaddpar, stats, 'EMAP_NUM', num_emap_pixels, '# exposure map pixels in aperture'
    fxaddpar, stats, 'EMAP_AVG', mean_exposure,   'mean exposure map value in aperture'
    fxaddpar, stats, 'EMAP_MED', median_exposure, 'median exposure map value in aperture'
    fxaddpar, stats, 'EMAP_MIN', min_exposure,    'min exposure map value in aperture'
    fxaddpar, stats, 'EMAP_MAX', max_exposure,    'max exposure map value in aperture'
    fxaddpar, stats, 'CREATOR', creator_string
    writefits, stats_fn, 0, stats
    
    ;; ------------------------------------------------------------------------
    ;; If /EXTRACT_SPECTRA not specified, then skip everything else in loop.
    if NOT keyword_set(extract_spectra) then continue
    
  
    ;; ------------------------------------------------------------------------
    ;; Create source spectrum and assign a BACKSCAL value that equal to the integral 
    ;; of the exposure map over the source extraction region (the AE convention).  
    ;; The exposure map carries information both on how much _time_ each section of the
    ;; sky was observed (due to dithering) and how _sensitive_ (effective area at the energy
    ;; used to create the emap) the observatory is at each location.  The integration is how 
    ;; the geometric area of the region comes into play.
    ;; Thus the observed background signal at each location is proportional to the emap there
    ;; (integration time & sensitivity), and the total background counts detected in the
    ;; extraction region is the sum (integral) of the observed signal across the region.

    if (NOT is_diffuse) then begin
      ; Point Source
      ; The natural method would be to simply dmextract from src_events_fn (where polygon is 
      ; already applied), but alas dmextract computes BACKSCAL as the polygon region area 
      ; divided by the "field area".
      ; To be compatible with BACKSCALs computed in background spectra, we want here the 
      ; actual area of the polygon in pixel^2.
      ; When we extract from neighborhood.evt, dmextract computes BACKSCAL as the fractional
      ; area of the polygon, with respect to the field are (neighborhood_area) which we know.
    
      ; Also, in CIAO 2.3 we had some instances where dmextracting from src_events_fn led to BACKSCAL=0!
      cmd = string(env_events_fn, temp_region_fn, DETCHANS, src_spectrum_fn, $
                 F="(%'dmextract ""%s[sky=region(%s)][bin pi=1:%d:1]"" %s opt=pha1 error=gaussian')")
      run_command, cmd
        
      src_header = headfits(src_spectrum_fn, EXT=1)
      src_area   = neighborhood_area * sxpar(src_header, 'BACKSCAL')
;help, src_area, neighborhood_area, sxpar(src_header, 'BACKSCAL')

      if (src_area EQ 0) then message, 'ERROR: dmextract produced BACKSCAL=0!'
      src_area_comment = 'src extraction area (skypix^2) by dmextract'

      ; We do NOT simply sum up the emap pixels that fall inside the region 
      ; (and multiply by the pixel area) because the emap binning may be 
      ; quite coarse compared to the polygon, and the area of the polygon
      ; would be only crudely approximated by the number of emap pixels contained.
      ; Instead we multiply the geometric area of the region, as computed by CIAO, 
      ; times the mean exposure map value in the source region.
      src_exposurearea = src_area*mean_exposure

    endif else begin
      ; Diffuse Source
      
      ; If the aperture is empty, then mkwarf and mkacisrmf are going to fail, so print a warning and abort this extraction.
      if (wideband_src_counts EQ 0) then begin
        print, stats_fn, F='(%"WARNING! Aborted extraction because no data found in aperture! You should investigate, and then remove %s so that the MERGE will not see it.")'
        continue
      endif

      ; In this case we've "pixelized" the region above when we extracted source.evt above.
      ; We don't care about DM's estimate of the geometric area so we can just make a 
      ; spectrum from source.evt. 
      ; We include the WMAP option to support mkwarf and mkacisrmf later.
      ; We do NOT follow the recommendation on ahelp pages to bin the WMAP as [bin det=8] for 3 reasons:
      ;   1. We think this coarse binning could increase the chance of error messages in mkwarf and mkacisrmf at the field edges.
      ;   2. Omitting ranges for detx/dety leads to a WMAP that has a huge margin of zeros around the detector.
      ;   3. Testing shows that mkacisrmf has the same run-time with x1 and x8 binning in the WMAP.
      min_detx = min (wideband_events.detx, MAX=max_detx)
      min_dety = min (wideband_events.dety, MAX=max_dety)
      cmd = string(src_events_fn, DETCHANS, src_spectrum_fn, 500, 2000, min_detx-1, max_detx+1, min_dety-1, max_dety+1, $
                   F="(%'dmextract ""%s[bin pi=1:%d:1]"" %s opt=pha1 error=gaussian wmap=""[energy=%6.1f:%7.1f][bin detx=%d:%d,dety=%d:%d]""')")
      run_command, cmd
      
      ; If the WMAP is empty, then mkwarf and mkacisrmf are going to fail, so print a warning and abort this extraction.
      wmap = readfits(src_spectrum_fn, /SILENT)
      if (total(/INT, wmap) EQ 0) then begin
        print, stats_fn, F='(%"WARNING! Aborted extraction because WMAP is zero! You should investigate, and then remove %s so that the MERGE will not see it.")'
        continue
      endif
      
      ; Here we can directly integrate the emap, being careful to convert from square emap pixels to square sky pixels.  
      src_header = headfits(src_spectrum_fn, EXT=1)
      pixel_size = sxpar(emap_header, 'CDELT1P')
      src_exposurearea = (pixel_size^2) * integral_exposure

      ; Now, for diffuse sources the concept of "source extraction area" (src_area) is generally more poorly defined 
      ; than in the point source case because the diffuse region is likely to have very strong variations in the 
      ; emap, including portions that are zero due to point source masks and field edges.
      ; In order to get some kind of characteristic area for the region we're going to simply divide the emap integral
      ; by the median of the non-zero emap pixel values.
      ind  = where(emap GT 0, count)
  
      if (count GT 0) then begin
        src_area         = src_exposurearea / median(emap[ind])
        src_area_comment = 'approximate FOV (skypix^2), emap integral / median emap'
      endif else begin
        src_area         = 0
        src_area_comment = 'emap is zero in aperture'
      endelse
      
    endelse
    
    ; We start a dmhedit command file here, and finish & run it later.
    openw, unit, temp_text_fn, /GET_LUN
    comment = '(sec*cm^2*skypixel^2); EXPOSURE not used for bkg scaling'
    printf, unit, src_exposurearea, comment, F='(%"#add\nBACKSCAL = %f / %s")'


    ;; ------------------------------------------------------------------------
    ;; We must decide which CCDs may have observed this source so we can compute
    ;; an ARF for them.   CIAO should do this, but it does not!
    ;; We could just compute an ARF for every CCD, but as a compromise to save
    ;; computations we'll process whatever CCDs appear in the neighborhood.
    src_ccd_fraction          = histogram([      inband_events.ccd_id], MIN=0, MAX=9) / float(n_elements(inband_events)      )
    neighborhood_ccd_fraction = histogram([neighborhood_events.ccd_id], MIN=0, MAX=9) / float(n_elements(neighborhood_events))
    
    ccd_list                  = where((src_ccd_fraction GT 0) OR (neighborhood_ccd_fraction GT 0), ccd_count)
    
    
    ;; ------------------------------------------------------------------------
    ;; We must work around CIAO's sometimes inappropriate assignment of the EXPOSURE
    ;; keyword, so that xspec will derive accurate flux values.  The problem is 
    ;; explained at http://asc.harvard.edu/ciao/ahelp/times.html:
    ;;
    ;; "As explained above the ONTIME/LIVETIME/EXPOSURE keywords are
    ;; for the aim chip which, by CXC convention, corresponds to the first
    ;; GTI. Each chip has different values of each of these keywords. IF
    ;; someone is doing analysis on an off-aim-chip CCD, AND the values
    ;; are considerably different for each chip, then the analysis will be
    ;; impacted. For most observations the values for the different chips
    ;; are about the same but this can be problem for observations with a
    ;; lot of telemetry saturation, crowded fields, or period of extended
    ;; background flares."
    
    ;; Our solution is choose ONTIME/LIVETIME/EXPOSURE values from the source's primary CCD,
    ;; and to then weight each CCD's ARF (below) by the ratio of it's EXPOSURn to
    ;; EXPOSURE.
    primary_ccd_fraction_i    = max(src_ccd_fraction, primary_ccd)
    
    primary_ccd_name = string(primary_ccd,F='(I0)')
    src_ontime   = sxpar(src_header, 'ONTIME'  + primary_ccd_name)
    src_livetime = sxpar(src_header, 'LIVTIME' + primary_ccd_name)
    src_exposure = sxpar(src_header, 'EXPOSUR' + primary_ccd_name)

    if (src_exposure LE 0) then begin
      print, 'EXPOSUR' + primary_ccd_name, src_spectrum_fn, F='(%"ERROR: keyword %s in %s is not positive!")'
      GOTO, FAILURE
    endif

    comment = "primary CCD is " + primary_ccd_name
    print, comment
    printf, unit, src_ontime,   comment, $
                  src_livetime, comment, $
                  src_exposure, comment, $
                  F='(%"#add\nONTIME = %f / %s\nLIVETIME = %f / %s\nEXPOSURE = %f / %s")'

    free_lun, unit
    
    cmd = string(src_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd, /QUIET

   ;; ------------------------------------------------------------------------
    ;; Save spectral information for summary plots.
    fxaddpar, stats, 'CCD_CNT',  ccd_count,              'number of CCDs under source'
    fxaddpar, stats, 'CCD_PRIM', primary_ccd,            'primary CCD ID '
    fxaddpar, stats, 'CCD_FRAC', primary_ccd_fraction_i, 'primary CCD fraction'
    fxaddpar, stats, 'SRC_AREA',      src_area, src_area_comment
    fxaddpar, stats, 'SRC_RAD',  sqrt(src_area/!PI),           'sqrt(SRC_AREA/!PI) (skypix)'
    fxaddpar, stats, 'EXPOSURE', src_exposure,                 'EXPOSURE from src spectrum file'

    
    ;; ------------------------------------------------------------------------
    ;; ------------------------------------------------------------------------
    ;; BUILD ARF AND RMF FILES
    ;; There are six cases here because we have two ways to build an ARF:
    ;;   1. mkarf for point sources
    ;;   2. mkwarf for diffuse sources
    ;; and three ways to build an RMF:
    ;;   A. mkrmf
    ;;   B. mkacisrmf
    ;;   C. use RMF library for Townsley et al. CTI corrector
    ;;
    ;; On http://cxc.harvard.edu/ciao/bugs/mkacisrmf.html you'll find a warning that
    ;; XSPEC requires the energy grids in the ARF and RMF to match, and a warning that
    ;; mkacisrmf can modify the energy grid you request, and a recommendation that 
    ;; the mkacisrmf should be run first then mkarf/mkwarf told to match the binning.
    ;; 
    ;; When the Townsley et al. CTI corrector is used the same is true -- the 
    ;; RMF binning is predefined by the RMF library and one must construct the ARF
    ;; to match the binning.
    ;;
    ;; BUT, mkwarf uses WMAP, then passes a "weightfile" output to mkrmf.

    ;;POINT SOURCES
    ;; 1A: mkrmf     with explicit energy binning, then mkarf to match energy binning
    ;; 1B: mkacisrmf with explicit energy binning, then mkarf to match energy binning
    ;; 1C: weighted RMF from Townsley library,     then mkarf to match energy binning
    
    ;;DIFFUSE SOURCES
    ;; 2A: mkwarf    with WMAP & with explicit energy binning, 
    ;;                                         then mkrmf with energy binning passed through "weightfile" 
    ;; 2B: mkacisrmf with WMAP & with explicit energy binning, 
    ;;                                         then mkwarf with WMAP to match energy binning
    ;; 2C: weighted RMF from Townsley library, then mkwarf with WMAP to match energy binning

    ;;
    ;; Two large code blocks are shared among "orthogonal" subgroups of these six
    ;; cases so it is not convenient to block structure the code.
    ;; Also there are ordering requirements since some ARF constructors require the RMF
    ;; and some RMF constructors require the ARF.
    ;; Thus the code below has an odd structure (or lack of structure).
    ;; ------------------------------------------------------------------------
    ;; ------------------------------------------------------------------------
    file_delete, arf_fn, /ALLOW_NONEXISTENT
    
    ; We reuse the point-source RMF when possible since it can take a very long time to compute.
    ; For diffuse source we never reuse the RMF since the observer might change the region.
    reuse_rmf = file_test(rmf_fn) && ~is_diffuse
    
    ; If the observer is supplying an RMF via GENERIC_RMF_FN then set reuse_rmf so it will be used.
    if keyword_set(generic_rmf_fn) then begin
      rmf_fn    = generic_rmf_fn
      reuse_rmf = 1
    endif
    
    if reuse_rmf then print, 'Re-using existing RMF file.' $
                 else file_delete, rmf_fn, /ALLOW_NONEXISTENT

    ; NOTE that if you use an energy range too large then two things can happen:
    ;   (1) mkacisrmf may give a warning:
    ;       INFO: Effective user energy (keV) grids will be re-arranged in 0.25000 - 11.00000
    ;       See Bug Page for mkacisrmf (CIAO 4.1).
    ;   (2) The mkwarf may fail because the energy range is outside that covered in the FEF files:
    ;       ERROR: Min egridspec energy=0.25 below min FEF energy=0.277
    ;       ERROR: Max egridspec energy=11 above max FEF energy=9.886
    energy_grid_spec = '0.3:9.886:0.005'
    
    ; The addrmf tool has the unfortunate habit of re-ordering the SPECRESP and EBOUNDS
    ; HDU's.  Ideally we'd be able to deal with that by simply specifying the HDU name
    ; in subsequent commands (e.g. mkwarf) that need the RMF.  
    ; But, the CXC and the LKT CTI corrector use different legal names for the
    ; matrix (SPECRESP in older CIAO versions, MATRIX in CIAO 4.0, and SPECRESP MATRIX in Townsley et al.).  
    ; Also CIAO 4.0 seems to have a bug that prevents it from accepting the HDU name [SPECRESP MATRIX] in the mkarf call.  
    ; Re-ordering the HDU's after addrmf (using dmcopy & dmappend) results in a file that has
    ; FITS errors (according to fverify) and leads to errors in XSPEC.
    ; So ... we're going to have to keep track of which HDU _number_ in the RMF file
    ; contains the SPECRESP matrix.
    if reuse_rmf then begin
      fits_open, rmf_fn, fcb, /NO_ABORT, MESSAGE=error
      if keyword_set(error) then message, 'ERROR reading ' + rmf_fn
      fits_close, fcb
      
      specresp_hdu_number = 1 + (where(strmatch(fcb.EXTNAME, '*SPECRESP*') OR strmatch(fcb.EXTNAME, '*MATRIX*')))[0]
    endif else specresp_hdu_number = 2


    ;; ------------------------------------------------------------------------
    ;; RMF: 1A, 1B
    if (rmf_tool NE 'Townsley') AND (NOT is_diffuse) then begin
      ;; Use CIAO to build an RMF appropriate for the peak on the primary CCD.
      on_this_ccd = where(inband_events.ccd_id EQ primary_ccd)
    
      chipx  = mean( inband_events[on_this_ccd].chipx, /DOUBLE )
      chipy  = mean( inband_events[on_this_ccd].chipy, /DOUBLE )
      
      if (rmf_tool EQ 'mkrmf') then begin
        ;; RMF: 1A
        ;; Look up correct FEF file and call mkrmf.
        cmd = string(src_spectrum_fn, primary_ccd, round(chipx), round(chipy), F="(%'acis_fef_lookup infile=%s chipid=%d chipx=%d chipy=%d')")
  
        run_command, cmd
        
        cmd = string(rmf_fn, energy_grid_spec, DETCHANS, mkrmf_log_fn, $
                     F="(%'mkrmf infile="")acis_fef_lookup.outfile"" weights=none outfile=%s axis1=""energy=%s"" axis2=""pi=1:%d:1"" logfile=%s')")
        if ~reuse_rmf then run_command, cmd
      endif else begin
        ;; RMF: 1B
        ;; Use the more recent tool mkacisrmf.  The detector location is specified by (ccd_id,chipx,chipy).
        ;; We must specify a matched pair "infile" = phase2_response_file and "gain" = gainfile or use CALDB for both;
        ;; see http://asc.harvard.edu/ciao/threads/mkacisrmf/index.html#location
        cmd = string(phase2_response_file, rmf_fn, energy_grid_spec, DETCHANS, primary_ccd, round(chipx), round(chipy), $
                     gainfile, src_events_fn, mkrmf_log_fn,$
                     F="(%'mkacisrmf infile=%s outfile=%s wmap=none energy=""%s"" channel=1:%d:1 chantype=PI ccd_id=%d chipx=%d chipy=%d gain=%s obsfile=%s logfile=%s')")
        if ~reuse_rmf then run_command, cmd
      endelse
    endif ; (rmf_tool NE 'Townsley') AND (NOT is_diffuse)


    ;; ------------------------------------------------------------------------
    ;; RMF: 1C, 2C 
    if (rmf_tool EQ 'Townsley') then begin
      ;; Townsley et. al CTI corrector RMF library.
      ;; We'll weight the RMFs in the library by the number of counts in each RMF region.
      rmf_files  = 'junk'
      rmf_counts = 0L
      for jj = 0, ccd_count-1 do begin
        this_ccd = ccd_list[jj]
        ind = where(inband_events.ccd_id EQ this_ccd, count)
        if (count GT 0) then begin
          these_events = inband_events[ind]
          
          yrange_spec = ['_y1-128','_y129-256','_y257-384','_y385-512','_y513-640','_y641-768','_y769-896','_y897-1024']
          chipy_histogram = histogram([these_events.chipy], MIN=1, BINSIZE=128, NBINS=8)
          rmf_files  = [rmf_files, string(this_ccd,F='(%"ccd%d")') + yrange_spec + '.rmf']
          rmf_counts = [rmf_counts, chipy_histogram]
        endif ;(count GT 0)
      endfor
      
      ind = where(rmf_counts GT 0, num_rmf)
      rmf_files  = rmf_files [ind]
      rmf_counts = rmf_counts[ind]
      
;help, total(/INT, rmf_counts), n_elements(inband_events)

      ind = where(file_test(rmf_dir + rmf_files) EQ 0, num_not_found)
      if (num_not_found GT 0) then begin
        print, 'ERROR: cannot find RMF files: ', rmf_files[ind]
        print, 'WARNING: Skipping RMF & ARF generation; you must make your own or remove this source from the catalog before running /MERGE_OBSERVATIONS!'
        continue
      endif
      
      if (num_rmf EQ 1) then begin
        ; We use a symbolic link to avoid copying RMF's.
        if ~reuse_rmf then file_link, rmf_dir_from_source + rmf_files[0], rmf_fn, /VERBOSE
 
      endif else begin
        ; We run the addrmf command using an ASCII list file because the command line
        ; parameters seem to have limiting length restrictions
        rmf_weights = rmf_counts / total(rmf_counts)
        forprint, rmf_files, rmf_weights, F='(A0,1x,F5.3)', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
        cd, CURRENT=cwd  &  cwd=cwd+'/'
        cmd = string( temp_text_fn, cwd+rmf_fn, F="(%'addrmf ""@%s"" rmffile=%s')")

        if ~reuse_rmf then begin
          run_command, /HEASOFT, DIRECTORY=rmf_dir, cmd, STATUS=status 
        
          if NOT keyword_set(status) then begin
            specresp_hdu_number = 3
          endif else begin
            ; Sometimes the library RMFs don't have the same structure and addrmf fails.
            ; In this case we simply link to the library RMF with the largest weight.
            file_delete, rmf_fn, /ALLOW_NONEXISTENT
            dum = max(rmf_weights,imax)
            file_link, rmf_dir_from_source + rmf_files[imax], rmf_fn, /VERBOSE
          endelse
        endif
      endelse ;num_rmf GT 1
      
      if ~reuse_rmf then begin
        ; As of CIAO 3.2.2 the mkwarf program creates another problem for us.  
        ; Since it insists on looking up RMF information, if we use an energy range that's outside that 
        ; known by the CALDB then we will get this error message:
        ;         ERROR: Max egridspec energy=9.98 above max FEF energy=9.886
        ;       See Bug Page for mkacisrmf (CIAO 4.1).
        ; Thus we have to trim down the energy range used in the Townsley RMF.
        cmd = string(rmf_fn, specresp_hdu_number, temp_rmf_fn, $
                   F="(%'dmcopy ""%s[%d][ENERG_HI<9.88]"" %s option=all')")
        run_command, cmd
         
        file_delete, rmf_fn, /ALLOW_NONEXISTENT
        file_move, temp_rmf_fn, rmf_fn
      endif
    endif ;(rmf_tool EQ 'Townsley')


    ;; ------------------------------------------------------------------------
    ;; RMF: 2B
    if (rmf_tool EQ 'mkacisrmf') AND is_diffuse then begin
          ;; Use the more recent tool mkacisrmf.  It gets the weights directly from src_spectrum_fn[WMAP]
          ;; rather than from the "weightfile" output of mkwarf.
          ;; The ccd_id, chipx, chipy parameters are ignored when wmap supplied, but they must be present.
          ;;
          ;; The "infile" parameter must be CALDB so that mkacisrmf will use information in the header
          ;; of the WMAP file to select the appropriate "Phase 2 response calibration file".
          ;; The "gain" parameter must also be CALDB so that an appropraitely matched gain file is used.
          ;; Every Phase 2 response calibration file has a corresponding matched gain file in the CALDB.
          ;; See http://asc.harvard.edu/ciao/threads/mkacisrmf/index.html#extracted
          print
          print, 'WARNING: mkacisrmf can run for a very long time ...'
          cmd = string(rmf_fn, src_spectrum_fn, energy_grid_spec, DETCHANS, keyword_set(aspect_fn) ? aspect_fn : '', mkrmf_log_fn, $
                     F="(%'mkacisrmf infile=CALDB outfile=%s wmap=""%s[WMAP]"" energy=""%s"" channel=1:%d:1 chantype=PI ccd_id=3 chipx=0 chipy=0 gain=CALDB asolfile=%s logfile=%s')")

          if ~reuse_rmf then run_command, cmd
     endif ;(rmf_tool EQ 'mkacisrmf')


    ;; ------------------------------------------------------------------------
    ;; ARF: 1A, 1B, 1C
    if (NOT is_diffuse) then begin
      ; Construct a point source ARF, with consideration that a source can span multiple CCDs.
      ccd_arf_fn = obsdir      + string(ccd_list, F='(%"source%d.arf")')
      asphist_fn = asphist_dir + string(ccd_list, F='(%"/ccd%d.asphist")')
      ccd_name   = string(ccd_list, F='(I0)')
      detsubsys  = "ACIS-" + ccd_name
      
      primary_arf_fn = ccd_arf_fn[ where(ccd_list EQ primary_ccd) ]
      
      time_spent_on_each_ccd = fltarr(ccd_count)
      specresp = 0
      for jj=0,ccd_count-1 do begin
        ; Make an ARF for a single CCD at the catalog position.
        ; Since the extraction aperture has a finite size, over which the ARF could
        ; vary (due to exposure variations) we really ought to compute an ARF for the
        ; extraction region, somehow weighted by the PSF.  At present there's no
        ; easy way to do that.
        cmd = string(ccd_arf_fn[jj], asphist_fn[jj], rmf_fn, specresp_hdu_number, $
                     xpos_catalog, ypos_catalog, grating_spec, detsubsys[jj], pbkfile, mskfile, dafile, $
                     F="(%'mkarf outfile=%s asphistfile=""%s[ASPHIST]"" obsfile="")asphistfile"" engrid=""grid(%s[%d][cols ENERG_LO,ENERG_HI])""  sourcepixelx=%f sourcepixely=%f grating=%s detsubsys=%s pbkfile=%s maskfile=%s dafile=%s verbose=0')")
        run_command, cmd
        
        arf_table = mrdfits(ccd_arf_fn[jj], 1, arf_header, /SILENT, STATUS=status)
        if (status NE 0) then message, 'ERROR reading ' + ccd_arf_fn[jj]
        
        ; Weight the single-CCD ARF by the ratio of that CCD's EXPOSURn to the
        ; EXPOSURE keyword in the spectrum (!) file, not the ARF file!!
        ; See notes above about "reassinging ONTIME/LIVETIME/EXPOSURE values" in the spectrum file.
  
        this_ccd_exposure = sxpar(src_header, 'EXPOSUR' + ccd_name[jj])

        if (this_ccd_exposure LE 0) then begin
          print, 'EXPOSUR' + ccd_name[jj], src_spectrum_fn, F='(%"ERROR: keyword %s in %s is not positive!")'
          GOTO, FAILURE
        endif

        specresp = specresp + (this_ccd_exposure/float(src_exposure)) * arf_table.SPECRESP
        
        ; Figure out how much time was spent on this CCD.
        ; I have to assume that FRACEXPO is computed by mkarf with respect to "EXPOSURE" in ARF header.
        time_spent_on_each_ccd[jj] = sxpar(arf_header, 'EXPOSURE') * sxpar(arf_header, 'FRACEXPO')
      endfor ;jj
    
      ;; Let's use the primary CCD's FITS headers for the final ARF file, 
      ;; but set ONTIME, LIVTIME, & EXPOSURE to match files in spectrum, and
      ;; update FRACEXPO to be the fraction of time spent on _any_ CCD.
      pheader    = headfits(primary_arf_fn)
      arf_header = headfits(primary_arf_fn, EXT=1)

      fxaddpar, arf_header, 'ONTIME',   src_ontime
      fxaddpar, arf_header, 'LIVETIME', src_livetime
      fxaddpar, arf_header, 'EXPOSURE', src_exposure
      fxaddpar, arf_header, 'FRACEXPO', total(time_spent_on_each_ccd) / src_exposure, $
                strjoin(string(100*time_spent_on_each_ccd/src_exposure,F='(%"%d%%")'),' + ')
    endif ; point source

                
    ;; ------------------------------------------------------------------------
    ;; ARF: 2B, 2C
    if ((rmf_tool EQ 'mkacisrmf') OR (rmf_tool EQ 'Townsley')) AND is_diffuse then begin
        ; We've already built (above) a weighted RMF, either manually using 
        ; the Townsley RMF library or via mkacisrmf using CALDB.
        ; Now we want to build an ARF with the same energy binning that's
        ; weighted using the WMAP output of dmextract above.
        ;
        ; Even though we don't care about the "weightfile" output of mkwarf, we have   
        ; to supply a filename -- "none" is not accepted -- and the tool will still try
        ; to look up RMF information.  For certain early epochs of data CALDB won't be able
        ; to find CTI-corrected RMF information and will die.  As a workaround we supply
        ; an arbitrary but valid value for "feffile" to prevent CALDB from doing a search.

        dummy_feffile = (file_search( getenv('CALDB')+'/data/chandra/acis/fef_pha/*fef*', COUNT=fef_count ))[0]
        if (fef_count EQ 0) then message, 'ERROR: could not find a dummy feffile to pass to mkwarf'

        cmd = string(src_spectrum_fn, arf_fn, temp_wgt_fn, rmf_fn, specresp_hdu_number, dummy_feffile, pbkfile, mskfile, dafile, keyword_set(aspect_fn) ? aspect_fn : '', $
                     F="(%'mkwarf infile=""%s[WMAP]"" outfile=%s weightfile=%s spectrumfile=NONE egrid=""grid(%s[%d][cols ENERG_LO,ENERG_HI])"" feffile=%s pbkfile=%s mskfile=%s dafile=%s asolfile=%s')")
      
        run_command, cmd
        
        if ~file_test(arf_fn) then begin
          print, stats_fn, F='(%"WARNING! Aborted extraction because mkwarf failed! You should investigate, and then remove %s so that the MERGE will not see it.")'
          continue
        endif

        ; Need to read ARF to populate arf_table, specresp, & pheader in prep for later section 
        ; where PSF Fraction=1 should be applied.
        arf_table = mrdfits(arf_fn, 1, arf_header, /SILENT, STATUS=status)
        if (status NE 0) then message, 'ERROR reading ' + arf_fn
  
        specresp = arf_table.SPECRESP
        pheader = headfits(arf_fn)    
     endif ;(rmf_tool NE 'Townsley') AND is_diffuse



    ;; ------------------------------------------------------------------------
    ;; ARF, RMF: 2A
    if (rmf_tool EQ 'mkrmf') AND is_diffuse then begin
      ;; Build a weighted ARF using the WMAP output of dmextract above.
      ;; We specify the energy binning directly and it is then passed to mkrmf
      ;; through the "weightfile" output of mkwarf.
        cmd = string(src_spectrum_fn, arf_fn, mkrmf_wgt_fn, energy_grid_spec, pbkfile, mskfile, dafile, keyword_set(aspect_fn) ? aspect_fn : '', $
                     F="(%'mkwarf infile=""%s[WMAP]"" outfile=%s weightfile=%s spectrumfile=NONE egrid=""%s"" feffile=CALDB pbkfile=%s mskfile=%s dafile=%s asolfile=%s')")
      
        run_command, cmd

        if ~file_test(arf_fn) then begin
          print, stats_fn, F='(%"WARNING! Aborted extraction because mkwarf failed! You should investigate, and then remove %s so that the MERGE will not see it.")'
          continue
        endif

        ; Need to read ARF to populate arf_table, specresp, & pheader in prep for later section 
        ; where PSF Fraction=1 should be applied.
        arf_table = mrdfits(arf_fn, 1, arf_header, /SILENT, STATUS=status)
        if (status NE 0) then message, 'ERROR reading ' + arf_fn
  
        specresp = arf_table.SPECRESP
        pheader = headfits(arf_fn)    

        ;; The mkrmf tool wants the "weightfile" produced by mkwarf above.
        ;; The "energy=0:1" param to mkrmf is a dummy to keep the parser happy -- the energy grid comes
        ;; from the weightfile.
        cmd = string(mkrmf_wgt_fn, rmf_fn, DETCHANS, mkrmf_log_fn, $
                     F="(%'mkrmf infile=CALDB weights=%s outfile=%s axis1=""energy=0:1"" axis2=""pi=1:%d:1"" logfile=%s')")
        if ~reuse_rmf then run_command, cmd

    endif ; (rmf_tool EQ 'mkrmf') AND is_diffuse
    

    
    
    ;; ------------------------------------------------------------------------
    ;; Scale the ARF.
    ENERG_LO = arf_table.ENERG_LO
    ENERG_HI = arf_table.ENERG_HI
    channel_midenergy = 0.5 * (ENERG_LO + ENERG_HI)

    if (NOT is_diffuse) then begin
      ;; For point sources we are scaling down the ARF by the PSF fraction, so that the calibration 
      ;; is corrected for the light we discarded.
  
      ; We use linterp instead of interpol() to avoid wild extrapolation if the PSF samples
      ; we have don't cover the energy range of the ARF very well.
      if (n_elements(psf_fraction) GT 1) then begin
        linterp, psf_fraction.energy, psf_fraction.fraction, channel_midenergy, psf_frac_column
      endif else begin
        psf_frac_column = replicate(psf_fraction.fraction, n_elements(ENERG_LO))
      endelse
      
      if (max(psf_frac_column) GT 1) then begin
        print, 'ERROR: PSF fraction larger than 1 detected'
        GOTO, FAILURE 
      endif

      effective_fov = 1
      specresp_unit = 'cm^2'
    endif else begin
      ;; For diffuse sources we choose to scale up the ARF (and change its units) so as to put 
      ;; the calibration on a "per arcsec^2" basis.
      ;; See the "Diffuse Sources" section of the AE manual for an explanation of this idea; 
      ;; the comment block below is simply a different wording of the same explanation.
            
      ; The observer will at some point in the reduction want to normalize fluxes and luminosities by
      ; a "field of view on the sky" in order to get a "surface brightness". I think that diffuse
      ; extractions from multiple obsids can be most clearly combined in the MERGE stage if they are
      ; FIRST expressed in terms of the physical quantity of surface brightness. The fundamental
      ; reason for this is that the concept of "field of view on the sky" is most clearly defined for
      ; a single obsid. An extraction region fixed on the sky may cover very differently sized
      ; portions of the detector in each obsid, due to field edges and point source masking.
      ; 
      ; Thus, I think that at this point we should express the physical calibration of this
      ; single-obsid diffuse extraction in terms of counts/s/cm^2/skypix^2 (rather than the usual
      ; counts/s/cm^2). It is at first tempting to use the OGIP keyword AREASCAL to store the skypix^2
      ; quantity, since XSPEC will normalize the spectrum by AREASCAL when it is read in. However, to
      ; support the cplinear background model AE uses AREASCAL for the purpose of background spectrum
      ; scaling; thus it would be very confusing to use AREASCAL here to represent field of view on
      ; the sky. The other places we could effect a field of view normaliation would seem to be in the
      ; EXPOSURE keyword or in the ARF; I choose the latter. Thus, we will compute a field of view
      ; (skypix^2) quantity below, and will scale up the ARF by that quantity; the ARF will then be in
      ; units of cm^2*skypix^2. XSPEC will then be modeling surface brightness (not flux) in units of
      ; counts/s/cm^2/skypix^2.
      ; 
      ; What "field of view" quantity is appropriate? IF the response of the instrument was constant
      ; over the extraction region, then the geometric size of the region would obviously be the
      ; appropriate quantity. However, the response of the detector is clearly not constant over the
      ; extraction region, due to field edges, source masking, multiple CCDs in the region,
      ; vignetting, etc. Our goal should be understood to be to find a field of view size that, when
      ; combined with the ARF given to XSPEC, will correctly calibrate the surface brightness of the
      ; extracted data. Now, IF we had an ARF which accounted for the exposure variation across the
      ; extraction region (including areas of zero response), then an appropriate field of view would
      ; seem to be the simple geometric area of the region. However, the ARF we get from mkwarf is
      ; very definately not such an ARF. Instead, mkwarf forms a weighted average of ARFs computed
      ; over the CCD tiles which were involved in the observation. All of these ARFs are "full
      ; strength", i.e. none are reduced to account for lower-than-normal exposure anywhere (e.g. due
      ; to chip gaps, source masking, or field edges); see Helpdesk Ticket #8154. In short, mkwarf
      ; (unlike mkarf) knows nothing about the exposure map.
      ; 
      ; Thus, we can think of the field of view quantity that we seek as the physical size of a
      ; virtual detector whose response is described by the ARF in hand (from mkwarf) that would
      ; produce the observed data in the given EXPOSURE time. This is analogous to the "effective
      ; exposure time" that we compute for point sources, which is the exposure time required to
      ; produce the observed data if the source was located on a section of the detector with nominal
      ; response (e.g. on-axis).
      ; 
      ; Now, we have on hand an exposure map within the extraction region, computed for some
      ; mono-energy E0. Note that the exposure map at any point (x,y) is supposed to be precisely
      ; ARF(x,y,E0) * EXPOSURE. The integral of this exposure map (cm^2*s*skypix^2) can be thought of
      ; as a complete "measure" of the depth of the observation (at energy E0) in which effective area
      ; (cm^2), integration time (s), and extraction region size (skypix^2) are interchangeable. IF
      ; the exposure map is correctly calibrated, then our goal would seem to be to give XSPEC an
      ; EXPOSURE value and ARF such that the observed spectrum at energy E0 is normalized by the
      ; integral of the exposure map (cm^2*s*skypix^2). Thus we should simply scale the ARF in hand so
      ; that ARF(E0) = <integral of emap> / EXPOSURE.

      if NOT keyword_set(emap_energy) then begin
        print, F='(%"ERROR: for diffuse extraction you must supply an EMAP_ENERGY value (keV).")'
        GOTO, FAILURE
      endif
      
      ; The units of effective_fov are (arcsec/skypixel)^2 * (sec*cm^2*skypixel^2) / (sec*cm^2) = arcsec^2
      effective_fov   = (arcsec_per_skypixel^2) * src_exposurearea / (src_exposure * interpol(specresp, channel_midenergy, emap_energy))
      psf_frac_column = 1
      specresp_unit   = 'cm^2*arcsec^2'
    
      ; However, in practice we often find that emap(x,y,E0) != ARF(x,y,E0) * EXPOSURE. It is not
      ; clear why this occurs. Certainly there is a danger that the observer may compute the exposure
      ; map months or years prior to the ARF; if the mission calibration is revised during that period
      ; then the two will be inconsistent. There may be other software-related reasons why the two do
      ; not agree perfectly. 
      ; Thus, to give the observer some insight into the consistency between the emap and ARF we will
      ; compare the SRC_AREA and effective_fov quantities.
      
      print, emap_energy, (arcsec_per_skypixel^2) * src_area, effective_fov, F='(%"\nWARNING! The exposure map built for energy E0=%0.1f and the observatory ARF are supposed to be related by emap(x,y) = ARF(E0,x,y) * EXPOSURE.\nIf the ''source area'' and ''effective field of view'' estimates below are significantly different, then you should suspect that your emap and ARF are inconsistent, perhaps because they were made with different releases of CALDB.\n  SRC_AREA      = %d (arcsec^2)\n  EFFECTIVE_FOV = %d (arcsec^2)")'
    endelse ; diffuse source    
    
    ;; ------------------------------------------------------------------------
    ;; Write the ARF file.
    ; We want to write a binary table which has ENERG_LO, ENERG_HI, & SPECRESP columns that
    ; carry the TTYPE, TFORM, TUNIT, TLMIN, TLMAX, etc. information from arf_header, and
    ; which has the new columns BASE & PSF_FRAC.
    ; There are no convenient tools in AstroLib to read all the keywords asssociated with
    ; a column and write them back out (possibly in a different column number).
    ; Thus I will be lazy and assume the CIAO ARF file has ENERG_LO, ENERG_HI, & SPECRESP 
    ; in columns 1, 2, & 3 with no other columns.  We'll use the CIAO header and the
    ; various column-specific keywords for columns 1-3, adding BASE & PSF_FRAC as columns
    ; 4 & 5.
    arf_row = {ENERG_LO:0.0, ENERG_HI:0.0, SPECRESP:0.0, BASE:0.0, PSF_FRAC:0.0, EFFECTIVE_FOV:0.0}
    arf_table = replicate( arf_row, n_elements(ENERG_LO) )
    
    arf_table.ENERG_LO      = ENERG_LO
    arf_table.ENERG_HI      = ENERG_HI
    arf_table.BASE          = specresp
    arf_table.PSF_FRAC      = psf_frac_column
    arf_table.EFFECTIVE_FOV = effective_fov
    arf_table.SPECRESP      = specresp * psf_frac_column * effective_fov

    fxaddpar, pheader, 'CREATOR', creator_string
    writefits, arf_fn, 0, pheader
    
    fxaddpar, arf_header, 'CREATOR', creator_string
    fxaddpar, arf_header, 'TUNIT3' , specresp_unit
    fxaddpar, arf_header, 'TUNIT4' , 'cm^2'    , 'from mkarf'
    fxaddpar, arf_header, 'TUNIT5' , ''        , 'PSF fraction'
    fxaddpar, arf_header, 'TUNIT6' , 'arcsec^2', 'effective FOV'
    
    if (ccd_count GT 1) then $
      fxaddpar, arf_header, 'DETNAM', 'ACIS-' + strjoin(string(ccd_list,F='(I0)')), 'source spans multiple CCDs'
      
    mwrfits, arf_table, arf_fn, arf_header

    
    ;; Record the mean ARF value over the set of ARF bins whose mid-energy falls in the range specified by user.
    ;; (Thus a bin is included only if >=50% of it is in the energy range.) 
    ;; Since the ARF is not evenly sampled we must integrate the ARF & divide by Erange.
    arf_energy = 0.5 * (arf_table.ENERG_LO + arf_table.ENERG_HI)
    ind = where((arf_energy GE energy_range[0]) AND (arf_energy LE energy_range[1]))
    band_arf_table = arf_table[ind]
    arf_mean_i = total(band_arf_table.SPECRESP * (band_arf_table.ENERG_HI - band_arf_table.ENERG_LO), /D)$
                                         / total((band_arf_table.ENERG_HI - band_arf_table.ENERG_LO), /D)

    ;; Save calibration information for summary plots.
    fxaddpar, stats, 'FRACEXPO', sxpar(arf_header,'FRACEXPO'), 'FRACEXPO from src ARF file'
    fxaddpar, stats, 'MEAN_ARF', arf_mean_i, 'mean ARF value ('+specresp_unit+' counts/photon)'
    
    fxaddpar, stats, 'CREATOR', creator_string
    get_date, date_today, /TIMETAG
    fxaddpar, stats, 'EXTRDATE', date_today, 'date extracted'
    writefits, stats_fn, 0, stats
  endfor ; ii

  count = total(source_not_observed, /INT)
  if (count GT 0) then print, count, F="(%'\nWARNING!  %d sources were not observed.')"

  save, sourcename, energy0, energy1, FILE='lowest_energies.sav'
;  print, '============================================================================='
;  print, 'Lowest two in-band energies (useful for NH estimation) saved in lowest_energies.sav'
;  print, '============================================================================='
  
endif ;keyword_set(extract_events) OR keyword_set(extract_spectra)



;; =============================================================================
if keyword_set(timing) then begin
;; =============================================================================
    color_manager, /PS_PSEUDO, RED=red, GREEN=green

    ; SNR_RANGE[1] is the user's goal for defining groups; SNR_RANGE[0] is the lower limit allowed before we abort the grouping attempt
    if (n_elements(snr_range) EQ 0) then $
      snr_range = [2,20]
    if (n_elements(snr_range) NE 2) then begin
      print, 'ERROR: keyword SNR_RANGE should be a 2-element vector giving the range of SNR allowed for each time bin, e.g. [2.5,5].'
      GOTO, FAILURE      
    endif
    
    if (snr_range[1] LT 0) then begin
      print, 'ERROR: minimum SNR value (SNR_RANGE[1]) must be positive'
      GOTO, FAILURE
    endif
    
    if (n_elements(num_groups_range) EQ 0) then $
      num_groups_range = [2,50]
    if (n_elements(num_groups_range) NE 2) then begin
      print, 'ERROR: keyword NUM_GROUPS_RANGE should be a 2-element vector specifying how many time bins are desired, e.g. [10,250].'
      GOTO, FAILURE      
    endif

  
  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be off-field.
    if source_not_observed[ii] then continue

    ;; Construct filenames.
    obsdir      = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]

    stats_fn        = obsdir + obs_stats_basename
    src_events_fn   = obsdir + src_events_basename
    lc_smooth_fn    = obsdir + lc_smooth_basename
    event_plot_fn   = obsdir + event_plot_basename
    
    if (NOT file_test(stats_fn)) then begin
;     print, F='(%"\n===================================================================")'
;     print, 'Source: ', sourcename[ii]
;     print, 'EXTRACTION SKIPPED: source not observed.'
      source_not_observed[ii] = 1
      continue
    endif

    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, ['pset dmcopy clobber=yes', 'pset dmextract clobber=yes']
  
    print, F='(%"\n===================================================================")'
    print, 'Source: ', sourcename[ii]
    stats = headfits(stats_fn)
    arf_mean_i = sxpar(stats,'MEAN_ARF')
    
    if (arf_mean_i EQ 0) then begin
      print, 'WARNING: the MEAN_ARF value in obs.stats is either missing or zero; the light curve normalization will be incorrect.'
      arf_mean_i = 1.0
    endif

    ;; Extract in-band source event list to compute statistics.
    ;; We do NOT go ahead and sort the events by time with dmsort (in preparation for the Kolmogorov-Smirnov statistic computation below) because as of Oct 2005 dmsort has bugs which sometimes trash GTI tables!
    cmd = string(src_events_fn, 1000*energy_range, inband_events_fn, $
                 F="(%'dmcopy ""%s[energy=%6.1f:%7.1f]"" %s')")
    run_command, cmd

    inband_events = mrdfits(inband_events_fn, 1, src_header, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + inband_events_fn
    
    
    ;; The code below is complex because we must deal with the case of zero inband events.
    ;; In such a case we cannot simply skip our timing analysis because our later multi-obsid 
    ;; variability analysis needs to know about the exposure found in this obsid, even if no
    ;; counts were observed.

    ; If the FITS table is empty, mrdfits will return a scalar zero.
    if NOT keyword_set(inband_events) then begin
      ;; There are no in-band data so we skip various statistics.
      print, 'WARNING: no in-band data found in source region.'
      inband_src_counts      = 0
      primary_ccd_fraction_i = 0
      primary_ccd            = 3
    endif else begin
      inband_src_counts = n_elements(inband_events)
    
      ; Handle sources spanning CCDs and dim sources.
      ccd_histogram = histogram([inband_events.ccd_id], MIN=0, MAX=9)
      
      primary_ccd_fraction_i = max(ccd_histogram, primary_ccd) / total(ccd_histogram)
    endelse
        
    ;; ------------------------------------------------------------------------
    ;; Find the GTI table corresponding to the primary CCD.
    extno   = 0
    success = 0
    repeat begin
      extno = extno + 1
      gti_header = headfits(inband_events_fn, EXTEN=extno, ERRMSG=error)
      if keyword_set(error) then break

      hduname = strtrim(sxpar(gti_header, 'HDUNAME'),2)
      if (hduname EQ string(primary_ccd,F='(%"GTI%d")')) then success = 1
    endrep until success

    if (success EQ 0) then begin
      print, primary_ccd, F='(%"\nWARNING! Could not find GTI table for CCD%d; assuming no bad intervals.")'
    endif else begin
      gti_table = mrdfits(src_events_fn, extno, gti_header, /SILENT)
      if ((gti_table.start)[0] EQ (gti_table.stop)[0]) then begin
        print, inband_events_fn, F='(%"\nWARNING! empty GTI table found in %s; assuming no bad intervals.")'
        success = 0
      endif
    endelse
  
    if (success EQ 0) then begin
      gti_header = src_header
      gti_table = {start:sxpar(gti_header,'TSTART') , stop:sxpar(gti_header,'TSTOP') }
    endif
    
    num_gti = n_elements(gti_table)

    total_exposure    = total(gti_table.stop - gti_table.start, /DOUBLE)
      
      
    
    if (inband_src_counts EQ 0) then begin
      probks = !VALUES.F_NAN
      print, 'Analysis skipped -- no in-band counts.'
    endif else begin
      ;; ------------------------------------------------------------------------
      ;; Sort the event times to support two compuations later which require sorting:
      ;; 1. Cumulative distribution of observed events.
      ;; 2. Call to function uniq() in the light curve table code.
      event_time = (inband_events.time)[sort(inband_events.time)]
      
      event_time_bin_index = lonarr(inband_src_counts)
      
      ;; Compute the Kolmogorov-Smirnov statistic for a uniform model light curve.
      ;; The code below essentially concatenates all the good time intervals together
      ;; and computes the cumulative exposure time at each of the observed events.
      ;; BACKGROUND is NOT subtracted, and variation in the background is not accounted for!
      prior_exposure = 0
      for jj = 0, num_gti-1 do begin
        prior_exposure = prior_exposure + ((event_time - gti_table[jj].start) > 0)  $
                                        - ((event_time - gti_table[jj].stop ) > 0)
      endfor
      
      uniform_cum_distn = prior_exposure / total_exposure
      
      ; These lines derived from ksone.pro in AstroLib.
      cum_distn_before_step = (  findgen(inband_src_counts)) / inband_src_counts
      cum_distn_after_step  = (1+findgen(inband_src_counts)) / inband_src_counts

      ks_distance = max( abs( uniform_cum_distn - cum_distn_before_step ) ) > $
                    max( abs( uniform_cum_distn - cum_distn_after_step  ) )
      
      if (primary_ccd_fraction_i LT 1) then begin
        print, 'WARNING!  The source spans multiple CCDs; variability may be overestimated.'
      endif  
      
      if (inband_src_counts LT 4) then begin
        ; We need at least 4 counts for KS probability to be meaningful.
        probks = !VALUES.F_NAN
        print, 'Variability analysis skipped -- too few in-band counts.'
      endif else prob_ks, ks_distance, inband_src_counts, probks


      ;; ------------------------------------------------------------------------
      ;; Plot photon arrival times vs energy, and overplot the cumulative distributions
      ;; used in KS above.
      device, FILENAME=event_plot_fn
      
      tstart = sxpar(gti_header, 'TSTART')
      tstop  = sxpar(gti_header, 'TSTOP')
      
      tit = string(sourcename[ii],probks, F='(%"%s, P!DKS!N=%0.2g")')
            
      plot, (event_time-tstart)/3600., [inband_events.energy/1000.], XRANGE=[0,(tstop-tstart)/3600.], YRANGE=energy_range, XSTYLE=1+2, XMARGIN=[8,6], YSTYLE=1+2+8, THICK=3, PSYM=(inband_src_counts LT 200) ? 1 : 3, TITLE=tit, XTIT='Time (hrs)', YTIT='Energy (keV)'
      
      axis, YAXIS=1, /SAVE, YRANGE=[0,1], YSTYLE=1+2, YTICKS=2, YTIT='Cumulative Distribution, data & uniform model', COLOR=red
      
      xx = [tstart, rebin(event_time,2*inband_src_counts,/SAMPLE), tstop]
      yy = [rebin(cum_distn_before_step,2*inband_src_counts,/SAMPLE), 1, 1]

      oplot, (xx-tstart)/3600., yy, COLOR=red
      
      xx = [tstart,event_time,tstop]
      yy = [0,uniform_cum_distn,1]
      
      gti_fractions = (gti_table.stop - gti_table.start)/total_exposure
      for jj = 0, num_gti-2 do begin
        xx = [xx, gti_table[jj].stop, gti_table[jj+1].start]
        cum_fraction = total(gti_fractions[0:jj], /DOUBLE)
        yy = [yy, cum_fraction, cum_fraction]
      endfor
      
      sind = sort(xx)
      
      oplot, (xx[sind]-tstart)/3600., yy[sind], COLOR=green

      device, /close
    endelse  ; (inband_src_counts GT 0) 

    
      
    ;; ------------------------------------------------------------------------
    ;; Compute a finely, unequally, binned light curve which will then be grouped.
    ;; Compute smoothed light curve, and median energy curve, using adaptive kernel smoothing.
    
    ;; We're going to place time bin boundaries at the times that events occurred so we don't 
    ;; lose any timing resolution.  We'll define other bin boundaries as necessary to get a 
    ;; reasonable sampling of the time interval.
    ;; We rely below on the fact that event_time was sorted earlier.
    ;;
    ;; The convention that the /MERGE stage expects is that an event is counted (COUNTS column) 
    ;; in the bin to the LEFT of the boundary.  
    t_min   = gti_table[0].start
    t_max   = gti_table[num_gti-1].stop
    
    if (t_max LE t_min) then begin
      print, 'ERROR: time span covered by GTI table is zero!'  
      continue
    endif

    min_num_bins = 400
    max_binsize  = (t_max - t_min) / min_num_bins
    
    dim = min_num_bins + 10 + inband_src_counts
    bin_edges = dblarr(dim)
 
    bin_edges[0] = t_min
    event_ind    = 0L
    for jj=1L,dim-1 do begin
      ; Start by assuming that we'll define an empty bin with the nominal width.
      nominal_bin_stop = bin_edges[jj-1] + max_binsize

      ; If there is an event remaining, and its timestamp is close enough to define a bin then do it.
      if (event_ind LT inband_src_counts) then begin
        event_bin_stop = event_time[event_ind]
        if (event_bin_stop LE nominal_bin_stop) then begin
          ; Place a bin edge at this event timestamp.
          bin_edges[jj] = event_bin_stop

          ; Assign this time bin to every event in this time bin..
          ; Note there can be multiple events with the same timestamp.
          ; Recall that we've sorted "event_time" previously.
          repeat begin
            event_time_bin_index[event_ind] = jj-1
            event_ind = event_ind + 1
            if (event_ind GE inband_src_counts) then break ; repeat loop
          endrep until (event_time[event_ind] GT event_bin_stop)
          
          continue ; jj loop
        endif ; Defining a time bin.
      endif ; Unprocessed events remaining.
      
      ; Otherwise try to construct an empty bin of nominal width.
      if (nominal_bin_stop GE t_max) then break
      
      bin_edges[jj] = nominal_bin_stop
    endfor ;jj
    if (jj GE (dim-1)) then message, 'ERROR: loop failed to terminate.'
    
    ; The final bin edge is at TSTOP.
    bin_edges[jj] = t_max
    
    bin_edges = bin_edges[0:jj]
    
   
    ; Convert a list of bin edges to a list (one shorter) of min, center, and max values.
    nbins      = n_elements(bin_edges) - 1
    bin_min    = bin_edges[0:nbins-1]
    bin_max    = bin_edges[1:nbins]
    bin_center = 0.5*(bin_min + bin_max)
    
     
    ;; Compute the exposure*EA in each bin by integrating the GTIs over the bins.
    exposure = dblarr(nbins)
    for jj = 0, num_gti-1 do begin
      this_gti_integral = ((gti_table[jj].stop < bin_max) - (gti_table[jj].start > bin_min)) > 0

      exposure = exposure + (this_gti_integral * arf_mean_i)
    endfor
;   print, 'total exposure = ', total(exposure)
      
    nan_mask  = replicate(!VALUES.F_NAN,nbins)

    if (inband_src_counts EQ 0) then begin
      ;; Since there are no events, make various null columns for the light curve file.
      flux                = nan_mask
      flux_error          = nan_mask
      error               = nan_mask
      median_energy       = nan_mask   
      median_energy_error = nan_mask
      radius              = nan_mask
      hist                = replicate(0, nbins)
      group_codes         = replicate(-1,nbins)
      group_codes[0]      = 1
      this_snr_goal       = !VALUES.F_NAN
    endif else begin
      ;; Do some sanity checking.
      if (max(event_time_bin_index) GT (nbins-1)) then message, 'ERROR: bug in code.'
      
      ;; Make a high resolution histogram of the event times & save reverse indexes.
      ;; We use NBINS keyword to ensure that we know the number of elements in "hist" so that we can
      ;; properly decode the rindex array returned.
      hist = histogram( event_time_bin_index, MIN=0L, BINSIZE=1, NBINS=nbins, REVERSE_INDICES=rindex )
      
      ; Re-order the data vector so that data belonging to each bin are grouped
      ; together, and those groups appear in the order of the bins.
      ; Sorting the data up front will avoid applying REVERSE_INDEXES to the data
      ; vector multiple times later.
      ; Expand the first segment of REVERSE_INDICES into vectors member_index_start and member_index_stop
      ; that define segments of the ordered data vector (sorted_energy_data) which belong to each time bin.
      member_index_start    =rindex[0:nbins-1] - (nbins + 1)
      member_index_stop     =rindex[1:nbins]   - (nbins + 1) - 1
      sorting_indexes= rindex[nbins+1:*]
      sorted_energy_data    = (inband_events.energy)[sorting_indexes]

      ;; Group the high resolution, low significance histogram.  We don't have a background available here.
      group_bins_to_snr, hist, 0, 0, /GROUP_WITHOUT_BACKGROUND, $
                         SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                         this_snr_goal, group_codes

      ; Compute median energy over each group.
      ind = [where(group_codes EQ 1, num_groups), nbins]

      median_energy       = replicate(!VALUES.F_NAN,nbins)
      median_energy_error = replicate(!VALUES.F_NAN,nbins)
      
      defensive_count = 0L
      defensive_data  = sorted_energy_data[0]
      for kk=0,num_groups-1 do begin
        ind_left  = ind[kk]
        ind_right = ind[kk+1] - 1
        
        ; Locate the data associated with each group.
        concat_array_segments, sorted_energy_data, member_index_start[ind_left : ind_right], $
                                                   member_index_stop [ind_left : ind_right], data, num_data

        defensive_count=defensive_count+n_elements(data)
        ; Compute the median energy statistic and estimate an error using the 
        if (num_data GT 0) then begin
          defensive_data =[defensive_data, data]

          ; Compute the median energy and 1-sigma (68%) confidence interval.
          median_with_ci, data, CONFIDENCE_LEVEL=0.6827, median_value, limit_lower, limit_upper

          median_energy      [ind_left : ind_right] = median_value
          
          median_energy_error[ind_left : ind_right] = (limit_upper - limit_lower) / 2.0
        endif
      endfor ;kk

      if (inband_src_counts NE defensive_count) then message, 'BUG in grouping code detected!'
      if (total(defensive_data[1:*] NE sorted_energy_data) GT 0) then message, 'BUG in grouping code detected!'
      

      ;; Adaptively smooth the light curve.
      ;; With a normal histogram, the first and last time values at which an 
      ;; estimate of the distribution are available (i.e. the centers of the 
      ;; first & last bins) are inset half a bin from the range of the data ([t_min, t_max]).  
      ;; Similarly, with the variable bin size used here, distribution estimates 
      ;; whose kernels fall outside the range of the data are not meaningful.   
      ;; (For a flat kernel there are groups of identical smoothed values at the 
      ;; beginning and end of the vector.)  
      ;; * Thus, the largest useful kernel would be positioned at the center of 
      ;; the time range and would have a radius of nbins/2 (= MAX_RADIUS).
      ;; * Later we will discard smoothed values whose kernel falls outside the range of the data.
      
      ;; For bright sources there are a large number of time bins; if we let adaptive_density_2d try every possible
      ;; set of radii, then it will be very slow and consume a huge amount of memory.
      ;; Thus, we supply a MAX_RADIUS parameter which anticipates the worst-case situation where we need 
      ;; our kernel to encompass this_snr_goal^2 bins that each have 1 count, plus up to min_num_bins that are empty.
      max_radius = ceil((min_num_bins + this_snr_goal^2)/2) 
      
      ;; We use the FIELD_MASK input to leave gaps in light curve corresponding to bad time intervals.
      
      ;; Because we've decided to construct light curves even when the source spans multiple CCDs, we
      ;; can sometimes end up with the entries where HIST GT 1 and EXPOSURE EQ 0, which adaptive_density_2d
      ;; would choke on.  We simply discard those events.
      if (primary_ccd_fraction_i LT 1) then hist = hist * (exposure GT 0)

      adaptive_density_2d, hist, this_snr_goal, EMAP=exposure, $
                           FIELD_MASK=(exposure GT 0), MAX_RADIUS=max_radius, $
                           flux, flux_error, radius, SILENT=1

      ;; Set smoothed quantities to NaN at the ends of the time series where the kernel falls outside the time range of the data.
      bin_index = lindgen(nbins)
      first_index = (        where((radius LE (bin_index-0))       AND finite(flux)) )[0]
      last_index  = (reverse(where((radius LE (nbins-1-bin_index)) AND finite(flux))))[0]
      
      if (((last_index-first_index) LT 1) OR (first_index EQ -1) OR (last_index EQ -1)) then begin
        ; All of the bins used the full dataset, i.e. the smooth curves are flat.
        ; Retain just two non-masked bins in the middle.
        ind   = where(finite(flux), num_finite)
        mid   = (num_finite/2) > 1
        ind   = ind[[mid-1,mid]]
      endif else begin
        ; Trim down the time series.
        ind = bin_index[first_index:last_index]
      endelse
       
      nan_mask[ind] = 1.0
    endelse ; (inband_src_counts GT 0)
    
    
    ;; Write smoothed lightcurve and median energies to a FITS table.
    pheader = headfits(inband_events_fn)
    fxaddpar, pheader, 'CREATOR', creator_string

    fxbhmake, theader, nbins, 'LIGHTCURVE', 'nonstandard format', /DATE, /INIT
    fxaddpar, theader, 'CREATOR', creator_string
    fxaddpar, theader, 'MEAN_ARF', arf_mean_i, 'mean ARF value (cm^2 counts/photon)'
    fxaddpar, theader, 'PROB_KS',  probks,  'KS variability significance level'
    fxaddpar, theader, 'TSTART', sxpar(pheader, 'TSTART')
    fxaddpar, theader, 'TSTOP',  sxpar(pheader, 'TSTOP')
    fxaddpar, theader, 'SNR_GOAL', this_snr_goal
    fxaddpar, theader, 'NUMGRPS', fix(total(group_codes>0))
    fxaddpar, theader, 'TUNIT1', "s",       'left bin edge'
    fxaddpar, theader, 'TUNIT2', "s",       'bin center'
    fxaddpar, theader, 'TUNIT3', "s",       'right bin edge'
    fxaddpar, theader, 'TUNIT4', "counts",  '# events in bin'
    fxaddpar, theader, 'TUNIT5', "sec*cm^2",'exposure*EA in bin'
    fxaddpar, theader, 'TUNIT6', "bins",    'kernel radius'
    fxaddpar, theader, 'TUNIT7', "count/s/cm^2", 'via adaptive kernel smoothing'
    fxaddpar, theader, 'TUNIT8', "count/s/cm^2", 'error on COUNT_RATE'
    fxaddpar, theader, 'TUNIT9', "flag",    'grouping flags'
    fxaddpar, theader, 'TUNIT10', "eV",      'median event energy in group'
    fxaddpar, theader, 'TUNIT11', "eV",      'error on GRP_MEDIAN_ENERGY'

    ; We could compute exposure under each window using radius values -- would have to do that BEFORE trimming
    ; the ends of the vectors above.
    row = { TIME_MIN: 0D, TIME: 0D, TIME_MAX: 0D, COUNTS:0, EXPOSURE:0.0, RADIUS:0.0, COUNT_RATE: 0.0, RATE_ERR: 0.0, GROUPING:0, GRP_MEDIAN_ENERGY: 0.0, GRP_MEDIAN_ENERGY_ERR: 0.0}
    bin_table = replicate(row, nbins)
    bin_table.TIME_MIN          = bin_min 
    bin_table.TIME              = bin_center 
    bin_table.TIME_MAX          = bin_max 
    bin_table.COUNTS            = hist
    bin_table.EXPOSURE          = exposure
    bin_table.RADIUS            = nan_mask * radius
    bin_table.COUNT_RATE        = nan_mask * flux  
    bin_table.RATE_ERR          = nan_mask * flux_error 
    bin_table.GROUPING          = group_codes
    bin_table.GRP_MEDIAN_ENERGY     = median_energy
    bin_table.GRP_MEDIAN_ENERGY_ERR = median_energy_error
    
    writefits, lc_smooth_fn, 0, pheader
    mwrfits, bin_table, lc_smooth_fn, theader
    print, this_snr_goal, lc_smooth_fn, F='(%"Grouped and smoothed light curves (SNR=%0.1f) & median energy timeseries written to %s")'

    fxaddpar, stats, 'PROB_KS',  probks, 'KS variability significance level'
    fxaddpar, stats, 'CREATOR', creator_string
    writefits, stats_fn, 0, stats
    
    if (this_snr_goal GT snr_range[0]) then begin
      ; If we expected to achieve the SNR goal in adaptive_density_2d then defensively check that occured.
      snr = (nan_mask * flux) / (nan_mask * flux_error)
      if (total(/INT, (snr/this_snr_goal LT 0.95)) GT 0) then print, 'WARNING! Some smoothed light curve samples failed to meet the SNR goal.'
    endif
  endfor; ii
  
  count = total(source_not_observed, /INT)
  if (count GT 0) then print, count, F="(%'\nWARNING!  %d sources were not observed.')"

  color_manager, /X_PSEUDO

endif ;keyword_set(timing)



;; =============================================================================
if keyword_set(arf_correction_filename) then begin
;; =============================================================================
  ;; Read ARF correction table.
  correction_table = mrdfits(arf_correction_filename, 1, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + arf_correction_filename

  corr_energy = correction_table.ENERGY
  corr_factor = correction_table.(1)
  
  arf_correction_column = (tag_names(correction_table))[1]
  
  function_1d, id1, corr_energy, corr_factor, TITLE=arf_correction_filename, XTIT='Energy (keV)', YTIT=arf_correction_column

  for ii = 0, num_sources-1 do begin
    print, F='(%"\n===================================================================")'
    print, 'Source: ', sourcename[ii]

    ;; Construct filenames.
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    arf_fn    = obsdir + arf_basename
    stats_fn  = obsdir + obs_stats_basename

    if (NOT file_test(arf_fn)) then begin
      print, 'CORRECTION SKIPPED: no ARF found.'
      continue
    endif

    ;; ------------------------------------------------------------------------
    ;; Read the existing ARF.
    pheader   = headfits(arf_fn)
    arf_table = mrdfits(arf_fn, 1, arf_header, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + arf_fn


    ;; ------------------------------------------------------------------------
    ;; Add a new column if necessary.
    if (NOT tag_exist( arf_table, arf_correction_column, INDEX=colnum )) then begin
      colnum = n_tags(arf_table)
      
      old_arf_table = temporary(arf_table)
      
      arf_row = create_struct( old_arf_table[0], arf_correction_column, 0.0 )
      arf_table = replicate( arf_row, n_elements(old_arf_table) )
      
      copy_struct, old_arf_table, arf_table
    endif

    ;; ------------------------------------------------------------------------
    ;; Interpolate correction to ARF energy samples, and write to table.
    arf_energy = 0.5 * (arf_table.ENERG_LO + arf_table.ENERG_HI)
    arf_table.(colnum) = interpol(corr_factor, corr_energy, arf_energy)


    ;; ------------------------------------------------------------------------
    ;; Recompute SPECRESP column
    colnames = tag_names(arf_table)
    is_correction = (colnames NE 'ENERG_LO') AND (colnames NE 'ENERG_HI') AND (colnames NE 'SPECRESP') AND (colnames NE 'BASE')
    
    specresp = arf_table.BASE
    for colnum=0, n_tags(arf_table)-1 do begin
      if (is_correction[colnum]) then begin
        specresp = specresp * arf_table.(colnum)
        print, 'Applying ', colnames[colnum]
      endif
    endfor
    
    arf_table.SPECRESP = specresp
    
    
    ;; ------------------------------------------------------------------------
    ;; Rewrite the ARF file.    
    writefits, arf_fn, 0, pheader
    mwrfits, arf_table, arf_fn, arf_header

    ;; ------------------------------------------------------------------------
    ;; Record the mean ARF value over the set of ARF bins whose mid-energy falls in the range specified by user.
    ;; (Thus a bin is included only if >=50% of it is in the energy range.) 
    ;; Since the ARF is not evenly sampled we must integrate the ARF & divide by Erange.
    arf_energy = 0.5 * (arf_table.ENERG_LO + arf_table.ENERG_HI)
    ind = where((arf_energy GE energy_range[0]) AND (arf_energy LE energy_range[1]))
    band_arf_table = arf_table[ind]
    arf_mean_i = total(band_arf_table.SPECRESP * (band_arf_table.ENERG_HI - band_arf_table.ENERG_LO), /D)$
                                                                       / total((band_arf_table.ENERG_HI - band_arf_table.ENERG_LO), /D)

    stats = headfits(stats_fn)
    fxaddpar, stats, 'MEAN_ARF', arf_mean_i, 'mean ARF value (cm^2 counts/photon)'
    writefits, stats_fn, 0, stats
  endfor
endif ;keyword_set(arf_correction_filename)



;; =============================================================================
if keyword_set(extract_backgrounds) then begin
;; =============================================================================

  if NOT keyword_set(min_num_cts)        then min_num_cts=5
  if NOT keyword_set(tweak_backscal)     then tweak_backscal=1
  
  if (tweak_backscal LE 0) then begin
    print, 'ERROR: parameter "tweak_backscal" must be positive'
    GOTO, FAILURE
  endif
  
  ;; For speed we'll do our search for a background region using an image rather than an event list.
  ;; Apply the specified energy filter so we're counting useful background events.
  
  run_command, string(emap_filename, F="(%'get_sky_limits %s verbose=0 precision=2')")
  run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec

  ; Change from "xmin:xmax:#bins" syntax to "xmin:xmax:delx" syntax to ensure that 
  ; temp_bkgimg_fn has square pixels to keep CIAO happy.
  tokens = strsplit(filterspec, '=:#,', /EXTRACT)
  xmin  = float(tokens[1])
  xmax  = float(tokens[2])
  xbins = float(tokens[3])
  ymin  = float(tokens[5])
  ymax  = float(tokens[6])
  binsize = (xmax-xmin)/xbins
  filterspec = string(xmin,xmax,binsize,ymin,ymax,binsize, F='(%"x=%7.2f:%7.2f:%7.4f,y=%7.2f:%7.2f:%7.4f")')

  cmd = string(obsdata_filename, 1000*energy_range, filterspec, temp_bkgimg_fn, F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin %s]"" %s')")
  run_command, cmd  

  
  num_emap_pixels = replicate(-1L,num_sources)

  bkg_geometric_area = !PI*100^2
  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be off-field.
    if source_not_observed[ii] then continue

    ;; Construct filenames.
    sourcedir            = sourcename[ii] + '/'
    unnamed_src_stats_fn = sourcedir + src_stats_basename
    obsdir               = sourcedir + obsname + '/' + extraction_subdir[ii]
    obs_stats_fn         = obsdir + obs_stats_basename
    src_region_fn        = obsdir + src_region_basename
    src_spectrum_fn      = obsdir + src_spectrum_basename
    bkg_region_fn        = obsdir + bkg_region_basename
    bkg_pixels_region_fn = obsdir + bkg_pixels_region_basename
    bkg_emap_fn          = obsdir + bkg_emap_basename
    bkg_events_fn        = obsdir + bkg_events_basename
    bkg_spectrum_fn      = obsdir + bkg_spectrum_basename

    if (~file_test(obs_stats_fn)) then begin
;     print, F='(%"\n===================================================================")'
;     print, 'Source: ', sourcename[ii]
;     print, 'EXTRACTION SKIPPED: source not observed.'
      source_not_observed[ii] = 1
      continue
    endif
    
    print, F='(%"\n===================================================================")'
    print, 'Source: ', sourcename[ii]

    if keyword_set(reuse_background) AND file_test(bkg_spectrum_fn) then continue
    
    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, ['pset dmcopy clobber=yes', 'pset dmimgpick clobber=yes', 'pset dmextract clobber=yes']

    
    ; Look up allowed range of background normalizations.
    unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (~keyword_set(error)) then begin
      BKSCL_LO = sxpar(unnamed_src_stats, 'BKSCL_LO') ; Smallest allowed bkg scaling.
      BKSCL_GL = sxpar(unnamed_src_stats, 'BKSCL_GL') ; Target bkg scaling.
      BKSCL_HI = sxpar(unnamed_src_stats, 'BKSCL_HI') ; Largest allowed bkg scaling.
    endif else message, 'ERROR reading '+unnamed_src_stats_fn
    
    ; Look up exposure integral in source extraction region.
    header = headfits(src_spectrum_fn, ERRMSG=error, EXT=1)    
    if (~keyword_set(error)) then begin
      src_exposurearea = sxpar(header, 'BACKSCAL')
      DETCHANS         = sxpar(header, 'DETCHANS')
    endif else begin
      print, 'WARNING: source spectrum '+src_spectrum_fn+' not found.'
      src_exposurearea = 1
      DETCHANS = 1024
    endelse
    
    bkg_region_supplied = file_test(bkg_region_fn)
      
    ; Initialize vars used in search below.
    obs_stats = headfits(obs_stats_fn, ERRMSG=error)
    if keyword_set(error) then message, 'ERROR reading '+obs_stats_fn
    xpos_catalog = sxpar(obs_stats, 'X_CAT')
    ypos_catalog = sxpar(obs_stats, 'Y_CAT')
    SRC_CNTS     = sxpar(obs_stats, 'SRC_CNTS')
    
    if (~bkg_region_supplied) then begin
      ;; Make a starting guess for the (unmasked) geometric area of the background region.
      ;; If a previous extraction was done use that saved radius, otherwise use the bkg_geometric_area
      ;; value from the previous source.
      ;; Adjust the guess upward if the specified background area ratio suggests that.
      bkg_radius = sxpar(obs_stats, 'BKG_RAD', COUNT=count)
      if (count EQ 1 ) then begin
        bkg_geometric_area = !PI *              (bkg_radius)^2
      endif else begin
        bkg_geometric_area = !PI * (sxpar(obs_stats, 'SRC_RAD'))^2 * BKSCL_GL
      endelse 
    endif ;NOT bkg_region_supplied
      
    BKSCL_HI_vote = !VALUES.F_INFINITY
    done        = 0
    loop_count  = 0
    upper_bound =  !VALUES.F_INFINITY
    lower_bound = -!VALUES.F_INFINITY
    repeat begin
      loop_count = loop_count + 1
        
      ;; ------------------------------------------------------------------------
      if bkg_region_supplied then begin
        ;; If we find bkg_region_fn already exists then we use it.
        ;; This gives the observer a hook for supplying non-circular background regions.
        ae_ds9_to_ciao_regionfile, bkg_region_fn, temp_region_fn
          
        bkg_radius = 0
        bkg_region = string(temp_region_fn, F="(%'region(%s)')")
        done       = 1
  
      endif else begin
        ;; Otherwise use a circular background region.
        bkg_radius = sqrt(bkg_geometric_area/!PI)
        bkg_region = strcompress( string(xpos_catalog, ypos_catalog, bkg_radius, $
                                  F='(%"circle(%0.1f,%0.1f,%0.1f)")'), /REMOVE_ALL )
      endelse ;construct circular region
    
      ;; ------------------------------------------------------------------------
      ;; Find out how many in-band events are in that region.
      cmd = string(temp_bkgimg_fn, bkg_region, temp_image_fn, F="(%'dmcopy ""%s[sky=%s][opt update=no]"" %s')")
      run_command, cmd
      
      bkg_image = readfits(temp_image_fn, /SILENT)
      bkg_counts = total(bkg_image)

      ;; ------------------------------------------------------------------------
      ;; Evaluate the stopping criteria for defining the background region.
      ; 1. The region's background exposure ratio (bkg_exposurearea/src_exposurearea) MUST end up in the range 
      ;    [BKSCL_LO,BKSCL_HI] (obtained from source.stats).  (For coding convenience, a value just above BKSCL_HI is acceptible.)
      ;
      ; 2. If the region's background exposure ratio reaches the target value of BKSCL_GL and the region 
      ;    contains at least MIN_NUM_CTS in-band counts then the search is stopped.
      
      ; Integrate the emap over the region.
      ; Note that the region may extend beyond the boundaries of the emap
      ; and the emap is full of "holes" made by source masks.
      cmd = string(emap_filename, bkg_region, bkg_emap_fn, F="(%'dmcopy ""%s[sky=%s]"" %s')")
      run_command, cmd
    
      emap = readfits(bkg_emap_fn, emap_header, /SILENT)
    
      pixel_size = sxpar(emap_header, 'CDELT1P')
    
      ; Ignore any negative emap pixels.
      bkg_exposurearea = (pixel_size^2) * total(emap>0, /DOUBLE) 

      BACKSCAL         = bkg_exposurearea / src_exposurearea
      
      ; Record the smallest bkg_normalization value with acceptible bkg_counts
      if (bkg_counts GE min_num_cts) then BKSCL_HI_vote <= BACKSCAL
    
      if bkg_region_supplied then begin
        num_emap_pixels[ii] = total(emap GT 0)
        done = 1
      endif else begin
        if            (BACKSCAL LT BKSCL_LO) then begin
          ; Region is too small; raise lower bound.
          lower_bound = max([lower_bound,bkg_geometric_area])
        endif else if (BACKSCAL GT BKSCL_HI) then begin
          ; Region is too large; lower upper bound.
          upper_bound = min([upper_bound,bkg_geometric_area])
        endif else begin
        
          ; Region's size falls in acceptible range.  Now aim for target size and minimum number of counts.
          if (bkg_counts LT min_num_cts) || (BACKSCAL LT BKSCL_GL) then begin
            ; Region is too small; raise lower bound.
            if ((upper_bound-lower_bound)/upper_bound LT 0.05) then begin
              done = 1
              print, "Background region area is smaller than, but within 5% of, the asymptotic value."
            endif else begin            
              lower_bound = max([lower_bound,bkg_geometric_area])
            endelse
          endif else begin
            ; If we make it here, then BACKSCAL is in its acceptible range, and we have enough counts.
            ; We arbitrarily decide to stop the search for the optimal region if the search range has narrowed.
            if ((upper_bound-lower_bound)/upper_bound LT 0.05) then begin
              done = 1
              print, "Background region area is larger than, but within 5% of, the asymptotic value."
            endif else begin            
            ; Region is too large; lower upper bound.
              upper_bound = min([upper_bound,bkg_geometric_area])
            endelse
          endelse
        endelse
        
        
        ; Adjust the region size in preparation for the next iteration.
        if (NOT finite(upper_bound)) then begin
            ; If no upper bound is yet known, then take a big step up.
            bkg_geometric_area = bkg_geometric_area * 2.
        endif else if (NOT finite(lower_bound)) then begin
            ; If no lower bound is yet known, then take a big step down.
            bkg_geometric_area = bkg_geometric_area / 2.
        endif else begin
            ; Bisect the search interval.
            bkg_geometric_area = 0.5 * (upper_bound + lower_bound)
        endelse
        ;help, upper_bound, bkg_geometric_area, lower_bound
        
        ; Enforce a hard upper limit on the bkg region size (4' radius) to prevent an infinite search.
        if (bkg_geometric_area GT (!PI * (2*60.0*4)^2)) then begin
          print, 'WARNING, background region search aborted when region grew to 4 arcmin radius!!!'
          done = 1
        endif
      endelse ; ~bkg_region_supplied

print, sqrt([lower_bound,upper_bound]/!PI), F='(%"Radius range = %0.1f:%0.1f")'

      ; Stop if we've taken an excessive number of steps.
      if (loop_count GT 50) then begin
        print, 'WARNING, background region search aborted after 50 iterations!!!'
        done = 1
      endif
    endrep until (done)
  
    ;; Summarize the quality of the background region.
    if ~finite(BACKSCAL) then begin
      print, 'ERROR: BACKSCAL is not finite.'
      GOTO, FAILURE
    endif
    
    PROB_NO_SOURCE_binomial = binomial(SRC_CNTS, $
                                       SRC_CNTS + bkg_counts, $
                                       1D/(1D + BACKSCAL) , /DOUBLE) > 0
    
;   PROB_NO_SOURCE_poisson  = (1 - poisson_distribution(bkg_counts/BACKSCAL, SRC_CNTS - 1)) > 0
  
    src_cnts_error        = (1 + sqrt(SRC_CNTS   + 0.75))
    bkg_subtraction_error = (1 + sqrt(bkg_counts + 0.75)) / BACKSCAL

    print, bkg_counts, BACKSCAL, loop_count,               F='(%"\nBackground region: %d in-band background counts; normalization =%6.1f; %d iterations.")' 
    
    print, SRC_CNTS, src_cnts_error,                       F='(%"SRC_CNTS                              =%8.1f (+-%5.1f)")' 
  
    print, bkg_counts / BACKSCAL, bkg_subtraction_error,   F='(%"bkg counts in aperture                =%8.1f (+-%5.1f)")' 
  
    print, PROB_NO_SOURCE_binomial,                        F='(%"PROB_NO_SOURCE                        = %8.2g")'
;   print, PROB_NO_SOURCE_binomial, PROB_NO_SOURCE_poisson, F='(%"PROB_NO_SOURCE, actual and asymptotic:%8.2g %8.2g")'
    
      
    ;; ------------------------------------------------------------------------
    cmd = string(emap_filename, bkg_region, bkg_emap_fn, F="(%'dmcopy ""%s[sky=%s]"" %s')")
    run_command, cmd
    
    ; Remove a region file produced by the ae_better_backgrounds tool to avoid confusion about how
    ; the background spectrum was created..
    file_delete, bkg_pixels_region_fn, /ALLOW_NONEXISTENT
                
    ;; The set of emap pixels above define the actual background region to apply to the event list.
    ;; BELOW WE REQUIRE EMAP VALUE TO BE >1, INSTEAD OF >0, BECAUSE CIAO 3.0.1 HAS A BUG THAT 
    ;; CAUSES ZERO VALUES TO PASS THE >0 TEST!

    cmd1 = string(obsdata_filename, bkg_emap_fn, temp_events_fn, $
                  F="(%'dmimgpick ""%s[cols time,ccd_id,sky,pi,energy]"" %s %s method=closest')")

    cmd2 = string(temp_events_fn, bkg_events_fn, F="(%'dmcopy ""%s[#6>1]"" %s')")
    run_command, [cmd1,cmd2]


    ;; ------------------------------------------------------------------------
    ;; Extract background spectrum.
    ;; NOTE: if we ever implement a time filter on the background data then we must 
    ;; reduce bkg_exposurearea below by the ratio 
    ;; (EXPOSURE from bkg_spectrum_fn)/(EXPOSURE from bkg_events_fn) to account for the lost exposure.
    ;; Such time filtering would mess up MIN_NUM_CTS requirement!
    
    cmd = string(bkg_events_fn, DETCHANS, bkg_spectrum_fn, $
                 F="(%'dmextract ""%s[bin pi=1:%d:1]"" %s opt=pha1 error=gaussian')")
    run_command, cmd


    ;; ------------------------------------------------------------------------
    ;; The AE convention is that the BACKSCAL keywords, derived from
    ;; integrals of the exposure map, are used to represent geometric
    ;; area, effective area, and integration time differences between the
    ;; source and background regions.  
    ;; The EXPOSURE keywords are NOT used for background scaling.  We set EXPOSURE=0
    ;; in the background spectrum as a flag to signify the AE convention is being used.
    
    comment = 'EXPOSURE not used for bkg scaling'
    
    openw, unit, temp_text_fn, /GET_LUN
    printf, unit, comment, comment, comment, bkg_exposurearea*tweak_backscal, '(sec*cm^2*skypixel^2); '+comment, $
                  F='(%"#add\nONTIME = 0 / %s\nLIVETIME = 0 / %s\nEXPOSURE = 0 / %s\nBACKSCAL = %f / %s")'
    free_lun, unit
    
    cmd = string(bkg_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
    run_command, cmd, /QUIET


    ;; ------------------------------------------------------------------------
    ;; Save information for summary plots.
    if (bkg_radius EQ 0) then begin
      fxaddpar, obs_stats, 'BKG_RAD',  0,                            'observer supplied background'
    endif else begin
      fxaddpar, obs_stats, 'BKG_RAD',  bkg_radius,                   'background extraction radius (sky pixels)'
    endelse

    fxaddpar, obs_stats, 'BKG_CNTS', bkg_counts,                     string(energy_range, F="(%'background counts, %5.3f:%6.3f keV')") 
    fxaddpar, obs_stats, 'BACKSCAL', BACKSCAL,                      'scaling for BKG_CNTS' 
    fxaddpar, obs_stats, 'BKSCL_LO', BKSCL_LO,                      'smallest BACKSCAL allowed'
    fxaddpar, obs_stats, 'BKSCL_GL', BKSCL_GL,                      'target   BACKSCAL'
    fxaddpar, obs_stats, 'BKSCL_HI', BKSCL_HI,                      'largest  BACKSCAL allowed'
  
    ; This extraction gets to cast votes, used by the adjustment algorithm, for an upper limit on BKSCL_LO and a lower limit on BKSCL_HI.
    
    ; The masked background algorithm we've used above does not have any requirements on BKSCL_LO, so don't cast a vote (0 is no-vote flag).
    BKSCL_LO_vote = 0
      
    ; If the search did NOT identify any scaling that meets the MIN_NUM_CTS goal, then cast a vote to raise BKSCL_HI by 20%.
    if ~finite(BKSCL_HI_vote)               then BKSCL_HI_vote = 1.20 * BKSCL_HI
      
    fxaddpar, obs_stats, 'VOTE_LO', BKSCL_LO_vote, 'vote for upper limit on BKSCL_LO' 
    fxaddpar, obs_stats, 'VOTE_HI', BKSCL_LO_vote, 'vote for lower limit on BKSCL_HI' 
    
    fxaddpar, obs_stats, 'BACKGRND', bkg_counts / bkg_exposurearea , string(energy_range, F="(%'background Intensity (photons/cm^2/sec/skypixel^2),, %5.3f:%6.3f keV')")
    fxaddpar, obs_stats, 'CREATOR', creator_string
    writefits, obs_stats_fn, 0, obs_stats

  endfor ; ii
  
  ;; Report the smallest background regions.
  ind = where(num_emap_pixels NE -1, count)
  if (count GT 0) then begin
    print, F='(%"\n\rThe following sources used observer-supplied background regions.  \n\rUse the ds9 calls below to review whether the pixelization of the regions is acceptable.\n\rSOURCE NAME           # UNMASKED PIXELS IN BKG REGION")'
    forprint, sourcename[ind], num_emap_pixels[ind], $
              'ds9 ' + sourcename[ind] + '/' + obsname + '/' + extraction_subdir[ind] + bkg_emap_basename, $
              F='(A20,2x,I7,4x,A)'
  endif
  
  count = total(source_not_observed, /INT)
  if (count GT 0) then print, count, F="(%'\nWARNING!  %d sources were not observed.')"
  
endif ;keyword_set(extract_backgrounds)





;; =============================================================================
if keyword_set(merge_observations) then begin

;debug_pb = fltarr(num_sources)

  ;; =============================================================================
  ;; Computing composite source & background spectra is a little tricky.  See the theory section of the manual! 
  
  cd, CURRENT=cwd  &  cwd=cwd+'/'

  if ~keyword_set(skip_spectra_p) then begin
    print, 'Photometry computed on the following energy ranges:'
    forprint, eband_lo, eband_hi, F='(F4.1," -",F4.1," keV")', /SILENT
  endif

  ; Remove some entries in CIAO's dmmerge_header_lookup.txt file that produce annoying messages from dmmerge.
  run_command, /QUIET, 'egrep -v "BTIMDRFT|BTIMNULL|BTIMRATE|DEC_NOM|DEC_PNT|DS_IDENT|OBJECT|OBS_ID|RA_NOM|RA_PNT|ROLL_NOM|ROLL_PNT|SEQ_NUM|OBI_NUM|TITLE|DATAMODE|OBSERVER" '+getenv('ASCDS_INSTALL')+'/data/dmmerge_header_lookup.txt > '+dmmerge_header_lookup_fn
  
  obsname_list = ''

  for ii = 0, num_sources-1 do begin
    ; Skip sources already determined to be not extracted.
    if source_not_observed[ii] then continue

    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, /HEASOFT, ['pset addrmf clobber=yes', 'pset addarf chatter=0 clobber=yes']
    run_command, /QUIET, ['pset dmcopy clobber=yes', 'pset reproject_events clobber=yes', 'pset dmmerge clobber=yes lookupTab='+dmmerge_header_lookup_fn]

    basedir   = sourcename[ii] + '/' 
    sourcedir = basedir + merge_subdir[ii]
    named2generic_extraction_path = keyword_set(merge_subdir[ii]) ?  '../' : ''

    composite_psf_fn     = sourcedir + psf_basename
    merged_env_events_fn = sourcedir + env_events_basename
    merged_src_events_fn = sourcedir + src_events_basename
    merged_region_fn     = sourcedir + src_region_basename

    merged_src_spectrum_basename = sourcename[ii] + '.pi'
    merged_bkg_spectrum_basename = sourcename[ii] + '_bkg.pi'
    merged_arf_basename          = sourcename[ii] + '.arf'
    merged_rmf_basename          = sourcename[ii] + '.rmf'
    merged_sequenced_lc_basename = sourcename[ii] + '.sequenced_lc.ps'
    merged_stacked_lc_basename   = sourcename[ii] + '.stacked_lc.ps'

    unnamed_src_stats_fn   = sourcename[ii] + '/' + src_stats_basename
            src_stats_fn   = sourcedir            + src_stats_basename
    photometry_fn          = sourcedir + src_photometry_basename
    merged_src_spectrum_fn = sourcedir + merged_src_spectrum_basename
    merged_bkg_spectrum_fn = sourcedir + merged_bkg_spectrum_basename
    merged_arf_fn          = sourcedir + merged_arf_basename
    merged_rmf_fn          = sourcedir + merged_rmf_basename
    merged_lc_smooth_fn    = sourcedir + lc_smooth_basename
    merged_stacked_lc_fn   = sourcedir + merged_stacked_lc_basename
    merged_sequenced_lc_fn = sourcedir + merged_sequenced_lc_basename

    
    ;; ------------------------------------------------------------------------
    ;; Read the fundamental source properties from the unnamed source.stats file.
    unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra  = sxpar(unnamed_src_stats, 'RA')
      dec = sxpar(unnamed_src_stats, 'DEC')
    endif else message, 'ERROR reading '+unnamed_src_stats_fn
    
    ; These top-level properties form the basis of the named source.stats file.
    src_stats    = unnamed_src_stats
 
    is_diffuse = sxpar(src_stats, 'DIFFUSE')
 
    
    ;; ------------------------------------------------------------------------
    ;; Figure out how far this source has been processed by looking for the data
    ;; products produced by various stages.
    ;; We can NOT search for "neighborhood.evt" because an earlier run of this 
    ;; stage could have made a file <sourcename>/<merge_name>/neighborhood.evt 
    ;; which would be misinterpreted here as an observation! 
    ;; We must instead search for "obs.stats" which appears only in observation
    ;; directories, and then see which of those observations has a "neighborhood.evt".
    
    ; If the observer has specified a list of observations then look for them, otherwise look for any.
    pattern_base = sourcename[ii] + '/' + (keyword_set(obsname) ? obsname : '*') + '/'
    
    if keyword_set(extraction_subdir[ii]) then begin
      ; When EXTRACTION_NAME is supplied the observer desires to merge those extractions, ignoring any obsids that
      ; do not have an extraction of that name.
      ; The directory where the merged products are stored is controlled independently via MERGE_NAME.
      obs_stats_fn   = file_search( pattern_base + extraction_subdir[ii] + obs_stats_basename, COUNT=num_obs )
    endif else begin
      ; Look for unnamed extractions.
      obs_stats_fn   = file_search( pattern_base +                         obs_stats_basename, COUNT=num_obs )
    endelse    
        
    fxaddpar, src_stats, 'NUM_OBS',  num_obs, 'number of observations extracted'

    src_stats_exists = file_test(src_stats_fn)
    if (num_obs EQ 0) then begin
      source_not_observed[ii] = 1
      
      ; If the (named) stats file already exists then we must update it.
      if src_stats_exists then GOTO, MERGE_IS_EMPTY $
      else continue
    endif else begin
      ; We are going to process this source, so create the named merge dir and start the messages
      if ~src_stats_exists then file_mkdir, sourcedir
      
      print, F='(%"\n===================================================================")'
      print, sourcename[ii], strtrim(sxpar(unnamed_src_stats,'LABEL'),2), F='(%"Source: %s (%s)")'      
    endelse

    ;; ------------------------------------------------------------------------
    ;; Make pathnames to all the files we use, e.g. PSFs, neighborhoods, source spectra, bkg spectra?
    obs_data_template =  { $
                         obsname            :'' , $
                         obs_stats_fn       :'', $
                         obs_dir            :'', $
                         unnamed_obs_dir    :'', $
                         psf_fn             :'', $
                         env_events_fn      :'', $
                         psf_frac_fn        :'', $
                         src_events_fn      :'', $
                         src_spectrum_fn    :'', $
                         rmf_fn             :'', $
                         arf_fn             :'', $
                         bkg_spectrum_fn1   :'', $
                         bkg_spectrum_fn2   :'', $
                         lc_smooth_fn       :'', $                         
                         off_angle          :0.0, $
                         emap_avg           :0.0, $
                         src_radius         :0.0, $
                         src_area           :0.0, $
                         mask_radius        :0.0, $
                         fracspec           :0.0, $
                         psf_fraction       :0.0, $ 
                         emap_med           :0.0, $
                         emap_max           :0.0, $
                         overlap            :0.0, $
                         prob_ks            :0.0, $
                         bkg_counts         :0.0, $
                         src_counts         :0.0, $
                         xpos_data          :0.0, $
                         ypos_data          :0.0, $
                         time_on_detector   :0.0, $
                         num_in_warning     :0L   $
                         }
    obs_data = replicate(obs_data_template, num_obs)
                       
    obs_data.obs_dir          = file_dirname(obs_stats_fn, /MARK_DIRECTORY)
    obs_data.unnamed_obs_dir  =       strmid(obs_stats_fn, 0, 1+reform(strpos(obs_stats_fn, '/', 1+strlen(sourcename[ii])), 1,num_obs))
    ; In the line below, delete the vector obs_stats_fn so it won't be mistakenly used after obs_data is pruned.
    obs_data.obs_stats_fn     =    temporary(obs_stats_fn)
    
    obs_data.psf_fn           = obs_data.unnamed_obs_dir + psf_basename
    obs_data.env_events_fn    =         obs_data.obs_dir + env_events_basename
    obs_data.psf_frac_fn      =         obs_data.obs_dir + obs_frac_basename
    obs_data.src_events_fn    =         obs_data.obs_dir + src_events_basename
    obs_data.src_spectrum_fn  =         obs_data.obs_dir + src_spectrum_basename
    obs_data.rmf_fn           =         obs_data.obs_dir + rmf_basename
    obs_data.arf_fn           =         obs_data.obs_dir + arf_basename
    obs_data.bkg_spectrum_fn1 =         obs_data.obs_dir + bkg_spectrum_basename
    obs_data.bkg_spectrum_fn2 = obs_data.unnamed_obs_dir + bkg_spectrum_basename
    obs_data.lc_smooth_fn     =         obs_data.obs_dir + lc_smooth_basename

    if keyword_set(generic_rmf_fn) then obs_data.rmf_fn = generic_rmf_fn

    ; We choose to perform merging operations only where no observation is missing files.
    psf_available          = (num_obs EQ total( file_test(obs_data.psf_fn)))
    neighborhood_available = (num_obs EQ total( file_test(obs_data.env_events_fn)))
    spectra_available      = (num_obs EQ total( file_test(obs_data.src_spectrum_fn)   AND $
                                                file_test(obs_data.rmf_fn)            AND $
                                                file_test(obs_data.arf_fn)))
    timing_available       = (num_obs EQ total(file_test(obs_data.lc_smooth_fn)))
    
    skip_psf          = keyword_set(skip_psf_p)          || ~psf_available 
    skip_neighborhood = keyword_set(skip_neighborhood_p) || ~neighborhood_available
    skip_spectra      = keyword_set(skip_spectra_p)      || ~spectra_available    
    skip_timing       = keyword_set(skip_timing_p)       || ~timing_available 
    
    ra_data      = 0
    dec_data     = 0
    
    ;; ------------------------------------------------------------------------
    ;; READ SPECTRA AND SOURCE PROPERTY KEYWORDS FROM EACH EXTRACTION
    ;; ------------------------------------------------------------------------    
    comment_psf_fraction = ''

    for jj = 0, num_obs-1 do begin
      row = obs_data[jj]  
         
      ;; Read information from spectral files and PSF fraction files.    
      if spectra_available then begin
        src_spectrum = mrdfits(obs_data[jj].src_spectrum_fn, 1, src_header, /SILENT, STATUS=status)
        if (status NE 0) then message, 'ERROR reading ' + obs_data[jj].src_spectrum_fn
        
        if (jj EQ 0) then begin
          ; Expand the tags in the obs_data structure.
          src_channels = src_spectrum.CHANNEL
          
          ; Add tags to the template to hold spectra.
          obs_data_template = create_struct(obs_data_template, $
                                            'src_observed_counts'        , fltarr(n_elements(src_channels)), $
                                            'bkg_observed_counts'        , fltarr(n_elements(src_channels)), $
                                            'bkg_spectrum_fn', '', $
                                            'bkg_arf_fn'     , '', $
                                            'obsid'          , '', $
                                            'src_backscal'   , 0.0, $
                                            'bkg_backscal'   , 0.0, $
                                            'src_exposure'   , 0.0, $
                                            'bkg_exposure'   , 0.0, $
                                            
                                            'psf_x_var'      , 0.0, $
                                            'psf_y_var'      , 0.0, $
                                            'bkg_x_var'      , 0.0, $
                                            'bkg_y_var'      , 0.0 $
                                            )
          temp     = temporary(obs_data)
          obs_data = replicate(obs_data_template, num_obs)
          copy_struct, temp, obs_data

          row = obs_data[jj]  
        endif ; jj EQ 0
      
        if (total(src_channels - src_spectrum.CHANNEL) NE 0) then $
            message, 'ERROR: spectra have different channel sets!'
        
        row.src_observed_counts  = src_spectrum.COUNTS
        row.obsid        = strtrim(sxpar(src_header, 'OBS_ID'),2)
        row.src_backscal         = sxpar(src_header, 'BACKSCAL')
        row.src_exposure         = sxpar(src_header, 'EXPOSURE')
        
        hdr_vals = [row.src_backscal,row.src_exposure]
        dum = where((hdr_vals EQ 0) OR (~finite(hdr_vals)), count)
        if (count GT 0) then message, 'ERROR: BACKSCAL or EXPOSURE keyword is not valid in '+obs_data[jj].src_spectrum_fn

        
        ; Process the BKG spectrum, if available
        ; Use a bkg spectrum (and optional ARF) from the named extraction dir, if available,
        ; or use a bkg spectrum from the generic extraction dir.
        if            file_test(row.bkg_spectrum_fn1) then begin
          row.bkg_spectrum_fn     = row.bkg_spectrum_fn1
          row.bkg_arf_fn          = row.obs_dir         + bkg_arf_basename
        endif else if file_test(row.bkg_spectrum_fn2) then begin
          row.bkg_spectrum_fn     = row.bkg_spectrum_fn2
          row.bkg_arf_fn          = row.unnamed_obs_dir + bkg_arf_basename
        endif else begin
          row.bkg_spectrum_fn     = ''
          row.bkg_arf_fn          = ''
        endelse
  
        if keyword_set(row.bkg_spectrum_fn) then begin
          bkg_spectrum = mrdfits(row.bkg_spectrum_fn, 1, bkg_header, /SILENT, STATUS=status)
          if (status NE 0) then message, 'ERROR reading ' + row.bkg_spectrum_fn
          
          if (total(src_channels - bkg_spectrum.CHANNEL) NE 0) then $
              message, 'ERROR: spectra have different channel sets!'
          row.bkg_observed_counts  = bkg_spectrum.COUNTS
          row.bkg_backscal         = sxpar(bkg_header, 'BACKSCAL')
          row.bkg_exposure         = sxpar(bkg_header, 'EXPOSURE')
          
          hdr_vals = [row.bkg_backscal]
          dum = where((hdr_vals EQ 0) OR (~finite(hdr_vals)), count)
          if (count GT 0) then message, 'ERROR: BACKSCAL or EXPOSURE keyword is not valid in '+obs_data[jj].bkg_spectrum_fn
        endif else print, 'WARNING!  Background spectrum for ObsId '+row.obsid+' is missing.'

        if keyword_set(row.bkg_arf_fn) && file_test(row.bkg_arf_fn) && ~keyword_set(emap_energy) then begin
          print, row.bkg_arf_fn, F='(%"ERROR: to use background ARF (%s) in scaling background spectrum you must supply an EMAP_ENERGY value (keV).")'
          GOTO, FAILURE
        endif
                
        ; AE spectra should have bkg EXPOSURE = 0.
        ; EPIC spectra should have identical src and bkg EXPOSURE.
        if ((row.bkg_exposure NE 0) && (abs(row.bkg_exposure - row.src_exposure)/row.src_exposure GT 0.01)) then $
          message, 'ERROR: background spectrum does not follow AE conventions'
        

        if psf_available then begin
          ; Use the PSF fraction entry that's at the nominal energy.
          psf_fraction = mrdfits(obs_data[jj].psf_frac_fn, 1, /SILENT, STATUS=status)
          if (status NE 0) then message, 'ERROR reading ' + obs_data[jj].psf_frac_fn
          
          ind = where(abs(psf_fraction.energy - nominal_psf_energy) LT 0.1, count)
          if (count EQ 0) then ind=[0]
          
          this = psf_fraction[ind[0]]
    
          row.psf_x_var            = this.x_sdev^2
          row.psf_y_var            = this.y_sdev^2
                                            
          dum = where(tag_names(this) EQ 'BKG_X_SDEV', count)
          if (count EQ 0) then begin
            ; For backward compatibility (before BKG_X_SDEV columns existed) ...
            row.bkg_x_var = this.x_sdev^2
            row.bkg_y_var = this.y_sdev^2
          endif else begin
            row.bkg_x_var = this.bkg_x_sdev^2
            row.bkg_y_var = this.bkg_y_sdev^2
          endelse
        endif ; psf_available
      endif ; spectra_available
      
      
     
      ;; Read FITS keywords in obs.stats.
      obs_stats = headfits(obs_data[jj].obs_stats_fn)
      row.obsname      =           strtrim( sxpar(obs_stats,'OBSNAME'), 2 )
      row.bkg_counts   =                    sxpar(obs_stats,'BKG_CNTS')
      row.src_counts   =                    sxpar(obs_stats,'SRC_CNTS')
      row.xpos_data    =                    sxpar(obs_stats,'X_DATA')
      row.ypos_data    =                    sxpar(obs_stats,'Y_DATA')
      row.off_angle    =                    sxpar(obs_stats,'THETA')
      row.emap_avg     =                    sxpar(obs_stats,'EMAP_AVG')
      row.src_radius   =                    sxpar(obs_stats,'SRC_RAD')
      row.src_area     =                    sxpar(obs_stats,'SRC_AREA')
      row.mask_radius  =                    sxpar(obs_stats,'MSK_RAD')
      row.fracspec     =                    sxpar(obs_stats,'FRACSPEC')
      row.psf_fraction =                    sxpar(obs_stats,'PSF_FRAC',COMMENT=comment_psf_fraction)
      row.emap_med     =                    sxpar(obs_stats,'EMAP_MED')
      row.emap_max     =                    sxpar(obs_stats,'EMAP_MAX')
      row.overlap      =                    sxpar(obs_stats,'OVERLAP')
      row.prob_ks      =                    sxpar(obs_stats,'PROB_KS')
      row.time_on_detector =                sxpar(obs_stats,'EXPOSURE')*sxpar(obs_stats,'FRACEXPO')
      row.num_in_warning   =                sxpar(obs_stats,'SRC_CNTS')*sxpar(obs_stats,'WARNFRAC')
    
      ;; Store the data.
      obs_data[jj] = temporary(row)
    endfor ; jj
    
    unpruned_exposure = total(obs_data.emap_avg)
    
    ;; ------------------------------------------------------------------------
    ;; PRUNE THE OBSERVATIONS TO ELIMINATE EXTRACTION REGIONS FALLING OFF THE DETECTOR EDGE
    ;; ------------------------------------------------------------------------
    
; 2009 Jan We cannot think of a clear reason to discard sources near the field edge.  Non-uniformity in the emap leads to inaccuracy in the ARF, but that's true anywhere, including chip gaps.  The use of a Chandra+ACIS PSF (e.g. from MARX) for aperture correction also seems not quite right, since aperture correction seems like a property of the HRMA, not of the dithering; again, though, this is an issue anywhere the emap is not flat.  Thus, it's hard to see what's special about the field edge and why we should whack these extractions.  However, I'm leaving the code below, commented out, in case we change our minds and want a mechanism for pruning edge extractions.

;    accepted_ind = where((obs_data.emap_med/obs_data.emap_max) GT emap_uniformity_limit, num_obs)
;                          
;    count = n_elements(obs_data)-num_obs
;    if (num_obs EQ 0) then begin
;      print, 'WARNING: all apertures appear to be spanning a detector edge; no data left to merge.'
;      GOTO, MERGE_IS_EMPTY
;    endif else if (count GT 0) then begin
;      print, count, F="(%'WARNING: ignoring %d extractions with apertures that appear to be spanning a detector edge.')"
;    end
;
;    obs_data = obs_data[accepted_ind]
     

    ;; ------------------------------------------------------------------------
    ;; PRUNE THE OBSERVATIONS TO ELIMINATE SEVERE OVERLAP OF EXTRACTION REGIONS
    ;; We do not have confidence that backgrounds can be accurately estimated 
    ;; when two sources share counts via severly overlapping extraction regions.
    ;; ------------------------------------------------------------------------
    accepted_ind = where(obs_data.overlap LT overlap_limit, num_obs)
      
    if (num_obs EQ 0) then begin
      ; Although all the extractions have excessive overlap, we still need to merge some subset of the data so that we'll
      ; have rough photometry of the source to help us decide later whether to prune this source or his neighbor.
      ; Merging only the _single_ ObsId with the minimum overlap proved to be a poor design, because there are sometimes
      ; cases where that one happens to have photometry very differerent from one or more other ObsIds with overlap values 
      ; comparable to the smallest one.
      ; Thus we arbitrarily stretch the overlap limit upward by 20% for this source.
      stretched_overlap_limit = 1.2 * min(obs_data.overlap)
      print, n_elements(obs_data), stretched_overlap_limit, F="(%'WARNING: all %d extractions have excessive OVERLAP; accepting only those with OVERLAP comparable to the best extraction (i.e. OVERLAP<=%0.2f).')"      
      accepted_ind = where(obs_data.overlap LT stretched_overlap_limit, num_obs)
    endif
    
    obs_data = obs_data[accepted_ind]
        

    ; Use RMF to figure out the energy bounds for each spectral channel.
    ; We pass null ARF_FN because we have not yet loaded any OBS_DATA.
    ae_photometry, RMF_FN=obs_data[0].rmf_fn, ARF_FN=''
    

    ;; ------------------------------------------------------------------------
    ;; IF DESIRED, PRUNE THE OBSERVATIONS TO OPTIMIZE SOMETHING
    ;; ------------------------------------------------------------------------
    if spectra_available && (num_obs GT 1) && (keyword_set(merge_for_pb) || keyword_set(merge_for_position)) then begin
      
      
      ;; ------------------------------------------------------------------------
      if keyword_set(merge_for_pb) then begin
        if ~keyword_set(min_num_cts) then begin
          print, 'ERROR: MIN_NUM_CTS must be specified!'
          goto, FAILURE
        endif
        
        ;; The P_B calculation below is supposed to replicate that in the section "COMPUTE PHOTOMETRY".

        ;; The observer wishes to prune the observation to minimize Pb, computed over ENERGY_RANGE.
        ;; Pb is the significance of the observed SRC_CNTS as a disproof of the "null hypothesis" which is 
        ;; that there is no source, i.e. that all the observed counts are background.  
        ;; We use equation A7 from Weisskopf 2006 (astro-ph/0609585).
        Pb_single_obs       = fltarr(num_obs)
        SRC_CNTS_single_obs = lonarr(num_obs)
        flag_value = 100  ; A large Pb value signifying that we have already considered an obsid.
        accepted_ind = -1 ; Defensive coding ...
        jj=0
        while 1 do begin          
          if (jj LT num_obs) then begin
            ; Compute Pb for each obsid on its own.
            proposed_ind = jj
          endif else begin
            ; Propose to add the best unused observation.
            temp = min(Pb_single_obs, this_ind)
            ; Stop the search when we have considered all obsids (Pb_single_obs is full of flag values).
            if (temp EQ flag_value) then break
            
            proposed_ind = [accepted_ind, this_ind]
          endelse
          
          ; Compute photometry, Pb on the proposed set of extractions.
          ae_photometry, OBS_DATA=obs_data[proposed_ind], SRC_CHANNELS=src_channels
          ae_photometry, energy_range[0], energy_range[1], photometry

          
          if (jj LT num_obs) then begin
            ; Compute Pb for each obsid on its own.
            SRC_CNTS_single_obs[jj  ] = photometry.SRC_CNTS
                  Pb_single_obs[jj++] = photometry.PROB_NO_SOURCE 
                  
            ; Once Pb_single_obs is complete, begin the optimization search.
            if (jj EQ num_obs) then begin
              ; Accept the obsid with the smallest Pb.
              best_PROB_NO_SOURCE = min(Pb_single_obs, accepted_ind)
              SRC_CNTS_accepted   = SRC_CNTS_single_obs[accepted_ind]
              
              ; Flag the obsid we just considered so it won't be considered again.
              Pb_single_obs[accepted_ind] = flag_value
            endif 
          endif else begin
            ; Accept the proposed set of obsids if:
            ; * We do not yet have the specified minimum number of SRC_CNTS
            ; * Pb is zero 
            ; * Pb has decreased 
            if (SRC_CNTS_accepted LT min_num_cts) || (photometry.PROB_NO_SOURCE EQ 0) || $
                                                     (photometry.PROB_NO_SOURCE LT best_PROB_NO_SOURCE) then begin
              accepted_ind        = proposed_ind
              best_PROB_NO_SOURCE = photometry.PROB_NO_SOURCE
              SRC_CNTS_accepted   = photometry.SRC_CNTS
            endif
            
            ; Flag the obsid we just considered so it won't be considered again.
            Pb_single_obs[this_ind] = flag_value
          endelse
         endwhile
        fxaddpar, src_stats, 'MERGPRUN', 'MERGE_FOR_PB', 'pruning algorithm applied' 
        
;debug_pb[ii] = best_PROB_NO_SOURCE
        
      ;; ------------------------------------------------------------------------
      endif else if keyword_set(merge_for_position) then begin
        ;; The observer wishes to prune the observation to minimize position error, computed over ENERGY_RANGE.
        ;; The position uncertainty calculation below is supposed to replicate that
        ;; in the section "ESTIMATE POSITION UNCERTAINTY".
      
        position_error_single_obs = fltarr(num_obs)
        flag_value = 1E6  ; A large error value signifying that we have already considered an obsid.
        jj=0
        while 1 do begin
          if (jj LT num_obs) then begin
            ; Compute position error for each obsid on its own.
            proposed_ind = jj
          endif else begin
            ; Propose to add the best unused observation.
            temp = min(/NAN, position_error_single_obs, this_ind)
            ; Stop the search when we have considered all obsids (position_error_single_obs is full of flag values).
            if (temp EQ flag_value) then break
            
            proposed_ind = [accepted_ind, this_ind]
          endelse
          
          ; Compute photometry on the proposed set of extractions.
          ae_photometry, OBS_DATA=obs_data[proposed_ind], SRC_CHANNELS=src_channels
          ae_photometry, energy_range[0], energy_range[1], photometry, x_distribution_variance, y_distribution_variance


          ; The position error calculations below must be skipped when any of the terms are not available.
          if (photometry.SRC_CNTS EQ 0) || (x_distribution_variance LE 0) || (y_distribution_variance LE 0)  then begin
            this_position_error = !VALUES.F_NAN    
          endif else begin
            expos_data = sqrt(x_distribution_variance) / SQRT(photometry.SRC_CNTS)
            eypos_data = sqrt(y_distribution_variance) / SQRT(photometry.SRC_CNTS)
            this_position_error = sqrt( expos_data^2 + eypos_data^2 ) 
          endelse
          
          if (jj LT num_obs) then begin
            ; Compute position error for each obsid on its own.
            position_error_single_obs[jj++] = this_position_error
                           
            ; Once position_error_single_obs is complete, begin the optimization search.
            if (jj EQ num_obs) then begin
              ; Accept the obsid with the smallest position error
              best_position_error = min(/NAN, position_error_single_obs, accepted_ind)
             
              ; Flag the obsid we just considered so it won't be considered again.
              position_error_single_obs[accepted_ind] = flag_value
            endif 
          endif else begin
            ; If position error is zero or has decreased then accept the proposed set of obsids.
            if (this_position_error EQ 0) || (this_position_error LT best_position_error) then begin
              accepted_ind        = proposed_ind
              best_position_error = this_position_error
            endif
            
            ; Flag the obsid we just considered so it won't be considered again.
            position_error_single_obs[this_ind] = flag_value
          endelse
         endwhile
        fxaddpar, src_stats, 'MERGPRUN', 'MERGE_FOR_POSITION', 'pruning algorithm applied' 
      endif       
      
      
      
      ; Defensively check for duplicates in the accepted_ind indexes since a mistake here is really bad!
      if (n_elements(accepted_ind[UNIQ(accepted_ind, SORT(accepted_ind))]) GT num_obs) then message, 'ERROR: BUG IN AE!!' 
      
      obs_data = obs_data[accepted_ind]
      
      fxaddpar, src_stats, 'BANDPRUN', string(energy_range, F="(%'[%0.2f:%0.2f] keV')")
    endif else begin
      ; pruning NOT attempted
      sxdelpar, src_stats, ['MERGPRUN','BANDPRUN']
    endelse

    
    ; Load the extractions we've chosen to merge into the ae_photometry routine.
    if spectra_available then ae_photometry, OBS_DATA=obs_data, SRC_CHANNELS=src_channels
    
    num_obs = n_elements(obs_data)

    fxaddpar, src_stats, 'MERGNUM',  num_obs,  'number of observations merged'

    mergfrac = total(obs_data.emap_avg)/unpruned_exposure
    if (mergfrac LT 0.99) then print, round(100*(1-mergfrac)), F="(%'\nPruning algorithms discarded %d%% of the exposure.')"
    fxaddpar, src_stats, 'MERGFRAC', mergfrac, 'fraction of extracted data merged'
    
    
    ;; ------------------------------------------------------------------------
    ;; MERGE FITS KEYWORDS
    ;; ------------------------------------------------------------------------
    ;; Appropriately combine single-obsid source properties from multiple obs.stats files.
    total_src_counts = total(obs_data.src_counts)
    total_exposure   = spectra_available ? total(obs_data.src_exposure) : 1.0
    
    dum = max(obs_data.emap_avg, imax)
    fxaddpar, src_stats, 'PRIM_OBS', obs_data[imax].obsname,                   'deepest merged observation'
    
    fxaddpar, src_stats, 'EMAP_TOT', total(      obs_data.emap_avg),                 'sum of EMAP_AVG values for merged observations'
    fxaddpar, src_stats, 'WARNFRAC', total(/INT, obs_data.num_in_warning)  / total_src_counts,       'fraction of events in merged warning regions'
    fxaddpar, src_stats, 'OBSNAME',  strjoin(obs_data.obsname,','),            'merged observations'
    fxaddpar, src_stats, 'SRC_CNTS', round(total_src_counts),                  'source counts, in-band, merged observations'

    if ~is_diffuse then begin
      fxaddpar, src_stats, 'FRACEXPO', total(      obs_data.time_on_detector)/ total_exposure,         'time_on_detector/EXPOSURE'
      
      ; For off-axis angles, use the average weighted by exposure.
      exposure_weight = obs_data.emap_avg * (obs_data.off_angle    GT 0)
      merged_off_angle       = total(obs_data.off_angle   * exposure_weight) / (total(exposure_weight) > 1)
      fxaddpar, src_stats, 'THETA',    merged_off_angle,                       'average off-axis angle (arcmin) for merged observations'
      fxaddpar, src_stats, 'THETA_LO', min(obs_data.off_angle),                'smallest off-axis angle (arcmin) for merged observations'
      fxaddpar, src_stats, 'THETA_HI', max(obs_data.off_angle),                'largest off-axis angle (arcmin) for merged observations'
      
      ; For source extraction properties, use the average weighted by exposure.
      exposure_weight = obs_data.emap_avg * (obs_data.src_area     GT 0)
      merged_src_area        = total(obs_data.src_area    * exposure_weight) / (total(exposure_weight) > 1)
      fxaddpar, src_stats, 'SRC_AREA',  merged_src_area,                      'average extraction area (skypix^2) for merged observations'
  
      exposure_weight = obs_data.emap_avg * (obs_data.src_radius   GT 0)
      merged_src_radius      = total(obs_data.src_radius  * exposure_weight) / (total(exposure_weight) > 1)
      fxaddpar, src_stats, 'SRC_RAD',  merged_src_radius,                      'average source radius (skypix) for merged observations'
  
      exposure_weight = obs_data.emap_avg * (obs_data.psf_fraction GT 0)
      merged_psf_fraction    = total(obs_data.psf_fraction * exposure_weight) / (total(exposure_weight) > 1)
      fxaddpar, src_stats, 'PSF_FRAC', merged_psf_fraction,                    'average '+comment_psf_fraction+' for merged observations'
  
      ; Report crowding information about the most-crowded observation merged.
      fxaddpar, src_stats, 'FRACSPEC', min(obs_data.fracspec),                 'smallest target PSF fraction for merged observations'
      fxaddpar, src_stats, 'OVRLP_LM', overlap_limit,                          'overlap limit used for pruning'
      fxaddpar, src_stats, 'OVRLP_LO', min(obs_data.overlap, imin),            'smallest overlap fraction for merged observations'
      fxaddpar, src_stats, 'OVRLP_HI', max(obs_data.overlap, imax),            'largest overlap fraction for merged observations'
      fxaddpar, src_stats, 'BESTOBS' , obs_data[imin].obsname,                 'least crowded merged observation'
      fxaddpar, src_stats, 'WORSTOBS', obs_data[imax].obsname,                 'most crowded merged observation'
    endif ; ~is_diffuse

    ; For mask radius, report the maximum value.
    fxaddpar, src_stats, 'MSK_RAD', max(obs_data.mask_radius),               'largest mask radius (sky pixels) for merged observations'
    
    ; Report timing information from the ObsId showing the most variability.
    fxaddpar, src_stats, 'PROB_KS', min(/NAN,obs_data.prob_ks),              'KS significance for most-variable merged observation'
    
    
    
    ; Use the most on-axis observation as both the template for the composite PSF and as the reference tangent plane for reprojected event lists.
    dum = min(obs_data.off_angle, reference_observation_index)

    
    ;; ------------------------------------------------------------------------
    ;; MERGE PSF IMAGES
    ;; ------------------------------------------------------------------------
    if (sxpar(src_stats, 'DIFFUSE')) then GOTO, MERGE_NEIGHBORHOODS
    
    ;; Construct composite PSF image by weighting single-observation normalized 
    ;; PSFs by exposure.
    file_delete, /ALLOW_NONEXISTENT, composite_psf_fn
    
    if (skip_psf) then begin
      if NOT keyword_set(skip_psf_p) then print, F='(%"\nWARNING!  PSFs missing; not merged.")'
      GOTO, MERGE_NEIGHBORHOODS
    endif
    
    
    ; Use the most on-axis observation as the template for the composite PSF.
    composite_psf = 0
    composite_psf_hdr = headfits(obs_data[reference_observation_index].psf_fn, ERRMSG=error )
    if (keyword_set(error)) then message, 'ERROR reading ' + obs_data[reference_observation_index].psf_fn
    fxaddpar, composite_psf_hdr, 'EQUINOX', 2000.0
    
    
    radius50 = !VALUES.F_INFINITY
                                       
    exposure_weight = obs_data.emap_avg / (total(obs_data.emap_avg) > 1)
    for jj = 0, num_obs-1 do begin
      ; Read this observation's PSF closest to the nominal energy.      
      fits_open, obs_data[jj].psf_fn, fcb
      psf_energy = fltarr(1+fcb.NEXTEND)
      for kk =0, fcb.NEXTEND do begin
        fits_read, fcb, dum, header, /HEADER_ONLY, /NO_PDU, EXTEN_NO=kk
        psf_energy[kk] = sxpar(header, 'ENERGY') 
      endfor
      dum = min(abs(psf_energy - nominal_psf_energy), imin)
      fits_read, fcb, psf_img, header, /NO_PDU, EXTEN_NO=imin
      fits_close, fcb
      
      ; Set all the NaN values to zero to keep future computations happy.
      ind = where(finite(psf_img) EQ 0, count)
      if (count GT 0) then psf_img[ind] = 0
      
      ; Each PSF is resampled onto the composite PSF template by hastrom.pro, which should not change the scale of the image pixels.
      ; The resampled PSF is then normalized by the total power under the full PSF, and is then scaled by the relative exposure of the observation.
      fxaddpar, header, 'EQUINOX', 2000.0
      hastrom, psf_img, header, composite_psf_hdr, MISSING=0
      
      psf_total = sxpar(header, 'PSF_TOTL')
      if (psf_total EQ 0) then begin
        print, "WARNING: obsolete PSFs in "+obs_data[jj].unnamed_obs_dir+" may have incorrect scaling.  AE versions 3.6 or higher can make better PSFs, but REMEMBER THAT THE /CONSTRUCT_REGIONS STAGE WILL OVERWRITE THE SOURCE'S EXTRACTION REGION FILE!"
        psf_total = total(psf_img, /DOUBLE)
      endif
      composite_psf = composite_psf + psf_img * float(exposure_weight[jj]/psf_total) 
      
      ; Keep track of the smallest RADIUS50 value found.
      this_radius50 = sxpar(header, 'RADIUS50')
      if (this_radius50 GT 0) then radius50 = radius50 < this_radius50
    endfor ;jj, looping over observations
    
    sxdelpar, composite_psf_hdr, 'EXTNAME'
    fxaddpar, composite_psf_hdr, 'HDUNAME', 'COMPOSITE PSF'
    fxaddpar, composite_psf_hdr, 'CREATOR', creator_string
    fxaddpar, composite_psf_hdr, 'RADIUS50', radius50, 'smallest RADIUS50 value'
    fxaddpar, composite_psf_hdr, 'RA',             ra, 'source position', F='(F10.6)'
    fxaddpar, composite_psf_hdr, 'DEC',           dec, 'source position', F='(F10.6)'
    writefits, composite_psf_fn, composite_psf, composite_psf_hdr

    
MERGE_NEIGHBORHOODS:
    ;; ------------------------------------------------------------------------
    ;; MERGE NEIGHBORHOOD EVENT LISTS
    ;; ------------------------------------------------------------------------
    ;; Align & merge the neighborhood event lists.
    ;; Use the most on-axis observation as the reference tangent plane for reprojected event lists.
    file_delete, /ALLOW_NONEXISTENT, merged_env_events_fn, merged_src_events_fn, merged_region_fn
    
    if (skip_neighborhood) then begin
      if NOT keyword_set(skip_neighborhood_p) then print, F='(%"\nWARNING!  Neighborhoods missing; not merged.")'
      GOTO, MERGE_SPECTRA
    endif
    
    if (num_obs EQ 1) then begin
      path = named2generic_extraction_path+strmid(obs_data[0].env_events_fn,strlen(basedir))
      file_link, path, merged_env_events_fn
      print, merge_subdir[ii]+env_events_basename, path, F='(%"              %s --> %s")' 
    endif else begin
      temp_events_fn    = tempdir + string(indgen(num_obs), F='(%"temp%d.evt")')
      
      for jj=0,num_obs-1 do begin
        ; Link to reference observation's data, or reproject other observations.
        if (jj EQ reference_observation_index) then begin
          file_delete, temp_events_fn[jj], /ALLOW_NONEXISTENT
          file_link, cwd+obs_data[reference_observation_index].env_events_fn, temp_events_fn[jj]
        endif else begin
          cmd = string(obs_data[jj].env_events_fn, temp_events_fn[jj], obs_data[reference_observation_index].env_events_fn, $
                       F='(%"reproject_events %s %s match=%s aspect=none random=-1")')
          run_command, cmd
        endelse
      endfor
      
      ; We use columnList to remove the PHAS column so that 3x3 & 5x5 files can be merged.
      ; We whack the expno column to work around a bug in CIAO 3.4 that corrupts the GTI tables.
      ; We save the STATUS column for use by the ae_afterglow_report tool.
      forprint, temp_events_fn, F='(%"%s[subspace -expno,-sky]")', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
      cmd = string( temp_text_fn, merged_env_events_fn, $
                    F="(%'dmmerge ""@-%s"" columnList=""-phas"" outfile=%s  ')")
      run_command, cmd
      
      ; We have to set TDMIN & TDMAX to get ds9 to produce a nice default binning of the data.
      ; Let's use the FOV of the reprojection master (TDMIN/TDMAX keywords for the X and Y columns).
      env_header = headfits(obs_data[reference_observation_index].env_events_fn, EXT=1)
  
      fxbfind, env_header, 'TTYPE', dum1, TTYPE, dum2, 'null'
      colnames = strlowcase( strtrim(TTYPE,2) )
      x_colnum = 1+where(strlowcase(colnames) EQ 'x')
      y_colnum = 1+where(strlowcase(colnames) EQ 'y')
      
      TDMINx = sxpar(env_header, string(x_colnum,F='(%"TDMIN%d")'), COUNT=c1)
      TDMAXx = sxpar(env_header, string(x_colnum,F='(%"TDMAX%d")'), COUNT=c2)
      TDMINy = sxpar(env_header, string(y_colnum,F='(%"TDMIN%d")'), COUNT=c3)
      TDMAXy = sxpar(env_header, string(y_colnum,F='(%"TDMAX%d")'), COUNT=c4)
      
      if (c1+c2+c3+c4 EQ 4) then begin
        ; Figure out the column numbers for X and Y in the merged file.
        merged_header = headfits(merged_env_events_fn, EXT=1)
        fxbfind, merged_header, 'TTYPE', dum1, TTYPE, dum2, 'null'
        colnames = strlowcase( strtrim(TTYPE,2) )
        x_colnum = 1+where(strlowcase(colnames) EQ 'x')
        y_colnum = 1+where(strlowcase(colnames) EQ 'y')
        
        openw, unit, temp_text_fn, /GET_LUN
        printf, unit, x_colnum, TDMINx, x_colnum, TDMAXx, y_colnum, TDMINy, y_colnum, TDMAXy, F='(%"#add\nTDMIN%d=%d\nTDMAX%d=%d\nTDMIN%d=%d\nTDMAX%d=%d")'
        free_lun, unit
        
        cmd = string(merged_env_events_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
        run_command, cmd, /QUIET
      endif
    endelse

    ;; ------------------------------------------------------------------------
    ;; Evaluate the consistency between the data and the PSF.
    ;; The composite PSF we have on hand is the full PSF rather than one truncated
    ;; by all the extraction regions.  (We made the decision not to burn up the
    ;; disk space required to save truncated PSFs back in the EXTRACT stage.)
    ;; This full PSF cannot be compared to the composite EXTRACTED event data 
    ;; (which is computed below).  Instead we must compare to the un-truncated 
    ;; composite event data (computed above).
    ;;
    ;; However, we can't analyze data too far in the wings of the PSF because we
    ;; may run into a neighboring source.
    ;; Thus, we choose to work in a simple circular aperture that approximates the
    ;; smallest extraction region used in all the observations.
 
; Nov 2007  Commented out this call due to worries about the CPU resources required.    
;    ae_radial_profile, ASPECT_FN='none', ra, dec, merged_env_events_fn, src_radius, energy_range, composite_psf_fn, tempdir, $
;                       PLOT=(verbose GT 1), WIDGET_IDS=ae_radial_profile_ids, PSF_NAME='PSF', $
;                       ks_psf, R_MEDIAN, EE_AT_RM
;    
;    fxaddpar, src_stats, 'KS_PSF',   ks_psf,   'KS significance between PSF & events'
;    fxaddpar, src_stats, 'R_MEDIAN', R_MEDIAN, 'median event distance from X_CAT,Y_CAT'
;    fxaddpar, src_stats, 'EE_AT_RM', EE_AT_RM, 'truncated PSF encircled fraction at R_MEDIAN'


    ;; ------------------------------------------------------------------------
    ;; MERGE SOURCE EVENT LISTS
    ;; ------------------------------------------------------------------------
    ;; Align & merge the source event lists.
    ;; Use the most on-axis observation as the reference tangent plane for reprojected event lists.
    if (num_obs EQ 1) then begin
      path = named2generic_extraction_path+strmid(obs_data[0].src_events_fn,strlen(basedir))
      file_link, path, merged_src_events_fn
      print, merge_subdir[ii]+src_events_basename, path, F='(%"              %s --> %s")' 
    endif else begin
      temp_events_fn    = tempdir + string(indgen(num_obs), F='(%"temp%d.evt")')
      
      for jj=0,num_obs-1 do begin
        ; Link to reference observation's data, or reproject other observations.
        if (jj EQ reference_observation_index) then begin
          file_delete, temp_events_fn[jj], /ALLOW_NONEXISTENT
          file_link, cwd+obs_data[reference_observation_index].src_events_fn, temp_events_fn[jj]
        endif else begin
          cmd = string(obs_data[jj].src_events_fn, temp_events_fn[jj], obs_data[reference_observation_index].src_events_fn, $
                       F='(%"reproject_events %s %s match=%s aspect=none random=-1")')
          run_command, cmd
        endelse
      endfor
      
      ; We use columnList to remove the PHAS column so that 3x3 & 5x5 files can be merged.
      ; We whack the expno column to work around a bug in CIAO 3.4.
      ; We save the STATUS column for use by the ae_afterglow_report tool.
      forprint, temp_events_fn, F='(%"%s[subspace -expno,-sky]")', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
      cmd = string( temp_text_fn, merged_src_events_fn, $
                    F="(%'dmmerge ""@-%s"" columnList=""-phas"" outfile=%s  ')")
      run_command, cmd
      
      if (c1+c2+c3+c4 EQ 4) then begin
        openw, unit, temp_text_fn, /GET_LUN
        printf, unit, x_colnum, TDMINx, x_colnum, TDMAXx, y_colnum, TDMINy, y_colnum, TDMAXy, F='(%"#add\nTDMIN%d=%d\nTDMAX%d=%d\nTDMIN%d=%d\nTDMAX%d=%d")'
        free_lun, unit
        
        cmd = string(merged_src_events_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
        run_command, cmd, /QUIET
      endif
    endelse

    
    ;; ------------------------------------------------------------------------
    ;; Compute a mean position for all the in-band events in all observations.
    ;; The energy range is controlled by the ENERGY_RANGE keyword.
    
    ;; The position is easy, we just filter the composite event list by energy, 
    ;; find the mean event position, and convert to celestial coordinates.
    cmd = string(merged_src_events_fn, 1000*energy_range, inband_events_fn, $
                         F="(%'dmcopy ""%s[energy=%6.1f:%7.1f]"" %s')")
    run_command, cmd

    inband_events = mrdfits(inband_events_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + inband_events_fn

    ; If the FITS table is empty, mrdfits will return a scalar zero.
    if NOT keyword_set(inband_events) then begin
      ;; There are no in-band data so we skip various statistics.
      print, 'WARNING: no in-band data found in source region.'
    endif else begin 
      ; Build astrometic structure from data header.
      fxbfind, theader, 'TTYPE', dum1, TTYPE, dum2, 'null'
      fxbfind, theader, 'TCTYP', dum1, TCTYP, dum2, 'null'
      fxbfind, theader, 'TCRVL', dum1, TCRVL, dum2, 0.0D
      fxbfind, theader, 'TCRPX', dum1, TCRPX, dum2, 0.0D
      fxbfind, theader, 'TCDLT', dum1, TCDLT, dum2, 0.0D
      colnames = strlowcase( strtrim(TTYPE,2) )
      x_ind    = where(strlowcase(colnames) EQ 'x')
      y_ind    = where(strlowcase(colnames) EQ 'y')
      make_astr, event2wcs_astr, DELTA=TCDLT[[x_ind,y_ind]], CTYPE=TCTYP[[x_ind,y_ind]], $
                                 CRPIX=TCRPX[[x_ind,y_ind]], CRVAL=TCRVL[[x_ind,y_ind]]

      ; Convert mean position to celestial coordinates.
      ; REMEMBER THAT THE xy2ad and ad2xy programs assume that (x,y) are 
      ; ZERO-BASED pixel indexes.  Thus we must subtract 1 from the (x,y) 
      ; positions when converting to RA,DEC.
      xy2ad, mean(inband_events.x, /DOUBLE)-1, mean(inband_events.y, /DOUBLE)-1, event2wcs_astr, ra_data, dec_data
    endelse

    fxaddpar, src_stats, 'RA_DATA',  ra_data,      'source position, data mean', F='(F10.6)'
    fxaddpar, src_stats, 'DEC_DATA', dec_data,     'source position, data mean', F='(F10.6)'

    if is_diffuse then begin
      ; Assign RA and DEC so that COLLATE stage can make sensible region files.
      fxaddpar, src_stats, 'RA',  ra_data,      'source position, data mean', F='(F10.6)'
      fxaddpar, src_stats, 'DEC', dec_data,     'source position, data mean', F='(F10.6)'
    endif

        
    ;; ------------------------------------------------------------------------
    ;; Construct composite region file showing extraction polygons for all observations.
   
    ; Open region file.
    ; CIAO filtering has very odd quirks.  It seems to produce fewer errors if
    ; we include the DS9 header comment line in the region file.
    openw,  region1_unit, merged_region_fn, /GET_LUN
    printf, region1_unit, "# Region file format: DS9 version 3.0"
    
    for jj = 0, num_obs-1 do begin
      theader = headfits(obs_data[jj].env_events_fn, EXT=1, ERRMSG=error )
      if (keyword_set(error)) then message, 'ERROR reading ' + obs_data[jj].env_events_fn
      
      ; Build astrometic structure from data header.
      fxbfind, theader, 'TTYPE', dum1, TTYPE, dum2, 'null'
      fxbfind, theader, 'TCTYP', dum1, TCTYP, dum2, 'null'
      fxbfind, theader, 'TCRVL', dum1, TCRVL, dum2, 0.0D
      fxbfind, theader, 'TCRPX', dum1, TCRPX, dum2, 0.0D
      fxbfind, theader, 'TCDLT', dum1, TCDLT, dum2, 0.0D
      colnames = strlowcase( strtrim(TTYPE,2) )
      x_ind    = where(strlowcase(colnames) EQ 'x')
      y_ind    = where(strlowcase(colnames) EQ 'y')
      make_astr, event2wcs_astr, DELTA=TCDLT[[x_ind,y_ind]], CTYPE=TCTYP[[x_ind,y_ind]], $
                                 CRPIX=TCRPX[[x_ind,y_ind]], CRVAL=TCRVL[[x_ind,y_ind]]

      
      ;; Convert polygon to WCS & write to the region file.      
      ;; REMEMBER THAT THE xy2ad and ad2xy programs assume that (x,y) are 
      ;; ZERO-BASED pixel indexes.  Thus we must subtract 1 from the sky (x,y) 
      ;; positions when converting to RA,DEC.
      ae_ds9_to_ciao_regionfile, obs_data[jj].obs_dir + src_region_basename, '/dev/null', $
                                 /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y
      
      xy2ad, polygon_x-1, polygon_y-1, event2wcs_astr, polygon_ra, polygon_dec
      
      polygon = dblarr(2,n_elements(polygon_ra))
      polygon[0,*] = polygon_ra
      polygon[1,*] = polygon_dec
  
      src_region = 'polygon(' + strcompress(strjoin(string(polygon,F='(F10.6)'),","), /REMOVE) + ')'
      
      ; Assign a color to the obsname.  The first element of obsname_list is ''.
;      obs_index = where(obs_data[jj].obsname EQ obsname_list, count)
;      if (count EQ 0) then begin
;        ; Append the obsname, and point to it.
;        obsname_list = [obsname_list,obs_data[jj].obsname]
;        obs_index    = n_elements(obsname_list)-1
;      endif
;      color = region_colors[(obs_index-1) mod n_elements(region_colors)]

      color = 'green'
      printf, region1_unit, src_region, obs_data[jj].obsname, color, F='(%"J2000;%s # move=0 edit=0 tag={%s} color=%s")' 
    endfor ; loop over neighborhood event files
    free_lun, region1_unit

    

MERGE_SPECTRA:
    ;; ------------------------------------------------------------------------
    ;; MERGE SOURCE AND BACKGROUND SPECTRA
    ;; ------------------------------------------------------------------------
    ;; Process the component source and background spectra.

    ;; ------------------------------------------------------------------------
    ;; ESTIMATE POSITION UNCERTAINTY
    ;; ------------------------------------------------------------------------
    if (skip_spectra) then begin
      ; If spectra are not available, then we can't make the ae_photometry calls below..
      GOTO, MERGE_RESPONSES
    endif
    
    ;; Estimate the standard deviation (uncertainty) on the mean data position computed earlier.
    ;; The energy range is controlled by the ENERGY_RANGE keyword.

    ;; Estimating this position error is a bit tricky.  
    ;; For N observations, the composite event list is a MIXTURE of data from 2N
    ;; distributions, two from each obsid: 
    ;; * A PSF (truncated by the extraction region).
    ;; * A flat background footprint (truncated by the extraction region).
    ;;
    ;; The distribution of the mixture, p(x), is a weighted sum of these distributions:
    ;;   p(x) = w1*p1(x) + w2*p2(x) + w3*p3(x) + ...
    ;; where the weights sum to 1.0.
    ;;
    ;; We use single-obsid photometry for each obsid in the specified energy band to get
    ;; these weights.
    ;;
    ;; The variance of p(x), VARp, can be written as
    ;;   VARp = Ep[X^2] - u^2
    ;; where Ep[.] is the expectation operator using p(x), and u is the mean of p(x),
    ;; rather than in the usual form Ep[(X-u)^2].
    ;;
    ;; These expectation operators deal nicely with p(x) as a linear combination of distributions:
    ;;   VARp = w1*Ep1[X^2] + w2*Ep2[X^2] + ... - u^2
    ;;
    ;; The mean of p(x), u, is just the weighted sum of the individual means:
    ;;   u = w1*u1 + w2*u2 + ...
    ;;
    ;; IF we had earlier computed and saved the Ep?[X^2] and u? quantities for each observation 
    ;; (in a common coordinate system, .e.g centered on the source position) then we would
    ;; compute VARp as shown above. 
    ;;
    ;; However, for now we have (in obs.psffrac) only the standard deviations without the
    ;; means.  So, here we will make the simplifying assumption that the means are identical.
    ;; Then we can write the variance in the standard way:
    ;;   VARp = Ep[(X-u)^2]
    ;;        = w1*Ep1[(X-u)^2] + w2*Ep2[(X-u)^2] + ...
    ;;        = w1*VARp1        + w2*VARp2        + ...    
    
    ae_photometry, energy_range[0], energy_range[1], photometry, x_distribution_variance, y_distribution_variance
           
            
    ; The position error calculations below must be skipped when any of the terms are not available.
    if photometry.SRC_CNTS EQ 0 || (ra_data  EQ 0) || (x_distribution_variance LE 0)  ||$
                                   (dec_data EQ 0) || (y_distribution_variance LE 0)  then begin
      pos_data_err = !VALUES.F_NAN    
    endif else begin
      ; Compute the "standard error of the mean" by dividing by sqrt(N).
      ; That formula is asymptotically correct as N gets large.  I don't know what to do for small N.
      ; We do NOT use the Student's T distribution for this calculation.  The Student's T is applied in the case where the variance of the parent distribution is (poorly) estimated from the N data points. In our case we've estimated the variance of the parent distribution of the composite event data.
      expos_data = sqrt(x_distribution_variance) / SQRT(photometry.SRC_CNTS)
      eypos_data = sqrt(y_distribution_variance) / SQRT(photometry.SRC_CNTS)
   
      ; These X and Y position errors describe an "error ellipse" around our estimated position.
      ; It's convenient and apparently traditional to approximate this ellipse by an
      ; "error circle" whose radius is the root mean square of the X & Y standard deviations, 
      ; often called a "dRMS" or "1DRMS" error (short for "1 deviation RMS").
      ; The integral of the error ellipse inside this dRMS radius (i.e. significance of this
      ; circular confidence region) varies from 63% for equal errors in X & Y (a circular ellipse)
      ; to 68% for a highly eccentric ellipse.
      
      ; I don't know a good astronomical reference for this practice, but see:
     
      ; Section 2.2 of "A Standardized Algorithm for the Determination of Position Errors 
      ; by the Example of GPS with and without 'Selective Availability'" 
      ; Ingo Harre, published in 'The International Hydrographic Review', Vol. 2, No. 1 (New Series), June 2001
      ; http://www.mar-it.de/NavGen/final_text3.pdf
           
      ; Section 4-10 of Hydrographic Surveying engineering manual, US Army Corps of Engineers
      ; http://www.nap.usace.army.mil/channel/em/4.pdf
  
      ; Section 4.5 of http://www.usace.army.mil/usace-docs/eng-manuals/em1110-1-1003/c-4.pdf
  
      ; Section 2.4.3 of Principles and Practice of GPS Surveying
      ; http://www.gmat.unsw.edu.au/snap/gps/gps_survey/chap2/243.htm
  
  
      ; And we'll also convert from SKY pixels to arcseconds 
      ; for the convenience of the source table generator.
      pos_data_err = sqrt( expos_data^2 + eypos_data^2 ) * event2wcs_astr.CDELT[1] * 3600
      
    endelse
    fxaddpar, src_stats, 'ERR_DATA', pos_data_err, 'error circle (arcsec) around (RA_DATA,DEC_DATA), aka dRMS error', F='(F10.6)'


    
    ;; ------------------------------------------------------------------------
    ;; CREATE COMPOSITE SPECTRA AND RESPONSES

    ;; NOTE that to support two approaches to spectral fitting---simultaneously modeling of the background spectrum, 
    ;; and traditional background subtraction---we are storing the background scaling information in the AREASCAL column, 
    ;; rather than the traditional BACKSCAL column.  Section 2.3 of the XSPEC manual shows that AREASCAL will scale the
    ;; background spectrum in both styles of analysis.
    ;;
    ;; Note that this trick---making a single background spectral file that can either be declared as the BACKFILE to
    ;; the source spectrum, OR can be loaded as a second observed spectrum for simultaneous fitting---can only be done
    ;; because we are using the convention that both BACKSCAL=1 and AREASCAL=1 in the source spectrum.

    ;; We are careful to set both src & bkg EXPOSURE keywords to the same total exposure so that XSPEC
    ;; will not do any more scaling of the background beyond what we've specified via AREASCAL.
    ;; ------------------------------------------------------------------------
MERGE_RESPONSES:
    file_delete, /ALLOW_NONEXISTENT, merged_src_spectrum_fn, merged_bkg_spectrum_fn, merged_rmf_fn, merged_arf_fn, photometry_fn 
    
    if (skip_spectra) then begin
      if NOT keyword_set(skip_spectra_p) then print, F='(%"\nWARNING!  Spectra or responses missing; not merged.")'
      GOTO, MERGE_TIMING
    endif
    
    ; Retrieve the multi-obsid spectra for all channels.
    ae_photometry, SRC_CHANNELS=src_channels, SRC_CNTS_spectrum=SRC_CNTS_spectrum, BKG_CNTS_spectrum=BKG_CNTS_spectrum
    num_channels = n_elements(src_channels)

    ;; ------------------------------------------------------------------------
    ;; Build the FITS header parts that are common to src & bkg.
    theader = 0   &   dum=temporary(theader)
    fxaddpar, theader, 'EXPOSURE', total_exposure, 'total exposure in merged obsids'
    fxaddpar, theader, 'EXTNAME',  'SPECTRUM'
    fxaddpar, theader, 'CORRFILE',  'none'
    fxaddpar, theader, 'CORRSCAL',  1.0
    fxaddpar, theader, 'RESPFILE',  merged_rmf_basename
    fxaddpar, theader, 'ANCRFILE',  merged_arf_basename
    fxaddpar, theader, 'HDUCLASS',  'OGIP'
    fxaddpar, theader, 'HDUCLAS1',  'SPECTRUM'
    fxaddpar, theader, 'HDUVERS',   '1.2.0'
    fxaddpar, theader, 'POISSERR', 'T' 
    fxaddpar, theader, 'SYS_ERR',  0  
    fxaddpar, theader, 'QUALITY',  0  
    fxaddpar, theader, 'DETCHANS', num_channels  
    fxaddpar, theader, 'OBS_ID',   strjoin(obs_data.obsid,',')
    fxaddpar, theader, 'GROUPING', 0
    fxaddpar, theader, 'CREATOR', creator_string  
    
    keynames = ['TELESCOP','INSTRUME','FILTER','CHANTYPE','OBJECT']
    for jj=0,n_elements(keynames)-1 do begin
      fxaddpar, theader, keynames[jj], sxpar(src_header, keynames[jj])
    endfor
    
    
    ;; ------------------------------------------------------------------------
    ;; Write the source spectrum.
    fxhmake,  pheader, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, pheader, 'CREATOR', creator_string
    fxaddpar, pheader, "FNFITS", merged_src_spectrum_fn

    writefits, merged_src_spectrum_fn, 0, pheader
    
    row = { CHANNEL: 0, COUNTS: 0L }
    bin_table = replicate(row, num_channels)
    bin_table.CHANNEL = src_channels 
    bin_table.COUNTS  = SRC_CNTS_spectrum
    
    src_theader = theader
    fxaddpar, src_theader, 'BACKFILE',  merged_bkg_spectrum_basename
    fxaddpar, src_theader, 'BACKSCAL',  1.0, 'background scaling completely specified in BACKFILE'
    fxaddpar, src_theader, 'AREASCAL',  1.0, 'background scaling completely specified in BACKFILE'
    fxaddpar, src_theader, 'HDUCLAS2',  'TOTAL'
    fxaddpar, src_theader, 'HDUCLAS3',  'COUNT'
    mwrfits, bin_table, merged_src_spectrum_fn, src_theader
    print, 'Src spectrum: ', merge_subdir[ii]+merged_src_spectrum_basename
    
    
    ;; ------------------------------------------------------------------------
    ;; Write the background spectrum.
    ;; The single (not energy-dependent) background scaling we will use for this multi-obsid spectrum is that which produces the correct photometry over ENERGY_RANGE.
    ae_photometry, energy_range[0], energy_range[1], photometry
    
    fxhmake, pheader, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, pheader, 'CREATOR', creator_string
    fxaddpar, pheader, "FNFITS", merged_bkg_spectrum_fn

    writefits, merged_bkg_spectrum_fn, 0, pheader
    
    row = { CHANNEL: 0, COUNTS: 0L}
    bin_table = replicate(row, num_channels)
    bin_table.CHANNEL = src_channels 
    bin_table.COUNTS  = BKG_CNTS_spectrum

    bkg_theader = theader
    fxaddpar, bkg_theader, 'BACKFILE',  'none'
    fxaddpar, bkg_theader, 'BACKSCAL',  1.0,                 'background scaling specified by AREASCAL'
    fxaddpar, bkg_theader, 'AREASCAL',  photometry.BACKSCAL, 'background scaling'
    fxaddpar, bkg_theader, 'HDUCLAS2',  'BKG'
    fxaddpar, bkg_theader, 'HDUCLAS3',  'COUNT'
    mwrfits, bin_table, merged_bkg_spectrum_fn, bkg_theader
    print, 'Bkg spectrum: ', merge_subdir[ii]+merged_bkg_spectrum_basename
    
    
    ;; ------------------------------------------------------------------------
    ;; Compare the shapes of the source & background spectra using a 2-sided 
    ;; Kolmogorov-Smirnov statistic.
    total_src_observed_counts = total(SRC_CNTS_spectrum)
    total_bkg_observed_counts = total(BKG_CNTS_spectrum)
    src_cum_distn = total(SRC_CNTS_spectrum, /CUMULATIVE) / total_src_observed_counts
    bkg_cum_distn = total(BKG_CNTS_spectrum, /CUMULATIVE) / total_bkg_observed_counts

    n_eff = (total_src_observed_counts*total_bkg_observed_counts) / float(total_src_observed_counts + total_bkg_observed_counts)

    ks = max( abs( src_cum_distn - bkg_cum_distn ) )
    prob_ks, ks, n_eff, ks_spect


    ;; ------------------------------------------------------------------------
    ;; The final RMF is a weighted average of the constituent RMFs.
    ;; The final ARF is a weighted average of the constituent ARFs.
    ;; We run the addrmf,addarf commands using an ASCII list file because the command line
    ;; parameters seem to have limiting length restrictions.
    rmf_fn        = strmid(obs_data.rmf_fn, strlen(basedir))
    arf_fn        = strmid(obs_data.arf_fn, strlen(basedir))
    rmf_result_fn = strmid(merged_rmf_fn,   strlen(basedir))
    arf_result_fn = strmid(merged_arf_fn,   strlen(basedir))
  
    exposure_fraction = obs_data.src_exposure / total_exposure

    ; Use the supplied generic RMF, or link to a single obsid's RMF, or average multiple RMFs.
    use_longest_observation = 0
    dum = max(obs_data.src_exposure,imax)

    if keyword_set(generic_rmf_fn) then begin
      ; Don't create any merged RMF file.
    endif else if (num_obs EQ 1) then begin
      ; We use a symbolic link to avoid copying the single RMF & ARF.
      path = named2generic_extraction_path+rmf_fn
      file_link, path, basedir+rmf_result_fn
      print, rmf_result_fn, path, F='(%"              %s --> %s")' 
    endif else begin
      ; Use addrmf to combine the RMFs weighting by exposure_fraction.
      forprint, rmf_fn, exposure_fraction, F='(A0,1x,G0.3)', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
      cmd = string( temp_text_fn, rmf_result_fn, F="(%'addrmf ""@%s"" rmffile=%s')")

      run_command, /HEASOFT, DIRECTORY=basedir, cmd, STATUS=status
      
      if keyword_set(status) then begin
        ; Sometimes the RMFs don't have the same structure and addrmf fails.
        ; In this case we simply link to the primary obsid's RMF.
        print, 'WARNING: addrmf call failed, probably because we are trying to combine RMFs with different energy bins.'
        print, 'WARNING: we will use the RMF & ARF from the longest observation.'
        file_delete, merged_rmf_fn, /ALLOW_NONEXISTENT
        path = named2generic_extraction_path+rmf_fn[imax]
        file_link, path, basedir+rmf_result_fn
        print, rmf_result_fn, path, F='(%"              %s --> %s")' 
        use_longest_observation = 1
      endif 
    endelse ;(num_obs GT 1)
    
    
    ; Link to a single ARF, or link to the longest ARF if we had an addrmf failure above, or average multiple ARFs.
    if (num_obs EQ 1) then begin
      ; We use a symbolic link to avoid copying the single  ARF.
      path = named2generic_extraction_path+arf_fn
      file_link, path, basedir+arf_result_fn
      print, arf_result_fn, path, F='(%"              %s --> %s")' 
    endif else if use_longest_observation then begin
      ;!!!!!! As of March 05, when addrmf fails above due to mismatched energy bins then addarf will fail with "ERROR: Energy bins mismatch", but will NOT return a non-zero exit status.  Thus, we must use the flag "use_longest_observation" to signal that we must link to the longest observation's ARF here rather than calling addarf which would likely suffer an undetected failure.
      path = named2generic_extraction_path+arf_fn[imax]
      file_link, path, basedir+arf_result_fn
      print, arf_result_fn, path, F='(%"              %s --> %s")'
    endif else begin
      ; Use addarf to combine the ARFs weighting by exposure_fraction.
      forprint, arf_fn, exposure_fraction, F='(A0,1x,G0.3)', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
      cmd = string( temp_text_fn, arf_result_fn, F="(%'addarf ""@%s"" out_ARF=%s')")

      run_command, /HEASOFT, DIRECTORY=basedir, cmd, STATUS=status 

      if keyword_set(status) then begin
        print, 'WARNING: addrmf call failed, probably because we are trying to combine ARFs with different energy bins.'
        print, 'WARNING: we will use the ARF from the longest observation.'
        file_delete, merged_arf_fn, /ALLOW_NONEXISTENT
        path = named2generic_extraction_path+arf_fn[imax]
        file_link, path, basedir+arf_result_fn
        print, arf_result_fn, path, F='(%"              %s --> %s")'
      endif   
    endelse
    
    
    ;; ------------------------------------------------------------------------
    ;; COMPUTE PHOTOMETRY
    ;; ------------------------------------------------------------------------

    ;; ------------------------------------------------------------------------
    ;; Integrate the spectrum and ARF over a variety of interesting bands and write a photometry table
    
    ; Load the merged response files into the ae_photometry object.
    ae_photometry, RMF_FN=keyword_set(generic_rmf_fn) ? generic_rmf_fn : merged_rmf_fn, ARF_FN=merged_arf_fn 

    flux_table = replicate( photometry, n_elements(eband_lo) )
    for jj = 0, n_elements(flux_table)-1 do begin
      ae_photometry, eband_lo[jj], eband_hi[jj], photometry
      
      flux_table[jj] = photometry
    endfor ;jj, looping over energy bands in flux table
    
            
    flux_header = 0  &  dum=temporary(flux_header)
    fxaddpar, flux_header, 'EXTNAME', 'WIDE_BAND_PHOTOMETRY'
    fxaddpar, flux_header, 'EXPOSURE', total_exposure, 'total exposure in merged obsids'
    fxaddpar, flux_header, 'TUNIT1', 'keV'
    fxaddpar, flux_header, 'TUNIT2', 'keV'
    fxaddpar, flux_header, 'TUNIT3', 'none'
    fxaddpar, flux_header, 'TUNIT4', 'none'
    fxaddpar, flux_header, 'TUNIT5', 'cm^2 counts/photon'
    fxaddpar, flux_header, 'TUNIT6', 'counts'
    fxaddpar, flux_header, 'TUNIT7', 'counts'
    fxaddpar, flux_header, 'TUNIT8', 'none'
    fxaddpar, flux_header, 'TUNIT9', 'counts'
    fxaddpar, flux_header, 'TUNIT10','counts'
    fxaddpar, flux_header, 'TUNIT11','counts'
    fxaddpar, flux_header, 'TUNIT12','none'
    fxaddpar, flux_header, 'TUNIT13','none'
    fxaddpar, flux_header, 'TUNIT14','photon/cm^2/s'
    fxaddpar, flux_header, 'TUNIT15','photon/cm^2/s'
    fxaddpar, flux_header, 'TUNIT16','keV'
    fxaddpar, flux_header, 'TUNIT17','keV'
    fxaddpar, flux_header, 'TUNIT18','keV'
    fxaddpar, flux_header, 'TUNIT19','keV'
    fxaddpar, flux_header, 'TUNIT20','keV'
    fxaddpar, flux_header, 'TUNIT21','keV'
    fxaddpar, flux_header, 'TUNIT22','keV'
    fxaddpar, flux_header, 'TUNIT23','keV'

    fxhmake,  header, /INITIALIZE, /EXTEND, /DATE
    fxaddpar, header, 'CREATOR', creator_string
    writefits, photometry_fn, 0, header
    
    mwrfits, flux_table, photometry_fn, flux_header
    print, 'Fluxes      : ', merge_subdir[ii]+src_photometry_basename

    
    ;; ------------------------------------------------------------------------
    ;; Save summary information about merged spectra.
    fxaddpar, src_stats, 'BACKGRND', total_bkg_observed_counts / total(obs_data.bkg_backscal) , $
                     'background Intensity (photons/cm^2/sec/skypixel^2), total-band, merged observations'

    area_exposure_ratio = float(obs_data.src_backscal)/obs_data.bkg_backscal
    fxaddpar, src_stats, 'SCAL_MAX', 1. / min(area_exposure_ratio), 'largest bkg scaling in merged observations'
    fxaddpar, src_stats, 'SCAL_MIN', 1. / max(area_exposure_ratio), 'smallest bkg scaling in merged observations'
    fxaddpar, src_stats, 'KS_SPECT',  ks_spect,       'KS significance between src & bkg spectra'
    fxaddpar, src_stats, 'EXPOSURE',  total_exposure, 'total exposure in merged observations'

    ae_photometry, nominal_psf_energy, nominal_psf_energy, photometry

    fxaddpar, src_stats, 'EFFAREA',  photometry.MEAN_ARF, $
                                     string(nominal_psf_energy, F='(%"ARF value @%6.4f keV (cm^2 counts/photon)")')  
       
    
    
MERGE_TIMING:    
    ;; ------------------------------------------------------------------------
    ;; MERGE TIMING INFORMATION
    ;; ------------------------------------------------------------------------
    file_delete, /ALLOW_NONEXISTENT, merged_stacked_lc_fn, merged_sequenced_lc_fn
    
    if (skip_timing) then begin
      GOTO, MERGE_SAVE_STATS
    endif

    ;; Merge the FITS light curve files.
    forprint, obs_data.lc_smooth_fn, F='(%"%s")', TEXTOUT=temp_text_fn, /SILENT, /NOCOMMENT
    cmd = string( temp_text_fn, merged_lc_smooth_fn, $
                  F="(%'dmmerge ""@-%s"" columnList="""" outfile=%s  ')")
    run_command, cmd
    
      
    ;; Gather the single-obsid light curves that we wish to plot.
    ;; The first num_obs pointers are for the binned & grouped light curves.
    ;; The "time" values are the centers of the bins.
    ;; The second num_obs pointers are for the smoothed light curves.
    times                = ptrarr(2*num_obs)
    halfbin_sizes        = ptrarr(2*num_obs)
    rates                = ptrarr(2*num_obs)
    rate_errors          = ptrarr(2*num_obs)
    median_energies      = ptrarr(2*num_obs)
    median_energy_errors = ptrarr(2*num_obs)
    tstart          = dblarr(num_obs)
    tstop           = dblarr(num_obs)
    variability_probs = fltarr(num_obs)
    rate_span  = 0
    min_energy = !VALUES.F_INFINITY
    max_energy = 0
    abutted_exposure = 0
    abutted_counts   = 0
    num_groups_all_obs = 0L

    ; Even pointers are binned LC, odd are smooth LC.
    for jj = 0, num_obs-1 do begin
      lc_fn = obs_data[jj].lc_smooth_fn
      if (NOT file_test(lc_fn)) then continue

      tb = mrdfits(lc_fn, 1, lc_header, /SILENT, STATUS=status)
      if (status NE 0) then message, 'ERROR reading ' + lc_fn
      
      variability_probs[jj] = sxpar(lc_header, 'PROB_KS')
      
      tstart[jj] = min(tb.TIME_MIN)
      tstop [jj] = max(tb.TIME_MAX)
      
      ; Extract the binned time series and integrate over the groups.
      counts      = tb.COUNTS
      exposure    = tb.EXPOSURE
      group_codes = tb.GROUPING
      
      abutted_counts   = [abutted_counts,   counts]
      abutted_exposure = [abutted_exposure, exposure]
      
;help, counts, exposure, group_codes
      ; Integrate counts and exposure over each group.
      ind = [where(group_codes EQ 1, num_groups), n_elements(tb)]
      
      num_groups_all_obs = num_groups_all_obs + num_groups

      time           = dblarr(num_groups)
      halfbin_size   = fltarr(num_groups)
      total_counts   = fltarr(num_groups)
      group_exposure = fltarr(num_groups)
      median_energy       = fltarr(num_groups)
      median_energy_error = fltarr(num_groups)
      
      for kk=0,num_groups-1 do begin
        ind_left  = ind[kk]
        ind_right = ind[kk+1] - 1
        
        time_left          = ((tb.time)[ind_left]  - tstart[jj]) / 1000.  ; to get ks
        time_right         = ((tb.time)[ind_right] - tstart[jj]) / 1000.  ; to get ks
        time          [kk] = mean([time_left,time_right])
        halfbin_size  [kk] = 0.5 * (time_right - time_left)
  
        total_counts  [kk] = total(counts  [ind_left : ind_right])
        group_exposure[kk] = total(exposure[ind_left : ind_right])
        median_energy      [kk] = tb[ind_left].GRP_MEDIAN_ENERGY      / 1000.  ; to get keV 
        median_energy_error[kk] = tb[ind_left].GRP_MEDIAN_ENERGY_ERR  / 1000.  ; to get keV
      endfor ;kk

      aa = total(counts) & bb = total(total_counts)
      if (abs(aa-bb)/aa GT 0.01) then message, 'ERROR: looks like a code bug'
      
      aa = total(exposure) & bb = total(group_exposure)
      if (abs(aa-bb)/aa GT 0.01) then message, 'ERROR: looks like a code bug'
      
;help, time, total_counts, group_exposure, halfbin_size    

      ; Keep track of the span of the data.
      min_energy = min_energy < min([10,median_energy],/NAN)
      max_energy = max_energy > max([ 0,median_energy],/NAN)
      
      ; Change NaN values to zero in median_energy_error to keep oploterror happy later.
      ind = where(finite(median_energy_error) EQ 0, num_nan)
      if (num_nan GT 0) then median_energy_error[ind] = 0

      rate  =      total_counts  / group_exposure * 1000.
      error = sqrt(total_counts) / group_exposure * 1000.
      times               [jj] = ptr_new(time)
      halfbin_sizes       [jj] = ptr_new(halfbin_size)
      rates               [jj] = ptr_new(rate)
      rate_errors         [jj] = ptr_new(error)
      median_energies     [jj] = ptr_new(median_energy)      
      median_energy_errors[jj] = ptr_new(median_energy_error)      

      rate_span = rate_span > max([0,rate+error],/NAN)

      
      ; Extract the smoothed time series information.  Convert time to ks relative to TSTART.
      time                = (tb.TIME - tstart[jj]) / 1000.  ; to get ks
      rate                =  tb.COUNT_RATE         * 1000.
      error               =  tb.RATE_ERR           * 1000.



      ; Keep track of the span of the data.
      rate_span  = rate_span > max([0,rate+error],/NAN)

      ; Trim off the NaN tails.
      ind = where(finite(rate), count)
      if (count EQ 0) then begin
        count = 1
        ind   = [0]
      endif
      first_index = ind[0]
      last_index  = ind[count-1]
      
      time                = time               [first_index:last_index]
      rate                = rate               [first_index:last_index]
      error               = error              [first_index:last_index]
      
;help, time, rate, error, median_energy     

      times               [num_obs+jj] = ptr_new(time)
      rates               [num_obs+jj] = ptr_new(rate)
      rate_errors         [num_obs+jj] = ptr_new(error)
    endfor ;jj
    

    ;; ------------------------------------------------------------------------
    ;; Test the composite data for variability.
    uniform_flux_model = total(abutted_exposure, /DOUBLE, /CUMULATIVE) / total(abutted_exposure, /DOUBLE)
    inband_src_counts  = total(abutted_counts,   /DOUBLE)
    cum_count_distn    = total(abutted_counts,   /DOUBLE, /CUMULATIVE) / inband_src_counts
   
    ; Keep in mind that the time bins we're using here were constructed (in /TIMING) 
    ; so that events fall on the bin boundaries.  The event is counted (abutted_counts) in 
    ; the bin to the LEFT of the boundary.
    ; Thus the stair steps in the cumulative count distribution occur at bin boundaries.
    ; At each of those steps, the K-S test requires that we compute two distances:
    ; 1. From the model distribution to the base of the stair step.
    ;    This is done by comparing uniform_flux_model[i] to cum_count_distn[i-1].
    ; 2. From the model distribution to the stop of the stair step.
    ;    This is done by comparing uniform_flux_model[i] to cum_count_distn[i].
    ks_distance = max( abs( uniform_flux_model - [0,cum_count_distn] ) ) > $
                  max( abs( uniform_flux_model -    cum_count_distn  ) )
    
    if (inband_src_counts LT 4) then begin
      ; We need at least 4 counts for KS probability to be meaningful.
      probks = !VALUES.F_NAN
      print, 'Variability analysis skipped -- too few in-band counts.'
    endif else prob_ks, ks_distance, inband_src_counts, probks
      
    fxaddpar, src_stats, 'MERGE_KS',  probks, 'KS variability across multiple ObsIds'
        

    if (num_groups_all_obs LE 1) then goto, LC_CLEANUP
    ;; ------------------------------------------------------------------------
    ;; Make a plot for each obsid, stacked on the same page.
    ; Put a little margin on the axis ranges.
    time_span       = max(tstop-tstart) / 1000.
    xrange          = [-0.02*time_span, 1.02*time_span]
    rate_axis_range = [0, 1.02*rate_span]
    
    energy_span = max_energy - min_energy
    if (energy_span LT 1.0) then begin
      min_energy = 0 > (min_energy - 0.5 * (1.0-energy_span))
      max_energy = min_energy + 1.0
    endif
    energy_axis_range = [min_energy, max_energy]
    
    ;; Set up the Postscript page.
    page_long_dim  = 11 
    page_short_dim = 8.5 
    landscape_t    = (num_obs LE 2)
    
    ; Center the "plot region" on the page.
    if (landscape_t) then begin
      xsize = 10.0
      ysize = 7.5
      xoffset = (page_short_dim - ysize) / 2.
      yoffset = page_long_dim - (page_long_dim - xsize) / 2.
    endif else begin
      xsize   = 7.5
      ysize   = 10.0
      xoffset = (page_short_dim - xsize) / 2.
      yoffset = (page_long_dim  - ysize) / 2.
    endelse
    
    color_manager, /PS_PSEUDO, FILENAME=merged_stacked_lc_fn, LANDSCAPE=landscape_t, $
            XOFFSET=xoffset, YOFFSET=yoffset, XSIZE=xsize, YSIZE=ysize, /INCHES, $
            RED=red, BLUE=blue, WHITE=white
    
    !X.THICK=3
    !Y.THICK=3
    xticklen =  0.02*num_obs
    yticklen =  0.01
    charsize = 1 + (num_obs GT 2)

    ; Plot the observations in time order.
    sort_ind = sort(tstart)
    
    !P.MULTI = [0,0,num_obs]
    for kk = 0, num_obs-1 do begin
      tit    = string(obs_data[sort_ind[kk]].obsid, variability_probs[sort_ind[kk]], F='(%"%s (P!DKS!N=%0.2g)")')
      xtit   = ''
      ytit   = ''
      ytit2  = ''
      xstyle = 1+4
      ymargin= [0,2]
      
      if (kk EQ 0) then begin
        tit  = sourcename[ii] + '; ' + tit
      endif
      
      if (kk EQ num_obs-1) then begin
        xtit    = 'time (ks)'
        ytit    = 'photons/ks/cm^2'
        ytit2   = 'Median Energy (keV)'
        xstyle  = 1+8
        ymargin = [3,2]
      endif
       

      ; Plot the lightcurve axes.
      plot, [0], /NODATA, TITLE=tit, $   
            XMARGIN=[11,6],  XTIT=xtit, XSTYLE=xstyle, XRANGE=xrange,          XTICKLEN=xticklen, $
            YMARGIN=ymargin, YTIT=ytit, YSTYLE=1+8,    YRANGE=rate_axis_range, YTICKLEN=yticklen, $
            CHARSIZE=charsize, THICK=3                                                                           
      
      jj = sort_ind[kk]
      if ptr_valid(times[jj]) then begin
        ; Plot the binned/grouped lightcurve. We CANNOT USE PSYM=10 with unequal bin sizes!
        num_groups   = n_elements(*times[jj])
        nskip_binned = 1 > floor(num_groups/5.)
        zerovec      = replicate(0,num_groups)
        oploterror, *times[jj], *rates[jj], *halfbin_sizes[jj],          zerovec, COLOR=white, ERRCOLOR=white, PSYM=3, ERRTHICK=3   
        oploterror, *times[jj], *rates[jj],            zerovec, *rate_errors[jj], COLOR=white, ERRCOLOR=white, PSYM=3, ERRTHICK=3, NSKIP=nskip_binned   
                                                                                                                         
        if ptr_valid(times[num_obs+jj]) then begin
          ; Plot the smooth lightcurve
          time       = congrid(*times      [num_obs+jj], num_groups*10)
          rate       = congrid(*rates      [num_obs+jj], num_groups*10)
          rate_error = congrid(*rate_errors[num_obs+jj], num_groups*10)
          
          ; Since the smooth lightcurves are NaN at the ends (where the kernel falls off the data)
          ; the range of times left in the plot may be too small to be worth showing.
          if ( (max(time)-min(time))/time_span LT 0.10 ) then begin 
;            oploterror, mean(time,/NAN), mean(rate,/NAN), 0.025*time_span, mean(rate_error,/NAN), /NOHAT
          endif else begin
;            oplot, time, rate-rate_error
;            oplot, time, rate+rate_error
;           oploterror, time, rate, rate_error, NSKIP=1 > floor(n_elements(time)/5.), COLOR=??, ERRCOLOR=??, THICK=0.5
            oplot, time, rate, COLOR=blue, THICK=0.5
          endelse
        endif ; ptr_valid(times[jj])
        
        ; Plot the median energy time series.  We CANNOT USE PSYM=10 with unequal bin sizes!
        !Y.WINDOW = !Y.WINDOW - [0, 0.5*(!Y.WINDOW[1]-!Y.WINDOW[0])]
        axis, YAXIS=1, /SAVE, YRANGE=energy_axis_range, YTIT=ytit2, YTICKLEN=yticklen, COLOR=red, CHARSIZE=charsize
        
        oploterror, *times[jj], *median_energies[jj], *halfbin_sizes[jj],                   zerovec, COLOR=red, ERRCOLOR=red, PSYM=3, ERRTHICK=3   
        oploterror, *times[jj], *median_energies[jj],            zerovec, *median_energy_errors[jj], COLOR=red, ERRCOLOR=red, PSYM=3, ERRTHICK=3, NSKIP=nskip_binned   
      endif else begin                                 
      endelse
    endfor ;kk, looping over obsids
        
    device, /close
    !P.MULTI = 0

    ;; ------------------------------------------------------------------------
    ;; Make a single plot with a "broken" time axis showing all the light curves.
    ; Find the time span of all the lightcurves plus breaks in the time axis.
    time_span    = total(tstop-tstart) / 1000.
    break_length = time_span * 0.01
    time_span    = time_span + (break_length * (num_obs-1))
    xrange       = [-0.02*time_span, 1.02*time_span]
    
    xsize = 10.0
    ysize = 7.5
    xoffset = (page_short_dim - ysize) / 2.
    yoffset = page_long_dim - (page_long_dim - xsize) / 2.
    color_manager, /PS_PSEUDO, FILENAME=merged_sequenced_lc_fn, /LANDSCAPE, $
            XOFFSET=xoffset, YOFFSET=yoffset, XSIZE=xsize, YSIZE=ysize, /INCHES, $
            RED=red, BLUE=blue

    ; Plot the lightcurve axes.
    xstyle  = 1
    ymargin = [3,2]
    plot, [0], /NODATA, TITLE=string(sourcename[ii], float(probks), F='(%"%s (P!DKS!N=%0.2g)")'), $
          XMARGIN=[11,6],  XTIT='time (ks)',       XSTYLE=xstyle, XRANGE=xrange,          XTICKLEN=yticklen, $
          YMARGIN=ymargin, YTIT='photons/ks/cm^2', YSTYLE=1+8,    YRANGE=rate_axis_range, YTICKLEN=yticklen, $
          CHARSIZE=1, THICK=3 
    
    num_groups = 0L
    for kk = 0, num_obs-1 do begin
      jj = kk
      if ptr_valid(times[jj]) then num_groups = num_groups + n_elements(*times[jj])
    endfor ;kk, looping over obsids
    nskip_binned = 1 > floor(num_groups/5.)
    
;    num_times = 0L
;    for kk = 0, num_obs-1 do begin
;      jj = num_obs+kk
;      if ptr_valid(times[jj]) then num_times = num_times + n_elements(*times[jj])
;    endfor ;kk, looping over obsids
;    nskip_smoothed = 1 > floor(num_times/5.)

    ; Plot the binned and smoothed light curves.
    time_offset = 0.
    for kk = 0, num_obs-1 do begin
      jj = sort_ind[kk]
      if ptr_valid(times[jj]) then begin
        ; Plot the binned/grouped lightcurve. We CANNOT USE PSYM=10 with unequal bin sizes!
        num_groups = n_elements(*times[jj])
        zerovec    = replicate(0,num_groups)
        oploterror, *times[jj] + time_offset, *rates[jj], *halfbin_sizes[jj],          zerovec, COLOR=white, ERRCOLOR=white, PSYM=3, ERRTHICK=3   
        oploterror, *times[jj] + time_offset, *rates[jj],            zerovec, *rate_errors[jj], COLOR=white, ERRCOLOR=white, PSYM=3, ERRTHICK=3, NSKIP=nskip_binned   
      
        if ptr_valid(times[num_obs+jj]) then begin
          ; Plot the smooth lightcurve
          time       = congrid(*times      [num_obs+jj] + time_offset, num_groups*10)
          rate       = congrid(*rates      [num_obs+jj]              , num_groups*10)
          rate_error = congrid(*rate_errors[num_obs+jj]              , num_groups*10)
           
          ; Since the smooth lightcurves are NaN at the ends (where the kernel falls off the data)
          ; the range of times left in the plot may be too small to be worth showing.
          if ( (max(time)-min(time))/time_span LT 0.05 ) then begin
          endif else begin
            oplot, time, rate, COLOR=blue, THICK=0.5
          endelse
        endif
      endif else begin
      endelse

      time_offset = time_offset + ((tstop[sort_ind[kk]] - tstart[sort_ind[kk]]) / 1000.)

      if (kk LT (num_obs-1)) then begin
        ; Make a graphic indicating a break in the time axis.
        oplot, [time_offset,time_offset], rate_axis_range, LINE=1
        time_offset = time_offset + break_length
        oplot, [time_offset,time_offset], rate_axis_range, LINE=1
      endif
    endfor ;kk, looping over obsids
    
    
    ; Plot the median energy time series.
    !Y.WINDOW = !Y.WINDOW - [0, 0.5*(!Y.WINDOW[1]-!Y.WINDOW[0])]
    axis, YAXIS=1, /SAVE, YRANGE=energy_axis_range, YTIT='Median Energy (keV)', YTICKLEN=yticklen, COLOR=red, CHARSIZE=1
    time_offset = 0.
    for kk = 0, num_obs-1 do begin
      jj = sort_ind[kk]
      if ptr_valid(times[jj]) then begin
        ; Plot the median energy time series.  We CANNOT USE PSYM=10 with unequal bin sizes!
        num_groups = n_elements(*times[jj])
        zerovec    = replicate(0,num_groups)
        oploterror, *times[jj] + time_offset, *median_energies[jj], *halfbin_sizes[jj],                   zerovec, COLOR=red, ERRCOLOR=red, PSYM=3, ERRTHICK=3   
        oploterror, *times[jj] + time_offset, *median_energies[jj],            zerovec, *median_energy_errors[jj], COLOR=red, ERRCOLOR=red, PSYM=3, ERRTHICK=3, NSKIP=nskip_binned   
      endif
      time_offset = time_offset + ((tstop[sort_ind[kk]] - tstart[sort_ind[kk]]) / 1000.) + break_length
    endfor ;kk, looping over obsids

    device, /close
    color_manager, /X_PSEUDO
    print, merged_stacked_lc_basename, merged_sequenced_lc_fn, F='(%"Binned light curve, smooth light curve, & median energy timeseries plotted in %s, %s")'

LC_CLEANUP:
    ptr_free, times, halfbin_sizes, rates, rate_errors, median_energies, median_energy_errors    
    

MERGE_SAVE_STATS:    
    fxaddpar, src_stats, 'CREATOR',  creator_string
    writefits, src_stats_fn, 0, src_stats
    continue
    
MERGE_IS_EMPTY:
    file_delete, /ALLOW_NONEXISTENT, photometry_fn
    file_delete, /ALLOW_NONEXISTENT, composite_psf_fn
    file_delete, /ALLOW_NONEXISTENT, merged_env_events_fn, merged_src_events_fn, merged_region_fn
    file_delete, /ALLOW_NONEXISTENT, merged_src_spectrum_fn, merged_bkg_spectrum_fn, merged_rmf_fn, merged_arf_fn, photometry_fn 
    file_delete, /ALLOW_NONEXISTENT, merged_stacked_lc_fn, merged_sequenced_lc_fn
    sxdelpar, src_stats, ['MERGPRUN', 'BANDPRUN', 'PRIM_OBS', 'EMAP_TOT', 'FRACEXPO', 'WARNFRAC', 'OBSNAME', 'SRC_CNTS', 'THETA', 'THETA_LO', 'THETA_HI', 'SRC_AREA', 'SRC_RAD', 'PSF_FRAC', 'MSK_RAD', 'FRACSPEC', 'OVRLP_LM', 'OVRLP_LO', 'OVRLP_HI', 'BESTOBS', 'WORSTOBS', 'RA_DATA', 'DEC_DATA', 'BACKGRND', 'SCAL_MAX', 'SCAL_MIN', 'KS_SPECT', 'EXPOSURE', 'EFFAREA', 'PROB_KS', 'MERGE_KS', 'ERR_DATA' ]
    
    fxaddpar, src_stats, 'MERGFRAC', 0, 'fraction of extracted data merged'
    fxaddpar, src_stats, 'MERGNUM',  0, 'number of observations merged'
    GOTO, MERGE_SAVE_STATS

  endfor ;ii, looping over sources

  count = total(source_not_observed, /INT)
  if ~keyword_set(obsname) && (count GT 0) then print, count, F="(%'\nWARNING!  %d sources were not extracted from any observation')"

;save, debug_pb, FILE='debug_pb.sav'

endif ;keyword_set(merge_observations)



;; =============================================================================
if keyword_set(check_positions) then begin
;; =============================================================================



  ;; Set up the Postscript page using lengths in inches for 12 images per page.
  page_long_dim  = 11 
  page_short_dim = 8.5 
  
  image_size = 2.16
  gap_size   = 0.25
  window_xsize = 4*image_size + 3*gap_size
  window_ysize = 3*image_size + 3*gap_size
      
  window_xoffset = (page_short_dim - window_ysize) / 2.
  window_yoffset = page_long_dim - (page_long_dim - window_xsize) / 2.
      
  color_manager, /PS_GREY, FILENAME='mugshots.ps', /LANDSCAPE, /INCHES,$
                 XOFFSET=window_xoffset, YOFFSET=window_yoffset, $
                 XSIZE=window_xsize,     YSIZE=window_ysize
                                                       
  temp = convert_coord( !D.X_CH_SIZE, !D.Y_CH_SIZE, /DEV, /TO_NORM)
  x_ch_size = temp[0]
  y_ch_size = temp[1]


  for ii = 0, num_sources-1 do begin
    print, F='(%"\n===================================================================")'
 
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii]
 
    merged_env_events_fn  = sourcedir + env_events_basename
    merged_src_events_fn  = sourcedir + src_events_basename
    merged_region_fn      = sourcedir + src_region_basename
    composite_psf_fn      = sourcedir + psf_basename
    composite_img_fn      = sourcedir + env_image_basename
    temp_composite_img_fn = tempdir + env_image_basename
    event_reg_fn          = sourcedir + evt_region_basename

    unnamed_src_stats_fn   = sourcename[ii] + '/' + src_stats_basename
    src_stats_fn           = sourcedir            + src_stats_basename

    ;; ------------------------------------------------------------------------
    ;; Read the fundamental source properties from the unnamed source.stats file.
    unnamed_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      src_label = strtrim(sxpar(unnamed_stats,'LABEL'),2)
      ra  = sxpar(unnamed_stats, 'RA')
      dec = sxpar(unnamed_stats, 'DEC')
    endif else message, 'ERROR reading '+unnamed_src_stats_fn

    ;; ------------------------------------------------------------------------
    ;; Read stats about merged observations of source.
    src_stats    = headfits(src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      ra_data     = sxpar(src_stats, 'RA_DATA')
      dec_data    = sxpar(src_stats, 'DEC_DATA')
      off_angle   = sxpar(src_stats, 'THETA')
      src_radius  = sxpar(src_stats, 'SRC_RAD')
      mask_radius = sxpar(src_stats, 'MSK_RAD')
    endif else message, 'ERROR reading '+src_stats_fn
  
    if (off_angle LT theta_range[0]) OR (off_angle GT theta_range[1]) then begin
      print, sourcename[ii], src_label, F='(%"Skipping source not in THETA_RANGE: %s (%s)")'      
      continue
    endif else $
      print, sourcename[ii], src_label, F='(%"Source: %s (%s)")'      



    ;; ------------------------------------------------------------------------
    if (NOT (file_test(merged_env_events_fn) AND file_test(composite_psf_fn))) then begin
      print, 'WARNING!  Not present in any observation'
      file_delete, /ALLOW_NONEXISTENT, composite_img_fn, event_reg_fn
      continue
    endif
    
    ; Remove any temp files from the previous source. 
    list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
    if (count GT 0) then file_delete, list
    
    run_command, /QUIET, 'pset dmcopy clobber=yes'
    
    ;; Read merged neighborhood event list and extract astrometry.
    bt = mrdfits(merged_env_events_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + merged_env_events_fn
    
    xpos_events = bt.x  &  ypos_events = bt.y
    
    fxbfind, theader, 'TTYPE', dum1, TTYPE, dum2, 'null'
    fxbfind, theader, 'TCTYP', dum1, TCTYP, dum2, 'null'
    fxbfind, theader, 'TCRVL', dum1, TCRVL, dum2, 0.0D
    fxbfind, theader, 'TCRPX', dum1, TCRPX, dum2, 0.0D
    fxbfind, theader, 'TCDLT', dum1, TCDLT, dum2, 0.0D
    colnames = strlowcase( strtrim(TTYPE,2) )
    x_ind    = where(strlowcase(colnames) EQ 'x')
    y_ind    = where(strlowcase(colnames) EQ 'y')
    make_astr, event2wcs_astr, DELTA=TCDLT[[x_ind,y_ind]], CTYPE=TCTYP[[x_ind,y_ind]], $
                               CRPIX=TCRPX[[x_ind,y_ind]], CRVAL=TCRVL[[x_ind,y_ind]]

    ;; ------------------------------------------------------------------------
    ;; Convert in-band neighborhood event list to RA/DEC & make a "point" region for each event, useful
    ;; for marking the events on a smoothed or reconstructed image.
    ;; REMEMBER THAT THE xy2ad and ad2xy programs assume that (x,y) are 
    ;; ZERO-BASED pixel indexes.  Thus we must subtract 1 from the sky (x,y) 
    ;; positions when converting to RA,DEC.
    ind = where((1000*energy_range[0] LE bt.energy) AND (bt.energy LE 1000*energy_range[1]), count)
    if (count GT 0) then begin
      xy2ad, xpos_events[ind]-1, ypos_events[ind]-1, event2wcs_astr, ra_evt, dec_evt
      
      openw,  region2_unit, event_reg_fn, /GET_LUN
      printf, region2_unit, "# Region file format: DS9 version 3.0"
      !TEXTUNIT = region2_unit
      forprint, TEXTOUT=5, ra_evt, dec_evt, F='("J2000;cross point ",F10.6,1x,F10.6," #")', /NoCOMMENT
      free_lun, region2_unit
    endif

    ;; ------------------------------------------------------------------------
    ;; Construct composite neighborhood image ON THE SAME BIN GRID PHASE (not range) 
    ;; used in the composite PSF image, and with the source position at the center.
    
    ; Read composite PSF.
    ; Extract conversion from PSF image index coordinates to RA/DEC (WCS) coordinates.
    ; Find the 0-based index coordinates of the PSF pixel EDGE nearest the source position.
    ; Centers of pixels are x.0, edges are x.5 in index coordinates.
    composite_psf = readfits(composite_psf_fn, composite_psf_hdr, /SILENT)
    extast, composite_psf_hdr, psf2wcs_astr
    
    ad2xy, ra, dec, psf2wcs_astr, src_column, src_row
    edge_near_src_x = round(src_column+0.5) - 0.5
    edge_near_src_y = round(src_row   +0.5) - 0.5
    
    ; Perform linear conversion from PSF pixel index coordinates to sky (x,y) coordinates.
    ; We cannot use xy2ad.pro for such conversions to PHYSICAL (sky) system.
    crvalP = [sxpar(composite_psf_hdr, 'CRVAL1P'), sxpar(composite_psf_hdr, 'CRVAL2P')]
    crpixP = [sxpar(composite_psf_hdr, 'CRPIX1P'), sxpar(composite_psf_hdr, 'CRPIX2P')]
    cdeltP = [sxpar(composite_psf_hdr, 'CDELT1P'), sxpar(composite_psf_hdr, 'CDELT2P')]
    ctype  = [sxpar(composite_psf_hdr, 'CTYPE1P'), sxpar(composite_psf_hdr, 'CTYPE2P')]

    center_sky_x = crvalP[0] + cdeltP[0] * (edge_near_src_x+1 - crpixP[0])
    center_sky_y = crvalP[1] + cdeltP[1] * (edge_near_src_y+1 - crpixP[1])
    
    ; Find the half-dimensions from the center (center_sky_x, center_sky_y) of the 
    ; data image that will be required to contain all the events.
    min_radius = ((1.1 * mask_radius) > 5/cdeltP[0])
    x_halfdim = ceil( max(abs( (center_sky_x - xpos_events)/cdeltP[0] )) > min_radius )
    y_halfdim = ceil( max(abs( (center_sky_y - ypos_events)/cdeltP[1] )) > min_radius )
    
    ; And make the image, which should have dimensions that are EVEN.
    ; We write to the temp_dir to reduce NFS traffic.
    cmd = string( merged_env_events_fn, 1000*energy_range, $
                  center_sky_x - x_halfdim*cdeltP[0], $
                  center_sky_x + x_halfdim*cdeltP[0], 2*x_halfdim, $
                  center_sky_y - y_halfdim*cdeltP[1], $
                  center_sky_y + y_halfdim*cdeltP[1], 2*y_halfdim, $
                  temp_composite_img_fn, $
    F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin x=%9.4f:%9.4f:#%d,y=%9.4f:%9.4f:#%d]"" %s')")
    run_command, cmd

    composite_img = readfits(temp_composite_img_fn, composite_img_hdr, /SILENT)
    
    ; We must overwrite composite data image file to get rid of GTI tables that dmcopy keeps around
    ; so that we can put the reconstructed image in the second HDU.
    ; There seems to be no CIAO option to suppress GTI tables.
    writefits, temp_composite_img_fn, composite_img, composite_img_hdr
    extast, composite_img_hdr, compimg2wcs_astr 

    
    ; Defensively check that composite neighborhood image and PSF binnings match!
    pixsize_mismatch = (compimg2wcs_astr.CDELT - psf2wcs_astr.CDELT) / compimg2wcs_astr.CDELT
    if (max(pixsize_mismatch) GT 0.01) then begin
      help, compimg2wcs_astr, /ST
      help, psf2wcs_astr,     /ST
      message, 'ERROR: data image and PSF binning do not match!'
    endif

   
    
    ;; ------------------------------------------------------------------------
    ;; Correlate PSF & data images to refine position estimate.
    ;; The range over which we search is arbitrarily set to SRC_RAD/2.  
    ;; In /CONSTRUCT_REGIONS, SRC_RAD is determined from the polygon, and later in 
    ;; /EXTRACT_EVENTS it is recomputed.

    ; Convert src_radius from sky pixels to PSF image pixels.
    search_radius = 1 > ceil((src_radius / 2.0) / sxpar(composite_psf_hdr, 'CDELT1P')) 

    ra_corr = !VALUES.F_NAN  &  dec_corr = !VALUES.F_NAN  &  quantization_corr = !VALUES.F_NAN  
    if keyword_set(skip_correlation) then $
      GOTO, END_OF_CORRELATION
    
    ; Compute size of 1 PSF pixel in arcseconds.
    quantization_corr = psf2wcs_astr.CDELT[1] * 3600

    print, search_radius * quantization_corr, F='(%"Correlating PSF & data over +- %0.1f arcseconds ...")'

    ; Find offset between two images.
    xy2ad, 0, 0, psf2wcs_astr, ra_corner, dec_corner
    ad2xy, ra_corner, dec_corner, compimg2wcs_astr, xoffset_b, yoffset_b

    xoffset_b = round(xoffset_b)
    yoffset_b = round(yoffset_b)
    
    corrmat = correl_images( double(composite_img), composite_psf, XOFFSET_B=xoffset_b, YOFFSET_B=yoffset_b, $
                             XSHIFT=search_radius, YSHIFT=search_radius )
    corrmat_analyze, corrmat, xoffset_optimum, yoffset_optimum, XOFF_INIT=xoffset_b, YOFF_INIT=yoffset_b
    
    if ((abs(xoffset_optimum) > abs(yoffset_optimum)) EQ search_radius) then begin
      print, 'WARNING!  Correlation postion cannot be computed; peak lies on search boundary.'
    endif else begin
      col_offset = xoffset_optimum - xoffset_b
      row_offset = yoffset_optimum - yoffset_b
      
      print, col_offset, row_offset, [col_offset,row_offset] * psf2wcs_astr.CDELT * 3600, $
             F='(%"  X,Y offset of correlation peak: %d, %d (pixels); %0.2f, %0.2f (arcsec)")'
          
      ;; Convert catalog position (RA,DEC) into PSF pixel indexes (0-based).
      ad2xy, ra, dec, psf2wcs_astr, column, row
      
      ;; Add optimum pixel offsets & convert back to RA,DEC.
      xy2ad, column+col_offset, row+row_offset, psf2wcs_astr, ra_corr, dec_corr
    endelse

END_OF_CORRELATION:

    ;; ------------------------------------------------------------------------
    ;; The composite image, and its reconstruction, are intended to be sized such that 
    ;; they are suitable for printing to mugshots.ps below, or inclusion in an "atlas"
    ;; page for the source.  Also, these reconstructions are intended to be quick &
    ;; dirty glimpses into whether there are missed point sources, or extended sources.
    ;; For both reasons, composite images with large dimensions are not desirable so we
    ;; will rebin as much as possible.
    
    rebin_factor = 1
    radius50 = sxpar(composite_psf_hdr, 'RADIUS50')
    if NOT finite(radius50) then begin
      print, 'Keyword RADIUS50 not found; using on-axis value of 0.85 skypix.'
      radius50 = 0.85 * arcsec_per_skypixel
    endif
    
    ; RADIUS50 is radius (arcsec) enclosing 50% PSF
    ; Let's rebin so that this radius is equal to at least 2 pixels.
    arcsec_per_psfpixel = psf2wcs_astr.CDELT[1] * 3600
    
    num_across          = radius50 / (arcsec_per_psfpixel)
    
    rebin_factor        = 1 > floor(num_across/2.)
    
    
    if (rebin_factor GT 1) then begin
      ; Rebin data & PSF images.  
      print, rebin_factor*arcsec_per_psfpixel, F='(%"\nRebinning composite data and composite PSF images to %4.1f arcsec/pixel.")'
      
      search_radius = !VALUES.F_NAN

      ; The rebinned data image is resaved.  
      ; We choose to crop the data image to avoid incomplete pixels in the rebinned image.
      proposed_xdim = floor((size(composite_img, /DIM))[0] / rebin_factor)
      proposed_ydim = floor((size(composite_img, /DIM))[1] / rebin_factor) 

      file_move, /OVERWRITE, temp_composite_img_fn, temp_image_fn
      cmd = string(temp_image_fn, proposed_xdim*rebin_factor, rebin_factor, proposed_ydim*rebin_factor, rebin_factor, temp_composite_img_fn, F="(%'dmcopy ""%s[bin #1=1:%d:%d,#2=1:%d:%d]"" %s')")
      run_command, cmd      
      composite_img = readfits(temp_composite_img_fn, composite_img_hdr, /SILENT)
      
      ; The rebinned PSF image is NOT saved.
      ; We choose to let the upper and right edges of the result be incomplete rather than to crop the PSF.
      cmd = string(composite_psf_fn, rebin_factor, rebin_factor, temp_image_fn, F="(%'dmcopy ""%s[bin #1=::%d,#2=::%d]"" %s')")
      run_command, cmd      
      composite_psf = readfits(temp_image_fn, composite_psf_hdr, /SILENT)
      
      ; Update the FITS astrometry structures.
      extast, composite_img_hdr, compimg2wcs_astr 
      extast, composite_psf_hdr, psf2wcs_astr
      arcsec_per_psfpixel = psf2wcs_astr.CDELT[1] * 3600
   endif 
        
    ; Defensively check that composite neighborhood image and PSF binnings match!
    pixsize_mismatch = (compimg2wcs_astr.CDELT - psf2wcs_astr.CDELT) / compimg2wcs_astr.CDELT
    if (max(pixsize_mismatch) GT 0.01) then begin
      help, compimg2wcs_astr, /ST
      help, psf2wcs_astr,     /ST
      message, 'ERROR: data image and PSF binning do not match!'
    endif

    
;    ;; ------------------------------------------------------------------------
;    ;; Estimate the background level of the image and save to a header keyword.
;    ;; We coarsely rebin the image to push most of the PSF into ~1 pixel, but we
;    ;; ensure that the rebinned image retains a reasonable number (100) of pixels.
;    num_across          = radius50 / (arcsec_per_psfpixel)
;    
;    for rebin_factor= (1 > ceil(num_across * 4.)), 2, -1 do begin
;      proposed_xdim = floor((size(composite_img, /DIM))[0] / rebin_factor)
;      proposed_ydim = floor((size(composite_img, /DIM))[1] / rebin_factor) 
;      if (proposed_xdim*proposed_ydim GE 100) then break
;    endfor
;    
;    print, rebin_factor*arcsec_per_psfpixel, F='(%"\nEstimating background in composite data image rebinned to %4.1f arcsec/pixel.")'
;    
;     ; We choose to crop the data image to avoid incomplete pixels in the rebinned image.
;    cmd = string(temp_composite_img_fn, proposed_xdim*rebin_factor,rebin_factor, proposed_ydim*rebin_factor,rebin_factor, temp_image_fn, F="(%'dmcopy ""%s[bin #1=1:%d:%d,#2=1:%d:%d]"" %s')")
;    run_command, cmd      
;    bigpixel_image = readfits(temp_image_fn, bigpixel_hdr, /SILENT)
;    help, bigpixel_image
;    
;    ; Find the pixel size.
;    extast, bigpixel_hdr, bigpixel_astr 
;    arcsec_per_bigpixel = bigpixel_astr.CDELT[1] * 3600
;    
;    estimate_poisson_background, bigpixel_image, bkg_estimate, SIGNIFICANCE=0.99, /VERBOSE
;
;    cmd = string(temp_composite_img_fn, bkg_estimate * (arcsec_per_skypixel / arcsec_per_bigpixel)^2, $
;                 F="(%'dmhedit infile=""%s[1]"" filelist=none operation=add key=BACKGRND value=""%f"" comment=""background (events/skypixel^2), total-band, merged observations""')")
;
;    run_command, cmd
; 
    
    ; Finally, now that we've finished the heavy I/O involving the composite image, we can move it from 
    ; the temp_dir to the source directory.
    file_move, /OVERWRITE, temp_composite_img_fn, composite_img_fn
    
    ;; ------------------------------------------------------------------------
    if keyword_set(skip_reconstruction) then begin
      ra_ml = !VALUES.F_NAN  &  dec_ml = !VALUES.F_NAN  
      GOTO, END_OF_RECONSTRUCTION
    endif
    
    ;; The correlations performed in Max_Likelihood.pro have the property that the
    ;; position of the point source in the PSF image is assumed to be:
    ;; a) the center of the central pixel if the PSF dimension is odd
    ;; b) the center of the pixel N/2 if the PSF dimension is even
    ;; Conveniently the formula fix(N/2) produces the correct value in both cases.
    ;;
    ;; One way to think about what I mean by the above is demonstrated by reconstructing
    ;; the PSF image itself (as demonstrated by the test program test_maxlik.pro).
    ;; When the PSF dimension is odd the resulting delta function in the output falls
    ;; at the central pixel.
    ;; When the PSF dimension is even the resulting delta function in the output falls
    ;; at pixel N/2 (which is just past the "4-corners middle" of the image).
    ;;
    ;; The CIAO PSF images do not follow that convention however.  (They seem to 
    ;; usually have even dimensions, and the point source is on or near the
    ;; "4-corners middle" of the image.)
    ;;
    ;; Thus, to assign meaningful astrometry to the reconstructed image we just account
    ;; for the offset between the Max_Likelihood convention and the actual source position
    ;; in the PSF image.
    
    ; Find PSF pixel index coordinates of source position.
    ad2xy, ra, dec, psf2wcs_astr, src_column, src_row
    
    psf_xdim = sxpar(composite_psf_hdr, 'NAXIS1')
    psf_ydim = sxpar(composite_psf_hdr, 'NAXIS2')

    xoffset = src_column - fix(psf_xdim/2)
    yoffset = src_row    - fix(psf_ydim/2)

    ; Although the original MARX PSF was constructed with the source position at the center of the center pixel, in this rebinned PSF that will not be true.  I've hidden the message below because it is alarming, but provides no information useful to the observer.
    ; print, [xoffset, yoffset] * arcsec_per_psfpixel, F='(%"\nReconstruction astrometry has been adjusted for the (%0.2f, %0.2f) arcsec offset between the center of the PSF image and the source position.")'

    ; Build a header for the reconstructed image.  WCS is taken from data image,
    ; and adjusted by offsets above.  I don't copy & adjust the PHYSICAL (sky)
    ; coordinate system because I can't figure out how LTV* & LTM* work.
    mkhdr, maxlik_hdr, composite_img, /IMAGE

    maxlik2wcs_astr   = compimg2wcs_astr
    maxlik2wcs_astr.CRPIX = maxlik2wcs_astr.CRPIX - [xoffset,yoffset]
    putast, maxlik_hdr, maxlik2wcs_astr, CD_TYPE=1  ;CD_TYPE=1 ensures CDELT keywords carry plate scale   
    
    fxaddpar, maxlik_hdr, 'HDUNAME', 'Max_Likelihood'
    fxaddpar, maxlik_hdr, 'CREATOR', creator_string
    

    ;; Perform reconstruction of composite image.
    ;  The convolutions done in the recon wrap around the image edges.  To avoid artifacts we
    ;  pad the data image on the right and upper sides with a border equal in size to the PSF.
    ;
    ;  !!!!  
    ;  For reasons I cannot fathom, flux is NOT CONSERVED in Max_Likelihood.pro if the data image has odd dimensions.
    ;  !!!!  
    ;    
    ;  Make composite_img & normalized_psf single precision to help speed in Max_Likelihood
    ;  on 32-bit processors.  On 64-bit processor it seems to make no difference.
    img_xdim = (size(composite_img, /DIM))[0]
    img_ydim = (size(composite_img, /DIM))[1]
    
    padded_xdim = img_xdim + psf_xdim
    padded_ydim = img_ydim + psf_ydim
    
    padded_xdim += (padded_xdim MOD 2)
    padded_ydim += (padded_ydim MOD 2)   
    
    padded_img = fltarr(padded_xdim, padded_ydim)
    padded_img[0,0] = composite_img
      
    normalized_psf = float(composite_psf / total(composite_psf, /DOUBLE))
    
    print, max(maxlikelihood_iterations), F='(%"reconstructing with %d ML iterations ...")'
    help, composite_img, padded_img, normalized_psf
    psf_ft     = 0
    maxlik_img = 0
    for mm=1, max(maxlikelihood_iterations) do begin
      ; If you use the /NO_FT option it runs faster but the result has a border of zeros.
      Max_Likelihood, padded_img, normalized_psf, maxlik_img, FT_PSF=psf_ft
      
      ; Save reconstruction in extension of composite_img_fn.
      if (total(mm EQ maxlikelihood_iterations) GT 0) then begin
        fxaddpar, maxlik_hdr, 'ML_ITERS', mm
        mwrfits, maxlik_img[0:img_xdim-1, 0:img_ydim-1], composite_img_fn, maxlik_hdr
      endif
    endfor

    ; Remove padding from recon image.
    maxlik_img = maxlik_img[0:img_xdim-1, 0:img_ydim-1]
    help, maxlik_img
    print
    
    ; Verify that global flux has been preserved, since we've had trouble with this before!
    data_sum  = total(composite_img)
    recon_sum = total(maxlik_img)
    if (data_sum GT 0) && (abs(data_sum-recon_sum)/data_sum GT 0.10) then begin
      print, 'WARNING! Global flux in the data and recon images differs by >10%.'
      help, data_sum, recon_sum
    endif 
    
    ;; ------------------------------------------------------------------------
    ;; Print composite data image & reconstructed image, 6 sources to a page.
    if (ii mod 6 EQ 0) then erase  ; Add a page break.
    
    xoffset = 2*(ii mod 2)*(image_size+gap_size)
    yoffset = (ii/2 mod 3)*(image_size+gap_size) + gap_size
    
    tv, 255 - bytscl(composite_img), /INCHES, xoffset,  yoffset, XSIZE=image_size, YSIZE=image_size
    xoffset2 = xoffset+image_size+gap_size
    tv, 255 - bytscl(maxlik_img),    /INCHES, xoffset2, yoffset, XSIZE=image_size, YSIZE=image_size
   
    xyouts, (xoffset+image_size+gap_size/2)/window_xsize, $
             yoffset/window_ysize - y_ch_size, sourcename[ii], /NORMAL, ALIGN=0.5

    plots, (xoffset + [0,0,2*image_size+gap_size,2*image_size+gap_size,0])/window_xsize, $
           (yoffset + [0,image_size,image_size,0,0])/window_ysize, /NORMAL
 
    ;; ------------------------------------------------------------------------
    ;; Try to identify the position of the reconstruction peak in order to obtain
    ;; an independent estimate of the source's position.
    
    ;; The range over which we search is arbitrarily set to SRC_RAD/2.  
    ;; In /CONSTRUCT_REGIONS, SRC_RAD is determined from the polygon, and later in 
    ;; /EXTRACT_EVENTS it is recomputed.

    ; Convert src_radius from sky pixels to recon image pixels.
    search_radius = 1 > ((src_radius / 2.0) / sxpar(composite_psf_hdr, 'CDELT1P')) 

    ; Compute size of 1 PSF pixel in arcseconds.
    quantization_ml = maxlik2wcs_astr.CDELT[1] * 3600

;   print, off_angle, F='(%"Mean off-axis angle: %4.1f arcmin")'
    print, search_radius * quantization_ml, F='(%"Finding reconstruction peak within +- %0.1f arcseconds of catalog position ...")'
    
    ; Convert catalog position to array index coordinates.
    ad2xy, ra, dec, maxlik2wcs_astr, src_column, src_row
    
    ; Extract a sub-image of the appropriate size around the catalog source position.
    search_minx = floor(src_column-search_radius) > 0
    search_miny = floor(src_row   -search_radius) > 0
    search_maxx =  ceil(src_column+search_radius) < ((size(maxlik_img, /DIM))[0]-1)
    search_maxy =  ceil(src_row   +search_radius) < ((size(maxlik_img, /DIM))[1]-1)
    
    search_img = maxlik_img[search_minx:search_maxx, search_miny:search_maxy]
    
    ; Find the array index coordinates of the largest pixel in the search region.
    dum  = max(search_img, imax)
    index_to_point, imax, xindex, yindex, size(search_img)
    xindex += search_minx
    yindex += search_miny
    
    ; If that pixel is a local maximum, then compute the centroid of the 3x3 neighborhood.
    ll_neighbor_ind = [0,1,2,3]
    ur_neighbor_ind = [5,6,7,8]
    central_ind     = 4
    centroid_island = maxlik_img[xindex-1:xindex+1, yindex-1:yindex+1]

    local_max_found = (centroid_island[central_ind] GE max(centroid_island[ll_neighbor_ind])) AND $
                      (centroid_island[central_ind] GT max(centroid_island[ur_neighbor_ind]))
    
    if local_max_found then begin
      offset = [-1,0,1]
      
      make_2d, offset, offset, x_offset, y_offset
      
      xy2ad, xindex + total(centroid_island*x_offset) / total(centroid_island), $
             yindex + total(centroid_island*y_offset) / total(centroid_island), maxlik2wcs_astr, ra_ml, dec_ml
    endif else begin
      print, 'WARNING!  Brightest recon pixel near source is not a local maximum; ML position estimate skipped.'
      ra_ml = !VALUES.F_NAN  &  dec_ml = !VALUES.F_NAN
    endelse

END_OF_RECONSTRUCTION:

    ;; ------------------------------------------------------------------------
    ;; Append markers showing all three positions for each source to merged extraction region file.
    openw,  region1_unit, merged_region_fn, /GET_LUN, /APPEND

    printf, region1_unit, F='(%"# Catalog (crosses), Data Mean (diamonds), Reconstruction Peak (boxes), and Correlation Peak (circles) positions")'
  
    color = 'green'

    printf,   region1_unit, ra,      dec,      color, F='(%"J2000;cross   point %10.6f %10.6f # tag={cat}  color=%s")'
    printf,   region1_unit, ra_data, dec_data, color, F='(%"J2000;diamond point %10.6f %10.6f # tag={data} color=%s")'
    if finite(ra_corr) AND finite(dec_corr) then $
      printf, region1_unit, ra_corr, dec_corr, color, F='(%"J2000;circle  point %10.6f %10.6f # tag={corr} color=%s")'
    if finite(ra_ml) AND finite(dec_ml) then $
      printf, region1_unit, ra_ml,   dec_ml,   color, F='(%"J2000;box     point %10.6f %10.6f # tag={ml}   color=%s")'

    free_lun, region1_unit
    
    if NOT keyword_set(skip_reconstruction) then begin
      prefix   = '"'+env_image_basename+'['
      suffix   = ']" -regionfile '+src_region_basename
      hdu_args = strjoin(prefix+strtrim(indgen(1+n_elements(maxlikelihood_iterations)),2)+suffix, ' ')
      print, sourcedir, hdu_args, F="(%'\nTo display reconstructed image run:\n  (cd %s; ds9 -log %s) &')"  
    endif
        

    ;; ------------------------------------------------------------------------
    ;; Save information to source stats file.    
    fxaddpar, src_stats, 'CREATOR',  creator_string

    fxaddpar, src_stats, 'RA_ML',    ra_ml,  'source position, reconstruction peak', F='(F10.6)'
    fxaddpar, src_stats, 'DEC_ML',   dec_ml, 'source position, reconstruction peak', F='(F10.6)'
    fxaddpar, src_stats, 'QUANTCOR', quantization_corr, 'quantization of CORR position (arcsec)', F='(F7.4)'
    fxaddpar, src_stats, 'RA_CORR',  ra_corr,  'source position, correlation peak', F='(F10.6)'
    fxaddpar, src_stats, 'DEC_CORR', dec_corr, 'source position, correlation peak', F='(F10.6)'

    writefits, src_stats_fn, 0, src_stats
  endfor ; loop over sources 
    
  device, /close
  color_manager, /X_PSEUDO

  print, '============================================================================='
  print, 'Wrote composite source images to mugshots.ps'
  print, 'Catalog (crosses), Data Mean (diamonds), Reconstruction Peak (boxes), and Correlation Peak (circles) positions written to {sourcename}/{extraction_name}/' + src_region_basename
  print, '============================================================================='
endif ;keyword_set(check_positions)



;; =============================================================================
if keyword_set(fit_spectra) then begin
;; =============================================================================


  result = routine_info( 'acis_extract', /SOURCE )
  fdecomp, result.PATH, disk, codedir
  
  if NOT keyword_set(model_filename) then begin
    print, 'ERROR:Parameter MODEL_FILENAME not supplied!'
    GOTO, FAILURE
  endif
  
  if     keyword_set(min_num_cts) then begin
    print, 'ERROR: keyword MIN_NUM_CTS no longer used by /FIT_SPECTRA; see the manual.'
    GOTO, FAILURE
  endif

  ; SNR_RANGE[1] is the user's goal for defining groups; SNR_RANGE[0] is the lower limit allowed before we abort the grouping attempt
  if (n_elements(snr_range) EQ 0) then $
    snr_range = (keyword_set(cstat)) ? [1,3] : [1,3]
  if (n_elements(snr_range) NE 2) then begin
    print, 'ERROR: keyword SNR_RANGE should be a 2-element vector giving the range of SNR allowed for each spectral group, e.g. [2.5,5].'
    GOTO, FAILURE      
  endif
  
  if (snr_range[1] LT 0) then begin
    print, 'ERROR: minimum SNR value (SNR_RANGE[1]) must be positive'
    GOTO, FAILURE
  endif
  
  ; Default minimum number of noticed groups is 3 for display in CSTAT fit, and 8 for fitting with chi^2.
  if (n_elements(num_groups_range) EQ 0) then $
    num_groups_range = (keyword_set(cstat)) ? [2+3,250] : [2+8,250]
  if (n_elements(num_groups_range) NE 2) then begin
    print, 'ERROR: keyword NUM_GROUPS_RANGE should be a 2-element vector specifying how many spectral groups are desired, e.g. [10,250].'
    GOTO, FAILURE      
  endif
 
  if ~keyword_set(fit_timeout) then fit_timeout = 600
  
  
;; Loop until all sources have been processed.
processed_flag        = replicate(0B,num_sources)
noprogress_pass_count = 0
num_to_process_prev   = num_sources+1

repeat begin
source_index = where(processed_flag EQ 0, num_to_process)

; When an historic lock file exists we must take steps to prevent infinite looping.
if (num_to_process GE num_to_process_prev) then begin
  noprogress_pass_count++
  print, 'Waiting 5 minutes ...'
  wait, 5*60
endif
num_to_process_prev = num_to_process

  for kk = 0, num_to_process-1 do begin
    ii = source_index[kk]
    processed_flag[ii] = 1
    
    print, F='(%"\n===================================================================")'
    
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii]
    photometry_fn          = sourcedir + src_photometry_basename
    merged_src_spectrum_fn = sourcedir + sourcename[ii] + '.pi'
    merged_bkg_spectrum_fn = sourcedir + sourcename[ii] + '_bkg.pi'
    merged_arf_fn          = sourcedir + sourcename[ii] + '.arf'
    merged_rmf_fn          = sourcedir + sourcename[ii] + '.rmf'
    lock_fn                = sourcedir + modelsubdir + 'ae_lock'

    unnamed_src_stats_fn   = sourcename[ii] + '/' + src_stats_basename

    unnamed_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    if keyword_set(error) then message, 'ERROR reading '+unnamed_src_stats_fn
    
    print, sourcename[ii], strtrim(sxpar(unnamed_stats,'LABEL'),2), F='(%"Source: %s (%s)")'      

    if (NOT file_test(merged_src_spectrum_fn)) then begin
      print, 'WARNING! SOURCE SKIPPED: could not find spectrum ', merged_src_spectrum_fn
      continue
    endif
    
    file_mkdir, sourcedir + modelsubdir
    
    ;; Wait if we cannot create a lock file.
    catch, error_code
    if (error_code NE 0) then begin
      catch, /CANCEL
;     print, !ERROR_STATE.MSG
      print, 'Blocked by lock file ', lock_fn, ' ...'
      print, 'Will return to this source later.'
      processed_flag[ii] = 0
      continue
    endif 
    file_link, session_name, lock_fn
    catch, /CANCEL
    
    ;; Build a name for the model using the basename of MODEL_FILENAME and if supplied
    ;; appending the basename of MODEL_CHANGES_FILENAME.
    fdecomp, model_filename, disk, dir, base_model_name
    
    model_name = base_model_name
  
    if keyword_set(model_changes_filename) then begin
      fdecomp, model_changes_filename, disk, dir, model_changes_name, model_changes_qual

      ; Look for a local file in the source directory to override the specified MODEL_CHANGES_FILENAME.
      custom_model_name = strjoin([model_name,model_changes_name], '_')
      fit_custom_fn     = sourcedir + modelsubdir + custom_model_name + '.' + model_changes_qual[0]
      
      if NOT file_test(fit_custom_fn) then begin
        ; No local override found, so prepare to use the specified files.
        if (n_elements(model_changes_filename) EQ 1) then begin
          fit_custom_fn = model_changes_filename 
        endif else begin
          ; Concatenate all the MODEL_CHANGES files specified.
          fit_custom_fn = tempdir + 'model_changes.xcm'
          cmd = string(strjoin(model_changes_filename, ' '), fit_custom_fn, F='(%"cat %s >! %s")')
          run_command, /UNIX, cmd, /QUIET
        endelse
      endif
      
      if file_test(fit_custom_fn) then begin
        model_name = custom_model_name
        print, 'CUSTOMIZATIONS to xcm script:'
        run_command, /UNIX, 'cat '+fit_custom_fn, result, /QUIET
        forprint, result, F="(%'  > %s')"
        print
      endif else fit_custom_fn = ''
    endif else fit_custom_fn = ''

    if keyword_set(interactive) then model_name = model_name + '_interactive'
    
    
    ;; Group the spectrum.
    ;; Even if /CSTAT we will use this grouped spectrum for plotting.
    ;; We let ae_group_spectrum assign the filename grouped_spectrum_fn.
    grouped_spectrum_fn = ''

    ae_group_spectrum, merged_src_spectrum_fn, (keyword_set(group_without_background) ? '' : merged_bkg_spectrum_fn), $
                       grouped_spectrum_fn, $
                       CHANNEL_RANGE=channel_range, $
                       SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range, $
                       CREATOR_STRING=creator_string, $
                       this_snr_goal, grp_name, channel_starting_group, num_groups, inband_src_counts, inband_scaled_bkg_counts
    
 
    if (num_groups LT num_groups_range[0]) then begin
      print, num_groups-2, F='(%"WARNING! only %d unignored groups in spectrum.")'
      
      if NOT keyword_set(cstat) then begin
        print, 'WARNING! SOURCE SKIPPED: You might wish to try the C-statistic option for this source.'
        file_delete, lock_fn, /ALLOW_NONEXISTENT
        continue
      endif
    endif
  
    
    if keyword_set(cstat) then begin
      ; C-stat fitting on raw spectrum; plot grouped spectrum via extra_spectrum_filename.
      fit_result_root         = 'nogrp_' + model_name
      merged_src_spectrum_fn  = file_basename(merged_src_spectrum_fn)
      ignore_spec            = string(channel_range+[-1,1], F='(%"1-%i,%i-**")')
      
      extra_spectrum_filename = file_basename(grouped_spectrum_fn)
      extra_ignore_spec      = string(num_groups, F='(%"1,%d")')
    endif else begin
      ; Chi^2 fitting on grouped spectrum; plot UNgrouped spectrum via extra_spectrum_filename.
      extra_spectrum_filename = file_basename(merged_src_spectrum_fn)
      extra_ignore_spec      = string(channel_range+[-1,1], F='(%"1-%i,%i-**")')

      fit_result_root         = grp_name + '_' + model_name
      merged_src_spectrum_fn  = file_basename(grouped_spectrum_fn)
      ignore_spec            = string(num_groups, F='(%"1,%d")')
    endelse
    
    fit_xcm_fn             = modelsubdir +fit_result_root + '.xcm'

        
    ; Read RMF so that we can convert between detector channels and energy.
    ae_channel_energy_and_arf, merged_rmf_fn, merged_arf_fn, $
      channel_number, channel_lowenergy, channel_highenergy, channel_midenergy

    ; Define the energy range over which the background spectrum will influence the fit.
    if keyword_set(cstat) then begin
      ; A reasonable energy range over which the bkg spectrum will be fit by cplinear is [0.1,10.0] keV.
      ; This is needed, at least, to allow XSPEC to plot with a log-scaled energy axis.
      ; We feel that this bkg fitting range should be WIDER than that used for the source spectrum (channel_range), since the instrumental background extends beyond the limits of Chandra sensitivity to X-rays, and any bkg data lying just beyond the limits on the source spectrum help constrain the bkg at/near those energies.
      bkg_channel_range = fltarr(2)
      ind =         (where(channel_midenergy GE  0.1))[0]
      bkg_channel_range[0] = channel_number[ind]
      
      ind = (reverse(where(channel_midenergy LE 10.0)))[0]
      bkg_channel_range[1] = channel_number[ind]
      
      bkg_ignore_spec        = string(bkg_channel_range+[-1,1], F='(%"1-%i,%i-**")')
    endif else begin
      ; This is the chi^2 case.
      bkg_channel_range = channel_range
    endelse
      
    
    ; Read the bkg spectrum.
    bin_table = mrdfits(merged_bkg_spectrum_fn, 1, theader, /SILENT, STATUS=status)
    if (status NE 0) then message, 'ERROR reading ' + merged_bkg_spectrum_fn
       
    channels            = bin_table.CHANNEL
    bkg_observed_counts = bin_table.COUNTS
    
    ; Remove background counts outside the energy band used for fitting the background, and count them.
    ind = where((channels LT bkg_channel_range[0]) OR (channels GT bkg_channel_range[1]))
    bkg_observed_counts[ind] = 0
    inband_bkg_counts = total(/INT, bkg_observed_counts)
    
    
    if keyword_set(cstat) then begin
      max_cplinear_vertices = 10
      ; We need to choose up to max_cplinear_vertices energies at which the cplinear bkg model will place a vertex.
      ; In an earlier version (3394 2009-04-01) we tried placing the vertices at energies midway *between* the 
      ; observed events (using ae_group_spectrum), however we ultimately decided to try placing them at observed
      ; energies in hopes of reducing the chance that XSPEC would peg a cplinear rate parameter at zero, which can 
      ; wreak havoc with the fitting process.  (The cplinear model cannot be driven to zero at the observed energies
      ; because Cstat would blow up to infinity.)
      
      ; The first and last vertices are no longer fixed to the endpoints of the src spectrum's fitting range!!. 
      
      ; Identify which channels in the energy band have data; these are candidate locations for cplinear vertex energies.
      ind = where(bkg_observed_counts GT 0, num_nonzero_channels)
      
      if (num_nonzero_channels LE 1) then begin
        ; The normal algorithm below won't work for cases with 0 or 1 nonzero channels.
        ; When there are zero bkg counts, AE's fitting scripts will not attempt to build a background model. 
        ; When there is one bkg count, we arbitrarily define vertices at each end of the bkg energy range.
        channel_at_vertex = bkg_channel_range
        num_vertices      = 2
      endif else begin
        ; Iteratively search for a set of max_cplinear_vertices or less non-zero channels such that the intervals
        ; between them have approximately the same number of non-zero channels.
        
        nonzero_channels = channels[ind]
        
        nonzero_channels_per_segment = 1 > floor(num_nonzero_channels/max_cplinear_vertices)
        while (1) do begin
          ; The first and last nonzero channels get vertices.
          is_vertex_flag = bytarr(num_nonzero_channels)
          
          left_ind  = 0L
          right_ind = num_nonzero_channels-1
          
          is_vertex_flag[left_ind++]  = 1
          is_vertex_flag[right_ind--] = 1
          
          ; Decimate the list of nonzero channels by the factor nonzero_channels_per_segment, working from both ends.
          while (1) do begin
            ; Advance the left index and mark a vertex.
            left_ind  += nonzero_channels_per_segment
            
            if ((right_ind-left_ind) LT nonzero_channels_per_segment) then break
            
            is_vertex_flag[left_ind++] = 1
            
            ; Advance the right index and mark a vertex.
            right_ind -= nonzero_channels_per_segment
            
            if ((right_ind-left_ind) LT nonzero_channels_per_segment) then break
            
            is_vertex_flag[right_ind--] = 1
          endwhile
          
          ; Count  the vertices.
          channel_at_vertex = nonzero_channels[where(is_vertex_flag,num_vertices)]
        
         ;help, num_vertices, nonzero_channels_per_segment
         
          ; Decide whether to accept these vertices, or increase nonzero_channels_per_segment.
          if      (num_vertices GT  6) && (nonzero_channels_per_segment EQ 1) then nonzero_channels_per_segment++ $
          else if (num_vertices GT max_cplinear_vertices)                     then nonzero_channels_per_segment++ $
          else break
        endwhile ; search for vertex energies
      endelse
      
      print, inband_bkg_counts, num_vertices-1, F='(%"%d in-band bkg counts will be modeled by %d cplinear line segments.")'
     ;print, channel_at_vertex

      ; Convert vertex channel numbers to units of energy.
      vertex_energy    = replicate(-1.0,max_cplinear_vertices) 
      vertex_energy[0] = interpol(channel_midenergy, channel_number, channel_at_vertex)
    endif


    ;; Create XSPEC script.
    ;; Prefix contains source-specific commands.
    openw,  xcm_unit, sourcedir + fit_xcm_fn, /GET_LUN
    printf, xcm_unit, merged_src_spectrum_fn,       F='(%"set spectrum_filename       \"%s\"")'
    if (extra_spectrum_filename NE "") then $
      printf, xcm_unit, extra_spectrum_filename,    F='(%"set extra_spectrum_filename \"%s\"")'
    printf, xcm_unit,     ignore_spec,              F='(%"set     ignore_spec         \"%s\"")'
    if (extra_spectrum_filename NE "") then $
      printf, xcm_unit, extra_ignore_spec,          F='(%"set extra_ignore_spec       \"%s\"")'
    printf, xcm_unit, fit_result_root,              F='(%"set model_name              \"%s\"")'
    printf, xcm_unit, keyword_set(cstat),           F='(%"set c_stat_flag               %d")'
    printf, xcm_unit, inband_src_counts, inband_scaled_bkg_counts, $
                                                    F='(%"set spectrum_description    \"NET_CNTS=%d-%0.1f\"")'
    if keyword_set(cstat) then begin
      printf, xcm_unit, bkg_ignore_spec,            F='(%"set bkg_ignore_spec         \"%s\"")'
      printf, xcm_unit, strjoin(string(vertex_energy,F='(%"%0.2f")'),' '), $
                                                    F='(%"set cplinear_energies        {%s}")'
    endif
    printf, xcm_unit, codedir+'xspec_scripts',      F='(%"set model_directory         \"%s\"")'

    printf, xcm_unit, keyword_set(interactive),     F='(%"set interactive_flag          %d")'
    
    free_lun, xcm_unit

    ; Append MODEL_FILENAME to XSPEC script prefix using "sed" to insert any user-supplied customizations to the model.
    if (fit_custom_fn EQ '') then begin
      cmd = string(model_filename, sourcedir + fit_xcm_fn, F="(%'cat %s >>! %s')")
    endif else begin
      cmd = string(fit_custom_fn, model_filename, sourcedir + fit_xcm_fn, F="(%'sed -e ""/AE CUSTOMIZATIONS/r %s"" %s >>! %s')")
    endelse
    run_command, /UNIX, cmd
    
    
    ;; Perform the fit.
    ae_perform_fit, sourcedir, fit_result_root, INTERACTIVE=keyword_set(interactive), TEMPDIR=tempdir, FIT_TIMEOUT=fit_timeout

    file_delete, lock_fn, /ALLOW_NONEXISTENT
  endfor ;kk
endrep until (num_to_process EQ 0) OR (noprogress_pass_count GE 3)

if (num_to_process GT 0) then begin
  print
  print, 'WARNING!  These sources could not be processed.  Perhaps lock files remain from a previous session ...'
  forprint, SUBSET=source_index, sourcename
endif
endif ;keyword_set(fit_spectra)



CLEANUP:
;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(temproot) then begin
  list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, temproot
endif

if (exit_code EQ 0) then return $
else begin
  print, 'acis_extract: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP

END  ; END of acis_extract


; =============================================================================
;;; Tool that constructs rectangles around the readout streaks of each source supplied.
;;; The code uses a simple fixed width for the rectangle; something based on the PSF would be better!

;;; ASPECT_FN is an aspect file for the observation; required by dmcoords.

PRO ae_make_streak_regions, catalog_or_srclist, obsname, obsdata_filename, region_filename, ASPECT_FN=aspect_fn, $
                  EXTRACTION_NAME=extraction_name


obs_stats_basename       = 'obs.stats'

repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  temproot = 'AE' + session_name +'.noindex/'
  temproot = filepath(temproot, /TMP)
endrep until (NOT file_test(temproot))
file_mkdir, temproot
tempdir = temproot

run_command, /INIT, PARAM_DIR=tempdir

ymargin = 32
xsize   = 10

  ;; Input catalog should be an ascii file with source names. 
  ;; 
  readcol, catalog_or_srclist, sourcename, FORMAT='A', COMMENT=';'
  num_sources = n_elements(sourcename)

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)

  openw, region_unit, region_filename, /GET_LUN
  for ii = 0, num_sources-1 do begin
    print, F='(%"\n===================================================================")'
    print, sourcename[ii], F='(%"\nSource: %s")'
    obsdir    = sourcename[ii] + '/' + obsname + '/' + extraction_subdir[ii]
    stats_fn  = obsdir + obs_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    ; Find the nominal src position in chip coordinates.
    cmd = string(obsdata_filename, aspect_fn, sxpar(stats, 'X_CAT'), sxpar(stats, 'Y_CAT'),  F="(%'dmcoords %s asolfile=%s opt=sky x=%f y=%f')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords chipx chipy chip_id', result
    chipx = float(result[0])
    chipy = float(result[1])
    chip_id = float(result[2])
help, chip_id
    
    ; Compute the center of the ROTBOX.
    cmd = string(obsdata_filename, aspect_fn, chipx, 512.5,  F="(%'dmcoords %s asolfile=%s opt=chip chipx=%f chipy=%f')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords x y', result
    xcenter = float(result[0])
    ycenter = float(result[1])
    
    ; Compute angle of chip system wrt sky system.
    cmd = string(obsdata_filename, aspect_fn, chipx, 1,  F="(%'dmcoords %s asolfile=%s opt=chip chipx=%f chipy=%f')")
    run_command, cmd

    run_command, /QUIET, 'pget dmcoords x y', result
    x0 = float(result[0])
    y0 = float(result[1])
    
    angle = !RADEG * atan(x0-xcenter,ycenter - y0)
help, angle
    
    printf, region_unit, xcenter, ycenter, xsize, 1024+ymargin, angle, F='(%"rotbox(%6.1f,%6.1f,%5.1f,%6.1f,%6.1f)")'
  endfor
    
  free_lun, region_unit
  
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



; ; =============================================================================
; ; Obsolete tool to estimate the cropping in the CIAO PSF library.
; PRO estimate_cropping
; 
; repeat begin
;   session_name = string(random()*1E4, F='(I4.4)')
;   temproot = 'AE' + session_name +'.noindex/'
;   temproot = filepath(temproot, /TMP)
; endrep until (NOT file_test(temproot))
; file_mkdir, temproot
; tempdir = temproot
; 
; run_command, /INIT, PARAM_DIR=tempdir
; 
; arcsec_per_skypixel = 0.492 
; 
; f2lib = "$CALDB/data/chandra/acis/cpf/2dpsf/acisi1998-11-052dpsf2N0002.fits"
; temp_image_fn = "f2.psf"
; 
; f2_pixsiz = 12/24.0
; 
; energy_table = mrdfits(f2lib, 2, energy_header, /SILENT, STATUS=status)
; psf_energy = energy_table.energy
; 
; 
; for ii = 0,1 do begin
;   case ii of
;     0: lib = { name:'acisi1998-11-052dpsf1N0002.fits', dim:256, pixsiz:6/24.0, $
;                        elevation_samples:[0,5,10], crop_fraction:fltarr(3,n_elements(psf_energy)) }
; 
;     1: lib = { name:'acisi1998-11-052dpsf3N0002.fits', dim:512, pixsiz:2/24.0, $
;                        elevation_samples:[0,5], crop_fraction:fltarr(2,n_elements(psf_energy)) }
;   endcase
;   
;   print, lib
;   
;   for jj=0, n_elements(lib.elevation_samples)-1 do begin
;     dety  = 4096.5 - lib.elevation_samples[jj] * (60/arcsec_per_skypixel)  ; arcmin
;     
;     for kk=0, n_elements(psf_energy)-1 do begin
;       ; Make a full-sized F2 PSF, using its natural binning.
;       cmd = string( f2_pixsiz, f2_pixsiz, 512, 512, psf_energy[kk], 4096.5, dety, $
;                     f2lib, temp_image_fn, $
;                     F="(%'mkpsf coord=DET binspax=%6.4f binspay=%6.4f sizeoutx=%d sizeouty=%d energy=%f x=%7.2f y=%7.2f psflibfile=""%s"" outfile=%s outpsffile="""" ')")
; 
;       run_command, cmd
; 
;       psf_img = readfits(temp_image_fn, psf_header, /SILENT)
;       
;       ; Extract the section of the F2 image covered by the library under test.
;       field_of_view = lib.dim * lib.pixsiz / f2_pixsiz   ; F2 image pixels
;       dim           = round(field_of_view)
;       corner        = (sxpar(psf_header,'NAXIS1') / 2) - (dim/2)
;       
;       lib_img = psf_img[corner:corner+dim-1, corner:corner+dim-1]
;       tvscl, lib_img < max(lib_img)/10.
;       
;       central_power = total(lib_img, /DOUBLE)
;       total_power   = total(psf_img, /DOUBLE)
;       
;       lib.crop_fraction[jj,kk] = (total_power - central_power) / total_power
; 
; help, dim, central_power, total_power
;     endfor ;kk
; 
;   endfor ;jj
;   
;   fxhmake,  pheader, /EXTEND, /DATE, /INITIALIZE
;   fxaddpar, pheader, 'CREATOR', "estimate_cropping, $Revision: 3519 $"  
;   fxaddpar, pheader, "FNFITS",  lib.name
;   fxaddpar, pheader, "HDUNAME", 'CROP_FRACTION'
;   
;   writefits, lib.name, lib.crop_fraction, pheader
;   
;   mwrfits, energy_table, lib.name, energy_header
; 
;   elevation_table = {elevation: lib.elevation_samples}
;   fxbhmake, theader, n_elements(elevation_table), 'ELEVATIONS', /DATE
;   fxaddpar, theader, 'CREATOR', "estimate_cropping, $Revision: 3519 $"  
;   
;   mwrfits, elevation_table, lib.name, theader
; 
; endfor ;ii
; 
; CLEANUP:
; ;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
; ;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
; if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles
; 
; if file_test(temproot) then begin
;   list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
;   if (count GT 0) then file_delete, list
;   file_delete, temproot
; endif
; 
; return
; end



;; DESIGN NOTES

; Are we content to linearly extrapolate the PSF_FRAC(energy)?

; HEXTRACT.pro does NOT adjust PHYSICAL coordinates.  Here's how to do that manually:
; See http://iraf.noao.edu/projects/ccdmosaic/Imagedef/imagedef.html for definition
; of LTV? and LTM?_? keywords.
;          crpix = [sxpar(this_header, 'CRPIX1P'), sxpar(this_header, 'CRPIX2P')] - trim
;          ltv   = [sxpar(this_header, 'LTV1'),    sxpar(this_header, 'LTV2')]    - trim
;          
;          hextract, this_psf_img, this_header, trim, xdim-1-trim, trim, ydim-1-trim, /SILENT
;          fxaddpar, this_header, 'CRPIX1P', crpix[0]
;          fxaddpar, this_header, 'CRPIX2P', crpix[1]
;          fxaddpar, this_header, 'LTV1',    ltv[0]
;          fxaddpar, this_header, 'LTV2',    ltv[1]

;offaxis angle:              ~0:5'  ~6:7'  ~8:11'  ~12:12.4' ~13:20'
;ACIS-I library:             F3     F1     F1      F1,F2     F2
;PSF pix size (sky pixels):  0.0833 0.25   0.5     0.5       1.0
;PSF dimension:              515    259    130     130,515   259
;
;Corners of I-array are 12.4' off axis.

