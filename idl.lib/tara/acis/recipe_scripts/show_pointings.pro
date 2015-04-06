;;; This tool will drive ds9 to display the supplied region file on top of a series of data files.

;;; This is useful for visualizing a region file that covers a large field of view where a single data file would be inconvenient to load or navigate.


@acis_extract

PRO show_pointings, REGION_FILE=region_file, PATTERN=event_file_pattern

if ~keyword_set(event_file_pattern) then event_file_pattern = 'obsid*/acis.validation.evt2'

obs_event_fn = file_search(event_file_pattern, COUNT=num_obs)
if (num_obs EQ 0) then begin
  print, 'NO event data found!'
  retall
endif

session_name = string(random()*1E4, F='(I4.4)')

run_command, /INIT

ae_send_to_ds9, my_ds9, NAME=string(session_name, F="(%'show_pointings.%s')"), OPTION_STRING='-log -bin factor 8'

for ii=0, num_obs-1 do begin
  ae_send_to_ds9, my_ds9, obs_event_fn[ii], region_file
  
  print, obs_event_fn[ii], F="(%'\nThe region file is shown on the data %s. \n TYPE ""c"" and PRESS RETURN to continue.')"
  line = ''
  repeat read, line until strmatch(line,'c')
 
endfor ;ii



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
  print, 'show_pointings: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
return
end

