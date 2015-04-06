;;; $Id: ae_recipe.pro 3519 2009-08-12 15:00:14Z psb6 $
;;; Patrick Broos, 2004

;;; Programs to automate the AE recipe found in the Getting Started section of 
;;; the AE manual and in recipe.txt.

;!! These scripts assume a very particular set of conventions for paths to 
;!! the various files that AE uses, as shown in the Getting Started section of 
;!! the AE manual and in recipe.txt.

@acis_extract

;#############################################################################
;;; Source List Manager tool

;;; See AE manual for usage.

;;; The master source list is maintained in the file "all.srclist".

;;; Comment lines begin with ";".

;;; We retain entries for REMOVED sources rather than excising them from the list 
;;; to keep sequence numbers stable.  A comment character (;) is used to hide them 
;;; from AE, and a tag "(REMOVED)" is added to distinguish them from regular comments.

;;; Note that the definitive coordinates of a source and the source label are always stored 
;;; in the source.stats file at the top level of the source directory; 
;;; the ADD and MOVE threads in this tool write coordinates there.
;;; This tool accepts a MERGE_NAME input only for the UPDATE_POSITIONS_CORR and UPDATE_POSITIONS_DATA
;;; threads since AE's position estimates may be stored in a named source.stats file.

PRO ae_source_manager, RA=ra, DEC=dec, POSITION_TYPE=posntype, $
                       NAME=sourcename, LABEL=label, $
                       
                       ADD=add, PROVENANCE=provenance, $

                       REMOVE=remove, TRASH_DIR=trash_dir, OBSID_LIST=obsid_list, $
                       
                       MOVE=move, NO_RENAME=no_rename, $ 
                       
                       UPDATE_POSITIONS_RECON=update_positions_recon, $
                       UPDATE_POSITIONS_CORR=update_positions_corr, $
                       UPDATE_POSITIONS_DATA=update_positions_data, $
                       MERGE_NAME=merge_name, $
                       
                       SORT_RA=sort_ra, SORT_BULLSEYE=sort_bullseye, $
                       SET_LABEL_AS_SEQUENCE=set_label_as_sequence, $
                       SKIP_BACKUP=skip_backup

src_stats_basename       = 'source.stats'
obs_stats_basename       = 'obs.stats'
srclist_fn               = 'all.srclist'
precision  = 1
creator_string = "ae_source_manager, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()
print

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
  

;; For /ADD and /MOVE, the required RA & DEC inputs must be the same length as the
;; optional inputs NAME, LABEL, & PROVENANCE.
if keyword_set(add) OR keyword_set(move) then begin
  num_sources = n_elements(ra)
  if (num_sources EQ 0) then begin
    print, 'ERROR: RA and DEC inputs must be supplied.'
    retall
  endif
  
  if (n_elements(dec) NE num_sources) then begin
    print, 'ERROR: RA and DEC inputs must be same length.'
    retall
  endif
  
  if (size(ra,/TYPE) NE 5) || (size(dec,/TYPE) NE 5) then begin
    print, 'ERROR: RA and DEC inputs must be double precision floats.'
    retall
  endif
  
  case (n_elements(provenance)) of
    num_sources:
    1          : provenance = replicate(provenance, num_sources)
    0          : provenance = replicate('unknown', num_sources)
    else       : begin
                 print, 'ERROR: PROVENANCE input must be same length as RA/DEC.'
                 retall
                 end
  endcase

  case (n_elements(posntype)) of
    num_sources:
    1          : posntype = replicate(posntype, num_sources)
    0          : begin
                 print, 'ERROR: type of source position must be supplied via POSITION_TYPE.'
                 retall
                 end
    else       : begin
                 print, 'ERROR: POSITION_TYPE input must be same length as RA/DEC.'
                 retall
                 end
  endcase

  if keyword_set(sourcename) then begin
    if (n_elements(sourcename) NE num_sources) then begin
      print, 'ERROR: NAME input must be same length as RA/DEC.'
      retall
    endif
  endif 
  
  if keyword_set(label) then begin
    if (n_elements(label) NE num_sources) then begin
      print, 'ERROR: LABEL input must be same length as RA/DEC.'
      retall
    endif
    
    if (total(strmatch(strtrim(label,2),'')) GT 0) then begin
      print, 'ERROR: LABEL input must not be the empty string.'
      retall
    endif
  endif 
  
endif ; /ADD or /MOVE


;; Read any existing source list.  
;; We cannot use readcol because it won't read comment lines with spaces.
if file_test(srclist_fn) then begin
  Nlines = file_lines(srclist_fn) 
  if (Nlines GT 0) then begin
    master_srclist = strarr(Nlines)
    openr, unit, srclist_fn, /GET_LUN
    readf, unit, master_srclist
    free_lun, unit
  
    if NOT strmatch(master_srclist[0], ";*") then $
      master_srclist = ['; Master Source List',master_srclist] 
  endif ;(Nlines GT 0)
endif else begin
  if keyword_set(move) OR keyword_set(remove) then print, 'WARNING: file '+srclist_fn+' not found.'
endelse

;; We need master_srclist to have at least one comment line to make the code easier later, so we'll add a comment line if necessary.
if (n_elements(master_srclist) EQ 0) then master_srclist =  '; Master Source List'
  

;; For /MOVE and /REMOVE, existing sources must be identified via NAME or LABEL.
if (keyword_set(move) OR keyword_set(remove)) AND NOT keyword_set(sourcename) then begin
  if NOT keyword_set(label) then begin
    print, 'ERROR: you must identify sources via NAME or LABEL inputs.'
    retall
  endif
  
  ; Look up NAME values using supplied LABEL values.
  print, 'Looking up sources by their LABELs ...'
  label = strtrim(label,2)  
  sourcename  = strarr(n_elements(label))
  for ii = 0, n_elements(master_srclist)-1 do begin
    this_name = master_srclist[ii]
    if strmatch(this_name, ";*") then continue
    
    sourcedir = this_name + '/' 
    
    unnamed_src_stats_fn  = sourcedir + src_stats_basename
    unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      this_label  = strtrim(sxpar(unnamed_src_stats, 'LABEL', COUNT=count),2)
      if (count GT 0) then begin
        ind = (where(label EQ this_label, count))[0]
        if (count GT 0) then begin
          if (sourcename[ind] NE '') then begin
            print, 'ERROR: Two sources have the same label '+this_label
            retall
          endif
          
          sourcename[ind] = this_name
        endif ; match to this_label found
      endif ; source has LABEL keyword
    endif else print, 'WARNING! Could not read '+unnamed_src_stats_fn
  endfor ;ii

  ind = where(sourcename EQ '', count)
  if (count GT 0) then begin
    print, 'ERROR: could not find these LABELs: '
    forprint, label, SUBSET=ind
    retall
  endif

  forprint, label, sourcename, F='(%"LABEL %s == %s")'
endif ; LABEL supplied instead of NAME



;; For /MOVE and /REMOVE, keep a list of the obsids that observed the specified sources.
if (keyword_set(move) OR keyword_set(remove)) then begin
  if (n_elements(obsid_list) EQ 0) then obsid_list = ''
  num_sources = n_elements(sourcename)
  for ii = 0, num_sources-1 do begin
    if file_test(sourcename[ii], /DIRECTORY) then begin
      ; Keep a list of the obsids that observed the discarded sources.
      obs_stats_fn = file_search(sourcename[ii], obs_stats_basename, COUNT=num_obs)
      if (num_obs GT 0) then begin
        these_obsids = strmid(obs_stats_fn,  1+reform(strpos(obs_stats_fn, '/'), 1,num_obs))
        these_obsids = strmid(these_obsids, 0, reform(strpos(these_obsids, '/'), 1,num_obs))

        obsid_list   = [obsid_list, these_obsids]
        obsid_list   = obsid_list[uniq(obsid_list,sort(obsid_list))]
      endif
    endif
  endfor ;ii
  if (n_elements(obsid_list) GT 1) then obsid_list = obsid_list[1:*]
endif


;; ADD sources.
if keyword_set(add) then begin
  if NOT keyword_set(sourcename) then begin
    sourcename = strcompress(/REMOVE_ALL, adstring(ra,dec,precision,/TRUNCATE))
  endif
  
  for ii=0,num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' 
    unnamed_src_stats_fn  = sourcedir + src_stats_basename
                       
    ; Use existing src_stats file if possible.
    if file_test(unnamed_src_stats_fn) then begin
      print, 'WARNING: modifying existing source '+sourcename[ii]
      unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
      if keyword_set(error) then message, 'ERROR reading '+unnamed_src_stats_fn
    endif else begin
      print, 'Adding source '+sourcename[ii]
      file_mkdir, sourcedir
      fxhmake, unnamed_src_stats, /INITIALIZE, /EXTEND, /DATE
      fxaddpar, unnamed_src_stats, 'LABEL', 'unlabeled'
    endelse
 
    ; Check whether this source is in the master list.
    dum = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      ; Append the source to the master list and assign a default label as the 1-based 
      ; sequence number in the list, counting removed sources.
      master_srclist = [master_srclist, sourcename[ii]]
      comment_flag = strmatch(master_srclist, ";*")
      removed_flag = strmatch(master_srclist, "*(REMOVED)*")
      seq_num = round(n_elements(master_srclist)-total(comment_flag)+total(removed_flag)) 
      fxaddpar, unnamed_src_stats, 'LABEL', strtrim(string(seq_num),2), 'sequence number'
    endif
    
    fxaddpar, unnamed_src_stats, 'CREATOR', creator_string
    fxaddpar, unnamed_src_stats, 'OBJECT',   sourcename[ii], 'source name'
    get_date, date_today, /TIMETAG
    fxaddpar, unnamed_src_stats, 'POSNDATE',     date_today, 'date RA,DEC were changed'
    fxaddpar, unnamed_src_stats, 'RA',               ra[ii], 'source position', F='(F10.6)'
    fxaddpar, unnamed_src_stats, 'DEC',             dec[ii], 'source position', F='(F10.6)'
    fxaddpar, unnamed_src_stats, 'POSNTYPE',   posntype[ii], 'type of source position'
    fxaddpar, unnamed_src_stats, 'PROVENAN', provenance[ii], 'source provenance'
    fxaddpar, unnamed_src_stats, 'BKSCL_LO',           20.0, 'smallest BACKSCAL allowed'
    fxaddpar, unnamed_src_stats, 'BKSCL_GL',           30.0, 'target   BACKSCAL'
    fxaddpar, unnamed_src_stats, 'BKSCL_HI',           40.0, 'largest  BACKSCAL allowed'

    ; Record any observer-supplied LABEL, overwriting the default label above.
    if keyword_set(label) then fxaddpar, unnamed_src_stats, 'LABEL', label[ii]
    
    print, 'Source LABEL is "'+strtrim(sxpar(unnamed_src_stats, 'LABEL'),2)+'"'
    
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
  endfor ;ii

  GOTO, WRITE_SRCLIST
endif ; /ADD


;; /MOVE
if keyword_set(move) then begin
  if NOT keyword_set(sourcename) then begin
    print, 'ERROR: existing source name must be supplied via NAME.'
    retall
  endif

  if keyword_set(no_rename) then begin
    sourcename_new = sourcename
  endif else begin
    sourcename_new = strcompress(/REMOVE_ALL, adstring(ra,dec,precision,/TRUNCATE))
  endelse
  
  was_moved_flag = bytarr(num_sources)
  for ii=0,num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' 
    unnamed_src_stats_fn  = sourcedir + src_stats_basename

    ; Before modifying any files, determine if the source will be renamed and if
    ; there are any obstacles to doing so.
    name_is_changing = (sourcename[ii] NE sourcename_new[ii])
    if name_is_changing AND file_test(sourcename_new[ii]) then begin
      print, 'WARNING: '+sourcename[ii]+' cannot be moved because new name duplicates existing source '+sourcename_new[ii]
      continue
    endif
    
    ; Read existing src_stats file.
    if file_test(unnamed_src_stats_fn) then begin
      unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
      if keyword_set(error) then message, 'ERROR reading '+unnamed_src_stats_fn
    endif else begin
      print, 'ERROR: source directory '+sourcename[ii]+' not found'
      continue
    endelse
 
    ; Compute how far source moved.
    hours_per_degree = 24D/360
    gcirc, 1, sxpar(unnamed_src_stats,'RA')*hours_per_degree, sxpar(unnamed_src_stats,'DEC'), ra[ii]*hours_per_degree, dec[ii], distance

    ; Check whether this source is in the master list.
    ind = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      print, 'WARNING: source '+sourcename[ii]+' not found in '+srclist_fn
      ; Append the source to the master list and assign a default label as the 1-based 
      ; sequence number in the list, counting removed sources.
      master_srclist = [master_srclist, sourcename[ii]]
      comment_flag = strmatch(master_srclist, ";*")
      removed_flag = strmatch(master_srclist, "*(REMOVED)*")
      seq_num = round(n_elements(master_srclist)-total(comment_flag)+total(removed_flag)) 
      fxaddpar, unnamed_src_stats, 'LABEL', string(seq_num), 'sequence number'
    endif else begin
      ; Change the name of this source.
      master_srclist[ind] = sourcename_new[ii]
    endelse
    
    fxaddpar, unnamed_src_stats, 'CREATOR', creator_string
    fxaddpar, unnamed_src_stats, 'OBJECT', sourcename_new[ii], 'source name'
    get_date, date_today, /TIMETAG
    fxaddpar, unnamed_src_stats, 'POSNDATE',     date_today, 'date RA,DEC were changed'
    fxaddpar, unnamed_src_stats, 'RA',               ra[ii], 'source position', F='(F10.6)'
    fxaddpar, unnamed_src_stats, 'DEC',             dec[ii], 'source position', F='(F10.6)'
    fxaddpar, unnamed_src_stats, 'POSNTYPE',   posntype[ii], 'type of source position'
    fxaddpar, unnamed_src_stats, 'PREVNAME', sourcename[ii]
    
    ; Record any observer-supplied LABEL.
    if keyword_set(label) then fxaddpar, unnamed_src_stats, 'LABEL', label[ii]
    
    msg = string( sourcename[ii], sxpar(unnamed_src_stats, 'LABEL'), distance, F='(%"%s (%s) moved %5.2f arcsec")' )

    ; Remove obsolete information and resave.
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
    was_moved_flag[ii] = 1
    
    ; Rename the source directory.
    if name_is_changing then begin
      print, msg + ' (renamed to '+sourcename_new[ii]+')'
      
      ; Remove files that use the old source name.
      filelist = file_search(sourcedir, sourcename[ii]+'*', COUNT=count)
      if (count GT 0) then file_delete, filelist
      
      file_move, sourcedir, sourcename_new[ii]
    endif else print, msg
  endfor ;ii
  
  ind = where(~was_moved_flag, count)
  if (count GT 0) then begin
    print
    forprint, SUBSET=ind, sourcename, F='(%"WARNING!  Source %s was NOT moved!")'
  endif

  GOTO, WRITE_SRCLIST
endif ; /MOVE


;; REMOVE sources
if keyword_set(remove) then begin
  num_sources = n_elements(sourcename)
  
  if NOT keyword_set(trash_dir) then trash_dir = './trash'

  file_mkdir, trash_dir
  for ii = 0, num_sources-1 do begin
    ; We retain the master sourcelist entry rather than removing it to keep sequence numbers stable.
    ; A comment character (;) is used to hide it from AE, and a tag "(REMOVED)" is added to distinguish it
    ; from regular comments.
    ind = where(sourcename[ii] EQ master_srclist, count)
    if (count EQ 0) then begin
      print, 'WARNING: source '+sourcename[ii]+' not found in '+srclist_fn
    endif else master_srclist[ind] = ';'+master_srclist[ind]+' (REMOVED)'
    
    if file_test(sourcename[ii], /DIRECTORY) then begin
      print, 'Moving '+sourcename[ii]+'/ to '+expand_path(trash_dir)+' ...'
      file_move, sourcename[ii], trash_dir
    endif else print, 'WARNING: source directory '+sourcename[ii]+' not found.' 
  endfor ; ii
  
  GOTO, WRITE_SRCLIST
endif ; /REMOVE


;; SORT source list
if keyword_set(sort_bullseye) or keyword_set(sort_ra) then begin
  ; Split the source list into comments and sources.
  comment_flag = strmatch(master_srclist, ";*")
  comment_ind = where(comment_flag, COMPLEMENT=src_ind, NCOMPLEMENT=num_entries)
  comment_list = master_srclist[comment_ind]

  if (num_entries GT 0) then begin
    ; Remove the comment lines.
    master_srclist = master_srclist[src_ind]
    
    ; Read the RA/DEC values.
    print, 'Looking up source coordinates ...'
    ra  = replicate(!VALUES.D_NAN,num_entries)
    dec = replicate(!VALUES.D_NAN,num_entries)
    for ii=0, num_entries-1 do begin
      sourcedir = master_srclist[ii] + '/' 
      
      unnamed_src_stats_fn  = sourcedir + src_stats_basename
      unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
      
      if (NOT keyword_set(error)) then begin
        ra[ii]  = sxpar(unnamed_src_stats, 'RA')
        dec[ii] = sxpar(unnamed_src_stats, 'DEC')
      endif else print, 'WARNING! Could not read '+unnamed_src_stats_fn
    endfor ;ii
    
    if keyword_set(sort_ra) then begin
      sort_ind = sort(ra)
    endif else begin
      openw,  region_unit, 'bullseye.reg', /GET_LUN
      printf, region_unit, "# Region file format: DS9 version 3.0"
 
      sort_ind = lonarr(num_entries)
      ; Compute distances and position angles from the median position.
      ra_ref  = median(ra)
      dec_ref = median(dec)
      hours_per_degree = 24D/360
      gcirc,  1, ra_ref*hours_per_degree, dec_ref, ra*hours_per_degree, dec, distance
      posang, 1, ra_ref*hours_per_degree, dec_ref, ra*hours_per_degree, dec, angle
      
      ; Sort by distances.
      distance_sort_ind = sort(distance)
      
      ; Define annuli containing 200 sources.  Within each, sort by angle.
      group_size  = 199
      group_start = 0L
      repeat begin
        group_end = (group_start + group_size-1) < (num_entries-1)
        group_size= 200
        
        printf, region_unit, ra_ref, dec_ref, distance[distance_sort_ind[group_end]], F='(%"J2000;circle %10.6f %10.6f %d\" # tag={bullseye}")' 
        
        group_ind = distance_sort_ind[group_start:group_end]
        
        sorted_group_ind = group_ind[sort(angle[group_ind])]
        
        sort_ind[group_start] = sorted_group_ind
        
        group_start = group_end+1
      endrep until (group_start GE num_entries)
      free_lun, region_unit
      
;      forprint, distance, angle, SUBSET=sort_ind
;      plot, ra, dec, /NODATA
;      for ii=0,(num_entries/10)-1 do begin
;        s=ii*10
;        e=s+9
;        oplot, ra[s:e], dec[s:e], PSYM=1
;        wait,0.5 
;      endfor
    endelse
  endif ; (num_entries GT 0)
  
  master_srclist = [comment_list, master_srclist[sort_ind]]

  GOTO, WRITE_SRCLIST
endif ; /SORT


;; SET_LABEL_AS_SEQUENCE
if keyword_set(set_label_as_sequence) then begin
  ; Assign LABEL values using 1-based sequence numbers.
  print, 'Assigning LABELs using 1-based sequence numbers ...'
  seq_num = 1
  for ii = 0, n_elements(master_srclist)-1 do begin
    this_name = master_srclist[ii]
    if strmatch(this_name, ";*") then continue
    
    sourcedir = this_name + '/' 
    
    unnamed_src_stats_fn  = sourcedir + src_stats_basename
    unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
    
    if (NOT keyword_set(error)) then begin
      old_label  = strtrim(sxpar(unnamed_src_stats, 'LABEL', COUNT=count),2)
      new_label  = strtrim(string(seq_num),2)
      seq_num    = seq_num + 1
      fxaddpar, unnamed_src_stats, 'LABEL', new_label, 'sequence number'
      
      msg = this_name+' labeled '+new_label
      if (count GT 0) then begin
        msg = msg+'  (was '+old_label+')'
        fxaddpar, unnamed_src_stats, 'PREVLABL', old_label
      endif
      print, msg
    
      writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
    endif else print, 'WARNING! Could not read '+unnamed_src_stats_fn
  endfor ;ii
  return
endif ; /SET_LABEL_AS_SEQUENCE

 
if (keyword_set(update_positions_recon) || keyword_set(update_positions_corr) || keyword_set(update_positions_data)) then begin
  if NOT keyword_set(sourcename) then begin
    print, 'ERROR: source name must be supplied via NAME.'
    retall
  endif
  
  ;; Replace RA & DEC keywords with RA_CORR,DEC_CORR or RA_DATA,DEC_DATA.
  if keyword_set(update_positions_recon) then begin
    ra_kywd    = 'RA_ML'
    dec_kywd   = 'DEC_ML'
    posntype_new = 'AE reconstruction' 
  endif else if keyword_set(update_positions_corr) then begin
    ra_kywd    = 'RA_CORR'
    dec_kywd   = 'DEC_CORR'
    posntype_new = 'AE correlation'
  endif else begin
    ra_kywd    = 'RA_DATA'
    dec_kywd   = 'DEC_DATA'
    posntype_new = 'AE mean data'
  endelse
  
  if keyword_set(posntype) then begin
    print, 'ERROR: do not supply POSITION_TYPE; ae_source_manager will assign the value ', posntype_new
    retall
  endif
  
  num_sources = n_elements(sourcename)

  ra_new          = dblarr(num_sources)
  dec_new         = dblarr(num_sources)
  source_can_move = bytarr(num_sources)

  if keyword_set(merge_name)      then merge_subdir = merge_name + '/' $
                                  else merge_subdir = ''
  if (n_elements(merge_subdir) EQ 1) then merge_subdir = replicate(merge_subdir,num_sources>1)
  
  ; Find new coordinates for each source.
  for ii = 0, num_sources-1 do begin
    sourcedir = sourcename[ii] + '/' + merge_subdir[ii] 
    stats_fn  = sourcedir + src_stats_basename
    stats = headfits(stats_fn, ERRMSG=error)
    
    if (~keyword_set(error)) then begin
      ra_new [ii] = sxpar(stats,  ra_kywd, COUNT=count1)
      dec_new[ii] = sxpar(stats, dec_kywd, COUNT=count2)
      
      source_can_move[ii] = (count1 EQ 1) && (count2 EQ 1) && finite(ra_new[ii]) && finite(dec_new[ii]) 
      if ~source_can_move[ii] then  print, 'WARNING!  Keywords '+ra_kywd+','+dec_kywd+' not defined for '+sourcename[ii]
    endif else print, 'ERROR! Could not read '+stats_fn
  endfor
  
  ind = where(source_can_move, count)
  if (count GT 0) then $
    ae_source_manager, /MOVE, NO_RENAME=keyword_set(no_rename), NAME=sourcename[ind], RA=ra_new[ind], DEC=dec_new[ind], POSITION_TYPE=posntype_new
  return
endif ; /UPDATE_POSITIONS_*


WRITE_SRCLIST:
  if (keyword_set(move) OR keyword_set(remove)) then begin
    print
    print, 'The specified sources contained extractions from these obsids:'
    forprint, obsid_list
  endif  
  
  if NOT keyword_set(skip_backup) then begin
    ;; Maintain several backups of the source list.
    for ii=9,1,-1 do begin
      backup_fn       = string(srclist_fn,ii,  F='(%"%s-%d")')
      older_backup_fn = string(srclist_fn,ii+1,F='(%"%s-%d")')
      if file_test(backup_fn) then file_move, backup_fn, older_backup_fn, /OVERWRITE
    endfor
    
    if file_test(srclist_fn) then file_move, srclist_fn, backup_fn, /OVERWRITE
  endif
  
  ;; Save the modified source list.
  forprint, TEXTOUT=srclist_fn, master_srclist, /NoCOMMENT, /SILENT
  comment_flag = strmatch(master_srclist, ";*")
  removed_flag = strmatch(master_srclist, "*(REMOVED)*")
  print, srclist_fn, n_elements(master_srclist)-total(comment_flag), total(removed_flag), F='(%"Master source list %s contains %d active and %d removed sources.")'
  return
end ; ae_source_manager



;#############################################################################
;;; For a SINGLE obsid construct a catalog that leads to non-overlapping regions.
;;;
;;; Several input files are located using standard naming conventions as shown 
;;; in the Getting Started section of the AE manual.
;;;
;;; The required parameter EVTFILE_BASENAME specifies the name of the observation event list file in
;;; the directory ../obsXXXX/.  In our recipe we use EVTFILE_BASENAME='validation.evt' for passes that 
;;; prune and reposition sources, and use EVTFILE_BASENAME='spectral.evt' for the photometry/fitting extraction.

;;; SOURCE_NOT_OBSERVED is an output vector flagging sources not observed in this obsid. 
;;; This can be passed along to ae_standard_extraction to help speed it up a little..

;;; If /IGNORE_IF_FRESH is specified then processing is aborted if either of these conditions are true:
;;; * No sources in the catalog were observed by this obsid. This can happen in recon_detect extractions
;;;
;;; * All sources that were observed are found to have fresh extractions, determined by comparing the
;;;   timestamps on the position (POSNDATE) and on the extraction (EXTRDATE).

;;; WAS_IGNORED is an output boolean reporting whether if the IGNORE_IF_FRESH logic decided to 
;;; skip the processing of this observation.


PRO ae_make_catalog, obsname, EVTFILE_BASENAME=evtfile_basename, $
    SRCLIST_FILENAME=srclist_fn, $
    NOMINAL_PSF_FRAC=nominal_psf_frac_kywd, MINIMUM_PSF_FRAC=minimum_psf_frac_kywd, REGION_ONLY=region_only, REUSE_NEIGHBORHOOD=reuse_neighborhood, $
    RESTART=restart, SHOW=show, $
    SOURCE_NOT_OBSERVED=source_not_observed, IGNORE_IF_FRESH=ignore_if_fresh, WAS_IGNORED=was_ignored, $
    _EXTRA=extra

resolve_routine, 'acis_extract_tools', /COMPILE_FULL_FILE

exit_code = 0
creator_string = "ae_make_catalog, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

env_events_basename      = 'neighborhood.evt'
src_events_basename      = 'source.evt'
src_region_basename      = 'extract.reg'
src_emap_basename        = 'source.emap'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
;print, 'Using temporary directory: ', tempdir

;; Save any existing PFILES environment variable and establish access to CIAO.
;print, F='(%"\nae_make_catalog: ============================================================")' 
;print, 'ae_make_catalog: Initializing access to CIAO.'
;print, F='(%"ae_make_catalog: ============================================================\n")'
inherited_pfiles = getenv("PFILES")
run_command, /INIT, PARAM_DIR=tempdir
  
active_catfile = tempdir + 'temp.cat'
temp_events_fn = tempdir + 'temp.evt'
temp_region_fn = tempdir + 'temp.reg'
temp_pi_fn     = tempdir + 'temp.pi'

was_ignored = 0
if ~keyword_set(evtfile_basename) then begin
  print, 'ERROR: parameter EVTFILE_BASENAME must be supplied'
  GOTO, FAILURE
endif

if keyword_set(srclist_fn) then begin
  print, 'WARNING: Aperture adjustment to avoid overlap may be unreliable when only a subset of full catalog is processed.'
endif else srclist_fn = 'all.srclist'

if ~keyword_set(obsname) then begin
  print, 'ERROR: parameter "obsname" must be supplied'
  GOTO, FAILURE
endif

if (size(obsname,/TNAME) NE 'STRING') then begin
  print, 'ERROR: parameter "obsname" must be a string'
  GOTO, FAILURE
endif

print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: PROCESSING OBSERVATION ' + obsname
print,   F='(%"ae_make_catalog: ============================================================")'  

psf_energ         = 1.4967

if (n_elements(show) EQ 0) then show = 1

;; In this program all variables, except the keyword NOMINAL_PSF_FRAC, represent
;; PSF fractions as integer percentages.
nominal_psf_frac  = 90
minimum_psf_frac = 40
if keyword_set(nominal_psf_frac_kywd) then nominal_psf_frac = 99 < round(100*nominal_psf_frac_kywd) > 10
if keyword_set(minimum_psf_frac_kywd) then minimum_psf_frac = 99 < round(100*minimum_psf_frac)      > 10
initial_down_step =-32
initial_up_step   =  4
minimum_step      =  2
step_ratio        = 2.0

obsdir      = '../obs' + obsname + '/'
catfile     = obsdir + 'obs.cat'

evtfile     = obsdir + evtfile_basename
emapfile    = obsdir + 'obs.emap'
aspect_fn   = obsdir + 'obs.asol'

regionfile  = obsdir + 'extract.reg'
collatefile = obsdir + 'obs.collated'
model_savefile = obsdir + 'ae_make_catalog.sav'



;; =====================================================================
;; CONSTRUCT INITIAL CATALOG
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ae_make_catalog: ERROR: no entries read from source list ', srclist_fn
  GOTO, FAILURE
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\nae_make_catalog: %d sources found in catalog.")'

cat = replicate({sourcename:'', ra:0D, dec:0D, psf_frac:nominal_psf_frac, psf_energ:psf_energ, overlap:0.0}, num_sources)
cat.sourcename = sourcename

;; Write a temp catalog holding the full source list.
;; Ask AE to determine which sources were observed by this obsid.
forprint, TEXTOUT=active_catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, F=fmt, /NoCOMMENT, /SILENT

print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Asking AE which sources were observed in obsid ',obsname
print,   F='(%"ae_make_catalog: ============================================================\n")'  

; Pass a scaler as SOURCE_NOT_OBSERVED to ensure that it does NOT take the role of an input to the CONSTRUCT_REGIONS stage.
; The output SOURCE_NOT_OBSERVED vector corresponds to all.srclist and is returned to the caller.
source_not_observed = 0 
acis_extract, active_catfile, obsname, evtfile, /CONSTRUCT_REGIONS, SOURCE_NOT_OBSERVED=source_not_observed, /QUERY_ONLY, EMAP_FILENAME=emapfile, ASPECT_FN=aspect_fn


; Abort if no source was observed. 
observed_ind = where(~source_not_observed, num_sources)
if (num_sources EQ 0) then begin
  print, 'ae_make_catalog: No sources were observed in obsid ', obsname
  was_ignored = 1
  GOTO, CLEANUP
endif

; Prune the catalog to the sources observed in this obsid.
cat = cat[observed_ind]

;; Write a temp catalog holding the pruned source list.
forprint, TEXTOUT=active_catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, F=fmt, /NoCOMMENT, /SILENT

;; Read any FRACSPEC values from any existing obs.stats files so that we start our search
;; where the PSF fraction was set from any previous extraction.
print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Running COLLATE stage to find any existing PSF fractions ' 
print,   F='(%"ae_make_catalog: ============================================================\n")'  
acis_extract, active_catfile, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, VERBOSE=0, _EXTRA=extra

bt_initial=mrdfits(collatefile, 1, /SILENT)
if (n_elements(bt_initial) NE num_sources) then message, 'BUG in ae_make_catalog!'

column_names = tag_names(bt_initial)

; During the AE runs, the catalog fields RA and DEC are left as zero as a flag 
; telling CONSTRUCT_REGIONS to get the coordinates from the existing source.stats file 
; (to avoid rounding errors from repeated conversion between IDL and FITS).
;
; For sources we have not extracted before, start with the nominal PSF fraction and
; set the initial adjustment to be a large downward step (so we can reasonably rapidly
; find our way to the minimum fraction for sources that need it.)
frac_step = replicate(initial_down_step, num_sources)

; For sources that we have extracted before, start with a slightly higher PSF fraction and
; set the initial adjustment to be an upward step of moderate size.
if (total(/INT, strmatch(column_names, 'FRACSPEC')) GT 0) then begin
  FRACSPEC = bt_initial.FRACSPEC * 100
  ind = where(finite(FRACSPEC) AND (FRACSPEC GT 0), count)
  if (count GT 0) then begin
    cat[ind].psf_frac = minimum_psf_frac > (FRACSPEC[ind] + minimum_step/2.0) < nominal_psf_frac
    frac_step[ind]    = initial_up_step
  endif
endif

fmt = '(A,1x,F10.6,1x,F10.6,1x,F5.3,1x,F7.5)'



;; =====================================================================
;; If desired, abort the processing if all the sources have fresh extractions.
if keyword_set(ignore_if_fresh) && (total(/INT, stregex(/BOOLEAN, column_names, 'POSNDATE|EXTRDATE')) EQ 2) then begin
  POSNDATE = strtrim(bt_initial.POSNDATE,2)
  EXTRDATE = strtrim(bt_initial.EXTRDATE,2)
  ; Any missing dates prevent us from concluding that extractions are fresh.
  if ((total(/INT, strmatch(POSNDATE,'')) + total(/INT, strmatch(EXTRDATE,''))) EQ 0) then begin
    was_ignored = 1
    for ii=0,num_sources-1 do begin
      ; Extraction is stale if position has a later timestamp.
      if date_conv(POSNDATE[ii],'R') GT date_conv(EXTRDATE[ii],'R') then begin
        was_ignored = 0
        break
      endif
    endfor ;ii
    if was_ignored then begin
      print, 'ae_make_catalog: All sources have up-to-date extractions in obsid ', obsname
      GOTO, CLEANUP      
    endif
  endif
endif ; keyword_set(ignore_if_fresh)



;; =====================================================================
;; Iteratively adjust PSF fractions to eliminate overlapping regions.
;; The strategy for avoiding infinite iteration is as follows:
;; Sources in conflict get smaller.
;; Sources not in conflict get larger.
;; Each source's step size is reduced when it changes direction.
;; When the step size is smaller than minimum_step the source is not adjusted.
;; Thus, it should be the case that the set of sources in play is steadily reduced.

; The dist_to_frac_tried data structure (101xnum_sources) keeps track of the distance to
; the nearest PSF fraction that has been tried so far.
row = abs(indgen(101) - nominal_psf_frac)
dist_to_frac_tried = rebin(reform(row, n_elements(row),  1,/OVERWRITE), n_elements(row), num_sources, /SAMPLE)

match_existing = 0

num_to_process = num_sources
sources_to_process  =                 lindgen(num_sources)

; Computing the OVERLAP property is expensive (a call to dmcopy) so we try to carefully cache them.
; We have to maintain a 2-D array of OVERLAP values for each possible pair of neighbors because a given source's "neighbor" can change during processing as the aperture sizes are adjusted!
overlap_cache       = replicate(!VALUES.F_NAN,num_sources,num_sources)

first_iteration_completed = 0
if keyword_set(restart) then begin
  restore, /V, model_savefile
  GOTO, RESTART
endif

repeat begin
  if first_iteration_completed then begin
    ; Save state in case something crashes.
    save, FILENAME=model_savefile
;   print, 'Saved state to ', model_savefile
  endif
  
RESTART:  
  ; Write a temp catalog holding only those sources that need AE processing on this pass.
  forprint, TEXTOUT=active_catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, SUBSET=sources_to_process, F=fmt, /NoCOMMENT, /SILENT
  
  ; Invalidate cached OVERLAP values that involve the sources we're about to reprocess.
  overlap_cache[sources_to_process,*] = !VALUES.F_NAN
  overlap_cache[*,sources_to_process] = !VALUES.F_NAN
  
  

  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running /CONSTRUCT_REGIONS on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  

  ; Pass a scaler as SOURCE_NOT_OBSERVED to ensure that it does NOT take the role of an input to the CONSTRUCT_REGIONS stage.
  active_source_not_observed = 0 
  acis_extract, active_catfile, obsname, evtfile, /CONSTRUCT_REGIONS, SOURCE_NOT_OBSERVED=active_source_not_observed, EMAP_FILENAME=emapfile, ASPECT_FN=aspect_fn, MASK_FRACTION=0.98, MASK_MULTIPLIER=1.0, REGION_ONLY=keyword_set(region_only), _EXTRA=extra

  
  ; Extract Events (to get SRC_CNTS statistic).
  ; The SOURCE_NOT_OBSERVED input is used below only for efficiency, to pass along identifications of off-field sources made by CONSTRUCT_REGIONS above.
  ; Pass /REGION_ONLY since we only care about PSF fraction of the primary energy.
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running /EXTRACT_EVENTS on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  
  acis_extract, active_catfile, obsname, evtfile, /EXTRACT_EVENTS, SOURCE_NOT_OBSERVED=active_source_not_observed, EMAP_FILENAME=emapfile, ASPECT_FN=aspect_fn, REUSE_NEIGHBORHOOD=(keyword_set(reuse_neighborhood) || first_iteration_completed), /REGION_ONLY, _EXTRA=extra

  
  ; Collate the active sources that we've just changed above, and merge with a collation of the full
  ; catalog that we maintain in "bt" (which must be the same length as the "cat" structure).
  ; We need the /SINGLE_OBS option so that SRC_CNTS comes from obs.stats rather than from source.photometry.  
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Running COLLATE stage on ' + active_catfile
  print,   F='(%"ae_make_catalog: ============================================================\n")'  
  acis_extract, active_catfile, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, MATCH_EXISTING=match_existing, VERBOSE=0, _EXTRA=extra
  match_existing = 1

  bt_active=mrdfits(collatefile, 1, /SILENT)
  if (n_elements(bt_active) NE n_elements(sources_to_process)) then message, 'BUG in ae_make_catalog!'

  if (n_elements(bt_active) EQ num_sources) then begin
    ; The entire catalog is "active" (e.g. on the first iteration), so just use the collated table.
    bt_all = bt_active
    
    ; The speedup trick of collating only the sources we've changed (above) leaves the 
    ; "neighbor" properties in bt_all with incorrect values that must recomputed.
    ; Here, we pre-compute the 2-D array of distances between source positions.
    make_2d, bt_all.X_CAT, bt_all.X_CAT, xpos_i, xpos_j
    make_2d, bt_all.Y_CAT, bt_all.Y_CAT, ypos_i, ypos_j
    
    distance_src2src = sqrt((xpos_i-xpos_j)^2. + (ypos_i-ypos_j)^2.)  
    xpos_i=0  & xpos_j=0  &  ypos_i=0  &  ypos_j=0
    
    src_num = lindgen(num_sources)
    distance_src2src[src_num,src_num] = 1E10
    
  endif else begin
    ; Otherwise, we have to merge the active table back into the full table.
    bt_all[sources_to_process] = bt_active
    
    ; Note that the speedup trick of collating only the sources we've changed (above) leaves the 
    ; "neighbor" properties in bt_all with corrupted values.  Thus we must recompute these here:
    ; For each source region, find which source has a region most overlapping.
    ; Note the the "neighbor" relationship is NOT symmetric, e.g. A's most overlapping neighbor may be B, and B's most overlapping neighbor may be C!    
    temp_src_radius       = bt_all.SRC_RAD
    temp_distance_src2src = fltarr(num_sources)
    temp_distance_reg2reg = fltarr(num_sources)
    temp_neighbor         = lonarr(num_sources)
    for ii = 0, num_sources-1 do begin
      temp_distance_src2src[ii] = min(/NAN, distance_src2src[*,ii])
      temp_distance_reg2reg[ii] = min(/NAN, distance_src2src[*,ii] - temp_src_radius - temp_src_radius[ii], ind)
      temp_neighbor[ii] = ind
    endfor   
        
    bt_all.distance_src2src = temp_distance_src2src    
    bt_all.distance_reg2reg = temp_distance_reg2reg
    bt_all.neighbor         = temp_neighbor
  endelse
  
  if (n_elements(bt_all) NE n_elements(cat)) then message, 'BUG in ae_make_catalog!'
  if (n_elements(bt_all) NE     num_sources) then message, 'BUG in ae_make_catalog!'

  
  ; Calculate an "overlap" metric for sources that are close enough to potentially have overlapping apertures.
  ; Calculate the fraction of events extracted by THIS source that also fall into the neighbor's aperture.
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print,        'ae_make_catalog: Calculating OVERLAP of crowded extraction regions.'
  print,   F='(%"ae_make_catalog: ============================================================\n")'  
; print, 'PFILES=', getenv('PFILES')  
  if (num_sources EQ 1) then overlap_cache[0] = 0
  for this_source = 0, num_sources-1 do begin
    neighbor_source = bt_all[this_source].neighbor
    
    ; Skip computation when overlap has already been computed.
    if finite(overlap_cache[this_source,neighbor_source]) then continue
      
    if bt_all[this_source].distance_reg2reg GE bt_all[this_source].SRC_RAD then begin
      ; The neighbor is too far to invest the time in looking for shared counts.
      overlap_cache[this_source,neighbor_source] = 0.
      continue
    endif
    
    this_env_events_fn = strtrim(bt_all[    this_source].OBSDIR,2) + env_events_basename
    this_region_fn     = strtrim(bt_all[    this_source].OBSDIR,2) + src_region_basename
    neighbor_region_fn = strtrim(bt_all[neighbor_source].OBSDIR,2) + src_region_basename

    ; Find area (in arbitrary units) of this source's extraction region.
    openw,  region_unit, temp_region_fn, /GET_LUN
    printf, region_unit, "# Region file format: DS9 version 3.0"
    
    ae_ds9_to_ciao_regionfile, this_region_fn, '/dev/null', /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y
    polygon = fltarr(2,n_elements(polygon_x))
    polygon[0,*] = polygon_x
    polygon[1,*] = polygon_y

    printf,   region_unit, 'polygon(' + strcompress(strjoin(string(polygon,F='(F8.2)'),","), /REMOVE) + ')', F='($,A0)'
    flush,    region_unit
    
    cmd = string(this_env_events_fn, temp_region_fn, temp_pi_fn, F="(%'dmextract ""%s[sky=region(%s)][bin pi]"" %s clobber=yes')")
    run_command, cmd
 
    this_region_area = sxpar(headfits(temp_pi_fn, EXT=1), 'BACKSCAL')
    
    ; Find area (in arbitrary units) of the intersection of this and the neighbor's extraction regions.
    ae_ds9_to_ciao_regionfile, neighbor_region_fn, '/dev/null', /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y
    polygon = fltarr(2,n_elements(polygon_x))
    polygon[0,*] = polygon_x
    polygon[1,*] = polygon_y

    printf,   region_unit, '&polygon(' + strcompress(strjoin(string(polygon,F='(F8.2)'),","), /REMOVE) + ')'
    free_lun, region_unit
     
    cmd = string(this_env_events_fn, temp_region_fn, temp_pi_fn, F="(%'dmextract ""%s[sky=region(%s)][bin pi]"" %s clobber=yes')")
    run_command, cmd
 
    intersection_area = sxpar(headfits(temp_pi_fn, EXT=1), 'BACKSCAL')
    
    ; CIAO 4.0 seems to have a bug.  If the intersection of the regions is null, it returns BACKSCAL=1.0 (instead of 0).
    if (intersection_area EQ 1.0) then intersection_area = 0.0
        
    overlap_cache[this_source,neighbor_source]  = intersection_area / this_region_area
  endfor

  cat.overlap = overlap_cache[lindgen(num_sources),bt_all.neighbor]
  
  
  ; Estimate the number of counts not extracted for each source.
  counts_lost = (bt_all.SRC_CNTS/bt_all.FRACSPEC) - bt_all.SRC_CNTS
  
  
  ; The relationship of overlapping regions can be complex.  I think it's essential to process the 
  ; sources sequentially rather than via vector operations.
  ; Note the the "neighbor" relationship is NOT symmetric, e.g. A's most overlapping neighbor may be B, and B's most overlapping neighbor may be C!    

  ; The code below seeks to choose the "sign" of each source's next PSF fraction adjustment:
  ; step_sign =  0: Frac is not changing; source omitted from AE run.
  ; step_sign =  1: Frac is scheduled for increase.
  ; step_sign = -1: Frac is scheduled for reduction.
  step_sign = replicate(0,num_sources)
  
  for ii=0, num_sources-1 do begin
    if (~finite(cat[ii].overlap)) then message, 'BUG in ae_make_catalog!'
    
    if (cat[ii].overlap GT 0.) then begin
      ; This source region is sharing counts with that of the neighbor jj.
      jj = bt_all[ii].neighbor
      
      ; If either is already scheduled for reduction then this overlap is being addressed and there's no more work to be done.
      if ((step_sign[ii] EQ -1) OR (step_sign[jj] EQ -1)) then continue

      ; Since the neighbor and thus overlap properties of sources are NOT symmetric, it is possible that one of these sources (ii or jj) is already scheduled for an INCREASE.  We ignore such a proposal and allow it to be scheduled for decrease below.
      
      ; We need to try to schedule one of them to be reduced.
      ; Even if our step size is at the minimum, we will shrink a region to avoid overlap.
      ii_can_reduce = (cat[ii].psf_frac GT minimum_psf_frac)
      jj_can_reduce = (cat[jj].psf_frac GT minimum_psf_frac)
      
      if (ii_can_reduce AND jj_can_reduce) then begin
        ; Use counts_lost to choose which should reduce.
        if            (counts_lost[ii] GT counts_lost[jj]) then begin
          step_sign[ii] =  0 ; stay put
          step_sign[jj] = -1 ; reduce
        endif else if (counts_lost[jj] GT counts_lost[ii]) then begin
          step_sign[jj] =  0 ; stay put
          step_sign[ii] = -1 ; reduce
        endif else begin
          ; When counts_lost estimates are equal (e.g. both zero) then reduce the larger PSF fraction.
          if (cat[ii].psf_frac LT cat[jj].psf_frac) then begin
            step_sign[ii] =  0 ; stay put
            step_sign[jj] = -1 ; reduce
          endif else begin
            step_sign[jj] =  0 ; stay put
            step_sign[ii] = -1 ; reduce
          endelse
        endelse
      endif else if ((NOT ii_can_reduce) AND (NOT jj_can_reduce)) then begin
        ; Overlap cannot be fixed; both stay put.
        step_sign[ii] = 0 ; stay put
        step_sign[jj] = 0 ; stay put
      endif else if (     ii_can_reduce  AND (NOT jj_can_reduce)) then begin
        step_sign[jj] =  0 ; stay put
        step_sign[ii] = -1 ; reduce
      endif else if ((NOT ii_can_reduce) AND      jj_can_reduce ) then begin
        step_sign[ii] =  0 ; stay put
        step_sign[jj] = -1 ; reduce
      endif else message, 'Bug in logic.'
    endif else begin
      ; NOT overlapping with anything, so consider increasing.
      ; Since the neighbor and thus overlap properties of sources are NOT symmetric, it is possible that one of these sources (ii or jj) is already scheduled for a DECREASE.  We respect such a proposal and do NOT scheduled it for an increase here.
      
      ; If we're at the minimum step size then stop trying to increase.  This criterion is what terminates the adjustment of a source.

      if (step_sign[ii] EQ 0) && (cat[ii].psf_frac LT nominal_psf_frac) && (abs(frac_step[ii]) GT minimum_step) then step_sign[ii] = 1
    endelse
  endfor ;ii
  
  
  ; The direction of the adjustment desired in this iteration is carried in step_sign.
  ; The actual PSF fraction step to take is carried in frac_step, which is a signed quantity.
  ; When the direction of adjustment reverses we reduce the step size.  
  reversing_ind = where( frac_step*step_sign LT 0, count )
  if (count GT 0) then frac_step[reversing_ind] = (-frac_step[reversing_ind]/step_ratio)
  
  ; Adjust the PSF fractions in the catalog.
  sources_to_process = where(step_sign NE 0, num_to_process)

  if (num_to_process EQ 0) then break
  previous_cat_psf_frac = cat.psf_frac
  
  ; For each source we're adjusting, the interval between cat.psf_frac and cat.psf_frac + frac_step 
  ; defines a range of new PSF fractions that are reasonable to take.
  ; For efficiency, we add the further criterion that we will choose the value in that range that
  ; is farthest from any PSF fraction we have already tried for this source.
  for ii=0, num_to_process-1 do begin
    ind = sources_to_process[ii]
    ; Define the range of PSF fractions (expressed here as integer percentages) that are under consideration.
    if (frac_step[ind] GT 0) then begin
      ; Interval extends to the right; clip at nominal_psf_frac.
      min_frac = nominal_psf_frac < (cat[ind].psf_frac + 1             )
      max_frac = nominal_psf_frac < (cat[ind].psf_frac + frac_step[ind])
    endif else begin
      ; Interval extends to the left; clip at minimum_psf_frac.
      max_frac =     minimum_psf_frac > (cat[ind].psf_frac - 1             )
      min_frac =     minimum_psf_frac > (cat[ind].psf_frac + frac_step[ind])    
    endelse                     
    
    ; Select the best candidate in that range.
    dum = max(dist_to_frac_tried[min_frac:max_frac, ind], imax)
    cat[ind].psf_frac = min_frac + imax
    
    ; Update the dist_to_frac_tried data structure.
    dist_to_frac_tried[0,ind] = dist_to_frac_tried[*,ind] < abs(indgen(101) - cat[ind].psf_frac) 
  endfor ;ii                                                                  
  
  ; Defensively range check the catalog and loop to call AE.
  cat.psf_frac = minimum_psf_frac > cat.psf_frac < nominal_psf_frac
  
  print, F='(%"\nae_make_catalog: ============================================================")'  
  print, num_to_process, F='(%"ae_make_catalog: %d crowded sources are being reprocessed ...")'
  forprint, cat.sourcename, bt_all.label, (cat.psf_frac-previous_cat_psf_frac)/100., cat.psf_frac/100., SUBSET=sources_to_process, F='(%"%s (%s): frac stepped by %5.2f to %4.2f")'

  first_iteration_completed = 1
endrep until (0)


;; =====================================================================
; Write out the full catalog with RA & DEC filled in so it serves as an archive.
cat.RA  = bt_all.RA
cat.DEC = bt_all.DEC
forprint, TEXTOUT=catfile, cat.sourcename, cat.ra, cat.dec, cat.psf_frac/100., cat.psf_energ, F=fmt, /NoCOMMENT, /SILENT
print, 'ae_make_catalog: Wrote catalog ', catfile


;; =====================================================================
print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Saving OVERLAP values for each extraction region'
print,   F='(%"ae_make_catalog: ============================================================\n")'  
ae_poke_source_property, catfile, obsname, KEYWORD='OVERLAP', VALUE=cat.overlap, COMMENT='fraction of my counts in neighbor aperture', _EXTRA=extra


; Report pairs of sources that significantly overlap.
report_flag = cat.overlap GT 0.1
for ii=0,num_sources-1 do $
  if report_flag[ii] then report_flag[bt_all[ii].neighbor] = 0

ind = where(report_flag, num_crowded)
if (num_crowded GT 0) then begin
  neighbor_label = (bt_all.LABEL)       [bt_all.neighbor]
  neighbor_name  = (bt_all.CATALOG_NAME)[bt_all.neighbor]
  ind = ind[reverse(sort((cat.overlap)[ind]))]
  print, num_crowded, F='(%"\nae_make_catalog: WARNING!  these %d pairs of sources remain severely crowded (OVERLAP > 0.1).")'
  forprint, bt_all.LABEL, bt_all.CATALOG_NAME, neighbor_name, neighbor_label, cat.overlap, SUBSET=ind, F='(%"%12s  %s <> %s  %12s  %7.1f")' 
endif


;; =====================================================================
;; Collate full catalog, and generate a master region file.
;; We cannot supply MATCH_EXISTING for speedup because we have added the property OVERLAP above.
print, F='(%"\nae_make_catalog: ============================================================")'  
print,        'ae_make_catalog: Running COLLATE stage on the full catalog'
print,   F='(%"ae_make_catalog: ============================================================\n")'  
acis_extract, catfile, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, REGION_FILE=regionfile, VERBOSE=0, _EXTRA=extra


; Show the regions in ds9.
cmd = string(evtfile, regionfile, F='(%"ds9 -log %s -region %s &")')
if show then spawn, cmd

if file_test(tempdir) then begin
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, tempdir
endif


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
  print, 'ae_make_catalog: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end ; ae_make_catalog



;#############################################################################
;;; Perform a standard extraction on a single obsid.
;;;
;;; Several input files are located using standard naming conventions as shown 
;;; in the Getting Started section of the AE manual.
;;;
;;; The extraction regions must have been constructed previously, e.g. with ae_make_catalog.
;;;
;;; The required parameter EVTFILE_BASENAME specifies the name of the observation event list file in
;;; the directory ../obsXXXX/.  In our recipe we use EVTFILE_BASENAME='validation.evt' for passes that 
;;; prune and reposition sources, and use EVTFILE_BASENAME='spectral.evt' for the photometry/fitting extraction.
;;;
;;; If /BETTER_BACKGROUNDS is specified, then the ae_better_backgrounds tool is used to build bkg spectra.

;;; If BETTER_BACKGROUNDS is omitted, then a masked background emap and a masked event list are constructed by applying 
;;; the mask region file ../obsXXXX/mask.reg, typically created by ae_make_catalog and containing circular masks from the 
;;; /CONSTRUCT_REGIONS stage, plus any mask regions supplied via EXTRA_MASKFILE.
;;; The AE EXTRACT_BACKGROUNDS stage is then used to build bkg spectra for each source.

PRO ae_standard_extraction, obsname, EVTFILE_BASENAME=evtfile_basename, BETTER_BACKGROUNDS=better_backgrounds, $
    SRCLIST_FILENAME=srclist_fn, EXTRACTION_NAME=extraction_name, $
    SOURCE_NOT_OBSERVED=source_not_observed, $
    EXTRA_MASKFILE=extra_maskfile, REUSE_MASKING=reuse_masking, $
    EXTRACT_SPECTRA=extract_spectra, TIMING=timing, EXTRACT_BACKGROUNDS=extract_backgrounds,  $
    _EXTRA=extra

exit_code = 0
creator_string = "ae_standard_extraction, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

if (n_elements(extract_spectra) EQ 0)     then extract_spectra     = 1
if (n_elements(timing) EQ 0)              then timing              = 1
if (n_elements(extract_backgrounds) EQ 0) then extract_backgrounds = 1

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
;print, 'Using temporary directory: ', tempdir

temp_events_fn   = tempdir + 'temp.evt'

if keyword_set(extract_backgrounds) && ~keyword_set(better_backgrounds) then begin
  ;; Save any existing PFILES environment variable and establish access to CIAO.
;  print, F='(%"\nae_standard_extraction: ============================================================")' 
;  print, 'ae_standard_extraction: Initializing access to CIAO.'
;  print, F='(%"ae_standard_extraction: ============================================================\n")'
  inherited_pfiles = getenv("PFILES")
  run_command, /INIT, PARAM_DIR=tempdir
endif

if ~keyword_set(evtfile_basename) then begin
  print, 'ERROR: parameter EVTFILE_BASENAME must be supplied'
  GOTO, FAILURE
endif

if ~keyword_set(source_not_observed) then source_not_observed = 0

if ~keyword_set(srclist_fn)      then srclist_fn = 'all.srclist'
if keyword_set(extract_backgrounds) && (srclist_fn NE 'all.srclist') then $
  print, 'WARNING: Background estimation may be unreliable when only a subset of full catalog is processed.'

if (n_elements(extraction_name) EQ 0) then extraction_name=''

if ~keyword_set(obsname) then begin
  print, 'ERROR: parameter "obsname" must be supplied'
  GOTO, FAILURE
endif

if (size(obsname,/TNAME) NE 'STRING') then begin
  print, 'ERROR: parameter "obsname" must be a string'
  GOTO, FAILURE
endif


obsdir          = '../obs' + obsname + '/'

evtfile         = obsdir + evtfile_basename
bkg_evtfile     = obsdir + 'background.evt'
emapfile        = obsdir + 'obs.emap'
bkg_emapfile    = obsdir + 'background.emap'

ardlib_filename = obsdir + 'ardlib.par'
pbk_filename    = obsdir + 'obs.pbkfile'
msk_filename    = obsdir + 'obs.mskfile'
aspect_fn       = obsdir + 'obs.asol'
maskfile        = obsdir + 'mask.reg'
collatefile     = obsdir + 'all.collated'

if NOT file_test(pbk_filename) then pbk_filename = 'NONE'

if keyword_set(extract_backgrounds) && ~keyword_set(better_backgrounds) && ~keyword_set(reuse_masking) then begin
  ;; =====================================================================
  ;; Make masked background event list & emap using the masks.
  print, F='(%"\nae_standard_extraction: ============================================================")' 
  print, 'ae_standard_extraction: Constructing masked background emap and event list'
  print, F='(%"ae_standard_extraction: ============================================================")'
  
  
  ; Build region file containing all the masks.
  if keyword_set(extra_maskfile) then begin
    print, 'Spawning ds9 to perform coordinate conversions on EXTRA_MASKFILE ...'
    ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name
    ae_send_to_ds9, my_ds9, emapfile
  
    ;; Load region file into ds9 and resave in PHYSICAL coordinates.
    cmd = strarr(4)
    cmd[0] = string(my_ds9,                 F='(%"xpaset -p ''%s'' regions delete all")')
    cmd[1] = string(my_ds9, extra_maskfile, F='(%"xpaset -p ''%s'' regions load %s")')
    cmd[2] = string(my_ds9,                 F='(%"xpaset -p ''%s'' regions system physical")')
    cmd[3] = string(my_ds9, maskfile,       F='(%"xpaset -p ''%s'' regions save %s")')
    run_command, cmd, /QUIET
  endif else begin
    cmd = string(maskfile,       F="(%'echo ""# Region file format: DS9 version 3.0"" >! %s')")
  endelse
  run_command, cmd, /UNIX
    
  cmd = string(obsname,maskfile, F="(%'grep -h background */%s/extract.reg           >>! %s')")
  run_command, cmd, /UNIX
  
  ;; CIAO (Jan 2008) has a bug when update=no is used with exclude which results in cropping of the output image.
  ;; As a workaround we add small regions at the corners of the emap.
  emap      = readfits(emapfile, emap_header)
  extast, emap_header, emap2wcs_astr
  emap_col_dim = (size(emap, /DIM))[0]
  emap_row_dim = (size(emap, /DIM))[1]
  crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
  crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
  cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]
  
  x_sky = crvalP[0] + cdeltP[0] * (([0,emap_col_dim-1]+1) - crpixP[0])
  y_sky = crvalP[1] + cdeltP[1] * (([0,emap_row_dim-1]+1) - crpixP[1])

  openw, unit, maskfile, /GET_LUN, /APPEND
  !TEXTUNIT = unit
  forprint, TEXTOUT=5, x_sky, y_sky, F='(%"circle(%f,%f,1) # tag={bug workaround}")', /NoCOMMENT
  free_lun, unit
  
  
  ; Apply masks to emap.
  print
  print, 'The following dmcopy can run a while on large catalogs.'
 
  cmd = string(emapfile, maskfile, bkg_emapfile, F="(%'dmcopy ""%s[exclude sky=region(%s)][opt full,update=no]"" %s clobber=yes')")
  run_command, cmd
    
    ; Discard events where emap is zero.
  cmd = string(evtfile, bkg_emapfile, temp_events_fn, F="(%'dmimgpick ""%s[cols time,ccd_id,chip,det,sky,pi,energy]"" %s %s method=closest clobber=yes')")
  run_command, cmd
  
  cmd = string(temp_events_fn, bkg_evtfile, F="(%'dmcopy ""%s[#8>1]"" %s clobber=yes')")
  run_command, cmd
    
  cmd = string(bkg_evtfile, maskfile, bkg_emapfile, maskfile, F="(%'ds9 -tile -log %s -region %s -linear %s -region %s -zoom to fit -match frames wcs &')")
  run_command, cmd
endif ; constructing background emap and event list



if keyword_set(extract_spectra) then begin
  ;; =====================================================================
  ;; Extract Spectra.
  print, F='(%"\nae_standard_extraction: ============================================================")' 
  print, 'ae_standard_extraction: Running /EXTRACT_SPECTRA stage'
  print, F='(%"ae_standard_extraction: ============================================================\n")'
  acis_extract, srclist_fn, obsname, evtfile, /EXTRACT_SPECTRA, EMAP_FILENAME=emapfile, ASPECT_FN=aspect_fn, ASPHIST_DIR=obsdir+'asphist', ARDLIB_FILENAME=ardlib_filename, PBKFILE=pbk_filename, MSKFILE=msk_filename, EXTRACTION_NAME=extraction_name, SOURCE_NOT_OBSERVED=source_not_observed, _EXTRA=extra
endif

if keyword_set(timing) then begin
  ;; =====================================================================
  ;; Timing Analysis.
  print, F='(%"\nae_standard_extraction: ============================================================")'
  print, 'ae_standard_extraction: Running /TIMING stage'
  print, F='(%"ae_standard_extraction: ============================================================\n")'
  acis_extract, srclist_fn, obsname, /TIMING, EXTRACTION_NAME=extraction_name, SOURCE_NOT_OBSERVED=source_not_observed, _EXTRA=extra
endif
  
if keyword_set(extract_backgrounds) then begin
  ;; =====================================================================
  ;; Extract background for each source.
  ;; We omit any EXTRACTION_NAME supplied so the bkg spectrum will be in the
  ;; generic (not named) directory.
  if keyword_set(better_backgrounds) then begin
  print, F='(%"\nae_standard_extraction: ============================================================")' 
    print, 'ae_standard_extraction: Running ae_better_backgrounds tool'
    print, F='(%"ae_standard_extraction: ============================================================\n")'
    ae_better_backgrounds, obsname, EVTFILE_BASENAME=evtfile_basename, SRCLIST_FILENAME=srclist_fn,  SOURCE_NOT_OBSERVED=source_not_observed, _EXTRA=extra
  endif else begin
    print, F='(%"\nae_standard_extraction: ============================================================")' 
    print, 'ae_standard_extraction: Running /EXTRACT_BACKGROUNDS stage'
    print, F='(%"ae_standard_extraction: ============================================================\n")'
    acis_extract, srclist_fn, obsname, bkg_evtfile, /EXTRACT_BACKGROUNDS, EMAP_FILENAME=bkg_emapfile, SOURCE_NOT_OBSERVED=source_not_observed, _EXTRA=extra
  endelse
endif

;; =====================================================================
;; Collate results.
; Commented out to reduce run times.
;print, F='(%"\nae_standard_extraction: ============================================================")' 
;print, 'ae_standard_extraction: Running COLLATE stage'
;print, F='(%"ae_standard_extraction: ============================================================\n")'
;acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, EXTRACTION_NAME=extraction_name, VERBOSE=0

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
  print, 'ae_standard_extraction: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP

end ; ae_standard_extraction





;#############################################################################
;;; Apply "better" source masking to construct a background event list and emap.
;;; Then (optionally) re-extract the background for every source.

;;; This tool tries to mask pixels on the sky where the estimated flux from point sources is significant compared to the 
;;; estimated non-point source flux ( point_source_flux / diffuse_flux  >  THRESHOLD ).
;;; The goal is to get bright stars more heavily masked than weak ones.

;;; This tool uses the first part of the ae_better_backgrounds tool to build models of the point sources for each ObsId 
;;; (in the files obsXXXX/ae_better_backgrounds.sav).
;;; If you already have those models from running ae_better_backgrounds then supply /REUSE_MODELS to re-use them.

;;; To build those models, ae_better_backgrounds must estimate photometry for each source.
;;; Thus, your entire catalog must already have been extracted, including backgrounds for each source.  
;;; The ae_better_backgrounds tool will then run the MERGE_OBSERVATIONS stage on a SINGLE obsid to estimate fluxes for each source.
;;; Note that a source might have no net counts in any single observation.

;;; With those point source models in hand, we have a model of the point-source flux in every pixel on the sky.
;;; We also need a way to estimate the background flux in every pixel (to decide if that pixel needs to be masked).
;;; We use code from our adaptive smoothing tool for this.

;;; Once a sky mask is defined, it is applied to the event list file in the directory ../obsXXXX/ specified by the 
;;; required parameter EVTFILE_BASENAME to produce obsXXXX/background.evt.

PRO ae_better_masking, obsname, EVTFILE_BASENAME=evtfile_basename, THRESHOLD=threshold, $

    VERBOSE=verbose,  $

    BACKGROUND_MODEL_FILENAME=background_model_filename, $

    SOURCE_NOT_OBSERVED=source_not_observed, GENERIC_RMF_FN=generic_rmf_fn, $
  
    SRCLIST_FILENAME=srclist_fn,  EXTRACTION_NAME=extraction_name, EMAP_BASENAME=emap_basename, $

		MIN_NUM_CTS=min_counts, EXTRA_MASKFILE=extra_maskfile, $
		
    REUSE_MODELS=reuse_models, SKIP_EXTRACT_BACKGROUNDS=skip_extract_backgrounds, _EXTRA=extra

  
exit_code = 0
creator_string = "ae_better_masking, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

if NOT keyword_set(threshold)        then threshold = 0.1
if NOT keyword_set(min_counts)       then min_counts=100
if ~keyword_set(evtfile_basename) then begin
  print, 'ERROR: parameter EVTFILE_BASENAME must be supplied'
  GOTO, FAILURE
endif
if NOT keyword_set(emap_basename)    then emap_basename    = 'obs.emap'

if ~keyword_set(srclist_fn) then srclist_fn = 'all.srclist'


obsdir       = '../obs' + obsname + '/'
srclist_fn   = 'all.srclist'

evtfile      = obsdir + evtfile_basename
emapfile     = obsdir + emap_basename
bkg_evtfile  = obsdir + 'background.evt'

bkg_emapfile = obsdir + 'background.emap'

regionfile      = obsdir + 'extract.reg'
wing_flux_fn    = obsdir + 'wing_flux.img' 
wing_counts_fn  = obsdir + 'wing_counts.img'
bkg_countsfile  = obsdir + 'bkg_counts.img'

collatefile     = obsdir + 'all.collated'

obs_stats_basename       = 'obs.stats'
src_region_basename      = 'extract.reg'
psf_basename             = 'source.psf'
bkg_spectrum_basename    = 'background.pi'
rmf_basename               = 'source.rmf'
arf_basename               = 'source.arf'

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
repeat begin
  session_name = string(random()*1E4, F='(I4.4)')
  tempdir = 'AE' + session_name +'.noindex/'
  tempdir = filepath(tempdir, /TMP)
endrep until (NOT file_test(tempdir))
file_mkdir, tempdir
;print, 'Using temporary directory: ', tempdir

;; Save any existing PFILES environment variable and establish access to CIAO.
inherited_pfiles = getenv("PFILES")
run_command, /INIT, PARAM_DIR=tempdir

temp_events_fn   = tempdir + 'temp.evt'
temp_region_fn   = tempdir + 'temp.reg'
temp_image_fn    = tempdir + 'temp.img'



;; =====================================================================
;; BUILD AN IMAGE THAT MODELS THE LIGHT EXPECTED FROM ALL THE POINT SOURCES
savefile_basename = 'ae_better_backgrounds.sav'

if ~keyword_set(reuse_models) then begin
  ae_better_backgrounds, obsname, /BUILD_MODELS_ONLY, EVTFILE_BASENAME=evtfile_basename, $
    
      BACKGROUND_MODEL_FILENAME=background_model_filename, $
    
      SOURCE_NOT_OBSERVED=source_not_observed, GENERIC_RMF_FN=generic_rmf_fn, $
      
      SRCLIST_FILENAME=srclist_fn, EXTRACTION_NAME=extraction_name, $
      EMAP_BASENAME=emap_basename, $
  
      SAVEFILE_BASENAME=savefile_basename, $
      VERBOSE=verbose,  $
      
      _EXTRA=extra
endif

; The key variables computed by ae_better_backgrounds that we are restoring below are:
;   emap, emap_header, emap2wcs_astr, emap_col_dim, emap_row_dim, skypix_per_emappix:
;     Exposure map and related information.

;   bt: 
;     Collation on the observed sources, including photometry (NET_CNTS)

;   observation_counts_model_img:
;     Image (on the same pixel grid as emap) modeling the point source counts expected to be observed.
;   
model_savefile     = obsdir + savefile_basename
restore, /V, model_savefile

CATALOG_NAME = strtrim(bt.CATALOG_NAME,2)
NET_CNTS     = bt.NET_CNTS[0]
EXPOSURE     = bt.EXPOSURE
ENERG_LO     = bt[0].ENERG_LO[0]
ENERG_HI     = bt[0].ENERG_HI[0]
X_CAT        = bt.X_CAT
Y_CAT        = bt.Y_CAT

num_sources = n_elements(bt)


if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)


;; =====================================================================
;; Convert our model of the point source counts expected to be observed into flux units.
;; This is necessary because our masking algorithm is comparing a background flux estimate to this point source flux model.
star_counts = temporary(observation_counts_model_img)
star_flux = star_counts / emap



 
;; =====================================================================
;; Any region supplied by the observer via EXTRA_MASKFILE defines our initial mask.
if keyword_set(extra_maskfile) then begin
  print, 'Spawning ds9 to perform coordinate conversions on EXTRA_MASKFILE ...'
  ae_send_to_ds9, my_ds9, NAME='acis_extract_'+session_name
  ae_send_to_ds9, my_ds9, emapfile

  ;; Load region file into ds9 and resave in PHYSICAL coordinates.
  cmd = strarr(4)
  cmd[0] = string(my_ds9,                 F='(%"xpaset -p ''%s'' regions delete all")')
  cmd[1] = string(my_ds9, extra_maskfile, F='(%"xpaset -p ''%s'' regions load %s")')
  cmd[2] = string(my_ds9,                 F='(%"xpaset -p ''%s'' regions system physical")')
  cmd[3] = string(my_ds9, temp_region_fn, F='(%"xpaset -p ''%s'' regions save %s")')
  run_command, cmd, /QUIET

  cmd1 = string(emapfile, temp_region_fn, temp_image_fn, F="(%'dmcopy ""%s[sky=region(%s)][opt full,update=no]"" %s clobber=yes')")
  
  run_command, cmd1

  star_mask = (readfits(temp_image_fn) GT 0)
endif else begin
  star_mask = bytarr(emap_col_dim,emap_row_dim)
endelse


;; =====================================================================
;; We are going to explicitly mask one pixel at each source position, mostly as a visual reminder of where the sources lie.
;; We cannot use xy2ad.pro to convert sky to array index systems.

crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]

for ii = 0, num_sources-1 do begin
  xind_catalog = round((crpixP[0] + (X_CAT[ii]-crvalP[0])/cdeltP[0]) - 1)
  yind_catalog = round((crpixP[1] + (Y_CAT[ii]-crvalP[1])/cdeltP[1]) - 1)
  
  star_mask[xind_catalog,yind_catalog] = 1
endfor ;ii


;; =====================================================================
;; Apply star_mask to an in-band image of the data.

print, 'energy band is ', ENERG_LO, ENERG_HI
run_command, string(emapfile, F="(%'get_sky_limits %s verbose=0 precision=3')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec

if (filterspec EQ '') then message, 'ERROR running get_sky_limits'

cmd = string(evtfile, 1000*[ENERG_LO,ENERG_HI], filterspec, temp_image_fn, F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin %s]"" %s clobber=yes')")
run_command, cmd  

masked_data = readfits(temp_image_fn)
masked_emap = emap

total_star_counts = total(star_counts, /DOUBLE)
print, 100*total_star_counts/total(/INTEGER,masked_data), total_star_counts, F='(%"Stellar models explain %5.1f%% (%d) of the detected events.")'

mask_ind = where(star_mask, mask_count)
masked_data[mask_ind] = 0
masked_emap[mask_ind] = 0
help, star_mask, masked_data, masked_emap


;; =====================================================================
;; Iteratively apply additional masking of pixels where flux from stars
;; is large compared to flux from background.
;; Much of this code is taken from adaptive_density_2d.pro.

; As explained in the header comments of adaptive_density_2d.pro
; we crop the data image where exposure is <10% nominal to 
; avoid artifacts in the bkg_flux image. 
; 
; For the same reasons we'll also add those low-exposure field edges 
; to the mask we're making for the background emap & event list.
; If the off-field threhold is set too high, then you'll mask out pixels in the hole at the center of the emap.
off_field = where(emap LT (0.05 * max(emap)), off_field_count)
print, off_field_count, ' emap pixels masked for <10% nominal exposure'
if (off_field_count GT 0) then begin
  star_mask  [off_field] =  1
  masked_data[off_field] =  0
  masked_emap[off_field] =  0
endif

; We can save considerable time by telling adaptive_density_2d to only compute
; background estimates where we need them, which is 
; (a) pixels that are not already masked
; (b) pixels where star_flux is nonzero.  
; The other pixels in bkg_flux will come back with the value NaN.
field_mask = (star_mask EQ 0) AND (star_flux GT 0)

max_radius = 500
print, threshold, F='(%"Masking pixels where star_flux/bkg_flux > %f")'

;; It is vital to the operation of the search below that the radii
;; list starts with r=0 (a kernel consisting of only the central pixel).
;; We have to limit the number of kernels to avoid excessive memory requirements.
skip = 0
repeat begin
  skip++
  radii = skip * indgen(1+(max_radius/skip))
  max_radius_index = n_elements(radii)-1
  num_kernels      = n_elements(radii)
endrep until (num_kernels LT 100)

if NOT keyword_set(silent) then print, 'Using kernel radii: ', radii

kernel_footprint, kf_id, /CREATE, IMAGE=masked_data, RADII=radii

print, F='(%"Finished building Tophat kernels.")'
size_spec = size(masked_data)
bright_count = 0L
initial_radius_index = 0 

bkg_flux = make_array( SIZE=size(star_flux), VALUE=!VALUES.f_nan )
help, bkg_flux

repeat begin
  masking_done = 1
  
  ; Find indexes of pixels we're considering masking.
  field_ind = where(field_mask, num_pixels)
  
  ; Sort them by their star_flux.
  sorted_index1D = field_ind[ reverse( sort( star_flux[field_ind] ) ) ]
  print, num_pixels, F='(%"Examining star_flux/bkg_flux in %d pixels ...")'

  checkpoint_rows = round([.2,.4,.6,.8]*num_pixels) > 1
  start_time = systime(1)
  
  for ii = 0L,num_pixels-1 do begin
    if (ii EQ checkpoint_rows[0]) then begin
      elapsed_time = (systime(1)-start_time)
      estimated_total_time = num_pixels * (elapsed_time/checkpoint_rows[0])

      print, checkpoint_rows[0], ceil((estimated_total_time-elapsed_time)/60.),$
	     F='(%"%d pixels processed; estimate %d more minutes to finish")'
      checkpoint_rows = shift(checkpoint_rows,-1)
    endif

    index1D = sorted_index1D[ii]
    index_to_point, index1D, xx, yy, size_spec
    
    ; Find the background level at this pixel's location by searching for a 
    ; circular region that contains min_counts counts.
    ; Arrange for the search to have an efficient starting point and direction.
    radius_index     = initial_radius_index
    search_direction = 0 ;just started
    search_done = 0
;help,ii
    repeat begin
;help,radius_index
      ;; Retrieve the 1-D indexes of pixels that fall under the kernel.
      ;; We're doing Top Hat kernels and simply looking for min_counts
      ;; counts so we ignore the "weight" values returned by kernel_footprint.
      kernel_footprint, kf_id, xx, yy, radius_index, pixel_list
 
      counts = total(/INTEGER,masked_data[pixel_list])
      significance_is_good = (counts GE min_counts)
      
      if significance_is_good then begin
        if (search_direction EQ 1) then begin
          ; We were searching UP from bad kernels and found a good one, so
          ; stop and keep this kernel.
          search_done = 1 
        endif else begin
          ; We just started, or were searching down and found a good one, so
          ; we need to keep going down, if possible.
          if (radius_index LE 0) then begin
            search_done = 1 
          endif else begin
            search_direction = -1 ; down
            radius_index     = radius_index - 1
          endelse
        endelse
        
      endif else begin
        if (search_direction EQ -1) then begin
          ; We were searching DOWN from good kernels and found a bad one, so
          ; stop and keep the next larger kernel.
          radius_index++
          search_direction = 1 ;up
        endif else begin
          ; We just started (search_direction==0), or were searching up and found a bad one,
          ; so we need to keep going up, if possible.
          if (radius_index GE max_radius_index) then begin
            print, 'WARNING: search truncated at max kernel radius'
            search_done = 1 
          endif else begin
            search_direction = 1 ;up
            radius_index++
          endelse
        endelse
      endelse ; significance is bad

    endrep until search_done
    
    ;; Save the next smaller kernel as the starting point for the next pixel.
    ;; The way the search above works, if the starting kernel turns out to
    ;; be the one we're looking for, then we must step down one kernel and 
    ;; then back up, wasting time.  If we start just below the goal, then
    ;; we make one step and we're done.
    initial_radius_index = (radius_index - 1) > 0

    exposure = total(masked_emap[pixel_list])
    bkg_flux[index1D] = counts /exposure
    
    if ((star_flux[index1D]/bkg_flux[index1D]) GT threshold) then begin
;     print, masked_data[index1D], xx, yy, F='(%"masked %d counts in pixel (%4d,%4d)")' 
      masking_done = 0
      bright_count++
      star_mask  [index1D] = 1
      masked_data[index1D] = 0
      masked_emap[index1D] = 0
      field_mask [index1D] = 0
    endif
    
  endfor

endrep until masking_done
print, bright_count, ' pixels added to mask due to stellar contamination'
kernel_footprint, kf_id, /DESTROY


;; =====================================================================
;; Save results.

mask_ind = where( star_mask, mask_count )
star_counts[mask_ind] = 0
star_flux  [mask_ind] = 0
print, mask_count, ' total emap pixels masked'

; Consistency check.
dum = where(masked_emap[mask_ind] NE 0, count)
if (count GT 0) then message, 'ERROR: mask_ind inconsistent with masked_emap'

;  Multiply by emap to get background counts model.
bkg_counts  = bkg_flux  * emap
                          
writefits, bkg_emapfile,   masked_emap, emap_header
writefits, wing_flux_fn,   star_flux,   emap_header
writefits, wing_counts_fn, star_counts, emap_header
writefits, bkg_countsfile, bkg_counts,  emap_header


;; =====================================================================
;; Background event list is made by discarding events where background emap is zero.
                
cmd2 = string(evtfile, bkg_emapfile, temp_events_fn, F="(%'dmimgpick ""%s[cols time,ccd_id,chip,det,sky,pi,energy]"" %s %s method=closest clobber=yes')")

cmd3 = string(temp_events_fn, bkg_evtfile, F="(%'dmcopy ""%s[#8>1]"" %s clobber=yes')")
run_command, [cmd2,cmd3]
  

;; =====================================================================
cmd = string(bkg_emapfile, regionfile, keyword_set(extra_maskfile) ? '-region '+extra_maskfile : '', bkg_evtfile, 1000*[ENERG_LO,ENERG_HI],  wing_counts_fn, bkg_countsfile, F="(%'ds9 -tile -log %s -region %s %s -bin factor 4 ""%s[energy>%d && energy<%d]""  %s %s -zoom to fit -match frames wcs &')")
run_command, cmd

print
print, 'The 4 panels in ds9 are'

print, bkg_emapfile,                                                        F='(%"  %s shows the masked exposure map.")'

print, bkg_evtfile, ENERG_LO, ENERG_HI,                                     F='(%"  %s shows the masked event list (%0.1f:%0.1f keV).")'

print, wing_counts_fn, total(star_counts), ENERG_LO, ENERG_HI, bkg_evtfile, F='(%"  %s shows the %0.1f counts (%0.1f:%0.1f keV) from known point sources expected to remain in %s.")'

print, bkg_countsfile, ENERG_LO, ENERG_HI,                                  F='(%"  %s shows the estimated background (counts in %0.1f:%0.1f keV) where it was computed.")'


if NOT keyword_set(skip_extract_backgrounds) then begin
  ;; =====================================================================
  ;; Extract background for each source.
  acis_extract, srclist_fn, obsname, bkg_evtfile, /EXTRACT_BACKGROUNDS, EMAP_FILENAME=bkg_emapfile, _EXTRA=extra
  
  
  ;; =====================================================================
  ;; Collate results.
  ; Commented out to reduce run times.
  ;acis_extract, srclist_fn, obsname, /SINGLE_OBS, COLLATED_FILENAME=collatefile, VERBOSE=0
endif


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
  print, 'ae_better_masking: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP

end ; ae_better_masking





;#############################################################################
;;; Build background regions which account for contamination from neighboring sources.
;;;
;;; The entire catalog must already have been processed through the EXTRACT_SPECTRA stage.
;;; This tool will run the MERGE_OBSERVATIONS stage on a SINGLE obsid to estimate fluxes for all the sources.
;;; Note that a source might have zero or negative net counts in any single observation.

;;; The required parameter EVTFILE_BASENAME specifies the name of the observation event list file in
;;; the directory ../obsXXXX/.  In our recipe we use EVTFILE_BASENAME='validation.evt' for passes that 
;;; prune and reposition sources, and use EVTFILE_BASENAME='spectral.evt' for the photometry/fitting extraction.

;;; The optional input BACKGROUND_MODEL_FILENAME can be used to supply a model (FITS image) 
;;; of any background component not represented by the point sources in the catalog.
;;; For example such a model could be constructed for readout streaks so the streaks will
;;; participate in the algorithm used to contruct background regions.
;;; The BACKGROUND_MODEL_FILENAME should be in units of observed counts (i.e. exposure 
;;; variation is represented in the model).

;;; The stopping criteria controlling the growth of the background region are: 

;;; 1. The region's background exposure ratio (bkg_exposurearea/src_exposurearea) MUST end up in the range 
;;;    [BKSCL_LO,BKSCL_HI] (obtained from source.stats).  (For coding convenience, a value just above BKSCL_HI is acceptible.)

;;; 2. If the region's background exposure ratio reaches the target value of BKSCL_GL and the region 
;;;    contains at least MIN_NUM_CTS in-band counts then the search is stopped.

;;; 3. After the BKSCL_LO criterion is met, the region's growth is stopped if the background imbalance metric exceeds
;;;    BACKGROUND_IMBALANCE_THRESHOLD.  However,the background imbalance metric will be unstable in the early iterations of the search, 
;;;    because  the unit of area added to the bkg region (1 pixel) is large with respect to the current region.
;;;    I don't know a rigourous way to detect this problem, so I'll just disable the background imbalance metric (below) 
;;;    when there are few pixels in the background region.

PRO ae_better_backgrounds, obsname, EVTFILE_BASENAME=evtfile_basename, $
    NUMBER_OF_PASSES=number_of_passes, $ 
  
		THETA_RANGE=theta_range, BACKGROUND_MODEL_FILENAME=background_model_filename, $
  
    SOURCE_NOT_OBSERVED=source_not_observed, GENERIC_RMF_FN=generic_rmf_fn, $
    
    SRCLIST_FILENAME=srclist_fn, EXTRACTION_NAME=extraction_name, $
    EMAP_BASENAME=emap_basename, $

		MIN_NUM_CTS=min_num_cts, BACKGROUND_IMBALANCE_THRESHOLD=background_imbalance_threshold, $
    COMPACTNESS_GAIN=compactness_gain, $
    
    VERBOSE=verbose, SHOW=show, PAUSE_FOR_REVIEW=pause_for_review, $
      
    PHOTOMETRY_STAGE_ONLY=photometry_stage_only, SKIP_PHOTOMETRY_STAGE=skip_photometry_stage, $
    BUILD_MODELS_ONLY=build_models_only, SAVEFILE_BASENAME=savefile_basename, $
    REUSE_MODELS=reuse_models

exit_code = 0
creator_string = "ae_better_backgrounds, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

;; Create a randomly named scratch directory that will not conflict with another instance of AE.
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

;; Save any existing PFILES environment variable and establish access to CIAO.
inherited_pfiles = getenv("PFILES")
run_command, /INIT, PARAM_DIR=tempdir

if (n_elements(verbose)                   EQ 0) then verbose                   = 0
if (n_elements(show)                      EQ 0) then show                      = 1
if (n_elements(theta_range)               NE 2) then theta_range               = [0,100.]
if (n_elements(compactness_gain)          NE 1) then compactness_gain          = 0.01
compactness_gain = float(compactness_gain)

if ~keyword_set(background_imbalance_threshold) then background_imbalance_threshold = 0.10
   
if ~keyword_set(evtfile_basename) then begin
  print, 'ERROR: parameter EVTFILE_BASENAME must be supplied'
  GOTO, FAILURE
endif
if ~keyword_set(savefile_basename)      then savefile_basename = 'ae_better_backgrounds.sav'
if ~keyword_set(emap_basename)          then emap_basename     = 'obs.emap'


obsdir                      = '../obs' + obsname + '/'
evtfile                     = obsdir + evtfile_basename
emapfile                    = obsdir + emap_basename
model_savefile              = obsdir + savefile_basename
observation_counts_model_fn = obsdir + 'observation_counts_model.img'
collatefile                 = obsdir + 'all.collated'
regionfile                  = obsdir + 'extract.reg'
data_image_fn               = obsdir + 'inband_data.img'

if ~keyword_set(srclist_fn)      then srclist_fn = 'all.srclist'
if (srclist_fn NE 'all.srclist') && ~keyword_set(reuse_models) then $
  print, 'WARNING: Background estimation may be unreliable when only a subset of full catalog is processed.'

if keyword_set(reuse_models) && (~file_test(model_savefile)) then begin
  print, 'WARNING: MODEL_SAVEFILE '+model_savefile+' not found; ignoring /REUSE_MODELS.'
  reuse_models = 0
endif

arcsec_per_skypixel = 0.492 

; Make spectra with 685 channels to match RMFs from CTI corrector.
DETCHANS = 685

src_stats_basename         = 'source.stats'
obs_stats_basename         = 'obs.stats'
obs_frac_basename          = 'obs.psffrac'
src_region_basename        = 'extract.reg'
psf_basename               = 'source.psf'
rmf_basename               = 'source.rmf'
arf_basename               = 'source.arf'
bkg_spectrum_basename      = 'background.pi'
bkg_emap_basename          = 'background.emap'
bkg_pixels_region_basename = 'background_pixels.reg'
bkg_events_basename        = 'background.evt'

temp_image_fn    = tempdir + 'temp.img'
catalog_region_fn= cache_dir + 'catalog.reg'
temp_events_fn   = tempdir + 'temp.evt'
temp_text_fn     = tempdir + 'temp.txt'


if (verbose GE 1) then begin
  my_ds9 = "DS9:ae_better_backgrounds"
  ; Look for an existing ds9 session ...
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  run_command, string(my_ds9, F='(%"xpaaccess %s")'), result, /IGNORE_STATUS, /QUIET
  if (result[0] NE 'yes') then $
    run_command, 'ds9 -xpa local -tile -title ae_better_backgrounds -log &'
endif
if (verbose GE 2) then begin
  window,0
  window,1
  window,2
  term1=replicate(!VALUES.F_NAN,10000)
  term2=term1
  term3=term2
endif


; The map "pixel_status" takes on these flag values:
off_field_status = 0B  ; not allowed in background region because exposure is too small
masked_status    = 1B  ; not allowed in background region because too close to source
available_status = 2B  ; available for use in background, but not yet considered
candidate_status = 3B  ; currently competing to be accepted into the background region
background_status= 4B  ; accepted into the background region


;; =====================================================================
;; Collect photometry information from the existing single-observation extraction.
if keyword_set(reuse_models) then begin
  print, F='(%"\n===================================================================")'
  print, 'WARNING! Using source models saved from previous session in '+model_savefile
  restore, model_savefile, /VERBOSE
  print, F='(%"===================================================================\n")'
endif else begin
  if ~keyword_set(skip_photometry_stage) then begin
    print, F='(%"\nae_better_backgrounds: ============================================================")' 
    print, 'ae_better_backgrounds: Running MERGE_OBSERVATIONS stage on observation '+obsname
    print, F='(%"ae_better_backgrounds: ============================================================\n")'
    ; We need to run MERGE_OBSERVATIONS using this SINGLE observation for two reasons
    ; not related to time variability of the sources:           
    ; (1) We must ensure that all the properties collated below refer to one observation.
    ;     For example, SRC_CNTS is used later with single-obsid background information 
    ;     in Pb calculations.
    ; (2) Since a source may have a different PSF in different obsids the effects of 
    ;     crowding may be quite different.
    ; We want the single-obsid photometry for all sources, so disable the OVERLAP pruning in /MERGE.
    merge_name = 'EPOCH_'+obsname
    acis_extract, srclist_fn, obsname, EXTRACTION_NAME=extraction_name, MERGE_NAME=merge_name, /MERGE_OBSERVATIONS, OVERLAP_LIMIT=1E6, /SKIP_PSF, /SKIP_NEIGHBORHOOD, /SKIP_TIMING, EBAND_LO=0.5, EBAND_HI=8.0, SOURCE_NOT_OBSERVED=source_not_observed, GENERIC_RMF_FN=generic_rmf_fn
  
    print, F='(%"\nae_better_backgrounds: ============================================================")' 
    print, 'ae_better_backgrounds: Running COLLATE stage on observation '+obsname
    print, F='(%"ae_better_backgrounds: ============================================================\n")'
    ;; Collect the flux & background information from the existing extraction.
    ;; Do NOT use /SINGLE_OBS because we need the photometry information.
    ;; It's tempting to use the /MATCH_EXISTING option to save time, but I'm nervous about not having any control over the
    ;; format of the existing collatefile. 
    acis_extract, srclist_fn, obsname, EXTRACTION_NAME=extraction_name, MERGE_NAME=merge_name, COLLATED_FILENAME=collatefile, VERBOSE=0, SOURCE_NOT_OBSERVED=source_not_observed  
    
    if keyword_set(photometry_stage_only) then GOTO, CLEANUP
  endif ;~keyword_set(skip_photometry_stage)

  bt=mrdfits(collatefile, 1, /SILENT)
  temp = n_elements(bt)

  ; Sources which were not observed in this obsid are discarded here so that we don't waste time/space
  ; contructing various data structures which have an entry for each source.
  ; We must be careful to get source names from bt.CATALOG_NAME rather than from reading srclist_fn ourselves!
  bt = bt[where(bt.NUM_OBS GT 0, num_sources)]
  
  if (temp-num_sources GT 0) then print, temp-num_sources, F='(%"\nWARNING: Ignoring %d sources not in this observation.")'
endelse
                                                                                          
if (verbose GE 1) then run_command, 'egrep "label|polygon" '+regionfile+' >! '+catalog_region_fn

num_sources = n_elements(bt)

ind = where(bt.NUM_OBS NE 1, count)
if (count GT 0) then begin
  print, 'ERROR: these sources have NUM_OBS != 1'
  forprint, SUBSET=ind, bt.CATALOG_NAME, bt.NUM_OBS
  GOTO, FAILURE
endif         


if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)


; Below we extract the source properties we're going to need.  
; This design is a bit sloppy---we require that all these properties correspond to the single obsid we're working with.
; Thus it's tempting to collate with /SINGLE_OBS so we grab these properties directly from <src>/<obs>/obs.stats.
; However, we also need quantities (SRC_CNTS, NET_CNTS, EXPOSURE) calculated in /MERGE, run in single-obsid mode.
; To collate these quantities we must omit the /SINGLE_OBS option.
; Thus, several single-source properties (e.g. SRC_AREA) are actually being collated from <src>/source.stats, 
; rather than from obs.stats.
;
; We could of course eliminate the MERGE stage from this algorithm, instead grabbing the single-obsid quantities we
; need directly from files in <src>/<obs>/, but that would require duplicating some code from the MERGE stage.

CATALOG_NAME = strtrim(bt.CATALOG_NAME,2)
SRC_CNTS = bt.SRC_CNTS[0] 
NET_CNTS =(bt.NET_CNTS[0])
BACKSCAL =(bt.BACKSCAL[0])
EXPOSURE = bt.EXPOSURE
ENERG_LO = bt[0].ENERG_LO[0]
ENERG_HI = bt[0].ENERG_HI[0]
MSK_RAD  = bt.MSK_RAD
X_CAT    = bt.X_CAT
Y_CAT    = bt.Y_CAT
RA       = bt.RA
DEC      = bt.DEC
LABEL    = strtrim(bt.LABEL,2)
THETA    = bt.THETA
; The SRC_AREA property is a hassle.  IF the observer has run ae_make_catalog on a source but then
; (for efficiency) skipped EXTRACT_SPECTRA because the region did not change much, then SRC_AREA
; will be missing.  In such cases we use instead PGN_AREA.
SRC_AREA = bt.SRC_AREA
PGN_AREA = bt.PGN_AREA
ind = where((SRC_AREA EQ 0) OR (~finite(SRC_AREA)), count)
if (count GT 0) then begin
  print, count, F='(%"WARNING: PGN_AREA substituted for SRC_AREA for %d sources.")'
  SRC_AREA[ind] = PGN_AREA[ind]
endif

;; Check for invalid values in critical properties.
temp = SRC_AREA+SRC_CNTS+NET_CNTS+BACKSCAL+EXPOSURE+ENERG_LO+ENERG_HI+MSK_RAD+X_CAT+Y_CAT+RA+DEC+THETA
ind = where(~finite(temp), count)
if (count GT 0) then begin
  print, 'ERROR: NaN values found in critical columns (SRC_AREA,SRC_CNTS,NET_CNTS,BACKSCAL,EXPOSURE,ENERG_LO,ENERG_HI,MSK_RAD,X_CAT,Y_CAT,RA,DEC,THETA) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, FAILURE
endif

temp = SRC_AREA*EXPOSURE*ENERG_LO*ENERG_HI*MSK_RAD*X_CAT*Y_CAT*RA*abs(DEC)*THETA
ind = where(temp LE 0, count)
if (count GT 0) then begin
  print, 'ERROR: zero or negative values found in critical columns (SRC_AREA,EXPOSURE,ENERG_LO,ENERG_HI,MSK_RAD,X_CAT,Y_CAT,RA,DEC,THETA) of '+collatefile
  forprint, CATALOG_NAME, SUBSET=ind
  GOTO, FAILURE
endif

if (~array_equal(strtrim(bt.OBSNAME,2), obsname)) then begin
  print, 'ERROR: the collated photometry in '+collatefile+' appears to come from the wrong observation!'
  GOTO, FAILURE
endif

if keyword_set(reuse_models) then begin
  GOTO, CONSTRUCT_BACKGROUNDS
endif

print, F='(%"\nae_better_backgrounds: ============================================================")' 
print, 'ae_better_backgrounds: Building source models ...'
print, F='(%"ae_better_backgrounds: ============================================================\n")'

;; =====================================================================
;; Read the exposure map which defines the pixelization of the background region.
emap         = readfits(emapfile, emap_header)
extast, emap_header, emap2wcs_astr
emap_col_dim = (size(emap, /DIM))[0]
emap_row_dim = (size(emap, /DIM))[1]

skypix_per_emappix     = sxpar(emap_header, 'CDELT1P') ;linear sky pixels per emap pixel


;; =====================================================================
;; IMPORTANT REMARKS ABOUT PIXELIZATION OF THE SOURCE EXTRACTION REGION

;; It is important to keep in mind that actual extraction regions in AE are
;; polygons with corresponding areas that are estimated carefully.
;; Lots of calculations in this algorithm mimick the background subtraction
;; done in AE which scales the background counts by the ratio of the 
;; extraction polygon area and the background region area,
;; represented by the variables src_exposurearea and bkg_exposurearea.
;; As in AE, these are expressed in units of (sec*cm^2*skypixel^2).
;;
;; However at several points this algorithm must integrate a pixelized source
;; model (scaled PSF image) over the extraction region polygon.  
;; The crude method used for this is to sum up the source model pixels that fall
;; inside the region and then scale up by the ratio of the polygon area to the
;; pixelized aperture area.

 
;; =====================================================================
;; Construct a model of each point source by resampling and scaling the PSF.
;; All these models are in units of COUNTS, i.e. the star's flux has been passed through the exposure map.
;; We store only the non-zero portion of these huge sparse arrays.

num_models = num_sources + keyword_set(background_model_filename)

models = replicate({LABEL:'', col_min:0, row_min:0, col_max:-1, row_max:-1, counts_in_model:0.0, data_ptr: ptr_new()}, num_models)
models.LABEL = LABEL

observation_counts_model_img = fltarr(emap_col_dim, emap_row_dim)

for ii = 0, num_sources-1 do begin
  print, F='(%"\n===================================================================")'
  print, CATALOG_NAME[ii], LABEL[ii], F='(%"Source: %s (%s)")'  
  obsdir          = CATALOG_NAME[ii] + '/' + obsname + '/' + extraction_subdir[ii]
  obs_stats_fn    = obsdir + obs_stats_basename

  ; To support faster processing on multiple computers, we allow the observer to run this
  ; model-building part of the code AFTER the MERGE stage of AE has been run for another
  ; obsid.
  ; Thus, we can NOT expect that any of the merged data products (PSF, ARF, or RMF) 
  ; continue to correspond to the single obsid we're working on here, so we directly
  ; access the single-obsid response files we need.
  psf_fn          = CATALOG_NAME[ii] + '/' + obsname + '/' + psf_basename
  psf_frac_fn     = obsdir + obs_frac_basename

  ; We discarded earlier sources not observed in this ObsId (NUM_OBS EQ 0), so any missing extraction found here is a bug!
  if (~file_test(obs_stats_fn) || ~file_test(psf_fn)) then begin
    message, 'ERROR: cannot find '+obs_stats_fn+' or '+psf_fn
  endif

  if (NET_CNTS[ii] LE 0) then begin
    ; We only allow positive NET_CNTS to avoid negative star models in the algorithm.
    print, 'Skipping source with non-positive NET_CNTS'
    continue
  endif
  
  
  ;; ------------------------------------------------------------------------
  ;; Construct a *model* (in the form of a real-valued counts image) for the light that should have been observed from this source.  
  ;; We are modelling only power from the point source itself (derived from NET_CNTS photometry computed above); not any background.
  ;;
  ;; THE ALGORITHM BELOW ASSUMES THAT THE PSF IMAGE IS ALREADY A MODEL FOR THE DISTRIBUTION OF COUNTS *DETECTED* 
  ;; ON A FINITE, DITHERED ACIS, as opposed to a model of the distribution of photons striking the HRMA focal plane.
  ;; Currently (2009), MARX produces such an observational PSF.  
  ;; Thus, our desired source model is simply the observational PSF image scaled appropriately by NET_CNTS.
  ;; IF in the future we change to using a PSF image that assumes an infinite detector, then we should look back at version 3323 (Jan 2009)
  ;; for more complex code that applies the shape of the emap to the PSF image in order to simulate the distribution of observed counts.
  this_model = models[ii]
  
  ; Read the PSF for this obsid and set all the NaN values to zero to keep total,max,contour, etc. routines happy.
  psf_img = readfits(psf_fn, psf_header, /SILENT)
  
  ind = where(~finite(psf_img), count)
  if (count GT 0) then psf_img[ind] = 0
  
  ; Regrid the PSF to the emap's pixel grid and renormalize to model the source's observed brightness.
  ; We have to be careful with this normalization.
  ; * Our measurement of the source brightness, NET_CNTS, is obtained from a finite aperture.
  ; * We can estimate how many counts should be detected on an infinite detector by applying our aperture correction (PSF fraction value in obs.psffrac) to NET_CNTS.
  ; * The PSF image, and thus the source model we're building here, are finite however.  The total power in our model should reflect the fraction of the infinite PSF which is covered by the foot print of our PSF image (PSF_footprint_fraction below).
  ; * Regridding with hastrom.pro does NOT preserve the power -- we must make sure the regridded image (not the original one) has the desired normalization.
  
  ; First, we compute the fraction of the infinite PSF which is covered by the foot print of our PSF image.
  ; The PSF generator has estimated the integral of the infinite PSF and saved in the keyword PSF_TOTL.
  psf_total = sxpar(psf_header, 'PSF_TOTL')
  if (psf_total EQ 0) then begin
    print, "WARNING: obsolete PSFs in "+psf_fn
    PSF_footprint_fraction = 1.0
  endif else begin
    PSF_footprint_fraction = total(psf_img, /DOUBLE) / psf_total
  endelse
  
  ; Regrid the PSF image to the emap's pixel grid.
  fxaddpar, psf_header, 'EQUINOX', 2000.0      
  hastrom, psf_img, psf_header, emap_header, MISSING=0 
  
  ; Extract the footprint of the PSF in the emap's pixel grid.
  index_to_point, where(psf_img GT 0), col, row, size(psf_img)
  this_model.col_min         = min(col)
  this_model.row_min         = min(row)
  this_model.col_max         = max(col)
  this_model.row_max         = max(row)
  counts_model = psf_img[this_model.col_min:this_model.col_max, this_model.row_min:this_model.row_max]  
  
  ; Look up the mono-energy of the PSF, and find the corresponding PSF fraction in file obs.psffrac.
  psf_energy = sxpar(psf_header, 'ENERGY')
  print, psf_energy, F='(%"PSF monoenergy is %0.2f keV")'
  
  table = mrdfits(psf_frac_fn, 1, /SILENT, STATUS=status)
  if (status NE 0) then message, 'ERROR reading ' + psf_frac_fn
  
  ind = where(abs(table.energy - psf_energy) LT 0.1, count)
  if (count EQ 0) then begin
    print, "ERROR: no entry for the PSF's mono-energy was found in "+psf_frac_fn
    GOTO, FAILURE
  endif
  
  psf_fraction = table[ind[0]].fraction
  
  ; Normalize the source model.
  normalization = (NET_CNTS[ii]/psf_fraction) * PSF_footprint_fraction / total(counts_model, /DOUBLE)
 
  if finite(normalization) && (normalization GT 0) then begin        
    counts_model *= normalization
    
    observation_counts_model_img[this_model.col_min:this_model.col_max, this_model.row_min:this_model.row_max] += counts_model
    
    ; Save the sparse counts model.
    this_model.counts_in_model = total(   counts_model, /DOUBLE)
    this_model.data_ptr        = ptr_new( counts_model )
    models[ii] = this_model
    
    ; I can't think of any powerful consistency checks to perform on counts_model.  It should be larger than NET_CNTS.
    if (this_model.counts_in_model LT NET_CNTS[ii]) then begin 
      print, this_model.counts_in_model, NET_CNTS[ii], F='(%"ERROR! Source model has fewer counts (%0.1f) than extraction (%0.1f).")'
      GOTO, FAILURE
    endif
  endif else begin
    print, 'ERROR: normalization not finite or not positive:', normalization
    GOTO, FAILURE
  endelse

endfor ;ii

;; Accept the optional model (image) supplied by the observer.
if keyword_set(background_model_filename) then begin
  this_model = models[num_sources]
  
  bkg_img = readfits(background_model_filename, bkg_header, /SILENT)

  ; Determine the total power the final model should have.
  power = total(bkgimg, /DOUBLE) 
  
  ; Regrid supplied image onto the emap pixel grid.
  fxaddpar, bkg_header, 'EQUINOX', 2000.0      
  hastrom, bkg_img, bkg_header, emap_header, MISSING=0    
  
  ; Extract the footprint of the model in the emap's pixel grid.
  index_to_point, where(bkg_img GT 0), col, row, size(bkg_img)
  this_model.col_min         = min(col)
  this_model.row_min         = min(row)
  this_model.col_max         = max(col)
  this_model.row_max         = max(row)
  counts_model = bkg_img[this_model.col_min:this_model.col_max, this_model.row_min:this_model.row_max]  


  ; Normalize to achieve the proper final power in the model.
  normalization = float(power/total(counts_model, /DOUBLE))
  
  counts_model *= normalization

  ; Save the sparse counts model.
  this_model.counts_in_model = total(   counts_model, /DOUBLE)
  this_model.data_ptr        = ptr_new( counts_model )
  models[num_sources] = this_model
  
  print, round(this_model.counts_in_model), F='(%"Observer-supplied background model contains %d counts.")'
endif

; Save the counts model of the entire observation and display in ds9.
writefits, observation_counts_model_fn, observation_counts_model_img, emap_header
print, observation_counts_model_fn, F="(%'Saving counts model of entire observation to %s')"
cmd = string(observation_counts_model_fn, regionfile, F='(%"ds9 -log %s -region %s &")')
if show then spawn, cmd

print, model_savefile, F="(%'Saving source models, collated table, data image, and emap to %s')"
save, emap, emap_header, emap2wcs_astr, emap_col_dim, emap_row_dim, skypix_per_emappix, bt, models, observation_counts_model_img, FILE=model_savefile

  
;; =====================================================================
;; Construct an in-band image of the data.
print, 'energy band is ', ENERG_LO, ENERG_HI

run_command, string(emapfile, F="(%'get_sky_limits %s verbose=0 precision=3')")
run_command, /QUIET, 'pget get_sky_limits dmfilter', filterspec

if (filterspec EQ '') then message, 'ERROR running get_sky_limits'

cmd = string(evtfile, 1000*[ENERG_LO,ENERG_HI], filterspec, data_image_fn, F="(%'dmcopy ""%s[energy=%6.1f:%7.1f][bin %s]"" %s clobber=yes')")
run_command, cmd  


CONSTRUCT_BACKGROUNDS:

if keyword_set(build_models_only) then GOTO, CLEANUP 

inband_data = readfits(data_image_fn)



print, F='(%"\nae_better_backgrounds: ============================================================")' 
print, 'ae_better_backgrounds: Building background regions and extracting spectra ...'
print, F='(%"ae_better_backgrounds: ============================================================\n")'


; Construct an initial pixel status map to be used at the onset of processing for each source.
; In this initial map, all pixels are available for use in backgrounds except those with very low exposure.
initial_pixel_status = replicate(available_status, emap_col_dim, emap_row_dim)
ind = where(emap LT max(emap)/10., count)
if (count GT 0) then initial_pixel_status[ind] = off_field_status

if (verbose GE 1) then begin
  ; Wait for ds9 to register with XPA.  
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  repeat begin
    run_command, string(my_ds9, F='(%"xpaaccess %s")'), result, /IGNORE_STATUS, /QUIET
    if (result[0] EQ 'yes') then break
    print, 'waiting for ds9 to come up...'
    wait,3
  endrep until (0)

  ; Display the event data.  
  run_command, /QUIET, string(my_ds9, data_image_fn, F='(%"xpaset -p %s file  %s")')
endif



;; =====================================================================
; Calculate the positions of the emap pixel centers in physical (x,y) coordinates, 
; keeping in mind the 1-based FITS convention vs the 0-based IDL array index convention.
; We cannot use xy2ad.pro for conversions between array and PHYSICAL (sky) systems.
crvalP = [sxpar(emap_header, 'CRVAL1P'), sxpar(emap_header, 'CRVAL2P')]
crpixP = [sxpar(emap_header, 'CRPIX1P'), sxpar(emap_header, 'CRPIX2P')]
cdeltP = [sxpar(emap_header, 'CDELT1P'), sxpar(emap_header, 'CDELT2P')]

emap_pixel_x = (findgen(emap_col_dim)+1-crpixP[0])*cdeltP[0] + crvalP[0]
emap_pixel_y = (findgen(emap_row_dim)+1-crpixP[1])*cdeltP[1] + crvalP[1]
make_2d, emap_pixel_x, emap_pixel_y
  
;; =====================================================================
;; Pre-calculate a normalized grid of positions relative to the source where
;; we will create the initial set of background pixel candidates.
N = 6
seed_grid_ii = (indgen(2*N) - (N-0.5)) / (N-0.5)
seed_grid_jj = seed_grid_ii
make_2d, seed_grid_ii, seed_grid_jj
seed_grid_ii = reform(seed_grid_ii, n_elements(seed_grid_ii))
seed_grid_jj = reform(seed_grid_jj, n_elements(seed_grid_jj))

;; =====================================================================
;; Define a background region for each source.


processing_rate = fltarr(num_sources)

;; When /REUSE_MODELS is specified we need to read the catalog supplied to 
;; see which sources are to have backgrounds computed; see comments below.
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_in_catalog)

if (num_in_catalog EQ 0) then begin
  print, 'ERROR: no entries read from source list ', srclist_fn
  GOTO, FAILURE
endif

sourcename = sourcename[ind]


case (n_elements(min_num_cts)) of
  num_sources:
  1          : min_num_cts = replicate(min_num_cts,num_sources)
  0          : min_num_cts = replicate(          5,num_sources)
  else       : begin
               print, 'ERROR: MIN_NUM_CTS input must be scalar or must match the length of the SRCLIST: ', n_elements(sourcename)
               GOTO, FAILURE
               end
endcase

for ii = 0, num_sources-1 do begin
  t0 = systime(1)
  ;; ------------------------------------------------------------------------
  ;; When /REUSE_MODELS is specified the observer may supply a sourcelist that
  ;; does not match the sources found in the previously computed collated table (bt),
  ;; for example if he/she wants to construct a background for only one source
  ;; but wants to consider contamination from the whole catalog.
  ;; We are looping (ii) over the collated table so we can clearly index vectors
  ;; like SRC_CNTS, NET_CNTS, etc.
  ;; As we loop, we will skip any source in bt that is not in the sourcelist
  ;; supplied by the observer.
  srclist_ind = where(CATALOG_NAME[ii] EQ sourcename, count)

  if (count EQ 0) then begin
   ;print, 'Skipping source not in list: ', CATALOG_NAME[ii]
    continue
  endif 
  

  if (THETA[ii] LT theta_range[0]) || (THETA[ii] GT theta_range[1]) then begin
    print, 'Skipping source not in THETA_RANGE: ', CATALOG_NAME[ii]
    continue
  endif 
  
  ; Remove any temp files from the previous source. 
  list = reverse(file_search(tempdir,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
    
  run_command, /QUIET, ['pset dmcopy clobber=yes', 'pset dmimgpick clobber=yes', 'pset dmextract clobber=yes']

  print, F='(%"\n===================================================================")'
  print, CATALOG_NAME[ii], LABEL[ii], F='(%"Source: %s (%s)")'  
  
  sourcedir            = CATALOG_NAME[ii] + '/' 
  unnamed_src_stats_fn = sourcedir + src_stats_basename
  obsdir               = sourcedir + obsname + '/' + extraction_subdir[ii]
  region_fn            = obsdir + src_region_basename
  bkg_emap_fn          = obsdir + bkg_emap_basename
  bkg_pixels_region_fn = obsdir + bkg_pixels_region_basename
  bkg_events_fn        = obsdir + bkg_events_basename
  bkg_spectrum_fn      = obsdir + bkg_spectrum_basename
  obs_stats_fn         = obsdir + obs_stats_basename
  
  if (~file_test(obs_stats_fn)) then begin
    print, 'BACKGROUND CONSTRUCTION SKIPPED: source not observed.'
    continue
  endif

  if (verbose GE 2) then begin
    wset, 0
    erase
    wset, 1
    erase
    wset, 2
    erase
  endif
  
  ;; ------------------------------------------------------------------------
  ; Look up allowed range of background normalizations directly from source.stats (not from the collated table) 
  ; since they may have changed since we built the source models and collated table.
  unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)

  if (~keyword_set(error)) then begin
    BKSCL_LO = sxpar(unnamed_src_stats, 'BKSCL_LO') ; Smallest allowed bkg scaling.
    BKSCL_GL = sxpar(unnamed_src_stats, 'BKSCL_GL') ; Target bkg scaling.
    BKSCL_HI = sxpar(unnamed_src_stats, 'BKSCL_HI') ; Largest allowed bkg scaling.
    if (BKSCL_LO LE 0) || (BKSCL_GL LE 0) || (BKSCL_HI LE 0) then begin
      print, 'ERROR: BKSCL_LO,BKSCL_GL,BKSCL_HI are either missing from source.stats or are non-positive!'
      GOTO, FAILURE
    endif
    if (BKSCL_LO GT BKSCL_GL) || (BKSCL_GL GT BKSCL_HI) then begin
      print, 'ERROR: BKSCL_GL is not between BKSCL_LO and BKSCL_HI!'
      GOTO, FAILURE
    endif
  endif else message, 'ERROR reading '+unnamed_src_stats_fn
  
  
  ;; ------------------------------------------------------------------------
  ;; Identify the emap/model pixels that fall in this source's extraction aperture.
  ;; For speed we have to apply ContainsPoints() on a sub-region of the emap.
  ae_ds9_to_ciao_regionfile, region_fn, '/dev/null', /IGNORE_BACKGROUND_TAG, POLYGON_X=polygon_x, POLYGON_Y=polygon_y

  bbox_ind = where((emap_pixel_x GE (min(polygon_x)-1)) AND (emap_pixel_y GE (min(polygon_y)-1)) AND $
                   (emap_pixel_x LT (max(polygon_x)+1)) AND (emap_pixel_y LT (max(polygon_y)+1)) )
  
  bbox_pixel_x = emap_pixel_x[bbox_ind]
  bbox_pixel_y = emap_pixel_y[bbox_ind]
  
  ;; ------------------------------------------------------------------------
  ;; This is the re-entry point into the search code for cases where we want to repeat the search in order to
  ;; re-construct the bkg region identified by the "reserve_region" structure.
  ;; The re-entry point must be placed BEFORE any variables used in the search are initialized (e.g. pixel_status image below).
  ;; The second pass through the search loop is implemented by manipulating the loop stopping value, max_num_bkg_pixels.
  max_num_bkg_pixels = n_elements(emap)/10
  is_first_pass      = 1
  
REPEAT_SEARCH:

  o_poly=obj_new('IDLanROI', polygon_x, polygon_y)

  aperture_ind = where((o_poly->ContainsPoints(bbox_pixel_x,bbox_pixel_y) EQ 1), aperture_count)
  if (aperture_count EQ 0) then begin
    print, 'WARNING: extraction region is smaller than one emap pixel.'
    dum = min(sqrt((bbox_pixel_x-X_CAT[ii])^2 + (bbox_pixel_y-Y_CAT[ii])^2),aperture_ind)
    aperture_count = 1
  endif
  
  aperture_ind = bbox_ind[aperture_ind]
  
  ; The exposure-area of the extraction region is not well estimated by the pixelized aperture represented by aperture_ind,
  ; so as in AE we multiply an accurate geometric area of the polygon by the mean emap value; units are (sec*cm^2*skypixel^2).
  mean_exposure    = mean(emap[aperture_ind], /DOUBLE)
  src_exposurearea = float(SRC_AREA[ii]*mean_exposure)
  
  ; This correction from the pixelized aperture area to the real extraction region area 
  ; must be used later where we again are trying to integrate something over the extraction region.
  aperture_pixelization_correction = SRC_AREA[ii] / ((skypix_per_emappix)^2 * aperture_count)
;help, aperture_count, aperture_pixelization_correction


  ;; ------------------------------------------------------------------------
  ;; Decide which region (pixels) around the current source should be eliminated from 
  ;; consideration for the background region for that source.
  ;; At a minimum it seems obvious we want to eliminate the extraction region itself.
  ;; Since our aperture is pixelated we test whether any of several positions in each pixel
  ;; are interior to the polygon.
  masked_emap = emap
  
  pixel_status = initial_pixel_status
  pixel_status[aperture_ind] = masked_status
  masked_emap [aperture_ind] = 0
  
  hs = abs(cdeltP[0]) * 0.3   ; "half-size" of emap pixel, in skypix units
  
  is_inside = (o_poly->ContainsPoints(bbox_pixel_x+hs,bbox_pixel_y+hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x-hs,bbox_pixel_y+hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x-hs,bbox_pixel_y-hs) EQ 1) OR $
              (o_poly->ContainsPoints(bbox_pixel_x+hs,bbox_pixel_y-hs) EQ 1)
  
  mask_ind = where(is_inside, mask_count)
  if (mask_count GT 0) then begin
    pixel_status[bbox_ind[mask_ind]] = masked_status
    masked_emap [bbox_ind[mask_ind]] = 0
  endif

  obj_destroy,o_poly 

  
  ;; We don't really want to apply aggressive masking to the current source because we may
  ;; eliminate regions which are valuable for the goal of minimizing background bias.
  ;; However we have the additional goal of not including too many counts from the current source 
  ;; to fall in its own background region.
  ;; It's not obvious how to balance these desires.  A simple ad hoc penalty we'll adopt is
  ;; to set the model contamination (goal) for the current source to zero; then any source counts
  ;; that fall in the background region act as a penalty.  
  ;; This is done in code found a bit later.

  ;; ------------------------------------------------------------------------
  ;; Nominate a set of initial background pixel candidates consisting of two subsets.
  ;; First, we want to nominate all the pixels adjacent to masked pixels, since for severe
  ;; crowding those close-in pixels are often the best choices.
  ;; Second we want to nominate a grid of pixels covering a broad surrounding area to give
  ;; the search lots of choices, including some far from the source. 
  ;; By using multiple seeds we allow discontiguous regions, but there's nothing wrong with that
  
  index_to_point, where(pixel_status EQ masked_status), temp_col, temp_row, size(pixel_status)
  temp_col = [temp_col+1,temp_col+0,temp_col-1,temp_col+0]
  temp_row = [temp_row+0,temp_row+1,temp_row+0,temp_row-1]
  
  ; Calculate the 0-based column/row position of the source.
  ad2xy, RA[ii], DEC[ii], emap2wcs_astr, src_column, src_row
  
  ; Define a region in which candidate pixel "seeds" will be generated.
  seed_radius = 10 > (2 * MSK_RAD[ii] / skypix_per_emappix)    ; units are array indexes      
  
  nominated_candidates     = replicate({col:0, row:0}, n_elements(temp_col)+n_elements(seed_grid_ii))
  nominated_candidates.col = [temp_col,round(src_column + seed_radius * seed_grid_ii)]
  nominated_candidates.row = [temp_row,round(src_row    + seed_radius * seed_grid_jj)]
  num_candidates = 0L
 
;plot, nominated_candidates.col, nominated_candidates.row, PSYM=3, /YNOZ
  
  ;; ------------------------------------------------------------------------
  ;; To speed up the algorithm, define a search domain that is a subset of the full observation.
  ;; We process only those models that overlap this domain, and we restrict the 
  ;; background region to lie in this domain.
  ;;
  ;; We want this domain to encompass a certain minimum observed area on the sky (1.5x1.5 arcmin^2)
  half_dim = (0.75 * 60) / (arcsec_per_skypixel * skypix_per_emappix)  ; half-width of nominal domain (emap pixel units)
  min_num_active_pixels = (2*half_dim)^2                               ; area of nominal domain

  domain = {col_min:floor(src_column-half_dim) > 1, $
            row_min:floor(src_row   -half_dim) > 1,    $
            col_max: ceil(src_column+half_dim) < (emap_col_dim-2),    $
            row_max: ceil(src_row   +half_dim) < (emap_row_dim-2)}
  step_size = 0
  repeat begin
    domain.col_min -= step_size
    domain.row_min -= step_size
    domain.col_max += step_size
    domain.row_max += step_size
    ; For convenience later, this domain stays 1 pixel away from the emap edges
    domain.col_min >= 1
    domain.row_min >= 1
    domain.col_max <= (emap_col_dim-2)
    domain.row_max <= (emap_row_dim-2)
  
    num_active_pixels = total(/INTEGER, pixel_status[domain.col_min:domain.col_max, domain.row_min:domain.row_max] EQ available_status)
    
    if (domain.col_min EQ 1) AND (domain.row_min EQ 1) AND (domain.col_max EQ (emap_col_dim-2)) AND (domain.row_max EQ (emap_row_dim-2)) then break
    
    step_size = 1 > ceil((min_num_active_pixels - num_active_pixels) / ((domain.col_max-domain.col_min)+(domain.row_max-domain.row_min)))
  endrep until (num_active_pixels GE min_num_active_pixels)

; print, num_active_pixels * (arcsec_per_skypixel * skypix_per_emappix / 60)^2, num_active_pixels,$
         F='(%"Bkg region domain is %4.1f arcmin^2 (%d emap pixels)")'

  ; The set of models we work with include those whose footprints intersect the domain, AND
  ; the source we're processing (models[ii])!
  model_in_domain = (domain.col_max GE models.col_min) AND $
                    (domain.col_min LE models.col_max) AND $
                    (domain.row_max GE models.row_min) AND $
                    (domain.row_min LE models.row_max)
  model_in_domain[ii] = 1
  
  domain_ind    = where(model_in_domain, num_models)
  domain_models = models[domain_ind]
  
  ; Identify the source we're processing in this sub-list of models, and defensively confirm.
  self_index = (where(domain_ind EQ ii))[0] 

  self_model = domain_models[self_index]
  if (self_model.LABEL NE LABEL[ii]) then message, 'Bug in logic!'

  ;; ------------------------------------------------------------------------
  ;; Estimate the contamination from each source model falling within the extraction aperture
  ;; by integrating each source model over the aperture pixels (col,row).
  index_to_point, aperture_ind, col, row, size(emap)
                 
  aperture_model_counts = fltarr(num_models)
  for jj=0,num_models-1 do begin
    this_model = domain_models[jj]
    ind = where( (col GE this_model.col_min) AND (col LE this_model.col_max) AND $
                 (row GE this_model.row_min) AND (row LE this_model.row_max), count )

    if (count GT 0) then begin
      model_inside_aperture     = (*this_model.data_ptr)[col[ind]-this_model.col_min, row[ind]-this_model.row_min]
      aperture_model_counts[jj] = total(model_inside_aperture,/DOUBLE) * aperture_pixelization_correction
    endif
  endfor ;jj
  
  ; As described earlier, set the model contamination (goal) for the current source to zero; 
  ; then any source counts that fall in the background region act as a penalty.  
  aperture_model_counts[self_index] = 0

  aperture_model_counts_total = total(aperture_model_counts)

  ;; ------------------------------------------------------------------------
  ;; Form a cumulative distribution of exposure as a function of distance from the source.
  ;; Distance is in units of skypix.
  ;; We need to integrate over our search domain in the real exposure map rather than 
  ;; analytically integrate over a disk because the FOV has edges.
  ;; The integral must exclude masked pixels, since these are not available to be used in the bkg region.
  
  ; Compute distance from source to each pixel in the full emap, in units of emap pixels.
  dist_circle, distance_map, [emap_col_dim, emap_row_dim], src_column, src_row
  
  ; Find index (into full emap array) of pixels in the current domain (defined above).
  domain_index       = (lindgen(emap_col_dim, emap_row_dim))[domain.col_min:domain.col_max, domain.row_min:domain.row_max]
  
  ; Find index (into full emap array) of pixels in the current domain that are available for use in the background region.
  available_index    = domain_index[where(pixel_status[domain_index] EQ available_status, num_available)]

  ; Sort available_index by distance from the source
  sorted_available_index = available_index[sort(distance_map[available_index])]
  
  ; Build a cumulative distribution of the sorted pixel distances, in units of skypixels.
  ; For example, cumulative_pixel_distance[9] is the sum of the distances to the nearest 10 available pixels.
  ; BUild a corresponding cumulative distribution of the exposure-areas of the sorted pixels, in units of sec*cm^2*skypixel^2
  ; For example, cumulative_exposurearea[9] is the sum of the exposure-areas of the nearest 10 available pixels.
  cumulative_pixel_distance     = skypix_per_emappix   * total(/CUMULATIVE, /DOUBLE, distance_map[sorted_available_index])
  cumulative_exposurearea       = skypix_per_emappix^2 * total(/CUMULATIVE, /DOUBLE,  masked_emap[sorted_available_index])
  cumulative_exposurearea_index = 0L
  

;temp_img = replicate(2*num_available, emap_col_dim, emap_row_dim)
;temp_img[sorted_available_index] = lindgen(num_available)
;temp_img=temp_img[domain.col_min:domain.col_max, domain.row_min:domain.row_max]
;writefits, 'temp.img', temp_img
;stop

    
  
  ;; =====================================================================
  ;; Define a data structure to hold emap pixels that are candidates to be added to the background region.
  ;; (col,row) are array index coordinates of the pixel.
  ;; pixel_exposurearea is the integral of the emap over the pixel (sec*cm^2*skypixel^2).
  ;; distance is distance from the source in units of skypix.
  ;; model_counts is the integral of all the stellar models over this pixel
  ;; Some of the data types in this structure are chosen for speed in the inner loop later.
  candidate_template = { $
    ; Properties of the pixel itself.
    col:0, row:0, pixel_exposurearea:0D, distance:0D, model_counts:fltarr(num_models), $
      
    ; Properties of the current bkg region expanded by adding this pixel.
    ; "bkg_distance_sum" and "bkg_exposurearea" are DOUBLE because they are running sums over 
    ; the tags "pixel_exposurearea" and "distance".
    ; Everything else is FLOAT to try to speed up the code.
    bkg_distance_sum:0D, bkg_exposurearea:0D, $      
    bkg_scaling:0.0, bkg_scaling_correction:0.0, $                                   
    bkg_model_counts_total:0.0, subtracted_model_counts_total:0.0, $
    bkg_flat_counts:0.0, aperture_flat_counts:0.0, subtracted_flat_counts:0.0, $
    background_imbalance:0.0, compactness_bias:0.0   }


  ; Discard any existing candidate store (CS) data structure.
  CS = 0  &  dum = temporary(CS)

  ;; ------------------------------------------------------------------------
  ;; Iteratively add pixels to the background region until we've found enough counts.
  ;; As a failsafe we stop the loop after n_elements(emap)/10 iterations.
  bkg_data_counts_total  = 0L
  current_region_metric = 0
  
  ; One pixel candiate structure, named R, is used to store several cumulative bkg region
  ; properties: bkg_exposurearea, bkg_distance_sum.
  ; Vector cumulative properties, bkg_model_counts and subtracted_model_counts, are stored in variables,
  ; R_bkg_model_counts and R_subtracted_model_counts, to save storage.
  R = candidate_template
  R_bkg_model_counts = fltarr(num_models)  ;It's important to be float, not double, for speed in the inner loop later.
       
  largest_uncorrupted_region = {bkg_normalization: -!VALUES.F_INFINITY} 
  lowest_imbalance_region    = {bkg_normalization: -!VALUES.F_INFINITY, background_imbalance: !VALUES.F_INFINITY} 
  reserve_region             = {bkg_normalization: -!VALUES.F_INFINITY, background_imbalance: !VALUES.F_INFINITY, num_bkg_pixels:0L} 
  
  BKSCL_HI_vote =  !VALUES.F_INFINITY
  
  for num_bkg_pixels = 1L, max_num_bkg_pixels do begin
    ;; ------------------------------------------------------------------------
    ;; Try to add nominated candidate pixels to the candidate store.
    for ll=0L,n_elements(nominated_candidates)-1 do begin
      col = nominated_candidates[ll].col
      row = nominated_candidates[ll].row

      ; Pixels outside the current search domain are excluded.
      if (col LT domain.col_min) || (col GT domain.col_max) ||  $
         (row LT domain.row_min) || (row GT domain.row_max) then continue
      
      ; A pixel that has been masked, accepted into the background region, or already accepted as a candidate is ignored.
      if (pixel_status[col,row] EQ available_status) then begin
;        ; For speed we will discard a nominated candidate pixel if it is adjacent
;        ; to an existing candidate pixel.
;        if (pixel_status[col+1,row  ] EQ candidate_status) || $
;           (pixel_status[col-1,row  ] EQ candidate_status) || $
;           (pixel_status[col  ,row+1] EQ candidate_status) || $
;           (pixel_status[col  ,row-1] EQ candidate_status) then continue
              
        pixel_status[col,row] = candidate_status
        
        ; Add the pixel to the candidate store.
        pixel = candidate_template
        pixel.col = col
        pixel.row = row
        pixel.pixel_exposurearea = skypix_per_emappix^2 * emap[col,row] ; (sec*cm^2*skypixel^2)
        pixel.distance           = sqrt((col - src_column )^2 + (row - src_row)^2) * skypix_per_emappix  ;skypix
        
        ; Look up the number of counts each source model predicts for this pixel.
        ind_models_applicable = where( (col GE domain_models.col_min) AND (col LE domain_models.col_max) AND $
                                       (row GE domain_models.row_min) AND (row LE domain_models.row_max), num_models_applicable )
        
        if (num_models_applicable GT 0) then begin
          model_counts = fltarr(num_models)
        
          for jj=0,num_models_applicable-1 do begin
            ind        = ind_models_applicable[jj]
            this_model = domain_models[ind]
            
            model_counts[ind] = (*this_model.data_ptr)[col-this_model.col_min, row-this_model.row_min]
          endfor ;jj
          
          pixel.model_counts    = temporary(model_counts)
        endif
        
        
        ; Enlarge the candidate store if necessary.
        ; Because we're later doing a very large number of vector operations on 
        ; CS (for efficiency), it's important to size this array 
        ; not too much larger than it needs to be.
        if (num_candidates EQ n_elements(CS)) then begin
          temp = replicate(candidate_template, num_candidates + (10 > 0.05*num_candidates > n_elements(nominated_candidates)))
          
          if (num_candidates GT 0) then temp[0] = CS
          CS = temporary(temp)
         ;print, 'CS enlarged to ', n_elements(CS)
        endif
        
        CS[num_candidates] = pixel
        num_candidates++
      endif ; creating a new candidate pixel
    endfor ;ll

    if (num_candidates LE 0) then begin
      print, num_bkg_pixels * (arcsec_per_skypixel * skypix_per_emappix / 60)^2, num_bkg_pixels, F='(%"WARNING: ran out of candidate pixels for background; accepting a region with area %4.1f arcmin^2 (%d emap pixels).")'
      break
    endif


    ;; ------------------------------------------------------------------------
    ;; We need some mechanism for the metric to favor compactness of the background region
    ;; since, lacking any specific information about the unmodeled background component,
    ;; it seems the most one can say is that the background in the source aperture has better
    ;; correlation with nearby background pixels than it does with distant ones.
    ;;
    ;; If one assumed a model of this background component, then of course one could simply 
    ;; add another goal of minimizing the background subtraction bias for this component.
    ;; I don't know what to assume however, so I've decided to arbitrarily adopt a compactness
    ;; metric which is
    ;;    INTEGRAL of emap over bkg region{ distance_i }
    ;;    -------------------------------------------          minus 1.0
    ;;    INTEGRAL of emap over compact region{ distance_i }
    ;; Where the "compact region" is an annulus with the same exposure-area.
    ;;
    ;; This gives me a metric that is zero for a compact region, and 1.0 when the region is
    ;; "twice as fat" as it could be.  Totally ad hoc, I know!
    ;;
    ;; It would be a lot faster to estimate compact_region_integral_distance by the expression 
    ;;   INTEGRAL{ r * (2*!PI*dr) }, r=0...disk_radius
    ;; where disk_radius is chosen so the disk and actual background regions have the same
    ;; GEOMETRIC area.  However at the field edge such a disk-shaped compact region would fall off
    ;; the exposure map, giving the disk far less exposure-area than the actual background region.
    ;; This we're forced to actually integrate the emap over a disk-shaped region.
    
    while (cumulative_exposurearea[cumulative_exposurearea_index] LE R.bkg_exposurearea) do begin
      cumulative_exposurearea_index++
      if (cumulative_exposurearea_index GE n_elements(cumulative_exposurearea)) then begin
        print, 'WARNING: growth of background region was stopped when an implementation limit was reached.'
        GOTO, REGION_COMPLETE
      endif
    endwhile
    
    compact_region_integral_distance = float(cumulative_pixel_distance[cumulative_exposurearea_index]) 
    
   
    ;; ------------------------------------------------------------------------
    ;; There are a lot of confusing variable names required, and they are different
    ;; from the names used in the manual.
    ;;
    ; We have observed scalar quantities, before and after scaling:
    ; bkg_data_counts_total   = N   : counts observed in bkg region
    ;
    ; We have modeled vector quantities with num_models elements, before and after scaling.
    ; aperture_model_counts   : contaminate counts predicted by models in src aperture
    ;                           Sum of this vector is aperture_model_counts_total = Bp
    ; bkg_model_counts        : contaminate counts predicted by models in bkg region  
    ;                           Sum of this vector is this.bkg_model_counts_total = Np
    ; subtracted_model_counts : bkg_model_counts with bkg scaling applied 
    ;                           Sum of this vector is subtracted_model_counts_total = Np * S 
    ;    
    ; We have some derived quanties.
    ; bkg_flat_counts         = Nf     : inferred flat bkg component counts in bkg region 
    ; aperture_flat_counts    = Bf     : inferred flat bkg component counts in aperture
    ; photometric_bias        = DELTAp : Bp - Np*S
    ; bkg_scaling             = S    : background scaling
    ; bkg_scaling_correction  = c    : correction to scaling
    ;                                                        
    ;                   
    ; Quantities associated with the current bkg region use no prefix in their name; temporary
    ; updates to these quantities associated with the candidate region at hand use the "this_" prefix.

    ;; ------------------------------------------------------------------------
    ;; For each pixel that is a candiate for acceptance into the background region, 
    ;; evaluate the region metric we would find if we accepted that pixel.
    ;; EFFICIENCY INSIDE THIS LOOP IS CRITICAL TO PERFORMANCE!!
    ;; WE'VE OMITTED THE /DOUBLE OPTION TO THE TOTAL() CALLS AND USED SOME CODE
    ;; CONSTRUCTS THAT GAIN SPEED BUT LOSE CLARITY.
    ;; When thinking about execution speed, keep in mind that the following variables
    ;; are vectors with num_models elements:
    ;;   aperture_model_counts
    ;;   bkg_model_counts, this.model_counts, this_bkg_model_counts
    ;;   this_subtracted_model_counts
    ;;   photometric_bias
    
;t1=systime(1)


; In debugger, verify there are no type conversions being done (e.g. involving constants) 
; and that there are no needless
; DOUBLE computations (although I do not know if that actually matters for speed).
; 
; Look for intermediate vars to eliminate.
; Look at parentheses.
; Look for constants to pre-compute.
; 
; Think about cache memory usage, and whether use of temporary() can free up memory.

    ;; For efficiency we perform vector calculations on the entire candidate pixel store (CS).
    ;; It's true that CS contains some unused elements, which waste 
    ;; CPU resources, but we assume that num_candidates generally grows almost monotonically
    ;; and we are careful to expand CS only by a little each time.
    
    ; Several global cumulative quantites are incremented/updated here for the candidate pixel
    ; we're considering, and stored for later retrieval if we choose this candidate.
    ; IT IS VERY IMPORTANT, however, that the total number of counts OBSERVED in the background,
    ; bkg_data_counts_total, is NOT updated before this candidate is judged.
    ; We must evaluate the candidates based solely on our MODELS of the contaminating
    ; background components, not considering what DATA the candidate pixel happens to contain.
    ; There are cases where all enlargements of the region are bad, and if we consider what data
    ; lies in the candidates then the algorithm will tend to favor pixels with no data, which
    ; introduces a horrible bias to our search.
    CS.bkg_distance_sum = (R.bkg_distance_sum + CS.distance)
    CS.bkg_exposurearea = (R.bkg_exposurearea + CS.pixel_exposurearea)

    ; Compute the exposure-area ratio, i.e. the nominal background scaling.
    ; Be sure that both terms are in units of (sec*cm^2*skypixel^2).
    CS.bkg_scaling      = src_exposurearea / float(CS.bkg_exposurearea)  

    ; Estimate how many counts from each contaminating source are statistically expected 
    ; to be in the proposed background region, and how many are expected to be subtracted.
    ; Note that one of these array elements represents "self", i.e. how much light from 
    ; the current source will be (mistakenly) subtracted.
    nx = N_elements(R_bkg_model_counts)
    ny = N_elements(CS)
    bkg_model_counts_2d              = rebin(reform(R_bkg_model_counts,    nx, 1,/OVERWRITE), nx, ny, /SAMPLE) + CS.model_counts
    CS.bkg_model_counts_total        = total(bkg_model_counts_2d,1)
    
    subtracted_model_counts_2d       = rebin(reform(CS.bkg_scaling,        1, ny,/OVERWRITE), nx, ny, /SAMPLE) * temporary(bkg_model_counts_2d)
    
    aperture_model_counts_2d         = rebin(reform(aperture_model_counts, nx, 1,/OVERWRITE), nx, ny, /SAMPLE)
    
          
    ; Compute a background imbalance metric that judges how fairly all the background components are represented
    ; in the background regions we are considering.
    ; This is tricky, and vital to the success of the search.
    ; Many approaches are possible.  We've tried computing this metric after the background has been
    ; adjusted to remove photometric bias, but found very unsatisfactory results (e.g. the algorithm
    ; sometimes latches onto power from distant sources, which it then has to scale down.)
    ;
    ; Thus, we choose to compute the metric using the nominal scaling.
    
    ; The denominator of the metric (its normalization) turns out to be problematic.
    ; (We need a normalization in order to reasonably combine this metric with the compactness metric.)
    ; * It turns out that it must be a constant (i.e. not varying between candidates) in order to avoid pathological behavior in the search.
    ; * If we allow the normalization to become zero, then the metric becomes infinity for all candidates and we lose
    ;   our power to guide the search.
    ;   Thus, we impose a floor of 1.0 in the normalizations below.
    CS.background_imbalance = total(abs(temporary(aperture_model_counts_2d) - temporary(subtracted_model_counts_2d)),1) $
                              / $
                              (1 > 2.0 * aperture_model_counts_total)
    
    ; As desribed earlier, our "compactness bias" is the ratio between two integrals.
    ; Make sure it's non-negative so that the region_metric_slope calculation later is ok.
    CS.compactness_bias = (compactness_gain * ((float(CS.bkg_distance_sum) / compact_region_integral_distance) - 1.0)) > 0.0

    
    
    ;; ------------------------------------------------------------------------
    ;; Choose which candidate pixel should be accepted.  We try to strike a compromise between the goals of
    ;; minimum bias metric and compactness of the background region by including a compactness penalty in
    ;; the metric.
    ;;
    ;; We tried and rejected a scheme where the set of "good enough" metrics was defined relative to the minimum
    ;; metric in hand; this allowed the metric to creep up with each iteration unchecked.
    ;; 
    ;; We tried and rejected a scheme where we define an absolute range of "good enough" metrics; if at some point in our search
    ;; the min metric is forced out of this range, then distance will suddenly stop playing a role in 
    ;; the search.  This seems undesirable.
    ;;
    ;; Of course if the observer knows something about the structure of the background not related to
    ;; point sources, then supplying a map of that will drive the search to correctly subtract it.

    ;; If we simply accept the candidate pixel with the smallest new metric, then in situations 
    ;; where every candidate is increasing the metric we will sometimes choose candiates with low
    ;; exposure values.  This is short sighted---we're taking small steps towards badness even if
    ;; a larger step might result in a smaller RATE towards badness.
    ;; Thus, we should be picking the candidate with the best metric SLOPE with respect to exposure!
    
    ; Combine the spectral quality and compactness metrics we're trying to simultaneously minimize.
    region_metric = (CS.background_imbalance + CS.compactness_bias) 

    region_metric_slope   = (region_metric - current_region_metric) / float(CS.pixel_exposurearea)
    best_slope            = min(region_metric_slope[0:num_candidates-1], accept_ind)
    current_region_metric = region_metric[accept_ind]
    
    if (verbose GE 2) then begin
      ; Define a "postage stamp" region around the source, for some VERBOSE displays we show.
      candidate_col = (CS.col)[0:num_candidates-1]
      candidate_row = (CS.row)[0:num_candidates-1]
      ps_cmin = min( candidate_col, MAX=ps_cmax)
      ps_rmin = min( candidate_row, MAX=ps_rmax)
    
      ; We put this display code here because we want CS and region_metric_slope to be intact.
     ;metric = hist_equal(-CS.background_imbalance[0:num_candidates-1], TOP=155) + 100
      metric = hist_equal(-region_metric_slope   [0:num_candidates-1], TOP=155) + 100
                    
      metric_map = bytarr(emap_col_dim, emap_row_dim)
      metric_map[candidate_col, candidate_row] = metric
      
      ps_to_show = metric_map[ps_cmin:ps_cmax,ps_rmin:ps_rmax] 
      wset, 0
      tv, rebin(ps_to_show, 4*(ps_cmax-ps_cmin+1), 4*(ps_rmax-ps_rmin+1), /SAMPLE)
    endif
    

    ;; Accept the chosen pixel into the background region.
    ;; The running sum R_bkg_model_counts is maintained outside the candidate structures.
    R = CS[accept_ind]
    R_bkg_model_counts       += R.model_counts
    pixel_status[R.col,R.row] = background_status
    
    
    ;; As described earlier, the total observed counts in the background, bkg_data_counts_total,
    ;; must NOT be updated in the inner loop prior to evaluation of the candidate.
    ;; Thus we do that here, when the candidate is accepted.
    bkg_data_counts_total    += inband_data[R.col, R.row]

    ;; Remove the chosen pixel from candiate store and metric vector.
    ;; We simply overwrite the chosen pixel with the last one in the list, and
    ;; shorten the list.
    ;region_metric      [accept_ind] = region_metric      [num_candidates-1]
    ;region_metric_slope[accept_ind] = region_metric_slope[num_candidates-1]
    CS[accept_ind] = CS[num_candidates-1]
    num_candidates = num_candidates-1
    
    ;; Nominate new candiates which are adjacent to the background pixel just accepted.
    if (n_elements(nominated_candidates) NE 4) then $
      nominated_candidates = replicate(nominated_candidates[0], 4)
    nominated_candidates.col = (R.col + [1,0,-1,0])
    nominated_candidates.row = (R.row + [0,1,0,-1])

    
    ;; ------------------------------------------------------------------------
    ;; Correct the background scaling (see derivation in the AE manual) to eliminate photometric bias.
    
    ; We need below the subtracted total if NOMINAL scaling were used.
    R.subtracted_model_counts_total = R.bkg_model_counts_total * R.bkg_scaling

    ; Infer how many observed counts in the bkg region are not attributable to our models, i.e.
    ; are from a "flat" component.
    R.bkg_flat_counts      = 0.0 > (bkg_data_counts_total - R.bkg_model_counts_total)
    R.aperture_flat_counts = R.bkg_flat_counts * R.bkg_scaling
      
    if (bkg_data_counts_total LE 0) then begin
      ; When there is no observed background data then we cannot compute any "correction" to the background scaling
      R.bkg_scaling_correction = 1.0

    endif else begin
      ; Compute a "photometric bias" with respect to the modeled contaminating sources, 
      ; i.e. the total counts we think are in the aperture minus the total counts we 
      ; think will be subtracted by the proposed region, assuming nominal scaling.
      photometric_bias          = aperture_model_counts_total - R.subtracted_model_counts_total
      
      ; We have a complication when the models show zero counts expected in the aperture
      ; (aperture_model_counts_total EQ 0) and we estimate no flat component (bkg_flat_counts EQ 0).
      ; In this case a bkg region containing any model counts must be scaled to zero in order to 
      ; eliminate photometric bias.
      ; We avoid this silly result by placing an arbitrary lower limit on the scaling.
      ; We also place an arbitrary upper limit to be conservative.
      R.bkg_scaling_correction = 0.1 > (1.0 + (temporary(photometric_bias) $
                                                / $
                                                ((R.bkg_model_counts_total+R.bkg_flat_counts) * R.bkg_scaling))) $
                                     < 10.0
    endelse ; (bkg_data_counts_total GT 0)
 
    
    ;; ------------------------------------------------------------------------
    ;; Apply the correction to the scaling, and recompute quantities that depend on the scaling.
    R.bkg_scaling             *=                      R.bkg_scaling_correction
    bkg_exposurearea_corrected = R.bkg_exposurearea / R.bkg_scaling_correction
    
    ; The quantity R.aperture_flat_counts is NOT recomputed; it is a function of the NOMINAL scaling!

    ; The "flat" component would, by definition, be correctly subtracted if we used the nominal scaling.
    ; Thus, the actual flat component we will be subtracting is related to what we wanted to 
    ; subtract by the bkg_scaling correction we have adopted.    
    R.subtracted_flat_counts        = R.aperture_flat_counts * R.bkg_scaling_correction
    
    ;; Some of the vector quantities that were computed earlier for the winning
    ;; candiate were not saved in the candidate structure (for efficiency reasons).
    ;; These must be recomputed here.
    R_subtracted_model_counts       = R_bkg_model_counts       * R.bkg_scaling
    R.subtracted_model_counts_total = R.bkg_model_counts_total * R.bkg_scaling
    
    
    ;; ------------------------------------------------------------------------
    ;; Evaluate the expected (modeled) quality of the rescaled background region, 
    ;; i.e. how well it represents all the bkg components.
   
    ; Compute a background imbalance metric as simply the fraction of observed bkg data that we think
    ; comes from the "wrong" background component.
    ; In the expressions below we compute the biases (model - subtracted) for each bkg component, including
    ; the flat component, then sum the absolute values of those biases, and then divide by two to handle
    ; double counting, and then normalize by the total counts expected in the aperture.
    R.background_imbalance = (       (abs(R.aperture_flat_counts  - R.subtracted_flat_counts ) $
                              + total(abs(  aperture_model_counts - R_subtracted_model_counts),1))) $
                             / $
                             (1 > 2.0 * ( R.aperture_flat_counts + aperture_model_counts_total)) 
                   
    
    ;; Print some status.
    if (verbose GE 2) then begin
      ps_to_show = pixel_status[ps_cmin:ps_cmax,ps_rmin:ps_rmax] > masked_status
      wset, 1
      tvscl, rebin(ps_to_show, 4*(ps_cmax-ps_cmin+1), 4*(ps_rmax-ps_rmin+1), /SAMPLE)
                                       
      if (verbose GE 3) then begin
        ; Save and plot the three terms of the metric:
        term1[num_bkg_pixels] = R.bkg_scaling
        term2[num_bkg_pixels] = R.background_imbalance
        term3[num_bkg_pixels] = aperture_model_counts_total - R.subtracted_model_counts_total
        wset, 2
        ind_min = 0 > (num_bkg_pixels-200)
        y1 = term1[ind_min:num_bkg_pixels]
        y2 = term2[ind_min:num_bkg_pixels]
        y3 = term3[ind_min:num_bkg_pixels]
        plot,  y1, PSYM=1, YRANGE=minmax([y1,y2,y3]) ; +        is scaling   
        oplot, y2, PSYM=2                            ; *        is background imbalance
        oplot, y3, PSYM=4                            ; diamond  is photometric bias

        print, bkg_data_counts_total, F='(%"\n%d counts in bkg region")'
        if (verbose GE 4) then begin
          flag = (abs(aperture_model_counts) GT 0.1) OR (abs(R_subtracted_model_counts) GT 0.1)
          ;flag[self_index] = 0
          ind = where(flag, count)
          if (count GT 0) then begin
            forprint, domain_models.LABEL, aperture_model_counts - R_subtracted_model_counts, SUBSET=ind, F='(%"%6s %7.2f")'
          endif
        endif
        print
        print, R.bkg_scaling_correction, F='(%"scaling correction  =%5.2f")'
        print, R.background_imbalance,   F='(%"background imbalance=%5.2f")'
        print, R.compactness_bias,       F='(%"compactness metric  =%5.2f")'
    ;      print, 'accepted metric:', (current_region_metric)
      endif ;(verbose GE 3)
    endif ;(verbose GE 2)
 
    
    ;; Evaluate the stopping criteria for defining the background region (shown in block comment at top of this tool).
    bkg_normalization = 1.0 / R.bkg_scaling
    
    ; To catch infinite looping we look for NaN values used in the stopping criteria.
    vital_values = [bkg_normalization, R.background_imbalance, bkg_data_counts_total]
    if (~array_equal(finite(vital_values),1)) then begin
      message, 'ERROR: NaN value found in stopping criteria'
      GOTO, FAILURE
    endif
    
    all_goals_met      = (bkg_normalization      GE BKSCL_GL)                      && $
                         (R.background_imbalance LE background_imbalance_threshold) && $
                         (bkg_data_counts_total  GE min_num_cts[ii])

    reached_BKSCL_LO   = (bkg_normalization      GE BKSCL_LO)
    reached_BKSCL_HI   = (bkg_normalization      GE BKSCL_HI)
    

    ;; ------------------------------------------------------------------------
    ; We need to collect information to be used later to cast "votes" for adjustments to BKSCL_LO and BKSCL_HI.
    
    ; For the BKSCL_HI vote, we must record the smallest bkg_normalization value with acceptible bkg_data_counts_total.
    if (bkg_data_counts_total GE min_num_cts[ii]) then BKSCL_HI_vote <= bkg_normalization
    
    
    ; For the BKSCL_LO vote, we must record information about the behavior of the background imbalance metric as we
    ; proceeded through the search:
    ; 1. Among the regions with acceptible background imbalance, keep track of the one with the largest normalization.
    ;
    ; 2. Since there may be no acceptible regions at all, also keep track of the region with the smallest background imbalance.
    
    if (R.background_imbalance GT background_imbalance_threshold) then begin
      ; The current bkg region is bad, so update lowest_imbalance_region.
      if (R.background_imbalance LE lowest_imbalance_region.background_imbalance) then begin
        lowest_imbalance_region.background_imbalance = R.background_imbalance
        lowest_imbalance_region.bkg_normalization    = bkg_normalization
      endif
      
      ; It's important to realize that background imbalance is NOT a monotonic metric; it will normally tend to oscillate 
      ; around zero as the algorithm proceeds because enlargement of the bkg region is quantized.
      ; In cases where the region completely covers a neighboring source background imbalance will tend to start growing since
      ; we have "run out" of samples for that neighbor.
      ; At any point in the search you can't really know if a background imbalance violation is going to get better or worse if you proceed.
      ; Thus, we maintain a "reserve" marker in the sequence of bkg regions built by the search that indicates the bkg region 
      ; that we would prefer IF the search produces a region with failing background imbalance.
      if reached_BKSCL_LO then begin
        ; The bkg region is in the allowed scaling range, and is thus a candidate for our "reserve choice".
        ; Reserve it if it's as goods or better than what we've reserved so far.
        if (R.background_imbalance LE reserve_region.background_imbalance) then begin
          reserve_region.num_bkg_pixels       = num_bkg_pixels
          reserve_region.background_imbalance = R.background_imbalance
          reserve_region.bkg_normalization    = bkg_normalization
        endif 
      endif
    endif else begin
      ; The current bkg region has low background imbalance, so update largest_uncorrupted_region.
      largest_uncorrupted_region.bkg_normalization   >= bkg_normalization
    endelse

        
    ;; ------------------------------------------------------------------------
    ; Decide if the search should stop.
    if all_goals_met || reached_BKSCL_HI      then break
    
    ; Manually break at max_num_bkg_pixels so that num_bkg_pixels comes out right in second-pass situation.
    if (num_bkg_pixels GE max_num_bkg_pixels) then break
    
  endfor  ; num_bkg_pixels loop adding pixels to background region

REGION_COMPLETE:
  ;; There seem to be ? ways we can get here:
  ;; 1. Jump to REGION_COMPLETE label because the cumulative_exposurearea integral could not be computed.
  ;; 2. Break out of for-loop when the bkg region fills the entire search domain.
  ;; 3. Break out of for-loop (just above) when (all_goals_met || reached_BKSCL_HI).
  ;; 4. The loop stops when (num_bkg_pixels GE max_num_bkg_pixels), usually on a second pass (run to adopt our "reserve choice").
  ;;
  ;; In #1 and #2 there is no guarantee that reached_BKSCL_LO is true!

  ; If the search produced a result with failing background imbalance, then we want to revert to our "reserve choice".
  if is_first_pass && (R.background_imbalance GT background_imbalance_threshold) && (reserve_region.num_bkg_pixels GT 0) then begin
    is_first_pass = 0
    
    print, 'Could not find any region in the specified scaling range with acceptible background imbalance; adopting the least-bad one.'
    ; Change hard endpoint of loop to our reserved region, then restart loop.
    max_num_bkg_pixels = reserve_region.num_bkg_pixels
    GOTO, REPEAT_SEARCH
  endif
  
  if ~reached_BKSCL_LO then print, CATALOG_NAME[ii], LABEL[ii], bkg_normalization, BKSCL_LO, F='(%"WARNING! %s (%s): adopted region has BACKSCAL (%0.1f) smaller than BKSCL_LO (%0.1f).")'  


  if ~finite(R.bkg_scaling) then begin
    print, 'ERROR: R.bkg_scaling is not finite.'
    GOTO, FAILURE
  endif
  
  PROB_NO_SOURCE_binomial = binomial(SRC_CNTS[ii], $
                                     SRC_CNTS[ii] + bkg_data_counts_total, $
                                     R.bkg_scaling / (R.bkg_scaling+1D) , /DOUBLE) > 0
  
; PROB_NO_SOURCE_poisson  = (1 - poisson_distribution(bkg_data_counts_total*R.bkg_scaling, SRC_CNTS[ii] - 1)) > 0
    
  dt = systime(1)-t0
  print, num_bkg_pixels, dt, F='(%"Accepted %6d background pixels in %5d seconds.")' 
  processing_rate[ii] = num_bkg_pixels/dt
  
  
  ;; Summarize the quality of the background region.
  src_cnts_error        = (1 + sqrt(SRC_CNTS[ii]          + 0.75))
  bkg_subtraction_error = (1 + sqrt(bkg_data_counts_total + 0.75)) * R.bkg_scaling

  ; Some of the vector quantities that were computed earlier for the winning
  ; candiate were not saved in the candidate structure (for efficiency reasons).
  ; These must be recomputed here.
  R_subtracted_model_counts = R_bkg_model_counts * R.bkg_scaling
  
    
  print, bkg_data_counts_total, R.bkg_exposurearea/src_exposurearea, bkg_normalization, F='(%"\nBackground region: %d in-band background counts; nominal normalization =%6.1f; adopted normalization =%6.1f")' 
  
  print, SRC_CNTS[ii], src_cnts_error,                               F='(%"SRC_CNTS                              =%7.1f (+-%5.1f)")' 

  print, bkg_data_counts_total*R.bkg_scaling, bkg_subtraction_error, F='(%"bkg counts in aperture                = %6.1f (+-%5.1f)")' 

  print, PROB_NO_SOURCE_binomial,                                    F='(%"PROB_NO_SOURCE                        = %8.2g")'
; print, PROB_NO_SOURCE_binomial, PROB_NO_SOURCE_poisson, F='(%"PROB_NO_SOURCE, actual and asymptotic: %8.2g %8.2g")'
  
  print, "PREDICTED BIAS FROM CONTAMINATING SOURCES:"
  print, "         contaminating source label"
  print, "         |     PSF normalization"
  print, "         |     |     predicted to be in extraction aperture"
  print, "         |     |     |     predicted to be subtracted by background region"
  print, "         |     |     |     |"
  print, round(domain_models[self_index].counts_in_model), R_subtracted_model_counts[self_index], F='(%"      self %5d   --- %5.1f  (counts)")'
  print, R.aperture_flat_counts, R.subtracted_flat_counts,                   F='(%"      flat       %5.1f %5.1f")'

  flag = (abs(aperture_model_counts) GT 0.1) OR (abs(R_subtracted_model_counts) GT 0.1)
  flag[self_index] = 0
  ind = where(flag, count)
  if (count GT 0) then begin
    forprint, domain_models.LABEL, round(domain_models.counts_in_model), (aperture_model_counts), (R_subtracted_model_counts), SUBSET=ind, F='(%"%10s %5d %5.1f %5.1f")'
  endif
  print, R.aperture_flat_counts+aperture_model_counts_total, R.subtracted_flat_counts+total(R_subtracted_model_counts), F='(%"-------------------------\ntotal:           %5.1f %5.1f")'
  print, R.background_imbalance,  F='(%"background imbalance=%6.2f")'
  print, R.compactness_bias,      F='(%"compactness metric  =%6.2f")'

  
  ;; Ok, a background region is now defined to be the pixels where pixel_status[col,row] EQ background_status.
  region_index = where(pixel_status EQ background_status)
  index_to_point, region_index, col, row, size(pixel_status)

  ; Make a ds9 region file which marks the masked and background region pixels.
  openw,  region1_unit, bkg_pixels_region_fn, /GET_LUN
  printf, region1_unit, F='(%"# Region file format: DS9 version 3.0 \nglobal color=DodgerBlue \nJ2000")'
  !TEXTUNIT = region1_unit  
  xy2ad, col, row, emap2wcs_astr, ra_pt, dec_pt
  forprint, TEXTOUT=5, /NoCOM, ra_pt, dec_pt, F='(%"cross point %10.6f %10.6f # tag={bkg region}")'
  
; index_to_point, where(pixel_status EQ masked_status), temp_col, temp_row, size(pixel_status)
; xy2ad, temp_col, temp_row, emap2wcs_astr, ra_pt, dec_pt
; forprint, TEXTOUT=5, /NoCOM, ra_pt, dec_pt, F='(%"x point %10.6f %10.6f # tag={mask region} color=red")'
  
  free_lun, region1_unit

  if (verbose GE 1) then begin
    cmd1 = string(my_ds9,                       F='(%"xpaset -p %s regions delete all")') 
    cmd2 = string(my_ds9, bkg_pixels_region_fn, F='(%"xpaset -p %s regions load %s")')
    cmd3 = string(my_ds9, catalog_region_fn,    F='(%"xpaset -p %s regions load %s")')
    cmd4 = string(my_ds9, RA[ii], DEC[ii],      F='(%"xpaset -p %s pan to %10.6f %10.6f wcs fk5 degrees")')
    run_command, /QUIET, [cmd1,cmd2,cmd3,cmd4]
  endif  


  ;; Write the corresponding cropped background exposure map to a file.
  ;; WE CANNOT USE HEXTRACT BECAUSE THE PHYSICAL COORDINATE SYSTEM (AND WHO KNOWS WHAT ELSE) ARE NOT UPDATED!!!
  bkg_emap               = fltarr(emap_col_dim,emap_row_dim)
  bkg_emap[region_index] = emap[region_index] 
  bkg_emap_header = emap_header

  writefits, temp_image_fn, bkg_emap, bkg_emap_header


  cmd = string(temp_image_fn, 1+min(col), 1+max(col), 1+min(row), 1+max(row), bkg_emap_fn, F="(%'dmcopy ""%s[#1=%d:%d,#2=%d:%d]"" %s ')")
  run_command, cmd
 
  
   ;; The set of emap pixels above define the actual background region to apply to the event list.
  ;; BELOW WE REQUIRE EMAP VALUE TO BE >1, INSTEAD OF >0, BECAUSE CIAO 3.0.1 HAS A BUG THAT 
  ;; CAUSES ZERO VALUES TO PASS THE >0 TEST!

  cmd1 = string(evtfile, bkg_emap_fn, temp_events_fn, $
                F="(%'dmimgpick ""%s[cols time,ccd_id,sky,pi,energy]"" %s %s method=closest')")

  cmd2 = string(temp_events_fn, bkg_events_fn, F="(%'dmcopy ""%s[#6>1]"" %s')")
  run_command, [cmd1,cmd2]


  ;; ------------------------------------------------------------------------
  ;; Extract background spectrum.
  ;; NOTE: if we ever implement a time filter on the background data then we must 
  ;; reduce bkg_exposurearea below by the ratio 
  ;; (EXPOSURE from bkg_spectrum_fn)/(EXPOSURE from bkg_events_fn) to account for the lost exposure.
  ;; Such time filtering might mess up MIN_NUM_CTS requirement!
  
  cmd = string(bkg_events_fn, DETCHANS, bkg_spectrum_fn, $
               F="(%'dmextract ""%s[bin pi=1:%d:1]"" %s opt=pha1 error=gaussian')")
  run_command, cmd

  
  ;; ------------------------------------------------------------------------
  ;; Save some statistics about the background region.
  
  ;; ------------------------------------------------------------------------
  ;; The AE convention is that the BACKSCAL keywords in spectrum files, derived from
  ;; integrals of the exposure map, are used to represent geometric
  ;; area, effective area, and integration time differences between the
  ;; source and background regions.  
  ;; The EXPOSURE keywords are NOT used for background scaling.  We set EXPOSURE=0
  ;; in the background spectrum as a flag to signify the AE convention is being used.
  ;;
  ;; Background scaling is a bit confusing.  
  ;; * In this code the variable bkg_scaling (S) is multiplied by the background.
  ;; * The BACKSCAL keyword in obs.stats is 1/bkg_scaling.
  ;; * However the keyword BACKSCAL in the background spectrum file represents
  ;;   the "measure" (integral of the emap) of the background region.
  ;;   It is later combined with BACKSCAL in the source spectrum file to get the
  ;;   actual scaling applied to the background spectrum.
  comment  = 'EXPOSURE not used for bkg scaling'
  comment2 = string(ENERG_LO, ENERG_HI, F="(%'total bkg intensity (photons/cm^2/sec/skypixel^2),, %5.3f:%6.3f keV')")
  comment3 = string(ENERG_LO, ENERG_HI, F="(%'flat bkg intensity (photons/cm^2/sec/skypixel^2),, %5.3f:%6.3f keV')")

  
  ; Write the keywords that are unique to background.pi.
  openw, unit, temp_text_fn, /GET_LUN
  printf, unit, comment, comment, comment, $
          bkg_exposurearea_corrected, '(sec*cm^2*skypixel^2); '+comment, $
          F='(%"#add\nONTIME = 0 / %s\nLIVETIME = 0 / %s\nEXPOSURE = 0 / %s\nBACKSCAL = %g / %s")'
  free_lun, unit
  
  cmd = string(bkg_spectrum_fn, temp_text_fn, F="(%'dmhedit infile=%s filelist=%s')")
  run_command, cmd, /QUIET


  ; Write the keywords that are common in background.pi and obs.stats.
  openw, unit, temp_text_fn, /GET_LUN
  printf, unit, creator_string, $
          R.bkg_scaling_correction,                            'scaling correction (already in BACKSCAL)', $
          bkg_data_counts_total / bkg_exposurearea_corrected,  comment2, $
          R.bkg_flat_counts     / bkg_exposurearea_corrected,  comment3, $
          R_subtracted_model_counts[self_index],               'expected subtracted counts from self', $
          R.background_imbalance,                              'bkg spectrum metric', $
          R.compactness_bias,                                  'bkg compactness metric', $
          current_region_metric,                               'bkg region metric', $
          F='(%"#add\nCREATOR = %s\nBACKCORR = %f / %s\nBACKGRND = %g / %s\nFLATGRND = %g / %s\nSELFCNTS = %g / %s\nBKGMETR1 = %f / %s\nBKGMETR2 = %f / %s\nBKGMETR = %f / %s")'
  free_lun, unit
  
  cmd = string(bkg_spectrum_fn, obs_stats_fn, temp_text_fn, F="(%'dmhedit infile=""%s %s"" filelist=%s')")
  run_command, cmd, /QUIET

  ; Write the keywords unique to obs.stats.
  obs_stats = headfits(obs_stats_fn, ERRMSG=error)
  if keyword_set(error) then message, 'ERROR reading '+obs_stats_fn
  
  fxaddpar, obs_stats, 'BKG_RAD',  0,                     'region is from ae_better_backgrounds'
  fxaddpar, obs_stats, 'BKG_CNTS', bkg_data_counts_total, string(ENERG_LO, ENERG_HI, F="(%'background counts, %5.3f:%6.3f keV')") 
  fxaddpar, obs_stats, 'BACKSCAL', bkg_normalization,     'normalization for BKG_CNTS' 
  fxaddpar, obs_stats, 'BKSCL_LO', BKSCL_LO,              'smallest BACKSCAL allowed'
  fxaddpar, obs_stats, 'BKSCL_GL', BKSCL_GL,              'target   BACKSCAL'
  fxaddpar, obs_stats, 'BKSCL_HI', BKSCL_HI,              'largest  BACKSCAL allowed'
  
  ; This extraction gets to cast votes, used by the adjustment algorithm, for an upper limit on BKSCL_LO and a lower limit on BKSCL_HI.
  
  if finite(largest_uncorrupted_region.bkg_normalization) then begin
    ; Acceptible regions (those with acceptible background imbalance) were found in the search. 
    ; The largest one is our vote for an upper limit on BKSCL_LO.
    BKSCL_LO_vote = largest_uncorrupted_region.bkg_normalization
  endif else begin
    ; Every single region we tested during the search failed the background imbalance criterion.
    ; The least bad one is our vote for an upper limit on BKSCL_LO.
    BKSCL_LO_vote = lowest_imbalance_region.bkg_normalization
  endelse
    
  ; If the search did NOT identify any scaling that meets the MIN_NUM_CTS goal, then cast a vote to raise BKSCL_HI by some arbitrary amount.
  ; Do NOT try to estimate the BKSCL_HI that should enclose MIN_NUM_CTS by scaling up from the number of counts found in the
  ; current region!!!  That sort of data-based adjustment here will surely harm convergence of the search since the number of
  ; counts actually found in any region has a random component.
  if ~finite(BKSCL_HI_vote)                                         then BKSCL_HI_vote = 1.20 * BKSCL_HI
    
  fxaddpar, obs_stats, 'VOTE_LO', BKSCL_LO_vote, 'vote for upper limit on BKSCL_LO' 
  fxaddpar, obs_stats, 'VOTE_HI', BKSCL_HI_vote, 'vote for lower limit on BKSCL_HI' 

  writefits, obs_stats_fn, 0, obs_stats

               
  if keyword_set(pause_for_review) then begin
    print, F='(%"\nPress return to continue to the next source ...")'
    read, '? ', cmd1
  endif  
endfor ; ii loop over the catalog

ind = where(processing_rate GT 0, count)
if (count GT 0) then print, 'Median processing rate (accepted pixels/s): ', median(processing_rate[ind])



CLEANUP:
;  if (verbose GE 1) then run_command, string(my_ds9, F='(%"xpaset -p %s exit")'), /QUIET

  if (n_elements(models) GT 0) then ptr_free, models.data_ptr

;; Restore the PFILES the caller had so it can spawn CIAO & HEASOFT commands
;; We cannot let the caller see the PFILES value used in AE because AE creates a temporary param_dir which is destroyed below.
if keyword_set(inherited_pfiles) then setenv, 'PFILES='+inherited_pfiles

if file_test(temproot) then begin
  list = reverse(file_search(temproot,'*',/MATCH_INITIAL_DOT,COUNT=count))
  if (count GT 0) then file_delete, list
  file_delete, temproot
endif

if (exit_code EQ 0) then begin
  if (n_elements(number_of_passes) EQ 0) then begin
    ; The caller is relying on the ABB instance to decide if the source models would likely benefit from another pass through the photometry and background calculations.
    num_missing_backgrounds = total(/INT, BACKSCAL EQ 0)
    if (num_missing_backgrounds GT 0) && keyword_set(build_models_only) then begin
      print, 'ERROR: ae_better_backgrounds has been asked to build source models, but some sources are missing backgrounds!'
      goto, FAILURE
    endif
    
    if (num_missing_backgrounds GT 0) && ~keyword_set(reuse_models) then begin
      print, F='(%"\n===================================================================")'
      print, num_missing_backgrounds, F="(%'WARNING!  %d sources are missing backgrounds; running an extra pass through ae_better_backgrounds to help the source models converge.')"
      print, F='(%"===================================================================\n")'
      number_of_passes = 2
    endif else number_of_passes = 1
  endif

  if (--number_of_passes GT 0) then begin
    ; Make a recursive call after decrementing number_of_passes.
    ; We omit the PHOTOMETRY_STAGE_ONLY, SKIP_PHOTOMETRY_STAGE, REUSE_MODELS, BUILD_MODELS_ONLY options since the point of the extra passes is to get the models to converge!
    ae_better_backgrounds, obsname, EVTFILE_BASENAME=evtfile_basename, $
      NUMBER_OF_PASSES=number_of_passes, $ 
  
      THETA_RANGE=theta_range, BACKGROUND_MODEL_FILENAME=background_model_filename, $
    
      SOURCE_NOT_OBSERVED=source_not_observed, GENERIC_RMF_FN=generic_rmf_fn, $
      
      SRCLIST_FILENAME=srclist_fn, EXTRACTION_NAME=extraction_name, $
      EMAP_BASENAME=emap_basename, $
  
      MIN_NUM_CTS=min_num_cts, BACKGROUND_IMBALANCE_THRESHOLD=background_imbalance_threshold, $
      COMPACTNESS_GAIN=compactness_gain, $
      
      VERBOSE=verbose, PAUSE_FOR_REVIEW=pause_for_review, $
        
      SAVEFILE_BASENAME=savefile_basename
  endif
  return 
endif else begin
  print, 'ae_better_backgrounds: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end ; ae_better_backgrounds



;#############################################################################
;;; Tool to analyze the quality of background extractions, and to adjust the 
;;; background scaling range (BKSCL_LO,BKSCL_HI) allowed for each source, and
;;; the target background scaling (BKSCL_GL).

;;; The OVERLAP_LIMIT parameter is the same one accepted by the MERGE stage of AE.
;;; It defines extractions that are too crowded to be merged, and thus should have no vote
;;; here on the BACKSCAL range.
;;;
;;; The MIN_NUM_CTS parameter is a goal for the *merged* background spectrum.
;;; When this goal is in conflict with the "background imbalance" goal in ABB, the MIN_NUM_CTS will defer (i.e. not be met).
;#############################################################################
PRO ae_adjust_backscal_range, OVERLAP_LIMIT=overlap_limit, MIN_NUM_CTS=min_num_cts, SRCLIST_FILENAME=srclist_fn, RERUN_SRCLIST_FILENAME=rerun_srclist_fn, EXTRACTION_NAME=extraction_name, SCALE_RANGE_BY_FACTOR=scale_range_by_factor, SET_LIMIT_RATIO=set_limit_ratio

exit_code = 0
creator_string = "ae_adjust_backscal_range, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

if n_elements(overlap_limit) EQ 0 then overlap_limit    = 0.10
if ~keyword_set(min_num_cts)      then min_num_cts      = 100
if ~keyword_set(srclist_fn)       then       srclist_fn = 'all.srclist'
if ~keyword_set(rerun_srclist_fn) then rerun_srclist_fn = 'rerun.srclist'

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
  
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ae_make_catalog: ERROR: no entries read from source list ', srclist_fn
  GOTO, FAILURE
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\nae_adjust_backscal_range: %d sources found in catalog.")'


if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)

src_stats_basename       = 'source.stats'
obs_stats_basename         = 'obs.stats'

obsid_list     = ''
LABEL          = strarr(num_sources)
conflict_flag  = bytarr(num_sources)
rerun_flag     = bytarr(num_sources)
new_BKSCL_LO   = fltarr(num_sources)
new_BKSCL_GL   = fltarr(num_sources)
new_BKSCL_HI   = fltarr(num_sources)
requested_BKSCL_GL_scaling = fltarr(num_sources)

obs_data_template =  { $
                      SRC_CNTS:0L, $
                      BKG_CNTS:0L, $
                      BACKSCAL:0.0,$
                      BKGMETR1:0.0,$
                      VOTE_LO :0.0,$
                      VOTE_HI :0.0,$
                      OVERLAP :0.0 $
                     }
                   
for ii = 0, num_sources-1 do begin
  ;; Construct filenames.
  sourcedir            = sourcename[ii] + '/'
  unnamed_src_stats_fn = sourcedir + src_stats_basename

  ; Look up allowed range of background normalizations.
  unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
  
  if (~keyword_set(error)) then begin
    LABEL[ii]= sxpar(unnamed_src_stats, 'LABEL')
    BKSCL_LO = sxpar(unnamed_src_stats, 'BKSCL_LO', COUNT=count1)
    BKSCL_GL = sxpar(unnamed_src_stats, 'BKSCL_GL', COUNT=count2)
    BKSCL_HI = sxpar(unnamed_src_stats, 'BKSCL_HI', COUNT=count3)
  endif else message, 'ERROR reading '+unnamed_src_stats_fn

  if (count1*count2*count3 EQ 0) then begin
    print, 'ERROR: '+unnamed_src_stats_fn+' is missing the keywords BKSCL_LO,BKSCL_GL,BKSCL_HI.'
    print, "See 2009-06-22 What's New entry in the AE manual for patch to update your AE sources."
    GOTO, FAILURE
  endif


  ; Find a list of obs.stats files and read information related to extractions..
  pattern = sourcename[ii] + '/*/' + extraction_subdir[ii] + obs_stats_basename
  obs_stats_fn = file_search( pattern, COUNT=num_obs )
  
  if (num_obs EQ 0) then begin
    print, sourcename[ii], LABEL[ii], F='(%"%s (%s) was not observed.")'
    continue
  endif
  
  these_obsids = strmid(obs_stats_fn,  1+reform(strpos(obs_stats_fn, '/'), 1,num_obs))
  these_obsids = strmid(these_obsids, 0, reform(strpos(these_obsids, '/'), 1,num_obs))

  ;; Handle the tool mode where the existing BKSCL range is scaled by a specified factor.
  if keyword_set(scale_range_by_factor) then begin
    rerun_flag[ii] = 1  
    BKSCL_LO *= scale_range_by_factor
    BKSCL_GL *= scale_range_by_factor
    BKSCL_HI *= scale_range_by_factor
    
    print, sourcename[ii], LABEL[ii], BKSCL_LO, BKSCL_GL, BKSCL_HI, F='(%"%s (%s): [%0.1f--%0.1f--%0.1f]")'

    fxaddpar, unnamed_src_stats, 'BKSCL_LO', BKSCL_LO, 'smallest BACKSCAL allowed'
    fxaddpar, unnamed_src_stats, 'BKSCL_GL', BKSCL_GL, 'target   BACKSCAL'
    fxaddpar, unnamed_src_stats, 'BKSCL_HI', BKSCL_HI, 'largest  BACKSCAL allowed'
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats

    new_BKSCL_LO[ii] = BKSCL_LO
    new_BKSCL_GL[ii] = BKSCL_GL
    new_BKSCL_HI[ii] = BKSCL_HI

    obsid_list   = [obsid_list, these_obsids]
    obsid_list   =  obsid_list[uniq(obsid_list,sort(obsid_list))]
    
    continue
  endif
  

  ;; Handle the tool mode where the existing BKSCL limits are adjusted to have a specified ratio.
  ;; We arbitrarily let the BKSCL_HI limit remain fixed and adjust the BKSCL_LO limit.
  if keyword_set(set_limit_ratio) then begin
    rerun_flag[ii] = 1  
    BKSCL_LO = BKSCL_HI / set_limit_ratio
    BKSCL_GL = BKSCL_LO > (BKSCL_GL < BKSCL_HI)
    
    print, sourcename[ii], LABEL[ii], BKSCL_LO, BKSCL_GL, BKSCL_HI, F='(%"%s (%s): [%0.1f--%0.1f--%0.1f]")'

    fxaddpar, unnamed_src_stats, 'BKSCL_LO', BKSCL_LO, 'smallest BACKSCAL allowed'
    fxaddpar, unnamed_src_stats, 'BKSCL_GL', BKSCL_GL, 'target   BACKSCAL'
    fxaddpar, unnamed_src_stats, 'BKSCL_HI', BKSCL_HI, 'largest  BACKSCAL allowed'
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats

    new_BKSCL_LO[ii] = BKSCL_LO
    new_BKSCL_GL[ii] = BKSCL_GL
    new_BKSCL_HI[ii] = BKSCL_HI

    obsid_list   = [obsid_list, these_obsids]
    obsid_list   =  obsid_list[uniq(obsid_list,sort(obsid_list))]
    
    continue
  endif
  



  ;; Read info from each single extraction.
  obs_data = replicate(obs_data_template,num_obs)
  for jj=0,num_obs-1 do begin
    header = headfits(obs_stats_fn[jj], ERRMSG=error )
    if (keyword_set(error)) then message, 'ERROR reading ' + obs_stats_fn[jj] 
    obs_data[jj].SRC_CNTS = sxpar(header, 'SRC_CNTS')
    obs_data[jj].BKG_CNTS = sxpar(header, 'BKG_CNTS')
    obs_data[jj].BACKSCAL = sxpar(header, 'BACKSCAL')
    obs_data[jj].BKGMETR1 = sxpar(header, 'BKGMETR1')
    obs_data[jj].VOTE_LO  = sxpar(header, 'VOTE_LO')
    obs_data[jj].VOTE_HI  = sxpar(header, 'VOTE_HI')
    obs_data[jj].OVERLAP  = sxpar(header, 'OVERLAP')
  endfor ;jj
               
  ;; Ignore the extractions that AE's MERGE stage is going to be excluded due to excessive OVERLAP.
  ;; The code below is lifted from AE's MERGE stage.
  accepted_ind = where(obs_data.OVERLAP LT overlap_limit, num_obs)
    
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
  
  
  ;; ------------------------------------------------------------------------
  ; Adjust the target bkg scaling (BKSCL_GL) to better achieve goals in the merged dataset (assuming that all extractions are merged).
  
  ; AE is going to use the merged background data to compute the PROB_NO_SOURCE statistic.
  ; We could attempt to compute a confidence interval on PROB_NO_SOURCE, establish some sort of requirement 
  ; on that confidence interval, and then compute how many BKG_CNTS are needed.
  ; Instead, we'll punt and force the observer to specify a goal for the total number of BKG_CNTS 
  ; (assuming that all extractions are merged).
  ; Estimate the BACKSCAL range resizing needed to achieve this MIN_NUM_CTS goal.
  resize1 = min_num_cts / total(obs_data.BKG_CNTS)
  if ~finite(resize1) then resize1 = 10
  
  ; It seems reasonable to require that the statistical uncertainty in the background subtraction should be
  ; smaller than the statistical uncertainty in the counts extracted from the source aperture.
  ; In other words, of the two noise terms in AE's calculation of error on NET_CNTS (assuming that all extractions are merged), 
  ; the term from the extraction aperture should dominate.
  ;
  ; Given:
  ;   NET_CNTS = SRC_CNTS - (BKG_CNTS * bkg_scaling)
  ;     where bkg_scaling is the scaling for the merged data.
  ;
  ;   src_cnts_error = (1 + sqrt(SRC_CNTS + 0.75))
  ;   bkg_cnts_error = (1 + sqrt(BKG_CNTS + 0.75))                              (1)
  ;
  ; Then the error on NET_CNTS is:
  ;   sqrt( src_cnts_error^2 + (bkg_cnts_error*bkg_scaling)^2 )
  ;
  ; We adopt this arbitrary goal: 
  ;      src_cnts_error = 4.0*bkg_cnts_error*bkg_scaling                        (2)
  ; Then the error on NET_CNTS is:
  ;      sqrt(src_cnts_error^2 + (src_cnts_error/4)^2) = src_cnts_error * 1.03
  ; Thus, the photometry error is only 3% larger than it would be with an infinitely large background sample.
  ;
  ; Thus, we are trying to estimate what sized bkg region would achive the goal in equation (2).
  ; Changing the bkg region size should change both BKG_CNTS and bkg_scaling, so there is not an obvious algebraic solution 
  ; for the desired resizing of the background region(s).  We'll have to iteratively estimate...
 
  src_cnts_error      = (1 + sqrt(total(/INT, obs_data.SRC_CNTS)   + 0.75))
  current_bkg_scaling = total(obs_data.BKG_CNTS/obs_data.BACKSCAL) / total(obs_data.BKG_CNTS)
  
  if ~finite(current_bkg_scaling) then begin
    resize2 = 10
  endif else begin
    resize2 = 1.0
    for kk=1,100 do begin
      proposed_bkg_scaling = current_bkg_scaling / resize2
      proposed_BKG_CNTS    = total(obs_data.BKG_CNTS)     * resize2
      
      proposed_bkg_cnts_error = (1 + sqrt(proposed_BKG_CNTS + 0.75))
      
      if (src_cnts_error GT 4.0*proposed_bkg_cnts_error*proposed_bkg_scaling) then begin
        ; The background subtraction error meets the goal; we can shrink the bkg region.
        resize2 /= 1.05
      endif else begin
        ; The background subtraction error is too large; we must expand the bkg region.
        resize2 *= 1.10
      endelse
;     print, resize2
    endfor ; kk
  endelse
  
  
  ; The larger of the two rescaling requests governs our action.
  resize =  resize1 > resize2
  
  if (resize LT 0.50) then begin
    ; The bkg regions want to significantly shrink; let's avoid a huge adjustment.
    ; The width of this "dead band", resize=[0.50,1.0], should control how fast these adjustments converge.
    BKSCL_GL *=       (0.2 > resize)
  endif else if (resize GT 1.0) then begin
    ; The bkg regions must grow; let's avoid both a tiny or a huge adjustment.
    BKSCL_GL *= 1.2 > (resize < 5.0)
  endif else begin
    ; The bkg regions want to shrink only a little (0.50 < resize < 1.0).  Let's declare success and let this source be stable! 
    ; 
  endelse
  
  requested_BKSCL_GL_scaling[ii] = resize

  
  ;; ------------------------------------------------------------------------
  ; Reconcile the new goal (BKSCL_GL) with any requests from the individual extractions to move the bkg scaling range endpoints.
  
  ; A VOTE_* value of 0 means "no vote".
  ind = where(obs_data.VOTE_LO EQ 0, count)
  if (count GT 0) then obs_data[ind].VOTE_LO = !VALUES.F_NAN
  ind = where(obs_data.VOTE_HI EQ 0, count)
  if (count GT 0) then obs_data[ind].VOTE_HI = !VALUES.F_NAN
  
  ; The smallest LO vote and the largest HI vote are the relevant ones.
  ; We arbitrarily prohibit very small bkg scalings.
  VOTE_LO = min(obs_data.VOTE_LO, /NAN) 
  VOTE_HI = max(obs_data.VOTE_HI, /NAN)
  if finite(VOTE_LO) then VOTE_LO >= 0.5
  
  if ((VOTE_HI/VOTE_LO) GT (BKSCL_HI/BKSCL_LO)) then begin
    ; Both votes exist (are not NaN) and can NOT be satisfied (their ratio exceeds the ratio of the scaling range).
    ; In version 3368 (2009-02-25) we coded a compromise scaling range that produces equal shortfalls at both ends, i.e.
    ;   (BKSCL_LO-VOTE_LO) = (VOTE_HI-BKSCL_HI)
    ; while maintaining the original ratio (BKSCL_HI/BKSCL_LO).
    ;
    ; However, we subsequently decided to let VOTE_LO (from background imbalance goal) override VOTE_HI (from MIN_NUM_CTS goal).
    ; Thus, we simply discard the VOTE_HI value and carry on.
    conflict_flag[ii] = 1
    VOTE_HI = !VALUES.F_NAN
  endif 
  
  ; Calculate the constraints on scaling implied by the single-obsid votes.
  rescale_min = (VOTE_HI/BKSCL_HI)
  if ~finite(rescale_min) then rescale_min = -!VALUES.F_INFINITY
  
  rescale_max = (VOTE_LO/BKSCL_LO)
  if ~finite(rescale_max) then rescale_max =  !VALUES.F_INFINITY
  
  ; Calculate the request for scaling implied by the new BKSCL_GL value.
  rescale_request                                  = (BKSCL_GL/BKSCL_HI) > 1.0
  if (rescale_request EQ 1.0) then rescale_request = (BKSCL_GL/BKSCL_LO) < 1.0
  
  ; The single-obsid votes get the first say in deciding the direction of the scaling.
  ; If they don't care, then the new goal (BKSCL_GL) can decide.
  if      (rescale_min     GT 1.0) then direction = 'up' $
  else if (rescale_max     LT 1.0) then direction = 'down' $
  else if (rescale_request GT 1.0) then direction = 'up' $
  else if (rescale_request LT 1.0) then direction = 'down' $
  else direction = ''

  if            (direction EQ 'up') then begin
    ; Try to scale upward to satisfy both votes and goal, but no farther than rescale_max.
    rescale = rescale_max < (rescale_min > rescale_request)
  
  endif else if (direction EQ 'down') then begin
    ; Try to scale downward to satisfy both votes and goal, but no farther than rescale_min.
    rescale = rescale_min > (rescale_max < rescale_request)
  
  endif else begin
    rescale = 1.0
  endelse
  
  ; Apply the rescaling to the endpoints of the range.
  BKSCL_LO *= rescale
  BKSCL_HI *= rescale
  
  ; Force the goal to lie in the new range.
  BKSCL_GL = BKSCL_LO > (BKSCL_GL < BKSCL_HI)
  
  
  if (BKSCL_GL LT BKSCL_LO) then message, 'BUG!'
  if (BKSCL_GL GT BKSCL_HI) then message, 'BUG!'
  if ~finite(BKSCL_LO) || ~finite(BKSCL_GL) || ~finite(BKSCL_HI) then message, 'BKSCL_* values are not finite!'
  if (BKSCL_LO LE 0)   || (BKSCL_GL LE 0)   || (BKSCL_HI LE 0)   then message, 'BKSCL_* values are not positive!'
  
  ;; ------------------------------------------------------------------------
  ; Save the adjusted scaling range and goal parameters only if any have changed significantly.
  fractional_change = [ BKSCL_LO / sxpar(unnamed_src_stats, 'BKSCL_LO'), $
                        BKSCL_GL / sxpar(unnamed_src_stats, 'BKSCL_GL'), $
                        BKSCL_HI / sxpar(unnamed_src_stats, 'BKSCL_HI') ]
         
  
  if (max(fractional_change) GT 1.10) || (min(fractional_change) LT 0.90)  then begin
  
    rerun_flag[ii] = 1  
    print, sourcename[ii], LABEL[ii], BKSCL_LO, BKSCL_GL, BKSCL_HI, F='(%"%s (%s): [%0.1f--%0.1f--%0.1f]")'

    fxaddpar, unnamed_src_stats, 'BKSCL_LO', BKSCL_LO, 'smallest BACKSCAL allowed'
    fxaddpar, unnamed_src_stats, 'BKSCL_GL', BKSCL_GL, 'target   BACKSCAL'
    fxaddpar, unnamed_src_stats, 'BKSCL_HI', BKSCL_HI, 'largest  BACKSCAL allowed'
    writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
    
    new_BKSCL_LO[ii] = BKSCL_LO
    new_BKSCL_GL[ii] = BKSCL_GL
    new_BKSCL_HI[ii] = BKSCL_HI
    
    obsid_list   = [obsid_list, these_obsids]
    obsid_list   =  obsid_list[uniq(obsid_list,sort(obsid_list))]
  endif
endfor ;ii

ind = where(rerun_flag, count)
if (count EQ 0) then begin 
  file_delete, rerun_srclist_fn, /ALLOW_NONEXISTENT
  print, 'No adjustments are required.'
endif else begin
  print
  forprint, TEXTOUT=rerun_srclist_fn, SUBSET=ind, sourcename, LABEL, new_BKSCL_LO, new_BKSCL_GL, new_BKSCL_HI, F='(%"%s (%s): [%0.1f--%0.1f--%0.1f]")', /NoCOMMENT, /SILENT
  print, count, rerun_srclist_fn, n_elements(obsid_list)-1, F='(%"%d sources listed in ''%s'' require new background extractions from these %d ObsIds:")'
  forprint, obsid_list[1:*]
endelse

ind = where(requested_BKSCL_GL_scaling GT 1.0, count)
if (count GT 0) then begin
  ind = ind[reverse(sort(requested_BKSCL_GL_scaling[ind]))]
  print
  fn = 'want_more_bkg_cnts.srclist'
  forprint, TEXTOUT=fn, SUBSET=ind, sourcename, LABEL, requested_BKSCL_GL_scaling,  F='(%"%s (%s): %0.1f")', /NoCOMMENT, /SILENT
  print, count, fn, fn, F='(%"WARNING! The %d sources listed in ''%s'' have too few background counts to satisfy the goals imposed on the merged background spectrum, perhaps because the BACKGROUND_IMBALANCE_THRESHOLD goal in a single extraction precludes adjustment of the BACKSCAL range.\nThe fractional background growth desired is shown in ''%s''.\nYou may wish to review the background regions for these sources in the /SHOW stage.")'
endif


ind = where(conflict_flag, count)
if (count GT 0) then begin
  print
  fn = 'conflict.srclist'
  forprint, TEXTOUT=fn, SUBSET=ind, sourcename, LABEL, F='(%"%s (%s): ")', /NoCOMMENT, /SILENT
  print, count, fn, F='(%"WARNING! The %d sources listed in ''%s'' have conflicting votes for adjusting the BACKSCAL range.\nSome extractions have voted for larger regions (to satisfy the MIN_NUM_CTS goal imposed on each extraction) and some have voted for smaller regions (to satisfy the BACKGROUND_IMBALANCE_THRESHOLD goal imposed on each extraction).\nYou may wish to review the background regions for these sources in the /SHOW stage.\nThe votes for smaller regions have been respected; the votes for larger regions have been ignored.")'
endif


CLEANUP:
if (exit_code EQ 0) then return $
else begin
  print, 'ae_adjust_backscal_range: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
end   ; ae_adjust_backscal_range


;#############################################################################
;;; Tool to reset BKSCL_* keywords in source.stats using values in an obs.stats. 
;#############################################################################
PRO ae_reset_backscal_range, SRCLIST_FILENAME=srclist_fn

if ~keyword_set(srclist_fn)       then       srclist_fn = 'all.srclist'
readcol, srclist_fn, sourcename, FORMAT='A', COMMENT=';'

; Trim whitespace and remove blank lines.
sourcename = strtrim(sourcename,2)
ind = where(sourcename NE '', num_sources)

if (num_sources EQ 0) then begin
  print, 'ae_make_catalog: ERROR: no entries read from source list ', srclist_fn
  retall
endif

sourcename = sourcename[ind]
print, num_sources, F='(%"\nae_reset_backscal_range: %d sources found in catalog.")'

if keyword_set(extraction_name) then extraction_subdir = extraction_name + '/' $
                                else extraction_subdir = ''
if (n_elements(extraction_subdir) EQ 1) then extraction_subdir = replicate(extraction_subdir,num_sources>1)

src_stats_basename       = 'source.stats'
obs_stats_basename         = 'obs.stats'

for ii = 0, num_sources-1 do begin
  ;; Construct filenames.
  sourcedir            = sourcename[ii] + '/'
  unnamed_src_stats_fn = sourcedir + src_stats_basename

  ; Look up allowed range of background normalizations.
  unnamed_src_stats = headfits(unnamed_src_stats_fn, ERRMSG=error)
  
  if (~keyword_set(error)) then begin
  endif else message, 'ERROR reading '+unnamed_src_stats_fn
  
  ; Find a list of obs.stats files and read information related to extractions..
  pattern = sourcename[ii] + '/*/' + extraction_subdir[ii] + obs_stats_basename
  obs_stats_fn = file_search( pattern, COUNT=num_obs )
  
  if (num_obs EQ 0) then begin
    print, sourcename[ii], LABEL[ii], F='(%"%s (%s) was not observed.")'
    continue
  endif
  
  header = headfits(obs_stats_fn[0], ERRMSG=error )
  if (keyword_set(error)) then message, 'ERROR reading ' + obs_stats_fn[0] 

  fxaddpar, unnamed_src_stats, 'BKSCL_LO', sxpar(header, 'BKSCL_LO'), 'smallest BACKSCAL allowed'
  fxaddpar, unnamed_src_stats, 'BKSCL_GL', sxpar(header, 'BKSCL_GL'), 'target   BACKSCAL'
  fxaddpar, unnamed_src_stats, 'BKSCL_HI', sxpar(header, 'BKSCL_HI'), 'largest  BACKSCAL allowed'
  writefits, unnamed_src_stats_fn, 0, unnamed_src_stats
endfor ;ii    
return
end




;#############################################################################
;;; Code for analyzing Pb values from three bands to produce Pbslice.reg and prune.srclist.
;;; This code was originally in recipe.txt, and is a routine now for convenience.
;;; All sorts of input file names and assumptions are hard-coded, based on recipe.txt!!
;#############################################################################
PRO ae_analyze_pb, ANALYZE=analyze, BOUNDARIES=boundaries, INVALID_THRESHOLD=invalid_threshold, $
                   MAKE_REGIONS=make_regions, _EXTRA=extra

creator_string = "ae_analyze_pb, version"+strmid("$Date: 2009-08-12 11:00:14 -0400 (Wed, 12 Aug 2009) $", 6, 11)
print, creator_string
print, systime()

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
  
if keyword_set(analyze) then begin
  ; Define energy bands for which Pb will be examined.
  band_name   = ['Full','Soft','Hard']
  band_full   = 0
  band_soft   = 1
  band_hard   = 2
  num_bands   = 3
  
  ; Read summary FITS tables.
  bt_full = mrdfits('tables/Pb_full_band.collated', 1)
  bt_soft = mrdfits('tables/Pb_soft_band.collated', 1)
  bt_hard = mrdfits('tables/Pb_hard_band.collated', 1)
  num_sources = n_elements(bt_full)
  
  ; Verify that only one energy band appears in the collations.
  if (n_elements(bt_full[0].ENERG_HI) GT 1) || (n_elements(bt_soft[0].ENERG_HI) GT 1) || (n_elements(bt_hard[0].ENERG_HI) GT 1) then begin
    print, 'ERROR: each Pb_*_band.collated must have photometry for only ONE energy band.'
    retall
  endif
  
  print, bt_full[0].ENERG_LO, bt_full[0].ENERG_HI, F='(%"Full band is %3.1f - %4.1f keV.")'
  print, bt_soft[0].ENERG_LO, bt_soft[0].ENERG_HI, F='(%"Soft band is %3.1f - %4.1f keV.")'
  print, bt_hard[0].ENERG_LO, bt_hard[0].ENERG_HI, F='(%"Hard band is %3.1f - %4.1f keV.")'

  CATALOG_NAME = strtrim(bt_full.CATALOG_NAME,2)
  LABEL        = strtrim(bt_full.LABEL       ,2)
  PROVENAN     = strtrim(bt_full.PROVENAN    ,2)

  NUM_OBS      = fltarr(num_bands, num_sources)
  OVRLP_LM     = fltarr(num_bands, num_sources)
  OVRLP_LO     = fltarr(num_bands, num_sources)
  SRC_CNTS     = fltarr(num_bands, num_sources)
  NET_CNTS     = fltarr(num_bands, num_sources)
  Pb           = fltarr(num_bands, num_sources)

  NUM_OBS [band_full,*] = bt_full.NUM_OBS      
  OVRLP_LM[band_full,*] = bt_full.OVRLP_LM      
  OVRLP_LO[band_full,*] = bt_full.OVRLP_LO      
  SRC_CNTS[band_full,*] = bt_full.SRC_CNTS      
  NET_CNTS[band_full,*] = bt_full.NET_CNTS      
  Pb      [band_full,*] = bt_full.PROB_NO_SOURCE
  
  NUM_OBS [band_soft,*] = bt_soft.NUM_OBS      
  OVRLP_LM[band_soft,*] = bt_soft.OVRLP_LM      
  OVRLP_LO[band_soft,*] = bt_soft.OVRLP_LO      
  SRC_CNTS[band_soft,*] = bt_soft.SRC_CNTS      
  NET_CNTS[band_soft,*] = bt_soft.NET_CNTS      
  Pb      [band_soft,*] = bt_soft.PROB_NO_SOURCE
  
  NUM_OBS [band_hard,*] = bt_hard.NUM_OBS      
  OVRLP_LM[band_hard,*] = bt_hard.OVRLP_LM      
  OVRLP_LO[band_hard,*] = bt_hard.OVRLP_LO      
  SRC_CNTS[band_hard,*] = bt_hard.SRC_CNTS      
  NET_CNTS[band_hard,*] = bt_hard.NET_CNTS      
  Pb      [band_hard,*] = bt_hard.PROB_NO_SOURCE

  ; Pb values (in each of 3 bands) fall into four categories:
  ; 3. Not observed:      (NUM_OBS EQ 0)              Pb unavailable because no extractions found.
  ; 2. Excessive overlap: (OVRLP_LO GE OVRLP_LM)      Pb suspect because OVERLAP of merged data exceeds the limit supplied to MERGE stage.
  ; 1. Pb ignored:        (SRC_CNTS LT MIN_NUM_CTS)   So few counts that we refuse to believe any Pb value.
  ; 0. Pb relevant:        NOT 1,2, or 3
  
  Pb_status = bytarr(num_bands, num_sources)  ; Category number [0..3] from above.
  
  ; We arbitrarily disregard Pb values derived from very few SRC_CNTS.
  MIN_NUM_CTS   = 3
  ind = where((SRC_CNTS LT MIN_NUM_CTS), count)
  if (count GT 0) then begin
    ; We want to ignore these Pb values in computation of Pb_min below so set to NaN.
    Pb       [ind] = !VALUES.F_NAN
    Pb_status[ind] = 1
  endif
  
  ; Some sources may fail the OVERLAP limit in merged observations.
  ind = where(OVRLP_LO GE OVRLP_LM, count)
  if (count GT 0) then begin
    ; Since the backgrounds may be unreliable these Pb values are unreliable, so set to NaN..
    Pb       [ind] = !VALUES.F_NAN
    Pb_status[ind] = 2
  endif
  
  ; Some sources may be unobserved.
  ind = where((NUM_OBS EQ 0), count)
  if (count GT 0) then begin
    ; No Pb values are available; defensively set to NaN.
    Pb       [ind] = !VALUES.F_NAN
    Pb_status[ind] = 3
  endif
  
  ; Find which band has the smallest Pb, ignoring the NANs.
  ; Track the status of that minimum Pb value.
  ; In the code below, "imin" is in index into the 2-D arrays Pb_min and Pb_status.
  Pb_min        = min(/NAN, Pb, DIM=1, imin)
  Pb_min_origin = band_name[imin MOD num_bands]
  Pb_status     = Pb_status[imin]  
  
  ; Report any sources there were not observed.
  ind = where(Pb_status EQ 3, count)
  if (count GT 0) then begin
    forprint, TEXTOUT=2, SUBSET=ind, CATALOG_NAME, LABEL, F="(%'%s (%s)')"
    print, count, F="(%'The %d sources above were not extracted; they have been placed in the BEST Pb slice (blue) to ensure they will be retained.')"
    Pb_min[ind] = 0.0
  endif
  
  ; Report sources failing the OVERLAP limit in all observations.
  ind = where(Pb_status EQ 2, count)
  if (count GT 0) then begin
    forprint, TEXTOUT=2, SUBSET=ind, CATALOG_NAME, LABEL, F="(%'%s (%s)')"
    print, count, F="(%'The %d sources above have excessive OVERLAP in all observations; they have been placed in the CROWDED Pb slice (orange) to ensure they will be discarded.')"
    Pb_min[ind] = !VALUES.F_NAN
    ; Display the OVERLAP_LIMIT values that would be required for these sources to have data to merge.
    dataset_1d, id4, bt_full.OVRLP_LO , XTIT='OVERLAP_LIMIT required to survive', DENSITY_TITLE='excessive overlap sources'
  endif
  
  ; Report sources too weak to produce a usable Pb.
  ind = where(Pb_status EQ 1, count)
  if (count GT 0) then begin
    forprint, TEXTOUT=2, SUBSET=ind, CATALOG_NAME, LABEL, F="(%'%s (%s)')"
    print, count, F="(%'The %d sources above were too weak in all bands to define Pb; they have been placed in the WORST Pb slice (magenta) to ensure they will be discarded.')"
    Pb_min[ind] = 0.999
  endif
  
  ; These boundaries are defined below in terms of the percent chance that 
  ; the observed number or more counts could come from the background.
  
  if ~keyword_set(boundaries) then $
  boundaries   = [    '0.0', '0.1',  '0.3', '0.6','1.0','1.5',  '100.0']
  slice_color  = [ 'DodgerBlue','green','cyan','yellow','red','magenta',   'orange' , 'Tan']
  slice_name   = [boundaries+'-'+boundaries[1:*],                           'CROWDED', 'REVIVED']
  num_slices   = n_elements(slice_color)
  
  ; Choose a Pb threshold, typically at one of the boundaries.
  if ~keyword_set(invalid_threshold) then $
  invalid_threshold   = 1.0   ; 1%
  
  boundaries         /= 100.
  invalid_threshold  /= 100.
  
  ; Propose which sources should be pruned.
  ; Classify sources into one of the Pb slices or into the CROWDED category (Pb_min set to flag value NaN).
  prune_it = (Pb_min GT invalid_threshold)
  slice    = value_locate(boundaries, Pb_min) 
  ind = where(~finite(Pb_min), count)
  
  if (count GT 0) then begin
    prune_it[ind] = 1
    slice   [ind] = num_slices-2  ;CROWDED slice
  endif
  if (min(slice) LT 0)            then message, 'ERROR: Pb boundaries do not span values in Pb_min.'
  if (max(slice) GT num_slices-2) then message, 'ERROR: Pb boundaries do not span values in Pb_min.'

  
  ;; ------------------------------------------------------------------------
  ; When two neighboring sources are both proposed to be pruned above, it's often the case that either one 
  ; would survive if the other disappeared.  For example, if one neighbor is pruned and the other is
  ; re-extracted, then its aperture may grow sufficiently to produce a satisfactory Pb.
  ; Thus, we wish to "revive" some of the source proposed for pruning and let them participate in another round of catalog pruning.
  ; There are two tricky parts to this idea:
  ;
  ; 1. We must be careful to identify condemned sources that are interating with a condemned neighbor.
  ;    Examining the relationship of the sources in the merge on hand is NOT sufficient---due to OVERLAP pruning,
  ;    pathological cases exist where in the current merge the sources appear to be isolated, but in some of the 
  ;    unmerged obsids they are interacting.
  ;    
  ; 2. Once a two condemned sources are known to be interacting with each other, we must choose one of them to revive.
  ;    Choosing the brighter one (as measured by NET_CNTS) seems sensible, but again in pathological cases the 
  ;    current merge does not always reveal which is truly brighter.
  ;
  ; Thus, we are going to re-merge the list of condemned sources, using ALL their extractions, and then
  ; compare their full-band photometry to choose sources to revive.
  condemned_ind = where(prune_it, count)
  if (count GT 1) then begin
    print, F='(%"\nae_analyze_pb: ============================================================")' 
    print, 'ae_analyze_pb: MERGING and COLLATING the condemned sources to perform photometry using all extractions.' 
    print, F='(%"ae_analyze_pb: ============================================================\n")'
    
    forprint, TEXTOUT=  'condemned.srclist', SUBSET=condemned_ind, CATALOG_NAME, LABEL, F="(%'%s (%s)')", /NoCOMMENT
  
    energy_range = [bt_full[0].ENERG_LO, bt_full[0].ENERG_HI]
    acis_extract, 'condemned.srclist', /MERGE_OBSERVATIONS, OVERLAP_LIMIT=99, ENERGY_RANGE=energy_range, EBAND_LO=energy_range[0], EBAND_HI=energy_range[1], /SKIP_PSF, /SKIP_TIMING, _EXTRA=extra
    acis_extract, 'condemned.srclist', COLLATED_FILENAME='condemned.collated'
    bt_condemned =                               mrdfits('condemned.collated', 1)
    
    ; Look for pairs of condemned sources that are close enough (distance_reg2reg < SRC_RAD) to suspect that
    ; they are interacting, and move the brighter one into the REVIVED category so it can have another chance
    ; to survive on the next pass.  
    for ii=0,count-1 do begin
      if (bt_condemned[ii].DISTANCE_REG2REG LT bt_condemned[ii].SRC_RAD) then begin
        ; Find indexes for the pair in the full source list.
            this_ind      = condemned_ind[             ii          ]
        neighbor_ind      = condemned_ind[bt_condemned[ii].NEIGHBOR]
        
            this_net_cnts =  bt_condemned[             ii          ].NET_CNTS
        neighbor_net_cnts =  bt_condemned[bt_condemned[ii].NEIGHBOR].NET_CNTS
        
        if prune_it[this_ind] && prune_it[neighbor_ind] then begin
          ; Both sources remain on the prune list, so revive the brighter one (as measured in the all-obsid merge we just did).
          if (this_net_cnts GT neighbor_net_cnts) then begin
            ; Revive this source.
            slice   [this_ind]     = num_slices-1  ;REVIVED slice
            prune_it[this_ind]     = 0
          endif else begin
            ; Revive the neighbor
            slice   [neighbor_ind] = num_slices-1  ;REVIVED slice
            prune_it[neighbor_ind] = 0
          endelse
        endif 
      endif ; close pair of sources
    endfor ;ii loop through condemned sources
  endif ; prune_it flag is true for some sources
  
  
  
  ; Find the set of PROVENAN values present.
  PROVENAN_list = PROVENAN[uniq(PROVENAN,sort(PROVENAN))]
  num_provenan  = n_elements(PROVENAN_list)
  
  ; Tabulate how many sources are in each proposed Pb slice.
  table = intarr(num_slices, num_provenan+1)
  for ii=0,num_slices-1 do begin
    ind = where(slice EQ ii, count)
    if (count GT 0) then for jj=0,num_provenan-1 do table[ii,jj] = round(total(PROVENAN[ind] EQ PROVENAN_list[jj]))
  endfor
     
  provenan_totals = total(table,1)
  print
  print, 'Pb slice boundaries (%):', boundaries
  forprint, boundaries, F="(%'  %5.1f')"
  print, 'Pb threshold (%):', invalid_threshold
  print
  print, 'PROVENAN', 'Pb (%):',slice_name, F='(A20,A10,20(A12))'
  print, 'vvvvv  ', 'total |', slice_color, F='(A20,A10,20(A12))'
  for jj=0,num_provenan-1 do print, PROVENAN_list[jj], provenan_totals[jj],'|',table[*,jj], F='(A20,I8,A2,20(I12))'
  print, '------------------------------------------------------------------------------------------------------', F='(24x,A)'
  print, total(table),total(table,2), F='(20x,I8,2x,20(I12))'
  
  prune_ind = where(prune_it, count)
  if (count GT 0) then $
    forprint, TEXTOUT=  'prune.srclist', SUBSET=prune_ind[sort(LABEL[prune_ind])], CATALOG_NAME, LABEL, F="(%'%s (%s)')", /NoCOMMENT
  save, FILE='Pbslice.sav'
  print, 'Saved Pb slice information to Pbslice.sav'
  
  dataset_1d, id3, alog10(Pb_min),           XTIT='log Pb_min'
  dataset_1d, id2,    100*Pb_min ,           XTIT='Pb_min (%)'
  dataset_2d, id1, SRC_CNTS[band_full,*], 100*Pb_min, PSYM=3, YTIT='Pb_min (%)', XTIT='SRC_CNTS in band '+band_name[band_full]
  
  ; Configure the second plot above to show a cumulative distribution of Pb, and make any adjustment 
  ; to the Pb boundaries (via option BOUNDARIES) needed to give you useful 'slices' of the data.
  return
endif

if keyword_set(make_regions) then begin
  restore, 'Pbslice.sav'
  
  ; Make some region files that slice the data in Pb so we can visualize the sources that would
  ; be retained for various pruning thresholds we might choose.
  ; We separate this long-running code from the similar tabulation code above to allow the observer to adjust the Pb slices efficiently. 
  pb_fn = 'Pbslice.reg'
  file_delete, pb_fn, /ALLOW_NONEXISTENT
  for ii=0,num_slices-1 do begin
    ind = where(slice EQ ii, count)
    if (count GT 0) then begin
      basename='Pbslice_'+slice_name[ii]
      forprint, TEXTOUT=basename+'.srclist', CATALOG_NAME, SUBSET=ind, /NoCOMMENT
      
      acis_extract, basename+'.srclist', MERGE_NAME='Pb_full_band', COLLATED_FILENAME='/dev/null', REGION_FILE='temp.reg', REGION_TAG=basename, MATCH_EXISTING=bt_full
      
      cmd = string(slice_color[ii], pb_fn, F='(%"egrep ''label|cat|polygon'' temp.reg | sed -e ''s/green/%s/'' >> %s")')
      print,cmd
      spawn, cmd
    endif
  endfor
  
  ; Display the Pb slices on the multi-obsid band-limited data in ds9.
  cmd = 'ds9 -log -bin factor 4 ../project.evt -region '+pb_fn+' &'
  print, cmd
  spawn, cmd 
  
  
  ; Make region file showing sources that survived only in hard or soft bands, not in full band.
  pb_fn = 'Pb_valid_narrow_band.reg'
  file_delete, pb_fn, /ALLOW_NONEXISTENT

  full_band_detection = ~prune_it AND  (Pb[band_full,*] LE invalid_threshold)
  soft_only_detection = ~prune_it AND ((Pb[band_soft,*] LE invalid_threshold) AND ~full_band_detection)
  hard_only_detection = ~prune_it AND ((Pb[band_hard,*] LE invalid_threshold) AND ~full_band_detection)

  ind = where(soft_only_detection, num_soft)
  if (num_soft GT 0) then begin
    forprint, TEXTOUT='valid_only_soft.srclist', SUBSET=ind, CATALOG_NAME, /NoCOMMENT
  
    acis_extract, 'valid_only_soft.srclist', MERGE_NAME='Pb_soft_band', COLLATED_FILENAME='/dev/null', REGION_FILE='temp.reg',REGION_TAG='valid_only_soft', MATCH_EXISTING=bt_full
    
    spawn, "egrep 'label|cat|polygon' temp.reg | sed -e ''s/green/red/'' >> "+pb_fn
    print, num_soft, ' sources detected only in soft band'
  endif
  
  ind = where(hard_only_detection, num_hard)
  if (num_hard GT 0) then begin
    forprint, TEXTOUT='valid_only_hard.srclist', SUBSET=ind, CATALOG_NAME, /NoCOMMENT
  
    acis_extract, 'valid_only_hard.srclist', MERGE_NAME='Pb_hard_band', COLLATED_FILENAME='/dev/null', REGION_FILE='temp.reg',REGION_TAG='valid_only_hard', MATCH_EXISTING=bt_full
    
    spawn, "egrep 'label|cat|polygon' temp.reg | sed -e ''s/green/DodgerBlue/'' >> "+pb_fn
    print, num_hard, ' sources detected only in hard band'
  endif
  
  if (num_soft GT 0) || (num_hard GT 0) then begin
    
    ind = where(full_band_detection, num_full)
    if (num_full GT 0) then begin
      forprint, TEXTOUT='valid_full_band.srclist', SUBSET=ind, CATALOG_NAME, /NoCOMMENT
    
      acis_extract, 'valid_full_band.srclist', MERGE_NAME='Pb_full_band', COLLATED_FILENAME='/dev/null', REGION_FILE='temp.reg',REGION_TAG='valid_full_band', MATCH_EXISTING=bt_full
      
      spawn, "egrep 'label|cat|polygon' temp.reg >> "+pb_fn
      print, num_full, ' sources detected in full band'
    endif
    
    cmd = string(1000*[bt_soft[0].ENERG_LO,bt_soft[0].ENERG_HI], pb_fn, 1000*[bt_hard[0].ENERG_LO,bt_hard[0].ENERG_HI], pb_fn, F="(%'ds9 -log -bin factor 4 ""../project.evt[energy>%d && energy<%d]"" -region %s ""../project.evt[energy>%d && energy<%d]"" -region %s &')" )
    print, cmd
    spawn, cmd 
  endif 
  
endif
return
end


;#############################################################################
;;; Algorithm for analyzing the 7 routine spectral fits produced by recipe.txt,
;;; suggesting which other models should be run, and suggesting the best spectral model
;;; (to be reviewed by the observer).
;#############################################################################
PRO ae_suggest_spectral_model, COLLATE=collate, ANALYZE=analyze, $
      NH_min=NH_min_p, NH_max=NH_max_p, kT_min=kT_min_p, kT_max=kT_max_p

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
  
  
; ---------------------------------------------------------------------
; Collate the fitting results.
; The * in the HDUNAME pattern is there to pick up any models that required customization, e.g. via MODEL_CHANGES_FILENAME='xspec_scripts/noerr.xcm'

if keyword_set(collate) then begin
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_A*'   , COLLATED_FILENAME='tables/tbabs_vapec_A.collated' 
  
  bt = mrdfits('tables/tbabs_vapec_A.collated',1)
  
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_B*'   , COLLATED_FILENAME='tables/tbabs_vapec_B.collated'   , MATCH_EXISTING=bt
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_C*'   , COLLATED_FILENAME='tables/tbabs_vapec_C.collated'   , MATCH_EXISTING=bt
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_D*'   , COLLATED_FILENAME='tables/tbabs_vapec_D.collated'   , MATCH_EXISTING=bt
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_E*'   , COLLATED_FILENAME='tables/tbabs_vapec_E.collated'   , MATCH_EXISTING=bt
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_std1*', COLLATED_FILENAME='tables/tbabs_vapec_std1.collated', MATCH_EXISTING=bt
  acis_extract, 'xspec.srclist', MERGE_NAME='photometry', HDUNAME='nogrp_tbabs_vapec_std2*', COLLATED_FILENAME='tables/tbabs_vapec_std2.collated', MATCH_EXISTING=bt
  
  save, FILE='fit_spectra.sav'
endif else restore, 'fit_spectra.sav'


if keyword_set(analyze) then begin
  ; These parameters define what NH and kT values are considered "extreme".
  if keyword_set(NH_min_p) then NH_min = NH_min_p else NH_min =  1E20
  if keyword_set(NH_max_p) then NH_max = NH_max_p else NH_max =  1E23
  if keyword_set(kT_min_p) then kT_min = kT_min_p else kT_min =  0.5
  if keyword_set(kT_max_p) then kT_max = kT_max_p else kT_max = 15
  
  ; We must put the standard models at the start of this list for later code to work properly.
  num_standard_models = 2
  collated_files = ['tables/tbabs_vapec_std1.collated',$
                    'tables/tbabs_vapec_std2.collated',$
                    'tables/tbabs_vapec_A.collated',   $
                    'tables/tbabs_vapec_B.collated',   $
                    'tables/tbabs_vapec_C.collated',   $
                    'tables/tbabs_vapec_D.collated',   $
                    'tables/tbabs_vapec_E.collated']
                    
  num_models = n_elements(collated_files)
  
  is_frozen  = replicate(0B, num_models)
  is_frozen[0:num_standard_models-1] = 1
  
  bt = mrdfits(collated_files[0], 1)
  num_sources = n_elements(bt)
  
  review_reason = strarr(num_sources)
  NH            = fltarr(num_models, num_sources)
  KT            = fltarr(num_models, num_sources)
  F0P5_8        = fltarr(num_models, num_sources)
  FC0P5_8       = fltarr(num_models, num_sources)
  MODEL         = strarr(num_models, num_sources)
  CSTAT         = fltarr(num_models, num_sources)   
  band_full = 0
  print, 'Using the energy band ', bt[0].ENERG_LO[band_full], bt[0].ENERG_HI[band_full]
  NET_CNTS      = bt.NET_CNTS  [band_full]
  SRC_SIGNIF    = bt.SRC_SIGNIF[band_full]
  LABEL = strtrim(bt.LABEL,2)
  
  for model_ind=0,num_models-1 do begin
    print, collated_files[model_ind]
    bt = mrdfits(collated_files[model_ind], 1)
    
    NH     [model_ind,*] =         bt.NH * 1E22
    KT     [model_ind,*] =         bt.KT
    F0P5_8 [model_ind,*] =         bt.F0P5_8
    FC0P5_8[model_ind,*] =         bt.FC0P5_8
    MODEL  [model_ind,*] = strtrim(bt.MODEL,2)
    CSTAT  [model_ind,*] =         bt.CSTAT
  endfor
   
  free_NH      = NH     [num_standard_models:*,*]
  free_KT      = KT     [num_standard_models:*,*]
  free_F0P5_8  = F0P5_8 [num_standard_models:*,*]
  free_FC0P5_8 = FC0P5_8[num_standard_models:*,*]
  
  dsn = 'thawed models'
  dataset_1d,id1, DATASET=dsn, alog10(free_NH)     , XTIT='log NH'
  dataset_1d,id2, DATASET=dsn,        free_KT      , XTIT='kT'
  dataset_1d,id3, DATASET=dsn, alog10(free_F0P5_8 ), XTIT='log F[0.5:8]'
  dataset_1d,id4, DATASET=dsn, alog10(free_FC0P5_8), XTIT='log Fc[0.5:8]'
  dataset_1d,id5, DATASET=dsn,     FC0P5_8/F0P5_8,   XTIT='FC0P5_8/F0P5_8 [0.5:8]'
                                        
  dataset_2d,id15, DATASET=dsn, alog10(free_NH), free_KT,  XTIT='log NH', YTIT='kT', PSYM=1, NAN=[15,-1]
  
  dataset_2d,id16, DATASET=dsn, alog10(free_NH), alog10(free_FC0P5_8), XTIT='log NH', YTIT='log Fc[0.5:8]', PSYM=1, NAN=[15,-10]
  dataset_2d,id17, DATASET=dsn,        free_KT , alog10(free_FC0P5_8), XTIT='kT'    , YTIT='log Fc[0.5:8]', PSYM=1, NAN=[-5,-10]
  
  ; ---------------------------------------------------------------------
  ;Identify astrophysically extreme fits.
  
  is_extreme =  (alog10(F0P5_8)   LT -16)    OR $     ; flux range
                (alog10(F0P5_8)   GT -13)    OR $     
                (alog10(FC0P5_8)  LT -16)    OR $     ; corrected flux range
                (alog10(FC0P5_8)  GT -13)    OR $     
                (       NH        LT NH_min) OR $     ; NH range
                (       NH        GT NH_max) OR $     
                (       KT        LT kT_min) OR $     ; kT range
                (       KT        GT kT_max) OR $   
                ((FC0P5_8/F0P5_8) GT 10)              ; flux correction factor
  
  print, 100.0 * total(/INT, is_extreme) / n_elements(is_extreme), n_elements(is_extreme), F='(%"\n%0.1f%% of the %d models are ''extreme''.")'
  
  
  ; ---------------------------------------------------------------------
  ; Fit a two-temperature model for bright sources.
  ; We put this step here merely so that the review_reason "bright" will appear first in the list of reasons.
  
  ind = where((NET_CNTS GT 100), count)
  if (count GT 0) then begin
    review_reason[ind] = review_reason[ind]+'bright '
    
    forprint, TEXTOUT='thermal_2T.srclist', bt.CATALOG_NAME, SUBSET=ind, /NoCOMMENT
    print, count, F='(%"\nFit the appropriate non-standard model to %d sources in thermal_2T.srclist.")'
  endif
  
  ; ---------------------------------------------------------------------
  ; Begin building a recommendation for the best model for each source.
  
  ; Initially, we simply select the model with the smallest Cstat value. However, we're willing to accept an alternative model with a modestly increased Cstat if the alternative eliminates an undesirable property of the minimum-Cstat model, such as an astrophysically unlikely set of parameter values or a frozen parameter.
  
  ; I don't know of a rigorous statistical argument addressing the question of "how far" from the best fit one should stray when choosing alternate models. Desperate for some delta-Cstat threshold to define "acceptable" alternative fits, I turn to the table in Section 2.2 of the XSPEC12 manual that associates a delta_stat value of 4.61 with a 90% confidence region for two parameters. I latch onto the two parameter case because the observer is (mostly) thinking about the physical plausibility of NH and kT. I latch onto the 90% confidence level arbitrarily.
  
  ; Similarly, I don't know a rigorous Cstat increase one should tolerate to avoid a best-fit model that has a frozen parameter. I arbitrarily choose a value of 1.0
  
  extreme_cstat_penalty = 4.61
  frozen_cstat_penalty  = 1.0
  
  bestfit_model_ind = lonarr(num_sources)
  chosen_model_ind  = lonarr(num_sources)
  
  for ii=0,num_sources-1 do begin
    ; Find the best-fit model, i.e. the one with the smallest Cstat.
    min_cstat = min(CSTAT[*,ii], ind, /NAN)
    
    ; Record the best-fit model for use later.
    ; Our initial recommendation ("cmi"=="chosen model index") is the best-fit model.
    bestfit_model_ind[ii] = ind
    cmi                   = ind      
    
    ; Loop through the models looking for a preferred one.
    ; "tmi" is "this model index"
    for tmi=0,num_models-1 do begin
      ; Compare this model and currently chosen model with regard to extreme parameters.
      skip = 0
      case 1 of
        ( is_extreme[tmi,ii] &&  is_extreme[cmi,ii]):  ; no difference
        (~is_extreme[tmi,ii] && ~is_extreme[cmi,ii]):  ; no difference
        ( is_extreme[tmi,ii] && ~is_extreme[cmi,ii]):$ ; extreme never preferred
          skip = 1
        (~is_extreme[tmi,ii] &&  is_extreme[cmi,ii]):$ ; Reasonable model preferred if CSTAT not too high.
          begin
          if (CSTAT[tmi,ii] LT (CSTAT[cmi,ii] + extreme_cstat_penalty)) then begin
            print, MODEL[cmi,ii], MODEL[tmi,ii], LABEL[ii], F='(%"  Replacing extreme model %s with reasonable model %s for source %s")'
            cmi = tmi
            review_reason[ii] = review_reason[ii]+'not_best '
            skip = 1
          endif
          end
      endcase
      if skip then continue
    
      ; Compare this model and currently chosen model with regard to frozen parameters.
      case 1 of
        ( is_frozen[tmi] &&  is_frozen[cmi]):  ; no difference
        (~is_frozen[tmi] && ~is_frozen[cmi]):  ; no difference
        ( is_frozen[tmi] && ~is_frozen[cmi]):$ ; frozen never preferred
          skip = 1
        (~is_frozen[tmi] &&  is_frozen[cmi]):$ ; Thawed model preferred if CSTAT not too high.
          begin
          if (CSTAT[tmi,ii] LT (CSTAT[cmi,ii] + frozen_cstat_penalty)) then begin
            print, MODEL[cmi,ii], MODEL[tmi,ii], LABEL[ii], F='(%"  Replacing frozen model %s with thawed model %s for source %s")'
            cmi = tmi
            review_reason[ii] = review_reason[ii]+'not_best '
            skip = 1
          endif
          end
      endcase
      if skip then continue
    
      
      ; If we get here then this model and the chosen model have the same extreme and frozen properties.
      ; Now we simply accept a smaller Cstat
      if (CSTAT[tmi,ii] LT CSTAT[cmi,ii]) then begin
        print, MODEL[cmi,ii], MODEL[tmi,ii], LABEL[ii], F='(%"  Replacing model %s with better model %s for source %s")'
        cmi = tmi
        review_reason[ii] = review_reason[ii]+'not_best '
       continue
      endif
      
      ; Save the chosen model.
      chosen_model_ind[ii] = cmi
    endfor ; tmi
  endfor ; ii
  
  chosen_model = MODEL[chosen_model_ind,indgen(num_sources)]
  print, F='(%"\nThe distribution of recommended models is shown below:")'
  forprint, file_basename(collated_files,'.collated'), histogram(chosen_model_ind) 
  
  
  ; ---------------------------------------------------------------------
  ; Identify for review sources where the chosen model remains extreme.
  
  ind = where(is_extreme[chosen_model_ind,indgen(num_sources)], count)
  if (count GT 0) then review_reason[ind] = review_reason[ind]+'extreme '
  print, count, F='(%"\nThe recommended model for %d sources is extreme.")'
    
   
  
  ; ---------------------------------------------------------------------
  ; Identify sources where the chosen model has extremely low NH.
  ; For these we perform another unfrozen fit with initial parameter values at a different point than in the routine models.
  ; We do not recommend this model (since we don't know yet how this one will turn out.)
  chosen_has_low_NH = (NH[chosen_model_ind,indgen(num_sources)] LE NH_min)
  
  ind = where(chosen_has_low_NH, count)
  if (count GT 0) then begin
    review_reason[ind] = review_reason[ind]+'NH_low '
    forprint, TEXTOUT='NH_low.srclist', bt.CATALOG_NAME, SUBSET=ind, /NoCOMMENT
    print, count, F='(%"\nFit the appropriate non-standard model to %d sources in NH_low.srclist.")'
  endif
  
  
  ; ---------------------------------------------------------------------
  ; Identify sources where the chosen model has extremely high kT.
  ; For these we perform a fit with kT frozen at its high limit, and we recommend that as the best model.
  
  ; Identify sources where ALL the free models have extremely high kT.
  ; For these we want to show the observer a fit with kT frozen at its high limit, but we do not recommend it as the best model.
  
  chosen_is_hot = (KT[chosen_model_ind,indgen(num_sources)] GT kT_max)
  all_are_hot   = (min(free_KT, DIMENSION=1)                GT kT_max)
  
  ind = where(chosen_is_hot, count)
  if (count GT 0) then begin
    review_reason[ind] = review_reason[ind]+'kT_max '
    chosen_model [ind] = 'nogrp_tbabs_vapec_kT_max*'  ; The wildcard is needed to match _noerr models.
  endif
  
  ind = where(chosen_is_hot OR all_are_hot, count)
  if (count GT 0) then begin
    forprint, TEXTOUT='thermal_kT_max.srclist', bt.CATALOG_NAME, SUBSET=ind, /NoCOMMENT
    print, count, F='(%"\nFit the appropriate non-standard model to %d sources in thermal_kT_max.srclist.")'
  endif
  
  
  ; ---------------------------------------------------------------------
  ; When all the free models have extreme kT, or when the chosen thermal model requires a high temperature, then run a power law model.
  
  ; THIS RECIPE WAS WRITTEN FOR ANALYSIS OF RICH STAR-FORMATION REGIONS, WHERE A POWERLAW IS LIKELY TO BE THE CORRECT MODEL ONLY FOR A FEW BACKGROUND AGN. Thus, our algorithm for choosing the "best model" to suggest to the observer DOES NOT CONSIDER THE POWERLAW FIT. The observer is expect to carefully review the cases where a powerlaw model was tried and make the hard decision about when it should be adopted, perhaps using other information such as where the source lies in the cluster, counterpart properties, variability, etc.
  
  ind = where(all_are_hot OR ((KT[chosen_model_ind,indgen(num_sources)] GT 6)), count)
  if (count GT 0) then begin
    review_reason[ind] = review_reason[ind]+'powerlaw '
    
    forprint, TEXTOUT='pow.srclist', bt.CATALOG_NAME, SUBSET=ind, /NoCOMMENT
    print, count, F='(%"\nFit the appropriate non-standard model to %d sources in pow.srclist.")'
  endif
  
  
  ; ---------------------------------------------------------------------
  ; Identify for review sources where the best-fit model has frozen parameters. 
  
  ind = where(bestfit_model_ind LT num_standard_models, count)
  if (count GT 0) then review_reason[ind] = review_reason[ind]+'best-fit is frozen '
  print, count, F='(%"\nThe best-fit model for %d sources is one of the standard models with frozen kT.")'
    
      
  ; ---------------------------------------------------------------------
  ; Flag for visual review sources whose fits are sensitive to initial parameters.
  ; The sensitivity criterion must of course OMIT the frozen "standard" models!
  
  is_sensitive = (abs(alog10(min(free_NH,    DIM=1) / max(free_NH,    DIM=1))) GT 0.5) OR $ ; NH stability
                 (abs(alog10(min(free_F0P5_8,DIM=1) / max(free_F0P5_8,DIM=1))) GT 0.5) OR $ ; flux stability
                 (abs(       min(free_KT,    DIM=1) - max(free_KT,    DIM=1))  GT 1)        ; kT stability
                 
  ind = where(is_sensitive, count)
  if (count GT 0) then review_reason[ind] = review_reason[ind]+'sensitive '
  print, count, F='(%"\nThe free models for %d sources are sensitive to initial parameters.")'
  
  
  save, FILE='fit_spectra.sav'
endif
return
end



;############################################################################# 
PRO ae_recipe
return
end

