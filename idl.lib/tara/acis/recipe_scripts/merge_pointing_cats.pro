;;; $Id: merge_pointing_cats.pro 3306 2008-12-25 17:03:35Z patb $
;;; This script merges catalogs from multiple "pointings" in the Carina project.
;;; Plenty of assumptions about the directory structure and file names are made.

;;; The merged catalog is saved in 'merge_pointing_cats.sav'.

;;; Typical Usage to create master catalog from several pointings:

;  cd /Volumes/Bulk/projects/carina/data
;  idl |& tee -a merge_pointing_cats.log
;    catalog_list = file_search('*/*/proposed.sav')
;    forprint, catalog_list
;    merge_pointing_cats, catalog_list, 'tangentplane_reference.evt'

@acis_extract
@match_xy

PRO merge_pointing_cats, catalog_list, match_events_fn

merge_savefn = 'proposed.project.sav'
merge_regfn  = 'proposed.project.reg'

for ii=0, n_elements(catalog_list)-1 do begin
  ; Restore catalog "proposed_cat" from this pointing.
  restore, catalog_list[ii]
  
  pointing_dir = file_dirname(catalog_list[ii])
  print, pointing_dir, F="(%'Processing the catalog %s.')"
  
  if (ii EQ 0) then begin
    project_cat = proposed_cat
  endif else begin
    ; Reproject X/Y coordinates of project_cat to this pointing so that the catalog can be displayed on this pointing's data (below).
    match_xy_reproject_cat, project_cat, pointing_dir +'/tangentplane_reference.evt'
    
    match_xy, /QUIET, match_struct, project_cat, 'project_cat', /INIT
    
    match_xy, /QUIET, match_struct, proposed_cat, 'pointing', 0.99, UNION_CAT=project_cat, UNION_REG=merge_regfn, /OMIT_LABELS
    
    spawn, 'ds9 -log '+ pointing_dir + '/full_band/iarray_all.img -region '+merge_regfn+' &'

    print, merge_regfn, F="(%'\nReview the growing catalog which is displayed on the ""full"" band data for the current pointing.\nIf you remove any regions in ds9, then re-save the regionfile ""%s"".  \n TYPE ""c"" and PRESS RETURN to continue.')"
    line = ''
    repeat read, line until strmatch(line,'c')
   
   catalog_ds9_interface, project_cat, merge_regfn, /PRUNE_CATALOG
  endelse
  
  save, project_cat, FILE=merge_savefn
endfor ;ii

; Reproject X/Y coordinates of project_cat to the project's tangent plane reference, and write region file.
match_xy_reproject_cat, project_cat, match_events_fn
catalog_ds9_interface,  project_cat, merge_regfn, /WRITE_REGFILE, /OMIT_LABELS
save,                   project_cat, FILE=merge_savefn
print, 'Merged catalog is in ', merge_savefn, ' and ', merge_regfn
  


if (total(strmatch(tag_names(project_cat), 'LABEL')) GT 0) then begin
  ;; Look for duplicate source labels
  num_sources = n_elements(project_cat)
  label       = strtrim(project_cat.LABEL,2)
  
  sort_index    = sort(label)
  sorted_labels = label[sort_index]
  
  uniq_index = uniq(sorted_labels)
  num_uniq   = n_elements(uniq_index)
  if (num_uniq LT num_sources) then begin
    print, F='(%"\n=============================================================================")'
    print, num_sources-num_uniq, F="(%'WARNING: The catalog contains %d duplicate source labels:')"
  
    next = 0
    for ii=0L,num_uniq-1 do begin
      this = uniq_index[ii]
      if (this GT next) then begin
        print
        for jj=next,this do begin
          duplicate_index = sort_index[jj]
          print, label[duplicate_index], project_cat[duplicate_index].RA, project_cat[duplicate_index].DEC, F='(A,1x,F10.6,1x,F10.6)'
        endfor
      endif ;this GT next
      
      next = this+1
    endfor
    
    print, '============================================================================='
  endif ;duplicates
endif

return
end

