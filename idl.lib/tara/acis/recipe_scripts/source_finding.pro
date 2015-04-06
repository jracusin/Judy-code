;;; This is an automated version of the Townsley ACIS analysis recipe source_finding.txt

;;; $Id: source_finding.pro 3189 2008-08-06 15:21:09Z patb $

;;; The input template_name must be a string array containing any of these template names:  
;;;   'fullfield','sarray','iarray','central_1', 'core_0.5'.

;;; This tool should be run from a "pointing" directory, i.e. one with composite emaps, and the directories full_band/, hard_band/, and soft_band/ containing composite images.

;;; It should be safe to run multiple instances of the /DETECT branch of this tool since we use a private PFILES dir and all wavdetect output files are either named using the template or are put in a temp directory.

@acis_extract
@match_xy

PRO source_finding, template_name, DETECT=do_detect, MERGE=do_merge

creator_string = "source_finding.pro, version"+strmid("$Date: 2008-08-06 11:21:09 -0400 (Wed, 06 Aug 2008) $", 6, 11)
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

file_mkdir, 'trash'

num_templates = n_elements(template_name)

image_fn   =       template_name+'.img'
emap_fn    = '../'+template_name+'.emap'
catalog_fn =       template_name+'.sources'

;; ------------------------------------------------------------------------
;; Run wavdetect on each band of each scene.
;; See source_finding.txt for a justification of these wavelet scales.
if keyword_set(do_detect) then begin
  for ii=0, num_templates-1 do begin
    case 1 of
     strmatch(template_name[ii],'fullfield*'): $
        begin
        scales='1.0 1.414 2.0 2.828 4.0 5.657 8.0 11.314 16.0'
        end
     strmatch(template_name[ii],'sarray*')   : $
        begin
        scales=              '2.828 4.0 5.657 8.0 11.314 16.0'
        end
     strmatch(template_name[ii],'iarray*')   : $
        begin
        scales='1.0 1.414 2.0 2.828 4.0 5.657 8.0'
        end
     strmatch(template_name[ii],'central_1*'): $
        begin
        scales='1.0 1.414 2.0 2.828 4.0'
        end
     strmatch(template_name[ii],'core_0.5*') : $
        begin
        scales='1.0 1.414 2.0'
        end
     else: begin
           print, 'ERROR! Ignoring unknown template named ', template_name[ii]
           goto, FAILURE
           end
    endcase
   
    for jj=0,2 do begin
      case jj of
        0: band_name = 'full'
        1: band_name = 'hard'
        2: band_name = 'soft'
      endcase
      band_dir = band_name+'_band'
      
      run_command, DIRECTORY=band_dir, $
        string(image_fn[ii], emap_fn[ii], catalog_fn[ii], scales, replicate(strmid(tempdir,0,strlen(tempdir)-1),5), F="(%'wavdetect infile=%s expfile=%s outfile=%s sigthresh=1e-5 scales=""%s"" regfile=%s/regfile scellfile=%s/scellfile imagefile=%s/imagefile defnbkgfile=%s/defnbkgfile interdir=%s log=no clobber=yes ')")
  
    endfor ; jj (band index)
  endfor ; ii (template index)
endif ; do_detect


;; ------------------------------------------------------------------------
;; Merge all the scene catalogs for each band.
if keyword_set(do_merge) then begin
  cd, CURRENT=cwd  &  pointing_name=file_basename(cwd)
  st = 0.99 ;significance threshold
  for jj=0,2 do begin
    case jj of
      0: band_name = 'full'
      1: band_name = 'hard'
      2: band_name = 'soft'
    endcase
    band_dir = band_name+'_band'
    cd, band_dir
  
    for ii=0, num_templates-1 do begin
      ; Read a wd catalog.
      wd_cat = build_wavdetect_cat(catalog_fn[ii], NAME=band_name+'_'+template_name[ii], /QUIET )
    
      if (ii EQ 0) then begin
        union_cat = wd_cat
      endif 
      match_xy, /QUIET, match_struct, union_cat, 'union', /INIT 
      match_xy, /QUIET, match_struct,    wd_cat, template_name[ii], st, UNION_CAT=union_cat, UNION_REG='union.reg', /OMIT_LABELS
    endfor ; ii (template index)
    
    ae_send_to_ds9, my_ds9, NAME=string(session_name, pointing_name, band_name, F="(%'source_finding.%s@%s/%s')"), OPTION_STRING='-log -bin factor 8'

    ae_send_to_ds9, my_ds9, image_fn, replicate('union.reg', num_templates) 
  
    print, band_name, band_name, F="(%'\nReview the ""%s"" band catalog which is displayed on the ""%s"" band data.\nIf you remove any regions in ds9, then re-save the regionfile ""union.reg"".  \n TYPE ""c"" and PRESS RETURN to continue.')"
    line = ''
    repeat read, line until strmatch(line,'c')
    
    catalog_ds9_interface, union_cat, 'union.reg', /PRUNE_CATALOG
    
    ; Save these catalogs.
    mwrfits, union_cat, 'union.sources', /CREATE
    save, FILE='merge.sav', union_cat, catalog_fn
    
    cd, '..'
  endfor ; jj (band index)
  
  
  ;; ------------------------------------------------------------------------
  ;; Merge the full_band, hard_band, & soft_band catalogs.
  restore, 'hard_band/merge.sav'
  hard_cat = union_cat      
  
  restore, 'soft_band/merge.sav'            
  soft_cat = union_cat
  
  restore, 'full_band/merge.sav'           
  full_cat = union_cat
  
  ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ; Merge the full catalog with the hard catalog.
  match_xy, /QUIET, match_struct, full_cat, 'full', /INIT
  match_xy, /QUIET, match_struct, hard_cat, 'hard', st, UNION_CAT=union_cat, UNION_REG='union.reg', /OMIT_LABELS
  
  ; Merge the full_hard catalog with the soft catalog.  
  match_xy, /QUIET, match_struct, union_cat, 'union', /INIT
  match_xy, /QUIET, match_struct,  soft_cat,  'soft', st, UNION_CAT=union_cat, UNION_REG='union.reg', /OMIT_LABELS
   
  ae_send_to_ds9, my_ds9, NAME=string(session_name, pointing_name, F="(%'source_finding.%s@%s')"), OPTION_STRING='-log -bin factor 8'
  ae_send_to_ds9, my_ds9, 'full_band/'+image_fn, replicate('union.reg', num_templates) 
  
  print, F="(%'\nReview the final catalog which is displayed on the ""full"" band data.\nIf you remove any regions in ds9, then re-save the regionfile ""union.reg"".  \n TYPE ""c"" and PRESS RETURN to continue.')"
  line = ''
  repeat read, line until strmatch(line,'c')
  
  catalog_ds9_interface, union_cat, 'union.reg', /PRUNE_CATALOG
  
  ; Save these catalogs.
  mwrfits, union_cat, 'union.sources', /CREATE
  save, FILE='merge.sav', union_cat, catalog_fn

  print, n_elements(union_cat), F="(%'Wrote %d sources to merge.sav (IDL savefile) and to union.sources (FITS).')"
endif ; do_merge  

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
  print, 'source_finding: Returning to top level due to fatal error.'
  retall
endelse

FAILURE:
exit_code = 1
GOTO, CLEANUP
return
end
   
