pro read_swift_grb_table

  ;;; DAVIDE'S TABLE ONLY GOOD THROUGH DECEMBER 2012
  file='~/Swift/swiftgrb.tdat3';.txt'
  
  grbs=create_struct('name', '','target_id', 0L, 'other_id', '','det_flag', '', 'slew_info', '',$
                     'ra', 0., 'dec', 0., 'pos_err', 0., 'lii', 0., 'bii', 0., 'pos_flag', '', $
                     'pos_ref', '', 'start_time', '', 'stop_time', '', 'duration', 0., $
                     'trigger_time', '', 'trigger_ref', '', 'bat_detection', '', 'bat_dettype', $
                     '', 'bat_ra', 0., 'bat_dec', 0., 'bat_pos_err', 0., 'bat_pos_ref', '', $
                     'bat_theta', 0., 'bat_phi', 0., 'bat_imagesig', 0., 'bat_t90', 0., 'bat_t50', 0., $
                     'bat_start', 0., 'bat_stop', 0., 'bat_t100_start', 0., 'bat_t100_stop', 0., $
                     'bat_fluence_model', '', 'bat_fluence', 0., 'bat_fluence_err', 0., $
                     'bat_fluence1', 0., 'bat_fluence1_err', 0., 'bat_fluence2', 0., $
                     'bat_fluence2_err', 0., 'bat_fluence3', 0., 'bat_fluence3_err', 0., $
                     'bat_fluence4', 0., 'bat_fluence4_err', 0., 'bat_peak_time', 0.,$
                     'bat_peak_model', '', 'bat_peak_flux', 0., 'bat_peak_flux_err', 0.,$
                     'bat_peak_flux1', 0., 'bat_peak_flux1_err', 0., 'bat_peak_flux2', 0.,$
                     'bat_peak_flux2_err', 0., 'bat_peak_flux3', 0., 'bat_peak_flux3_err', 0., $
                     'bat_peak_flux4', 0., 'bat_peak_flux4_err', 0., 'bat_peakfluxp', 0., $
                     'bat_peakfluxp_err', 0., 'bat_peakfluxp1', 0., 'bat_peakfluxp1_err', 0., $
                     'bat_peakfluxp2', 0., 'bat_peakfluxp2_err', 0., 'bat_peakfluxp3', 0., $
                     'bat_peakfluxp3_err', 0., 'bat_peakfluxp4', 0., 'bat_peakfluxp4_err', 0., $
                     'bat_plsl', 0., 'bat_plsl_err', 0., 'bat_pl_chi2', 0., 'bat_pl_dof', 0, $
                     'bat_ctslope', 0., 'bat_ctslope_err', 0., 'bat_ctslope_n_err', 0., $
                     'bat_ctezero', 0., 'bat_ctezero_err', 0., 'bat_ctezero_n_err', 0., $
                     'bat_ct_chi2', 0., 'bat_ct_dof', 0, 'bat_epeak', 0., 'bat_epeak_ref', '', $
                     'bat_eiso', 0., 'bat_eiso1000', 0., 'bat_eiso_alpha', 0., 'bat_eiso_beta', 0., $
                     'bat_eiso_norm', 0., 'bat_eiso_dur', 0., 'bat_eiso_ref', '', 'bat_redshift', 0., $
                     'bat_hrd1', 0., 'bat_hrd1_err', 0., 'bat_hrd2', 0., 'bat_hrd2_err', 0., $
                     'bat_comment', '', 'xrt_detection', '', 'xrt_ra', 0., 'xrt_dec', 0., $
                     'xrt_pos_err', 0., 'xrt_pos_ref', '', 'xrt_onsource', 0., 'xrt_c100_rate', 0., $
                     'xrt_c100_start', 0., 'xrt_c100_stop', 0., 'xrt_c100_expo', 0., $
                     'xrt_c100_mode', '', 'xrt_e1_rate', 0., 'xrt_e1_rate_err', 0., 'xrt_e1_rate1', 0.,$
                     'xrt_e1_rate1_err', 0., 'xrt_e1_rate2', 0., 'xrt_e1_rate2_err', 0., $
                     'xrt_e1_rate3', 0., 'xrt_e1_rate3_err', 0., 'xrt_e1_start', 0., 'xrt_e1_stop', 0.,$
                     'xrt_e1_expo', 0., 'xrt_hrd1', 0., 'xrt_hrd1_err', 0., 'xrt_hrd2', 0., $
                     'xrt_hrd2_err', 0., 'xrt_e1_mode', '', 'xrt_lcchange', '', 'xrt_flare', '', $
                     'uvot_detection', '', 'uvot_ra', 0., 'uvot_dec', 0., 'uvot_pos_err', 0., $
                     'uvot_pos_ref', '', 'uvot_onsource', 0., 'uvot_vv_flux', 0., 'uvot_vv_mag', 0., $
                     'uvot_vv_mag_err', 0., 'uvot_vv_start', 0., 'uvot_vv_stop', 0., $
                     'uvot_vv_expo', 0., 'uvot_w1_flux', 0., 'uvot_w1_mag', 0., 'uvot_w1_mag_err', 0., $
                     'uvot_w1_start', 0., 'uvot_w1_stop', 0., 'uvot_w1_expo', 0., 'redshift', 0., $
                     'redshift_err', 0., 'redshift_type', '', 'redshift_line', '', 'redshift_from', '',$
                     'redshift_ref', '', 'galactic_nh', 0., 'followup', '', 'radio_detection', '', $
                     'radio_ref', '', 'infra_detection', '', 'infra_ref', '', 'opt_detection', '', $
                     'opt_ref', '', 'ot_ra', '', 'ot_dec', '', 'ot_pos_err', '', 'ot_pos_ref', '', $
                     'other_obs', '', 'other_obs_ref', '', 'other_obs2', '', 'other_obs2_ref', '',$
                     'other_obs3', '', 'other_obs3_ref', '', 'other_obs4', '', 'other_obs4_ref', '', $
                     'supernova_flag', '', 'galaxy_flag', '', 'galaxy_name', '', 'galaxy_type', '', $
                     'galaxy_ra', '', 'galaxy_dec', '', 'galaxy_pos_ref', '', 'galaxy_redshift', '', $
                     'galaxy_ref', '', 'galaxy_offset', '', 'ground_counterpart', '', 'web_page', '', $
                     'comments', '')
  
  grbs=replicate(grbs,1000)
  ntags=n_tags(grbs)
  tag_names=tag_names(grbs)

  openr,lun,file,/get_lun
  i=0
  i0=0
  data=0
  k=0
  skip=readline(lun,delim='|')
  skip=readline(lun,delim='|')
  skip=readline(lun,delim='|')
  skip=readline(lun,delim='|')
  skip=readline(lun,delim='|')
  while not eof(lun) do begin

     chunks=readline(lun,delim='|')
;     if chunks[0] eq '<DATA>' then begin
;        data=1
;        i0=i
;     endif else k=i-i0

;     if data and i gt i0 then begin
        for j=0,ntags-1 do begin
           grbs[k].(j)=chunks[j+1]
;           print,tag_names[j],k,chunks[j+1],j
        endfor 
;     endif 


;     i=i+1
     k=k+1
;stop
  endwhile
  close,lun
  free_lun,lun
  
  w=where(grbs.name ne '')
  grbs=grbs[w]
  grbs.name=strcompress(grbs.name,/rem)

  mwrfits,grbs,'~/Swift/swiftgrb.fits',/create

return
end 
