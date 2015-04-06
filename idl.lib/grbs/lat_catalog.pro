@fit_functions
@fit_functions_flares
pro get_eiso

  ;;; must read in BAT info
  ;;; must read in GBM info
  ;;; forget Konus/everyone else? - more systematic

  ;;; determine best prompt model
  ;;; cross-check with redshift list
  ;;; run calc_eiso

;  |name          |trigger_time           |fluence   |flnc_band_ampl|flnc_band_epeak|flnc_band_alpha|flnc_band_beta|flnc_spectrum_start|flnc_spectrum_stop|flnc_best_fitting_model|flnc_best_model_redchisq|flnc_plaw_ampl|flnc_plaw_pivot|flnc_plaw_index|flnc_plaw_redchisq|flnc_comp_ampl|flnc_comp_epeak|flnc_comp_index|flnc_comp_pivot|flnc_comp_redchisq|flnc_sbpl_ampl|flnc_sbpl_pivot|flnc_sbpl_indx1|flnc_sbpl_brken|flnc_sbpl_brksc|flnc_sbpl_indx2|flnc_sbpl_redchisq|

    h0=71.
  omega_m=0.27
  grbstr1=create_struct('grb','','z',0.,$
                        'alpha_pl',0.,'alpha_pl_err',0.,$
                        'kpl',0.,'kpl_err',0.,'chi2pl',0.,$
                        'alpha_cpl',0.,'alpha_cpl_err',fltarr(2),$
                        'epeak',0.,'epeak_err',fltarr(2),$
                        'kcpl',0.,'kcpl_err',fltarr(2),$
                        'chi2cpl',0.,$
                        'kband',0.,'kband_err',fltarr(2),$
                        'alpha_band',0.,'alpha_band_err',fltarr(2),$
                        'beta_band',0.,'beta_band_err',fltarr(2),$
                        'tstart',0.,'tstop',0.,$
                        'model','',$
                        'sbat',0.,'sbat_err',0.,$
                        'sbat_50_100',0.,'sbat_25_50',0.,$
                        'hr',0.,'hr_err',0.,$
                        'sme',0.,'sme_err',fltarr(2),$
                        'sband',0.,'sband_err',fltarr(2),$
                        'eiso_bestfit',0d,'eiso_bestfit_err',dblarr(2),$
                        'eiso_band',0d,'eiso_band_err',dblarr(2),$
                        'eiso_cpl',0d,'eiso_cpl_err',dblarr(2),$
                        'eiso',0d,'eiso_err',dblarr(2),$
                        'shb',0)


return
end 

pro lat_catalog
  
  lat=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB110625A','GRB110731A']

  cd,'~/GRBs'

  nlat=n_elements(lat)

  for i=0,nlat-1 do begin
     cd,lat[i]

     lc=lcout2fits(/phil)
     file='lc_fit_out_idl_int8.dat'
     if not exist(file) then file='lc_fit_out_idl_int7.dat'
     if exist(file) then begin 
        read_lcfit,file,pnames,p

        sfile='UL_specfits.fits'
        if exist(sfile) then begin 
           spec=mrdfits(sfile,1)
           ns=n_elements(spec)-1
           np=n_elements(p)

           ploterror,lc.time,lc.src_rate*spec[ns].unabs_cfratio,lc.time-lc.tstart,lc.src_rate_err*spec[ns].unabs_cfratio,psym=3,/nohat,/xlog,/ylog
           mo=fit_models(file=file)
           tmp=execute('y='+mo+'(lc.time,p)')
           oplot,lc.time,y*spec[ns].unabs_cfratio,color=!green
           det=where(lc.src_rate_err gt 0)
           
           print,lat[i],max(lc[det].tstop)
           k=get_kbrd(10)
           if k eq 's' then stop
        endif 
     endif 
     cd,'..'
  endfor 
;;LAT Catalog:
;;- gather fits to LAT bursts XRT light curves
;;- get limits on t_jet & theta_j
;;- list of Swift Eiso, t_jet, theta_j, E_gam
;;- list of steep to plateau times

;;; write out GRB, jet break time or limit, theta_j - but need Eiso
;; need to update Eisos

return
end 
