pro gbmcat,doitanyway=doitanyway


  ;; or forget all this and use downloaded fits file, fixed for that 5/15/14
  if not exist('~/Fermi/GBM_GRB_Catalog.fits') or keyword_set(doitanyway) then begin 
;     file='~/Fermi/GBM_GRB_Catalog_2014_03_06.txt'
      file='~/Fermi/GBM_GRB_Catalog_2015_03_15.txt'

     readcol,file,head,format='(a)',delim='$'

     tags0=str_sep(head[2],'|')
     tags0=strtrim(tags0,2)
     ntags=n_elements(tags)
     w=where(tags0 ne '',ntags)
     tags=tags0[w]

     type=replicate('0d',306)
     type[[0,1,2,3,28,30,32,45,66,95,125,155,191,218,248,266,302,303,304,305]]="''"

     t=''
     for i=0,99 do t=t+"'"+tags[i]+"',"+type[i]+","
     t=t+"'"+tags[i]+"',"+type[i]
     tmp=execute('gbm1=create_struct('+t+')')

     t=''
     for i=101,199 do t=t+"'"+tags[i]+"',"+type[i]+","
     t=t+"'"+tags[i]+"',"+type[i]
     tmp=execute('gbm2=create_struct('+t+')')

     t=''
     for i=201,ntags-2 do t=t+"'"+tags[i]+"',"+type[i]+","
     t=t+"'"+tags[i]+"',"+type[i]
     tmp=execute('gbm3=create_struct('+t+')')

     combine_structs,gbm1,gbm2,gbm4
     combine_structs,gbm4,gbm3,gbm

     ngrbs=n_elements(head)-3
     gbm=replicate(gbm,ngrbs)
     for j=0,ngrbs-1 do begin
        for i=0,ntags-1 do begin 
           yo=str_sep(head[j+3],'|')
           if yo[w[i]] ne ' ' then gbm[j].(i)=yo[w[i]]
        endfor 

        print,j
     endfor 

     s=sort(gbm.name)
     gbm=gbm[s]

     mwrfits,gbm,'~/Fermi/GBM_GRB_Catalog.fits',/create
  endif else gbm=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  ngrbs=n_elements(gbm)

;;; PARSE OUT RELEVANT INFO

  gbm2=create_struct('grb','','grb_name','',$
                     'ra',0.,'dec',0.,'trigtime','','trigtime_met',0d,$
                     't90',0.,'t90_err',0.,$
                     'model_bestfit','',$
                     'alpha_pl',0.,'alpha_pl_err',fltarr(2),$
                     'norm_pl',0.,'norm_pl_err',fltarr(2),$
                     'fluence_pl',0.,'fluence_pl_err',0.,$
                     'alpha_cpl',0.,'alpha_cpl_err',fltarr(2),$
                     'epeak_cpl',0.,'epeak_cpl_err',fltarr(2),$
                     'norm_cpl',0.,'norm_cpl_err',fltarr(2),$
                     'fluence_cpl',0.,'fluence_cpl_err',0.,$
                     'alpha_sbpl',0.,'alpha_sbpl_err',fltarr(2),$
                     'beta_sbpl',0.,'beta_sbpl_err',fltarr(2),$
                     'epeak_sbpl',0.,'epeak_sbpl_err',fltarr(2),$
                     'norm_sbpl',0.,'norm_sbpl_err',fltarr(2),$
                     'fluence_sbpl',0.,'fluence_sbpl_err',0.,$
                     'alpha_band',0.,'alpha_band_err',fltarr(2),$
                     'beta_band',0.,'beta_band_err',fltarr(2),$
                     'e0_band',0.,'e0_band_err',fltarr(2),$
                     'epeak_band',0.,'epeak_band_err',fltarr(2),$
                     'norm_band',0.,'norm_band_err',fltarr(2),$
                     'fluence_band',0.,'fluence_band_err',0.,$
                     'tstart',0.,'tstop',0.,$
                     'norm_energy',0.)

  gbm2=replicate(gbm2,ngrbs)

  ;;; want any fit to BAT burst, PL, comp, Band
  ;;; want fluence spectrumn (flnc) not Peak flux (pflx)

  for i=0,ngrbs-1 do begin 
;     rac=strsplit(gbm[i].ra,' ',/ext)
;     decc=strsplit(gbm[i].dec,' ',/ext)
;     hms2radec,rac[0],rac[1],rac[2],decc[0],decc[1],decc[2],ra,dec
     gbm2[i].ra=gbm[i].ra
     gbm2[i].dec=gbm[i].dec
     daycnv,gbm[i].trigger_time+2400000.5,yr,mn,day,hr
     mnt=(hr-fix(hr))*60.
     sec=(mnt-fix(mnt))*60.
     hr=fix(hr)
     mnt=fix(mnt)
     gbm2[i].trigtime_met=date2met(ntostr(yr)+'-'+ntostr(mn)+'-'+ntostr(day)+'-'+ntostr(hr)+':'+ntostr(mnt)+':'+ntostr(sec,4));date2met(gbm[i].trigger_time)
  endfor 
  
  gbm2.grb_name=gbm.name
  gbm2.trigtime=gbm.trigger_time
  gbm2.t90=gbm.t90
  gbm2.t90_err=gbm.t90_error
  gbm2.norm_energy=100.
  gbm2.model_bestfit=gbm.flnc_best_fitting_model
  gbm2.alpha_pl=gbm.flnc_plaw_index
  gbm2.alpha_pl_err=rotate([[gbm.flnc_plaw_index_neg_err],[gbm.flnc_plaw_index_pos_err]],1)
  gbm2.norm_pl=gbm.flnc_plaw_ampl
  gbm2.norm_pl_err=rotate([[gbm.flnc_plaw_ampl_neg_err],[gbm.flnc_plaw_ampl_pos_err]],1)
  gbm2.fluence_pl=gbm.flnc_plaw_ergflnc
  gbm2.fluence_pl_err=gbm.flnc_plaw_ergflnc_error

  gbm2.alpha_cpl=gbm.flnc_comp_index
  gbm2.alpha_cpl_err=rotate([[gbm.flnc_comp_index_neg_err],[gbm.flnc_comp_index_pos_err]],1)
  gbm2.epeak_cpl=gbm.flnc_comp_epeak
  gbm2.epeak_cpl_err=rotate([[gbm.flnc_comp_epeak_neg_err],[gbm.flnc_comp_epeak_pos_err]],1)
  gbm2.norm_cpl=gbm.flnc_comp_ampl
  gbm2.norm_cpl_err=rotate([[gbm.flnc_comp_ampl_neg_err],[gbm.flnc_comp_ampl_pos_err]],1)
  gbm2.fluence_cpl=gbm.flnc_comp_ergflnc
  gbm2.fluence_cpl_err=gbm.flnc_comp_ergflnc_error

  gbm2.alpha_sbpl=gbm.flnc_sbpl_indx1
  gbm2.alpha_sbpl_err=rotate([[gbm.flnc_sbpl_indx1_neg_err],[gbm.flnc_sbpl_indx1_pos_err]],1)
  gbm2.epeak_sbpl=gbm.flnc_sbpl_brken
  gbm2.epeak_sbpl_err=rotate([[gbm.flnc_sbpl_brken_neg_err],[gbm.flnc_sbpl_brken_pos_err]],1)
  gbm2.beta_sbpl=gbm.flnc_sbpl_indx2
  gbm2.beta_sbpl_err=rotate([[gbm.flnc_sbpl_indx2_neg_err],[gbm.flnc_sbpl_indx2_pos_err]],1)
  gbm2.norm_sbpl=gbm.flnc_sbpl_ampl
  gbm2.norm_sbpl_err=rotate([[gbm.flnc_sbpl_ampl_neg_err],[gbm.flnc_sbpl_ampl_pos_err]],1)
  gbm2.fluence_sbpl=gbm.flnc_sbpl_ergflnc
  gbm2.fluence_sbpl_err=gbm.flnc_sbpl_ergflnc_error

  gbm2.alpha_band=gbm.flnc_band_alpha
  gbm2.alpha_band_err=rotate([[gbm.flnc_band_alpha_neg_err],[gbm.flnc_band_alpha_pos_err]],1)
  gbm2.epeak_band=gbm.flnc_band_epeak
  gbm2.epeak_band_err=rotate([[gbm.flnc_band_epeak_neg_err],[gbm.flnc_band_epeak_pos_err]],1)
  gbm2.e0_band=gbm2.epeak_band*(2.+gbm2.alpha_band)
  gbm2.e0_band_err[0]=gbm2.epeak_band_err[0]/gbm2.epeak_band*gbm2.e0_band
  gbm2.e0_band_err[1]=gbm2.epeak_band_err[1]/gbm2.epeak_band*gbm2.e0_band
  gbm2.beta_band=gbm.flnc_band_beta
  gbm2.beta_band_err=rotate([[gbm.flnc_band_beta_neg_err],[gbm.flnc_band_beta_pos_err]],1)
  gbm2.norm_band=gbm.flnc_band_ampl
  gbm2.norm_band_err=rotate([[gbm.flnc_band_ampl_neg_err],[gbm.flnc_band_ampl_pos_err]],1)
  gbm2.fluence_band=gbm.flnc_band_ergflnc
  gbm2.fluence_band_err=gbm.flnc_band_ergflnc_error

  gbm2.tstart=gbm.flnc_spectrum_start
  gbm2.tstop=gbm.flnc_spectrum_stop

  mwrfits,gbm2,'~/Fermi/GBM_GRB_Catalog_parse.fits',/create


  return
end 
