@plot_lc_prelease15
pro grb060906_lc
  
  files=['/bulk/shadow/racusin/grbs/grb060906/seg0_v2/sw00228316991xwtw2po_cl_1.evt','/bulk/shadow/racusin/grbs/grb060906/seg0_v9/sw00228316000xpcw2po_cl.evt','/bulk/shadow/racusin/grbs/grb060906/seg2_v17/sw00228316002xpcw2po_cl.evt','/bulk/shadow/racusin/grbs/grb060906/seg3_v18/sw00228316003xpcw2po_cl.evt','/bulk/shadow/racusin/grbs/grb060906/seg4_v11/sw00228316004xpcw2po_cl.evt','/bulk/shadow/racusin/grbs/grb060906/seg5_v9/sw00228316005xpcw2po_cl.evt']
  
  
  ;need to corrects params
  src_ra=40.7537694538
  src_dec= 30.3608189729
  back_ra=40.822411
  back_dec=30.381097
  src_rad=20 ;radius in pixels of the source
  back_rad=60 ;radius in pixels of the background
  ct_bin=[50,50,20,20,20,20]
  type=[0,1,1,1,1,1]
  BAT_time=179224366d
  
;  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0,'',[3e3]
  i=[3,4,5]
  plot_lc,type[i],files[i],src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin[i],BAT_time,0,1.,1,0,''
  set_plot,'x'
  replot_xrt_lc,title='GRB 060901'
end
