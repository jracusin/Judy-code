@plot_lc_prelease15
pro grb060901_lc
  
  files=['/bulk/shadow/racusin/grbs/grb060901/seg1_v7/sw00020039001xpcw2po_cl.evt','/bulk/shadow/racusin/grbs/grb060901/seg2_v7/sw00020039002xpcw2po_cl.evt']
  
  
  ;need to corrects params
  src_ra=287.158235211
  src_dec=-6.63551208155
  back_ra=287.12598
  back_dec=-6.5744516
  src_rad=20 ;radius in pixels of the source
  back_rad=60 ;radius in pixels of the background
  ct_bin=[20,20]
  type=[1,1]
  BAT_time=178829040d;date2met('2005-122-09:25:40')
  
;  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0,'',[3e3]
  i=[1,2]
  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0,''
  set_plot,'x'
  replot_xrt_lc,title='GRB 060901'
end
