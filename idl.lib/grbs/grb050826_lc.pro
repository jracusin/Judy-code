@plot_lc_prelease15.pro
pro grb050826_lc
  
  title='GRB050826'
  
  files=['00152113000-xrt/sw00152113000xpcw4po_cl.evt','00152113002-xrt/sw00152113002xpcw4po_cl.evt']

  src_ra=87.7563158764
  src_dec=-2.64416766315
  back_ra=87.711741
  back_dec=-2.6965534
  src_rad=[20,10]               ;radius in pixels of the source
  back_rad=[40,40]              ;radius in pixels of the background
  ct_bin=[20,20]                ;counts per bin
  type=[1,1]                    ;1=PC, 0=WT
  BAT_time=date2met('2005-238-06:18:10')
  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0

  set_plot,'x'


end


pro fit_grb050826
  
  files=['../00152113000-xrt/sw00152113000xpcw4po_cl.evt','../00152113002-xrt/sw00152113002xpcw4po_cl.evt']
  BAT_time=date2met('2005-238-06:18:10')
  
  src_ra=87.7563158764
  src_dec=-2.64416766315
  back_ra=87.711741
  back_dec=-2.6965534
  title='GRB 050826'
  fit_flares,ulfiles=files,ffiles=files,ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=20,brad=60,title=title
  
  return
end 
