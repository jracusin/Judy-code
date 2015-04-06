@plot_lc_prelease15
pro grb050502b_lc
  
  files=['00116116000-xrt/sw00116116000xwtw2po_cl_ff.evt',$
         '00116116000-xrt/sw00116116000xpcw4po_cl_ff2.evt',$
         '00116116001-xrt/sw00116116001xpcw4po_cl.evt',$
         '00116116002-xrt/sw00116116002xpcw4po_cl.evt',$
         '00116116003-xrt/sw00116116003xpcw4po_cl.evt',$
         '00116116004-xrt/sw00116116004xpcw4po_cl.evt',$
         '00116116005-xrt/sw00116116005xpcw4po_cl.evt']
  
  
  ;need to corrects params
  src_ra='09:30:10.200'
  src_dec='16:59:51.00'
  back_ra=142.48143d
  back_dec=16.998997d
  src_rad=[20,20,20,20,20,20,15] ;radius in pixels of the source
  back_rad=[40,40,40,40,40,40,40] ;radius in pixels of the background
  ct_bin=[25,60,20,20,20,20,20]
  type=[0,1,1,1,1,1,1]
  BAT_time=date2met('2005-122-09:25:40')
  
;  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0,'',[3e3]
  i=1
  plot_lc,type[i],files[i],src_ra,src_dec,src_rad[i],back_ra,back_dec,back_rad[i],ct_bin[i],BAT_time,0,1.,1,0,'',[3e3,3.5e4]
  set_plot,'x'
  replot_xrt_lc,'GRB 050502b'
end

pro fit_grb050502b
  
  files='../'+['00116116000-xrt/sw00116116000xwtw2po_cl_ff.evt',$
               '00116116000-xrt/sw00116116000xpcw4po_cl_ff2.evt',$
               '00116116001-xrt/sw00116116001xpcw4po_cl.evt',$
               '00116116002-xrt/sw00116116002xpcw4po_cl.evt',$
               '00116116003-xrt/sw00116116003xpcw4po_cl.evt',$
               '00116116004-xrt/sw00116116004xpcw4po_cl.evt',$
               '00116116005-xrt/sw00116116005xpcw4po_cl.evt']
  
  src_ra=142.5425
  src_dec=16.9975
  back_ra=142.48143d
  back_dec=16.998997d
  BAT_time=date2met('2005-122-09:25:40')
  title='GRB 050502b'
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=20,brad=60,title=title,/claudio
    
  
  
  return
end 
