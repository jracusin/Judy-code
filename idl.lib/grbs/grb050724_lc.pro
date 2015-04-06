 @plot_lc_prelease15
pro grb050724_lc
  
  ;   .com /bulk/pkg/xray/idl_lib/swift_sot/plot_lc_prelease15.pro
 
  title='GRB050724'
;  files=['00147478000-xrt/sw00147478000xwtw2po_cl_ff.evt','00147478000-xrt/pc_orb1.evt']
  
  files=['00147478000-xrt/sw00147478000xwtw2po_cl_ff.evt','00147478000-xrt/sw00147478000xpcw4po_cl.evt','00147478003-xrt/sw00147478003xpcw4po_cl.evt','00147478006-xrt/sw00147478006xpcw4po_cl.evt','00147478001-xrt/sw00147478001xpcw4po_cl.evt','00147478004-xrt/sw00147478004xpcw4po_cl.evt','00147478007-xrt/sw00147478007xpcw4po_cl.evt','00147478002-xrt/sw00147478002xpcw4po_cl.evt','00147478005-xrt/sw00147478005xpcw4po_cl.evt','00147478008-xrt/sw00147478008xpcw4po_cl.evt','00147478009-xrt/sw00147478009xpcw4po_cl.evt']
  src_ra=246.185904693d
  src_dec=-27.5404286727d
  back_ra=246.25162d
  back_dec=-27.50507d
;  src_rad=20
;  back_rad=40
  src_rad=[20,20,20,20,20,20,20,20,20,15,15]               ;radius in pixels of the source
  back_rad=60;[40,40,40,40,40,40,40,40,40,40,40]              ;radius in pixels of the background
  ct_bin=[150,25,20,20,20,20,20,20,20,20,20]                ;counts per bin
  type=[0,1,1,1,1,1,1,1,1,1,1]                    ;1=PC, 0=WT
  BAT_time=date2met('2005-205-12:34:09')
  
  n=indgen(n_elements(files))
  i=n[1:*]
  plot_lc,type[i],files[i],src_ra,src_dec,src_rad[i],back_ra,back_dec,back_rad,ct_bin[i],BAT_time,0,1.,1,0,''
;  plot_lc,type[0:1],files[0:1],src_ra,src_dec,src_rad[0:1],back_ra,back_dec,back_rad[0:1],ct_bin[0:1],BAT_time,0,1.,1,0
  
  stop
  set_plot,'x'
  
  replot_xrt_lc,title

end



pro fit_grb050724,_extra=_extra
  
  files='../'+['00147478000-xrt/sw00147478000xwtw2po_cl_ff.evt','00147478000-xrt/sw00147478000xpcw4po_cl.evt','00147478003-xrt/sw00147478003xpcw4po_cl.evt','00147478006-xrt/sw00147478006xpcw4po_cl.evt','00147478001-xrt/sw00147478001xpcw4po_cl.evt','00147478004-xrt/sw00147478004xpcw4po_cl.evt','00147478007-xrt/sw00147478007xpcw4po_cl.evt','00147478002-xrt/sw00147478002xpcw4po_cl.evt','00147478005-xrt/sw00147478005xpcw4po_cl.evt','00147478008-xrt/sw00147478008xpcw4po_cl.evt','00147478009-xrt/sw00147478009xpcw4po_cl.evt']
  src_ra=246.185904693d
  src_dec=-27.5404286727d
  back_ra=246.25162d
  back_dec=-27.50507d
  BAT_time=date2met('2005-205-12:34:09')
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=20,brad=60,_extra=_extra ;,ultstarts2=300,ultstops2=10000
  

  
  return
end 
