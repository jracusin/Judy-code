@plot_lc_prelease15
pro grb050822_lc

  files=['00151486000-xrt/sw00151486000xwtw2po_cl_f1.evt',$
         '00151486000-xrt/sw00151486000xpcw4po_cl_f1.evt',$
         '00151486000-xrt/sw00151486000xwtw2po_cl_f2.evt',$
         '00151486000-xrt/sw00151486000xpcw4po_cl_f2.evt',$
         '00151486001-xrt/sw00151486001xpcw4po_cl.evt',$
         '00151486002-xrt/sw00151486002xpcw4po_cl.evt',$
         '00151486003-xrt/sw00151486003xpcw4po_cl.evt',$
         '00151486004-xrt/sw00151486004xpcw4po_cl.evt',$
         '00151486005-xrt/sw00151486005xpcw4po_cl.evt',$
         '00151486006-xrt/sw00151486006xpcw4po_cl.evt',$
         '00151486007-xrt/sw00151486007xpcw4po_cl.evt',$
         '00151486008-xrt/sw00151486008xpcw4po_cl.evt',$
         '00151486009-xrt/sw00151486009xpcw4po_cl.evt',$
         '00151486010-xrt/sw00151486010xpcw4po_cl.evt',$
         '00151486011-xrt/sw00151486011xpcw4po_cl.evt',$
         '00151486012-xrt/sw00151486012xpcw4po_cl.evt',$
         '00151486013-xrt/sw00151486013xpcw4po_cl.evt',$
         '00151486014-xrt/sw00151486014xpcw4po_cl.evt',$
         '00151486015-xrt/sw00151486015xpcw4po_cl.evt',$
         '00151486016-xrt/sw00151486016xpcw4po_cl.evt',$
         '00151486018-xrt/sw00151486018xpcw4po_cl.evt',$
         '00151486020-xrt/sw00151486020xpcw4po_cl.evt',$
         '00151486021-xrt/sw00151486021xpcw4po_cl.evt',$
         '00151486022-xrt/sw00151486022xpcw4po_cl.evt']
stop
src_ra='03:24:26'
src_dec='-46:02:07'
back_ra='03:24:06.309'
back_dec='-46:01:59.78'
;src_rad=[25,25,25,25,25,25,25,25,20,20,20,20,20,20,20,20,20,20,20,15,15,15,15,15]                   ;radius in pixels of the source
src_rad=[15,15,15,15,15,15,15,15,15,15,15,15,15,15,10,10,10,10,10,10,10,10,10,10]
back_rad=60    ;radius in pixels of the background
ct_bin=[100,20,100,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50]        ;counts per bin
type=intarr(n_elements(files))
;type[1:*]=1
type=[0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]       ;1=PC, 0=WT
BAT_time=date2met('2005-234-03:49:29')

i=indgen(24)
n=i[0:1];[2:*]

plot_lc,type[n],files[n],src_ra,src_dec,src_rad[n],back_ra,back_dec,back_rad,ct_bin[n],BAT_time,0,1.,1,0,''

set_plot,'x'
replot_xrt_lc,'GRB 050822'

end


pro fit_grb050822,_extra=_extra
  
  files='../'+['00151486000-xrt/sw00151486000xwtw2po_cl_ff.evt',$
         '00151486000-xrt/sw00151486000xpcw4po_cl.evt',$
         '00151486001-xrt/sw00151486001xpcw4po_cl.evt',$
         '00151486002-xrt/sw00151486002xpcw4po_cl.evt',$
         '00151486003-xrt/sw00151486003xpcw4po_cl.evt',$
         '00151486004-xrt/sw00151486004xpcw4po_cl.evt',$
         '00151486005-xrt/sw00151486005xpcw4po_cl.evt',$
         '00151486006-xrt/sw00151486006xpcw4po_cl.evt',$
         '00151486007-xrt/sw00151486007xpcw4po_cl.evt',$
         '00151486008-xrt/sw00151486008xpcw4po_cl.evt',$
         '00151486009-xrt/sw00151486009xpcw4po_cl.evt',$
         '00151486010-xrt/sw00151486010xpcw4po_cl.evt',$
         '00151486011-xrt/sw00151486011xpcw4po_cl.evt',$
         '00151486012-xrt/sw00151486012xpcw4po_cl.evt',$
         '00151486013-xrt/sw00151486013xpcw4po_cl.evt',$
         '00151486014-xrt/sw00151486014xpcw4po_cl.evt',$
         '00151486015-xrt/sw00151486015xpcw4po_cl.evt',$
         '00151486016-xrt/sw00151486016xpcw4po_cl.evt',$
         '00151486018-xrt/sw00151486018xpcw4po_cl.evt',$
         '00151486020-xrt/sw00151486020xpcw4po_cl.evt',$
         '00151486021-xrt/sw00151486021xpcw4po_cl.evt',$
         '00151486022-xrt/sw00151486022xpcw4po_cl.evt']
  

  src_ra=51.1130611688;51.11
  src_dec=-46.0332753114;-46.0352777778
  back_ra=51.0262875
  back_dec=-46.0332722222
  BAT_time=date2met('2005-234-03:49:29')
  title='GRB 050822'
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=15,brad=40,title=title
  
  return
end 
