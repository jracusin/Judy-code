@plot_lc_prelease15
pro grb060111a_lc

  files=['00176818000-xrt/sw00176818000xwtw2po_cl_ff.evt',$
         '00176818000-xrt/sw00176818000xpcw2po_cl.evt',$
         '00176818001-xrt/sw00176818001xpcw2po_cl.evt',$
         '00176818002-xrt/sw00176818002xpcw2po_cl.evt',$
         '00176818003-xrt/sw00176818003xpcw2po_cl.evt',$
         '00176818004-xrt/sw00176818004xpcw3po_cl.evt',$
         '00176818005-xrt/sw00176818005xpcw2po_cl.evt',$
         '00176818006-xrt/sw00176818006xpcw2po_cl.evt',$
         '00176818007-xrt/sw00176818007xpcw2po_cl.evt',$
         '00176818007-xrt/sw00176818007xpcw3po_cl.evt']


src_ra='18:24:49'
src_dec='37:36:12'
back_ra='18:24:43.816'
back_dec='37:39:34.26'
src_rad=[25,25,20,20,20,20,20,20,20,15]     ;radius in pixels of the source
back_rad=60;[60,60,60,60,60,60,60,60,60,60]    ;radius in pixels of the background
ct_bin=[100,50,50,20,20,20,20,20,20,20]    ;counts per bin
type=[0,1,1,1,1,1,1,1,1,1]       ;1=PC, 0=WT
BAT_time=date2met('2006-011-04:23:06')
plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0

set_plot,'x'
replot_xrt_lc,'GRB 060111a'

end


pro fit_grb060111a,_extra=_extra
  
  files='../'+['00176818000-xrt/sw00176818000xwtw2po_cl_ff.evt',$
         '00176818000-xrt/sw00176818000xpcw2po_cl.evt',$
         '00176818001-xrt/sw00176818001xpcw2po_cl.evt',$
         '00176818002-xrt/sw00176818002xpcw2po_cl.evt',$
         '00176818003-xrt/sw00176818003xpcw2po_cl.evt',$
         '00176818004-xrt/sw00176818004xpcw3po_cl.evt',$
         '00176818005-xrt/sw00176818005xpcw2po_cl.evt',$
         '00176818006-xrt/sw00176818006xpcw2po_cl.evt',$
         '00176818007-xrt/sw00176818007xpcw2po_cl.evt']

;  files=['sw00176818000xwtw2po_cl_ff.evt',$
;         'sw00176818000xpcw2po_cl.evt',$
;         'sw00176818001xpcw2po_cl.evt',$
;         'sw00176818002xpcw2po_cl.evt',$
;         'sw00176818003xpcw2po_cl.evt',$
;         'sw00176818004xpcw3po_cl.evt',$
;         'sw00176818005xpcw2po_cl.evt',$
;         'sw00176818006xpcw2po_cl.evt',$
;         'sw00176818007xpcw2po_cl.evt']

  
  src_ra=276.204166667d
  src_dec=37.6033333333d
  back_ra=276.182566667d
  back_dec=37.6595166667d
  
  BAT_time=date2met('2006-011-04:23:06')
  title='GRB 060111a'
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=20,brad=30,title=title,_extra=_extra,xrange=[70,1000],xstyle=1,/zoom,yrange=[10,200],ystyle=1
  
  return
end 
