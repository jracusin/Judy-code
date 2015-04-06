@plot_lc_prelease15
pro grb050819_lc
  
  segs=ntostr(indgen(6))
  w=where(segs lt 10)
  segs[w]='0'+segs[w]
  files='00151131000-xrt/sw00151131000xwtw2po_cl.evt'
  files=[files,'001511310'+segs+'-xrt/sw001511310'+segs+'xpcw4po_cl.evt']
  
  ex=intarr(n_elements(files))
  for i=0,n_elements(files)-1 do ex[i]=exist(files[i])
  wex=where(ex eq 0,newx)
  if newx gt 1 then begin
     print,'Files does not exist: '+files[wex]
     return
  endif 
  
  ;need to corrects params
  src_ra=358.756156709d
  src_dec=24.859895431d
  back_ra=358.7107d
  back_dec=24.81668d
  src_rad=[20,20,20,20,20,20,10] ;radius in pixels of the source
  back_rad=[40,40,40,40,40,40,40] ;radius in pixels of the background
  ct_bin=[50,20,20,20,20,20,20]
  type=[0,1,1,1,1,1,1]
  BAT_time=date2met('2005-231-16:23:55')
  
  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0,'',[8e3,1e5]
  
  set_plot,'x'
  replot_xrt_lc,'GRB 050819'

;  plot_lc,type[10:*],files[10:*],src_ra,src_dec,src_rad[10:*],back_ra,back_dec,back_rad[10:*],ct_bin[10:*],BAT_time,0,1.,1,0
;  plot_lc,type[0],files[0],src_ra,src_dec,src_rad[0],back_ra,back_dec,back_rad[0],ct_bin[0],BAT_time,0,1.,1,0
  
end

pro fit_grb050819
  
  segs=ntostr(indgen(6))
  w=where(segs lt 10)
  segs[w]='0'+segs[w]
  files='00151131000-xrt/sw00151131000xwtw2po_cl.evt'
  files='../'+[files,'001511310'+segs+'-xrt/sw001511310'+segs+'xpcw4po_cl.evt']
  src_ra=358.756156709d
  src_dec=24.859895431d
  back_ra=358.7107d
  back_dec=24.81668d
  BAT_time=date2met('2005-231-16:23:55')
  title='GRB 050819'
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=20,brad=60,title=title
  
  return
end 
