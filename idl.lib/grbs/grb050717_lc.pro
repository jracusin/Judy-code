@plot_lc_prelease15
pro grb050717_lc
  
  segs=ntostr(indgen(6))
  w=where(segs lt 10)
  segs[w]='0'+segs[w]
  files='00146372000-xrt/sw00146372000xwtw2po_cl_ff.evt'
  files=[files,'001463720'+segs+'-xrt/sw001463720'+segs+'xpcw4po_cl.evt']
  
  ex=intarr(n_elements(files))
  for i=0,n_elements(files)-1 do ex[i]=exist(files[i])
  wex=where(ex eq 0,newx)
  if newx gt 1 then begin
     print,'Files does not exist: '+files[wex]
     return
  endif 
  
  ;need to corrects params
  src_ra='14:17:24.9'
  src_dec='-50:32:13.2'
;  src_ra='14:17:24.551'
;  src_dec='-50:31:59.82'
  
;  src_ra=214.35229437d
;  src_dec=-50.5332827942d
  back_ra=214.25031d
  back_dec=-50.53065d
  src_rad=25;[25,20,20,20,20,10,10] ;radius in pixels of the source
  back_rad=40;[40,40,40,40,40,40,40] ;radius in pixels of the background
  ct_bin=[60,20,20,20,20,20,20]
  type=[0,1,1,1,1,1,1]
  BAT_time=date2met('2005-198-10:30:52.21')
  
  plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0
  set_plot,'x'
  replot_xrt_lc,'GRB 050717'
  
end
