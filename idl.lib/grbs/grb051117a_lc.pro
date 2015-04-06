@plot_lc_prelease15
pro grb051117a_lc
  
  segs=ntostr(indgen(19))
  w=where(segs lt 10)
  segs[w]='0'+segs[w]
  files='00164268000-xrt/sw00164268000xwtw2po_cl.evt'
  segs=[segs[0:2],segs[4],segs[6],segs[8:*]]
  files=[files,'001642680'+segs+'-xrt/sw001642680'+segs+'xpcw2po_cl.evt']
  
  ;need to corrects params
  src_ra=228.39098553d
  src_dec=30.8702994582d
  back_ra=228.48155;228.45642
  back_dec=30.757018;30.779293
  src_rad=[15,15,15,15,15,15,15,15,15,15,15,15,10,10,10,10,10] ;radius in pixels of the source
  back_rad=[40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40] ;radius in pixels of the background
  ct_bin=[400,200,100,50,50,50,50,50,50,50,50,50,50,50,50,50,50]
  type=[0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  BAT_time=date2met('2005-321-10:51:20')
  
  x=indgen(17)
  i=x[2:*]
  plot_lc,type[i],files[i],src_ra,src_dec,src_rad[i],back_ra,back_dec,back_rad[i],ct_bin[i],BAT_time,0,1.,1,0
  set_plot,'x'
  replot_xrt_lc,'GRB 051117a'

;  plot_lc,type[10:*],files[10:*],src_ra,src_dec,src_rad[10:*],back_ra,back_dec,back_rad[10:*],ct_bin[10:*],BAT_time,0,1.,1,0
;  plot_lc,type[0],files[0],src_ra,src_dec,src_rad[0],back_ra,back_dec,back_rad[0],ct_bin[0],BAT_time,0,1.,1,0
  

end

pro fit_grb051117a
  segs=ntostr(indgen(19))
  w=where(segs lt 10)
  segs[w]='0'+segs[w]
  files='00164268000-xrt/sw00164268000xwtw2po_cl.evt'
  segs=[segs[0:2],segs[4],segs[6],segs[8:*]]
  files='../'+[files,'001642680'+segs+'-xrt/sw001642680'+segs+'xpcw2po_cl.evt']
  
  ;need to corrects params
  src_ra=228.39098553d
  src_dec=30.8702994582d
  back_ra=228.48155;228.45642
  back_dec=30.757018;30.779293
  BAT_time=date2met('2005-321-10:51:20')
  title='GRB 051117a'
  
  fit_flares,ulfiles=files[1:*],ffiles=files[0],ra=src_ra,dec=src_dec,backra=back_ra,backdec=back_dec,bat_trig=bat_time,srad=15,brad=40,title=title,/zoom,xrange=[100,1e4],yrange=[1,300],xstyle=1,ystyle=1
  
  
  return
end 
