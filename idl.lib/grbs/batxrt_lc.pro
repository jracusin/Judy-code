pro batxrt_lc,batfile,batfluxfact,battrig,xrtfluxfact,phil=phil,_extra=_extra,name=name,ps=ps
  
  if n_params() eq 0 then begin
     print,'syntax - batxrt_lc,batfile,batfluxfact,battrig,xrtfluxfact'
     return
  endif 
  
  if keyword_set(ps) then begplot,name='bat_xrt_lc.ps',/color,/land
  lc=lcout2fits(phil=phil)
  bat=mrdfits(batfile,1)
  b=where(bat.rate gt 0 and bat.time-battrig gt 0.)
  x=where(lc.src_rate gt 0)
  yrange=prange([bat[b].rate*batfluxfact,lc[x].src_rate*xrtfluxfact],[bat[b].error*batfluxfact,lc[x].src_rate_err*xrtfluxfact])
  
  xrange=prange([bat[b].time-battrig,lc[x].time],[bat[b].error,lc[x].tstop-lc[x].time])
  xrange[0]=1.0
  yrange[0]=1e-14
  xrange[1]=10.^round(alog10(xrange[1])+0.5)
  
  if n_elements(name) gt 0 then title='black-BAT blue-WT red-pc       '+name+'                        ' else title='black-BAT blue-WT red-pc                                            '
  
  plot_like_qdp,xrange=xrange,flux=xrtfluxfact,yrange=yrange,xstyle=1,_extra=_extra,title=title,phil=phil
  
  w=where((bat.rate-bat.error)*batfluxfact gt 1e-14)
;  baterr=bat[1:*].timedel-bat[0:n_elements(bat)-2].timedel
  baterr=bat.timedel
  
  oploterror,bat[w].time- 206081971.70,bat[w].rate*batfluxfact,baterr[w]/2.,bat[w].error*batfluxfact,psym=3,/nohat
  
  if keyword_set(ps) then endplot
  
  stop
  return
end 
