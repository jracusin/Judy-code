pro plotloghist,r,bin=bin,_extra=_extra,overplot=overplot,color=color,line=line

  if n_elements(bin) eq 0 then bin=1.
  plothist,alog10(r),x,y,bin=bin,/noplot

  if not keyword_set(overplot) then plot,10^minmax(x),minmax(y),/nodata,_extra=_extra,/xlog
  oplot,[10^(x[0]-bin),10^x,10^(x[n_elements(x)-1]+bin)],[0,y,0],psym=10,color=color,line=line

return
end
