pro plot_cdf,x,y,overplot=overplot,xtitle=xtitle,color=color,line=line,thick=thick

  if n_params() eq 0 then begin 
     print,'syntax - plot_cdf,x,y,/overplot,xtitle=xtitle,color=color,line=line'
     return
  end 

  if n_elements(xrange) eq 0 then xrange=minmax(x)
  if n_elements(ytitle) eq 0 then ytitle='P' else ytitle=''
  if not keyword_set(overplot) then begin
     plot,xrange,[0,1],/nodata,_extra=_extra,ytitle=ytitle,xtitle=xtitle
  endif

  n=n_elements(x)
  for i=0,n-1 do begin 
     if i eq 0 then begin 
        oplot,[0,x[i]],[0,0],line=line,color=color,thick=thick
        oplot,[x[i],x[i]],[0,y[i]],line=line,color=color,thick=thick
     endif
     if i lt n-1 then begin 
        oplot,[x[i],x[i+1]],[y[i],y[i]],line=line,color=color,thick=thick
        oplot,[x[i+1],x[i+1]],[y[i],y[i+1]],line=line,color=color,thick=thick
     endif
  endfor

return
end 
