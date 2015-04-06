pro ploterror2,x,y,xerr,yerr,_extra=_extra

  plot,x,y,_extra=_extra
  nx=n_elements(x)
  ny=n_elements(y)
  nxerr=n_elements(xerr)
  nyerr=n_elements(yerr)
  pxerr=xerr
  if nyerr eq 0 then begin 
     pyerr=xerr
     nyerr=nxerr
     nxerr=0
  endif else pyerr=yerr

  if nxerr gt 0 then begin 
     if nxerr gt nx then $
        for i=0,nx-1 do oplot,[x[i]-pxerr[0,i],x[i]+pxerr[1,i]],[y[i],y[i]],color=color,psym=psym else $
           for i=0,nx-1 do oplot,[x[i]-pxerr[i],x[i]+pxerr[i]],[y[i],y[i]],color=color,psym=psym 
  endif 
  if nyerr gt ny then $
     for i=0,ny-1 do oplot,[x[i],x[i]],[y[i]-pyerr[0,i],y[i]+pyerr[1,i]],color=color,psym=psym else $
        for i=0,ny-1 do oplot,[x[i],x[i]],[y[i]-pyerr[i],y[i]+pyerr[i]],color=color,psym=psym

  return
end 
