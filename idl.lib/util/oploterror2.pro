pro oploterror2,x,y,xerr,yerr,color=color,_extra=_extra,xmin=xmin,ymin=ymin

  if n_params() eq 0 then begin
     print,'syntax - oploterror2,x,y,xerr,yerr,_extra=_extra'
     print,'         oploterror for asymmetric errors'
     print,'         errors must be fltarr(2,n)'
     return
  end 

  n=n_elements(x)
  sx=size(xerr)
  if sx[0] le 1 then xerr=[[xerr],[xerr]]
  if sx[1] gt 2 then xerr=rotate(xerr,4)

  sy=size(yerr)
  if sy[0] le 1 then yerr=[[yerr],[yerr]]
  if sy[1] gt 2 then yerr=rotate(yerr,4)

;  if n_elements(xmin) eq 0 then xmin=min([x,0.])
;  if n_elements(ymin) eq 0 then ymin=min([y,0.])

  plots,x,y,_extra=_extra,color=color
  for i=0,n-1 do begin
     xlow=x[i]-xerr[0,i]
;     if xlow lt xmin then xlow=xmin
     xhigh=x[i]+xerr[1,i]
     oplot,[xlow,xhigh],[y[i],y[i]],color=color,line=0
     ylow=y[i]-yerr[0,i]
;     if ylow lt ymin then ylow=ymin
     yhigh=y[i]+yerr[1,i]
     oplot,[x[i],x[i]],[ylow,yhigh],color=color,line=0
  endfor 

return
end 
     
