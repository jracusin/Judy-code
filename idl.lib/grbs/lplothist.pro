pro lplothist,b,x,y,_extra=_extra,xrange=xrange

  if n_params() eq 0 then begin
     plothist
     return
  end

  w=where(b ne 0 and b ge xrange[0] and b le xrange[1])
  if n_elements(xrange) gt 0 then xrange1=alog10(xrange) else xrange=minmax(alog10(b))
  logb=alog10(b[w])

;  plothist,logb,x,y,/noplot,_extra=_extra,xrange=xrange,/xsty
  xr=xrange1[1]-xrange1[0]
  xx=indgen(xr+1)+xrange1[0]
  fxx='10!U'+ntostr(fix(xx))+'!N'
;  fxx=ntostr(fix(xx))
  wf1=where(fxx eq '10!U1!N',nwf1)
  if nwf1 gt 0 then fxx[wf1]='10'

  wf0=where(fxx eq '10!U0!N',nwf0)
  if nwf0 gt 0 then fxx[wf0]='1'

  if n_elements(logb) eq 1 then logb=[xrange1[0]-1.,logb]

  plothist,logb,x,y,_extra=_extra,xtickname=[fxx,' '],xtickv=xx,xrange=xrange1,/xsty;,xticks=n_elements(xx)
  oplot,xrange,[xrange1[0],xrange1[0]]
  oplot,[xrange1[0],xrange1[0]],minmax(y)

  return
end 
