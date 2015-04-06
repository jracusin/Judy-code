pro make_crpaper_tables,cr
  
  alpha=cr.alpha
  alphaerr0=cr.alphaerr[0]
  alphaerr1=cr.alphaerr[1]
  beta=cr.beta
  betaerr0=cr.betaerr[0]
  betaerr1=cr.betaerr[1]
  tbreak=cr.tbreak
  tbreakerr0=cr.tbreakerr[0]
  tbreakerr1=cr.tbreakerr[1]
  
  w=where(finite(alphaerr0) eq 0,nw)
  if nw gt 0 then alphaerr0[w]=0.
  w=where(finite(alphaerr1) eq 0,nw)
  if nw gt 0 then alphaerr1[w]=0.
  w=where(finite(betaerr0) eq 0,nw)
  if nw gt 0 then betaerr0[w]=0.
  w=where(finite(betaerr1) eq 0,nw)
  if nw gt 0 then betaerr1[w]=0.
  w=where(finite(tbreakerr0) eq 0,nw)
  if nw gt 0 then tbreakerr0[w]=0.
  w=where(finite(tbreakerr1) eq 0,nw)
  if nw gt 0 then tbreakerr1[w]=0.
  
  
  grbs=cr.grb
  grb=grbs
  for i=0,n_elements(cr)-2 do begin
     if grbs[i+1] eq grbs[i] then grb[i+1]=''
  endfor 
  
  alpha=sigfig(alpha,3)+'^{+'+sigfig(alphaerr1,3)+'}_{-'+sigfig(alphaerr0,3)+'}'
  beta=sigfig(beta,3)+'^{+'+sigfig(betaerr1,3)+'}_{-'+sigfig(betaerr0,3)+'}'
  tbreak=sigfig(tbreak,3)+'^{+'+sigfig(tbreakerr1,3)+'}_{-'+sigfig(tbreakerr0,3)+'}'
  
  s='  &   '
  d='$'
  openw,lun,'~/papers/jetbreaks1/grb_table.dat',/get_lun
  printf,lun,grb+s+d+alpha+d+s+d+tbreak+d+s+d+beta+d+' \\'
  close,/all
  
  stop
  return
end 
