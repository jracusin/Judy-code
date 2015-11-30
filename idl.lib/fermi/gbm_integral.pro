pro gbm_integral

  g=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  l=mrdfits('~/GRBs/Integral_SPI/combined_triglist.fits',1)

  ind=intarr(n_elements(g))
  ind[*]=-1
  for i=0,n_elements(g)-1 do begin
     met=date2met(g[i].trigger_time,/fermi)
     w=where(abs(met-l.trigtime) lt 10.,nw)
     if nw gt 0 then ind[i]=w[0]
  endfor 

  help,where(ind ne -1)
  stop



  return
end 
