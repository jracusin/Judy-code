pro sell_fermi

  cd,'~/Fermi/Sell_Fermi/'
  readcol,'lc_crab_nebula_synch_all3day_239557417_324854180_259200_100.lc',time, e_time, flux, ed_flux ,eu_flux, ulim_flux, index, e_index ,TS, sundist, exposure, npred,format='(d,d,d,d,d,d,d,d,d,d,d,d)'

  plot,[2008.5,2012],[0,150],/nodata,xtitle='Year',ytitle='Gamma-ray Flux',xrange=[2008.5,2012],/xsty
  n=n_elements(time)
  year=dblarr(n)
  for i=0,n-1 do begin 
     date=met2date_judy(time[i])
     year[i]=date[0]+date[1]/365.+(date[2]+date[3]/60.+date[4]/3600.)/24.
  endfor 

  x=[year,reverse(year)]
  y=[flux+ed_flux,reverse(flux-ed_flux)]
;  x=smooth(x,5)
;  y=smooth(y,5)
  w=where((x lt 2009.4 or x gt 2009.55 ) or (x lt 2011.1 or x gt 2011.15) or (x lt 2011.15))
  
  polyfill,x,y,color=!blue


stop

  return
end 
