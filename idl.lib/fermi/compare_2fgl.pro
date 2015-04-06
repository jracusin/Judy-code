pro compare_2fgl,ra,dec,err

  if n_params() eq 0 then begin 
     print,'syntax - compare_2fgl,ra,dec,err'
     return
  end 

  cat=mrdfits('~/Fermi/gll_psc_v06.fit',1)

  dist=separation(cat.raj2000,cat.dej2000,ra,dec)/3600.

  w=where(dist lt err)
  s=sort(dist[w])
  w=w[s]

  print,'Distance from Source'
  print,'Name               Association                  Flux100       Dist (deg)'
  colprint,cat[w].source_name,cat[w].assoc1,cat[w].raj2000,cat[w].dej2000,cat[w].energy_flux100,dist[w]

  return
end 
