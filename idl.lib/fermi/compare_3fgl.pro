pro compare_3fgl,ra,dec,err

  if n_params() eq 0 then begin 
     print,'syntax - compare_2fgl,ra,dec,err'
     return
  end 

;  cat=mrdfits('~/Fermi/gll_psc_v06.fit',1)
  readcol,'~/Fermi/3FGL_list_for_Judy.txt',name,raj2000,decj2000,sig,Flux100,Unc_Flux100,Flux1000,Unc_Flux1000,format='(a,f,f,f,f,f,f)'


  dist=separation(raj2000,decj2000,ra,dec)/3600.

  w=where(dist lt err)
  s=sort(dist[w])
  w=w[s]

  print,'Distance from Source'
  print,'Name                      RA            Dec        Signif_avg   Flux100         Dist (deg)'
  colprint,name[w],raj2000[w],decj2000[w],sig[w],flux100[w],dist[w]

  return
end 
