pro grb_known_src,ra,dec,err

  twofgl=mrdfits('~/Fermi/gll_psc_v06.fit',1)

  dist=separation(twofgl.raj2000,twofgl.dej2000,ra,dec)/3600.

  w=where(dist lt 15.,nw)

  plot,twofgl[w].raj2000,twofgl[w].dej2000,psym=2
  print,'2FGL sources within 15 deg: ',nw
  print,twofgl[w].assoc1

  plots,ra,dec,psym=1,color=!red

  return
end 

  
