pro grb_position

  ;;;GRB 120129A IPN

  ;; IPN
  ra=30.44
  dec=59.282
  cra=[23.362,31.74,34.401,28.869]
  cdec=[68.842,56.353,51.346,62.399]
  cra=[cra,cra[0]]
  cdec=[cdec,cdec[0]]

  ;; GBM 
  gra=53.920
  gdec=62.590

  plot,[40,20],[50,70],/nodata,xtitle='RA',ytitle='Dec',/yno,xrange=[40,20],/xsty

  oplot,cra,cdec,color=!green
  plots,ra,dec,psym=1
  plots,gra,gdec,psym=2,color=!red
stop
return
end 
  
