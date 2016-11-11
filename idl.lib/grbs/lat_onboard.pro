pro lat_onboard

;  num=[1,2,3,4,5,6]
;  ra=[308.5,308.267,308.283,308.383,308.367,308.433]
;  dec=[6.9,6.9,6.917,6.933,6.950,7.017]

  num=[0,1,2,3,4,5,6]
  ra=[168.533,168.683,172.450,172.483,172.783,172.050,172.533]
  dec=[44.833,44.783,43.317,43.000,42.917,42.9,42.967]
  bra=171.248
  bdec=42.343
  locqual=[0.8864,0.9911,0.9769,0.9974,0.9961,0.9240,0.9995]
  for i=0,6 do print,i,ra[i],dec[i],separation(ra[i],dec[i],bra,bdec)/3600.,locqual[i]

  plot,ra,dec,psym=1,/iso,/yno,xrange=[165,175],yrange=[40,45]
  tvcircle,0.5,ra[2],dec[2],/data
  plots,bra,bdec,psym=2,color=!red

stop
end
