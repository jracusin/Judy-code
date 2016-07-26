pro lat_onboard

  num=[1,2,3,4,5,6]
  ra=[308.5,308.267,308.283,308.383,308.367,308.433]
  dec=[6.9,6.9,6.917,6.933,6.950,7.017]

  plot,ra,dec,psym=1,/iso,/yno,xrange=[308,309],yrange=[6.5,7.5]
  tvcircle,0.5,ra[1],dec[1],/data

stop
end
