pro image_analysis
  
  
  file=pickfile(filter='acis*.fits')
  l=mrdfits(file,1)
  
  w=where(l.x gt 4160 and l.x lt 4190 and l.y gt 4090 and l.y lt 4110)
  
