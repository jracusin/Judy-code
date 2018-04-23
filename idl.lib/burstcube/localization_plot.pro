pro localization_plot

  i=findgen(37)*10.
  raoff=60.
  decoff=40.

  rad=7.
  bcra=(rad*sin(i*!dtor))/cos((rad*cos(decoff*!dtor))*!dtor)
  bcdec=rad*cos(i*!dtor)
  mollview,'GW151226_LALInference_skymap.fits',title='GW151226',rot=[90,0,0],/nobar,colt=-3,outline=create_struct('coord','C','ra',bcra+raoff,'dec',bcdec+decoff),graticule=[30,30],ps='GW151226_w_BC.ps',charthick=5,charsize=1.8

  spawn,'convert -rotate 270 GW151226_w_BC.ps GW151226_w_BC.pdf'

  return
end 
