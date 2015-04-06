pro collect_87a_results

  dir='/Volumes/Firewire1/racusin/sn1987a/hardsoft_images'
  cd,dir

  scale=0.01475
;  cos43=cos(43.*!dtor) 
  ring=mrdfits('ring/new_sn1987a_fpars.fits',1)
  bilat=mrdfits('bilat/new_sn1987a_fpars.fits',1)
  lobes=mrdfits('lobes/new_sn1987a_fpars.fits',1)
  f=scale;*cos43
  n=['16m','16h','16s','17m','17h','17s','19m','19h','19s','20m','20h','20s']
  band=['1.5-8.0','2.0-8.0','0.3-0.8','1.5-8.0','2.0-8.0','0.3-0.8','1.5-8.0','2.0-8.0','0.3-0.8','1.5-8.0','2.0-8.0','0.3-0.8']
  n=strmid(n,0,2)

  print,'Obs #  Band (keV)    r0 (arcsec)            r0 (arcsec)           r0 (arcsec)'
  print,'                        ring                   bilat                lobes'
  colprint,n,'    '+band,$
           '  '+numdec(ring.r0*f,3),'-'+numdec(ring.r0err[0]*f,3),'+'+numdec(ring.r0err[1]*f,3),$
           '  '+numdec(bilat.r0*f,3),'-'+numdec(bilat.r0err[0]*f,3),'+'+numdec(bilat.r0err[1]*f,3),$
           '  '+numdec(lobes.r0*f,3),'-'+numdec(lobes.r0err[0]*f,3),'+'+numdec(lobes.r0err[1]*f,3)

return
end 
