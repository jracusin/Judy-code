pro healprob,map,sigarea

  w=where(map ge 1e-8,nw)
  npix=n_elements(map)*1d
  s=sort(1-map[w])

  j=1.
  if nw gt 1e4 then j=100.
  if nw gt 1e3 then j=10.
  c=cumulative(map[w[s]],j)

  sig1=sigprob(1.)
  sig2=sigprob(2.)
  sig3=sigprob(3.)
  
  w1=where(c le sig1,nw1)
  w2=where(c le sig2,nw2)
  w3=where(c le sig3,nw3)

  allsky=4*!pi*(180/!pi)^2
  sigarea=[nw1,nw2,nw3]*j/npix*allsky
  return
end 
