function pr_correlate,x,y,z,silent=silent

  r12=r_correlate(x,y)
  r13=r_correlate(x,z)
  r23=r_correlate(y,z)
  
  num=n_elements(x)
  pc=(r12[0]-(r23[0]*r13[0]))/(((1.-r13[0]^2.)^0.5)*((1.-r23[0]^2.)^0.5))
  if not keyword_set(silent) then print,"Partial correlation: ",pc
  fac=(1.+pc)*(1.-pc)
  if not keyword_set(silent) then print,"Fac (must be >0 for correct null hyp): ", fac
  if fac lt 0 then print,'INVALID NULL HYPOTHESIS'
  t = pc * ((num - 2.0)/fac)^0.5
  df=num-2.
  probrs = ibeta(0.5*df, 0.5, df/(df+t^2))

  pr=[pc,probrs]

  return,pr
end 
