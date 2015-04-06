pro fill_crflags,cr,crflag
  
  n=n_elements(cr)
  crflag=lonarr(n)
  
  w1=where(not cr.highlat1 or not cr.highlat2,nw1)
  if nw1 gt 0 then crflag[w1]=2L^1
  w2=where(cr.isms2ai or cr.isms
     



