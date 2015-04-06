function bknlin,t,p
  
  norm=p[0]
  m1=p[1]
  m2=p[3]
  tb=p[2]
  w1=where(t le tb,nw1)
  w2=where(t ge tb,nw2)
  
  yfit=dblarr(n_elements(t))
  if nw1 gt 0 then yfit[w1]=norm+t[w1]*m1
  if nw2 gt 0 then yfit[w2]=norm+(tb*(m1-m2)+t[w2]*m2);(tb^(m1-m2)*t[w2]^m2)
  
  return,yfit
end 
  
