function bkn2lin,t,p
  
  norm=p[0]
  m1=p[1]
  m2=p[3]
  m3=p[5]
  tb1=p[2]
  tb2=p[4]
  w1=where(t le tb1,nw1)
  w2=where(t ge tb1 and t le tb2,nw2)
  w3=where(t ge tb2,nw3)

  yfit=dblarr(n_elements(t))
  if nw1 gt 0 then yfit[w1]=norm+t[w1]*m1
  if nw2 gt 0 then yfit[w2]=norm+(tb1*(m1-m2)+t[w2]*m2);(tb^(m1-m2)*t[w2]^m2)
  if nw3 gt 0 then yfit[w2]=norm+(tb2*(m2-m3)+tb1*(m1-m2)+t[w2]*m2)
  
  return,yfit
end 
  
