pro bpow,x,a,ans
  
  norm=a[0]
  ind1=a[1]
  ind2=a[3]
  bt=a[2]
  
  ans = fltarr(n_elements(x))
  
  w1=where(x le bt)
  w2=where(x ge bt)
  
  ans[w1]=x[w1]^(-ind1)
  ans[w2]=bt^(ind2-ind1)*(x[w2])^(-ind2)
  
  ans= norm*ans          
  a=[norm,ind1,bt,ind2]
  
  return
end 
