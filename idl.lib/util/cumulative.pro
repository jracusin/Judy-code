function cumulative,x

  n=n_elements(x)
  y=dblarr(n)
  for i=1,n-1 do y[i]=total(x[0:i])
  y[0]=x[0]

  return,y
end
