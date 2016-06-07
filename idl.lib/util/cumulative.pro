function cumulative,x,j

  if n_elements(j) eq 0 then j=1
  n=n_elements(x)
  y=dblarr(n)
  for i=1L,n-1,j do y[i]=total(x[0:i])
  y[0]=x[0]
  w=where(y ne 0)
  y=y[w]

  return,y
end
