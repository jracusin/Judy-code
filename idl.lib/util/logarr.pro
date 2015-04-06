function logarr,xmin,xmax,bin=bin

  if n_elements(xmin) eq 0 then xmin=1d
  if n_elements(xmax) eq 0 then xmax=1d6
  if n_elements(bin) eq 0 then bin=1d

  n=round(alog10(xmax)+1)-round(alog10(xmin)-1)
  xxmin=round(alog10(xmin)-1)

  arr=(findgen(9/bin)*bin)+1.
  x=0d
  for i=0d,n do x=[x,arr*10^(i+xxmin)]
  x=[x[1:*],10^n]
  w=where(x ge xmin and x le xmax)
  x=x[w]
  r=rem_dup(x)
  x=x[r]

  return,x
end
