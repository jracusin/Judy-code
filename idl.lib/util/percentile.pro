function percentile,x,perc0,ind

  if n_params() eq 0 then begin
     print,'syntax - p90=percentile(x,0.9,[ind])'
     return,0
  end 
  if perc0 gt 1 then perc=perc0/100. else perc=perc0
  s=sort(x)
  n=n_elements(x)
  p1ind=round((1-perc)/2.*n)
  p2ind=round((1+perc)/2.*n)
  if p2ind eq n then p2ind=p2ind-1
  p1=x[s[p1ind]]
  p2=x[s[p2ind]]

  ind=s[[p1ind,p2ind]]
  p=[p1,p2]

  return,p
end 
