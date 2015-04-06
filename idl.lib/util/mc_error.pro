function mc_error,x,med,sig=sig,prob=prob,doplot=doplot
  
  if n_params() eq 0 then begin
     print,'syntax - error=mc_error(x,median,sig=sig,[prob=prob,/doplot]'
     return,0
  end 

  p=0.
  if n_elements(sig) gt 0 and n_elements(prob) eq 0 then p=sigprob(sig)
  if n_elements(prob) gt 0 then p=prob

  if p eq 0 then p=0.9

  n=n_elements(x)

  s=sort(x)

  conf1=(1.-p)/2.
  conf2=(1.-p)/2.+p
  s1=round(conf1*n)
  s2=round(conf2*n)

  med=median(x)

  error=[med-x[s[s1]],x[s[s2]]-med]

  if keyword_set(doplot) then begin
     plothist,x,bin=(max(x)-min(x))/100.
     oplot,[med,med],!y.crange
     oplot,[med-error[0],med-error[0]],!y.crange,line=2
     oplot,[med+error[1],med+error[1]],!y.crange,line=2
  endif 

  return,error
end 
