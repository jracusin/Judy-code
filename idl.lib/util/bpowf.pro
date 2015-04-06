function bpowf,x,a
  
     norm=a[0]
     ind1=a[1]
     ind2=a[3]
     bt=a[2]
     
     ans = fltarr(n_elements(x))
     w1=where(x le bt, nw1)
     if nw1 ge 1 then ans(w1) = (x[w1])^(-ind1)
     w2=where(x gt bt, nw2)
     if nw2 ge 1 then ans(w2) = bt^(-ind1+ind2)*(x[w2])^(-ind2)
     ans= norm* ans          
     a=[norm,ind1,bt,ind2]

  return,ans
end

