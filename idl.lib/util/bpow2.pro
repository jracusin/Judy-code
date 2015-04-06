pro bpow2,x,a,ans
  
     norm=a[0]
     ind1=a[1]
     ind2=a[3]
     bt=a[2]
     
     ans = fltarr(n_elements(x))
;     if bt gt 0.0 then begin
        w1=where(x le bt, nw1)
        if nw1 ge 1 then ans(w1) = (x[w1])^(-ind1)
;        if nw1 ge 1 then ans(w1) = (bt/x(w1))^ind1
        w2=where(x gt bt, nw2)
        if nw2 ge 1 then ans(w2) = bt^(-ind1+ind2)*(x[w2])^(-ind2)
;        if nw2 ge 1 then ans(w2) = (bt/x(w2))^ind2
     
        ans= norm* ans          
        a=[norm,ind1,bt,ind2]
;     endif 

  return
end

