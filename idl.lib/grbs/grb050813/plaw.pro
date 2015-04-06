function plaw,t
  
  common plaw,alpha,cons,mint,maxt
  
  nt=n_elements(t)
  a=dblarr(nt)
 
  if alpha eq 1 then begin
;     t0=alog(mint/max(t))
;     a=alog(t/max(t))
     a=(t-mint)/(maxt-mint)
  endif else begin
;     t0=mint^(-alpha+1.)/(-alpha+1.)
;     a=t^(-alpha+1.)/(-alpha+1.)
;     for i=0,nt-1 do a[i]=total(t[0:i]^(-alpha))
;     for i=0,nt-1 do a[i]=abs(total((t[0:i]^(-alpha+1.))/(-alpha+1.)))*norm
;     for i=0,nt-1 do a[i]=t
;     for i=0,nt-1 do a[i]=((t[i]^(-alpha+1.))/(-alpha+1.)*norm-t0)
     malph1=-alpha+1.
     for i=0,nt-1 do a[i]=(1/malph1)*(t[i]/(maxt-mint))^malph1+cons*t[i]-(1./malph1)*(mint/(maxt-mint))^malph1-cons*mint
     maxa=(1/malph1)*((maxt/(maxt-mint))^malph1-(mint/(maxt-mint))^malph1)+cons*(maxt-mint)
;     a=a/max(a)
;  if cons ne 0 then a=a+cons;/(maxt-mint)
endelse   
;  a=a/a[n_elements(t)-1]
;  a=1.-a/(max(abs(a)))
;  a=a/a[nt-1]
;  a=abs(a)
;  a=(a-min(a))/(max(a)-min(a))
;  a=a/total(a)
print,a,maxa
;stop
a=a/maxa
print,a
;stop
;a=(a-min(a))/(max(a)-min(a))
  
;  print,a
;  print,t0

  
  return,a
  
end

