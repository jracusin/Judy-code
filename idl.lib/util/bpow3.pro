pro bpow3,x,a,yfit
  
  ;fit 3 part broken power law
  norm=a[0]
  ind1=a[1]
  bt1=a[2]
  ind2=a[3]
  bt2=a[4]
  ind3=a[5]
  
  w1=where(x le bt1)
  w2=where(x ge bt1 and x le bt2)
  w3=where(x ge bt2)
  
  yfit=fltarr(n_elements(x))
  
  yfit[w1]=x[w1]^(-ind1)
  yfit[w2]=bt1^(ind2-ind1)*x[w2]^(-ind2)
  yfit[w3]=bt1^(ind2-ind1)*bt2^(ind3-ind2)*x[w3]^(-ind3)
  
  yfit=norm*yfit
  
  return
end

