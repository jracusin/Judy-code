function yoni_function,t,p,f1,f2
  
;  p=[1.,60.,-1.,2.,5.,1.,1000.,-0.5,1.05,0.88]
  norm1=p[0]
  expfact=p[1]
  norm2=p[2]
  t0=p[3]
  alpha1=p[4]
  alpha2=p[5]
  s1=p[6]
  
  norm2=p[7]
  t1=p[8]
  alpha3=p[9]
  alpha4=p[10]
  s2=p[11]
  
  fe=norm1*exp(-t/expfact)
  f1=norm2/2.*((t/t0)^(s1*alpha1)+(t/t0)^(s1*alpha2))^(-1./s1)
  f2=norm3/2.*((t/t1)^(s2*alpha3)+(t/t1)^(s2*alpha4))^(-1./s2)
  
  yfit=f1+f2

  return,yfit
end 
