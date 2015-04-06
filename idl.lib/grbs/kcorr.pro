function kband,x,p

  alpha=p[0]
  beta=p[2]
  e0=p[1]/(p[0]-p[2]) ;; input Epeak, use E0
  w0=where(x lt (alpha-beta)*e0)
  w1=where(x gt (alpha-beta)*e0)
  y=dblarr(n_elements(x))
  y[w0]=x[w0]*(x[w0]/100.)^p[0]*exp(-x[w0]/p[1])
  y[w1]=x[w1]*((p[0]-p[2])*p[1]/100.)^(p[0]-p[2])*exp(p[2]-p[0])*(x[w1]/100.)^p[2]

  return,y
end

function kcorr,z,param,pow=pow,band=band

  if n_params() eq 0 then begin
     print,'syntax:  k=kcorr(z,params,pow=pow,band=band)'
     print,'          if pow then params = phind'
     print,'          if band then params=[alpha,epeak,beta]'
     return,0
  end 
  emin=1.
  emax=10000.
;  s0=fluence/qpint1d('x*x^(-p[0])',/expression,emin,emax,gamma)

  if keyword_set(pow) then begin 
     func='x*x^(-p[0])'
     k1=qpint1d(func,/expression,emin/(1+z),emax/(1+z),param)
     k2=qpint1d(func,/expression,emin,emax,param)
  endif 
  if keyword_set(band) then begin 
     k1=qpint1d('kband',emin/(1+z),emax/(1+z),param)
     k2=qpint1d('kband',emin,emax,param)

  endif 

  k=k1/k2

  return,k
end 
