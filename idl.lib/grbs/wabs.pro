function wabs,eng,nh,z
  ;;;; reproducing XSPEC WABS models from Morrison & McCammon 1983

  w=where(eng ge 0.03 and eng le 10.0,nw)
  ab=dblarr(n_elements(eng)) & sigma_e=dblarr(nw)
  ab[*]=1.
  e1=[0.03,0.1,0.284,0.4,0.532,0.707,0.867,1.303,1.84,2.471,3.21,4.038,7.111,8.331d]
  e2=[0.1,0.284,0.4,0.532,0.707,0.867,1.303,1.84,2.471,3.21,4.038,7.111,8.331,10.0d]
  em=(e2-e1)/2.+e1

  c0=[17.3,34.6,78.1,71.4,95.5,308.9,120.6,141.3,202.7,342.7,352.2,433.9,629.0,701.2d]
  c1=[608.1,267.9,18.8,66.8,145.8,-380.6,169.3,146.8,104.7,18.7,18.7,-2.4,30.9,25.2d]
  c2=[-2150.,-476.0,4.3,-51.4,-61.1,294.,-47.7,-31.5,-17.0,0.0,0.0,0.75,0.0,0.0d]

  if n_elements(z) eq 0 then en=eng[w] else en=eng[w]*(1+z)
  sigma_e1=(c0+c1*e1+c2*e1^2)*e1^(-3)*1d-24
  sigma_e2=(c0+c1*e2+c2*e2^2)*e2^(-3)*1d-24
  sigma_em=(c0+c1*em+c2*em^2)*em^(-3)*1d-24
  
;  sigma_e=interpol(sigma_em,em,en,/lsquad)
  for i=0,nw-1 do begin
     pen=en[i]
     k=where(pen ge e1 and pen le e2)
     sigma_e[i]=(c0[k]+c1[k]*pen+c2[k]*pen^2)*pen^(-3)*1d-24
  endfor 

;  abs=exp(-nh*1d22*sigma_em)

;  ab[w]=interpol(abs*1d12,em,en)
;  ab[w]=ab[w]/1d12

;  plot,en,ab,psym=3,/xlog,/ylog,yrange=[1e-2,1],xrange=[1,3]
;  oplot,em,abs,psym=1

  ab[w]=exp(-nh*1d22*sigma_e)
;stop
return,ab
end 
