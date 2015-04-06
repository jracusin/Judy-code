function conv_conf,mean,sig,conf,err
  
  if n_params() eq 0 then begin 
     print,'syntax - conv_conf,mean,sig,conf'
     return,0
  end 

  if mean eq 0 then xrange=[-sig*10,sig*10] else $
     xrange=[mean-mean*2.,mean+mean*2.]

  x=dindgen(1000.)/1000.*(xrange[1]-xrange[0])+xrange[0]

  y=gaussian(x,[1.0,mean,sig])

  plot,x,y
  ty=total(y)
  w=where(x ge mean-sig and x le mean+sig)
  sig1=total(y[w])/ty
;  print,'1 sig = ',sig1

  p=dblarr(500)
  for i=0,499 do begin
     w=where(x ge x[i] and x le (max(x)-x[i]))
     p[i]=total(y[w])/ty
  endfor 

  conf0=approx(p,conf,m)
;  print,x[m]
  err=mean-x[m]

;  print,conf,err
;  print,err/sig
  
;stop
 
return,err
end 
