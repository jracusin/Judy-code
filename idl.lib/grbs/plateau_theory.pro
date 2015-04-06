pro plateau_theory

  t=[findgen(100)+1,(findgen(10)+1)*100,(findgen(10)+1)*1e3,(findgen(10)+1)*1e4]
  t=t[rem_dup(t)]

  f1=pow(t,[1e3,6.])

  f2=bknpow(t,[1.,0.5,100,2.])

  f3=bknpow(t,[1.5,0.8,1e4,2.])

  plot,[1,1e5],[1e-6,1e4],/xlog,/ylog,/nodata

  oplot,t,f1
  oplot,t,f2,line=1
  oplot,t,f3,line=2

  f=f1+f2+f3
  oplot,t,f,line=3,color=!green

return
end 
