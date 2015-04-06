pro tmp3,time,cts,err,cons,a
  
  a=[1000,-2,cons]
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 3)
  parinfo.value=a
;  parinfo[2].fixed=1
  weights=1./err^2
  
  fit=mpcurvefit(time,cts,weights,a,SIGMA,FUNCTION_NAME='pwr_wcons',/noder,parinfo=parinfo,chisq=chisq)
  
  print,a,sigma
  ploterror,time,cts,err,psym=1,/xlog,/ylog,yrange=[1e-6,1],xtitle='T-T0(s)',ytitle='cts s!U-1!N'
  oplot,time,a[0]*time^a[1]+a[2]

end 
