pro tmpks,src,bg,d,prob,alpha
  
  gooda=1.635
  als=indgen(1000)/500.-1.0
  d=dblarr(1000) & prob=d
  alpha=gooda+als
  for i=0,999 do begin
     al=alpha[i]
     
     kstest,src,bg,al,c,dd,p,/noplot
     
     d[i]=dd
     prob[i]=p
  endfor 
  
  !p.multi=[0,1,2]
  plot,alpha,-alog10(prob),psym=1,xtitle=!tsym.alpha,ytitle='-log!L10!N prob'
  w=max(prob,m)
  legend,['Max prob = '+ntostr(prob[m]),!tsym.alpha+' = '+ntostr(alpha[m])],/bottom,/right,box=0
  
  kstest,src,bg,alpha[m],title='Best fit'
  w1=where(alpha lt alpha[m])
  w2=where(alpha gt alpha[m])
  psig=min(abs(prob[w1]-0.32),msig1)
  psig=min(abs(prob[w2]-0.32),msig2)
  kstest,src,bg,alpha[w1[msig1]],title='1'+!tsym.sigma+' confidence'
  kstest,src,bg,alpha[w2[msig2]],title='1'+!tsym.sigma+' confidence'
  p90=min(abs(prob[w1]-0.1),m901)
  p90=min(abs(prob[w2]-0.1),m902)
  kstest,src,bg,alpha[w1[m901]],title='90% confidence'
  kstest,src,bg,alpha[w2[m902]],title='90% confidence'
  !p.multi=0
  
  return
end 
