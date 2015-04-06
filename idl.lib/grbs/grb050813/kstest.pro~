pro kstest,src,bg,alph,c,d,prob,title=title,noplot=noplot
  
  common plaw,alpha,cons,mint,maxt
  
  if keyword_set(noplot) then doplot=0 else doplot=1
;  src=mrdfits('src0.evt',1)
;  bg=mrdfits('bg0.evt',1)
;  gti=mrdfits('src0.evt',2)
  
  btime=date2met('2005-225-06:45:09.44')
  
  stime=src.time-btime
  w=where(stime lt 1100)
  stime=stime[w]  
  bgtime=bg.time-btime
  w=where(bgtime lt 1100)
  bgtime=bgtime[w]
  
  mint=82.
  maxt=1087.
  galclus=0.5 ;0.5 cts/ks
  t=stime-mint                  ;/max(stime)

  if n_elements(c) eq 0 then cons=(n_elements(bgtime)/4.+galclus)/(maxt-mint) else cons=c
;  cons=0

  if n_elements(alph) eq 0 then alpha=1 else alpha=alph
  tle=!tsym.alpha+'='+ntostr(alpha)+'    c='+ntostr(cons)
  if n_elements(title) ne 0 then title=tle+'   '+title else title=tle
  ksonep,t,'plaw2',d,prob,plot=doplot,xtitle='T-T0(s)',title=title,xrange=[0,1005],xstyle=1
  print,d,prob
  if doplot then legend,['d = '+ntostr(d),'prob = '+ntostr(prob)],/bottom,/right,box=0

 ; stop
  return
end 
