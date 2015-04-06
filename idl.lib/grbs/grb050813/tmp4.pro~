pro tmp4,src,bg,bp,time0,cts0,err0,time,cts,err,_extra=_extra
  

  btime=date2met('2005-225-06:45:09.44')
  t=[48,64,99,791.3,1228.6,2351.4,2416.9,2532.2,2472,740,8665,9302,12882]
  tmin=[82,131,196,296,1094.1,5450.6,11295.2,17052.5,22810.7,28597.8,237117.7,497728.2,723137.4]
  tmax=[130,195,295,1087.3,2322.7,8108.9,13895.3,19681.3,25467.8,29673.8,268370.4,562248.3,833990.2]
  time1=[255142.5,537434.2,800969.6]
;  qcorr=[0.777870,0.777870,0.777870,0.777870,0.777870,0.857632,0.857632,0.938922,0.704770,0.704770,0.973300,1,1,1]
  qcorr=1.63
  scts1=[15,14,14]
  bcts1=[36,34,50]
  t1=[8665,9302,12882]
  
;  n=n_elements(t)-n_elements(scts0)
  n=10
  scts=dblarr(n)
  bcts=dblarr(n)
  time=dblarr(n)
;  plot,[200,400],[200,400],/nodata,/iso,/yno
  colors=[!red,!blue,!green,!yellow,!cyan,!magenta,!orange,!purple,!forestgreen,!sienna]
  for i=0,n-1 do begin 
     s=where(src.time-btime ge tmin[i] and src.time-btime le tmax[i],ns)
     nwss=0
     for is=0,ns-1 do begin
        ws=where(src[s[is]].detx eq bp.rawx+1 and src[s[is]].dety eq bp.rawy+1,nws)
        nwss=nwss+nws
     endfor 
     scts[i]=ns-nwss
     b=where(bg.time-btime ge tmin[i] and bg.time-btime le tmax[i],nb)
     nwbb=0
     for ib=0,nb-1 do begin
        wb=where(bg[b[ib]].detx eq bp.rawx+1 and bg[b[ib]].dety eq bp.rawy+1,nwb)
;        if nwb gt 0 then 
        nwbb=nwbb+nwb
     endfor 
     bcts[i]=nb-nwbb
     if ns gt 0 then time[i]=mean(src[s].time-btime) else time[i]=mean(bg[b].time-btime)
;     plots,src[s].detx,src[s].dety,psym=1,color=colors[i]
      
  endfor 

;  stop
;  scts=[scts,scts0]
;  bcts=[bcts,bcts0]
;  time=[time,time0]
  
  asab=1./4
  cts=(scts-bcts*asab)/t[0:n-1]
  err=sqrt(scts+bcts*asab^2)/t[0:n-1]
  cts1=(scts1-bcts1*asab)/t1
  err1=sqrt(scts1+bcts1*asab^2)/t1
  
;  a=[1,0,0.0005]
;  weights=1./err1^2
;  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 3)
;  parinfo[1].fixed=1
;  fit=mpcurvefit(time1,cts1,weights,A,SIGMA,FUNCTION_NAME='pwr_wcons',/noder,parinfo=parinfo,chisq=chisq)  
;  print,a[2]
;  stop
  bat=1.56707e-7
  xrt=3.48449e-11;4.45032e-11
  cts=[cts0,cts,cts1]
  err=[err0,err,err1]
  time=[time0,time,time1]
  tmin=[time0-0.032,tmin]
  tmax=[time0+0.032,tmax]
  
  w1=where(cts-err le 1e-8,nw1)
  w=where(cts-err gt 1e-8,nw)
  w0=where(cts lt 0,nw)
  
  for i=0,nw-1 do begin
     get_2sig_ul,cts[w0[i]],err[w0[i]],newerr
     err[w0[i]]=newerr
  endfor 
  flux=dblarr(n_elements(cts)) & ferr=flux
  flux[0:11]=cts[0:11]*bat
  flux[12:*]=cts[12:*]*xrt      ;*qcorr
  flux[12:15]=flux[12:15]*qcorr
  ferr[0:11]=err[0:11]*bat
  ferr[12:*]=err[12:*]*xrt;/qcorr
  
  A=[1000.,-1.0,1e-13]
  weights=1./ferr[12:*]^2
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 3)
  parinfo.value=a
 
  fit=mpcurvefit(time[12:*],flux[12:*],weights,A,SIGMA,FUNCTION_NAME='pwr_wcons',/noder,parinfo=parinfo,chisq=chisq)  
;  plot_plot_lc,time[0:3],cts[0:3],err[0:3],a,sigma,/noplot
;  cts=[cts0,cts,cts1]
;  err=[err0,err,err1]
;  time=[time0,time,time1]
;  tmin=[time0-0.032,tmin]
;  tmax=[time+0.032,tmax]
  
  ploterror,time[w],flux[w],ferr[w],psym=3,/xlog,/ylog,xrange=[0.01,max(time)],yrange=[1e-15,max(flux)],xtitle='T-T0 (s)',ytitle='Flux!L(0.2-10 keV)!N [ergs cm!U-2!N s!U-1!N]',_extra=_extra,/nohat
;  w1=where(cts-err le 1e-6,nw1)
  flux[w1]=flux[w1]+ferr[w1]
  
  plotsym,1,3,thick=4
  if nw1 gt 0 then oplot,time[w1],flux[w1],psym=8
  fit=a[0]*time^a[1]+a[2]
  oplot,time,fit
  oplot,[1e-4,time],replicate(a[2],n_elements(time)+1),line=2
  legend,[!tsym.alpha+' = '+ntostr(round(a[1]*100.)/100.,5)+!tsym.plusminus+ntostr(round(sigma[1]*100.)/100.,4),'','F!L"cluster"!N = '+ntostr(round(a[2]*1e17)/1000.,5)+!tsym.plusminus+ntostr(round(sigma[2]*1e17)/1000.,5)+' x 10!U'+ntostr(round(alog10(a[2])))+'!N (ergs cm!U-2!N s!U-1!N)'],/top,/right,box=0
;  plot_plot_lc,time,cts,err,a,sigma,/nofit,/nohat,xrange=[1e-2,1e6]
  for i=0,n_elements(tmin)-1 do oplot,[tmin[i],tmax[i]],[flux[i],flux[i]]
  
;  flux=[lc[w[0:11]].rate*2.0115e-8/0.1285,cts[12:*]*1.1254e-12/2.3809e-2] 
;  error=[lc[w[0:11]].error*2.0115e-8/0.1285,err[12:*]*1.1254e-12/2.3809e-2]    
 
  
  stop
  return
end 
