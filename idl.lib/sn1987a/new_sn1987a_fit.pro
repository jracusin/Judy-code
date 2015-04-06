pro new_sn1987a_fit,im,a,sigma,chisq,dof,iter,fitim,perror,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat
  
  a0=0.
  a1=1.
  x0=0.
  y0=0.
  sigr=10.
;  sigr=13.
  r0=50.
  s0=1.
  sigt0=20.*!dtor
  sigr0=2.
  s1=1.
  sigt1=20.*!dtor
  sigr1=2.
  s2=1.
  sigt2=20.*!dtor
  sigr2=2.
  s3=1.
  sigt3=20.*!dtor
  sigr3=2.
  s4=1.
  sigt4=20.*!dtor
  sigr4=2.
  s5=1.
  sigt5=20.*!dtor
  sigr5=2.
  s6=1.
  sigt6=20.*!dtor
  sigr6=2.
  
  theta=[45,135,225,315,345,345,345]*!dtor
  if keyword_set(bilat) then theta=[0,180,225,315,345,345,345]*!dtor
  theta0=theta[0]
  theta1=theta[1]
  theta2=theta[2]
  theta3=theta[3]
  theta4=theta[4]
  theta5=theta[5]
  theta6=theta[6]
  
  
  counts=total(im)
  
;  a =[a0,a1,x0,y0,sigx,sigy,b1,r0,theta0,sigr,sigt,c0,c1,phi0,counts,mu0,sigt2,b2]
  a=[a0,a1,x0,y0,sigr,r0,s0,theta0,sigt0,sigr0,s1,theta1,sigt1,sigr1,$
     s2,theta2,sigt2,sigr2,s3,theta3,sigt3,sigr3,counts,$
     s4,theta4,sigt4,sigr4,s5,theta5,sigt5,sigr5,s6,theta6,sigt6,sigr6]*1d
  
;  new_sn1987a_model,x,a,fim
  
;  stop
  parname=['a0','a1','x0','y0','sigr','r0',$
           's0','theta0','sigt0','sigr0',$
           's1','theta1','sigt1','sigr1',$
           's2','theta2','sigt2','sigr2',$
           's3','theta3','sigt3','sigr3','counts',$
           's4','theta4','sigt4','sigr4',$
           's5','theta5','sigt5','sigr5',$
           's6','theta6','sigt6','sigr6']
  
  parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0],$
                       parname:''}, n_elements(a))
  
  parinfo.parname=parname
  parinfo.value=a
  parinfo[22].fixed=1 ;;counts
  parinfo[[2,3,5]].limited=[1,1]
;  parinfo[[2,3,5,7,11,15,19]].limited=[1,1]
  
;  parinfo[[0,1]].limited=[1,0] ;a0 & a1
;  parinfo[[0,1]].limits=[0,1]
  
;  parinfo[[6,10,14,18]].limited=[1,0]
;  parinfo[[6,10,14,18]].limits=[0,1e100]
  parinfo[[2,3]].limits=[-30,30] ;x0&y0
  parinfo[5].limits=[30,80]     ; r0
  parinfo[[8,12,16,20]].limited=[1,1] ;;sigt
  parinfo[[8,12,16,20]].limits=[20,90]*!dtor
  
  parinfo[[6,10,14,18,23,27,31]].limited=[1,0] ;s0,s1,s2,...
  parinfo[[6,10,14,18,23,27,31]].limits=[0,0]
  parinfo[[7,11,15,19]].limited=[1,1] ;;theta
;  parinfo[[7,11,15,19]].limits=[[10,80],[100,170],[190,260],[280,350]]*!dtor ;;theta
;  parinfo[[7,11,15,19]].limits=[[0,90],[90,180],[180,270],[270,360]]*!dtor
  parinfo[[7,11,15,19]].limits=[[0,100],[80,190],[170,280],[260,370]]*!dtor
  if keyword_set(bilat) then parinfo[[7,11]].limits=[[-90,90],[90,270]]*!dtor
  
;  weights=1./im^2
  weights=1./im ;;Poisson weights
;  weights=sqrt(im)
  err=sqrt(im)
  wbad=where(im EQ 0,nwbad)
  if nwbad gt 0 then err[wbad]=0.01
;  IF nwbad GT 0 THEN weights[wbad]=0.
  
;  xpos=lindgen(200,200L)
;  xpos=reform(xpos,200L*200L)
;  image = reform(im,200L*200L)
  xr=indgen(200)
  yc=indgen(200)
  xpos=xr#(yc*0+1)
  ypos=(xr*0+1)#yc
  
  if keyword_set(lobes) then model='sn1987a_model_lobes'
  if keyword_set(ring) then model='sn1987a_model_ring'
  if keyword_set(ptsrc) then model='sn1987a_model_ptsrc'
  if keyword_set(bilat) then model='sn1987a_model_2lobes'
  
  case model of
     'sn1987a_model_lobes': begin
        parinfo[[9,13,17,21]].fixed=1            
        ddof=18
     end 
     'sn1987a_model_ring': begin 
        parinfo[6:21].fixed=1                              
        ddof=6
     end 
     'sn1987a_model_ptsrc': begin 
        parinfo[[8,12,16,20]].fixed=1  
        ddof=18
     end 
     'sn1987a_model_2lobes': begin 
        parinfo[[9,13,14,15,16,17,18,19,20,21]].fixed=1  
        ddof=13
     end 
     
  endcase
  
 ;;; Svet's test
;  parinfo[4].fixed=1
  
  
  result=mpfit2dfun(model,xpos,ypos,im,err,a,$;sigma,$
                    /noder,niter=iter,nprint=10,$
                    bestnorm=chisq,ftol=0.0001,$
                    perror=sigma,parinfo=parinfo,$
                    dof=dof,yfit=fitim);,weights=weights)
;  a=result
;  result=yfit

;  result=mpcurvefit(xpos,image,weights,a,sigma,$
;                    FUNCTION_name=model,$
;                    /noderivative,iter=iter,nprint=10,$
;                    chisq=chisq,ftol=0.0001,$
;                    sigma=sigma,parinfo=parinfo)
  
;  fitim=reform(result,200L,200L)
;  wpar=where(a-result ne 0.,npar)
;  wim=where(im gt 0,nim)
;  dof2=nim-npar
;  sigma=sigma;*sqrt(chisq/dof2)
  
  a=result
  parinfo.value=result
  parinfo[5].limits=[0,100]
  sigma[5]=0.5
  w=where(im ne 0 and fitim ne 0 and err ne 0,nw)
  dof=nw-ddof
  chisq=total((im[w]-fitim[w])^2/err[w]^2)
  conf_error2d,xpos,ypos,im,err,a,sigma,model,perror,bestfit,pvarunit,bestchisq,yfit,psave,/d2,parinfo=parinfo,wpar=[4,5],weights=weights,/doplot,delchi0=1.
  
  return
end 
