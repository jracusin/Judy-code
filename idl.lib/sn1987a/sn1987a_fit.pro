; This routine performs a fit to the SN1987A data using model sn1987a_model.pro
;

pro sn1987a_fit,im,a,fitim,chisq,iter,sigma,r0errs,i,stack=stack

  IF n_elements(a) EQ 0 THEN BEGIN 
     if keyword_set(stack) then $ 
        file='sn1987a_fpars_stack.fits' else $
        file='sn1987a_fpars.fits'
     a0 = 0.
     a1 = 1.0
     x0 = 0.
     y0 = 0.
;     if exist(file) then begin 
;        fpars=mrdfits(file,1)
;        if not keyword_set(stack) then fpars=fpars[i]
;        sigx = fpars.sigx       ;1.3686833;1.3831841;1.2
;        sigy = fpars.sigy       ;0.97751346;0.99691797;1.2      
;        theta0 = fpars.theta0   ;50.;0.
;        sigr = fpars.sigr       ;0.35;0.2
;        sigt = fpars.sigt       ;35.0
;        phi0 = fpars.phi0       ;160.0;180.0
;        mu0=fpars.mu0           ;140.;90.
;        print,'Using best fit from stack'
;     endif else begin 
        sigx = 1.;5
        sigy = 1.;2      
        theta0 = 50.            ;0.
        sigr = 0.35             ;0.2
        sigt = 35.0
        phi0 = 160.0            ;180.0
        mu0= 140.               ;90.
;     endelse 
     b0 = 0.
     b1 = 1.                    ;0.6
     r0 = 1.5                   ;0.6;*100.
     c0 = 3.0
     c1 = 1.0
     counts = total(im)         ;9600.0
     sigt2=35.0
     b2=9.                      ;0.6

     a = dblarr(18)
     a[0] = a0
     a[1] = a1
     a[2] = x0
     a[3] = y0
     a[4] = sigx
     a[5] = sigy
     a[6] = b1
     a[7] = r0
     a[8] = theta0
     a[9] = sigr
     a[10] = sigt
     a[11] = c0
     a[12] = c1
     a[13] = phi0
     a[14] = counts
     a[15] = mu0
     a[16] = sigt2
     a[17] = b2
  ENDIF  
  
  
  a[14]=total(im)  

  xpos=lindgen(200,200L)
  xpos=reform(xpos,200L*200L)
  image = reform(im,200L*200L)


  IF NOT keyword_set(stack) THEN BEGIN 
      parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                           limits:[0.D,0],tied:'',$
                           parname:''}, 18)
      fixed=[4,5,11,14,17]
;      parinfo[fixed].fixed=1
      
      parinfo(14).fixed = 1
;      parinfo(4).fixed=1
;      parinfo(5).fixed=1
;      parinfo(1).fixed=1
;      parinfo(17).fixed=1
;      parinfo[6].limited=1
;      parinfo[6].limits=[0,1e3]
      
;      parinfo[12].limited=[1,0]
;      parinfo[12].limits[0]=0
      parinfo[7].limited=[1,1]
      parinfo[7].limits=[0,10]
;      parinfo[1].limited=[1,0]
;      parinfo[1].limits[0]=0
      parinfo[8].limited=[1,1]
      parinfo[8].limits=[0,90]
      parinfo[15].limited=[1,1]
      parinfo[15].limits=[90,180]
;      parinfo[1].limited=[1,0]
;      parinfo[1].limits=[0,0]
;      parinfo[6].limited=[1,0]
;      parinfo[6].limits=[0,0]
      
      parinfo[4].tied='p(5)'
;      parinfo[4].fixed=1
      parinfo[[0,6,11,12,16]].limited=[1,0]
      parinfo[[0,6,11,12,16]].limits=[0,1]
      
      
      parinfo.parname=['a0','a1','x0','y0','sigx','sigy','b1','r0','theta0',$
                    'sigr','sigt','c0','c1','phi0','counts','mu0',$
                    'sigt2','b2']
      
      weights=1./im
      wbad=where(im EQ 0,nwbad)
      IF nwbad GT 0 THEN weights[wbad]=0.
      
      result=mpcurvefit(xpos,image,weights,a,sigma,$
                        FUNCTION_name='sn1987a_model',$
                        /noderivative,iter=iter,nprint=10,$
                        ftol=0.0001,chisq=chisq,$
                        sigma=sigma,parinfo=parinfo,$
                        nfree=18-n_elements(fixed))
      
      
;      steppar,xpos,image,weights,a,par1ind=7,par1min=1.0,par1max=1.5,nstep1=10,dof=dof,chi2red=chi2red,/plot,/dev,par1val=par1val,chi2val=chi2val,function_name='sn1987a_model'
;      resulterr=mpfiterror('sn1987a_model_fcn',xpos,image,err,a,$
;                           weights=weights,$
;                           parinfo=parinfo,bestnorm=chisq,delchi=delchi,$
;                           toldel=toldel,intpar=7,perror=sigma,chierr=chierr)
      
;      stop
;      result=mpcurvefit(xpos,image,weights,a,sigma,$
;                        FUNCTION_name='sn1987a_model',$
;                        /noderivative,iter=iter,nprint=10,$
;                        ftol=0.0001,chisq=chisq,$
;                        sigma=sigma,parinfo=parinfo,nfree=13)
      
      ;step through each parameter to get err on 
;       bounds=[[0,1d15],$ ;0
;               [a[1],a[1]],$              ;1
;               [-1,1],$              ;2
;               [-1,1],$              ;3
;               [a[4],a[4]],$              ;4
;               [a[5],a[5]],$              ;5
;               [-1d30,1e30],$              ;6
;               [0,10],$              ;7
;               [0,360],$              ;8
;               [0,10],$              ;9
;               [0,1e5],$              ;10
;               [-1e10,1e10],$              ;11
;               [-1e10,1e10],$              ;12
;               [0,1e4],$              ;13
;               [a[14],a[14]],$              ;14
;               [0,1e3],$              ;15
;               [-1e4,1e4],$              ;16
;               [a[17],a[17]]]               ;17
      
      
;       steppar,xpos,image,weights,a,/noder,chi2red=chisq,/plot,$
;          FUNCTION_name='sn1987a_model',par1ind=7,par1min=-0.2,$
;          par1max=3.2,nstep1=30,sigmaa=sigma,par1val=par1val,$
;          chi2val=chi2val,bounds=bounds,nprint=10
;       print,par1val,chi2val
;       stop
;      result=mpfitfun('sn1987a_model_fcn',xpos,image,err,a,$
;                      weights=weights,parinfo=parinfo,ftol=0.0001,$
;                      /noder,nprint=10,niter=iter,nfree=13,$
;                      bestnorm=chisq,perror=sigma,yfit=yfit)
;      a=result
;      result=yfit

;      parinfo.value=a
;      resulterr=mpfiterror('sn1987a_model_fcn',xpos,image,err,a,$
;                        weights=weights,parinfo=parinfo,bestnorm=chisq,$
;                        intpar=7,perror=sigma,nprint=10,ftol=0.0001,$
;                        niter=iter,nfree=13,/noder)
;      sigmalow=a[7]-resulterr[7].min
;      sigmahigh=resulterr[7].max-a[7]
;      sigma[7]=mean([sigmahigh,sigmalow])
;      r0errs=[sigmalow,sigmahigh]
;      print,r0errs
      r0errs=[sigma[7],sigma[7]]
  ENDIF
  IF keyword_set(stack) THEN BEGIN 
      parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                           limits:[0.D,0],$
                           parname:''}, 18)
      parinfo(14).fixed = 1
;      parinfo[1].limited=[1,0]
;      parinfo[1].limits=[0,0]
;      parinfo[6].limited=[1,0]
;      parinfo[6].limits=[0,0]
;      parinfo[1].limited=[1,0]
;      parinfo[1].limits[0]=0
;      parinfo[6].limited=[1,0]
;      parinfo[6].limits[0]=0
;      parinfo[12].limited=[1,0]
;      parinfo[12].limits[0]=0
      
      parinfo[8].limited=[1,1]
      parinfo[8].limits=[0,90]
      parinfo[15].limited=[1,1]
      parinfo[15].limits=[90,180]
      
      
;      parinfo[[11]].fixed=1
      parinfo.parname=['a0','a1','x0','y0','sigx','sigy','b1','r0','theta0',$
                    'sigr','sigt','c0','c1','phi0','counts','mu0',$
                    'sigt2','b2']
      
      weights=1./im
      wbad=where(im EQ 0,nwbad)
      IF nwbad GT 0 THEN weights[wbad]=0.
      result=mpcurvefit(xpos,image,weights,a,sigma,$
                        FUNCTION_name='sn1987a_model',$
                        /noderivative,iter=iter,nprint=10,$
                        ftol=0.0001,chisq=chisq,$
                        sigma=sigma,parinfo=parinfo,nfree=16)
      ;parinfo.values=a
      r0errs=[sigma[7],sigma[7]]
      
  ENDIF 

  npar=15
;  tmp=where(image GT mean(image),ngt)
;  DOF     = ngt - npar ; deg of freedom
;  SIGMA  = SIGMA * SQRT(CHISQ / DOF)  

;help,result
  print,iter
  fitim=reform(result,200L,200L)

  return
end
