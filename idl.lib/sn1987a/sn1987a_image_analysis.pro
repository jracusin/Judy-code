PRO sn1987a_fit_wrapper,im,a,sim,fitim,diff,chisq,iter,sigma,r0errs,i,stack=stack

;  sim=fltarr(400,400)

;  center=[99,99]
;  x=center[0]
;  y=center[1]
;  sim[x:x+199,y:y+199]=im
  
  if keyword_set(stack) then stack_images,sim,a=a else $
     sim=im
  sn1987a_fit,sim,a,fitim,chisq,iter,sigma,r0errs,i,stack=stack


  diff=abs(sim-fitim)

  return
END 

PRO sn1987a_image_analysis,suffix,stack=stack

  files=['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits']
;  files=files[5]
  
  n=[1,2,3,4,5,6,7,8,9,10,11,12,13,14]
  if keyword_set(stack) then n=0
;  n=[3,4,5,6,7,8,9,10,12,13]
  nn=n_elements(n)
  
;  nn=1
  fpars=create_struct('a0',0d,'a1',0d,'x0',0d,'y0',0d,'sigx',0d,'sigy',0d,$
                      'b1',0d,'r0',0d,'theta0',0d,'sigr',0d,'sigt',0d,$
                      'c0',0d,'c1',0d,'phi0',0d,'counts',0d,'mu0',0d,$
                      'sigt2',0d,'b2',0d,'chisq',0d,'iter',0,$
                      'r0err',fltarr(2),$
                      'x0err',0d,'y0err',0d)
  fpars=replicate(fpars,nn)

  IF n_params() EQ 0 THEN suffix=''
  if keyword_set(stack) then suffix='_stack'
  
  FOR i=0,nn-1 DO BEGIN 
     
     if n[0] ne 0 then begin 
        file=files[i]
        im=mrdfits(file,0)
        print,'Fitting image of '+file
     endif 
        
      IF i GT 0 THEN a=a3

      sn1987a_fit_wrapper,im,a,sim,fitim,diff,chisq,iter,sigma,r0errs,i,stack=stack
      
      IF i EQ 0 THEN a3=a

      fpars[i].a0=a[0]
      fpars[i].a1=a[1]
      fpars[i].x0=a[2]
      fpars[i].y0=a[3]
      fpars[i].sigx=a[4]
      fpars[i].sigy=a[5]
      fpars[i].b1=a[6]
      fpars[i].r0=a[7]
      fpars[i].theta0=a[8]
      fpars[i].sigr=a[9]
      fpars[i].sigt=a[10]
      fpars[i].c0=a[11]
      fpars[i].c1=a[12]
      fpars[i].phi0=a[13]
      fpars[i].counts=a[14]
      fpars[i].mu0=a[15]
      fpars[i].sigt2=a[16]
      fpars[i].b2=a[17]
      fpars[i].chisq=chisq
      fpars[i].iter=iter
      fpars[i].r0err=r0errs;sigma[7]
      fpars[i].x0err=sigma[2]
      fpars[i].y0err=sigma[3]
      
      ofile='sn1987a'+ntostr(n[i])+'_fitim'+suffix+'.fits'

      mwrfits,sim,ofile,/create
      mwrfits,fitim,ofile;,/create
      mwrfits,diff,ofile

      mwrfits,fpars,'sn1987a_fpars'+suffix+'.fits',/create
     ;stop
  ENDFOR 


  return
END 

