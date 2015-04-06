pro sn1987a_sims,m,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat
  
    if not keyword_set(lobes) and not keyword_set(ring) and not keyword_set(ptsrc) and not keyword_set(bilat) then begin 
     print,'Need to specify model (lobes, ring, ptsrc)'
     return
  endif 
  
  if keyword_set(ring) then sdir='ring/'
  if keyword_set(ptsrc) then sdir='ptsrc/'
  if keyword_set(lobes) then sdir='lobes/'
  if keyword_set(bilat) then sdir='bilat/'
  
;  n=[1,6]
;  n=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19]
;  dir='~jracusin/data/sn1987a/image_analysis/'
  n=[1,2,3,4]
  dir='/Volumes/Firewire1/racusin/sn1987a/simulations/'
  models=['0deg_torus','45deg_torus','45deg_torus_4spot']
  if n_elements(m) eq 0 then begin 
     m=0
     read,'which model, m=0 0deg, m=1 45deg, m=2 45deg_4spot? ',m
  endif 
  model=models[m]
  dir=dir+model+'/'
  cd,dir
  files=dir+['test_4600_'+model+'.fits','test_5200_'+model+'.fits','test_6200_'+model+'.fits','test_7600_'+model+'.fits']
  print,files
  nn=n_elements(files)
  
  fpars=create_struct('a0',0d,'a0err',0d,'a1',0d,'a1err',0d,$
                      'x0',0d,'x0err',0d,'y0',0d,'y0err',0d,$
                      'sigr',0d,'sigrerr',0d,'r0',0d,'r0err',dblarr(2),$
                      's0',0d,'s0err',0d,'theta0',0d,'theta0err',0d,$
                      'sigt0',0d,'sigt0err',0d,'sigr0',0d,'sigr0err',0d,$
                      's1',0d,'s1err',0d,'theta1',0d,'theta1err',0d,$
                      'sigt1',0d,'sigt1err',0d,'sigr1',0d,'sigr1err',0d,$
                      's2',0d,'s2err',0d,'theta2',0d,'theta2err',0d,$
                      'sigt2',0d,'sigt2err',0d,'sigr2',0d,'sigr2err',0d,$
                      's3',0d,'s3err',0d,'theta3',0d,'theta3err',0d,$
                      'sigt3',0d,'sigt3err',0d,'sigr3',0d,'sigr3err',0d,$
                      'counts',0d,'iter',0,'chisq',0d,'dof',0d)
  
  counts=[890,6000,12800,12900.]
  npars=n_tags(fpars)
  fpars=replicate(fpars,nn)
  r0=dblarr(nn) & r0err=dblarr(2,nn)
  !p.multi=[0,4,4]
  g=0
  print,'set g'
;  stop
  if g ne 0 then fpars=mrdfits(sdir+'new_sn1987a_fpars.fits',1)

  for i=g,nn-1 do begin
     print,i
     im=mrdfits(files[i])
     im=im-min(im)
     im=im/max(im) ;;;LOSE FLUX INFO
     im=im/total(im)*counts[i]
     ;;; only deproject if image is inclined
     if m ge 1 then deproject_image,im,newim,inc=45. else newim=im
     
     print,total(im),total(newim)
;     sn1987a_fit,newim,a,fitim,chisq,iter,sigma,r0errs,i,stack=stack

     sn1987a_sim_fit,newim,a,sigma,chisq,dof,iter,fitim,perror,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat
;     !p.multi=0
     rdis,im
     rdis,newim
     rdis,fitim
     
;     for j=0,n_elements(a)-1 do fpars[i].(j)=a[j]
     for j=0,npars-3,2 do fpars[i].(j)=a[j/2]
     
;     dof=200.^2-22.
;     csigma=sigma*sqrt(chisq/dof)

     for j=1,npars-2,2 do fpars[i].(j)=sigma[(j-1)/2]
     r0[i]=a[5]
     r0err[*,i]=perror[*,5]
     fpars[i].r0err=r0err[*,i]
     fpars[i].iter=iter
     fpars[i].chisq=chisq
     fpars[i].dof=dof
     
     ofile='new_sn1987a'+ntostr(n[i])+'_fitim.fits'
     mwrfits,newim,sdir+ofile,/create
     mwrfits,fitim,sdir+ofile
     diff=newim-fitim
     mwrfits,diff,sdir+ofile
     
     mwrfits,fpars,sdir+'new_sn1987a_fpars.fits',/create
;stop
  endfor 
  print,r0,r0err
  
  !p.multi=0
  return
end 
