pro fix_psf,ev
  
  fwhm=8.47
 
  img=fltarr(40,40)
  img[ev.detx-275,ev.dety-300]=1;ev.pha
  gcntrd,img,19,19,xcen,ycen,fwhm
  psf=psf_gaussian(npixel=40,fwhm=fwhm,ndim=2,/normal,centroid=[xcen,ycen])
  !p.multi=[0,2,1]
  rdis,img
  plots,xcen,ycen,psym=1,color=!blue
  rdis,psf
  plots,xcen,ycen,psym=1,color=!blue
  !p.multi=0
  m1=17
  m2=19
  print,xcen,ycen
  print,total(img[m1:m2,*]),total(psf[m1:m2,*]),n_elements(ev)/(1-total(psf[m1:m2,*]))
  
;  stop
  return
end 
