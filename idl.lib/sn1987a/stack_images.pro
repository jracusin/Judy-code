PRO plot_stack_images,gim,fitim,a

  !p.multi=0

  begplot,name='stack_images.ps',/land
  simpctable
  c0=!white
  c1=!black
  !p.multi=[0,2,1]
  x0=round(a[2]*25.+100.)
  y0=round(a[3]*25.+100.)
;  gim[x0-34.:x0+34.,30]=max(gim)/2.
  rdis,gim,low=min(gim),high=max(gim),charsize=1,color=c0
  title='Stacked Image'
  xyouts,4500,13000,title,/device,charsize=1
  oplot,[x0-34,x0+34],[30,30]
  oplot,[0,0],[0,199]
  oplot,[198,198],[0,199]
  oplot,[0,199],[0,0]
  oplot,[0,199],[199,199]
  xyouts,5500,5500,'1 arcsec',/device,charsize=1
  
;  fitim[x0-34.:x0+34.,30]=max(fitim)/2.
  rdis,fitim,low=min(fitim),high=max(fitim),charsize=1,color=c0
  title='Simulated Image'
  xyouts,16500,13000,title,/device,charsize=1
  oplot,[x0-34,x0+34],[30,30]
  oplot,[0,0],[0,199]
  oplot,[198,198],[0,199]
  oplot,[0,199],[0,0]
  oplot,[0,199],[199,199]
  xyouts,17500,5500,'1 arcsec',/device,charsize=1
  print,x0,y0
  endplot
  !p.multi=0
END 

PRO stack_images,gim,fitim,suffix=suffix,a=a

  IF n_elements(suffix) EQ 0 THEN suffix='' ELSE suffix='_'+suffix
  fpars=mrdfits('sn1987a_fpars'+suffix+'.fits',1)

  sigx=2.
  sigy=1.5

  x0=fpars.x0;*25.
  y0=fpars.y0;*25.

  tim=fltarr(2000,2000)
;  n=ntostr([1,2,3,4,5,6,7,8,9,10,11,12,13])
  n=ntostr([1,2,3,4,5,6,7,8,9,10,11,12,13,14]) ;include 11?
  nn=n_elements(n)
  low=-1.41882e-10;-5.41807e-10
  high=0.000230541;0.00821329
  newlow=0
  newhigh=0
  !p.multi=[0,3,5]
  FOR i=0,nn-1 DO begin
  
      im=mrdfits('sn1987a'+n[i]+'_fitim'+suffix+'.fits',0)
      
      print,min(im),max(im)
      xc=round(x0[i]*5.)
      yc=round(y0[i]*5.)
      cim=congrid(im,1000,1000,/center);,center=[x0[i],y0[i]])

      tim[(499+xc):(1498+xc),(499+yc):(1498+yc)]=tim[(499+xc):(1498+xc),(499+yc):(1498+yc)]+cim;/fpars[i].counts
      rdis,im/fpars[i].counts;,low=low,high=high,title=ntostr(i+1) ;,low=min(im),high=max(im)
;      gim=congrid(tim,400,400)
;      gim=gim[99:298,99:298]
;      rdis,gim,low=min(gim),high=max(gim)
      
      if min(im) lt newlow then newlow=min(im)/fpars[i].counts
      if max(im) gt newhigh then newhigh=max(im)/fpars[i].counts
  ENDFOR
  cim=0 
  
  print,'new limits:',newlow,newhigh
  
  gim=congrid(tim,400,400)
  gim=gim[99:298,99:298]
  rdis,gim,low=min(gim),high=max(gim)
  
  sn1987a_fit,gim,a,fitim,chisq,iter,sigma,/stack
  rdis,fitim,low=min(fitim),high=max(fitim)
;  rdis,gim,low=min(gim),high=max(gim)
  
  tvellipse,a[4]*25.,a[5]*25.,a[2]*25.+100.,a[3]*25.+100.,/data

  print,a,chisq,iter,sigma

  plot_stack_images,gim,fitim,a
  

  return
END 
