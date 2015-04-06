function sn1987a_model_ring,x,y,a,doplot=doplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  This new model needs to be simplified and clearly define the characteristic radius.
;  It should consist of a 2-D Gaussian elliptical (or circular?) torus plus 4 point 
;  sources with varying weights.
;
;  Parameters:  
;    xpos = 1D array or x values  
;    ypos = 1D array of y values
;    x0 = X coordinate of center of remnant
;    y0 = Y coordinate of center of remnant
;    sigr = Gaussian width of circular torus
;    r0 = radius of peak brightness of torus
;    a0 = background level of torus component
;    a1 = weighting of torus component
;    s0 = intensity of spot 1      
;    theta0 = position angle of spot 1
;    sigt0 = tangential width of spot 1
;    sigr0 = radial width of spot 1
;    s1 = intensity of spot 2
;    theta1 = position angle of spot 2
;    sigt1 = tangential width of spot 2
;    sigr1 = radial width of spot 2
;    s2 = intensity of spot 3
;    theta2 = position angle of spot 3
;    sigt2 = tangential width of spot 3
;    sigr2 = radial width of spot 3
;    s3 = intensity of spot 4
;    theta3 = position angle of spot 4
;    sigt3 = tangential width of spot 4
;    sigr3 = radial width of spot 4
;
;  
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  a0=a[0] ;background component
  a1=a[1] ;scaling of torus
  x0=a[2] ;x center
  y0=a[3] ;y center
  
  sigr=a[4] ;gaussian width of torus
  r0=a[5] ;radius of peak brightness
  
  s0=a[6] ;intensity of spot 1
  theta0=a[7] ;position angle of spot 1
  sigt0=a[8] ;tangential width of spot 1
  sigr0=a[9] ;radial width of spot 1
  
  s1=a[10] ;intensity of spot 2
  theta1=a[11] ;position angle of spot 2
  sigt1=a[12] ;tangential width of spot 2
  sigr1=a[13] ;radial width of spot 2
  s2=a[14] ;intensity of spot 3
  theta2=a[15] ;position angle of spot 3
  sigt2=a[16] ;tangential width of spot 3
  sigr2=a[17] ;radial width of spot 3
  s3=a[18] ;intensity of spot 4
  theta3=a[19] ;position angle of spot 4
  sigt3=a[20] ;tangential width of spot 4
  sigr3=a[21] ;radial width of spot 4
  counts=a[22]
  
  ;;Step 1: build Gaussian ellipse
  
  xpos = (float(indgen(200)) - 100.0) - 0.5
  xpos = rebin(xpos,200,200,/sample)
  ypos = transpose(xpos)
  
  r2=(xpos-x0)^2+(ypos-y0)^2
  r=sqrt(r2)
  
;  g=1./(2.*!pi*sigr^2)*exp(-(r-r0)^2/(2.*sigr^2))
  g=exp(-(r-r0)^2/(2.*sigr^2))
;  g=exp(-0.5*((r-r0)/sigr)^2)
  
  image1=a0+a1*g
  
  if keyword_set(doplot) then begin 
     !p.multi=[0,3,2]
     rdis,image1,/silent
     !p.multi=0
  endif 
  image=image1*counts/total(image1)
  
  return,image
end 

