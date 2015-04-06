function sn1987a_model_ptsrc,x,y,a,doplot=doplot
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
  
  s2=a[14]                      ;intensity of spot 3
  theta2=a[15] ;position angle of spot 3
  sigt2=a[16] ;tangential width of spot 3
  sigr2=a[17] ;radial width of spot 3
  
  s3=a[18]                      ;intensity of spot 4
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
  
  ;;Step 2: add point sources
  ;each point source on torus has position angle,intensity,tangential width (& radial width?)
  ;theta0,s0,sigt0,sigr0
  theta = atan(ypos,xpos)
  w=where(theta lt 0)
  theta[w]=theta[w]+2*!pi
  
  t0=theta-theta0
  w=where(t0 gt !pi,nw)
  if nw gt 0 then t0[w]=t0[w]-2.*!pi
   
  t1=theta-theta1
  w=where(t1 gt !pi,nw)
  if nw gt 0 then t1[w]=t1[w]-2.*!pi
  
  t2=theta-theta2
;  w=where(t2 lt -!pi)
;  t2[w]=t2[w]+2.*!pi
  
  t3=theta-theta3
  w=where(t3 lt -!pi,nw)
  if nw gt 0 then t3[w]=t3[w]+2.*!pi
  
;  image2=s0*exp(-(r-r0)^2/(2.*sigr0^2)) ;spot 1
;  image3=s1*exp(-(r-r0)^2/(2.*sigr1^2)) ;spot 2
;  image4=s2*exp(-(r-r0)^2/(2.*sigr2^2)) ;spot 3
;  image5=s3*exp(-(r-r0)^2/(2.*sigr3^2)) ;spot 4
  ;; symetric point source model 
  rp=sqrt((xpos-r0*cos(theta0))^2+(ypos-r0*sin(theta0))^2)
  image2=s0*exp(-rp^2/(2.*sigr0^2)) ;spot 1
  rp=sqrt((xpos-r0*cos(theta1))^2+(ypos-r0*sin(theta1))^2)
  image3=s1*exp(-rp^2/(2.*sigr1^2)) ;spot 2
  rp=sqrt((xpos-r0*cos(theta2))^2+(ypos-r0*sin(theta2))^2)
  image4=s2*exp(-rp^2/(2.*sigr2^2)) ;spot 3
  rp=sqrt((xpos-r0*cos(theta3))^2+(ypos-r0*sin(theta3))^2)
  image5=s3*exp(-rp^2/(2.*sigr3^2)) ;spot 4
  
;  rp=sqrt((xpos-r0*cos(theta4))^2+(ypos-r0*sin(theta4))^2)
;  image6=s4*exp(-rp^2/(2.*sigr3^2)) ;spot 5  
;  rp=sqrt((xpos-r0*cos(theta5))^2+(ypos-r0*sin(theta5))^2)
;  image7=s5*exp(-rp^2/(2.*sigr3^2)) ;spot 6
;  rp=sqrt((xpos-r0*cos(theta6))^2+(ypos-r0*sin(theta6))^2)
;  image8=s6*exp(-rp^2/(2.*sigr3^2)) ;spot 7
    
  image6=image1*[image2+image3+image4+image5]+image1
  
  ;;Step 4: Scale by total counts in image
  image7=image6*counts/total(image6)
  
;  doplot=1
  if keyword_set(doplot) then begin 
     !p.multi=[0,3,2]
     rdis,image1,/silent
     rdis,image2,/silent
     rdis,image3,/silent
     rdis,image4,/silent
     rdis,image5,/silent
     rdis,image7,/silent
     !p.multi=0
  endif 
  
  image=image7

  return,image
end 
