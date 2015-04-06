pro sn1987a_model, x, a, image,image5,image2,image3

; This program simulates the X-ray image, based on a similar program
;	written to simulate the radio image of SN1987A in Gaensler et al. 1997.
;	This version creates a parameterized model of SN1987A that can be
;	used to do a nonlinear regression fit to the observed data.
;	It returns a 400 x 400 pixel image of the SN1987A model.
;
;	DNB - 08/19/03
;
;	The model consists of a 2-D Gaussian ellipse plus two-lobed 
;	elliptical toroidal component, with the lobes weighted differently.
;	
;	Input parameters:
;	xpos = 1D array of input x values
;	ypos = 1D array of input y values
;	;a0 = background level for component 1 (default = 0)
;	a1 = weighting of component 1 (default = 1.0)
;	x0 = X coordinate of center of remnant (default = 0)
;	y0 = Y coordinate of center of remnant (default = 0)
;	sigx = Gaussian sigma of component #1 in X direction (default = 1.2)
;	sigy = Gaussian sigma of component #1 in Y direction (default = 0.9)
;
;	b0 = background level for component 2 (default = 0)
;	b1 = weighting of component 2 (default = 0.6)
;	r0 = peak radius of component 2 (default = 0.6)
;	theta0 = angle of lobe peak brightness, relative to major axis of
;		ellipse (default = 0)
;	sigr = Gaussian width of toroidal component (default = 0.2)
;	sigt = Gaussian angular width of lobes (default = 35.0)
;
;	c0 = background level for modulation function (default = 3.0)
;	c1 = modulation amplitude for lobes (default = 1.0)
;	phi0 = angular rotation of lobe modulation (default = 180.0)
;
;	counts = total number of counts desired in output image (for scaling)
;	mu0 = angle of second set of lobes
;   sigt2= gaussian angular width of second lobes
  

  a0 = a[0]
  a1 = a[1]
  x0 = a[2]
  y0 = a[3]
  sigx = a[4]
  sigy = a[5]
;  b0 = a0
  b1 = a[6]
  r0 = a[7]
  theta0 = a[8]
  sigr = a[9]
  sigt = a[10]
  c0 = a[11]
  c1 = a[12]
  phi0 = a[13]
  counts = a[14]
  mu0=a[15]
  sigt2=a[16]
  b2=a[17]

; Step 1: build Gaussian ellipse
;  print,'Step 1: building Gaussian ellipse ...'

  image = fltarr(200,200)
  xpos = (float(indgen(200)) - 100.0)/25.0

  xpos = rebin(xpos,200,200,/sample) +0.02;+ 0.5;0.001
  ypos = transpose(xpos)

;  u = ((xpos-x0)/sigx)^2 + ((ypos-y0)/sigy)^2
;  image1 = a0 + a1*exp(-u/2.0)
;tvscl, image1

; Step 2: build double-lobed elliptical toroid
;  print,'Step 2: building double-lobed elliptical toroid ...'
  
;  fact=sqrt(sigx^2+sigy^2)/sigr
;  r = sqrt(((xpos-x0)^2/(sigx/fact)) + ((ypos-y0)^2/(sigy/fact)))
  
  r = sqrt(((xpos-x0)^2/sigx) + ((ypos-y0)^2/sigy)) ;shape of torus
;  r = sqrt(((xpos-x0)/sigx)^2 + ((ypos-y0)/sigy)^2)
  v = ((r-r0)/sigr)^2 ;width of torus
  theta = atan(ypos,xpos)       ;/!dtor
  image1=a1*exp(-v/2.0)         ; torus

;  image1=a1*(image1-min(image1))/max(image1)
  
;  w = (sin(theta-theta0*!dtor)/sin(sigt*!dtor))^2 ;position of lobes 1
;  k = (sin(theta-mu0*!dtor)/sin(sigt2*!dtor))^2 ;position of lobes 2
    
  w = (sin(theta-theta0*!dtor)/sin(sigt*!dtor))^2 ;position of lobes 1
  k = (sin(theta-mu0*!dtor)/sin(sigt2*!dtor))^2 ;position of lobes 2
  
;  image2 = b0 + b1*exp(-v/2.0)*(exp(-w/2.0)+exp(-k/2.0))
;  image2 = a0+b1*exp(-v/2.0)*(exp(-w/2.0)+exp(-k/2.0))
;  image2 = a0+b1*exp(-v/2.0)*(exp(-w/2.0)+exp(-k/2.0))
  image2=(exp(-w/2.0)+exp(-k/2.0)) ;lobe function
;  image2=b1*(image2-min(image2))/max(image2)
  image2=b1*image2;/max(image2)
  
; Step 3: build modulation function for double-lobed toroid
;  print,'Step 3: building modulation function for toroid ...'

  phi = atan(ypos,xpos)/!dtor
;  image3 = c0+c1*cos((phi-phi0)*!dtor)
  image3=c0+c1*cos((phi-phi0)*!dtor)
;  image3=c0+c1*(image3-min(image3))/max(image3)
;  image3=c0+c1*image3/max(image3)
  
;  image3 = image2*(c1*cos((phi-phi0)*!dtor)+c0)

; Step 4: add images (Gaussian ellipse plus double-lobed modulated elliptical toroid)
;  print, 'Step 4: adding Gaussian ellipse and modulated double-lobed elliptical toroid ...'
  image4 = (a0+image1*image2)*image3
;  image4=image3
;  image4=a0+image1*image2*image3

;print,'Step 5: rescaling ...'
  rate = counts/total(image4)
  image5 = rate*image4              ; rescale for correct total # counts

  image = reform(image5,200L*200L)
    
;  stop
  print,'model components'
  !p.multi=[0,2,2]
  print,rate
  rdis,abs(image1),low=min(abs(image1)),high=max(abs(image1))
  rdis,abs(image2),low=min(abs(image2)),high=max(abs(image2))
  rdis,abs(image3),low=min(abs(image3)),high=max(abs(image3))
;  aplot,1,[0,0],[0,0],/nodata,color=!white
;  rdis,abs(image4);,low=min(image4),high=max(image4)
  rdis,image5,low=min(abs(image5)),high=max(abs(image5))
  
;  a=[f.a0,f.a1,f.x0,f.y0,f.sigx,f.sigy,f.b1,f.r0,f.theta0,f.sigr,f.sigt,f.c0,f.c1,f.phi0,f.counts,f.mu0,f.sigt2,f.b2]
  
;  stop
;  !p.multi=0
;  stop
return
end
