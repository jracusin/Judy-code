pro sn1987a_model_gauss, x, a, image,image5,image2,image3

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
;	a0 = background level for component 1 (default = 0)
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
;	
;

  a0 = a[0]
  a1 = a[1]
  x0 = a[2]
  y0 = a[3]
  sigx = a[4]
  sigy = a[5]
  counts = a[14]


; Step 1: build Gaussian ellipse
;  print,'Step 1: building Gaussian ellipse ...'

  image = fltarr(200,200)
  xpos = (float(indgen(200)) - 100.0)/25.0

  xpos = rebin(xpos,200,200,/sample) +0.02;+ 0.5;0.001
  ypos = transpose(xpos)

  u = ((xpos-x0)/sigx)^2 + ((ypos-y0)/sigy)^2
  image1 = a0 + a1*exp(-u/2.0)

;print,'Step 5: rescaling ...'
  r = counts/total(image1)
  image5 = r*image1              ; rescale for correct total # counts
 ; tvscl,image

  image = reform(image5,200L*200L)

return
end
