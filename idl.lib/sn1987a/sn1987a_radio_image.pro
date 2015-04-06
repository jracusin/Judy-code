pro sn1987a_radio_image,image

; This program simulates the radio image of SN1987A in Gaensler et al. 1997.

x0 = 0
y0 = 0
sigx = 1.2
sigy = 0.9
a0 = 0.0
a1 = 1.0


image = fltarr(400,400)
xpos = (float(indgen(400)) - 200.0)/50.0

xpos = rebin(xpos,400,400,/sample) + 0.001
ypos = transpose(xpos)

u = ((xpos-x0)/sigx)^2 + ((ypos-y0)/sigy)^2
image1 = a0 + a1*exp(-u/2.0)

b0 = 0.0
b1 = 0.5
r0 = 0.6
theta0 = 0.0
sigr = 0.2
sigt = 35.0
r = sqrt(((xpos-x0)^2/sigx) + ((ypos-y0)^2/sigy))
v = ((r-r0)/sigr)^2
theta = atan(ypos/xpos)/!dtor
w = ((theta-theta0)/sigt)^2
image2 = b0 + b1*exp(-v/2.0)*exp(-w/2.0)

image3 = image1 + image2
image3(0:199,*) = image3(0:199,*) + image2(0:199,*) ; left side is twice as bright
image = image3/40.0	; rescale for correct total # counts

tvscl,image

stop
end
